/*
 * $Id: debugger.prg 9 2014-08-28 21:31:01Z alex; $
 */

/* this file adapted FOR hbide from hwgdebug.prg by alex;(Alexey Zapolskiy(pepan@mail.ru))
 * (HWGUI - Harbour Win32 GUI library source code)
 * The GUI Debugger
 *
 * Copyright 2013 Alexander Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */


#include "fileio.ch"
#include "hbide.ch"
#include "common.ch"
#include "hbclass.ch"
#include "hbqtgui.ch"

#define MODE_INPUT                                1
#define MODE_INIT                                 2
#define MODE_WAIT_ANS                             3
#define MODE_WAIT_BR                              4

#define ANS_BRP                                   1
#define ANS_CALC                                  2
#define ANS_STACK                                 3
#define ANS_LOCAL                                 4
#define ANS_WATCH                                 5
#define ANS_AREAS                                 6
#define ANS_REC                                   7
#define ANS_OBJECT                                8

#define CMD_QUIT                                  1
#define CMD_GO                                    2
#define CMD_STEP                                  3
#define CMD_TRACE                                 4
#define CMD_NEXTR                                 5
#define CMD_TOCURS                                6
#define CMD_EXIT                                  7
#define CMD_STACK                                 8
#define CMD_EXP                                   9
#define CMD_LOCAL                                 10
#define CMD_STATIC                                11
#define CMD_PRIV                                  12
#define CMD_PUBL                                  13
#define CMD_WATCH                                 14
#define CMD_AREA                                  15
#define CMD_REC                                   16
#define CMD_OBJECT                                17
#define CMD_TERMINATE                             18

#define BUFF_LEN                                  1024

#define cMsgNotSupp                               "Command isn't supported"


CLASS clsDebugger INHERIT IdeObject

   DATA   oIde
   
   DATA   nRowWatch
   
   DATA   nRowAreas                               INIT 0

   DATA   aSources
   DATA   oOutputResult

   DATA   lDebugging                              INIT .F.
   DATA   handl1                                  INIT -1
   DATA   handl2
   DATA   cBuffer
   DATA   nId1                                    INIT 0
   DATA   nId2                                    INIT -1

   DATA   cAppName
   DATA   cPrgName                                INIT ""

   DATA   cInspectVar                             INIT ""

   DATA   aBP                                     INIT {}
   DATA   aWatches                                INIT {}
   DATA   nCurrLine                               INIT 0
   DATA   nMode                                   INIT MODE_INPUT
   DATA   nAnsType
   DATA   cPrgBP
   DATA   nLineBP
   DATA   aBPLoad                                 INIT {}
   DATA   nBPLoad

   DATA   nExitMode                               INIT 2
   DATA   nVerProto                               INIT 0

   DATA   qTimer
   DATA   aTabs

   DATA   oUI
   
   DATA   cCurrentProject

   METHOD init( oIde )
   METHOD start( cExe )
   METHOD loadBreakPoints()
   METHOD clearBreakPoints( cPrg )
   METHOD deleteBreakPoint( cPrg, nLine )
   METHOD toggleBreakPoint( cAns, cLine )
   METHOD addBreakPoint( cPrg, nLine )
   METHOD timerProc()
   METHOD dbgRead()
   METHOD send( ... )
   METHOD setMode( newMode )
   METHOD setCurrLine( nLine, cName )
   METHOD getCurrLine()
   METHOD getCurrPrgName()
   METHOD getBP( nLine, cPrg )
   METHOD doCommand( nCmd, cDop, cDop2 )
   METHOD setWindow( cPrgName )
   METHOD stopDebug()

   METHOD inspectObject( lClicked )

   METHOD showStack( arr, n )
   METHOD showVars( arr, n, nVarType )
   METHOD showWatch( arr, n )
   METHOD showAreas( arr, n )
   METHOD requestRecord( row, col )
   METHOD showRec( arr, n )
   METHOD showObject( arr, n )

   METHOD wait4connection( cStr )

   METHOD ui_init()
   METHOD ui_load()
   METHOD ui_tableWatch_ins()
   METHOD ui_tableWatch_del()

   METHOD changeWatch( item )
   
   METHOD terminateDebug()
   METHOD exitDbg()

   ENDCLASS


METHOD clsDebugger:init( oIde )
   ::oIde      := oIde
   ::aBP       := {}
   ::aWatches  := {}
   ::nCurrLine := 0
   ::nId1      := 0
   ::nId2      := -1

   ::setMode( MODE_INIT )

   ::cBuffer         := Space( BUFF_LEN )
   ::aTabs           := ::oIde:aTabs
   ::oOutputResult   := ::oIde:oOutputResult

   WITH OBJECT ::qTimer := QTimer()
      :setInterval( 30 )
      :connect( "timeout()",  {|| ::timerProc() } )
      :start()
   ENDWITH

   ::oUI = hbqtui_debugger()
   ::ui_init()

   RETURN Self


METHOD clsDebugger:start( cExe )
   LOCAL cPath, cFile, cExt

   hb_fNameSplit( cExe, @cPath, @cFile, @cExt )

   FErase( cExe + ".d1" )
   FErase( cExe + ".d2" )

   ::handl1 := FCreate( cExe + ".d1" )
   FWrite( ::handl1, "init,!" )
   FClose( ::handl1 )
   ::handl2 := FCreate( cExe + ".d2" )
   FClose( ::handl2 )

   hb_processOpen( cExe )                         //+ Iif( !Empty( cParams ), cParams, "" ) )

   ::handl1 := FOpen( cExe + ".d1", FO_READWRITE + FO_SHARED )
   ::handl2 := FOpen( cExe + ".d2", FO_READ + FO_SHARED )
   IF ::handl1 != -1 .AND. ::handl2 != -1
      ::cAppName := Lower( cFile + cExt )
   ELSE
      ::handl1 := ::handl2 := -1
      hbide_showWarning( "No connection" )
      RETURN .F.
   ENDIF

   IF ::wait4connection("ver")
      ::oOutputResult:oWidget:append( "Connected Ok!" )
   ELSE
      ::oOutputResult:oWidget:append( "Not connected! Debug terminated." )
      RETURN .F.
   ENDIF

   ::timerProc()

   ::loadBreakPoints()

   DO WHILE ! Empty( ::aBPLoad )
      hb_idleSleep( 0.1 )
   ENDDO

   ::lDebugging := .T.

   ::oOutputResult:oWidget:append( "Debug started." )
   
   ::doCommand( CMD_GO )
   RETURN .T.


METHOD clsDebugger:LoadBreakPoints()
   LOCAL i, j, oEditor, cBP, pos

   ::oOutputResult:oWidget:append( "Loading breakpoints..." )
   FOR j := 1 TO Len( ::aSources )
      FOR i := 1 TO Len( ::aTabs )
         oEditor := ::aTabs[ i, TAB_OEDITOR ]
         IF oEditor:oTab:caption == ::aSources[ j ]
            cBP := oEditor:qCoEdit:qEdit:getBreakPointsVector()
            IF ! Empty( cBP )
               DO WHILE .T.
                  pos := At( ",", cBP )
                  IF pos = 0
                     AAdd( ::aBPLoad, { Val( cBP ), oEditor:oTab:caption } )
                     EXIT
                  ELSE
                     Aadd( ::aBPLoad, { Val( SubStr( cBP, 1, pos - 1 ) ), oEditor:oTab:caption } )
                     cBP := SubStr( cBP, pos + 1, Len( cBP ) - pos )
                  ENDIF
               ENDDO
            ENDIF
         ENDIF
      NEXT i
   NEXT j

   IF ! Empty( ::aBPLoad )
      ::nBPLoad := 1
      ::addBreakPoint( ::aBPLoad[ 1,2 ], ::aBPLoad[ 1,1 ] )
   ENDIF

   WHILE ! Empty( ::aBPLoad )
      ::timerProc()                               //wait for load
   ENDDO
   RETURN .T.


METHOD clsDebugger:clearBreakPoints( cPrg )
   LOCAL n

   IF PCount() == 0
      cPrg := ""
   ENDIF

   FOR n := 1 TO Len( ::aBP )
      IF ( Empty( cPrg ) .OR. cPrg == ::aBP[ n,2 ] ) .AND. ::aBP[ n,1 ] <> 0
         ::deleteBreakPoint( ::aBP[ n,2 ], ::aBP[ n,1 ] )
         ::wait4connection("ok")
      ENDIF
   NEXT
   RETURN .T.


METHOD clsDebugger:deleteBreakPoint( cPrg, nLine )
      ::oOutputResult:oWidget:append( "Deleting break point: " + cPrg + ": " + Str( nLine ) )
      ::send( "brp", "del", cPrg, LTrim( Str( nLine ) ) )
      IF ::nMode != MODE_WAIT_ANS
         ::nAnsType := ANS_BRP
         ::cPrgBP   := cPrg
         ::nLineBP  := nLine
         ::setMode( MODE_WAIT_ANS )
      ENDIF
   RETURN .T.


METHOD clsDebugger:toggleBreakPoint( cAns, cLine )
   LOCAL nLine := Val( cLine ), i

   IF cAns == "line"
      FOR i := 1 TO Len( ::aBP )
         IF ::aBP[ i,1 ] == 0
            ::aBP[ i,1 ] := nLine
            ::aBP[ i,2 ] := ::cPrgBP
            EXIT
         ENDIF
      NEXT
      IF i > Len( ::aBP )
         Aadd( ::aBP, { nLine, ::cPrgBP } )
      ENDIF
   ELSE
      IF ( i := ::getBP( nLine, ::cPrgBP ) ) == 0
         hbide_showWarning( "Error deleting BP line " + cLine )
         ::oUI:activateWindow()
      ELSE
         ::aBP[ i,1 ] := 0
      ENDIF
   ENDIF
   RETURN NIL


METHOD clsDebugger:addBreakPoint( cPrg, nLine )

   IF ::nMode != MODE_INPUT .AND. Empty( ::aBPLoad )
      RETURN NIL
   ENDIF

   IF .T.
      IF ::getBP( nLine, cPrg ) == 0
         ::oOutputResult:oWidget:append( "Setting break point: " + cPrg + ": " + Str( nLine ) )
         ::send( "brp", "add", cPrg, LTrim( Str( nLine ) ) )
      ELSE
         ::oOutputResult:oWidget:append( "Deleting break point: " + cPrg + ": " + Str( nLine ) )
         ::send( "brp", "del", cPrg, LTrim( Str( nLine ) ) )
      ENDIF
      IF ::nMode != MODE_WAIT_ANS
         ::nAnsType := ANS_BRP
         ::cPrgBP   := cPrg
         ::nLineBP  := nLine
         ::setMode( MODE_WAIT_ANS )
      ENDIF
   ENDIF
   RETURN NIL


METHOD clsDebugger:timerProc()
   LOCAL n, arr
   STATIC nLastSec := 0

   IF ::nMode != MODE_INPUT
      IF ! Empty( arr := ::dbgRead() )
         IF arr[ 1 ] == "quit"
            ::setMode( MODE_INIT )
            ::stopDebug()
            RETURN NIL
         ENDIF
         IF ::nMode == MODE_WAIT_ANS
            IF Left( arr[ 1 ], 1 ) == "b" .AND. ( n := Val( SubStr( arr[ 1 ], 2 ) ) ) == ::nId1
               IF ::nAnsType == ANS_CALC
                  IF arr[ 2 ] == "value"
                     IF ! Empty( ::cInspectVar )
                        IF Substr( Hex2Str( arr[ 3 ] ), 2, 1 ) == "O"
                           ::nMode := MODE_INPUT
                           ::inspectObject(.F.)
//                           ::cInspectVar := NIL
                           RETURN NIL
                        ELSE
                           ::oUI:tableObjectInspector:setRowCount( 0 )
                           hbide_showWarning( ::cInspectVar + " isn't an object" )
                           ::oUI:activateWindow()
                        ENDIF
                     ELSE
                        //???  ::SetResult( Hex2Str( arr[3] ) )
                     ENDIF
                  ELSE
                     ::oOutputResult:oWidget:append( "-- BAD ANSWER --" )
                  ENDIF
               ELSEIF ::nAnsType == ANS_BRP
                  IF arr[ 2 ] == "err"
                     ::oOutputResult:oWidget:append( "-- BAD LINE --" )
                     ::toggleBreakPoint( "line", Str( ::nLineBP ) )
                  ELSE
                     ::oOutputResult:oWidget:append( "Ok" )
                     ::toggleBreakPoint( arr[ 2 ], arr[ 3 ] )
                  ENDIF

                  IF ! Empty( ::aBPLoad )
                     IF ++::nBPLoad <= Len( ::aBPLoad )
                        ::addBreakPoint( ::aBPLoad[ ::nBPLoad,2 ], ::aBPLoad[ ::nBPLoad,1 ] )
                        RETURN NIL
                     ELSE
                        ::aBPLoad := {}
                        ::oOutputResult:oWidget:append( "Breakpoints loaded." )
                     ENDIF
                  ENDIF
               ELSEIF ::nAnsType == ANS_STACK
                  IF arr[ 2 ] == "stack"
                     ::showStack( arr, 3 )
                  ENDIF
               ELSEIF ::nAnsType == ANS_LOCAL
                  IF arr[ 2 ] == "valuelocal"
                     ::showVars( arr, 3, 1 )
                  ELSEIF arr[ 2 ] == "valuepriv"
                     ::showVars( arr, 3, 2 )
                  ELSEIF arr[ 2 ] == "valuepubl"
                     ::showVars( arr, 3, 3 )
                  ELSEIF arr[ 2 ] == "valuestatic"
                     ::showVars( arr, 3, 4 )
                  ENDIF
               ELSEIF ::nAnsType == ANS_WATCH
                  IF arr[ 2 ] == "valuewatch"
                     ::showWatch( arr, 3 )
                  ENDIF
               ELSEIF ::nAnsType == ANS_AREAS
                  IF arr[ 2 ] == "valueareas"
                     ::showAreas( arr, 3 )
                  ENDIF
               ELSEIF ::nAnsType == ANS_REC
                  IF arr[ 2 ] == "valuerec"
                     ::showRec( arr, 3 )
                  ENDIF
               ELSEIF ::nAnsType == ANS_OBJECT
                  IF arr[ 2 ] == "valueobj"
                     ::showObject( arr, 3 )
                  ENDIF
               ENDIF
               ::setMode( MODE_INPUT )
            ENDIF
         ELSE
            IF Left( arr[ 1 ], 1 ) == "a" .AND. ( n := Val( SubStr( arr[ 1 ], 2 ) ) ) > ::nId2
               ::nId2 := n
               IF arr[2] == "."
                  ::oOutputResult:oWidget:append( "-- BAD LINE --" )
               ELSE
                  IF ! ( ::cPrgName == arr[ 2 ] )
                     ::cPrgName := arr[ 2 ]
                  ENDIF
                  ::oUi:labelStatus:setText("Stoped")
                  ::setCurrLine( ::nCurrLine := Val( arr[ 3 ] ), ::cPrgName )
                  n := 4
                  DO WHILE .T.
                     IF arr[ n ] == "ver"
                        ::nVerProto := Val( arr[ n+1 ] )
                        n += 2
                     ELSEIF arr[ n ] == "stack"
                        ::showStack( arr, n+1 )
                        n += 2 + Val( arr[ n+1 ] ) * 3
                     ELSEIF arr[ n ] == "valuelocal"
                        ::showVars( arr, n+1, 1 )
                        n += 2 + Val( arr[ n+1 ] ) * 3
                     ELSEIF arr[ n ] == "valuepriv"
                        ::showVars( arr, n+1, 2 )
                        n += 2 + Val( arr[ n+1 ] ) * 3
                     ELSEIF arr[ n ] == "valuepubl"
                        ::showVars( arr, n+1, 3 )
                        n += 2 + Val( arr[ n+1 ] ) * 3
                     ELSEIF arr[ n ] == "valuestatic"
                        ::showVars( arr, n+1, 4 )
                        n += 2 + Val( arr[ n+1 ] ) * 3
                     ELSEIF arr[ n ] == "valuewatch"
                        ::showWatch( arr, n+1 )
                        n += 2 + Val( arr[ n+1 ] )
                     ELSE
                        EXIT
                     ENDIF
                  ENDDO
                  ::oOutputResult:oWidget:append( "Debugger (" + arr[ 2 ] + ", line " + arr[ 3 ] + ")" )

                  ::ui_load()
                  ::oUI:show()
                  ::oUI:activateWindow()
               ENDIF
               ::setMode( MODE_INPUT )
               nLastSec := Seconds()

            ENDIF
         ENDIF
      ENDIF

   ENDIF
   RETURN NIL


METHOD clsDebugger:dbgRead()
   LOCAL n, s, arr

   FSeek( ::handl2, 0, 0 )
   s := ""
   DO WHILE ( n := Fread( ::handl2, @::cBuffer, Len( ::cBuffer ) ) ) > 0
      s += Left( ::cBuffer, n )
      IF ( n := At( ",!", s ) ) > 0
         IF ( arr := hb_aTokens( Left( s, n + 1 ), "," ) ) != NIL .AND. Len( arr ) > 2 .AND. arr[ 1 ] == arr[ Len( arr ) - 1 ]
            RETURN arr
         ELSE
            EXIT
         ENDIF
      ENDIF
   ENDDO
   RETURN NIL


METHOD clsDebugger:Send( ... )
   LOCAL i
   LOCAL arr := hb_aParams()
   LOCAL s := ""

   FSeek( ::handl1, 0, 0 )
   FOR i := 1 TO Len( arr )
      s += arr[ i ] + ","
   NEXT
   FWrite( ::handl1, LTrim( Str( ++::nId1 ) ) + "," + s + LTrim( Str( ::nId1 ) ) + ",!" )
   RETURN NIL


METHOD clsDebugger:SetMode( newMode )

   ::nMode := newMode
   IF newMode == MODE_INPUT
   ELSE
      IF newMode == MODE_WAIT_ANS .OR. newMode == MODE_WAIT_BR
         IF newMode == MODE_WAIT_BR
            ::nCurrLine := 0
         ENDIF
      ELSEIF newMode == MODE_INIT
         ::lDebugging := .F.
      ENDIF
   ENDIF
   RETURN NIL


METHOD clsDebugger:getBP( nLine, cPrg )
   cPrg := Lower( iif( cPrg == NIL, ::cPrgName, cPrg ) )
   RETURN Ascan( ::aBP, {|a| a[ 1 ] == nLine .and. Lower( a[ 2 ] ) == cPrg } )


METHOD clsDebugger:DoCommand( nCmd, cDop, cDop2 )
   IF ::nMode == MODE_INPUT
      IF nCmd == CMD_GO
         ::oUi:labelStatus:setText("Program executing")
         ::send( "cmd", "go" )

      ELSEIF nCmd == CMD_STEP
         ::oUi:labelStatus:setText("Program executing")
         ::send( "cmd", "step" )

      ELSEIF nCmd == CMD_TOCURS
         ::oUi:labelStatus:setText("Program executing")
         ::send( "cmd", "to", ::getCurrPrgName(), Ltrim( Str( ::getCurrLine() ) ) )

      ELSEIF nCmd == CMD_TRACE
         ::send( "cmd", "trace" )

      ELSEIF nCmd == CMD_NEXTR
         ::send( "cmd", "nextr" )

      ELSEIF nCmd == CMD_EXP
         ::send( "exp", cDop )
         ::nAnsType := ANS_CALC
         ::setMode( MODE_WAIT_ANS )
         RETURN NIL

      ELSEIF nCmd == CMD_STACK
         ::send( "view", "stack", cDop )
         ::nAnsType := ANS_STACK
         ::setMode( MODE_WAIT_ANS )
         RETURN NIL

      ELSEIF nCmd == CMD_LOCAL
         ::send( "view", "local", cDop )
         ::nAnsType := ANS_LOCAL
         ::setMode( MODE_WAIT_ANS )
         RETURN NIL

      ELSEIF nCmd == CMD_PRIV
         IF ::nVerProto > 1
            ::send( "view", "priv", cDop )
            ::nAnsType := ANS_LOCAL
            ::setMode( MODE_WAIT_ANS )
         ELSE
            hbide_showWarning( cMsgNotSupp )
            ::oUI:activateWindow()
         ENDIF
         RETURN NIL

      ELSEIF nCmd == CMD_PUBL
         IF ::nVerProto > 1
            ::send( "view", "publ", cDop )
            ::nAnsType := ANS_LOCAL
            ::setMode( MODE_WAIT_ANS )
         ELSE
            hbide_showWarning( cMsgNotSupp )
            ::oUI:activateWindow()
         ENDIF
         RETURN NIL

      ELSEIF nCmd == CMD_STATIC
         IF ::nVerProto > 1
            ::send( "view", "static", cDop )
            ::nAnsType := ANS_LOCAL
            ::setMode( MODE_WAIT_ANS )
         ELSE
            hbide_showWarning( cMsgNotSupp )
            ::oUI:activateWindow()
         ENDIF
         RETURN NIL

      ELSEIF nCmd == CMD_WATCH
         IF Empty( cDop2 )
            ::send( "view", "watch", cDop )
         ELSE
            ::send( "watch", cDop, cDop2 )
         ENDIF
         ::nAnsType := ANS_WATCH
         ::setMode( MODE_WAIT_ANS )
         RETURN NIL

      ELSEIF nCmd == CMD_AREA
         ::send( "view", "areas" )
         ::nAnsType := ANS_AREAS
         ::setMode( MODE_WAIT_ANS )
         RETURN NIL

      ELSEIF nCmd == CMD_REC
         IF ::nVerProto > 1
            ::send( "insp", "rec", cDop )
            ::nAnsType := ANS_REC
            ::setMode( MODE_WAIT_ANS )
         ELSE
            hbide_showWarning( cMsgNotSupp )
            ::oUI:activateWindow()
         ENDIF
         RETURN NIL

      ELSEIF nCmd == CMD_OBJECT
         IF ::nVerProto > 1
            ::send( "insp", "obj", cDop )
            ::nAnsType := ANS_OBJECT
            ::setMode( MODE_WAIT_ANS )
         ELSE
            hbide_showWarning( cMsgNotSupp )
            ::oUI:activateWindow()
         ENDIF
         RETURN NIL

      ELSEIF nCmd == CMD_QUIT
         ::nExitMode := 2
         RETURN NIL

      ELSEIF nCmd == CMD_EXIT
         ::nExitMode := 1
         RETURN NIL

      ELSEIF nCmd == CMD_TERMINATE
         ::send( "cmd", "quit" )
         ::lDebugging := .F.
         ::stopDebug()

      ENDIF

      ::setMode( MODE_WAIT_BR )

   ELSEIF nCmd == CMD_EXIT
      ::nExitMode := 1

   ENDIF
   RETURN NIL


METHOD clsDebugger:setCurrLine( nLine, cName )
   LOCAL qCursor

   IF ! ::lDebugging
      ::lDebugging := .T.
   ENDIF

   ::setWindow( cName )
   qCursor := ::oIde:qCurEdit:textCursor()

   qCursor:movePosition( QTextCursor_Down, QTextCursor_MoveAnchor, nLine - 1 )
   ::oIde:qCurEdit:setTextCursor( qCursor )
   ::oIde:manageFocusInEditor()

   ::oUI:activateWindow()
   
   RETURN NIL


METHOD clsDebugger:getCurrLine()
   LOCAL qCursor
   qCursor := ::oIde:qCurEdit:textCursor()
   RETURN qCursor:blockNumber() + 1


METHOD clsDebugger:getCurrPrgName()
   LOCAL oEditor
   oEditor := ::oIde:oCurEditor
   RETURN (oEditor:cFile + oEditor:cExt)


METHOD clsDebugger:setWindow( cPrgName )
   LOCAL qCursor
   LOCAL i, oEditor

   FOR i := 1 TO Len(::aTabs)
      oEditor := ::aTabs[ i, TAB_OEDITOR ]
      IF (oEditor:cFile + oEditor:cExt) == cPrgName
         cPrgName := oEditor:cPath + oEditor:cFile + oEditor:cExt
         EXIT
      ENDIF
   NEXT i
   ::oIde:oSM:editSource( cPrgName, 0, 0, 0, NIL, NIL, .f., .t. )
   qCursor := ::oIde:qCurEdit:textCursor()
   qCursor:setPosition( 0 )
   ::oIde:qCurEdit:setTextCursor( qCursor )
   RETURN .T.


METHOD clsDebugger:stopDebug()

   IF ::handl1 != -1
      FClose( ::handl1 )
      FClose( ::handl2 )
      ::handl1 := -1
   ENDIF
   
   ::oUi:labelStatus:setText("Exited")
   
   RETURN NIL


METHOD clsDebugger:inspectObject( lClicked )
   LOCAL index, oTable, nRow, cObjName
   LOCAL cType := ""
   IF lClicked
      index := ::oUI:tabWidget:currentIndex()
      DO CASE
      CASE index = 0
         oTable := ::oUI:tableVarLocal
      CASE index = 1
         oTable := ::oUI:tableVarPrivate
      CASE index = 2
         oTable := ::oUI:tableVarPublic
      CASE index = 3
         oTable := ::oUI:tableVarStatic
      ENDCASE
   
      IF oTable:rowCount() == 0
         RETURN NIL
      ENDIF

      nRow := oTable:currentRow()
      IF nRow >= 0
         cType := oTable:item(nRow, 1):text()
      ENDIF
      IF cType != "O"//! ( cType $ "OA")
         hbide_showWarning( "Select object in table, please." )
         ::oUI:activateWindow()
         RETURN NIL
      ENDIF
   
      cObjName := oTable:item(nRow, 0):text()
      ::cInspectVar := cObjName
   ENDIF
   
   ::setMode( MODE_INPUT )
   ::doCommand( CMD_OBJECT, ::cInspectVar )
   ::wait4connection( "valueobj" )
   ::timerProc()
   RETURN NIL


METHOD clsDebugger:showStack( arr, n )
   LOCAL i
   LOCAL nLen := Val( arr[ n ] )

   ::oUI:tableStack:setRowCount( nLen )
   FOR i := 1 TO nLen
      ::oUI:tableStack:setItem( i - 1, 0, QTableWidgetItem( arr[ ++n ] ) )
      ::oUI:tableStack:setItem( i - 1, 1, QTableWidgetItem( arr[ ++n ] ) )
      ::oUI:tableStack:setItem( i - 1, 2, QTableWidgetItem( arr[ ++n ] ) )
   NEXT
   RETURN NIL


METHOD clsDebugger:showVars( arr, n, nVarType )
   LOCAL nLen := Val( arr[n] )
   LOCAL i, oTable
   //::oUI:tabWidget:setCurrentIndex(nVarType)

   DO CASE
   CASE nVarType = 1
      oTable := ::oUI:tableVarLocal
   CASE nVarType = 2
      oTable := ::oUI:tableVarPrivate
   CASE nVarType = 3
      oTable := ::oUI:tableVarPublic
   CASE nVarType = 4
      oTable := ::oUI:tableVarStatic
   ENDCASE

   oTable:setRowCount( nLen )
   FOR i := 1 TO nLen
      oTable:setItem( i-1, 0, QTableWidgetItem( Hex2Str( arr[ ++n ] ) ) )
      oTable:setItem( i-1, 1, QTableWidgetItem( Hex2Str( arr[ ++n ] ) ) )
      oTable:setItem( i-1, 2, QTableWidgetItem( Hex2Str( arr[ ++n ] ) ) )
   NEXT
   RETURN NIL


METHOD clsDebugger:showWatch( arr, n )
   LOCAL i, nLen := Val( arr[ n ] )

   IF nLen == 1
      ::oUI:tableWatchExpressions:setItem( ::nRowWatch, 1, QTableWidgetItem(Hex2Str( arr[ ++n ] ) ) )
   ELSE
      FOR i := 1 TO nLen
         ::oUI:tableWatchExpressions:setItem( i - 1, 1, QTableWidgetItem( Hex2Str( arr[ ++n ] ) ) )
      NEXT
   ENDIF
   RETURN NIL


METHOD clsDebugger:showAreas( arr, n )
   LOCAL i, j, cAlias, item
   LOCAL nAreas := Val( arr[n] )
   LOCAL nAItems := Val( Hex2Str(arr[++n]) )

   ::oUI:tableOpenTables:setRowCount( nAreas )
   FOR i := 1 TO nAreas
      FOR j := 1 TO nAItems
         item := QTableWidgetItem( Hex2Str( arr[ ++n ] ) )
         IF i == 1 .AND. j == 1
            IF Left(item:text(),1) == "*"
               cAlias := Right(item:text(), Len(item:text()) - 1)
            ELSE
               cAlias = item:text()
            ENDIF
         ENDIF
         ::oUI:tableOpenTables:setItem( i - 1, j - 1, item )
      NEXT
   NEXT
   
   IF nAreas == 0
      ::oUI:tableCurrentRecord:setRowCount( 0 )
   ELSE
      ::oUI:tableOpenTables:setCurrentCell( 0, 0 )
      ::setMode( MODE_INPUT )
      ::doCommand(CMD_REC,cAlias)
      ::wait4connection( "valuerec" )
      ::timerProc()
   ENDIF

   RETURN NIL


METHOD clsDebugger:ShowRec( arr, n )
   LOCAL i, j
   LOCAL nFields := Val( arr[n] )

//      hwg_Setwindowtext( oInspectDlg:handle, "Record inspector ("+Hex2Str(arr[++n])+", rec."+Hex2Str(arr[++n])+")" )
//?Hex2Str(arr[++n])
//?Hex2Str(arr[++n])
   n := n + 2
   ::oUI:tableCurrentRecord:setRowCount( nFields )
   FOR i := 1 TO nFields
      FOR j := 1 TO 4
         ::oUI:tableCurrentRecord:setItem( i - 1, j - 1, QTableWidgetItem( Hex2Str( arr[ ++n ] ) ) )
      NEXT
   NEXT
   RETURN NIL


METHOD clsDebugger:showObject( arr, n )
   LOCAL i, j
   LOCAL nLen := Val( arr[n] )

   ::oUI:tableObjectInspector:setRowCount( nLen )

   IF nLen == 0
      ::cInspectVar := ""
      RETURN NIL
   ENDIF
   
   FOR i := 1 TO nLen
      FOR j := 1 TO 3
         ::oUI:tableObjectInspector:setItem( i - 1, j - 1, QTableWidgetItem( Hex2Str( arr[ ++n ] ) ) )
      NEXT
   NEXT

   RETURN NIL


STATIC FUNCTION Int2Hex( n )
   LOCAL n1 := Int( n/16 ), n2 := n % 16

   IF n > 255
      RETURN "XX"
   ENDIF
   RETURN Chr( iif( n1 < 10, n1 + 48, n1 + 55 ) ) + Chr( iif( n2 < 10, n2 + 48, n2 + 55 ) )


STATIC FUNCTION Str2Hex( stroka )
   LOCAL i
   LOCAL cRes := ""
   LOCAL nLen := Len( stroka )

   FOR i := 1 to nLen
      cRes += Int2Hex( Asc( Substr( stroka, i, 1 ) ) )
   NEXT
   RETURN cRes


STATIC FUNCTION Hex2Str( stroka )
   LOCAL cRes := "", i := 1, nLen := Len( stroka )

   DO WHILE i <= nLen
      cRes += Chr( Hex2Int( Substr( stroka, i, 2 ) ) )
      i += 2
   ENDDO
   RETURN cRes


STATIC FUNCTION Hex2Int( stroka )
   LOCAL i := Asc( stroka ), res

   IF i > 64 .AND. i < 71
      res := ( i - 55 ) * 16
   ELSEIF i > 47 .AND. i < 58
      res := ( i - 48 ) * 16
   ELSE
      RETURN 0
   ENDIF

   i := Asc( SubStr( stroka, 2, 1 ) )
   IF i > 64 .AND. i < 71
      res += i - 55
   ELSEIF i > 47 .AND. i < 58
      res += i - 48
   ENDIF
   RETURN res


METHOD clsDebugger:wait4connection( cStr )
   LOCAL n, i
   LOCAL nSec := Seconds()

   ::qTimer:stop()
   DO WHILE .T.
      FSeek( ::handl2, 0, 0 )
      n := Fread( ::handl2, @::cBuffer, Len( ::cBuffer ) )
      IF n > 0
         IF cStr == "ok"
            IF At( "err", ::cBuffer ) > 0 .OR. At( "ok", ::cBuffer ) > 0
               EXIT
            ENDIF
         ELSE
            IF At( cStr, ::cBuffer ) > 0
               EXIT
            ENDIF
         ENDIF
      ENDIF

      IF Seconds() - nSec > 5
         ::oOutputResult:oWidget:append( "Waited for " + cStr + ". Not answer. May be a bad query." )
//         hbide_showWarning( "Waited for " + cStr + ". Not answer. May be a bad query." )
         ::oUI:activateWindow()
         ::qTimer:start()
         RETURN .F.
      ENDIF
//    hb_idleSleep(0.1)
      FOR i := 1 TO 50000                         //hb_idleSleep not works //todo sleep()
         // empty loop
      NEXT
   ENDDO
   ::qTimer:start()
   RETURN .T.


METHOD clsDebugger:ui_init()
   LOCAL oHeaders

   WITH OBJECT oHeaders := QStringList()
      :append( "Expression" )
      :append( "Value" )
   ENDWITH
   ::oUI:tableWatchExpressions:setHorizontalHeaderLabels( oHeaders )

   WITH OBJECT oHeaders := QStringList()
      :append( "Prg" )
      :append( "Proc" )
      :append( "Line" )
   ENDWITH
   ::oUI:tableStack:setHorizontalHeaderLabels( oHeaders )

   WITH OBJECT oHeaders := QStringList()
      :append( "Name" )
      :append( "Type" )
      :append( "Value" )
   ENDWITH
   ::oUI:tableVarLocal:setHorizontalHeaderLabels( oHeaders )
   ::oUI:tableVarPrivate:setHorizontalHeaderLabels( oHeaders )
   ::oUI:tableVarPublic:setHorizontalHeaderLabels( oHeaders )
   ::oUI:tableVarStatic:setHorizontalHeaderLabels( oHeaders )

   WITH OBJECT oHeaders := QStringList()
      :append( "Alias" )
      :append( "Area" )
      :append( "RDD" )
      :append( "Records" )
      :append( "Current" )
      :append( "BOF" )
      :append( "EOF" )
      :append( "FOUND" )
      :append( "DEL" )
   ENDWITH
   ::oUI:tableOpenTables:setHorizontalHeaderLabels( oHeaders )
   
   WITH OBJECT oHeaders := QStringList()
      :append( "FieldName" )
      :append( "Type" )
      :append( "Len" )
      :append( "Value" )
   ENDWITH
   ::oUI:tableCurrentRecord:setHorizontalHeaderLabels( oHeaders )

   WITH OBJECT oHeaders := QStringList()
      :append( "Name" )
      :append( "Type" )
      :append( "Value" )
   ENDWITH
   ::oUI:tableObjectInspector:setHorizontalHeaderLabels( oHeaders )

   ::oUI:btnGo:connect( "clicked()", { || ::doCommand( CMD_GO ) } )
   ::oUI:btnStep:connect( "clicked()", { || ::doCommand( CMD_STEP ) } )
   ::oUI:btnToCursor:connect( "clicked()", { || ::doCommand( CMD_TOCURS ) } )
   ::oUI:btnExit:connect( "clicked()", { || ::exitDbg() } )

   ::oUI:btnAddExpression:connect( "clicked()", { || ::ui_tableWatch_ins() } )
   ::oUI:btnDeleteExpression:connect( "clicked()", { || ::ui_tableWatch_del() } )
   ::oUI:tableWatchExpressions:connect( "itemChanged(QTableWidgetItem*)", { | item | ::changeWatch( item ) } )
   ::oUI:tableOpenTables:connect( "cellActivated(int,int)", { | row, col | ::requestRecord( row, col ) } )   
   ::oUI:btnInspect:connect( "clicked()", { || ::inspectObject(.T.) } )
   ::oUI:connect( QEvent_Close   , {|| ::exitDbg() } )
   RETURN NIL


METHOD clsDebugger:ui_load()
   ::setMode( MODE_INPUT )
   ::doCommand( CMD_STACK, "on" )
   ::wait4connection( "stack" )
   ::timerProc()
   ::setMode( MODE_INPUT )
   ::doCommand( CMD_LOCAL, "on" )
   ::wait4connection( "valuelocal" )
   ::timerProc()
   ::setMode( MODE_INPUT )
   ::doCommand( CMD_PRIV, "on" )
   ::wait4connection( "valuepriv" )
   ::timerProc()
   ::setMode( MODE_INPUT )
   ::doCommand( CMD_PUBL, "on" )
   ::wait4connection( "valuepubl" )
   ::timerProc()
   ::setMode( MODE_INPUT )
   ::doCommand( CMD_STATIC, "on" )
   ::wait4connection( "valuestatic" )
   ::timerProc()

   ::setMode( MODE_INPUT )
   ::doCommand( CMD_AREA )
   ::wait4connection( "valueareas" )
   ::timerProc()
   
   IF ! Empty( ::cInspectVar )
      ::inspectObject(.F.)
   ENDIF
   
   ::doCommand( CMD_WATCH, "on")
   RETURN NIL


METHOD clsDebugger:ui_tableWatch_ins()
   ::oUI:tableWatchExpressions:insertRow( ::oUI:tableWatchExpressions:rowCount() )
   AAdd( ::aWatches, { ::oUI:tableWatchExpressions:rowCount() - 1, "" } )
   RETURN NIL


METHOD clsDebugger:ui_tableWatch_del()
   LOCAL i
   LOCAL r := ::oUI:tableWatchExpressions:currentRow()
   local ri := 0

   FOR i := 1 TO Len( ::aWatches )
      IF ::aWatches[i,1] == r
         ri := i
         ::setMode( MODE_INPUT )
         ::doCommand( CMD_WATCH, "del", LTrim( Str( i ) ) )
      ENDIF
      IF ::aWatches[ i,1 ] > r
         --::aWatches[ i,1 ]
      ENDIF
   NEXT
   IF ri > 0
      hb_ADel( ::aWatches, ri, .T. )
   ENDIF
   ::oUI:tableWatchExpressions:removeRow( r )
   RETURN NIL


METHOD clsDebugger:changeWatch( item )
   LOCAL i
   LOCAL rc := ::oUI:tableWatchExpressions:rowCount()
   LOCAL r := 0

   IF item:column() == 0
      FOR i := 1 TO Len( ::aWatches )
         IF ::aWatches[ i,1 ] == item:row()
            IF Empty( ::aWatches[ i,2 ] )
               ::aWatches[ i,2 ] := item:text()
               ::nRowWatch := item:Row()
            ELSE
               ::setMode( MODE_INPUT )
               ::doCommand( CMD_WATCH, "del", Ltrim( Str( i ) ) )
               ::wait4connection( "ok" )
               r := i
               ::nRowWatch := rc - 1
            ENDIF
        ENDIF
      NEXT
      IF r > 0
         AAdd( ::aWatches, { rc - 1, item:text() } )
         hb_ADel( ::aWatches, r, .T. )
      ENDIF
      ::setMode( MODE_INPUT )
      ::doCommand( CMD_WATCH, "add", Str2Hex( item:text() ) )
   ENDIF
   RETURN NIL


METHOD clsDebugger:terminateDebug()
   ::SetMode( MODE_INPUT )
   ::doCommand( CMD_TERMINATE )
   RETURN NIL


METHOD clsDebugger:requestRecord( row, col )
   LOCAL item, cAlias
   HB_SYMBOL_UNUSED( col )
   IF row == ::nRowAreas
      RETURN NIL
   ELSE 
      ::nRowAreas := row
   ENDIF
   item := ::oUI:tableOpenTables:item(row, 0)
   IF Left(item:text(),1) == "*"
      cAlias := Right(item:text(), Len(item:text()) - 1)
   ELSE
      cAlias = item:text()
   ENDIF

   ::setMode( MODE_INPUT )
   ::doCommand(CMD_REC,cAlias)
   ::wait4connection( "valuerec" )
   ::timerProc()
   RETURN NIL

  
METHOD clsDebugger:exitDbg()

   IF ::nExitMode == 1
      IF ::handl1 != -1
         ::Send( "cmd", "exit" )
      ENDIF
   ELSEIF ::nExitMode == 2
      ::Send( "cmd", "quit" )
   ENDIF
   ::lDebugging := .F.
   ::StopDebug()

   ::oIde:oDebugger = NIL

   RETURN .T.
