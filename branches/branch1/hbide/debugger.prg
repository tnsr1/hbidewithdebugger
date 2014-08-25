/*
 * $Id: hwgdebug.prg 2159 2013-07-19 08:36:53Z alkresin $
 */

/* this file adapted FOR hbide from hwgdebug.prg by alex;(Alexey Zapolski(pepan@mail.ru))
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
/*
#include "hwgui.ch"
#include "hxml.ch"*/
#include "fileio.ch"
#include "hbide.ch"
#include "common.ch"
#include "hbclass.ch"
#include "hbqtgui.ch"

#define MODE_INPUT      1
#define MODE_INIT       2
#define MODE_WAIT_ANS   3
#define MODE_WAIT_BR    4

#define ANS_BRP         1
#define ANS_CALC        2
#define ANS_STACK       3
#define ANS_LOCAL       4
#define ANS_WATCH       5
#define ANS_AREAS       6
#define ANS_REC         7
#define ANS_OBJECT      8

#define CMD_QUIT        1
#define CMD_GO          2
#define CMD_STEP        3
#define CMD_TRACE       4
#define CMD_NEXTR       5
#define CMD_TOCURS      6
#define CMD_EXIT        7
#define CMD_STACK       8
#define CMD_EXP         9
#define CMD_LOCAL      10
#define CMD_STATIC     11
#define CMD_PRIV       12
#define CMD_PUBL       13
#define CMD_WATCH      14
#define CMD_AREA       15
#define CMD_REC        16
#define CMD_OBJECT     17
#define CMD_TERMINATE  18

#define BUFF_LEN     1024
#define RES_LEN       100

#define  CLR_LGREEN  12507070
#define  CLR_GREEN      32768
#define  CLR_DBLUE    8404992
#define  CLR_LBLUE1  16759929
#define  CLR_LBLUE2  16764831
#define  CLR_LIGHT1  15132390
#define  CLR_LIGHT2  12632256

#define EDIT_RES         1900

/*
#define MENU_VIEW        1901
#define MENU_STACK       1902
#define MENU_VARS        1903
#define MENU_WATCH       1904
#define MENU_RUN         1905
#define MENU_INIT        1906
#define MENU_QUIT        1907
#define MENU_EXIT        1908
#define MENU_BRP         1909
#define MENU_CMDLINE     1910

#ifndef __PLATFORM__UNIX
REQUEST HWG_SAVEFILE, HWG_SELECTFOLDER
#endif
REQUEST GETENV, HB_FGETDATETIME
REQUEST HB_OSPATHLISTSEPARATOR
REQUEST HWG_RUNCONSOLEAPP, HWG_RUNAPP

#ifdef __PLATFORM__UNIX
ANNOUNCE HB_GTSYS
REQUEST HB_GT_CGI_DEFAULT
#endif

#ifdef __XHARBOUR__
#xtranslate HB_PROCESSOPEN([<n,...>]) =>  HB_OPENPROCESS(<n>)
#endif
*/
#DEFINE cMsgNotSupp "Command isn't supported"

CLASS clsDebugger

    DATA oIde
    
    DATA cCurrentProject
    DATA aSources
    DATA oOutputResult

    DATA lModeIde INIT .T.
    DATA lDebugging INIT .F.
    DATA hHrbProj
    DATA handl1 INIT -1
    DATA handl2
    DATA cBuffer
    DATA nId1 INIT 0
    DATA nId2 INIT -1

    DATA oIni
    DATA cHrbPath INIT "hrb"
    DATA cAppName
    DATA cPrgName INIT ""
    DATA cTextLocate
    DATA nLineLocate

    DATA oTimer
    
    DATA oSayState
    DATA oEditExpr
    DATA oBtnExp
    DATA oMainFont
    
    DATA oBrwRes
    DATA oStackDlg
    DATA oVarsDlg
    DATA oWatchDlg
    DATA oAreasDlg
    DATA oInspectDlg
    DATA cInspectVar
    DATA lViewCmd INIT .T.
    DATA oTabMain
    DATA nTabsMax INIT 5
    DATA cPaths INIT ";"

    DATA aBP INIT {}
    DATA aWatches INIT {}
    DATA aExpr INIT {}
    DATA nCurrLine INIT 0
    DATA nMode INIT MODE_INPUT
    DATA nAnsType
    DATA cPrgBP
    DATA aBPLoad INIT {}
    DATA nBPLoad
    DATA lAnimate INIT .F.
    DATA nAnimate INIT 3

    DATA nExitMode INIT 1
    DATA nVerProto INIT 0

    DATA cIniPath
    DATA cCurrPath
    
    DATA qTimer
    DATA aTabs
    
    DATA   oDebugWatch
    DATA   oDebugVariables
    DATA   oDebugStack
    DATA   oDebugWorkAreas
   
    METHOD init( p_oParent )
    METHOD start( cExe )   
    METHOD LoadBreakPoints()
    METHOD ClearBreakPoints( cPrg )
    METHOD DeleteBreakPoint( cPrg, nLine )
    METHOD ToggleBreakPoint( cAns, cLine )
    METHOD AddBreakPoint( cPrg, nLine )
    METHOD TimerProc()
    METHOD dbgRead()
    METHOD Send( ... )
    METHOD SetMode( newMode )
    METHOD SetCurrLine( nLine, cName )
    METHOD getBP( nLine, cPrg )
    METHOD DoCommand( nCmd, cDop, cDop2 )
    METHOD SetWindow( cPrgName )
    METHOD StopDebug()
    
    METHOD InspectObject( cObjName )
    
    METHOD ShowStack( arr, n )
    METHOD ShowVars( arr, n, nVarType )
    METHOD ShowAreas( arr, n )
    METHOD ShowRec( arr, n )
    METHOD ShowObject( arr, n )
    
    METHOD hu_Get( cTitle, tpict, txget )
    METHOD SetPath( cRes, cName, lClear )
    METHOD wait4connection()
        
ENDCLASS

METHOD clsDebugger:init( p_oParent )
   ::oIde := p_oParent
   ::aBP := {}
   ::aWatches := {}
   ::aExpr := {}
   ::nCurrLine := 0
   ::nId1 := 0
   ::nId2 := -1
   
   ::SetMode( MODE_INIT )

   ::cBuffer := Space( BUFF_LEN )
   
   ::aTabs := ::oIde:aTabs
   ::oOutputResult := ::oIde:oOutputResult
         
   ::oDebugWatch := ::oIde:oDebugWatch
   ::oDebugVariables := ::oIde:oDebugVariables
   ::oDebugStack := ::oIde:oDebugStack
   ::oDebugWorkAreas := ::oIde:oDebugWorkAreas
   
   ::qTimer := QTimer()
   ::qTimer:setInterval( 30 )
   ::qTimer:connect( "timeout()",  {|| ::TimerProc() } )
   ::qTimer:start()
   
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

   DirChange(cPath)

   hb_processOpen( cExe ) //+ Iif( !Empty( cParams ), cParams, "" ) )

   ::handl1 := FOpen( cExe + ".d1", FO_READWRITE + FO_SHARED )
   ::handl2 := FOpen( cExe + ".d2", FO_READ + FO_SHARED )
   IF ::handl1 != -1 .AND. ::handl2 != -1
      ::cAppName := Lower( cFile + cExt )
   ELSE
      ::handl1 := ::handl2 := -1
      hbide_showWarning( "No connection" )
      RETURN .F.
   ENDIF
   
   ::wait4connection()

   ::LoadBreakPoints()
   
   ::lDebugging := .T.
   
//   ::DoCommand( CMD_GO )
   
   RETURN .T.

METHOD clsDebugger:LoadBreakPoints()
   LOCAL i, j, oEditor, cBP, pos

   FOR j := 1 TO Len(::aSources)
      FOR i := 1 TO Len(::aTabs)
         oEditor := ::aTabs[ i, TAB_OEDITOR ]
         IF oEditor:oTab:caption == ::aSources[j]
            cBP := oEditor:qCoEdit:qEdit:GetBreakPointsVector()
            IF !Empty(cBP)
               DO WHILE .T.
                  pos := At(",", cBP)
                  IF pos = 0
                     Aadd( ::aBPLoad, { Val(cBP), oEditor:oTab:caption } )
                     EXIT
                  ELSE
                     Aadd( ::aBPLoad, { Val(SubStr(cBP, 1, pos - 1)), oEditor:oTab:caption } )
                     cBP = SubStr(cBP, pos + 1, Len(cBP - 1))
                  ENDIF
               ENDDO
            ENDIF
         ENDIF
      NEXT i
   NEXT j
   
   IF !Empty( ::aBPLoad )
      ::nBPLoad := 1
      ::AddBreakPoint( ::aBPLoad[1,2], ::aBPLoad[1,1] )
   ENDIF

   RETURN .T.

METHOD clsDebugger:ClearBreakPoints( cPrg )
   LOCAL n
   IF PCOUNT() == 0
      cPrg := ""
   ENDIF

   FOR n := 1 TO Len( ::aBP )
      IF (Empty(cPrg) .OR. cPrg = ::aBP[n,2]) .AND. ::aBP[n,1] <> 0
         ::AddBreakPoint( ::aBP[n,2], ::aBP[n,1] )
         DO WHILE .T.
            EXIT
            //???
         ENDDO
      ENDIF
   NEXT
   RETURN .T.
   
METHOD clsDebugger:DeleteBreakPoint( cPrg, nLine )
      ::Send( "brp", "del", cPrg, Ltrim(Str(nLine)) )
      IF ::nMode != MODE_WAIT_ANS
         ::nAnsType := ANS_BRP
         ::cPrgBP := cPrg
         ::SetMode( MODE_WAIT_ANS )
//         ::TimerProc()
      ENDIF
   RETURN .T.

METHOD clsDebugger:ToggleBreakPoint( cAns, cLine )
   LOCAL nLine := Val( cLine ), i

   IF cAns == "line"
      FOR i := 1 TO Len(::aBP)
         IF ::aBP[i,1] == 0
            ::aBP[i,1] := nLine
            EXIT
         ENDIF
      NEXT
      IF i > Len(::aBP)
         Aadd( ::aBP, { nLine, ::cPrgBP } )
      ENDIF
   ELSE
      IF ( i := ::getBP( nLine, ::cPrgBP ) ) == 0
         hbide_showWarning( "Error deleting BP line " + cLine )
      ELSE
         ::aBP[i,1] := 0
      ENDIF
   ENDIF
//   ::SetCurrLine(nLine, ::cPrgName)
   RETURN NIL

METHOD clsDebugger:AddBreakPoint( cPrg, nLine )

   IF ::nMode != MODE_INPUT .AND. Empty( ::aBPLoad )
      RETURN NIL
   ENDIF
//   IF ::nLine == NIL
//      ::nLine := GetCurrLine()
//   ENDIF

//      IF cPrg == NIL
//         cPrg := oText:cargo
//      ENDIF

      IF ::getBP( nLine, cPrg ) == 0
         ::Send( "brp", "add", cPrg, Ltrim(Str(nLine)) )
      ELSE
         ::Send( "brp", "del", cPrg, Ltrim(Str(nLine)) )
      ENDIF
      IF ::nMode != MODE_WAIT_ANS
         ::nAnsType := ANS_BRP
         ::cPrgBP := cPrg
         ::SetMode( MODE_WAIT_ANS )
      ENDIF
//   ENDIF
   RETURN NIL
   
METHOD clsDebugger:TimerProc()
   LOCAL n, arr
STATIC nLastSec := 0

//?"TimerProc():" + Time()
   IF ::nMode != MODE_INPUT
      IF !Empty( arr := ::dbgRead() )
         IF arr[1] == "quit"
            ::SetMode( MODE_INIT )
            ::StopDebug()
            RETURN NIL
         ENDIF
         IF ::nMode == MODE_WAIT_ANS
            IF Left(arr[1],1) == "b" .AND. ( n := Val( Substr(arr[1],2) ) ) == ::nId1
               IF ::nAnsType == ANS_CALC
                  IF arr[2] == "value"
                     IF !Empty( ::cInspectVar )
                        IF ( Substr( Hex2Str(arr[3]),2,1 ) ) == "O"
                           ::nMode := MODE_INPUT
                           ::InspectObject( ::cInspectVar )
                           ::cInspectVar := NIL
                           RETURN NIL
                        ELSE
                           hbide_showWarning( ::cInspectVar + " isn't an object" )
                        ENDIF
                     ELSE
//???                        ::SetResult( Hex2Str( arr[3] ) )
                     ENDIF
                  ELSE
                     ::oOutputResult:oWidget:append( ( "-- BAD ANSWER --" ) )
                  ENDIF                 
               ELSEIF ::nAnsType == ANS_BRP
                  IF arr[2] == "err"
                     ::oOutputResult:oWidget:append( "-- BAD LINE --" )
                  ELSE
                     ::ToggleBreakPoint( arr[2], arr[3] )
                  ENDIF
                  IF !Empty( ::aBPLoad )
                     IF ++::nBPLoad <= Len(::aBPLoad)
                        ::AddBreakPoint( ::aBPLoad[::nBPLoad,2], ::aBPLoad[::nBPLoad,1] )
                        RETURN NIL
                     ELSE
                        ::aBPLoad := NIL
                     ENDIF
                  ENDIF
               ELSEIF ::nAnsType == ANS_STACK
                  IF arr[2] == "stack"
                     ::ShowStack( arr, 3 )
                  ENDIF
               ELSEIF ::nAnsType == ANS_LOCAL
                  IF arr[2] == "valuelocal"
                     ::ShowVars( arr, 3, 1 )
                  ELSEIF arr[2] == "valuepriv"
                     ::ShowVars( arr, 3, 2 )
                  ELSEIF arr[2] == "valuepubl"
                     ::ShowVars( arr, 3, 3 )
                  ELSEIF arr[2] == "valuestatic"
                     ::ShowVars( arr, 3, 4 )
                  ENDIF
               ELSEIF ::nAnsType == ANS_WATCH
                  IF arr[2] == "valuewatch"
                     ::ShowWatch( arr, 3 )
                  ENDIF
               ELSEIF ::nAnsType == ANS_AREAS
                  IF arr[2] == "valueareas"
                     ::ShowAreas( arr, 3 )
                  ENDIF
               ELSEIF ::nAnsType == ANS_REC
                  IF arr[2] == "valuerec"
                     ::ShowRec( arr, 3 )
                  ENDIF
               ELSEIF ::nAnsType == ANS_OBJECT
                  IF arr[2] == "valueobj"
                     ::ShowObject( arr, 3 )
                  ENDIF
               ENDIF
               ::SetMode( MODE_INPUT )
            ENDIF
         ELSE
            IF Left(arr[1],1) == "a" .AND. ( n := Val( Substr(arr[1],2) ) ) > ::nId2
               ::nId2 := n
               IF arr[2] == "."
                  ::oOutputResult:oWidget:append( "-- BAD LINE --" )
               ELSE
                  IF !( ::cPrgName == arr[2] )
                     ::cPrgName := arr[2]
                     ::SetPath( ::cPaths, ::cPrgName )
                  ENDIF
                  ::SetCurrLine( ::nCurrLine := Val( arr[3] ), ::cPrgName )
                  n := 4
                  DO WHILE .T.
                     IF arr[n] == "ver"
                        ::nVerProto := Val( arr[n+1] )
                        n += 2
                     ELSEIF arr[n] == "stack"
                        ::ShowStack( arr, n+1 )
                        n += 2 + Val( arr[n+1] ) * 3
                     ELSEIF arr[n] == "valuelocal"
                        ::ShowVars( arr, n+1, 1 )
                        n += 2 + Val( arr[n+1] ) * 3
                     ELSEIF arr[n] == "valuepriv"
                        ::ShowVars( arr, n+1, 2 )
                        n += 2 + Val( arr[n+1] ) * 3
                     ELSEIF arr[n] == "valuepubl"
                        ::ShowVars( arr, n+1, 3 )
                        n += 2 + Val( arr[n+1] ) * 3
                     ELSEIF arr[n] == "valuestatic"
                        ::ShowVars( arr, n+1, 4 )
                        n += 2 + Val( arr[n+1] ) * 3
                     ELSEIF arr[n] == "valuewatch"
                        ::ShowWatch( arr, n+1 )
                        n += 2 + Val( arr[n+1] )
                     ELSE
                        EXIT
                     ENDIF
                  ENDDO
                  ::oOutputResult:oWidget:append( /*HWindow():GetMain():handle*/"Debugger ("+arr[2]+", line "+arr[3]+")" )
               ENDIF
               ::SetMode( MODE_INPUT )
               nLastSec := Seconds()
            ENDIF
         ENDIF
      ENDIF
   ELSEIF ::lAnimate .AND. Seconds() - nLastSec > ::nAnimate
      ::Send( "cmd", "step" )
      ::SetMode( MODE_WAIT_BR )
   ENDIF

   RETURN NIL

METHOD clsDebugger:dbgRead()
   LOCAL n, s := "", arr

   FSeek( ::handl2, 0, 0 )
   DO WHILE ( n := Fread( ::handl2, @::cBuffer, Len(::cBuffer) ) ) > 0
      s += Left( ::cBuffer, n )
      IF ( n := At( ",!", s ) ) > 0
         IF ( arr := hb_aTokens( Left( s,n+1 ), "," ) ) != NIL .AND. Len( arr ) > 2 .AND. arr[1] == arr[Len(arr)-1]
            RETURN arr
         ELSE
            EXIT
         ENDIF
      ENDIF
   ENDDO
   RETURN NIL

METHOD clsDebugger:Send( ... )
Local arr := hb_aParams(), i, s := ""

   FSeek( ::handl1, 0, 0 )
   FOR i := 1 TO Len( arr )
      s += arr[i] + ","
   NEXT
   FWrite( ::handl1, Ltrim(Str(++::nId1)) + "," + s + Ltrim(Str(::nId1)) + ",!" )

   RETURN NIL

METHOD clsDebugger:SetMode( newMode )

   ::nMode := newMode
   IF newMode == MODE_INPUT
   ELSE
      IF newMode == MODE_WAIT_ANS .OR. newMode == MODE_WAIT_BR
         IF newMode == MODE_WAIT_BR
            ::nCurrLine := 0
            //SetCurrLine()
         ENDIF
      ELSEIF newMode == MODE_INIT
         ::lDebugging := .F.
      ENDIF
   ENDIF

   RETURN NIL

METHOD clsDebugger:SetCurrLine( nLine, cName )
LOCAL qCursor

   IF PCount() < 2
      hbide_showWarning( "Not all parameters passed into clsDebugger:SetCurrLine" )
      RETURN .F.
   ENDIF
?"SetCurrLine", cName, nLine
   IF !::lDebugging
      ::lDebugging := .T.
   ENDIF

//   oText := GetTextObj( cName, @nTab )
   ::SetWindow( cName )
   qCursor := ::oIde:qCurEdit:textCursor()

   qCursor:movePosition( QTextCursor_Down, QTextCursor_MoveAnchor, nLine )
   ::oIde:qCurEdit:setTextCursor( qCursor )
   ::oIde:manageFocusInEditor()

//      IF !Empty( nLine ) .AND. oText:nTextLen >= nLine
//         oText:GoTo( nLine )

   RETURN NIL

METHOD clsDebugger:getBP( nLine, cPrg )
   cPrg := Lower( Iif( cPrg==NIL, ::cPrgName, cPrg ) )
   RETURN Ascan( ::aBP, {|a|a[1]==nLine .and. Lower(a[2])==cPrg} )

METHOD clsDebugger:DoCommand( nCmd, cDop, cDop2 )

   IF ::nMode == MODE_INPUT
      IF nCmd == CMD_GO
//         ::SetWindow( ::cPrgName )
         ::Send( "cmd", "go" )

      ELSEIF nCmd == CMD_STEP
         ::Send( "cmd", "step" )

      ELSEIF nCmd == CMD_TOCURS
         ::Send( "cmd", "to", ::cPrgName, Ltrim(Str(::GetCurrLine())) )

      ELSEIF nCmd == CMD_TRACE
         ::Send( "cmd", "trace" )

      ELSEIF nCmd == CMD_NEXTR
         ::Send( "cmd", "nextr" )

      ELSEIF nCmd == CMD_EXP
         ::Send( "exp", cDop )
         ::nAnsType := ANS_CALC
         ::SetMode( MODE_WAIT_ANS )
         RETURN NIL

      ELSEIF nCmd == CMD_STACK
         ::Send( "view", "stack", cDop )
         ::nAnsType := ANS_STACK
         ::SetMode( MODE_WAIT_ANS )
         RETURN NIL

      ELSEIF nCmd == CMD_LOCAL
         ::Send( "view", "local", cDop )
         ::nAnsType := ANS_LOCAL
         ::SetMode( MODE_WAIT_ANS )
         RETURN NIL

      ELSEIF nCmd == CMD_PRIV
         IF ::nVerProto > 1
            ::Send( "view", "priv", cDop )
            ::nAnsType := ANS_LOCAL
            ::SetMode( MODE_WAIT_ANS )
         ELSE
            hbide_showWarning( cMsgNotSupp )
         ENDIF
         RETURN NIL

      ELSEIF nCmd == CMD_PUBL
         IF ::nVerProto > 1
            ::Send( "view", "publ", cDop )
            ::nAnsType := ANS_LOCAL
            ::SetMode( MODE_WAIT_ANS )
         ELSE
            hbide_showWarning( cMsgNotSupp )
         ENDIF
         RETURN NIL

      ELSEIF nCmd == CMD_STATIC
         IF ::nVerProto > 1
            ::Send( "view", "static", cDop )
            ::nAnsType := ANS_LOCAL
            ::SetMode( MODE_WAIT_ANS )
         ELSE
            hbide_showWarning( cMsgNotSupp )
         ENDIF
         RETURN NIL

      ELSEIF nCmd == CMD_WATCH
         IF Empty( cDop2 )
            ::Send( "view", "watch", cDop )
         ELSE
            ::Send( "watch", cDop, cDop2 )
         ENDIF
         ::nAnsType := ANS_WATCH
         ::SetMode( MODE_WAIT_ANS )
         RETURN NIL

      ELSEIF nCmd == CMD_AREA
         ::Send( "view", "areas" )
         ::nAnsType := ANS_AREAS
         ::SetMode( MODE_WAIT_ANS )
         RETURN NIL

      ELSEIF nCmd == CMD_REC
         IF ::nVerProto > 1
            ::Send( "insp", "rec", cDop )
            ::nAnsType := ANS_REC
            ::SetMode( MODE_WAIT_ANS )
         ELSE
            hbide_showWarning( cMsgNotSupp )
         ENDIF
         RETURN NIL

      ELSEIF nCmd == CMD_OBJECT
         IF ::nVerProto > 1
            ::Send( "insp", "obj", cDop )
            ::nAnsType := ANS_OBJECT
            ::SetMode( MODE_WAIT_ANS )
         ELSE
            hbide_showWarning( cMsgNotSupp )
         ENDIF
         RETURN NIL

      ELSEIF nCmd == CMD_QUIT
         ::nExitMode := 2
//         ::EndWindow()
         RETURN NIL

      ELSEIF nCmd == CMD_EXIT
         ::nExitMode := 1
//         ::EndWindow()
         RETURN NIL

      ELSEIF nCmd == CMD_TERMINATE
         ::Send( "cmd", "quit" )
         ::lDebugging := .F.
         ::StopDebug()

      ENDIF
      ::SetMode( MODE_WAIT_BR )
   ELSEIF nCmd == CMD_EXIT
      ::nExitMode := 1
//      ::EndWindow()

   ENDIF
   RETURN NIL

METHOD clsDebugger:SetWindow( cPrgName )
   LOCAL qCursor
   ::oIde:oSM:editSource( cPrgName, 0, 0, 0, NIL, NIL, .f., .t. )
   qCursor := ::oIde:qCurEdit:textCursor()
   qCursor:setPosition( 0 )

   RETURN .T.

METHOD clsDebugger:StopDebug()

   ::oDebugWatch:Close()
   ::oDebugVariables:Close()
   ::oDebugStack:Close()
   ::oDebugWorkAreas:Close()

   IF ::handl1 != -1
      FClose( ::handl1 )
      FClose( ::handl2 )
      ::handl1 := -1
   ENDIF
   RETURN NIL

METHOD clsDebugger:InspectObject( cObjName )
//   LOCAL oDlg, oBrw

/*
   INIT DIALOG oDlg TITLE "Object inspector ("+cObjName+")" AT 30, 30 SIZE 480, 400 ;
     FONT HWindow():GetMain():oFont

   @ 0,0 BROWSE oBrw ARRAY OF oDlg          ;
         SIZE 480,340                       ;
         FONT HWindow():GetMain():oFont     ;
         STYLE WS_VSCROLL                   ;
         ON SIZE ANCHOR_TOPABS + ANCHOR_LEFTABS + ANCHOR_RIGHTABS + ANCHOR_BOTTOMABS

   oBrw:aArray := {}
   oBrw:AddColumn( HColumn():New( "Name",{|v,o|o:aArray[o:nCurrent,1]},"C",12,0 ) )
   oBrw:AddColumn( HColumn():New( "Type",{|v,o|o:aArray[o:nCurrent,2]},"C",2,0 ) )
   oBrw:AddColumn( HColumn():New( "Value",{|v,o|o:aArray[o:nCurrent,3]},"C",60,0 ) )

   oBrw:bcolorSel := oBrw:htbcolor := CLR_LGREEN
   oBrw:tcolorSel := oBrw:httcolor := 0

   @ 45, 360 BUTTON "Refresh" ON CLICK {|| oInspectDlg:=oDlg,DoCommand(CMD_OBJECT,cObjName) } SIZE 100, 28 ON SIZE ANCHOR_BOTTOMABS
   @ 335, 360 BUTTON "Close" ON CLICK {|| oDlg:Close() } SIZE 100, 28 ON SIZE ANCHOR_RIGHTABS + ANCHOR_BOTTOMABS

   ACTIVATE DIALOG oDlg NOMODAL

   oInspectDlg := oDlg
*/
   ::DoCommand( CMD_OBJECT, cObjName )

   RETURN NIL
   
METHOD clsDebugger:ShowStack( arr, n )
   HB_SYMBOL_UNUSED( arr )
   HB_SYMBOL_UNUSED( n )
   RETURN NIL
   
METHOD clsDebugger:ShowVars( arr, n, nVarType )
   HB_SYMBOL_UNUSED( arr )
   HB_SYMBOL_UNUSED( n )
   HB_SYMBOL_UNUSED( nVarType )

   RETURN NIL

METHOD clsDebugger:ShowAreas( arr, n )
   HB_SYMBOL_UNUSED( arr )
   HB_SYMBOL_UNUSED( n )

   RETURN NIL
   
METHOD clsDebugger:ShowRec( arr, n )
   HB_SYMBOL_UNUSED( arr )
   HB_SYMBOL_UNUSED( n )

   RETURN NIL

METHOD clsDebugger:ShowObject( arr, n )
   HB_SYMBOL_UNUSED( arr )
   HB_SYMBOL_UNUSED( n )

   RETURN NIL
   
METHOD clsDebugger:SetPath( cRes, cName, lClear )
   LOCAL arr, i, cFull
   
   HB_SYMBOL_UNUSED( lClear )

   IF !Empty( cRes ) .OR. !Empty( cRes := ::hu_Get( "Path to source files", "@S256", ::cPaths ) )
      ::cPaths := Iif( Left( cRes,1 ) != ";", ";" + cRes, cRes )
      arr := hb_aTokens( ::cPaths, ";" )
      IF !Empty( cName )
         FOR i := 1 TO Len( arr )
            cFull := arr[i] + ;
               Iif( Empty(arr[i]).OR.Right( arr[i],1 ) $ "\/", "", hb_OsPathSeparator() ) + ::cPrgName
?"cFull:", cFull
            IF ::SetWindow( cFull )
               EXIT
            ENDIF
         NEXT
      ENDIF
   ENDIF

   RETURN NIL

METHOD clsDebugger:hu_Get( cTitle, tpict, txget )
   HB_SYMBOL_UNUSED( cTitle )
   HB_SYMBOL_UNUSED( tpict )
   HB_SYMBOL_UNUSED( txget )

   RETURN ""
   
STATIC FUNCTION Hex2Str( stroka )
   LOCAL cRes := "", i := 1, nLen := Len( stroka )

   DO WHILE i <= nLen
      cRes += Chr( Hex2Int( Substr( stroka,i,2 ) ) )
      i += 2
   ENDDO
   RETURN cRes

STATIC FUNCTION Hex2Int( stroka )
   LOCAL i := ASC( stroka ), res

   IF i > 64 .AND. i < 71
      res := ( i - 55 ) * 16
   ELSEIF i > 47 .AND. i < 58
      res := ( i - 48 ) * 16
   ELSE
      RETURN 0
   ENDIF

   i := ASC( SubStr( stroka,2,1 ) )
   IF i > 64 .AND. i < 71
      res += i - 55
   ELSEIF i > 47 .AND. i < 58
      res += i - 48
   ENDIF
   RETURN res
   
METHOD clsDebugger:wait4connection()
   LOCAL n

   DO WHILE .T.
      FSeek( ::handl2, 0, 0 )
      n := Fread( ::handl2, @::cBuffer, Len(::cBuffer) ) 
      IF n > 0
         IF At("ver", ::cBuffer) > 0
            EXIT
         ENDIF
      ENDIF
//TODO exit by timeout
   ENDDO
   
   RETURN NIL