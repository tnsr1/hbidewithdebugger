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

CLASS clsDebugger

    DATA oIde
    
    DATA cCurrentProject
    DATA aSources

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
    DATA nMode
    DATA nAnsType
    DATA cPrgBP
    DATA aBPLoad INIT {}
    DATA nBPLoad
    DATA lAnimate INIT .F.
    DATA nAnimate INIT 3

    DATA nExitMode INIT 1
    DATA nVerProto INIT 0
    DATA cMsgNotSupp INIT "Command isn't supported"

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
    
ENDCLASS

METHOD clsDebugger:init( p_oParent )
   ::oIde := p_oParent
   ::aBP := {}
   ::aWatches := {}
   ::aExpr := {}
   ::nCurrLine := 0
   ::nId1 := 0
   ::nId2 := -1
   
   ::aTabs := ::oIde:aTabs
         
   ::oDebugWatch := ::oIde:oDebugWatch
   ::oDebugVariables := ::oIde:oDebugVariables
   ::oDebugStack := ::oIde:oDebugStack
   ::oDebugWorkAreas := ::oIde:oDebugWorkAreas
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

   ::qTimer := QTimer()
   ::qTimer:setInterval( 30000 )
   ::qTimer:connect( "timeout()",  {|| ::TimerProc() } )
   
   ::LoadBreakPoints()
   
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
Local nLine := Val( cLine ), i

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
   ::SetCurrLine(nLine, ::cPrgName)
Return Nil

METHOD clsDebugger:AddBreakPoint( cPrg, nLine )

   IF ::nMode != MODE_INPUT .AND. Empty( ::aBPLoad )
      Return Nil
   ENDIF
//   IF ::nLine == Nil
//      ::nLine := GetCurrLine()
//   ENDIF

//      IF cPrg == Nil
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
Return Nil
   
METHOD clsDebugger:TimerProc()

Return Nil

METHOD clsDebugger:dbgRead()
Local n, s := "", arr

   FSeek( ::handl2, 0, 0 )
   DO WHILE ( n := Fread( ::handl2, @::cBuffer, Len(::cBuffer) ) ) > 0
      s += Left( ::cBuffer, n )
      IF ( n := At( ",!", s ) ) > 0
         IF ( arr := hb_aTokens( Left( s,n+1 ), "," ) ) != Nil .AND. Len( arr ) > 2 .AND. arr[1] == arr[Len(arr)-1]
            Return arr
         ELSE
            EXIT
         ENDIF
      ENDIF
   ENDDO
Return Nil

METHOD clsDebugger:Send( ... )
Local arr := hb_aParams(), i, s := ""

   FSeek( ::handl1, 0, 0 )
   FOR i := 1 TO Len( arr )
      s += arr[i] + ","
   NEXT
   FWrite( ::handl1, Ltrim(Str(++::nId1)) + "," + s + Ltrim(Str(::nId1)) + ",!" )

Return Nil

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

Return Nil

METHOD clsDebugger:SetCurrLine( nLine, cName )
LOCAL qCursor, cSource

   IF PCount() < 2
      hbide_showWarning( "Not all parameters passed into clsDebugger:SetCurrLine" )
      RETURN .F.
   ENDIF

   IF !::lDebugging
      ::lDebugging := .T.
   ENDIF

//   oText := GetTextObj( cName, @nTab )
   cSource := cName

   ::oIde:oSM:editSource( cSource, 0, 0, 0, NIL, NIL, .f., .t. )
   qCursor := ::oIde:qCurEdit:textCursor()
   qCursor:setPosition( 0 )
   qCursor:movePosition( QTextCursor_Down, QTextCursor_MoveAnchor, nLine )
   ::oIde:qCurEdit:setTextCursor( qCursor )
   ::oIde:manageFocusInEditor()

//      IF !Empty( nLine ) .AND. oText:nTextLen >= nLine
//         oText:GoTo( nLine )

Return Nil

METHOD clsDebugger:getBP( nLine, cPrg )
   cPrg := Lower( Iif( cPrg==Nil, ::cPrgName, cPrg ) )
Return Ascan( ::aBP, {|a|a[1]==nLine .and. Lower(a[2])==cPrg} )
