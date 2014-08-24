/*
 * $Id: edit.prg 339 2014-08-16 18:27:58Z bedipritpal $
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009-2014 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
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
/*----------------------------------------------------------------------*/
/*
 *                                EkOnkar
 *                          ( The LORD is ONE )
 *
 *                            Harbour-Qt IDE
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                               27Dec2009
 */
/*----------------------------------------------------------------------*/

#include "common.ch"
#include "hbclass.ch"
#include "hbqtgui.ch"
#include "hbide.ch"
#include "xbp.ch"
#include "hbtoqt.ch"
#include "hbqtstd.ch"

/*----------------------------------------------------------------------*/

#define __customContextMenuRequested__            1
#define __textChanged__                           2
#define __copyAvailable__                         3
#define __modificationChanged__                   4
#define __redoAvailable__                         5
#define __selectionChanged__                      6
#define __undoAvailable__                         7
#define __updateRequest__                         8
#define __cursorPositionChanged__                 9

#define __timerTimeout__                          23

#define __selectionMode_stream__                  1
#define __selectionMode_column__                  2
#define __selectionMode_line__                    3

/*----------------------------------------------------------------------*/

CLASS IdeEdit INHERIT IdeObject

   DATA   oEditor

   DATA   qEdit
   DATA   nOrient                                 INIT  0

   DATA   nMode                                   INIT  0
   DATA   nLineNo                                 INIT  -99
   DATA   nMaxDigits                              INIT  5       // Tobe
   DATA   nMaxRows                                INIT  100
   DATA   nLastLine                               INIT  -99
   DATA   nCurLineNo                              INIT  0
   DATA   nPrevLineNo                             INIT  -1
   DATA   nPrevLineNo1                            INIT  -1

   DATA   aBookMarks                              INIT  {}

   DATA   lModified                               INIT  .F.
   DATA   lIndentIt                               INIT  .F.
   DATA   lUpdatePrevWord                         INIT  .F.
   DATA   lCopyWhenDblClicked                     INIT  .F.
   DATA   cCurLineText                            INIT  ""

   DATA   cProto                                  INIT ""
   DATA   cProtoOrg                               INIT ""
   DATA   qTimer
   DATA   nProtoLine                              INIT -1
   DATA   nProtoCol                               INIT -1
   DATA   isSuspended                             INIT .F.

   DATA   nProtoRows                              INIT 1
   DATA   nProtoCols                              INIT 10

   DATA   fontFamily
   DATA   pointSize
   DATA   currentPointSize
   DATA   qFont
   DATA   aLastEditingPosition                    INIT {}
   DATA   aBlockCopyContents                      INIT {}
   DATA   isLineSelectionON                       INIT .F.
   DATA   aSelectionInfo                          INIT { -1,-1,-1,-1,1,0 }
   DATA   aViewportInfo                           INIT { -1,-1,-1,-1,-1,-1 }

   DATA   isColumnSelectionON                     INIT .F.
   DATA   lReadOnly                               INIT .F.
   DATA   isHighLighted                           INIT .f.
   DATA   cLastWord, cCurWord
   DATA   hLogicals
   DATA   isMatchingPair                          INIT .F.

   METHOD new( oIde, oEditor, nMode )
   METHOD create( oIde, oEditor, nMode )
   METHOD destroy()
   METHOD execEvent( nMode, p, p1 )
   METHOD execKeyEvent( nMode, nEvent, p, p1, p2 )
   METHOD connectEditSignals()
   METHOD disconnectEditSignals()

   METHOD reload()
   METHOD redo()
   METHOD undo()
   METHOD cut()
   METHOD copy()
   METHOD paste()
   METHOD selectAll()

   METHOD setReadOnly( lReadOnly )
   METHOD setNewMark()
   METHOD setTooltipMark( nIndex )
   METHOD gotoMark( nIndex )
   METHOD duplicateLine()
   METHOD deleteLine()
   METHOD blockComment()
   METHOD streamComment()
   METHOD blockIndent( nDirctn )
   METHOD moveLine( nDirection )
   METHOD caseUpper()
   METHOD caseLower()
   METHOD caseInvert()
   METHOD convertQuotes()
   METHOD convertDQuotes()
   METHOD findLastIndent()
   METHOD reLayMarkButtons()
   METHOD presentSkeletons()
   METHOD handleCurrentIndent()
   METHOD loadFuncHelp()
   METHOD clickFuncHelp()
   METHOD goto( nLine )
   METHOD gotoFunction()
   METHOD toggleLineNumbers()
   METHOD toggleHorzRuler()

   METHOD toggleSelectionMode()
   METHOD toggleStreamSelectionMode()
   METHOD toggleColumnSelectionMode()
   METHOD toggleLineSelectionMode()
   METHOD clearSelection()
   METHOD togglePersistentSelection()
   METHOD toggleCodeCompetion()
   METHOD toggleCompetionTips()

   METHOD getWord( lSelect )
   METHOD getLine( nLine, lSelect )
   METHOD getText()
   METHOD getSelectedText()
   METHOD getColumnNo()
   METHOD getLineNo()
   METHOD insertSeparator( cSep )
   METHOD insertText( cText )

   METHOD suspendPrototype()
   METHOD resumePrototype()
   METHOD showPrototype( cProto )
   METHOD hidePrototype()
   METHOD completeCode( p )

   METHOD setLineNumbersBkColor( nR, nG, nB )
   METHOD setCurrentLineColor( nR, nG, nB )
   METHOD getCursor()                             INLINE ::qEdit:textCursor()
   METHOD find( cText, nPosFrom )
   METHOD refresh()
   METHOD isModified()                            INLINE ::oEditor:qDocument:isModified()
   METHOD setFont()
   METHOD markCurrentFunction()
   METHOD copyBlockContents()
   METHOD pasteBlockContents()
   METHOD insertBlockContents( oKey )
   METHOD cutBlockContents( k )
   METHOD zoom( nKey )
   METHOD blockConvert( cMode )
   METHOD dispStatusInfo()
   METHOD toggleCurrentLineHighlightMode()
   METHOD currentFunctionIndex()
   METHOD toPreviousFunction()
   METHOD toNextFunction()

   METHOD home()
   METHOD end()
   METHOD down()
   METHOD up()
   METHOD goBottom()
   METHOD goTop()
   METHOD left()
   METHOD right()
   METHOD panEnd()
   METHOD panHome()
   METHOD pageUp()
   METHOD pageDown()
   METHOD printPreview()
   METHOD paintRequested( qPrinter )
   METHOD tabs2spaces()
   METHOD spaces2tabs()
   METHOD removeTrailingSpaces()
   METHOD formatBraces( nMode )
   METHOD upperCaseKeywords()
   METHOD findEx( cText, nFlags, nStart )
   METHOD highlightAll( cText )
   METHOD unHighlight()
   METHOD parseCodeCompletion( cSyntax )

   METHOD highlightPage()
   METHOD reformatLine( nPos, nDeleted, nAdded )
   METHOD handleTab( key )
   METHOD alignAt( cAt )
   METHOD stringify()
   METHOD execContextMenu( p )
   METHOD execToolsBox( p )
   METHOD openHeaderFile()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeEdit:new( oIde, oEditor, nMode )

   DEFAULT oIde    TO ::oIde
   DEFAULT oEditor TO ::oEditor
   DEFAULT nMode   TO ::nMode

   ::oIde    := oIde
   ::oEditor := oEditor
   ::nMode   := nMode

   ::fontFamily       := ::oINI:cFontName
   ::pointSize        := ::oINI:nPointSize
   ::currentPointSize := ::oINI:nPointSize

   ::hLogicals := {=>}
   hb_hCaseMatch( ::hLogicals, .F. )
   ::hLogicals := { "t" => NIL, "f" => NIL, "or" => NIL, "and" => NIL, "not" => NIL }

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:create( oIde, oEditor, nMode )
   LOCAL nBlock, oPalette

   DEFAULT oIde    TO ::oIde
   DEFAULT oEditor TO ::oEditor
   DEFAULT nMode   TO ::nMode

   ::oIde    := oIde
   ::oEditor := oEditor
   ::nMode   := nMode

   ::qEdit   := HBQPlainTextEdit()
   //
   WITH OBJECT ::qEdit
      :setLineWrapMode( QTextEdit_NoWrap )
      :ensureCursorVisible()
      :setContextMenuPolicy( Qt_CustomContextMenu )
      :setTabChangesFocus( .f. )
      :setFocusPolicy( Qt_StrongFocus )
      :setObjectName( hbide_getNextIDasString( "HBQPlainTextEdit" ) )
      :setMouseTracking( .T. )
   ENDWITH

   oPalette := ::qEdit:palette()
   oPalette:setColor( QPalette_Inactive, QPalette_Highlight, QColor( Qt_yellow ) )
   ::qEdit:setPalette( oPalette )

   ::setFont()

   ::qEdit:hbSetSpaces( ::nTabSpaces )

   ::qEdit:hbSetCompleter( ::qCompleter )

   ::toggleCurrentLineHighlightMode()
   ::toggleLineNumbers()
   ::toggleHorzRuler()

   FOR EACH nBlock IN ::aBookMarks
      ::qEdit:hbBookMarks( nBlock )
   NEXT

   ::connectEditSignals()

   WITH OBJECT ::qEdit
      :connect( QEvent_KeyPress           , {|p| ::execKeyEvent( 101, QEvent_KeyPress           , p ) } )
      :connect( QEvent_Wheel              , {|p| ::execKeyEvent( 102, QEvent_Wheel              , p ) } )
      :connect( QEvent_FocusIn            , {| | ::execKeyEvent( 104, QEvent_FocusIn                ) } )
      :connect( QEvent_Resize             , {| | ::execKeyEvent( 106, QEvent_Resize                 ) } )
      :connect( QEvent_FocusOut           , {| | ::execKeyEvent( 105, QEvent_FocusOut               ) } )
      :connect( QEvent_MouseButtonPress   , {| | ::execKeyEvent( 106, QEvent_MouseButtonPress       ) } )

      :hbSetEventBlock( {|p,p1,p2| ::execKeyEvent( 115, 1001, p, p1, p2 ) } )
   ENDWITH

   ::qTimer := QTimer()
   ::qTimer:setInterval( 2000 )
   ::qTimer:connect( "timeout()",  {|| ::execEvent( __timerTimeout__ ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:destroy()

   IF Self == ::oEditor:oEdit
      ::oSourceThumbnailDock:oWidget:hide()
   ENDIF

   ::oEditor := NIL

   ::qTimer:disconnect( "timeout()" )
   IF ::qTimer:isActive()
      ::qTimer:stop()
   ENDIF
   ::qTimer := NIL

   WITH OBJECT ::qEdit
      :disconnect( QEvent_KeyPress            )
      :disconnect( QEvent_Wheel               )
      :disconnect( QEvent_FocusIn             )
      :disconnect( QEvent_FocusOut            )
      :disconnect( QEvent_Resize              )
      :disconnect( QEvent_MouseButtonDblClick )
      :disconnect( QEvent_MouseMove           )
   ENDWITH

   ::disconnectEditSignals()

   ::qEdit:setParent( QWidget() )
   ::qFont := NIL

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD IdeEdit:disconnectEditSignals()

   ::qEdit:disConnect( "customContextMenuRequested(QPoint)" )
   ::qEdit:disConnect( "textChanged()"                      )
   ::qEdit:disConnect( "selectionChanged()"                 )
   ::qEdit:disConnect( "cursorPositionChanged()"            )
   ::qEdit:disConnect( "copyAvailable(bool)"                )

   #if 0
   ::qEdit:disConnect( "updateRequest(QRect,int)"           )
   ::qEdit:disConnect( "modificationChanged(bool)"          )
   ::qEdit:disConnect( "redoAvailable(bool)"                )
   ::qEdit:disConnect( "undoAvailable(bool)"                )
   #endif

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD IdeEdit:connectEditSignals()

   ::qEdit:connect( "customContextMenuRequested(QPoint)", {|p   | ::execEvent( __customContextMenuRequested__, p ) } )
   ::qEdit:connect( "selectionChanged()"                , {|p   | ::execEvent( __selectionChanged__, p    ) } )
   ::qEdit:connect( "cursorPositionChanged()"           , {|    | ::execEvent( __cursorPositionChanged__  ) } )
   ::qEdit:connect( "copyAvailable(bool)"               , {|p   | ::execEvent( __copyAvailable__, p       ) } )

#if 0
   ::qEdit:connect( "modificationChanged(bool)"         , {|p   | ::execEvent( __modificationChanged__, p ) } )
   ::qEdit:connect( "textChanged()"                     , {|    | ::execEvent( __textChanged__            ) } )
   ::qEdit:connect( "updateRequest(QRect,int)"          , {|p,p1| ::execEvent( __updateRequest__, p, p1   ) } )
   ::qEdit:connect( "redoAvailable(bool)"               , {|p   | ::execEvent( __redoAvailable__, p       ) } )
   ::qEdit:connect( "undoAvailable(bool)"               , {|p   | ::execEvent( __undoAvailable__, p       ) } )
#endif

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD IdeEdit:execEvent( nMode, p, p1 )
   LOCAL qCursor, lOtherEdit

   HB_SYMBOL_UNUSED( p1 )

   IF ::lQuitting
      RETURN NIL
   ENDIF

   qCursor := ::qEdit:textCursor()
   ::nCurLineNo := qCursor:blockNumber()

   SWITCH nMode

   CASE __timerTimeout__
      IF empty( ::cProto )
         ::hidePrototype()
      ELSE
         ::showPrototype()
      ENDIF
      EXIT

   CASE __cursorPositionChanged__
      ::oEditor:dispEditInfo( ::qEdit )   /* Is a MUST */
      ::markCurrentFunction()             /* Optimized */
      EXIT

   CASE __selectionChanged__
      lOtherEdit := ! ( ::oEditor:qCqEdit == ::qEdit )
      IF lOtherEdit
         ::oEditor:qCqEdit := ::qEdit
         ::oEditor:qCoEdit := Self
         IF HB_ISOBJECT( ::oEditor:qHiliter )
            ::oEditor:qHiliter:hbSetEditor( ::qEdit )
            ::qEdit:hbSetHighlighter( ::oEditor:qHiliter )
         ENDIF
      ENDIF
      IF ::aSelectionInfo[ 1 ] > -1 .AND. ::aSelectionInfo[ 1 ] == ::aSelectionInfo[ 3 ]
         ::oDK:setStatusText( SB_PNL_SELECTEDCHARS, Len( ::getSelectedText() ) )
      ELSE
         ::oDK:setStatusText( SB_PNL_SELECTEDCHARS, 0 )
      ENDIF

      ::oDK:showSelectedTextToolbar( Self )
      ::unHighlight()
      ::oUpDn:show( Self )
      EXIT

   CASE __copyAvailable__
      IF p .AND. ::lCopyWhenDblClicked
         ::qEdit:copy()
      ENDIF
      ::lCopyWhenDblClicked := .f.
      EXIT

   CASE __customContextMenuRequested__
      ::execContextMenu( p )
      EXIT

#if 0
   CASE __textChanged__
      ::aLastEditingPosition := { ::qEdit:horizontalScrollBar():value(), ::qEdit:verticalScrollBar():value(), ::qEdit:textCursor()  }
      EXIT
   CASE __modificationChanged__
      ::oEditor:setTabImage( ::qEdit )
      EXIT
   CASE __redoAvailable__
      EXIT
   CASE __undoAvailable__
      EXIT
   CASE __updateRequest__
      EXIT
#endif
   ENDSWITCH

   RETURN NIL

/*----------------------------------------------------------------------*/
/* DEFAULT Ctrl+M */
METHOD IdeEdit:execToolsBox( p )
   IF Empty( p )
      RETURN ::oAC:showContextWidget( Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:execContextMenu( p )
   LOCAL n, cAct, qPos, qAct, qCursor //qRect

   IF .T.
      qCursor := ::qEdit:textCursor()
      qPos := p

      ::oEM:aActions[ 17, 2 ]:setEnabled( !empty( qCursor:selectedText() ) )

      n := ascan( ::oEditor:aEdits, {|o| o == Self } )

      ::oEM:aActions[ 18, 2 ]:setEnabled( Len( ::oEditor:aEdits ) == 0 .OR. ::oEditor:nSplOrient == -1 .OR. ::oEditor:nSplOrient == 1 )
      ::oEM:aActions[ 19, 2 ]:setEnabled( Len( ::oEditor:aEdits ) == 0 .OR. ::oEditor:nSplOrient == -1 .OR. ::oEditor:nSplOrient == 2 )
      ::oEM:aActions[ 21, 2 ]:setEnabled( n > 0 )
      IF empty( qAct := ::oEM:qContextMenu:exec( ::qEdit:mapToGlobal( qPos ) ) )
         RETURN Self
      ENDIF
      cAct := strtran( qAct:text(), "&", "" )
      SWITCH cAct
      CASE "Split Horizontally"
         ::oEditor:split( 1, Self )
         EXIT
      CASE "Split Vertically"
         ::oEditor:split( 2, Self )
         EXIT
      CASE "Close Splitted Instance"
         IF n > 0  /* 1 == Main Edit */
            hb_adel( ::oEditor:aEdits, n, .t. )
            ::oEditor:qCqEdit := ::oEditor:qEdit
            ::oEditor:qCoEdit := ::oEditor:oEdit
            ::destroy()
            ::oIde:manageFocusInEditor()
         ENDIF
         EXIT
      CASE "Save as Skeleton..."
         ::oSK:saveAs( ::getSelectedText() )
         EXIT
      CASE "Change Theme"
         ::oEditor:applyTheme()
         EXIT
      CASE "Goto Function"
         ::gotoFunction()
         EXIT
      CASE "Cut"
         ::cut()
         EXIT
      CASE "Copy"
         ::copy()
         EXIT
      CASE "Paste"
         ::paste()
         EXIT
      CASE "Undo"
         ::undo()
         EXIT
      CASE "Redo"
         ::redo()
         EXIT
      CASE "Diff"
         ::oEditor:vssExecute( "Diff" )
         EXIT
      CASE "Show Selected Text"
         ::oIde:showFragment( ::getSelectedText(), "Selected Text", QIcon( hbide_image( "selectionline" ) ) )
         EXIT
      CASE "Get Latest Version"
         ::oEditor:vssExecute( "Get" )
         EXIT
      CASE "Checkin"
         ::oEditor:vssExecute( "Checkin" )
         EXIT
      CASE "Undo Checkout"
         ::oEditor:vssExecute( "Undocheckout" )
         EXIT
      CASE "Checkout"
         ::oEditor:vssExecute( "Checkout" )
         EXIT
      OTHERWISE
         IF "." $ cAct
            ::oEditor:cTheme := SubStr( cAct, At( ".", cAct ) + 2 )
            ::oTH:changeSyntaxHilighting( ::qEdit, @::oEditor:cTheme, ::oEditor:qHiliter )
         ENDIF
         EXIT
      ENDSWITCH
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD IdeEdit:execKeyEvent( nMode, nEvent, p, p1, p2 )
   LOCAL key, kbm, lAlt, lCtrl, lShift

   HB_SYMBOL_UNUSED( nMode )
   HB_SYMBOL_UNUSED( p1 )
   HB_SYMBOL_UNUSED( p2 )

   SWITCH nEvent

   CASE QEvent_KeyPress     /* The key is sent here prior to applying to editor */
      key    := p:key()
      kbm    := p:modifiers()

      lAlt   := hb_bitAnd( kbm, Qt_AltModifier     ) == Qt_AltModifier
      lCtrl  := hb_bitAnd( kbm, Qt_ControlModifier ) == Qt_ControlModifier
      lShift := hb_bitAnd( kbm, Qt_ShiftModifier   ) == Qt_ShiftModifier

      SWITCH key                            /* On top of any user defined action be executed - QPlainTextEdit's default keys */
      CASE Qt_Key_Tab
      CASE Qt_Key_Backtab
         p:accept()
         ::handleTab( key )
         RETURN .T.
      CASE Qt_Key_Right
         IF lAlt
            p:accept()
            ::oEM:nextEditor()
            RETURN .T.
         ENDIF
         EXIT
      CASE Qt_Key_Left
         IF lAlt
            p:accept()
            ::oEM:previousEditor()
            RETURN .T.
         ENDIF
         EXIT
      CASE Qt_Key_L
         IF lCtrl
            IF ! Empty( ::aLastEditingPosition )
               ::qEdit:horizontalScrollBar():setValue( ::aLastEditingPosition[ 1 ] )
               ::qEdit:verticalScrollBar():setValue( ::aLastEditingPosition[ 2 ] )
               ::qEdit:setTextCursor( ::aLastEditingPosition[ 3 ] )
               RETURN .T.
            ENDIF
         ENDIF
         EXIT
      ENDSWITCH

      IF ::oSC:execKey( Self, key, lAlt, lCtrl, lShift )   /* User Defined Actions */
         RETURN .f.
      ENDIF

      SWITCH ( key )

      CASE Qt_Key_F3
         IF ! lCtrl .AND. ! lAlt
            ::oFR:find( .f. )
         ENDIF
         EXIT
      CASE Qt_Key_Insert
         IF lCtrl
            ::copy()
         ENDIF
         EXIT
      CASE Qt_Key_Backspace
         IF ! lCtrl .AND. ! lAlt
            IF ::getLineNo() == ::nProtoLine .AND. ::getColumnNo() <= ::nProtoCol + 1
               ::hidePrototype()
            ENDIF
         ENDIF
         EXIT
      CASE Qt_Key_Space
         IF ! lAlt .AND. ! lShift .AND. ! lCtrl
            ::lUpdatePrevWord := .t.
         ENDIF
         EXIT
      CASE Qt_Key_Return
      CASE Qt_Key_Enter
         ::lIndentIt := .t.
         EXIT
      CASE Qt_Key_ParenLeft
         IF ! lCtrl .AND. ! lAlt
            ::loadFuncHelp()                /* Also invokes prototype display */
         ENDIF
         EXIT
      CASE Qt_Key_Escape
      CASE Qt_Key_ParenRight
         IF ! lCtrl .AND. ! lAlt
            ::hidePrototype()
         ENDIF
         EXIT
      CASE Qt_Key_PageUp
         IF lAlt
            ::toPreviousFunction()
         ENDIF
         EXIT
      CASE Qt_Key_PageDown
         IF lAlt
            ::toNextFunction()
         ENDIF
         EXIT
      ENDSWITCH
      EXIT
   CASE QEvent_Enter
   CASE QEvent_FocusIn
      IF key == QEvent_FocusIn
         ::oUpDn:show()
         ::oDK:showSelectedTextToolbar()
      ENDIF
      EXIT
   CASE QEvent_Resize
      ::oUpDn:show()
      ::oDK:showSelectedTextToolbar()
      EXIT
   CASE QEvent_Leave
   CASE QEvent_FocusOut
      EXIT
   CASE QEvent_Wheel
      EXIT
   CASE 1001                                /* Fired from hbqt_hbqplaintextedit.cpp */
      SWITCH p
      CASE QEvent_MouseButtonDblClick
      // ::lCopyWhenDblClicked := .t.
         IF ! ::openHeaderFile()
            ::clickFuncHelp()
         ENDIF
         EXIT
      CASE 21000                            /* Sends Block Info { t,l,b,r,mode,state } hbGetBlockInfo() */
         ::aSelectionInfo := p1
         ::oDK:setButtonState( "SelectionMode", ::aSelectionInfo[ 5 ] > 1 )
         EXIT
      CASE 21011
         ::copyBlockContents()
         EXIT
      CASE 21012
         ::pasteBlockContents()
         EXIT
      CASE 21013
         ::insertBlockContents( p1 )
         EXIT
      CASE 21014                            /* ->hbCut() */
         ::cutBlockContents( p1 )
         EXIT
      CASE 21017                            /* Sends Block Info { t,l,b,r,mode,state } hbGetBlockInfo() */
         ::aViewportInfo := p1
         EXIT
      CASE 21041
         ::qEdit:hbSetFieldsListActive( ::oEM:updateFieldsList( p1 ) )
         EXIT
      CASE 21042
         ::qEdit:hbSetFieldsListActive( ::oEM:updateFieldsList() )
         EXIT
      ENDSWITCH
      EXIT
   ENDSWITCH

   RETURN .F.  /* Important - NEVER CHANGE IT TO .T. */

/*----------------------------------------------------------------------*/

METHOD IdeEdit:zoom( nKey )

   DEFAULT nKey TO 0

   IF nKey == 1
      IF ::currentPointSize + 1 < 30
         ::currentPointSize++
      ENDIF

   ELSEIF nKey == -1
      IF ::currentPointSize - 1 > 3
         ::currentPointSize--
      ENDIF

   ELSEIF nKey == 0
      ::currentPointSize := ::pointSize

   ELSEIF nKey >= 3 .AND. nKey <= 30
      ::currentPointSize := nKey

   ELSE
      RETURN Self

   ENDIF

   ::setFont()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:setFont()
#if 0
   ::qFont := QFont( ::fontFamily, ::currentPointSize )
   //::qFont:setFamily( ::fontFamily )
   ::qFont:setFixedPitch( .t. )
   //::qFont:setPointSize( ::currentPointSize )

   ::qEdit:setFont( ::qFont )
#else                                             // Probably fixes an issue on Linux and Mac
   ::oIde:oFont:oWidget := NIL
   ::oIde:oFont := NIL
   IF ::fontFamily == "Courier New"
      ::fontFamily := "Courier"
   ENDIF
   ::oIde:oFont := XbpFont():new()
   ::oFont:fixed := .t.
   ::oFont:create( hb_ntos( ::currentPointSize ) + "." + ::fontFamily ) //"10.Courier" )

   ::qEdit:setFont( ::oFont:oWidget )
#endif
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:highlightPage()

   IF HB_ISOBJECT( ::oEditor:qHiliter )
      ::qEdit:hbHighlightPage()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:dispStatusInfo()
   LOCAL nMode

   nMode := ::aSelectionInfo[ 5 ]
   ::oDK:setButtonState( "SelectionMode", nMode > 1 )
   ::oDK:setStatusText( SB_PNL_STREAM, iif( nMode == 2, "Column", iif( nMode == 3, "Line", "Stream" ) ) )

   RETURN Self

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_blockContents( aContents )
   LOCAL oldContents
   STATIC contents := {}

   oldContents := contents
   IF HB_ISARRAY( aContents )
      contents := aclone( aContents )
   ENDIF

   RETURN oldContents

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_setQCursor( qEdit, a_ )
   LOCAL qCursor

   IF HB_ISARRAY( a_ )
      qCursor := a_[ 1 ]
      qCursor:movePosition( QTextCursor_Start, QTextCursor_MoveAnchor )
      qCursor:movePosition( QTextCursor_Down , QTextCursor_MoveAnchor, a_[ 2 ] )
      qCursor:movePosition( QTextCursor_Right, QTextCursor_MoveAnchor, a_[ 3 ] )
      qEdit:setTextCursor( qCursor )
      qCursor:endEditBlock()
   ELSE
      qCursor := qEdit:textCursor()
      qCursor:beginEditBlock()
      RETURN { qCursor, qCursor:blockNumber(), qCursor:columnNumber() }
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_qReplaceLine( qCursor, nLine, cLine )

   qCursor:movePosition( QTextCursor_Start      , QTextCursor_MoveAnchor )
   qCursor:movePosition( QTextCursor_Down       , QTextCursor_MoveAnchor, nLine )
   qCursor:movePosition( QTextCursor_StartOfLine, QTextCursor_MoveAnchor )
   qCursor:movePosition( QTextCursor_EndOfLine  , QTextCursor_KeepAnchor )
   qCursor:insertText( cLine )

   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_qPositionCursor( qCursor, nRow, nCol )

   qCursor:movePosition( QTextCursor_Start, QTextCursor_MoveAnchor       )
   qCursor:movePosition( QTextCursor_Down , QTextCursor_MoveAnchor, nRow )
   qCursor:movePosition( QTextCursor_Right, QTextCursor_MoveAnchor, nCol )

   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_invert( cBuffer )
   LOCAL s, i, c, nLen

   s    := ""
   nLen := Len( cBuffer )
   FOR i := 1 TO nLen
      c := substr( cBuffer, i, 1 )
      IF isAlpha( c )
         s += iif( isUpper( c ), lower( c ), upper( c ) )
      ELSE
         s += c
      ENDIF
   NEXT

   RETURN s

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_convertALine( cLine, cMode )

   SWITCH cMode
   CASE "toupper"
      cLine := upper( cLine )
      EXIT
   CASE "tolower"
      cLine := lower( cLine )
      EXIT
   CASE "invert"
      cLine := hbide_invert( cLine )
      EXIT
   CASE "sgl2dbl"
      cLine := strtran( cLine, "'", '"' )
      EXIT
   CASE "dbl2sgl"
      cLine := strtran( cLine, '"', "'" )
      EXIT
   ENDSWITCH

   RETURN cLine

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_qCursorDownInsert( qCursor )
   LOCAL nRow := qCursor:blockNumber()

   qCursor:movePosition( QTextCursor_Down, QTextCursor_MoveAnchor )
   IF qCursor:blockNumber() == nRow
      qCursor:movePosition( QTextCursor_EndOfBlock, QTextCursor_MoveAnchor )
      qCursor:insertBlock()
      qCursor:movePosition( QTextCursor_NextBlock, QTextCursor_MoveAnchor )
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD IdeEdit:copyBlockContents()
   LOCAL nT, nL, nB, nR, nW, i, cLine, nMode, qClip, aCord
   LOCAL cClip := ""

   HB_TRACE( HB_TR_DEBUG, "IdeEdit:copyBlockContents( aCord )" )

   aCord := ::aSelectionInfo

   hbide_normalizeRect( aCord, @nT, @nL, @nB, @nR )
   nMode := aCord[ 5 ]

   ::aBlockCopyContents := {}

   nW := nR - nL
   FOR i := nT TO nB
      cLine := ::getLine( i + 1 )
      cLine := strtran( cLine, chr( 13 ) )
      cLine := strtran( cLine, chr( 10 ) )

      IF nMode == __selectionMode_stream__
         IF aCord[ 1 ] > aCord[ 3 ]  // Selection - bottom to top
            IF i == nT .AND. i == nB
               cLine := substr( cLine, min( aCord[ 2 ], aCord[ 4 ] ) + 1, nW )
            ELSEIF i == aCord[ 1 ]
               cLine := substr( cLine, 1, aCord[ 2 ] )
            ELSEIF i == aCord[ 3 ]
               cLine := substr( cLine, aCord[ 4 ] + 1 )
            ENDIF
         ELSE                        // Selection - top to bottom or same row
            IF i == nT .AND. i == nB
               cLine := substr( cLine, min( aCord[ 2 ], aCord[ 4 ] ) + 1, nW )
            ELSEIF i == aCord[ 1 ]
               cLine := substr( cLine, aCord[ 2 ] + 1 )
            ELSEIF i == aCord[ 3 ]
               cLine := substr( cLine, 1, aCord[ 4 ] )
            ENDIF
         ENDIF

      ELSEIF nMode == __selectionMode_column__
         cLine := pad( substr( cLine, nL + 1, nW ), nW )

      ELSEIF nMode == __selectionMode_line__
         // Nothing to do, complete line is already pulled

      ENDIF

      aadd( ::aBlockCopyContents, cLine )
      cClip += cLine + iif( nT == nB, "", iif( i < nB, hb_eol(), "" ) )
   NEXT

   hbide_blockContents( { nMode, ::aBlockCopyContents } )

   qClip := QClipboard()
   qClip:clear()
   qClip:setText( cClip )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:pasteBlockContents()
   LOCAL i, nCol, qCursor, nMaxCol, aCopy, a_, nPasteMode, nMode

   IF ::lReadOnly
      RETURN Self
   ENDIF

   nMode := ::aSelectionInfo[ 5 ]

   aCopy := hb_ATokens( StrTran( QClipboard():text(), Chr( 13 ) + Chr( 10 ), _EOL ), _EOL )
   IF empty( aCopy )
      RETURN Self
   ENDIF

   nPasteMode := nMode   /* OR Stream - needs to be thought carefully */

   a_:= hbide_blockContents()
   IF !empty( a_ )
      IF ( Len( a_[ 2 ] ) == len( aCopy ) ) .OR. ( len( a_[ 2 ] ) == len( aCopy ) + 1 )
         IF a_[ 2,1 ] == aCopy[ 1 ]
            nPasteMode := a_[ 1 ]
         ENDIF
      ENDIF
   ENDIF

   nPasteMode := iif( empty( nPasteMode ), __selectionMode_stream__, nPasteMode )
   qCursor    := ::qEdit:textCursor()
   nCol       := qCursor:columnNumber()

   qCursor:beginEditBlock()
   //
   SWITCH nPasteMode
   CASE __selectionMode_column__
      FOR i := 1 TO Len( aCopy )
         qCursor:insertText( aCopy[ i ] )
         IF i < Len( aCopy )
            hbide_qCursorDownInsert( qCursor )

            qCursor:movePosition( QTextCursor_EndOfLine, QTextCursor_MoveAnchor )
            nMaxCol := qCursor:columnNumber()
            IF nMaxCol < nCol
               qCursor:insertText( replicate( " ", nCol - nMaxCol ) )
            ENDIF
            qCursor:movePosition( QTextCursor_StartOfLine, QTextCursor_MoveAnchor       )
            qCursor:movePosition( QTextCursor_Right      , QTextCursor_MoveAnchor, nCol )
         ENDIF
      NEXT
      EXIT
   CASE __selectionMode_stream__
      FOR i := 1 TO Len( aCopy )
         qCursor:insertText( aCopy[ i ] )
         IF i < Len( aCopy )
            qCursor:insertText( hb_eol() )
         ENDIF
      NEXT
      EXIT
   CASE __selectionMode_line__
      qCursor:movePosition( QTextCursor_StartOfLine, QTextCursor_MoveAnchor       )
      FOR i := 1 TO Len( aCopy )
         qCursor:insertText( aCopy[ i ] )
         qCursor:insertBlock()
      NEXT
      EXIT
   ENDSWITCH

   qCursor:endEditBlock()
   ::qEdit:ensureCursorVisible()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:insertBlockContents( oKey )  /* Only called if block selection is on */
   LOCAL nT, nL, nB, nR, nW, i, cLine, cKey, qCursor, aCord, qCur

   IF ::lReadOnly
      RETURN Self
   ENDIF

   cKey := chr( hbxbp_QKeyEventToAppEvent( oKey ) )

   aCord := ::aSelectionInfo
   hbide_normalizeRect( aCord, @nT, @nL, @nB, @nR )
   nW := nR - nL

   qCursor := ::qEdit:textCursor()
   qCur := ::qEdit:textCursor()
   qCursor:beginEditBlock()

   IF nW == 0
      FOR i := nT TO nB
         cLine := ::getLine( i + 1 )
         cLine := pad( substr( cLine, 1, nL ), nL ) + cKey + substr( cLine, nL + 1 )
         hbide_qReplaceLine( qCursor, i, cLine )
      NEXT

      hbide_qPositionCursor( qCursor, qCur:blockNumber(), nR + 1 )
   ELSE
      FOR i := nT TO nB
         cLine := ::getLine( i + 1 )
         cLine := pad( substr( cLine, 1, nL ), nL ) + replicate( cKey, nW ) + substr( cLine, nR + 1 )
         hbide_qReplaceLine( qCursor, i, cLine )
      NEXT

      hbide_qPositionCursor( qCursor, qCur:blockNumber(), nR )
   ENDIF

   ::qEdit:setTextCursor( qCursor )
   qCursor:endEditBlock()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:cutBlockContents( k )
   LOCAL nT, nL, nB, nR, i, cLine, qCursor, nSelMode, aCord, qCur

   IF ::lReadOnly
      RETURN Self
   ENDIF

   k := iif( empty( k ), Qt_Key_X, k )
   IF k == Qt_Key_X
      ::copyBlockContents()
   ENDIF
   aCord := ::aSelectionInfo
   hbide_normalizeRect( aCord, @nT, @nL, @nB, @nR )

   nSelMode := aCord[ 5 ]

   qCursor := ::qEdit:textCursor()
   qCur := ::qEdit:textCursor()
   qCursor:beginEditBlock()

   IF k == Qt_Key_Backspace
      IF nSelMode == __selectionMode_column__
         FOR i := nT TO nB
            cLine := ::getLine( i + 1 )
            cLine := pad( substr( cLine, 1, nL - 1 ), nL - 1 ) + substr( cLine, nL + 1 )
            hbide_qReplaceLine( qCursor, i, cLine )
         NEXT
         hbide_qPositionCursor( qCursor, qCur:blockNumber(), nR - 1 )
      ENDIF
   ELSE
      IF k == Qt_Key_Delete .OR. k == Qt_Key_X
         IF nSelMode == __selectionMode_column__
            FOR i := nT TO nB
               cLine := ::getLine( i + 1 )
               cLine := pad( substr( cLine, 1, nL ), nL ) + substr( cLine, nR + 1 )
               hbide_qReplaceLine( qCursor, i, cLine )
            NEXT
            hbide_qPositionCursor( qCursor, qCur:blockNumber(), nL )
            ::qEdit:hbSetSelectionInfo( { nT, nL, nB, nL, __selectionMode_column__ } )

         ELSEIF nSelMode == __selectionMode_stream__
            hbide_qPositionCursor( qCursor, nT, nL )
            qCursor:movePosition( QTextCursor_Down       , QTextCursor_KeepAnchor, nB - nT )
            qCursor:movePosition( QTextCursor_StartOfLine, QTextCursor_KeepAnchor          )
            qCursor:movePosition( QTextCursor_Right      , QTextCursor_KeepAnchor, nR      )
            qCursor:removeSelectedText()
            ::qEdit:hbSetSelectionInfo( { -1, -1, -1, -1, __selectionMode_stream__ } )

         ELSEIF nSelMode == __selectionMode_line__
            hbide_qPositionCursor( qCursor, nT, nL )
            qCursor:movePosition( QTextCursor_Down       , QTextCursor_KeepAnchor, nB - nT + 1 )
            qCursor:movePosition( QTextCursor_StartOfLine, QTextCursor_KeepAnchor          )
            qCursor:removeSelectedText()
            ::qEdit:hbSetSelectionInfo( { -1, -1, -1, -1, __selectionMode_stream__ } )
            ::isLineSelectionON := .f.

         ENDIF
      ENDIF
   ENDIF

   ::qEdit:setTextCursor( qCursor )
   qCursor:endEditBlock()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:blockComment()  /* Toggles the block comments - always inserted at the begining of the line */
   LOCAL nT, nL, nB, nR, nW, i, cLine, qCursor, aCord, nMode, a_
   LOCAL cComment := "// "
   LOCAL nLen := Len( cComment )

   IF ::lReadOnly
      RETURN Self
   ENDIF

   aCord := ::aSelectionInfo
   hbide_normalizeRect( aCord, @nT, @nL, @nB, @nR )
   nW := nR - nL

   IF nW >= 0
      nMode := aCord[ 5 ]
      a_:= hbide_setQCursor( ::qEdit )
      qCursor := a_[ 1 ]

      FOR i := nT TO nB
         cLine := ::getLine( i + 1 )

         SWITCH nMode
         CASE __selectionMode_stream__
         CASE __selectionMode_line__
            IF substr( cLine, 1, nLen ) == cComment
               cLine := substr( cLine, nLen + 1 )
            ELSE
               cLine := cComment + cLine
            ENDIF
            EXIT
         CASE __selectionMode_column__
            IF substr( cLine, nL + 1, nLen ) == cComment
               cLine := pad( substr( cLine, 1, nL ), nL ) + substr( cLine, nL + nLen + 1 )
            ELSE
               cLine := pad( substr( cLine, 1, nL ), nL ) + cComment + substr( cLine, nL + 1 )
            ENDIF
            EXIT
         ENDSWITCH

         hbide_qReplaceLine( qCursor, i, cLine )
      NEXT
#if 1
      hbide_setQCursor( ::qEdit, a_ )
#else
      hbide_qPositionCursor( qCursor, qCur:blockNumber(), nL )
      ::qEdit:hbSetSelectionInfo( { nT, nL, nB, nL, __selectionMode_column__ } )
#endif
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:streamComment()
   LOCAL nT, nL, nB, nR, nW, i, cLine, qCursor, aCord, nMode, a_

   IF ::lReadOnly
      RETURN Self
   ENDIF

   aCord := ::aSelectionInfo
   hbide_normalizeRect( aCord, @nT, @nL, @nB, @nR )
   nW := nR - nL

   IF nW >= 0
      nMode   := aCord[ 5 ]
      a_:= hbide_setQCursor( ::qEdit ) ; qCursor := a_[ 1 ]

      FOR i := nT TO nB
         cLine := ::getLine( i + 1 )

         DO CASE
         CASE nMode == __selectionMode_stream__
            IF i == nT
               cLine := substr( cLine, 1, nL ) + "/* " + substr( cLine, nL + 1 )
            ELSEIF i == nB
               cLine := substr( cLine, 1, nR ) + " */" + substr( cLine, nR + 1 )
            ENDIF

         CASE nMode == __selectionMode_line__
            IF i == nT
               cLine := "/* " + cLine
            ELSEIF i == nB
               cLine += " */"
            ENDIF

         ENDCASE

         hbide_qReplaceLine( qCursor, i, cLine )
      NEXT

      hbide_setQCursor( ::qEdit, a_ )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:handleTab( key )
   LOCAL nT, nL, nB, nR, i, cLine, qCursor, aCord, nMode, nCol, nRow
   LOCAL cComment := space( ::nTabSpaces )
   LOCAL nLen := ::nTabSpaces
   LOCAL nOff := iif( key == Qt_Key_Tab, nLen, -nLen )

   IF ::lReadOnly
      RETURN Self
   ENDIF

   aCord := ::aSelectionInfo
   hbide_normalizeRect( aCord, @nT, @nL, @nB, @nR )
   nMode := aCord[ 5 ]

   qCursor := ::qEdit:textCursor()
   qCursor:beginEditBlock()
   nCol := qCursor:columnNumber()
   nRow := qCursor:blockNumber()

   SWITCH nMode
   CASE __selectionMode_column__
      FOR i := nT TO nB
         cLine := ::getLine( i + 1 )
         IF key == Qt_Key_Tab
            //cLine := substr( cLine, 1, nCol ) + cComment + substr( cLine, nCol + 1 )
            cLine := substr( cLine, 1, nL ) + cComment + substr( cLine, nL + 1 )
         ELSE
            //cLine := substr( cLine, 1, nCol - 3 ) + substr( cLine, nCol + 1 )
            cLine := substr( cLine, 1, nL - 3 ) + substr( cLine, nL + 1 )
         ENDIF
         hbide_qReplaceLine( qCursor, i, cLine )
      NEXT
      hbide_qPositionCursor( qCursor, nRow, max( 0, nCol + nOff ) )
      ::qEdit:hbSetSelectionInfo( { nT, max( 0, nL + nOff ), nB, max( 0, nR + nOff ), __selectionMode_column__ } )
      EXIT
   CASE __selectionMode_stream__
   CASE __selectionMode_line__
      IF nL >= 0    /* Selection is marked */
      // ::cutBlockContents( Qt_Key_Delete )  /* Other editors DO it like but FOR source code it must be different */
         FOR i := nT TO nB
            cLine := ::getLine( i + 1 )
            IF key == Qt_Key_Tab
               cLine := cComment + cLine
            ELSE
               cLine := substr( cLine, nLen + 1 )
            ENDIF
            hbide_qReplaceLine( qCursor, i, cLine )
         NEXT
         hbide_qPositionCursor( qCursor, nRow, max( 0, nCol + nOff ) )
      ELSE
         IF key == Qt_Key_Tab
            qCursor:insertText( Space( ::nTabSpaces ) )
         ELSE
            cLine := ::getLine( nRow + 1 )
            cLine := substr( cLine, 1, nCol - nLen ) + substr( cLine, nCol + 1 )
            hbide_qReplaceLine( qCursor, nRow, cLine )
            hbide_qPositionCursor( qCursor, nRow, max( 0, nCol + nOff ) )
         ENDIF
      ENDIF
      EXIT
   ENDSWITCH
   ::qEdit:setTextCursor( qCursor )
   qCursor:endEditBlock()

   RETURN .t.

/*----------------------------------------------------------------------*/

METHOD IdeEdit:blockIndent( nDirctn )
   IF nDirctn == 1
      ::handleTab( Qt_Key_Tab )
   ELSE
      ::handleTab( Qt_Key_Backtab )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:stringify()
   LOCAL nT, nL, nB, nR, nW, i, cLine, qCursor, aCord, a_, cTkn, cT1

   IF ::lReadOnly
      RETURN Self
   ENDIF
   aCord := ::aSelectionInfo
   hbide_normalizeRect( aCord, @nT, @nL, @nB, @nR )
   nW := nR - nL
   IF nW > 0
      a_:= hbide_setQCursor( ::qEdit ) ; qCursor := a_[ 1 ]
      IF aCord[ 5 ] == __selectionMode_column__
         FOR i := nT TO nB
            cLine := ::getLine( i + 1 )
            cTkn  := SubStr( cLine, nL + 1, nR - nL )
            cT1   := Trim( cTkn )
            cTkn  := '"' + cT1 + '"' + Space( Len( cTkn ) - Len( cT1 ) )
            cLine := SubStr( cLine, 1, nL ) + cTkn + SubStr( cLine, nR + 1 )
            hbide_qReplaceLine( qCursor, i, cLine )
         NEXT
      ELSEIF aCord[ 1 ] == aCord[ 3 ]  /* same line selection */
         cLine := qCursor:block():text()
         cTkn  := SubStr( cLine, nL + 1, nR - nL )
         cTkn  := '"' + cTkn + '"'
         cLine := SubStr( cLine, 1, nL ) + cTkn + SubStr( cLine, nR + 1 )
         hbide_qReplaceLine( qCursor, qCursor:blockNumber(), cLine )
      ENDIF
      hbide_setQCursor( ::qEdit, a_ )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:alignAt( cAt )
   LOCAL nT, nL, nB, nR, nW, i, cLine, qCursor, aCord, a_, nMax, n, c1st, c2nd
   LOCAL  nCol, nMode, aAt, cREdgt, aReg

   IF ::lReadOnly
      RETURN Self
   ENDIF

   nMax := 0
   aCord := ::aSelectionInfo
   hbide_normalizeRect( aCord, @nT, @nL, @nB, @nR )
   nW := nR - nL + 1
   a_:= hbide_setQCursor( ::qEdit ) ; qCursor := a_[ 1 ]

   IF Left( cAt, 2 ) == "\L"          //   \L    align at left column of selected block
      nCol := nL
      nMode := 4
   ELSEIF Left( cAt, 2 ) == "\R"          //   \L    align at right column of selected block
      nCol := nR
      nMode := 5
   ELSEIF Left( cAt, 1 ) == "\"
      cAt := SubStr( cAt, 2 )
      IF IsDigit( Left( cAt,1 ) )         //   \52   align at column 52
         nMode := 2
         nCol  := Val( cAt )
      ELSEIF ! Empty( cAt )               //   \[]   regular expression
         nMode := 3
      ENDIF
   ELSE
      nMode := 1
   ENDIF

   SWITCH nMode
   CASE 1
      IF nR - nL >= 0
         FOR i := nT TO nB
            cLine := ::getLine( i + 1 )
            IF ( n := At( cAt, SubStr( cLine, nL, nW ) ) ) > 0
               nMax := Max( nMax, n )
            ENDIF
         NEXT
      ENDIF
      IF nMax > 0
         nMax += nL - 2
         FOR i := nT TO nB
            cLine := ::getLine( i + 1 )
            IF ( n := At( cAt, SubStr( cLine, nL, nW ) ) ) > 0
               c1st := SubStr( cLine, 1, nL + n - 2 )
               c2nd := SubStr( cLine, nL + n - 1 )
               cLine := PadR( c1st, nMax ) + c2nd
            ENDIF
            hbide_qReplaceLine( qCursor, i, cLine )
         NEXT
      ENDIF
      EXIT
   CASE 2
      IF nR - nL == 0 .AND. nL != nCol  /* Only when a thin vertical line is visible */
         FOR i := nT TO nB
            cLine := ::getLine( i + 1 )
            c1st  := PadR( SubStr( cLine, 1, nL ), nCol - 1 ) + SubStr( cLine, nL + 1 )
            hbide_qReplaceLine( qCursor, i, c1st )
         NEXT
      ENDIF
      EXIT
   CASE 3
      cREdgt := hb_regexComp( cAt )
      IF ! hb_IsRegEx( cREdgt )
         Alert( "Not a valid RegEx" )
         RETURN Self
      ENDIF
      aAt := {}
      IF nR - nL == 0
         FOR i := nT TO nB
            cLine := ::getLine( i + 1 )
            //                exp, string, lMatchCase, lNewLine, nMaxMatch, nMatchWhich, lMatchOnly
            IF ! Empty( aReg := hb_regExAll( cREdgt, SubStr( cLine, nL + 1 ), .F., .F., 0, 1, .F.  ) )
               AAdd( aAt, nL + aReg[ 1, 2 ] )
            ELSE
               AAdd( aAt, 0 )
            ENDIF
         NEXT
         AEval( aAt, {|e| nMax := Max( nMax, e ) } )
         FOR i := nT TO nB
            cLine := ::getLine( i + 1 )
            c1st  := PadR( SubStr( cLine, 1, nL ), nMax - 1 ) + LTrim( SubStr( cLine, nL + 1 ) )
            hbide_qReplaceLine( qCursor, i, c1st )
         NEXT
      ELSE
         FOR i := nT TO nB
            cLine := ::getLine( i + 1 )
            //                exp, string, lMatchCase, lNewLine, nMaxMatch, nMatchWhich, lMatchOnly
            IF ! Empty( aReg := hb_regExAll( cREdgt, SubStr( cLine, nL, nW ), .F., .F., 0, 1, .F.  ) )
               AAdd( aAt, nL + aReg[ 1, 2 ] - 1 )
            ELSE
               AAdd( aAt, 0 )
            ENDIF
         NEXT
         AEval( aAt, {|e| nMax := Max( nMax, e ) } )
         IF nMax > 0
            n := 0
            FOR i := nT TO nB
               n++
               IF aAt[ n ] > 0
                  cLine := ::getLine( i + 1 )
                  c1st  := SubStr( cLine, 1, aAt[ n ] - 1 )
                  c2nd  := SubStr( cLine, aAt[ n ] )
                  cLine := PadR( c1st, nMax - 1 ) + c2nd
                  hbide_qReplaceLine( qCursor, i, cLine )
               ENDIF
            NEXT
         ENDIF

      ENDIF
      EXIT
   CASE 4                                         /* align left text right to the selection line */
      IF nR - nL == 0                             /* Only when a thin vertical line is visible */
         FOR i := nT TO nB
            cLine := ::getLine( i + 1 )
            c1st  := PadR( SubStr( cLine, 1, nL ), nL ) + LTrim( SubStr( cLine, nL + 1 ) )
            hbide_qReplaceLine( qCursor, i, c1st )
         NEXT
      ENDIF
      EXIT
   CASE 5                                         /* align right text left to the selection line */
      IF nR - nL == 0                             /* Only when a thin vertical line is visible */
         FOR i := nT TO nB
            cLine := ::getLine( i + 1 )
            c1st  := PadL( Trim( SubStr( cLine, 1, nL ) ), nL ) + SubStr( cLine, nL + 1 )
            hbide_qReplaceLine( qCursor, i, c1st )
         NEXT
      ENDIF
      EXIT
   ENDSWITCH

   hbide_setQCursor( ::qEdit, a_ )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:blockConvert( cMode )
   LOCAL nT, nL, nB, nR, nW, i, cLine, qCursor, aCord, a_, nMode

   IF ::lReadOnly
      RETURN Self
   ENDIF

   aCord := ::aSelectionInfo
   hbide_normalizeRect( aCord, @nT, @nL, @nB, @nR )
   nW := nR - nL

   IF nW >= 0
      nMode := aCord[ 5 ]
      a_:= hbide_setQCursor( ::qEdit ) ; qCursor := a_[ 1 ]

      FOR i := nT TO nB
         cLine := ::getLine( i + 1 )

         DO CASE
         CASE nMode == __selectionMode_stream__
            IF nT == nB
               cLine := substr( cLine, 1, nL ) + hbide_convertALine( substr( cLine, nL + 1, nW ), cMode ) + substr( cLine, nL + 1 + nW )
            ELSE
               IF i == nT
                  cLine := substr( cLine, 1, nL ) + hbide_convertALine( substr( cLine, nL + 1 ), cMode )
               ELSEIF i == nB
                  cLine := hbide_convertALine( substr( cLine, 1, nR ), cMode ) + substr( cLine, nR + 1 )
               ELSE
                  cLine := hbide_convertALine( cLine, cMode )
               ENDIF
            ENDIF

         CASE nMode == __selectionMode_column__
            cLine := pad( substr( cLine, 1, nL ), nL ) + hbide_convertALine( pad( substr( cLine, nL + 1, nW ), nW ), cMode ) + substr( cLine, nR + 1 )

         CASE nMode == __selectionMode_line__
            cLine := hbide_convertALine( cLine, cMode )

         ENDCASE

         hbide_qReplaceLine( qCursor, i, cLine )
      NEXT

      hbide_setQCursor( ::qEdit, a_ )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:getSelectedText()
   LOCAL nT, nL, nB, nR, nW, i, cLine, nMode, cClip := "", aCord

   HB_TRACE( HB_TR_DEBUG, "IdeEdit:getSelectedText()", ProcName( 1 ), procName( 2 ) )

   aCord := ::aSelectionInfo
   hbide_normalizeRect( aCord, @nT, @nL, @nB, @nR )
   nMode := aCord[ 5 ]

   nW := nR - nL
   FOR i := nT TO nB
      cLine := ::getLine( i + 1 )

      IF nMode == __selectionMode_stream__
         IF i == nT .AND. i == nB
            cLine := substr( cLine, nL + 1, nR - nL )
         ELSEIF i == nT
            cLine := substr( cLine, nL + 1 )
         ELSEIF i == nB
            cLine := substr( cLine, 1, nR + 1 )
         ENDIF

      ELSEIF nMode == __selectionMode_column__
         cLine := pad( substr( cLine, nL + 1, nW ), nW )

      ELSEIF nMode == __selectionMode_line__
         // Nothing to do, complete line is already pulled

      ENDIF

      cClip += cLine + iif( i < nB, hb_eol(), "" )
   NEXT

   RETURN cClip

/*----------------------------------------------------------------------*/

METHOD IdeEdit:caseUpper()
   RETURN ::blockConvert( "toupper" )

/*----------------------------------------------------------------------*/

METHOD IdeEdit:caseLower()
   RETURN ::blockConvert( "tolower" )

/*----------------------------------------------------------------------*/

METHOD IdeEdit:caseInvert()
   RETURN ::blockConvert( "invert" )

/*----------------------------------------------------------------------*/

METHOD IdeEdit:convertQuotes()
   RETURN ::blockConvert( "dbl2sgl" )

/*----------------------------------------------------------------------*/

METHOD IdeEdit:convertDQuotes()
   RETURN ::blockConvert( "sgl2dbl" )

/*----------------------------------------------------------------------*/

METHOD IdeEdit:currentFunctionIndex()
   LOCAL n := -1, nCurLine

   IF !empty( ::aTags )
      nCurLine := ::getLineNo()
      IF Len( ::aTags ) == 1
         n := 1
      ELSEIF ( n := ascan( ::aTags, {|e_| e_[ 3 ] >= nCurLine } ) ) == 0
         n := Len( ::aTags )
      ELSEIF n > 0
         n--
      ENDIF
   ENDIF

   RETURN n

/*----------------------------------------------------------------------*/

METHOD IdeEdit:toNextFunction()
   LOCAL n

   IF ( n := ::currentFunctionIndex() ) >= 0
      IF n < Len( ::aTags )
         IF ::find( ::aTags[ n+1, 8 ], QTextDocument_FindCaseSensitively )
            ::qEdit:centerCursor()
            ::down()
         ENDIF
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:toPreviousFunction()
   LOCAL n

   IF ( n := ::currentFunctionIndex() ) > 1
      IF ::find( ::aTags[ n-1, 8 ], QTextDocument_FindCaseSensitively )
         ::qEdit:centerCursor()
         ::down()
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:markCurrentFunction()
   LOCAL n

   IF ::oFuncDock:oWidget:isVisible()
      IF ::nPrevLineNo1 != ::getLineNo()
         ::nPrevLineNo1 := ::getLineNo()
         IF ( n := ::currentFunctionIndex() ) > 0
            ::oIde:oFuncList:setItemColorFG( ::aTags[ n,7 ], { 255,0,0 } )
            ::oIde:oFuncList:setVisible( ::aTags[ n,7 ] )
         ENDIF
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:presentSkeletons()
   ::oSK:selectByMenuAndPostText( ::qEdit )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:toggleCurrentLineHighlightMode()
   ::qEdit:hbHighlightCurrentLine( ::lCurrentLineHighlightEnabled )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:toggleLineNumbers()
   ::qEdit:hbNumberBlockVisible( ::lLineNumbersVisible )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:toggleHorzRuler()
   ::qEdit:hbHorzRulerVisible( ::lHorzRulerVisible )
   RETURN Self

/*----------------------------------------------------------------------*/
/* Fired by icon */

METHOD IdeEdit:toggleSelectionMode()
   ::qEdit:hbSetSelectionMode( iif( ::oDK:setButtonState( "SelectionMode" ), 2, 1 ), .f. )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:toggleStreamSelectionMode()
   ::qEdit:hbSetSelectionMode( 1, .t. )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:toggleColumnSelectionMode()
   ::qEdit:hbSetSelectionMode( 2, .t. )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:toggleLineSelectionMode()
   ::qEdit:hbSetSelectionMode( 3, .t. )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:clearSelection()
   ::qEdit:hbSetSelectionMode( 0, .t. )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:togglePersistentSelection()
   ::qEdit:hbTogglePersistentSelection()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:toggleCodeCompetion()
   ::qEdit:hbToggleCodeCompetion()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:toggleCompetionTips()
   ::qEdit:hbToggleCompetionTips()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:reload()
   LOCAL lLoad := .t.

   IF ::oEditor:qDocument:isModified()
      lLoad := hbide_getYesNo( "Source is in modified state", "Reload it anyway?", "Reload" )
   ENDIF
   IF lLoad
      ::oEditor:reload()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:redo()
   ::qEdit:redo()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:undo()
   ::qEdit:undo()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:cut()
   IF ::lReadOnly
      RETURN Self
   ENDIF
   ::cutBlockContents( Qt_Key_X )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:copy()
   ::copyBlockContents()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:paste()

   IF ::lReadOnly
      RETURN Self
   ENDIF
   IF ::aSelectionInfo[ 1 ] > -1
      ::cutBlockContents( Qt_Key_Delete )
   ENDIF
   ::pasteBlockContents()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:selectAll()
   ::qEdit:hbSelectAll()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:setReadOnly( lReadOnly )
   IF ::oEditor:lReadOnly
      lReadOnly := .t.
   ELSE
      IF ! HB_ISLOGICAL( lReadOnly )
         lReadOnly := ! ::qEdit:isReadOnly()
      ENDIF
   ENDIF
   ::lReadOnly := lReadOnly
   ::qEdit:setReadOnly( lReadOnly )
   ::oEditor:setTabImage()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:relayMarkButtons()
   LOCAL oBtn
   FOR EACH oBtn IN ::aMarkTBtns
      oBtn:hide()
   NEXT
   FOR EACH oBtn IN ::aBookMarks
      ::aMarkTBtns[ oBtn:__enumIndex() ]:show()
   NEXT
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:setTooltipMark( nIndex )
   LOCAL oBlock

   IF Len( ::aBookMarks ) >= nIndex
      oBlock := ::qEdit:document():findBlockByNumber( ::aBookMarks[ nIndex ] - 1 )
      IF oBlock:isValid()
         ::aMarkTBtns[ nIndex ]:setTooltip( hb_ntos( ::aBookMarks[ nIndex ] ) + " : " + oBlock:text() )
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:gotoMark( nIndex )

   IF Len( ::aBookMarks ) >= nIndex
      ::qEdit:hbGotoBookmark( ::aBookMarks[ nIndex ] )
      ::qEdit:centerCursor()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:setNewMark()
   LOCAL qCursor, nBlock, n

   IF !( qCursor := ::qEdit:textCursor() ):isNull()
      nBlock := qCursor:blockNumber() + 1

      IF ( n := ascan( ::aBookMarks, nBlock ) ) > 0
         hb_adel( ::aBookMarks, n, .t. )
         ::aMarkTBtns[ Len( ::aBookMarks ) + 1 ]:hide()
      ELSE
         IF Len( ::aBookMarks ) == 6
            RETURN Self
         ENDIF
         aadd( ::aBookMarks, nBlock )
         n := Len( ::aBookMarks )
         ::aMarkTBtns[ n ]:show()
      ENDIF

      ::qEdit:hbBookMarks( nBlock )
      ::qEdit:repaint()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:setLineNumbersBkColor( nR, nG, nB )
   ::qEdit:hbSetLineAreaBkColor( QColor( nR, nG, nB ) )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:setCurrentLineColor( nR, nG, nB )
   ::qEdit:hbSetCurrentLineColor( QColor( nR, nG, nB ) )
   RETURN Self

/*----------------------------------------------------------------------*/
/* TO BE EXTENDED */
METHOD IdeEdit:find( cText, nPosFrom )
   LOCAL lFound, nPos
   LOCAL qCursor := ::getCursor()

   nPos := qCursor:position()
   IF HB_ISNUMERIC( nPosFrom )
      qCursor:setPosition( nPosFrom )
   ENDIF
   ::qEdit:setTextCursor( qCursor )
   IF ! ( lFound := ::qEdit:find( cText, QTextDocument_FindCaseSensitively ) )
      IF ! HB_ISNUMERIC( nPosFrom )
         lFound := ::qEdit:find( cText, QTextDocument_FindBackward + QTextDocument_FindCaseSensitively )
      ENDIF
   ENDIF

   IF ! lFound
      qCursor:setPosition( nPos )
      ::qEdit:setTextCursor( qCursor )
   ELSE
      ::qEdit:centerCursor()
   ENDIF

   RETURN lFound

/*----------------------------------------------------------------------*/
/*  nFlags will decide the position, case sensitivity and direction
 */
METHOD IdeEdit:findEx( cText, nFlags, nStart )
   LOCAL qCursor, lFound, nPos

   DEFAULT cText  TO ::getSelectedText()
   IF Empty( cText )
      RETURN Self
   ENDIF
   DEFAULT nFlags TO 0
   DEFAULT nStart TO 0

   qCursor := ::getCursor()
   nPos    := qCursor:position()

   IF nStart == 0
      // No need to move cursor
   ELSEIF nStart == 1
      ::qEdit:moveCursor( QTextCursor_Start )
   ELSEIF nStart == 2
      ::qEdit:moveCursor( QTextCursor_End )
   ENDIF

   IF ( lFound := ::qEdit:find( cText, nFlags ) )
      ::qEdit:centerCursor()
      qCursor := ::qEdit:textCursor()

      ::qEdit:hbSetSelectionInfo( { qCursor:blockNumber(), qCursor:columnNumber() - Len( cText ), ;
                                    qCursor:blockNumber(), qCursor:columnNumber(), 1, .t., .f. } )
      qCursor:clearSelection()
   ELSE
      qCursor:setPosition( nPos )
      ::qEdit:setTextCursor( qCursor )
   ENDIF

   RETURN lFound

/*----------------------------------------------------------------------*/

METHOD IdeEdit:unHighlight()
   LOCAL qCursor, nPos, lModified

   IF ::isHighLighted
      ::isHighLighted := .f.
      qCursor := ::getCursor()
      nPos := qCursor:position()
      lModified := ::qEdit:document():isModified()
      ::qEdit:undo()
      IF ! lModified
         ::qEdit:document():setModified( .f. )
         ::oEditor:setTabImage( ::qEdit )
      ENDIF
      qCursor:setPosition( nPos )
      ::qEdit:setTextCursor( qCursor )
      RETURN .t.
   ENDIF

   RETURN .f.

/*----------------------------------------------------------------------*/

METHOD IdeEdit:highlightAll( cText )
   LOCAL qDoc, qFormat, qCursor, qFormatHL, qCur, lModified

   IF ::unHighLight()
      RETURN Self
   ENDIF

   DEFAULT cText TO ::getSelectedText()
   IF Empty( cText )
      RETURN Self
   ENDIF
   ::isHighLighted := .t.

   qDoc := ::oEditor:qDocument
   lModified := ::qEdit:document():isModified()

   qCur := ::getCursor()
   qCur:beginEditBlock()

   qCursor   := QTextCursor( qDoc )
   qFormat   := qCursor:charFormat()
   qFormatHL := qFormat
   qFormatHL:setBackground( QBrush( QColor( Qt_yellow ) ) )

   DO WHILE .t.
      qCursor := qDoc:find( cText, qCursor, 0 )
      IF qCursor:isNull()
         EXIT
      ENDIF
      qCursor:mergeCharFormat( qFormatHL )
   ENDDO
   qCur:endEditBlock()

   IF ! lModified
      ::qEdit:document():setModified( .f. )
      ::oEditor:setTabImage( ::qEdit )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:refresh()
   ::qEdit:hbRefresh()
   RETURN Self

/*----------------------------------------------------------------------*/
//                       TBrowse Like Navigation
/*----------------------------------------------------------------------*/

METHOD IdeEdit:home()
   ::qEdit:hbApplyKey( Qt_Key_Home )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:end()
   ::qEdit:hbApplyKey( Qt_Key_End )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:down()
   ::qEdit:hbApplyKey( Qt_Key_Down )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:up()
   ::qEdit:hbApplyKey( Qt_Key_Up )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:goBottom()
   ::qEdit:hbApplyKey( Qt_Key_End, Qt_ControlModifier )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:goTop()
   ::qEdit:hbApplyKey( Qt_Key_Home, Qt_ControlModifier )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:left()
   ::qEdit:hbApplyKey( Qt_Key_Left )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:right()
   ::qEdit:hbApplyKey( Qt_Key_Right )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:panEnd()
   LOCAL qCursor := ::getCursor()
   LOCAL cLine := ::getLine()
   ::qEdit:hbGetViewportInfo()
   IF Len( cLine ) - ::aViewportInfo[ 2 ] > ::aViewportInfo[ 4 ]
      qCursor:movePosition( QTextCursor_Right, QTextCursor_MoveAnchor, Len( cLine ) - ::aViewportInfo[ 2 ] )
   ELSE
      qCursor:movePosition( QTextCursor_EndOfLine )
   ENDIF
   ::qEdit:setTextCursor( qCursor )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:panHome()
   LOCAL qCursor := ::getCursor()
   ::qEdit:hbGetViewportInfo()
   IF ::aViewportInfo[ 2 ] == 0
      qCursor:movePosition( QTextCursor_StartOfLine )
   ELSE
      qCursor:movePosition( QTextCursor_Left, QTextCursor_MoveAnchor, qCursor:columnNumber() - ::aViewportInfo[ 2 ] )
   ENDIF
   ::qEdit:setTextCursor( qCursor )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:pageUp()
   ::qEdit:hbApplyKey( Qt_Key_PageUp )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:pageDown()
   ::qEdit:hbApplyKey( Qt_Key_PageDown )
   RETURN Self

/*----------------------------------------------------------------------*/
//
/*----------------------------------------------------------------------*/

METHOD IdeEdit:printPreview()
   LOCAL qDlg := QPrintPreviewDialog( ::oDlg:oWidget )

   qDlg:setWindowTitle( "hbIDE Preview Dialog" )
   qDlg:connect( "paintRequested(QPrinter*)", {|p| ::paintRequested( p ) } )
 * qDlg:setWindowState( Qt_WindowMaximized )
   qDlg:exec()
   qDlg:disconnect( "paintRequested(QPrinter*)" )

   RETURN self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:paintRequested( qPrinter )
   ::qEdit:print( qPrinter )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:upperCaseKeywords()
   LOCAL qDoc, cText, cRegEx, aMatches, aMatch
   STATIC b_

   qDoc := ::qEdit:document()

   IF !( qDoc:isEmpty() )
      qDoc:setUndoRedoEnabled( .f. )

      cText := qDoc:toPlainText()

      IF Empty( b_ )
         b_:= { 'function','procedure','thread','return','static','local','default', ;
                'if','else','elseif','endif','end', ;
                'docase','case','endcase','otherwise', ;
                'switch','endswitch', ;
                'do','while','exit','enddo','loop',;
                'for','each','next','step','to','in',;
                'with','replace','object','endwith','request',;
                'nil','and','or','in','not','self',;
                'class','endclass','method','data','var','destructor','inline','assign','access',;
                'inherit','init','create','virtual','message', 'from', 'setget',;
                'begin','sequence','try','catch','always','recover','hb_symbol_unused', ;
                'error','handler','private','public' }
      ENDIF
      cRegEx := ""
      aeval( b_, {|e| cRegEx += iif( empty( cRegEx ), "", "|" ) + "\b" + e + "\b" } )

      aMatches := hb_regExAll( cRegEx, cText, .f., .f., 0, 1, .f. )

      IF ! empty( aMatches )
         FOR EACH aMatch IN aMatches
            cText := stuff( cText, aMatch[ 2 ], aMatch[ 3 ] - aMatch[ 2 ] + 1, upper( aMatch[ 1 ] ) )
         NEXT
      ENDIF

      qDoc:clear()
      qDoc:setPlainText( cText )

      qDoc:setUndoRedoEnabled( .t. )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:formatBraces( nMode )
   LOCAL qDoc, cText

   IF ! HB_ISNUMERIC( nMode )
      nMode := 0
   ENDIF
   qDoc := ::qEdit:document()


   IF ! qDoc:isEmpty()
      qDoc:setUndoRedoEnabled( .f. )

      cText := qDoc:toPlainText()

      SWITCH nMode
      CASE 0
         cText := hbide_formatBrace( cText, "(", ")", 6 )
         cText := hbide_formatBrace( cText, "{", "}", 6 )
         cText := hbide_formatBrace( cText, "[", "]", 6 )
         EXIT
      CASE 1
         cText := hbide_formatOperators( cText, { ":=", "==", ">=", "<=", "!=", "<>", ".AND.", ".OR.", ".NOT." }, 1 )
         EXIT
      CASE 2
         cText := hbide_formatCommas( cText, 1 )
         EXIT
      ENDSWITCH

      qDoc:clear()
      qDoc:setPlainText( cText )

      qDoc:setUndoRedoEnabled( .t. )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:removeTrailingSpaces()
   LOCAL qDoc, cText, a_, s

   qDoc := ::qEdit:document()
   IF !( qDoc:isEmpty() )
      qDoc:setUndoRedoEnabled( .f. )
      cText := qDoc:toPlainText()
      a_:= hbide_memoToArray( cText )
      FOR EACH s IN a_
         s := trim( s )
      NEXT
      cText := hbide_arrayToMemo( a_ )
      qDoc:clear()
      qDoc:setPlainText( cText )
      qDoc:setUndoRedoEnabled( .t. )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:tabs2spaces()
   LOCAL qDoc, cText, cSpaces

   qDoc := ::qEdit:document()
   IF !( qDoc:isEmpty() )
      cSpaces := space( ::nTabSpaces )

      qDoc:setUndoRedoEnabled( .f. )

      cText := qDoc:toPlainText()
      qDoc:clear()
      qDoc:setPlainText( strtran( cText, chr( 9 ), cSpaces ) )

      qDoc:setUndoRedoEnabled( .t. )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:spaces2tabs()
   LOCAL qDoc, cText, cSpaces

   qDoc := ::qEdit:document()
   IF !( qDoc:isEmpty() )
      cSpaces := space( ::nTabSpaces )

      qDoc:setUndoRedoEnabled( .f. )

      cText := qDoc:toPlainText()
      qDoc:clear()
      qDoc:setPlainText( strtran( cText, cSpaces, chr( 9 ) ) )

      qDoc:setUndoRedoEnabled( .t. )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:duplicateLine()
   ::qEdit:hbDuplicateLine()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:deleteLine()
   ::qEdit:hbDeleteLine()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:moveLine( nDirection )
   ::qEdit:hbMoveLine( nDirection )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:getText()
   RETURN ::qEdit:textCursor():selectedText()

/*----------------------------------------------------------------------*/

METHOD IdeEdit:getWord( lSelect )
   LOCAL cText, qCursor := ::qEdit:textCursor()

   DEFAULT lSelect TO .F.

   qCursor:select( QTextCursor_WordUnderCursor )
   cText := qCursor:selectedText()

   IF lSelect
      ::qEdit:setTextCursor( qCursor )
   ENDIF
   RETURN cText

/*----------------------------------------------------------------------*/

METHOD IdeEdit:goto( nLine )
   LOCAL nRows, qGo
   LOCAL qCursor := ::qEdit:textCursor()

   IF empty( nLine )
      nRows := ::qEdit:blockCount()
      nLine := qCursor:blockNumber()

      qGo := QInputDialog( ::oDlg:oWidget )
      qGo:setInputMode( 1 )
      qGo:setIntMinimum( 1 )
      qGo:setIntMaximum( nRows )
      qGo:setIntValue( nLine + 1 )
      qGo:setLabelText( "Goto Line Number [1-" + hb_ntos( nRows ) + "]" )
      qGo:setWindowTitle( "Harbour" )

      ::oIde:setPosByIniEx( qGo, ::oINI:cGotoDialogGeometry )
      qGo:exec()
      ::oIde:oINI:cGotoDialogGeometry := hbide_posAndSize( qGo )
      nLine := qGo:intValue()
      ::qEdit:setFocus()
   ENDIF

   qCursor:movePosition( QTextCursor_Start )
   qCursor:movePosition( QTextCursor_Down, QTextCursor_MoveAnchor, nLine - 1 )
   ::qEdit:setTextCursor( qCursor )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:getLine( nLine, lSelect )
   LOCAL cText, qCursor := ::qEdit:textCursor()

   DEFAULT nLine   TO qCursor:blockNumber() + 1
   DEFAULT lSelect TO .F.

   IF nLine != qCursor:blockNumber() + 1
      qCursor:movePosition( QTextCursor_Start )
      qCursor:movePosition( QTextCursor_Down, QTextCursor_MoveAnchor, nLine - 1 )
   ENDIF

   qCursor:select( QTextCursor_LineUnderCursor )
   cText := qCursor:selectedText()
   IF lSelect
      ::qEdit:setTextCursor( qCursor )
   ENDIF

   RETURN cText

/*----------------------------------------------------------------------*/

METHOD IdeEdit:getColumnNo()
   RETURN ::qEdit:textCursor():columnNumber() + 1

/*----------------------------------------------------------------------*/

METHOD IdeEdit:getLineNo()
   RETURN ::qEdit:textCursor():blockNumber() + 1

/*----------------------------------------------------------------------*/

METHOD IdeEdit:insertSeparator( cSep )
   LOCAL qCursor := ::qEdit:textCursor()

   IF empty( cSep )
      cSep := ::cSeparator
   ENDIF
   WITH OBJECT qCursor
      :beginEditBlock()
      :movePosition( QTextCursor_StartOfBlock )
      :insertBlock()
      :movePosition( QTextCursor_PreviousBlock )
      :insertText( cSep )
      :movePosition( QTextCursor_NextBlock )
      :movePosition( QTextCursor_StartOfBlock )
      :endEditBlock()
   ENDWITH
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:insertText( cText )
   LOCAL qCursor, nL, nB

   IF HB_ISSTRING( cText ) .AND. !Empty( cText )
      qCursor := ::qEdit:textCursor()

      nL := Len( cText )
      nB := qCursor:position() + nL

      WITH OBJECT qCursor
         :beginEditBlock()
         :removeSelectedText()
         :insertText( cText )
         :setPosition( nB )
         :endEditBlock()
      ENDWITH
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/
/* called via qDocument:contentsChange(*/

METHOD IdeEdit:reformatLine( nPos, nDeleted, nAdded )
   LOCAL cProto, nRows, nCols, nCol, cWord
   LOCAL cPWord, cPPWord, nPostn, nLine, nLPrev, nLPrevPrev, nCPrev, nCPrevPrev, nOff, cCased
   LOCAL cCWord  := ""
   LOCAL cRest   := ""
   LOCAL qCursor := ::qEdit:textCursor()

   IF ::oEditor:lIsPRG
      qCursor:joinPreviousEditBlock()
      IF ! hbide_IsInCommentOrString( qCursor:block():text(), qCursor:columnNumber() )

         nPostn := qCursor:position()
         nLine  := qCursor:blockNumber()

         IF nPos == -1
            cCWord := " "
         ELSE
            IF ( nCol := nPostn - qCursor:block():position() ) > 0
               cCWord := SubStr( qCursor:block():text(), nCol, 1 )
            ENDIF
         ENDIF

         IF qCursor:movePosition( QTextCursor_EndOfLine, QTextCursor_KeepAnchor ) .AND. qCursor:position() > nPostn
            cRest := qCursor:selectedText()
         ENDIF
         qCursor:clearSelection()
         qCursor:setPosition( nPostn )

         qCursor:movePosition( QTextCursor_PreviousWord, QTextCursor_MoveAnchor, 1 )
         nLPrev := qCursor:blockNumber()
         IF nLPrev == nLine
            nCPrev := qCursor:position() - qCursor:block():position()
            qCursor:select( QTextCursor_WordUnderCursor )
            cPWord := qCursor:selectedText()
            qCursor:clearSelection()
            qCursor:setPosition( nPostn )

            qCursor:movePosition( QTextCursor_PreviousWord, QTextCursor_MoveAnchor, 2 )
            nLPrevPrev := qCursor:blockNumber()
            IF nLPrevPrev == nLine
               nCPrevPrev := qCursor:position() - qCursor:block():position()
               qCursor:select( QTextCursor_WordUnderCursor )
               cPPWord := qCursor:selectedText()
            ELSE
               nCPrevPrev := -1
               cPPWord := ""
            ENDIF
            qCursor:clearSelection()
            qCursor:setPosition( nPostn )

            /* Group I operations */
            IF cPWord == "." .AND. cPPWord $ ::hLogicals     /* ALWAYS */
               IF ! ::oINI:lSupressHbKWordsToUpper
                  WITH OBJECT qCursor
                     :movePosition( QTextCursor_PreviousWord, QTextCursor_MoveAnchor, 2 )
                     :select( QTextCursor_WordUnderCursor )
                     :removeSelectedText()
                     :insertText( upper( cPPWord ) )
                     :setPosition( nPostn )
                  ENDWITH
               ENDIF

            ELSEIF cPWord == ":=" .AND. cCWord == "=" .AND. nAdded == 1
               IF ::oINI:lISOperator
                  qCursor:insertText( " " )
               ENDIF
               IF ::oINI:lISAlignAssign
                  // look for previous lines and IF 2nd keyword is assignment operator then align both TO same offset
                  hbide_alignAssignments( qCursor )
               ENDIF

            ELSEIF ::oINI:lISCodeBlock .AND. Right( cPWord, 2 ) == "{|" .AND. cCWord == "|" .AND. nAdded == 1 .AND. Empty( cRest )
               qCursor:insertText( "|  }" )
               qCursor:setPosition( nPostn )

            ELSEIF cCWord == "(" .AND. ( hbide_isHarbourFunction( cPPWord, @cCased ) .OR. hbide_isQtFunction( cPPWord, @cCased ) .OR. hbide_isUserFunction( cPPWord, @cCased ) )
               hbide_replaceWord( qCursor, 2, cCased, nPostn )
               IF ::oINI:lISClosingP
                  IF cCWord == "(" .AND. nAdded == 1
                     qCursor:insertText( ")" )
                     IF ::oINI:lISSpaceP
                        qCursor:setPosition( nPostn )
                        qCursor:insertText( "  " )
                        nPostn++
                     ENDIF
                  ENDIF
               ENDIF
               qCursor:setPosition( nPostn )

            ELSEIF cCWord == " " .AND. hbide_isUserFunction( cPPWord, @cCased ) /* User dictionaries : only keywords */
               hbide_replaceWord( qCursor, 2, cCased, nPostn )

            ELSEIF cCWord == " " .AND. cPPWord != "#" .AND. hbide_isHarbourKeyword( cPWord )
               IF ! ::oINI:lSupressHbKWordsToUpper
                  WITH OBJECT qCursor
                     :movePosition( QTextCursor_PreviousWord, QTextCursor_MoveAnchor, 1 )
                     :select( QTextCursor_WordUnderCursor )
                     :removeSelectedText()
                     :insertText( upper( cPWord ) )
                     :setPosition( nPostn )
                  ENDWITH
               ENDIF

            ENDIF

            IF cCWord == " " .AND. ! Empty( cPWord ) .AND. ! ( Left( cPWord, 1 ) $ "`~!@#$%^&*()+1234567890-=+[]{}|\':;?/>.<," )
               IF Len( cPWord ) > 3
                  IF ! cPWord $ ::oEM:hEditingWords
                     ::oEM:hEditingWords[ cPWord ] := cPWord
                     //::oEM:updateCompleter()
                     ::oEM:updateCompleterByEditingWords( cPWord )
                  ENDIF
               ENDIF

            ELSEIF cCWord $ ",:" .AND. ! Empty( cPPWord ) .AND. ! ( Left( cPPWord, 1 ) $ "`~!@#$%^&*()+1234567890-=+[]{}|\':;?/>.<," )
               IF Len( cPPWord ) > 3
                  IF ! cPPWord $ ::oEM:hEditingWords
                     ::oEM:hEditingWords[ cPPWord ] := cPPWord
                     //::oEM:updateCompleter()
                     ::oEM:updateCompleterByEditingWords( cPPWord )
                  ENDIF
               ENDIF

            ENDIF

            /* Group II operations */
            IF empty( cPPWord ) .AND. cCWord == " "
               IF hbide_isStartingKeyword( cPWord, ::oIde )
                  WITH OBJECT qCursor                                       /* FUNCTION PROCEDURE CLASS */
                     :movePosition( QTextCursor_StartOfBlock )
                     :movePosition( QTextCursor_NextCharacter, QTextCursor_KeepAnchor, nCPrev )
                     :removeSelectedText()
                     :movePosition( QTextCursor_NextCharacter, QTextCursor_MoveAnchor, Len( cPWord ) + 1 )
                  ENDWITH

               ELSEIF hbide_isMinimumIndentableKeyword( cPWord, ::oIde ) .AND. ::oINI:lAutoIndent  /* LOCAL STATIC DEFAULT PRIVATE PUBLIC ENDCLASS RETURN */
                  WITH OBJECT qCursor
                     :movePosition( QTextCursor_StartOfBlock )
                     :movePosition( QTextCursor_NextCharacter, QTextCursor_KeepAnchor, nCPrev )
                     :removeSelectedText()
                     :insertText( space( ::nTabSpaces ) )
                     :movePosition( QTextCursor_NextCharacter, QTextCursor_MoveAnchor, Len( cPWord ) + 1 )
                  ENDWITH

               ELSEIF hbide_isIndentableKeyword( cPWord, ::oIde ) .AND. ::oINI:lAutoIndent         /* IF SWITCH FOR DO */
                  IF nCPrev < ::nTabSpaces
                     nOff := ::nTabSpaces - nCPrev
                     WITH OBJECT qCursor
                        :movePosition( QTextCursor_StartOfBlock )
                        :insertText( space( nOff ) )
                        :setPosition( nPostn + nOff )
                     ENDWITH

                  ELSEIF ( nOff := nCPrev % ::nTabSpaces ) > 0            /* We always go back to the previous indent */
                     WITH OBJECT qCursor
                        :movePosition( QTextCursor_StartOfBlock )
                        :movePosition( QTextCursor_NextCharacter, QTextCursor_KeepAnchor, nOff )
                        :removeSelectedText()
                        :setPosition( nPostn - nOff )
                     ENDWITH

                  ENDIF
               ENDIF
            ENDIF

            /* Group III operations */
            IF cCWord == " " .AND. nAdded == 1 .AND. Empty( cRest )   /* Only first time having only word on a line */
               cWord := Lower( cPWord )

               IF ::oINI:lISClosing
                  IF     ::oINI:lISIf      .AND. cWord == "if" .AND. Empty( cPPWord ) /* Protected for #if */
                     hbide_appendIf( qCursor, hbide_getFrontSpacesAndWord( qCursor:block():text() ), qCursor:position(), ::nTabSpaces, ::oINI:lISElse, ::oINI:lISEmbrace )

                  ELSEIF ::oINI:lISFor     .AND. cWord == "for"
                     hbide_appendFor( qCursor, hbide_getFrontSpacesAndWord( qCursor:block():text() ), qCursor:position() )

                  ELSEIF ::oINI:lISSwitch  .AND. cWord == "switch"
                     hbide_appendSwitch( qCursor, hbide_getFrontSpacesAndWord( qCursor:block():text() ), qCursor:position(), ::nTabSpaces, ::oINI:nISSwitchCases, ::oINI:lISSwitchOWise, ::oINI:lISExitSameLine )

                  ELSEIF ::oINI:lISDoCase  .AND. Lower( cPPWord ) == "do" .AND. cWord == "case"
                     hbide_appendCase( qCursor, hbide_getFrontSpacesAndWord( qCursor:block():text() ), qCursor:position(), ::oINI:nISCaseCases, ::oINI:lISCaseOWise  )

                  ELSEIF ::oINI:lISDoWhile .AND. Lower( cPPWord ) == "do" .AND. cWord == "while"
                     hbide_appendWhile( qCursor, hbide_getFrontSpacesAndWord( qCursor:block():text() ), qCursor:position() )

                  ENDIF
               ENDIF

               IF cWord == "elseif" .OR. cWord == "else" .OR. cWord == "endif"
                  hbide_alignToPrevWord( qCursor, "if", "endif", Len( cWord ), nPostn )

               ELSEIF cWord == "next"
                  hbide_alignToPrevWord( qCursor, "for", "next", Len( cWord ), nPostn )

               ELSEIF cWord == "endwith"
                  hbide_alignToPrevWord( qCursor, "with", "endwith", Len( cWord ), nPostn )

               ELSEIF Lower( cPPWord ) == "static" .AND. ( cPWord == "function" .OR. cPWord == "procedure" )
                  hbide_removeStartingSpaces( qCursor, nCPrevPrev )
                  IF ::oINI:lISFunction
                     hbide_appendFunction( qCursor, ::nTabSpaces, ::oINI:lISLocal, ::oINI:lISReturn, ::oINI:lISSeparator, ::oINI:lReturnAsBeginKeyword, ::cSeparator )
                  ENDIF

               ELSEIF Empty( cPPWord ) .AND. ( cPWord == "function" .OR. cPWord == "procedure" )
                  IF ::oINI:lISFunction
                     hbide_appendFunction( qCursor, ::nTabSpaces, ::oINI:lISLocal, ::oINI:lISReturn, ::oINI:lISSeparator, ::oINI:lReturnAsBeginKeyword, ::cSeparator )
                  ENDIF

               ELSEIF Lower( cPPWord ) == "create" .AND. cPWord == "class"
                  hbide_removeStartingSpaces( qCursor, nCPrevPrev )

               ELSEIF cPPWord == "CLASS" .AND. ! Empty( cPWord )
                  IF ::oINI:lISClass
                     hbide_appendClass( qCursor, ::nTabSpaces, ::oINI, cPWord )
                  ENDIF

               ENDIF
            ENDIF

            ::qEdit:setTextCursor( qCursor )
         ENDIF

      ENDIF /* hbide_IsInCommentOrString( qCursor:block():text(), qCursor:columnNumber() ) */
      qCursor:endEditBlock()
   ENDIF

   IF nPos != -1
      ::handleCurrentIndent()
   ENDIF

   IF ::nProtoLine != -1
      IF ::getLineNo() == ::nProtoLine .AND. ::getColumnNo() >= ::nProtoCol + 1
         IF !empty( cProto := hbide_formatProto_1( ::cProtoOrg, ::getLine(), ::nProtoCol, ::getColumnNo(), @nRows, @nCols ) )
            ::cProto     := cProto
            ::nProtoRows := nRows
            ::nProtoCols := nCols
            ::showProtoType()
         ENDIF
      ENDIF
   ENDIF

   HB_SYMBOL_UNUSED( nDeleted )
   RETURN cRest

/*----------------------------------------------------------------------*/

FUNCTION hbide_getFrontSpacesAndWordsByCursor( qCursor, /*@*/aWords )
   LOCAL cLine, cWord
   LOCAL nPos   := qCursor:position()
   LOCAL nBlock := qCursor:blockNumber()
   LOCAL nStart := 0

   aWords := {}
   IF Empty( cLine := qCursor:block():text() )
      RETURN 0

   ELSE
      DO WHILE SubStr( cLine, ++nStart, 1 ) == " " ; QApplication():processEvents() ; ENDDO
      nStart--

      qCursor:movePosition( QTextCursor_StartOfBlock )
      qCursor:movePosition( QTextCursor_StartOfWord )
      DO WHILE .T.
         QApplication():processEvents()
         IF ! qCursor:movePosition( QTextCursor_EndOfWord, QTextCursor_KeepAnchor )
            qCursor:movePosition( QTextCursor_NextWord )
         ENDIF
         IF qCursor:blockNumber() != nBlock
            EXIT
         ENDIF
         IF ! Empty( cWord := qCursor:selectedText() )
            AAdd( aWords, cWord )
         ENDIF
         qCursor:clearSelection()
      ENDDO

   ENDIF
   qCursor:clearSelection()
   qCursor:setPosition( nPos )

   RETURN nStart

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_removeStartingSpaces( qCursor, nCPrevPrev )
   LOCAL nPostn

   IF nCPrevPrev > 0
      nPostn := qCursor:position()
      WITH OBJECT qCursor
         :movePosition( QTextCursor_StartOfBlock )
         :movePosition( QTextCursor_NextWord, QTextCursor_KeepAnchor )
         :removeSelectedText()
         :setPosition( nPostn - nCPrevPrev )
      ENDWITH
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_alignToPrevWord( qCursor, cWord, cEWord, nLenCWord, nPostn )
   LOCAL cFWord, nCol, nInner := 0
   LOCAL lFound := .F.

   DO WHILE .T.
      IF qCursor:movePosition( QTextCursor_Up, QTextCursor_MoveAnchor )
         nCol := hbide_getFrontSpacesAndWord( qCursor:block():text(), @cFWord )
         IF Lower( cFWord ) == cWord .AND. nInner == 0
            lFound := .T.
            EXIT
         ELSEIF Lower( cFWord ) == cWord .AND. nInner > 0
            nInner--
         ELSEIF Lower( cFWord ) == cEWord
            nInner++
         ENDIF
      ELSE
         EXIT
      ENDIF
   ENDDO
   qCursor:setPosition( nPostn )
   IF lFound
      WITH OBJECT qCursor
         :movePosition( QTextCursor_StartOfBlock, QTextCursor_MoveAnchor )
         :movePosition( QTextCursor_NextWord, QTextCursor_KeepAnchor )
         :removeSelectedText()
         :insertText( Space( nCol ) )
         :movePosition( QTextCursor_Right, QTextCursor_MoveAnchor, nLenCWord + 1 )
      ENDWITH
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_replaceWord( qCursor, nWord, cWord, nPostn )

   WITH OBJECT qCursor
      :movePosition( QTextCursor_PreviousWord, QTextCursor_MoveAnchor, nWord )
      :select( QTextCursor_WordUnderCursor )
      :removeSelectedText()
      :insertText( cWord )
      :setPosition( nPostn )
   ENDWITH

   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_appendClass( qCursor, nTabSpaces, oINI, cClassName )
   LOCAL cMethod
   LOCAL aMethods := hb_ATokens( oINI:cISMethods, ";" )
   LOCAL nPostn := qCursor:position()
   LOCAL nClosingIndent := iif( oINI:lReturnAsBeginKeyword, 0, nTabSpaces )

   WITH OBJECT qCursor
      :movePosition( QTextCursor_EndOfBlock )
      :insertBlock()
      :insertText( Space( nTabSpaces ) + PadR( oINI:cISData, 7 ) + "xDummy" + Space( 34 ) + "INIT NIL" )
      :insertBlock()
      FOR EACH cMethod IN aMethods
         :insertBlock()
         :insertText( Space( nTabSpaces ) + "METHOD " + cMethod + "()" )
      NEXT
      :insertBlock()
      :insertBlock()
      :insertText( Space( nClosingIndent ) + "ENDCLASS " )
      :insertBlock()
      FOR EACH cMethod IN aMethods
         :insertBlock()
         IF oINI:cISFormat == "class:method"
            :insertText( "METHOD " + cClassName + ":" + cMethod + "()" )
         ELSE
            :insertText( "METHOD " + cMethod + "() CLASS " + cClassName )
         ENDIF
         :insertBlock()
         :insertBlock()
         :insertText( Space( nClosingIndent ) + "RETURN Self " )
         :insertBlock()
      NEXT
      :insertBlock()
      :setPosition( nPostn )
   ENDWITH

   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_appendFunction( qCursor, nTabSpaces, lLocal, lReturn, lSeparator, lReturnAsBeginKeyword, cSeparator  )
   LOCAL nPostn := qCursor:position()

   WITH OBJECT qCursor
      :movePosition( QTextCursor_EndOfBlock )
      IF lLocal
         :insertBlock()
         :insertText( Space( nTabSpaces ) + "LOCAL " )
         :insertBlock()
      ENDIF
      IF lReturn
         :insertBlock()
         :insertText( Space( iif( lReturnAsBeginKeyword, 0, nTabSpaces ) ) + "RETURN " )
         :insertBlock()
         IF lSeparator
            :insertBlock()
            :insertText( cSeparator )
            :insertBlock()
         ENDIF
      ENDIF
      :insertBlock()
      :setPosition( nPostn )
   ENDWITH

   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_appendCase( qCursor, nIndent, nCurPos, nCases, lOWise )
   LOCAL i

   WITH OBJECT qCursor
      :movePosition( QTextCursor_EndOfBlock )
      FOR i := 1 TO nCases
         :insertBlock()
         :insertText( Space( nIndent ) + "CASE " )
      NEXT
      IF lOWise
         :insertBlock()
         :insertText( Space( nIndent ) + "OTHERWISE" )
      ENDIF
      :insertBlock()
      :insertText( Space( nIndent ) + "ENDCASE" )
      :setPosition( nCurPos )
      :movePosition( QTextCursor_NextBlock )
      :movePosition( QTextCursor_EndOfLine )
   ENDWITH

   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_appendSwitch( qCursor, nIndent, nCurPos, nTabSpaces, nCases, lOWise, lExitSameLine )
   LOCAL i

   qCursor:movePosition( QTextCursor_EndOfBlock )
   FOR i := 1 TO nCases
      qCursor:insertBlock()
      IF lExitSameLine
         qCursor:insertText( Space( nIndent ) + "CASE   ; EXIT" )
      ELSE
         qCursor:insertText( Space( nIndent ) + "CASE " )
         qCursor:insertBlock()
         qCursor:insertText( Space( nIndent + nTabSpaces ) + "EXIT" )
      ENDIF
   NEXT
   IF lOWise
      qCursor:insertBlock()
      qCursor:insertText( Space( nIndent ) + "OTHERWISE" )
   ENDIF
   qCursor:insertBlock()
   qCursor:insertText( Space( nIndent ) + "ENDSWITCH" )
   qCursor:setPosition( nCurPos )

   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_appendWhile( qCursor, nIndent, nCurPos )

   qCursor:movePosition( QTextCursor_EndOfBlock )
   qCursor:insertBlock()
   qCursor:insertText( Space( nIndent ) + "ENDDO" )
   qCursor:setPosition( nCurPos )

   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_appendFor( qCursor, nIndent, nCurPos )

   qCursor:movePosition( QTextCursor_EndOfBlock )
   qCursor:insertBlock()
   qCursor:insertText( Space( nIndent ) + "NEXT" )
   qCursor:setPosition( nCurPos )

   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_appendIf( qCursor, nIndent, nCurPos, nTabSpaces, lElse, lEmbrace )
   LOCAL nCol, cLine
   LOCAL lAligned := .F.

   IF lEmbrace
      qCursor:movePosition( QTextCursor_StartOfBlock )
      IF qCursor:movePosition( QTextCursor_NextBlock )
         /* First line after IF must be starting on same indent where IF starts ; TO qualify FOR embracing */
         cLine := qCursor:block():text()
         nCol := hbide_getFrontSpacesAndWord( cLine )
         IF ! Empty( cLine ) .AND. nCol == nIndent
            lAligned := .T.
            qCursor:insertText( Space( nTabSpaces ) )
            DO WHILE qCursor:movePosition( QTextCursor_NextBlock )
               nCol := hbide_getFrontSpacesAndWord( qCursor:block():text() )
               IF nCol < nIndent
                  qCursor:movePosition( QTextCursor_PreviousBlock )
                  EXIT
               ENDIF
               qCursor:insertText( Space( nTabSpaces ) )
               QApplication():processEvents()
            ENDDO
         ENDIF
      ENDIF
   ENDIF
   IF ! lAligned
      qCursor:movePosition( QTextCursor_PreviousBlock )
      qCursor:setPosition( nCurPos )
   ENDIF

   qCursor:movePosition( QTextCursor_EndOfBlock )
   IF lElse
      qCursor:insertBlock()
      qCursor:insertText( Space( nIndent ) + "ELSE" )
   ENDIF
   qCursor:insertBlock()
   qCursor:insertText( Space( nIndent ) + "ENDIF" )
   qCursor:setPosition( nCurPos )

   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_alignAssignments( qCursor )
   LOCAL aWords, cLine, nIndent, nPostn, cCLine, nCBlock, nAssgnAt, nCol
   LOCAL lAssign := .F.

   nIndent := hbide_getFrontSpacesAndWordsByCursor( qCursor, @aWords )
   IF Len( aWords ) >= 2
      IF Len( aWords ) == 2 .AND. aWords[ 2 ] == ":=" .OR. ;
         Len( aWords ) == 4 .AND. aWords[ 4 ] == ":=" .OR. ;
         Len( aWords ) == 5 .AND. aWords[ 5 ] == ":=" .AND. aWords[ 1 ] == "::"

         cCLine   := qCursor:block():text()
         nPostn   := qCursor:position()
         nAssgnAt := At( ":=", cCLine )
         nCBlock  := qCursor:blockNumber()

         DO WHILE .T.
            IF qCursor:movePosition( QTextCursor_PreviousBlock, QTextCursor_MoveAnchor )
               IF ! Empty( cLine := qCursor:block():text() )
                  nCol := hbide_getFrontSpacesAndWordsByCursor( qCursor, @aWords )
                  IF nCol == nIndent .AND. Len( aWords ) >= 2 .AND. aWords[ 2 ] == ":="
                     nAssgnAt := Max( nAssgnAt, At( ":=", cLine ) )
                     lAssign  := .T.
                  ELSEIF nCol == nIndent .AND. Len( aWords ) >= 4 .AND. aWords[ 2 ] == ":" .AND. aWords[ 4 ] == ":="
                     nAssgnAt := Max( nAssgnAt, At( ":=", cLine ) )
                     lAssign  := .T.
                  ELSEIF nCol == nIndent .AND. Len( aWords ) >= 5 .AND. aWords[ 1 ] == "::" .AND. aWords[ 3 ] == ":" .AND. aWords[ 5 ] == ":="
                     nAssgnAt := Max( nAssgnAt, At( ":=", cLine ) )
                     lAssign  := .T.
                  ELSE
                     EXIT
                  ENDIF
               ENDIF
            ELSE
               EXIT
            ENDIF
         ENDDO
         /* Anyway we are TO move TO NEXT block */
         qCursor:movePosition( QTextCursor_NextBlock, QTextCursor_MoveAnchor )
         IF lAssign
            DO WHILE .T.
               cLine := qCursor:block():text()
               IF ! Empty( cLine )
                  nCol  := At( ":=", cLine )
                  cLine := Pad( Trim( SubStr( cLine, 1, nCol - 1 ) ), nAssgnAt - 1 ) + ":=" + Trim( SubStr( cLine, nCol + 2 ) )
                  qCursor:movePosition( QTextCursor_EndOfLine, QTextCursor_KeepAnchor )
                  qCursor:removeSelectedText()
                  qCursor:insertText( cLine )
               ENDIF
               IF qCursor:blockNumber() == nCBlock
                  EXIT
               ENDIF
               qCursor:movePosition( QTextCursor_NextBlock, QTextCursor_MoveAnchor )
            ENDDO
            /* We have reached on current line */
            qCursor:movePosition( QTextCursor_EndOfBlock, QTextCursor_MoveAnchor )
         ELSE
            qCursor:setPosition( nPostn )
         ENDIF
      ENDIF
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD IdeEdit:findLastIndent()
   LOCAL qCursor, qTextBlock, cText, cWord
   LOCAL nSpaces := 0

   qCursor := ::qEdit:textCursor()
   qTextBlock := qCursor:block()

   qTextBlock := qTextBlock:previous()
   DO WHILE .t.
      IF !( qTextBlock:isValid() )
         EXIT
      ENDIF
      IF !empty( cText := qTextBlock:text() )
         nSpaces := hbide_getFrontSpacesAndWord( cText, @cWord )
         IF !empty( cWord )
            IF ::oINI:lSmartIndent .AND. hbide_isIndentableKeyword( cWord, ::oIde )
               nSpaces += ::nTabSpaces
            ENDIF
            EXIT
         ENDIF
      ENDIF
      qTextBlock := qTextBlock:previous()
   ENDDO

   RETURN nSpaces

/*----------------------------------------------------------------------*/

METHOD IdeEdit:handleCurrentIndent()
   LOCAL qCursor, nSpaces

   IF ::lIndentIt
      ::lIndentIt := .f.
      IF ( nSpaces := ::findLastIndent() ) > 0
         qCursor := ::qEdit:textCursor()
         qCursor:insertText( space( nSpaces ) )
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:openHeaderFile()
   LOCAL aCord, nT, nL, nB, nR, nW, cLine, cExt, cNext, cHeader, cPath
   LOCAL lOpened := .F.

   IF ! Empty( ::oINI:aIncludePaths )

      aCord := ::aSelectionInfo
      hbide_normalizeRect( aCord, @nT, @nL, @nB, @nR )
      nW := nR - nL

      IF nW >= 0 .AND. nT == nB
         cLine := ::getLine( nT + 1 )
         IF SubStr( cLine, nL, 1 ) $ ['"]
            cExt := SubStr( cLine, nR + 1, 3 )
            cNext := SubStr( cLine, nR + 1 + 3, 1 )
            IF Lower( cExt ) == ".ch" .AND. cNext $ ['"]
               cHeader := SubStr( cLine, nL + 1, nW ) + cExt
            ELSEIF Lower( Left( cExt, 2 ) ) == ".h" .AND. Right( cExt, 1 ) $ ['"]
               cHeader := SubStr( cLine, nL + 1, nW ) + Left( cExt, 2 )
            ENDIF
            IF ! Empty( cHeader )
               FOR EACH cPath IN ::oINI:aIncludePaths
                  IF hb_FileExists( hbide_pathToOSPath( cPath + cHeader ) )
                     ::oIde:showHeaderFile( hb_MemoRead( cPath + cHeader ), cPath + cHeader, QIcon( hbide_image( hbide_imageForFileType( cExt ) ) ), .T. )
                     lOpened := .T.
                     EXIT
                  ENDIF
               NEXT
            ENDIF
         ENDIF
      ENDIF
   ENDIF
   RETURN lOpened

/*----------------------------------------------------------------------*/

METHOD IdeEdit:gotoFunction()
   LOCAL cWord, n
   LOCAL lFindCur := .f.
   IF !empty( cWord := ::getWord( .f. ) )
      IF ( n := ascan( ::aTags, {|e_| lower( cWord ) $ lower( e_[ 7 ] ) } ) ) > 0
         IF ::find( alltrim( ::aTags[ n,8 ] ) )
            lFindCur := .t.
         ENDIF
      ENDIF
      IF ! lFindCur
         ::oFN:jumpToFunction( cWord, .t. )
      ENDIF
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:clickFuncHelp()
   LOCAL cWord
   IF !empty( cWord := ::getWord( .f. ) )
      IF ! empty( ::oHL )
         ::oHL:jumpToFunction( cWord )
      ENDIF
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:loadFuncHelp()
   LOCAL qEdit, qCursor, qTextBlock, cText, cWord, nCol, cPro

   qEdit := ::qEdit
   qCursor    := qEdit:textCursor()
   qTextBlock := qCursor:block()
   cText      := qTextBlock:text()
   nCol       := qCursor:columnNumber()
   cWord      := hbide_getPreviousWord( cText, nCol )

   IF !empty( cWord )
      IF empty( cPro := ::oEM:getProto( cWord ) )
         IF ! empty( ::oHL )
            ::oHL:jumpToFunction( cWord )
         ENDIF
         IF !empty( cPro := ::oFN:positionToFunction( cWord, .t. ) )
            IF empty( ::cProto )
               ::showPrototype( cPro )
            ENDIF
         ENDIF
      ELSE
         ::showPrototype( cPro )
      ENDIF
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:resumePrototype()

   ::isSuspended := .f.
   IF !empty( ::qEdit )
      IF ::getLineNo() == ::nProtoLine .AND. ::getColumnNo() >= ::nProtoCol
         ::qEdit:hbShowPrototype( ::cProto, ::nProtoRows, ::nProtoCols )
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:suspendPrototype()

   ::isSuspended := .t.
   IF !empty( ::qEdit )
      ::qEdit:hbShowPrototype( "", 0, 0 )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:showPrototype( cProto )

   IF ! ::isSuspended  .AND. !empty( ::qEdit )
      IF !empty( cProto )
         ::cProtoOrg  := cProto
         ::cProto     := hbide_formatProto( cProto )
         ::nProtoLine := ::getLineNo()
         ::nProtoCol  := ::getColumnNo()
      ENDIF
      ::qEdit:hbShowPrototype( ::cProto, ::nProtoRows, ::nProtoCols )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:hidePrototype()

   IF !empty( ::qedit )
      ::nProtoLine := -1
      ::nProtoCol  := -1
      ::cProto     := ""
      ::nProtoCols := 10
      ::nProtoRows := 1
      ::qEdit:hbShowPrototype( "", 0, 0 )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:parseCodeCompletion( cSyntax )
   LOCAL cText, n, nFun, nAbr, nSpc

   nAbr := at( "-", cSyntax )
   nSpc := at( " ", cSyntax )
   nFun := at( "(", cSyntax )

   IF "[f]" $ cSyntax
      cText := alltrim( substr( cSyntax, 1, nSpc ) )

   ELSEIF nAbr > 0 .AND. iif( nSpc == 0, .t., nAbr < nSpc ).AND. iif( nFun == 0, .t., nAbr < nFun )
      cText := alltrim( substr( cSyntax, nAbr + 1 ) )

   ELSE
      IF ::oINI:lCompleteArgumented
         IF ( n := rat( ")", cSyntax ) ) > 0
            cText := trim( substr( cSyntax, 1, n ) )
         ELSE
            cText := trim( cSyntax )
         ENDIF
      ELSE
         IF nFun > 0 .AND. iif( nSpc == 0, .t., nFun < nSpc )
            cText := trim( substr( cSyntax, 1, nFun - 1 ) )
         ELSE
            cText := trim( cSyntax )
         ENDIF
      ENDIF
   ENDIF

   RETURN cText

/*----------------------------------------------------------------------*/

METHOD IdeEdit:completeCode( p )
   LOCAL qCursor := ::qEdit:textCursor()
   LOCAL cWord

   qCursor:movePosition( QTextCursor_Left )

   qCursor:movePosition( QTextCursor_StartOfWord )
   qCursor:movePosition( QTextCursor_EndOfWord, QTextCursor_KeepAnchor )

   cWord := qCursor:selectedText()
   IF cWord == "->"
      qCursor:clearSelection()
   ENDIF

   qCursor:insertText( ::parseCodeCompletion( p ) )
   qCursor:movePosition( QTextCursor_Left )
   qCursor:movePosition( QTextCursor_Right )

   ::qEdit:setTextCursor( qCursor )

   ::qEdit:hbSetFieldsListActive( ::oEM:updateFieldsList() )

   RETURN Self

/*----------------------------------------------------------------------*/

FUNCTION hbide_getPreviousWord( cText, nPos )
   LOCAL cWord, n

   cText := alltrim( substr( cText, 1, nPos ) )
   IF ( n := rat( " ", cText ) ) > 0
      cWord := substr( cText, n + 1 )
   ELSE
      cWord := cText
   ENDIF

   RETURN cWord

/*----------------------------------------------------------------------*/

FUNCTION hbide_getFirstWord( cText )
   LOCAL cWord, n

   cText := alltrim( cText )
   IF ( n := at( " ", cText ) ) > 0
      cWord := left( cText, n-1 )
   ELSE
      cWord := cText
   ENDIF

   RETURN cWord

/*----------------------------------------------------------------------*/

FUNCTION hbide_getSecondWord( cText )
   LOCAL cWord := "", a_

   a_:= hb_ATokens( AllTrim( cText ), " " )
   IF Len( a_ ) >= 2
      cWord := a_[ 2 ]
   ENDIF

   RETURN cWord

/*----------------------------------------------------------------------*/

FUNCTION hbide_getFrontSpacesAndWord( cText, cWord, cSWord )
   LOCAL n := 0

   DO WHILE .t.
      IF !( substr( cText, ++n, 1 ) == " " )
         EXIT
      ENDIF
   ENDDO
   n--

   cWord := hbide_getFirstWord( cText )
   cSWord := hbide_getSecondWord( cText )

   RETURN n

/*----------------------------------------------------------------------*/

FUNCTION hbide_formatProto_1( cProto, cText, nProtoCol, nCurCol, nRows, nCols )
   LOCAL s, nArgs, cArgs, aArgs, cArg, n, n1, i, nnn, cPro, cFunc

   IF nCurCol > nProtoCol
      n  := at( "(", cProto ) ; n1 := at( ")", cProto )
      IF n > 0 .AND. n1 > 0 .AND. "," $ cProto
         cProto := substr( cProto, 1, n1 )

         s := substr( cText, nProtoCol, nCurCol - nProtoCol )
         nArgs := 1
         FOR i := 1 TO Len( s )
            IF substr( s, i, 1 ) == ","
               nArgs++
            ENDIF
         NEXT

         nRows := 1; nCols := 0

         IF nArgs > 0
            n := at( "(", cProto ) ; n1 := at( ")", cProto )

            cFunc := substr( cProto, 1, n - 1 )
            cArgs := substr( cProto, n + 1, n1 - n - 1 )
            aArgs := hb_aTokens( cArgs, "," )
            cArgs := ""
            nCols := Len( cFunc ) + 1
            FOR EACH cArg IN aArgs
               cArg := alltrim( cArg )

               nRows++
               nCols := max( nCols, Len( cArg ) + 3 )

               cArg := StrTran( cArg, "<", "&lt;" )
               cArg := StrTran( cArg, ">", "&gt;" )

               nnn  := cArg:__enumIndex()
               IF nnn == nArgs
                  cArg := "<font color=red><b>" + cArg + "</b></font>"
               ENDIF
               IF nnn == Len( aArgs )
                  cArgs += "<br>" + "   " + cArg
               ELSE
                  cArgs += "<br>" + "   " + cArg + "<font color=red><b>" + "," + "</b></font>"
               ENDIF
            NEXT
            nCols += iif( nCols <= Len( cFunc ), 0, 1 )

            //cPro  := "<p style='white-space:pre'>" + "<font color=darkgreen><b>" + cFunc + "</b></font>" + ;
            cPro  := "<p style='white-space:pre'>" + "<b>" + cFunc + "</b>" + ;
                        "<font color=red><b>" + "(" + "</b></font>" + ;
                           cArgs + ;
                              "<font color=red><b>" + ")" + "</font>" + "</b></p>"
         ENDIF
      ENDIF
   ENDIF

   RETURN cPro

/*------------------------------------------------------------------------*/

FUNCTION hbide_formatProto( cProto )
   LOCAL n, n1, cArgs

   cProto := StrTran( cProto, "<", "&lt;" )
   cProto := StrTran( cProto, ">", "&gt;" )

   n  := at( "(", cProto )
   n1 := at( ")", cProto )

   IF n > 0 .AND. n1 > 0
      cArgs  := substr( cProto, n + 1, n1 - n - 1 )
      cArgs  := strtran( cArgs, ",", "<font color=red><b>" + "," + "</b></font>" )
      cProto := "<p style='white-space:pre'>" + "<b>" + substr( cProto, 1, n - 1 ) + "</b>" + ;
                   "<font color=red><b>" + "(" + "</b></font>" + ;
                      cArgs + ;
                         "<font color=red><b>" + ")" + "</font>" + "</b></p>"
   ENDIF
   RETURN cProto

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_normalizeRect( aCord, nT, nL, nB, nR )
   nT := iif( aCord[ 1 ] > aCord[ 3 ], aCord[ 3 ], aCord[ 1 ] )
   nB := iif( aCord[ 1 ] > aCord[ 3 ], aCord[ 1 ], aCord[ 3 ] )
   nL := iif( aCord[ 2 ] > aCord[ 4 ], aCord[ 4 ], aCord[ 2 ] )
   nR := iif( aCord[ 2 ] > aCord[ 4 ], aCord[ 2 ], aCord[ 4 ] )
   RETURN NIL

/*----------------------------------------------------------------------*/

FUNCTION hbide_isMatchingWord( cWord )

   STATIC hMatches

   IF Empty( hMatches )
      hMatches := {=>}
      hb_HCaseMatch( hMatches, .F. )
      hMatches[ "if"        ] := NIL
      hMatches[ "endif"     ] := NIL
      hMatches[ "for"       ] := NIL
      hMatches[ "next"      ] := NIL
      hMatches[ "switch"    ] := NIL
      hMatches[ "endswitch" ] := NIL
      hMatches[ "do"        ] := NIL
      hMatches[ "enddo"     ] := NIL
      hMatches[ "endcase"   ] := NIL
      hMatches[ "return"    ] := NIL
      hMatches[ "function"  ] := NIL
      hMatches[ "procedure" ] := NIL
      hMatches[ "method"    ] := NIL
      hMatches[ "class"     ] := NIL
      hMatches[ "endclass"  ] := NIL
      hMatches[ "with"      ] := NIL
      hMatches[ "endwith"   ] := NIL
   ENDIF

   RETURN cWord $ hMatches

FUNCTION hbide_isStartingKeyword( cWord, oIde )
   LOCAL s_b_

   IF Empty( s_b_ )
      s_b_:= {=>}
      hb_HCaseMatch( s_b_, .F. )

      IF ! oIde:oINI:lReturnAsBeginKeyword
         s_b_[ 'function'  ] := NIL
         s_b_[ 'procedure' ] := NIL
         s_b_[ 'class'     ] := NIL
         s_b_[ 'method'    ] := NIL
         s_b_[ 'static'    ] := NIL
      ELSE
         s_b_[ 'function'  ] := NIL
         s_b_[ 'procedure' ] := NIL
         s_b_[ 'class'     ] := NIL
         s_b_[ 'method'    ] := NIL
         s_b_[ 'static'    ] := NIL
         s_b_[ 'return'    ] := NIL
      ENDIF
   ENDIF

   RETURN cWord $ s_b_

/*----------------------------------------------------------------------*/

FUNCTION hbide_isMinimumIndentableKeyword( cWord, oIde )
   LOCAL s_b_

   IF Empty( s_b_ )
      s_b_:= {=>}
      hb_HCaseMatch( s_b_, .F. )

      IF ! oIde:oINI:lReturnAsBeginKeyword
         s_b_[ 'local'    ] := NIL
         s_b_[ 'private'  ] := NIL
         s_b_[ 'public'   ] := NIL
         s_b_[ 'endclass' ] := NIL
         s_b_[ 'default'  ] := NIL
         s_b_[ 'return'   ] := NIL
      ELSE
         s_b_[ 'local'    ] := NIL
         s_b_[ 'private'  ] := NIL
         s_b_[ 'public'   ] := NIL
         s_b_[ 'endclass' ] := NIL
         s_b_[ 'default'  ] := NIL
      ENDIF
   ENDIF

   RETURN cWord $ s_b_

/*----------------------------------------------------------------------*/

FUNCTION hbide_isIndentableKeyword( cWord )
   STATIC s_b_

   IF Empty( s_b_ )
      s_b_:= {=>}
      hb_HCaseMatch( s_b_, .F. )

      s_b_[ 'if'        ] := NIL
      s_b_[ 'else'      ] := NIL
      s_b_[ 'elseif'    ] := NIL
      s_b_[ 'docase'    ] := NIL
      s_b_[ 'case'      ] := NIL
      s_b_[ 'otherwise' ] := NIL
      s_b_[ 'do'        ] := NIL
      s_b_[ 'while'     ] := NIL
      s_b_[ 'switch'    ] := NIL
      s_b_[ 'for'       ] := NIL
      s_b_[ 'begin'     ] := NIL
      s_b_[ 'sequence'  ] := NIL
      s_b_[ 'try'       ] := NIL
      s_b_[ 'catch'     ] := NIL
      s_b_[ 'always'    ] := NIL
      s_b_[ 'recover'   ] := NIL
      s_b_[ 'finally'   ] := NIL
      s_b_[ 'with'      ] := NIL
   ENDIF

   RETURN cWord $ s_b_

/*----------------------------------------------------------------------*/

FUNCTION hbide_harbourKeywords()
   STATIC s_b_

   IF Empty( s_b_ )
      s_b_:= {=>}
      hb_HCaseMatch( s_b_, .F. )

      s_b_[ 'function'         ] := NIL
      s_b_[ 'procedure'        ] := NIL
      s_b_[ 'thread'           ] := NIL
      s_b_[ 'return'           ] := NIL
      s_b_[ 'request'          ] := NIL
      s_b_[ 'static'           ] := NIL
      s_b_[ 'local'            ] := NIL
      s_b_[ 'default'          ] := NIL
      s_b_[ 'if'               ] := NIL
      s_b_[ 'else'             ] := NIL
      s_b_[ 'elseif'           ] := NIL
      s_b_[ 'endif'            ] := NIL
      s_b_[ 'end'              ] := NIL
      s_b_[ 'docase'           ] := NIL
      s_b_[ 'case'             ] := NIL
      s_b_[ 'endcase'          ] := NIL
      s_b_[ 'otherwise'        ] := NIL
      s_b_[ 'switch'           ] := NIL
      s_b_[ 'endswitch'        ] := NIL
      s_b_[ 'do'               ] := NIL
      s_b_[ 'while'            ] := NIL
      s_b_[ 'enddo'            ] := NIL
      s_b_[ 'exit'             ] := NIL
      s_b_[ 'for'              ] := NIL
      s_b_[ 'each'             ] := NIL
      s_b_[ 'next'             ] := NIL
      s_b_[ 'step'             ] := NIL
      s_b_[ 'to'               ] := NIL
      s_b_[ 'class'            ] := NIL
      s_b_[ 'endclass'         ] := NIL
      s_b_[ 'method'           ] := NIL
      s_b_[ 'data'             ] := NIL
      s_b_[ 'var'              ] := NIL
      s_b_[ 'destructor'       ] := NIL
      s_b_[ 'inline'           ] := NIL
      s_b_[ 'setget'           ] := NIL
      s_b_[ 'assign'           ] := NIL
      s_b_[ 'access'           ] := NIL
      s_b_[ 'inherit'          ] := NIL
      s_b_[ 'init'             ] := NIL
      s_b_[ 'create'           ] := NIL
      s_b_[ 'virtual'          ] := NIL
      s_b_[ 'message'          ] := NIL
      s_b_[ 'begin'            ] := NIL
      s_b_[ 'sequence'         ] := NIL
      s_b_[ 'try'              ] := NIL
      s_b_[ 'catch'            ] := NIL
      s_b_[ 'always'           ] := NIL
      s_b_[ 'recover'          ] := NIL
      s_b_[ 'with'             ] := NIL
      s_b_[ 'replace'          ] := NIL
      s_b_[ 'hb_symbol_unused' ] := NIL
      s_b_[ 'error'            ] := NIL
      s_b_[ 'handler'          ] := NIL
      s_b_[ 'loop'             ] := NIL
      s_b_[ 'in'               ] := NIL
      s_b_[ 'object'           ] := NIL
      s_b_[ 'endwith'          ] := NIL
      s_b_[ 'nil'              ] := NIL
   ENDIF

   RETURN s_b_

/*----------------------------------------------------------------------*/

FUNCTION hbide_isHarbourKeyword( cWord )

   RETURN cWord $ hbide_harbourKeywords()

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_addInList( hHash, aList )
   LOCAL s

   FOR EACH s IN aList
      IF ! empty( s )
         s := alltrim( s )
         hHash[ s ] := s
      ENDIF
   NEXT

   RETURN NIL

/*----------------------------------------------------------------------*/

FUNCTION hbide_isHarbourFunction( cWord, cCased )

   STATIC s_b_
   IF empty( s_b_ )
      s_b_:= {=>}
      hb_hCaseMatch( s_b_, .F. )
      hbide_addInList( s_b_, hbide_getHarbourFunctions( hbide_getHarbourHbx() ) )
   ENDIF
   IF cWord $ s_b_
      cCased := s_b_[ cWord ]
      RETURN .T.
   ENDIF

   RETURN .f.

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_isQtFunction( cWord, cCased )

   STATIC s_b_
   IF empty( s_b_ )
      s_b_:= {=>}
      hb_hCaseMatch( s_b_, .F. )
      hbide_addInList( s_b_, hbide_getQtFunctions( hbide_getQtCoreFilelist() ) )
      hbide_addInList( s_b_, hbide_getQtFunctions( hbide_getQtGuiFilelist() ) )
      hbide_addInList( s_b_, hbide_getQtFunctions( hbide_getQtNetworkFilelist() ) )
   ENDIF
   IF cWord $ s_b_
      cCased := s_b_[ cWord ]
      RETURN .T.
   ENDIF

   RETURN .f.

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_isUserFunction( cWord, cCased )
   LOCAL oDict

   FOR EACH oDict IN hbide_setIDE():aUserDict
      IF oDict:lActive
         IF hb_HHasKey( oDict:hItems, cWord )
            SWITCH oDict:cConvMode
            CASE "ASIS"  ; cCased := oDict:hItems[ cWord ][ 1 ]          ; EXIT
            CASE "LOWER" ; cCased := Lower( oDict:hItems[ cWord ][ 1 ] ) ; EXIT
            CASE "UPPER" ; cCased := Upper( oDict:hItems[ cWord ][ 1 ] ) ; EXIT
            CASE "NONE"  ; cCased := cWord ; EXIT
            ENDSWITCH
            RETURN .T.
         ENDIF
      ENDIF
   NEXT

   RETURN .F.

/*----------------------------------------------------------------------*/
/* Pulled from harbour/bin/find.hb and adopted for file as buffer */
STATIC FUNCTION hbide_getHarbourFunctions( cFile )
   LOCAL pRegex
   LOCAL tmp
   LOCAL aDynamic := {}

   IF ! Empty( cFile ) .AND. ;
      ! Empty( pRegex := hb_regexComp( "^DYNAMIC ([a-zA-Z0-9_]*)$", .T., .T. ) )
      FOR EACH tmp IN hb_regexAll( pRegex, StrTran( cFile, Chr( 13 ) ),,,,, .T. )
         AAdd( aDynamic, tmp[ 2 ] )
      NEXT
   ENDIF

   RETURN aDynamic

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_getQtFunctions( cBuffer )
   LOCAL pRegex
   LOCAL tmp
   LOCAL aDynamic := {}

   IF ! Empty( cBuffer ) .AND. ;
      ! Empty( pRegex := hb_regexComp( "^([a-zA-Z0-9_]*.qth)$", .T., .T. ) )
      FOR EACH tmp IN hb_regexAll( pRegex, StrTran( cBuffer, Chr( 13 ) ),,,,, .T. )
         AAdd( aDynamic, StrTran( tmp[ 1 ], ".qth" ) )
      NEXT
   ENDIF

   RETURN aDynamic

/*----------------------------------------------------------------------*/
#pragma -km+

FUNCTION hbide_getHarbourHbx()
   #pragma __binarystreaminclude "harbour.hbx" | RETURN %s

FUNCTION hbide_getQtGuiFilelist()
   #pragma __binarystreaminclude "qtgui.txt" | RETURN %s

FUNCTION hbide_getQtCoreFilelist()
   #pragma __binarystreaminclude "qtcore.txt" | RETURN %s

FUNCTION hbide_getQtNetworkFilelist()
   #pragma __binarystreaminclude "qtnetwork.txt" | RETURN %s

/*----------------------------------------------------------------------*/
