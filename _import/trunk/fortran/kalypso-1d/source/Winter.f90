! winter.f90 : Winteracter type/interface/parameter definitions
!
!  *********************************************************
!  **    (c) Interactive Software Services Ltd. 1997-99   **
!  ** --------------------------------------------------- **
!  ** Licenced users of Winteracter are granted permission**
!  **      to integrate this source code in their own     **
!  **            Winteracter-based software               **
!  ** --------------------------------------------------- **
!  **  I.S.S. can be contacted on Tel +44 (0) 1543 503611 **
!  **                          or Fax +44 (0) 1543 574566 **
!  **              or Internet support@issltd.demon.co.uk **
!  *********************************************************
!
!  This file implements a full set of Fortran 9x interface definitions
!  for nearly all of the user-callable routines in the Winteracter library.
!  By inserting a USE WINTERACTER statement at the start of every program
!  unit which uses Winteracter you can take advantage of Fortran 90's
!  interface checking capabilities to ensure consistent argument types
!  and the correct number of arguments.
!
!  Compile this source file using your target Fortran 9x compiler and
!  make sure that the resulting module file(s) is/are available to
!  to the compiler at compile time, when compiling Winteracter-based
!  code which includes the USE WINTERACTER statement.
!
!  This source file will be regularly updated at each Winteracter release.
!  Be sure to use the latest version for best results (old versions will
!  normally be fully upwards compatible but will obviously not contain
!  interface definitions for newer routines).
!
!  Note that interface definitions are not included here for the small
!  number of Winteracter assumed-length CHARACTER FUNCTION's, such as
!  InfoFilename, InfoVersion, etc. The Fortran 9x standards do not
!  allow interface definitions for such functions. Whilst some compilers
!  allow such declarations others do not, so they are omitted.
!
MODULE WINTTYPES
!
      IMPLICIT NONE
!
! ********************************************************************
! TYPE declatations
! ********************************************************************
!
!  TYPE structure for passing information to WindowOpen
!
      TYPE WIN_STYLE
          SEQUENCE
          INTEGER           :: FLAGS         ! Bit encoded options
          INTEGER           :: X, Y          ! Window position
          INTEGER           :: WIDTH, HEIGHT ! Window width/height
          INTEGER           :: MENUID        ! Menu resource id
          CHARACTER(LEN=80) :: TITLE         ! Window title
      END TYPE WIN_STYLE
!
!  TYPE structure for receiving message information
!
      TYPE WIN_MESSAGE
          SEQUENCE
          INTEGER          :: WIN             ! Window Message came from
          INTEGER          :: VALUE1,VALUE2   ! General values
          INTEGER          :: VALUE3,VALUE4   ! General values
          INTEGER          :: X,Y             ! Co-ordinate
          REAL             :: GVALUE1,GVALUE2 ! VALUE1/2 in graphics units
          REAL             :: GX, GY          ! X/Y in graphics units
          INTEGER          :: TIME            ! Message time in milliseconds
      END TYPE WIN_MESSAGE
!
!  TYPE structure for encoding RGB values
!
      TYPE WIN_RGB
          SEQUENCE
          INTEGER          :: IRED           ! Amount of red   (0-255)
          INTEGER          :: IGREEN         ! Amount of green (0-255)
          INTEGER          :: IBLUE          ! Amount of blue  (0-255)
      END TYPE WIN_RGB
!
!  TYPE structure for setting font information
!
      TYPE WIN_FONT
         SEQUENCE
         INTEGER           :: IFONTNUM       ! Font style
         INTEGER           :: IWIDTH         ! Requested font width
         INTEGER           :: IHEIGHT        ! Requested font height
         INTEGER           :: IBOLD          ! Bold on/off
         INTEGER           :: ITALIC         ! Italic on/off
         INTEGER           :: IUNDER         ! Underline on/off
         INTEGER           :: IFCOL          ! Foreground colour
         INTEGER           :: IBCOL          ! Background clour
      END TYPE WIN_FONT
END MODULE WINTTYPES
!
MODULE WINTERACTER
!
      USE WINTTYPES
      IMPLICIT NONE
!
! ********************************************************************
! INTERFACE declarations
! ********************************************************************
!
!  WN : Window Management
!
      INTERFACE
        SUBROUTINE WindowOpen(WINIT,NCOL256)
            USE WINTTYPES
            IMPLICIT NONE
            TYPE(WIN_STYLE) , INTENT (IN)           :: WINIT
            INTEGER         , INTENT (IN), OPTIONAL :: NCOL256
        END SUBROUTINE WindowOpen
!
        SUBROUTINE WindowOpenChild(WINIT, IHANDLE)
            USE WINTTYPES
            IMPLICIT NONE
            TYPE(WIN_STYLE) , INTENT (IN)  :: WINIT 
            INTEGER         , INTENT (OUT) :: IHANDLE
        END SUBROUTINE
!
        SUBROUTINE WindowClose
            IMPLICIT NONE
        END SUBROUTINE WindowClose
!
        SUBROUTINE WindowCloseChild(IHANDLE)
            IMPLICIT NONE
            INTEGER         , INTENT (IN) :: IHANDLE
        END SUBROUTINE WindowCloseChild
!
        SUBROUTINE WindowRaise(IHANDLE)
            IMPLICIT NONE
            INTEGER         , INTENT (IN) :: IHANDLE
        END SUBROUTINE WindowRaise
!
        SUBROUTINE WindowScroll(DIRECN,NUMBER)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: DIRECN
            INTEGER         , INTENT (IN) :: NUMBER
        END SUBROUTINE WindowScroll
!
        SUBROUTINE WindowSelect(IHANDLE)
            IMPLICIT NONE
            INTEGER, INTENT (IN) :: IHANDLE
        END SUBROUTINE WindowSelect
!
        SUBROUTINE WindowStatusBarParts(NPARTS,IWIDTHS,ISTYLES)
            IMPLICIT NONE
            INTEGER, INTENT (IN)                              :: NPARTS
            INTEGER, INTENT (IN), DIMENSION(NPARTS)           :: IWIDTHS
            INTEGER, INTENT (IN), DIMENSION(NPARTS), OPTIONAL :: ISTYLES
        END SUBROUTINE WindowStatusBarParts
!
        SUBROUTINE WindowTitle(TITLE)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: TITLE
        END SUBROUTINE WindowTitle
!
        SUBROUTINE WindowUnitsFromPixels(IXPIX,IYPIX,IXWIN,IYWIN)
            IMPLICIT NONE
            INTEGER , INTENT (IN)  :: IXPIX
            INTEGER , INTENT (IN)  :: IYPIX
            INTEGER , INTENT (OUT) :: IXWIN
            INTEGER , INTENT (OUT) :: IYWIN
        END SUBROUTINE WindowUnitsFromPixels
!
        SUBROUTINE WindowUnitsToPixels(IXWIN,IYWIN,IXPIX,IYPIX)
            IMPLICIT NONE
            INTEGER , INTENT (IN)  :: IXWIN
            INTEGER , INTENT (IN)  :: IYWIN
            INTEGER , INTENT (OUT) :: IXPIX
            INTEGER , INTENT (OUT) :: IYPIX
        END SUBROUTINE WindowUnitsToPixels
!
!  TX : Text Output
!
        SUBROUTINE WindowJustifyNum(LCR)
            IMPLICIT NONE
            CHARACTER       , INTENT (IN) :: LCR
        END SUBROUTINE WindowJustifyNum
!
        SUBROUTINE WindowOutCentre(IY, STRING)
            IMPLICIT NONE
            INTEGER         , INTENT (IN) :: IY
            CHARACTER(LEN=*), INTENT (IN) :: STRING
        END SUBROUTINE WindowOutCentre
!
        SUBROUTINE WindowOutDouble(IX, IY, DVALUE, FRMAT)
            IMPLICIT NONE
            INTEGER         , INTENT (IN) :: IX
            INTEGER         , INTENT (IN) :: IY
            DOUBLE PRECISION, INTENT (IN) :: DVALUE
            CHARACTER(LEN=*), INTENT (IN) :: FRMAT
        END SUBROUTINE WindowOutDouble
!
        SUBROUTINE WindowOutInteger(IX, IY, IVALUE, IWIDTH)
            IMPLICIT NONE
            INTEGER         , INTENT (IN) :: IX
            INTEGER         , INTENT (IN) :: IY
            INTEGER         , INTENT (IN) :: IVALUE
            INTEGER         , INTENT (IN) :: IWIDTH
        END SUBROUTINE WindowOutInteger
!
        SUBROUTINE WindowOutReal(IX, IY, RVALUE, FRMAT)
            IMPLICIT NONE
            INTEGER         , INTENT (IN) :: IX
            INTEGER         , INTENT (IN) :: IY
            REAL            , INTENT (IN) :: RVALUE
            CHARACTER(LEN=*), INTENT (IN) :: FRMAT
        END SUBROUTINE WindowOutReal
!
        SUBROUTINE WindowOutStatusBar(IPART, STRING)
            IMPLICIT NONE
            INTEGER          , INTENT (IN) :: IPART
            CHARACTER(LEN=*) , INTENT (IN) :: STRING
        END SUBROUTINE WindowOutStatusBar
!
        SUBROUTINE WindowOutString(IX, IY, STRING)
            IMPLICIT NONE
            INTEGER         , INTENT (IN) :: IX
            INTEGER         , INTENT (IN) :: IY
            CHARACTER(LEN=*), INTENT (IN) :: STRING
        END SUBROUTINE WindowOutString
!
        FUNCTION WindowStringLength(STRING)
            IMPLICIT NONE
            INTEGER                       :: WindowStringLength
            CHARACTER(LEN=*), INTENT (IN) :: STRING
        END FUNCTION WindowStringLength
!
        SUBROUTINE WindowTagChar(TAG)
            IMPLICIT NONE
            CHARACTER       , INTENT (IN) :: TAG
        END SUBROUTINE WindowTagChar
!
!  CL : Area Clearing
!
        SUBROUTINE WindowClear
            IMPLICIT NONE
        END SUBROUTINE WindowClear
!
        SUBROUTINE WindowClearArea(IXTOPL, IYTOPL, IXBOTR, IYBOTR)
            IMPLICIT NONE
            INTEGER         , INTENT (IN) :: IXTOPL
            INTEGER         , INTENT (IN) :: IYTOPL
            INTEGER         , INTENT (IN) :: IXBOTR
            INTEGER         , INTENT (IN) :: IYBOTR
        END SUBROUTINE WindowClearArea
!
!  FS : Font Selection
!
        SUBROUTINE WindowFont(MYFONT)
            USE WINTTYPES
            IMPLICIT NONE
            TYPE (WIN_FONT) , INTENT (IN) :: MYFONT
        END SUBROUTINE WindowFont
!
        SUBROUTINE WindowFontBold(IBOLD)
            IMPLICIT NONE
            INTEGER         , INTENT (IN) :: IBOLD
        END SUBROUTINE WindowFontBold
!
        SUBROUTINE WindowFontColour(IFCOL, IBCOL)
            IMPLICIT NONE
            INTEGER         , INTENT (IN) :: IFCOL
            INTEGER         , INTENT (IN) :: IBCOL
        END SUBROUTINE WindowFontColour
!
        SUBROUTINE WindowFontItalic(ITALIC)
            IMPLICIT NONE
            INTEGER         , INTENT (IN) :: ITALIC
        END SUBROUTINE WindowFontItalic
!
        SUBROUTINE WindowFontRGB(ICOLNUM, MYRGB)
            USE WINTTYPES
            IMPLICIT NONE
            INTEGER         , INTENT (IN) :: ICOLNUM
            TYPE(WIN_RGB)   , INTENT (IN) :: MYRGB
        END SUBROUTINE WindowFontRGB
!
        SUBROUTINE WindowFontSize(IWIDTH, IHEIGHT)
            IMPLICIT NONE
            INTEGER         , INTENT (IN) :: IWIDTH
            INTEGER         , INTENT (IN) :: IHEIGHT
        END SUBROUTINE WindowFontSize
!
        SUBROUTINE WindowFontStyle(IFONTNUM)
            IMPLICIT NONE
            INTEGER         , INTENT (IN) :: IFONTNUM
        END SUBROUTINE WindowFontStyle
!
        SUBROUTINE WindowFontUnderline(IUNDER)
            IMPLICIT NONE
            INTEGER         , INTENT (IN) :: IUNDER
        END SUBROUTINE WindowFontUnderline
!
!  BM : Bitmap management
!
        SUBROUTINE WBitmapCreate(IHANDLE,IWIDTH,IHEIGHT)
            IMPLICIT NONE
            INTEGER         , INTENT (OUT)          :: IHANDLE
            INTEGER         , INTENT (IN)           :: IWIDTH
            INTEGER         , INTENT (IN)           :: IHEIGHT
        END SUBROUTINE WBitmapCreate
!
        SUBROUTINE WBitmapDestroy(IHANDLE)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)           :: IHANDLE
        END SUBROUTINE WBitmapDestroy
!
        SUBROUTINE WBitmapGet(IHANDLE, IMETHOD, IGX1, IGY1, IGX2, IGY2)
            IMPLICIT NONE
            INTEGER         , INTENT (IN OUT)       :: IHANDLE
            INTEGER         , INTENT (IN)           :: IMETHOD
            INTEGER         , INTENT (IN), OPTIONAL :: IGX1
            INTEGER         , INTENT (IN), OPTIONAL :: IGY1
            INTEGER         , INTENT (IN), OPTIONAL :: IGX2
            INTEGER         , INTENT (IN), OPTIONAL :: IGY2
        END SUBROUTINE WBitmapGet
!
        SUBROUTINE WBitmapPut(IHANDLE,IMETHOD,ISTRETCH,IGX1,IGY1,IGX2,IGY2)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)           :: IHANDLE
            INTEGER         , INTENT (IN)           :: IMETHOD
            INTEGER         , INTENT (IN)           :: ISTRETCH
            INTEGER         , INTENT (IN), OPTIONAL :: IGX1
            INTEGER         , INTENT (IN), OPTIONAL :: IGY1
            INTEGER         , INTENT (IN), OPTIONAL :: IGX2
            INTEGER         , INTENT (IN), OPTIONAL :: IGY2
        END SUBROUTINE WBitmapPut
!
        SUBROUTINE WBitmapPutPart(IHANDLE, IMETHOD, IXSRC,  IYSRC, &
                                  IWIDTH,  IHEIGHT, IXDEST, IYDEST)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)           :: IHANDLE
            INTEGER         , INTENT (IN)           :: IMETHOD
            INTEGER         , INTENT (IN)           :: IXSRC
            INTEGER         , INTENT (IN)           :: IYSRC
            INTEGER         , INTENT (IN)           :: IWIDTH
            INTEGER         , INTENT (IN)           :: IHEIGHT
            INTEGER         , INTENT (IN), OPTIONAL :: IXDEST
            INTEGER         , INTENT (IN), OPTIONAL :: IYDEST
        END SUBROUTINE WBitmapPutPart
!
        SUBROUTINE WBitmapView(IHANDLE,IXPOS,IYPOS,ITYPE)
            IMPLICIT NONE
            INTEGER, INTENT(IN)           :: IHANDLE
            INTEGER, INTENT(IN), OPTIONAL :: IXPOS
            INTEGER, INTENT(IN), OPTIONAL :: IYPOS
            INTEGER, INTENT(IN), OPTIONAL :: ITYPE
        END SUBROUTINE WBitmapView
!
!  ED : Text Editor
!
        SUBROUTINE WEditFile(FILENAME,MODE,IMENU,IFLAGS,IFONT,ISIZE,FILTER)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT(IN)           :: FILENAME
            INTEGER,          INTENT(IN), OPTIONAL :: MODE
            INTEGER,          INTENT(IN), OPTIONAL :: IMENU
            INTEGER,          INTENT(IN), OPTIONAL :: IFLAGS
            INTEGER,          INTENT(IN), OPTIONAL :: IFONT
            INTEGER,          INTENT(IN), OPTIONAL :: ISIZE
            CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: FILTER
        END SUBROUTINE WEditFile
!
        SUBROUTINE WEditGetCommand(COMMAND)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT(OUT) :: COMMAND
        END SUBROUTINE WEditGetCommand
      END INTERFACE
!
      INTERFACE WEditGetText
        SUBROUTINE WEditGetText1(CTEXT,NCHARS)
            IMPLICIT NONE
            INTEGER,          INTENT(IN)                     :: NCHARS
            CHARACTER(LEN=1), INTENT(OUT), DIMENSION(NCHARS) :: CTEXT
        END SUBROUTINE WEditGetText1
!
        SUBROUTINE WEditGetText2(CTEXT)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT(OUT) :: CTEXT
        END SUBROUTINE WEditGetText2
      END INTERFACE
!
      INTERFACE
        SUBROUTINE WEditGetTextPart(CTEXT,ICSTART)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT(OUT) :: CTEXT
            INTEGER,          INTENT(IN)  :: ICSTART
        END SUBROUTINE WEditGetTextPart
!
        INTEGER FUNCTION WEditPos(ICONV,IPOS)
            IMPLICIT NONE
            INTEGER, INTENT(IN) :: ICONV
            INTEGER, INTENT(IN) :: IPOS
        END FUNCTION WEditPos
!
        SUBROUTINE WEditPutCommand(COMMAND)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT(IN) :: COMMAND
        END SUBROUTINE WEditPutCommand
      END INTERFACE
!
      INTERFACE WEditPutText
        SUBROUTINE WEditPutText1(CTEXT,NCHARS)
            IMPLICIT NONE
            INTEGER,          INTENT(IN)                    :: NCHARS
            CHARACTER(LEN=1), INTENT(IN), DIMENSION(NCHARS) :: CTEXT
        END SUBROUTINE WEditPutText1
!
        SUBROUTINE WEditPutText2(CTEXT)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT(IN) :: CTEXT
        END SUBROUTINE WEditPutText2
      END INTERFACE
!
      INTERFACE
        SUBROUTINE WEditPutTextPart(CTEXT,ICSTART,ICEND)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT(IN)           :: CTEXT
            INTEGER,          INTENT(IN)           :: ICSTART
            INTEGER,          INTENT(IN), OPTIONAL :: ICEND
        END SUBROUTINE WEditPutTextPart
!
        SUBROUTINE WEditPrompt(STRING)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT(IN) :: STRING
        END SUBROUTINE WEditPrompt
!
!  MH : Message Handling
!
        SUBROUTINE WMessage(ITYPE, VALUE)
            USE WINTTYPES
            IMPLICIT NONE
            INTEGER          , INTENT (OUT) :: ITYPE
            TYPE(WIN_MESSAGE), INTENT (OUT) :: VALUE
        END SUBROUTINE WMessage
!
        SUBROUTINE WMessageEnable(ITYPE, ISWITCH)
            IMPLICIT NONE
            INTEGER          , INTENT (IN ) :: ITYPE
            INTEGER          , INTENT (IN ) :: ISWITCH
        END SUBROUTINE WMessageEnable
!
        SUBROUTINE WMessagePeek(ITYPE, VALUE)
            USE WINTTYPES
            IMPLICIT NONE
            INTEGER          , INTENT (OUT) :: ITYPE
            TYPE(WIN_MESSAGE), INTENT (OUT) :: VALUE
        END SUBROUTINE WMessagePeek
!
        SUBROUTINE WMessageTimer(MSECONDS,ID)
            IMPLICIT NONE
            INTEGER, INTENT (IN)          :: MSECONDS
            INTEGER, INTENT (IN),OPTIONAL :: ID
        END SUBROUTINE WMessageTimer
!
!  MN : Menu Management
!
        SUBROUTINE WMenuFloating(MENUID,IXPOS,IYPOS)
            IMPLICIT NONE
            INTEGER          , INTENT (IN) :: MENUID
            INTEGER          , INTENT (IN) :: IXPOS
            INTEGER          , INTENT (IN) :: IYPOS
        END SUBROUTINE WMenuFloating
!
        FUNCTION WMenuGetState(MENUITEM,IPROP)
            IMPLICIT NONE
            INTEGER                           WMenuGetState
            INTEGER          , INTENT (IN) :: MENUITEM
            INTEGER          , INTENT (IN) :: IPROP
        END FUNCTION WMenuGetState
!
        SUBROUTINE WMenuItemDelete(MENUITEM)
            IMPLICIT NONE
            INTEGER          , INTENT (IN) :: MENUITEM
        END SUBROUTINE WMenuItemDelete
!
        SUBROUTINE WMenuItemInsert(IDPOS,IPOS,IDENT,CVALUE)
            IMPLICIT NONE
            INTEGER          , INTENT (IN)           :: IDPOS
            INTEGER          , INTENT (IN)           :: IPOS
            INTEGER          , INTENT (IN)           :: IDENT
            CHARACTER(LEN=*) , INTENT (IN), OPTIONAL :: CVALUE
        END SUBROUTINE WMenuItemInsert
!
        SUBROUTINE WMenuRoot(MENUID)
            IMPLICIT NONE
            INTEGER          , INTENT (IN) :: MENUID
        END SUBROUTINE WMenuRoot
!
        SUBROUTINE WMenuSetState(MENUITEM,IPROP,IVALUE)
            IMPLICIT NONE
            INTEGER          , INTENT (IN) :: MENUITEM
            INTEGER          , INTENT (IN) :: IPROP
            INTEGER          , INTENT (IN) :: IVALUE
        END SUBROUTINE WMenuSetState
!
        SUBROUTINE WMenuSetString(MENUITEM,STRING)
            IMPLICIT NONE
            INTEGER         , INTENT (IN) :: MENUITEM
            CHARACTER(LEN=*), INTENT (IN) :: STRING
        END SUBROUTINE WMenuSetString
!
        SUBROUTINE WMenuToolbar(IDTOOLBAR)
            IMPLICIT NONE
            INTEGER         , INTENT (IN) :: IDTOOLBAR
        END SUBROUTINE WMenuToolbar
!
        SUBROUTINE WMenuTooltip(IDENT,STRING)
            IMPLICIT NONE
            INTEGER         , INTENT (IN) :: IDENT
            CHARACTER(LEN=*), INTENT (IN) :: STRING
        END SUBROUTINE WMenuTooltip
!
!  DM(1) : General Dialog Management
!
        SUBROUTINE WDialogClearField(IFIELD)
            IMPLICIT NONE
            INTEGER         , INTENT (IN) :: IFIELD
        END SUBROUTINE WDialogClearField
!
        SUBROUTINE WDialogColour(IFIELD,FORECOL,BACKCOL)
            USE WINTTYPES
            IMPLICIT NONE
            INTEGER,        INTENT(IN) :: IFIELD
            TYPE (WIN_RGB), INTENT(IN) :: FORECOL
            TYPE (WIN_RGB), INTENT(IN) :: BACKCOL
        END SUBROUTINE WDialogColour
!
        SUBROUTINE WDialogFieldState(IFIELD,ISTATE)
            IMPLICIT NONE
            INTEGER         , INTENT (IN) :: IFIELD
            INTEGER         , INTENT (IN) :: ISTATE
        END SUBROUTINE WDialogFieldState
!
        SUBROUTINE WDialogHide
            IMPLICIT NONE
        END SUBROUTINE WDialogHide
!
        SUBROUTINE WDialogLoad(IDIALOG)
            IMPLICIT NONE
            INTEGER         , INTENT (IN) :: IDIALOG
        END SUBROUTINE WDialogLoad
!
        SUBROUTINE WDialogRangeDouble(IFIELD,DFMIN,DFMAX)
            IMPLICIT NONE
            INTEGER         , INTENT (IN) :: IFIELD
            DOUBLE PRECISION, INTENT (IN) :: DFMIN
            DOUBLE PRECISION, INTENT (IN) :: DFMAX
        END SUBROUTINE WDialogRangeDouble
!
        SUBROUTINE WDialogRangeInteger(IFIELD,IFMIN,IFMAX)
            IMPLICIT NONE
            INTEGER         , INTENT (IN) :: IFIELD
            INTEGER         , INTENT (IN) :: IFMIN
            INTEGER         , INTENT (IN) :: IFMAX
        END SUBROUTINE WDialogRangeInteger
!
        SUBROUTINE WDialogRangeReal(IFIELD,RFMIN,RFMAX)
            IMPLICIT NONE
            INTEGER         , INTENT (IN) :: IFIELD
            REAL            , INTENT (IN) :: RFMIN
            REAL            , INTENT (IN) :: RFMAX
        END SUBROUTINE WDialogRangeReal
!
        SUBROUTINE WDialogRangeProgressBar(IFIELD,IPMIN,IPMAX)
            IMPLICIT NONE
            INTEGER         , INTENT (IN) :: IFIELD
            INTEGER         , INTENT (IN) :: IPMIN
            INTEGER         , INTENT (IN) :: IPMAX
        END SUBROUTINE WDialogRangeProgressBar
!
        SUBROUTINE WDialogRangeTrackBar(IFIELD,IPOSMIN,IPOSMAX,ITICKSTEP)
            IMPLICIT NONE
            INTEGER         , INTENT(IN)           :: IFIELD
            INTEGER         , INTENT(IN)           :: IPOSMIN
            INTEGER         , INTENT(IN)           :: IPOSMAX
            INTEGER         , INTENT(IN), OPTIONAL :: ITICKSTEP
        END SUBROUTINE WDialogRangeTrackBar
!
        SUBROUTINE WDialogSelect(IDIALOG)
            IMPLICIT NONE
            INTEGER         , INTENT (IN) :: IDIALOG
        END SUBROUTINE WDialogSelect
!
        SUBROUTINE WDialogSetField(IFIELD)
            IMPLICIT NONE
            INTEGER         , INTENT (IN) :: IFIELD
        END SUBROUTINE WDialogSetField
!
        SUBROUTINE WDialogSetTab(IFIELD,ITAB)
            IMPLICIT NONE
            INTEGER         , INTENT (IN) :: IFIELD
            INTEGER         , INTENT (IN) :: ITAB
        END SUBROUTINE WDialogSetTab
!
        SUBROUTINE WDialogShow(IXPOS,IYPOS,IFIELD,ITYPE)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)           :: IXPOS
            INTEGER         , INTENT (IN)           :: IYPOS
            INTEGER         , INTENT (IN), OPTIONAL :: IFIELD
            INTEGER         , INTENT (IN), OPTIONAL :: ITYPE
        END SUBROUTINE WDialogShow
!
        SUBROUTINE WDialogSpinnerStep(IFIELD,INORMAL,IFAST)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)           :: IFIELD
            INTEGER         , INTENT (IN)           :: INORMAL
            INTEGER         , INTENT (IN), OPTIONAL :: IFAST
        END SUBROUTINE WDialogSpinnerStep
!
        SUBROUTINE WDialogTabState(IFIELD,ITAB,ISTATE)
            IMPLICIT NONE
            INTEGER, INTENT(IN) :: IFIELD
            INTEGER, INTENT(IN) :: ITAB
            INTEGER, INTENT(IN) :: ISTATE
        END SUBROUTINE WDialogTabState
!
        SUBROUTINE WDialogTabstops(IFIELD,ITABS,NTABS)
            IMPLICIT NONE
            INTEGER, INTENT(IN)                   :: IFIELD
            INTEGER, INTENT(IN)                   :: NTABS
            INTEGER, INTENT(IN), DIMENSION(NTABS) :: ITABS
        END SUBROUTINE WDialogTabstops
!
        SUBROUTINE WDialogTitle(CTITLE)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN)  :: CTITLE
        END SUBROUTINE
!
        SUBROUTINE WDialogTrackBarStep(IFIELD,INORMAL,IFAST)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)           :: IFIELD
            INTEGER         , INTENT (IN)           :: INORMAL
            INTEGER         , INTENT (IN), OPTIONAL :: IFAST
        END SUBROUTINE WDialogTrackBarStep
!
        SUBROUTINE WDialogUnload(FLAGS)
            IMPLICIT NONE
            INTEGER         , INTENT (IN), OPTIONAL :: FLAGS
        END SUBROUTINE WDialogUnload
!
!  DM(2) : Dialog Field Assignment & Retrieval
!
        SUBROUTINE WDialogGetCheckBox(IFIELD,ISTATE)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)  :: IFIELD
            INTEGER         , INTENT (OUT) :: ISTATE
        END SUBROUTINE WDialogGetCheckBox
!
!
        SUBROUTINE WDialogGetDouble(IFIELD,DVALUE)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)  :: IFIELD
            DOUBLE PRECISION, INTENT (OUT) :: DVALUE
        END SUBROUTINE WDialogGetDouble
!
        SUBROUTINE WDialogGetInteger(IFIELD,IVALUE)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)  :: IFIELD
            INTEGER         , INTENT (OUT) :: IVALUE
        END SUBROUTINE WDialogGetInteger
!
      END INTERFACE
!
      INTERFACE WDialogGetMenu
        SUBROUTINE WDialogGetMenu1(IFIELD,IOPTION,CVALUE)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)            :: IFIELD
            INTEGER         , INTENT (OUT)           :: IOPTION
            CHARACTER(LEN=*), INTENT (OUT), OPTIONAL :: CVALUE
        END SUBROUTINE WDialogGetMenu1
!
        SUBROUTINE WDialogGetMenu2(IFIELD,IOPTION)
            IMPLICIT NONE
            INTEGER         ,      INTENT (IN)            :: IFIELD
            INTEGER, DIMENSION(*), INTENT (OUT)           :: IOPTION
        END SUBROUTINE WDialogGetMenu2
      END INTERFACE
!
      INTERFACE
        SUBROUTINE WDialogGetRadioButton(IFIELD,IVALUE)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)  :: IFIELD
            INTEGER         , INTENT (OUT) :: IVALUE
        END SUBROUTINE WDialogGetRadioButton
!
        SUBROUTINE WDialogGetReal(IFIELD,RVALUE)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)  :: IFIELD
            REAL            , INTENT (OUT) :: RVALUE
        END SUBROUTINE WDialogGetReal
!
        SUBROUTINE WDialogGetString(IFIELD,CVALUE)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)  :: IFIELD
            CHARACTER(LEN=*), INTENT (OUT) :: CVALUE
        END SUBROUTINE WDialogGetString
!
        SUBROUTINE WDialogGetStringLength(IFIELD,LENGTH)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)  :: IFIELD
            INTEGER         , INTENT (OUT) :: LENGTH
        END SUBROUTINE WDialogGetStringLength
!
        SUBROUTINE WDialogGetTab(IFIELD,ITAB)
            IMPLICIT NONE
            INTEGER, INTENT(IN)  :: IFIELD
            INTEGER, INTENT(OUT) :: ITAB
        END SUBROUTINE WDialogGetTab
!
        SUBROUTINE WDialogGetTrackBar(IFIELD,IPOS,ISTART,IEND)
            IMPLICIT NONE
            INTEGER, INTENT(IN)            :: IFIELD
            INTEGER, INTENT(OUT)           :: IPOS
            INTEGER, INTENT(OUT), OPTIONAL :: ISTART
            INTEGER, INTENT(OUT), OPTIONAL :: IEND
        END SUBROUTINE WDialogGetTrackBar
!
        SUBROUTINE WDialogPutCheckBox(IFIELD,ISTATE)
            IMPLICIT NONE
            INTEGER         , INTENT (IN) :: IFIELD
            INTEGER         , INTENT (IN) :: ISTATE
        END SUBROUTINE WDialogPutCheckBox
!
        SUBROUTINE WDialogPutDouble(IFIELD,DVALUE,FORMAT)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)           :: IFIELD
            DOUBLE PRECISION, INTENT (IN)           :: DVALUE
            CHARACTER(LEN=*), INTENT (IN), OPTIONAL :: FORMAT
        END SUBROUTINE WDialogPutDouble
!
        SUBROUTINE WDialogPutInteger(IFIELD,IVALUE)
            IMPLICIT NONE
            INTEGER         , INTENT (IN) :: IFIELD
            INTEGER         , INTENT (IN) :: IVALUE
        END SUBROUTINE WDialogPutInteger
!
        SUBROUTINE WDialogPutImage(IFIELD,IMAGE,ITYPE)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)           :: IFIELD
            INTEGER         , INTENT (IN)           :: IMAGE
            INTEGER         , INTENT (IN), OPTIONAL :: ITYPE
        END SUBROUTINE WDialogPutImage
      END INTERFACE
!
      INTERFACE WDialogPutMenu
        SUBROUTINE WDialogPutMenu1(IFIELD,OPTIONS,MAXOPT,IOPTION,CVALUE)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)               :: IFIELD
            INTEGER         , INTENT (IN)               :: MAXOPT
            CHARACTER(LEN=*), INTENT (IN), DIMENSION(*) :: OPTIONS
            INTEGER         , INTENT (IN)               :: IOPTION
            CHARACTER(LEN=*), INTENT (IN), OPTIONAL     :: CVALUE
        END SUBROUTINE WDialogPutMenu1
!
        SUBROUTINE WDialogPutMenu2(IFIELD,OPTIONS,MAXOPT,IOPTION)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)               :: IFIELD
            INTEGER         , INTENT (IN)               :: MAXOPT
            CHARACTER(LEN=*), INTENT (IN), DIMENSION(*) :: OPTIONS
            INTEGER         , INTENT (IN), DIMENSION(*) :: IOPTION
        END SUBROUTINE WDialogPutMenu2
      END INTERFACE
!
      INTERFACE WDialogPutOption
        SUBROUTINE WDialogPutOption1(IFIELD,IOPTION)
            IMPLICIT NONE
            INTEGER, INTENT (IN) :: IFIELD
            INTEGER, INTENT (IN) :: IOPTION
        END SUBROUTINE WDialogPutOption1
!
        SUBROUTINE WDialogPutOption2(IFIELD,IOPTION)
            IMPLICIT NONE
            INTEGER, INTENT (IN)               :: IFIELD
            INTEGER, INTENT (IN), DIMENSION(*) :: IOPTION
        END SUBROUTINE WDialogPutOption2
      END INTERFACE
!
      INTERFACE
        SUBROUTINE WDialogPutProgressBar(IFIELD,IVALUE,METHOD)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)           :: IFIELD
            INTEGER         , INTENT (IN)           :: IVALUE
            INTEGER         , INTENT (IN), OPTIONAL :: METHOD
        END SUBROUTINE WDialogPutProgressBar
!
        SUBROUTINE WDialogPutRadioButton(IFIELD)
            IMPLICIT NONE
            INTEGER         , INTENT (IN) :: IFIELD
        END SUBROUTINE WDialogPutRadioButton
!
        SUBROUTINE WDialogPutReal(IFIELD,RVALUE,FORMAT)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)           :: IFIELD
            REAL            , INTENT (IN)           :: RVALUE
            CHARACTER(LEN=*), INTENT (IN), OPTIONAL :: FORMAT
        END SUBROUTINE WDialogPutReal
!
        SUBROUTINE WDialogPutString(IFIELD,CVALUE)
            IMPLICIT NONE
            INTEGER         , INTENT (IN) :: IFIELD
            CHARACTER(LEN=*), INTENT (IN) :: CVALUE
        END SUBROUTINE WDialogPutString
!
        SUBROUTINE WDialogPutTrackBar(IFIELD,IPOS,ISTART,IEND)
            IMPLICIT NONE
            INTEGER, INTENT(IN)           :: IFIELD
            INTEGER, INTENT(IN)           :: IPOS
            INTEGER, INTENT(IN), OPTIONAL :: ISTART
            INTEGER, INTENT(IN), OPTIONAL :: IEND
        END SUBROUTINE WDialogPutTrackBar
!
!  DM(3) : Dialog Grid Field Assignment & Retrieval
!
        SUBROUTINE WGridClearCell(IFIELD,ICOL,IROW)
            IMPLICIT NONE
            INTEGER, INTENT(IN) :: IFIELD
            INTEGER, INTENT(IN) :: ICOL
            INTEGER, INTENT(IN) :: IROW
        END SUBROUTINE WGridClearCell
!
        SUBROUTINE WGridColourCell(IFIELD,ICOL,IROW,FORECOL,BACKCOL)
            USE WINTTYPES
            IMPLICIT NONE
            INTEGER,        INTENT(IN) :: IFIELD
            INTEGER,        INTENT(IN) :: ICOL
            INTEGER,        INTENT(IN) :: IROW
            TYPE (WIN_RGB), INTENT(IN) :: FORECOL
            TYPE (WIN_RGB), INTENT(IN) :: BACKCOL
        END SUBROUTINE WGridColourCell
!
        SUBROUTINE WGridColourColumn(IFIELD,ICOL,FORECOL,BACKCOL)
            USE WINTTYPES
            IMPLICIT NONE
            INTEGER,        INTENT(IN) :: IFIELD
            INTEGER,        INTENT(IN) :: ICOL
            TYPE (WIN_RGB), INTENT(IN) :: FORECOL
            TYPE (WIN_RGB), INTENT(IN) :: BACKCOL
        END SUBROUTINE WGridColourColumn
!
        SUBROUTINE WGridColourRow(IFIELD,IROW,FORECOL,BACKCOL)
            USE WINTTYPES
            IMPLICIT NONE
            INTEGER,        INTENT(IN) :: IFIELD
            INTEGER,        INTENT(IN) :: IROW
            TYPE (WIN_RGB), INTENT(IN) :: FORECOL
            TYPE (WIN_RGB), INTENT(IN) :: BACKCOL
        END SUBROUTINE WGridColourRow
!
        SUBROUTINE WGridColumns(IFIELD,NCOLS,ICOLS)
            IMPLICIT NONE
            INTEGER,                   INTENT(IN) :: IFIELD
            INTEGER,                   INTENT(IN) :: NCOLS
            INTEGER, DIMENSION(NCOLS), INTENT(IN) :: ICOLS
        END SUBROUTINE WGridColumns
!
        SUBROUTINE WGridGetCellCheckBox(IFIELD,ICOL,IROW,ISTATE)
            IMPLICIT NONE
            INTEGER,          INTENT(IN)  :: IFIELD
            INTEGER,          INTENT(IN)  :: ICOL
            INTEGER,          INTENT(IN)  :: IROW
            INTEGER,          INTENT(OUT) :: ISTATE
        END SUBROUTINE WGridGetCellCheckBox
!
        SUBROUTINE WGridGetCellDouble(IFIELD,ICOL,IROW,DVALUE)
            IMPLICIT NONE
            INTEGER,          INTENT(IN)  :: IFIELD
            INTEGER,          INTENT(IN)  :: ICOL
            INTEGER,          INTENT(IN)  :: IROW
            DOUBLE PRECISION, INTENT(OUT) :: DVALUE
        END SUBROUTINE WGridGetCellDouble
!
        SUBROUTINE WGridGetCellMenu(IFIELD,ICOL,IROW,IOPTION)
            IMPLICIT NONE
            INTEGER, INTENT(IN)  :: IFIELD
            INTEGER, INTENT(IN)  :: ICOL
            INTEGER, INTENT(IN)  :: IROW
            INTEGER, INTENT(OUT) :: IOPTION
        END SUBROUTINE WGridGetCellMenu
!
        SUBROUTINE WGridGetCellReal(IFIELD,ICOL,IROW,RVALUE)
            IMPLICIT NONE
            INTEGER, INTENT(IN)  :: IFIELD
            INTEGER, INTENT(IN)  :: ICOL
            INTEGER, INTENT(IN)  :: IROW
            REAL,    INTENT(OUT) :: RVALUE
        END SUBROUTINE WGridGetCellReal
!
        SUBROUTINE WGridGetCellInteger(IFIELD,ICOL,IROW,IVALUE)
            IMPLICIT NONE
            INTEGER, INTENT(IN)  :: IFIELD
            INTEGER, INTENT(IN)  :: ICOL
            INTEGER, INTENT(IN)  :: IROW
            INTEGER, INTENT(OUT) :: IVALUE
        END SUBROUTINE WGridGetCellInteger
!
        SUBROUTINE WGridGetCellString(IFIELD,ICOL,IROW,CVALUE)
            IMPLICIT NONE
            INTEGER,          INTENT(IN)  :: IFIELD
            INTEGER,          INTENT(IN)  :: ICOL
            INTEGER,          INTENT(IN)  :: IROW
            CHARACTER(LEN=*), INTENT(OUT) :: CVALUE
        END SUBROUTINE WGridGetCellString
!
        SUBROUTINE WGridGetCheckBox(IFIELD,ICOL,ISTATES,NVALUES)
            IMPLICIT NONE
            INTEGER, INTENT(IN)                  :: IFIELD
            INTEGER, INTENT(IN)                  :: ICOL
            INTEGER, INTENT(OUT)  , DIMENSION(*) :: ISTATES
            INTEGER, INTENT(INOUT)               :: NVALUES
        END SUBROUTINE WGridGetCheckBox
!
        SUBROUTINE WGridGetDouble(IFIELD,ICOL,DVALUES,NVALUES)
            IMPLICIT NONE
            INTEGER,          INTENT(IN)                  :: IFIELD
            INTEGER,          INTENT(IN)                  :: ICOL
            DOUBLE PRECISION, INTENT(OUT)  , DIMENSION(*) :: DVALUES
            INTEGER,          INTENT(INOUT)               :: NVALUES
        END SUBROUTINE WGridGetDouble
!
        SUBROUTINE WGridGetInteger(IFIELD,ICOL,IVALUES,NVALUES)
            IMPLICIT NONE
            INTEGER, INTENT(IN)                  :: IFIELD
            INTEGER, INTENT(IN)                  :: ICOL
            INTEGER, INTENT(OUT)  , DIMENSION(*) :: IVALUES
            INTEGER, INTENT(INOUT)               :: NVALUES
        END SUBROUTINE WGridGetInteger
!
        SUBROUTINE WGridGetMenu(IFIELD,ICOL,IOPTIONS,NOPTIONS)
            IMPLICIT NONE
            INTEGER, INTENT(IN)                  :: IFIELD
            INTEGER, INTENT(IN)                  :: ICOL
            INTEGER, INTENT(OUT)  , DIMENSION(*) :: IOPTIONS
            INTEGER, INTENT(INOUT)               :: NOPTIONS
        END SUBROUTINE WGridGetMenu
!
        SUBROUTINE WGridGetReal(IFIELD,ICOL,RVALUES,NVALUES)
            IMPLICIT NONE
            INTEGER, INTENT(IN)                  :: IFIELD
            INTEGER, INTENT(IN)                  :: ICOL
            REAL,    INTENT(OUT)  , DIMENSION(*) :: RVALUES
            INTEGER, INTENT(INOUT)               :: NVALUES
        END SUBROUTINE WGridGetReal
!
        SUBROUTINE WGridGetString(IFIELD,ICOL,CVALUES,NVALUES)
            IMPLICIT NONE
            INTEGER,          INTENT(IN)                  :: IFIELD
            INTEGER,          INTENT(IN)                  :: ICOL
            CHARACTER(LEN=*), INTENT(OUT)  , DIMENSION(*) :: CVALUES
            INTEGER,          INTENT(INOUT)               :: NVALUES
        END SUBROUTINE WGridGetString
!
        SUBROUTINE WGridLabelColumn(IFIELD,ICOL,STRING)
            IMPLICIT NONE
            INTEGER         , INTENT (IN) :: IFIELD
            INTEGER         , INTENT (IN) :: ICOL
            CHARACTER(LEN=*), INTENT (IN) :: STRING
        END SUBROUTINE WGridLabelColumn
!
        SUBROUTINE WGridLabelRow(IFIELD,IROW,STRING)
            IMPLICIT NONE
            INTEGER         , INTENT (IN) :: IFIELD
            INTEGER         , INTENT (IN) :: IROW
            CHARACTER(LEN=*), INTENT (IN) :: STRING
        END SUBROUTINE WGridLabelRow
!
        SUBROUTINE WGridPos(MSGVAL,ICOL,IROW)
            IMPLICIT NONE
            INTEGER, INTENT (IN)  :: MSGVAL
            INTEGER, INTENT (OUT) :: ICOL
            INTEGER, INTENT (OUT) :: IROW
        END SUBROUTINE WgridPos
!
        SUBROUTINE WGridPutCellCheckBox(IFIELD,ICOL,IROW,ISTATE)
            IMPLICIT NONE
            INTEGER,          INTENT(IN)           :: IFIELD
            INTEGER,          INTENT(IN)           :: ICOL
            INTEGER,          INTENT(IN)           :: IROW
            INTEGER,          INTENT(IN)           :: ISTATE
        END SUBROUTINE WGridPutCellCheckBox
!
        SUBROUTINE WGridPutCellDouble(IFIELD,ICOL,IROW,DVALUE,FORMAT)
            IMPLICIT NONE
            INTEGER,          INTENT(IN)           :: IFIELD
            INTEGER,          INTENT(IN)           :: ICOL
            INTEGER,          INTENT(IN)           :: IROW
            DOUBLE PRECISION, INTENT(IN)           :: DVALUE
            CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: FORMAT
        END SUBROUTINE WGridPutCellDouble
!
        SUBROUTINE WGridPutCellInteger(IFIELD,ICOL,IROW,IVALUE)
            IMPLICIT NONE
            INTEGER,          INTENT(IN)           :: IFIELD
            INTEGER,          INTENT(IN)           :: ICOL
            INTEGER,          INTENT(IN)           :: IROW
            INTEGER,          INTENT(IN)           :: IVALUE
        END SUBROUTINE WGridPutCellInteger
!
        SUBROUTINE WGridPutCellOption(IFIELD,ICOL,IROW,IOPTION)
            IMPLICIT NONE
            INTEGER,          INTENT(IN)           :: IFIELD
            INTEGER,          INTENT(IN)           :: ICOL
            INTEGER,          INTENT(IN)           :: IROW
            INTEGER,          INTENT(IN)           :: IOPTION
        END SUBROUTINE WGridPutCellOption
!
        SUBROUTINE WGridPutCellReal(IFIELD,ICOL,IROW,RVALUE,FORMAT)
            IMPLICIT NONE
            INTEGER,          INTENT(IN)           :: IFIELD
            INTEGER,          INTENT(IN)           :: ICOL
            INTEGER,          INTENT(IN)           :: IROW
            REAL,             INTENT(IN)           :: RVALUE
            CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: FORMAT
        END SUBROUTINE WGridPutCellReal
!
        SUBROUTINE WGridPutCellString(IFIELD,ICOL,IROW,CVALUE)
            IMPLICIT NONE
            INTEGER,          INTENT(IN) :: IFIELD
            INTEGER,          INTENT(IN) :: ICOL
            INTEGER,          INTENT(IN) :: IROW
            CHARACTER(LEN=*), INTENT(IN) :: CVALUE
        END SUBROUTINE WGridPutCellString
!
        SUBROUTINE WGridPutCheckBox(IFIELD,ICOL,ISTATES,NVALUES)
            IMPLICIT NONE
            INTEGER,          INTENT(IN)               :: IFIELD
            INTEGER,          INTENT(IN)               :: ICOL
            INTEGER,          INTENT(IN), DIMENSION(*) :: ISTATES
            INTEGER,          INTENT(IN)               :: NVALUES
        END SUBROUTINE WGridPutCheckBox
!
        SUBROUTINE WGridPutDouble(IFIELD,ICOL,DVALUES,NVALUES,FORMAT)
            IMPLICIT NONE
            INTEGER,          INTENT(IN)               :: IFIELD
            INTEGER,          INTENT(IN)               :: ICOL
            DOUBLE PRECISION, INTENT(IN), DIMENSION(*) :: DVALUES
            INTEGER,          INTENT(IN)               :: NVALUES
            CHARACTER(LEN=*), INTENT(IN), OPTIONAL     :: FORMAT
        END SUBROUTINE WGridPutDouble
!
        SUBROUTINE WGridPutInteger(IFIELD,ICOL,IVALUES,NVALUES)
            IMPLICIT NONE
            INTEGER, INTENT(IN)               :: IFIELD
            INTEGER, INTENT(IN)               :: ICOL
            INTEGER, INTENT(IN), DIMENSION(*) :: IVALUES
            INTEGER, INTENT(IN)               :: NVALUES
        END SUBROUTINE WGridPutInteger
!
        SUBROUTINE WGridPutMenu(IFIELD,ICOL,OPTION,MAXOPT,IOPTIONS,NOPTIONS)
            IMPLICIT NONE
            INTEGER,          INTENT(IN)               :: IFIELD
            INTEGER,          INTENT(IN)               :: ICOL
            CHARACTER(LEN=*), INTENT(IN), DIMENSION(*) :: OPTION
            INTEGER,          INTENT(IN)               :: MAXOPT
            INTEGER,          INTENT(IN), DIMENSION(*) :: IOPTIONS
            INTEGER,          INTENT(IN)               :: NOPTIONS
        END SUBROUTINE
!
        SUBROUTINE WGridPutOption(IFIELD,ICOL,IOPTIONS,NOPTIONS)
            IMPLICIT NONE
            INTEGER, INTENT(IN)               :: IFIELD
            INTEGER, INTENT(IN)               :: ICOL
            INTEGER, INTENT(IN), DIMENSION(*) :: IOPTIONS
            INTEGER, INTENT(IN)               :: NOPTIONS
        END SUBROUTINE WGridPutOption
!
        SUBROUTINE WGridPutReal(IFIELD,ICOL,RVALUES,NVALUES,FORMAT)
            IMPLICIT NONE
            INTEGER,          INTENT(IN)               :: IFIELD
            INTEGER,          INTENT(IN)               :: ICOL
            REAL,             INTENT(IN), DIMENSION(*) :: RVALUES
            INTEGER,          INTENT(IN)               :: NVALUES
            CHARACTER(LEN=*), INTENT(IN), OPTIONAL     :: FORMAT
        END SUBROUTINE WGridPutReal
!
        SUBROUTINE WGridPutString(IFIELD,ICOL,CVALUES,NVALUES)
            IMPLICIT NONE
            INTEGER,          INTENT(IN)               :: IFIELD
            INTEGER,          INTENT(IN)               :: ICOL
            CHARACTER(LEN=*), INTENT(IN), DIMENSION(*) :: CVALUES
            INTEGER,          INTENT(IN)               :: NVALUES
        END SUBROUTINE WGridPutString
!
        SUBROUTINE WGridRangeDouble(IFIELD,ICOL,DMINVAL,DMAXVAL)
            IMPLICIT NONE
            INTEGER,          INTENT(IN) :: IFIELD
            INTEGER,          INTENT(IN) :: ICOL
            DOUBLE PRECISION, INTENT(IN) :: DMINVAL
            DOUBLE PRECISION, INTENT(IN) :: DMAXVAL
        END SUBROUTINE WGridRangeDouble
!
        SUBROUTINE WGridRangeInteger(IFIELD,ICOL,IMINVAL,IMAXVAL)
            IMPLICIT NONE
            INTEGER, INTENT(IN) :: IFIELD
            INTEGER, INTENT(IN) :: ICOL
            INTEGER, INTENT(IN) :: IMINVAL
            INTEGER, INTENT(IN) :: IMAXVAL
        END SUBROUTINE WGridRangeInteger
!
        SUBROUTINE WGridRangeReal(IFIELD,ICOL,RMINVAL,RMAXVAL)
            IMPLICIT NONE
            INTEGER, INTENT(IN) :: IFIELD
            INTEGER, INTENT(IN) :: ICOL
            REAL,    INTENT(IN) :: RMINVAL
            REAL,    INTENT(IN) :: RMAXVAL
        END SUBROUTINE WGridRangeReal
!
        SUBROUTINE WGridRows(IFIELD,NROWS)
            IMPLICIT NONE
            INTEGER, INTENT(IN) :: IFIELD
            INTEGER, INTENT(IN) :: NROWS
        END SUBROUTINE WGridRows
!
        SUBROUTINE WGridSetCell(IFIELD,ICOL,IROW)
            IMPLICIT NONE
            INTEGER, INTENT(IN) :: IFIELD
            INTEGER, INTENT(IN) :: ICOL
            INTEGER, INTENT(IN) :: IROW
        END SUBROUTINE WGridSetCell
!
        SUBROUTINE WGridState(IFIELD,ICOL,ISTATE)
            IMPLICIT NONE
            INTEGER, INTENT(IN) :: IFIELD
            INTEGER, INTENT(IN) :: ICOL
            INTEGER, INTENT(IN) :: ISTATE
        END SUBROUTINE WGridState
!
        SUBROUTINE WGridStateCell(IFIELD,ICOL,IROW,ISTATE)
            IMPLICIT NONE
            INTEGER, INTENT(IN) :: IFIELD
            INTEGER, INTENT(IN) :: ICOL
            INTEGER, INTENT(IN) :: IROW
            INTEGER, INTENT(IN) :: ISTATE
        END SUBROUTINE WGridStateCell
!
!  CD : Common Dialog Management
!
        SUBROUTINE WHardcopyDevices
            IMPLICIT NONE
        END SUBROUTINE WHardcopyDevices
!
        SUBROUTINE WHardcopyOptions(IUNITS)
            IMPLICIT NONE
            INTEGER, INTENT (IN), OPTIONAL :: IUNITS
        END SUBROUTINE WHardcopyOptions
!
        SUBROUTINE WHardcopyOptionsText(IUNITS)
            IMPLICIT NONE
            INTEGER, INTENT (IN), OPTIONAL :: IUNITS
        END SUBROUTINE WHardcopyOptionsText
!
        SUBROUTINE WMessageBox(IBUTTON, ICON, IDEF, MESG, TITLE)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)     :: IBUTTON
            INTEGER         , INTENT (IN)     :: ICON
            INTEGER         , INTENT (IN)     :: IDEF
            CHARACTER(LEN=*), INTENT (IN)     :: MESG
            CHARACTER(LEN=*), INTENT (IN)     :: TITLE
        END SUBROUTINE WMessageBox
!
        SUBROUTINE WPrintImageOptions(IUNITS)
            IMPLICIT NONE
            INTEGER, INTENT (IN), OPTIONAL :: IUNITS
        END SUBROUTINE WPrintImageOptions
!
        SUBROUTINE WSelectDir(IFLAGS, PATH, TITLE)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)     :: IFLAGS
            CHARACTER(LEN=*), INTENT (IN OUT) :: PATH
            CHARACTER(LEN=*), INTENT (IN)     :: TITLE
        END SUBROUTINE WSelectDir
!
      END INTERFACE
!
      INTERFACE WSelectFile
!
        SUBROUTINE WSelectFile1(IFILTERID, IFLAGS, FILEDIR, TITLE, IEXTINDEX)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)               :: IFILTERID
            INTEGER         , INTENT (IN)               :: IFLAGS
            CHARACTER(LEN=*), INTENT (IN OUT)           :: FILEDIR
            CHARACTER(LEN=*), INTENT (IN)               :: TITLE
            INTEGER         , INTENT (IN OUT), OPTIONAL :: IEXTINDEX
        END SUBROUTINE WSelectFile1
!
        SUBROUTINE WSelectFile2(IFILTERID, IFLAGS, FILEDIR, TITLE, IEXTINDEX)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN)               :: IFILTERID
            INTEGER         , INTENT (IN)               :: IFLAGS
            CHARACTER(LEN=*), INTENT (IN OUT)           :: FILEDIR
            CHARACTER(LEN=*), INTENT (IN)               :: TITLE
            INTEGER         , INTENT (IN OUT), OPTIONAL :: IEXTINDEX
        END SUBROUTINE WSelectFile2
!
      END INTERFACE
!
      INTERFACE
!
        SUBROUTINE WSelectColour(COLRGB)
            USE WINTTYPES
            IMPLICIT NONE
            TYPE (WIN_RGB)  , INTENT (IN OUT) :: COLRGB
        END SUBROUTINE WSelectColour
!
!  GG : General Graphics
!
        SUBROUTINE IGrGetPixelRGB(XPOS,YPOS,IRED,IGREEN,IBLUE)
            IMPLICIT NONE
            REAL    , INTENT (IN)  :: XPOS
            REAL    , INTENT (IN)  :: YPOS
            INTEGER , INTENT (OUT) :: IRED
            INTEGER , INTENT (OUT) :: IGREEN
            INTEGER , INTENT (OUT) :: IBLUE
        END SUBROUTINE IGrGetPixelRGB
!
        SUBROUTINE IGrInit(TYPE,NHPIX,NVPIX,NCOLOR)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN)           :: TYPE
            INTEGER         , INTENT (IN), OPTIONAL :: NHPIX
            INTEGER         , INTENT (IN), OPTIONAL :: NVPIX
            INTEGER         , INTENT (IN), OPTIONAL :: NCOLOR
        END SUBROUTINE IGrInit
!
        SUBROUTINE IGrPause(ACTION)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: ACTION
        END SUBROUTINE IGrPause
!
        SUBROUTINE IGrSelect(ITARGET,IDENT)
            IMPLICIT NONE
            INTEGER, INTENT (IN)           :: ITARGET
            INTEGER, INTENT (IN), OPTIONAL :: IDENT
        END SUBROUTINE IGrSelect
!
!  GA : Graphics Area/Units
!
        SUBROUTINE IGrArea(XLEFT,YLOWER,XRIGHT,YUPPER)
            IMPLICIT NONE
            REAL, INTENT (IN) :: XLEFT
            REAL, INTENT (IN) :: YLOWER
            REAL, INTENT (IN) :: XRIGHT
            REAL, INTENT (IN) :: YUPPER
        END SUBROUTINE IGrArea
!
        SUBROUTINE IGrAreaClear()
            IMPLICIT NONE
        END SUBROUTINE IGrAreaClear
!
        SUBROUTINE IGrUnits(XLEFT,YLOWER,XRIGHT,YUPPER)
            IMPLICIT NONE
            REAL, INTENT (IN) :: XLEFT
            REAL, INTENT (IN) :: YLOWER
            REAL, INTENT (IN) :: XRIGHT
            REAL, INTENT (IN) :: YUPPER
        END SUBROUTINE IGrUnits
!
        SUBROUTINE IGrUnitsFromPixels(IXPIXL,IYPIXL,XPOS,YPOS)
            IMPLICIT NONE
            INTEGER, INTENT (IN)  :: IXPIXL
            INTEGER, INTENT (IN)  :: IYPIXL
            REAL   , INTENT (OUT) :: XPOS
            REAL   , INTENT (OUT) :: YPOS
        END SUBROUTINE IGrUnitsFromPixels
!
        SUBROUTINE IGrUnitsToPixels(XPOS,YPOS,IXPIXL,IYPIXL)
            IMPLICIT NONE
            REAL   , INTENT (IN)  :: XPOS
            REAL   , INTENT (IN)  :: YPOS
            INTEGER, INTENT (OUT) :: IXPIXL
            INTEGER, INTENT (OUT) :: IYPIXL
        END SUBROUTINE IGrUnitsToPixels
!
        SUBROUTINE IGrViewport(XLEFT,YLOWER,XRIGHT,YUPPER)
            IMPLICIT NONE
            REAL, INTENT (IN) :: XLEFT
            REAL, INTENT (IN) :: YLOWER
            REAL, INTENT (IN) :: XRIGHT
            REAL, INTENT (IN) :: YUPPER
        END SUBROUTINE IGrViewport
!
!  GS : Graphics Style Selection
!
        SUBROUTINE IGrColour(COLOUR)
            IMPLICIT NONE
            CHARACTER(LEN=*) , INTENT (IN) :: COLOUR
        END SUBROUTINE IGrColour
!
        SUBROUTINE IGrColourModel(NBITS)
            IMPLICIT NONE
            INTEGER , INTENT (IN) :: NBITS
        END SUBROUTINE IGrColourModel
!
        SUBROUTINE IGrColourN(NCOLOR)
            IMPLICIT NONE
            INTEGER , INTENT (IN) :: NCOLOR
        END SUBROUTINE IGrColourN
!
        SUBROUTINE IGrFillPattern(ISTYLE,IDENS,IANGLE)
            IMPLICIT NONE
            INTEGER , INTENT (IN)           :: ISTYLE
            INTEGER , INTENT (IN), OPTIONAL :: IDENS
            INTEGER , INTENT (IN), OPTIONAL :: IANGLE
        END SUBROUTINE IGrFillPattern
!
        SUBROUTINE IGrLineType(ILTYPE)
            IMPLICIT NONE
            INTEGER , INTENT (IN), OPTIONAL :: ILTYPE
        END SUBROUTINE IGrLineType
!
        SUBROUTINE IGrLineWidth(ISCRNW,IHARDW)
            IMPLICIT NONE
            INTEGER , INTENT (IN), OPTIONAL :: ISCRNW
            INTEGER , INTENT (IN), OPTIONAL :: IHARDW
        END SUBROUTINE IGrLineWidth
!
        SUBROUTINE IGrPaletteHLS(NCOLOR,IHUE,LIGHT,ISATUR,IPOST)
            IMPLICIT NONE
            INTEGER , INTENT (IN)            :: NCOLOR
            INTEGER , INTENT (IN)            :: IHUE
            INTEGER , INTENT (IN)            :: LIGHT
            INTEGER , INTENT (IN)            :: ISATUR
            INTEGER , INTENT (IN) , OPTIONAL :: IPOST
        END SUBROUTINE IGrPaletteHLS
!
        SUBROUTINE IGrPaletteInit()
            IMPLICIT NONE
        END SUBROUTINE IGrPaletteInit
!
        SUBROUTINE IGrPaletteRGB(NCOLOR,IRED,IGREEN,IBLUE,IPOST)
            IMPLICIT NONE
            INTEGER , INTENT (IN)            :: NCOLOR
            INTEGER , INTENT (IN)            :: IRED
            INTEGER , INTENT (IN)            :: IGREEN
            INTEGER , INTENT (IN)            :: IBLUE
            INTEGER , INTENT (IN) , OPTIONAL :: IPOST
        END SUBROUTINE IGrPaletteRGB
!
        SUBROUTINE IGrPlotMode(PLOTMD)
            IMPLICIT NONE
            CHARACTER(LEN=*) , INTENT (IN), OPTIONAL :: PLOTMD
        END SUBROUTINE IGrPlotMode
!
!  GD : Graphics Drawing/Movement
!
        SUBROUTINE IGrArc(XPOS,YPOS,RADIUS,SANGLE,AANGLE)
            IMPLICIT NONE
            REAL, INTENT (IN) :: XPOS
            REAL, INTENT (IN) :: YPOS
            REAL, INTENT (IN) :: RADIUS
            REAL, INTENT (IN) :: SANGLE
            REAL, INTENT (IN) :: AANGLE
        END SUBROUTINE IGrArc
!
        SUBROUTINE IGrArcRel(RADIUS,SANGLE,AANGLE)
            IMPLICIT NONE
            REAL, INTENT (IN) :: RADIUS
            REAL, INTENT (IN) :: SANGLE
            REAL, INTENT (IN) :: AANGLE
        END SUBROUTINE IGrArcRel
!
        SUBROUTINE IGrArrow(XPOS,YPOS,ALENGTH,ANGLE,IATYPE)
            IMPLICIT NONE
            REAL   , INTENT (IN) :: XPOS
            REAL   , INTENT (IN) :: YPOS
            REAL   , INTENT (IN) :: ALENGTH
            REAL   , INTENT (IN) :: ANGLE
            INTEGER, INTENT (IN) :: IATYPE
        END SUBROUTINE IGrArrow
!
        SUBROUTINE IGrArrowJoin(XFROM,YFROM,XTO,YTO,IATYPE)
            IMPLICIT NONE
            REAL   , INTENT (IN) :: XFROM
            REAL   , INTENT (IN) :: YFROM
            REAL   , INTENT (IN) :: XTO
            REAL   , INTENT (IN) :: YTO
            INTEGER, INTENT (IN) :: IATYPE
        END SUBROUTINE IGrArrowJoin
!
        SUBROUTINE IGrBlockCopy(XSOUR,YSOUR,XDEST,YDEST,WIDTH,HEIGHT)
            IMPLICIT NONE
            REAL, INTENT (IN) :: XSOUR
            REAL, INTENT (IN) :: YSOUR
            REAL, INTENT (IN) :: XDEST
            REAL, INTENT (IN) :: YDEST
            REAL, INTENT (IN) :: WIDTH
            REAL, INTENT (IN) :: HEIGHT
        END SUBROUTINE IGrBlockCopy
!
        SUBROUTINE IGrBlockMove(XSOUR,YSOUR,XDEST,YDEST,WIDTH,HEIGHT)
            IMPLICIT NONE
            REAL, INTENT (IN) :: XSOUR
            REAL, INTENT (IN) :: YSOUR
            REAL, INTENT (IN) :: XDEST
            REAL, INTENT (IN) :: YDEST
            REAL, INTENT (IN) :: WIDTH
            REAL, INTENT (IN) :: HEIGHT
        END SUBROUTINE IGrBlockMove
!
        SUBROUTINE IGrBorder()
            IMPLICIT NONE
        END SUBROUTINE IGrBorder
!
        SUBROUTINE IGrCircle(XPOS,YPOS,RADIUS)
            IMPLICIT NONE
            REAL, INTENT (IN) :: XPOS
            REAL, INTENT (IN) :: YPOS
            REAL, INTENT (IN) :: RADIUS
        END SUBROUTINE IGrCircle
!
        SUBROUTINE IGrCircleRel(RADIUS)
            IMPLICIT NONE
            REAL, INTENT (IN) :: RADIUS
        END SUBROUTINE IGrCircleRel
!
        SUBROUTINE IGrCurve(X,Y,NVERT,NSTEP)
            IMPLICIT NONE
            REAL   , INTENT (IN), DIMENSION(*) :: X
            REAL   , INTENT (IN), DIMENSION(*) :: Y
            INTEGER, INTENT (IN)               :: NVERT
            INTEGER, INTENT (IN), OPTIONAL     :: NSTEP
        END SUBROUTINE IGrCurve
!
        SUBROUTINE IGrEllipse(XPOS,YPOS,RADIUS,RATIO)
            IMPLICIT NONE
            REAL, INTENT (IN) :: XPOS
            REAL, INTENT (IN) :: YPOS
            REAL, INTENT (IN) :: RADIUS
            REAL, INTENT (IN) :: RATIO
        END SUBROUTINE IGrEllipse
!
        SUBROUTINE IGrEllipseRel(RADIUS,RATIO)
            IMPLICIT NONE
            REAL, INTENT (IN) :: RADIUS
            REAL, INTENT (IN) :: RATIO
        END SUBROUTINE IGrEllipseRel
!
        SUBROUTINE IGrJoin(XPOS1,YPOS1,XPOS2,YPOS2)
            IMPLICIT NONE
            REAL, INTENT (IN) :: XPOS1
            REAL, INTENT (IN) :: YPOS1
            REAL, INTENT (IN) :: XPOS2
            REAL, INTENT (IN) :: YPOS2
        END SUBROUTINE IGrJoin
!
        SUBROUTINE IGrJoinRel(XPOS1,YPOS1,DXPOS,DYPOS)
            IMPLICIT NONE
            REAL, INTENT (IN) :: XPOS1
            REAL, INTENT (IN) :: YPOS1
            REAL, INTENT (IN) :: DXPOS
            REAL, INTENT (IN) :: DYPOS
        END SUBROUTINE IGrJoinRel
!
        SUBROUTINE IGrLineTo(XPOS,YPOS)
            IMPLICIT NONE
            REAL, INTENT (IN) :: XPOS
            REAL, INTENT (IN) :: YPOS
        END SUBROUTINE IGrLineTo
!
        SUBROUTINE IGrLineToRel(DXPOS,DYPOS)
            IMPLICIT NONE
            REAL, INTENT (IN) :: DXPOS
            REAL, INTENT (IN) :: DYPOS
        END SUBROUTINE IGrLineToRel
!
        SUBROUTINE IGrMarker(XPOS,YPOS,MARKER)
            IMPLICIT NONE
            REAL   , INTENT (IN) :: XPOS
            REAL   , INTENT (IN) :: YPOS
            INTEGER, INTENT (IN) :: MARKER
        END SUBROUTINE IGrMarker
!
        SUBROUTINE IGrMarkerRel(MARKER)
            IMPLICIT NONE
            INTEGER, INTENT (IN) :: MARKER
        END SUBROUTINE IGrMarkerRel
!
        SUBROUTINE IGrMoveTo(XPOS,YPOS)
            IMPLICIT NONE
            REAL, INTENT (IN) :: XPOS
            REAL, INTENT (IN) :: YPOS
        END SUBROUTINE IGrMoveTo
!
        SUBROUTINE IGrMoveToRel(DXPOS,DYPOS)
            IMPLICIT NONE
            REAL, INTENT (IN) :: DXPOS
            REAL, INTENT (IN) :: DYPOS
        END SUBROUTINE IGrMoveToRel
!
        SUBROUTINE IGrParallel(XPOS1,YPOS1,XPOS2,YPOS2,ALENA,ITYPE)
            IMPLICIT NONE
            REAL   , INTENT (IN) :: XPOS1
            REAL   , INTENT (IN) :: YPOS1
            REAL   , INTENT (IN) :: XPOS2
            REAL   , INTENT (IN) :: YPOS2
            REAL   , INTENT (IN) :: ALENA
            INTEGER, INTENT (IN) :: ITYPE
        END SUBROUTINE IGrParallel
!
        SUBROUTINE IGrParallelRel(DXPOS,DYPOS,ALENA,ITYPE)
            IMPLICIT NONE
            REAL   , INTENT (IN) :: DXPOS
            REAL   , INTENT (IN) :: DYPOS
            REAL   , INTENT (IN) :: ALENA
            INTEGER, INTENT (IN) :: ITYPE
        END SUBROUTINE IGrParallelRel
!
        SUBROUTINE IGrPoint(XPOS,YPOS)
            IMPLICIT NONE
            REAL, INTENT (IN) :: XPOS
            REAL, INTENT (IN) :: YPOS
        END SUBROUTINE IGrPoint
!
        SUBROUTINE IGrPointRel(DXPOS,DYPOS)
            IMPLICIT NONE
            REAL, INTENT (IN) :: DXPOS
            REAL, INTENT (IN) :: DYPOS
        END SUBROUTINE IGrPointRel
!
        SUBROUTINE IGrPolygonComplex(XPOS,YPOS,NVERT)
            IMPLICIT NONE
            INTEGER, INTENT (IN)               :: NVERT
            REAL   , INTENT (IN), DIMENSION(*) :: XPOS
            REAL   , INTENT (IN), DIMENSION(*) :: YPOS
        END SUBROUTINE IGrPolygonComplex
!
        SUBROUTINE IGrPolygonGrad(XPOS,YPOS,NVERT,IDIR)
            IMPLICIT NONE
            INTEGER, INTENT (IN)               :: NVERT
            REAL   , INTENT (IN), DIMENSION(*) :: XPOS
            REAL   , INTENT (IN), DIMENSION(*) :: YPOS
            INTEGER, INTENT (IN)               :: IDIR
        END SUBROUTINE IGrPolygonGrad
!
        SUBROUTINE IGrPolygonSimple(X,Y,NVERT)
            IMPLICIT NONE
            INTEGER, INTENT (IN)               :: NVERT
            REAL   , INTENT (IN), DIMENSION(*) :: X
            REAL   , INTENT (IN), DIMENSION(*) :: Y
        END SUBROUTINE IGrPolygonSimple
!
        SUBROUTINE IGrPolyLine(XPOS,YPOS,NVERT)
            IMPLICIT NONE
            INTEGER, INTENT (IN)               :: NVERT
            REAL   , INTENT (IN), DIMENSION(*) :: XPOS
            REAL   , INTENT (IN), DIMENSION(*) :: YPOS
        END SUBROUTINE IGrPolyLine
!
        SUBROUTINE IGrRectangle(XPOS1,YPOS1,XPOS2,YPOS2)
            IMPLICIT NONE
            REAL, INTENT (IN) :: XPOS1
            REAL, INTENT (IN) :: YPOS1
            REAL, INTENT (IN) :: XPOS2
            REAL, INTENT (IN) :: YPOS2
        END SUBROUTINE IGrRectangle
!
        SUBROUTINE IGrRectangleRel(WIDTH,HEIGHT)
            IMPLICIT NONE
            REAL, INTENT (IN) :: WIDTH
            REAL, INTENT (IN) :: HEIGHT
        END SUBROUTINE IGrRectangleRel
!
        SUBROUTINE IGrTrapezium(XPOS1,YPOS1,XPOS2,YPOS2,ALENA,ALENB,ITYPE)
            IMPLICIT NONE
            REAL   , INTENT (IN) :: XPOS1
            REAL   , INTENT (IN) :: YPOS1
            REAL   , INTENT (IN) :: XPOS2
            REAL   , INTENT (IN) :: YPOS2
            REAL   , INTENT (IN) :: ALENA
            REAl   , INTENT (IN) :: ALENB
            INTEGER, INTENT (IN) :: ITYPE
        END SUBROUTINE IGrTrapezium
!
        SUBROUTINE IGrTrapeziumRel(DXPOS,DYPOS,ALENA,ALENB,ITYPE)
            IMPLICIT NONE
            REAL   , INTENT (IN) :: DXPOS
            REAL   , INTENT (IN) :: DYPOS
            REAL   , INTENT (IN) :: ALENA
            REAL   , INTENT (IN) :: ALENB
            INTEGER, INTENT (IN) :: ITYPE
        END SUBROUTINE IGrTrapeziumRel
!
        SUBROUTINE IGrTriangle(XPOS1,YPOS1,XPOS2,YPOS2,XPOS3,YPOS3)
            IMPLICIT NONE
            REAL, INTENT (IN) :: XPOS1
            REAL, INTENT (IN) :: YPOS1
            REAL, INTENT (IN) :: XPOS2
            REAL, INTENT (IN) :: YPOS2
            REAL, INTENT (IN) :: XPOS3
            REAL, INTENT (IN) :: YPOS3
        END SUBROUTINE IGrTriangle
!
        SUBROUTINE IGrTriangleRel(DXPOS1,DYPOS1,DXPOS2,DYPOS2)
            IMPLICIT NONE
            REAL, INTENT (IN) :: DXPOS1
            REAL, INTENT (IN) :: DYPOS1
            REAL, INTENT (IN) :: DXPOS2
            REAL, INTENT (IN) :: DYPOS2
        END SUBROUTINE IGrTriangleRel
!
!  GC : Graphics Character Output
!
        SUBROUTINE IGrCharConvert(FILNAM,TOFILE)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: FILNAM
            CHARACTER(LEN=*), INTENT (IN) :: TOFILE
        END SUBROUTINE IGrCharConvert
!
        SUBROUTINE IGrCharDirection(HORVER)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: HORVER
        END SUBROUTINE IGrCharDirection
!
        SUBROUTINE IGrCharFont(IFONT)
            IMPLICIT NONE
            INTEGER, INTENT (IN) :: IFONT
        END SUBROUTINE IGrCharFont
!
        SUBROUTINE IGrCharJustify(JUSTIF)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: JUSTIF
        END SUBROUTINE IGrCharJustify
!
        FUNCTION IGrCharLength(STRING)
            IMPLICIT NONE
            REAL                          :: IGrCharLength
            CHARACTER(LEN=*), INTENT (IN) :: STRING
        END FUNCTION IGrCharLength
!
        SUBROUTINE IGrCharOut(XPOS,YPOS,STRING)
            IMPLICIT NONE
            REAL         , INTENT (IN) :: XPOS
            REAL         , INTENT (IN) :: YPOS
            CHARACTER(LEN=*), INTENT (IN) :: STRING
        END SUBROUTINE IGrCharOut
!
        SUBROUTINE IGrCharOutRel(STRING)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: STRING
        END SUBROUTINE IGrCharOutRel
!
        SUBROUTINE IGrCharRotate(ANGLE)
            IMPLICIT NONE
            REAL     , INTENT (IN) :: ANGLE
        END SUBROUTINE IGrCharRotate
!
        SUBROUTINE IGrCharSet(FILNAM)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: FILNAM
        END SUBROUTINE IGrCharSet
!
        SUBROUTINE IGrCharSize(XSIZE,YSIZE)
            IMPLICIT NONE
            REAL, INTENT (IN) :: XSIZE
            REAL, INTENT (IN) :: YSIZE
        END SUBROUTINE IGrCharSize
!
        SUBROUTINE IGrCharSlant(ANGLE)
            IMPLICIT NONE
            REAL, INTENT (IN) :: ANGLE
        END SUBROUTINE IGrCharSlant
!
        SUBROUTINE IGrCharSpace(IASCII,SPACE)
            IMPLICIT NONE
            INTEGER, INTENT (IN) :: IASCII
            REAL   , INTENT (IN) :: SPACE
        END SUBROUTINE IGrCharSpace
!
        SUBROUTINE IGrCharSpacing(FIXPROP)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: FIXPROP
        END SUBROUTINE IGrCharSpacing
!
        SUBROUTINE IGrCharUnderline(ONOFF)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: ONOFF
        END SUBROUTINE IGrCharUnderline
!
!  GH : Graphics Hardcopy/Export
!
        SUBROUTINE IGrEndPage(ACTION)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: ACTION
        END SUBROUTINE IGrEndPage
!
        SUBROUTINE IGrHardColourToPen(NCOLOR,IPEN)
            IMPLICIT NONE
            INTEGER, INTENT (IN) :: NCOLOR
            INTEGER, INTENT (IN) :: IPEN
        END SUBROUTINE IGrHardColourToPen
!
        SUBROUTINE IGrHardCopy(DESTIN)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: DESTIN
        END SUBROUTINE IGrHardCopy
!
        SUBROUTINE IGrHardCopyOptions(NOPTN,IVALUE)
            IMPLICIT NONE
            INTEGER, INTENT (IN) :: NOPTN
            INTEGER, INTENT (IN) :: IVALUE
        END SUBROUTINE IGrHardCopyOptions
!
        SUBROUTINE IGrHardCopyOptLoad(FILENAME)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: FILENAME
        END SUBROUTINE IGrHardCopyOptLoad
!
        SUBROUTINE IGrHardCopyOptSave(FILENAME)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: FILENAME
        END SUBROUTINE IGrHardCopyOptSave
!
        SUBROUTINE IGrHardCopySelect(IACTN,IDRNUM)
            IMPLICIT NONE
            INTEGER, INTENT (IN) :: IACTN
            INTEGER, INTENT (IN) :: IDRNUM
        END SUBROUTINE IGrHardCopySelect
!
        SUBROUTINE IGrHardCopySelFile(FILENAME)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: FILENAME
        END SUBROUTINE IGrHardCopySelFile
!
        SUBROUTINE IGrHardCopyTextFile(INFILE,OUTFILE)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN)            :: INFILE
            CHARACTER(LEN=*), INTENT (IN), OPTIONAL  :: OUTFILE
        END SUBROUTINE IGrHardCopyTextFile
!
        SUBROUTINE IGrPrintImage(PRNAME)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN), OPTIONAL :: PRNAME
        END SUBROUTINE IGrPrintImage
!
        SUBROUTINE IGrPrintImageOptions(NOPTN,IVALUE)
            IMPLICIT NONE
            INTEGER, INTENT (IN) :: NOPTN
            INTEGER, INTENT (IN) :: IVALUE
        END SUBROUTINE IGrPrintImageOptions
!
        SUBROUTINE IGrSaveImage(FNAME)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: FNAME
        END SUBROUTINE IGrSaveImage
!
        END INTERFACE
!
        INTERFACE IGrSaveImageData
          SUBROUTINE IGrSaveImageData8(FNAME,BMPDATA,IWID,IHGT)
              IMPLICIT NONE
              CHARACTER(LEN=*), INTENT (IN)               :: FNAME
              CHARACTER(LEN=1), INTENT (IN), DIMENSION(*) :: BMPDATA
              INTEGER         , INTENT (IN)               :: IWID
              INTEGER         , INTENT (IN)               :: IHGT
          END SUBROUTINE IGrSaveImageData8
!
          SUBROUTINE IGrSaveImageData24(FNAME,IBMPDATA,IWID,IHGT)
              IMPLICIT NONE
              CHARACTER(LEN=*), INTENT (IN)               :: FNAME
              INTEGER         , INTENT (IN), DIMENSION(*) :: IBMPDATA
              INTEGER         , INTENT (IN)               :: IWID
              INTEGER         , INTENT (IN)               :: IHGT
          END SUBROUTINE IGrSaveImageData24
        END INTERFACE
!
        INTERFACE
!
!  GF : Graphics File Import
!
        SUBROUTINE IGrFileInfo(FNAME,INFO,MAXITEM)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN)                 :: FNAME
            INTEGER,          INTENT (OUT), DIMENSION(*)  :: INFO
            INTEGER,          INTENT (IN)                 :: MAXITEM
        END SUBROUTINE IGrFileInfo
!
        SUBROUTINE IGrLoadImage(FNAME,ISTRETCH)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN)           :: FNAME
            INTEGER         , INTENT (IN), OPTIONAL :: ISTRETCH
        END SUBROUTINE IGrLoadImage
!
        SUBROUTINE IGrReplay(FNAME)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: FNAME
        END SUBROUTINE IGrReplay
!
        SUBROUTINE IGrReplayArea(XLEFT,YLOWER,XRIGHT,YUPPER)
            IMPLICIT NONE
            REAL, INTENT (IN) :: XLEFT
            REAL, INTENT (IN) :: YLOWER
            REAL, INTENT (IN) :: XRIGHT
            REAL, INTENT (IN) :: YUPPER
        END SUBROUTINE IGrReplayArea
!
        SUBROUTINE IGrReplayOptions(NOPTN,IVALUE)
            IMPLICIT NONE
            INTEGER, INTENT (IN) :: NOPTN
            INTEGER, INTENT (IN) :: IVALUE
        END SUBROUTINE IGrReplayOptions
!
        SUBROUTINE IGrReplayToN(FNAME,NSCRN,OPTION)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: FNAME
            INTEGER         , INTENT (IN) :: NSCRN
            CHARACTER(LEN=*), INTENT (IN) :: OPTION
        END SUBROUTINE IGrReplayToN
!
!  GP : Graphics Proximity Checks
!
        FUNCTION IGrDistanceLine(X1,Y1,X2,Y2,XCHECK,YCHECK,METHOD)
            IMPLICIT NONE
            REAL                 :: IGrDistanceLine
            REAL   , INTENT (IN) :: X1
            REAL   , INTENT (IN) :: Y1
            REAL   , INTENT (IN) :: X2
            REAL   , INTENT (IN) :: Y2
            REAL   , INTENT (IN) :: XCHECK
            REAL   , INTENT (IN) :: YCHECK
            INTEGER, INTENT (IN) :: METHOD
        END FUNCTION IGrDistanceLine
!
        FUNCTION IGrInsideCircle(X,Y,RADIUS,XCHECK,YCHECK)
            IMPLICIT NONE
            LOGICAL           :: IGrInsideCircle
            REAL, INTENT (IN) :: X
            REAL, INTENT (IN) :: Y
            REAL, INTENT (IN) :: RADIUS
            REAL, INTENT (IN) :: XCHECK
            REAL, INTENT (IN) :: YCHECK
        END FUNCTION IGrInsideCircle
!
        FUNCTION IGrInsideEllipse(X,Y,RADX,RATIO,XCHECK,YCHECK)
            IMPLICIT NONE
            LOGICAL           :: IGrInsideEllipse
            REAL, INTENT (IN) :: X
            REAL, INTENT (IN) :: Y
            REAL, INTENT (IN) :: RADX
            REAL, INTENT (IN) :: RATIO
            REAL, INTENT (IN) :: XCHECK
            REAL, INTENT (IN) :: YCHECK
        END FUNCTION IGrInsideEllipse
!
        FUNCTION IGrInsidePolygon(X,Y,NVERT,XCHECK,YCHECK)
            IMPLICIT NONE
            LOGICAL                            :: IGrInsidePolygon
            REAL   , INTENT (IN), DIMENSION(*) :: X
            REAL   , INTENT (IN), DIMENSION(*) :: Y
            INTEGER, INTENT (IN)               :: NVERT
            REAL   , INTENT (IN)               :: XCHECK
            REAL   , INTENT (IN)               :: YCHECK
        END FUNCTION IGrInsidePolygon
!
        SUBROUTINE IGrIntersectLine(X1,Y1,X2,Y2,X3,Y3,X4,Y4,XINTER,YINTER,&
                                    ISTATUS)
            IMPLICIT NONE
            REAL   , INTENT (IN)  :: X1
            REAL   , INTENT (IN)  :: Y1
            REAL   , INTENT (IN)  :: X2
            REAL   , INTENT (IN)  :: Y2
            REAL   , INTENT (IN)  :: X3
            REAL   , INTENT (IN)  :: Y3
            REAL   , INTENT (IN)  :: X4
            REAL   , INTENT (IN)  :: Y4
            REAL   , INTENT (OUT) :: XINTER
            REAL   , INTENT (OUT) :: YINTER
            INTEGER, INTENT (OUT) :: ISTATUS
        END SUBROUTINE IGrIntersectLine
!
!  PG(1) : Housekeeping/Option Selection
!
        SUBROUTINE IPgArea(XLEFT,YLOWER,XRIGHT,YUPPER)
            IMPLICIT NONE
            REAL, INTENT (IN) :: XLEFT
            REAL, INTENT (IN) :: YLOWER
            REAL, INTENT (IN) :: XRIGHT
            REAL, INTENT (IN) :: YUPPER
        END SUBROUTINE IPgArea
!
        SUBROUTINE IPgClipRectangle(AREA)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: AREA
        END SUBROUTINE IPgClipRectangle
!
        SUBROUTINE IPgConFill2Granul(IGRAN)
            IMPLICIT NONE
            INTEGER, INTENT (IN) :: IGRAN
        END SUBROUTINE IPgConFill2Granul
!
        SUBROUTINE IPgContourLabel(ISET,LABEL)
            IMPLICIT NONE
            INTEGER         , INTENT (IN) :: ISET
            CHARACTER(LEN=*), INTENT (IN) :: LABEL
        END SUBROUTINE IPgContourLabel
!
        SUBROUTINE IPgDecimalPlaces(NDEC)
            IMPLICIT NONE
            INTEGER, INTENT (IN) :: NDEC
        END SUBROUTINE IPgDecimalPlaces
!
        SUBROUTINE IPgElevation(ANGLE)
            IMPLICIT NONE
            REAL, INTENT (IN) :: ANGLE
        END SUBROUTINE IPgElevation
!
        SUBROUTINE IPgGridDirection(IGRID)
            IMPLICIT NONE
            INTEGER, INTENT (IN) :: IGRID
        END SUBROUTINE IPgGridDirection
!
        SUBROUTINE IPgGridLines(IGRID)
            IMPLICIT NONE
            INTEGER, INTENT (IN) :: IGRID
        END SUBROUTINE IPgGridLines
!
        SUBROUTINE IPgMarker(ISET,MARKER)
            IMPLICIT NONE
            INTEGER, INTENT (IN) :: ISET
            INTEGER, INTENT (IN) :: MARKER
        END SUBROUTINE IPgMarker
!
        SUBROUTINE IPgMarkerFrequency(ISTART,IEVERY)
            IMPLICIT NONE
            INTEGER, INTENT (IN) :: ISTART
            INTEGER, INTENT (IN) :: IEVERY
        END SUBROUTINE IPgMarkerFrequency
!
    END INTERFACE
!
    INTERFACE IPgNewGraph
!
        SUBROUTINE IPgNewGraph1(NSETS,NVALUE,CUMUL,LAYOUT,GRTYPE)
            IMPLICIT NONE
            INTEGER         , INTENT (IN) :: NSETS
            INTEGER         , INTENT (IN) :: NVALUE
            CHARACTER(LEN=*), INTENT (IN) :: CUMUL
            CHARACTER(LEN=*), INTENT (IN) :: LAYOUT
            CHARACTER(LEN=*), INTENT (IN) :: GRTYPE
        END SUBROUTINE IPgNewGraph1
!
        SUBROUTINE IPgNewGraph2(NSETS,NVALUE,CUMUL,LAYOUT,GRTYPE)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)               :: NSETS
            INTEGER         , INTENT (IN), DIMENSION(2) :: NVALUE
            CHARACTER(LEN=*), INTENT (IN)               :: CUMUL
            CHARACTER(LEN=*), INTENT (IN)               :: LAYOUT
            CHARACTER(LEN=*), INTENT (IN)               :: GRTYPE
        END SUBROUTINE IPgNewGraph2
!
    END INTERFACE
!
    INTERFACE
!
        SUBROUTINE IPgRotation(ANGLE)
            IMPLICIT NONE
            REAL, INTENT (IN) :: ANGLE
        END SUBROUTINE IPgRotation
!
        SUBROUTINE IPgScaling(XSCAL,YSCAL)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: XSCAL
            CHARACTER(LEN=*), INTENT (IN) :: YSCAL
        END SUBROUTINE IPgScaling
!
        SUBROUTINE IPgStyle(ISET,ISTYL1,ISTYL2,ISTYL3,ICOL1,ICOL2)
            IMPLICIT NONE
            INTEGER, INTENT (IN)           :: ISET
            INTEGER, INTENT (IN), OPTIONAL :: ISTYL1
            INTEGER, INTENT (IN), OPTIONAL :: ISTYL2
            INTEGER, INTENT (IN), OPTIONAL :: ISTYL3
            INTEGER, INTENT (IN), OPTIONAL :: ICOL1
            INTEGER, INTENT (IN), OPTIONAL :: ICOL2
        END SUBROUTINE IPgStyle
!
        SUBROUTINE IPgStyle3DAxes(ISTYLE,IAXCOL1,IAXCOl2)
            IMPLICIT NONE
            INTEGER, INTENT (IN)               :: ISTYLE
            INTEGER, INTENT (IN), DIMENSION(3) :: IAXCOL1
            INTEGER, INTENT (IN), DIMENSION(3) :: IAXCOL2
        END SUBROUTINE IPgStyle3DAxes
!
        SUBROUTINE IPgStyleOutline(ICOL)
            IMPLICIT NONE
            INTEGER, INTENT (IN) :: ICOL
        END SUBROUTINE IPgStyleOutline
!
        SUBROUTINE IPgUnits(XLEFT,YLOWER,XRIGHT,YUPPER)
            IMPLICIT NONE
            REAL, INTENT (IN) :: XLEFT
            REAL, INTENT (IN) :: YLOWER
            REAL, INTENT (IN) :: XRIGHT
            REAL, INTENT (IN) :: YUPPER
        END SUBROUTINE IPgUnits
!
        SUBROUTINE IPgUnitsFromGrUnits(XEXT,YEXT,XPG,YPG)
            IMPLICIT NONE
            REAL, INTENT (IN)  :: XEXT
            REAL, INTENT (IN)  :: YEXT
            REAL, INTENT (OUT) :: XPG
            REAL, INTENT (OUT) :: YPG
        END SUBROUTINE IPgUnitsFromGrUnits
!
        SUBROUTINE IPgUnitsToGrUnits(XPG,YPG,XEXT,YEXT)
            IMPLICIT NONE
            REAL, INTENT (IN)  :: XPG
            REAL, INTENT (IN)  :: YPG
            REAL, INTENT (OUT) :: XEXT
            REAL, INTENT (OUT) :: YEXT
        END SUBROUTINE IPgUnitsToGrUnits
!
        SUBROUTINE IPgUnitsZ(ZMIN,ZMAX)
            IMPLICIT NONE
            REAL, INTENT (IN) :: ZMIN
            REAL, INTENT (IN) :: ZMAX
        END SUBROUTINE IPgUnitsZ
!
        SUBROUTINE IPgXKeyPos(RELPOS)
            IMPLICIT NONE
            REAL, INTENT (IN) :: RELPOS
        END SUBROUTINE IPgXKeyPos
!
        SUBROUTINE IPgXLabelPos(RELPOS)
            IMPLICIT NONE
            REAL, INTENT (IN) :: RELPOS
        END SUBROUTINE IPgXLabelPos
!
        SUBROUTINE IPgXScaleAngle(TANGLE,SANGLE)
            IMPLICIT NONE
            REAL, INTENT (IN) :: TANGLE
            REAL, INTENT (IN) :: SANGLE
        END SUBROUTINE IPgXScaleAngle
!
        SUBROUTINE IPgXScalePos(RELPOS)
            IMPLICIT NONE
            REAL, INTENT (IN) :: RELPOS
        END SUBROUTINE IPgXScalePos
!
        SUBROUTINE IPgXTickPos(YBOTTOM,YTOP)
            IMPLICIT NONE
            REAL, INTENT (IN) :: YBOTTOM
            REAL, INTENT (IN) :: YTOP
        END SUBROUTINE IPgXTickPos
!
        SUBROUTINE IPgXUserScale(SPOINT,NPOINT)
            IMPLICIT NONE
            INTEGER, INTENT (IN)                    :: NPOINT
            REAL   , INTENT (IN), DIMENSION(NPOINT) :: SPOINT
        END SUBROUTINE IPgXUserScale
!
        SUBROUTINE IPgXUserScaleHist(IBAR,NPOINT)
            IMPLICIT NONE
            INTEGER, INTENT (IN)                    :: NPOINT
            INTEGER, INTENT (IN), DIMENSION(NPOINT) :: IBAR
        END SUBROUTINE IPgXUserScaleHist
!
        SUBROUTINE IPgYLabelPos(RELPOS)
            IMPLICIT NONE
            REAL, INTENT (IN) :: RELPOS
        END SUBROUTINE IPgYLabelPos
!
        SUBROUTINE IPgYScaleAngle(TANGLE,SANGLE)
            IMPLICIT NONE
            REAL, INTENT (IN) :: TANGLE
            REAL, INTENT (IN) :: SANGLE
        END SUBROUTINE IPgYScaleAngle
!
        SUBROUTINE IPgYScalePos(RELPOS)
            IMPLICIT NONE
            REAL, INTENT (IN) :: RELPOS
        END SUBROUTINE IPgYScalePos
!
        SUBROUTINE IPgYTickPos(XLEFT,XRIGHT)
            IMPLICIT NONE
            REAL, INTENT (IN) :: XLEFT
            REAL, INTENT (IN) :: XRIGHT
        END SUBROUTINE IPgYTickPos
!
        SUBROUTINE IPgYUserScale(SPOINT,NPOINT)
            IMPLICIT NONE
            INTEGER, INTENT (IN)                    :: NPOINT
            REAL   , INTENT (IN), DIMENSION(NPOINT) :: SPOINT
        END SUBROUTINE IPgYUserScale
!
        SUBROUTINE IPgYUserScaleBar(IBAR,NPOINT)
            IMPLICIT NONE
            INTEGER, INTENT (IN)                    :: NPOINT
            INTEGER, INTENT (IN), DIMENSION(NPOINT) :: IBAR
        END SUBROUTINE IPgYUserScaleBar
!
        SUBROUTINE IPgZLabelPos(RELPOS)
            IMPLICIT NONE
            REAL, INTENT (IN) :: RELPOS
        END SUBROUTINE IPgZLabelPos
!
        SUBROUTINE IPgZScaleAngle(TANGLE,SANGLE)
            IMPLICIT NONE
            REAL, INTENT (IN) :: TANGLE
            REAL, INTENT (IN) :: SANGLE
        END SUBROUTINE IPgZScaleAngle
!
        SUBROUTINE IPgZUserScale(SPOINT,NPOINT)
            IMPLICIT NONE
            INTEGER, INTENT (IN)                    :: NPOINT
            REAL   , INTENT (IN), DIMENSION(NPOINT) :: SPOINT
        END SUBROUTINE IPgZUserScale
!
!  PG(2) : Axes Plotting/Annotation
!
        SUBROUTINE IPgAxes()
            IMPLICIT NONE
        END SUBROUTINE IPgAxes
!
        SUBROUTINE IPgAxesXY(XORIGIN,YORIGIN)
            IMPLICIT NONE
            REAL, INTENT (IN) :: XORIGIN
            REAL, INTENT (IN) :: YORIGIN
        END SUBROUTINE IPgAxesXY
!
        SUBROUTINE IPgBorder()
            IMPLICIT NONE
        END SUBROUTINE IPgBorder
!
        SUBROUTINE IPgKeyAll(DESCR,LAYOUT)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN)               :: LAYOUT
            CHARACTER(LEN=*), INTENT (IN), DIMENSION(*) :: DESCR
        END SUBROUTINE IPgKeyAll
!
        SUBROUTINE IPgKeySingle(ISET,XPOS,YPOS,DESCR)
            IMPLICIT NONE
            INTEGER      , INTENT (IN) :: ISET
            REAL         , INTENT (IN) :: XPOS
            REAL         , INTENT (IN) :: YPOS
            CHARACTER(LEN=*), INTENT (IN) :: DESCR
        END SUBROUTINE IPgKeySingle
!
        SUBROUTINE IPgTitle(LABEL,JUSTIF)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: LABEL
            CHARACTER(LEN=*), INTENT (IN) :: JUSTIF
        END SUBROUTINE IPgTitle
!
        SUBROUTINE IPgXGraticules(LTYPE)
            IMPLICIT NONE
            INTEGER, INTENT (IN) :: LTYPE
        END SUBROUTINE IPgXGraticules
!
        SUBROUTINE IPgXLabel(LABEL,JUSTIF)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: LABEL
            CHARACTER(LEN=*), INTENT (IN) :: JUSTIF
        END SUBROUTINE IPgXLabel
!
        SUBROUTINE IPgXLabelTop(LABEL,JUSTIF)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: LABEL
            CHARACTER(LEN=*), INTENT (IN) :: JUSTIF
        END SUBROUTINE IPgXLabelTop
!
        SUBROUTINE IPgXScale(SCALE)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: SCALE
        END SUBROUTINE IPgXScale
!
        SUBROUTINE IPgXScaleTop(SCALE)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: SCALE
        END SUBROUTINE IPgXScaleTop
!
        SUBROUTINE IPgXText(DESCR,NDESC)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)                   :: NDESC
            CHARACTER(LEN=*), INTENT (IN), DIMENSION(NDESC) :: DESCR
        END SUBROUTINE IPgXText
!
        SUBROUTINE IPgXTextTop(DESCR,NDESC)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)                   :: NDESC
            CHARACTER(LEN=*), INTENT (IN), DIMENSION(NDESC) :: DESCR
        END SUBROUTINE IPgXTextTop
!
        SUBROUTINE IPgYGraticules(LTYPE)
            IMPLICIT NONE
            INTEGER, INTENT (IN) :: LTYPE
        END SUBROUTINE IPgYGraticules
!
        SUBROUTINE IPgYLabelLeft(LABEL,JUSTIF)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: LABEL
            CHARACTER(LEN=*), INTENT (IN) :: JUSTIF
        END SUBROUTINE IPgYLabelLeft
!
        SUBROUTINE IPgYLabelRight(LABEL,JUSTIF)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: LABEL
            CHARACTER(LEN=*), INTENT (IN) :: JUSTIF
        END SUBROUTINE IPgYLabelRight
!
        SUBROUTINE IPgYScaleLeft(SCALE)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: SCALE
        END SUBROUTINE IPgYScaleLeft
!
        SUBROUTINE IPgYScaleRight(SCALE)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: SCALE
        END SUBROUTINE IPgYScaleRight
!
        SUBROUTINE IPgYTextLeft(DESCR,NDESC)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)                   :: NDESC
            CHARACTER(LEN=*), INTENT (IN), DIMENSION(NDESC) :: DESCR
        END SUBROUTINE IPgYTextLeft
!
        SUBROUTINE IPgYTextRight(DESCR,NDESC)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)                   :: NDESC
            CHARACTER(LEN=*), INTENT (IN), DIMENSION(NDESC) :: DESCR
        END SUBROUTINE IPgYTextRight
!
        SUBROUTINE IPgZGraticules(LTYPE)
            IMPLICIT NONE
            INTEGER, INTENT (IN) :: LTYPE
        END SUBROUTINE IPgZGraticules
!
        SUBROUTINE IPgZLabelLeft(LABEL,JUSTIF)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: LABEL
            CHARACTER(LEN=*), INTENT (IN) :: JUSTIF
        END SUBROUTINE IPgZLabelLeft
!
        SUBROUTINE IPgZLabelRight(LABEL,JUSTIF)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: LABEL
            CHARACTER(LEN=*), INTENT (IN) :: JUSTIF
        END SUBROUTINE IPgZLabelRight
!
        SUBROUTINE IPgZScale(SCALE)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: SCALE
        END SUBROUTINE IPgZScale
!
        SUBROUTINE IPgZText(DESCR,NDESC)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)                   :: NDESC
            CHARACTER(LEN=*), INTENT (IN), DIMENSION(NDESC) :: DESCR
        END SUBROUTINE IPgZText
!
!  PG(3) : Chart/Graph Plotting
!
        SUBROUTINE IPgBarChart(XVALUE)
            IMPLICIT NONE
            REAL, INTENT (IN), DIMENSION(*) :: XVALUE
        END SUBROUTINE IPgBarChart
!
        SUBROUTINE IPgErrorBars(YLOW,YHIGH)
            IMPLICIT NONE
            REAL, INTENT (IN), DIMENSION(*)  :: YLOW
            REAL, INTENT (IN), DIMENSION(*)  :: YHIGH
        END SUBROUTINE IPgErrorBars
!
        SUBROUTINE IPgFunctionLine(FUNC)
            IMPLICIT NONE
            INTERFACE
                FUNCTION FUNC(X)
                    IMPLICIT NONE
                    REAL              :: FUNC
                    REAL, INTENT (IN) :: X
                END FUNCTION FUNC
            END INTERFACE
        END SUBROUTINE IPgFunctionLine
!
        SUBROUTINE IPgHighLow(YLOW,YHIGH)
            IMPLICIT NONE
            REAL, INTENT (IN), DIMENSION(*)  :: YLOW
            REAL, INTENT (IN), DIMENSION(*)  :: YHIGH
        END SUBROUTINE IPgHighLow
!
        SUBROUTINE IPgHistogram(YVALUE)
            IMPLICIT NONE
            REAL, INTENT (IN), DIMENSION(*)  :: YVALUE
        END SUBROUTINE IPgHistogram
!
        SUBROUTINE IPgLinePlot(YVALUE)
            IMPLICIT NONE
            REAL, INTENT (IN), DIMENSION(*)  :: YVALUE
        END SUBROUTINE IPgLinePlot
!
        SUBROUTINE IPgPieChart(PIEVAL,ASTART,EXPAND)
            IMPLICIT NONE
            REAL         , INTENT (IN), DIMENSION(*)  :: PIEVAL
            REAL         , INTENT (IN)                :: ASTART
            CHARACTER(LEN=*), INTENT (IN)             :: EXPAND
        END SUBROUTINE IPgPieChart
!
        SUBROUTINE IPgScatterPlot(XVALUE,YVALUE)
            IMPLICIT NONE
            REAL, INTENT (IN), DIMENSION(*)  :: XVALUE
            REAL, INTENT (IN), DIMENSION(*)  :: YVALUE
        END SUBROUTINE IPgScatterPlot
!
        SUBROUTINE IPgScatterPlot3D(XVALUE,YVALUE,ZVALUE)
            IMPLICIT NONE
            REAL, INTENT (IN), DIMENSION(*)  :: XVALUE
            REAL, INTENT (IN), DIMENSION(*)  :: YVALUE
            REAL, INTENT (IN), DIMENSION(*)  :: ZVALUE
        END SUBROUTINE IPgScatterPlot3D
!
        SUBROUTINE IPgTableInteger(IVALUE)
            IMPLICIT NONE
            INTEGER, INTENT (IN), DIMENSION(*) :: IVALUE
        END SUBROUTINE IPgTableInteger
!
        SUBROUTINE IPgTableReal(RVALUE,FORMAT)
            IMPLICIT NONE
            REAL            , INTENT (IN), DIMENSION(*)  :: RVALUE
            CHARACTER(LEN=*), INTENT (IN)                :: FORMAT
        END SUBROUTINE IPgTableReal
!
        SUBROUTINE IPgXYPairs(XVALUE,YVALUE)
            IMPLICIT NONE
            REAL, INTENT (IN), DIMENSION(*)  :: XVALUE
            REAL, INTENT (IN), DIMENSION(*)  :: YVALUE
        END SUBROUTINE IPgXYPairs
!
!  PG(4) : Contour/Surface Plotting
!
        SUBROUTINE IPgConFill2Irreg(ZVALUE,NXDIM,NYDIM,ZCONTR,GRIDX,GRIDY)
            IMPLICIT NONE
            INTEGER, INTENT (IN)                         :: NXDIM
            INTEGER, INTENT (IN)                         :: NYDIM
            REAL   , INTENT (IN), DIMENSION(NXDIM,NYDIM) :: ZVALUE
            REAL   , INTENT (IN), DIMENSION(*)           :: ZCONTR
            REAL   , INTENT (IN), DIMENSION(*)           :: GRIDX
            REAL   , INTENT (IN), DIMENSION(*)           :: GRIDY
        END SUBROUTINE IPgConFill2Irreg
!
        SUBROUTINE IPgConFill2Reg(ZVALUE,NXDIM,NYDIM,ZCONTR)
            IMPLICIT NONE
            INTEGER, INTENT (IN)                         :: NXDIM
            INTEGER, INTENT (IN)                         :: NYDIM
            REAL   , INTENT (IN), DIMENSION(NXDIM,NYDIM) :: ZVALUE
            REAL   , INTENT (IN), DIMENSION(*)           :: ZCONTR
        END SUBROUTINE IPgConFill2Reg
!
        SUBROUTINE IPgContour2Irreg(ZVALUE,NXDIM,NYDIM,ZCONTR,GRIDX,GRIDY)
            IMPLICIT NONE
            INTEGER, INTENT (IN)                         :: NXDIM
            INTEGER, INTENT (IN)                         :: NYDIM
            REAL   , INTENT (IN), DIMENSION(NXDIM,NYDIM) :: ZVALUE
            REAL   , INTENT (IN), DIMENSION(*)           :: ZCONTR
            REAL   , INTENT (IN), DIMENSION(*)           :: GRIDX
            REAL   , INTENT (IN), DIMENSION(*)           :: GRIDY
        END SUBROUTINE IPgContour2Irreg
!
        SUBROUTINE IPgContour2Reg(ZVALUE,NXDIM,NYDIM,ZCONTR)
            IMPLICIT NONE
            INTEGER, INTENT (IN)                         :: NXDIM
            INTEGER, INTENT (IN)                         :: NYDIM
            REAL   , INTENT (IN), DIMENSION(NXDIM,NYDIM) :: ZVALUE
            REAL   , INTENT (IN), DIMENSION(*)           :: ZCONTR
        END SUBROUTINE IPgContour2Reg
!
        SUBROUTINE IPgSurf3Data(ZVALUE,NXDIM,NYDIM)
            IMPLICIT NONE
            INTEGER, INTENT (IN)                         :: NXDIM
            INTEGER, INTENT (IN)                         :: NYDIM
            REAL   , INTENT (IN), DIMENSION(NXDIM,NYDIM) :: ZVALUE
        END SUBROUTINE IPgSurf3Data
!
        SUBROUTINE IPgSurf3DataCont(ZVALUE,NXDIM,NYDIM,ZCONTR)
            IMPLICIT NONE
            INTEGER, INTENT (IN)                         :: NXDIM
            INTEGER, INTENT (IN)                         :: NYDIM
            REAL   , INTENT (IN), DIMENSION(NXDIM,NYDIM) :: ZVALUE
            REAL   , INTENT (IN), DIMENSION(*)           :: ZCONTR
        END SUBROUTINE IPgSurf3DataCont
!
        SUBROUTINE IPgSurf3Func(ZFUNC)
            IMPLICIT NONE
            INTERFACE
                FUNCTION ZFUNC(X,Y)
                    IMPLICIT NONE
                    REAL              :: ZFUNC
                    REAL, INTENT (IN) :: X
                    REAL, INTENT (IN) :: Y
                END FUNCTION ZFUNC
            END INTERFACE
        END SUBROUTINE IPgSurf3Func
!
        SUBROUTINE IPgSurf3FuncCont(ZFUNC,ZCONTR)
            IMPLICIT NONE
            INTERFACE
                FUNCTION ZFUNC(X,Y)
                    IMPLICIT NONE
                    REAL              :: ZFUNC
                    REAL, INTENT (IN) :: X
                    REAL, INTENT (IN) :: Y
                END FUNCTION ZFUNC
            END INTERFACE
            REAL    , INTENT (IN), DIMENSION(*) :: ZCONTR
        END SUBROUTINE IPgSurf3FuncCont
!
        SUBROUTINE IPgXYZSearchBox(BOXWID,BOXHGT)
            IMPLICIT NONE
            REAL, INTENT (IN) :: BOXWID
            REAL, INTENT (IN) :: BOXHGT
        END SUBROUTINE IPgXYZSearchBox
!
        SUBROUTINE IPgXYZToGrid(X,Y,Z,NPOINT,ZRECT,NXRECT,NYRECT,NXDIM,NYDIM)
            IMPLICIT NONE
            INTEGER, INTENT (IN)                          :: NPOINT
            REAL   , INTENT (IN),  DIMENSION(NPOINT)      :: X
            REAL   , INTENT (IN),  DIMENSION(NPOINT)      :: Y
            REAL   , INTENT (IN),  DIMENSION(NPOINT)      :: Z
            INTEGER, INTENT (IN)                          :: NXRECT
            INTEGER, INTENT (IN)                          :: NYRECT
            INTEGER, INTENT (IN)                          :: NXDIM
            INTEGER, INTENT (IN)                          :: NYDIM
            REAL   , INTENT (OUT), DIMENSION(NXDIM,NYDIM) :: ZRECT
        END SUBROUTINE IPgXYZToGrid
!
!  IF : Information
!
        FUNCTION InfoError(ITEM)
            IMPLICIT NONE
            INTEGER              :: InfoError
            INTEGER, INTENT (IN) :: ITEM
        END FUNCTION InfoError
!
        FUNCTION InfoGraphics(ITEM)
            IMPLICIT NONE
            REAL                 :: InfoGraphics
            INTEGER, INTENT (IN) :: ITEM
        END FUNCTION InfoGraphics
!
        FUNCTION InfoGrHardcopy(ITEM)
            IMPLICIT NONE
            INTEGER              :: InfoGrHardcopy
            INTEGER, INTENT (IN) :: ITEM
        END FUNCTION InfoGrHardcopy
!
        FUNCTION InfoGrPalette(ITEM)
            IMPLICIT NONE
            INTEGER              :: InfoGrPalette
            INTEGER, INTENT (IN) :: ITEM
        END FUNCTION InfoGrPalette
!
        FUNCTION InfoGrScreen(ITEM)
            IMPLICIT NONE
            INTEGER              :: InfoGrScreen
            INTEGER, INTENT (IN) :: ITEM
        END FUNCTION InfoGrScreen
!
        FUNCTION InfoHardware(ITEM)
            IMPLICIT NONE
            INTEGER              :: InfoHardware
            INTEGER, INTENT (IN) :: ITEM
        END FUNCTION InfoHardware
!
        FUNCTION InfoOpSystem(ITEM)
            IMPLICIT NONE
            INTEGER              :: InfoOpSystem
            INTEGER, INTENT (IN) :: ITEM
        END FUNCTION InfoOpSystem
!
        FUNCTION WInfoBitmap(IHANDLE, ITEM)
            IMPLICIT NONE
            INTEGER              :: WInfoBitmap
            INTEGER, INTENT (IN) :: IHANDLE
            INTEGER, INTENT (IN) :: ITEM
        END FUNCTION WInfoBitmap
!
        FUNCTION WInfoDialog(ITEM)
            IMPLICIT NONE
            INTEGER              :: WInfoDialog
            INTEGER, INTENT (IN) :: ITEM
        END FUNCTION WInfoDialog
!
        FUNCTION WInfoDialogField(IFIELD,ITEM)
            IMPLICIT NONE
            INTEGER              :: WInfoDialogField
            INTEGER, INTENT (IN) :: IFIELD
            INTEGER, INTENT (IN) :: ITEM
        END FUNCTION WInfoDialogField
!
        FUNCTION WInfoEditor(IHANDLE,ITEM)
            IMPLICIT NONE
            INTEGER              :: WInfoEditor
            INTEGER, INTENT (IN) :: IHANDLE
            INTEGER, INTENT (IN) :: ITEM
        END FUNCTION WInfoEditor
!
        SUBROUTINE WInfoEditorString(IHANDLE,ITEM,CVALUE)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)  :: IHANDLE
            INTEGER         , INTENT (IN)  :: ITEM
            CHARACTER(LEN=*), INTENT (OUT) :: CVALUE
        END SUBROUTINE WInfoEditorString
!
        SUBROUTINE WInfoErrorMessage(ITEM,MESSAGE)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)  :: ITEM
            CHARACTER(LEN=*), INTENT (OUT) :: MESSAGE
        END SUBROUTINE WInfoErrorMessage
!
        FUNCTION WInfoFont(ITEM)
            IMPLICIT NONE
            INTEGER              :: WInfoFont
            INTEGER, INTENT (IN) :: ITEM
        END FUNCTION WInfoFont
!
        SUBROUTINE WInfoFontName(ITEM,FNAME)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)  :: ITEM
            CHARACTER(LEN=*), INTENT (OUT) :: FNAME
        END SUBROUTINE WInfoFontName
!
        FUNCTION WInfoGrid(IFIELD,ITEM)
            IMPLICIT NONE
            INTEGER             :: WInfoGrid
            INTEGER, INTENT(IN) :: IFIELD
            INTEGER, INTENT(IN) :: ITEM
        END FUNCTION WInfoGrid
!
        FUNCTION WInfoGridCell(IFIELD,ICOL,IROW,ITEM)
            IMPLICIT NONE
            INTEGER             :: WInfoGridCell
            INTEGER, INTENT(IN) :: IFIELD
            INTEGER, INTENT(IN) :: ICOL
            INTEGER, INTENT(IN) :: IROW
            INTEGER, INTENT(IN) :: ITEM
        END FUNCTION WinfoGridCell
!
        FUNCTION WInfoMessage(ITEM)
            IMPLICIT NONE
            INTEGER              :: WInfoMessage
            INTEGER, INTENT (IN) :: ITEM
        END FUNCTION WInfoMessage
!
        FUNCTION WInfoScreen(ITEM)
            IMPLICIT NONE
            INTEGER              :: WInfoScreen
            INTEGER, INTENT (IN) :: ITEM
        END FUNCTION WInfoScreen
!
        FUNCTION WInfoWindow(ITEM)
            IMPLICIT NONE
            INTEGER              :: WInfoWindow
            INTEGER, INTENT (IN) :: ITEM
        END FUNCTION WInfoWindow
!
!  OS : Operating System Interface
!
        SUBROUTINE IOsArgument(N,STRING)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)  :: N
            CHARACTER(LEN=*), INTENT (OUT) :: STRING
        END SUBROUTINE IOsArgument
!
        SUBROUTINE IOsCommand(COMAND,IFLAGS,ITIMEOUT)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN)           :: COMAND
            INTEGER         , INTENT (IN), OPTIONAL :: IFLAGS
            INTEGER         , INTENT (IN), OPTIONAL :: ITIMEOUT
        END SUBROUTINE IOsCommand
!
        SUBROUTINE IOsCommandSilent(COMAND)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN)           :: COMAND
        END SUBROUTINE IOsCommandSilent
!
        SUBROUTINE IOsCopyFile(FROM,TO)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: FROM
            CHARACTER(LEN=*), INTENT (IN) :: TO
        END SUBROUTINE IOsCopyFile
!
        SUBROUTINE IOsDate(IYEAR,MONTH,IDAY)
            IMPLICIT NONE
            INTEGER, INTENT (OUT) :: IYEAR
            INTEGER, INTENT (OUT) :: MONTH
            INTEGER, INTENT (OUT) :: IDAY
        END SUBROUTINE IOsDate
!
        SUBROUTINE IOsDeleteFile(FILE)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: FILE
        END SUBROUTINE IOsDeleteFile
!
        SUBROUTINE IOsDirChange(DIRECT)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: DIRECT
        END SUBROUTINE IOsDirChange
!
        SUBROUTINE IOsDirCount(DIRECT,MATCH,NUMFIL)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN)  :: DIRECT
            CHARACTER(LEN=*), INTENT (IN)  :: MATCH
            INTEGER         , INTENT (OUT) :: NUMFIL
        END SUBROUTINE IOsDirCount
!
        SUBROUTINE IOsDirEntryType(TYPE)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: TYPE
        END SUBROUTINE IOsDirEntryType
!
        FUNCTION IOsDirExists(DIRECT)
            IMPLICIT NONE
            LOGICAL                       :: IOsDirExists
            CHARACTER(LEN=*), INTENT (IN) :: DIRECT
        END FUNCTION IOsDirExists
!
        SUBROUTINE IOsDirInfo(DIRECT,MATCH,FILNAM,NUMFIL,IFDATE,IFSIZE)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN)                     :: DIRECT
            CHARACTER(LEN=*), INTENT (IN)                     :: MATCH
            INTEGER         , INTENT (IN OUT)                 :: NUMFIL
            CHARACTER(LEN=*), INTENT (OUT), DIMENSION(NUMFIL) :: FILNAM
            INTEGER         , INTENT (OUT), DIMENSION(NUMFIL) :: IFDATE
            INTEGER         , INTENT (OUT), DIMENSION(NUMFIL) :: IFSIZE
        END SUBROUTINE IOsDirInfo
!
        SUBROUTINE IOsDirList(DIRECT,MATCH,FILNAM,NUMFIL)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN)                     :: DIRECT
            CHARACTER(LEN=*), INTENT (IN)                     :: MATCH
            INTEGER      , INTENT (IN OUT)                    :: NUMFIL
            CHARACTER(LEN=*), INTENT (OUT), DIMENSION(NUMFIL) :: FILNAM
        END SUBROUTINE IOsDirList
!
        SUBROUTINE IOsDirMake(DIRECT)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: DIRECT
        END SUBROUTINE IOsDirMake
!
        SUBROUTINE IOsDirName(DIRECT)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (OUT) :: DIRECT
        END SUBROUTINE IOsDirName
!
        SUBROUTINE IOsExecute(PRGNAM)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: PRGNAM
        END SUBROUTINE IOsExecute
!
        SUBROUTINE IOsExitProgram(ERRMES,IEXCOD)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: ERRMES
            INTEGER      , INTENT (IN) :: IEXCOD
        END SUBROUTINE IOsExitProgram
!
        SUBROUTINE IOsFileDate(IFDATE,IYEAR,MONTH,IDAY)
            IMPLICIT NONE
            INTEGER, INTENT (IN)  :: IFDATE
            INTEGER, INTENT (OUT) :: IYEAR
            INTEGER, INTENT (OUT) :: MONTH
            INTEGER, INTENT (OUT) :: IDAY
        END SUBROUTINE IOsFileDate
!
        SUBROUTINE IOsFileTime(IFDATE,IHOUR,MINUTE,ISECND)
            IMPLICIT NONE
            INTEGER, INTENT (IN)  :: IFDATE
            INTEGER, INTENT (OUT) :: IHOUR
            INTEGER, INTENT (OUT) :: MINUTE
            INTEGER, INTENT (OUT) :: ISECND
        END SUBROUTINE IOsFileTime
!
        SUBROUTINE IOsRenameFile(OLDNAM,NEWNAM)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: OLDNAM
            CHARACTER(LEN=*), INTENT (IN) :: NEWNAM
        END SUBROUTINE IOsRenameFile
!
        SUBROUTINE IOsTime(IHOUR,MINUTE,ISECND,MSECND)
            IMPLICIT NONE
            INTEGER, INTENT (OUT)           :: IHOUR
            INTEGER, INTENT (OUT)           :: MINUTE
            INTEGER, INTENT (OUT)           :: ISECND
            INTEGER, INTENT (OUT), OPTIONAL :: MSECND
        END SUBROUTINE IOsTime
!
        SUBROUTINE IOsVariable(VNAME,VALUE)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN)  :: VNAME
            CHARACTER(LEN=*), INTENT (OUT) :: VALUE
        END SUBROUTINE IOsVariable
!
        SUBROUTINE IOsWait(NCSECS)
            IMPLICIT NONE
            INTEGER, INTENT (IN) :: NCSECS
        END SUBROUTINE IOsWait
!
!  BF : Binary file handling
!
        SUBROUTINE IFileClose(IHANDLE)
            IMPLICIT NONE
            INTEGER, INTENT (IN)  :: IHANDLE
        END SUBROUTINE IFileClose
!
        SUBROUTINE IFileOpen(NAME,IACCESS,IHANDLE)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN)  :: NAME
            INTEGER         , INTENT (IN)  :: IACCESS
            INTEGER         , INTENT (OUT) :: IHANDLE
        END SUBROUTINE IFileOpen
!
      END INTERFACE
!
      INTERFACE IFileRead
!
        SUBROUTINE IFileRead1(IHANDLE,BUFFER,NTOREAD,NREAD)
            IMPLICIT NONE
            INTEGER, INTENT (IN)  :: IHANDLE
            INTEGER, INTENT (OUT) :: BUFFER
            INTEGER, INTENT (IN)  :: NTOREAD
            INTEGER, INTENT (OUT) :: NREAD
        END SUBROUTINE IFileRead1
!
        SUBROUTINE IFileRead2(IHANDLE,BUFFER,NTOREAD,NREAD)
            IMPLICIT NONE
            INTEGER, INTENT (IN)                :: IHANDLE
            INTEGER, INTENT (OUT), DIMENSION(*) :: BUFFER
            INTEGER, INTENT (IN)                :: NTOREAD
            INTEGER, INTENT (OUT)               :: NREAD
        END SUBROUTINE IFileRead2
!
        SUBROUTINE IFileRead3(IHANDLE,BUFFER,NTOREAD,NREAD)
            IMPLICIT NONE
            INTEGER, INTENT (IN)  :: IHANDLE
            REAL   , INTENT (OUT) :: BUFFER
            INTEGER, INTENT (IN)  :: NTOREAD
            INTEGER, INTENT (OUT) :: NREAD
        END SUBROUTINE IFileRead3
!
        SUBROUTINE IFileRead4(IHANDLE,BUFFER,NTOREAD,NREAD)
            IMPLICIT NONE
            INTEGER, INTENT (IN)                :: IHANDLE
            REAL   , INTENT (OUT), DIMENSION(*) :: BUFFER
            INTEGER, INTENT (IN)                :: NTOREAD
            INTEGER, INTENT (OUT)               :: NREAD
        END SUBROUTINE IFileRead4
!
        SUBROUTINE IFileRead5(IHANDLE,BUFFER,NTOREAD,NREAD)
            IMPLICIT NONE
            INTEGER, INTENT (IN)  :: IHANDLE
            LOGICAL, INTENT (OUT) :: BUFFER
            INTEGER, INTENT (IN)  :: NTOREAD
            INTEGER, INTENT (OUT) :: NREAD
        END SUBROUTINE IFileRead5
!
        SUBROUTINE IFileRead6(IHANDLE,BUFFER,NTOREAD,NREAD)
            IMPLICIT NONE
            INTEGER, INTENT (IN)                :: IHANDLE
            LOGICAL, INTENT (OUT), DIMENSION(*) :: BUFFER
            INTEGER, INTENT (IN)                :: NTOREAD
            INTEGER, INTENT (OUT)               :: NREAD
        END SUBROUTINE IFileRead6
!
        SUBROUTINE IFileRead7(IHANDLE,BUFFER,NTOREAD,NREAD)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)  :: IHANDLE
            DOUBLE PRECISION, INTENT (OUT) :: BUFFER
            INTEGER         , INTENT (IN)  :: NTOREAD
            INTEGER         , INTENT (OUT) :: NREAD
        END SUBROUTINE IFileRead7
!
        SUBROUTINE IFileRead8(IHANDLE,BUFFER,NTOREAD,NREAD)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)                :: IHANDLE
            DOUBLE PRECISION, INTENT (OUT), DIMENSION(*) :: BUFFER
            INTEGER         , INTENT (IN)                :: NTOREAD
            INTEGER         , INTENT (OUT)               :: NREAD
        END SUBROUTINE IFileRead8
!
      END INTERFACE
!
      INTERFACE IFileReadChar
!
        SUBROUTINE IFileReadChar1(IHANDLE,BUFFER,NTOREAD,NREAD)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)  :: IHANDLE
            CHARACTER(LEN=*), INTENT (OUT) :: BUFFER
            INTEGER         , INTENT (IN)  :: NTOREAD
            INTEGER         , INTENT (OUT) :: NREAD
        END SUBROUTINE IFileReadChar1
!
        SUBROUTINE IFileReadChar2(IHANDLE,BUFFER,NTOREAD,NREAD)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)                :: IHANDLE
            CHARACTER(LEN=*), INTENT (OUT), DIMENSION(*) :: BUFFER
            INTEGER         , INTENT (IN)                :: NTOREAD
            INTEGER         , INTENT (OUT)               :: NREAD
        END SUBROUTINE IFileReadChar2
!
      END INTERFACE
!
      INTERFACE
!
        SUBROUTINE IFileSeek(IHANDLE,IPOS,METHOD)
            IMPLICIT NONE
            INTEGER, INTENT (IN)     :: IHANDLE
            INTEGER, INTENT (IN OUT) :: IPOS
            INTEGER, INTENT (IN)     :: METHOD
        END SUBROUTINE IFileSeek
!
      END INTERFACE
!
      INTERFACE IFileWrite
!
        SUBROUTINE IFileWrite1(IHANDLE,BUFFER,NTOWRITE,NWRITTEN)
            IMPLICIT NONE
            INTEGER, INTENT (IN)  :: IHANDLE
            INTEGER, INTENT (IN)  :: BUFFER
            INTEGER, INTENT (IN)  :: NTOWRITE
            INTEGER, INTENT (OUT) :: NWRITTEN
        END SUBROUTINE IFileWrite1
!
        SUBROUTINE IFileWrite2(IHANDLE,BUFFER,NTOWRITE,NWRITTEN)
            IMPLICIT NONE
            INTEGER, INTENT (IN)               :: IHANDLE
            INTEGER, INTENT (IN), DIMENSION(*) :: BUFFER
            INTEGER, INTENT (IN)               :: NTOWRITE
            INTEGER, INTENT (OUT)              :: NWRITTEN
        END SUBROUTINE IFileWrite2
!
        SUBROUTINE IFileWrite3(IHANDLE,BUFFER,NTOWRITE,NWRITTEN)
            IMPLICIT NONE
            INTEGER, INTENT (IN)  :: IHANDLE
            REAL   , INTENT (IN)  :: BUFFER
            INTEGER, INTENT (IN)  :: NTOWRITE
            INTEGER, INTENT (OUT) :: NWRITTEN
        END SUBROUTINE IFileWrite3
!
        SUBROUTINE IFileWrite4(IHANDLE,BUFFER,NTOWRITE,NWRITTEN)
            IMPLICIT NONE
            INTEGER, INTENT (IN)               :: IHANDLE
            REAL   , INTENT (IN), DIMENSION(*) :: BUFFER
            INTEGER, INTENT (IN)               :: NTOWRITE
            INTEGER, INTENT (OUT)              :: NWRITTEN
        END SUBROUTINE IFileWrite4
!
        SUBROUTINE IFileWrite5(IHANDLE,BUFFER,NTOWRITE,NWRITTEN)
            IMPLICIT NONE
            INTEGER, INTENT (IN)  :: IHANDLE
            LOGICAL, INTENT (IN)  :: BUFFER
            INTEGER, INTENT (IN)  :: NTOWRITE
            INTEGER, INTENT (OUT) :: NWRITTEN
        END SUBROUTINE IFileWrite5
!
        SUBROUTINE IFileWrite6(IHANDLE,BUFFER,NTOWRITE,NWRITTEN)
            IMPLICIT NONE
            INTEGER, INTENT (IN)               :: IHANDLE
            LOGICAL, INTENT (IN), DIMENSION(*) :: BUFFER
            INTEGER, INTENT (IN)               :: NTOWRITE
            INTEGER, INTENT (OUT)              :: NWRITTEN
        END SUBROUTINE IFileWrite6
!
        SUBROUTINE IFileWrite7(IHANDLE,BUFFER,NTOWRITE,NWRITTEN)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)  :: IHANDLE
            DOUBLE PRECISION, INTENT (IN)  :: BUFFER
            INTEGER         , INTENT (IN)  :: NTOWRITE
            INTEGER         , INTENT (OUT) :: NWRITTEN
        END SUBROUTINE IFileWrite7
!
        SUBROUTINE IFileWrite8(IHANDLE,BUFFER,NTOWRITE,NWRITTEN)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)               :: IHANDLE
            DOUBLE PRECISION, INTENT (IN), DIMENSION(*) :: BUFFER
            INTEGER         , INTENT (IN)               :: NTOWRITE
            INTEGER         , INTENT (OUT)              :: NWRITTEN
        END SUBROUTINE IFileWrite8
!
      END INTERFACE
!
      INTERFACE IFileWriteChar
!
        SUBROUTINE IFileWriteChar1(IHANDLE,BUFFER,NTOWRITE,NWRITTEN)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)  :: IHANDLE
            CHARACTER(LEN=*), INTENT (IN)  :: BUFFER
            INTEGER         , INTENT (IN)  :: NTOWRITE
            INTEGER         , INTENT (OUT) :: NWRITTEN
        END SUBROUTINE IFileWriteChar1
!
        SUBROUTINE IFileWriteChar2(IHANDLE,BUFFER,NTOWRITE,NWRITTEN)
            IMPLICIT NONE
            INTEGER         , INTENT (IN)               :: IHANDLE
            CHARACTER(LEN=*), INTENT (IN), DIMENSION(*) :: BUFFER
            INTEGER         , INTENT (IN)               :: NTOWRITE
            INTEGER         , INTENT (OUT)              :: NWRITTEN
        END SUBROUTINE IFileWriteChar2
!
      END INTERFACE
!
      INTERFACE
!
!  DG : Debugging
!
        SUBROUTINE IDebug(STRING)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT(IN)           :: STRING
        END SUBROUTINE IDebug
!
        SUBROUTINE IDebugDouble(STRING,DVAL,FORMAT)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT(IN)           :: STRING
            DOUBLE PRECISION, INTENT(IN)           :: DVAL
            CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: FORMAT
        END SUBROUTINE IDebugDouble
!
        SUBROUTINE IDebugInteger(STRING,IVAL)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT(IN)           :: STRING
            INTEGER,          INTENT(IN)           :: IVAL
        END SUBROUTINE IDebugInteger
!
        SUBROUTINE IDebugLevel(LEVEL)
            IMPLICIT NONE
            INTEGER,          INTENT(IN)           :: LEVEL
        END SUBROUTINE IDebugLevel
!
        SUBROUTINE IDebugReal(STRING,RVAL,FORMAT)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT(IN)           :: STRING
            REAL,             INTENT(IN)           :: RVAL
            CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: FORMAT
        END SUBROUTINE IDebugReal
!
!  MI : Miscellaneous
!
        SUBROUTINE IPlotter(IPLOTR)
            IMPLICIT NONE
            INTEGER, INTENT (IN) :: IPLOTR
        END SUBROUTINE IPlotter
!
        SUBROUTINE IPrinter(IPRINT)
            IMPLICIT NONE
            INTEGER, INTENT (IN) :: IPRINT
        END SUBROUTINE IPrinter
!
        FUNCTION IRandomNumber(ISEED)
            IMPLICIT NONE
            REAL                 :: IRandomNumber
            INTEGER, INTENT (IN) :: ISEED
        END FUNCTION IRandomNumber
!
        SUBROUTINE WCursorShape(ISHAPE)
            IMPLICIT NONE
            INTEGER, INTENT (IN) :: ISHAPE
        END SUBROUTINE WCursorShape
!
        SUBROUTINE WCursorGXY(XPOS,YPOS)
            IMPLICIT NONE
            REAL,    INTENT (IN) :: XPOS
            REAL,    INTENT (IN) :: YPOS
        END SUBROUTINE WCursorGXY
!
        SUBROUTINE WFlushBuffer()
        END SUBROUTINE WFlushBuffer
!
        SUBROUTINE WCursorXY(IXPOS,IYPOS)
            IMPLICIT NONE
            INTEGER, INTENT (IN) :: IXPOS
            INTEGER, INTENT (IN) :: IYPOS
        END SUBROUTINE WCursorXY
!
        SUBROUTINE WglSelect(ITARGET,IDENT,IFLAGS)
            IMPLICIT NONE
            INTEGER, INTENT (IN)           :: ITARGET
            INTEGER, INTENT (IN), OPTIONAL :: IDENT
            INTEGER, INTENT (IN), OPTIONAL :: IFLAGS
        END SUBROUTINE WglSelect
!
        SUBROUTINE WglSwapBuffers()
            IMPLICIT NONE
        END SUBROUTINE WglSwapBuffers
!
      END INTERFACE
!
      INTERFACE WHelpFile
        SUBROUTINE WHelpFile1(FILENAME, TOPIC, MODE)
            CHARACTER(LEN=*),  INTENT(IN) :: FILENAME
            INTEGER, OPTIONAL, INTENT(IN) :: TOPIC
            INTEGER, OPTIONAL, INTENT(IN) :: MODE
        END SUBROUTINE WHelpFile1
!
        SUBROUTINE WHelpFile2(FILENAME, TOPIC)
            CHARACTER(LEN=*),  INTENT(IN) :: FILENAME
            CHARACTER(LEN=*),  INTENT(IN) :: TOPIC
        END SUBROUTINE WHelpFile2
      END INTERFACE
!
      INTERFACE
        SUBROUTINE WindowBell(ONOFF)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: ONOFF
        END SUBROUTINE WindowBell
!
        SUBROUTINE WInitialise(INITFN)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: INITFN
        END SUBROUTINE WInitialise
!
        SUBROUTINE WPlayMovie(MOVIEFILE, IFLAGS, ISTART, IEND)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT(IN)           :: MOVIEFILE
            INTEGER,          INTENT(IN), OPTIONAL :: IFLAGS
            INTEGER,          INTENT(IN), OPTIONAL :: ISTART
            INTEGER,          INTENT(IN), OPTIONAL :: IEND
        END SUBROUTINE WPlayMovie
!
        SUBROUTINE WPlaySound(FILENAME,IFLAGS)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN)           :: FILENAME
            INTEGER         , INTENT (IN), OPTIONAL :: IFLAGS
        END SUBROUTINE WPlaySound
!
!  CH : Character Manipulation
!
        FUNCTION IActualLength(STRING)
            IMPLICIT NONE
            INTEGER                       :: IActualLength
            CHARACTER(LEN=*), INTENT (IN) :: STRING
        END FUNCTION IActualLength
!
        SUBROUTINE IDoubleToString(DVALUE,STRING,FRMAT)
            IMPLICIT NONE
            DOUBLE PRECISION           , INTENT (IN)  :: DVALUE
            CHARACTER(LEN=*)           , INTENT (OUT) :: STRING
            CHARACTER(LEN=*)           , INTENT (IN)  :: FRMAT
        END SUBROUTINE IDoubleToString
!
        SUBROUTINE IFillString(STRING,CHR)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (OUT) :: STRING
            CHARACTER(LEN=1)  , INTENT (IN)  :: CHR
        END SUBROUTINE IFillString
!
        SUBROUTINE IJustifyString(STRING,LCR)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN OUT) :: STRING
            CHARACTER(LEN=*), INTENT (IN)    :: LCR
        END SUBROUTINE IJustifyString
!
        FUNCTION ILocateChar(STRING)
            IMPLICIT NONE
            INTEGER                       :: ILocateChar
            CHARACTER(LEN=*), INTENT (IN) :: STRING
        END FUNCTION ILocateChar
!
        SUBROUTINE ILocateString(STRING,ISTART,IEND)
            IMPLICIT NONE
            CHARACTER(LEN=*) , INTENT (IN)  :: STRING
            INTEGER      , INTENT (OUT) :: ISTART
            INTEGER      , INTENT (OUT) :: IEND
        END SUBROUTINE ILocateString
!
        SUBROUTINE ILowerCase(STRING)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN OUT) :: STRING
        END SUBROUTINE ILowerCase
!
        SUBROUTINE INextDouble(STRING,DVALUE)
            IMPLICIT NONE
            CHARACTER(LEN=*)          , INTENT (IN OUT) :: STRING
              DOUBLE PRECISION        , INTENT (OUT)    :: DVALUE
        END SUBROUTINE INextDouble
!
        SUBROUTINE INextInteger(STRING,IVALUE)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN OUT) :: STRING
            INTEGER      , INTENT (OUT)   :: IVALUE
        END SUBROUTINE INextInteger
!
        SUBROUTINE INextReal(STRING,RVALUE)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN OUT) :: STRING
            REAL         , INTENT (OUT)   :: RVALUE
        END SUBROUTINE INextReal
!
        SUBROUTINE INextString(STRING,SVALUE)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN OUT) :: STRING
            CHARACTER(LEN=*), INTENT (OUT)   :: SVALUE
        END SUBROUTINE INextString
!
        SUBROUTINE IntegerToString(IVALUE,STRING,FRMAT)
            IMPLICIT NONE
            INTEGER      , INTENT (IN)  :: IVALUE
            CHARACTER(LEN=*), INTENT (OUT) :: STRING
            CHARACTER(LEN=*), INTENT (IN)  :: FRMAT
        END SUBROUTINE IntegerToString
!
        FUNCTION IntValueOfChar(CHR)
            IMPLICIT NONE
            INTEGER                       :: IntValueOfChar
            CHARACTER(LEN=*), INTENT (IN) :: CHR
        END FUNCTION IntValueOfChar
!
        SUBROUTINE IRealToString(RVALUE,STRING,FRMAT)
            IMPLICIT NONE
            REAL         , INTENT (IN)  :: RVALUE
            CHARACTER(LEN=*), INTENT (OUT) :: STRING
            CHARACTER(LEN=*), INTENT (IN)  :: FRMAT
        END SUBROUTINE IRealToString
!
        SUBROUTINE IStringToDouble(STRING,DVALUE)
            IMPLICIT NONE
            CHARACTER(LEN=*)        , INTENT (IN)  :: STRING
            DOUBLE PRECISION        , INTENT (OUT) :: DVALUE
        END SUBROUTINE IStringToDouble
!
        SUBROUTINE IStringToInteger(STRING,IVALUE)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN)  :: STRING
            INTEGER      , INTENT (OUT) :: IVALUE
        END SUBROUTINE IStringToInteger
!
        SUBROUTINE IStringToReal(STRING,RVALUE)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN) :: STRING
            REAL         , INTENT (OUT) :: RVALUE
        END SUBROUTINE IStringToReal
!
        SUBROUTINE IUpperCase(STRING)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT (IN OUT) :: STRING
        END SUBROUTINE IUpperCase
!
!  MM : Maximum/Minimum Calculations
!
        FUNCTION IDoubleMax1(DARRAY,IDIM)
            IMPLICIT NONE
            DOUBLE PRECISION                                :: IDoubleMax1
            INTEGER          , INTENT (IN)                  :: IDIM
            DOUBLE PRECISION , INTENT (IN), DIMENSION(IDIM) :: DARRAY
        END FUNCTION IDoubleMax1
!
        FUNCTION IDoubleMax2(DARRAY,IDIM1,IDIM2,MDIM1,MDIM2)
            IMPLICIT NONE
            DOUBLE PRECISION                                      :: IDoubleMax2
            INTEGER          , INTENT (IN)                        :: IDIM1
            INTEGER          , INTENT (IN)                        :: IDIM2
            DOUBLE PRECISION , INTENT (IN),DIMENSION(IDIM1,IDIM2) :: DARRAY
            INTEGER          , INTENT (IN)                        :: MDIM1
            INTEGER          , INTENT (IN)                        :: MDIM2
        END FUNCTION IDoubleMax2
!
        SUBROUTINE IDoubleMaxMin1(DARRAY,IDIM,DPMIN,DPMAX)
            IMPLICIT NONE
            INTEGER          , INTENT (IN)                  :: IDIM
            DOUBLE PRECISION , INTENT (IN), DIMENSION(IDIM) :: DARRAY
            DOUBLE PRECISION , INTENT (OUT)                 :: DPMIN
            DOUBLE PRECISION , INTENT (OUT)                 :: DPMAX
        END SUBROUTINE IDoubleMaxMin1
!
        SUBROUTINE IDoubleMaxMin2(DARRAY,IDIM1,IDIM2,MDIM1,MDIM2,DPMIN,DPMAX)
            IMPLICIT NONE
            INTEGER          , INTENT (IN)                        :: IDIM1
            INTEGER          , INTENT (IN)                        :: IDIM2
            DOUBLE PRECISION , INTENT (IN),DIMENSION(IDIM1,IDIM2) :: DARRAY
            INTEGER          , INTENT (IN)                        :: MDIM1
            INTEGER          , INTENT (IN)                        :: MDIM2
            DOUBLE PRECISION , INTENT (OUT)                       :: DPMIN
            DOUBLE PRECISION , INTENT (OUT)                       :: DPMAX
        END SUBROUTINE IDoubleMaxMin2
!
        FUNCTION IDoubleMin1(DARRAY,IDIM)
            IMPLICIT NONE
            DOUBLE PRECISION                                :: IDoubleMin1
            INTEGER          , INTENT (IN)                  :: IDIM
            DOUBLE PRECISION , INTENT (IN), DIMENSION(IDIM) :: DARRAY
        END FUNCTION IDoubleMin1
!
        FUNCTION IDoubleMin2(DARRAY,IDIM1,IDIM2,MDIM1,MDIM2)
            IMPLICIT NONE
            DOUBLE PRECISION                                      :: IDoubleMin2
            INTEGER          , INTENT (IN)                        :: IDIM1
            INTEGER          , INTENT (IN)                        :: IDIM2
            DOUBLE PRECISION , INTENT (IN),DIMENSION(IDIM1,IDIM2) :: DARRAY
            INTEGER          , INTENT (IN)                        :: MDIM1
            INTEGER          , INTENT (IN)                        :: MDIM2
        END FUNCTION IDoubleMin2
!
        FUNCTION IRealMax1(ARRAY,IDIM)
            IMPLICIT NONE
            REAL                                   :: IRealMax1
            INTEGER, INTENT (IN)                   :: IDIM
            REAL   , INTENT (IN), DIMENSION(IDIM) :: ARRAY
        END FUNCTION IRealMax1
!
        FUNCTION IRealMax2(ARRAY,IDIM1,IDIM2,MDIM1,MDIM2)
            IMPLICIT NONE
            REAL                                          :: IRealMax2
            INTEGER, INTENT (IN)                          :: IDIM1
            INTEGER, INTENT (IN)                          :: IDIM2
            REAL   , INTENT (IN), DIMENSION (IDIM1,IDIM2) :: ARRAY
            INTEGER, INTENT (IN)                          :: MDIM1
            INTEGER, INTENT (IN)                          :: MDIM2
        END FUNCTION IRealMax2
!
        FUNCTION IRealMaxCum2(ARRAY,IDIM1,IDIM2,MDIM1,MDIM2)
            IMPLICIT NONE
            REAL                                          :: IRealMaxCum2
            INTEGER, INTENT (IN)                          :: IDIM1
            INTEGER, INTENT (IN)                          :: IDIM2
            REAL   , INTENT (IN), DIMENSION (IDIM1,IDIM2) :: ARRAY
            INTEGER, INTENT (IN)                          :: MDIM1
            INTEGER, INTENT (IN)                          :: MDIM2
        END FUNCTION IRealMaxCum2
!
        FUNCTION IRealMaxFunc1(FUNC,XMIN,XMAX,NVALX)
            IMPLICIT NONE
            REAL                  :: IRealMaxFunc1
            INTERFACE
                FUNCTION FUNC(X)
                    IMPLICIT NONE
                    REAL              :: FUNC
                    REAL, INTENT (IN) :: X
                END FUNCTION FUNC
            END INTERFACE
            REAL    , INTENT (IN) :: XMIN
            REAL    , INTENT (IN) :: XMAX
            INTEGER , INTENT (IN) :: NVALX
        END FUNCTION IRealMaxFunc1
!
        FUNCTION IRealMaxFunc2(FUNC,XMIN,YMIN,XMAX,YMAX,NVALX,NVALY)
            IMPLICIT NONE
            REAL                  :: IRealMaxFunc2
            INTERFACE
                FUNCTION FUNC(X,Y)
                    IMPLICIT NONE
                    REAL              :: FUNC
                    REAL, INTENT (IN) :: X
                    REAL, INTENT (IN) :: Y
                END FUNCTION FUNC
            END INTERFACE
            REAL    , INTENT (IN) :: XMIN
            REAL    , INTENT (IN) :: YMIN
            REAL    , INTENT (IN) :: XMAX
            REAL    , INTENT (IN) :: YMAX
            INTEGER , INTENT (IN) :: NVALX
            INTEGER , INTENT (IN) :: NVALY
        END FUNCTION IRealMaxFunc2
!
        SUBROUTINE IRealMaxMin1(ARRAY,IDIM,ARMIN,ARMAX)
            IMPLICIT NONE
            INTEGER, INTENT (IN)                   :: IDIM
            REAL   , INTENT (IN), DIMENSION (IDIM) :: ARRAY
            REAL   , INTENT (OUT)                  :: ARMIN
            REAL   , INTENT (OUT)                  :: ARMAX
        END SUBROUTINE IRealMaxMin1
!
        SUBROUTINE IRealMaxMin2(ARRAY,IDIM1,IDIM2,MDIM1,MDIM2,ARMIN,ARMAX)
            IMPLICIT NONE
            INTEGER, INTENT (IN)                          :: IDIM1
            INTEGER, INTENT (IN)                          :: IDIM2
            REAL   , INTENT (IN), DIMENSION (IDIM1,IDIM2) :: ARRAY
            INTEGER, INTENT (IN)                          :: MDIM1
            INTEGER, INTENT (IN)                          :: MDIM2
            REAL   , INTENT (OUT)                         :: ARMIN
            REAL   , INTENT (OUT)                         :: ARMAX
        END SUBROUTINE IRealMaxMin2
!
        SUBROUTINE IRealMaxMinFunc1(FUNC,XMIN,XMAX,NVALX,FMIN,FMAX)
            IMPLICIT NONE
            INTERFACE
                FUNCTION FUNC(X)
                    IMPLICIT NONE
                    REAL              :: FUNC
                    REAL, INTENT (IN) :: X
                END FUNCTION FUNC
            END INTERFACE
            REAL    , INTENT (IN)  :: XMIN
            REAL    , INTENT (IN)  :: XMAX
            INTEGER , INTENT (IN)  :: NVALX
            REAL    , INTENT (OUT) :: FMIN
            REAL    , INTENT (OUT) :: FMAX
        END SUBROUTINE IRealMaxMinFunc1
!
        SUBROUTINE IRealMaxMinFunc2(FUNC,XMIN,YMIN,XMAX,YMAX,NVALX,NVALY,FMIN,&
                                    FMAX)
            IMPLICIT NONE
            INTERFACE
                FUNCTION FUNC(X,Y)
                    IMPLICIT NONE
                    REAL              :: FUNC
                    REAL, INTENT (IN) :: X
                    REAL, INTENT (IN) :: Y
                END FUNCTION FUNC
            END INTERFACE
            REAL    , INTENT (IN)  :: XMIN
            REAL    , INTENT (IN)  :: YMIN
            REAL    , INTENT (IN)  :: XMAX
            REAL    , INTENT (IN)  :: YMAX
            INTEGER , INTENT (IN)  :: NVALX
            INTEGER , INTENT (IN)  :: NVALY
            REAL    , INTENT (OUT) :: FMIN
            REAL    , INTENT (OUT) :: FMAX
        END SUBROUTINE IRealMaxMinFunc2
!
        FUNCTION IRealMin1(ARRAY,IDIM)
            IMPLICIT NONE
            REAL                                :: IRealMin1
            INTEGER, INTENT (IN)                :: IDIM
            REAL   , INTENT (IN), DIMENSION (*) :: ARRAY
        END FUNCTION IRealMin1
!
        FUNCTION IRealMin2(ARRAY,IDIM1,IDIM2,MDIM1,MDIM2)
            IMPLICIT NONE
            REAL                                          :: IRealMin2
            INTEGER, INTENT (IN)                          :: IDIM1
            INTEGER, INTENT (IN)                          :: IDIM2
            REAL   , INTENT (IN), DIMENSION (IDIM1,IDIM2) :: ARRAY
            INTEGER, INTENT (IN)                          :: MDIM1
            INTEGER, INTENT (IN)                          :: MDIM2
        END FUNCTION IRealMin2
!
        FUNCTION IRealMinFunc1(FUNC,XMIN,XMAX,NVALX)
            IMPLICIT NONE
            REAL                  :: IRealMinFunc1
            INTERFACE
                FUNCTION FUNC(X)
                    IMPLICIT NONE
                    REAL              :: FUNC
                    REAL, INTENT (IN) :: X
                END FUNCTION FUNC
            END INTERFACE
            REAL    , INTENT (IN) :: XMIN
            REAL    , INTENT (IN) :: XMAX
            INTEGER , INTENT (IN) :: NVALX
        END FUNCTION IRealMinFunc1
!
        FUNCTION IRealMinFunc2(FUNC,XMIN,YMIN,XMAX,YMAX,NVALX,NVALY)
            IMPLICIT NONE
            REAL                  :: IRealMinFunc2
            INTERFACE
                FUNCTION FUNC(X)
                    IMPLICIT NONE
                    REAL              :: FUNC
                    REAL, INTENT (IN) :: X
                END FUNCTION FUNC
            END INTERFACE
            REAL    , INTENT (IN) :: XMIN
            REAL    , INTENT (IN) :: YMIN
            REAL    , INTENT (IN) :: XMAX
            REAL    , INTENT (IN) :: YMAX
            INTEGER , INTENT (IN) :: NVALX
            INTEGER , INTENT (IN) :: NVALY
        END FUNCTION IRealMinFunc2
!
      END INTERFACE
!
! ********************************************************************
! PARAMETER definitions
! ********************************************************************
!
!  Symbolic names used by many functions.
!  Description : Yes/No and other parameters used by several
!                Winteracter routines
!
      INTEGER, PARAMETER :: IntNo            = 0
      INTEGER, PARAMETER :: IntYes           = 1
      INTEGER, PARAMETER :: Centred          = 0
      INTEGER, PARAMETER :: IntNone          = 0
      INTEGER, PARAMETER :: IntPortrait      = 0
      INTEGER, PARAMETER :: IntLandscape     = 1
      INTEGER, PARAMETER :: IntPortraitWH    = 2
      INTEGER, PARAMETER :: IntLandscapeWH   = 3
      INTEGER, PARAMETER :: RLEComp          = 1
      INTEGER, PARAMETER :: TIFFComp         = 2
      INTEGER, PARAMETER :: WintOff          = 0
      INTEGER, PARAMETER :: WintOn           = 1
      INTEGER, PARAMETER :: Disabled         = 0
      INTEGER, PARAMETER :: Enabled          = 1
      INTEGER, PARAMETER :: UnChecked        = 0
      INTEGER, PARAMETER :: Checked          = 1
!
! ***************************************************************************
!
!  Group WM : Window Management
!
!  Subroutine  : WindowStatusBarParts
!  Description : Border styles
!
      INTEGER, PARAMETER :: BorderNone       = 0
      INTEGER, PARAMETER :: BorderSunken     = 1
      INTEGER, PARAMETER :: BorderRaised     = 2
!
!  Subroutine  : WindowOpen & WindowOpenChild
!  Description : Window type options
!
      INTEGER, PARAMETER :: SysMenuOn        =   1
      INTEGER, PARAMETER :: MinButton        =   2
      INTEGER, PARAMETER :: MaxButton        =   4
      INTEGER, PARAMETER :: MaxWindow        =   8
      INTEGER, PARAMETER :: InsideRoot       =  16   ! WindowOpenChild only
      INTEGER, PARAMETER :: StatusBar        =  32
      INTEGER, PARAMETER :: FixedSizeWin     =  64
      INTEGER, PARAMETER :: HideWindow       = 128
      INTEGER, PARAMETER :: OwnedByRoot      = 256   ! WindowOpenChild only
      INTEGER, PARAMETER :: AlwaysOnTop      = 512
!
      INTEGER, PARAMETER :: HideRoot         = 128   ! Old name for HideWindow
!
! ***************************************************************************
!
!  Group FS : Font selection
!
!  Subroutine  : WindowFontColour
!  Description : Font colours
!
      INTEGER, PARAMETER :: NotSet           = -1
      INTEGER, PARAMETER :: TextBlack        =  0
      INTEGER, PARAMETER :: TextRed          =  1
      INTEGER, PARAMETER :: TextYellow       =  2
      INTEGER, PARAMETER :: TextGreen        =  3
      INTEGER, PARAMETER :: TextCyan         =  4
      INTEGER, PARAMETER :: TextBlue         =  5
      INTEGER, PARAMETER :: TextMagenta      =  6
      INTEGER, PARAMETER :: TextWhite        =  7
      INTEGER, PARAMETER :: TextBlackBold    =  8
      INTEGER, PARAMETER :: TextRedBold      =  9
      INTEGER, PARAMETER :: TextYellowBold   = 10
      INTEGER, PARAMETER :: TextGreenBold    = 11
      INTEGER, PARAMETER :: TextCyanBold     = 12
      INTEGER, PARAMETER :: TextBlueBold     = 13
      INTEGER, PARAMETER :: TextMagentaBold  = 14
      INTEGER, PARAMETER :: TextWhiteBold    = 15
      INTEGER, PARAMETER :: TextUserColour   = 16
!
!  Subroutine  : WindowFontStyle
!  Description : Font types
!
      INTEGER, PARAMETER :: SystemProp       = 0
      INTEGER, PARAMETER :: SystemFixed      = 1
      INTEGER, PARAMETER :: TimesNewRoman    = 2
      INTEGER, PARAMETER :: Swiss            = 3
      INTEGER, PARAMETER :: CourierNew       = 4
      INTEGER, PARAMETER :: Symbols          = 5
      INTEGER, PARAMETER :: Wingdings        = 6
!
! ***************************************************************************
!
!  Group ED : Text Editor
!
!  Subroutine  : WEditFile
!  Description : Edit/View file in current child window
!
      INTEGER, PARAMETER :: WordWrap         =    1
      INTEGER, PARAMETER :: FileMustExist    =    2
      INTEGER, PARAMETER :: ViewOnly         =    4
      INTEGER, PARAMETER :: CommandLine      =    8
      INTEGER, PARAMETER :: CommandHistory   =   16
      INTEGER, PARAMETER :: NoToolbar        =   32
      INTEGER, PARAMETER :: RTFSupport       =   64
      INTEGER, PARAMETER :: NoFileNewOpen    =  256
      INTEGER, PARAMETER :: NoFileSaveAs     =  512
      INTEGER, PARAMETER :: NoFilePrint      = 1024
!
!  Menu identifiers for built in menu options.
!  Returned via WMessage/WMessagePeek
!
      INTEGER, PARAMETER :: EditorFileNew      = 32101
      INTEGER, PARAMETER :: EditorFileOpen     = 32102
      INTEGER, PARAMETER :: EditorFileSave     = 32103
      INTEGER, PARAMETER :: EditorFileSaveAs   = 32104
      INTEGER, PARAMETER :: EditorFilePrint    = 32105
      INTEGER, PARAMETER :: EditorFilePrintSel = 32106
!
!  Subroutine  : WEditPos
!  Description : Convert between line and character positions
!
      INTEGER, PARAMETER :: EditorCharToLine = 1
      INTEGER, PARAMETER :: EditorLineToChar = 2
!
! ***************************************************************************
!
!  Group MH : Message Handling
!
!  Subroutine  : WMessage & WMessageEnable
!  Description : Message types
!
      INTEGER, PARAMETER :: KeyDown          = 1
      INTEGER, PARAMETER :: MenuSelect       = 2
      INTEGER, PARAMETER :: PushButton       = 3
      INTEGER, PARAMETER :: MouseButDown     = 4
      INTEGER, PARAMETER :: MouseButUp       = 5
      INTEGER, PARAMETER :: MouseMove        = 6
      INTEGER, PARAMETER :: Expose           = 7
      INTEGER, PARAMETER :: Resize           = 8
      INTEGER, PARAMETER :: CloseRequest     = 9
      INTEGER, PARAMETER :: FieldChanged     = 10
      INTEGER, PARAMETER :: TabChanged       = 11
      INTEGER, PARAMETER :: BorderSelect     = 12
      INTEGER, PARAMETER :: TimerExpired     = 13
      INTEGER, PARAMETER :: EditorCommand    = 14
      INTEGER, PARAMETER :: BitmapScrolled   = 15
!
!  Description : Message sources
!
      INTEGER, PARAMETER :: FromWindow       = 0
      INTEGER, PARAMETER :: FromDialog       = 1
      INTEGER, PARAMETER :: FromBitmap       = 2
!
!  Subroutine  : WMessagePeek
!  Description : Extra message type for WMessagePeek
!
      INTEGER, PARAMETER :: NoMessage        = -1
!
!  Subroutine  : WMessage/WMessagePeek
!  Description : Non printable key codes & mouse button numbers
!
      INTEGER, PARAMETER :: KeyBackSpace     =   8
      INTEGER, PARAMETER :: KeyTab           =   9
      INTEGER, PARAMETER :: KeyReturn        =  13
      INTEGER, PARAMETER :: KeyEscape        =  27
      INTEGER, PARAMETER :: KeyDelete        = 127
      INTEGER, PARAMETER :: KeyCursorUp      = 258
      INTEGER, PARAMETER :: KeyCursorDown    = 259
      INTEGER, PARAMETER :: KeyCursorRight   = 260
      INTEGER, PARAMETER :: KeyCursorLeft    = 261
      INTEGER, PARAMETER :: KeyPageUp        = 262
      INTEGER, PARAMETER :: KeyPageDown      = 263
      INTEGER, PARAMETER :: KeyPageRight     = 264
      INTEGER, PARAMETER :: KeyPageLeft      = 265
      INTEGER, PARAMETER :: KeyUpExtreme     = 266
      INTEGER, PARAMETER :: KeyDownExtreme   = 267
      INTEGER, PARAMETER :: KeyRightExtreme  = 268
      INTEGER, PARAMETER :: KeyLeftExtreme   = 269
      INTEGER, PARAMETER :: KeyHome          = 270
      INTEGER, PARAMETER :: KeyEnd           = 271
      INTEGER, PARAMETER :: KeyInsert        = 272
      INTEGER, PARAMETER :: KeyDeleteUnder   = 273
      INTEGER, PARAMETER :: KeyShiftTab      = 274
      INTEGER, PARAMETER :: Keypad0          = 280
      INTEGER, PARAMETER :: Keypad1          = 281
      INTEGER, PARAMETER :: Keypad2          = 282
      INTEGER, PARAMETER :: Keypad3          = 283
      INTEGER, PARAMETER :: Keypad4          = 284
      INTEGER, PARAMETER :: Keypad5          = 285
      INTEGER, PARAMETER :: Keypad6          = 286
      INTEGER, PARAMETER :: Keypad7          = 287
      INTEGER, PARAMETER :: Keypad8          = 288
      INTEGER, PARAMETER :: Keypad9          = 289
      INTEGER, PARAMETER :: KeypadMinus      = 290
      INTEGER, PARAMETER :: KeypadPoint      = 291
      INTEGER, PARAMETER :: KeypadPlus       = 292
      INTEGER, PARAMETER :: KeypadDivide     = 293
      INTEGER, PARAMETER :: KeypadMultiply   = 294
      INTEGER, PARAMETER :: KeyPrint         = 300
!
      INTEGER, PARAMETER :: KeyF1            = 301
      INTEGER, PARAMETER :: KeyF2            = 302
      INTEGER, PARAMETER :: KeyF3            = 303
      INTEGER, PARAMETER :: KeyF4            = 304
      INTEGER, PARAMETER :: KeyF5            = 305
      INTEGER, PARAMETER :: KeyF6            = 306
      INTEGER, PARAMETER :: KeyF7            = 307
      INTEGER, PARAMETER :: KeyF8            = 308
      INTEGER, PARAMETER :: KeyF9            = 309
      INTEGER, PARAMETER :: KeyF10           = 310
      INTEGER, PARAMETER :: KeyF11           = 311
      INTEGER, PARAMETER :: KeyF12           = 312
      INTEGER, PARAMETER :: KeyF13           = 313
      INTEGER, PARAMETER :: KeyF14           = 314
      INTEGER, PARAMETER :: KeyF15           = 315
      INTEGER, PARAMETER :: KeyF16           = 316
      INTEGER, PARAMETER :: KeyF17           = 317
      INTEGER, PARAMETER :: KeyF18           = 318
      INTEGER, PARAMETER :: KeyF19           = 319
      INTEGER, PARAMETER :: KeyF20           = 320
      INTEGER, PARAMETER :: KeyShiftF1       = 321
      INTEGER, PARAMETER :: KeyShiftF2       = 322
      INTEGER, PARAMETER :: KeyShiftF3       = 323
      INTEGER, PARAMETER :: KeyShiftF4       = 324
      INTEGER, PARAMETER :: KeyShiftF5       = 325
      INTEGER, PARAMETER :: KeyShiftF6       = 326
      INTEGER, PARAMETER :: KeyShiftF7       = 327
      INTEGER, PARAMETER :: KeyShiftF8       = 328
      INTEGER, PARAMETER :: KeyShiftF9       = 329
      INTEGER, PARAMETER :: KeyShiftF10      = 330
      INTEGER, PARAMETER :: KeyShiftF11      = 331
      INTEGER, PARAMETER :: KeyShiftF12      = 332
      INTEGER, PARAMETER :: KeyShiftF13      = 333
      INTEGER, PARAMETER :: KeyShiftF14      = 334
      INTEGER, PARAMETER :: KeyShiftF15      = 335
      INTEGER, PARAMETER :: KeyShiftF16      = 336
      INTEGER, PARAMETER :: KeyShiftF17      = 337
      INTEGER, PARAMETER :: KeyShiftF18      = 338
      INTEGER, PARAMETER :: KeyShiftF19      = 339
      INTEGER, PARAMETER :: KeyShiftF20      = 340
      INTEGER, PARAMETER :: KeyCtrlF1        = 341
      INTEGER, PARAMETER :: KeyCtrlF2        = 342
      INTEGER, PARAMETER :: KeyCtrlF3        = 343
      INTEGER, PARAMETER :: KeyCtrlF4        = 344
      INTEGER, PARAMETER :: KeyCtrlF5        = 345
      INTEGER, PARAMETER :: KeyCtrlF6        = 346
      INTEGER, PARAMETER :: KeyCtrlF7        = 347
      INTEGER, PARAMETER :: KeyCtrlF8        = 348
      INTEGER, PARAMETER :: KeyCtrlF9        = 349
      INTEGER, PARAMETER :: KeyCtrlF10       = 350
      INTEGER, PARAMETER :: KeyCtrlF11       = 351
      INTEGER, PARAMETER :: KeyCtrlF12       = 352
      INTEGER, PARAMETER :: KeyCtrlF13       = 353
      INTEGER, PARAMETER :: KeyCtrlF14       = 354
      INTEGER, PARAMETER :: KeyCtrlF15       = 355
      INTEGER, PARAMETER :: KeyCtrlF16       = 356
      INTEGER, PARAMETER :: KeyCtrlF17       = 357
      INTEGER, PARAMETER :: KeyCtrlF18       = 358
      INTEGER, PARAMETER :: KeyCtrlF19       = 359
      INTEGER, PARAMETER :: KeyCtrlF20       = 360
!
      INTEGER, PARAMETER :: LeftButton       =   1
      INTEGER, PARAMETER :: MiddleButton     =   2
      INTEGER, PARAMETER :: RightButton      =   3
!
!  Microsoft-defined push-button identifiers for commonly used buttons
!
      INTEGER, PARAMETER :: IDOK             =   1
      INTEGER, PARAMETER :: IDCANCEL         =   2
      INTEGER, PARAMETER :: IDABORT          =   3
      INTEGER, PARAMETER :: IDRETRY          =   4
      INTEGER, PARAMETER :: IDIGNORE         =   5
      INTEGER, PARAMETER :: IDYES            =   6
      INTEGER, PARAMETER :: IDNO             =   7
      INTEGER, PARAMETER :: IDCLOSE          =   8
      INTEGER, PARAMETER :: IDHELP           =   9
!
! ***************************************************************************
!
!  Group MH : Menu Management
!
!  Subroutine  : WMenuGetState
!  Description : Menu option properties
!
      INTEGER, PARAMETER :: ItemEnabled      = 1
      INTEGER, PARAMETER :: ItemChecked      = 2
!
! ***************************************************************************
!
!  Group DM(1) : General Dialog Management
!
!  Subroutine  : WDialogShow
!  Description : Dialog types
!
      INTEGER, PARAMETER :: Modal            = 1
      INTEGER, PARAMETER :: Modeless         = 2
      INTEGER, PARAMETER :: SemiModeless     = 3
!
!  Subroutine  : WDialogFieldState
!  Description : State for fields not defined elsewhere
!
      INTEGER, PARAMETER :: DialogReadOnly   = 2
      INTEGER, PARAMETER :: DialogHidden     = 3
!
! ***************************************************************************
!
!  Group DM(2) : Assign/Retrieve Field Contents
!
!  Subroutine  : WDialogPutProgressBar
!  Description : Progress bar value assignment method
!
      INTEGER, PARAMETER :: Absolute         = 0
      INTEGER, PARAMETER :: Relative         = 1
!
! ***************************************************************************
!
!  Group CD : Common Dialog Management
!
!  Subroutine  : WHardcopyOptions
!  Description : Hardcopy units
!
      INTEGER, PARAMETER :: HcPoints         =  0
      INTEGER, PARAMETER :: HcInches         =  1
      INTEGER, PARAMETER :: HcCentimetres    =  2
      INTEGER, PARAMETER :: HcMillimetres    =  3
      INTEGER, PARAMETER :: HcControlPanel   =  4
!
!  Subroutine  : WMessageBox
!  Description : Message box display options
!
      INTEGER, PARAMETER :: OKOnly           = 0
      INTEGER, PARAMETER :: OKCancel         = 1
      INTEGER, PARAMETER :: RetryCancel      = 2
      INTEGER, PARAMETER :: YesNo            = 3
      INTEGER, PARAMETER :: YesNoCancel      = 4
      INTEGER, PARAMETER :: RetryAbortIgnore = 5
!
      INTEGER, PARAMETER :: NoIcon           = 0
      INTEGER, PARAMETER :: StopIcon         = 1
      INTEGER, PARAMETER :: QuestionIcon     = 2
      INTEGER, PARAMETER :: ExclamationIcon  = 3
      INTEGER, PARAMETER :: InformationIcon  = 4
!
!  Subroutine  : WSelectFile / WSelectDir
!  Description : Load/save dialog box options
!                Directory selection
!
      INTEGER, PARAMETER :: LoadDialog       =  0
      INTEGER, PARAMETER :: SaveDialog       =  1
      INTEGER, PARAMETER :: PromptOn         =  2
      INTEGER, PARAMETER :: NonExPath        =  4
      INTEGER, PARAMETER :: DirChange        =  8
      INTEGER, PARAMETER :: MultiFile        = 16
      INTEGER, PARAMETER :: AppendExt        = 32
      INTEGER, PARAMETER :: DirCreate        = 64
!
! ***************************************************************************
!
!  Group GG : General Graphics
!
!  Subroutine  : IGrSelect/WglSelect
!  Description : Target drawables
!
      INTEGER, PARAMETER :: DrawWin          =  1
      INTEGER, PARAMETER :: DrawBitmap       =  2
      INTEGER, PARAMETER :: DrawField        =  3
!
! ***************************************************************************
!
!  Group GS : Graphics Style Selection.
!
!  Subroutine  : IGrFillPattern
!  Description : Fill patterns
!
      INTEGER, PARAMETER :: Outline          =  0
      INTEGER, PARAMETER :: Hatched          =  1
      INTEGER, PARAMETER :: HatchedNoOut     = -1
      INTEGER, PARAMETER :: CrossHatch       =  2
      INTEGER, PARAMETER :: CrossHatchNoOut  = -2
      INTEGER, PARAMETER :: MixedColour      =  3
      INTEGER, PARAMETER :: Solid            =  4
!
      INTEGER, PARAMETER :: Sparse           = 1
      INTEGER, PARAMETER :: Medium           = 2
      INTEGER, PARAMETER :: Dense1           = 3
      INTEGER, PARAMETER :: Dense2           = 4
      INTEGER, PARAMETER :: Dense3           = 5
      INTEGER, PARAMETER :: ArcToCentre      = 1
      INTEGER, PARAMETER :: NoArcCentre      = 3
!
      INTEGER, PARAMETER :: DiagUp           = 1
      INTEGER, PARAMETER :: DiagDown         = 2
      INTEGER, PARAMETER :: FillHoriz        = 3
      INTEGER, PARAMETER :: FillVertic       = 4
!
!  Subroutine  : IGrLineType
!  Description : Line types
!
      INTEGER, PARAMETER :: SolidLine        = 0
      INTEGER, PARAMETER :: Dotted           = 1
      INTEGER, PARAMETER :: Dashed           = 2
      INTEGER, PARAMETER :: DotDash          = 3
      INTEGER, PARAMETER :: DotDotDash       = 4
      INTEGER, PARAMETER :: LongShort        = 5
      INTEGER, PARAMETER :: ShortDash        = 6
!
! ***************************************************************************
!
!  Group GC : Graphics Character Output.
!
!  Subroutine  : IGrCharFont
!  Description : Font numbers
!
      INTEGER, PARAMETER :: Helvetica        = 1
      INTEGER, PARAMETER :: HelveticaIt      = 2
      INTEGER, PARAMETER :: HelveticaBd      = 3
      INTEGER, PARAMETER :: HelveticaBdIt    = 4
      INTEGER, PARAMETER :: TimesRoman       = 5
      INTEGER, PARAMETER :: TimesRomanIt     = 6
      INTEGER, PARAMETER :: TimesRomanBd     = 7
      INTEGER, PARAMETER :: TimesRomanBdIt   = 8
      INTEGER, PARAMETER :: Courier          = 1
      INTEGER, PARAMETER :: CourierIt        = 2
      INTEGER, PARAMETER :: CourierBd        = 3
      INTEGER, PARAMETER :: CourierBdIt      = 4
!
! ***************************************************************************
!
!  Group GH : Graphics Hardcopy
!
!  Subroutine  : IGrHardCopyOptions
!  Description : Hardcopy options
!
      INTEGER, PARAMETER :: ImageWidth       =  1
      INTEGER, PARAMETER :: ImageHeight      =  2
      INTEGER, PARAMETER :: HorizPos         =  3
      INTEGER, PARAMETER :: VerticPos        =  4
      INTEGER, PARAMETER :: Orientation      =  5
      INTEGER, PARAMETER :: InvertColours    =  6
      INTEGER, PARAMETER :: UseColour        =  7
      INTEGER, PARAMETER :: NumCopies        =  8
      INTEGER, PARAMETER :: LineWidth        =  9
      INTEGER, PARAMETER :: LineJoin         = 10
      INTEGER, PARAMETER :: LineCap          = 11
      INTEGER, PARAMETER :: FillDensity      = 12
      INTEGER, PARAMETER :: PrintDensity     = 13
      INTEGER, PARAMETER :: HardwareText     = 14
      INTEGER, PARAMETER :: ControlD         = 15
      INTEGER, PARAMETER :: MaxStroke        = 16
      INTEGER, PARAMETER :: CarriageCtrl     = 17
      INTEGER, PARAMETER :: HPGLReplayInfo   = 18
      INTEGER, PARAMETER :: ResetStartEnd    = 19
      INTEGER, PARAMETER :: Transparency     = 20
      INTEGER, PARAMETER :: HPGLLabelTerm    = 21
      INTEGER, PARAMETER :: EPSFile          = 22
      INTEGER, PARAMETER :: ColourPlanes     = 23
      INTEGER, PARAMETER :: Compression      = 24
      INTEGER, PARAMETER :: DriverFill       = 25
      INTEGER, PARAMETER :: ImageFormat      = 26
      INTEGER, PARAMETER :: WMFFormat        = 27
!
      INTEGER, PARAMETER :: TextPrintCols    = 101
      INTEGER, PARAMETER :: TextPrintRows    = 102
      INTEGER, PARAMETER :: TextPrintPage1   = 103
      INTEGER, PARAMETER :: TextPrintPage2   = 104
      INTEGER, PARAMETER :: TextPrintFont    = 105
      INTEGER, PARAMETER :: TextPrintWrap    = 106
      INTEGER, PARAMETER :: TextPrintCC      = 107
!
      INTEGER, PARAMETER :: BwOnly           = 2
      INTEGER, PARAMETER :: MitreJoin        = 0
      INTEGER, PARAMETER :: RoundJoin        = 1
      INTEGER, PARAMETER :: BevelledJoin     = 2
      INTEGER, PARAMETER :: ButtCap          = 0
      INTEGER, PARAMETER :: RoundCap         = 1
      INTEGER, PARAMETER :: SquareCap        = 2
      INTEGER, PARAMETER :: PCXfile          = 0
      INTEGER, PARAMETER :: BMPuncompr       = 1
      INTEGER, PARAMETER :: BMPrle           = 2
      INTEGER, PARAMETER :: StandardWMF      = 0
      INTEGER, PARAMETER :: AldusWMF         = 1
      INTEGER, PARAMETER :: EnhancedWMF      = 2
!
!  Subroutine  : IGrHardCopySelect
!  Description : Hardcopy drivers
!
      INTEGER, PARAMETER :: HPGLplotter      =  1
      INTEGER, PARAMETER :: PostScript       =  2
      INTEGER, PARAMETER :: AcornDraw        =  3
      INTEGER, PARAMETER :: RasterPrinter    =  4
      INTEGER, PARAMETER :: Tek4014LN03      =  5
      INTEGER, PARAMETER :: RasterImage      =  6
      INTEGER, PARAMETER :: LotusPIC         =  7
      INTEGER, PARAMETER :: AutoCadDXF       =  8
      INTEGER, PARAMETER :: CGMetafile       =  9
      INTEGER, PARAMETER :: WinPrintMgr      = 10
      INTEGER, PARAMETER :: WinMetafile      = 11
      INTEGER, PARAMETER :: HPGL2            = 12
!
! ***************************************************************************
!
!  Group GF : Graphics Frile Import
!
!  Subroutine  : IGrFileInfo
!  Description : Information about graphics files
!
      INTEGER, PARAMETER :: FileNotFound     = -1
      INTEGER, PARAMETER :: TypeUnknown      =  0
      INTEGER, PARAMETER :: TypeBMP          =  1
      INTEGER, PARAMETER :: TypePCX          =  2
      INTEGER, PARAMETER :: TypeWMF          =  3
      INTEGER, PARAMETER :: TypeHPGL         =  4
      INTEGER, PARAMETER :: TypeHPGL2        =  5
      INTEGER, PARAMETER :: TypeCGM          =  6
      INTEGER, PARAMETER :: TypeLotusPIC     =  7
      INTEGER, PARAMETER :: TypeAcornDraw    =  8
      INTEGER, PARAMETER :: TypeTek4014      =  9
      INTEGER, PARAMETER :: TypePostScript   = 10
      INTEGER, PARAMETER :: TypeHPPCL        = 11
      INTEGER, PARAMETER :: TypeESCP2        = 12
      INTEGER, PARAMETER :: TypeESCP         = 13
      INTEGER, PARAMETER :: TypeDXF          = 14
!
!  Subroutine  : IGrReplayOptions
!  Description : Graphics replay option numbers
!
      INTEGER, PARAMETER :: ReplayPause      =  1
      INTEGER, PARAMETER :: ReplayClear      =  2
      INTEGER, PARAMETER :: ReplayStart      =  3
      INTEGER, PARAMETER :: ReplayEnd        =  4
!
! ***************************************************************************
!
!  Group GP : Graphics Proximity Checks
!
!  Subroutine  : IGrDistanceLine
!  Description : Distance to line method
!
      INTEGER, PARAMETER :: DistanceDirect   = 0
      INTEGER, PARAMETER :: DistanceExtended = 1
!
!  Subroutine  : IGrIntersectLine
!  Description : Line intersection status codes
!
      INTEGER, PARAMETER :: ParaCollinear    = 0
      INTEGER, PARAMETER :: ParallelLines    = 1
      INTEGER, PARAMETER :: IntersectBeyond  = 2
      INTEGER, PARAMETER :: IntersectOn1     = 3
      INTEGER, PARAMETER :: IntersectOn2     = 4
      INTEGER, PARAMETER :: IntersectOnBoth  = 5
!
! ***************************************************************************
!
!  Group PG1 : Housekeeping and Options.
!
!  Subroutine  : IPgStyle
!  Description : Presentation graphics point marker styles
!
      INTEGER, PARAMETER :: PgNone           = 0
      INTEGER, PARAMETER :: PgDot            = 0
      INTEGER, PARAMETER :: PgDigit          = 1
      INTEGER, PARAMETER :: PgLetter         = 2
      INTEGER, PARAMETER :: PgMarker         = 3
      INTEGER, PARAMETER :: PgSymbol         = 4
      INTEGER, PARAMETER :: PgNumbStr        = 5
      INTEGER, PARAMETER :: PgUserStr        = 6
!
! ***************************************************************************
!
!  Group IF : Information
!
!  Subroutine  : InfoError
!  Description : Last error
!                (INTERACTER compatible error codes <1000)
!                (Winteracter specific  error codes >1000)
!
      INTEGER, PARAMETER :: LastError        =  1
      INTEGER, PARAMETER :: IOErrorCode      =  2
      INTEGER, PARAMETER :: OsErrorCode      =  3
!
      INTEGER, PARAMETER :: ErrFileOpen      =  1
      INTEGER, PARAMETER :: ErrFileIO        =  2
      INTEGER, PARAMETER :: ErrFileClose     =  3
      INTEGER, PARAMETER :: ErrLargeNum      =  4
      INTEGER, PARAMETER :: ErrPrintGfx      =  5
      INTEGER, PARAMETER :: ErrNoSubstring   = 10
      INTEGER, PARAMETER :: ErrDecPoint      = 11
      INTEGER, PARAMETER :: ErrBadChar       = 12
      INTEGER, PARAMETER :: ErrOSCommand     = 13
      INTEGER, PARAMETER :: ErrBadUnits      = 16
      INTEGER, PARAMETER :: ErrNumToStr      = 18
      INTEGER, PARAMETER :: ErrBadRadius     = 20
      INTEGER, PARAMETER :: ErrBadSideLen    = 21
      INTEGER, PARAMETER :: ErrNoWidHgt      = 22
      INTEGER, PARAMETER :: ErrNoProgName    = 23
      INTEGER, PARAMETER :: ErrCumPlotSize   = 24
      INTEGER, PARAMETER :: ErrChrSymSetSize = 25
      INTEGER, PARAMETER :: ErrBadScreenFile = 27
      INTEGER, PARAMETER :: ErrBordersCross  = 28
      INTEGER, PARAMETER :: ErrPageSize      = 34
      INTEGER, PARAMETER :: ErrBadColour     = 42
      INTEGER, PARAMETER :: ErrContHeights   = 43
      INTEGER, PARAMETER :: ErrBadArea       = 44
      INTEGER, PARAMETER :: ErrNoDriver      = 46
      INTEGER, PARAMETER :: ErrFillComplex   = 49
      INTEGER, PARAMETER :: ErrSearchBox     = 51
      INTEGER, PARAMETER :: ErrNoSoftFont    = 53
      INTEGER, PARAMETER :: ErrDrivDevNum    = 54
      INTEGER, PARAMETER :: ErrSameNames     = 55
      INTEGER, PARAMETER :: ErrBadMaxMin     = 58
      INTEGER, PARAMETER :: ErrNoWinPrinter  = 63
      INTEGER, PARAMETER :: ErrImageDump     = 64
!
      INTEGER, PARAMETER :: ErrMenuItem      = 1001
      INTEGER, PARAMETER :: ErrLoadMenu      = 1002
      INTEGER, PARAMETER :: ErrWinHandle     = 1003
      INTEGER, PARAMETER :: ErrCommonDlg     = 1004
      INTEGER, PARAMETER :: ErrCurDialog     = 1005
      INTEGER, PARAMETER :: ErrFieldNum      = 1006
      INTEGER, PARAMETER :: ErrLoadDialog    = 1007
      INTEGER, PARAMETER :: ErrSelDialog     = 1008
      INTEGER, PARAMETER :: ErrOutOfRange    = 1009
      INTEGER, PARAMETER :: ErrFieldUndefined= 1010
      INTEGER, PARAMETER :: ErrOptionNum     = 1011
      INTEGER, PARAMETER :: ErrTabNum        = 1012
      INTEGER, PARAMETER :: ErrProgressRange = 1013
      INTEGER, PARAMETER :: ErrDialogType    = 1014
      INTEGER, PARAMETER :: ErrImageNum      = 1015
      INTEGER, PARAMETER :: ErrRootHidden    = 1016
      INTEGER, PARAMETER :: ErrBadCell       = 1017
      INTEGER, PARAMETER :: ErrGridSize      = 1018
      INTEGER, PARAMETER :: ErrBadTarget     = 1019
      INTEGER, PARAMETER :: ErrBitmapCreate  = 1020
      INTEGER, PARAMETER :: ErrBitmapHandle  = 1021
      INTEGER, PARAMETER :: ErrNotChildWin   = 1022
      INTEGER, PARAMETER :: ErrBufferSize    = 1023
      INTEGER, PARAMETER :: ErrEditorMode    = 1024
      INTEGER, PARAMETER :: ErrEditMenu      = 1025
      INTEGER, PARAMETER :: ErrModalMenu     = 1026
      INTEGER, PARAMETER :: ErrEditInit      = 1027
      INTEGER, PARAMETER :: ErrNotEditor     = 1028
      INTEGER, PARAMETER :: ErrTrackbarRange = 1029
      INTEGER, PARAMETER :: ErrHelpStart     = 1030
      INTEGER, PARAMETER :: ErrHelpName      = 1031
      INTEGER, PARAMETER :: ErrNoCommandLine = 1032
      INTEGER, PARAMETER :: ErrModalCommand  = 1033
      INTEGER, PARAMETER :: ErrCommandTimeOut= 1034
      INTEGER, PARAMETER :: ErrOpenGLInit    = 1035
      INTEGER, PARAMETER :: ErrBitmapDestroy = 1036
      INTEGER, PARAMETER :: ErrBitmapInUse   = 1037
      INTEGER, PARAMETER :: ErrWindowInUse   = 1038
      INTEGER, PARAMETER :: ErrViewerMode    = 1039
      INTEGER, PARAMETER :: ErrViewerInit    = 1040
!
!  Subroutine  : InfoFilename
!  Description : Default filenames
!
      INTEGER, PARAMETER :: DefaultHardCopy  =  2
      INTEGER, PARAMETER :: DefaultCharSet   =  3
      INTEGER, PARAMETER :: CharSetDir       =  5
      INTEGER, PARAMETER :: WinSysDir        =  8
      INTEGER, PARAMETER :: CurrentCharSet   =  9
!
!  Subroutine  : InfoGraphics
!  Description : Graphics information
!
      INTEGER, PARAMETER :: GraphicsX        =  1
      INTEGER, PARAMETER :: GraphicsY        =  2
      INTEGER, PARAMETER :: GraphicsChWidth  =  3
      INTEGER, PARAMETER :: GraphicsChHeight =  4
      INTEGER, PARAMETER :: GraphicsAreaMinX =  7
      INTEGER, PARAMETER :: GraphicsAreaMinY =  8
      INTEGER, PARAMETER :: GraphicsAreaMaxX =  9
      INTEGER, PARAMETER :: GraphicsAreaMaxY = 10
      INTEGER, PARAMETER :: GraphicsUnitMinX = 11
      INTEGER, PARAMETER :: GraphicsUnitMinY = 12
      INTEGER, PARAMETER :: GraphicsUnitMaxX = 13
      INTEGER, PARAMETER :: GraphicsUnitMaxY = 14
!
!  Subroutine  : InfoGrHardCopy
!  Description : Graphics hardcopy information
!
!  Note : 6-32, 38 and 40-42 same as InfoGrScreen
!  Note : 101-127 as for IGrHardCopyOptions(1-27,n) + 100
!
      INTEGER, PARAMETER :: PaperWidth       = 91
      INTEGER, PARAMETER :: PaperHeight      = 92
      INTEGER, PARAMETER :: DriverSelected   =100
!
!  Subroutine  : InfoGrScreen
!  Description : Screen graphics capabilities
!
      INTEGER, PARAMETER :: GraphicsAvail    =  1
      INTEGER, PARAMETER :: AreaClear        =  2
      INTEGER, PARAMETER :: AndPlotMode      =  3
      INTEGER, PARAMETER :: OrPlotMode       =  4
      INTEGER, PARAMETER :: EorPlotMode      =  5
      INTEGER, PARAMETER :: BrokenLines      =  6
      INTEGER, PARAMETER :: RGBEncoding      =  7
      INTEGER, PARAMETER :: PaletteBack      =  8
      INTEGER, PARAMETER :: PaletteFore      =  9
      INTEGER, PARAMETER :: StippleFillRect  = 10
      INTEGER, PARAMETER :: StippleFillTri   = 11
      INTEGER, PARAMETER :: StippleFillCirc  = 12
      INTEGER, PARAMETER :: SolidFillRect    = 13
      INTEGER, PARAMETER :: SolidFillTri     = 14
      INTEGER, PARAMETER :: SolidFillCirc    = 15
      INTEGER, PARAMETER :: BitBlock         = 16
      INTEGER, PARAMETER :: ImageSave        = 17
      INTEGER, PARAMETER :: GrPopUpMode      = 18
      INTEGER, PARAMETER :: HardTextSize     = 19
      INTEGER, PARAMETER :: IndepTextGr      = 20
      INTEGER, PARAMETER :: GINMode          = 21
      INTEGER, PARAMETER :: StippleFillArc   = 22
      INTEGER, PARAMETER :: SolidFillArc     = 23
      INTEGER, PARAMETER :: PaletteChange    = 24
      INTEGER, PARAMETER :: StippleFillPoly  = 25
      INTEGER, PARAMETER :: SolidFillPoly    = 26
      INTEGER, PARAMETER :: QualityFonts     = 27
      INTEGER, PARAMETER :: OutlineArc       = 28
      INTEGER, PARAMETER :: ReadPixel        = 29
      INTEGER, PARAMETER :: ColNumAvailable  = 30
      INTEGER, PARAMETER :: ColNumMax        = 31
      INTEGER, PARAMETER :: AspectRatio      = 32
      INTEGER, PARAMETER :: TextHandle       = 33
      INTEGER, PARAMETER :: PrevColReq       = 34
      INTEGER, PARAMETER :: ColourReq        = 35
      INTEGER, PARAMETER :: LineTypeReq      = 36
      INTEGER, PARAMETER :: PlotModeReq      = 37
      INTEGER, PARAMETER :: CharSetTypeHard  = 38
      INTEGER, PARAMETER :: CharSetTypeSoft  = 39
      INTEGER, PARAMETER :: HardTextRotate   = 40
      INTEGER, PARAMETER :: WideLines        = 41
      INTEGER, PARAMETER :: Col24Bits        = 42
!
      INTEGER, PARAMETER :: PlotNormal       =  0
      INTEGER, PARAMETER :: PlotOr           =  1
      INTEGER, PARAMETER :: PlotAnd          =  2
      INTEGER, PARAMETER :: PlotEor          =  3
!
!  Subroutine  : InfoHardware
!  Description : Printer/plotter information
!
      INTEGER, PARAMETER :: DensityNum       =  2
      INTEGER, PARAMETER :: PrintDense1      =  3
      INTEGER, PARAMETER :: PrintDense2      =  4
      INTEGER, PARAMETER :: PrintDense3      =  5
      INTEGER, PARAMETER :: PrintDense4      =  6
      INTEGER, PARAMETER :: PlotPens         =  7
      INTEGER, PARAMETER :: PrinterType      = 11
      INTEGER, PARAMETER :: PlotterType      = 12
      INTEGER, PARAMETER :: PrinterExp       = 21
      INTEGER, PARAMETER :: PlotterExp       = 22
      INTEGER, PARAMETER :: DataComp         = 31
      INTEGER, PARAMETER :: WinPrinter       = 33
!
!  Subroutine  : InfoOpSystem
!  Description : Operating System Information
!
      INTEGER, PARAMETER :: OSType           =  1
      INTEGER, PARAMETER :: NumCLArgs        =  2
      INTEGER, PARAMETER :: OSVersion        =  3
      INTEGER, PARAMETER :: MatchManyChar    =  4
      INTEGER, PARAMETER :: MatchSingleChar  =  5
      INTEGER, PARAMETER :: DirSepChar       =  6
      INTEGER, PARAMETER :: FileCaseSense    =  7
      INTEGER, PARAMETER :: FileSuffix       =  8
      INTEGER, PARAMETER :: CurDirIsRoot     =  9
      INTEGER, PARAMETER :: SystemTime       = 15
      INTEGER, PARAMETER :: WindowsVersion   = 16
      INTEGER, PARAMETER :: FreeDiskSpace    = 17
      INTEGER, PARAMETER :: ProcessorType    = 18
!
      INTEGER, PARAMETER :: Win32            =  6
!
      INTEGER, PARAMETER :: Win32s           =  2
      INTEGER, PARAMETER :: Windows95        =  3
      INTEGER, PARAMETER :: WindowsNT        =  4
!
      INTEGER, PARAMETER :: CPUx86           =  1
      INTEGER, PARAMETER :: CPUaxp           =  2
!
      INTEGER, PARAMETER :: DriveUnknown     =  0
      INTEGER, PARAMETER :: DriveRemovable   =  1
      INTEGER, PARAMETER :: DriveFixed       =  2
      INTEGER, PARAMETER :: DriveNetwork     =  3
      INTEGER, PARAMETER :: DriveCDROM       =  4
!
!  Subroutine  : InfoVersion
!  Description : Library version numbers
!
      INTEGER, PARAMETER :: IntVersion       =  1
      INTEGER, PARAMETER :: WintVersion      =  2
!
!  Subroutine  : WInfoBitmap
!  Description : Memory bitmap information
!
      INTEGER, PARAMETER :: BitmapWidth      =  1
      INTEGER, PARAMETER :: BitmapHeight     =  2
      INTEGER, PARAMETER :: BitmapHandle     =  3
      INTEGER, PARAMETER :: BitmapXPos       =  4
      INTEGER, PARAMETER :: BitmapYPos       =  5
!
!  Subroutine  : WInfoDialog
!  Description : Dialog field and exit information
!
      INTEGER, PARAMETER :: ExitButton       =  1
      INTEGER, PARAMETER :: ExitField        =  2
      INTEGER, PARAMETER :: CurrentDialog    =  3
      INTEGER, PARAMETER :: ExitButtonCommon =  4
      INTEGER, PARAMETER :: ExitDialog       =  5
      INTEGER, PARAMETER :: DialogXPos       =  6
      INTEGER, PARAMETER :: DialogYPos       =  7
      INTEGER, PARAMETER :: DialogWidth      =  8
      INTEGER, PARAMETER :: DialogHeight     =  9
!
      INTEGER, PARAMETER :: CommonCancel     =  0
      INTEGER, PARAMETER :: CommonIgnore     =  0
      INTEGER, PARAMETER :: CommonOK         =  1
      INTEGER, PARAMETER :: CommonOpen       =  1
      INTEGER, PARAMETER :: CommonSave       =  1
      INTEGER, PARAMETER :: CommonYes        =  1
      INTEGER, PARAMETER :: CommonRetry      =  1
      INTEGER, PARAMETER :: CommonAbort      =  2
      INTEGER, PARAMETER :: CommonNo         =  2
!
!  Subroutine  : WInfoDialogField
!  Description : Information about an individual Dialog field
!
      INTEGER, PARAMETER :: FieldXPos        =  1
      INTEGER, PARAMETER :: FieldYPos        =  2
      INTEGER, PARAMETER :: FieldWidth       =  3
      INTEGER, PARAMETER :: FieldHeight      =  4
      INTEGER, PARAMETER :: FieldType        =  5
      INTEGER, PARAMETER :: FieldState       =  6
      INTEGER, PARAMETER :: FieldTrackMin    =  7
      INTEGER, PARAMETER :: FieldTrackMax    =  8
!
      INTEGER, PARAMETER :: FieldTypeUnknown =  0
      INTEGER, PARAMETER :: FieldTypeLabel   =  1
      INTEGER, PARAMETER :: FieldTypePicture =  2
      INTEGER, PARAMETER :: FieldTypeGroup   =  3
      INTEGER, PARAMETER :: FieldTypeString  =  4
      INTEGER, PARAMETER :: FieldTypeInteger =  5
      INTEGER, PARAMETER :: FieldTypeReal    =  6
      INTEGER, PARAMETER :: FieldTypeDouble  =  7
      INTEGER, PARAMETER :: FieldTypeButton  =  8
      INTEGER, PARAMETER :: FieldTypeCheck   =  9
      INTEGER, PARAMETER :: FieldTypeRadio   = 10
      INTEGER, PARAMETER :: FieldTypeCombo   = 11
      INTEGER, PARAMETER :: FieldTypeList    = 12
      INTEGER, PARAMETER :: FieldTypeTab     = 13
      INTEGER, PARAMETER :: FieldTypeProgress= 14
      INTEGER, PARAMETER :: FieldTypeGrid    = 15
      INTEGER, PARAMETER :: FieldTypeTrackbar= 16
!
!  Subroutine  : WInfoEditor
!  Description : Integer information about Text Editor windows.
!
      INTEGER, PARAMETER :: EditFileSaved    = 1
      INTEGER, PARAMETER :: EditTextLength   = 2
      INTEGER, PARAMETER :: EditFileChanged  = 3
      INTEGER, PARAMETER :: EditSelStart     = 4
      INTEGER, PARAMETER :: EditSelEnd       = 5
!
!  Subroutine  : WInfoEditorString
!  Description : Character information about Text Editor windows.
!
      INTEGER, PARAMETER :: EditFindString    = 1
      INTEGER, PARAMETER :: EditReplaceString = 2
      INTEGER, PARAMETER :: EditFileName      = 3
!
!  Subroutine  : WInfoFont
!  Description : Font info
!
      INTEGER, PARAMETER :: FontXPos         =  1
      INTEGER, PARAMETER :: FontYPos         =  2
      INTEGER, PARAMETER :: FontBold         =  3
      INTEGER, PARAMETER :: FontItalic       =  4
      INTEGER, PARAMETER :: FontUnderline    =  5
      INTEGER, PARAMETER :: FontForeCol      =  6
      INTEGER, PARAMETER :: FontBackCol      =  7
      INTEGER, PARAMETER :: FontStyleNum     =  8
      INTEGER, PARAMETER :: FontWidth        =  9
      INTEGER, PARAMETER :: FontHeight       = 10
!
!  Subroutine  : WInfoGrid
!  Description : Grid field info
!
      INTEGER, PARAMETER :: GridColumns      =  1
      INTEGER, PARAMETER :: GridRowsMax      =  2
      INTEGER, PARAMETER :: GridRowsCur      =  3
      INTEGER, PARAMETER :: GridCol          =  4
      INTEGER, PARAMETER :: GridRow          =  5
!
!  Subroutine  : WInfoGridCell
!  Description : Grid cell info
!
      INTEGER, PARAMETER :: GridCellDefined  =  1
      INTEGER, PARAMETER :: GridCellType     =  2
!
!  Subroutine  : WInfoScreen
!  Description : Screen info
!
      INTEGER, PARAMETER :: ScreenWidth      = 1
      INTEGER, PARAMETER :: ScreenHeight     = 2
      INTEGER, PARAMETER :: ScreenColours    = 3
!
!  Subroutine  : WInfoWindow
!  Description : Window info
!
      INTEGER, PARAMETER :: WindowWidth      = 1
      INTEGER, PARAMETER :: WindowHeight     = 2
      INTEGER, PARAMETER :: OpenFlags        = 3
      INTEGER, PARAMETER :: WindowHandle     = 4
      INTEGER, PARAMETER :: WindowXPos       = 5
      INTEGER, PARAMETER :: WindowYPos       = 6
      INTEGER, PARAMETER :: ClientXPos       = 7
      INTEGER, PARAMETER :: ClientYPos       = 8
      INTEGER, PARAMETER :: WindowState      = 9
      INTEGER, PARAMETER :: WindowIsEditor   = 10
!
      INTEGER, PARAMETER :: WinMinimised     = 0
      INTEGER, PARAMETER :: WinNormal        = 1
      INTEGER, PARAMETER :: WinMaximised     = 2
!
! ***************************************************************************
!
!  Group OS : Operating System interface
!
!  Subroutine  : IOsCommand
!  Description : Execute an operating system command or a program
!
      INTEGER, PARAMETER :: ProcSilent       =  1
      INTEGER, PARAMETER :: ProcBlocked      =  2
!
! ***************************************************************************
!
!  Group BF : Binary File Handling
!
!  Subroutine  : IFileOpen
!  Description : Open modes
!
      INTEGER, PARAMETER :: ReadOnly         =  0
      INTEGER, PARAMETER :: WriteOnly        =  1
      INTEGER, PARAMETER :: ReadWrite        =  2
!
!  Subroutine  : IFileSeek
!  Description : Seek methods
!
      INTEGER, PARAMETER :: FromStart        =  0
      INTEGER, PARAMETER :: FromCurrent      =  1
      INTEGER, PARAMETER :: FromEnd          =  2
!
! ***************************************************************************
!
!  Group DG : Debugging
!
!  Subroutine  : IDebugLevel
!  Description : Error reporting level
!
      INTEGER, PARAMETER :: DbgSilent        = 0
      INTEGER, PARAMETER :: DbgStdOut        = 1
      INTEGER, PARAMETER :: DbgFile          = 2
      INTEGER, PARAMETER :: DbgStatBar       = 3
      INTEGER, PARAMETER :: DbgMsgBox        = 4
!
! ***************************************************************************
!
!  Group MI : Miscellaneous
!
!  Subroutine  : IPlotter
!  Description : Plotter types
!
      INTEGER, PARAMETER :: HPGLgeneric      =  1
      INTEGER, PARAMETER :: HP7475aA4        =  2
      INTEGER, PARAMETER :: HP7475aA3        =  3
      INTEGER, PARAMETER :: EpsonHI80HP      =  4
      INTEGER, PARAMETER :: Facit4550A4      =  5
      INTEGER, PARAMETER :: HP7440aA4        =  6
      INTEGER, PARAMETER :: HP7550aA4        =  7
      INTEGER, PARAMETER :: HP7550aA3        =  8
      INTEGER, PARAMETER :: Facit4551A4      =  9
      INTEGER, PARAMETER :: Facit4551A3      = 10
      INTEGER, PARAMETER :: HPLJetHPGL       = 11
      INTEGER, PARAMETER :: HPDJetHPGL       = 12
      INTEGER, PARAMETER :: HPLJ4HPGL        = 13
      INTEGER, PARAMETER :: DesignJet        = 14
!
!  Subroutine  : IPrinter
!  Description : Printer types
!
      INTEGER, PARAMETER :: GenericText      =  1
      INTEGER, PARAMETER :: IBMGfx80         =  2
      INTEGER, PARAMETER :: Panasonic1081    =  3
      INTEGER, PARAMETER :: AmstradDMP3000   =  4
      INTEGER, PARAMETER :: EpsonFX80        =  5
      INTEGER, PARAMETER :: EpsonFXwide      =  6
      INTEGER, PARAMETER :: EpsonMX80        =  7
      INTEGER, PARAMETER :: EpsonMXwide      =  8
      INTEGER, PARAMETER :: ShinwaCP80       =  9
      INTEGER, PARAMETER :: HPLJetPlus       = 10
      INTEGER, PARAMETER :: HPLJetII         = 11
      INTEGER, PARAMETER :: IBMGfxWide       = 12
      INTEGER, PARAMETER :: HPPJet           = 13
      INTEGER, PARAMETER :: HPDJet           = 14
      INTEGER, PARAMETER :: IBMPro80         = 15
      INTEGER, PARAMETER :: IBMProWide       = 16
      INTEGER, PARAMETER :: EpsonLQ80        = 17
      INTEGER, PARAMETER :: EpsonLQwide      = 18
      INTEGER, PARAMETER :: IBMX2480         = 19
      INTEGER, PARAMETER :: IBMX24wide       = 20
      INTEGER, PARAMETER :: HPDJet500C       = 21
      INTEGER, PARAMETER :: HPDJet500        = 22
      INTEGER, PARAMETER :: EpsonJX80        = 23
      INTEGER, PARAMETER :: EpsonJXwide      = 24
      INTEGER, PARAMETER :: EpsonLQCol80     = 25
      INTEGER, PARAMETER :: EpsonLQColwide   = 26
      INTEGER, PARAMETER :: DECLN03          = 27
      INTEGER, PARAMETER :: HPLJetIII        = 28
      INTEGER, PARAMETER :: HPLJet4          = 29
      INTEGER, PARAMETER :: HPPJetXL300      = 30
      INTEGER, PARAMETER :: HPDJet550C       = 31
      INTEGER, PARAMETER :: HPDJet1200C      = 32
      INTEGER, PARAMETER :: HPDJet540        = 33
      INTEGER, PARAMETER :: HPDJet540C       = 34
      INTEGER, PARAMETER :: HPCLJet          = 35
      INTEGER, PARAMETER :: EpsonStylus      = 36
      INTEGER, PARAMETER :: EpsonStylusCol   = 37
!
!  Subroutine  : WCursorShape
!  Description : Mouse cursor shape
!
      INTEGER, PARAMETER :: CurArrow         =  0
      INTEGER, PARAMETER :: CurHourGlass     =  1
      INTEGER, PARAMETER :: CurSmallHour     =  2
      INTEGER, PARAMETER :: CurCrossHair     =  3
      INTEGER, PARAMETER :: CurIbeam         =  4
      INTEGER, PARAMETER :: CurCircle        =  5
      INTEGER, PARAMETER :: CurFourPoint     =  6
      INTEGER, PARAMETER :: CurDoubleNS      =  7
      INTEGER, PARAMETER :: CurDoubleEW      =  8
      INTEGER, PARAMETER :: CurDoubleNESW    =  9
      INTEGER, PARAMETER :: CurDoubleNWSE    = 10
      INTEGER, PARAMETER :: CurVertical      = 11
!
!  Subroutine  : WglSelect
!  Description : Type of OpenGL support
!
      INTEGER, PARAMETER :: wglColourIndex   =  1
      INTEGER, PARAMETER :: wglDoubleBuffer  =  2
!
!  Subroutine  : WHelpFile
!  Description : Type of help window
!
      INTEGER, PARAMETER :: HelpWindow       =  0
      INTEGER, PARAMETER :: HelpPopup        =  1
!
!  Subroutine  : WPlayMovie
!  Description : Playback flags
!
      INTEGER, PARAMETER :: MovieFullScreen  =  1
      INTEGER, PARAMETER :: MovieASync       =  2
!
!  Subroutine  : WPlaySound
!  Description : Playback flags
!
      INTEGER, PARAMETER :: PlaySync         =  1
!
END MODULE WINTERACTER
