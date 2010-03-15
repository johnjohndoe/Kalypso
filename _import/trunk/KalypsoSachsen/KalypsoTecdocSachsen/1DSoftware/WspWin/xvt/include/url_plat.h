
/*
	Copyright 1989-1995 XVT Software Inc. All rights reserved.
	May be used freely by licensed and registered users of XVT.
	May be distributed in source form only when embedded in an XVT
	user's application.
*/

/* Sgetstr resources for win */
#include "winrid.h"
#include "winstr.h"

#ifndef APPNAME
#define APPNAME xvtapp
#endif

#ifndef QAPPNAME
#define QAPPNAME "xvtapp"
#endif

#ifndef LIBDIR
#define LIBDIR .\..\..\lib
#endif

#define DLG_PROMPT    4
#define DLG_RESPONSE  5
#define DLG_ICON     9001


/********************************/
/* Windows STANDARD EDIT MENU   */
/********************************/

#ifndef NO_STD_EDIT_MENU

#define DEFAULT_EDIT_MENU submenu M_EDIT STR_M_EDIT

MENU M_EDIT             STR_M_EDIT
	ITEM M_EDIT_UNDO     STR_M_EDIT_UNDO      DISABLED
	SEPARATOR
	ITEM M_EDIT_CUT      STR_M_EDIT_CUT       DISABLED
	ITEM M_EDIT_COPY     STR_M_EDIT_COPY      DISABLED
	ITEM M_EDIT_PASTE    STR_M_EDIT_PASTE     DISABLED
	ITEM M_EDIT_CLEAR    STR_M_EDIT_CLEAR     DISABLED

/* Old accelerators */
ACCEL M_EDIT_UNDO   BACK    ALT
ACCEL M_EDIT_CUT    DEL     SHIFT
ACCEL M_EDIT_COPY   INS     CONTROL
ACCEL M_EDIT_PASTE  INS     SHIFT
ACCEL M_EDIT_CLEAR  DEL     CONTROL
/* New accelerators */
ACCEL M_EDIT_UNDO   STR_M_EDIT_UNDO_ACCEL
ACCEL M_EDIT_CUT    STR_M_EDIT_CUT_ACCEL
ACCEL M_EDIT_COPY   STR_M_EDIT_COPY_ACCEL
ACCEL M_EDIT_PASTE  STR_M_EDIT_PASTE_ACCEL

#endif /* NO_STD_EDIT_MENU */


/********************************/
/* Windows STANDARD FILE MENU        */
/********************************/

#ifndef NO_STD_FILE_MENU

#define DEFAULT_FILE_MENU submenu M_FILE STR_M_FILE

MENU M_FILE             STR_M_FILE
	ITEM M_FILE_NEW      STR_M_FILE_NEW       DISABLED
	ITEM M_FILE_OPEN     STR_M_FILE_OPEN      DISABLED
	ITEM M_FILE_CLOSE    STR_M_FILE_CLOSE     DISABLED
	ITEM M_FILE_SAVE     STR_M_FILE_SAVE      DISABLED
	ITEM M_FILE_SAVE_AS  STR_M_FILE_SAVE_AS   DISABLED
	ITEM M_FILE_REVERT   STR_M_FILE_REVERT    DISABLED
	SEPARATOR
	ITEM M_FILE_PRINT    STR_M_FILE_PRINT     DISABLED
	ITEM M_FILE_PG_SETUP STR_M_FILE_PG_SETUP  DISABLED
	SEPARATOR
	ITEM M_FILE_QUIT     STR_M_FILE_QUIT

#endif /* NO_STD_FILE_MENU */


/********************************/
/* Windows STANDARD FONT MENU        */
/********************************/

#ifndef NO_STD_FONT_MENU

#define DEFAULT_FONT_MENU submenu M_FONT STR_M_FONT

MENU  M_FONT            STR_M_FONT
	ITEM M_FONT_SELECT   STR_M_FONT_SELECT

#endif /* NO_STD_FONT_MENU */


/*************************************/
/* Windows STANDARD HELP MENU        */
/*************************************/

#ifndef NO_STD_HELP_MENU

#define DEFAULT_HELP_MENU submenu M_HELP STR_M_HELP

Menu M_HELP                STR_M_HELP
	Item M_HELP_CONTENTS    STR_M_HELP_CONTENTS
	Item M_HELP_SEARCH      STR_M_HELP_SEARCH
	Item M_HELP_HELPONHELP  STR_M_HELP_HELPONHELP
	Separator
#ifdef XVT_HELP_OBJCLICK   
	Item M_HELP_OBJCLICK    STR_M_HELP_OBJCLICK
	Separator
#endif
	Item M_HELP_VERSION     STR_M_HELP_VERSION

Accel M_HELP_CONTENTS      STR_M_HELP_CONTENTS_ACCEL
Accel M_HELP_SEARCH        STR_M_HELP_SEARCH_ACCEL

#ifdef XVT_HELP_OBJCLICK
Accel M_HELP_OBJCLICK      STR_M_HELP_OBJCLICK_ACCEL
#endif

#endif /* NO_STD_HELP_MENU */


/********************************/
/* Windows DIALOGUES            */
/********************************/

/*
	Standard symbols for dialog and control styles.
*/
#transparent $$$

#define COMMON_DIALOG_STYLE \
   STYLE WS_POPUP | WS_VISIBLE | WS_SYSMENU | WS_CAPTION \
         | WS_CLIPSIBLINGS | WS_CLIPCHILDREN | DS_3DLOOK
#define DIALOG_LMD      DIALOG LOADONCALL MOVEABLE DISCARDABLE
#define STYLE_MODELESS  COMMON_DIALOG_STYLE | WS_BORDER
#define STYLE_MODAL     COMMON_DIALOG_STYLE | DS_MODALFRAME
#define WSGT            WS_GROUP | WS_TABSTOP
#define C_DEFBUTTON_GT  BS_DEFPUSHBUTTON | WS_CHILD | WSGT
#define C_BUTTON_GT     BS_PUSHBUTTON | WS_CHILD | WSGT
#define C_LTEXT         SS_LEFT | WS_CHILD | SS_NOPREFIX
#define C_CTEXT         SS_CENTER | WS_CHILD | SS_NOPREFIX
#define C_RTEXT         SS_RIGHT | WS_CHILD | SS_NOPREFIX
#define C_LEDIT_GT      ES_LEFT | WS_BORDER | WS_CHILD | WSGT
#define C_LBOX_GT       LBS_NOTIFY | WS_BORDER | WS_VSCROLL | WS_CHILD | WSGT
#define C_DSPLBOX_GT    WS_BORDER | WS_VSCROLL | WS_CHILD | WSGT
#define C_CHECK_GT      BS_CHECKBOX | WS_CHILD | WSGT
#define C_RADIO_GT      BS_RADIOBUTTON | WS_CHILD | WSGT
#define C_RADIO         BS_RADIOBUTTON | WS_CHILD
#define C_HSCROLL_GT    SBS_HORZ | WS_CHILD | WSGT
#define C_VSCROLL_GT    SBS_VERT | WS_CHILD | WSGT

/*
	Application icon is required; crosshair cursor is a standard XVT cursor
	shape that's not predefined by Windows
*/
ICON_RSRC       ICON DISCARDABLE    APPNAME.ico
CURSOR_CROSS    CURSOR DISCARDABLE  LIBDIR\croshair.cur
CURSOR_HELP     CURSOR DISCARDABLE  LIBDIR\objhelp.cur

/*
	MDI Windows menu
*/
xvt_WindowMenu MENU
BEGIN
	POPUP STR_XVT_WINDOWMENU
	BEGIN
		MENUITEM STR_IDM_WINDOWTILE,    IDM_WINDOWTILE
		MENUITEM STR_IDM_WINDOWCASCADE, IDM_WINDOWCASCADE
		MENUITEM STR_IDM_WINDOWICONS,   IDM_WINDOWICONS
	END
END

/*
	Dialog box for xvt_dm_post_ask()
*/
DB_ASK DIALOG_LMD 58, 19, 205, 73
STYLE_MODAL
CAPTION QAPPNAME
BEGIN
	CONTROL "", DLG_OK, "button", C_DEFBUTTON_GT, 156, 10, 44, 14
	CONTROL "", DLG_NO, "button", C_BUTTON_GT, 156, 30, 44, 14
	CONTROL "", DLG_CANCEL, "button", C_BUTTON_GT, 156, 50, 44, 14
	CONTROL "", 5, "static", C_LTEXT, 35, 10, 115, 55
	ICON 32514, DLG_ICON, 10, 10, 0, 0
END

/*
	Dialog box for xvt_dm_post_error()
*/
DB_ERROR DIALOG_LMD 58, 19, 205, 73
STYLE_MODAL
CAPTION QAPPNAME
BEGIN
	CONTROL "", DLG_OK, "button", C_DEFBUTTON_GT, 156, 10, 44, 14
	CONTROL "", DLG_CANCEL, "button", C_BUTTON_GT, 156, 30, 44, 14
	CONTROL "", DLG_NO, "button", C_BUTTON_GT, 156, 50, 44, 14
	CONTROL "", 5, "static", C_LTEXT, 35, 10, 115, 55
	ICON 32515, DLG_ICON, 10, 10, 0, 0
END

/*
	Dialog box for xvt_dm_post_note()
*/
DB_NOTE DIALOG_LMD 58, 19, 205, 73
STYLE_MODAL
CAPTION QAPPNAME
BEGIN
	CONTROL "", DLG_OK, "button", C_DEFBUTTON_GT, 156, 10, 44, 14
	CONTROL "", DLG_CANCEL, "button", C_BUTTON_GT, 156, 30, 44, 14
	CONTROL "", DLG_NO, "button", C_BUTTON_GT, 156, 50, 44, 14
	CONTROL "", 5, "static", C_LTEXT, 35, 10, 115, 55
	ICON 32516, DLG_ICON, 10, 10, 0, 0
END

/*
	Dialog box for xvt_dm_post_str_response().
*/
DB_RESPONSE DIALOG_LMD 12, 19, 222, 78
STYLE_MODAL
CAPTION QAPPNAME
BEGIN
	CONTROL "", DLG_RESPONSE, "edit", C_LEDIT_GT|ES_AUTOHSCROLL, 5, 60, 210, 12
	CONTROL STR_DB_RESPONSE_OK, DLG_OK, "button", C_DEFBUTTON_GT, 171, 5, 44, 14
	CONTROL STR_DB_RESPONSE_CANCEL, DLG_CANCEL, "button", C_BUTTON_GT, 171, 25, 44, 14
	CONTROL "", DLG_PROMPT, "static", C_LTEXT, 5, 5, 160, 46
END

/*
	Dialog box for printing cancellation
*/
DB_ABORT DIALOG_LMD 30,40,90, 76
STYLE_MODAL
CAPTION QAPPNAME
BEGIN
	CONTROL STR_DB_ABORT_CANCEL, DLG_CANCEL, "button", C_DEFBUTTON_GT, 23, 56, 44, 14
	CONTROL STR_DB_ABORT_PRINTING, -1, "static", C_CTEXT, 0, 8, 90, 8
	CONTROL "", 4, "static", C_CTEXT, 0, 18, 90, 16
	CONTROL "", 5, "static", C_CTEXT, 0, 40, 90, 8
END

$$$

/********************************/
/* WIN STANDARD ABOUT BOX       */
/********************************/
 
#ifndef NO_STD_ABOUT_BOX
 
DIALOG 9050,   106, 60, 300, 60   "" MODAL
	BUTTON 1,   224, 35, 70, 20   STR_DB_ABOUT_OK       DEFAULT
	BUTTON 2,   224, 5, 70, 20    STR_DB_ABOUT_CANCEL
	TEXT 4,   20, 5, 190, 16      STR_DB_ABOUT_TEXT1
	TEXT 5,   20, 21, 190, 16     STR_DB_ABOUT_TEXT2
	TEXT 6,   20, 37, 190, 16     STR_DB_ABOUT_TEXT3
 
#endif /* NO_STD_ABOUT_BOX */


/*********************/
/* Windows STRINGS   */
/*********************/

STRING STR_APPNAME QAPPNAME
STRING STR_HELPTYPE STR_WIN_HELPTYPE


