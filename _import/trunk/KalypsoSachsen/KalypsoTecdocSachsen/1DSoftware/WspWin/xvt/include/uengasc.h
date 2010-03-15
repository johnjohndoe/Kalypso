/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *  $RCSfile: uengasc.h,v $ 
 *  $Revision: 1.8 $
 *
 *  Purpose: XVT URL standard resource string translation data for English
 *           in ASCII codeset.
 *
 ****************************************************************************/

/****************************************************************************
    Standard Menus Section
 ****************************************************************************/

/*
* Standard File Menu Labels
*/
#if (XVTWS == MACWS)
#define STR_M_FILE                      "File"
#define STR_M_FILE_NEW                  "New"
#define STR_M_FILE_OPEN                 "Open..."
#define STR_M_FILE_CLOSE                "Close"
#define STR_M_FILE_SAVE                 "Save"
#define STR_M_FILE_SAVE_AS              "Save As..."
#define STR_M_FILE_REVERT               "Revert"
#define STR_M_FILE_PG_SETUP             "Page Setup..."
#define STR_M_FILE_PRINT                "Print..."
#define STR_M_FILE_QUIT                 "Quit"
#define STR_M_FILE_NEW_ACCEL            "N" ALT
#define STR_M_FILE_OPEN_ACCEL           "O" ALT
#define STR_M_FILE_CLOSE_ACCEL          "W" ALT
#define STR_M_FILE_SAVE_ACCEL           "S" ALT
#define STR_M_FILE_PRINT_ACCEL          "P" ALT
#define STR_M_FILE_QUIT_ACCEL           "Q" ALT
#elif (XVTWS == MTFWS)
#define STR_M_FILE                      "~File"
#define STR_M_FILE_NEW                  "~New"
#define STR_M_FILE_OPEN                 "~Open..."
#define STR_M_FILE_SAVE                 "~Save"
#define STR_M_FILE_SAVE_AS              "Save ~As..."
#define STR_M_FILE_PG_SETUP             "P~rint Setup"
#define STR_M_FILE_PRINT                "~Print"
#define STR_M_FILE_QUIT                 "E~xit"
#elif (XVTWS == WIN16WS) || (XVTWS == WIN32WS)
#define STR_M_FILE                      "~File"
#define STR_M_FILE_NEW                  "~New"
#define STR_M_FILE_OPEN                 "~Open..."
#define STR_M_FILE_CLOSE                "~Close"
#define STR_M_FILE_SAVE                 "~Save"
#define STR_M_FILE_SAVE_AS              "Save ~As..."
#define STR_M_FILE_REVERT               "Re~vert to Saved"
#define STR_M_FILE_PG_SETUP             "P~rint Setup..."
#define STR_M_FILE_PRINT                "~Print..."
#define STR_M_FILE_QUIT                 "E~xit"
#elif (XVTWS == PMWS)
#define STR_M_FILE                      "~File"
#define STR_M_FILE_NEW                  "~New"
#define STR_M_FILE_OPEN                 "~Open..."
#define STR_M_FILE_SAVE                 "~Save"
#define STR_M_FILE_SAVE_AS              "Save ~As..."
#define STR_M_FILE_REVERT               "Re~vert to Saved"
#define STR_M_FILE_PG_SETUP             "P~rint Setup..."
#define STR_M_FILE_PRINT                "~Print..."
#define STR_M_FILE_QUIT                 "E~xit"
#define STR_M_FILE_ABOUT                "A~bout..."
#endif

/*
* Standard Edit Menu Labels
*/
#if (XVTWS == MACWS)
#define STR_M_EDIT                      "Edit"
#define STR_M_EDIT_UNDO                 "Undo"
#define STR_M_EDIT_CUT                  "Cut"
#define STR_M_EDIT_COPY                 "Copy"
#define STR_M_EDIT_PASTE                "Paste"
#define STR_M_EDIT_CLEAR                "Clear"
#define STR_M_EDIT_SEL_ALL              "Select All"
#define STR_M_EDIT_CLIPBOARD            "Show Clipboard"
#define STR_M_EDIT_UNDO_ACCEL           "Z" ALT
#define STR_M_EDIT_CUT_ACCEL            "X" ALT
#define STR_M_EDIT_COPY_ACCEL           "C" ALT
#define STR_M_EDIT_PASTE_ACCEL          "V" ALT
#define STR_M_EDIT_SEL_ALL_ACCEL        "A" ALT
#elif (XVTWS == MTFWS)
#define STR_M_EDIT                      "~Edit"
#define STR_M_EDIT_UNDO                 "~Undo"
#define STR_M_EDIT_CUT                  "Cu~t"
#define STR_M_EDIT_COPY                 "~Copy"
#define STR_M_EDIT_PASTE                "~Paste"
#define STR_M_EDIT_CLEAR                "~Delete"
#define STR_M_EDIT_UNDO_ACCEL           BACK ALT
#define STR_M_EDIT_CUT_ACCEL            DEL SHIFT
#define STR_M_EDIT_COPY_ACCEL           INS CONTROL
#define STR_M_EDIT_PASTE_ACCEL          INS SHIFT
#elif (XVTWS == WIN16WS) || (XVTWS == WIN32WS)
#define STR_M_EDIT                      "~Edit"
#define STR_M_EDIT_UNDO                 "~Undo\tCtrl+Z"
#define STR_M_EDIT_CUT                  "Cu~t\tCtrl+X"
#define STR_M_EDIT_COPY                 "~Copy\tCtrl+C"
#define STR_M_EDIT_PASTE                "~Paste\tCtrl+V"
#define STR_M_EDIT_CLEAR                "~Delete"
#define STR_M_EDIT_UNDO_ACCEL           "Z" CONTROL
#define STR_M_EDIT_CUT_ACCEL            "X" CONTROL
#define STR_M_EDIT_COPY_ACCEL           "C" CONTROL
#define STR_M_EDIT_PASTE_ACCEL          "V" CONTROL
#elif (XVTWS == PMWS)
#define STR_M_EDIT                      "~Edit"
#define STR_M_EDIT_UNDO                 "~Undo\tAlt+BS"
#define STR_M_EDIT_COPY                 "~Copy\tCtrl+Ins"
#ifndef NO_DEL_ACCEL
#define STR_M_EDIT_CUT                  "Cu~t\tShift+Del"
#define STR_M_EDIT_PASTE                "~Paste\tShift+Ins"
#else
#define STR_M_EDIT_CUT                  "Cu~t"
#define STR_M_EDIT_PASTE                "~Paste"
#endif
#define STR_M_EDIT_CLEAR                "Cl~ear\tDel"
#define STR_M_EDIT_UNDO_ACCEL           BACK ALT
#define STR_M_EDIT_CUT_ACCEL            DEL SHIFT
#define STR_M_EDIT_COPY_ACCEL           INS CONTROL
#define STR_M_EDIT_PASTE_ACCEL          INS SHIFT
#define STR_M_EDIT_CLEAR_ACCEL          DEL
#endif

/*
* Standard Font Menu Labels
*/
#if (XVTWS == MACWS)
#define STR_M_FONT                      "Font"
#elif (XVTWS == MTFWS)
#define STR_M_FONT                      "Fon~t"
#define STR_M_FONT_COURIER              "~Courier"
#define STR_M_FONT_HELVETICA            "~Helvetica"
#define STR_M_FONT_SYSTEM               "~System"
#define STR_M_FONT_TIMES                "~Times"
#define STR_M_FONT_OTHER                "~Other"
#elif (XVTWS == WIN16WS) || (XVTWS == WIN32WS) || (XVTWS == PMWS)
#define STR_M_FONT                      "Fon~t"
#define STR_M_FONT_SELECT               "~Select..."
#endif /* (XVTWS == MACWS) */

/*
* Standard Style Menu Labels
*/
#if (XVTWS == MACWS)
#define STR_M_STYLE                     "Style"
    /* I18N - This string contains the menu items on the first half of the */
    /*        Style Menu. */
    /*        The /x characters (ie "/T") are Mac Menu Manager specific codes */
    /*           to determine the menu accelerator */
    /*        The <y characters (ie "<B") are Mac Menu Manager specific */
    /*           codes to set the font style for the menu item. */
    /*        The "-(" item is a Mac Menu Manager specific code to specify a */
    /*           menu separator line */
    /*        All items are separated by semi-colons ";" */
#define STR_MAC_STYLEM1                 "Plain Text/T;-(;Bold<B/B;Italic<I/I;Underline<U/U;Outline<O;Shadow<S;-(;Condense;Extend"

    /* I18N - This string represents the the order of font modifier action in */
    /*        STR_MAC_STYLEM1 above. The characters in this string should not */
    /*        be changed but the order may need to be changed. The characters have */
    /*        the following font modifier action representation:              */
    /*             P = Plain Text */
    /*             B = Bold Text */
    /*             I = Italic Text */
    /*             U = Underline Text */
    /*             O = Outline Text */
    /*             S = Shadow Text */
    /*             C = Condensed Text */
    /*             E = Extended Text */
    /*             space = separator or other non-font modifier action menu item */
    /*        The position of these characters must match exactly to the position of */
    /*        the corresponding item in the menu described by STR_MAC_STYLEM1. */
#define STR_MAC_STYLEM2                 "P BIUOS CE"

    /* I18N - This string contains the menu items on the second half of the */
    /*        Style Menu. */
    /*        This string assumes that the decimal number */
    /*           (ie "10" in "10 point") is first for each item and */
    /*           represents the font point size. */
    /*        A non-decimal character cannot begin the item. */
    /*        The %d represents the default point size for the Other dialog */
#define STR_MAC_STYLEM3                 "-(;9 point;10 point;12 point;14 point;18 point;24 point;36 point;48 point;72 point;Other %s%d%s..."
#elif (XVTWS == MTFWS)
#define STR_M_STYLE                     "~Style"
#define STR_M_STYLE_NORMAL              "~Normal"
#define STR_M_STYLE_BOLD                "~Bold"
#define STR_M_STYLE_ITALIC              "~Italic"
#define STR_M_STYLE_8                   " 8 Point"
#define STR_M_STYLE_10                  "10 Point"
#define STR_M_STYLE_12                  "12 Point"
#define STR_M_STYLE_14                  "14 Point"
#define STR_M_STYLE_18                  "18 Point"
#define STR_M_STYLE_24                  "24 Point"
#endif

/*
* Standard Help Menu Labels
*/
#if (XVTWS == MACWS)
#define STR_M_HELP                      "Help"
#define STR_M_HELP_ONWINDOW             "This Window"
#define STR_M_HELP_SEARCH               "Search..."
#define STR_M_HELP_CONTENTS             "Contents"
#define STR_M_HELP_INDEX                "Index"
#define STR_M_HELP_OBJCLICK             "Object Click"
#define STR_M_HELP_HELPONHELP           "Help On Help"
#define STR_M_HELP_VERSION              "About..."
#elif (XVTWS == MTFWS)
#define STR_M_HELP                      "~Help"
#define STR_M_HELP_ONWINDOW             "On ~Window"
#define STR_M_HELP_CONTENTS             "Co~ntents"
#define STR_M_HELP_INDEX                "~Index"
#define STR_M_HELP_OBJCLICK             "On ~Context"
#define STR_M_HELP_HELPONHELP           "On ~Help"
#define STR_M_HELP_VERSION              "On ~Version"
#define STR_M_HELP_KEYBOARD             "On ~Keys"
#elif (XVTWS == WIN16WS) || (XVTWS == WIN32WS)
#define STR_M_HELP                      "~Help"
#define STR_M_HELP_CONTENTS             "~Contents"
#define STR_M_HELP_SEARCH               "~Search for help on..."
#define STR_M_HELP_HELPONHELP           "~How to use help"
#define STR_M_HELP_OBJCLICK             "~Object click\tShift+F1"
#define STR_M_HELP_VERSION              "~About..."
#define STR_M_HELP_CONTENTS_ACCEL       F1 ALT
#define STR_M_HELP_SEARCH_ACCEL         F1 CONTROL
#define STR_M_HELP_OBJCLICK_ACCEL       F1 SHIFT
#elif (XVTWS == PMWS)
#define STR_M_HELP                      "~Help"
#define STR_M_HELP_INDEX                "Help ~index"
#define STR_M_HELP_ONWINDOW             "~General help"
#define STR_M_HELP_HELPONHELP           "~Using help"
#define STR_M_HELP_KEYBOARD             "~Keys help"
#define STR_M_HELP_OBJCLICK             "~Object click\tAlt+F1"
#define STR_M_HELP_VERSION              "~Product information"
#define STR_M_HELP_ONCONTEXT_ACCEL      F1
#define STR_M_HELP_OBJCLICK_ACCEL       F1 ALT
#endif /* (XVTWS == MACWS) */

/*
* Windows MDI Menus
*/
#if (XVTWS == WIN16WS) || (XVTWS == WIN32WS)
#define STR_XVT_WINDOWMENU              "&Window"
#define STR_IDM_WINDOWTILE              "&Tile"
#define STR_IDM_WINDOWCASCADE           "&Cascade"
#define STR_IDM_WINDOWICONS             "&Arrange Icons"
#endif /* (XVTWS == WIN16WS) || (XVTWS == WIN32WS) */

/****************************************************************************
    Standard Dialogs Section
 ****************************************************************************/

/*
* Standard Error Dialog
*/
#define STR_DB_ERROR                    ""
#if (XVTWS == WIN16WS) || (XVTWS == WIN32WS)
#define STR_DB_ERROR_OK                 ""
#define STR_DB_ERROR_CANCEL             ""
#define STR_DB_ERROR_NO                 ""
#define STR_DB_ERROR_TEXT               ""
#else
#define STR_DB_ERROR_OK                 "OK"
#endif

/*
* Standard Note Dialog
*/
#define STR_DB_NOTE                     ""
#if (XVTWS == WIN16WS) || (XVTWS == WIN32WS)
#define STR_DB_NOTE_OK                  ""
#define STR_DB_NOTE_CANCEL              ""
#define STR_DB_NOTE_NO                  ""
#define STR_DB_NOTE_TEXT                ""
#else
#define STR_DB_NOTE_OK                  "OK"
#endif
/*
* Standard Warning Dialog
*/
#define STR_DB_WARNING                  ""
#define STR_DB_WARNING_OK               "OK"

/*
* Standard Abort Dialog
*/
#if (XVTWS == MACWS)
#define STR_DB_ABORT                    ""
#define STR_DB_ABORT_TEXT               "Press \021-. to cancel printing."
#elif (XVTWS == WIN16WS) || (XVTWS == WIN32WS)
#define STR_DB_ABORT_CANCEL             "Cancel"
#define STR_DB_ABORT_PRINTING           "Printing"
#define STR_DB_ABORT_TEXT1              ""
#define STR_DB_ABORT_TEXT2              ""
#elif (XVTWS == PMWS)
#define STR_DB_ABORT_CANCEL             "Cancel"
#define STR_DB_ABORT_PRINTING           "Printing"
#define STR_DB_ABORT_TEXT1              ""
#define STR_DB_ABORT_TEXT2              ""
#endif

/*
* Standard Ask Dialog
*/
#define STR_DB_ASK                      ""
#define STR_DB_ASK_OK                   ""
#define STR_DB_ASK_CANCEL               ""
#define STR_DB_ASK_OTHER                ""
#define STR_DB_ASK_TEXT                 ""
#if (XVTWS == PMWS)
#define STR_DB_ASK_HELP                 "Help"
#endif

/*
* Standard Response Dialog
*/
#if (XVTWS == MACWS)
#define STR_DB_RESPONSE                 "String Response Dialog"
#elif (XVTWS == MTFWS)
#define STR_DB_RESPONSE                 ""
#elif (XVTWS == WIN16WS) || (XVTWS == WIN32WS)
#define STR_DB_RESPONSE_EDIT            ""
#define STR_DB_RESPONSE_TEXT            ""
#elif (XVTWS == PMWS)
#define STR_DB_RESPONSE_HELP            "Help"
#define STR_DB_RESPONSE_TEXT            ""
#endif /* (XVTWS == MACWS) */
#define STR_DB_RESPONSE_OK              "OK"
#define STR_DB_RESPONSE_CANCEL          "Cancel"

/*
* Standard Font Selection Dialog
*/
#if (XVTWS == MACWS)
#define STR_DB_FONTSEL                  "Font Selection Dialog"
#define STR_DB_FONTSEL_OK               "OK"
#define STR_DB_FONTSEL_CANCEL           "Cancel"
#define STR_DB_FONTSEL_FONT             "Font:"
#define STR_DB_FONTSEL_STYLE            "Style"
#define STR_DB_FONTSEL_BOLD             "Bold"
#define STR_DB_FONTSEL_ITALIC           "Italic"
#define STR_DB_FONTSEL_UNDERLINE        "Underline"
#define STR_DB_FONTSEL_OUTLINE          "Outline"
#define STR_DB_FONTSEL_SHADOW           "Shadow"
#define STR_DB_FONTSEL_SPACING          "Spacing"
#define STR_DB_FONTSEL_NORMAL           "Normal"
#define STR_DB_FONTSEL_CONDENSED        "Condensed"
#define STR_DB_FONTSEL_EXTENDED         "Extended"
#define STR_DB_FONTSEL_SIZE             "Size:"
#define STR_DB_FONTSEL_RECT             174,  68, 399, 226
#define STR_DB_FONTSEL_OK_RECT          329, 196,  60,  20
#define STR_DB_FONTSEL_CANCEL_RECT      329, 163,  60,  20
#define STR_DB_FONTSEL_FONT_RECT         23,  16, 150,  16
#define STR_DB_FONTSEL_LIST_RECT         23,  35, 150, 136
#define STR_DB_FONTSEL_STYLE_RECT       195,  16, 117, 113
#define STR_DB_FONTSEL_BOLD_RECT        200,  34, 108,  18
#define STR_DB_FONTSEL_ITALIC_RECT      200,  52, 108,  18
#define STR_DB_FONTSEL_UNDERLINE_RECT   200,  70, 108,  18
#define STR_DB_FONTSEL_OUTLINE_RECT     200,  88, 108,  18
#define STR_DB_FONTSEL_SHADOW_RECT      200, 106, 108,  18
#define STR_DB_FONTSEL_SPACING_RECT     195, 139, 117,  77
#define STR_DB_FONTSEL_NORMAL_RECT      200, 157, 108,  18
#define STR_DB_FONTSEL_CONDENSED_RECT   200, 175, 108,  18
#define STR_DB_FONTSEL_EXTENDED_RECT    200, 193, 108,  18
#define STR_DB_FONTSEL_SIZE_RECT      	 23, 181, 150,  16
#define STR_DB_FONTSEL_LISTEDIT_RECT     23, 197,  69,  22
#endif /* (XVTWS == MACWS) */

/*
* Standard Font Size Selection Dialog
*/
#if (XVTWS == MACWS)
#define STR_DB_FONTSIZE                 "Other Font Size"
#define STR_DB_FONTSIZE_OK              "OK"
#define STR_DB_FONTSIZE_CANCEL          "Cancel"
#define STR_DB_FONTSIZE_FONTSIZE        "Size:" /* "Font Size:" */
#define STR_DB_FONTSIZE_POINTS          "points"
#define STR_DB_FONTSIZE_RECT            174,  68, 217,  85
#define STR_DB_FONTSIZE_OK_RECT         147,  55,  60,  20
#define STR_DB_FONTSIZE_CANCEL_RECT      74,  55,  60,  20
#define STR_DB_FONTSIZE_EDIT_RECT        48,  13,  50,  22
#define STR_DB_FONTSIZE_FONTSIZE_RECT    10,  16,  36,  16
#define STR_DB_FONTSIZE_POINTS_RECT     103,  16,  55,  16
#endif /* (XVTWS == MACWS) */

/*
* Standard About Dialog
*/
#define STR_DB_ABOUT                    ""
#define STR_DB_ABOUT_OK                 "Help"
#define STR_DB_ABOUT_CANCEL             "Cancel"
#if (XVTWS == WIN16WS) || (XVTWS == WIN32WS) || (XVTWS == PMWS)
#define STR_DB_ABOUT_TEXT1              "An XVT Application"
#define STR_DB_ABOUT_TEXT2              "Version 4.57"
#define STR_DB_ABOUT_TEXT3              "by XVT Software."
#elif (XVTWS == MACWS)
#define STR_DB_ABOUT_TEXT1              "An XVT Application, Version 4.57"
#define STR_DB_ABOUT_TEXT2              "by XVT Software."
#else
#define STR_DB_ABOUT_TEXT1              "This program was developed with XVT,"
#define STR_DB_ABOUT_TEXT2              "the Extensible Virtual Toolkit."
#endif

/*
* Standard Open File Dialog
*/
#if (XVTWS == MACWS)
#define STR_DB_OPENFILE_SELECT          "Select"
#elif (XVTWS == MTFWS)
#define STR_DB_OPENFILE                 ""
#define STR_DB_OPENFILE_OPEN            "Open"

#endif /* (XVTWS == MACWS) */

/*
* Standard Page Setup Dialog
*/
#if (XVTWS == MTFWS)
#define STR_D_PAGE_SETUP_DIALOG         "Printer Page Setup"
#define STR_D_PAGE_SIZE                 "Page Size"
#define STR_D_PAGE_USLETTER             "US Letter"
#define STR_D_PAGE_USLEGAL              "US Legal"
#define STR_D_PAGE_A4LETTER             "A4 Letter"
#define STR_D_PAGE_B5LETTER             "B5 Letter"
#define STR_D_PAGE_ORIENTATION          "Orientation"
#define STR_D_PAGE_OLANDSCAPE           "Landscape"
#define STR_D_PAGE_OPORTRAIT            "Portrait"
#define STR_D_PAGE_OPT_ENLARGMENT       "Scale (%)"
#define STR_D_PAGE_INP_ENLARGMENT       "100"
#define STR_D_PAGE_IMAGE                "Image"
#define STR_D_PAGE_IMAGE_MONO           "Monochrome"
#define STR_D_PAGE_IMAGE_GRAY           "Grayscale"
#define STR_D_PAGE_IMAGE_COLOR          "Color"
#define STR_D_PAGE_OK                   "OK"
#define STR_D_PAGE_CANCEL               "Cancel"
#define STR_D_PAGE_SETUP_DIALOG_RCT     140,  60, 250, 250
#define STR_D_PAGE_SIZE_RCT              15,  10, 100, 110 
#define STR_D_PAGE_USLETTER_RCT          20,  35,  90,  16 
#define STR_D_PAGE_USLEGAL_RCT           20,  55,  90,  16 
#define STR_D_PAGE_A4LETTER_RCT          20,  75,  90,  16 
#define STR_D_PAGE_B5LETTER_RCT          20,  95,  90,  16  
#define STR_D_PAGE_ORIENTATION_RCT      135,  10, 100,  80 
#define STR_D_PAGE_OLANDSCAPE_RCT       140,  35,  90,  16  
#define STR_D_PAGE_OPORTRAIT_RCT        140,  55,  90,  16  
#define STR_D_PAGE_OPT_ENLARGMENT_RCT    15, 130, 100,  60  
#define STR_D_PAGE_INP_ENLARGMENT_RCT    30, 155,  70,  16 
#define STR_D_PAGE_IMAGE_RCT            135, 100, 100,  90  
#define STR_D_PAGE_IMAGE_MONO_RCT       140, 125,  90,  16  
#define STR_D_PAGE_IMAGE_GRAY_RCT       140, 145,  90,  16  
#define STR_D_PAGE_IMAGE_COLOR_RCT      140, 165,  90,  16  
#define STR_D_PAGE_OK_RCT                30, 210,  70,  25  
#define STR_D_PAGE_CANCEL_RCT           150, 210,  70,  25  

#elif (XVTWS == PMWS)
#define STR_STR_DB_PRSETUP              "Printer Setup"
#define STR_STR_DB_PRSETUP_OK           "OK"
#define STR_STR_DB_PRSETUP_CANCEL       "Cancel"
#define STR_STR_DB_PRSETUP_SETUP        "~Setup"
#define STR_STR_DB_PRSETUP_TEXT         "Installed Printers"
#endif /* (XVTWS == MTFWS) */

/****************************************************************************
    Platform Specific String Section
 ****************************************************************************/

#if (XVTWS == MACWS)
#define STR_MAC_SIGNATURE               "R4 XVT Application"
#define STR_MAC_HELP                    "Help"
    /* I18N - This string contains the about item on the Apple menu. */
    /*        The %s represents the application name from appl_name in */
    /*        the XVT_CONFIG structure */
#define STR_MAC_ABOUT                   "About %s..."
#define STR_MAC_CLICK                   "Click mouse to continue."
    /* I18N - This string is used in the open file dialog to select directories. */
    /*        The %s represents the current directory name selected. */
#define STR_MAC_SELECT                  "Select \322%s\323"
    /* I18N - This string is used in the notification manager routines. */
    /*        The %s represents the current application name. */
#define STR_MAC_NOTIFICATION			"The application \322%s\323 needs your attention.\n\n"
#define STR_MAC_LOW_MEM_WARNING			"There is very little memory available. Please increase the preferred memory size in the application's Get Info window."

#elif (XVTWS == MTFWS)
#define STR_WM_HELPTYPE                 "hlp"
#define STR_WM_PRINT_001                "Error accessing page setup info."
#define STR_WM_PRINT_002                "xvtprolg.ps"
    /* I18N - This string contains a printing error message. */
    /*        The %s represents the file name of a font file */
#define STR_WM_PRINT_003                "Can't access AFM file %s."
#elif (XVTWS == WIN16WS) || (XVTWS == WIN32WS)
#define STR_WIN_HELPTYPE                "HLP"
/* strings used in bothstr.h and as default string in SGETSTR */
#define STR_WIN_OK                      "OK"
#define STR_WIN_CANCEL                  "Cancel"
#define STR_WIN_CANT_SHOW_ABOUT_BOX     "Can't show the about box"
#define STR_WIN_CANCELLING              "Cancelling..."
#define STR_WIN_INITIALIZING            "Initializing"
#define STR_WIN_UNTITLED                "(Untitled)"
#define STR_WIN_STARTING_DOC            "(Starting Document)"
#define STR_WIN_STARTED_DOC             "(Started Document)"
#define STR_WIN_ENDING_DOC              "(Ending Document)"
    /* I18N - This string contains a message from the print dialog. */
    /*        The %d represents the page number of the current printing page */
#define STR_WIN_STARTING_PAGE           "(Starting Page %d)"
    /* I18N - This string contains a message from the print dialog. */
    /*        The %d represents the page number of the finished printing page */
#define STR_WIN_FINISHING_PAGE          "(Finishing Page %d)"
#define STR_WIN_SYSTEM_ERROR            "System Error"
#define STR_WIN_PICTURE_STILL_LOCKED    "Picture still locked."
#define STR_WIN_CANNOT_PRINT            "Cannot Print"
#define STR_WIN_ALERT                   "Alert"
#define STR_WIN_FDLG_FILTER             "Files (*.%s)|*.%s|"
#define STR_WIN_FDLG_FILTER_ALL         "All Files (*.*)|*.*|"
#define STR_WIN_NO_EXT_RES              "Cannot find resource file - will use default resources"
#define STR_WIN_DEFAULTFAMILY           "Arial"
#define STR_WIN_SYSTEMFAMILY            "System"
#define STR_WIN_HELVETICAFAMILY         "Arial"
#define STR_WIN_FIXEDFAMILY             "Courier New"
#define STR_WIN_TIMESFAMILY             "Times New Roman"
#define STR_WIN_COURIERFAMILY           "Courier New"
#elif (XVTWS == PMWS)
#define STR_PM_HELPTYPE                 "hlp"
#define STR_PM_OK                       "OK"
#define STR_PM_CANCEL                   "Cancel"
#define STR_PM_CANT_SHOW_ABOUT_BOX      "Can't show the about box"
#define STR_PM_CANCELLING               "Cancelling..."
#define STR_PM_INITIALIZING             "Initializing"
#define STR_PM_UNTITLED                 "(Untitled)"
#define STR_PM_STARTING_DOC             "(Starting Document)"
#define STR_PM_STARTED_DOC              "(Started Document)"
#define STR_PM_ENDING_DOC               "(Ending Document)"
    /* I18N - This string contains a message from the print dialog. */
    /*        The %d represents the page number of the current printing page */
#define STR_PM_STARTING_PAGE            "(Printing Page %d)"
    /* I18N - This string contains a message from the print dialog. */
    /*        The %d represents the page number of the finished printing page */
#define STR_PM_FINISHING_PAGE           "(Finishing Page %d)"
#define STR_PM_SYSTEM_ERROR             "System Error"
#define STR_PM_PICTURE_STILL_LOCKED     "Picture still locked."
#define STR_PM_CANNOT_PRINT             "Cannot Print"
#define STR_PM_OPENING_PRINTER          "(Opening Printer)"
#define STR_PM_NO_ERROR_INFO_AVAILABLE  "No error information available"
#define STR_PM_PRINTING_ERROR           "Printing Error"
#define STR_PM_CANT_CREATE_PRINT_THREAD "Could not create print thread"
    /* I18N - This string contains PM system errors. */
    /*        The %x represents a hex system error id */
#define STR_PM_ERROR                    "PM Error 0x%x"
#define STR_PM_ESCAPE_FAILED            "Printer Escape Failed"
#define STR_PM_ALT                      "Alt"
#define STR_PM_CTRL                     "Ctrl"
#define STR_PM_CTRLF6                   "~Next Window\tCtrl+F6"
#define STR_PM_OPEN                     "Open"
#define STR_PM_SAVEAS                   "Save As"
#define STR_PM_DEFAULTFAMILY            "Helv"
#define STR_PM_COURIERFAMILY            "Courier"
#define STR_PM_FIXEDFAMILY              "Courier"
#define STR_PM_HELVETICAFAMILY          "Helvetica"
#define STR_PM_SYSTEMFAMILY             "System Proportional"
#define STR_PM_TIMESFAMILY              "Times New Roman"
#endif /* (XVTWS == MACWS) */
/****************************************************************************
    Help System Section
 ****************************************************************************/

/*
 * R4 Help System Error Messages
 */
#define STR_HELP_CLIP_ERR               "Cannot open clipboard for writing"
#define STR_HELP_MEM_ERR                "Out of memory"
#define STR_HELP_CLIP_PUT_ERR           "Cannot place information on clipboard"
#define STR_HELP_PRINT_ERR              "Printing failed."
#define STR_HELP_PRINT_OK               "Topic printed"
#define STR_HELP_CLIP_OK                "Topic copied to clipboard."
    /* I18N - This string is used to signify the topic sequence number in */
    /*        the help viewer */
    /*        The first %d represents which topic in a sequence of topics */
    /*        that is currently being viewed */
    /*        The second %d represents the total number of topics that have */
    /*        been viewed */
#define STR_HELP_THREAD_INFO            "Topic: %d of %d"
    /* I18N - This string is used to show if a topic has been marked */
    /*        (bookmarks) */
    /*        The %c represents a the single byte character 'x' */
    /*        or ' ' (space). */
    /*        This means the item is either marked (x) or unmarked ( ). */
#define STR_HELP_MARKED_INFO            "Marked: [%c]"
#define STR_HELP_COPYPART               "No selection has been made - nothing to copy"

/*
* R4 Help System Topic Button Labels
*/
#define STR_HELP_MARK                   "Mark"
#define STR_HELP_UNMARK                 "Unmark"

/*
* R4 Help System Topic Navigation Menu
*/
#define STR_HELP_NAV_SEARCH             SEARCH_STR
#define STR_HELP_NAV_GOTO               "~Goto..." 
#define STR_HELP_NAV_MARK               "Set Book~mark" 
#define STR_HELP_NAV_BACKLINK           "Go ~Back to Topic" 
#define STR_HELP_NAV_FORWLINK           "Go ~Forward to Topic" 
#define STR_HELP_NAV_PREVPAGE           "~Previous Topic  <<" 
#define STR_HELP_NAV_NEXTPAGE           "~Next Topic  >>"

/*
* R4 Help System Menu Bar
*/
#define STR_MHELP_FILE                  "~File"
#define STR_MHELP_EDIT                  "~Edit"
#define STR_MHELP_NAV                   "~Navigate"
#define STR_MHELP_HELP                  "~Help" 

/*
* R4 Help System File Menu Labels
*/
#if (XVTWS == MACWS)
#define STR_MHELP_FILE_PRINT            "Print..." 
#define STR_MHELP_FILE_PRINT_SETUP      "Page Setup..." 
#define STR_MHELP_FILE_EXIT             "Quit"
#define STR_MHELP_FILE_EXIT_ACCEL       "Q" ALT
#define STR_MHELP_FILE_PRINT_ACCEL      "P" ALT
#else  /* !MACWS */
#define STR_MHELP_FILE_PRINT            "~Print"
#define STR_MHELP_FILE_PRINT_SETUP      "P~rint Setup..."
#define STR_MHELP_FILE_EXIT             "E~xit"
#endif /* !MACWS */

/*
* R4 Help System Edit Menu Labels
*/
#if (XVTWS == WIN16WS) || (XVTWS == WIN32WS) || (XVTWS == PMWS)
#define STR_MHELP_EDIT_COPY             "~Copy\tCtrl+C"
#define STR_MHELP_TSE_EDIT_COPY         "~Copy\tCtrl+C"
#define STR_MHELP_EDIT_COPY_ACCEL       "C" CONTROL
#else  /* ! ((XVTWS == WIN16WS) || (XVTWS == WIN32WS) || (XVTWS == PMWS)) */
#define STR_MHELP_EDIT_COPY             "~Copy"
#define STR_MHELP_TSE_EDIT_COPY         "~Copy"
#endif /* (XVTWS == WIN16WS) || (XVTWS == WIN32WS) || (XVTWS == PMWS) */
#if XVTWS != MTFWS
#define STR_MHELP_EDIT_COPYPART         "Co~py Part of Topic..."
#endif
#define STR_MHELP_EDIT_AS_WRAPPED       "Copy as ~Wrapped"

/*
* R4 Help System Navigation Menu Labels
*/
#define STR_MHELP_NAV_SEARCH            "~Search..."
#define STR_MHELP_NAV_GOTO              "~Goto..."
#define STR_MHELP_NAV_MARK              "Book~mark"
#define STR_MHELP_NAV_BACKLINK          "Go ~Back to Topic"
#define STR_MHELP_NAV_FORWLINK          "Go ~Forward to Topic"
#define STR_MHELP_NAV_PREVPAGE          "~Previous Topic  <<"
#define STR_MHELP_NAV_NEXTPAGE          "~Next Topic  >>"

/*
* R4 Help System Help Menu Labels
*/
#define STR_MHELP_HELP_ONHELP           "~How to Use Help"
#define STR_MHELP_HELP_ABOUT            "~About Help"

/*
* R4 Help System Edit Selection Menu Bar
*/
#define STR_MHELP_TSE_EDIT              "~Edit"
#define STR_MHELP_TSE_HELP              "~Help"

/*
* R4 Help System Help Menu Labels
*/
#define STR_MHELP_TSE_HELP_ONHELP       "~How to Use Help"

/*
* R4 Help System Help Topic Window
*/
#define STR_HELP_TOPIC_WIN              "topic window"
#define STR_HELP_TOPIC_INFOLBL          "Information and status"
#define STR_HELP_TOPIC_BOOKMARK         "Mark"
#define STR_HELP_TOPIC_BACKLINK         "Back"
#if XVTWS == MTFWS
#define STR_HELP_TOPIC_SEARCH           "Search"
#define STR_HELP_TOPIC_GOTO             "Go To"
#define STR_HELP_TOPIC_FORWLINK         "Fwd"
#define STR_HELP_TOPIC_PREVPAGE         "--"
#define STR_HELP_TOPIC_NEXTPAGE         "++"
#else
#define STR_HELP_TOPIC_SEARCH           "Search..."
#define STR_HELP_TOPIC_GOTO             "Go To..."
#define STR_HELP_TOPIC_FORWLINK         "Forward"
#define STR_HELP_TOPIC_PREVPAGE         "<<"
#define STR_HELP_TOPIC_NEXTPAGE         ">>"
#endif

/*
* R4 Help System Help Goto Dialog
*/
#define STR_HELP_GOTO_DLG               "Topic Goto Dialog"
#define STR_HELP_GOTO_GROUP             "Goto Options"
#define STR_HELP_GOTO_INDEX             "Index"
#define STR_HELP_GOTO_GLOSSARY          "Glossary"
#define STR_HELP_GOTO_CONTENTS          "Contents"
#define STR_HELP_GOTO_KEYBOARD          "Keyboard"
#define STR_HELP_GOTO_BOOK_LBL          "Bookmarks:"
#define STR_HELP_GOTO_CANCEL            "Cancel"
#if XVTWS != MTFWS
#define STR_HELP_GOTO_BOOKMARK_BTN      "Goto Mark"
#else
#define STR_HELP_GOTO_BOOKMARK_BTN      "Goto"
#endif /* ! MTFWS */

/*
* R4 Help System Help Search Dialog
*/
#define STR_HELP_SEARCH_DLG             "Topic Search Dialog"
#define STR_HELP_SEARCH_RADIO_LBL       "Search Options"
#define STR_HELP_SEARCH_BY_TOPICNAME    "By Topic"
#define STR_HELP_SEARCH_BY_KEYWORD      "By Keyword"
#if XVTWS == MACWS
#define STR_HELP_SEARCH_ITEMS_LBL       "Search Items:"
#define STR_HELP_SEARCH_MATCH_LBL       "Keyword Matches:"
#else
#define STR_HELP_SEARCH_ITEMS_LBL       "Search Items"
#define STR_HELP_SEARCH_MATCH_LBL       "Keyword Matches"
#endif /* MACWS */
#define STR_HELP_SEARCH_CANCEL          "Cancel"
#if XVTWS != MTFWS
#define STR_HELP_SEARCH_GOTO_MATCH      "Goto Selection"
#else
#define STR_HELP_SEARCH_GOTO_MATCH      "Goto"
#endif /* ! MTFWS */

/*
* R4 Help System Help Copy Window
*/
#define STR_HELP_TOPIC_SELCOPY          "Help Copy"
#define STR_HELP_TOPIC_SELCOPY_WIN_LBL  "Select text and choose Edit/Copy"

/*
* R4 Help System About Window
*/
#define STR_HELP_ABOUT                  "About helpview"
#define STR_HELP_ABOUT_CANCEL           "Cancel"

/**********Macro Definitions for Control's Rectangles*********************/
#if XVTWS == MACWS
#define REC_HELP_TOPIC_WIN              HV_URL_RECT(240,  48, 376, 384)
#define REC_HELP_TOPIC_INFOLBL          HV_URL_RECT(  8, 320, 280,  20)
#define REC_HELP_TOPIC_SEARCH           HV_URL_RECT(  8, 352,  68,  20)
#define REC_HELP_TOPIC_GOTO             HV_URL_RECT( 80, 352,  68,  20) 
#define REC_HELP_TOPIC_BOOKMARK         HV_URL_RECT(152, 352,  68,  20)
#define REC_HELP_TOPIC_BACKLINK         HV_URL_RECT(224, 352,  68,  20)
#define REC_HELP_TOPIC_FORWLINK         HV_URL_RECT(296, 352,  68,  20)
#define REC_HELP_TOPIC_PREVPAGE         HV_URL_RECT(312, 320,  32,  20)
#define REC_HELP_TOPIC_NEXTPAGE         HV_URL_RECT(344, 320,  32,  20)
#else
#define REC_HELP_TOPIC_WIN              HV_URL_RECT(240,  48, 376, 384)
#define REC_HELP_TOPIC_INFOLBL          HV_URL_RECT(  8, 320, 280,  24)
#define REC_HELP_TOPIC_SEARCH           HV_URL_RECT(  8, 352,  64,  24)
#define REC_HELP_TOPIC_GOTO             HV_URL_RECT( 80, 352,  64,  24) 
#define REC_HELP_TOPIC_BOOKMARK         HV_URL_RECT(152, 352,  64,  24)
#define REC_HELP_TOPIC_BACKLINK         HV_URL_RECT(224, 352,  64,  24)
#define REC_HELP_TOPIC_FORWLINK         HV_URL_RECT(296, 352,  64,  24)
#define REC_HELP_TOPIC_PREVPAGE         HV_URL_RECT(312, 320,  32,  24)
#define REC_HELP_TOPIC_NEXTPAGE         HV_URL_RECT(344, 320,  32,  24)
#endif

#if XVTWS == WMWS
#define REC_HELP_GOTO_DLG               HV_URL_RECT(238,  56, 209, 312)
#define REC_HELP_GOTO_GROUP             HV_URL_RECT(  8,   8, 192, 112)
#define REC_HELP_GOTO_INDEX             HV_URL_RECT( 48,  32, 112,  12)
#define REC_HELP_GOTO_GLOSSARY          HV_URL_RECT( 48,  48, 112,  12)
#define REC_HELP_GOTO_CONTENTS          HV_URL_RECT( 48,  64, 112,  12)
#define REC_HELP_GOTO_KEYBOARD          HV_URL_RECT( 48,  80, 112,  12)
#define REC_HELP_GOTO_BOOK_LBL          HV_URL_RECT(  8, 128,  96,  16)
#define REC_GOTO_BOOKMARK_LIST          HV_URL_RECT( 16, 144, 184, 112) 
#define REC_HELP_GOTO_BOOKMARK_BTN      HV_URL_RECT( 16, 272,  72,  24)
#define REC_HELP_GOTO_CANCEL            HV_URL_RECT(128, 272,  72,  24)
#define REC_HELP_SEARCH_DLG             HV_URL_RECT(131,  37, 288, 336)
#define REC_HELP_SEARCH_RADIO_LBL       HV_URL_RECT(  8,   8, 272,  64)
#define REC_HELP_SEARCH_BY_TOPICNAME    HV_URL_RECT( 24,  32,  96,  24)
#define REC_HELP_SEARCH_BY_KEYWORD      HV_URL_RECT(136,  32, 112,  24)
#define REC_HELP_SEARCH_ITEMS_LBL       HV_URL_RECT(  8,  80, 136,  16)
#define REC_SEARCH_SELECT_LIST          HV_URL_RECT(  8,  96, 272,  96)
#define REC_HELP_SEARCH_MATCH_LBL       HV_URL_RECT(  8, 200, 144,  16)
#define REC_SEARCH_MATCH_LIST           HV_URL_RECT(  8, 216, 272,  80)
#define REC_HELP_SEARCH_GOTO_MATCH      HV_URL_RECT( 24, 304,  80,  24)
#define REC_HELP_SEARCH_CANCEL          HV_URL_RECT(184, 304,  80,  24)
#elif XVTWS == MACWS
#define REC_HELP_GOTO_DLG               HV_URL_RECT( 85,  44, 272, 352)
#define REC_HELP_GOTO_GROUP             HV_URL_RECT( 10,   7, 252, 158)
#define REC_HELP_GOTO_INDEX             HV_URL_RECT( 80,  32, 112,  20)
#define REC_HELP_GOTO_GLOSSARY          HV_URL_RECT( 80,  65, 112,  20)
#define REC_HELP_GOTO_CONTENTS          HV_URL_RECT( 80,  98, 112,  20)
#define REC_HELP_GOTO_KEYBOARD          HV_URL_RECT( 80, 131, 112,  20)
#define REC_HELP_GOTO_BOOK_LBL          HV_URL_RECT( 10, 175, 252,  16)
#define REC_GOTO_BOOKMARK_LIST          HV_URL_RECT( 10, 195, 252, 112)
#define REC_HELP_GOTO_BOOKMARK_BTN      HV_URL_RECT(182, 322,  80,  20)
#define REC_HELP_GOTO_CANCEL            HV_URL_RECT(109, 322,  60,  20)
#define REC_HELP_SEARCH_DLG             HV_URL_RECT(240,  96, 292, 338)
#define REC_HELP_SEARCH_RADIO_LBL       HV_URL_RECT( 10,   7, 272,  64)
#define REC_HELP_SEARCH_BY_TOPICNAME    HV_URL_RECT( 15,  28, 262,  20)
#define REC_HELP_SEARCH_BY_KEYWOR       HV_URL_RECT( 15,  48, 262,  20)
#define REC_HELP_SEARCH_ITEMS_LBL       HV_URL_RECT( 10,  81, 272,  16)
#define REC_SEARCH_SELECT_LIST          HV_URL_RECT( 10, 101, 272,  96)
#define REC_HELP_SEARCH_MATCH_LBL       HV_URL_RECT( 10, 209, 272,  16)
#define REC_SEARCH_MATCH_LIST           HV_URL_RECT( 10, 229, 272,  72)
#define REC_HELP_SEARCH_GOTO_MATCH      HV_URL_RECT(176, 308, 106,  20)
#define REC_HELP_SEARCH_CANCEL          HV_URL_RECT(103, 308,  60,  20)
#else
#define REC_HELP_GOTO_DLG               HV_URL_RECT( 85,  44, 209, 376)
#define REC_HELP_GOTO_GROUP             HV_URL_RECT(  8,   8, 192, 168)
#define REC_HELP_GOTO_INDEX             HV_URL_RECT( 48,  40, 112,  24)
#define REC_HELP_GOTO_GLOSSARY          HV_URL_RECT( 48,  72, 112,  24)
#define REC_HELP_GOTO_CONTENTS          HV_URL_RECT( 48, 104, 112,  24)
#define REC_HELP_GOTO_KEYBOARD          HV_URL_RECT( 48, 136, 112,  24)
#define REC_HELP_GOTO_BOOK_LBL          HV_URL_RECT(  8, 184,  96,  16)
#define REC_GOTO_BOOKMARK_LIST          HV_URL_RECT( 16, 208, 184, 112)
#define REC_HELP_GOTO_BOOKMARK_BTN      HV_URL_RECT( 16, 336,  80,  24)
#define REC_HELP_GOTO_CANCEL            HV_URL_RECT(112, 336,  80,  24)
#define REC_HELP_SEARCH_DLG             HV_URL_RECT(240,  96, 288, 336)
#define REC_HELP_SEARCH_RADIO_LBL       HV_URL_RECT(  8,   8, 272,  56)
#define REC_HELP_SEARCH_BY_TOPICNAME    HV_URL_RECT( 24,  32,  96,  24)
#define REC_HELP_SEARCH_BY_KEYWOR       HV_URL_RECT(136,  32, 112,  24)
#define REC_HELP_SEARCH_ITEMS_LBL       HV_URL_RECT(  8,  72, 136,  16)
#define REC_SEARCH_SELECT_LIST          HV_URL_RECT(  8,  96, 272,  96)
#define REC_HELP_SEARCH_MATCH_LBL       HV_URL_RECT(  8, 200, 144,  16)
#define REC_SEARCH_MATCH_LIST           HV_URL_RECT(  8, 224, 272,  72)
#define REC_HELP_SEARCH_GOTO_MATCH      HV_URL_RECT( 16, 304, 112,  24)
#define REC_HELP_SEARCH_CANCEL          HV_URL_RECT(152, 304, 112,  24)
#endif

#if XVTWS == MACWS
#define REC_HELP_TOPIC_SELCOPY          HV_URL_RECT( 95, 249, 348, 176)
#define REC_HELP_TOPIC_SELCOPY_WIN_LBL  HV_URL_RECT(  0,   0, 320,  16)
#else
#define REC_HELP_TOPIC_SELCOPY          HV_URL_RECT( 95, 249, 320, 176)
#define REC_HELP_TOPIC_SELCOPY_WIN_LBL  HV_URL_RECT(  0,   0, 320,  24)
#endif


/****************************************************************************
    Common Code Section (strings found in V layer)
 ****************************************************************************/

#define STR_CC_NULL_ARG_STR             "NULL argument."
#define STR_CC_EMPTY_LIST_STR           "Empty list."
#define STR_CC_SLIST_GET_FAIL_STR       "xvtv_slist_get failed on element %d"
#define STR_CC_NAMED_COL_NOT_FOUND_STR  "Named color '%s' not found.\n"
#define STR_CC_FATAL_RECURSE_STR        "FATAL ERROR within last chance error handler\nexiting application"
#define STR_CC_INV_ERRMSG_STR           "Invalid XVT_ERRMSG object"
#define STR_CC_UNKNOWN_STR              "unknown"
#define STR_CC_DEF_TWICE_STR            "Warning.  Defined string %s and id %d twice.\n"

