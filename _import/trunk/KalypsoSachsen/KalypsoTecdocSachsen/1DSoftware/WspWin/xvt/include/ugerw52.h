/****************************************************************************
 *
 *  Copyright 1987-1995 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *  $RCSfile: ugerw52.h,v $
 *  $Revision: 1.5 $
 *
 *  Purpose: XVT URL standard resource string translation data for English
 *           in ASCII codeset.
 *
 
****************************************************************************/

/****************************************************************************
    Standard Menus Section
 
****************************************************************************/

#if (XVTWS == WIN16WS) || (XVTWS == WIN32WS)

/*
* Standard File Menu Labels
*/
#define STR_M_FILE                      "~Datei"
#define STR_M_FILE_NEW                  "~Neu"
#define STR_M_FILE_OPEN                 "Ö~ffnen"
#define STR_M_FILE_CLOSE                "Sch~ließen"
#define STR_M_FILE_SAVE                 "~Speichern"
#define STR_M_FILE_SAVE_AS              "Speichern ~unter..."
#define STR_M_FILE_REVERT               "~Letzte Version"
#define STR_M_FILE_PG_SETUP             "Druck~ereinrichtung..."
#define STR_M_FILE_PRINT                "~Drucken..."
#define STR_M_FILE_QUIT                 "~Beenden"

/*
* Standard Edit Menu Labels
*/
#define STR_M_EDIT                      "~Bearbeiten"
#define STR_M_EDIT_UNDO                 "~Rückgängig\tSTRG+Z"
#define STR_M_EDIT_CUT                  "~Ausschneiden\tSTRG+X"
#define STR_M_EDIT_COPY                 "~Kopieren\tSTRG+C"
#define STR_M_EDIT_PASTE                "~Einfügen\tSTRG+V"
#define STR_M_EDIT_CLEAR                "~Löschen"
#define STR_M_EDIT_UNDO_ACCEL           "Z" CONTROL
#define STR_M_EDIT_CUT_ACCEL            "X" CONTROL
#define STR_M_EDIT_COPY_ACCEL           "C" CONTROL
#define STR_M_EDIT_PASTE_ACCEL          "V" CONTROL

/*
* Standard Font Menu Labels
*/
#define STR_M_FONT                      "~Schriftart"
#define STR_M_FONT_SELECT               "~Auswählen..."

/*
* Standard Help Menu Labels
*/
#define STR_M_HELP                      "~Hilfe"
#define STR_M_HELP_CONTENTS             "~Inhalt"
#define STR_M_HELP_SEARCH               "~Suchen..."
#define STR_M_HELP_HELPONHELP           "~Hilfe benutzen"
#define STR_M_HELP_OBJCLICK             "~Objektklick\tUMSCHALTTASTE+F1"
#define STR_M_HELP_VERSION              "I~nfo..."
#define STR_M_HELP_CONTENTS_ACCEL       F1 ALT
#define STR_M_HELP_SEARCH_ACCEL         F1 CONTROL
#define STR_M_HELP_OBJCLICK_ACCEL       F1 SHIFT

/*
* Windows MDI Menus
*/
#define STR_XVT_WINDOWMENU              "&Fenster"
#define STR_IDM_WINDOWTILE              "&Nebeneinander"
#define STR_IDM_WINDOWCASCADE           "Über&lappend"
#define STR_IDM_WINDOWICONS             "&Symbole anordnen"

/****************************************************************************
    Standard Dialogs Section
 
****************************************************************************/

/*
* Standard Error Dialog
*/
#define STR_DB_ERROR                    ""
#define STR_DB_ERROR_OK                 ""
#define STR_DB_ERROR_CANCEL             ""
#define STR_DB_ERROR_NO                 ""
#define STR_DB_ERROR_TEXT               ""

/*
* Standard Note Dialog
*/
#define STR_DB_NOTE                     ""
#define STR_DB_NOTE_OK                  ""
#define STR_DB_NOTE_CANCEL              ""
#define STR_DB_NOTE_NO                  ""
#define STR_DB_NOTE_TEXT                ""

/*
* Standard Warning Dialog
*/
#define STR_DB_WARNING                  ""
#define STR_DB_WARNING_OK               "OK"

/*
* Standard Abort Dialog
*/
#define STR_DB_ABORT_CANCEL             "Abbrechen"
#define STR_DB_ABORT_PRINTING           "Druck läuft"
#define STR_DB_ABORT_TEXT1              ""
#define STR_DB_ABORT_TEXT2              ""

/*
* Standard Ask Dialog
*/
#define STR_DB_ASK                      ""
#define STR_DB_ASK_OK                   ""
#define STR_DB_ASK_CANCEL               ""
#define STR_DB_ASK_OTHER                ""
#define STR_DB_ASK_TEXT                 ""

/*
* Standard Response Dialog
*/
#define STR_DB_RESPONSE_EDIT            ""
#define STR_DB_RESPONSE_TEXT            ""
#define STR_DB_RESPONSE_OK              "OK"
#define STR_DB_RESPONSE_CANCEL          "Abbrechen"

/*
* Standard About Dialog
*/
#define STR_DB_ABOUT                    ""
#define STR_DB_ABOUT_OK                 "Hilfe"
#define STR_DB_ABOUT_CANCEL             "Abbrechen"
#define STR_DB_ABOUT_TEXT1              "Eine XVT-Anwendung"
#define STR_DB_ABOUT_TEXT2              "Version 4.57"
#define STR_DB_ABOUT_TEXT3              "von XVT Software."


/****************************************************************************
    Platform Specific String Section
 

****************************************************************************/

#define STR_WIN_HELPTYPE                "HLP"
/* strings used in bothstr.h and as default string in SGETSTR */
#define STR_WIN_OK                      "OK"
#define STR_WIN_CANCEL                  "Abbrechen"
#define STR_WIN_CANT_SHOW_ABOUT_BOX     "Info-Kästchen kann nicht angezeigt werden"
#define STR_WIN_CANCELLING              "Bricht ab..."
#define STR_WIN_INITIALIZING            "Initialisiert"
#define STR_WIN_UNTITLED                "(Unbenannt)"
#define STR_WIN_STARTING_DOC            "(Dokument wird begonnen)"
#define STR_WIN_STARTED_DOC             "(Begonnenes Dokument)"
#define STR_WIN_ENDING_DOC              "(Dokument wird beendet)"
    /* I18N - This string contains a message from the print dialog. */
    /*        The %d represents the page number of the current printing page */
#define STR_WIN_STARTING_PAGE           "(Seite %d wird gestartet)"
    /* I18N - This string contains a message from the print dialog. */
    /*        The %d represents the page number of the finished printing page */
#define STR_WIN_FINISHING_PAGE          "(Seite %d wird beendet)"
#define STR_WIN_SYSTEM_ERROR            "Systemfehler"
#define STR_WIN_PICTURE_STILL_LOCKED    "Bild noch gesperrt."
#define STR_WIN_CANNOT_PRINT            "Druck nicht möglich"
#define STR_WIN_ALERT                   "Warnung"
#define STR_WIN_FDLG_FILTER             "Dateien (*.%s)|*.%s|"
#define STR_WIN_FDLG_FILTER_ALL         "Alle Dateien (*.*)|*.*|"
#define STR_WIN_NO_EXT_RES              "Ressourcen-Datei kann nicht gefunden werden - Standard- Ressourcen werden verwendet"
#define STR_WIN_DEFAULTFAMILY           "Arial"
#define STR_WIN_SYSTEMFAMILY            "System"
#define STR_WIN_HELVETICAFAMILY         "Arial"
#define STR_WIN_FIXEDFAMILY             "Courier New"
#define STR_WIN_TIMESFAMILY             "Times New Roman"
#define STR_WIN_COURIERFAMILY           "Courier New"

/****************************************************************************
    Help System Section
 
****************************************************************************/

/*
 * R4 Help System Error Messages
 */
#define STR_HELP_CLIP_ERR               "Zwischenablage kann nicht zum Schreiben geöffnet werden"
#define STR_HELP_MEM_ERR                "Speicherkapazität erschöpft"
#define STR_HELP_CLIP_PUT_ERR           "Daten können nicht in der Zwischenablage plaziert werden"
#define STR_HELP_PRINT_ERR              "Drucken fehlgeschlagen."
#define STR_HELP_PRINT_OK               "Thema gedruckt"
#define STR_HELP_CLIP_OK                "Thema in Zwischenablage kopiert."
    /* I18N - This string is used to signify the topic sequence number in */
    /*        the help viewer */
    /*        The first %d represents which topic in a sequence of topics */
    /*        that is currently being viewed */
    /*        The second %d represents the total number of topics that have */
    /*        been viewed */
#define STR_HELP_THREAD_INFO            "Thema: %d von %d"
    /* I18N - This string is used to show if a topic has been marked */
    /*        (bookmarks) */
    /*        The %c represents a the single byte character 'x' */
    /*        or ' ' (space). */
    /*        This means the item is either marked (x) or unmarked ( ). */
#define STR_HELP_MARKED_INFO            "Markiert: [%c]"
#define STR_HELP_COPYPART               "Keine Auswahl getroffen - nichts zu kopieren"

/*
* R4 Help System Topic Button Labels
*/
#define STR_HELP_MARK                   "Markieren"
#define STR_HELP_UNMARK                 "Markierung aufheben"

/*
* R4 Help System Topic Navigation Menu
*/
#define STR_HELP_NAV_SEARCH             SEARCH_STR
#define STR_HELP_NAV_GOTO               "~Gehe zu..."
#define STR_HELP_NAV_MARK               "~Lesezeichen setzen"
#define STR_HELP_NAV_BACKLINK           "~Zurück zu Thema"
#define STR_HELP_NAV_FORWLINK           "~Weiter zu Thema"
#define STR_HELP_NAV_PREVPAGE           "~Vorheriges Thema <<"
#define STR_HELP_NAV_NEXTPAGE           "~Nächstes Thema >>"

/*
* R4 Help System Menu Bar
*/
#define STR_MHELP_FILE                  "~Datei"
#define STR_MHELP_EDIT                  "~Bearbeiten"
#define STR_MHELP_NAV                   "~Navigieren"
#define STR_MHELP_HELP                  "~Hilfe"

/*
* R4 Help System File Menu Labels
*/
#define STR_MHELP_FILE_PRINT            "~Drucken "
#define STR_MHELP_FILE_PRINT_SETUP      "D~ruckereinrichtung..."
#define STR_MHELP_FILE_EXIT             "~Beenden"

/*
* R4 Help System Edit Menu Labels
*/
#define STR_MHELP_EDIT_COPY             "~Kopieren\tStrg+C"
#define STR_MHELP_TSE_EDIT_COPY         "~Kopieren\tStrg+C"
#define STR_MHELP_EDIT_COPY_ACCEL       "C" CONTROL
#define STR_MHELP_EDIT_COPYPART         "Thema teilweise ~kopieren..."
#define STR_MHELP_EDIT_AS_WRAPPED       "Als ~Umbruch kopieren"

/*
* R4 Help System Navigation Menu Labels
*/
#define STR_MHELP_NAV_SEARCH            "~Suchen..."
#define STR_MHELP_NAV_GOTO              "~Gehe zu..."
#define STR_MHELP_NAV_MARK              "~Lesezeichen"
#define STR_MHELP_NAV_BACKLINK          "~Zurück zu Thema"
#define STR_MHELP_NAV_FORWLINK          "~Weiter zu Thema"
#define STR_MHELP_NAV_PREVPAGE          "~Vorheriges Thema <<"
#define STR_MHELP_NAV_NEXTPAGE          "~Nächstes Thema >>"

/*
* R4 Help System Help Menu Labels
*/
#define STR_MHELP_HELP_ONHELP           "~Hilfe benutzen"
#define STR_MHELP_HELP_ABOUT            "~Info..."

/*
* R4 Help System Edit Selection Menu Bar
*/
#define STR_MHELP_TSE_EDIT              "~Bearbeiten"
#define STR_MHELP_TSE_HELP              "~Hilfe"

/*
* R4 Help System Help Menu Labels
*/
#define STR_MHELP_TSE_HELP_ONHELP       "~Hilfe benutzen"

/*
* R4 Help System Help Topic Window
*/
#define STR_HELP_TOPIC_WIN              "Themenfenster"
#define STR_HELP_TOPIC_INFOLBL          "Informationen und Status"
#define STR_HELP_TOPIC_BOOKMARK         "Markieren"
#define STR_HELP_TOPIC_BACKLINK         "Zurück"
#define STR_HELP_TOPIC_SEARCH           "Suchen..."
#define STR_HELP_TOPIC_GOTO             "Gehe zu..."
#define STR_HELP_TOPIC_FORWLINK         "Vorwärts"
#define STR_HELP_TOPIC_PREVPAGE         "<<"
#define STR_HELP_TOPIC_NEXTPAGE         ">>"

/*
* R4 Help System Help Goto Dialog
*/
#define STR_HELP_GOTO_DLG               "Thema Dialog"
#define STR_HELP_GOTO_GROUP             "Optionen zu"
#define STR_HELP_GOTO_INDEX             "Index"
#define STR_HELP_GOTO_GLOSSARY          "Glossar"
#define STR_HELP_GOTO_CONTENTS          "Inhalt"
#define STR_HELP_GOTO_KEYBOARD          "Tastatur"
#define STR_HELP_GOTO_BOOK_LBL          "Lesezeichen:"
#define STR_HELP_GOTO_CANCEL            "Abbrechen"
#define STR_HELP_GOTO_BOOKMARK_BTN      "Gehe zur Markierung"

/*
* R4 Help System Help Search Dialog
*/
#define STR_HELP_SEARCH_DLG             "Thema Dialog"
#define STR_HELP_SEARCH_RADIO_LBL       "Suchoptionen"
#define STR_HELP_SEARCH_BY_TOPICNAME    "Nach Thema"
#define STR_HELP_SEARCH_BY_KEYWORD      "Nach Schlüsselwort"
#define STR_HELP_SEARCH_ITEMS_LBL       "Suchpunkte"
#define STR_HELP_SEARCH_MATCH_LBL       "Schlüsselwort-Übereinstimmungen"
#define STR_HELP_SEARCH_CANCEL          "Abbrechen"
#define STR_HELP_SEARCH_GOTO_MATCH      "Gehe zu Auswahl"

/*
* R4 Help System Help Copy Window
*/
#define STR_HELP_TOPIC_SELCOPY          "Hilfe-Kopie"
#define STR_HELP_TOPIC_SELCOPY_WIN_LBL  "Text markieren und Bearbeiten/Kopieren auswählen"

/**********Macro Definitions for Control's Rectangles*********************/
#define REC_HELP_TOPIC_WIN          HV_URL_RECT(240, 48, 376, 384)
#define REC_HELP_TOPIC_INFOLBL      HV_URL_RECT(8,  320, 280, 24)
#define REC_HELP_TOPIC_SEARCH       HV_URL_RECT(8,  352, 64,  24)
#define REC_HELP_TOPIC_GOTO         HV_URL_RECT(80, 352, 64,  24)
#define REC_HELP_TOPIC_BOOKMARK     HV_URL_RECT(152,352, 64,  24)
#define REC_HELP_TOPIC_BACKLINK     HV_URL_RECT(224,352, 64,  24)
#define REC_HELP_TOPIC_FORWLINK     HV_URL_RECT(296,352, 64,  24)
#define REC_HELP_TOPIC_PREVPAGE     HV_URL_RECT(312,320, 32,  24)
#define REC_HELP_TOPIC_NEXTPAGE     HV_URL_RECT(344,320, 32,  24)
#define REC_HELP_GOTO_DLG           HV_URL_RECT(85, 44,  252, 376)
#define REC_HELP_GOTO_GROUP         HV_URL_RECT(16, 8,   220, 168)
#define REC_HELP_GOTO_INDEX         HV_URL_RECT(70, 40,  112, 24)
#define REC_HELP_GOTO_GLOSSARY      HV_URL_RECT(70, 72,  112, 24)
#define REC_HELP_GOTO_CONTENTS      HV_URL_RECT(70, 104, 112, 24)
#define REC_HELP_GOTO_KEYBOARD      HV_URL_RECT(70, 136, 112, 24)
#define REC_HELP_GOTO_BOOK_LBL      HV_URL_RECT(16, 184, 96,  16)
#define REC_GOTO_BOOKMARK_LIST      HV_URL_RECT(16, 208, 220, 112)
#define REC_HELP_GOTO_BOOKMARK_BTN  HV_URL_RECT(8,  336, 148, 24)
#define REC_HELP_GOTO_CANCEL        HV_URL_RECT(164,336, 80,  24)
#define REC_HELP_SEARCH_DLG         HV_URL_RECT(240,96,  288, 336)
#define REC_HELP_SEARCH_RADIO_LBL   HV_URL_RECT(8,  8,   272, 56)
#define REC_HELP_SEARCH_BY_TOPICNAME HV_URL_RECT(16,32,  112, 24)
#define REC_HELP_SEARCH_BY_KEYWOR   HV_URL_RECT(128,32,  148, 24)
#define REC_HELP_SEARCH_ITEMS_LBL   HV_URL_RECT(8,  72,  272, 16)
#define REC_SEARCH_SELECT_LIST      HV_URL_RECT(8,  96,  272, 96)
#define REC_HELP_SEARCH_MATCH_LBL   HV_URL_RECT(8,  200, 272, 16)
#define REC_SEARCH_MATCH_LIST       HV_URL_RECT(8,  224, 272, 72)
#define REC_HELP_SEARCH_GOTO_MATCH  HV_URL_RECT(8,  304, 124, 24)
#define REC_HELP_SEARCH_CANCEL      HV_URL_RECT(156,304, 124, 24)

#define REC_HELP_TOPIC_SELCOPY      HV_URL_RECT(95, 249, 320, 176)
#define REC_HELP_TOPIC_SELCOPY_WIN_LBL HV_URL_RECT(0 ,0, 320, 24)


/****************************************************************************^M
    Common Code Section (strings found in V layer)
 
****************************************************************************/

#define STR_CC_NULL_ARG_STR             "NULL-Argument."
#define STR_CC_EMPTY_LIST_STR           "Liste leer."
#define STR_CC_SLIST_GET_FAIL_STR       "xvtv_slist_get bei Element %d fehlgeschlagen"
#define STR_CC_NAMED_COL_NOT_FOUND_STR  "Bezeichnete Farbe '%s' nicht gefunden.\n"
#define STR_CC_FATAL_RECURSE_STR        "NICHT BEHEBBARER FEHLER in der letztmöglichen Fehlerbehandlungsroutine\nAnwendung wird beendet"
#define STR_CC_INV_ERRMSG_STR           "Ungültiges XVT_ERRMSG-Objekt"
#define STR_CC_UNKNOWN_STR              "Unbekannt"
#define STR_CC_DEF_TWICE_STR            "Warnung. Festgelegte Zeichenkette %s und id %d doppelt.\n"

#endif /* (XVTWS == WIN16WS) || (XVTWS == WIN32WS) */
