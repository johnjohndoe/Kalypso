/****************************************************************************
 *
 *  Copyright 1987-1995 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *  $RCSfile: uitaw52.h,v $
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
#define STR_M_FILE                      "~File"
#define STR_M_FILE_NEW                  "~Nuovo"
#define STR_M_FILE_OPEN                 "~Apri..."
#define STR_M_FILE_CLOSE                "~Chiudi"
#define STR_M_FILE_SAVE                 "~Salva"
#define STR_M_FILE_SAVE_AS              "Sal~va con nome..."
#define STR_M_FILE_REVERT               "~Ripristina"
#define STR_M_FILE_PG_SETUP             "~Imposta stampante..."
#define STR_M_FILE_PRINT                "Sta~mpa..."
#define STR_M_FILE_QUIT                 "~Esci"

/*
* Standard Edit Menu Labels
*/
#define STR_M_EDIT                      "~Modifica"
#define STR_M_EDIT_UNDO                 "~Annulla\tCtrl+Z"
#define STR_M_EDIT_CUT                  "Ta~glia\tCtrl+X"
#define STR_M_EDIT_COPY                 "Co~pia\tCtrl+C"
#define STR_M_EDIT_PASTE                "I~ncolla\tCtrl+V"
#define STR_M_EDIT_CLEAR                "E~limina"
#define STR_M_EDIT_UNDO_ACCEL           "Z" CONTROL
#define STR_M_EDIT_CUT_ACCEL            "X" CONTROL
#define STR_M_EDIT_COPY_ACCEL           "C" CONTROL
#define STR_M_EDIT_PASTE_ACCEL          "V" CONTROL

/*
* Standard Font Menu Labels
*/
#define STR_M_FONT                      "~Carattere"
#define STR_M_FONT_SELECT               "~Seleziona..."

/*
* Standard Help Menu Labels
*/
#define STR_M_HELP                      "~?"
#define STR_M_HELP_CONTENTS             "~Sommario"
#define STR_M_HELP_SEARCH               "~Cerca argomento..."
#define STR_M_HELP_HELPONHELP           "~Uso della Guida"
#define STR_M_HELP_OBJCLICK             "Selezione ~oggetto\tShift+F1"
#define STR_M_HELP_VERSION              "~Inform. su..."
#define STR_M_HELP_CONTENTS_ACCEL       F1 ALT
#define STR_M_HELP_SEARCH_ACCEL         F1 CONTROL
#define STR_M_HELP_OBJCLICK_ACCEL       F1 SHIFT

/*
* Windows MDI Menus
*/
#define STR_XVT_WINDOWMENU              "&Finestra"
#define STR_IDM_WINDOWTILE              "&Affianca"
#define STR_IDM_WINDOWCASCADE           "Sovra&pponi"
#define STR_IDM_WINDOWICONS             "&Disponi icone"

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
#define STR_DB_ABORT_CANCEL             "Annulla"
#define STR_DB_ABORT_PRINTING           "Stampa"
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
#define STR_DB_RESPONSE_CANCEL          "Annulla"

/*
* Standard About Dialog
*/
#define STR_DB_ABOUT                    ""
#define STR_DB_ABOUT_OK                 "Guida"
#define STR_DB_ABOUT_CANCEL             "Annulla"
#define STR_DB_ABOUT_TEXT1              "Un'applicazione XVT"
#define STR_DB_ABOUT_TEXT2              "versione 4.57"
#define STR_DB_ABOUT_TEXT3              "prodotto da XVT Software."


/****************************************************************************
    Platform Specific String Section
 

****************************************************************************/

#define STR_WIN_HELPTYPE                "HLP"
/* strings used in bothstr.h and as default string in SGETSTR */
#define STR_WIN_OK                      "OK"
#define STR_WIN_CANCEL                  "Annulla"
#define STR_WIN_CANT_SHOW_ABOUT_BOX     "Impossibile visualizzare la finestra Inform. su"
#define STR_WIN_CANCELLING              "Annullamento in corso..."
#define STR_WIN_INITIALIZING            "Inizializzazione"
#define STR_WIN_UNTITLED                "(Senza nome)"
#define STR_WIN_STARTING_DOC            "(Documento in fase di avvio)"
#define STR_WIN_STARTED_DOC             "(Documento avviato)"
#define STR_WIN_ENDING_DOC              "(Documento concluso)"
    /* I18N - This string contains a message from the print dialog. */
    /*        The %d represents the page number of the current printing page */
#define STR_WIN_STARTING_PAGE           "(Inizio pagina %d)"
    /* I18N - This string contains a message from the print dialog. */
    /*        The %d represents the page number of the finished printing page */
#define STR_WIN_FINISHING_PAGE          "(Termine pagina %d)"
#define STR_WIN_SYSTEM_ERROR            "Errore di sistema"
#define STR_WIN_PICTURE_STILL_LOCKED    "Immagine ancora bloccata."
#define STR_WIN_CANNOT_PRINT            "Impossibile stampare"
#define STR_WIN_ALERT                   "Avviso"
#define STR_WIN_FDLG_FILTER             "File (*.%s)|*.%s|"
#define STR_WIN_FDLG_FILTER_ALL         "Tutti i file (*.*)|*.*|"
#define STR_WIN_NO_EXT_RES              "Impossibile trovare il file di risorsa - verranno usate le risorse predefinite"
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
#define STR_HELP_CLIP_ERR               "Impossibile aprire gli Appunti per scrivere"
#define STR_HELP_MEM_ERR                "Memoria esaurita"
#define STR_HELP_CLIP_PUT_ERR           "Impossibile inserire informazioni negli Appunti"
#define STR_HELP_PRINT_ERR              "Stampa non riuscita."
#define STR_HELP_PRINT_OK               "Argomento stampato"
#define STR_HELP_CLIP_OK                "Argomento copiato negli Appunti."
    /* I18N - This string is used to signify the topic sequence number in */
    /*        the help viewer */
    /*        The first %d represents which topic in a sequence of topics */
    /*        that is currently being viewed */
    /*        The second %d represents the total number of topics that have */
    /*        been viewed */
#define STR_HELP_THREAD_INFO            "Argomento: %d di %d"
    /* I18N - This string is used to show if a topic has been marked */
    /*        (bookmarks) */
    /*        The %c represents a the single byte character 'x' */
    /*        or ' ' (space). */
    /*        This means the item is either marked (x) or unmarked ( ). */
#define STR_HELP_MARKED_INFO            "Contrassegnato: [%c]"
#define STR_HELP_COPYPART               "Nessuna selezione eseguita - niente da copiare"

/*
* R4 Help System Topic Button Labels
*/
#define STR_HELP_MARK                   "Contrassegna"
#define STR_HELP_UNMARK                 "Elimina contrassegno"

/*
* R4 Help System Topic Navigation Menu
*/
#define STR_HELP_NAV_SEARCH             SEARCH_STR
#define STR_HELP_NAV_GOTO               "~Vai a..."
#define STR_HELP_NAV_MARK               "~Imposta segnalibro"
#define STR_HELP_NAV_BACKLINK           "~Torna all'argomento"
#define STR_HELP_NAV_FORWLINK           "Vai all'~argomento"
#define STR_HELP_NAV_PREVPAGE           "Argomento ~precedente  <<"
#define STR_HELP_NAV_NEXTPAGE           "Argomento ~successivo  <<"

/*
* R4 Help System Menu Bar
*/
#define STR_MHELP_FILE                  "~File"
#define STR_MHELP_EDIT                  "~Modifica"
#define STR_MHELP_NAV                   "~Naviga"
#define STR_MHELP_HELP                  "~Guida"

/*
* R4 Help System File Menu Labels
*/
#define STR_MHELP_FILE_PRINT            "~Stampa"
#define STR_MHELP_FILE_PRINT_SETUP      "~Imposta stampante..."
#define STR_MHELP_FILE_EXIT             "~Esci"

/*
* R4 Help System Edit Menu Labels
*/
#define STR_MHELP_EDIT_COPY             "~Copia\tCtrl+C"
#define STR_MHELP_TSE_EDIT_COPY         "~Copia\tCtrl+C"
#define STR_MHELP_EDIT_COPY_ACCEL       "C" CONTROL
#define STR_MHELP_EDIT_COPYPART         "~Copia parte dell'argomento..."
#define STR_MHELP_EDIT_AS_WRAPPED       "Copia come ~impaginato"

/*
* R4 Help System Navigation Menu Labels
*/
#define STR_MHELP_NAV_SEARCH            "~Cerca..."
#define STR_MHELP_NAV_GOTO              "~Vai a..."
#define STR_MHELP_NAV_MARK              "Segna~libro"
#define STR_MHELP_NAV_BACKLINK          "~Torna all'argomento"
#define STR_MHELP_NAV_FORWLINK          "Vai all'~argomento"
#define STR_MHELP_NAV_PREVPAGE          "Argomento ~precedente  <<"
#define STR_MHELP_NAV_NEXTPAGE          "Argomento ~successivo  <<"

/*
* R4 Help System Help Menu Labels
*/
#define STR_MHELP_HELP_ONHELP           "~Uso della Guida"
#define STR_MHELP_HELP_ABOUT            "~Inform. sulla Guida"

/*
* R4 Help System Edit Selection Menu Bar
*/
#define STR_MHELP_TSE_EDIT              "~Modifica"
#define STR_MHELP_TSE_HELP              "~Guida"

/*
* R4 Help System Help Menu Labels
*/
#define STR_MHELP_TSE_HELP_ONHELP       "~Uso della Guida"

/*
* R4 Help System Help Topic Window
*/
#define STR_HELP_TOPIC_WIN              "finestra dell'argomento"
#define STR_HELP_TOPIC_INFOLBL          "Informazioni e stato"
#define STR_HELP_TOPIC_BOOKMARK         "Contrassegna"
#define STR_HELP_TOPIC_BACKLINK         "Indietro"
#define STR_HELP_TOPIC_SEARCH           "Cerca..."
#define STR_HELP_TOPIC_GOTO             "Vai a..."
#define STR_HELP_TOPIC_FORWLINK         "Avanti"
#define STR_HELP_TOPIC_PREVPAGE         "<<"
#define STR_HELP_TOPIC_NEXTPAGE         ">>"

/*
* R4 Help System Help Goto Dialog
*/
#define STR_HELP_GOTO_DLG               "Finestra di dialogo Vai all'argomento"
#define STR_HELP_GOTO_GROUP             "Vai alle opzioni"
#define STR_HELP_GOTO_INDEX             "Indice"
#define STR_HELP_GOTO_GLOSSARY          "Glossario"
#define STR_HELP_GOTO_CONTENTS          "Sommario"
#define STR_HELP_GOTO_KEYBOARD          "Tastiera"
#define STR_HELP_GOTO_BOOK_LBL          "Segnalibri:"
#define STR_HELP_GOTO_CANCEL            "Annulla"
#define STR_HELP_GOTO_BOOKMARK_BTN      "Vai al contrassegno"

/*
* R4 Help System Help Search Dialog
*/
#define STR_HELP_SEARCH_DLG             "Finestra di dialogo Ricerca argomento"
#define STR_HELP_SEARCH_RADIO_LBL       "Opzioni di ricerca"
#define STR_HELP_SEARCH_BY_TOPICNAME    "Per argomento"
#define STR_HELP_SEARCH_BY_KEYWORD      "Per parola chiave"
#define STR_HELP_SEARCH_ITEMS_LBL       "Cerca elementi"
#define STR_HELP_SEARCH_MATCH_LBL       "Corrispondenze di parole chiave"
#define STR_HELP_SEARCH_CANCEL          "Annulla"
#define STR_HELP_SEARCH_GOTO_MATCH      "Vai alla selezione"

/*
* R4 Help System Help Copy Window
*/
#define STR_HELP_TOPIC_SELCOPY          "Guida di Copia"
#define STR_HELP_TOPIC_SELCOPY_WIN_LBL  "Selezionare il testo, quindi Modifica/Copia"

/**********Macro Definitions for Control's Rectangles********************/
#define REC_HELP_TOPIC_WIN          HV_URL_RECT(240, 48, 376, 384)
#define REC_HELP_TOPIC_INFOLBL      HV_URL_RECT(8,  320, 280, 24)
#define REC_HELP_TOPIC_SEARCH       HV_URL_RECT(8,  352, 64,  24)
#define REC_HELP_TOPIC_GOTO         HV_URL_RECT(80, 352, 64,  24)
#define REC_HELP_TOPIC_BOOKMARK     HV_URL_RECT(152,352, 64,  24)
#define REC_HELP_TOPIC_BACKLINK     HV_URL_RECT(224,352, 64,  24)
#define REC_HELP_TOPIC_FORWLINK     HV_URL_RECT(296,352, 64,  24)
#define REC_HELP_TOPIC_PREVPAGE     HV_URL_RECT(312,320, 32,  24)
#define REC_HELP_TOPIC_NEXTPAGE     HV_URL_RECT(344,320, 32,  24)
#define REC_HELP_GOTO_DLG           HV_URL_RECT(85, 44,  264, 376)
#define REC_HELP_GOTO_GROUP         HV_URL_RECT(22, 8,   220, 168)
#define REC_HELP_GOTO_INDEX         HV_URL_RECT(76, 40,  112, 24)
#define REC_HELP_GOTO_GLOSSARY      HV_URL_RECT(76, 72,  112, 24)
#define REC_HELP_GOTO_CONTENTS      HV_URL_RECT(76, 104, 112, 24)
#define REC_HELP_GOTO_KEYBOARD      HV_URL_RECT(76, 136, 112, 24)
#define REC_HELP_GOTO_BOOK_LBL      HV_URL_RECT(22, 184, 96,  16)
#define REC_GOTO_BOOKMARK_LIST      HV_URL_RECT(22, 208, 220, 112)
#define REC_HELP_GOTO_BOOKMARK_BTN  HV_URL_RECT(8,  336, 150, 24)
#define REC_HELP_GOTO_CANCEL        HV_URL_RECT(176,336, 80,  24)
#define REC_HELP_SEARCH_DLG         HV_URL_RECT(240,96,  288, 336)
#define REC_HELP_SEARCH_RADIO_LBL   HV_URL_RECT(8,  8,   272, 56)
#define REC_HELP_SEARCH_BY_TOPICNAME HV_URL_RECT(16,32,  112, 24)
#define REC_HELP_SEARCH_BY_KEYWOR   HV_URL_RECT(140,32,  136, 24)
#define REC_HELP_SEARCH_ITEMS_LBL   HV_URL_RECT(8,  72,  272, 16)
#define REC_SEARCH_SELECT_LIST      HV_URL_RECT(8,  96,  272, 96)
#define REC_HELP_SEARCH_MATCH_LBL   HV_URL_RECT(8,  200, 272, 16)
#define REC_SEARCH_MATCH_LIST       HV_URL_RECT(8,  224, 272, 72)
#define REC_HELP_SEARCH_GOTO_MATCH  HV_URL_RECT(8,  304, 128, 24)
#define REC_HELP_SEARCH_CANCEL      HV_URL_RECT(152,304, 128, 24)

#define REC_HELP_TOPIC_SELCOPY      HV_URL_RECT(95, 249, 320, 176)
#define REC_HELP_TOPIC_SELCOPY_WIN_LBL HV_URL_RECT(0 ,0, 320, 24)


/****************************************************************************
    Common Code Section (strings found in V layer)
 
****************************************************************************/

#define STR_CC_NULL_ARG_STR             "Argomento NULLO."
#define STR_CC_EMPTY_LIST_STR           "Elenco vuoto."
#define STR_CC_SLIST_GET_FAIL_STR       "xvtv_slist_get non riuscito sull'elemento %d"
#define STR_CC_NAMED_COL_NOT_FOUND_STR  "Colore '%s' non trovato.\n"
#define STR_CC_FATAL_RECURSE_STR        "ERRORE FATALE all'ultimo tentativo del gestore di errore\napplicazione terminata"
#define STR_CC_INV_ERRMSG_STR           "Oggetto XVT_ERRMSG non valido"
#define STR_CC_UNKNOWN_STR              "ignoto"
#define STR_CC_DEF_TWICE_STR            "Attenzione. Stringa %s e id %d definiti due volte.\n"

#endif /* (XVTWS == WIN16WS) || (XVTWS == WIN32WS) */
