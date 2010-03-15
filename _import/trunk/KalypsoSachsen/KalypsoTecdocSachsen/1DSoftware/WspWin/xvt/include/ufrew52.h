/****************************************************************************
 *
 *  Copyright 1987-1995 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *  $RCSfile: ufrew52.h,v $
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
#define STR_M_FILE                      "~Fichier"
#define STR_M_FILE_NEW                  "~Nouveau"
#define STR_M_FILE_OPEN                 "~Ouvrir..."
#define STR_M_FILE_CLOSE                "~Fermer"
#define STR_M_FILE_SAVE                 "~Enregistrer"
#define STR_M_FILE_SAVE_AS              "En~registrer sous..."
#define STR_M_FILE_REVERT               "Réc~upérer l'enregistrement précédent"
#define STR_M_FILE_PG_SETUP             "~Configuration de l'imprimante..."
#define STR_M_FILE_PRINT                "~Imprimer..."
#define STR_M_FILE_QUIT                 "~Quitter"

/*
* Standard Edit Menu Labels
*/
#define STR_M_EDIT                      "~Edition"
#define STR_M_EDIT_UNDO                 "~Annuler\tCtrl+Z"
#define STR_M_EDIT_CUT                  "~Couper\tCtrl+X"
#define STR_M_EDIT_COPY                 "Co~pier\tCtrl+C"
#define STR_M_EDIT_PASTE                "C~oller\tCtrl+V"
#define STR_M_EDIT_CLEAR                "~Effacer"
#define STR_M_EDIT_UNDO_ACCEL           "Z" CONTROL
#define STR_M_EDIT_CUT_ACCEL            "X" CONTROL
#define STR_M_EDIT_COPY_ACCEL           "C" CONTROL
#define STR_M_EDIT_PASTE_ACCEL          "V" CONTROL

/*
* Standard Font Menu Labels
*/
#define STR_M_FONT                      "~Police"
#define STR_M_FONT_SELECT               "~Sélectionner..."

/*
* Standard Help Menu Labels
*/
#define STR_M_HELP                      "~?"
#define STR_M_HELP_CONTENTS             "~Index"
#define STR_M_HELP_SEARCH               "~Rechercher l'Aide sur..."
#define STR_M_HELP_HELPONHELP           "~Utiliser l'Aide"
#define STR_M_HELP_OBJCLICK             "~Clic sur objet\tMaj+F1"
#define STR_M_HELP_VERSION              "~A propos..."
#define STR_M_HELP_CONTENTS_ACCEL       F1 ALT
#define STR_M_HELP_SEARCH_ACCEL         F1 CONTROL
#define STR_M_HELP_OBJCLICK_ACCEL       F1 SHIFT

/*
* Windows MDI Menus
*/
#define STR_XVT_WINDOWMENU              "&Fenêtre"
#define STR_IDM_WINDOWTILE              "&Mosaïque"
#define STR_IDM_WINDOWCASCADE           "&Cascade"
#define STR_IDM_WINDOWICONS             "&Réorganiser icônes"

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
#define STR_DB_ABORT_CANCEL             "Annuler"
#define STR_DB_ABORT_PRINTING           "Impression"
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
#define STR_DB_RESPONSE_CANCEL          "Annuler"

/*
* Standard About Dialog
*/
#define STR_DB_ABOUT                    ""
#define STR_DB_ABOUT_OK                 "Aide"
#define STR_DB_ABOUT_CANCEL             "Annuler"
#define STR_DB_ABOUT_TEXT1              "Une application XVT"
#define STR_DB_ABOUT_TEXT2              "Version 4.57"
#define STR_DB_ABOUT_TEXT3              "par XVT Software."


/****************************************************************************
    Platform Specific String Section
 

****************************************************************************/

#define STR_WIN_HELPTYPE                "HLP"
/* strings used in bothstr.h and as default string in SGETSTR */
#define STR_WIN_OK                      "OK"
#define STR_WIN_CANCEL                  "Annuler"
#define STR_WIN_CANT_SHOW_ABOUT_BOX     "Impossible d'afficher la boîte A propos"
#define STR_WIN_CANCELLING              "Annulation..."
#define STR_WIN_INITIALIZING            "Initialisation"
#define STR_WIN_UNTITLED                "(Sans titre)"
#define STR_WIN_STARTING_DOC            "(Lancement du document)"
#define STR_WIN_STARTED_DOC             "(Document lancé) "
#define STR_WIN_ENDING_DOC              "(Fin du document)"
    /* I18N - This string contains a message from the print dialog. */
    /*        The %d represents the page number of the current printing page */
#define STR_WIN_STARTING_PAGE           "(Lancement de la page %d)"
    /* I18N - This string contains a message from the print dialog. */
    /*        The %d represents the page number of the finished printing page */
#define STR_WIN_FINISHING_PAGE          "(Fin de la page %d)"
#define STR_WIN_SYSTEM_ERROR            "Erreur système"
#define STR_WIN_PICTURE_STILL_LOCKED    "Image encore verrouillée."
#define STR_WIN_CANNOT_PRINT            "Impossible d'imprimer"
#define STR_WIN_ALERT                   "Alerte"
#define STR_WIN_FDLG_FILTER             "Fichiers (*.%s)|*.%s|"
#define STR_WIN_FDLG_FILTER_ALL         "Tous les fichiers (*.*)|*.*|"
#define STR_WIN_NO_EXT_RES              "Fichier ressources introuvable. Utilisation des ressources par défaut"
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
#define STR_HELP_CLIP_ERR               "Impossible d'ouvrir le Presse-papiers pour y écrire"
#define STR_HELP_MEM_ERR                "Mémoire insuffisante"
#define STR_HELP_CLIP_PUT_ERR           "Impossible de mettre des données dans le Presse-papiers"
#define STR_HELP_PRINT_ERR              "Echec de l'impression."
#define STR_HELP_PRINT_OK               "Rubrique imprimée"
#define STR_HELP_CLIP_OK                "Rubrique copiée dans le Presse-papiers."
    /* I18N - This string is used to signify the topic sequence number in */
    /*        the help viewer */
    /*        The first %d represents which topic in a sequence of topics */
    /*        that is currently being viewed */
    /*        The second %d represents the total number of topics that have */
    /*        been viewed */
#define STR_HELP_THREAD_INFO            "Rubrique : %d de %d"
    /* I18N - This string is used to show if a topic has been marked */
    /*        (bookmarks) */
    /*        The %c represents a the single byte character 'x' */
    /*        or ' ' (space). */
    /*        This means the item is either marked (x) or unmarked ( ). */
#define STR_HELP_MARKED_INFO            "Marquée : [%c]"
#define STR_HELP_COPYPART               "Aucune sélection : rien à copier"

/*
* R4 Help System Topic Button Labels
*/
#define STR_HELP_MARK                   "Marquer"
#define STR_HELP_UNMARK                 "Annuler marquage"

/*
* R4 Help System Topic Navigation Menu
*/
#define STR_HELP_NAV_SEARCH             SEARCH_STR
#define STR_HELP_NAV_GOTO               "~Aller à..."
#define STR_HELP_NAV_MARK               "Définir ~signet"
#define STR_HELP_NAV_BACKLINK           "~Reculer à la rubrique"
#define STR_HELP_NAV_FORWLINK           "~Avancer à la rubrique"
#define STR_HELP_NAV_PREVPAGE           "Rubrique ~précédente <<"
#define STR_HELP_NAV_NEXTPAGE           "Rubrique ~suivante >>"

/*
* R4 Help System Menu Bar
*/
#define STR_MHELP_FILE                  "~Fichier"
#define STR_MHELP_EDIT                  "~Editer"
#define STR_MHELP_NAV                   "~Naviguer"
#define STR_MHELP_HELP                  "~Aide"

/*
* R4 Help System File Menu Labels
*/
#define STR_MHELP_FILE_PRINT            "~Imprimer"
#define STR_MHELP_FILE_PRINT_SETUP      "~Configuration de l'imprimante..."
#define STR_MHELP_FILE_EXIT             "~Quitter"

/*
* R4 Help System Edit Menu Labels
*/
#define STR_MHELP_EDIT_COPY             "~Copier\tCtrl+C"
#define STR_MHELP_TSE_EDIT_COPY         "~Copier\tCtrl+C"
#define STR_MHELP_EDIT_COPY_ACCEL       "C" CONTROL
#define STR_MHELP_EDIT_COPYPART         "~Copier une partie de rubrique..."
#define STR_MHELP_EDIT_AS_WRAPPED       "Copier avec renvois à la ligne"

/*
* R4 Help System Navigation Menu Labels
*/
#define STR_MHELP_NAV_SEARCH            "~Rechercher..."
#define STR_MHELP_NAV_GOTO              "~Aller à..."
#define STR_MHELP_NAV_MARK              "~Signet"
#define STR_MHELP_NAV_BACKLINK          "R~eculer à la rubrique"
#define STR_MHELP_NAV_FORWLINK          "A~vancer à la rubrique"
#define STR_MHELP_NAV_PREVPAGE          "Rubrique ~précédente <<"
#define STR_MHELP_NAV_NEXTPAGE          "Rubrique ~suivante >>"

/*
* R4 Help System Help Menu Labels
*/
#define STR_MHELP_HELP_ONHELP           "~Utiliser l'Aide"
#define STR_MHELP_HELP_ABOUT            "~A propos de l'Aide"

/*
* R4 Help System Edit Selection Menu Bar
*/
#define STR_MHELP_TSE_EDIT              "~Edition"
#define STR_MHELP_TSE_HELP              "~Aide"

/*
* R4 Help System Help Menu Labels
*/
#define STR_MHELP_TSE_HELP_ONHELP       "~Utiliser l'Aide"

/*
* R4 Help System Help Topic Window
*/
#define STR_HELP_TOPIC_WIN              "fenêtre de rubriques"
#define STR_HELP_TOPIC_INFOLBL          "Informations et états"
#define STR_HELP_TOPIC_BOOKMARK         "Marquer"
#define STR_HELP_TOPIC_BACKLINK         "Reculer"
#define STR_HELP_TOPIC_SEARCH           "Rechercher..."
#define STR_HELP_TOPIC_GOTO             "Aller à..."
#define STR_HELP_TOPIC_FORWLINK         "Avancer"
#define STR_HELP_TOPIC_PREVPAGE         "<<"
#define STR_HELP_TOPIC_NEXTPAGE         ">>"

/*
* R4 Help System Help Goto Dialog
*/
#define STR_HELP_GOTO_DLG               "Aller à une rubrique"
#define STR_HELP_GOTO_GROUP             "Options Aller à"
#define STR_HELP_GOTO_INDEX             "Index"
#define STR_HELP_GOTO_GLOSSARY          "Glossaire"
#define STR_HELP_GOTO_CONTENTS          "Sommaire"
#define STR_HELP_GOTO_KEYBOARD          "Clavier"
#define STR_HELP_GOTO_BOOK_LBL          "Signets :"
#define STR_HELP_GOTO_CANCEL            "Annuler"
#define STR_HELP_GOTO_BOOKMARK_BTN      "Aller à marque"

/*
* R4 Help System Help Search Dialog
*/
#define STR_HELP_SEARCH_DLG             "Recherche une rubrique"
#define STR_HELP_SEARCH_RADIO_LBL       "Options de recherche"
#define STR_HELP_SEARCH_BY_TOPICNAME    "Par rubrique"
#define STR_HELP_SEARCH_BY_KEYWORD      "Par mot clé"
#define STR_HELP_SEARCH_ITEMS_LBL       "Rechercher éléments"
#define STR_HELP_SEARCH_MATCH_LBL       "Correspondance mot clé"
#define STR_HELP_SEARCH_CANCEL          "Annuler"
#define STR_HELP_SEARCH_GOTO_MATCH      "Aller à sélection"

/*
* R4 Help System Help Copy Window
*/
#define STR_HELP_TOPIC_SELCOPY          "Copier l'Aide"
#define STR_HELP_TOPIC_SELCOPY_WIN_LBL  "Sélectionner texte et choisir Editer/Copier"

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
#define REC_HELP_GOTO_DLG           HV_URL_RECT(85, 44,  252, 376)
#define REC_HELP_GOTO_GROUP         HV_URL_RECT(16, 8,   220, 168)
#define REC_HELP_GOTO_INDEX         HV_URL_RECT(70, 40,  112, 24)
#define REC_HELP_GOTO_GLOSSARY      HV_URL_RECT(70, 72,  112, 24)
#define REC_HELP_GOTO_CONTENTS      HV_URL_RECT(70, 104, 112, 24)
#define REC_HELP_GOTO_KEYBOARD      HV_URL_RECT(70, 136, 112, 24)
#define REC_HELP_GOTO_BOOK_LBL      HV_URL_RECT(16, 184, 96,  16)
#define REC_GOTO_BOOKMARK_LIST      HV_URL_RECT(16, 208, 220, 112)
#define REC_HELP_GOTO_BOOKMARK_BTN  HV_URL_RECT(16, 336, 128, 24)
#define REC_HELP_GOTO_CANCEL        HV_URL_RECT(156,336, 80,  24)
#define REC_HELP_SEARCH_DLG         HV_URL_RECT(240,96,  288, 336)
#define REC_HELP_SEARCH_RADIO_LBL   HV_URL_RECT(8,  8,   272, 56)
#define REC_HELP_SEARCH_BY_TOPICNAME HV_URL_RECT(24,32,  112, 24)
#define REC_HELP_SEARCH_BY_KEYWOR   HV_URL_RECT(136,32,  112, 24)
#define REC_HELP_SEARCH_ITEMS_LBL   HV_URL_RECT(8,  72,  272, 16)
#define REC_SEARCH_SELECT_LIST      HV_URL_RECT(8,  96,  272, 96)
#define REC_HELP_SEARCH_MATCH_LBL   HV_URL_RECT(8,  200, 272, 16)
#define REC_SEARCH_MATCH_LIST       HV_URL_RECT(8,  224, 272, 72)
#define REC_HELP_SEARCH_GOTO_MATCH  HV_URL_RECT(16, 304, 116, 24)
#define REC_HELP_SEARCH_CANCEL      HV_URL_RECT(148,304, 116, 24)

#define REC_HELP_TOPIC_SELCOPY      HV_URL_RECT(95, 249, 320, 176)
#define REC_HELP_TOPIC_SELCOPY_WIN_LBL HV_URL_RECT(0 ,0, 320, 24)


/****************************************************************************
    Common Code Section (strings found in V layer)
 
****************************************************************************/

#define STR_CC_NULL_ARG_STR             "Argument NULL"
#define STR_CC_EMPTY_LIST_STR           "Liste vide."
#define STR_CC_SLIST_GET_FAIL_STR       "Echec de xvtv_slist_get sur élément %d"
#define STR_CC_NAMED_COL_NOT_FOUND_STR  "Couleur '%s' indiquée introuvable.\n"
#define STR_CC_FATAL_RECURSE_STR        "ERREUR IRRECUPERABLE pendant traitement d'erreur de dernier recours.\NSortie de l'application"
#define STR_CC_INV_ERRMSG_STR           "Objet XVT_ERRMSG incorrect"
#define STR_CC_UNKNOWN_STR              "inconnu"
#define STR_CC_DEF_TWICE_STR            "Avertissement. Chaîne %s et id %d définies deux fois.\n"

#endif /* (XVTWS == WIN16WS) || (XVTWS == WIN32WS) */
