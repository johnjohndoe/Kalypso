/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *	$RCSfile: xvt_help.h,v $ 
 *  $Revision: 1.7 $
 *
 *  Purpose: XVT help subsystem macros and types.
 *
 ****************************************************************************/

#ifndef XVT_INCL_HELP
#define XVT_INCL_HELP


/*
*	Help Versions
*/
#define XVT_HELP_VERSION_MAJOR  4
#define XVT_HELP_VERSION_MINOR  57
#define XVT_HELP_VERSION_PATCH  0
#define XVT_HELP_VERSION  XVT_MAKE_VERSION(XVT_HELP_VERSION_MAJOR,XVT_HELP_VERSION_MINOR,XVT_HELP_VERSION_PATCH)



/*
* Types
*/

typedef struct s_xvt_help_info {long*  fake;}  *XVT_HELP_INFO;
#define NULL_HELP_INFO  (XVT_HELP_INFO)0


typedef enum e_xvt_help_flavor
{
	XVT_HELP_FLAVOR_NONE,
	XVT_HELP_FLAVOR_NTVSRV,
	XVT_HELP_FLAVOR_NTVBND,
	XVT_HELP_FLAVOR_PORTSRV,
	XVT_HELP_FLAVOR_PORTBND
} XVT_HELP_FLAVOR;



/*
* Help System Flags.
*/

/* for xvt_help_open_helpfile */
#define HSF_INDEX_ON_DISK		0x001L	/* Default value is in-memory */
#define HSF_NO_TOPIC_WARNING	0x002L	/* No warning for missing topics */
#define HSF_NO_HELPMENU_ASSOC	0x004L	/* Don't associate topics to helpmenu */
#define HSF_APPNAME_TITLE		0x008L	/* show APPNAME in title */
#define HSF_NO_BEEP_MODAL  		0x010L	/* don't beep for help on modal dialog*/

/* internal use, only */
#define HSF_EXIT_ON_CLOSE   	0x020L   /* exit when topic window closes */

/* for customization */
#define HSF_USER   	            0x040L



/*
* function prototypes
*/

extern   XVT_HELP_INFO                          XVT_CALLCONV1
         xvt_help_open_helpfile                 XVT_CALLCONV2
         (
            FILE_SPEC*  hfile,
            unsigned long  flags
         );

extern   void                                   XVT_CALLCONV1
         xvt_help_close_helpfile                XVT_CALLCONV2
         (
            XVT_HELP_INFO  hi
         );


extern   void                                   XVT_CALLCONV1
         xvt_help_display_topic                 XVT_CALLCONV2
         (
            XVT_HELP_INFO  hi,
            XVT_HELP_TID  topic
         );


extern   void                                   XVT_CALLCONV1
         xvt_help_search_topic                  XVT_CALLCONV2
         (
            XVT_HELP_INFO  hi,
            XVT_HELP_TID  topic,
            char  *pattern
         );


extern   void                                   XVT_CALLCONV1
         xvt_help_set_menu_assoc                XVT_CALLCONV2
         (
            XVT_HELP_INFO  hi,
            WINDOW  win,
            MENU_TAG  tag,
            XVT_HELP_TID  topic,
            unsigned long  flags
         );


extern   XVT_HELP_TID                          XVT_CALLCONV1
         xvt_help_get_menu_assoc               XVT_CALLCONV2
         (
            XVT_HELP_INFO  hi,
            WINDOW  win,
            MENU_TAG  tag
         );


extern   void                                  XVT_CALLCONV1
         xvt_help_set_win_assoc                XVT_CALLCONV2
         (
            XVT_HELP_INFO  hi,
            WINDOW  win,
            XVT_HELP_TID  topic,
            unsigned long  flags
         );


extern   XVT_HELP_TID                          XVT_CALLCONV1
         xvt_help_get_win_assoc                XVT_CALLCONV2
         (
            XVT_HELP_INFO  hi,
            WINDOW  win
         );


extern   void                                  XVT_CALLCONV1
         xvt_help_assoc_all                    XVT_CALLCONV2
         (
            XVT_HELP_INFO  hi,
            WINDOW  win,
            long  rid,
            WIN_DEF  *wdef
         );


extern   void                                  XVT_CALLCONV1
         xvt_help_disassoc_all                 XVT_CALLCONV2
         (
            XVT_HELP_INFO  hi,
            WINDOW  win
         );


extern   void                                  XVT_CALLCONV1
         xvt_help_begin_objclick               XVT_CALLCONV2
         (
            XVT_HELP_INFO  hi,
            WINDOW  win,
            unsigned long  flags
         );


extern   void                                  XVT_CALLCONV1
         xvt_help_end_objclick                 XVT_CALLCONV2
         (
            XVT_HELP_INFO  hi
         );


extern   BOOLEAN                              XVT_CALLCONV1
         xvt_help_process_event               XVT_CALLCONV2
         (
            XVT_HELP_INFO  hi,
            WINDOW  win,
            EVENT  *ev
         );


extern   XVT_HELP_FLAVOR                      XVT_CALLCONV1
         xvt_help_get_flavor                  XVT_CALLCONV2
         (
            void
         );


/*
* Reserved help topic IDs
*/
#define XVT_TPC_BASE			32000
#define XVT_TPC_HELPONHELP		(XVT_TPC_BASE +  0)
#define XVT_TPC_INDEX			(XVT_TPC_BASE +  1)
#define XVT_TPC_TUTORIAL		(XVT_TPC_BASE +  2)
#define XVT_TPC_BASICSKILLS		(XVT_TPC_BASE +  3)
#define XVT_TPC_PROCEDURES		(XVT_TPC_BASE +  4)
#define XVT_TPC_KEYBOARD		(XVT_TPC_BASE +  5)
#define XVT_TPC_CONTENTS		(XVT_TPC_BASE +  6)
#define XVT_TPC_ABOUT			(XVT_TPC_BASE +  7)
#define XVT_TPC_COMMANDS		(XVT_TPC_BASE +  8)
#define XVT_TPC_GLOSSARY		(XVT_TPC_BASE +  9)
#define XVT_TPC_ABOUTHELP		(XVT_TPC_BASE + 10)

/* Motif specific */
#define XVT_TPC_ONHELP			XVT_TPC_HELPONHELP
#define XVT_TPC_ONKEYS			XVT_TPC_KEYBOARD
#define XVT_TPC_ONVERSION		XVT_TPC_ABOUT

/* Other help menu item topics */
#define XVT_TPC_ONCONTEXT		(XVT_TPC_BASE + 20)
#define XVT_TPC_SEARCH			(XVT_TPC_BASE + 21)
#define XVT_TPC_ONWINDOW		(XVT_TPC_BASE + 22)
#define XVT_TPC_OBJCLICK		(XVT_TPC_BASE + 23)

/* predefined dialog topics */
#define XVT_TPC_FILE_OPEN     (XVT_TPC_BASE + 30)
#define XVT_TPC_FILE_SAVE     (XVT_TPC_BASE + 40)
#define XVT_TPC_ASK           (XVT_TPC_BASE + 50)
#define XVT_TPC_NOTE          (XVT_TPC_BASE + 60)
#define XVT_TPC_ERROR         (XVT_TPC_BASE + 70)
#define XVT_TPC_WARNING       (XVT_TPC_BASE + 80)
#define XVT_TPC_STRING_PROMPT (XVT_TPC_BASE + 90)
#define XVT_TPC_FONT_SEL      (XVT_TPC_BASE + 100)
#define XVT_TPC_PAGE_SETUP    (XVT_TPC_BASE + 110)
#define XVT_TPC_MESSAGE       (XVT_TPC_BASE + 120)
#define XVT_TPC_FATAL         (XVT_TPC_BASE + 130)

/* maximum predefined topic */
#define XVT_TPC_MAX			  (XVT_TPC_BASE + 130)


/*
*	Resource-related macros.
*/


/* 
*	Help Menu Tags
*/

#define TagBASE					M_HELP
#define M_HELP_HELPMENU			(TagBASE +  0)
#define M_HELP_ONCONTEXT		(TagBASE +  1)
#define M_HELP_HELPONHELP		(TagBASE +  2)
#define M_HELP_ONWINDOW			(TagBASE +  3)
#define M_HELP_KEYBOARD			(TagBASE +  4)
#define M_HELP_INDEX			(TagBASE +  5)
#define M_HELP_TUTORIAL			(TagBASE +  6)
#define M_HELP_SEARCH			(TagBASE +  7)
#define M_HELP_OBJCLICK			(TagBASE +  8)
#define M_HELP_VERSION			(TagBASE +  9)
#define M_HELP_GOTO				(TagBASE + 10)
#define M_HELP_GLOSSARY			(TagBASE + 11)
#define M_HELP_CONTENTS			(TagBASE + 12)
/* internal use -- highest help tag */
#define M_HELP_LAST				(TagBASE + 12)


#ifndef NO_HELP_RESOURCES

/*
* Help menu text strings.
*
* (Because of the Mac, these are numbered using even values.)
*/
#define HELP_STR_BASE		XVTV_STRING_RES_BASE
#define TextHELPMENU		(HELP_STR_BASE +   0)
#define TextONWINDOW		(HELP_STR_BASE +   2)
#define TextHELPONHELP		(HELP_STR_BASE +   4)
#define TextKEYBOARD		(HELP_STR_BASE +   6)
#define TextINDEX			(HELP_STR_BASE +   8)
#define TextCONTENTS		(HELP_STR_BASE +  10)
#define TextTUTORIAL		(HELP_STR_BASE +  12)
#define TextVERSION			(HELP_STR_BASE +  14)
#define TextSEARCH			(HELP_STR_BASE +  16)
#define TextONCONTEXT		(HELP_STR_BASE +  18)
#define TextOBJCLICK		(HELP_STR_BASE +  20)

/* Topic menubar navigate strings */
#define TextNAV_SEARCH 		(HELP_STR_BASE +  22)
#define TextNAV_GOTO 		(HELP_STR_BASE +  24)
#define TextNAV_MARK 		(HELP_STR_BASE +  26)
#define TextNAV_BACKLINK 	(HELP_STR_BASE +  28)
#define TextNAV_FORWLINK 	(HELP_STR_BASE +  30)
#define TextNAV_PREVPAGE 	(HELP_STR_BASE +  32)
#define TextNAV_NEXTPAGE 	(HELP_STR_BASE +  34)

/* General strings */
#define TextCLIP_ERR 		(HELP_STR_BASE +  36)
#define TextMEM_ERR 		(HELP_STR_BASE +  38)
#define TextCLIP_PUT_ERR 	(HELP_STR_BASE +  40)
#define TextPRINT_ERR 		(HELP_STR_BASE +  42)
#define TextPRINT_OK 		(HELP_STR_BASE +  44)
#define TextCLIP_OK 		(HELP_STR_BASE +  46)
#define TextTHREAD_INFO 	(HELP_STR_BASE +  48)
#define TextMARKED_INFO 	(HELP_STR_BASE +  50)

/* Copy selection window */
#define TextCOPYPART_NONE	(HELP_STR_BASE +  52)

/* Some labels */
#define TextMARK			(HELP_STR_BASE +  54)
#define TextUNMARK			(HELP_STR_BASE +  56)

/* hyper link & hot link attribute strings */
#define TextHYPERLINK		(HELP_STR_BASE +  60)
#define TextHOTLINK			(HELP_STR_BASE +  62)


/* 
 * Define the local resource file constants
 */
/* Window, dialog identifiers */
#define HELP_RES_BASE			29500
#define TOPIC_WIN_RID			(HELP_RES_BASE +   0)
#define GOTO_DLG_RID			(HELP_RES_BASE +   3)
#define SEARCH_DLG_RID			(HELP_RES_BASE +   4)
#define TOPIC_SELCOPY_RID		(HELP_RES_BASE +   5)
#define HELPVIEW_ABOUT_RID		(HELP_RES_BASE +   6)

/* Topic window menubar */
#define TOPICWIN_MENUBAR 		(HELP_RES_BASE +  10)
#define MHELP_FILE 				(HELP_RES_BASE +  11)
#define MHELP_FILE_PRINT_SETUP 	(HELP_RES_BASE +  12)
#define MHELP_FILE_PRINT 		(HELP_RES_BASE +  13)
#define MHELP_FILE_EXIT 		(HELP_RES_BASE +  14)
#define MHELP_EDIT 				(HELP_RES_BASE +  15)
#define MHELP_EDIT_COPY 		M_EDIT_COPY	/* (HELP_RES_BASE +  16) */ 
#define MHELP_EDIT_AS_WRAPPED	(HELP_RES_BASE +  17)
#define MHELP_NAV 				(HELP_RES_BASE +  18)
#define MHELP_NAV_SEARCH 		(HELP_RES_BASE +  19)
#define MHELP_NAV_GOTO 			(HELP_RES_BASE +  20)
#define MHELP_NAV_MARK 			(HELP_RES_BASE +  21)
#define MHELP_NAV_BACKLINK 		(HELP_RES_BASE +  22)
#define MHELP_NAV_FORWLINK 		(HELP_RES_BASE +  23)
#define MHELP_NAV_PREVPAGE 		(HELP_RES_BASE +  24)
#define MHELP_NAV_NEXTPAGE 		(HELP_RES_BASE +  25)
#define MHELP_HELP 				(HELP_RES_BASE +  26)
#define MHELP_HELP_ONHELP 		M_HELP_HELPONHELP
#define MHELP_HELP_ABOUT 		(HELP_RES_BASE +  27)
#define MHELP_EDIT_COPYPART		(HELP_RES_BASE +  28)

/* Topic specific control identifiers */
#define TOPIC_SEARCH			(HELP_RES_BASE +  30)
#define TOPIC_BOOKMARK			(HELP_RES_BASE +  31)
#define TOPIC_GOTO				(HELP_RES_BASE +  32)
#define TOPIC_BACKLINK			(HELP_RES_BASE +  33)
#define TOPIC_FORWLINK			(HELP_RES_BASE +  34)
#define TOPIC_VSCROLL			(HELP_RES_BASE +  35)
#define TOPIC_INFOGROUP			(HELP_RES_BASE +  36)
#define TOPIC_INFOLBL			(HELP_RES_BASE +  37)
#define TOPIC_CLIENTW			(HELP_RES_BASE +  38)
#define TOPIC_PREVPAGE			(HELP_RES_BASE +  39)
#define TOPIC_NEXTPAGE			(HELP_RES_BASE +  40)

/* Shared identifiers */
#define CLIENT_AREA				(HELP_RES_BASE +  50)

/* Search dialog ids */
#define SEARCH_BY_TOPICNAME		(HELP_RES_BASE +  60)
#define SEARCH_BY_KEYWORD		(HELP_RES_BASE +  61)
#define SEARCH_SELECT_LIST		(HELP_RES_BASE +  62)
#define SEARCH_MATCH_LIST		(HELP_RES_BASE +  63)
#define SEARCH_GOTO_MATCH		(HELP_RES_BASE +  64)
#define SEARCH_CANCEL			DLG_CANCEL
#define SEARCH_RADIO_LBL		(HELP_RES_BASE +  66)
#define SEARCH_ITEMS_LBL		(HELP_RES_BASE +  67)
#define SEARCH_MATCH_LBL		(HELP_RES_BASE +  68)

/* Goto dialog ids */
#define GOTO_CONTENTS			M_HELP_CONTENTS
#define GOTO_INDEX				M_HELP_INDEX
#define GOTO_CONTENTS			M_HELP_CONTENTS
#define GOTO_GLOSSARY			M_HELP_GLOSSARY
#define GOTO_KEYBOARD			M_HELP_KEYBOARD
#define GOTO_BOOKMARK_LIST		(HELP_RES_BASE +  70)
#define GOTO_BOOKMARK_BTN		(HELP_RES_BASE +  71)
#define GOTO_CANCEL				DLG_CANCEL
#define GOTO_GROUP				(HELP_RES_BASE +  73)
#define GOTO_BOOK_LBL			(HELP_RES_BASE +  74)

/* Popup window sample definition */
#define POPUP_WIN_RID			(HELP_RES_BASE +  80)

/* Topic selection copy menubar */
#define EDITSEL_MENUBAR 		(HELP_RES_BASE +  90)
#define MHELP_TSE_EDIT 			(HELP_RES_BASE +  91)
#define MHELP_TSE_EDIT_COPY 	/* (HELP_RES_BASE +  92) */ M_EDIT_COPY
#define MHELP_TSE_HELP      	(HELP_RES_BASE +  93)
#define MHELP_TSE_HELP_ONHELP	M_HELP_HELPONHELP
#define MHELP_TSE_HELP_ABOUT	(HELP_RES_BASE +  94)

/* Other RID for selection copy window */
#define TOPIC_SELCOPY_WIN_TX	(HELP_RES_BASE + 100)
#define TOPIC_SELCOPY_WIN_LBL	(HELP_RES_BASE + 101)

/* RID's for viewer about box */
#define XHV_STATIC_1			(HELP_RES_BASE + 110)
#define XHV_STATIC_2			(HELP_RES_BASE + 111)

#endif  /* NO_HELP_RESOURCES */

#endif  /* XVT_INCL_HELP */

