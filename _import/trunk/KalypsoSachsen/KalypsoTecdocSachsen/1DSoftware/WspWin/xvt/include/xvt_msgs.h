/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *  $RCSfile: xvt_msgs.h,v $
 *  $Revision: 1.6 $
 *
 *  Purpose:  Standard Portable Error Message Defintion
 *
 ****************************************************************************/
 
#if 0
/*===========================================================================
   Standard XVT Error Messages
   This file has no executable meaning, it simply defines all the standard 
   error categories and messages for extraction by the ERRSCAN tool. 
   The "functions" used below are simply ERRSCAN tokens.

   PLEASE:
   Add new "standard" messages and categories below, simply inserting message
   definition into appropriate category, using a new message suffix and number.
   After addition, re-run ERRSCAN so that new message defintion(s) are added 
   to appropriate files (ERRCODES.TXT and xvt_perr.h).
   Also, PLEASE, add message to file verrtxt.c, so the text gets hardcoded.

   Besides "standard" messages, defined here or (for platform specific ones)
   in xvt_plat.h, messages may be introduced by calling xvt_errmsg_sig[_if].
   For such messages, for each message number in one category select a unique
   number (and name suffix) according to the following rule:

   To follow the XVT assert4 legacy, the range 0-30000 is reserved for
   application (if it chooses to use any of the XVT error categories).
   The toolkit space 30000 - 65535 should be divided into 1000 message ID 
   chunks as follows:
 
    30000 - 30999:  T-layer ( xvt... common code)
    31000 - 31999:  V-layer ( common code )
    32000 - 32999:  O-layer ( common code )
    33000 - 39999:  -
    40000 - 40999:  K-layer, Mac
    41000 - 42999:  -
    43000 - 43999:  K-layer, Ch
    44000 - 49999:  -
    50000 - 50999:  K-layer, Win common code
    51000 - 51999:  K-layer, Win 3.x
    52000 - 52999:  K-layer, Win NT
    55000 - 55999:  K-layer, PM
    56000 - 59999:  -
    60000 - 60999:  K-layer, X common code
    61000 - 61999:  K-layer, Xm
    62000 - 62999:  K-layer, Xol
    63000 - 65535:  -
 
   Within each of the message ID chunks above, toolkits are free to set
   a policy of their own.

-----------------------------------------------------------------------------
*/
	/* Major error categories - used to define minor categories, NOT messages */
    xvt_errmsg_def_mjr(ERR,             "APP",       1, "Application");
    xvt_errmsg_def_mjr(ERR,             "ARG",       2, "Invalid argument");
    xvt_errmsg_def_mjr(ERR,             "ASSERT",    3, "XVT release 3 assert");
    xvt_errmsg_def_mjr(ERR,             "ERRNO",     4, "XVT_ERRNO set");
    xvt_errmsg_def_mjr(ERR,             "FAIL",      5, "Correctly requested operation failed");
    xvt_errmsg_def_mjr(ERR,             "REQ",       6, "Invalid request or request context");
    xvt_errmsg_def_mjr(ERR,             "SYS",       7, "Underlying system generated error");
    xvt_errmsg_def_mjr(ERR,             "EMF",       8, "Error messaging facility");

    /* Error categories (major/minor) - used to define error messages */
    xvt_errmsg_def_cat(ERR_ARG,         "NULL",      1, "NULL handle");
    xvt_errmsg_def_cat(ERR_ARG,         "INV",       2, "Invalid object");
    xvt_errmsg_def_cat(ERR_ARG,         "TYPE",      3, "Incorrect object type");
    xvt_errmsg_def_cat(ERR_ARG,         "FORMAT",    4, "Wrong argument format");
    xvt_errmsg_def_cat(ERR_ARG,         "VALUE",     5, "Bad value");
    xvt_errmsg_def_cat(ERR_ARG,         "SIZE",      6, "Bad size");
    xvt_errmsg_def_cat(ERR_ARG,         "INCOMP",    7, "Incomplete");
    xvt_errmsg_def_cat(ERR_ASSERT,      "0",         1, "Signaled assert");
    xvt_errmsg_def_cat(ERR_ASSERT,      "2",         2, "Signaled assert2");
    xvt_errmsg_def_cat(ERR_ASSERT,      "4",         4, "Signaled assert4");
    xvt_errmsg_def_cat(ERR_FAIL,        "NOMEM",     1, "Failed to allocate memory");
    xvt_errmsg_def_cat(ERR_FAIL,        "CREATE",    2, "Failed to create");
    xvt_errmsg_def_cat(ERR_FAIL,        "FIND",      3, "Failed to find");
    xvt_errmsg_def_cat(ERR_FAIL,        "ACCESS",    4, "Failed to access");
    xvt_errmsg_def_cat(ERR_FAIL,        "ADD",       5, "Failed to add");
    xvt_errmsg_def_cat(ERR_FAIL,        "IO",        6, "Failed I/O");
    xvt_errmsg_def_cat(ERR_FAIL,        "INTERN",    7, "Internal error");
    xvt_errmsg_def_cat(ERR_FAIL,        "DEVICE",    8, "Device Driver failure");
    xvt_errmsg_def_cat(ERR_FAIL,        "PARSE",     9, "Parse Failure");
    xvt_errmsg_def_cat(ERR_REQ,         "DURING",    1, "Request invalid during operation");
    xvt_errmsg_def_cat(ERR_REQ,         "RECURSIVE", 2, "Recursive request");
    xvt_errmsg_def_cat(ERR_REQ,         "INUSE",     3, "Operation target is in use");
    xvt_errmsg_def_cat(ERR_REQ,         "STATE",     4, "Operation target is in wrong state");
    xvt_errmsg_def_cat(ERR_REQ,         "ATTR",      5, "Operation target attribute is wrong");
    xvt_errmsg_def_cat(ERR_REQ,         "AFTER",     6, "Request invalid after operation" );
    xvt_errmsg_def_cat(ERR_SYS,         "OS",        1, "Operating system error");
    xvt_errmsg_def_cat(ERR_SYS,         "WS",        2, "Window system error");
    xvt_errmsg_def_cat(ERR_SYS,         "TK",        3, "Toolkit error");
    xvt_errmsg_def_cat(ERR_SYS,         "TEDIT",     4, "Text edit error");
    xvt_errmsg_def_cat(ERR_EMF,         "SEV",       1, "Error severity codes");
    xvt_errmsg_def_cat(ERR_EMF,         "LABEL",     2, "Message facility labels");
    xvt_errmsg_def_cat(ERR_EMF,         "FRAME",     3, "Error Message Frame problems");

	/* Standard  error messages */
    xvt_errmsg_def_std(ERR_ARG_NULL,    "OBJECT",    1, "NULL Object handle");
    xvt_errmsg_def_std(ERR_ARG_NULL,    "WIN",       2, "NULL Window handle");
    xvt_errmsg_def_std(ERR_ARG_NULL,    "FONT",      3, "NULL Font handle");
    xvt_errmsg_def_std(ERR_ARG_NULL,    "PALETTE",   4, "NULL Color Palette handle");
    xvt_errmsg_def_std(ERR_ARG_NULL,    "PICTURE",   5, "NULL Picture handle");
    xvt_errmsg_def_std(ERR_ARG_NULL,    "PIXMAP",    6, "NULL Pixmap handle");
    xvt_errmsg_def_std(ERR_ARG_NULL,    "IMAGE",     7, "NULL Image handle");
    xvt_errmsg_def_std(ERR_ARG_NULL,    "RECT",      8, "NULL Rectangle pointer");
    xvt_errmsg_def_std(ERR_ARG_NULL,    "POINT",     9, "NULL Point pointer");
    xvt_errmsg_def_std(ERR_ARG_NULL,    "SLIST",    10, "NULL SLIST pointer");
    xvt_errmsg_def_std(ERR_ARG_NULL,    "BUFFER",   11, "NULL buffer pointer");
    xvt_errmsg_def_std(ERR_ARG_NULL,    "POINTER",  12, "NULL pointer");
    xvt_errmsg_def_std(ERR_ARG_NULL,    "EVENT",    13, "NULL EVENT pointer");
    xvt_errmsg_def_std(ERR_ARG_NULL,    "HANDLER",  14, "NULL EVENT handler");
    xvt_errmsg_def_std(ERR_ARG_INV,     "OBJECT",    1, "Invalid object handle");
    xvt_errmsg_def_std(ERR_ARG_INV,     "WIN",       2, "Invalid Window handle");
    xvt_errmsg_def_std(ERR_ARG_INV,     "FONT",      3, "Invalid Font handle");
    xvt_errmsg_def_std(ERR_ARG_INV,     "PALETTE",   4, "Invalid Color Palette handle");
    xvt_errmsg_def_std(ERR_ARG_INV,     "PICTURE",   5, "Invalid Picture handle");
    xvt_errmsg_def_std(ERR_ARG_INV,     "PIXMAP",    6, "Invalid Pixmap handle");
    xvt_errmsg_def_std(ERR_ARG_INV,     "IMAGE",     7, "Invalid Image handle");
    xvt_errmsg_def_std(ERR_ARG_INV,     "SLIST",     8, "Invalid SLIST pointer");
    xvt_errmsg_def_std(ERR_ARG_INV,     "TX",        9, "Invalid TX object handle");
    xvt_errmsg_def_std(ERR_ARG_INV,     "ESCAPE",   10, "Invalid Escape Code");
    xvt_errmsg_def_std(ERR_ARG_INV,     "ATTR",     11, "Invalid Attribute");
    xvt_errmsg_def_std(ERR_ARG_INV,     "PRINTRCD", 12, "Invalid Print Record");
    xvt_errmsg_def_std(ERR_ARG_INV,     "EVENT",    13, "Invalid EVENT argument");
    xvt_errmsg_def_std(ERR_ARG_INV,     "CTXEFONT", 15, "Migration context has no current font");
    xvt_errmsg_def_std(ERR_ARG_INV,     "CHAR",     16, "Invalid multibyte character encountered");
    xvt_errmsg_def_std(ERR_ARG_INV,     "CXO",      17, "Invalid CXO handle");
    xvt_errmsg_def_std(ERR_ARG_INV,     "NAV",      18, "Invalid XVT_NAV handle");
    xvt_errmsg_def_std(ERR_ARG_INV,     "FLAG",     19, "Invalid flag combination");
    xvt_errmsg_def_std(ERR_ARG_INV,     "PATTERN",  20, "Invalid formatting pattern");
    xvt_errmsg_def_std(ERR_ARG_TYPE,    "WIN",       1, "Incorrect Window type");
    xvt_errmsg_def_std(ERR_ARG_TYPE,    "PIXMAP",    2, "Incorrect Pixmap type");
    xvt_errmsg_def_std(ERR_ARG_TYPE,    "EVENT",     3, "Incorrect EVENT type");
    xvt_errmsg_def_std(ERR_ARG_TYPE,    "SCROLL",    4, "Incorrect SCROLL_TYPE");
    xvt_errmsg_def_std(ERR_ARG_TYPE,    "DLG",       5, "Incorrect Dialog type");
    xvt_errmsg_def_std(ERR_ARG_FORMAT,  "DIRNAM",    1, "Directory name format invalid");
    xvt_errmsg_def_std(ERR_ARG_FORMAT,  "FILNAM",    2, "Filename format incorrect");
    xvt_errmsg_def_std(ERR_ARG_FORMAT,  "RECT",      3, "Rectangle in invalid format");
    xvt_errmsg_def_std(ERR_ARG_VALUE,   "BAD",       1, "Bad value");
    xvt_errmsg_def_std(ERR_ARG_VALUE,   "UNKNOWN",   2, "Unknown value");
    xvt_errmsg_def_std(ERR_ARG_VALUE,   "TOOHIGH",   3, "Value too high");
    xvt_errmsg_def_std(ERR_ARG_VALUE,   "TOOLOW",    4, "Value too low");
    xvt_errmsg_def_std(ERR_ARG_VALUE,   "TOOMANYCOL",5, "Too many colors");
    xvt_errmsg_def_std(ERR_ARG_SIZE,    "BAD",       1, "Bad size");
    xvt_errmsg_def_std(ERR_ARG_SIZE,    "TOOHIGH",   2, "Size too big");
    xvt_errmsg_def_std(ERR_ARG_SIZE,    "TOOLOW",    3, "Size insufficient, too small");
    xvt_errmsg_def_std(ERR_ARG_SIZE,    "NOBUF",     4, "Insufficient buffer size");
    xvt_errmsg_def_std(ERR_ARG_INCOMP,  "SPEC",      1, "Incomplete specification");
    xvt_errmsg_def_std(ERR_FAIL_NOMEM,  "HEAP",      1, "Failed to allocate heap memory");
    xvt_errmsg_def_std(ERR_FAIL_NOMEM,  "GLOBAL",    2, "Failed to allocate global memory");
    xvt_errmsg_def_std(ERR_FAIL_NOMEM,  "STACK",     3, "Failed to allocate stack space");
    xvt_errmsg_def_std(ERR_FAIL_NOMEM,  "RESOURCE",  4, "Failed to allocate resource");
    xvt_errmsg_def_std(ERR_FAIL_NOMEM,  "OBJECT",    5, "Failed to allocate object");
    xvt_errmsg_def_std(ERR_FAIL_CREATE, "OBJECT",    1, "Failed to create object");
    xvt_errmsg_def_std(ERR_FAIL_CREATE, "WINDOW",    2, "Failed to create window");
    xvt_errmsg_def_std(ERR_FAIL_CREATE, "IMAGE",     3, "Failed to create image");
    xvt_errmsg_def_std(ERR_FAIL_CREATE, "CONTROL",   4, "Failed to create control");
    xvt_errmsg_def_std(ERR_FAIL_CREATE, "CLIPBD",    5, "Failed to create clipboard");
    xvt_errmsg_def_std(ERR_FAIL_CREATE, "DIALOG",    6, "Failed to create dialog");
    xvt_errmsg_def_std(ERR_FAIL_CREATE, "PALETTE",   7, "Failed to create color palette");
    xvt_errmsg_def_std(ERR_FAIL_CREATE, "PICTURE",   8, "Failed to create picture");
    xvt_errmsg_def_std(ERR_FAIL_CREATE, "PIXMAP",    9, "Failed to create pixelmap");
    xvt_errmsg_def_std(ERR_FAIL_CREATE, "FONT",     10, "Failed to create font object");
    xvt_errmsg_def_std(ERR_FAIL_CREATE, "FILE",     11, "Failed to create file");
    xvt_errmsg_def_std(ERR_FAIL_CREATE, "TX",       12, "Failed to create TX object");
    xvt_errmsg_def_std(ERR_FAIL_CREATE, "XPMIMAGE", 13, "Failed to create Xpm image object");
    xvt_errmsg_def_std(ERR_FAIL_FIND,   "FILE",      1, "Failed to find file");
    xvt_errmsg_def_std(ERR_FAIL_FIND,   "NAME",      2, "Failed to find name");
    xvt_errmsg_def_std(ERR_FAIL_FIND,   "RESOURCE",  3, "Failed to find resource");
    xvt_errmsg_def_std(ERR_FAIL_FIND,   "FONT",      4, "Failed to find font");
    xvt_errmsg_def_std(ERR_FAIL_FIND,   "NAMEDCOLOR",5, "Failed to find named color");
    xvt_errmsg_def_std(ERR_FAIL_FIND,   "RESFILE",   6, "Failed to find resource file"
);
    xvt_errmsg_def_std(ERR_FAIL_ACCESS, "FILE",      1, "Failed to access file");
    xvt_errmsg_def_std(ERR_FAIL_ACCESS, "DIR",       2, "Failed to access directory");
    xvt_errmsg_def_std(ERR_FAIL_ACCESS, "OBJECT",    3, "Failed to access object");
    xvt_errmsg_def_std(ERR_FAIL_ACCESS, "MENU",      4, "Failed to access menu");
    xvt_errmsg_def_std(ERR_FAIL_ADD,    "SLIST",     1, "Failed to add to SLIST");
    xvt_errmsg_def_std(ERR_FAIL_ADD,    "TX",        2, "Failed to add to TX object");
    xvt_errmsg_def_std(ERR_FAIL_IO,     "READ",      1, "Read failed");
    xvt_errmsg_def_std(ERR_FAIL_IO,     "WRITE",     2, "Write failed");
    xvt_errmsg_def_std(ERR_FAIL_IO,     "SEEK",      3, "IO Seek failed");
    xvt_errmsg_def_std(ERR_FAIL_INTERN, "DATA",      1, "Internal data corrupted");
    xvt_errmsg_def_std(ERR_FAIL_INTERN, "CALL",      2, "Internal call unexpected");
    xvt_errmsg_def_std(ERR_FAIL_INTERN, "STATE",     3, "Internal state unexpected");
    xvt_errmsg_def_std(ERR_FAIL_INTERN, "OBSFURL",   4, "Obsolete version of FURL file");
    xvt_errmsg_def_std(ERR_FAIL_INTERN, "IMAGE",     5, "Internal image error");
    xvt_errmsg_def_std(ERR_FAIL_PARSE,  "XBMIMAGE",   1, "No terminator '}' in xbm definition");
    xvt_errmsg_def_std(ERR_FAIL_PARSE,  "XPMDECLARATION", 2, "Could not parse Xpm declaration section");
    xvt_errmsg_def_std(ERR_FAIL_PARSE,  "XPMVALUES",  3, "Could not parse Xpm values section");
    xvt_errmsg_def_std(ERR_FAIL_PARSE,  "XPMPIXELNAME", 4, "Too many characters in color name");
    xvt_errmsg_def_std(ERR_FAIL_PARSE,  "XPMCOLORS",  5, "Could not parse Xpm colors section");
    xvt_errmsg_def_std(ERR_FAIL_PARSE,  "XPMPIXELS",  6, "Could not parse Xpm pixels section");
    xvt_errmsg_def_std(ERR_FAIL_PARSE,  "XPMEXTENSION", 7, "Could not parse Xpm extensions");
    xvt_errmsg_def_std(ERR_FAIL_PARSE,  "OVERFLOW",   8, "Overflow of value during parse");
    xvt_errmsg_def_std(ERR_FAIL_PARSE,  "PATH",       9, "Too many characters in path");
    xvt_errmsg_def_std(ERR_FAIL_PARSE,  "FORMAT",    10, "Illegal format specification");
    xvt_errmsg_def_std(ERR_REQ_DURING,  "UPDATE",    1, "Request during E_UPDATE processing");
    xvt_errmsg_def_std(ERR_REQ_AFTER,   "APP_CREATE",1, "Request after xvt_app_create");
    xvt_errmsg_def_std(ERR_REQ_RECURSIVE,"CALL",     1, "Recursive call requested");
    xvt_errmsg_def_std(ERR_REQ_INUSE,    "OBJECT",   1, "Request while object in use");
    xvt_errmsg_def_std(ERR_REQ_STATE,    "OBJECT",   1, "Request while object in wrong state");
    xvt_errmsg_def_std(ERR_REQ_STATE,    "IMAGEFMT", 3, "Invalid image format");
    xvt_errmsg_def_std(ERR_REQ_ATTR,     "OBJECT",   1, "Object attribute prevents request");
    xvt_errmsg_def_std(ERR_REQ_ATTR,     "NOSCROLL", 2, "Window doesn't have scrollbar");
    xvt_errmsg_def_std(ERR_SYS_OS,       "BADWORD",  1, "Bad machine word length");
    xvt_errmsg_def_std(ERR_SYS_TEDIT,    "INTERNAL", 1, "Internal Text Edit error");
    xvt_errmsg_def_std(ERR_SYS_TEDIT,    "INVKILL",  2, "Attempt to destroy invalid TXEDIT");
    xvt_errmsg_def_std(ERR_SYS_TEDIT,    "KILL",     3, "Error freeing TXEDIT");
    xvt_errmsg_def_std(ERR_SYS_TEDIT,   "HIGHLIGHT", 4, "Highlight conflict");
    xvt_errmsg_def_std(ERR_EMF_SEV,      "WARNING",  1, "WARNING: ");
    xvt_errmsg_def_std(ERR_EMF_SEV,      "ERROR",    2, "ERROR: ");
    xvt_errmsg_def_std(ERR_EMF_SEV,      "FATAL",    3, "FATAL ERROR: ");
    xvt_errmsg_def_std(ERR_EMF_LABEL,    "CAT",      1, "Category: ");
    xvt_errmsg_def_std(ERR_EMF_LABEL,    "FUNC",     2, "Function: ");
    xvt_errmsg_def_std(ERR_EMF_LABEL,    "FILE",     3, "File:     ");
    xvt_errmsg_def_std(ERR_EMF_LABEL,    "LINE",     4, "line: ");
    xvt_errmsg_def_std(ERR_EMF_LABEL,    "XVTPROC",  5, "XVT portability toolkit internal call");
    xvt_errmsg_def_std(ERR_EMF_LABEL,    "FMT",      6, "%s\n%s\n%s\n%s\n");
    xvt_errmsg_def_std(ERR_EMF_FRAME,    "HANDLERS", 1, "Pushed err.handlers left on frame exit");
    xvt_errmsg_def_std(ERR_EMF_FRAME,    "APIMRKS",  2, "API function found on frame exit");
    xvt_errmsg_def_std(ERR_EMF_FRAME,    "MARKED",   3, "API function already marked in frame");
    xvt_errmsg_def_std(ERR_EMF_FRAME,    "NOHANDLER",4, "Attempt to pop non-existent handler");
    xvt_errmsg_def_std(ERR_EMF_FRAME,    "PUSH",     5, "Pushing error handler within handler");
    xvt_errmsg_def_std(ERR_EMF_FRAME,    "MAXERR",   6, "Too many errors encountered");
    xvt_errmsg_def_std(ERR_EMF_FRAME,    "MRKROOM",  7, "No room for recusive API mark");
    xvt_errmsg_def_std(ERR_EMF_FRAME,    "MRKSYNC",  8, "API marks out of sync");
#endif /* 0 */

#endif /* XVT_INCL_XVTERR */
