/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *	$RCSfile: xvt_menu.h,v $ 
 *  $Revision: 1.3 $
 *
 *  Purpose: XVT menu subsystem definitions.
 *
 ****************************************************************************/

#ifndef XVT_INCL_XVT_MENU
#define XVT_INCL_XVT_MENU

#define MAX_MENU_TAG		31999	/* max allowable application menu tag */

#define M_FILE			32000
#define M_FILE_NEW		(M_FILE+1)
#define M_FILE_OPEN 		(M_FILE+2)
#define M_FILE_CLOSE		(M_FILE+3)
#define M_FILE_SAVE 		(M_FILE+4)
#define M_FILE_SAVE_AS		(M_FILE+5)
#define M_FILE_REVERT		(M_FILE+6)
#define M_FILE_PG_SETUP 	(M_FILE+7)
#define M_FILE_PRINT		(M_FILE+8)
#define M_FILE_QUIT 		(M_FILE+9)
#define M_FILE_ABOUT		(M_FILE+10)

#define M_EDIT			32025
#define M_EDIT_UNDO 		(M_EDIT+1)
#define M_EDIT_CUT		(M_EDIT+2)
#define M_EDIT_COPY 		(M_EDIT+3)
#define M_EDIT_PASTE		(M_EDIT+4)
#define M_EDIT_CLEAR		(M_EDIT+5)
#define M_EDIT_SEL_ALL		(M_EDIT+6)
#define M_EDIT_CLIPBOARD	(M_EDIT+7)

#define M_FONT			32050	/* needs range of 300 for Mac */
#define M_STYLE			32350
#define M_HELP			32450   /* reserve about 50 for Help */
#define M_DEFAULT_SEPARATOR 32765  /* indicates no tag value set */
#define FONT_MENU_TAG		32766	/* magic cookie */
/* XVT/Mac reserves 32767 */

#endif 		/* XVT_INCL_XVT_MENU */
