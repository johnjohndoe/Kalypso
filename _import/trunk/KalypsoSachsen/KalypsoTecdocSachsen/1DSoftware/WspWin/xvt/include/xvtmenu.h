/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *  $RCSfile: xvtmenu.h,v $ 
 *  $Revision: 1.4 $
 *
 *  Purpose: Interface to the menu object.
 *
 ****************************************************************************/

#ifndef XVT_INCL_XVTMENU
#define XVT_INCL_XVTMENU

extern   XVT_FNTID                              XVT_CALLCONV1
         xvt_menu_get_font_sel                  XVT_CALLCONV2
         (
            WINDOW        window
         );

extern   void                                   XVT_CALLCONV1
         xvt_menu_set_font_sel                  XVT_CALLCONV2
         (
            WINDOW        window,
            XVT_FNTID     font_id
         );

extern   MENU_ITEM*                             XVT_CALLCONV1
         xvt_menu_get_tree                      XVT_CALLCONV2
         (
            WINDOW        window
         );

extern   void                                   XVT_CALLCONV1
         xvt_menu_set_item_checked              XVT_CALLCONV2
         (
            WINDOW 			window,
			MENU_TAG 		tag,
			BOOLEAN 		check
         );

extern   void                                   XVT_CALLCONV1
         xvt_menu_set_item_enabled              XVT_CALLCONV2
         (
            WINDOW 			window,
			MENU_TAG 		tag,
			BOOLEAN 		enable
         );

extern   void                                   XVT_CALLCONV1
         xvt_menu_set_item_title                XVT_CALLCONV2
         (
            WINDOW 			window,
			MENU_TAG 		tag,
			char*           text
         );

extern   void                                   XVT_CALLCONV1
         xvt_menu_set_tree                      XVT_CALLCONV2
         (
            WINDOW 			window,
			MENU_ITEM		*mip
         );

extern   void                                   XVT_CALLCONV1
         xvt_menu_update                        XVT_CALLCONV2
         (
            WINDOW 			window 
         );

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_menu_popup                         XVT_CALLCONV2
         (
            MENU_ITEM          *menu_p,
            WINDOW 			    window, 
            PNT                 pos,
            XVT_POPUP_ALIGNMENT alignment,
            MENU_TAG            item
         );

#endif /* XVT_INCL_XVTMENU */
