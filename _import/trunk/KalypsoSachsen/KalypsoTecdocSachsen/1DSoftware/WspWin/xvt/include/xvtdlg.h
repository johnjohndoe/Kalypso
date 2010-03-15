/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *	$RCSfile: xvtdlg.h,v $ 
 *  $Revision: 1.4 $
 *
 *  Purpose: Interface to the dialog object.
 *
 ****************************************************************************/

#ifndef XVT_INCL_XVTDLG
#define XVT_INCL_XVTDLG

extern   WINDOW                                 XVT_CALLCONV1
         xvt_dlg_create_def                     XVT_CALLCONV2
         (
            WIN_DEF *     win_def_p,
            EVENT_MASK    mask,
            EVENT_HANDLER eh,
            long          app_data
         );


extern   WINDOW                                 XVT_CALLCONV1
         xvt_dlg_create_res                     XVT_CALLCONV2
         (
            WIN_TYPE      wtype,
            int           rid,
            EVENT_MASK    mask,
            EVENT_HANDLER eh,
            long          app_data
         );


#endif /* XVT_INCL_XVTDLG */
