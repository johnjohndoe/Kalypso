/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *	$RCSfile: xvttimer.h,v $ 
 *  $Revision: 1.4 $
 *
 *  Purpose: Interface to the timer object.
 *
 ****************************************************************************/

#ifndef XVT_INCL_XVTTIMER
#define XVT_INCL_XVTTIMER

extern   long                                   XVT_CALLCONV1
         xvt_timer_create                       XVT_CALLCONV2
         (
            WINDOW win,
            long   interval
         );


extern   void                                   XVT_CALLCONV1
         xvt_timer_destroy                      XVT_CALLCONV2
         (
            long id
         );


#endif /* XVT_INCL_XVTTIMER */
