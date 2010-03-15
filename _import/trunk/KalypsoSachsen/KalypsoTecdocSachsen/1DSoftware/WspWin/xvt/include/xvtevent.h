/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *	$RCSfile: xvtevent.h,v $ 
 *  $Revision: 1.4 $
 *
 *  Purpose: Interface to the EVENT object.
 *
 ****************************************************************************/

#ifndef XVT_INCL_XVTEVENT
#define XVT_INCL_XVTEVENT

extern	XVT_FNTID                             XVT_CALLCONV1
		xvt_event_get_font                    XVT_CALLCONV2
		(
   		EVENT    *ep                          /* E_FONT event */ 
		);

extern  BOOLEAN                               XVT_CALLCONV1
        xvt_event_is_virtual_key              XVT_CALLCONV2
        (
   		EVENT    *ep                          /* any event, usually E_CHAR */ 
        ) ;

extern	void                                  XVT_CALLCONV1
		xvt_event_set_font                    XVT_CALLCONV2
		(
   		EVENT    *ep,                         /* E_FONT event */
		XVT_FNTID font_id					  /* font object */
		);

#endif /* XVT_INCL_XVTEVENT */
