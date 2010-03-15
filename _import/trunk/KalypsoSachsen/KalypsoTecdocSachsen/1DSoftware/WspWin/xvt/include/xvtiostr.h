/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *	$RCSfile: xvtiostr.h,v $ 
 *  $Revision: 1.4 $
 *
 *  Purpose: Public I/O stream object interface.
 *
 ****************************************************************************/

#ifndef XVT_INCL_XVTIOSTR
#define XVT_INCL_XVTIOSTR

extern XVT_IOSTREAM  						XVT_CALLCONV1
xvt_iostr_create_fread  					XVT_CALLCONV2
(
	FILE *filep
);

extern XVT_IOSTREAM  						XVT_CALLCONV1
xvt_iostr_create_fwrite 					XVT_CALLCONV2
(
	FILE *filep
);

extern XVT_IOSTREAM  						XVT_CALLCONV1
xvt_iostr_create_read  					XVT_CALLCONV2
(
	XVT_IOSTR_CONTEXT context,
	XVT_IOSTR_RWFUNC  get_bytes,
	XVT_IOSTR_SZFUNC  num_bytes
);

extern XVT_IOSTREAM  						XVT_CALLCONV1
xvt_iostr_create_write 					XVT_CALLCONV2
(
	XVT_IOSTR_CONTEXT context,
	XVT_IOSTR_RWFUNC  put_bytes
);

extern XVT_IOSTR_CONTEXT					XVT_CALLCONV1
xvt_iostr_get_context						XVT_CALLCONV2
(
	XVT_IOSTREAM iostream
);

extern void 								XVT_CALLCONV1
xvt_iostr_destroy 							XVT_CALLCONV2
(
	XVT_IOSTREAM iostream
);

#endif /* XVT_INCL_XVTIOSTR */
