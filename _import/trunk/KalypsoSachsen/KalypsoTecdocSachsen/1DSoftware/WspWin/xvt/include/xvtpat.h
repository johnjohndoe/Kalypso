/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *
 *  Purpose: Interface to the pattern object.
 *
 ****************************************************************************/

#ifndef XVT_INCL_XVTPATT
#define XVT_INCL_XVTPATT

extern   XVT_PATTERN                            XVT_CALLCONV1
         xvt_pattern_create                     XVT_CALLCONV2
         (
            const char * patstr
         );


extern   void                                   XVT_CALLCONV1
         xvt_pattern_destroy                    XVT_CALLCONV2
         (
            XVT_PATTERN pat
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_pattern_match                      XVT_CALLCONV2
         (
            XVT_PATTERN pat,
            const char *str,
            const char **endstr
         );


extern   const char *                           XVT_CALLCONV1
         xvt_pattern_format_string              XVT_CALLCONV2
         (
            XVT_PATTERN pat,
            const char  *str,
            char        *buf,
            size_t      buflen,
            BOOLEAN     complete_string,
            int         *start,
            int         *end
         );


#endif /* XVT_INCL_XVTPATT */
