/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *  $RCSfile: xvtstr.h,v $ 
 *  $Revision: 1.12 $
 *
 *  Purpose: Interface to the string object.
 *
 ****************************************************************************/

#ifndef XVT_INCL_XVTSTR
#define XVT_INCL_XVTSTR

extern   int                                    XVT_CALLCONV1
         xvt_str_collate                        XVT_CALLCONV2
         (
            const char * mbs1,
            const char * mbs2
         );

extern   int                                    XVT_CALLCONV1
         xvt_str_collate_ignoring_case          XVT_CALLCONV2
         (
            const char * mbs1,
            const char * mbs2
         );

extern   int                                    XVT_CALLCONV1
         xvt_str_compare                        XVT_CALLCONV2
         (
            const char * mbs1,
            const char * mbs2
         );

extern   int                                    XVT_CALLCONV1
         xvt_str_compare_wchar                  XVT_CALLCONV2
         (
            const char * mbs1,
            XVT_WCHAR    wc
         );

extern   int                                    XVT_CALLCONV1
         xvt_str_compare_ignoring_case          XVT_CALLCONV2
         (
            const char * mbs1,
            const char * mbs2
         );

extern   int                                    XVT_CALLCONV1
         xvt_str_compare_n_char                	XVT_CALLCONV2
         (
            const char * mbs1,
            const char * mbs2,
            const size_t n
         );

extern   int                                    XVT_CALLCONV1
         xvt_str_compare_n_size_ignoring_case   XVT_CALLCONV2
         (
            const char * mbs1,
            const char * mbs2,
            const size_t n
         );

extern   char *                                 XVT_CALLCONV1
         xvt_str_concat                         XVT_CALLCONV2
         (
            char * mbs1,
            const char * mbs2
         );

extern   char *                                 XVT_CALLCONV1
         xvt_str_concat_n_char                  XVT_CALLCONV2
         (
            char * mbs1,
            const char * mbs2,
            const size_t n
         );

extern   int                                    XVT_CALLCONV1
         xvt_str_convert_mb_to_wc               XVT_CALLCONV2
         (
            XVT_WCHAR * wc,
            const char * mbs
         );

extern   int                                    XVT_CALLCONV1
         xvt_str_convert_mbs_to_wcs             XVT_CALLCONV2
         (
            XVT_WCHAR * wcs,
            const char * mbs,
            const size_t n
         );

extern   size_t                                 XVT_CALLCONV1
         xvt_str_convert_to_lower               XVT_CALLCONV2
         (
            char * mbs1,
            const char * mbs2,
            const size_t n
         );

extern   size_t                                 XVT_CALLCONV1
         xvt_str_convert_to_upper               XVT_CALLCONV2
         (
            char * mbs1,
            const char * mbs2,
            const size_t n
         );

extern   int                                    XVT_CALLCONV1
         xvt_str_convert_wc_to_mb               XVT_CALLCONV2
         (
            char * mbs,
            const XVT_WCHAR wc
         );

extern   XVT_WCHAR                              XVT_CALLCONV1
         xvt_str_convert_wchar_to_lower         XVT_CALLCONV2
         (
            XVT_WCHAR wc
         );

extern   XVT_WCHAR                              XVT_CALLCONV1
         xvt_str_convert_wchar_to_upper         XVT_CALLCONV2
         (
            XVT_WCHAR wc
         );

extern   int                                    XVT_CALLCONV1
         xvt_str_convert_wcs_to_mbs             XVT_CALLCONV2
         (
            char * mbs,
            const XVT_WCHAR * wcs,
            const size_t n
         );

extern   char *                                 XVT_CALLCONV1
         xvt_str_copy                           XVT_CALLCONV2
         (
            char * mbs1,
            const char * mbs2
         );

extern   size_t                                 XVT_CALLCONV1
         xvt_str_copy_n_char                    XVT_CALLCONV2
         (
            char * mbs1,
            const char * mbs2,
            const size_t n
         );

extern   size_t                                 XVT_CALLCONV1
         xvt_str_copy_n_size                    XVT_CALLCONV2
         (
            char * mbs1,
            const char * mbs2,
            size_t n
         );

extern   XVT_CODESET_MAP                        XVT_CALLCONV1
         xvt_str_create_codeset_map             XVT_CALLCONV2
         (
            XVT_IOSTREAM fromcodeset,
            XVT_IOSTREAM tocodeset
         );

extern   void                                   XVT_CALLCONV1
         xvt_str_destroy_codeset_map            XVT_CALLCONV2
         (
            XVT_CODESET_MAP codeset_map
         );

extern   char *                                 XVT_CALLCONV1
         xvt_str_duplicate                      XVT_CALLCONV2
         (
            const char * mbs
         );

extern   char *                                 XVT_CALLCONV1
         xvt_str_find_char_set                  XVT_CALLCONV2
         (
            const char * mbs,
            const char * mbset
         );

extern   char *                                 XVT_CALLCONV1
         xvt_str_find_eol                       XVT_CALLCONV2
         (
            char *       buf,
            long         nbytes,
            long *       lenp,
            EOL_FORMAT * fp
         );

extern   char *                                 XVT_CALLCONV1
         xvt_str_find_first_char                XVT_CALLCONV2
         (
            const char * mbs,
            const char * mbc
         );

extern   char *                                 XVT_CALLCONV1
         xvt_str_find_last_char                 XVT_CALLCONV2
         (
            const char * mbs,
            const char * mbc
         );

extern   char *                                 XVT_CALLCONV1
         xvt_str_find_first_wchar               XVT_CALLCONV2
         (
            const char * mbs,
            XVT_WCHAR    wc
         );

extern   char *                                 XVT_CALLCONV1
         xvt_str_find_last_wchar                XVT_CALLCONV2
         (
            const char * mbs,
            XVT_WCHAR    wc
         );

extern   char *                                 XVT_CALLCONV1
         xvt_str_find_not_char_set              XVT_CALLCONV2
         (
            const char * mbs,
            const char * mbset
         );

extern   char *                                 XVT_CALLCONV1
         xvt_str_find_substring                 XVT_CALLCONV2
         (
            const char * mbs1,
            const char * mbs2
         );

extern   char *                                 XVT_CALLCONV1
         xvt_str_find_token                     XVT_CALLCONV2
         (
            const char * mbs,
            const char * delimiter,
            size_t * n
         );

extern   size_t                                 XVT_CALLCONV1
         xvt_str_get_byte_count                 XVT_CALLCONV2
         (
            const char * mbs
         );

extern   size_t                                 XVT_CALLCONV1
         xvt_str_get_char_count                 XVT_CALLCONV2
         (
            const char * mbs
         );

extern   int                                    XVT_CALLCONV1
         xvt_str_get_char_size                  XVT_CALLCONV2
         (
            const char * mbs
         );

extern   char *                                 XVT_CALLCONV1
         xvt_str_get_mnemonic                   XVT_CALLCONV2
         (
            const char * label,
            short *      offset,
            char *       buf,
            const size_t buf_size
         );

extern   char *                                 XVT_CALLCONV1
         xvt_str_get_next_char                  XVT_CALLCONV2
         (
            const char * mbs
         );

extern   size_t                                 XVT_CALLCONV1
         xvt_str_get_n_char_count               XVT_CALLCONV2
         (
            const char * mbs,
            const size_t n,
            size_t * used
         );

extern   size_t                                 XVT_CALLCONV1
         xvt_str_get_n_char_size                XVT_CALLCONV2
         (
            const char * mbs,
            const size_t n
         );

extern   char *                                 XVT_CALLCONV1
         xvt_str_get_prev_char                  XVT_CALLCONV2
         (
            const char * start,
            const char * mbs
         );

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_str_is_alnum                       XVT_CALLCONV2
         (
            const char * mbs
         );

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_str_is_alpha                       XVT_CALLCONV2
         (
            const char * mbs
         );

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_str_is_cntrl                       XVT_CALLCONV2
         (
            const char * mbs
         );

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_str_is_digit                       XVT_CALLCONV2
         (
            const char * mbs
         );

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_str_is_equal                       XVT_CALLCONV2
         (
            const char * mbs1,
            const char * mbs2
         );

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_str_is_equal_n_size                XVT_CALLCONV2
         (
            const char * mbs1,
            const char * mbs2,
            size_t       n
         );

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_str_is_graph                       XVT_CALLCONV2
         (
            const char * mbs
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_str_is_invariant                   XVT_CALLCONV2
         (
            const char * mbs
         );

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_str_is_lower                       XVT_CALLCONV2
         (
            const char * mbs
         );

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_str_is_print                       XVT_CALLCONV2
         (
            const char * mbs
         );

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_str_is_punct                       XVT_CALLCONV2
         (
            const char * mbs
         );

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_str_is_space                       XVT_CALLCONV2
         (
            const char * mbs
         );

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_str_is_upper                       XVT_CALLCONV2
         (
            const char * mbs
         );

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_str_is_xdigit                      XVT_CALLCONV2
         (
            const char * mbs
         );

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_str_match                          XVT_CALLCONV2
         (
            const char *  src,
            const char *  pat,
            BOOLEAN case_sensitive
         );

extern   double                                 XVT_CALLCONV1
         xvt_str_parse_double                   XVT_CALLCONV2
         (
            const char * mbs,
            char ** mbsend
         );

extern   long                                   XVT_CALLCONV1
         xvt_str_parse_long                     XVT_CALLCONV2
         (
            const char * mbs,
            char ** mbsend,
			short base
         );

extern   unsigned long                          XVT_CALLCONV1
         xvt_str_parse_ulong                    XVT_CALLCONV2
         (
            const char * mbs,
            char ** mbsend,
			short base
         );

extern   char *                                 XVT_CALLCONV1
         xvt_str_set_mnemonic                   XVT_CALLCONV2
         (
            const char * label,
            const short  offset,
            char *       buf,
            const size_t buf_size
         );

extern   size_t                                 XVT_CALLCONV1
         xvt_str_sprintf                        XVT_CALLCONV2
         (
            char * mbs,
            const char * format,
            ...
         );

extern   long                                   XVT_CALLCONV1
         xvt_str_translate_codeset              XVT_CALLCONV2
         (
            XVT_CODESET_MAP codeset_map,
            char *string,
            char *strbuf,
            size_t bufsize
         );

extern   size_t                                 XVT_CALLCONV1
         xvt_str_vsprintf                       XVT_CALLCONV2
         (
            char * mbs,
            const char * format,
            va_list varg
         );

#endif /* XVT_INCL_XVTSTR */

