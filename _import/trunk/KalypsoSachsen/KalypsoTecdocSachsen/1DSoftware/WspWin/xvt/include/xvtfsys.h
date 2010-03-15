/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *  $RCSfile: xvtfsys.h,v $ 
 *  $Revision: 1.4 $
 *
 *  Purpose: Interface to the file system object.
 *
 ****************************************************************************/

#ifndef XVT_INCL_XVTFSYS
#define XVT_INCL_XVTFSYS

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_fsys_build_pathname                XVT_CALLCONV2
         (
			char *mbs,
			const char *volname,
			const char *dirname,
			const char *leafroot, 
			const char *leafext,
			const char *leafvers
         ) ;

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_fsys_convert_dir_to_str            XVT_CALLCONV2
         (
            DIRECTORY * dirp,
            char *      path,
            int         sz_path
         ) ;

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_fsys_convert_str_to_dir            XVT_CALLCONV2
         (
            char *      path,
            DIRECTORY * dirp
         ) ;


extern   void                                   XVT_CALLCONV1
         xvt_fsys_get_default_dir               XVT_CALLCONV2
         (
            DIRECTORY * dirp
         ) ;


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_fsys_get_dir                       XVT_CALLCONV2
         (
            DIRECTORY * dirp
         ) ;

extern	long                                    XVT_CALLCONV1
        xvt_fsys_get_file_attr                  XVT_CALLCONV2
        (
           FILE_SPEC* file,
           long       attr
        )  ;
	 

extern   SLIST                                  XVT_CALLCONV1
         xvt_fsys_list_files                    XVT_CALLCONV2
         (
            char *  type,
            char *  pat,
            BOOLEAN dirs
         ) ;


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_fsys_parse_pathname                XVT_CALLCONV2
         (
			const char *mbs,
			char *volname,
			char *dirname,
			char *leafroot, 
			char *leafext,
			char *leafvers
         ) ;

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_fsys_rem_file                      XVT_CALLCONV2
         (
            FILE_SPEC * file
         ) ;



extern   void                                   XVT_CALLCONV1
         xvt_fsys_restore_dir                   XVT_CALLCONV2
         (
            void
         ) ;


extern   void                                   XVT_CALLCONV1
         xvt_fsys_save_dir                      XVT_CALLCONV2
         (
            void
         ) ;

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_fsys_set_dir                       XVT_CALLCONV2
         (
            DIRECTORY * dirp
         ) ;


extern   void                                   XVT_CALLCONV1
         xvt_fsys_set_dir_startup               XVT_CALLCONV2
         (
            void
         ) ;


extern 	BOOLEAN                                 XVT_CALLCONV1
        xvt_fsys_set_file_attr                  XVT_CALLCONV2
        (
           FILE_SPEC* file,
           long       attr,
           long       value
        )  ;

#endif /* XVT_INCL_XVTFSYS */
