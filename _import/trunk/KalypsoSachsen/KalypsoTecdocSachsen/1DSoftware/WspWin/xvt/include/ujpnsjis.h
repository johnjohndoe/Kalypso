/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *  $RCSfile: ujpnsjis.h,v $
 *  $Revision: 1.9 $
 *
 *  Purpose: XVT URL standard resource string translation data for English
 *           in ASCII codeset.
 *
 
****************************************************************************/

/****************************************************************************
    Standard Menus Section
 
****************************************************************************/

/*
* Standard File Menu Labels
*/
#if (XVTWS == MACWS)
#define STR_M_FILE                      "�t�@�C��"
#define STR_M_FILE_NEW                  "�V�K"
#define STR_M_FILE_OPEN                 "�J��..."
#define STR_M_FILE_CLOSE                "����"
#define STR_M_FILE_SAVE                 "�ۑ�"
#define STR_M_FILE_SAVE_AS              "�ʖ��ŕۑ�..."
#define STR_M_FILE_REVERT               "��"
#define STR_M_FILE_PG_SETUP             "�p���ݒ�..."
#define STR_M_FILE_PRINT                "���..."
#define STR_M_FILE_QUIT                 "�I��"
#define STR_M_FILE_NEW_ACCEL            "N" ALT
#define STR_M_FILE_OPEN_ACCEL           "O" ALT
#define STR_M_FILE_CLOSE_ACCEL          "W" ALT
#define STR_M_FILE_SAVE_ACCEL           "S" ALT
#define STR_M_FILE_PRINT_ACCEL          "P" ALT
#define STR_M_FILE_QUIT_ACCEL           "Q" ALT
#elif (XVTWS == MTFWS)
#define STR_M_FILE                      "�t�@�C��(~F)"
#define STR_M_FILE_NEW                  "�V�K�쐬(~N)"
#define STR_M_FILE_OPEN                 "�J��...(~O)"
#define STR_M_FILE_SAVE                 "�㏑���ۑ�(~S)"
#define STR_M_FILE_SAVE_AS              "���O��t���ĕۑ�...(~A)"
#define STR_M_FILE_PG_SETUP             "�v�����^�̐ݒ�(~R)"
#define STR_M_FILE_PRINT                "���(~P)"
#define STR_M_FILE_QUIT                 "�I��(~X)"
#elif (XVTWS == WIN16WS) || (XVTWS == WIN32WS)
#define STR_M_FILE                      "�t�@�C��(~F)"
#define STR_M_FILE_NEW                  "�V�K�쐬(~N)"
#define STR_M_FILE_OPEN                 "�J��...(~O)"
#define STR_M_FILE_CLOSE                "����(~C)"
#define STR_M_FILE_SAVE                 "�㏑���ۑ�(~S)"
#define STR_M_FILE_SAVE_AS              "���O��t���ĕۑ�...(~A)"
#define STR_M_FILE_REVERT               "�ۑ��̏�Ԃɖ߂�(~V)"
#define STR_M_FILE_PG_SETUP             "�v�����^�̐ݒ�...(~R)"
#define STR_M_FILE_PRINT                "���...(~P)"
#define STR_M_FILE_QUIT                 "�I��(~X)"
#elif (XVTWS == PMWS)
#define STR_M_FILE                      "�t�@�C��(~F)"
#define STR_M_FILE_NEW                  "�V�K(~N)"
#define STR_M_FILE_OPEN                 "�I�[�v��...(~O)"
#define STR_M_FILE_SAVE                 "�ۊ�(~S)"
#define STR_M_FILE_SAVE_AS              "�ʂ̖��O�ŕۊ�...(~A)"
#define STR_M_FILE_REVERT               "�ۑ��̏�Ԃɖ߂�(~V)"
#define STR_M_FILE_PG_SETUP             "�v�����^�ݒ�...(~R)"
#define STR_M_FILE_PRINT                "���...(~P)"
#define STR_M_FILE_QUIT                 "�I��(~X)"
#define STR_M_FILE_ABOUT                "���i���(~B)"
#endif

/*
* Standard Edit Menu Labels
*/
#if (XVTWS == MACWS)
#define STR_M_EDIT                      "�ҏW"
#define STR_M_EDIT_UNDO                 "������"
#define STR_M_EDIT_CUT                  "�J�b�g"
#define STR_M_EDIT_COPY                 "�R�s�["
#define STR_M_EDIT_PASTE                "�y�[�X�g"
#define STR_M_EDIT_CLEAR                "����"
#define STR_M_EDIT_SEL_ALL              "���ׂĂ�I��"
#define STR_M_EDIT_CLIPBOARD            "�N���b�v�{�[�h�\��"
#define STR_M_EDIT_UNDO_ACCEL           "Z" ALT
#define STR_M_EDIT_CUT_ACCEL            "X" ALT
#define STR_M_EDIT_COPY_ACCEL           "C" ALT
#define STR_M_EDIT_PASTE_ACCEL          "V" ALT
#define STR_M_EDIT_SEL_ALL_ACCEL        "A" ALT
#elif (XVTWS == MTFWS)
#define STR_M_EDIT                      "�ҏW(~E)"
#define STR_M_EDIT_UNDO                 "���ɖ߂�(~U)"
#define STR_M_EDIT_CUT                  "�؂���(~T)"
#define STR_M_EDIT_COPY                 "�R�s�[(~C)"
#define STR_M_EDIT_PASTE                "�\��t��(~P)"
#define STR_M_EDIT_CLEAR                "�폜(~D)"
#define STR_M_EDIT_UNDO_ACCEL           BACK ALT
#define STR_M_EDIT_CUT_ACCEL            DEL SHIFT
#define STR_M_EDIT_COPY_ACCEL           INS CONTROL
#define STR_M_EDIT_PASTE_ACCEL          INS SHIFT
#elif (XVTWS == WIN16WS) || (XVTWS == WIN32WS)
#define STR_M_EDIT                      "�ҏW(~E)"
#define STR_M_EDIT_UNDO                 "���ɖ߂�(~U)\tCtrl+Z"
#define STR_M_EDIT_CUT                  "�؂���(~T)\tCtrl+X"
#define STR_M_EDIT_COPY                 "�R�s�[(~C)\tCtrl+C"
#define STR_M_EDIT_PASTE                "�\��t��(~P)\tCtrl+V"
#define STR_M_EDIT_CLEAR                "�폜(~D)"
#define STR_M_EDIT_UNDO_ACCEL           "Z" CONTROL
#define STR_M_EDIT_CUT_ACCEL            "X" CONTROL
#define STR_M_EDIT_COPY_ACCEL           "C" CONTROL
#define STR_M_EDIT_PASTE_ACCEL          "V" CONTROL
#elif (XVTWS == PMWS)
#define STR_M_EDIT                      "�ҏW(~E)"
#define STR_M_EDIT_UNDO                 "��蒼��(~U)\tAlt+BS"
#define STR_M_EDIT_COPY                 "����(~C)\tCtrl+Ins"
#define STR_M_EDIT_PASTE                "�\�t��(~P)\tShift+Ins"
#ifndef NO_DEL_ACCEL
#define STR_M_EDIT_CUT                  "�ؔ���(~T)\tShift+Del"
#define STR_M_EDIT_CLEAR                "�N���A(~E)\tDel"
#else
#define STR_M_EDIT_CUT                  "�ؔ���(~T)"
#define STR_M_EDIT_CLEAR                "�N���A(~E)"
#endif
#define STR_M_EDIT_UNDO_ACCEL           BACK ALT
#define STR_M_EDIT_CUT_ACCEL            DEL SHIFT
#define STR_M_EDIT_COPY_ACCEL           INS CONTROL
#define STR_M_EDIT_PASTE_ACCEL          INS SHIFT
#define STR_M_EDIT_CLEAR_ACCEL          DEL
#endif

/*
* Standard Font Menu Labels
*/
#if (XVTWS == MACWS)
#define STR_M_FONT                      "�t�H���g"
#elif (XVTWS == MTFWS)
#define STR_M_FONT                      "�t�H���g(~T)"
#define STR_M_FONT_COURIER              "Courier(~C)"
#define STR_M_FONT_HELVETICA            "Helvetica(~H)"
#define STR_M_FONT_SYSTEM               "�V�X�e��(~S)"
#define STR_M_FONT_TIMES                "Times(~T)"
#define STR_M_FONT_OTHER                "���̑�(~O)"
#elif (XVTWS == WIN16WS) || (XVTWS == WIN32WS) || (XVTWS == PMWS)
#define STR_M_FONT                      "�t�H���g(~T)"
#define STR_M_FONT_SELECT               "�I��...(~S)"
#endif /* (XVTWS == MACWS) */

/*
* Standard Style Menu Labels
*/
#if (XVTWS == MACWS)
#define STR_M_STYLE                     "�X�^�C��"
    /* I18N - This string contains the menu items on the first half of the */
    /*        Style Menu. */
    /*        The /x characters (ie "/T") are Mac Menu Manager specific codes */
    /*           to determine the menu accelerator */
    /*        The <y characters (ie "<B") are Mac Menu Manager specific */
    /*           codes to set the font style for the menu item. */
    /*        The "-(" item is a Mac Menu Manager specific code to specify a */
    /*           menu separator line */
    /*        All items are separated by semi-colons ";" */
#define STR_MAC_STYLEM1                 "�W��/T;-(;�{�[���h<B/B;�C�^���b�N<I/I;�A���_�[���C��<U/U;�A�E�g���C��<O;�V���h�E<S;-(;�R���f���X;�G�N�X�e���h"

     /* I18N - This string represents the the order of font modifier action in */
    /*        STR_MAC_STYLEM1 above. The characters in this string should not */
    /*        be changed but the order may need to be changed. The characters have */
    /*        the following font modifier action representation:              */
    /*             P = Plain Text */
    /*             B = Bold Text */
    /*             I = Italic Text */
    /*             U = Underline Text */
    /*             O = Outline Text */
    /*             S = Shadow Text */
    /*             C = Condensed Text */
    /*             E = Extended Text */
    /*             space = separator or other non-font modifier action menu item */
    /*        The position of these characters must match exactly to the position of */
    /*        the corresponding item in the menu described by STR_MAC_STYLEM1. */
#define STR_MAC_STYLEM2                 "P BIUOS CE"

    /* I18N - This string contains the menu items on the second half of the */
    /*        Style Menu. */
    /*        This string assumes that the decimal number */
    /*           (ie "10" in "10 point") is first for each item and */
    /*           represents the font point size. */
    /*        A non-decimal character cannot begin the item. */
    /*        The %d represents the default point size for the Other dialog */
#define STR_MAC_STYLEM3                 "-(;9;10;12;14;18;24;36;48;72;���̑�...%s%d%s"
#elif (XVTWS == MTFWS)
#define STR_M_STYLE                     "�X�^�C��(~S)"
#define STR_M_STYLE_NORMAL              "�W��(~N)"
#define STR_M_STYLE_BOLD                "����(~B)"
#define STR_M_STYLE_ITALIC              "�Α�(~I)"
#define STR_M_STYLE_8                   " 8�|�C���g"
#define STR_M_STYLE_10                  "10�|�C���g"
#define STR_M_STYLE_12                  "12�|�C���g"
#define STR_M_STYLE_14                  "14�|�C���g"
#define STR_M_STYLE_18                  "18�|�C���g"
#define STR_M_STYLE_24                  "24�|�C���g"
#endif

/*
* Standard Help Menu Labels
*/
#if (XVTWS == MACWS)
#define STR_M_HELP                      "�w���v"
#define STR_M_HELP_ONWINDOW             "���̃E�B���h�E"
#define STR_M_HELP_SEARCH               "����..."
#define STR_M_HELP_CONTENTS             "�ڎ�"
#define STR_M_HELP_INDEX                "����"
#define STR_M_HELP_OBJCLICK             "�I�u�W�F�N�g��N���b�N"
#define STR_M_HELP_HELPONHELP           "�w���v�Ɋւ���w���v"
#define STR_M_HELP_VERSION              "...�ɂ���"
#elif (XVTWS == MTFWS)
#define STR_M_HELP                      "�w���v(~H)"
#define STR_M_HELP_ONWINDOW             "�E�B���h�E�ɂ���(~W)"
#define STR_M_HELP_CONTENTS             "�ڎ�(~N)"
#define STR_M_HELP_INDEX                "����(~I)"
#define STR_M_HELP_OBJCLICK             "�R���e�L�X�g�ɂ���(~C)"
#define STR_M_HELP_HELPONHELP           "�w���v�ɂ���(~H)"
#define STR_M_HELP_VERSION              "�o�[�W�������(~V)"
#define STR_M_HELP_KEYBOARD             "�L�[�ɂ���(~K)"
#elif (XVTWS == WIN16WS) || (XVTWS == WIN32WS)
#define STR_M_HELP                      "�w���v(~H)"
#define STR_M_HELP_CONTENTS             "�ڎ�(~C)"
#define STR_M_HELP_SEARCH               "�L�[���[�h�Ō���(~S)"
#define STR_M_HELP_HELPONHELP           "�w���v�̎g����(~H)"
#define STR_M_HELP_OBJCLICK             "�I�u�W�F�N�g��N���b�N(~O)\tShift+F1"
#define STR_M_HELP_VERSION              "�o�[�W�������(~A)..."
#define STR_M_HELP_CONTENTS_ACCEL       F1 ALT
#define STR_M_HELP_SEARCH_ACCEL         F1 CONTROL
#define STR_M_HELP_OBJCLICK_ACCEL       F1 SHIFT
#elif (XVTWS == PMWS)
#define STR_M_HELP                      "�w���v(~H)"
#define STR_M_HELP_INDEX                "�w���v����(~I)"
#define STR_M_HELP_ONWINDOW             "��ʃw���v(~G)"
#define STR_M_HELP_HELPONHELP           "�w���v�g�p�@(~U)"
#define STR_M_HELP_KEYBOARD             "�L�[�w���v(~K)"
#define STR_M_HELP_OBJCLICK             "�I�u�W�F�N�g��N���b�N(~O)\tAlt+F1"
#define STR_M_HELP_VERSION              "�o�[�W�������(~P)"
#define STR_M_HELP_ONCONTEXT_ACCEL      F1
#define STR_M_HELP_OBJCLICK_ACCEL       F1 ALT
#endif /* (XVTWS == MACWS) */

/*
* Windows MDI Menus
*/
#if (XVTWS == WIN16WS) || (XVTWS == WIN32WS)
#define STR_XVT_WINDOWMENU              "�E�B���h�E�\��(&W)"
#define STR_IDM_WINDOWTILE              "���ׂĕ\��(~T)"
#define STR_IDM_WINDOWCASCADE           "�d�˂ĕ\��(~C)"
#define STR_IDM_WINDOWICONS             "�A�C�R���̐���(~A)"
#endif /* (XVTWS == WIN16WS) || (XVTWS == WIN32WS) */

/****************************************************************************
    Standard Dialogs Section
 
****************************************************************************/

/*
* Standard Error Dialog
*/
#define STR_DB_ERROR                    ""
#if (XVTWS == WIN16WS) || (XVTWS == WIN32WS)
#define STR_DB_ERROR_OK                 ""
#define STR_DB_ERROR_CANCEL             ""
#define STR_DB_ERROR_NO                 ""
#define STR_DB_ERROR_TEXT               ""
#else
#define STR_DB_ERROR_OK                 "OK"
#endif

/*
* Standard Note Dialog
*/
#define STR_DB_NOTE                     ""
#if (XVTWS == WIN16WS) || (XVTWS == WIN32WS)
#define STR_DB_NOTE_OK                  ""
#define STR_DB_NOTE_CANCEL              ""
#define STR_DB_NOTE_NO                  ""
#define STR_DB_NOTE_TEXT                ""
#else
#define STR_DB_NOTE_OK                  "OK"
#endif
/*
* Standard Warning Dialog
*/
#define STR_DB_WARNING                  ""
#define STR_DB_WARNING_OK               "OK"

/*
* Standard Abort Dialog
*/
#if (XVTWS == MACWS)
#define STR_DB_ABORT                    ""
#define STR_DB_ABORT_TEXT               "����𒆎~����ɂ�\n\021�������Ȃ���s���I�h(.)���^�C�v���ĉ������B"
#elif (XVTWS == WIN16WS) || (XVTWS == WIN32WS)
#define STR_DB_ABORT_CANCEL             "�L�����Z��"
#define STR_DB_ABORT_PRINTING           "�����"
#define STR_DB_ABORT_TEXT1              ""
#define STR_DB_ABORT_TEXT2              ""
#elif (XVTWS == PMWS)
#define STR_DB_ABORT_CANCEL             "���"
#define STR_DB_ABORT_PRINTING           "�����"
#define STR_DB_ABORT_TEXT1              ""
#define STR_DB_ABORT_TEXT2              ""
#endif

/*
* Standard Ask Dialog
*/
#define STR_DB_ASK                      ""
#define STR_DB_ASK_OK                   ""
#define STR_DB_ASK_CANCEL               ""
#define STR_DB_ASK_OTHER                ""
#define STR_DB_ASK_TEXT                 ""
#if (XVTWS == PMWS)
#define STR_DB_ASK_HELP                 "�w���v"
#endif

/*
* Standard Response Dialog
*/
#if (XVTWS == MACWS)
#define STR_DB_RESPONSE                 "�����񉞓��_�C�A���O"
#elif (XVTWS == MTFWS)
#define STR_DB_RESPONSE                 ""
#elif (XVTWS == WIN16WS) || (XVTWS == WIN32WS)
#define STR_DB_RESPONSE_EDIT            ""
#define STR_DB_RESPONSE_TEXT            ""
#elif (XVTWS == PMWS)
#define STR_DB_RESPONSE_HELP            "�w���v"
#define STR_DB_RESPONSE_TEXT            ""
#endif /* (XVTWS == MACWS) */
#define STR_DB_RESPONSE_OK              "OK"
#define STR_DB_RESPONSE_CANCEL          "�L�����Z��"

/*
* Standard Font Selection Dialog
*/
#if (XVTWS == MACWS)
#define STR_DB_FONTSEL                  "�t�H���g�I���_�C�A���O"
#define STR_DB_FONTSEL_OK               "OK"
#define STR_DB_FONTSEL_CANCEL           "�L�����Z��"
#define STR_DB_FONTSEL_FONT             "�t�H���g:"
#define STR_DB_FONTSEL_STYLE            "�X�^�C��"
#define STR_DB_FONTSEL_BOLD             "�{�[���h"
#define STR_DB_FONTSEL_ITALIC           "�C�^���b�N"
#define STR_DB_FONTSEL_UNDERLINE        "�A���_�[���C��"
#define STR_DB_FONTSEL_OUTLINE          "�A�E�g���C��"
#define STR_DB_FONTSEL_SHADOW           "�V���h�E"
#define STR_DB_FONTSEL_SPACING          "������"
#define STR_DB_FONTSEL_NORMAL           "�W��"
#define STR_DB_FONTSEL_CONDENSED        "�R���f���X"
#define STR_DB_FONTSEL_EXTENDED         "�G�N�X�e���h"
#define STR_DB_FONTSEL_SIZE             "�T�C�Y:"
#define STR_DB_FONTSEL_RECT             174,  68, 409, 226
#define STR_DB_FONTSEL_OK_RECT          329, 196,  70,  20
#define STR_DB_FONTSEL_CANCEL_RECT      329, 163,  70,  20
#define STR_DB_FONTSEL_FONT_RECT         23,  16, 150,  16
#define STR_DB_FONTSEL_LIST_RECT         23,  35, 150, 136
#define STR_DB_FONTSEL_STYLE_RECT       195,  16, 117, 113
#define STR_DB_FONTSEL_BOLD_RECT        200,  34, 108,  18
#define STR_DB_FONTSEL_ITALIC_RECT      200,  52, 108,  18
#define STR_DB_FONTSEL_UNDERLINE_RECT   200,  70, 108,  18
#define STR_DB_FONTSEL_OUTLINE_RECT     200,  88, 108,  18
#define STR_DB_FONTSEL_SHADOW_RECT      200, 106, 108,  18
#define STR_DB_FONTSEL_SPACING_RECT     195, 139, 117,  77
#define STR_DB_FONTSEL_NORMAL_RECT      200, 157, 108,  18
#define STR_DB_FONTSEL_CONDENSED_RECT   200, 175, 108,  18
#define STR_DB_FONTSEL_EXTENDED_RECT    200, 193, 108,  18
#define STR_DB_FONTSEL_SIZE_RECT      	 23, 181, 150,  16
#define STR_DB_FONTSEL_LISTEDIT_RECT     23, 197,  69,  22
#endif /* (XVTWS == MACWS) */

/*
* Standard Font Size Selection Dialog
*/
#if (XVTWS == MACWS)
#define STR_DB_FONTSIZE                 "���̑�"
#define STR_DB_FONTSIZE_OK              "OK"
#define STR_DB_FONTSIZE_CANCEL          "�L�����Z��"
#define STR_DB_FONTSIZE_FONTSIZE        "�T�C�Y:" /* "�t�H���g�T�C�Y:" */
#define STR_DB_FONTSIZE_POINTS          "�|�C���g"
#define STR_DB_FONTSIZE_RECT            174,  68, 217,  85
#define STR_DB_FONTSIZE_OK_RECT         147,  55,  60,  20
#define STR_DB_FONTSIZE_CANCEL_RECT      64,  55,  70,  20
#define STR_DB_FONTSIZE_EDIT_RECT        56,  13,  50,  22
#define STR_DB_FONTSIZE_FONTSIZE_RECT    10,  16,  41,  16
#define STR_DB_FONTSIZE_POINTS_RECT     111,  16,  55,  16
#endif /* (XVTWS == MACWS) */

/*
* Standard About Dialog
*/
#define STR_DB_ABOUT                    ""
#define STR_DB_ABOUT_OK                 "�w���v"
#define STR_DB_ABOUT_CANCEL             "�L�����Z��"
#if (XVTWS == WIN16WS) || (XVTWS == WIN32WS) || (XVTWS == PMWS)
#define STR_DB_ABOUT_TEXT1              "XVT�A�v���P�[�V����"
#define STR_DB_ABOUT_TEXT2              "�o�[�W����4.57"
#define STR_DB_ABOUT_TEXT3              "XVT Software."
#elif (XVTWS == MACWS)
#define STR_DB_ABOUT_TEXT1              "XVT�A�v���P�[�V�����o�[�W����4.57"
#define STR_DB_ABOUT_TEXT2              "XVT Software."
#else
#define STR_DB_ABOUT_TEXT1              "���̃v���O�����́AXVT�iExtensible Virtual Toolkit�j��"
#define STR_DB_ABOUT_TEXT2              "�g�p���ĊJ�����܂����B"
#endif

/*
* Standard Open File Dialog
*/
#if (XVTWS == MACWS)
#define STR_DB_OPENFILE_SELECT          "�I��"
#elif (XVTWS == MTFWS)
#define STR_DB_OPENFILE                 ""
#define STR_DB_OPENFILE_OPEN            "�J��"

#endif /* (XVTWS == MACWS) */

/*
* Standard Page Setup Dialog
*/
#if (XVTWS == MTFWS)
#define STR_D_PAGE_SETUP_DIALOG         "�v�����^�y�[�W�@���C�A�E�g�̐ݒ�"
#define STR_D_PAGE_SIZE                 "�p���T�C�Y"
#define STR_D_PAGE_USLETTER             "US Letter"
#define STR_D_PAGE_USLEGAL              "US Legal"
#define STR_D_PAGE_A4LETTER             "A4 Letter"
#define STR_D_PAGE_B5LETTER             "B5 Letter"
#define STR_D_PAGE_ORIENTATION          "����̌���"
#define STR_D_PAGE_OLANDSCAPE           "��"
#define STR_D_PAGE_OPORTRAIT            "�c"
#define STR_D_PAGE_OPT_ENLARGMENT       "�{��(%)"
#define STR_D_PAGE_INP_ENLARGMENT       "100"
#define STR_D_PAGE_IMAGE                "�L�����o�X"
#define STR_D_PAGE_IMAGE_MONO           "���m�N��"
#define STR_D_PAGE_IMAGE_GRAY           "�O���C�X�P�[��"
#define STR_D_PAGE_IMAGE_COLOR          "�J���[�\��"
#define STR_D_PAGE_OK                   "OK"
#define STR_D_PAGE_CANCEL               "�L�����Z��"
#define STR_D_PAGE_SETUP_DIALOG_RCT     140,  60, 250, 250
#define STR_D_PAGE_SIZE_RCT              15,  10, 100, 110
#define STR_D_PAGE_USLETTER_RCT          20,  35,  90,  16
#define STR_D_PAGE_USLEGAL_RCT           20,  55,  90,  16
#define STR_D_PAGE_A4LETTER_RCT          20,  75,  90,  16
#define STR_D_PAGE_B5LETTER_RCT          20,  95,  90,  16
#define STR_D_PAGE_ORIENTATION_RCT      135,  10, 100,  80
#define STR_D_PAGE_OLANDSCAPE_RCT       140,  35,  90,  16
#define STR_D_PAGE_OPORTRAIT_RCT        140,  55,  90,  16
#define STR_D_PAGE_OPT_ENLARGMENT_RCT    15, 130, 100,  60
#define STR_D_PAGE_INP_ENLARGMENT_RCT    30, 155,  70,  16
#define STR_D_PAGE_IMAGE_RCT            135, 100, 100,  90
#define STR_D_PAGE_IMAGE_MONO_RCT       140, 125,  90,  16
#define STR_D_PAGE_IMAGE_GRAY_RCT       140, 145,  90,  16
#define STR_D_PAGE_IMAGE_COLOR_RCT      140, 165,  90,  16
#define STR_D_PAGE_OK_RCT                30, 210,  70,  25
#define STR_D_PAGE_CANCEL_RCT           150, 210,  70,  25

#elif (XVTWS == PMWS)
#define STR_STR_DB_PRSETUP              "�v�����^�[�ݒ�"
#define STR_STR_DB_PRSETUP_OK           "����"
#define STR_STR_DB_PRSETUP_CANCEL       "���"
#define STR_STR_DB_PRSETUP_SETUP        "~�ݒ�"
#define STR_STR_DB_PRSETUP_TEXT         "�g�ݍ��܂�Ă���v�����^�["
#endif /* (XVTWS == MTFWS) */

/****************************************************************************
    Platform Specific String Section
 
****************************************************************************/

#if (XVTWS == MACWS)
#define STR_MAC_SIGNATURE               "R4 XVT�A�v���P�[�V����"
#define STR_MAC_HELP                    "�w���v"
    /* I18N - This string contains the about item on the Apple menu. */
    /*        The %s represents the application name from appl_name in */
    /*        the XVT_CONFIG structure */
#define STR_MAC_ABOUT                   "%s �ɂ���..."
#define STR_MAC_CLICK                   "������ɂ̓}�E�X���N���b�N���Ă��������B"
    /* I18N - This string is used in the open file dialog to select directories. */
    /*        The %s represents the current directory name selected. */
#define STR_MAC_SELECT                  "�I�� \322%s\323"
    /* I18N - This string is used in the notification manager routines. */
    /*        The %s represents the current application name. */
#define STR_MAC_NOTIFICATION			"�A�v���P�[�V���� \322%1$s\323 ���m�F���ĉ������B\n\n"
#define STR_MAC_LOW_MEM_WARNING			"�������[���s�����Ă��܂��B�A�v���P�[�V�����́u��������v�Ń������[���m�ۂ��ĉ������B"
#elif (XVTWS == MTFWS)
#define STR_WM_HELPTYPE                 "hlp"
#define STR_WM_PRINT_001                "�p���ݒ���A�N�Z�X���ɃG���["
#define STR_WM_PRINT_002                "xvtprolg.ps"
    /* I18N - This string contains a printing error message. */
    /*        The %s represents the file name of a font file */
#define STR_WM_PRINT_003                "AFM�t�@�C��%s�ɃA�N�Z�X�ł��܂���B"
#elif (XVTWS == WIN16WS) || (XVTWS == WIN32WS)
#define STR_WIN_HELPTYPE                "HLP"
/* strings used in bothstr.h and as default string in SGETSTR */
#define STR_WIN_OK                      "OK"
#define STR_WIN_CANCEL                  "�L�����Z��"
#define STR_WIN_CANT_SHOW_ABOUT_BOX     "�o�[�W��������\���ł��܂���B"
#define STR_WIN_CANCELLING              "�L�����Z����..."
#define STR_WIN_INITIALIZING            "��������"
#define STR_WIN_UNTITLED                "(���̖��ݒ�)"
#define STR_WIN_STARTING_DOC            "(�����J�n)"
#define STR_WIN_STARTED_DOC             "(�����J�n)"
#define STR_WIN_ENDING_DOC              "(�����I��)"
    /* I18N - This string contains a message from the print dialog. */
    /*        The %d represents the page number of the current printing page */
#define STR_WIN_STARTING_PAGE           "(%d�y�[�W���J�n)"
    /* I18N - This string contains a message from the print dialog. */
    /*        The %d represents the page number of the finished printing page */
#define STR_WIN_FINISHING_PAGE          "(%d�y�[�W���I����)"
#define STR_WIN_SYSTEM_ERROR            "�V�X�e���G���["
#define STR_WIN_PICTURE_STILL_LOCKED    "�G�́A�܂����b�N���ꂽ�܂܂ł��B"
#define STR_WIN_CANNOT_PRINT            "����ł��܂���"
#define STR_WIN_ALERT                   "���b�Z�[�W��\��"
#define STR_WIN_FDLG_FILTER             "�t�@�C��(*.%s)|*.%s|"
#define STR_WIN_FDLG_FILTER_ALL         "���ׂẴt�@�C��(*.*)|*.*|"
#define STR_WIN_NO_EXT_RES              "���\�[�X�t�@�C����������܂���B�W�����\�[�X���g�p���܂��B"
#define STR_WIN_DEFAULTFAMILY           "�l�r �S�V�b�N"
#define STR_WIN_SYSTEMFAMILY            "System"
#define STR_WIN_HELVETICAFAMILY         "�l�r �S�V�b�N"
#define STR_WIN_FIXEDFAMILY             "�l�r ����"
#define STR_WIN_TIMESFAMILY             "�l�r ����"
#define STR_WIN_COURIERFAMILY           "�l�r ����"
#elif (XVTWS == PMWS)
#define STR_PM_HELPTYPE                 "hlp"
#define STR_PM_OK                       "����"
#define STR_PM_CANCEL                   "���"
#define STR_PM_CANT_SHOW_ABOUT_BOX      "�o�[�W�������{�b�N�X��\���ł��܂���B"
#define STR_PM_CANCELLING               "�����..."
#define STR_PM_INITIALIZING             "��������"
#define STR_PM_UNTITLED                 "(���̖��ݒ�)"
#define STR_PM_STARTING_DOC             "(�������J�n���܂�)"
#define STR_PM_STARTED_DOC              "(�������J�n���܂���)"
#define STR_PM_ENDING_DOC               "(�������I�����܂�)"
    /* I18N - This string contains a message from the print dialog. */
    /*        The %d represents the page number of the current printing page */
#define STR_PM_STARTING_PAGE            "(%d�y�[�W��������ł�)"
    /* I18N - This string contains a message from the print dialog. */
    /*        The %d represents the page number of the finished printing page */
#define STR_PM_FINISHING_PAGE           "(%d�y�[�W�̈�����I�����܂�)"
#define STR_PM_SYSTEM_ERROR             "�V�X�e���G���["
#define STR_PM_PICTURE_STILL_LOCKED     "�s�N�`���[�����b�N���ꂽ�܂܂ł��B"
#define STR_PM_CANNOT_PRINT             "����ł��܂���B"
#define STR_PM_OPENING_PRINTER          "(�v�����^�[���I�[�v�����܂�)"
#define STR_PM_NO_ERROR_INFO_AVAILABLE  "�G���[���͂���܂���B"
#define STR_PM_PRINTING_ERROR           "����G���["
#define STR_PM_CANT_CREATE_PRINT_THREAD "�v�����g��X���b�h���쐬�ł��܂���ł����B"
    /* I18N - This string contains PM system errors. */
    /*        The %x represents a hex system error id */
#define STR_PM_ERROR                    "PM�G���[0x%x"
#define STR_PM_ESCAPE_FAILED            "�v�����^�[�G�X�P�[�v�����s���܂����B"
#define STR_PM_ALT                      "Alt"
#define STR_PM_CTRL                     "Ctrl"
#define STR_PM_CTRLF6                   "~���̃E�B���h�E\tCtrl+F6"
#define STR_PM_OPEN                     "�I�[�v��"
#define STR_PM_SAVEAS                   "���O��t���ĕۊ�"
#define STR_PM_DEFAULTFAMILY            "MINCHO"
#define STR_PM_COURIERFAMILY            "GOTHIC"
#define STR_PM_FIXEDFAMILY              "GOTHIC"
#define STR_PM_HELVETICAFAMILY          "MINCHO"
#define STR_PM_SYSTEMFAMILY             "MINCHO System"
#define STR_PM_TIMESFAMILY              "GOTHIC"
#endif /* (XVTWS == MACWS) */

/****************************************************************************
    Help System Section
 
****************************************************************************/

/*
 * R4 Help System Error Messages
 */
#define STR_HELP_CLIP_ERR               "�������ݗp�ɃN���b�v�{�[�h���J���܂���B"
#define STR_HELP_MEM_ERR                "�������[�s�\��"
#define STR_HELP_CLIP_PUT_ERR           "�N���b�v�{�[�h�ɏ���\�邱�Ƃ��ł��܂���B"
#define STR_HELP_PRINT_ERR              "����ł��܂���ł����B"
#define STR_HELP_PRINT_OK               "�g�s�b�N���������܂����B"
#define STR_HELP_CLIP_OK                "�g�s�b�N���N���b�v�{�[�h�ɃR�s�[����܂����B"
    /* I18N - This string is used to signify the topic sequence number in */
    /*        the help viewer */
    /*        The first %d represents which topic in a sequence of topics */
    /*        that is currently being viewed */
    /*        The second %d represents the total number of topics that have */
    /*        been viewed */
#define STR_HELP_THREAD_INFO            "�g�s�b�N: %d �� %d"
    /* I18N - This string is used to show if a topic has been marked */
    /*        (bookmarks) */
    /*        The %c represents a the single byte character 'x' */
    /*        or ' ' (space). */
    /*        This means the item is either marked (x) or unmarked ( ). */
#define STR_HELP_MARKED_INFO            " [%c]�}�[�N�𗘗p"
#define STR_HELP_COPYPART               "�I������܂���ł����B�R�s�[������̂�����܂���B"

/*
* R4 Help System Topic Button Labels
*/
#define STR_HELP_MARK                   "�}�[�N"
#define STR_HELP_UNMARK                 "�}�[�N����菜��"

/*
* R4 Help System Topic Navigation Menu
*/
#if (XVTWS == MACWS)
#define STR_HELP_NAV_SEARCH             SEARCH_STR
#define STR_HELP_NAV_GOTO               "�W�����v..."
#define STR_HELP_NAV_MARK               "��������w��"
#define STR_HELP_NAV_BACKLINK           "�g�s�b�N�� �߂�"
#define STR_HELP_NAV_FORWLINK           "�g�s�b�N�ɐi��"
#define STR_HELP_NAV_PREVPAGE           "�O�̃g�s�b�N <<"
#define STR_HELP_NAV_NEXTPAGE           "���̃g�s�b�N >>"
#else
#define STR_HELP_NAV_SEARCH             SEARCH_STR
#define STR_HELP_NAV_GOTO               "�W�����v(~G)..."
#define STR_HELP_NAV_MARK               "��������w��(~M)"
#define STR_HELP_NAV_BACKLINK           "�g�s�b�N�� �߂�(~B)"
#define STR_HELP_NAV_FORWLINK           "�g�s�b�N�ɐi��(~F)"
#define STR_HELP_NAV_PREVPAGE           "�O�̃g�s�b�N(~P)<<"
#define STR_HELP_NAV_NEXTPAGE           "���̃g�s�b�N(~N)>>"
#endif /* MACWS */

/*
* R4 Help System Menu Bar
*/
#if (XVTWS == MACWS)
#define STR_MHELP_FILE                  "�t�@�C��"
#define STR_MHELP_EDIT                  "�ҏW"
#define STR_MHELP_NAV                   "�i�r�Q�[�g����"
#define STR_MHELP_HELP                  "�w���v"
#else
#define STR_MHELP_FILE                  "�t�@�C��(~F)"
#define STR_MHELP_EDIT                  "�ҏW(~E)"
#define STR_MHELP_NAV                   "�i�r�Q�[�g����(~N)"
#define STR_MHELP_HELP                  "�w���v(~H)"
#endif /* MACWS */

/*
* R4 Help System File Menu Labels
*/
#if (XVTWS == MACWS)
#define STR_MHELP_FILE_PRINT            "���..."
#define STR_MHELP_FILE_PRINT_SETUP      "�p���ݒ�..."
#define STR_MHELP_FILE_EXIT             "�I��"
#define STR_MHELP_FILE_EXIT_ACCEL       "Q" ALT
#define STR_MHELP_FILE_PRINT_ACCEL      "P" ALT
#else  /* !MACWS */
#define STR_MHELP_FILE_PRINT            "���(~P)"
#define STR_MHELP_FILE_PRINT_SETUP      "�v�����^�̐ݒ�(~R)..."
#define STR_MHELP_FILE_EXIT             "�I��(~X)"
#endif /* !MACWS */

/*
* R4 Help System Edit Menu Labels
*/
#if (XVTWS == WIN16WS) || (XVTWS == WIN32WS) || (XVTWS == PMWS)
#define STR_MHELP_EDIT_COPY             "�R�s�[(~C)\tCtrl+C"
#define STR_MHELP_TSE_EDIT_COPY         "�R�s�[(~C)\tCtrl+C"
#define STR_MHELP_EDIT_COPY_ACCEL       "C" CONTROL
#elif (XVTWS == MACWS)
#define STR_MHELP_EDIT_COPY             "�R�s�["
#define STR_MHELP_TSE_EDIT_COPY         "�R�s�["
#else  /* ! ((XVTWS == WIN16WS) || (XVTWS == WIN32WS) || (XVTWS == PMWS)) */
#define STR_MHELP_EDIT_COPY             "�R�s�[(~C)"
#define STR_MHELP_TSE_EDIT_COPY         "�R�s�[(~C)"
#endif /* (XVTWS == WIN16WS) || (XVTWS == WIN32WS) || (XVTWS == PMWS) */

#if (XVTWS == MACWS)
#define STR_MHELP_EDIT_COPYPART         "�g�s�b�N�̈ꕔ���R�s�[����..."
#define STR_MHELP_EDIT_AS_WRAPPED       "���b�v�ǂ���ɃR�s�[����"
#elif (XVTWS == MTFWS)
#define STR_MHELP_EDIT_AS_WRAPPED       "���b�v�ǂ���ɃR�s�[����(~W)"
#else
#define STR_MHELP_EDIT_COPYPART         "�g�s�b�N�̈ꕔ���R�s�[����(~P)..."
#define STR_MHELP_EDIT_AS_WRAPPED       "���b�v�ǂ���ɃR�s�[����(~W)"
#endif

/*
* R4 Help System Navigation Menu Labels
*/
#if (XVTWS == MACWS)
#define STR_MHELP_NAV_SEARCH            "����..."
#define STR_MHELP_NAV_GOTO              "�W�����v..."
#define STR_MHELP_NAV_MARK              "������"
#define STR_MHELP_NAV_BACKLINK          "�g�s�b�N�ɖ߂�"
#define STR_MHELP_NAV_FORWLINK          "�g�s�b�N�ɐi��"
#define STR_MHELP_NAV_PREVPAGE          "�O�̃g�s�b�N <<"
#define STR_MHELP_NAV_NEXTPAGE          "���̃g�s�b�N >>"
#else
#define STR_MHELP_NAV_SEARCH            "����(~S)..."
#define STR_MHELP_NAV_GOTO              "�W�����v(~G)..."
#define STR_MHELP_NAV_MARK              "������(~M)"
#define STR_MHELP_NAV_BACKLINK          "�g�s�b�N�ɖ߂�(~B)"
#define STR_MHELP_NAV_FORWLINK          "�g�s�b�N�ɐi��(~F)"
#define STR_MHELP_NAV_PREVPAGE          "�O�̃g�s�b�N(~P)  <<"
#define STR_MHELP_NAV_NEXTPAGE          "���̃g�s�b�N(~N)  >>"
#endif

/*
* R4 Help System Help Menu Labels
*/
#if (XVTWS == MACWS)
#define STR_MHELP_HELP_ONHELP           "�w���v�̎g����"
#define STR_MHELP_HELP_ABOUT            "�w���v�ɂ���"
#else
#define STR_MHELP_HELP_ONHELP           "�w���v�̎g����(~H)"
#define STR_MHELP_HELP_ABOUT            "�w���v�ɂ���(~A)"
#endif

/*
* R4 Help System Edit Selection Menu Bar
*/
#if (XVTWS == MACWS)
#define STR_MHELP_TSE_EDIT              "�ҏW"
#define STR_MHELP_TSE_HELP              "�w���v"
#else
#define STR_MHELP_TSE_EDIT              "�ҏW(~E)"
#define STR_MHELP_TSE_HELP              "�w���v(~H)"
#endif

/*
* R4 Help System Help Menu Labels
*/
#if (XVTWS == MACWS)
#define STR_MHELP_TSE_HELP_ONHELP       "�w���v�̎g����"
#else
#define STR_MHELP_TSE_HELP_ONHELP       "�w���v�̎g����(~H)"
#endif

/*
* R4 Help System Help Topic Window
*/
#define STR_HELP_TOPIC_WIN              "�g�s�b�N��E�B���h�E"
#define STR_HELP_TOPIC_INFOLBL          "���ƃX�e�[�^�X"
#define STR_HELP_TOPIC_BOOKMARK         "�������t����"
#define STR_HELP_TOPIC_BACKLINK         "�߂�"
#if XVTWS == MTFWS
#define STR_HELP_TOPIC_SEARCH           "��������"
#define STR_HELP_TOPIC_GOTO             "�W�����v����"
#define STR_HELP_TOPIC_FORWLINK         "�i��"
#define STR_HELP_TOPIC_PREVPAGE         "--"
#define STR_HELP_TOPIC_NEXTPAGE         "++"
#else
#define STR_HELP_TOPIC_SEARCH           "����..."
#define STR_HELP_TOPIC_GOTO             "�W�����v..."
#define STR_HELP_TOPIC_FORWLINK         "�i��"
#define STR_HELP_TOPIC_PREVPAGE         "<<"
#define STR_HELP_TOPIC_NEXTPAGE         ">>"
#endif

/*
* R4 Help System Help Goto Dialog
*/
#define STR_HELP_GOTO_DLG               "�g�s�b�N�W�����v��_�C�A���O"
#define STR_HELP_GOTO_GROUP             "�W�����v��I�v�V����"
#define STR_HELP_GOTO_INDEX             "����"
#define STR_HELP_GOTO_GLOSSARY          "�p��W"
#define STR_HELP_GOTO_CONTENTS          "�ڎ�"
#define STR_HELP_GOTO_KEYBOARD          "�L�[�{�[�h"
#define STR_HELP_GOTO_BOOK_LBL          "������:"
#define STR_HELP_GOTO_CANCEL            "�L�����Z��"
#if XVTWS != MTFWS
#define STR_HELP_GOTO_BOOKMARK_BTN      "�}�[�N�փW�����v"
#else
#define STR_HELP_GOTO_BOOKMARK_BTN      "�W�����v"
#endif /* ! MTFWS */

/*
* R4 Help System Help Search Dialog
*/
#define STR_HELP_SEARCH_DLG             "�g�s�b�N�����_�C�A���O"
#define STR_HELP_SEARCH_RADIO_LBL       "�����I�v�V����"
#define STR_HELP_SEARCH_BY_TOPICNAME    "�g�s�b�N��"
#define STR_HELP_SEARCH_BY_KEYWORD      "�L�[���[�h��"
#if XVTWS == MACWS
#define STR_HELP_SEARCH_ITEMS_LBL       "��������:"
#define STR_HELP_SEARCH_MATCH_LBL       "�L�[���[�h�Ō���:"
#else
#define STR_HELP_SEARCH_ITEMS_LBL       "��������"
#define STR_HELP_SEARCH_MATCH_LBL       "�L�[���[�h�Ō���"
#endif /* MACWS */
#define STR_HELP_SEARCH_CANCEL          "�L�����Z��"
#if XVTWS != MTFWS
#define STR_HELP_SEARCH_GOTO_MATCH      "�W�����v�̑I��"
#else
#define STR_HELP_SEARCH_GOTO_MATCH      "�W�����v"
#endif /* ! MTFWS */

/*
* R4 Help System Help Copy Window
*/
#define STR_HELP_TOPIC_SELCOPY          "�R�s�[�Ɋւ���w���v"
#define STR_HELP_TOPIC_SELCOPY_WIN_LBL  "�e�L�X�g��I�����A�ҏW/�R�s�[��I��ł��������B"

/*
* R4 Help System About Window
*/
#define STR_HELP_ABOUT                  "helpview �ɂ���"
#define STR_HELP_ABOUT_CANCEL           "�L�����Z��"

/**********Macro Definitions for Control's Rectangles*********************/
#if XVTWS == MACWS
#define REC_HELP_TOPIC_WIN              HV_URL_RECT(240,  48, 376, 384)
#define REC_HELP_TOPIC_INFOLBL          HV_URL_RECT(  8, 320, 280,  20)
#define REC_HELP_TOPIC_SEARCH           HV_URL_RECT(  8, 352, 105,  20)
#define REC_HELP_TOPIC_GOTO             HV_URL_RECT( 80, 352, 105,  20) 
#define REC_HELP_TOPIC_BOOKMARK         HV_URL_RECT(152, 352, 105,  20)
#define REC_HELP_TOPIC_BACKLINK         HV_URL_RECT(224, 352, 105,  20)
#define REC_HELP_TOPIC_FORWLINK         HV_URL_RECT(296, 352, 105,  20)
#define REC_HELP_TOPIC_PREVPAGE         HV_URL_RECT(312, 320,  32,  20)
#define REC_HELP_TOPIC_NEXTPAGE         HV_URL_RECT(344, 320,  32,  20)
#else
#define REC_HELP_TOPIC_WIN              HV_URL_RECT( 60,  48, 570, 384)
#define REC_HELP_TOPIC_INFOLBL          HV_URL_RECT(  8, 320, 280,  24)
#define REC_HELP_TOPIC_SEARCH           HV_URL_RECT(  8, 352, 105,  24)
#define REC_HELP_TOPIC_GOTO             HV_URL_RECT( 80, 352, 105,  24) 
#define REC_HELP_TOPIC_BOOKMARK         HV_URL_RECT(152, 352, 105,  24)
#define REC_HELP_TOPIC_BACKLINK         HV_URL_RECT(224, 352, 105,  24)
#define REC_HELP_TOPIC_FORWLINK         HV_URL_RECT(296, 352, 105,  24)
#define REC_HELP_TOPIC_PREVPAGE         HV_URL_RECT(312, 320,  32,  24)
#define REC_HELP_TOPIC_NEXTPAGE         HV_URL_RECT(344, 320,  32,  24)
#endif

#if XVTWS == MACWS
#define REC_HELP_GOTO_DLG               HV_URL_RECT( 85,  44, 272, 352)
#define REC_HELP_GOTO_GROUP             HV_URL_RECT( 10,   7, 252, 158)
#define REC_HELP_GOTO_INDEX             HV_URL_RECT( 80,  32, 112,  20)
#define REC_HELP_GOTO_GLOSSARY          HV_URL_RECT( 80,  65, 112,  20)
#define REC_HELP_GOTO_CONTENTS          HV_URL_RECT( 80,  98, 112,  20)
#define REC_HELP_GOTO_KEYBOARD          HV_URL_RECT( 80, 131, 112,  20)
#define REC_HELP_GOTO_BOOK_LBL          HV_URL_RECT( 10, 175, 252,  16)
#define REC_GOTO_BOOKMARK_LIST          HV_URL_RECT( 10, 195, 252, 112)
#define REC_HELP_GOTO_BOOKMARK_BTN      HV_URL_RECT(154, 322, 108,  20)
#define REC_HELP_GOTO_CANCEL            HV_URL_RECT( 71, 322,  70,  20)
#define REC_HELP_SEARCH_DLG             HV_URL_RECT(240,  96, 292, 338)
#define REC_HELP_SEARCH_RADIO_LBL       HV_URL_RECT( 10,   7, 272,  64)
#define REC_HELP_SEARCH_BY_TOPICNAME    HV_URL_RECT( 15,  28, 262,  20)
#define REC_HELP_SEARCH_BY_KEYWOR       HV_URL_RECT( 15,  48, 262,  20)
#define REC_HELP_SEARCH_ITEMS_LBL       HV_URL_RECT( 10,  81, 272,  16)
#define REC_SEARCH_SELECT_LIST          HV_URL_RECT( 10, 101, 272,  96)
#define REC_HELP_SEARCH_MATCH_LBL       HV_URL_RECT( 10, 209, 272,  16)
#define REC_SEARCH_MATCH_LIST           HV_URL_RECT( 10, 229, 272,  72)
#define REC_HELP_SEARCH_GOTO_MATCH      HV_URL_RECT(186, 308,  96,  20)
#define REC_HELP_SEARCH_CANCEL          HV_URL_RECT(103, 308,  70,  20)
#else
#define REC_HELP_GOTO_DLG               HV_URL_RECT( 85,  44, 252, 376)
#define REC_HELP_GOTO_GROUP             HV_URL_RECT( 16,   8, 220, 168)
#define REC_HELP_GOTO_INDEX             HV_URL_RECT( 70,  40, 112,  24)
#define REC_HELP_GOTO_GLOSSARY          HV_URL_RECT( 70,  72, 112,  24)
#define REC_HELP_GOTO_CONTENTS          HV_URL_RECT( 70, 104, 112,  24)
#define REC_HELP_GOTO_KEYBOARD          HV_URL_RECT( 70, 136, 112,  24)
#define REC_HELP_GOTO_BOOK_LBL          HV_URL_RECT( 16, 184,  96,  16)
#define REC_GOTO_BOOKMARK_LIST          HV_URL_RECT( 16, 208, 220, 112)
#define REC_HELP_GOTO_BOOKMARK_BTN      HV_URL_RECT(  8, 336, 136,  24)
#define REC_HELP_GOTO_CANCEL            HV_URL_RECT(156, 336,  88,  24)
#define REC_HELP_SEARCH_DLG             HV_URL_RECT(240,  96, 288, 336)
#define REC_HELP_SEARCH_RADIO_LBL       HV_URL_RECT(  8,   8, 272,  56)
#define REC_HELP_SEARCH_BY_TOPICNAME    HV_URL_RECT( 24,  32, 112,  24)
#define REC_HELP_SEARCH_BY_KEYWOR       HV_URL_RECT(136,  32, 122,  24)
#define REC_HELP_SEARCH_ITEMS_LBL       HV_URL_RECT(  8,  72, 136,  16)
#define REC_SEARCH_SELECT_LIST          HV_URL_RECT(  8,  96, 272,  96)
#define REC_HELP_SEARCH_MATCH_LBL       HV_URL_RECT(  8, 200, 144,  16)
#define REC_SEARCH_MATCH_LIST           HV_URL_RECT(  8, 224, 272,  72)
#define REC_HELP_SEARCH_GOTO_MATCH      HV_URL_RECT(  8, 304, 124,  24)
#define REC_HELP_SEARCH_CANCEL          HV_URL_RECT(156, 304, 124,  24)
#endif

#if XVTWS == MACWS
#define REC_HELP_TOPIC_SELCOPY          HV_URL_RECT( 95, 249, 348, 176)
#define REC_HELP_TOPIC_SELCOPY_WIN_LBL  HV_URL_RECT(  0,   0, 320,  16)
#else
#define REC_HELP_TOPIC_SELCOPY          HV_URL_RECT( 95, 249, 320, 176)
#define REC_HELP_TOPIC_SELCOPY_WIN_LBL  HV_URL_RECT(  0,   0, 320,  24)
#endif


/****************************************************************************
    Common Code Section (strings found in V layer)
 
****************************************************************************/

#define STR_CC_NULL_ARG_STR             "�k������"
#define STR_CC_EMPTY_LIST_STR           "���X�g����ł��B"
#define STR_CC_SLIST_GET_FAIL_STR       "�G�������g%d��xvtv_slist_get�����s����܂���ł����B"
#define STR_CC_NAMED_COL_NOT_FOUND_STR  "�w��̃J���[ '%s' ��������܂���B\n"
#define STR_CC_FATAL_RECURSE_STR        "�v���I�G���[���ŏI�`�����X�G���[��n���h�����Ŕ������܂����B�A�v���P�[�V�������I�����܂��B"
#define STR_CC_INV_ERRMSG_STR           "������XVT_ERRMSG�I�u�W�F�N�g�ł��B"
#define STR_CC_UNKNOWN_STR              "���m�F"
#define STR_CC_DEF_TWICE_STR            "�x���B�X�g�����O%s��id %d���x��`���܂����B\n"
