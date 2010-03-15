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
#define STR_M_FILE                      "ファイル"
#define STR_M_FILE_NEW                  "新規"
#define STR_M_FILE_OPEN                 "開く..."
#define STR_M_FILE_CLOSE                "閉じる"
#define STR_M_FILE_SAVE                 "保存"
#define STR_M_FILE_SAVE_AS              "別名で保存..."
#define STR_M_FILE_REVERT               "回復"
#define STR_M_FILE_PG_SETUP             "用紙設定..."
#define STR_M_FILE_PRINT                "印刷..."
#define STR_M_FILE_QUIT                 "終了"
#define STR_M_FILE_NEW_ACCEL            "N" ALT
#define STR_M_FILE_OPEN_ACCEL           "O" ALT
#define STR_M_FILE_CLOSE_ACCEL          "W" ALT
#define STR_M_FILE_SAVE_ACCEL           "S" ALT
#define STR_M_FILE_PRINT_ACCEL          "P" ALT
#define STR_M_FILE_QUIT_ACCEL           "Q" ALT
#elif (XVTWS == MTFWS)
#define STR_M_FILE                      "ファイル(~F)"
#define STR_M_FILE_NEW                  "新規作成(~N)"
#define STR_M_FILE_OPEN                 "開く...(~O)"
#define STR_M_FILE_SAVE                 "上書き保存(~S)"
#define STR_M_FILE_SAVE_AS              "名前を付けて保存...(~A)"
#define STR_M_FILE_PG_SETUP             "プリンタの設定(~R)"
#define STR_M_FILE_PRINT                "印刷(~P)"
#define STR_M_FILE_QUIT                 "終了(~X)"
#elif (XVTWS == WIN16WS) || (XVTWS == WIN32WS)
#define STR_M_FILE                      "ファイル(~F)"
#define STR_M_FILE_NEW                  "新規作成(~N)"
#define STR_M_FILE_OPEN                 "開く...(~O)"
#define STR_M_FILE_CLOSE                "閉じる(~C)"
#define STR_M_FILE_SAVE                 "上書き保存(~S)"
#define STR_M_FILE_SAVE_AS              "名前を付けて保存...(~A)"
#define STR_M_FILE_REVERT               "保存の状態に戻す(~V)"
#define STR_M_FILE_PG_SETUP             "プリンタの設定...(~R)"
#define STR_M_FILE_PRINT                "印刷...(~P)"
#define STR_M_FILE_QUIT                 "終了(~X)"
#elif (XVTWS == PMWS)
#define STR_M_FILE                      "ファイル(~F)"
#define STR_M_FILE_NEW                  "新規(~N)"
#define STR_M_FILE_OPEN                 "オープン...(~O)"
#define STR_M_FILE_SAVE                 "保管(~S)"
#define STR_M_FILE_SAVE_AS              "別の名前で保管...(~A)"
#define STR_M_FILE_REVERT               "保存の状態に戻す(~V)"
#define STR_M_FILE_PG_SETUP             "プリンタ設定...(~R)"
#define STR_M_FILE_PRINT                "印刷...(~P)"
#define STR_M_FILE_QUIT                 "終了(~X)"
#define STR_M_FILE_ABOUT                "製品情報(~B)"
#endif

/*
* Standard Edit Menu Labels
*/
#if (XVTWS == MACWS)
#define STR_M_EDIT                      "編集"
#define STR_M_EDIT_UNDO                 "取り消し"
#define STR_M_EDIT_CUT                  "カット"
#define STR_M_EDIT_COPY                 "コピー"
#define STR_M_EDIT_PASTE                "ペースト"
#define STR_M_EDIT_CLEAR                "消去"
#define STR_M_EDIT_SEL_ALL              "すべてを選択"
#define STR_M_EDIT_CLIPBOARD            "クリップボード表示"
#define STR_M_EDIT_UNDO_ACCEL           "Z" ALT
#define STR_M_EDIT_CUT_ACCEL            "X" ALT
#define STR_M_EDIT_COPY_ACCEL           "C" ALT
#define STR_M_EDIT_PASTE_ACCEL          "V" ALT
#define STR_M_EDIT_SEL_ALL_ACCEL        "A" ALT
#elif (XVTWS == MTFWS)
#define STR_M_EDIT                      "編集(~E)"
#define STR_M_EDIT_UNDO                 "元に戻す(~U)"
#define STR_M_EDIT_CUT                  "切り取り(~T)"
#define STR_M_EDIT_COPY                 "コピー(~C)"
#define STR_M_EDIT_PASTE                "貼り付け(~P)"
#define STR_M_EDIT_CLEAR                "削除(~D)"
#define STR_M_EDIT_UNDO_ACCEL           BACK ALT
#define STR_M_EDIT_CUT_ACCEL            DEL SHIFT
#define STR_M_EDIT_COPY_ACCEL           INS CONTROL
#define STR_M_EDIT_PASTE_ACCEL          INS SHIFT
#elif (XVTWS == WIN16WS) || (XVTWS == WIN32WS)
#define STR_M_EDIT                      "編集(~E)"
#define STR_M_EDIT_UNDO                 "元に戻す(~U)\tCtrl+Z"
#define STR_M_EDIT_CUT                  "切り取り(~T)\tCtrl+X"
#define STR_M_EDIT_COPY                 "コピー(~C)\tCtrl+C"
#define STR_M_EDIT_PASTE                "貼り付け(~P)\tCtrl+V"
#define STR_M_EDIT_CLEAR                "削除(~D)"
#define STR_M_EDIT_UNDO_ACCEL           "Z" CONTROL
#define STR_M_EDIT_CUT_ACCEL            "X" CONTROL
#define STR_M_EDIT_COPY_ACCEL           "C" CONTROL
#define STR_M_EDIT_PASTE_ACCEL          "V" CONTROL
#elif (XVTWS == PMWS)
#define STR_M_EDIT                      "編集(~E)"
#define STR_M_EDIT_UNDO                 "やり直し(~U)\tAlt+BS"
#define STR_M_EDIT_COPY                 "複写(~C)\tCtrl+Ins"
#define STR_M_EDIT_PASTE                "貼付け(~P)\tShift+Ins"
#ifndef NO_DEL_ACCEL
#define STR_M_EDIT_CUT                  "切抜き(~T)\tShift+Del"
#define STR_M_EDIT_CLEAR                "クリア(~E)\tDel"
#else
#define STR_M_EDIT_CUT                  "切抜き(~T)"
#define STR_M_EDIT_CLEAR                "クリア(~E)"
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
#define STR_M_FONT                      "フォント"
#elif (XVTWS == MTFWS)
#define STR_M_FONT                      "フォント(~T)"
#define STR_M_FONT_COURIER              "Courier(~C)"
#define STR_M_FONT_HELVETICA            "Helvetica(~H)"
#define STR_M_FONT_SYSTEM               "システム(~S)"
#define STR_M_FONT_TIMES                "Times(~T)"
#define STR_M_FONT_OTHER                "その他(~O)"
#elif (XVTWS == WIN16WS) || (XVTWS == WIN32WS) || (XVTWS == PMWS)
#define STR_M_FONT                      "フォント(~T)"
#define STR_M_FONT_SELECT               "選択...(~S)"
#endif /* (XVTWS == MACWS) */

/*
* Standard Style Menu Labels
*/
#if (XVTWS == MACWS)
#define STR_M_STYLE                     "スタイル"
    /* I18N - This string contains the menu items on the first half of the */
    /*        Style Menu. */
    /*        The /x characters (ie "/T") are Mac Menu Manager specific codes */
    /*           to determine the menu accelerator */
    /*        The <y characters (ie "<B") are Mac Menu Manager specific */
    /*           codes to set the font style for the menu item. */
    /*        The "-(" item is a Mac Menu Manager specific code to specify a */
    /*           menu separator line */
    /*        All items are separated by semi-colons ";" */
#define STR_MAC_STYLEM1                 "標準/T;-(;ボールド<B/B;イタリック<I/I;アンダーライン<U/U;アウトライン<O;シャドウ<S;-(;コンデンス;エクステンド"

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
#define STR_MAC_STYLEM3                 "-(;9;10;12;14;18;24;36;48;72;その他...%s%d%s"
#elif (XVTWS == MTFWS)
#define STR_M_STYLE                     "スタイル(~S)"
#define STR_M_STYLE_NORMAL              "標準(~N)"
#define STR_M_STYLE_BOLD                "太字(~B)"
#define STR_M_STYLE_ITALIC              "斜体(~I)"
#define STR_M_STYLE_8                   " 8ポイント"
#define STR_M_STYLE_10                  "10ポイント"
#define STR_M_STYLE_12                  "12ポイント"
#define STR_M_STYLE_14                  "14ポイント"
#define STR_M_STYLE_18                  "18ポイント"
#define STR_M_STYLE_24                  "24ポイント"
#endif

/*
* Standard Help Menu Labels
*/
#if (XVTWS == MACWS)
#define STR_M_HELP                      "ヘルプ"
#define STR_M_HELP_ONWINDOW             "このウィンドウ"
#define STR_M_HELP_SEARCH               "検索..."
#define STR_M_HELP_CONTENTS             "目次"
#define STR_M_HELP_INDEX                "索引"
#define STR_M_HELP_OBJCLICK             "オブジェクト･クリック"
#define STR_M_HELP_HELPONHELP           "ヘルプに関するヘルプ"
#define STR_M_HELP_VERSION              "...について"
#elif (XVTWS == MTFWS)
#define STR_M_HELP                      "ヘルプ(~H)"
#define STR_M_HELP_ONWINDOW             "ウィンドウについて(~W)"
#define STR_M_HELP_CONTENTS             "目次(~N)"
#define STR_M_HELP_INDEX                "索引(~I)"
#define STR_M_HELP_OBJCLICK             "コンテキストについて(~C)"
#define STR_M_HELP_HELPONHELP           "ヘルプについて(~H)"
#define STR_M_HELP_VERSION              "バージョン情報(~V)"
#define STR_M_HELP_KEYBOARD             "キーについて(~K)"
#elif (XVTWS == WIN16WS) || (XVTWS == WIN32WS)
#define STR_M_HELP                      "ヘルプ(~H)"
#define STR_M_HELP_CONTENTS             "目次(~C)"
#define STR_M_HELP_SEARCH               "キーワードで検索(~S)"
#define STR_M_HELP_HELPONHELP           "ヘルプの使い方(~H)"
#define STR_M_HELP_OBJCLICK             "オブジェクト･クリック(~O)\tShift+F1"
#define STR_M_HELP_VERSION              "バージョン情報(~A)..."
#define STR_M_HELP_CONTENTS_ACCEL       F1 ALT
#define STR_M_HELP_SEARCH_ACCEL         F1 CONTROL
#define STR_M_HELP_OBJCLICK_ACCEL       F1 SHIFT
#elif (XVTWS == PMWS)
#define STR_M_HELP                      "ヘルプ(~H)"
#define STR_M_HELP_INDEX                "ヘルプ索引(~I)"
#define STR_M_HELP_ONWINDOW             "一般ヘルプ(~G)"
#define STR_M_HELP_HELPONHELP           "ヘルプ使用法(~U)"
#define STR_M_HELP_KEYBOARD             "キーヘルプ(~K)"
#define STR_M_HELP_OBJCLICK             "オブジェクト･クリック(~O)\tAlt+F1"
#define STR_M_HELP_VERSION              "バージョン情報(~P)"
#define STR_M_HELP_ONCONTEXT_ACCEL      F1
#define STR_M_HELP_OBJCLICK_ACCEL       F1 ALT
#endif /* (XVTWS == MACWS) */

/*
* Windows MDI Menus
*/
#if (XVTWS == WIN16WS) || (XVTWS == WIN32WS)
#define STR_XVT_WINDOWMENU              "ウィンドウ表示(&W)"
#define STR_IDM_WINDOWTILE              "並べて表示(~T)"
#define STR_IDM_WINDOWCASCADE           "重ねて表示(~C)"
#define STR_IDM_WINDOWICONS             "アイコンの整列(~A)"
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
#define STR_DB_ABORT_TEXT               "印刷を中止するには\n\021を押しながらピリオド(.)をタイプして下さい。"
#elif (XVTWS == WIN16WS) || (XVTWS == WIN32WS)
#define STR_DB_ABORT_CANCEL             "キャンセル"
#define STR_DB_ABORT_PRINTING           "印刷中"
#define STR_DB_ABORT_TEXT1              ""
#define STR_DB_ABORT_TEXT2              ""
#elif (XVTWS == PMWS)
#define STR_DB_ABORT_CANCEL             "取消"
#define STR_DB_ABORT_PRINTING           "印刷中"
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
#define STR_DB_ASK_HELP                 "ヘルプ"
#endif

/*
* Standard Response Dialog
*/
#if (XVTWS == MACWS)
#define STR_DB_RESPONSE                 "文字列応答ダイアログ"
#elif (XVTWS == MTFWS)
#define STR_DB_RESPONSE                 ""
#elif (XVTWS == WIN16WS) || (XVTWS == WIN32WS)
#define STR_DB_RESPONSE_EDIT            ""
#define STR_DB_RESPONSE_TEXT            ""
#elif (XVTWS == PMWS)
#define STR_DB_RESPONSE_HELP            "ヘルプ"
#define STR_DB_RESPONSE_TEXT            ""
#endif /* (XVTWS == MACWS) */
#define STR_DB_RESPONSE_OK              "OK"
#define STR_DB_RESPONSE_CANCEL          "キャンセル"

/*
* Standard Font Selection Dialog
*/
#if (XVTWS == MACWS)
#define STR_DB_FONTSEL                  "フォント選択ダイアログ"
#define STR_DB_FONTSEL_OK               "OK"
#define STR_DB_FONTSEL_CANCEL           "キャンセル"
#define STR_DB_FONTSEL_FONT             "フォント:"
#define STR_DB_FONTSEL_STYLE            "スタイル"
#define STR_DB_FONTSEL_BOLD             "ボールド"
#define STR_DB_FONTSEL_ITALIC           "イタリック"
#define STR_DB_FONTSEL_UNDERLINE        "アンダーライン"
#define STR_DB_FONTSEL_OUTLINE          "アウトライン"
#define STR_DB_FONTSEL_SHADOW           "シャドウ"
#define STR_DB_FONTSEL_SPACING          "文字間"
#define STR_DB_FONTSEL_NORMAL           "標準"
#define STR_DB_FONTSEL_CONDENSED        "コンデンス"
#define STR_DB_FONTSEL_EXTENDED         "エクステンド"
#define STR_DB_FONTSEL_SIZE             "サイズ:"
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
#define STR_DB_FONTSIZE                 "その他"
#define STR_DB_FONTSIZE_OK              "OK"
#define STR_DB_FONTSIZE_CANCEL          "キャンセル"
#define STR_DB_FONTSIZE_FONTSIZE        "サイズ:" /* "フォントサイズ:" */
#define STR_DB_FONTSIZE_POINTS          "ポイント"
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
#define STR_DB_ABOUT_OK                 "ヘルプ"
#define STR_DB_ABOUT_CANCEL             "キャンセル"
#if (XVTWS == WIN16WS) || (XVTWS == WIN32WS) || (XVTWS == PMWS)
#define STR_DB_ABOUT_TEXT1              "XVTアプリケーション"
#define STR_DB_ABOUT_TEXT2              "バージョン4.57"
#define STR_DB_ABOUT_TEXT3              "XVT Software."
#elif (XVTWS == MACWS)
#define STR_DB_ABOUT_TEXT1              "XVTアプリケーションバージョン4.57"
#define STR_DB_ABOUT_TEXT2              "XVT Software."
#else
#define STR_DB_ABOUT_TEXT1              "このプログラムは、XVT（Extensible Virtual Toolkit）を"
#define STR_DB_ABOUT_TEXT2              "使用して開発しました。"
#endif

/*
* Standard Open File Dialog
*/
#if (XVTWS == MACWS)
#define STR_DB_OPENFILE_SELECT          "選択"
#elif (XVTWS == MTFWS)
#define STR_DB_OPENFILE                 ""
#define STR_DB_OPENFILE_OPEN            "開く"

#endif /* (XVTWS == MACWS) */

/*
* Standard Page Setup Dialog
*/
#if (XVTWS == MTFWS)
#define STR_D_PAGE_SETUP_DIALOG         "プリンタページ　レイアウトの設定"
#define STR_D_PAGE_SIZE                 "用紙サイズ"
#define STR_D_PAGE_USLETTER             "US Letter"
#define STR_D_PAGE_USLEGAL              "US Legal"
#define STR_D_PAGE_A4LETTER             "A4 Letter"
#define STR_D_PAGE_B5LETTER             "B5 Letter"
#define STR_D_PAGE_ORIENTATION          "印刷の向き"
#define STR_D_PAGE_OLANDSCAPE           "横"
#define STR_D_PAGE_OPORTRAIT            "縦"
#define STR_D_PAGE_OPT_ENLARGMENT       "倍率(%)"
#define STR_D_PAGE_INP_ENLARGMENT       "100"
#define STR_D_PAGE_IMAGE                "キャンバス"
#define STR_D_PAGE_IMAGE_MONO           "モノクロ"
#define STR_D_PAGE_IMAGE_GRAY           "グレイスケール"
#define STR_D_PAGE_IMAGE_COLOR          "カラー表示"
#define STR_D_PAGE_OK                   "OK"
#define STR_D_PAGE_CANCEL               "キャンセル"
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
#define STR_STR_DB_PRSETUP              "プリンター設定"
#define STR_STR_DB_PRSETUP_OK           "了解"
#define STR_STR_DB_PRSETUP_CANCEL       "取消"
#define STR_STR_DB_PRSETUP_SETUP        "~設定"
#define STR_STR_DB_PRSETUP_TEXT         "組み込まれているプリンター"
#endif /* (XVTWS == MTFWS) */

/****************************************************************************
    Platform Specific String Section
 
****************************************************************************/

#if (XVTWS == MACWS)
#define STR_MAC_SIGNATURE               "R4 XVTアプリケーション"
#define STR_MAC_HELP                    "ヘルプ"
    /* I18N - This string contains the about item on the Apple menu. */
    /*        The %s represents the application name from appl_name in */
    /*        the XVT_CONFIG structure */
#define STR_MAC_ABOUT                   "%s について..."
#define STR_MAC_CLICK                   "続けるにはマウスをクリックしてください。"
    /* I18N - This string is used in the open file dialog to select directories. */
    /*        The %s represents the current directory name selected. */
#define STR_MAC_SELECT                  "選択 \322%s\323"
    /* I18N - This string is used in the notification manager routines. */
    /*        The %s represents the current application name. */
#define STR_MAC_NOTIFICATION			"アプリケーション \322%1$s\323 を確認して下さい。\n\n"
#define STR_MAC_LOW_MEM_WARNING			"メモリーが不足しています。アプリケーションの「情報を見る」でメモリーを確保して下さい。"
#elif (XVTWS == MTFWS)
#define STR_WM_HELPTYPE                 "hlp"
#define STR_WM_PRINT_001                "用紙設定情報アクセス中にエラー"
#define STR_WM_PRINT_002                "xvtprolg.ps"
    /* I18N - This string contains a printing error message. */
    /*        The %s represents the file name of a font file */
#define STR_WM_PRINT_003                "AFMファイル%sにアクセスできません。"
#elif (XVTWS == WIN16WS) || (XVTWS == WIN32WS)
#define STR_WIN_HELPTYPE                "HLP"
/* strings used in bothstr.h and as default string in SGETSTR */
#define STR_WIN_OK                      "OK"
#define STR_WIN_CANCEL                  "キャンセル"
#define STR_WIN_CANT_SHOW_ABOUT_BOX     "バージョン情報を表示できません。"
#define STR_WIN_CANCELLING              "キャンセル中..."
#define STR_WIN_INITIALIZING            "初期化中"
#define STR_WIN_UNTITLED                "(名称未設定)"
#define STR_WIN_STARTING_DOC            "(文書開始)"
#define STR_WIN_STARTED_DOC             "(文書開始)"
#define STR_WIN_ENDING_DOC              "(文書終了)"
    /* I18N - This string contains a message from the print dialog. */
    /*        The %d represents the page number of the current printing page */
#define STR_WIN_STARTING_PAGE           "(%dページを開始)"
    /* I18N - This string contains a message from the print dialog. */
    /*        The %d represents the page number of the finished printing page */
#define STR_WIN_FINISHING_PAGE          "(%dページを終了中)"
#define STR_WIN_SYSTEM_ERROR            "システムエラー"
#define STR_WIN_PICTURE_STILL_LOCKED    "絵は、まだロックされたままです。"
#define STR_WIN_CANNOT_PRINT            "印刷できません"
#define STR_WIN_ALERT                   "メッセージを表示"
#define STR_WIN_FDLG_FILTER             "ファイル(*.%s)|*.%s|"
#define STR_WIN_FDLG_FILTER_ALL         "すべてのファイル(*.*)|*.*|"
#define STR_WIN_NO_EXT_RES              "リソースファイルが見つかりません。標準リソースを使用します。"
#define STR_WIN_DEFAULTFAMILY           "ＭＳ ゴシック"
#define STR_WIN_SYSTEMFAMILY            "System"
#define STR_WIN_HELVETICAFAMILY         "ＭＳ ゴシック"
#define STR_WIN_FIXEDFAMILY             "ＭＳ 明朝"
#define STR_WIN_TIMESFAMILY             "ＭＳ 明朝"
#define STR_WIN_COURIERFAMILY           "ＭＳ 明朝"
#elif (XVTWS == PMWS)
#define STR_PM_HELPTYPE                 "hlp"
#define STR_PM_OK                       "了解"
#define STR_PM_CANCEL                   "取消"
#define STR_PM_CANT_SHOW_ABOUT_BOX      "バージョン情報ボックスを表示できません。"
#define STR_PM_CANCELLING               "取消中..."
#define STR_PM_INITIALIZING             "初期化中"
#define STR_PM_UNTITLED                 "(名称未設定)"
#define STR_PM_STARTING_DOC             "(文書を開始します)"
#define STR_PM_STARTED_DOC              "(文書を開始しました)"
#define STR_PM_ENDING_DOC               "(文書を終了します)"
    /* I18N - This string contains a message from the print dialog. */
    /*        The %d represents the page number of the current printing page */
#define STR_PM_STARTING_PAGE            "(%dページを印刷中です)"
    /* I18N - This string contains a message from the print dialog. */
    /*        The %d represents the page number of the finished printing page */
#define STR_PM_FINISHING_PAGE           "(%dページの印刷を終了します)"
#define STR_PM_SYSTEM_ERROR             "システムエラー"
#define STR_PM_PICTURE_STILL_LOCKED     "ピクチャーがロックされたままです。"
#define STR_PM_CANNOT_PRINT             "印刷できません。"
#define STR_PM_OPENING_PRINTER          "(プリンターをオープンします)"
#define STR_PM_NO_ERROR_INFO_AVAILABLE  "エラー情報はありません。"
#define STR_PM_PRINTING_ERROR           "印刷エラー"
#define STR_PM_CANT_CREATE_PRINT_THREAD "プリント･スレッドを作成できませんでした。"
    /* I18N - This string contains PM system errors. */
    /*        The %x represents a hex system error id */
#define STR_PM_ERROR                    "PMエラー0x%x"
#define STR_PM_ESCAPE_FAILED            "プリンターエスケープが失敗しました。"
#define STR_PM_ALT                      "Alt"
#define STR_PM_CTRL                     "Ctrl"
#define STR_PM_CTRLF6                   "~次のウィンドウ\tCtrl+F6"
#define STR_PM_OPEN                     "オープン"
#define STR_PM_SAVEAS                   "名前を付けて保管"
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
#define STR_HELP_CLIP_ERR               "書き込み用にクリップボードを開けません。"
#define STR_HELP_MEM_ERR                "メモリー不十分"
#define STR_HELP_CLIP_PUT_ERR           "クリップボードに情報を貼ることができません。"
#define STR_HELP_PRINT_ERR              "印刷できませんでした。"
#define STR_HELP_PRINT_OK               "トピックが印刷されました。"
#define STR_HELP_CLIP_OK                "トピックがクリップボードにコピーされました。"
    /* I18N - This string is used to signify the topic sequence number in */
    /*        the help viewer */
    /*        The first %d represents which topic in a sequence of topics */
    /*        that is currently being viewed */
    /*        The second %d represents the total number of topics that have */
    /*        been viewed */
#define STR_HELP_THREAD_INFO            "トピック: %d の %d"
    /* I18N - This string is used to show if a topic has been marked */
    /*        (bookmarks) */
    /*        The %c represents a the single byte character 'x' */
    /*        or ' ' (space). */
    /*        This means the item is either marked (x) or unmarked ( ). */
#define STR_HELP_MARKED_INFO            " [%c]マークを利用"
#define STR_HELP_COPYPART               "選択されませんでした。コピーするものがありません。"

/*
* R4 Help System Topic Button Labels
*/
#define STR_HELP_MARK                   "マーク"
#define STR_HELP_UNMARK                 "マークを取り除く"

/*
* R4 Help System Topic Navigation Menu
*/
#if (XVTWS == MACWS)
#define STR_HELP_NAV_SEARCH             SEARCH_STR
#define STR_HELP_NAV_GOTO               "ジャンプ..."
#define STR_HELP_NAV_MARK               "しおりを指定"
#define STR_HELP_NAV_BACKLINK           "トピックに 戻る"
#define STR_HELP_NAV_FORWLINK           "トピックに進む"
#define STR_HELP_NAV_PREVPAGE           "前のトピック <<"
#define STR_HELP_NAV_NEXTPAGE           "次のトピック >>"
#else
#define STR_HELP_NAV_SEARCH             SEARCH_STR
#define STR_HELP_NAV_GOTO               "ジャンプ(~G)..."
#define STR_HELP_NAV_MARK               "しおりを指定(~M)"
#define STR_HELP_NAV_BACKLINK           "トピックに 戻る(~B)"
#define STR_HELP_NAV_FORWLINK           "トピックに進む(~F)"
#define STR_HELP_NAV_PREVPAGE           "前のトピック(~P)<<"
#define STR_HELP_NAV_NEXTPAGE           "次のトピック(~N)>>"
#endif /* MACWS */

/*
* R4 Help System Menu Bar
*/
#if (XVTWS == MACWS)
#define STR_MHELP_FILE                  "ファイル"
#define STR_MHELP_EDIT                  "編集"
#define STR_MHELP_NAV                   "ナビゲートする"
#define STR_MHELP_HELP                  "ヘルプ"
#else
#define STR_MHELP_FILE                  "ファイル(~F)"
#define STR_MHELP_EDIT                  "編集(~E)"
#define STR_MHELP_NAV                   "ナビゲートする(~N)"
#define STR_MHELP_HELP                  "ヘルプ(~H)"
#endif /* MACWS */

/*
* R4 Help System File Menu Labels
*/
#if (XVTWS == MACWS)
#define STR_MHELP_FILE_PRINT            "印刷..."
#define STR_MHELP_FILE_PRINT_SETUP      "用紙設定..."
#define STR_MHELP_FILE_EXIT             "終了"
#define STR_MHELP_FILE_EXIT_ACCEL       "Q" ALT
#define STR_MHELP_FILE_PRINT_ACCEL      "P" ALT
#else  /* !MACWS */
#define STR_MHELP_FILE_PRINT            "印刷(~P)"
#define STR_MHELP_FILE_PRINT_SETUP      "プリンタの設定(~R)..."
#define STR_MHELP_FILE_EXIT             "終了(~X)"
#endif /* !MACWS */

/*
* R4 Help System Edit Menu Labels
*/
#if (XVTWS == WIN16WS) || (XVTWS == WIN32WS) || (XVTWS == PMWS)
#define STR_MHELP_EDIT_COPY             "コピー(~C)\tCtrl+C"
#define STR_MHELP_TSE_EDIT_COPY         "コピー(~C)\tCtrl+C"
#define STR_MHELP_EDIT_COPY_ACCEL       "C" CONTROL
#elif (XVTWS == MACWS)
#define STR_MHELP_EDIT_COPY             "コピー"
#define STR_MHELP_TSE_EDIT_COPY         "コピー"
#else  /* ! ((XVTWS == WIN16WS) || (XVTWS == WIN32WS) || (XVTWS == PMWS)) */
#define STR_MHELP_EDIT_COPY             "コピー(~C)"
#define STR_MHELP_TSE_EDIT_COPY         "コピー(~C)"
#endif /* (XVTWS == WIN16WS) || (XVTWS == WIN32WS) || (XVTWS == PMWS) */

#if (XVTWS == MACWS)
#define STR_MHELP_EDIT_COPYPART         "トピックの一部をコピーする..."
#define STR_MHELP_EDIT_AS_WRAPPED       "ラップどおりにコピーする"
#elif (XVTWS == MTFWS)
#define STR_MHELP_EDIT_AS_WRAPPED       "ラップどおりにコピーする(~W)"
#else
#define STR_MHELP_EDIT_COPYPART         "トピックの一部をコピーする(~P)..."
#define STR_MHELP_EDIT_AS_WRAPPED       "ラップどおりにコピーする(~W)"
#endif

/*
* R4 Help System Navigation Menu Labels
*/
#if (XVTWS == MACWS)
#define STR_MHELP_NAV_SEARCH            "検索..."
#define STR_MHELP_NAV_GOTO              "ジャンプ..."
#define STR_MHELP_NAV_MARK              "しおり"
#define STR_MHELP_NAV_BACKLINK          "トピックに戻る"
#define STR_MHELP_NAV_FORWLINK          "トピックに進む"
#define STR_MHELP_NAV_PREVPAGE          "前のトピック <<"
#define STR_MHELP_NAV_NEXTPAGE          "次のトピック >>"
#else
#define STR_MHELP_NAV_SEARCH            "検索(~S)..."
#define STR_MHELP_NAV_GOTO              "ジャンプ(~G)..."
#define STR_MHELP_NAV_MARK              "しおり(~M)"
#define STR_MHELP_NAV_BACKLINK          "トピックに戻る(~B)"
#define STR_MHELP_NAV_FORWLINK          "トピックに進む(~F)"
#define STR_MHELP_NAV_PREVPAGE          "前のトピック(~P)  <<"
#define STR_MHELP_NAV_NEXTPAGE          "次のトピック(~N)  >>"
#endif

/*
* R4 Help System Help Menu Labels
*/
#if (XVTWS == MACWS)
#define STR_MHELP_HELP_ONHELP           "ヘルプの使い方"
#define STR_MHELP_HELP_ABOUT            "ヘルプについて"
#else
#define STR_MHELP_HELP_ONHELP           "ヘルプの使い方(~H)"
#define STR_MHELP_HELP_ABOUT            "ヘルプについて(~A)"
#endif

/*
* R4 Help System Edit Selection Menu Bar
*/
#if (XVTWS == MACWS)
#define STR_MHELP_TSE_EDIT              "編集"
#define STR_MHELP_TSE_HELP              "ヘルプ"
#else
#define STR_MHELP_TSE_EDIT              "編集(~E)"
#define STR_MHELP_TSE_HELP              "ヘルプ(~H)"
#endif

/*
* R4 Help System Help Menu Labels
*/
#if (XVTWS == MACWS)
#define STR_MHELP_TSE_HELP_ONHELP       "ヘルプの使い方"
#else
#define STR_MHELP_TSE_HELP_ONHELP       "ヘルプの使い方(~H)"
#endif

/*
* R4 Help System Help Topic Window
*/
#define STR_HELP_TOPIC_WIN              "トピック･ウィンドウ"
#define STR_HELP_TOPIC_INFOLBL          "情報とステータス"
#define STR_HELP_TOPIC_BOOKMARK         "しおりを付ける"
#define STR_HELP_TOPIC_BACKLINK         "戻る"
#if XVTWS == MTFWS
#define STR_HELP_TOPIC_SEARCH           "検索する"
#define STR_HELP_TOPIC_GOTO             "ジャンプする"
#define STR_HELP_TOPIC_FORWLINK         "進む"
#define STR_HELP_TOPIC_PREVPAGE         "--"
#define STR_HELP_TOPIC_NEXTPAGE         "++"
#else
#define STR_HELP_TOPIC_SEARCH           "検索..."
#define STR_HELP_TOPIC_GOTO             "ジャンプ..."
#define STR_HELP_TOPIC_FORWLINK         "進む"
#define STR_HELP_TOPIC_PREVPAGE         "<<"
#define STR_HELP_TOPIC_NEXTPAGE         ">>"
#endif

/*
* R4 Help System Help Goto Dialog
*/
#define STR_HELP_GOTO_DLG               "トピックジャンプ･ダイアログ"
#define STR_HELP_GOTO_GROUP             "ジャンプ･オプション"
#define STR_HELP_GOTO_INDEX             "索引"
#define STR_HELP_GOTO_GLOSSARY          "用語集"
#define STR_HELP_GOTO_CONTENTS          "目次"
#define STR_HELP_GOTO_KEYBOARD          "キーボード"
#define STR_HELP_GOTO_BOOK_LBL          "しおり:"
#define STR_HELP_GOTO_CANCEL            "キャンセル"
#if XVTWS != MTFWS
#define STR_HELP_GOTO_BOOKMARK_BTN      "マークへジャンプ"
#else
#define STR_HELP_GOTO_BOOKMARK_BTN      "ジャンプ"
#endif /* ! MTFWS */

/*
* R4 Help System Help Search Dialog
*/
#define STR_HELP_SEARCH_DLG             "トピック検索ダイアログ"
#define STR_HELP_SEARCH_RADIO_LBL       "検索オプション"
#define STR_HELP_SEARCH_BY_TOPICNAME    "トピック別"
#define STR_HELP_SEARCH_BY_KEYWORD      "キーワード別"
#if XVTWS == MACWS
#define STR_HELP_SEARCH_ITEMS_LBL       "検索項目:"
#define STR_HELP_SEARCH_MATCH_LBL       "キーワードで検索:"
#else
#define STR_HELP_SEARCH_ITEMS_LBL       "検索項目"
#define STR_HELP_SEARCH_MATCH_LBL       "キーワードで検索"
#endif /* MACWS */
#define STR_HELP_SEARCH_CANCEL          "キャンセル"
#if XVTWS != MTFWS
#define STR_HELP_SEARCH_GOTO_MATCH      "ジャンプの選択"
#else
#define STR_HELP_SEARCH_GOTO_MATCH      "ジャンプ"
#endif /* ! MTFWS */

/*
* R4 Help System Help Copy Window
*/
#define STR_HELP_TOPIC_SELCOPY          "コピーに関するヘルプ"
#define STR_HELP_TOPIC_SELCOPY_WIN_LBL  "テキストを選択し、編集/コピーを選んでください。"

/*
* R4 Help System About Window
*/
#define STR_HELP_ABOUT                  "helpview について"
#define STR_HELP_ABOUT_CANCEL           "キャンセル"

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

#define STR_CC_NULL_ARG_STR             "ヌル引数"
#define STR_CC_EMPTY_LIST_STR           "リストが空です。"
#define STR_CC_SLIST_GET_FAIL_STR       "エレメント%dでxvtv_slist_getが実行されませんでした。"
#define STR_CC_NAMED_COL_NOT_FOUND_STR  "指定のカラー '%s' が見つかりません。\n"
#define STR_CC_FATAL_RECURSE_STR        "致命的エラーが最終チャンスエラー･ハンドラ内で発生しました。アプリケーションを終了します。"
#define STR_CC_INV_ERRMSG_STR           "無効なXVT_ERRMSGオブジェクトです。"
#define STR_CC_UNKNOWN_STR              "未確認"
#define STR_CC_DEF_TWICE_STR            "警告。ストリング%sとid %dを二度定義しました。\n"
