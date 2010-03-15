extern void xdCheckRadioButton XVT_CC_ARGS( ( WINDOW xdWindow, int check,	int start, int end ) );
void setTitle( char*, char*, char* );
void setProjectType( const BOOL bTypeLWA );

/************** GHJ ******************/
extern double res_factor;
extern void	ChangeFontAndSize(HWND hWnd);
/*************************************/

/*  object types for help association code  */
typedef enum
{
	xd_ILLEGAL,		/*  illegal object type  */
	xd_CNTR,		/*  container (window or dialog  */
	xd_CTRL,		/*  standard control  */
	xd_MENU			/*  menu item  */
} xd_OBJ_TYPE;

extern XVT_HELP_INFO xdGetHelpInfo XVT_CC_ARGS(( void ));
extern void xdSetHelpAssoc XVT_CC_ARGS((
		WINDOW  container,
		long  obj_id,
		long  topic_id,
		xd_OBJ_TYPE  obj_type ));
extern void xdRemoveHelpAssoc XVT_CC_ARGS(( WINDOW  xdWindow ));

extern void do_TASK_MENUBAR XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));

extern void do_MENU_BAR_2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));

extern void do_MENU_ERG XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));


#define dlg_107_eh DLG_107_eh
extern long XVT_CALLCONV1
	DLG_107_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));

#define dlg_110_eh DLG_110_eh
extern long XVT_CALLCONV1
	DLG_110_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));
#define dlg_122_eh DLG_122_eh
extern long XVT_CALLCONV1
	DLG_122_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));
#define dlg_124_eh DLG_124_eh
extern long XVT_CALLCONV1
	DLG_124_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));
#define dlg_128_eh DLG_128_eh
extern long XVT_CALLCONV1
	DLG_128_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));

#define dlg_135_eh DLG_135_eh
extern long XVT_CALLCONV1
	DLG_135_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));
#define dlg_136_eh DLG_136_eh
extern long XVT_CALLCONV1
	DLG_136_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));
#define dlg_137_eh DLG_137_eh
extern long XVT_CALLCONV1
	DLG_137_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));
#define dlg_138_eh DLG_138_eh
extern long XVT_CALLCONV1
	DLG_138_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));

#define dlg_150_eh DLG_OPEN_FILE_eh
extern long XVT_CALLCONV1
	DLG_OPEN_FILE_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));

#define dlg_151_eh DLG_151_eh
extern long XVT_CALLCONV1
	DLG_151_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));

#define dlg_DLG_FLAECHE_eh DLG_DLG_FLAECHE_eh
extern long XVT_CALLCONV1
	DLG_FLAECHE_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));

#define dlg_154_eh DLG_154_eh
extern long XVT_CALLCONV1
	DLG_154_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));

#define dlg_156_eh DLG_156_eh
extern long XVT_CALLCONV1
	DLG_156_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));
#define dlg_157_eh DLG_157_eh
extern long XVT_CALLCONV1
	DLG_157_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));
#define dlg_158_eh DLG_158_eh
extern long XVT_CALLCONV1
	DLG_158_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));
#define dlg_159_eh DLG_159_eh
extern long XVT_CALLCONV1
	DLG_159_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));
#define dlg_160_eh DLG_160_eh
extern long XVT_CALLCONV1
	DLG_160_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));
#define dlg_161_eh DLG_161_eh
extern long XVT_CALLCONV1
	DLG_161_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));

#define dlg_162_eh DLG_CHANGE_RAUH_eh
extern long XVT_CALLCONV1
	DLG_CHANGE_RAUH_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));
#define dlg_163_eh DLG_163_eh
extern long XVT_CALLCONV1
	DLG_163_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));

#define DLG_CHANGE_STATION_eh DLG_CHANGE_STATION_eh // Dialog 166
extern long XVT_CALLCONV1
	DLG_CHANGE_STATION_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));

#define dlg_167_eh DLG_167_eh
extern long XVT_CALLCONV1
	DLG_167_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));

#define dlg_168_eh DLG_168_eh
extern long XVT_CALLCONV1
	DLG_168_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));

#define dlg_169_eh DLG_169_eh
extern long XVT_CALLCONV1
	DLG_169_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));


#define dlg_140_eh DLG_140_eh
extern long XVT_CALLCONV1
	DLG_140_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));
#define dlg_141_eh DLG_141_eh
extern long XVT_CALLCONV1
	DLG_141_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));
#define dlg_142_eh DLG_142_eh
extern long XVT_CALLCONV1
	DLG_142_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));
#define dlg_143_eh DLG_143_eh
extern long XVT_CALLCONV1
	DLG_143_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));
#define dlg_144_eh DLG_144_eh
extern long XVT_CALLCONV1
	DLG_144_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));
#define dlg_145_eh DLG_145_eh
extern long XVT_CALLCONV1
	DLG_145_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));

#define dlg_147_eh DLG_147_eh
extern long XVT_CALLCONV1
	DLG_147_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));
#define dlg_148_eh DLG_148_eh
extern long XVT_CALLCONV1
	DLG_148_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));

#define dlg_203_eh DLG_203_eh
extern long XVT_CALLCONV1
	DLG_203_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));
#define dlg_204_eh DLG_204_eh
extern long XVT_CALLCONV1
	DLG_204_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));

#define dlg_208_eh DLG_208_eh
extern long XVT_CALLCONV1
	DLG_208_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));
#define dlg_209_eh DLG_209_eh
extern long XVT_CALLCONV1
	DLG_209_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));
#define dlg_210_eh DLG_210_eh
extern long XVT_CALLCONV1
	DLG_210_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));
#define dlg_211_eh DLG_211_eh
extern long XVT_CALLCONV1
	DLG_211_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));
#define dlg_212_eh DLG_212_eh
extern long XVT_CALLCONV1
	DLG_212_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));
#define dlg_213_eh DLG_213_eh
extern long XVT_CALLCONV1
	DLG_213_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));

#define dlg_217_eh DLG_217_eh
extern long XVT_CALLCONV1
	DLG_217_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));
#define dlg_218_eh DLG_218_eh
extern long XVT_CALLCONV1
	DLG_218_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));

#define dlg_222_eh DLG_222_eh
extern long XVT_CALLCONV1
	DLG_222_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));

#define dlg_337_eh DLG_337_eh
extern long XVT_CALLCONV1
	DLG_337_eh XVT_CALLCONV2 XVT_CC_ARGS( ( WINDOW xdWindow, EVENT* xdEvent ) );

#define win_116_eh WIN_GRAPH_116_eh
extern long XVT_CALLCONV1
	WIN_GRAPH_116_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));
#define win_117_eh WIN_GRAFIK_117_eh
extern long XVT_CALLCONV1
	WIN_GRAFIK_117_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));
#define win_121_eh WIN_121_eh
extern long XVT_CALLCONV1
	WIN_121_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));

#define win_122_eh QWERT_EDIT_eh
extern long XVT_CALLCONV1
	QWERT_EDIT_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));
#define win_130_eh WIN_130_eh
extern long XVT_CALLCONV1
	WIN_130_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));
#define win_131_eh LINES_eh
extern long XVT_CALLCONV1
	LINES_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));


#define win_133_eh WIN_LAYER_eh
extern long XVT_CALLCONV1
	WIN_LAYER_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));

#define dlg_armco_eh DLG_ARMCO_eh
extern long XVT_CALLCONV1
	DLG_ARMCO_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));


//#define dlg_222_eh DLG_222_eh
extern long XVT_CALLCONV1
	DLG_EXECUTE_EXTERN_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));


#ifndef CURSOR_DRUCK
#define CURSOR_DRUCK	12
#endif


#define TASK_MENUBAR 1000

#define TASK_MENUBAR_43_35 1035  //Projektübersicht
#define TASK_MENUBAR_43_44 1044  // Projekte speich. unter
#define TASK_MENUBAR_43_45 1045 // Projekte
#define TASK_MENUBAR_43_46 1046 // Arhivieren(H)
#define TASK_MENUBAR_43_46_124 1124  /*archivieren*/
#define TASK_MENUBAR_43_46_125 1125  /*zurückladen*/
#define TASK_MENUBAR_43_48 1048 /*EXIT*/

#define TASK_MENUBAR_43_11 1011  //Importieren(H)  // unused
#define TASK_MENUBAR_43_11_21 1021 //Projekt aufnehmen
#define TASK_MENUBAR_43_11_23 1023 //Projekteintrag löschen

#define TASK_MENUBAR_50 1050  /*Zustand(h)*/

//#define TASK_MENUBAR_13 1013  //Jetzt Plotten
#define TASK_MENUBAR_50_127 1127  /*Zustände*/
#define TASK_MENUBAR_13_39 1039 /* Zustamndsdatei editieren */
#define TASK_MENUBAR_50_128 1128 /*Zustand speichern als.. */
#define TASK_MENUBAR_13_42 1042 /* Rauheiten ändern */
#define TASK_MENUBAR_13_59 1059   /*Interpolieren*/
#define TASK_MENUBAR_50_5 1005   /*Netzverknüpfung*/

#define TASK_MENUBAR_50_133 1133   /*Vergleichszustand (H)*/ // unused
#define TASK_MENUBAR_13_60 1060   /*Gelände (H->H)*/
#define TASK_MENUBAR_13_60_62 1062  /*Gelände neu*/
#define TASK_MENUBAR_13_60_63 1063  /*Gelände löschen*/
#define TASK_MENUBAR_13_64 1064   /*Fläche (H->H)*/ // unused
#define TASK_MENUBAR_13_64_65 1065 /*Fläche neu*/
#define TASK_MENUBAR_13_64_66 1066  /*Fläche löschen*/

#define TASK_MENUBAR_1 1001    // CAD-Programm
#define TASK_MENUBAR_1_91 1091 // Längs. plotten
#define TASK_MENUBAR_1_2 1002 //Berechnung (H) // Hauptmenu
#define TASK_MENUBAR_1_68_70 1070 // indirekt benutzt
#define TASK_MENUBAR_1_68_71 1071  // Profildatei-Geländeverknüpfung-löschen // indirekt benutzt
#define TASK_MENUBAR_1_69_72 1072 // indirekt benutzt
#define TASK_MENUBAR_1_69_73 1073 // Profildatei-Flächenberechnung-löschen // indirekt benutzt

#define TASK_MENUBAR_1_86 1086 //Ergebnisse der Massenberechnung

#define TASK_MENUBAR_14 1014    // Extras // unused
#define TASK_MENUBAR_14_16 1016 /*Abflussdatei*/
#define TASK_MENUBAR_14_17 1017  /*Verlustdatei*/

#define TASK_MENUBAR_14_18 2000  /*Teilgebite*/

#define TASK_MENUBAR_30 1030 // Längsschnitt vergleichen
#define TASK_MENUBAR_30_32 1032  //Auswerten(h)
#define TASK_MENUBAR_30_7 1007
#define TASK_MENUBAR_30_75 1075
#define TASK_MENUBAR_2_3 1003  /*Massenberechnung*/

#define TASK_MENUBAR_30_84_87 1087//Editor   
#define TASK_MENUBAR_30_84_88 1088 //Wehre

#define TASK_MENUBAR_30_29 1029 // Ergebnisse(H)

#define TASK_MENUBAR_30_29_93 1093   
#define TASK_MENUBAR_13_58_78 1078     /*Antragstellerschild*/
#define TASK_MENUBAR_30_29_58 1058   // DFX(h)
#define TASK_MENUBAR_30_83 1083      // Auswertung->Pruefung(h)

#define TASK_MENUBAR_13_58_74 1074   //DXF:Querprof.

#define TASK_MENUBAR_30_24 1024   ///*Laengsschnitt einsehen*/
#define TASK_MENUBAR_30_12 1012    // Tabelle 
#define TASK_MENUBAR_2_85_89 1089 /*Gerinne*/

#define TASK_MENUBAR_30_90 1090 // Rohre

#define TASK_MENUBAR_30_90_92 1092 // Strassenbau

#define TASK_MENUBAR_30_80 1080 //Beispiellisten 

// #define TASK_MENUBAR_30_85 1085 

#define TASK_MENUBAR_29_32_81 1081 /*Auswertung:Vergleich*/

#define TASK_MENUBAR_29_32_83_82 1082 /*Auswertung:Prüfung*/
#define TASK_MENUBAR_29_32_83_84 1084 /*Auswertung:Prüfung:Voreinstellung*/

#define TASK_MENUBAR_14_21 1021  //Verzeichniseinstellung (H) // unused

#define TASK_MENUBAR_14_27 1027   //Sonderprogramm

#define TASK_MENUBAR_14_28 2001  /*Kopf.txt editieren*/

#define TASK_MENUBAR_WSPWIN_MAPPER 2002

#define TASK_MENUBAR_8 1008 // Hilfe(H)
#define TASK_MENUBAR_8_9 1009 //Inhalt
#define TASK_MENUBAR_8_20 1020 //Optionen...
#define TASK_MENUBAR_8_28 1028 //Kontext
#define TASK_MENUBAR_8_36 1036 // Info

#define TASK_MENUBAR_6 1006 // Konvertieren (H)
#define TASK_MENUBAR_6_15 1015
#define TASK_MENUBAR_43_6_26 1026 //WASPILA
#define TASK_MENUBAR_43_6_61 1061 // DA66
#define TASK_MENUBAR_43_6_77 1077 // WSPWIN-->JABRON
#define TASK_MENUBAR_43_6_95 1095  //JABRON-->WSPWIN
#define TASK_MENUBAR_43_6_38 1038  //CADDY

#define TASK_MENUBAR_ACCESS  1037  // Import aus Access-Export

#define TASK_MENUBAR_13_67 1067  //Plotprogramm (extern) 

#define TASK_MENUBAR_29_31_33 1033
#define TASK_MENUBAR_29_31_34 1034




#ifndef Application
#define Application 100
#endif

#ifndef ZOOM_1
#define ZOOM_1 101
#endif
#ifndef ZOOM_1_TEXT_1
#define ZOOM_1_TEXT_1 1
#endif

#ifndef ZOOM_2
#define ZOOM_2 102
#endif
#ifndef ZOOM_2_TEXT_1
#define ZOOM_2_TEXT_1 1
#endif
/*OK-Brücke:Überfallbeiwert*/
#ifndef DLG_107
#define DLG_107 107
#endif
#ifndef DLG_107_PB_CANCEL
#define DLG_107_PB_CANCEL 1
#endif
#ifndef DLG_107_PB_OK
#define DLG_107_PB_OK 2
#endif
#ifndef DLG_107_TEXT
#define DLG_107_TEXT 3
#endif
#ifndef DLG_107_TEXT_1
#define DLG_107_TEXT_1 5
#endif
#ifndef DLG_107_TEXT_2
#define DLG_107_TEXT_2 12
#endif
#ifndef DLG_107_EDIT
#define DLG_107_EDIT 4
#endif
#ifndef DLG_107_EDIT_6
#define DLG_107_EDIT_6 6
#endif
#ifndef DLG_107_EDIT_7
#define DLG_107_EDIT_7 7
#endif
#ifndef DLG_107_PB_DB
#define DLG_107_PB_DB 1085
#endif
#ifndef DLG_107_RADIOBUTTON_1
#define DLG_107_RADIOBUTTON_1 9
#endif
#ifndef DLG_107_RADIOBUTTON_2
#define DLG_107_RADIOBUTTON_2 10
#endif
#ifndef DLG_107_RADIOBUTTON_3
#define DLG_107_RADIOBUTTON_3 11
#endif


#ifndef DLG_110
#define DLG_110 110
#endif
#ifndef DLG_110_PUSHBUTTON_13
#define DLG_110_PUSHBUTTON_13 1
#endif
#ifndef DLG_110_PUSHBUTTON_14
#define DLG_110_PUSHBUTTON_14 2
#endif
#ifndef DLG_110_TEXT_1
#define DLG_110_TEXT_1 3
#endif
#ifndef DLG_110_TEXT_2
#define DLG_110_TEXT_2 4
#endif
#ifndef DLG_110_TEXT_3
#define DLG_110_TEXT_3 5
#endif
#ifndef DLG_110_TEXT_4
#define DLG_110_TEXT_4 6
#endif
#ifndef DLG_110_TEXT_5
#define DLG_110_TEXT_5 7
#endif
#ifndef DLG_110_TEXT_6
#define DLG_110_TEXT_6 8
#endif
#ifndef DLG_110_TEXT_7
#define DLG_110_TEXT_7 9
#endif
#ifndef DLG_110_EDIT_8
#define DLG_110_EDIT_8 10
#endif
#ifndef DLG_110_EDIT_9
#define DLG_110_EDIT_9 11
#endif
#ifndef DLG_110_EDIT_10
#define DLG_110_EDIT_10 12
#endif
#ifndef DLG_110_EDIT_11
#define DLG_110_EDIT_11 13
#endif
#ifndef DLG_110_EDIT_12
#define DLG_110_EDIT_12 14
#endif
#ifndef DLG_110_CHECKBOX_15
#define DLG_110_CHECKBOX_15 15
#endif
#ifndef DLG_110_GROUPBOX_16
#define DLG_110_GROUPBOX_16 16
#endif
#ifndef DLG_110_GROUPBOX_17
#define DLG_110_GROUPBOX_17 17
#endif
#ifndef DLG_110_DB
#define DLG_110_DB   1036
#endif

#ifndef DLG_122
#define DLG_122 122
#endif
#ifndef DLG_122_PUSHBUTTON_4
#define DLG_122_PUSHBUTTON_4 1
#endif
#ifndef DLG_122_TEXT_1
#define DLG_122_TEXT_1 2
#endif
#ifndef DLG_122_TEXT_2
#define DLG_122_TEXT_2 3
#endif
#ifndef DLG_122_PUSHBUTTON_3
#define DLG_122_PUSHBUTTON_3 4
#endif

#ifndef DLG_124
#define DLG_124 124
#endif
#ifndef DLG_124_GROUPBOX_2
#define DLG_124_GROUPBOX_2 1
#endif
#ifndef DLG_124_RADIOBUTTON_12
#define DLG_124_RADIOBUTTON_12 2
#endif
#ifndef DLG_124_RADIOBUTTON_13
#define DLG_124_RADIOBUTTON_13 3
#endif
#ifndef DLG_124_RADIOBUTTON_14
#define DLG_124_RADIOBUTTON_14 4
#endif
#ifndef DLG_124_RADIOBUTTON_15
#define DLG_124_RADIOBUTTON_15 5
#endif
#ifndef DLG_124_RADIOBUTTON_16
#define DLG_124_RADIOBUTTON_16 6
#endif
#ifndef DLG_124_RADIOBUTTON_17
#define DLG_124_RADIOBUTTON_17 7
#endif
#ifndef DLG_124_RADIOBUTTON_18
#define DLG_124_RADIOBUTTON_18 8
#endif
#ifndef DLG_124_GROUPBOX_19
#define DLG_124_GROUPBOX_19 9
#endif
#ifndef DLG_124_GROUPBOX_20
#define DLG_124_GROUPBOX_20 10
#endif
#ifndef DLG_124_RADIOBUTTON_21
#define DLG_124_RADIOBUTTON_21 11
#endif
#ifndef DLG_124_RADIOBUTTON_22
#define DLG_124_RADIOBUTTON_22 12
#endif
#ifndef DLG_124_RADIOBUTTON_23
#define DLG_124_RADIOBUTTON_23 13
#endif
#ifndef DLG_124_GROUPBOX_24
#define DLG_124_GROUPBOX_24 14
#endif
#ifndef DLG_124_RADIOBUTTON_25
#define DLG_124_RADIOBUTTON_25 15
#endif
#ifndef DLG_124_RADIOBUTTON_26
#define DLG_124_RADIOBUTTON_26 16
#endif
#ifndef DLG_124_RADIOBUTTON_27
#define DLG_124_RADIOBUTTON_27 17
#endif
#ifndef DLG_124_RADIOBUTTON_28
#define DLG_124_RADIOBUTTON_28 18
#endif
#ifndef DLG_124_RADIOBUTTON_29
#define DLG_124_RADIOBUTTON_29 19
#endif
#ifndef DLG_124_GROUPBOX_30
#define DLG_124_GROUPBOX_30 20
#endif
#ifndef DLG_124_RADIOBUTTON_31
#define DLG_124_RADIOBUTTON_31 21
#endif
#ifndef DLG_124_RADIOBUTTON_32
#define DLG_124_RADIOBUTTON_32 22
#endif
#ifndef DLG_124_RADIOBUTTON_33
#define DLG_124_RADIOBUTTON_33 23
#endif
#ifndef DLG_124_GROUPBOX_34
#define DLG_124_GROUPBOX_34 24
#endif
#ifndef DLG_124_RADIOBUTTON_35
#define DLG_124_RADIOBUTTON_35 25
#endif
#ifndef DLG_124_RADIOBUTTON_36
#define DLG_124_RADIOBUTTON_36 26
#endif
#ifndef DLG_124_RADIOBUTTON_37
#define DLG_124_RADIOBUTTON_37 27
#endif
#ifndef DLG_124_GROUPBOX_38
#define DLG_124_GROUPBOX_38 28
#endif
#ifndef DLG_124_RADIOBUTTON_39
#define DLG_124_RADIOBUTTON_39 29
#endif
#ifndef DLG_124_RADIOBUTTON_40
#define DLG_124_RADIOBUTTON_40 30
#endif
#ifndef DLG_124_RADIOBUTTON_41
#define DLG_124_RADIOBUTTON_41 31
#endif
#ifndef DLG_124_RADIOBUTTON_42
#define DLG_124_RADIOBUTTON_42 32
#endif
#ifndef DLG_124_GROUPBOX_43
#define DLG_124_GROUPBOX_43 33
#endif
#ifndef DLG_124_TEXT_44
#define DLG_124_TEXT_44 34
#endif
#ifndef DLG_124_RADIOBUTTON_45
#define DLG_124_RADIOBUTTON_45 35
#endif
#ifndef DLG_124_RADIOBUTTON_46
#define DLG_124_RADIOBUTTON_46 36
#endif
#ifndef DLG_124_RADIOBUTTON_47
#define DLG_124_RADIOBUTTON_47 37
#endif
#ifndef DLG_124_RADIOBUTTON_48
#define DLG_124_RADIOBUTTON_48 38
#endif
#ifndef DLG_124_GROUPBOX_49
#define DLG_124_GROUPBOX_49 39
#endif
#ifndef DLG_124_PUSHBUTTON_50
#define DLG_124_PUSHBUTTON_50 40
#endif
#ifndef DLG_124_PUSHBUTTON_51
#define DLG_124_PUSHBUTTON_51 41
#endif

#ifndef DLG_128
#define DLG_128 128
#endif
#ifndef DLG_128_PUSHBUTTON_7
#define DLG_128_PUSHBUTTON_7 1
#endif
#ifndef DLG_128_PUSHBUTTON_8
#define DLG_128_PUSHBUTTON_8 2
#endif
#ifndef DLG_128_TEXT_1
#define DLG_128_TEXT_1 3
#endif
#ifndef DLG_128_TEXT_2
#define DLG_128_TEXT_2 4
#endif
#ifndef DLG_128_TEXT_3
#define DLG_128_TEXT_3 5
#endif
#ifndef DLG_128_EDIT_4
#define DLG_128_EDIT_4 6
#endif
#ifndef DLG_128_EDIT_5
#define DLG_128_EDIT_5 7
#endif
#ifndef DLG_128_EDIT_6
#define DLG_128_EDIT_6 8
#endif

#ifndef DLG_135
#define DLG_135 135
#endif
#ifndef DLG_135_PUSHBUTTON_1
#define DLG_135_PUSHBUTTON_1 1
#endif
#ifndef DLG_135_PUSHBUTTON_2
#define DLG_135_PUSHBUTTON_2 2
#endif
#ifndef DLG_135_PUSHBUTTON_3
#define DLG_135_PUSHBUTTON_3 3
#endif
#ifndef DLG_135_LBOX_5
#define DLG_135_LBOX_5 4
#endif
#ifndef DLG_135_EDIT_6
#define DLG_135_EDIT_6 5
#endif
#ifndef DLG_135_EDIT_7
#define DLG_135_EDIT_7 6
#endif
#ifndef DLG_135_EDIT_8
#define DLG_135_EDIT_8 7
#endif
#ifndef DLG_135_EDIT_9
#define DLG_135_EDIT_9 8
#endif
#ifndef DLG_135_EDIT_10
#define DLG_135_EDIT_10 9
#endif
#ifndef DLG_135_EDIT_11
#define DLG_135_EDIT_11 10
#endif
#ifndef DLG_135_PUSHBUTTON_4
#define DLG_135_PUSHBUTTON_4 11
#endif


#ifndef DLG_136
#define DLG_136 136
#endif
#ifndef DLG_136_TEXT_2
#define DLG_136_TEXT_2 1
#endif
#ifndef DLG_136_TEXT_3
#define DLG_136_TEXT_3 2
#endif
#ifndef DLG_136_TEXT_6
#define DLG_136_TEXT_6 3
#endif
#ifndef DLG_136_EDIT_7
#define DLG_136_EDIT_7 4
#endif
#ifndef DLG_136_EDIT_8
#define DLG_136_EDIT_8 5
#endif
#ifndef DLG_136_EDIT_9
#define DLG_136_EDIT_9 6
#endif
#ifndef DLG_136_EDIT_10
#define DLG_136_EDIT_10 7
#endif
#ifndef DLG_136_EDIT_11
#define DLG_136_EDIT_11 8
#endif
#ifndef DLG_136_EDIT_12
#define DLG_136_EDIT_12 9
#endif
#ifndef DLG_136_EDIT_13
#define DLG_136_EDIT_13 10
#endif
#ifndef DLG_136_EDIT_14
#define DLG_136_EDIT_14 11
#endif
#ifndef DLG_136_EDIT_15
#define DLG_136_EDIT_15 12
#endif
#ifndef DLG_136_EDIT_16
#define DLG_136_EDIT_16 13
#endif
#ifndef DLG_136_EDIT_18
#define DLG_136_EDIT_18 14
#endif
#ifndef DLG_136_EDIT_19
#define DLG_136_EDIT_19 15
#endif
#ifndef DLG_136_EDIT_20
#define DLG_136_EDIT_20 16
#endif
#ifndef DLG_136_EDIT_21
#define DLG_136_EDIT_21 17
#endif
#ifndef DLG_136_EDIT_22
#define DLG_136_EDIT_22 18
#endif
#ifndef DLG_136_EDIT_23
#define DLG_136_EDIT_23 19
#endif
#ifndef DLG_136_EDIT_24
#define DLG_136_EDIT_24 20
#endif
#ifndef DLG_136_EDIT_25
#define DLG_136_EDIT_25 21
#endif
#ifndef DLG_136_EDIT_26
#define DLG_136_EDIT_26 22
#endif
#ifndef DLG_136_EDIT_27
#define DLG_136_EDIT_27 23
#endif
#ifndef DLG_136_EDIT_28
#define DLG_136_EDIT_28 24
#endif
#ifndef DLG_136_EDIT_29
#define DLG_136_EDIT_29 25
#endif
#ifndef DLG_136_EDIT_30
#define DLG_136_EDIT_30 26
#endif
#ifndef DLG_136_EDIT_31
#define DLG_136_EDIT_31 27
#endif
#ifndef DLG_136_EDIT_32
#define DLG_136_EDIT_32 28
#endif
#ifndef DLG_136_EDIT_33
#define DLG_136_EDIT_33 29
#endif
#ifndef DLG_136_EDIT_34
#define DLG_136_EDIT_34 30
#endif
#ifndef DLG_136_EDIT_35
#define DLG_136_EDIT_35 31
#endif
#ifndef DLG_136_EDIT_36
#define DLG_136_EDIT_36 32
#endif
#ifndef DLG_136_EDIT_37
#define DLG_136_EDIT_37 33
#endif
#ifndef DLG_136_EDIT_38
#define DLG_136_EDIT_38 34
#endif
#ifndef DLG_136_EDIT_39
#define DLG_136_EDIT_39 35
#endif
#ifndef DLG_136_EDIT_40
#define DLG_136_EDIT_40 36
#endif
#ifndef DLG_136_EDIT_41
#define DLG_136_EDIT_41 37
#endif
#ifndef DLG_136_EDIT_42
#define DLG_136_EDIT_42 38
#endif
#ifndef DLG_136_EDIT_43
#define DLG_136_EDIT_43 39
#endif
#ifndef DLG_136_EDIT_44
#define DLG_136_EDIT_44 40
#endif
#ifndef DLG_136_EDIT_45
#define DLG_136_EDIT_45 41
#endif
#ifndef DLG_136_EDIT_46
#define DLG_136_EDIT_46 42
#endif
#ifndef DLG_136_EDIT_47
#define DLG_136_EDIT_47 43
#endif
#ifndef DLG_136_EDIT_48
#define DLG_136_EDIT_48 44
#endif
#ifndef DLG_136_EDIT_49
#define DLG_136_EDIT_49 45
#endif
#ifndef DLG_136_EDIT_50
#define DLG_136_EDIT_50 46
#endif
#ifndef DLG_136_EDIT_51
#define DLG_136_EDIT_51 47
#endif
#ifndef DLG_136_EDIT_52
#define DLG_136_EDIT_52 48
#endif
#ifndef DLG_136_EDIT_53
#define DLG_136_EDIT_53 49
#endif
#ifndef DLG_136_TEXT_66
#define DLG_136_TEXT_66 50
#endif
#ifndef DLG_136_VSCROLL_85
#define DLG_136_VSCROLL_85 51
#endif
#ifndef DLG_136_LBOX_86
#define DLG_136_LBOX_86 52
#endif
#ifndef DLG_136_EDIT_87
#define DLG_136_EDIT_87 53
#endif
#ifndef DLG_136_EDIT_88
#define DLG_136_EDIT_88 54
#endif
#ifndef DLG_136_PUSHBUTTON_1
#define DLG_136_PUSHBUTTON_1 55
#endif
#ifndef DLG_136_PUSHBUTTON_2
#define DLG_136_PUSHBUTTON_2 56
#endif
#ifndef DLG_136_PUSHBUTTON_3
#define DLG_136_PUSHBUTTON_3 57
#endif
#ifndef DLG_136_PUSHBUTTON_4
#define DLG_136_PUSHBUTTON_4 58
#endif
#ifndef DLG_136_PUSHBUTTON_5
#define DLG_136_PUSHBUTTON_5 59
#endif
#ifndef DLG_136_PUSHBUTTON_6
#define DLG_136_PUSHBUTTON_6 60
#endif

#ifndef DLG_136_PUSHBUTTON_7
#define DLG_136_PUSHBUTTON_7 61
#endif

#ifndef DLG_137
#define DLG_137 137
#endif
#ifndef DLG_137_PUSHBUTTON_8
#define DLG_137_PUSHBUTTON_8 1
#endif
#ifndef DLG_137_TEXT_1
#define DLG_137_TEXT_1 2
#endif
#ifndef DLG_137_TEXT_2
#define DLG_137_TEXT_2 3
#endif
#ifndef DLG_137_GROUPBOX_3
#define DLG_137_GROUPBOX_3 4
#endif
#ifndef DLG_137_TEXT_4
#define DLG_137_TEXT_4 5
#endif
#ifndef DLG_137_TEXT_5
#define DLG_137_TEXT_5 6
#endif
#ifndef DLG_137_EDIT_6
#define DLG_137_EDIT_6 7
#endif
#ifndef DLG_137_EDIT_7
#define DLG_137_EDIT_7 8
#endif
#ifndef DLG_137_PUSHBUTTON_9
#define DLG_137_PUSHBUTTON_9 9
#endif
#ifndef DLG_137_EDIT_10
#define DLG_137_EDIT_10 10
#endif
#ifndef DLG_137_TEXT_11
#define DLG_137_TEXT_11 11
#endif
#ifndef DLG_138
#define DLG_138 138
#endif
#ifndef DLG_138_TEXT_1
#define DLG_138_TEXT_1 1
#endif
#ifndef DLG_138_TEXT_2
#define DLG_138_TEXT_2 2
#endif
#ifndef DLG_138_PUSHBUTTON_1
#define DLG_138_PUSHBUTTON_1 3
#endif
#ifndef DLG_138_LISTBOX_1
#define DLG_138_LISTBOX_1 4
#endif
#ifndef DLG_138_PUSHBUTTON_2
#define DLG_138_PUSHBUTTON_2 5
#endif

#ifndef DLG_140
#define DLG_140 140
#endif
#ifndef DLG_140_PUSHBUTTON_6
#define DLG_140_PUSHBUTTON_6 1
#endif
#ifndef DLG_140_TEXT_7
#define DLG_140_TEXT_7 2
#endif
#ifndef DLG_140_TEXT_8
#define DLG_140_TEXT_8 3
#endif
#ifndef DLG_140_EDIT_9
#define DLG_140_EDIT_9 4
#endif
#ifndef DLG_140_EDIT_10
#define DLG_140_EDIT_10 5
#endif
#ifndef DLG_140_PUSHBUTTON_11
#define DLG_140_PUSHBUTTON_11 6
#endif
#ifndef DLG_140_EDIT_12
#define DLG_140_EDIT_12 7
#endif
#ifndef DLG_140_EDIT_13
#define DLG_140_EDIT_13 8
#endif
#ifndef DLG_140_TEXT_14
#define DLG_140_TEXT_14 9
#endif

#ifndef DLG_141
#define DLG_141 141
#endif
#ifndef DLG_141_TEXT_1
#define DLG_141_TEXT_1 1
#endif
#ifndef DLG_141_TEXT_2
#define DLG_141_TEXT_2 2
#endif
#ifndef DLG_141_TEXT_3
#define DLG_141_TEXT_3 3
#endif
#ifndef DLG_141_TEXT_4
#define DLG_141_TEXT_4 4
#endif
#ifndef DLG_141_TEXT_5
#define DLG_141_TEXT_5 5
#endif
#ifndef DLG_141_EDIT_6
#define DLG_141_EDIT_6 6
#endif
#ifndef DLG_141_EDIT_7
#define DLG_141_EDIT_7 7
#endif
#ifndef DLG_141_EDIT_8
#define DLG_141_EDIT_8 8
#endif
#ifndef DLG_141_EDIT_9
#define DLG_141_EDIT_9 9
#endif
#ifndef DLG_141_EDIT_10
#define DLG_141_EDIT_10 10
#endif
#ifndef DLG_141_PUSHBUTTON_11
#define DLG_141_PUSHBUTTON_11 11
#endif
#ifndef DLG_141_PUSHBUTTON_12
#define DLG_141_PUSHBUTTON_12 12
#endif


#ifndef DLG_142
#define DLG_142 142
#endif
#ifndef DLG_142_TEXT_1
#define DLG_142_TEXT_1 1
#endif
#ifndef DLG_142_TEXT_2
#define DLG_142_TEXT_2 2
#endif
#ifndef DLG_142_TEXT_3
#define DLG_142_TEXT_3 3
#endif
#ifndef DLG_142_TEXT_4
#define DLG_142_TEXT_4 4
#endif
#ifndef DLG_142_TEXT_5
#define DLG_142_TEXT_5 5
#endif
#ifndef DLG_142_TEXT_6
#define DLG_142_TEXT_6 6
#endif
#ifndef DLG_142_EDIT_7
#define DLG_142_EDIT_7 7
#endif
#ifndef DLG_142_EDIT_8
#define DLG_142_EDIT_8 8
#endif
#ifndef DLG_142_EDIT_9
#define DLG_142_EDIT_9 9
#endif
#ifndef DLG_142_EDIT_10
#define DLG_142_EDIT_10 10
#endif
#ifndef DLG_142_EDIT_11
#define DLG_142_EDIT_11 11
#endif
#ifndef DLG_142_EDIT_12
#define DLG_142_EDIT_12 12
#endif
#ifndef DLG_142_PUSHBUTTON_13
#define DLG_142_PUSHBUTTON_13 13
#endif
#ifndef DLG_142_PUSHBUTTON_14
#define DLG_142_PUSHBUTTON_14 14
#endif

#define DLG_143 143
#define DLG_143_PUSHBUTTON_5 1
#define DLG_143_TEXT_1 2
#define DLG_143_TEXT_2 3
#define DLG_143_EDIT_3 4
#define DLG_143_EDIT_4 5
#define DLG_143_EDIT_HEIGHT_L 9
#define DLG_143_EDIT_HEIGHT_R 10
#define DLG_143_PUSHBUTTON_6 6
#define DLG_143_TEXT_7 7
#define DLG_143_PUSHBUTTON_8 8

#ifndef DLG_144
#define DLG_144 144
#endif
#ifndef DLG_144_TEXT_2
#define DLG_144_TEXT_2 1
#endif
#ifndef DLG_144_TEXT_4
#define DLG_144_TEXT_4 2
#endif
#ifndef DLG_144_TEXT_5
#define DLG_144_TEXT_5 3
#endif
#ifndef DLG_144_TEXT_6
#define DLG_144_TEXT_6 4
#endif
#ifndef DLG_144_EDIT_1
#define DLG_144_EDIT_1 5
#endif
#ifndef DLG_144_EDIT_2
#define DLG_144_EDIT_2 6
#endif
#ifndef DLG_144_EDIT_3
#define DLG_144_EDIT_3 7
#endif
#ifndef DLG_144_EDIT_4
#define DLG_144_EDIT_4 8
#endif
#ifndef DLG_144_PUSHBUTTON_1
#define DLG_144_PUSHBUTTON_1 9
#endif
#ifndef DLG_144_PUSHBUTTON_2
#define DLG_144_PUSHBUTTON_2 10
#endif

#ifndef DLG_145
#define DLG_145 145
#endif
#ifndef DLG_145_TEXT_1
#define DLG_145_TEXT_1 1
#endif
#ifndef DLG_145_PUSHBUTTON_11
#define DLG_145_PUSHBUTTON_11 2
#endif
#ifndef DLG_145_TEXT_2
#define DLG_145_TEXT_2 3
#endif
#ifndef DLG_145_TEXT_3
#define DLG_145_TEXT_3 4
#endif
#ifndef DLG_145_RADIOBUTTON_4
#define DLG_145_RADIOBUTTON_4 5
#endif
#ifndef DLG_145_RADIOBUTTON_5
#define DLG_145_RADIOBUTTON_5 6
#endif
#ifndef DLG_145_RADIOBUTTON_6
#define DLG_145_RADIOBUTTON_6 7
#endif
#ifndef DLG_145_RADIOBUTTON_7
#define DLG_145_RADIOBUTTON_7 8
#endif
#ifndef DLG_145_GROUPBOX_8
#define DLG_145_GROUPBOX_8 9
#endif
#ifndef DLG_145_GROUPBOX_9
#define DLG_145_GROUPBOX_9 10
#endif
#ifndef DLG_145_PUSHBUTTON_10
#define DLG_145_PUSHBUTTON_10 11
#endif

#define DLG_147 147
#define DLG_147_TEXT_1 1
#define DLG_147_TEXT_2 2
#define DLG_147_TEXT_3 3
#define DLG_147_EDIT 4
#define DLG_147_PUSHBUTTON_1 5
#define DLG_147_PUSHBUTTON_2 6
#define DLG_147_PUSHBUTTON_3 7

#ifndef DLG_148
#define DLG_148 148
#endif
#ifndef DLG_148_LBOX_1
#define DLG_148_LBOX_1 1
#endif
#ifndef DLG_148_LBOX_2
#define DLG_148_LBOX_2 7
#endif
#ifndef DLG_148_TEXT_2
#define DLG_148_TEXT_2 2
#endif
#ifndef DLG_148_PUSHBUTTON_3
#define DLG_148_PUSHBUTTON_3 3
#endif
#ifndef DLG_148_PUSHBUTTON_4
#define DLG_148_PUSHBUTTON_4 4
#endif
#ifndef DLG_148_PUSHBUTTON_5
#define DLG_148_PUSHBUTTON_5 5
#endif
#define IDC_PROJEKT_NEU   1033
#define IDC_PROJEKT_DEL   1034

#ifndef DLG_149
#define DLG_149 149
#endif
#ifndef DLG_149_PUSHBUTTON_7
#define DLG_149_PUSHBUTTON_7 1
#endif
#ifndef DLG_149_TEXT_1
#define DLG_149_TEXT_1 2
#endif
#ifndef DLG_149_TEXT_2
#define DLG_149_TEXT_2 3
#endif
#ifndef DLG_149_TEXT_3
#define DLG_149_TEXT_3 4
#endif
#ifndef DLG_149_TEXT_4
#define DLG_149_TEXT_4 5
#endif
#ifndef DLG_149_TEXT_5
#define DLG_149_TEXT_5 6
#endif
#ifndef DLG_149_TEXT_6
#define DLG_149_TEXT_6 7
#endif


#ifndef DLG_OPEN_FILE
#define DLG_OPEN_FILE 150
#endif
#ifndef DLG_150_PUSHBUTTON_OK
#define DLG_150_PUSHBUTTON_OK 1
#endif
#ifndef DLG_150_PUSHBUTTON_CANCEL
#define DLG_150_PUSHBUTTON_CANCEL 2
#endif
#ifndef DLG_150_EDIT_3
#define DLG_150_EDIT_3 3
#endif
#ifndef DLG_150_LBOX_4
#define DLG_150_LBOX_4 4
#endif
#ifndef DLG_150_LISTEDIT_5
#define DLG_150_LISTEDIT_5 5
#endif
#ifndef DLG_150_TEXT_6
#define DLG_150_TEXT_6 6
#endif
#ifndef DLG_150_TEXT_7
#define DLG_150_TEXT_7 7
#endif
#ifndef DLG_151
#define DLG_151 151
#endif
#ifndef DLG_151_TEXT_1
#define DLG_151_TEXT_1 1
#endif
#ifndef DLG_151_PUSHBUTTON_8
#define DLG_151_PUSHBUTTON_8 2
#endif
#ifndef DLG_151_TEXT_2
#define DLG_151_TEXT_2 3
#endif
#ifndef DLG_151_GROUPBOX_3
#define DLG_151_GROUPBOX_3 4
#endif
#ifndef DLG_151_TEXT_4
#define DLG_151_TEXT_4 5
#endif
#ifndef DLG_151_TEXT_5
#define DLG_151_TEXT_5 6
#endif
#ifndef DLG_151_EDIT_6
#define DLG_151_EDIT_6 7
#endif
#ifndef DLG_151_EDIT_7
#define DLG_151_EDIT_7 8
#endif
#ifndef DLG_151_PUSHBUTTON_9
#define DLG_151_PUSHBUTTON_9 9
#endif


#ifndef DLG_FLAECHE
#define DLG_FLAECHE 153
#endif
#ifndef DLG_FLAECHE_PB_OK
#define DLG_FLAECHE_PB_OK 1
#endif
#ifndef DLG_FLAECHE_EDIT_1
#define DLG_FLAECHE_EDIT_1 4
#endif
#ifndef DLG_FLAECHE_EDIT_2
#define DLG_FLAECHE_EDIT_2 5
#endif
#ifndef DLG_FLAECHE_EDIT_3
#define DLG_FLAECHE_EDIT_3 8
#endif


#ifndef DLG_154
#define DLG_154 154
#endif
#ifndef DLG_154_PUSHBUTTON_13
#define DLG_154_PUSHBUTTON_13 1
#endif
#ifndef DLG_154_PUSHBUTTON_14
#define DLG_154_PUSHBUTTON_14 2
#endif
#ifndef DLG_154_TEXT_1
#define DLG_154_TEXT_1 3
#endif
#ifndef DLG_154_TEXT_2
#define DLG_154_TEXT_2 4
#endif
#ifndef DLG_154_TEXT_3
#define DLG_154_TEXT_3 5
#endif
#ifndef DLG_154_TEXT_4
#define DLG_154_TEXT_4 6
#endif
#ifndef DLG_154_TEXT_5
#define DLG_154_TEXT_5 7
#endif
#ifndef DLG_154_TEXT_6
#define DLG_154_TEXT_6 8
#endif
#ifndef DLG_154_TEXT_7
#define DLG_154_TEXT_7 9
#endif
#ifndef DLG_154_EDIT_8
#define DLG_154_EDIT_8 10
#endif
#ifndef DLG_154_EDIT_9
#define DLG_154_EDIT_9 11
#endif
#ifndef DLG_154_EDIT_10
#define DLG_154_EDIT_10 12
#endif
#ifndef DLG_154_EDIT_11
#define DLG_154_EDIT_11 13
#endif
#ifndef DLG_154_EDIT_12
#define DLG_154_EDIT_12 14
#endif
#ifndef DLG_154_CHECKBOX_15
#define DLG_154_CHECKBOX_15 15
#endif
#ifndef DLG_154_GROUPBOX_16
#define DLG_154_GROUPBOX_16 16
#endif
#ifndef DLG_154_GROUPBOX_17
#define DLG_154_GROUPBOX_17 17
#endif
#ifndef DLG_154_DB
#define DLG_154_DB          18
#endif

#define DLG_155 155
#define DLG_155_PUSHBUTTON_16 1
#define DLG_155_PUSHBUTTON_17 2
#define DLG_155_TEXT_1 3
#define DLG_155_TEXT_2 4
#define DLG_155_TEXT_3 5
#define DLG_155_TEXT_4 6
#define DLG_155_TEXT_6 7
#define DLG_155_EDIT_7 8
#define DLG_155_EDIT_8 9
#define DLG_155_EDIT_9 10
#define DLG_155_EDIT_11 11
#define DLG_155_EDIT_12 12
#define DLG_155_TEXT_13 13
#define DLG_155_EDIT_14 14
#define DLG_155_EDIT_15 15

#ifndef DLG_156
#define DLG_156 156
#endif
#ifndef DLG_156_PUSHBUTTON_OK
#define DLG_156_PUSHBUTTON_OK 1
#endif
#ifndef DLG_156_PUSHBUTTON_QUIT
#define DLG_156_PUSHBUTTON_QUIT 2
#endif
#ifndef DLG_156_TEXT_1
#define DLG_156_TEXT_1 3
#endif
#ifndef DLG_156_TEXT_2
#define DLG_156_TEXT_2 4
#endif
#ifndef DLG_156_TEXT_3
#define DLG_156_TEXT_3 5
#endif
#ifndef DLG_156_TEXT_4
#define DLG_156_TEXT_4 6
#endif
#ifndef DLG_156_TEXT_5
#define DLG_156_TEXT_5 7
#endif
#ifndef DLG_156_EDIT_1
#define DLG_156_EDIT_1 8
#endif
#ifndef DLG_156_EDIT_2
#define DLG_156_EDIT_2 9
#endif
#ifndef DLG_156_EDIT_3
#define DLG_156_EDIT_3 10
#endif
#ifndef DLG_156_EDIT_4
#define DLG_156_EDIT_4 11
#endif
#ifndef DLG_156_EDIT_5
#define DLG_156_EDIT_5 12
#endif
#ifndef DLG_156_EDIT_R1
#define DLG_156_EDIT_R1 13
#endif
#ifndef DLG_156_EDIT_R2
#define DLG_156_EDIT_R2 14
#endif
#ifndef DLG_156_EDIT_R3
#define DLG_156_EDIT_R3 15
#endif
#ifndef DLG_156_EDIT_W1
#define DLG_156_EDIT_W1 16
#endif
#ifndef DLG_156_EDIT_W2
#define DLG_156_EDIT_W2 17
#endif
#ifndef DLG_156_EDIT_FCRED
#define DLG_156_EDIT_FCRED 18
#endif
#ifndef DLG_156_TEXT_17
#define DLG_156_TEXT_17 19
#endif
#ifndef DLG_156_TEXT_18
#define DLG_156_TEXT_18 20
#endif
#ifndef DLG_156_TEXT_19
#define DLG_156_TEXT_19 21
#endif
#ifndef DLG_156_TEXT_20

#define DLG_156_TEXT_20 22
#endif
#ifndef DLG_156_TEXT_21
#define DLG_156_TEXT_21 23
#endif
#ifndef DLG_156_TEXT_22
#define DLG_156_TEXT_22 24
#endif
#ifndef DLG_156_LISTEDIT
#define DLG_156_LISTEDIT 25
#endif
#ifndef DLG_156_TEXT_26
#define DLG_156_TEXT_26 26
#endif
#ifndef DLG_156_DB
#define DLG_156_DB    1075
#endif

#ifndef DLG_157
#define DLG_157 157
#endif
#ifndef DLG_157_PUSHBUTTON_OK
#define DLG_157_PUSHBUTTON_OK 1
#endif
#ifndef DLG_157_PUSHBUTTON_QUIT
#define DLG_157_PUSHBUTTON_QUIT 2
#endif
#ifndef DLG_157_TEXT_26
#define DLG_157_TEXT_26 3
#endif
#ifndef DLG_157_TEXT_27
#define DLG_157_TEXT_27 4
#endif
#ifndef DLG_157_TEXT_28
#define DLG_157_TEXT_28 5
#endif
#ifndef DLG_157_TEXT_29
#define DLG_157_TEXT_29 6
#endif
#ifndef DLG_157_TEXT_30
#define DLG_157_TEXT_30 7
#endif
#ifndef DLG_157_EDIT_1
#define DLG_157_EDIT_1 8
#endif
#ifndef DLG_157_EDIT_2
#define DLG_157_EDIT_2 9
#endif
#ifndef DLG_157_EDIT_3
#define DLG_157_EDIT_3 10
#endif
#ifndef DLG_157_EDIT_4
#define DLG_157_EDIT_4 11
#endif
#ifndef DLG_157_EDIT_5
#define DLG_157_EDIT_5 12
#endif
#ifndef DLG_157_EDIT_RCO
#define DLG_157_EDIT_RCO 13
#endif
#ifndef DLG_157_EDIT_R1CO
#define DLG_157_EDIT_R1CO 14
#endif
#ifndef DLG_157_EDIT_R2CO
#define DLG_157_EDIT_R2CO 15
#endif
#ifndef DLG_157_EDIT_BCO
#define DLG_157_EDIT_BCO 16
#endif
#ifndef DLG_157_EDIT_FCRED
#define DLG_157_EDIT_FCRED 17
#endif
#ifndef DLG_157_TEXT_42
#define DLG_157_TEXT_42 18
#endif
#ifndef DLG_157_TEXT_43
#define DLG_157_TEXT_43 19
#endif
#ifndef DLG_157_TEXT_44
#define DLG_157_TEXT_44 20
#endif
#ifndef DLG_157_TEXT_45
#define DLG_157_TEXT_45 21
#endif
#ifndef DLG_157_TEXT_46
#define DLG_157_TEXT_46 22
#endif
#ifndef DLG_157_DB
#define DLG_157_DB    1076
#endif

#ifndef DLG_158
#define DLG_158 158
#endif
#ifndef DLG_158_PUSHBUTTON_7
#define DLG_158_PUSHBUTTON_7 1
#endif
#ifndef DLG_158_PUSHBUTTON_8
#define DLG_158_PUSHBUTTON_8 2
#endif
#ifndef DLG_158_TEXT_1
#define DLG_158_TEXT_1 3
#endif
#ifndef DLG_158_TEXT_2
#define DLG_158_TEXT_2 4
#endif
#ifndef DLG_158_TEXT_3
#define DLG_158_TEXT_3 5
#endif
#ifndef DLG_158_EDIT_1
#define DLG_158_EDIT_1 6
#endif
#ifndef DLG_158_EDIT_2
#define DLG_158_EDIT_2 7
#endif
#ifndef DLG_158_EDIT_3
#define DLG_158_EDIT_3 8
#endif
#ifndef DLG_159
#define DLG_159 159
#endif
#ifndef DLG_159_PUSHBUTTON_OK
#define DLG_159_PUSHBUTTON_OK 1
#endif
#ifndef DLG_159_PUSHBUTTON_QUIT
#define DLG_159_PUSHBUTTON_QUIT 2
#endif
#ifndef DLG_159_TEXT_1
#define DLG_159_TEXT_1 3
#endif
#ifndef DLG_159_TEXT_2
#define DLG_159_TEXT_2 4
#endif
#ifndef DLG_159_TEXT_3
#define DLG_159_TEXT_3 5
#endif
#ifndef DLG_159_TEXT_4
#define DLG_159_TEXT_4 6
#endif
#ifndef DLG_159_TEXT_5
#define DLG_159_TEXT_5 7
#endif
#ifndef DLG_159_TEXT_6
#define DLG_159_TEXT_6 8
#endif
#ifndef DLG_159_TEXT_7
#define DLG_159_TEXT_7 9
#endif
#ifndef DLG_159_GROUPBOX_8
#define DLG_159_GROUPBOX_8 10
#endif
#ifndef DLG_159_GROUPBOX_9
#define DLG_159_GROUPBOX_9 11
#endif
#ifndef DLG_159_EDIT_1
#define DLG_159_EDIT_1 14
#endif
#ifndef DLG_159_EDIT_2
#define DLG_159_EDIT_2 12
#endif
#ifndef DLG_159_EDIT_3
#define DLG_159_EDIT_3 13
#endif
#ifndef DLG_159_EDIT_4
#define DLG_159_EDIT_4 17
#endif
#ifndef DLG_159_EDIT_5
#define DLG_159_EDIT_5 15
#endif
#ifndef DLG_159_EDIT_6
#define DLG_159_EDIT_6 16
#endif

#ifndef DLG_160
#define DLG_160 160
#endif
#ifndef DLG_160_LISTBUTTON_2
#define DLG_160_LISTBUTTON_2 1
#endif
#ifndef DLG_160_TEXT_3
#define DLG_160_TEXT_3 2
#endif
#ifndef DLG_160_PUSHBUTTON_12
#define DLG_160_PUSHBUTTON_12 3
#endif
#ifndef DLG_160_PUSHBUTTON_13
#define DLG_160_PUSHBUTTON_13 4
#endif
#ifndef DLG_160_TEXT_14
#define DLG_160_TEXT_14 5
#endif
#ifndef DLG_160_TEXT_15
#define DLG_160_TEXT_15 6
#endif
#ifndef DLG_160_EDIT_16
#define DLG_160_EDIT_16 7
#endif
#ifndef DLG_160_EDIT_17
#define DLG_160_EDIT_17 8
#endif
#ifndef DLG_160_TEXT_18
#define DLG_160_TEXT_18 9
#endif
#ifndef DLG_160_TEXT_19
#define DLG_160_TEXT_19 10
#endif
#ifndef DLG_160_EDIT_20
#define DLG_160_EDIT_20 11
#endif
#ifndef DLG_160_EDIT_21
#define DLG_160_EDIT_21 12
#endif
#ifndef DLG_160_TEXT_22
#define DLG_160_TEXT_22 13
#endif
#ifndef DLG_160_EDIT_23
#define DLG_160_EDIT_23 14
#endif
#ifndef DLG_160_PUSHBUTTON_DB
#define DLG_160_PUSHBUTTON_DB 15
#endif

#ifndef DLG_161
#define DLG_161 152
#endif
#ifndef DLG_161_PB_OK
#define DLG_161_PB_OK 1
#endif
#ifndef DLG_161_PB_CANCEL
#define DLG_161_PB_CANCEL 2
#endif
#ifndef DLG_161_LBOX_1
#define DLG_161_LBOX_1 3
#endif
#ifndef DLG_161_TEXT_2
#define DLG_161_TEXT_2 4
#endif
#ifndef DLG_161_TEXT_3
#define DLG_161_TEXT_3 5
#endif
#ifndef DLG_161_LBOX_4
#define DLG_161_LBOX_4 6
#endif
#ifndef DLG_161_PB_NOTPAINT
#define DLG_161_PB_NOTPAINT 7
#endif
#ifndef DLG_161_PB_PAINT
#define DLG_161_PB_PAINT 8
#endif
#ifndef DLG_161_TEXT_9
#define DLG_161_TEXT_9 9
#endif
#ifndef DLG_161_EDIT_10
#define DLG_161_EDIT_10 10
#endif
#ifndef DLG_161_EDIT_12
#define DLG_161_EDIT_12 12
#endif

#ifndef DLG_CHANGE_RAUH
#define DLG_CHANGE_RAUH 162
#endif
#ifndef DLG_CHANGE_RAUH_PB_OK
#define DLG_CHANGE_RAUH_PB_OK 1
#endif
#ifndef DLG_CHANGE_RAUH_PB_CANCEL
#define DLG_CHANGE_RAUH_PB_CANCEL 2
#endif
#ifndef DLG_CHANGE_RAUH_LBOX
#define DLG_CHANGE_RAUH_LBOX 3
#endif
#ifndef DLG_CHANGE_RAUH_EDIT_5
#define DLG_CHANGE_RAUH_EDIT_5 4
#endif
#ifndef DLG_CHANGE_RAUH_EDIT_6
#define DLG_CHANGE_RAUH_EDIT_6 5
#endif
#ifndef DLG_CHANGE_RAUH_EDIT_7
#define DLG_CHANGE_RAUH_EDIT_7 6
#endif
#ifndef DLG_CHANGE_RAUH_TEXT_8
#define DLG_CHANGE_RAUH_TEXT_8 7
#endif
#ifndef DLG_CHANGE_RAUH_TEXT_9
#define DLG_CHANGE_RAUH_TEXT_9 8
#endif
#ifndef DLG_CHANGE_RAUH_TEXT_10
#define DLG_CHANGE_RAUH_TEXT_10 9
#endif
#ifndef DLG_CHANGE_RAUH_TEXT_11
#define DLG_CHANGE_RAUH_TEXT_11 10
#endif
#ifndef DLG_CHANGE_RAUH_PB_PLOTTEN
#define DLG_CHANGE_RAUH_PB_PLOTTEN 20
#endif
#ifndef DLG_162_STEMPEL
#define DLG_162_STEMPEL   22
#endif
#ifndef DLG_162_STANDART_PLOT
#define DLG_162_STANDART_PLOT  1039
#endif
#ifndef DLG_162_PLOTDB
#define DLG_162_PLOTDB         1040
#endif

#ifndef DLG_163
#define DLG_163 163
#endif
#ifndef DLG_163_PB_OK
#define DLG_163_PB_OK 1
#endif
#ifndef DLG_163_EDIT_1
#define DLG_163_EDIT_1 4
#endif
#ifndef DLG_163_EDIT_2
#define DLG_163_EDIT_2 5
#endif
#ifndef DLG_163_EDIT_3
#define DLG_163_EDIT_3 8
#endif
/*Dialog 166 */
#ifndef DLG_CHANGE_STATION
#define DLG_CHANGE_STATION 166
#endif
#ifndef DLG_CHANGE_STATION_LOW
#define DLG_CHANGE_STATION_LOW 1
#endif
#ifndef DLG_CHANGE_STATION_HIGH
#define DLG_CHANGE_STATION_HIGH	2
#endif
#ifndef DLG_CHANGE_STATION_COMBO
#define DLG_CHANGE_STATION_COMBO 3
#endif
/*enddef Dialog 166 */

#ifndef DLG_167
#define DLG_167 167
#endif
#ifndef DLG_167_TEXT
#define DLG_167_TEXT 1
#endif


#ifndef DLG_168
#define DLG_168 168
#endif
#ifndef DLG_168_LBOX
#define DLG_168_LBOX 1
#endif
#ifndef DLG_168_PUSHBUTTON_OK
#define DLG_168_PUSHBUTTON_OK 2
#endif
#ifndef DLG_168_PUSHBUTTON_CANCEL
#define DLG_168_PUSHBUTTON_CANCEL 3
#endif
#ifndef DLG_168_PUNKT
#define DLG_168_PUNKT 4
#endif

#ifndef DLG_169
#define DLG_169 169
#endif
#ifndef DLG_169_LBOX
#define DLG_169_LBOX 1
#endif
#ifndef DLG_169_PUSHBUTTON_OK
#define DLG_169_PUSHBUTTON_OK 2
#endif
#ifndef DLG_169_PUSHBUTTON_CANCEL
#define DLG_169_PUSHBUTTON_CANCEL 3
#endif

#ifndef DLG_203
#define DLG_203 203
#endif
#ifndef DLG_400
#define DLG_400 400     //wie 203 aber multiselect
#endif
#ifndef DLG_203_LBOX_1
#define DLG_203_LBOX_1 1
#endif
#ifndef DLG_203_EDIT_2
#define DLG_203_EDIT_2 2
#endif
#ifndef DLG_203_EDIT_3
#define DLG_203_EDIT_3 3
#endif
#ifndef DLG_203_EDIT_4
#define DLG_203_EDIT_4 4
#endif
#ifndef DLG_203_EDIT_5
#define DLG_203_EDIT_5 5
#endif
#ifndef DLG_203_PUSHBUTTON_6
#define DLG_203_PUSHBUTTON_6 6
#endif
#ifndef DLG_203_PUSHBUTTON_7
#define DLG_203_PUSHBUTTON_7 7
#endif
#ifndef DLG_203_PUSHBUTTON_8
#define DLG_203_PUSHBUTTON_8 8
#endif
#ifndef DLG_203_PUSHBUTTON_9
#define DLG_203_PUSHBUTTON_9 9
#endif
#ifndef DLG_203_PUSHBUTTON_10
#define DLG_203_PUSHBUTTON_10 10
#endif
#ifndef DLG_203_PUSHBUTTON_11
#define DLG_203_PUSHBUTTON_11 11
#endif
#ifndef DLG_203_PUSHBUTTON_12
#define DLG_203_PUSHBUTTON_12 12
#endif
#ifndef DLG_203_STEMPEL
#define DLG_203_STEMPEL     1038
#endif
#ifndef DLG_204
#define DLG_204 204
#endif
#ifndef DLG_204_LBOX_1
#define DLG_204_LBOX_1 1
#endif
#ifndef DLG_204_TEXT_2
#define DLG_204_TEXT_2 2
#endif
#ifndef DLG_204_PUSHBUTTON_3
#define DLG_204_PUSHBUTTON_3 3
#endif
#ifndef DLG_204_PUSHBUTTON_4
#define DLG_204_PUSHBUTTON_4 4
#endif

#ifndef DLG_208
#define DLG_208 208
#endif
#ifndef DLG_208_TEXT_1
#define DLG_208_TEXT_1 1
#endif
#ifndef DLG_208_TEXT_2
#define DLG_208_TEXT_2 2
#endif
#ifndef DLG_208_TEXT_3
#define DLG_208_TEXT_3 3
#endif
#ifndef DLG_208_TEXT_4
#define DLG_208_TEXT_4 4
#endif
#ifndef DLG_208_TEXT_5
#define DLG_208_TEXT_5 5
#endif
#ifndef DLG_208_EDIT_6
#define DLG_208_EDIT_6 6
#endif
#ifndef DLG_208_EDIT_7
#define DLG_208_EDIT_7 7
#endif
#ifndef DLG_208_EDIT_8
#define DLG_208_EDIT_8 8
#endif
#ifndef DLG_208_EDIT_9
#define DLG_208_EDIT_9 9
#endif
#ifndef DLG_208_EDIT_10
#define DLG_208_EDIT_10 10
#endif
#ifndef DLG_208_PUSHBUTTON_11
#define DLG_208_PUSHBUTTON_11 11
#endif
#ifndef DLG_208_PUSHBUTTON_12
#define DLG_208_PUSHBUTTON_12 12
#endif
#ifndef DLG_208_TEXT_13
#define DLG_208_TEXT_13 13
#endif
#ifndef DLG_208_EDIT_14
#define DLG_208_EDIT_14 14
#endif

#ifndef DLG_209
#define DLG_209 209
#endif
#ifndef DLG_209_TEXT_1
#define DLG_209_TEXT_1 1
#endif
#ifndef DLG_209_TEXT_2
#define DLG_209_TEXT_2 2
#endif
#ifndef DLG_209_TEXT_3
#define DLG_209_TEXT_3 3
#endif
#ifndef DLG_209_TEXT_4
#define DLG_209_TEXT_4 4
#endif
#ifndef DLG_209_TEXT_5
#define DLG_209_TEXT_5 5
#endif
#ifndef DLG_209_EDIT_6
#define DLG_209_EDIT_6 6
#endif
#ifndef DLG_209_EDIT_7
#define DLG_209_EDIT_7 7
#endif
#ifndef DLG_209_EDIT_8
#define DLG_209_EDIT_8 8
#endif
#ifndef DLG_209_EDIT_9
#define DLG_209_EDIT_9 9
#endif
#ifndef DLG_209_EDIT_10
#define DLG_209_EDIT_10 10
#endif
#ifndef DLG_209_TEXT_11
#define DLG_209_TEXT_11 11
#endif
#ifndef DLG_209_TEXT_12
#define DLG_209_TEXT_12 12
#endif
#ifndef DLG_209_EDIT_13
#define DLG_209_EDIT_13 13
#endif
#ifndef DLG_209_TEXT_14
#define DLG_209_TEXT_14 14
#endif
#ifndef DLG_209_PUSHBUTTON_15
#define DLG_209_PUSHBUTTON_15 15
#endif
#ifndef DLG_209_PUSHBUTTON_16
#define DLG_209_PUSHBUTTON_16 16
#endif
#ifndef DLG_209_CHECKBOX_17
#define DLG_209_CHECKBOX_17 17
#endif
#ifndef DLG_209_TEXT_18
#define DLG_209_TEXT_18 18
#endif
#ifndef DLG_210
#define DLG_210 210
#endif
#ifndef DLG_210_LBOX_1
#define DLG_210_LBOX_1 1
#endif
#ifndef DLG_210_TEXT_2
#define DLG_210_TEXT_2 2
#endif
#ifndef DLG_210_PUSHBUTTON_3
#define DLG_210_PUSHBUTTON_3 3
#endif
#ifndef DLG_210_PUSHBUTTON_4
#define DLG_210_PUSHBUTTON_4 4
#endif
#ifndef DLG_211
#define DLG_211 211
#endif
#ifndef DLG_211_TEXT_1
#define DLG_211_TEXT_1 1
#endif
#ifndef DLG_211_TEXT_2
#define DLG_211_TEXT_2 2
#endif
#ifndef DLG_211_TEXT_3
#define DLG_211_TEXT_3 3
#endif
#ifndef DLG_211_TEXT_4
#define DLG_211_TEXT_4 4
#endif
#ifndef DLG_211_LBOX_5
#define DLG_211_LBOX_5 5
#endif
#ifndef DLG_211_CHECKBOX_6
#define DLG_211_CHECKBOX_6 6
#endif
#ifndef DLG_211_EDIT_7
#define DLG_211_EDIT_7 7
#endif
#ifndef DLG_211_EDIT_8
#define DLG_211_EDIT_8 8
#endif
#ifndef DLG_211_EDIT_9
#define DLG_211_EDIT_9 9
#endif
#ifndef DLG_211_EDIT_10
#define DLG_211_EDIT_10 10
#endif
#ifndef DLG_211_EDIT_11
#define DLG_211_EDIT_11 11
#endif
#ifndef DLG_211_TEXT_12
#define DLG_211_TEXT_12 12
#endif
#ifndef DLG_211_TEXT_13
#define DLG_211_TEXT_13 13
#endif
#ifndef DLG_211_TEXT_14
#define DLG_211_TEXT_14 14
#endif
#ifndef DLG_211_TEXT_15
#define DLG_211_TEXT_15 15
#endif
#ifndef DLG_211_TEXT_16
#define DLG_211_TEXT_16 16
#endif
#ifndef DLG_211_PUSHBUTTON_17
#define DLG_211_PUSHBUTTON_17 17
#endif
#ifndef DLG_211_PUSHBUTTON_18
#define DLG_211_PUSHBUTTON_18 18
#endif
#ifndef DLG_211_CHECKBOX_19
#define DLG_211_CHECKBOX_19 19
#endif
#ifndef DLG_211_EDIT_20
#define DLG_211_EDIT_20 20
#endif
#ifndef DLG_211_TEXT_21
#define DLG_211_TEXT_21 21
#endif
#ifndef DLG_212
#define DLG_212 212
#endif
#ifndef DLG_212_TEXT_1
#define DLG_212_TEXT_1 1
#endif
#ifndef DLG_212_TEXT_2
#define DLG_212_TEXT_2 2
#endif
#ifndef DLG_212_TEXT_3
#define DLG_212_TEXT_3 3
#endif
#ifndef DLG_212_TEXT_4
#define DLG_212_TEXT_4 4
#endif
#ifndef DLG_212_TEXT_5
#define DLG_212_TEXT_5 5
#endif
#ifndef DLG_212_EDIT_0
#define DLG_212_EDIT_0 6
#endif
#ifndef DLG_212_EDIT_1
#define DLG_212_EDIT_1 7
#endif
#ifndef DLG_212_EDIT_2
#define DLG_212_EDIT_2 8
#endif
#ifndef DLG_212_EDIT_3
#define DLG_212_EDIT_3 9
#endif
#ifndef DLG_212_PUSHBUTTON_10
#define DLG_212_PUSHBUTTON_10 10
#endif
#ifndef DLG_212_PUSHBUTTON_11
#define DLG_212_PUSHBUTTON_11 11
#endif
#ifndef DLG_212_CHECKBOX_12
#define DLG_212_CHECKBOX_12 12
#endif

#ifndef DLG_213
#define DLG_213 213
#endif
#ifndef DLG_213_LISTBUTTON_1
#define DLG_213_LISTBUTTON_1 1
#endif
#ifndef DLG_213_TEXT_2
#define DLG_213_TEXT_2 2
#endif
#ifndef DLG_213_TEXT_3
#define DLG_213_TEXT_3 3
#endif
#ifndef DLG_213_EDIT_4
#define DLG_213_EDIT_4 4
#endif
#ifndef DLG_213_TEXT_5
#define DLG_213_TEXT_5 5
#endif
#ifndef DLG_213_TEXT_6
#define DLG_213_TEXT_6 6
#endif
#ifndef DLG_213_TEXT_7
#define DLG_213_TEXT_7 7
#endif
#ifndef DLG_213_TEXT_8
#define DLG_213_TEXT_8 8
#endif
#ifndef DLG_213_TEXT_9
#define DLG_213_TEXT_9 9
#endif
#ifndef DLG_213_TEXT_10
#define DLG_213_TEXT_10 10
#endif
#ifndef DLG_213_PUSHBUTTON_11
#define DLG_213_PUSHBUTTON_11 11
#endif
#ifndef DLG_213_PUSHBUTTON_12
#define DLG_213_PUSHBUTTON_12 12
#endif

#ifndef DLG_217
#define DLG_217 217
#endif
#ifndef DLG_217_RADIOBUTTON_1
#define DLG_217_RADIOBUTTON_1 1
#endif
#ifndef DLG_217_RADIOBUTTON_2
#define DLG_217_RADIOBUTTON_2 2
#endif
#ifndef DLG_217_RADIOBUTTON_3
#define DLG_217_RADIOBUTTON_3 3
#endif
#ifndef DLG_217_GROUPBOX_4
#define DLG_217_GROUPBOX_4 4
#endif
#ifndef DLG_217_PUSHBUTTON_5
#define DLG_217_PUSHBUTTON_5 5
#endif
#ifndef DLG_217_PUSHBUTTON_6
#define DLG_217_PUSHBUTTON_6 6
#endif
#ifndef DLG_218
#define DLG_218 218
#endif
#ifndef DLG_218_EDIT_3
#define DLG_218_EDIT_3 1
#endif
#ifndef DLG_218_EDIT_4
#define DLG_218_EDIT_4 2
#endif
#ifndef DLG_218_TEXT_5
#define DLG_218_TEXT_5 3
#endif
#ifndef DLG_218_TEXT_6
#define DLG_218_TEXT_6 4
#endif
#ifndef DLG_218_TEXT_7
#define DLG_218_TEXT_7 5
#endif
#ifndef DLG_218_EDIT_8
#define DLG_218_EDIT_8 6
#endif
#ifndef DLG_218_PUSHBUTTON_9
#define DLG_218_PUSHBUTTON_9 7
#endif
#ifndef DLG_218_PUSHBUTTON_10
#define DLG_218_PUSHBUTTON_10 8
#endif
#ifndef DLG_219
#define DLG_219 219
#endif
#ifndef DLG_219_PUSHBUTTON_3
#define DLG_219_PUSHBUTTON_3 1
#endif
#ifndef DLG_219_EDIT_1
#define DLG_219_EDIT_1 2
#endif
#ifndef DLG_219_TEXT_2
#define DLG_219_TEXT_2 3
#endif
#ifndef DLG_219_PUSHBUTTON_4
#define DLG_219_PUSHBUTTON_4 4
#endif
#ifndef DLG_220
#define DLG_220 220
#endif
#ifndef DLG_220_LBOX_1
#define DLG_220_LBOX_1 1
#endif
#ifndef DLG_220_TEXT_2
#define DLG_220_TEXT_2 2
#endif
#ifndef DLG_220_PUSHBUTTON_3
#define DLG_220_PUSHBUTTON_3 3
#endif
#ifndef DLG_220_PUSHBUTTON_4
#define DLG_220_PUSHBUTTON_4 4
#endif
#ifndef DLG_222
#define DLG_222 222
#endif
#ifndef DLG_222_EDIT_1
#define DLG_222_EDIT_1 1
#endif
#ifndef DLG_222_TEXT_2
#define DLG_222_TEXT_2 2
#endif
#ifndef DLG_222_PUSHBUTTON_3
#define DLG_222_PUSHBUTTON_3 3
#endif
#ifndef DLG_222_PUSHBUTTON_4
#define DLG_222_PUSHBUTTON_4 4
#endif

#ifndef DLG_337
#define DLG_337 337
#endif
#ifndef DLG_337_EDIT_1
#define DLG_337_EDIT_1 1
#endif
#ifndef DLG_337_EDIT_2
#define DLG_337_EDIT_2 2
#endif
#ifndef DLG_337_EDIT_3
#define DLG_337_EDIT_3 3
#endif
#ifndef DLG_337_RADIOBUTTON_1
#define DLG_337_RADIOBUTTON_1 4
#endif
#ifndef DLG_337_RADIOBUTTON_2
#define DLG_337_RADIOBUTTON_2 5
#endif
#ifndef DLG_337_PUSHBUTTON_9
#define DLG_337_PUSHBUTTON_9 9
#endif
#ifndef DLG_337_PUSHBUTTON_10
#define DLG_337_PUSHBUTTON_10 10
#endif
#ifndef DLG_337_TEXT_11
#define DLG_337_TEXT_11 11
#endif
#ifndef DLG_337_TEXT_12
#define DLG_337_TEXT_12 12
#endif
#ifndef DLG_337_TEXT_13
#define DLG_337_TEXT_13 13
#endif
#ifndef DLG_337_TEXT_14
#define DLG_337_TEXT_14 14
#endif
#ifndef DLG_337_TEXT_15
#define DLG_337_TEXT_15 15
#endif
#ifndef DLG_337_TEXT_16
#define DLG_337_TEXT_16 16
#endif
#ifndef DLG_337_TEXT_17
#define DLG_337_TEXT_17 17
#endif
#ifndef DLG_337_TEXT_18
#define DLG_337_TEXT_18 18
#endif
#ifndef DLG_337_GROUPBOX_19
#define DLG_337_GROUPBOX_19 19
#endif
#ifndef DLG_337_GROUPBOX_20
#define DLG_337_GROUPBOX_20 20
#endif


#ifndef WIN_ERF_RAND_114
#define WIN_ERF_RAND_114 114
#endif
#ifndef WIN_114_EDIT_1
#define WIN_114_EDIT_1 1
#endif
#ifndef WIN_114_EDIT_2
#define WIN_114_EDIT_2 2
#endif
#ifndef WIN_114_EDIT_3
#define WIN_114_EDIT_3 3
#endif
#ifndef WIN_114_EDIT_4
#define WIN_114_EDIT_4 4
#endif
#ifndef WIN_114_EDIT_5
#define WIN_114_EDIT_5 5
#endif
#ifndef WIN_114_PUSHBUTTON_13
#define WIN_114_PUSHBUTTON_13 6
#endif
#ifndef WIN_114_PUSHBUTTON_14
#define WIN_114_PUSHBUTTON_14 7
#endif
#ifndef WIN_ERF_RAND_114_EDIT_15
#define WIN_ERF_RAND_114_EDIT_15 8
#endif
#ifndef WIN_ERF_RAND_114_EDIT_16
#define WIN_ERF_RAND_114_EDIT_16 9
#endif
#ifndef WIN_ERF_RAND_114_TEXT_17
#define WIN_ERF_RAND_114_TEXT_17 10
#endif
#ifndef WIN_ERF_RAND_114_EDIT_18
#define WIN_ERF_RAND_114_EDIT_18 11
#endif
#ifndef WIN_ERF_RAND_114_EDIT_19
#define WIN_ERF_RAND_114_EDIT_19 12
#endif
#ifndef WIN_ERF_RAND_114_EDIT_20
#define WIN_ERF_RAND_114_EDIT_20 13
#endif
#ifndef WIN_ERF_RAND_114_TEXT_21
#define WIN_ERF_RAND_114_TEXT_21 14
#endif
#ifndef WIN_ERF_RAND_114_TEXT_22
#define WIN_ERF_RAND_114_TEXT_22 15
#endif
#ifndef WIN_ERF_RAND_114_TEXT_23
#define WIN_ERF_RAND_114_TEXT_23 16
#endif
#ifndef WIN_ERF_RAND_114_EDIT_24
#define WIN_ERF_RAND_114_EDIT_24 17
#endif
#ifndef WIN_ERF_RAND_114_EDIT_25
#define WIN_ERF_RAND_114_EDIT_25 18
#endif
#ifndef WIN_ERF_RAND_114_EDIT_26
#define WIN_ERF_RAND_114_EDIT_26 19
#endif
#ifndef WIN_ERF_RAND_114_EDIT_27
#define WIN_ERF_RAND_114_EDIT_27 20
#endif
#ifndef WIN_ERF_RAND_114_EDIT_28
#define WIN_ERF_RAND_114_EDIT_28 21
#endif
#ifndef WIN_ERF_RAND_114_EDIT_29
#define WIN_ERF_RAND_114_EDIT_29 22
#endif
#ifndef WIN_ERF_RAND_114_EDIT_30
#define WIN_ERF_RAND_114_EDIT_30 23
#endif
#ifndef WIN_ERF_RAND_114_EDIT_31
#define WIN_ERF_RAND_114_EDIT_31 24
#endif
#ifndef WIN_ERF_RAND_114_EDIT_32
#define WIN_ERF_RAND_114_EDIT_32 25
#endif
#ifndef WIN_ERF_RAND_114_EDIT_33
#define WIN_ERF_RAND_114_EDIT_33 26
#endif
#ifndef WIN_ERF_RAND_114_EDIT_34
#define WIN_ERF_RAND_114_EDIT_34 27
#endif
#ifndef WIN_ERF_RAND_114_EDIT_35
#define WIN_ERF_RAND_114_EDIT_35 28
#endif
#ifndef WIN_ERF_RAND_114_EDIT_36
#define WIN_ERF_RAND_114_EDIT_36 29
#endif
#ifndef WIN_ERF_RAND_114_EDIT_37
#define WIN_ERF_RAND_114_EDIT_37 30
#endif
#ifndef WIN_ERF_RAND_114_EDIT_38
#define WIN_ERF_RAND_114_EDIT_38 31
#endif
#ifndef WIN_ERF_RAND_114_EDIT_39
#define WIN_ERF_RAND_114_EDIT_39 32
#endif
#ifndef WIN_ERF_RAND_114_EDIT_40
#define WIN_ERF_RAND_114_EDIT_40 33
#endif
#ifndef WIN_ERF_RAND_114_EDIT_41
#define WIN_ERF_RAND_114_EDIT_41 34
#endif
#ifndef WIN_ERF_RAND_114_EDIT_42
#define WIN_ERF_RAND_114_EDIT_42 35
#endif
#ifndef WIN_ERF_RAND_114_EDIT_43
#define WIN_ERF_RAND_114_EDIT_43 36
#endif
#ifndef WIN_ERF_RAND_114_EDIT_44
#define WIN_ERF_RAND_114_EDIT_44 37
#endif
#ifndef WIN_ERF_RAND_114_EDIT_45
#define WIN_ERF_RAND_114_EDIT_45 38
#endif
#ifndef WIN_ERF_RAND_114_EDIT_46
#define WIN_ERF_RAND_114_EDIT_46 39
#endif
#ifndef WIN_ERF_RAND_114_EDIT_47
#define WIN_ERF_RAND_114_EDIT_47 40
#endif
#ifndef WIN_ERF_RAND_114_EDIT_48
#define WIN_ERF_RAND_114_EDIT_48 41
#endif
#ifndef WIN_ERF_RAND_114_EDIT_49
#define WIN_ERF_RAND_114_EDIT_49 42
#endif
#ifndef WIN_ERF_RAND_114_EDIT_50
#define WIN_ERF_RAND_114_EDIT_50 43
#endif
#ifndef WIN_ERF_RAND_114_VSCROLL_51
#define WIN_ERF_RAND_114_VSCROLL_51 44
#endif

#define WIN_GRAPH_116 116
#define WIN_GRAPH_116_EDIT_1 1
#define WIN_GRAPH_116_EDIT_2 2
#define WIN_GRAPH_116_EDIT_3 3
#define WIN_GRAPH_116_EDIT_4 4
#define WIN_GRAPH_116_EDIT_5 5
#define WIN_GRAPH_116_EDIT_6 6
#define WIN_GRAPH_116_EDIT_7 7
#define WIN_GRAPH_116_EDIT_8 8
#define WIN_GRAPH_116_EDIT_9 9
#define WIN_GRAPH_116_PUSHBUTTON_26 10
#define WIN_GRAPH_116_PUSHBUTTON_27 11
#define WIN_GRAPH_116_LISTBUTTON_33 12
#define WIN_GRAPH_116_CHECKBOX_34 13   //ausgewählte Datensätze
#define WIN_GRAPH_116_TEXT_35 14
#define WIN_GRAPH_116_TEXT_36 15
#define WIN_GRAPH_116_TEXT_37 16
#define WIN_GRAPH_116_EDIT_38 17
#define WIN_GRAPH_116_EDIT_39 18
#define WIN_GRAPH_116_EDIT_40 19
#define WIN_GRAPH_116_EDIT_41 20
#define WIN_GRAPH_116_PUSHBUTTON_42 21
#define WIN_GRAPH_116_PUSHBUTTON_43 22
#define WIN_GRAPH_116_PUSHBUTTON_44 23
#define WIN_GRAPH_116_PUSHBUTTON_45 24
#define WIN_GRAPH_116_PUSHBUTTON_46 25
#define WIN_GRAPH_116_PUSHBUTTON_47 26
#define WIN_GRAPH_116_PUSHBUTTON_48 27
#define WIN_GRAPH_116_PUSHBUTTON_49 28
#define WIN_GRAPH_116_CHECKBOX_50 29
#define WIN_GRAPH_116_CHECKBOX_51 30
#define WIN_GRAPH_116_CHECKBOX_52 31
#define WIN_GRAPH_116_EDIT_53 32
#define WIN_GRAPH_116_TEDIT_54 33
#define WIN_GRAPH_116_TEXT_55 34
#define WIN_GRAPH_116_CHECKBOX_35 35 //Interpolation
#define WIN_GRAPH_116_CHECKBOX_36 36 //alle Datensätze
#define WIN_GRAPH_116_PUSHBUTTON_37 37 //Info-Button
#define WIN_GRAPH_116_CHECKBOX_38 38 //Spiegelbild
#define WIN_GRAPH_116_PUSHBUTTON_39 39 //Gauss-Krüger-Interpolation

#define WIN_scroll_116 2050    //****  Scrollbalken  WIN_116****

#define WIN116_EDIT0 2000     // X-Werte =1. Spalte
#define WIN116_EDIT1 2001
#define WIN116_EDIT2 2002
#define WIN116_EDIT3 2003
#define WIN116_EDIT4 2004

#define WIN116_EDIT5 2005     // y-Werte =2. Spalte
#define WIN116_EDIT6 2006
#define WIN116_EDIT7 2007
#define WIN116_EDIT8 2008
#define WIN116_EDIT9 2009

#define WIN116_EDIT10 2010     // Z-Werte =3. Spalte
#define WIN116_EDIT11 2011
#define WIN116_EDIT12 2012
#define WIN116_EDIT13 2013
#define WIN116_EDIT14 2014

#define WIN116_EDIT_TYP 2015   //Static-Text:Datenblock-Typ

#define WIN_116_LISTBOX_NEU 3049
#define WIN_116_LISTBOX_DEL 3051
#define WIN116_ZOOM_SCROLL 3052


#define WIN_GRAFIK_117 117

#define WIN_120 120
#define WIN_120_EDIT_1 1
#define WIN_120_EDIT_2 2
#define WIN_120_EDIT_3 3
#define WIN_120_EDIT_4 4
#define WIN_120_EDIT_5 5
#define WIN_120_PUSHBUTTON_26 6
#define WIN_120_PUSHBUTTON_27 7
#define WIN_120_LISTBUTTON_33 8
#define WIN_120_PUSHBUTTON_34 9
#define WIN_120_PUSHBUTTON_35 10
#define WIN_120_TEXT_18 11
#define WIN_120_TEXT_19 12
#define WIN_120_TEXT_20 13
#define WIN_120_EDIT_21 14
#define WIN_120_EDIT_22 15
#define WIN_120_EDIT_23 16
#define WIN_120_EDIT_24 17
#define WIN_120_EDIT_25 18
#define WIN_120_EDIT_26 19
#define WIN_120_EDIT_27 20
#define WIN_120_EDIT_28 21
#define WIN_120_PUSHBUTTON_44 22
#define WIN_120_PUSHBUTTON_45 23
#define WIN_120_PUSHBUTTON_46 24
#define WIN_120_PUSHBUTTON_47 25
#define WIN_120_PUSHBUTTON_48 26
#define WIN_120_TEDIT_55 27
#define WIN_120_TEXT_56 28
#define WIN_120_PUSHBUTTON_49 29 //Info-Button
#define WIN_120_PUSHBUTTON_30 30 //Interpolation

#define WIN120_EDIT0 3000
#define WIN120_EDIT1 3001
#define WIN120_EDIT2 3002
#define WIN120_EDIT3 3003
#define WIN120_EDIT4 3004
#define WIN120_EDIT5 3005
#define WIN120_EDIT6 3006
#define WIN120_EDIT7 3007
#define WIN120_EDIT8 3008
#define WIN120_EDIT9 3009
#define WIN120_EDIT10 3010
#define WIN120_EDIT11 3011
#define WIN120_EDIT12 3012
#define WIN120_EDIT13 3013
#define WIN120_EDIT14 3014
#define WIN120_EDIT15 3015
#define WIN120_EDIT16 3016
#define WIN120_EDIT17 3017
#define WIN120_EDIT18 3018
#define WIN120_EDIT19 3019
#define WIN120_EDIT20 3020
#define WIN120_EDIT21 3021
#define WIN120_EDIT22 3022
#define WIN120_EDIT23 3023
#define WIN120_EDIT24 3024
#define WIN120_EDIT25 3025
#define WIN120_EDIT26 3026
#define WIN120_EDIT27 3027
#define WIN120_EDIT28 3028
#define WIN120_EDIT29 3029
#define WIN120_EDIT30 3030
#define WIN120_EDIT31 3031
#define WIN120_EDIT32 3032
#define WIN120_EDIT33 3033
#define WIN120_EDIT34 3034
#define WIN120_EDIT35 3035
#define WIN120_EDIT36 3036
#define WIN120_EDIT37 3037
#define WIN120_EDIT38 3038
#define WIN120_EDIT39 3039
#define WIN120_EDIT40 3040
#define WIN120_EDIT41 3041
#define WIN120_EDIT42 3042
#define WIN120_EDIT43 3043
#define WIN120_EDIT44 3044

//****  EDIT-Felder für Schlüsseldaten  ****
#define WIN120_EDIT45 3045
#define WIN120_EDIT46 3046
#define WIN120_EDIT47 3047
#define WIN120_EDIT48 3048
#define WIN120_EDIT49 3052

#define WIN_120_LISTBOX_NEU 3049
#define WIN120_SCROLL 3050    //****  Scrollbalken  ****
#define WIN_120_LISTBOX_DEL 3051
#define WIN120_EDIT_TYP 3060



#define WIN_121 121
#define WIN_121_TEXT_1 1
#define WIN_121_TEXT_2 2
#define WIN_121_TEXT_3 3
#define WIN_121_TEXT_4 4
#define WIN_121_TEXT_5 5
#define WIN_121_TEXT_6 6
#define WIN_121_TEXT_8 7
#define WIN_121_TEXT_9 8
#define WIN_121_EDIT_16 9
#define WIN_121_EDIT_17 10
#define WIN_121_EDIT_18 11
#define WIN_121_EDIT_19 12
#define WIN_121_EDIT_20 13
#define WIN_121_EDIT_21 14
#define WIN_121_EDIT_22 15
#define WIN_121_EDIT_23 16
#define WIN_121_EDIT_24 17
#define WIN_121_EDIT_25 18
#define WIN_121_EDIT_27 19
#define WIN_121_PUSHBUTTON_38 20
#define WIN_121_PUSHBUTTON_39 21
#define WIN_121_PUSHBUTTON_40 22

#define QWERT_EDIT 122
#define WIN_122_EDIT_1 1
#define WIN_122_EDIT_2 2
#define WIN_122_EDIT_3 3
#define WIN_122_EDIT_4 4
#define WIN_122_EDIT_5 5
#define WIN_122_EDIT_6 6
#define WIN_122_EDIT_7 7
#define WIN_122_EDIT_8 8
#define WIN_122_EDIT_9 9
#define WIN_122_EDIT_10 10
#define WIN_122_EDIT_11 11
#define WIN_122_TEXT_13 12
#define WIN_122_TEXT_14 13
#define WIN_122_TEXT_15 14
#define WIN_122_TEXT_16 15
#define WIN_122_VSCROLL_17 16
#define WIN_122_PUSHBUTTON_18 17
#define WIN_122_PUSHBUTTON_19 18
#define QWERT_EDIT_LISTBUTTON_20 19
#define WIN_122_EDIT_21 20
#define WIN_122_EDIT_22 21
#define WIN_122_EDIT_23 22
#define WIN_122_EDIT_24 23
#define WIN_122_EDIT_25 24
#define QWERT_EDIT_TEXT_26 25

#define WIN_130 130
#define WIN_130_TEXT_3 1
#define WIN_130_TEXT_4 2
#define WIN_130_TEXT_5 3
#define WIN_130_TEXT_6 4
#define WIN_130_TEXT_7 5
#define WIN_130_TEXT_8 6
#define WIN_130_TEXT_9 7
#define WIN_130_TEXT_10 8
#define WIN_130_TEXT_11 9
#define WIN_130_PUSHBUTTON_21 10
#define WIN_130_PUSHBUTTON_22 11
#define WIN_130_DATENSATZ 12
#define WIN_130_STATION 13
#define WIN_130_HLINE 14
#define WIN_130_HWERT 15
#define WIN_130_SHOW 16
#define WIN_130_SFELD 17
#define WIN_130_SLINE 18
#define WIN_130_TEXT 19
#define WIN_130_TEXTSIZE 21

#define LINES 131

#define WIN_LAYER 133
#define WIN_LAYER_LBOX_1 1
#define WIN_LAYER_TEXT_2 2
#define WIN_LAYER_TEXT_3 3
#define WIN_LAYER_TEXT_5 4
#define WIN_LAYER_PB_CANCEL 5
#define WIN_LAYER_PB_OK 6
#define WIN_LAYER_TEXT_8 7

#define DLG_ARMCO                       117
#define DLG_ARMCO_ARMCOLIST             1067
#define DLG_ARMCO_R1                    1068
#define DLG_ARMCO_R2                    1069
#define DLG_ARMCO_W1                    1070
#define DLG_ARMCO_W2                    1071
#define DLG_ARMCO_W3                    1072
#define DLG_ARMCO_OK                    1073


/*END OF FILE WSPWIN.H*/
