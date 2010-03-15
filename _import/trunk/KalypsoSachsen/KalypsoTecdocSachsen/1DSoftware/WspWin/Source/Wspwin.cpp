/* WspWin.cpp */

#include <windows.h>
#include "xvt.h"
#include <fcntl.h>

#include "resource.h"
#include "wsphilfe.h"

#include "global_vars.h"
#include "global_types.h"

#include "..\..\wspdlg\Include\export.h"
#include "typen.h"

#include "laengs1.h"
#include "util2.h"
#include "wspdde.h"
#include "bce_allg.h"
#include "wsplist.h"
#include "read_cfg.h"
#include "rauh.h"
#include "wsplp2qp.h"
#include "strang.h"
#include "readprof.h"
#include "profpro2.h"
#include "verluste.h"
#include "lese_str.h"

#include "wspwin.h"
#include <string>

/*WINDOWS95 Color Changing*/
#define WIN_COLOR_GET_BLUE(color)   ((unsigned char)(((color) >> 16) & 0xFF))
#define WIN_COLOR_GET_GREEN(color) ((unsigned char)(((color) >> 8) & 0xFF))
#define WIN_COLOR_GET_RED(color)  ((unsigned char)((color) & 0xFF))

DWORD ConvertColorWin2XVT(DWORD color_win)
{
/*Windows Farbe nach XVT-Farbe konvertieren:
Windows-Definition:   BLUE-GREEN-RED
XVT-Definition:       RED-GREEN-BLUE
  */
  return XVT_MAKE_COLOR(WIN_COLOR_GET_RED(color_win),
    WIN_COLOR_GET_GREEN(color_win),
    WIN_COLOR_GET_BLUE(color_win));
};
/*Ende WINDOWS95 Color Changing*/

/*************************/

static long XVT_CALLCONV1 task_eh XVT_CALLCONV2 XVT_CC_ARGS((WINDOW xdWindow, EVENT *xdEvent));
extern long XVT_CALLCONV1 DLG_ABOUT_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent); // wspd102.cpp

// globale Variablen
DWORD exit_ausw;		// GHJ
WINDOW main_win;
HWND hwndWinDDE = NULL;
BYTE  WindowsPlatformId,MajorVersion;
char* start;
double sf;  // Fensterfaktor:Anpassen an verschiedene Auflösungen
WSP_PROFIL_LISTE* pWPL = NULL;
MENU_ITEM* main_menu_tree;

extern BOOLEAN berechnen, laengsschnitt;
extern HANDLE hProc_dag_bat;
extern BOOLEAN timermain;
extern BOOLEAN timermain2;
extern long timer_main;
extern long timer_main2;
extern int timer_zaehler;
extern BOOLEAN Save_Laengsschnitt;
extern st_daten steuerdaten;
extern char kon_start[100];

static UINT nProgressCancelMsg = ::RegisterWindowMessage("WspDlgProgressCancelMessage");
static UINT nMapperUpdateProjektMsg = ::RegisterWindowMessage("MapperUpdateProjektMsg");
static UINT nMapperUpdateProjektCompleteMsg = ::RegisterWindowMessage("MapperUpdateProjektCompleteMsg");
extern HANDLE hProc_Lwa2b;
extern HANDLE hProc_Wspr;
extern HANDLE hProc_Auswert;
static FARPROC defWndProc;
BOOL bBreakBerechnung;
double res_factor;

XVT_CONFIG xvt_config_2 = 
{
  TASK_MENUBAR,
    0,
    "wspwin",
    "wspbla",
    "WSPWIN - Titel",
};

int XVT_CALLCONV1 main XVT_CALLCONV2( int argc, char *argv[] )
{
	HMODULE hModule = GetModuleHandle( NULL );
	char exe_ver[256];
    GetFeatureVersion( exe_ver, hModule );
	BOOL bBfeLoaded = InitFeatureinfo(&exe_ver[0]);
	// falls bfe Datei nicht geladen werden konnte,
	//  oder DemoDate erreicht
	//	oder falsche Versionsnummer
	//		dann  Standard Demo-Einstellungen
	
	if( !bBfeLoaded )
	{	// hier Code eintragen für eventuelle Meldungen      kim
		// CharBuffer buffer( 1024 );
		// xvt_res_get_str( STR_NO_BFE_FILE, buffer, 1024 );
		// xvt_dm_post_fatal_exit( buffer ); // stürzt nach der Meldung leider ab !!???
	};
	
	// jetzt festlegen, welche Programmversion es ist
	setProjectType(GetFeature( "wsp_lwa_version" ));
	
  XVT_CONFIG xvt_config;
  xvt_config.menu_bar_ID = TASK_MENUBAR;
  xvt_config.base_appl_name = "wspwin";
  xvt_config.about_box_ID = 0;
  
  xvt_config.taskwin_title = new char[1000];
  xvt_res_get_str( STR_LOGO_WSPWIN, xvt_config.taskwin_title, 1000 ); //"WspWin-Steuerprogramm"
  xvt_config.appl_name = "WspWin";

  // Globale Variablen für die Titelsetzerei initialisieren
  start_dir = new char[250];
  Projektname_aktuell=new char[250];
  Gewaessername_aktuell=new char[20];
  Zustand_aktuell=new char[20];

  if( start_dir != NULL )
  {
    strcpy( start_dir,argv[0] );
    char* ptr = strrchr( start_dir, '\\' );
    ptr[1] = '\0';
    
    xvt_vobj_set_attr( NULL_WIN,ATTR_WIN_DRAWABLE_TWIN_BACKGRND,TRUE);
    xvt_app_create( argc, argv, 0L, task_eh, &xvt_config );
    return 0;
  }
  else
    return -1;
}


/*************   GHJ   *************/
// resizes dialogs and windows, including all of their child controls.
// sets font to MS Sans Serif 8, except in the case of FixedSys
void ChangeFontAndSize(HWND hWnd)
{
  LOGFONT logfont;
  HFONT hFont;
  RECT rect;
  int offset = 0;
  POINT pt1, pt2;
  HWND hParent = ::GetParent(hWnd);
  
  if (res_factor==1)
    return;			// do nothing
  
  // create MS Sans Serif 8 font
  logfont.lfHeight = -11;
  logfont.lfWidth = 0;
  logfont.lfEscapement = 0;
  logfont.lfOrientation = 0;
  logfont.lfWeight = FW_NORMAL;
  logfont.lfItalic = FALSE;
  logfont.lfUnderline = FALSE;
  logfont.lfStrikeOut = FALSE;
  logfont.lfCharSet = ANSI_CHARSET;
  logfont.lfOutPrecision = OUT_STRING_PRECIS;
  logfont.lfClipPrecision = CLIP_STROKE_PRECIS;
  logfont.lfQuality = DRAFT_QUALITY;
  logfont.lfPitchAndFamily = 34;
  strcpy(logfont.lfFaceName, "MS Sans Serif");
  
  hFont = ::CreateFontIndirect(&logfont);
  ::SendMessage(hWnd, WM_SETFONT, (WPARAM)hFont, MAKELPARAM(TRUE, 0));
  // get window rect (screen coords)
  ::GetWindowRect(hWnd, &rect);
  pt1.x = rect.left;
  pt1.y = rect.top;
  pt2.x = rect.right;
  pt2.y = rect.bottom;
  if (hParent!=NULL)
  {	// when parent exists, transform to client coords
    ::ScreenToClient(hParent, &pt1);
    ::ScreenToClient(hParent, &pt2);
  }
  // scale by res_factor
  pt1.x = (int)(pt1.x*res_factor);
  pt2.x = (int)(pt2.x*res_factor);
  pt1.y = (int)(pt1.y*res_factor);
  pt2.y = (int)(pt2.y*res_factor);
  // resize window
  ::MoveWindow(hWnd, pt1.x, pt1.y, pt2.x-pt1.x, pt2.y-pt1.y, TRUE);
  // walk through HWNDs
  for (HWND hWndChild = ::GetTopWindow(hWnd); hWndChild != NULL;
  hWndChild = ::GetNextWindow(hWndChild, GW_HWNDNEXT))
  {
    // send message with Windows SendMessage API
    hFont = (HFONT)::SendMessage(hWndChild, WM_GETFONT, 0, 0);
    if (hFont==::GetStockObject(SYSTEM_FIXED_FONT))
    {	// don't use MS Sans Serif
      logfont.lfHeight = -13;
      logfont.lfWeight = FW_BOLD;
    }
    else
    {
      logfont.lfHeight = -11;
      logfont.lfWeight = FW_NORMAL;
    }
    hFont = ::CreateFontIndirect(&logfont);
    ::SendMessage(hWndChild, WM_SETFONT, (WPARAM)hFont, MAKELPARAM(TRUE, 0));
    // get window rect (screen coords)
    ::GetWindowRect(hWndChild, &rect);
    pt1.x = rect.left;
    pt1.y = rect.top;
    pt2.x = rect.right;
    pt2.y = rect.bottom;
    // transform to client coords
    ::ScreenToClient(hWnd, &pt1);
    ::ScreenToClient(hWnd, &pt2);
    // scale by res_factor
    pt1.x = (int)(pt1.x*res_factor);
    pt2.x = (int)(pt2.x*res_factor);
    pt1.y = (int)(pt1.y*res_factor);
    pt2.y = (int)(pt2.y*res_factor);
    // resize window
    ::MoveWindow(hWndChild, pt1.x, pt1.y, pt2.x-pt1.x, pt2.y-pt1.y, TRUE);
  }
}

LRESULT CALLBACK MainWindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  LRESULT result;
  if (uMsg==nProgressCancelMsg)
  {
    bBreakBerechnung = TRUE;
    exit_ausw = -1;
    if (hProc_Wspr!=NULL)
    {
      TerminateProcess(hProc_Wspr, -1);
      hProc_Wspr = NULL;
    }
    if (hProc_Lwa2b!=NULL)
    {
      TerminateProcess(hProc_Lwa2b, -1);
      hProc_Wspr = NULL;
    }
    if (hProc_Auswert!=NULL)
    {
      TerminateProcess(hProc_Auswert, -1);
      hProc_Wspr = NULL;
    }
  }
  else if (uMsg==WM_ACTIVATEAPP && wParam)
  {
    if (::IsIconic(hwnd))    
      BringWindowToTop(hwnd);     // Für den Fall das Dialog mit parent focus hat
  }
  if (uMsg==nMapperUpdateProjektMsg)
  {
    if(WIN_116)
      xvt_vobj_destroy(WIN_116);
    if(WIN120)
      xvt_vobj_destroy(WIN120);
    SendMessage((HWND)wParam,nMapperUpdateProjektCompleteMsg,(WPARAM)(HWND)xvt_vobj_get_attr(main_win,ATTR_NATIVE_WINDOW),lParam);
  }
  if(uMsg== WM_DDE_INITIATE)
  {
    result=CallWindowProc((WNDPROC)DDEWndProc, hwnd, uMsg, wParam, lParam);
    return result;
  }
  
  
  result=CallWindowProc((WNDPROC)defWndProc, hwnd, uMsg, wParam, lParam);
  return result;
}
/***********************************/


static long XVT_CALLCONV1 task_eh XVT_CALLCONV2( WINDOW xdWindow, EVENT* xdEvent )
{
  //APPLICATION:Var_Decl******************************************
  
  switch (xdEvent->type) 
  {
  case E_CREATE:
    
    /*************   GHJ   *************/
    defWndProc = (FARPROC)GetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC);
    SetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC, (LONG)&MainWindowProc);
    /***********************************/
    /*	  only enable menu items on the task menubar if there is a task window with a menubar  */
#if(XVTWS == WINWS || XVTWS == PMWS)
			 if (!xvt_vobj_get_attr( NULL_WIN, ATTR_WIN_PM_NO_TWIN ))
       {
#endif
#if( XVTWS == WINWS || XVTWS == PMWS)
       }
#endif
       {
         /******  BEGIN Miet-Version *****************************************/
          /*
         _tzset(); // lokale Zeit setzen
         
         // Das Ende der Mietzeit festlegen
         
         // 10.10.2002
         struct tm mietEndeZeit = { 0, 0, 0, 15, 3, 103 }; // sec, min, h, mday,  Monat[0,..,11], Jahr seit 1900
         time_t mietEndeTime = mktime( &mietEndeZeit );

         // aktuelle Zeit holen
         time_t ltime;
         time( &ltime );

         // die beiden vergleichen
         if( mietEndeTime == (time_t)-1 )
         {
           xvt_dm_post_error( "%s", "Mietendzeit konnte nicht ermittelt werden.\n" );
           exit( 1 );
         }
         else if( ltime > mietEndeTime )
         {
           char buf[200];//Dick 26.11.99
           xvt_res_get_str( STR_MIETVERSION_NOTE, buf, sizeof(buf) ); //xvt_dm_post_error("Mietversion nicht mehr gültig ");
           xvt_dm_post_note( "%s", buf );
           exit( 1 );
         }
         */
         /******END Miet*******************************************************/
         
         WspwinMainBackgroundColor = ConvertColorWin2XVT(GetSysColor(COLOR_3DLIGHT));
         
         
         main_win = xdWindow;
         Menu_Win = xdWindow;
         
         /*****  globale Variablen assoziieren ******/
         WSP_START_VERZEICHNIS  = new char[151];
         start                  = new char[100];
         
         if ((WSP_START_VERZEICHNIS==NULL)||(str_netz==NULL)||(start == NULL) )
         {
           char buf[200];//Dick 26.11.99
           xvt_res_get_str(STR_FATAL_EXIT_1,buf,sizeof(buf));
           xvt_dm_post_fatal_exit("%s",buf);
           //xvt_dm_post_fatal_exit("Variablen können nicht alloziiert werden!\nProgramm wird beendet.");
         }
         
         /*****  Ende globale Variablen alloziieren *****/
         
         /* Hauptfenster auf maximale Größe setzen*/
         ShowWindow( (HWND)xvt_vobj_get_attr( xdWindow, ATTR_NATIVE_WINDOW ), SW_SHOWMAXIMIZED );
         
         /**** Windows Version  ******/
         OSVERSIONINFO WinVersionInfo;
         WinVersionInfo.dwOSVersionInfoSize=sizeof(OSVERSIONINFO);
         GetVersionEx(&WinVersionInfo);
         
         MajorVersion = (BYTE) WinVersionInfo.dwMajorVersion;
         if (WinVersionInfo.dwPlatformId == VER_PLATFORM_WIN32s)
         {
           char buf[200];//Dick 26.11.99
           xvt_res_get_str(STR_FATAL_EXIT_2,buf,sizeof(buf));
           xvt_dm_post_fatal_exit("%s",buf);
           //xvt_dm_post_fatal_exit("Falsche Windows-Version.Aktuelle Version unterstützt nur Windows95/NT\nProgramm wird beendet.");		 
         }
         
         if (WinVersionInfo.dwPlatformId ==VER_PLATFORM_WIN32_NT)
         {
           if(WinVersionInfo.dwMajorVersion >=4 )
             WindowsPlatformId = (BYTE)WinVersionInfo.dwPlatformId;
           
           else 
             WindowsPlatformId =(BYTE) WinVersionInfo.dwPlatformId;
           //xvt_dm_post_fatal_exit("Falsche Windows-Version.Aktuelle Version unterstützt nur Windows95/NT3.xx\nProgramm wird beendet.");		 
         }
         if (WinVersionInfo.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS)
           WindowsPlatformId = (BYTE)WinVersionInfo.dwPlatformId;
         
         /* Fensterskalierungsfaktor berechnen */
         
         if (WindowsPlatformId == VER_PLATFORM_WIN32_WINDOWS)
           sf = xvt_vobj_get_attr(NULL_WIN,ATTR_CTL_STATIC_TEXT_HEIGHT)/18.0;  // WIN95
         else if (WindowsPlatformId == VER_PLATFORM_WIN32_NT)
         {
           if(WinVersionInfo.dwMajorVersion >=4 )
		         {
         		  sf = xvt_vobj_get_attr(NULL_WIN,ATTR_CTL_STATIC_TEXT_HEIGHT)/18.0;  // WIN NT 4.0
		         }
           else
             sf = xvt_vobj_get_attr(NULL_WIN,ATTR_CTL_STATIC_TEXT_HEIGHT)/23.0;//WIN3.11-WIN NT
         }
           
           
         /******************   Listen instanziieren    *****************/
         //WSP_PROFIL_LISTE *pWPL=NULL;
         
         if (pWPL=Init_Profil_Liste(pWPL))
         {
           pWPL->PListNext=NULL;
           list = pWPL->PList;
           
           /* die folgende Zeile:"paint= new Paint" darf erst nach der
           Instanziierung von list erfolgen */
           
           paint=(Paint*)list;
         }
         else
         {
           char buf[200];//Dick 26.11.99
           xvt_res_get_str(STR_FATAL_EXIT_3,buf,sizeof(buf));
           xvt_dm_post_fatal_exit("%s",buf);
           //xvt_dm_post_fatal_exit("Listen können nicht angelegt werden!\nProgramm wird beendet.");
         }
         
         
         /****************** Ende  Listen instanziieren    *************/
         HWND hwndWin;
         hwndWin = (HWND)xvt_vobj_get_attr(main_win,ATTR_NATIVE_WINDOW);
         if((hwndWinDDE=CreateDDEWnd((HINSTANCE)GetWindowLong(hwndWin,GWL_HINSTANCE), hwndWin))==NULL)
         {
           char buf[200];//Dick 26.11.99
           xvt_res_get_str(STR_FATAL_EXIT_4,buf,sizeof(buf));
           xvt_dm_post_fatal_exit("%s",buf);
           //xvt_dm_post_fatal_exit("MapObjekt-Kommunikation kann nicht angelegt werden!\nProgramm wird beendet.");
         }
         
         xvt_menu_set_item_enabled(xdWindow, TASK_MENUBAR_8, TRUE);//Hilfe
         xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_43_44,FALSE);  //Projekt speichern unter
         xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_6,FALSE);  //Fremddaten
         xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_30_29,FALSE);  //Ergebnisse(H)
         xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_30_29_58,FALSE);  //DXF(H)
         
         xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_2_3, TRUE); //Massenberechnung
         xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_43_6_77,TRUE);  //WSPWIN-Jabron
         xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_43_6_95,TRUE);  //Jabron-WSPWIN
         
         
         xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_13_59, TRUE ); //Interpolation
         
         xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_43_6_61,TRUE);//DA66
         
         xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_13_67,TRUE );//Plotten:DXF
         
         HMENU menubar,menu_delete,menu_insert;

           menubar=GetMenu((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW));
           menu_delete=GetSubMenu(menubar,1);
           menu_delete=GetSubMenu(menubar,2);
           menu_delete=GetSubMenu(menubar,3);

		   char buf[200];
		   xvt_res_get_str(STR_TEILGEBIETE,buf,sizeof(buf));
           menu_insert=GetSubMenu(menubar,1);
           InsertMenu( menu_insert,8,MF_BYPOSITION|MF_STRING,TASK_MENUBAR_14_18,buf); 
         
         menubar=GetMenu((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW));           
         xvt_res_get_str(STR_KOPFTXT_MENUE_NAME,buf,sizeof(buf));
         menu_insert=GetSubMenu(menubar,5);
         
         AppendMenu(menu_insert,MF_STRING,TASK_MENUBAR_14_28,buf);
         
         menu_insert=GetSubMenu(menubar,0);
         InsertMenu( menu_insert,1077,MF_BYCOMMAND|MF_STRING,TASK_MENUBAR_43_6_26,"WASPILA"); 
         
         menu_insert = GetSubMenu( menubar, 5 ); // Menu 'Extras'
         AppendMenu( menu_insert, MF_STRING, TASK_MENUBAR_WSPWIN_MAPPER, "WspWin Mapper" );
         
         // Menu Fremddaten->Acces hinzufügen
         menu_insert = GetSubMenu( menubar, 0 ); // Menu 'Projekt'
         menu_insert = GetSubMenu( menu_insert, 6 );// Submenu 'Fremddaten'
         AppendMenu( menu_insert, MF_SEPARATOR, 0, NULL );
         AppendMenu( menu_insert, MF_STRING, TASK_MENUBAR_ACCESS, TEXT("&ACCESS") );
         
         main_menu_tree=xvt_menu_get_tree(xdWindow);
         xvt_menu_set_tree(xdWindow,main_menu_tree);
         main_win = xdWindow;
         Menu_Win = xdWindow;
           
         // GHJ
         // set res factor from screen mode
         res_factor = 1;	
         UINT smode = GetPrivateProfileInt("WSPWIN","SCREENMODE",0,INI_FILE_NAME);
         switch (smode)
         {
         case MODEAUTO:
           {
             RECT rect;
             // look at current screen resolution
             ::SystemParametersInfo(SPI_GETWORKAREA, 0, &rect, 0);
             if (rect.right-rect.left <= 800)
               res_factor = 0.8;
           }
           break;
           
         case MODE640:
         case MODE800:
           res_factor = 0.8;
           break;
           
         default:
           break;
         }
         xvt_menu_update(xdWindow);

         disable_menu(xdWindow);     // -> BCE_DLL
         
         /****** Projekte aus wsp.prj lesen ***************/
         lese_projektdatei();       // ->File:read_cfg.cpp
         /* About Box aus DLL starten*/
		 if (!xvt_dlg_create_res(WD_MODAL, IDD_DLG_ABOUT, EM_ALL, DLG_ABOUT_eh, 0L))
         xvt_dm_post_error("Can't open dialog INFO-BOX");

         //Dick 10.03.99
         FILE_SPEC fs;
         memset(&fs,0,sizeof(FILE_SPEC));
         strcpy(fs.name,start_dir);
         strcat(fs.name,"wspwin.hlp");
         hi=xvt_help_open_helpfile(&fs,HSF_APPNAME_TITLE);
         //GHJ 10.05.99
         if (hi!=NULL_HELP_INFO)
         {
           // Menü Projekt
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_43_45, HID_KAPITEL_4_2_2, 0L);	// Projekte
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_43_44, HID_KAPITEL_6_1_1, 0L);	// Projekte speich. unter
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_43_46, HID_KAPITEL_6_1_2, 0L);	// Archivieren
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_43_46_124, HID_KAPITEL_6_1_2, 0L);	// archivieren
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_43_46_125, HID_KAPITEL_6_1_2, 0L);	// zurückladen
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_6, HID_KAPITEL_7_1_1, 0L);	// Fremddaten
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_6_15, HID_KAPITEL_7_1_1, 0L);	// HYDRAWSP
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_43_6_61, HID_KAPITEL_7_1_3, 0L);	// DA66
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_43_6_77, HID_KAPITEL_7_1_2, 0L);	// WSPWIN->JABRON
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_43_6_95, HID_KAPITEL_7_1_2, 0L);	// JABRON->WSPWIN
           // Zustand Menü
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_50_127, HID_KAPITEL_4_3, 0L);	// Zustände
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_13_39, HID_KAPITEL_6_2_3, 0L);	// editieren Zustandsdatei
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_50_128, HID_KAPITEL_6_2_2, 0L);	// Zustand speich. unter
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_13_42, HID_KAPITEL_7_2_1, 0L);	// Rauheiten ändern
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_14_16, HID_KAPITEL_4_5_1, 0L);	// Abflussdatei
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_14_17, HID_KAPITEL_4_5_2, 0L);	// Verlustdatei
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_13_60_62, HID_KAPITEL_7_2_3, 0L);	// Gelände neu
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_13_60_63, HID_KAPITEL_7_2_3, 0L);	// Gelände löschen
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_13_64_65, HID_KAPITEL_7_2_4, 0L);	// Fläche neu
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_13_64_66, HID_KAPITEL_7_2_4, 0L);	// Fläche löschen
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_13_59, HID_KAPITEL_7_2_2, 0L);	// Interpolieren
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_50_5, HID_KAPITEL_5_5_2, 0L);	// Netzverknüpfung
           // Berechnung Menü
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_30_7, HID_KAPITEL_4_6_1, 0L);	// Spiegellinienberechnung
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_2_3, HID_KAPITEL_7_4_1, 0L);	// Massenberechnung
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_30_84_88, HID_KAPITEL_10_6, 0L);	// Wehre
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_30_90, HID_KAPITEL_10_5, 0L);	// Rohre
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_2_85_89, HID_KAPITEL_10_4, 0L);	// Gerinne
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_30_90_92, HID_KAPITEL_10_10, 0L);	// Strassenbau
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_30_29_93, HID_KAPITEL_10_8, 0L);	// Entlastungsbauwerke
           // Ergebnisse Menü
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_30_12, HID_KAPITEL_4_7_1, 0L);	// Tabelle
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_30_24, HID_KAPITEL_6_6, 0L);	// Laengsschnitt einsehen
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_29_32_81, HID_KAPITEL_7_5_1_2, 0L);	// Vergleich
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_29_32_83_84, HID_KAPITEL_7_5_1_3, 0L);	// Voreinstellung
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_29_32_83_82, HID_KAPITEL_7_5_1_3, 0L);	// Prüfprogramm
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_30_80, HID_KAPITEL_7_5_1_1, 0L);	// Beispiellisten
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_29_31_33, HID_KAPITEL_7_5_2, 0L);	// Wasserspiegel - Einfügen
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_29_31_34, HID_KAPITEL_7_5_2, 0L);	// Wasserspiegel - Löschen
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_1_86, HID_KAPITEL_7_4_1, 0L);	// Ergebnisse der Massenberechnung
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_30_84_87, HID_KAPITEL_4_7_1, 0L);	// Editor
           // Plotten Menü
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_13_67, HID_KAPITEL_6_7_3, 0L);	// Plotprogramm
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_13_58_74, HID_KAPITEL_6_7_1, 0L);	// DXF - Querprofil
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_1_91, HID_KAPITEL_6_7_1, 0L);	// DXF - Längsschnitt
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_13_58_78, HID_KAPITEL_6_7_1_1_2, 0L);	// DXF - Antragstellerschild
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_30_75, HID_KAPITEL_6_7_1_3, 0L);	// DXF - Konfiguration
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_1, HID_KAPITEL_6_7_2, 0L);	// CAD - Programm
           
           // Extras Menu
           xvt_help_set_menu_assoc(hi, xdWindow, TASK_MENUBAR_14_27, HID_KAPITEL_7_6_2, 0L);	// Sonderprogramm
         }
    }
    break;
  case E_DESTROY:
    /*	Application is being terminated; last event sent to application.	*/
    {
      //Dick 10.03.99
      if(hi!=NULL_HELP_INFO)
        xvt_help_close_helpfile(hi);
      hi=NULL_HELP_INFO;
      //
      if(hwndWinDDE!=NULL)
        DestroyWindow(hwndWinDDE);
      /* Globale Variablen freigeben */
      delete[] start_dir;
      delete[] WSP_START_VERZEICHNIS;
      delete[] start;
      Delete_Profil_Liste(pWPL);
      if ((WindowsPlatformId == VER_PLATFORM_WIN32_NT)&&(MajorVersion==4))
        _exit(1); // Notausgang !!!!
    }
    break;
  case E_SIZE:
    break;
  case E_CLOSE:
    {
      xvt_app_destroy();
    }
    break;
  case E_COMMAND:
    {
      do_TASK_MENUBAR(xdWindow, xdEvent);
    }
    break;
  case E_TIMER:    /*  Timer associated with TASK_WIN went off.		*/
    {
      if( berechnen )
      {
        int test=0;
        
        if( !LWA_PROJEKT )
        {
          BOOL bSuccess = FALSE;            
          bSuccess=GetExitCodeProcess(hProc_dag_bat,&exit_ausw);
          if(exit_ausw==STILL_ACTIVE && bSuccess)
            test=0;
          else
            test = 1;
        }
        else
          test = 1;

		int strError;
		int strError2;
		if( LWA_PROJEKT )
		{
			strError = STR_BER_LWA_ERROR;
			strError2 = STR_BER_LWA_ERROR2;
		}
		else
		{
			strError = STR_BER_BCE_ERROR;
			strError2 = STR_BER_BCE_ERROR2;
		}

		BOOLEAN insertWSP = GetPrivateProfileInt( "WSPWIN", "WSP_EINTRAGEN", 1,  "WSPWIN.INI" );


        if(timermain2==FALSE)
        {
          if(test!=0)
          { 
            extern SLIST batch_list;
            timermain2=TRUE;
            char error[15];
            strcpy(error,"error2.log");
            if(exit_ausw==0) //Dick 21.04.99
              pruefe_errorlog(error); //in util2.cpp
            else  //Dick 21.04.99
            {
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_BER_ABBRUCH,buf,sizeof(buf));
              xvt_dm_post_note("%s",buf);
              //xvt_dm_post_note("Berechnung wurde abgebrochen.");
              Save_Laengsschnitt=FALSE;
              if (batch_list!=NULL)
              {
                xvt_slist_destroy(batch_list);
                batch_list=NULL;
              }
            }
            CURSOR cursor = xvt_win_get_cursor(xdWindow);
            xvt_win_set_cursor(xdWindow,CURSOR_WAIT);
            if(Save_Laengsschnitt==TRUE)
            {
              extern plot ploti;
              
              SLIST_ELT e;
              int len;
              char ausgabe[100];
              char str_dat[100];
              char str[100],str2[100],*bat,*batbuffer;
              batbuffer=new char[30];
              //Neu Dick 9.02.99 WSP löschen muß auserhalb 
              for (e=xvt_slist_get_first(batch_list);e !=NULL;e=xvt_slist_get_next(batch_list,e))
              {
                bat = xvt_slist_get(batch_list,e,0L);
                strcpy(batbuffer,bat);
                strcat(batbuffer,"\0");
                str[0]='\0';
                xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str,50);
                strcat(str,"\\");
                strcat(str,batbuffer);
                
                read_varianten(str);
                xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str2,50);
                strcpy(str_dat,str2);
                strcat(str_dat,"\\");
                strcat(str_dat,STR_SPEC.name);
                
                len=strlen(str2);
                str2[len-4]='\0';
                strcat(str2,"dath\\");
                strcpy(ausgabe,str2);
                if( LWA_PROJEKT )
                {
                  batbuffer[2]='l';
                  batbuffer[3]='p';
                }
                else
                {
                  batbuffer[2]='w';
                  batbuffer[3]='l';
                }; // if LWA_Versio
                
                strcat(str2,batbuffer);
                if((access(str2,00))==0) //LPLOTDATEI vorhanden
                {
                  int handle=open(str2,O_RDONLY);
                  int length=filelength(handle);
                  close(handle);
                  if((length!=0) && (length!=-1))
                  {
                    if( LWA_PROJEKT )
                    {
                      batbuffer[2]='p';
                      batbuffer[3]='l';
                    };
                    
                    strcat(ausgabe,batbuffer);				     				                				      
                    
                    int test;
                    if( LWA_PROJEKT )
                      test = ConvertLaengsprofilLwaToBce(str2,ausgabe,pWPL,str_dat,steuerdaten.info,steuerdaten.q,steuerdaten.wsf_l);
                    else
                      test = ConvertLaengsprofilBceToBce(ausgabe,pWPL,str_dat,steuerdaten.info,steuerdaten.q,steuerdaten.wsf_l);
        
                    strcpy(ploti.name,ausgabe);//Dick 6.08.98 Vorsichtshalber für den Fahl das ploti.name immer noch NULL-String ist                                      
                    {
                      DIRECTORY dirp;
                      char str3[100];
                      xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str3,50);
                      int len=strlen(str3);
                      str3[len-4]='\0';
                      strcat(str3,"dath");
                      xvt_fsys_convert_str_to_dir(str3,&dirp);                                            
                      strcpy(str3,batbuffer);

                      if( insertWSP )
					  {
						  // nur löschen, wenn Sie auch eingetragen werden sollen
						  DeleteAlleWspInQuerprof(pWPL,&dirp,str3,STATION);//Dick 13.01.99
						  
						  DeleteAlleWspInQuerprof(pWPL,&dirp,str3,STATION_FIX);//Dick 05.02.99
					  }
                    }
                  }
                }
              }
              //
              for (e=xvt_slist_get_first(batch_list);e !=NULL;e=xvt_slist_get_next(batch_list,e))
              {                            
                bat = xvt_slist_get(batch_list,e,0L);
                strcpy(batbuffer,bat);
                strcat(batbuffer,"\0");
                str[0]='\0';
                xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str,50);
                strcat(str,"\\");
                strcat(str,batbuffer);
                
                read_varianten(str);
                xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str2,50);
                strcpy(str_dat,str2);
                strcat(str_dat,"\\");
                strcat(str_dat,STR_SPEC.name);
                
                len=strlen(str2);
                str2[len-4]='\0';
                strcat(str2,"dath\\");
                strcpy(ausgabe,str2);
                
                if( LWA_PROJEKT )
                {
                  batbuffer[2]='l';
                  batbuffer[3]='p';
                }
                else
                {
                  batbuffer[2]='w';
                  batbuffer[3]='l';
                };
                
                strcat(str2,batbuffer);
                if((access(str2,00))==0) //LPLOTDATEI vorhanden
                {
                  int handle=open(str2,O_RDONLY);
                  int length=filelength(handle);
                  close(handle);
                  if((length!=0) && (length!=-1))
                  {
                    if( LWA_PROJEKT )
                    {
                      batbuffer[2]='p';
                      batbuffer[3]='l';
                    };
                    

                    strcat(ausgabe,batbuffer);
                    
                    int test;
                    if( LWA_PROJEKT )
                      test=ConvertLaengsprofilLwaToBce(str2,ausgabe,pWPL,str_dat,steuerdaten.info,steuerdaten.q,steuerdaten.wsf_l);
                    
                    strcpy(ploti.name,ausgabe);//Dick 6.08.98 Vorsichtshalber für den Fahl das ploti.name immer noch NULL-String ist
                    //Dick 12.01.99 Wasserspiegel einfügen
                    {
    				   DIRECTORY dirp;
                      char str3[100];
                      xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str3,50);
                      int len=strlen(str3);
                      str3[len-4]='\0';
                      strcat(str3,"dath");
                      xvt_fsys_convert_str_to_dir(str3,&dirp);                                            
                      strcpy(str3,batbuffer);

					  if( insertWSP )
						  InsertWspLpToQuerprof(pWPL,&dirp,str3,steuerdaten.info,STATION);

					  if(steuerdaten.wsf_q && insertWSP)
                        InsertWspLpToQuerprof(pWPL,&dirp,str3,steuerdaten.info,STATION_FIX);
                      if(steuerdaten.u_grenze)
                        WspToUGrenze(pWPL,&dirp,str3);
                      //Bley 9.11.2000
                      // Vernetzungsdatei einlesen

                      if( steuerdaten.hmo && LWA_PROJEKT ) //umfunktioniert zu abstand da nicht mehr gebraucht
                      {
                        if (strang_anfang != NULL)
                          destroy_strang_tabelle();
                        strang_anfang=NULL;
                        MakeNewStrang(STRANGANZAHL);
                        read_profil_dat(strang_anfang);
                        
                        
                        xvt_fsys_set_dir(&dirp);
                        strcpy(file_spec.name,str3); // obsolet?
                        laengsschnitt=TRUE;
//                        if(!read_profildatei( pWPL, &STR_SPEC.dir, str3 ) )
                        if(!read_profildatei( pWPL, &dirp, str3 ) )
                          Ersetze_Station_mit_Abstand_dll(pWPL,&dirp, str3, strang_anfang, anzahl_strang_entries);
                        
                        /*****6.3******/
                        if (strang_anfang != NULL)
                          destroy_strang_tabelle();
                        strang_anfang=NULL;
                        MakeNewStrang(STRANGANZAHL);
                        read_profil_dat(strang_anfang);
                        
                        /***********/
                        
                        save_profildatei(pWPL);	
                      }
                      
                    }
                    

                    /*xvt_dm_post_note("Berechnung erfolgreich ausgeführt. "
                    "Protokoll der Berechnung in Datei profil.log "
                    "im Projektunterverzeichnis \\dath.");*/
                    char buf[200];//Dick 26.11.99
                    xvt_res_get_str(STR_BER_SUCCESS,buf,sizeof(buf));
					if( LWA_PROJEKT )
						xvt_dm_post_note("%s%s", buf, "error2.log und profil.log");
					else
						xvt_dm_post_note("%s%s", buf, "error2.log");
                  } //if filelength !=0
                  else
                  {	
                    /*
                    xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str,50);
                    char buf[200],buf2[200];//Dick 26.11.99
                    xvt_res_get_str(STR_KEIN_LAENGS,buf,sizeof(buf));
                    xvt_res_get_str(STR_BEI_BER_ERSTELLT,buf2,sizeof(buf2));
                    xvt_dm_post_note("%s %s %s %s",buf,str,bat,buf2);
                    //xvt_dm_post_note("Sie haben keine Längsschnittdatei für %s %s bei der Berechnung erstellt.",str,bat);
                    */

                    /*xvt_dm_post_note("Berechnung fehlerhaft ausgeführt. \n"
                    " Sehen Sie die Dateien %s und profil.log "
                    " im Projektunterverzeichnis \\dath ein.",which_file);*/
                    char buf[200],buf2[200];//Dick 26.11.99
                    xvt_res_get_str(strError,buf,sizeof(buf));
                    xvt_res_get_str(strError2,buf2,sizeof(buf2));
                    xvt_dm_post_error("%s%s%s",buf, "error2.log", buf2 );
                  }
                } //lp-Datei da
                else
                {
                  /*
                  xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str,50);
                  char buf[200],buf2[200];//Dick 26.11.99
                  xvt_res_get_str(STR_KEIN_LAENGS,buf,sizeof(buf));
                  xvt_res_get_str(STR_BEI_BER_ERSTELLT,buf2,sizeof(buf2));
                  xvt_dm_post_note("%s %s %s %s",buf,str,bat,buf2);
                  //xvt_dm_post_note("Sie haben keine Längsschnittdatei für %s %s bei der Berechnung erstellt.",str,bat);				 
                  */

                  /*xvt_dm_post_note("Berechnung fehlerhaft ausgeführt. \n"
                  " Sehen Sie die Dateien %s und profil.log "
                  " im Projektunterverzeichnis \\dath ein.",which_file);*/
                  char buf[200],buf2[200];//Dick 26.11.99
                  xvt_res_get_str(strError,buf,sizeof(buf));
                  xvt_res_get_str(strError2,buf2,sizeof(buf2));
                  xvt_dm_post_error( "%s%s%s", buf, "error2.log", buf2 );
                }
                
                } //for
                if (batch_list!=NULL)
                {
                  xvt_slist_destroy(batch_list);
                  batch_list=NULL;
                }
                delete[] batbuffer;
                     }//if(Save_Laengsschnitt==TRUE)
                     xvt_win_set_cursor(xdWindow,cursor);
                     hProc_dag_bat=NULL;
                     xvt_timer_destroy(timer_main2);
                     timermain2=FALSE;
        }                
       }
      }
      else
      {
        timer_zaehler++;
        int test=access(kon_start,4);
        if( LWA_PROJEKT )
          test = 1;
        
        if (timermain==FALSE)
        {
          if (test!=0)
          {
            timermain=TRUE;
            
            if( LWA_PROJEKT )
            {
              if (exit_ausw==0)
              {
                Schreibe_Verlust_Datei_BCE(); //in verluste.cpp
                lese_str();
              }
              else
              {
                char buf[200];//Dick 26.11.99
                xvt_res_get_str(STR_KONVERT_ABBRUCH,buf,sizeof(buf));
                xvt_dm_post_note("%s",buf);                         
                //xvt_dm_post_note("Konvertierung wurde abgebrochen.");
              }
            }
            else
            {
              profile_aus_str_ermitteln(); //in schreib.cpp
              profile_nach_profproj_schreiben(); //in profpro2.cpp
              char error[15];
              strcpy(error,"error.log");
              pruefe_errorlog(error); //in util2.cpp
            };
            xvt_timer_destroy(timer_main);
            timermain=FALSE;
            if(timer_main2)//Wen er sich verirrt
              xvt_timer_destroy(timer_main2);
          }    
        }
        if(timer_zaehler==1200)
        {
          char buf[200];//Dick 26.11.99
          xvt_res_get_str(STR_KONVERT_ABBRUCH2,buf,sizeof(buf));
          xvt_dm_post_note("%s",buf);
          //xvt_dm_post_note("Konvertierung länger als 20 Minuten nicht vorgesehen");
          xvt_timer_destroy(timer_main);
          timermain=FALSE;
        }
        
      } //else !berechnen
    }
    break;
  case E_QUIT:
		/*	System shutdown. (Sent only on those platforms which
    provide this information to XVT.)		*/
    {
      if (xdEvent->v.query) 
      {
        xvt_app_allow_quit();
      }
      else
        xvt_app_destroy();
    }
    break;
  case E_USER:
    {
      switch (xdEvent->v.user.id) {
      case -1:
      default:
        break;
      }
    }
    break;
  default:
    break;
}
return 0L;
}

void xdCheckRadioButton( WINDOW xdWindow, int check, int start, int end )
{
  WINDOW *radiobtns;
  int num_btns = end - start + 1;
  int i;
  radiobtns = (WINDOW *)xvt_mem_alloc(num_btns * sizeof(WINDOW));
  for (i = start; i <= end; i++) {
    radiobtns[i-start] = xvt_win_get_ctl(xdWindow, i);
  }
  xvt_ctl_check_radio_button(xvt_win_get_ctl(xdWindow, check), radiobtns,
    num_btns);
  xvt_mem_free((char *)radiobtns);
}

void setProjectType( const BOOL bTypeLWA )
{
  LWA_PROJEKT = ( bTypeLWA == TRUE );
  if( Menu_Win != NULL_WIN )
  {
    // manche Menus sind nur für eine Version verfügbar
    xvt_menu_set_item_enabled( Menu_Win, TASK_MENUBAR_50_5, LWA_PROJEKT ); // Netzverknuepfung
	xvt_menu_set_item_enabled( Menu_Win, TASK_MENUBAR_14_18, !LWA_PROJEKT ); //Teilgebiete (für Kalinin_Miljukov)

    xvt_menu_set_item_enabled( Menu_Win, TASK_MENUBAR_43_6_26, LWA_PROJEKT ); // Fredmdaten WASPILA?

    xvt_menu_set_item_enabled( Menu_Win, TASK_MENUBAR_30_80, LWA_PROJEKT ); //Listenauswertung
    xvt_menu_set_item_enabled( Menu_Win, TASK_MENUBAR_29_32_81, LWA_PROJEKT ); //Auswertung:Vergleich
    xvt_menu_set_item_enabled( Menu_Win, TASK_MENUBAR_30_83, LWA_PROJEKT ); //Auswertung:Prüfung

  }

}

void setTitle( char* strProjektname, char* strGewaesser, char* strZustand )
{
  int resID = LWA_PROJEKT == true ? STR_LOGO_LWA : STR_LOGO_BCE;

  CharBuffer buffer( 1000 );
  xvt_res_get_str( resID, buffer, 1000 );

  CharBuffer strTitle( 1000 );

  strcpy( strTitle, buffer );
  strcat( strTitle, "   " );
  if( strProjektname )
    strcat( strTitle, strProjektname );
  strcat( strTitle, "   ");
  if( strGewaesser )
    strcat( strTitle, strGewaesser );
  strcat( strTitle, "   ");
  if( strZustand )
    strcat( strTitle, strZustand );
  
  xvt_win_set_doc_title( Menu_Win, strTitle );
}; // setTitle


/********      end of file: wspwin.cpp     ***********/
