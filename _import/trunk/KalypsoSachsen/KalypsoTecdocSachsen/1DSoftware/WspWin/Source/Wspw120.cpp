/****************************************************************************
*             WSPW120 .CPP                                                  *
*             08.12.1994                                                    *
* Dagmar eingefügt:OK:sort_new_profil(),Aufruf(Dlg136getdaten);             *                                                              *
*                     teste_projekt_profile(druck);                          *
*						 extern BOOLEAN vergleich,BOOLEAN druck                  *
*                  CREATE(new_profil):Aufruf teste_projekt_profile(druck);  *                                                     *
*                  include profproj.h                                      *
*                  xvt_scr_set_focus_vobj(win_120[ch_id]);umgesetzt bei     *
*							 create,new_profil ans Ende in Abh. v. vergleich       *
*							 wegen "keine zahl eingegeben"                         *
*                  void wandle_abstand_in_string(void) hinter sort new profil*
*                  extern char profil_nr_string[8];                          *
*                  Aufruf profil_nr_ermitteln() bei create Dateiname         *
*                  Fkt. itoa in diesem Zusammenhang auskommentiert           *         *
*****************************************************************************/

#include <windows.h>
#include "xvt.h"

#include "wspwin.h"
#include "resource.h"
#include "wsphilfe.h"
#include "l_typen.h"
#include "typen.h"

#include "global_types.h"
#include "global_vars.h"
#include "..\..\wspdlg\include\export.h"

#include "util.h"
#include "bce_allg.h"
#include "list.h"
#include "dis_prof.h"
#include "readprof.h"
#include "profproj.h"
#include "profpro2.h"
#include "verzweig.h"
#include "configuration.h"
#include "strang.h"
#include "strang2.h"
#include "profverl.h"

/*
#define WIN_RES_ID WIN_120
#define WIN_FLAGS 0x1800L
#define WIN_CLASS ""
#define WIN_BORDER W_DOC
*/

BOOLEAN bewuchs_alle = FALSE;
int i_d, w_id;
short	 ch_id = -1;
HWND hwnd120;
RCT rect_120, rct_tmp120;
WINDOW win120_typ, win_120[100], scroll_120, win_list2;
int range = 135;
int anz = 15;
char* p;
char temp_len[15];
int druck;
TXEDIT tx_comment120;
int datensatz_aktuel_120 = 1;
int last_edit = -1;
char is_digit;


extern BOOLEAN checkbox_intr;
extern HWND hwnd_intr;
extern UINT nUeberfallbeiwertUpdateMsg;
extern UINT nRauheitswertUpdateMsg;
extern UINT nBewuchswertUpdateMsg ;
extern char ueberfalbeiwert[15];

extern char rauheit[20];
extern char ax[13],ay[13],dp[13];

extern MENU_ITEM* main_menu_tree;

extern WINDOW dlg_151, win_dlg_154, win_dlg_107, win_dlg_160, win_dlg_110;
extern ZOOM zoom_info;
extern BOOLEAN berechnen, editieren;
extern double sf;    //Fensterfaktor
extern WSP_PROFIL_LISTE *pWPL;
extern HWND hwnd_info;
extern MinMax pmm;
extern BOOLEAN neue_datensatz_wahl;
extern char str[25];
extern char temp[100];
extern char profilnummer[15];

extern BOOL GaussProfilMitRuecksprung;//Globale Erkennung
extern void GaussRuecksprungCheck(void);//Funktion zur Überprüfung
extern BOOLEAN GaussRuecksprungCheckVar;//Flag für den Start der Funktion zur Überprüfung

extern BOOLEAN Einf;
extern WINDOW dlg_sonderprofil;
//				  lwin;
extern BOOLEAN Edit_Fehler, Edit_Fehler_Feld[15], Edit_Fehler_Y, Edit_Fehler_Z;
extern int edit_line;
BOOLEAN new_koord = FALSE, Edit_X_120 = FALSE, Edit_Y_120 = FALSE, Edit_Z_120 = FALSE;
extern  neuer_datensatz new_ds;  //struct: neuer_datensatz  -->util.h
extern char profilstr[100];

/*************   GHJ   *************/
extern  XVT_HELP_INFO hi;

static WNDPROC defWndProc;
LRESULT CALLBACK Win120WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  switch (uMsg)
  {
  case WM_HELP:
    {
      LPHELPINFO lphi = (LPHELPINFO)lParam;
      if (hi!=NULL_HELP_INFO)
      {
        switch (lphi->iCtrlId)
        {
        case WIN_120_PUSHBUTTON_44: // "Punkt einfügen"
        case WIN_120_PUSHBUTTON_47: // "Punkt löschen"
          xvt_help_display_topic(hi, HID_KAPITEL_6_3_1);
          break;
          
        case WIN_120_PUSHBUTTON_45: // "Datensatz neu"
        case WIN_120_LISTBOX_NEU:   // "Listbox : Auswahl neuer Datensatz"
          xvt_help_display_topic(hi, HID_KAPITEL_4_4_3_1);
          break;
          
        case WIN_120_PUSHBUTTON_48: // "Datensatz löschen"
        case WIN_120_LISTBOX_DEL:   // "Listbox : Auswahl löschen Datensatz"
          xvt_help_display_topic(hi, HID_KAPITEL_6_3_2);
          break;
          
        case WIN_120_LISTBUTTON_33:
          switch (typ[scr.datensatz])
          {
          case SOHLHOEHE:     
          case GELAENDEHOEHE:     
          case SOHLHOEHE_2:
            xvt_help_display_topic(hi, HID_KAPITEL_4_4_2_1);
            break;
            
          case GELAENDE2:
          case FLAECHE:
            xvt_help_display_topic(hi, HID_KAPITEL_7_1_3);
            break;
            
          case AXM:
          case AYM:
          case DPM:
            xvt_help_display_topic(hi, HID_KAPITEL_5_4_1);
            break;
            
          case TRENNFLAECHEN:
            xvt_help_display_topic(hi, HID_KAPITEL_4_4_3_3);
            break;
            
          case DURCHST_BEREICH:
            xvt_help_display_topic(hi, HID_KAPITEL_4_4_3_4);
            break;
            
          case RAUHIGKEIT:
          case RAUHIGKEIT_KST:
            xvt_help_display_topic(hi, HID_KAPITEL_4_4_3_2_1);
            break;
            
          case MAUL:
            xvt_help_display_topic(hi, HID_KAPITEL_5_3_2_4);
            break;
            
          case EIPROFIL:
            xvt_help_display_topic(hi, HID_KAPITEL_5_3_2_6);
            break;
            
          case UK_BRUECKE:
          case OK_BRUECKE:
            xvt_help_display_topic(hi, HID_KAPITEL_5_1_1_2);
            break;
            
          case OK_WEHRS:
            xvt_help_display_topic(hi, HID_KAPITEL_5_2_2);
            break;
            
          case KREIS:
            xvt_help_display_topic(hi, HID_KAPITEL_5_3_2_1);
            break;
            
          case KREISSEGM:
            xvt_help_display_topic(hi, HID_KAPITEL_5_3_2_3);
            break;
            
          case ARMCO84:
          case ARMCO71:
            xvt_help_display_topic(hi, HID_KAPITEL_5_3_2_9);
            break;
            
          case NWRINNE:
            xvt_help_display_topic(hi, HID_KAPITEL_7_2_2);
            break;
            
          case GAUSS:
            break;
            
          case TRAPEZ:
            xvt_help_display_topic(hi, HID_KAPITEL_5_3_1);
            break;
            
          default:
            xvt_help_display_topic(hi, HID_KAPITEL_6_3);
            break;
          }
          break;
          
          default:
            xvt_help_display_topic(hi, HID_KAPITEL_6_3);
            break;
        }
      }
    }
    break; // WM_HELP
    
  case WM_COMMAND:        
    {
      switch(LOWORD(wParam))
      {
      case WIN_120_PUSHBUTTON_30:
        {
          checkbox_intr=(BOOLEAN)::SendMessage(hwnd_intr, BM_GETCHECK, 0, 0L);
          ::SetFocus(hwnd);
        }
        break;
        
      case WIN_120_PUSHBUTTON_49:
        {
          char help_path[MAX_PATH];
          strcpy(help_path,start_dir);
          strcat(help_path,"wspwin.hlp");
          switch (typ[scr.datensatz]) 
          {
          case UK_BRUECKE:
            WinHelp(hwnd,help_path,HELP_CONTEXTPOPUP ,HID_TOOLINFO_UK_BR);
            break;
          case OK_BRUECKE:
            WinHelp(hwnd,help_path,HELP_CONTEXTPOPUP ,HID_TOOLINFO_OK_BR);
            break;
          case OK_WEHRS:
            WinHelp(hwnd,help_path,HELP_CONTEXTPOPUP ,HID_TOOLINFO_OK_WR);
            break;
          }
          ::SetFocus(hwnd);                    
        }
        break;
      }
    } // WM_COMMAND
    break;
    
  default:
    break;
  } // switch uMsg
  
  if (uMsg==nUeberfallbeiwertUpdateMsg)
  {
    char *temp;
    temp=(char*)lParam;
    strcpy(ueberfalbeiwert,temp);
    EVENT *xdEvent;
    xdEvent= new EVENT;
    xdEvent->type =E_USER;
    xdEvent->v.user.id=E_USER_UEBERFALLBEIWERT;
    xvt_win_dispatch_event(WIN120,xdEvent);
    delete xdEvent;
  }
  if (uMsg==nRauheitswertUpdateMsg)
  {
    char *temp;
    temp=(char*)lParam;
    strcpy(rauheit,temp);
    EVENT *xdEvent;
    xdEvent= new EVENT;
    xdEvent->type =E_USER;
    xdEvent->v.user.id=E_USER_RAUH;
    xvt_win_dispatch_event(WIN120,xdEvent);
    delete xdEvent;
  }
  if (uMsg==nBewuchswertUpdateMsg)
  {
    
    typedef struct _Bewuchs
    {
      double ax;
      double ay;
      double dp;
    }Bewuchs;
    Bewuchs *bew;
    bew=(Bewuchs *)lParam;
    
    if(bew->ax!=BCE_NAN)
      sprintf(ax,"%.4lf",bew->ax);
    else
      strcpy(ax," ");
    if(bew->ay!=BCE_NAN)
      sprintf(ay,"%.4lf",bew->ay);
    else
      strcpy(ay," ");
    if(bew->dp!=BCE_NAN)
      sprintf(dp,"%.4lf",bew->dp);
    else
      strcpy(dp," ");
    bewuchs_alle=TRUE;
    EVENT *xdEvent;
    xdEvent= new EVENT;
    xdEvent->type =E_USER;
    xdEvent->v.user.id=E_USER_BEWUCHS;
    xvt_win_dispatch_event(WIN120,xdEvent);
    delete xdEvent;
  }
  return CallWindowProc(defWndProc, hwnd, uMsg, wParam, lParam);
}
/*********************************/


/*  Handler for window WIN_120 ("Alpha-numerischer Editor")  */
long XVT_CALLCONV1
#if XVT_CC_PROTO
WIN_120_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
WIN_120_eh XVT_CALLCONV2 (xdWindow, xdEvent)
WINDOW xdWindow;
EVENT *xdEvent;
#endif
{
  short xdControlId = xdEvent->v.ctl.id;
  switch (xdEvent->type) 
  {
  case E_CREATE:
    /* Window has been created; first event sent to newly-created window. */
    {
      char buf[200],buf2[200];
      /*************   GHJ   *************/
      defWndProc = (WNDPROC)GetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC);
      SetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC, (LONG)&Win120WindowProc);
      /***********************************/
      xvt_menu_set_tree(Menu_Win,main_menu_tree);
      char *zufuegen_ptr;
      
      i_d=3000;
      w_id=0;
      ch_id=0;
      dlg_151 = NULL_WIN;
      
      zoom_info.level = 0;
      
      set_menu_120(Menu_Win,FALSE); // in: BCE_DLL
      
      if(!(berechnen &&editieren))
      {
        xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_13_60, TRUE );   //Bearbeiten-Geländeverknüpfung
        xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_13_64, TRUE );   //Bearbeiten-Flächenberechnung
      }
      else
      {
        xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_13_60,FALSE);   //Bearbeiten-Geländeverknüpfung
        xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_13_64,FALSE);   //Bearbeiten-Flächenberechnung
      }
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_50_127,FALSE); // Menu: Zustände
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_50_128,FALSE); // Menu: Zustand speichern als
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_13_42,FALSE); // Menu: Rauheiten ändern
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_14_16,FALSE); // Menu: Abflussdatei
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_14_17,FALSE); // Menu: Verlustdatei
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_13_59,FALSE); // Menu:Interpolieren
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_30_24,FALSE);//Laengsschnitt einsehen
      
      xvt_menu_update(Menu_Win);

      if( !LWA_PROJEKT )
      {
        MENU_ITEM *men;
        men=xvt_menu_get_tree(Menu_Win);
        men[3].child[3].enabled=0;
        men[1].child[10].enabled=1;
        men[1].child[8].enabled=0;
        men[1].child[11].enabled=0;
        xvt_menu_set_tree(Menu_Win,men);
      };
      
      LONG info;
      
      hwnd120 =(HWND) xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW);
      info = GetWindowLong(hwnd120,GWL_STYLE);
      
      xvt_vobj_set_visible(xvt_win_get_ctl(xdWindow,WIN_120_PUSHBUTTON_34),FALSE);/* "Schlüsseldaten editieren" */
      xvt_vobj_set_visible(xvt_win_get_ctl(xdWindow,WIN_120_PUSHBUTTON_35),FALSE);/* "Schrift/Linie editieren" */
      
      WIN120 = xdWindow;
      
      /*************  Static-Text Feld:  Titel 3.Spalte **************/
      xvt_vobj_get_outer_rect(xvt_win_get_ctl(xdWindow,WIN_120_TEXT_19),&rct_tmp120); 
      xvt_rect_set(&rect_120,(short)(rct_tmp120.right+1),(short)(rct_tmp120.top),(short)(rct_tmp120.right+201),(short)(rct_tmp120.bottom));
      
      xvt_ctl_create(WC_TEXT, &rect_120, 0L, xdWindow,0L,0L,WIN120_EDIT_TYP);
      win120_typ=xvt_win_get_ctl(xdWindow,WIN120_EDIT_TYP);
      
      /************* 15 * 2  X-Y-Editfelder generieren***************/
      for (int j=0;j<=1;j++)
      {
        for (int i=1;i<=15;i++)
        {
          xvt_rect_set(&rect_120,(short)((63+j*135)*sf),(short)((60+i*30)*sf),(short)((198+j*135)*sf),(short)((90+i*30)*sf ));
          xvt_ctl_create(WC_EDIT, &rect_120, 0L, xdWindow,0L,0L,i_d + w_id);
          win_120[w_id]=xvt_win_get_ctl(xdWindow,i_d + w_id);
          w_id++;
        }
      }
      
      for (int i=30;i<=44;i++)   //3.Spalte initialisieren
        win_120[i]=NULL_WIN;
      
      /************ Heap-Größe ********************************/
      //	UINT free_heap = LocalCompact(0);
      //	xvt_dm_post_note("Max. Heap =%i  %2.1f %%",free_heap,(float)free_heap/70);
      /************ Heap-Größe ********************************/
      buf[0]='\0';
      xvt_res_get_str(STR_CHECKBOX_Z_WERT,buf,sizeof(buf));
      //Dick 21.07.99 
      hwnd_intr=CreateWindow("BUTTON",buf,BS_AUTOCHECKBOX  | BS_MULTILINE |WS_CHILD|WS_VISIBLE  ,345,545,104,42,
        hwnd120,(HMENU)WIN_120_PUSHBUTTON_30,(HINSTANCE)GetWindowLong(hwnd120,GWL_HINSTANCE),NULL);
      checkbox_intr=FALSE;
      
      /*********  SCROLLBAR RECHTS   2spaltig     *********************************/
      xvt_rect_set(&rect_120,(short)( 333*sf),(short)( 90*sf),(short)( 354*sf),(short)( 540*sf));
      scroll_120 = xvt_ctl_create(WC_VSCROLL, &rect_120, 0L, xdWindow, 0L,0L,WIN120_SCROLL);
      
      /********   Listbutton33: Datensatz auswählen    ****************************/
      win_list2=xvt_win_get_ctl(xdWindow,WIN_120_LISTBUTTON_33);
      
      pWPL->window = win_list2; //zum updaten von Listbutton33 in DLL
      //nach Geländeverknüpfung->Fkt:Connect_Prof_2Gel2()
      
      //Info-Button
      buf[0]='\0';
      RECT info_rect,w120_rect;
      HBITMAP info_bm;
      info_bm=(HBITMAP)LoadImage((HINSTANCE)GetWindowLong(hwnd120,GWL_HINSTANCE),MAKEINTRESOURCE(IDB_FRAGE1),IMAGE_BITMAP,0,0,LR_CREATEDIBSECTION);
      ::GetWindowRect((HWND)xvt_vobj_get_attr(win_list2,ATTR_NATIVE_WINDOW), &info_rect);
      ::GetWindowRect(hwnd120, &w120_rect);
      
      hwnd_info=CreateWindow("BUTTON",buf,BS_PUSHBUTTON | BS_BITMAP |WS_CHILD  ,info_rect.right-w120_rect.left,info_rect.top-w120_rect.top-w120_rect.top/2,20,20,
        hwnd120,(HMENU)WIN_120_PUSHBUTTON_49,(HINSTANCE)GetWindowLong(hwnd120,GWL_HINSTANCE),NULL);
      HANDLE iimage=(HWND)SendMessage(hwnd_info,BM_SETIMAGE,(WPARAM)IMAGE_BITMAP,(LPARAM)(HANDLE)info_bm);
      //
      /************************** Edit- SCHLÜSSELDATEN ****************************/
      w_id=45;
      for ( i=0;i<6;i++)   //Schlüsseldaten
        win_120[w_id +i] = xvt_win_get_ctl(xdWindow,WIN_120_EDIT_23+i);
      
      ChangeFontAndSize((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW));	// GHJ
      RECT rect;
      ::SystemParametersInfo(SPI_GETWORKAREA, 0, &rect, 0);
      if (rect.right-rect.left <= 800)
      {
        ::GetClientRect(::GetParent((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW)), &rect);
        ::MoveWindow((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), rect.left, rect.top, rect.right-rect.left, rect.bottom-rect.top, TRUE);
      }
      xvt_vobj_set_visible(xdWindow, TRUE);		// GHJ
      
      /***************************************************************************/
      if ((is_profil_open)&&(!new_profil))
      {
        range=ds_info[1]-13;//Dick 10.08.98
        scr.scrollpos=1;
        scr.anzahl=anz;
        scr.datensatz=1;
        
        list->GetMinMax(&pmm,scr.datensatz);
        list->GetScrollDaten(&scr);
        neue_datensatz_wahl = TRUE; //damit dlg110 angezeigt wird
        display_prof120(&win_120[0]);
        
        test_line9(str);
        xvt_vobj_set_title(win_120[46],str);   //Station
        
        xvt_res_get_str(STR_GAUSSRUECK,buf,sizeof(buf));
        xvt_res_get_str(STR_OFF_PROFIL,buf2,sizeof(buf2));
        
        if(GaussProfilMitRuecksprung)
          strcpy(temp,buf);
        else
          if (! list->GetSonderProfilTyp(&temp[0]))  // Profiltyp
            strcpy(temp,buf2);
          xvt_vobj_set_title(win_120[48],temp);
          
          /***GEWAESSERNAME***/
          char zufuegen[80];
          zufuegen_ptr=xvt_slist_get_elt(header_profil,5,0L);
          strcpy(zufuegen,zufuegen_ptr);
          if(strlen(zufuegen)<=36)
          {
            for(i=strlen(zufuegen);i<40;i++)
              zufuegen[i]=' ';
            zufuegen[40]='\0';
            strcat(zufuegen,netz_dat[0]);
            xvt_slist_change_str(header_profil,zufuegen,5);
          }
          xvt_vobj_set_title(win_120[45],netz_dat[0]);  //Gewässername
          
          /***VZK***/
          zufuegen_ptr=xvt_slist_get_elt(header_profil,4,0L);
          strcpy(zufuegen,zufuegen_ptr);
          if(strlen(zufuegen)<=36)
          {
            for(i=strlen(zufuegen);i<40;i++)
              zufuegen[i]=' ';
            zufuegen[40]='\0';
            strcat(zufuegen,vzk);
            xvt_slist_change_str(header_profil,zufuegen,4);
          }
          xvt_vobj_set_title(win_120[47],vzk);
          
          /***********     PK     ****************************/
          xvt_vobj_set_title(win_120[49],pk);
          zufuegen_ptr=xvt_slist_get_elt(header_profil,6,0L);
          strcpy(zufuegen,zufuegen_ptr);
          if(strlen(zufuegen)<=36)
          {
            for(i=strlen(zufuegen);i<40;i++)
              zufuegen[i]=' ';
            zufuegen[40]='\0';
            strcat(zufuegen,pk);
            xvt_slist_change_str(header_profil,zufuegen,6);
          }
          
          /**************Zustand************************/
          xvt_vobj_set_title(win_120[50],zustand);
          zufuegen_ptr=xvt_slist_get_elt(header_profil,2,0L);
          strcpy(zufuegen,zufuegen_ptr);
          if(strlen(zufuegen)<=36)
          {
            for(i=strlen(zufuegen);i<40;i++)
              zufuegen[i]=' ';
            zufuegen[40]='\0';
            strcat(zufuegen,zustand);
            xvt_slist_change_str(header_profil,zufuegen,2);
          }
          
          
          list->GetDateninfo(win_list2);   //Daten in LISTBUTTON ausgeben
          
          w_id=0;
      }
      if ((!is_profil_open)&&(new_profil))
      {
        SaveProfilFile = FALSE;
        
        xvt_slist_destroy(header_profil);
        header_profil = xvt_slist_create();
        
        for (i=0;i<14;i++)
          xvt_slist_add_at_elt(header_profil,NULL,"\0",0L);
          /*vorläufig bis Plotdaten über Dialog eingegeben werden, da
        zur Berechnung nötig bei WSPBCE:*/
        xvt_slist_add_at_elt(header_profil,NULL,"0 0 0 0 0 0 0 0 0",0L);
        
        list->DeleteList();                         // neue Profildatei anlegen !
        list->MakeNewNode(1);
        range = 0;
        scr.scrollpos = 1;
        anzahl_ds = 1;
        ds_info[0] = 1;
        ds_info[1] = 1;
        scr.anzahl=1;
        scr.datensatz=1;
        
        for (i=0;i<=TYPE_SIZE-1;i++) typ[i]=0;//Dick 8.12.98   //Datenblocktypliste init.
        typ[1] = 1;
        
        list->WriteTypDaten(typ[1],GELAENDEHOEHE,NULL);
        
        list->GetDateninfo(win_list2);
        
        p=&netz_dat[0][0];
        xvt_vobj_set_title(win_120[45],netz_dat[0]);      //Gewässername
        /*NEU*/
        char zufuegen[80];
        for (int i=0;i<40;i++)
          zufuegen[i]=' ';
        zufuegen[40]='\0';
        strcat(zufuegen,netz_dat[0]);
        xvt_slist_change_str(header_profil,zufuegen,5);
        /*****/
        
        xvt_vobj_set_title(win_120[46],station208);
        
        /********Zeile 9 schreibe: Station*****/
        write_line9(station208);
        
        /*NEU**/
        for (i=0;i<40;i++)
          zufuegen[i]=' ';
        zufuegen[40]='\0';
        strcat(zufuegen,zustand);
        xvt_slist_change_str(header_profil,zufuegen,2);   // Zustand
        xvt_vobj_set_title(win_120[50],zustand);
        /******/
        
        /***NEU***VZK*/
        for (i=0;i<40;i++)
          zufuegen[i]=' ';
        zufuegen[40]='\0';
        strcat(zufuegen,vzk);
        xvt_slist_change_str(header_profil,zufuegen,4);   // VZK
        xvt_vobj_set_title(win_120[47],vzk);
        
        
        /*****Neu***Querprofil*/
        profil_nr_ermitteln();
        zufuegen[0]='\0';
        strcat(zufuegen,"Querprofil ");
        strcat(zufuegen,profilnummer);
        xvt_slist_change_str(header_profil,zufuegen,7);
        
        /*NEU**PK***/
        for (i=0;i<40;i++)
          zufuegen[i]=' ';
        
        zufuegen[40]='\0';
        strcat(zufuegen,pk);
        
        for (i=0;i<(INT)strlen(netz_dat[0]);i++)
          zufuegen[i]=netz_dat[0][i];
        int j=10;
        for (i=0;i<(INT)strlen(zustand);i++,j++)
          zufuegen[j]=zustand[i];
        
        xvt_slist_change_str(header_profil,zufuegen,6);   // Zustand
        xvt_vobj_set_title(win_120[49],pk);
        
        //neuen Profildateinamen erzeugen:
        for (i=0;i<15;i++)
          dateiname[i] ='0';
        dateiname[0]=p[0];
        dateiname[1]=p[1];
        dateiname[8]= '.';
        dateiname[9]= 'p';
        dateiname[10]= 'r';
        dateiname[11]= 'f';
        dateiname[12]= '\0';
        
        /*************DAGMAR EINGEFUEGT*****************/
        profil_nr_ermitteln();
        strcpy(temp_len,profil_nr_string);
        
        /***NEU*****/
        int back=0;
        while(back==0)
        {
          j=7;
          for (i=strlen(temp_len);i>=1;i--)
          {
            dateiname[j] = temp_len[i-1];
            j--;
          }
          
          char testedatei[100];
          xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,testedatei,50);
          strcat(testedatei,"\\");
          strcat(testedatei,dateiname);
          back=access(testedatei,00);
          if(back==0)
          {
            int tempint=atoi(temp_len);
            tempint++;
            itoa(tempint,temp_len,10);
          }
        } //while back==0
        
        vergleich=FALSE;
        druck=0;
        teste_projekt_profile(druck);   //vergleich wird geändert
        if(vergleich)
        {
          SaveProfilFile = FALSE;
          xvt_vobj_destroy(xdWindow);
        }
        
        /*************/
        
      }
      if( !vergleich)
      {
        if (scroll_120 !=NULL_WIN)
        {
          xvt_sbar_set_range(scroll_120, HVSCROLL, 1,range );
          xvt_sbar_set_pos(scroll_120,HVSCROLL,1);
        }
        if (win_120[ch_id] !=NULL_WIN)
          xvt_scr_set_focus_vobj(win_120[ch_id]);
      }
      
      //Dick 12.01.99 Kommentar
      if( !vergleich)
      {
        tx_comment120=xvt_win_get_tx(xdWindow,WIN_120_TEDIT_55);
        SLIST_ELT e_c;
        char *line;
        
        if (slist_comment == NULL)
          slist_comment = xvt_slist_create();
        for (e_c=xvt_slist_get_first(slist_comment);e_c!=NULL;e_c=xvt_slist_get_next(slist_comment,e_c))
        {
          line = xvt_slist_get(slist_comment,e_c,0L);
          
          if (!xvt_tx_add_par(tx_comment120, USHRT_MAX,line ))
          {
            char buf[200];//Dick 26.11.99
            xvt_res_get_str(STR_ERROR_TXTEDIT_OUT,buf,sizeof(buf));
            xvt_dm_post_error("%s",buf);
            //xvt_dm_post_error("Fehler bei TXEDIT -Ausgabe");
          }
        }
      }
      //
   } // E_CREATE
   break;
   
  case E_DESTROY:
    {
      set_menu_120(Menu_Win,TRUE); // in:util.cpp
      
      if (dlg_136!=NULL_WIN)
        xvt_vobj_set_visible(dlg_136,TRUE);
      ch_id=0;
      pWPL->window = NULL;
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_50_127,TRUE); // Menu: Zustände
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_50_128,TRUE); // Menu: Zustand speichern als
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_13_42,TRUE); // Menu: Rauheiten ändern
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_14_16,TRUE); // Menu: Abflussdatei
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_14_17,TRUE); // Menu: Verlustdatei         
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_13_59,TRUE); // Menu:Interpolieren
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_30_24,TRUE);//Laengsschnitt einsehen
      
      xvt_menu_update(Menu_Win);

      if( !LWA_PROJEKT )
      {
        MENU_ITEM *men;
        men=xvt_menu_get_tree(Menu_Win);
        men[3].child[3].enabled=1;
        men[1].child[10].enabled=1;
        men[1].child[8].enabled=1;
        men[1].child[11].enabled=1;
        xvt_menu_set_tree(Menu_Win,men);
      };
      
      WIN120 = NULL_WIN;
      
      if (slist_comment!=NULL)
      {
        xvt_slist_destroy(slist_comment);
        slist_comment=NULL;
      }
      
      tx_comment120=NULL;
      
      berechnen=FALSE;
      editieren=FALSE;
      GaussProfilMitRuecksprung=FALSE;
      Einf=FALSE;
      Edit_Fehler=FALSE;
      DestroyWindow(hwnd_intr);
      hwnd_intr=NULL;
      DestroyWindow(hwnd_info);
      hwnd_info=NULL;
      hwnd120=NULL;
    } // E_DESTROY
    return 0L;
    
  case E_FOCUS:
    {
      /*      Window has lost or gained focus.        */
      if (xdEvent->v.active) 
      {
        /*              Window has gained focus         */
      }
      else
      {
        /*      Window has lost focus*/
      }
    }
    break;
    
  case E_SIZE:
    {
    }
    break;
    
  case E_UPDATE:
    /*              Window requires updating.       */
    {
      list->GetMinMax(&pmm,scr.datensatz);
      xvt_dwin_clear(xdWindow,WspwinMainBackgroundColor);
      if (scroll_120 != NULL_WIN)
        xvt_sbar_set_pos(scroll_120,HVSCROLL,scr.scrollpos);
    }
    break;
    
  case E_CLOSE:
    {
      if (dlg_sonderprofil != NULL_WIN)    // falls Dialog geöffnet erst schliessen
      {
        xvt_vobj_destroy(dlg_sonderprofil);
        dlg_sonderprofil = NULL_WIN;
      }
      WIN120 = NULL_WIN;
      xvt_vobj_destroy(xdWindow);
    }
    break;
    
  case E_CHAR:
    {
      switch(xdEvent->v.chr.ch)
      {
      case K_INS:   //Einfügen neuer Stationswert
        {
          int i=0;
          char buf[200];
          
          //Daten abspeichern ,wo zuletzt editiert wurde
          xdEvent->type =E_CONTROL;
          xdEvent->v.ctl.id=3000 + ch_id;
          xdEvent->v.ctl.ci.v.edit.focus_change = TRUE;
          xdEvent->v.ctl.ci.v.edit.active = FALSE;
          xvt_win_dispatch_event(xdWindow,xdEvent);
          //Daten einlesen
          list->GetScrollDaten(&scr);
          for(i=14;i>edit_line;i--)
          {
            scr.x[i]=scr.x[i-1];
            scr.y[i]=scr.y[i-1];
            scr.z[i]=scr.z[i-1]; 
          }
          scr.x[i]=BCE_NAN;
          scr.y[i]=BCE_NAN;
          scr.z[i]=BCE_NAN;
          display_prof120(&win_120[0]);
          xvt_vobj_set_title(win_120[edit_line],xvt_res_get_str(STR_NEU,buf,sizeof(buf)));
          xvt_scr_set_focus_vobj(win_120[edit_line]);
          Einf=TRUE;
          break;
        }
        
      case K_TAB:
      case ENTER:
        {
          if (ch_id <= 14)                                // 0...14
            xvt_scr_set_focus_vobj(win_120[ch_id+15]);
          else
          {
            if((ch_id>=15)&&(ch_id<29))                   // 15..28
              xvt_scr_set_focus_vobj(win_120[ch_id-14]);
            else
            {
              if (ch_id == 29)                                // 29-->14
                xvt_scr_set_focus_vobj(win_120[14]);
              else
              {
                if (ch_id==44)
                {                     // 44 --> 14
                  
                  //								  if (ch_id-30 + scr.scrollpos < ds_info[scr.datensatz]+1)
                  if (ch_id-29 + scr.scrollpos < ds_info[GELAENDEHOEHE])
                  {
                    //Wert in Edit-Feld 44 SICHERN
                    xdEvent->type =E_CONTROL;
                    xdEvent->v.ctl.id=3000 + ch_id;
                    xdEvent->v.ctl.ci.v.edit.focus_change = TRUE;
                    xdEvent->v.ctl.ci.v.edit.active = FALSE;
                    xvt_win_dispatch_event(xdWindow,xdEvent);
                    
                    
                    scr.scrollpos++;
                    list->GetScrollDaten(&scr);
                    display_prof120(&win_120[0]);
                    
                  }
                  else   //NEU
                    
                  {
                    xvt_scr_set_focus_vobj(win_120[14]);
                    if(Edit_Z_120!=TRUE)
                    {
                      scr.scrollpos++;
                      list->GetScrollDaten(&scr);
                      display_prof120(&win_120[0]);
                    }
                  }
                }
                else     // 30..43
                {
                  if (ch_id-29 + scr.scrollpos > ds_info[1])
                  {  //NEU
                    xvt_scr_set_focus_vobj(win_120[ch_id-29]);
                    if(Edit_Z_120!=TRUE)
                    {
                      list->GetScrollDaten(&scr);
                      display_prof120(&win_120[0]);
                    }
                  }
                  else
                  {
                    xvt_scr_set_focus_vobj(win_120[ch_id+1]);
                  }
                  
                }
              }
            }
          }
        }
        break;
      case K_UP:
        {
        }
        break;
      case K_DOWN:
        {
        }
        break;
      case K_LEFT:
        {
        }
        break;
      case K_RIGHT:
        {
        }
        break;
      case K_HOME:
        {
        }
        break;
      case ESC:
        {
          if (new_ds.listbox_win !=NULL_WIN )
            xvt_vobj_destroy(new_ds.listbox_win);
          new_ds.listbox_win=NULL_WIN;
        }
       }
    }
    break;
    
  case E_MOUSE_UP:
		/*
    Mouse was released
    */
    {
    }
    break;
  case E_MOUSE_DOWN:
    /*			Mouse was pressed		*/
    {
    }
    break;
  case E_MOUSE_DBL:
		/*
    Mouse was double clicked
    */
    {
    }
    break;
  case E_MOUSE_MOVE:
		/*
    Mouse was moved
    */
    {
    }
    break;
  case E_HSCROLL:
    {
    /*
    Horizontal scrollbar on frame was operated
      */
      switch (xdEvent->v.scroll.what)
      {
      case SC_LINE_UP:
        break;
      case SC_LINE_DOWN:
        break;
      case SC_PAGE_UP:
        break;
      case SC_PAGE_DOWN:
        break;
      case SC_THUMB:
        break;
      case SC_THUMBTRACK:
        break;
      default:
        break;
      }
    }
    break;
  case E_VSCROLL:
    {
    /*
    Vertical scrollbar on frame was operated
      */
      switch (xdEvent->v.scroll.what)
      {
      case SC_LINE_UP:
        break;
      case SC_LINE_DOWN:
        break;
      case SC_PAGE_UP:
        break;
      case SC_PAGE_DOWN:
        break;
      case SC_THUMB:
        break;
      case SC_THUMBTRACK:
        break;
      default:
        break;
      }
    }
    break;
    
  case E_CONTROL:
    {
      switch(xdControlId)
      {
      case WIN120_SCROLL: /* "Vertical Scrollbar:Datenblocktyp rechts" */
        {
          switch (xdEvent->v.ctl.ci.v.scroll.what)
          {
          case SC_LINE_UP:
            if (scr.scrollpos > 1)
            {
              
              if (ch_id >0)   // Cursor auf Edit-Feld->Daten sichern
              {
                xdEvent->type =E_CONTROL;
                xdEvent->v.ctl.id=3000 + ch_id;
                xdEvent->v.ctl.ci.v.edit.focus_change = TRUE;
                xdEvent->v.ctl.ci.v.edit.active = FALSE;
                xvt_win_dispatch_event(xdWindow,xdEvent);
              }
              ch_id=0;
              
              scr.scrollpos --;
              list->GetScrollDaten(&scr);
              display_prof120(&win_120[0]);
              xvt_sbar_set_pos(scroll_120,HVSCROLL,scr.scrollpos);
            }
            break;
          case SC_LINE_DOWN:
            if (scr.scrollpos < (ds_info[1]-14))
            {
              if (ch_id >0)   // Cursor auf Edit-Feld->Daten sichern
              {
                xdEvent->type =E_CONTROL;
                xdEvent->v.ctl.id=3000 + ch_id;
                xdEvent->v.ctl.ci.v.edit.focus_change = TRUE;
                xdEvent->v.ctl.ci.v.edit.active = FALSE;
                xvt_win_dispatch_event(xdWindow,xdEvent);
              }
              ch_id=0;
              
              
              scr.scrollpos ++;
              list->GetScrollDaten(&scr);
              display_prof120(&win_120[0]);
              xvt_sbar_set_pos(scroll_120,HVSCROLL,scr.scrollpos);
            }
            break;
          case SC_PAGE_UP:
            if (scr.scrollpos >14)
            {
              
              if (ch_id >0)   // Cursor auf Edit-Feld->Daten sichern
              {
                xdEvent->type =E_CONTROL;
                xdEvent->v.ctl.id=3000 + ch_id;
                xdEvent->v.ctl.ci.v.edit.focus_change = TRUE;
                xdEvent->v.ctl.ci.v.edit.active = FALSE;
                xvt_win_dispatch_event(xdWindow,xdEvent);
              }
              ch_id=0;
              
              scr.scrollpos =scr.scrollpos -14;
              if (scr.scrollpos <0)
                scr.scrollpos=1; //Dick
              list->GetScrollDaten(&scr);
              display_prof120(&win_120[0]);
              xvt_sbar_set_pos(scroll_120,HVSCROLL,scr.scrollpos);
            }
            break;
          case SC_PAGE_DOWN:
            if (scr.scrollpos < (ds_info[1]-14))
            {
              if (ch_id >0)   // Cursor auf Edit-Feld->Daten sichern
              {
                xdEvent->type =E_CONTROL;
                xdEvent->v.ctl.id=3000 + ch_id;
                xdEvent->v.ctl.ci.v.edit.focus_change = TRUE;
                xdEvent->v.ctl.ci.v.edit.active = FALSE;
                xvt_win_dispatch_event(xdWindow,xdEvent);
              }
              ch_id=0;
              
              
              scr.scrollpos = scr.scrollpos + 14;
              if (scr.scrollpos>ds_info[1]-14)
                scr.scrollpos =ds_info[1]-14;
              list->GetScrollDaten(&scr);
              display_prof120(&win_120[0]);
              xvt_sbar_set_pos(scroll_120,HVSCROLL,scr.scrollpos);
            }
            break;
          case SC_THUMB:
            //if (scr.scrollpos < (ds_info[1]-14))
            {
              if (ch_id >0)   // Cursor auf Edit-Feld->Daten sichern
              {
                xdEvent->type =E_CONTROL;
                xdEvent->v.ctl.id=3000 + ch_id;
                xdEvent->v.ctl.ci.v.edit.focus_change = TRUE;
                xdEvent->v.ctl.ci.v.edit.active = FALSE;
                xvt_win_dispatch_event(xdWindow,xdEvent);
              }
              ch_id=0;
              
              
              scr.scrollpos=xdEvent->v.ctl.ci.v.scroll.pos;
              list->GetScrollDaten(&scr);
              display_prof120(&win_120[0]);
              xvt_sbar_set_pos(scroll_120,HVSCROLL,scr.scrollpos);
            }
            break;
          case SC_THUMBTRACK:
            //if (scr.scrollpos < (ds_info[1]-14))
            {
              if (ch_id >0)   // Cursor auf Edit-Feld->Daten sichern
              {
                xdEvent->type =E_CONTROL;
                xdEvent->v.ctl.id=3000 + ch_id;
                xdEvent->v.ctl.ci.v.edit.focus_change = TRUE;
                xdEvent->v.ctl.ci.v.edit.active = FALSE;
                xvt_win_dispatch_event(xdWindow,xdEvent);
              }
              ch_id=0;
              
              
              scr.scrollpos=xdEvent->v.ctl.ci.v.scroll.pos;
              xvt_sbar_set_pos(scroll_120,HVSCROLL,scr.scrollpos);
              list->GetScrollDaten(&scr);
              display_prof120(&win_120[0]);
              
            }
            break;
          default:break;
           } //-switch
        }
        break;
    case WIN_120_LISTBUTTON_33:
      {
        SLIST slist;
        char *str;
        scr.scrollpos=1;   // muß auf 1 gesetzt werden, da sonst Ausgabe
        // von Trennflächen mißlingt
        scr.datensatz=(  xvt_list_get_sel_index(win_list2) + 1  );
        
        //Neu Dick 15.12.99 Info-Button
        switch (typ[scr.datensatz]) 
        {
        case UK_BRUECKE:                             
        case OK_BRUECKE:
        case OK_WEHRS:
          ShowWindow(hwnd_info,SW_SHOW);
          break;
        default:
          ShowWindow(hwnd_info,SW_HIDE);
        }
        //
        
        if (typ[scr.datensatz] == COMMENT)
        {
          win120_loesche_edit_felder();   //->dis_prof.h
          if (dlg_sonderprofil != NULL_WIN)
          {
            xvt_vobj_destroy(dlg_sonderprofil);
            dlg_sonderprofil = NULL_WIN;
          }
          //
        }
        else
        {
          if(datensatz_aktuel_120!=scr.datensatz)//Dick  9.08.98
          {
            datensatz_aktuel_120=scr.datensatz; 
            list->Koord_Update(0);
          }
          if (ds_info[1] < 15)//Dick 31.05.99 Korrektur scr.anzahl nach Update 
            scr.anzahl = ds_info[1];
          else
            scr.anzahl =15;
          list->GetScrollDaten(&scr);
          neue_datensatz_wahl = TRUE;
          display_prof120(&win_120[0]);
          xvt_sbar_set_pos(scroll_120,HVSCROLL,1);
        }
        /**********  Titel 3.Spalte setzen: **************/
        xvt_vobj_set_title(win120_typ,"\0");  //allg. Titel löschen
        if (scr.anzahl >0)
          if ((typ[scr.datensatz] >GELAENDEHOEHE)&&(typ[scr.datensatz] < MAUL))
          {
            if((typ[scr.datensatz]!=TRENNFLAECHEN)&&(typ[scr.datensatz]!=DURCHST_BEREICH)&&(typ[scr.datensatz]!= BORDVOLL)&&(typ[scr.datensatz]!= MODELLGRENZEN))
            { //Trennfl.+durchst.Ber.ausschliessen
              if(typ[scr.datensatz]!=KREISSEGM)
              {
                slist = xvt_slist_create();
                slist = xvt_list_get_sel(win_list2);
                str = xvt_slist_get_elt(slist,0,0L);
                xvt_vobj_set_title(win120_typ,str);
              }
              else
                xvt_vobj_set_title(win120_typ,"\0");
            }
          }
          else
            xvt_vobj_set_title(win120_typ,"\0");
          /***Ende  Titel 3.Spalte setzen: ****/
      }
      break;
      
    case WIN_120_PUSHBUTTON_26: /* "OK" */
      {
        if (ch_id >0)   // Cursor auf Edit-Feld
        {
          new_koord=TRUE;
          xdEvent->type =E_CONTROL;
          xdEvent->v.ctl.id=3000 + ch_id;
          xdEvent->v.ctl.ci.v.edit.focus_change = TRUE;
          xdEvent->v.ctl.ci.v.edit.active = FALSE;
          xvt_win_dispatch_event(xdWindow,xdEvent);
        }
        ch_id=0;
        
        if (dlg_sonderprofil != NULL_WIN)
        {
          xvt_vobj_destroy(dlg_sonderprofil);
          dlg_sonderprofil = NULL_WIN;
        }
        
        if (ds_info[1]>1 && GetFeature( "wsp_nodemo" ) )   /*in GELÄNDE müssen mind. 2 Datentupel stehen */
        {	                					 /* ansonsten wird Profil nicht gespeichert !  */
          int ds_tmp = scr.datensatz;  //Datensatz retten
          if (check_border())
          {
            if (new_profil)
            {   /***  DAGMAR EINGEFÜGT  ***/
              vergleich=FALSE;
              druck=1;
              teste_projekt_profile(druck);
              /*****/
              
              char tmp[15];
              test_line9(tmp);
              
              for (int h=0;h<100;h++)   //init.
                profilstr[h]=' ';
              profilstr[99]='\0';
              for (h=0;h<(INT)strlen(netz_dat[0]);h++)            //Gewässer
              {
                if ((netz_dat[0][h]=='\n')||(netz_dat[0][h]=='\0'))
                  profilstr[h]=' ';
                else
                  profilstr[h]=netz_dat[0][h];
              }
              int a=17;                                  //Station
              int len=strlen(tmp);
              len=len-1;
              for(h=len;h>=0;h--,a--)
                profilstr[a]=tmp[h];
              
              a=26;
              for(h=(INT)(strlen(pk)-1);h>=0;h--,a--)             //pk
                profilstr[a]=pk[h];   //Neu Dick 19->26 und minus 10.07.98
              
              a=31;
              for(h=(INT)(strlen(vzk)-1);h>=0;h--,a--)            //vzk
                profilstr[a]=vzk[h];  //Neu Dick 29->31 und minus 10.07.98
              
              for(h=0;h<(INT)strlen(zustand);h++)
                profilstr[33+h]=zustand[h];
              
              for (h=0;h<=11;h++)          // Filename
              {
                profilstr[44+h]=dateiname[h];
                uebergabe_name[h]=dateiname[h];
              }
              uebergabe_name[12]='\0';

              if( GetFeature( "wsp_nodemo" ) )
              {
                char* stat_str;
                float st_km_new,st_km_test;
                SLIST_ELT e;
                int kol=0,pos_gefunden=-1;
                char dummy_str[10];
                sscanf(profilstr,"%s%f",dummy_str,&st_km_new);
                for(e=xvt_slist_get_first(prof_datei);e!=NULL;e=xvt_slist_get_next(prof_datei,e))
                {
                  stat_str=xvt_slist_get(prof_datei,e,0L);
                  sscanf(stat_str,"%s%f",dummy_str,&st_km_test);
                  if(st_km_test > st_km_new)
                  {
                    xvt_slist_add_at_pos(prof_datei,kol,profilstr,0L);
                    pos_gefunden=kol;
                    break;
                  }
                  else
                    kol++;
                  
                }
                if(pos_gefunden<0)
                  xvt_slist_add_at_elt(prof_datei,NULL,profilstr,0L);
                xvt_list_suspend(lwin);
                xvt_list_clear(lwin);
                if (prof_datei != NULL)
                  xvt_list_add(lwin, -1, (char*)prof_datei);   //Ausgabe in Listbox
                xvt_list_resume(lwin);
              };

              // Listbox vorselektieren
              if ((xvt_slist_count(prof_datei))>0)
                xvt_list_set_sel(lwin, 0,  TRUE);
              // -Listbox vorselektieren
              anzahl_profil_dat_entries++;
              
              //NEU:
              istverzweigt=FALSE;
              teste_str_verzweigt(); //IN VERZWEIG.cpp

              if ( GetFeature( "wsp_nodemo" ) )
              {
                BOOL strang_vorwaerts = GetSortStrangVorwaerts();
                if (((istverzweigt==FALSE) &&(vzk[0]=='0')&&(pk[0]=='0'))||(anzahl_profil_dat_entries<=1))
                  sort_new_profil( &strang_anfang, &strang_ende, station208, vzk, pk, uebergabe_name, strang_vorwaerts );
                else
                {                           
                  if ( GetSortVerzweigt() )
                    verzweigtes_profil_sortieren( 0, strang_vorwaerts );
                  else
                    verzweigtes_profil_sortieren( 1, strang_vorwaerts );
                }
              };
              //ENDE VON NEU
              
              wandle_abstand_in_string();
              StrangUpdateIndex();
              dlg136_get_daten(scroll_position);
            } // -if (new_profil)

            if( GetFeature( "wsp_nodemo" ) )
            {
              list->Koord_Update(0);
              
              if( new_profil )
              {
                pWPL->data->file.dir = STR_SPEC.dir;
                strcpy( pWPL->data->file.name, dateiname );
              }; // if new_profil
              save_profildatei( pWPL );      /*  Profildatei sichern  */
            }
            else
            {
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_DEMO_NOTE_3,buf,sizeof(buf));
              xvt_dm_post_error("%s",buf);
              //xvt_dm_post_note("Speichern bei Demo nicht möglich");
            }

            SaveProfilFile = FALSE;
            
            
            WIN120 = NULL_WIN;
            xvt_vobj_destroy(xdWindow);
        }
        else
        {
          SaveProfilFile = TRUE;
          scr.datensatz = ds_tmp;
          list->GetScrollDaten(&scr);
        }
       }  // -if (ds_info[1] > 1)
       else
       {
         if(!GetFeature( "wsp_nodemo" ))
         {
           char buf[200];//Dick 26.11.99
           xvt_res_get_str(STR_DEMO_NOTE_3,buf,sizeof(buf));
           xvt_dm_post_error("%s",buf);
           //xvt_dm_post_note("Speichern bei Demo nicht möglich");
         }
         SaveProfilFile = FALSE;
         WIN120 = NULL_WIN;
         xvt_vobj_destroy(xdWindow);
         
       }
      }
      break;
      
    case WIN_120_PUSHBUTTON_27: /* "Abbruch" */
      {
        if (dlg_sonderprofil != NULL_WIN)
        {
          xvt_vobj_destroy(dlg_sonderprofil);
          dlg_sonderprofil = NULL_WIN;
        }
        char buf[200],buf2[200],buf3[200],buf4[200];
        xvt_res_get_str(STR_JA,buf,sizeof(buf));
        xvt_res_get_str(STR_NEIN,buf2,sizeof(buf2));
        xvt_res_get_str(STR_ABBRECHEN,buf3,sizeof(buf3));
        xvt_res_get_str(STR_PLOT101_ASK,buf4,sizeof(buf4));
        
        if (SaveStrangFile || SaveNetzFile || SaveProfilFile)
          //switch (xvt_dm_post_ask("Ja","Nein","Abbrechen","Sollen geänderte Daten\ngesichert werden ?"))
          switch(xvt_dm_post_ask(buf,buf2,buf3,buf4))
        {
    case RESP_DEFAULT:       //SICHERN
      {
        if (ds_info[1]>1 && GetFeature( "wsp_nodemo" ))   /*in GELÄNDE müssen mind. 2 Datentupel stehen */
        {						 /* ansonsten wird Profil nicht gespeichert !  */
          int ds_tmp = scr.datensatz;  //Datensatz retten
          if (check_border())
          {
            if (new_profil)   /***  DAGMAR EINGEFÜGT  ***/
            {
              char tmp[15];
              vergleich=FALSE;
              druck=1;
              teste_projekt_profile(druck);
              
              test_line9(tmp);
              
              for (int h=0;h<100;h++)  							 //init.
                profilstr[h]=' ';
              profilstr[99]='\0';
              
              for (h=0;h<(INT)strlen(netz_dat[0]);h++)            //Gewässer
              {
                if ((netz_dat[0][h]=='\n')||(netz_dat[0][h]=='\0'))
                  profilstr[h]=' ';
                else
                  profilstr[h]=netz_dat[0][h];
              }
              
              int a  = 17;                                  //Station
              int len=strlen(tmp);
              len=len-1;
              for(h=len;h>=0;h--,a--)
                profilstr[a]=tmp[h];
              
              a=26;
              for(h=(INT)(strlen(pk)-1);h>=0;h--,a--)             //pk
                profilstr[a]=pk[h];   //Neu Dick 19->26 und minus 10.07.98
              
              a=31;
              for(h=(INT)(strlen(vzk)-1);h>=0;h--,a--)            //vzk
                profilstr[a]=vzk[h];  //Neu Dick 29->31 und minus 10.07.98
              
              
              for(h=0;h<(INT)strlen(zustand);h++)
                profilstr[33+h]=zustand[h];
              
              for (h=0;h<=11;h++)          // Filename
              {
                profilstr[44+h]=dateiname[h];
                uebergabe_name[h]=dateiname[h];
              }
              uebergabe_name[12]='\0';


              if( GetFeature( "wsp_nodemo" ) )
              {
                /*Neu Dick 2.09.98*/
                char *stat_str,*dummy_str;
                float st_km_new,st_km_test;
                SLIST_ELT e;
                int kol=0,pos_gefunden=-1;
                dummy_str=new char[10];
                sscanf(profilstr,"%s%f",dummy_str,&st_km_new);
                for(e=xvt_slist_get_first(prof_datei);e!=NULL;e=xvt_slist_get_next(prof_datei,e))
                {
                  stat_str=xvt_slist_get(prof_datei,e,0L);
                  sscanf(stat_str,"%s%f",dummy_str,&st_km_test);
                  if(st_km_test > st_km_new)
                  {
                    xvt_slist_add_at_pos(prof_datei,kol,profilstr,0L);
                    pos_gefunden=kol;
                    break;
                  }
                  else
                    kol++;
                  
                }
                if(pos_gefunden<0)
                  xvt_slist_add_at_elt(prof_datei,NULL,profilstr,0L);
                delete []dummy_str;
                
                xvt_list_suspend(lwin);
                xvt_list_clear(lwin);
                if (prof_datei != NULL)
                  xvt_list_add(lwin, -1, (char*)prof_datei);   //Ausgabe in Listbox
                xvt_list_resume(lwin);
                /*Ende Neu*/
                // Listbox vorselektieren
              };

              if ((xvt_slist_count(prof_datei))>0)
                xvt_list_set_sel(lwin, 0,  TRUE);
              // -Listbox vorselektieren
              anzahl_profil_dat_entries++;
              
              //  NEU:
              istverzweigt=FALSE;
              teste_str_verzweigt(); //IN VERZWEIG.cpp
              BOOL strang_vorwaerts = GetSortStrangVorwaerts();
              if (((istverzweigt==FALSE) &&(vzk[0]=='0')&&(pk[0]=='0'))||(anzahl_profil_dat_entries<=1))
                sort_new_profil( &strang_anfang, &strang_ende, station208, vzk, pk, uebergabe_name, strang_vorwaerts );
              else
              {                           
                if ( GetSortVerzweigt() )
                  verzweigtes_profil_sortieren( 0, strang_vorwaerts );
                else
                  verzweigtes_profil_sortieren( 1, strang_vorwaerts );
              }
              //  ENDE NEU
              wandle_abstand_in_string();
              StrangUpdateIndex();
              dlg136_get_daten(scroll_position);
            }  //-if(new_profil)...

            if( GetFeature( "wsp_nodemo" ) )
            {
              list->Koord_Update(0);
              
              if( new_profil )
              {
                pWPL->data->file.dir = STR_SPEC.dir;
                strcpy( pWPL->data->file.name, dateiname );
              };
              save_profildatei(pWPL);   /*  Profildatei sichern  */
            }
            else
            {
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_DEMO_NOTE_3,buf,sizeof(buf));
              xvt_dm_post_error("%s",buf);
              //xvt_dm_post_note("Speichern bei Demo nicht möglich");
            }

            SaveProfilFile = FALSE;
            WIN120 = NULL_WIN;
            xvt_vobj_destroy(xdWindow);
            }
            else
            {
              scr.datensatz = ds_tmp;
              SaveProfilFile = TRUE;
              list->GetScrollDaten(&scr);
            }
          } // -if (ds_info[1]>1)
          else
          {
            if( !GetFeature( "wsp_nodemo" ) )
            {
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_DEMO_NOTE_3,buf,sizeof(buf));
              xvt_dm_post_error("%s",buf);
              //xvt_dm_post_note("Speichern bei Demo nicht möglich");
            }
            SaveProfilFile = FALSE;
            WIN120 = NULL_WIN;
            xvt_vobj_destroy(xdWindow);
          }
          }
          break;
         case RESP_2:          // nicht sichere_datenblock = exit
           {
             SaveProfilFile = FALSE;
             WIN120 = NULL_WIN;
             xvt_vobj_destroy(xdWindow);
           }
           break;
         case RESP_3:        //Abbruch= zurück
           {
           }
           break;
       }  //switch
       else
       {
         WIN120 = NULL_WIN;
         xvt_vobj_destroy(xdWindow);
       }
       
      }
      break;
      
      
    case WIN_120_PUSHBUTTON_34: /* "Schlüsseldaten editieren" */
      {
      }
      break;
    case WIN_120_PUSHBUTTON_35: /* "Schrift/Linie editieren" */
      {
      }
      break;
    case WIN_120_LISTBOX_NEU:   /* "Listbox : Auswahl neuer Datensatz"  */
                                /* wird durch Auswahl im Hauptmenu aktiviert
siehe: add_new_datensatz in:util.cpp        */
      
      {/*	List box was operated.*/
        if (xdEvent->v.ctl.ci.v.lbox.dbl_click)
        { 	/*			double click			*/
        }
        else
        {   /*		single click			*/
          SLIST_ELT e;
          SLIST daten;
          daten = xvt_slist_create();
          long *id;
          char *chr;
          bool ax_da=FALSE,ay_da=FALSE,dp_da=FALSE;
          char buf[50];
          list->GetMinMax(&pmm,scr.datensatz);
          
          read_res_datentypen( daten, 0 );
          // Datenblocktypen aus Resource:"wspdlg.rc" lesen
          new_ds.datensatz = xvt_slist_get_elt(xvt_list_get_sel(new_ds.listbox_win),0,0L);
          new_ds.id=0;
          
          for (e=xvt_slist_get_first(daten);e!=NULL;e=xvt_slist_get_next(daten,e))
          {
            chr = xvt_slist_get(daten,e,0L);
            if ( xvt_str_compare_ignoring_case(chr,new_ds.datensatz)==0  )
            {
              id = xvt_slist_get_data(e);
              new_ds.id =(short) *id;
            }
          }
          xvt_slist_destroy(daten);
          
          if ((new_ds.id==FUELLHOEHE)||(new_ds.id==NAU_MED)||(new_ds.id==GAUSSRUECK))
          {
            char buf1[200];//Dick 26.11.99
            xvt_res_get_str(STR_DATENBLOCKTYP_NOTE,buf1,sizeof(buf1));
            xvt_dm_post_error("%s",buf1);
            //xvt_dm_post_note("Datenblocktypen werden erst später implementiert.");
          }
          else
          {
            if (new_ds.listbox_win != NULL_WIN)
            {
              if (new_ds.listbox_win !=NULL_WIN )
                xvt_vobj_destroy(new_ds.listbox_win);
              new_ds.listbox_win=NULL_WIN;
              if (new_ds.id >0)
              {
                if(new_ds.id==AXM || new_ds.id==AYM || new_ds.id==DPM)
                {
                  for(int i=1;i<=ds_info[0];i++)
                  {
                    if(typ[i]==AXM)
                      ax_da=TRUE;
                    if(typ[i]==AYM)
                      ay_da=TRUE;
                    if(typ[i]==DPM)
                      dp_da=TRUE;
                  }
                  
                  if(!ax_da)
                  {
                    if((xvt_res_get_str(AXM+1000,buf,sizeof(buf)))==NULL)   //Kommentar
                      xvt_dm_post_error("Can't read string resource");
                    
                    new_ds.datensatz=buf;                                   
                    new_ds.id=AXM;                                                                      
                    add_new_datensatz(win_list2);   //in:util.cpp
                  }
                  if(!ay_da)
                  {
                    if((xvt_res_get_str(AYM+1000,buf,sizeof(buf)))==NULL)   //Kommentar
                      xvt_dm_post_error("Can't read string resource");
                    
                    new_ds.datensatz=buf;                                   
                    new_ds.id=AYM;                                                                      
                    add_new_datensatz(win_list2);   //in:util.cpp
                  }
                  if(!dp_da)
                  {
                    if((xvt_res_get_str(DPM+1000,buf,sizeof(buf)))==NULL)   //Kommentar
                      xvt_dm_post_error("Can't read string resource");
                    
                    new_ds.datensatz=buf;                                   
                    new_ds.id=DPM;                                                                      
                    add_new_datensatz(win_list2);   //in:util.cpp
                  }
                }
                else
                  add_new_datensatz(win_list2);   //in:util.cpp
              }
              
              scr.scrollpos = 1;
              
              if (ds_info[1]<15)
                scr.anzahl = ds_info[1];
              else	 scr.anzahl = anz;
              
              xvt_sbar_set_pos(scroll_120,HVSCROLL,scr.scrollpos);
              scr.datensatz =  anzahl_ds;
              
              //Neu Dick 15.12.99 Info-Button
              switch (typ[scr.datensatz]) 
              {
              case UK_BRUECKE:                             
              case OK_BRUECKE:
              case OK_WEHRS:
                ShowWindow(hwnd_info,SW_SHOW);
                break;
              default:
                ShowWindow(hwnd_info,SW_HIDE);
              }
              //
              
              if(datensatz_aktuel_120!=scr.datensatz)//Dick  9.08.98
              {
                datensatz_aktuel_120=scr.datensatz; 
                list->Koord_Update(1);//Dick 3.04.2000 0->1 weil es kann passieren daß x koord. noch BCE_NAN sind
              }
              
              list->GetScrollDaten(&scr);
              
              xvt_vobj_set_title(win120_typ,"\0");  //allg. Titel löschen
              win120_loesche_edit_felder();       //3.Spalte löschen
              
              neue_datensatz_wahl = TRUE;
              display_prof120(&win_120[0]);
              //Dick 31.05.99
              if ((typ[scr.datensatz] >GELAENDEHOEHE)&&(typ[scr.datensatz] < MAUL))
              {
                if((typ[scr.datensatz]!=TRENNFLAECHEN)&&(typ[scr.datensatz]!=DURCHST_BEREICH)&&(typ[scr.datensatz]!= BORDVOLL)&&(typ[scr.datensatz]!= BORDVOLL))
                { //Trennfl.+durchst.Ber.ausschliessen
                  if(typ[scr.datensatz]!=KREISSEGM)
                  {
                    SLIST slist;
                    char *str;
                    slist = xvt_slist_create();
                    slist = xvt_list_get_sel(win_list2);
                    str = xvt_slist_get_elt(slist,0,0L);
                    xvt_vobj_set_title(win120_typ,str);
                  }
                  else
                    xvt_vobj_set_title(win120_typ,"\0");
                }
              }
              //
              SaveProfilFile = TRUE;
          }
         } //-if ((new_ds.id==FUELLHOEHE).....
        }
      }
      break;
    case WIN_120_LISTBOX_DEL:   /* "Listbox : Auswahl löschen Datensatz"  */
                                /* wird durch Auswahl im Hauptmenu aktiviert
siehe: add_new_datensatz in:util.cpp        */
      {/*	List box was operated.*/
        if (xdEvent->v.ctl.ci.v.lbox.dbl_click)
        { 	/*			double click			*/
        }
        else
        {   /*		single click			*/
          bool ax_da=FALSE,ay_da=FALSE,dp_da=FALSE;
          char buf[50];
          int j,auswahl;
          if (new_ds.listbox_win != NULL_WIN)
          {
            new_ds.datensatz = xvt_slist_get_elt(xvt_list_get_sel(new_ds.listbox_win),0,0);
            new_ds.id= xvt_list_get_sel_index(new_ds.listbox_win);
            
            char buf1[200],buf2[200],buf3[200],buf4[200],buf5[200];
            xvt_res_get_str(STR_NEIN,buf1,sizeof(buf1));
            xvt_res_get_str(STR_LOESCHEN,buf2,sizeof(buf2));
            xvt_res_get_str(STR_BEWUCHS_DEL_ASK,buf3,sizeof(buf3));
            xvt_res_get_str(STR_DATENSATZ_DEL_ASK,buf4,sizeof(buf4));
            xvt_res_get_str(STR_LOESCHEN2,buf5,sizeof(buf5));
            if(typ[new_ds.id+1]==AXM || typ[new_ds.id+1]==AYM || typ[new_ds.id+1]==DPM)
              auswahl=xvt_dm_post_ask(buf1,buf2,0L,"%s",buf3);
            //auswahl=xvt_dm_post_ask("Nein","Löschen",0L,"Wollen Sie wirklich alle Bewuchsparameter löschen?");                            
            else
              auswahl=xvt_dm_post_ask(buf1,buf2,0L,"%s:\n%s\n%s",buf4,new_ds.datensatz,buf5);
            //auswahl=xvt_dm_post_ask("Nein","Löschen",0L,"Wollen Sie wirklich den Datensatz:\n%s\nlöschen?",new_ds.datensatz);
            switch (auswahl)
            {
            case RESP_DEFAULT:
              break;
            case RESP_2:
              if (typ[new_ds.id+1]==1)
                xvt_dm_post_note("Dieser Datensatz kann nicht gelöscht werden!");
              else
              {
                if(typ[new_ds.id+1]==AXM || typ[new_ds.id+1]==AYM || typ[new_ds.id+1]==DPM)
                {
                  for(j=1;j<=ds_info[0];j++)
                  {
                    if(typ[j]==AXM)
                    {
                      if((xvt_res_get_str(AXM+1000,buf,sizeof(buf)))==NULL)   //Kommentar
                        xvt_dm_post_error("Can't read string resource");
                      new_ds.datensatz=buf;                                   
                      new_ds.id=AXM;                                                                      
                      list->DeleteNode(j,(int*)ds_info, (int*)typ);
                      anzahl_ds=ds_info[0];
                      xvt_list_rem(win_list2,j-1);
                      xvt_list_resume(win_list2);
                      j--;
                    }
                    if(typ[j]==AYM)
                    {
                      if((xvt_res_get_str(AYM+1000,buf,sizeof(buf)))==NULL)   //Kommentar
                        xvt_dm_post_error("Can't read string resource");
                      new_ds.datensatz=buf;                                   
                      new_ds.id=AYM;                                                                      
                      list->DeleteNode(j,(int*)ds_info, (int*)typ);
                      anzahl_ds=ds_info[0];
                      xvt_list_rem(win_list2,j-1);
                      xvt_list_resume(win_list2);
                      j--;
                    }
                    if(typ[j]==DPM)
                    {
                      if((xvt_res_get_str(DPM+1000,buf,sizeof(buf)))==NULL)   //Kommentar
                        xvt_dm_post_error("Can't read string resource");
                      new_ds.datensatz=buf;                                   
                      new_ds.id=DPM;                                                                      
                      list->DeleteNode(j,(int*)ds_info, (int*)typ);
                      anzahl_ds=ds_info[0];
                      xvt_list_rem(win_list2,j-1);
                      xvt_list_resume(win_list2);
                      j--;
                    }
                  }
                  
                  
                }
                else
                {
                  list->DeleteNode(new_ds.id+1,(int*)ds_info, (int*)typ);
                  anzahl_ds=ds_info[0];
                  xvt_list_rem(win_list2,new_ds.id);
                  xvt_list_resume(win_list2);
                }
                xvt_list_set_sel(win_list2,0,TRUE);
                SaveProfilFile = TRUE;
                
                xvt_vobj_set_title(win120_typ,"\0");  //allg. Titel löschen
                win120_loesche_edit_felder();   //3.Spalte löschen
                
                scr.datensatz =  1;
                
                if(datensatz_aktuel_120!=1)//Dick  14.01.99
                {
                  datensatz_aktuel_120=1; 
                  list->Koord_Update(0);
                }
                list->GetScrollDaten(&scr);
                display_prof120(&win_120[0]);
                
                
                
              }
              break;
            case RESP_3:break;
            }
            if (new_ds.listbox_win != NULL_WIN)
            {
              xvt_vobj_destroy(new_ds.listbox_win);
              new_ds.listbox_win=NULL_WIN;
            }
          }
          
          //Neu Dick 15.12.99 Info-Button
          switch (typ[scr.datensatz]) 
          {
          case UK_BRUECKE:                             
          case OK_BRUECKE:
          case OK_WEHRS:
            ShowWindow(hwnd_info,SW_SHOW);
            break;
          default:
            ShowWindow(hwnd_info,SW_HIDE);
          }
          //
        }
      }
      break;
      
      /******************************************************************************/
    case WIN120_EDIT0:
    case WIN120_EDIT1:
    case WIN120_EDIT2:
    case WIN120_EDIT3:
    case WIN120_EDIT4:
    case WIN120_EDIT5:
    case WIN120_EDIT6:
    case WIN120_EDIT7:
    case WIN120_EDIT8:
    case WIN120_EDIT9:
    case WIN120_EDIT10:
    case WIN120_EDIT11:
    case WIN120_EDIT12:
    case WIN120_EDIT13:
    case WIN120_EDIT14:
      if(!Edit_Fehler_Y &&!Edit_Fehler_Z)	{  /*	 Edit control was operated.	*/
        ch_id=xdControlId-WIN120_EDIT0;     // WIN120_EDIT0==3000
        edit_line=ch_id;
        if(!Edit_Fehler|| (Edit_Fehler&& Edit_Fehler_Feld[ch_id]))
        { 
          if (xdEvent->v.ctl.ci.v.edit.focus_change)
          {
            if (xdEvent->v.ctl.ci.v.edit.active)
            {  	/*	+focus has entered the control		*/
              new_koord=FALSE;
              //}
              xvt_ctl_set_text_sel(win_120[ch_id],0,SHRT_MAX);
              xvt_sbar_set_pos(scroll_120,HVSCROLL,scr.scrollpos);
              last_edit = ch_id;
              
              
            } /* -focus has entered the control*/
            else
            {  /* focus has left the control			*/
              {
                if(ch_id + scr.scrollpos > ds_info[1])
                  Einf=1;
                xvt_vobj_get_title(win_120[ch_id],temp,10);
                is_digit = is_zahl(temp);
                if ( is_digit ==-1)  //keine Eingabe
                  temp[0]='\0';
                if ( is_digit ==0)
                {
                  Edit_Fehler=TRUE;
                  Edit_Fehler_Feld[ch_id]=TRUE;
                  char buf[200],buf2[200];//Dick 26.11.99
                  LoadString(NULL,STR_ERROR_REALFORMAT2,buf,sizeof(buf));
                  LoadString(NULL,STR_EINGABEFEHLER,buf2,sizeof(buf2));
                  MessageBox(hwnd120,buf,buf2,MB_OK|MB_ICONWARNING);
                  //MessageBox(hwnd120,"Ungültiges Zahlenformat !","Eingabefehler",MB_OK|MB_ICONWARNING);
                  xvt_scr_set_focus_vobj(win_120[ch_id]);
                }
                if(is_digit>0 || is_digit==-1)
                {
                  double xd,yd;
                  Edit_Fehler=FALSE;
                  Edit_Fehler_Feld[ch_id]=FALSE;
                  Edit_X_120=FALSE;
                  if(Einf && is_digit>0)
                  {
                    xd = atof(temp);
                    if(!checkbox_intr)
                      yd = BCE_NAN;
                    else
                      yd=list->Koord_Interpolieren(xd,xdControlId-3000+scr.scrollpos);
                    
                    list->Koord_Einf(xd,yd,xdControlId-3000+scr.scrollpos);                             
                  }
                  else if(!Einf&&(ch_id + scr.scrollpos <= ds_info[1])) 
                    list->SaveScrollDaten(xdControlId,&scr,temp,K_X);
                  Einf=FALSE;
                  list->GetScrollDaten(&scr);
                  display_prof120(&win_120[0]);
                  xvt_sbar_set_range(scroll_120, HVSCROLL, 1, ds_info[1]-13);//Neu 29.07.98
                  if (scr.anzahl<15)
                    scr.anzahl++;
                  else scr.anzahl =15;
                  GaussRuecksprungCheck();
                  GaussRuecksprungCheckVar=FALSE;
                  //	  last_edit = -1;
                }
              }
            }   /*focus has left the control*/
          }
          else
          {		/*	Contents of control were changed	*/
            SaveProfilFile = TRUE;
            Edit_Fehler=TRUE;
            Edit_Fehler_Feld[ch_id]=TRUE;
            GaussRuecksprungCheckVar=TRUE;
            Edit_X_120=TRUE;
          }
          
        }//if(!Edit_Fehler|| (Edit_Fehler&& Edit_Fehler_Feld[ch_id]))
      }
      break;
      
      /*---   2. Spalte    ------------------------------------------------------------*/
    case WIN120_EDIT15:
    case WIN120_EDIT16:
    case WIN120_EDIT17:
    case WIN120_EDIT18:
    case WIN120_EDIT19:
    case WIN120_EDIT20:
    case WIN120_EDIT21:
    case WIN120_EDIT22:
    case WIN120_EDIT23:
    case WIN120_EDIT24:
    case WIN120_EDIT25:
    case WIN120_EDIT26:
    case WIN120_EDIT27:
    case WIN120_EDIT28:
    case WIN120_EDIT29:
      if(!Edit_Fehler&&!Edit_Fehler_Z)	{	/*	 	Edit control was operated.	  */
        ch_id=xdControlId-WIN120_EDIT0;     // WIN120_EDIT0==3000
        edit_line=ch_id-15;
        if(!Edit_Fehler_Y || Edit_Fehler_Y&&Edit_Fehler_Feld[edit_line])
        {
          if (xdEvent->v.ctl.ci.v.edit.focus_change)
          {
            if (xdEvent->v.ctl.ci.v.edit.active)
            {			/*	  +focus has entered the control		*/
              xvt_ctl_set_text_sel(win_120[ch_id],0,SHRT_MAX);
              if ((last_edit>=0)&&(last_edit<=14))
                if (ch_id-15 != last_edit)
                  if (last_edit + scr.scrollpos > ds_info[1])  // NEU
                  {
                    double xd,yd;
                    
                    xvt_vobj_get_title(win_120[last_edit],temp,10);
                    is_digit = is_zahl(temp);
                    
                    if ( is_digit ==-1)  //keine Eingabe
                      temp[0]='\0';
                    if (is_digit>0)
                    {
                      xd = atof(temp);
                      yd = BCE_NAN;
                      
                      if (scr.anzahl<15)
                        scr.anzahl++;
                      else scr.anzahl =15;
                      
                      list->GetScrollDaten(&scr);
                      
                      display_prof120(&win_120[0]);
                    }
                    new_koord=FALSE;
                  }
                  
                  last_edit = ch_id;
                  
                  
            }     /*  -focus has entered the control     */
            else
            {		/*			focus has left the control			*/
              if (ch_id-15 + scr.scrollpos > ds_info[1])  // NEU
              {
                double xd,yd;
                
                xvt_vobj_get_title(win_120[ch_id],temp,10);
                is_digit = is_zahl(temp);
                if ( is_digit ==-1)  //keine Eingabe
                  temp[0]='\0';
                if ( is_digit ==0)
                {
                  int save=ch_id;   // da xvt_dm_post_note ch_id ändert !?!?
                  temp[0]='\0';
                  xvt_vobj_set_title(win_120[ch_id],"\0"); //	"Ungültiges Zahlenformat: z-Wert !"
                  ch_id=save;
                }
                if (is_digit>0)
                  yd = atof(temp);
                else yd = BCE_NAN;
                
                if (win_120[ch_id-15]!=NULL_WIN)
                  xvt_vobj_get_title(win_120[ch_id-15],temp,10);
                is_digit = is_zahl(temp);
                
                if ( is_digit ==-1)  //keine Eingabe
                  temp[0]='\0';
                if ( is_digit ==0)
                {
                  int save=ch_id;     // da xvt_dm_post_note ch_id ändert !?!?
                  temp[0]='\0';
                  xvt_vobj_set_title(win_120[ch_id-15],temp);
                  //					  xvt_dm_post_note("Ungültiges Zahlenformat: y-Wert !");
                  ch_id=save;
                }
                if (is_digit>0)
                {
                  xd = atof(temp);
                  
                  if (scr.anzahl<15)
                    scr.anzahl++;
                  else scr.anzahl =15;
                  
                  xvt_vobj_set_title(win_120[ch_id],"");
                  if (win_120[ch_id-15]!=NULL_WIN)
                    xvt_vobj_set_title(win_120[ch_id-15],"");
                  
                  list->GetScrollDaten(&scr);
                  display_prof120(&win_120[0]);
                }
                
                new_koord=FALSE;
              }
              else      // y-Koordinate editiert
                if ( ch_id-15 + scr.scrollpos <= ds_info[1] )
                {
                  if (win_120[ch_id]!=NULL_WIN)
                    xvt_vobj_get_title(win_120[ch_id],temp,10);
                  is_digit = is_zahl(temp);
                  if ( is_digit ==0)
                  {
                    char buf[200],buf2[200];//Dick 26.11.99
                    LoadString(NULL,STR_ERROR_REALFORMAT2,buf,sizeof(buf));
                    LoadString(NULL,STR_EINGABEFEHLER,buf2,sizeof(buf2));
                    Edit_Fehler_Feld[edit_line]=TRUE;
                    Edit_Fehler_Y=TRUE;
                    int save_ch=ch_id;
                    MessageBox(hwnd120,buf,buf2,MB_OK|MB_ICONWARNING);
                    xvt_scr_set_focus_vobj(win_120[save_ch]);
                  }
                  if ( is_digit ==-1)  //keine Eingabe
                    temp[0]='\0';
                  
                  //						 if (is_digit>0)
                  if ((is_digit>0 || is_digit==-1)&&!Einf)
                  {
                    list->SaveScrollDaten(xdControlId,&scr,temp,K_Y);
                    Edit_Y_120=FALSE;
                    Edit_Fehler_Y=FALSE;
                    Edit_Fehler_Feld[edit_line]=FALSE;
                  }
                  
                }
                
                
                if ((xdControlId==WIN120_EDIT29)&&(ds_info[1]>=15)&&(ch_id-15 + scr.scrollpos <= ds_info[1]))
                {
                  scr.scrollpos ++;
                  list->GetScrollDaten(&scr);
                  display_prof120(&win_120[0]);
                  xvt_sbar_set_pos(scroll_120,HVSCROLL,scr.scrollpos);
                }
            }
            last_edit = ch_id;
       }
       else
       {		/*			Contents of control were changed		*/
         SaveProfilFile = TRUE;
         Edit_Y_120=TRUE;
         Edit_Fehler_Feld[edit_line]=TRUE;
         Edit_Fehler_Y=TRUE;
       }
            }
      }
      break;
      
      /*-------------------------------------------------- */
    case WIN120_EDIT30:
    case WIN120_EDIT31:
    case WIN120_EDIT32:
    case WIN120_EDIT33:
    case WIN120_EDIT34:
    case WIN120_EDIT35:
    case WIN120_EDIT36:
    case WIN120_EDIT37:
    case WIN120_EDIT38:
    case WIN120_EDIT39:
    case WIN120_EDIT40:
    case WIN120_EDIT41:
    case WIN120_EDIT42:
    case WIN120_EDIT43:
    case WIN120_EDIT44:
      if(!Edit_Fehler&&!Edit_Fehler_Y){   /*  Edit control was operated.      */
        ch_id=xdControlId-WIN120_EDIT0;     //   WIN120_EDIT0 <=3000
        edit_line=ch_id-30;
        if(!Edit_Fehler_Z || Edit_Fehler_Z&&Edit_Fehler_Feld[edit_line])
        {
          if (xdEvent->v.ctl.ci.v.edit.focus_change)
          {
            if (xdEvent->v.ctl.ci.v.edit.active)
            {/*focus has entered the control	*/
              xvt_ctl_set_text_sel(win_120[ch_id],0,SHRT_MAX);
            }
            else
            {  /*	focus has left the control	*/
              if (ch_id-30 + scr.scrollpos <= ds_info[1])
              {
                if (win_120[ch_id]!=NULL_WIN)
                  xvt_vobj_get_title(win_120[ch_id],temp,10);
                is_digit = is_zahl(temp);
                if ( is_digit ==0)
                {
                  char buf[200],buf2[200];//Dick 26.11.99
                  LoadString(NULL,STR_ERROR_REALFORMAT2,buf,sizeof(buf));
                  LoadString(NULL,STR_EINGABEFEHLER,buf2,sizeof(buf2));
                  Edit_Fehler_Feld[edit_line]=TRUE;
                  Edit_Fehler_Z=TRUE;
                  int save_ch=ch_id;
                  MessageBox(hwnd120,buf,buf2,MB_OK|MB_ICONWARNING);
                  xvt_scr_set_focus_vobj(win_120[save_ch]);
                }
                else
                  //						if (is_digit >0)
                {
                  if ((is_digit>0 || is_digit==-1)&&!Einf)
                    list->SaveScrollDaten(xdControlId,&scr,temp,K_Z);
                  list->GetScrollDaten(&scr);
                  display_prof120(&win_120[0]);
                  last_edit =ch_id;
                  Edit_Z_120=FALSE;
                  Edit_Fehler_Z=FALSE;
                  Edit_Fehler_Feld[edit_line]=FALSE;
                }
              }
              else       //Edit-Feld > ds_info
              {
                xvt_vobj_set_title(win_120[ch_id],"");
                xvt_scr_set_focus_vobj(win_120[ch_id-30]);
              }
              
            }
          }
          else
          { 	/*	Contents of control were changed	*/
            SaveProfilFile = TRUE;
            Edit_Z_120=TRUE;
            Edit_Fehler_Feld[edit_line]=TRUE;
            Edit_Fehler_Z=TRUE;
          }
        }
      }
      break;
    case WIN_120_PUSHBUTTON_44: /* "Punkt einfügen" */
      {
        if(win122!=NULL_WIN)
        {
          xvt_vobj_destroy(win122);
          win122=NULL_WIN;
        }
        xdEvent->type=E_CHAR;
        xdEvent->v.chr.ch=K_INS;
        xvt_win_dispatch_event(WIN120,xdEvent);
      }
      break;
    case WIN_120_PUSHBUTTON_45: /* "Datensatz neu" */
      {
        if(win122!=NULL_WIN)
        {
          xvt_vobj_destroy(win122);
          win122=NULL_WIN;
        }
        if (dlg_sonderprofil != NULL_WIN)
        {
          xvt_vobj_destroy(dlg_sonderprofil);
          dlg_sonderprofil = NULL_WIN;
        }
        new_ds = display_new_datensatz(WIN120); //->util.cpp
        
      }
      break;
    case WIN_120_PUSHBUTTON_46: /* "Profil verlängern" */
      {
        ExecProfilverlaengerung(pWPL);
      }
      break;
    case WIN_120_PUSHBUTTON_47: /* "Punkt löschen" */
      {
        //Daten abspeichern ,wo zuletzt editiert wurde
        xdEvent->type =E_CONTROL;
        xdEvent->v.ctl.id=3000 + ch_id;
        xdEvent->v.ctl.ci.v.edit.focus_change = TRUE;
        xdEvent->v.ctl.ci.v.edit.active = FALSE;
        xvt_win_dispatch_event(xdWindow,xdEvent);
        
        xvt_scr_set_focus_vobj(win_120[edit_line]);
        xvt_vobj_set_title(win_120[edit_line],"\0");
        
        xdEvent->type =E_CONTROL;
        xdEvent->v.ctl.id=3000 + ch_id;
        xdEvent->v.ctl.ci.v.edit.focus_change = TRUE;
        xdEvent->v.ctl.ci.v.edit.active = FALSE;
        xvt_win_dispatch_event(xdWindow,xdEvent);
        
        SaveProfilFile = TRUE;
      }
      break;
    case WIN_120_PUSHBUTTON_48: /* "Datensatz löschen" */
      {
        if(win122!=NULL_WIN)
        {
          xvt_vobj_destroy(win122);
          win122=NULL_WIN;
        }
        if (dlg_sonderprofil != NULL_WIN)
        {
          xvt_vobj_destroy(dlg_sonderprofil);
          dlg_sonderprofil = NULL_WIN;
        }
        delete_datensatz(WIN120);
      }
      break;
    case WIN_120_TEDIT_55:
      if (xdEvent->v.ctl.ci.v.textedit.active)
      {
        if(list->ExistDatensatzTyp(COMMENT)==0)
        {
          if(win122!=NULL_WIN)
          {
            xvt_vobj_destroy(win122);
            win122=NULL_WIN;
          }
          if (dlg_sonderprofil != NULL_WIN)
          {
            xvt_vobj_destroy(dlg_sonderprofil);
            dlg_sonderprofil = NULL_WIN;
          }
          
          new_ds.datensatz="Kommentar";
          new_ds.id=COMMENT;
          
          if (new_ds.id>0)
            add_new_datensatz(win_list2);   //in:util.cpp
          
          
        }
        
      }
      else if (xdEvent->v.ctl.ci.v.textedit.focus_change)
      {
        T_PNUM t_pnum;
        T_LNUM t_lpnum;
        char *str,line[100],help[100];
        unsigned t_len;
        int i,j,comment_ds_num=0;
        
        
        for(i=1;i<=ds_info[0];i++)                      
          if(typ[i]==COMMENT)
            comment_ds_num=i;
          if(comment_ds_num!=0)
          {
            xvt_slist_destroy(slist_comment);
            slist_comment = xvt_slist_create();
            t_pnum = xvt_tx_get_num_pars(tx_comment120);
            /*		 t_lnum = xvt_tx_get_num_lines(tx_comment116); */
            
            if (tx_comment120 != BAD_TXEDIT)
              for (i=0;i<=t_pnum-1;i++)
              {
                t_lpnum = xvt_tx_get_num_par_lines(tx_comment120,i);
                for (j=0;j<=t_lpnum-1;j++)
                {
                  xvt_tx_get_line(tx_comment120,i, A_LOCK,j,0);
                  
                  str = xvt_tx_get_line(tx_comment120,i, A_GET,j,&t_len);
                  strncpy(help,str,t_len);
                  help[t_len]='\0';
                  strcpy(line,"CC ");
                  strcat(line,help);
                  strcat(line,"\n");
                  xvt_slist_add_at_elt(slist_comment,NULL,(char*)help,0L);
                  
                  xvt_tx_get_line(tx_comment120,i, A_UNLOCK,j,0);
                }
              }
              
              ds_info[comment_ds_num] = xvt_slist_count(slist_comment);
              
              SaveProfilFile =TRUE;
              
          }
      }
      break;
      /*******************************************************************************/
    default:
      break;
    }
    }
    break;
  case E_FONT:
		/*	User issued font command on window menu bar (menu bar at top of
    screen for Mac/CH).	*/
    {
    }
    break;
  case E_TIMER:
    /*  		Timer associated with window went off.	*/
    {
    }
    break;
  case E_USER:
    /*		Application initiated.	*/
    {
      switch (xdEvent->v.user.id) {
      case E_USER_RAUH:
        {
          if (dlg_sonderprofil !=NULL_WIN)
          {
            xvt_vobj_destroy(dlg_sonderprofil);
            dlg_sonderprofil = NULL_WIN;
          }
          if ((typ[scr.datensatz] != RAUHIGKEIT)&&(typ[scr.datensatz] != RAUHIGKEIT_KST))
          {
            for (int l=0;l<=ds_info[0];l++)
              if ((typ[l]==RAUHIGKEIT)||(typ[l]==RAUHIGKEIT_KST))
              {   /* Rauhigkeit suchen und anwählen */
                scr.datensatz=l;
                xvt_list_set_sel(win_list2,l-1,TRUE);
                
                neue_datensatz_wahl = TRUE;
                list->GetScrollDaten(&scr);      // Daten in scr einlesen
                display_prof120(&win_120[0]);
                last_edit = ch_id;
              }
          }
          /* wenn Rauhigkeit vorhanden ist sie jetzt in jedem Fall ausgewählt */
          if ((typ[scr.datensatz] == RAUHIGKEIT)||(typ[scr.datensatz] == RAUHIGKEIT_KST))
          {
            if (win_dlg_154 == NULL_WIN)
              if (!xvt_dlg_create_res(WD_MODELESS,DLG_154, EM_ALL, DLG_154_eh, 0L))
                xvt_dm_post_error("Can't open dialog 154");
              
              if (win_dlg_154 != NULL_WIN)
              {    /* Edit-Feld:'neuer Wert' in DLG_154 */
                xvt_vobj_set_title(xvt_win_get_ctl(win_dlg_154,DLG_154_EDIT_12),rauheit);
                
                xdEvent->type =E_CONTROL;
                xdEvent->v.ctl.id=DLG_154_EDIT_12;
                xdEvent->v.ctl.ci.v.edit.focus_change = TRUE;
                xdEvent->v.ctl.ci.v.edit.active = TRUE;
                xvt_win_dispatch_event(win_dlg_154,xdEvent);
              }
              
          }
          else 
          {
            char buf[200],buf2[200],buf3[200];
            xvt_res_get_str(STR_DATENSATZ,buf,sizeof(buf));
            xvt_res_get_str(STR_RAUHEIT,buf2,sizeof(buf2));
            xvt_res_get_str(STR_NICHT_VORHANDEN,buf3,sizeof(buf3));
            xvt_dm_post_note("%s: %s\n%s",buf,buf2,buf3);
            //xvt_dm_post_note("Datensatz: Rauheit\nist nicht vorhanden !");
          }
          strcpy(rauheit,"\0");
        }
        break;
      case E_USER_BEWUCHS:
        {
          if (dlg_sonderprofil !=NULL_WIN)
          {
            xvt_vobj_destroy(dlg_sonderprofil);
            dlg_sonderprofil = NULL_WIN;
          }
          if ((typ[scr.datensatz] != AXM)&&(typ[scr.datensatz] != AYM)&&(typ[scr.datensatz] != DPM))
          {
            for (int l=0;l<=ds_info[0];l++)
              if ((typ[l]==AXM)||(typ[l]==AYM)||(typ[l]==DPM))
              {  //den ersten finden
                scr.datensatz=l;
                xvt_list_set_sel(win_list2,l-1,TRUE);
                
                neue_datensatz_wahl = TRUE;
                list->GetScrollDaten(&scr);      // Daten in scr einlesen
                display_prof120(&win_120[0]);
                last_edit = ch_id;
                break;
              }
          }  /* wenn AX,AY,DP vorhanden ist eins jetzt in jedem Fall ausgewählt */
          if ((typ[scr.datensatz] == AXM)||(typ[scr.datensatz] == AYM)||(typ[scr.datensatz] == DPM))
          {
            if (win_dlg_110 == NULL_WIN)
              if (!xvt_dlg_create_res(WD_MODELESS,DLG_110, EM_ALL, DLG_110_eh, 0L))
                xvt_dm_post_error("Can't open dialog 110");
              
              if (win_dlg_110 != NULL_WIN)
              {    /* Edit-Feld:'neuer Wert' in DLG_110 */
                char wert[15];
                switch (typ[scr.datensatz])
                {
                case AXM:
                  strcpy(wert,ax);
                  break;
                case AYM:
                  strcpy(wert,ay);
                  break;
                case DPM:
                  strcpy(wert,dp);
                  break;
                default:
                  strcpy(wert,"\0");
                  break;
                };
                if (win_dlg_110 !=NULL_WIN)
                  xvt_vobj_set_title(xvt_win_get_ctl(win_dlg_110,DLG_110_EDIT_12),wert);
                
                xdEvent->type =E_CONTROL;
                xdEvent->v.ctl.id=DLG_110_EDIT_12;
                xdEvent->v.ctl.ci.v.edit.focus_change = TRUE;
                xdEvent->v.ctl.ci.v.edit.active = TRUE;
                xvt_win_dispatch_event(win_dlg_110,xdEvent);
              }
          }
          else 
          {
            char buf[200],buf2[200],buf3[200];
            xvt_res_get_str(STR_DATENSATZ,buf,sizeof(buf));
            xvt_res_get_str(STR_AX_AY_DP,buf2,sizeof(buf2));
            xvt_res_get_str(STR_NICHT_VORHANDEN,buf3,sizeof(buf3));
            xvt_dm_post_note("%s: %s\n%s",buf,buf2,buf3);
            //xvt_dm_post_note("Datensatz: AX, AY, DP\nist nicht vorhanden !");
          }
        }
        break;
      case E_USER_UEBERFALLBEIWERT:
        {
          
          if ((typ[scr.datensatz] != OK_BRUECKE)&&(typ[scr.datensatz] != OK_WEHRS))
          {
            if (dlg_sonderprofil !=NULL_WIN)
            {
              xvt_vobj_destroy(dlg_sonderprofil);
              dlg_sonderprofil = NULL_WIN;
            }
            
            for (int l=0;l<=ds_info[0];l++)
              if ((typ[l]==OK_BRUECKE)||(typ[l]==OK_WEHRS))
              {  //den ersten finden
                scr.datensatz=l;
                xvt_list_set_sel(win_list2,l-1,TRUE);
                
                neue_datensatz_wahl = TRUE;
                list->GetScrollDaten(&scr);      // Daten in scr einlesen
                display_prof120(&win_120[0]);
                last_edit =ch_id;
                xvt_dwin_invalidate_rect(WIN_117,0);
                break;
              }
          }  /* wenn OK_BRUECKE,OK_WEHRS vorhanden ist eins jetzt in jedem Fall ausgewählt */
          if ((typ[scr.datensatz] == OK_BRUECKE)||(typ[scr.datensatz] == OK_WEHRS))
          {
            if (typ[scr.datensatz] == OK_BRUECKE)
            {
              if (win_dlg_107 == NULL_WIN)
                if (!xvt_dlg_create_res(WD_MODELESS,DLG_107, EM_ALL, DLG_107_eh, 0L))
                  xvt_dm_post_error("Can't open dialog 107");
                
                if (win_dlg_107 != NULL_WIN)
                {    
                  if (win_dlg_107 !=NULL_WIN)
                    xvt_vobj_set_title(xvt_win_get_ctl(win_dlg_107,DLG_107_EDIT),ueberfalbeiwert);
                  
                  xdEvent->type =E_CONTROL;
                  xdEvent->v.ctl.id=DLG_107_EDIT;
                  xdEvent->v.ctl.ci.v.edit.focus_change = TRUE;
                  xdEvent->v.ctl.ci.v.edit.active = TRUE;
                  xvt_win_dispatch_event(win_dlg_107,xdEvent);
                }
            }
            if (typ[scr.datensatz] == OK_WEHRS)
            {
              if (win_dlg_160 == NULL_WIN)
                if (!xvt_dlg_create_res(WD_MODELESS,DLG_160, EM_ALL, DLG_160_eh, 0L))
                  xvt_dm_post_error("Can't open dialog 160");
                
                if (win_dlg_160 != NULL_WIN)
                {    
                  if (win_dlg_160 !=NULL_WIN)
                    xvt_vobj_set_title(xvt_win_get_ctl(win_dlg_160,DLG_160_EDIT_20),ueberfalbeiwert);
                  
                  xdEvent->type =E_CONTROL;
                  xdEvent->v.ctl.id=DLG_160_EDIT_20;
                  xdEvent->v.ctl.ci.v.edit.focus_change = TRUE;
                  xdEvent->v.ctl.ci.v.edit.active = TRUE;
                  xvt_win_dispatch_event(win_dlg_160,xdEvent);
                }
            }
          }
          else 
          {
            char buf[200],buf2[200],buf3[200];
            xvt_res_get_str(STR_DATENSATZ,buf,sizeof(buf));
            xvt_res_get_str(STR_OK_BR_W,buf2,sizeof(buf2));
            xvt_res_get_str(STR_NICHT_VORHANDEN,buf3,sizeof(buf3));
            xvt_dm_post_note("%s: %s\n%s",buf,buf2,buf3);
            //xvt_dm_post_note("Datensatz: OK_BRUECKE  oder OK_WEHRS\nist nicht vorhanden !");
          }
        }
        break;
      case -1:
      default:
        break;
    }
    }
    break;
  default:
    break;
  }
  xvt_tx_process_event(xdWindow, xdEvent);
  return 0L;
}
