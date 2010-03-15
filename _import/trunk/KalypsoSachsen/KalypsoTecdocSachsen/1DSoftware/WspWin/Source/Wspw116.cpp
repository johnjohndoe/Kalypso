/* WspW116.cpp */

/*
Handler for window WIN_GRAPH_116 
("Grafisch / interaktiver Editor")

*/

#include <windows.h>
#include "xvt.h"

#include "wspwin.h"
#include "resource.h"
#include "wsphilfe.h"		// GHJ
#include "l_typen.h"
#include "typen.h"

#include "global_types.h"
#include "global_vars.h"
#include "..\..\wspdlg\include\export.h"

#include "util.h"
#include "list.h"
#include "dis_prof.h"
#include "bce_allg.h"
#include "paint.h"
#include "readprof.h"
#include "printer.h"
#include "profverl.h"

// Definitionen

#define LEFT  1
#define RIGHT 2

#define NEU  0

//#define WIN_RES_ID WIN_GRAPH_116
//#define WIN_FLAGS 0x1083L
//#define WIN_CLASS ""
//#define WIN_BORDER W_DOC


//********************************************

// globale Variablen

BOOLEAN checkbox=FALSE, checkbox_alle = FALSE, checkbox_edit = FALSE, checkbox_edit_v = FALSE, 
checkbox_edit_h = FALSE, checkbox_intr = FALSE, checkbox_spiegel = FALSE;
HWND hwnd_intr = NULL, hwnd_info = NULL, hwnd_spiegel=NULL, hwnd_interpol = NULL;

WINDOW Edit_Win116[15], win116_typ; // Static-Text 3.Spalte: Typ

char ueberfalbeiwert[15];
UINT nUeberfallbeiwertUpdateMsg = ::RegisterWindowMessage("WspDlgUeberfallbeiwertUpdateMsg");
UINT nRauheitswertUpdateMsg = ::RegisterWindowMessage("WspDlgRauheitswertUpdateMsg");
UINT nBewuchswertUpdateMsg = ::RegisterWindowMessage("WspDlgBewuchswertUpdateMsg");

char rauheit[20];
char ax[13],ay[13],dp[13];

BOOLEAN stop116;
HWND hwnd116 = NULL;
WINDOW win_116_zoom_scroll;
RCT rect_116, rct_tmp;
BOOLEAN Edit_Fehler = FALSE, Edit_Fehler_Y = FALSE, Edit_Fehler_Z = FALSE, Edit_Fehler_Feld[15], 
Edit_X = FALSE, Edit_Y = FALSE, Edit_Z = FALSE;

int win_id = 0, id = 2000, n_edit = 5;

WINDOW xdedit6, xdedit7, xdedit8, xdedit9,xdedit10,xdedit11, xdlist1,
ctl_win,  //=Checkbox auswahl
ctl_win_alle,  //=Checkbox alle anzeigen
ctl_win_edit,  //=Checkbox Edit
ctl_win_edit_v,  //=Checkbox Edit Vertikal
ctl_win_edit_h,  //=Checkbox Edit Horizontal
scroll_116, ctl_win_intr, ctl_win_spiegel; //=Checkbox Spiegebild Bley 20.10.2000

int draw_liste[50];  //welche Datensätze sollen graphisch dargestellt werden
int draw_typ_liste_allg[200];
MinMax pmm;
int mark_pos;
char* pt;
bool create_116 = TRUE;
TXEDIT tx_comment116;
SLIST ber_var_text = NULL;
BOOLEAN Einf;
int last_edit_116 = -1, ch_id_116 = -1, ch_id_116_save = -1;

int edit_line=0;
bool back_tab=FALSE;
int thumb;            //Scroll-Thumb-Position
buhne sonderbauwerk;
BOOLEAN GaussRuecksprungCheckVar=FALSE;//Flag für den Start der Funktion zur Überprüfung
int datensatz_aktuel=1;
int limit_up = 0,limit_down = 0;   // Ende Scrollen Zoom

// globale externe Variablen

extern BOOLEAN berechnen,editieren;
extern BOOLEAN bewuchs_alle;

extern MENU_ITEM* main_menu_tree;

extern neuer_datensatz new_ds;
extern int anz;
extern MMP mmp;
extern ZOOM zoom_info;

extern double sf;  // Fensterfaktor
extern BOOLEAN neue_datensatz_wahl;
extern WINDOW win_list2;    //Listbutton3
extern WSP_PROFIL_LISTE *pWPL;
extern char temp[100];
char str[25];
extern BOOL GaussProfilMitRuecksprung;//Globale Erkennung

extern WINDOW dlg_sonderprofil, win_dlg_154, win_dlg_110, dlg_166, win_120[100], win_dlg_107, win_dlg_160, dlg_zoom_1;

extern BOOLEAN editor_closed;
extern int aktuelles_editfeld;

extern char is_digit;    //-->

extern WINDOW edit_buhne[6], radio_buhne[2];           
BOOLEAN ZOOM_IN = FALSE;


// Vorwärtsdeklarationen

void GaussRuecksprungCheck( void );//Funktion zur Überprüfung

// ZOOM_DIALOG

long XVT_CALLCONV1
#if XVT_CC_PROTO
ZOOM_1_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
ZOOM_1_eh XVT_CALLCONV2 (xdWindow, xdEvent)
WINDOW xdWindow;
EVENT *xdEvent;
#endif
{
  switch (xdEvent->type) 
  {
  case E_CLOSE:
    xvt_vobj_destroy(xdWindow);
    break;
  default:
    break;
  }
  return 0L;
}

/*************   GHJ   *************/
static WNDPROC defWndProc;
LRESULT CALLBACK Win116WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
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
        case WIN_GRAPH_116_PUSHBUTTON_43: // "Punkt einfügen"
        case WIN_GRAPH_116_PUSHBUTTON_46: // "Punkt löschen"
          xvt_help_display_topic(hi, HID_KAPITEL_6_3_1);
          break;
          
        case WIN_GRAPH_116_PUSHBUTTON_44: // "Datensatz einfügen"
        case WIN_116_LISTBOX_NEU:   // "Listbox : Auswahl neuer Datensatz"
          xvt_help_display_topic(hi, HID_KAPITEL_4_4_3_1);
          break;
          
        case WIN_GRAPH_116_PUSHBUTTON_47: // "Datensatz löschen"
        case WIN_116_LISTBOX_DEL:   // "Listbox : Auswahl löschen Datensatz"
          xvt_help_display_topic(hi, HID_KAPITEL_6_3_2);
          break;
          
        case WIN_GRAPH_116_PUSHBUTTON_48 :    //  Zoomen....
        case WIN_GRAPH_116_PUSHBUTTON_49:   // Normal 100%
          xvt_help_display_topic(hi, HID_KAPITEL_6_3_3);
          break;
          
        case WIN_GRAPH_116_CHECKBOX_50: // "Edit "
        case WIN_GRAPH_116_CHECKBOX_51: // "Edit Horizontal"
        case WIN_GRAPH_116_CHECKBOX_52: // "Edit Vertikal"
          xvt_help_display_topic(hi, HID_KAPITEL_6_3_4);
          break;
          
        case WIN_GRAPH_116_LISTBUTTON_33:
          switch (typ[scr.datensatz])
          {
          case SOHLHOEHE:     
          case GELAENDEHOEHE:     
          case SOHLHOEHE_2:
            xvt_help_display_topic(hi, HID_KAPITEL_4_4_2_1);
            break;
            
          case GELAENDE2:
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
            
          case TRAPEZ:
            xvt_help_display_topic(hi, HID_KAPITEL_5_3_1);
            break;
            
          default:
            if(berechnen && editieren)
              xvt_help_display_topic(hi, HID_KAPITEL_4_8_1);
            else
              xvt_help_display_topic(hi, HID_KAPITEL_6_3);
            break;
          }
          break;
          case WIN_GRAPH_116_PUSHBUTTON_42://Grafik drucken
            xvt_help_display_topic(hi, HID_KAPITEL_4_8_2);
            break;
          case WIN_GRAPH_116_PUSHBUTTON_45:  /* Profilverlängerung*/
            xvt_help_display_topic(hi, HID_KAPITEL_7_3_3);
            break;
          default:
            if(berechnen && editieren)
              xvt_help_display_topic(hi, HID_KAPITEL_4_8_1);
            else
              xvt_help_display_topic(hi, HID_KAPITEL_6_3);
            break;
        }
      }
    }
    
    break;
    
    case WM_LBUTTONDOWN:
    case WM_LBUTTONUP:
    case WM_MBUTTONDOWN:
    case WM_MBUTTONUP:
    case WM_MOUSEMOVE:
    case WM_RBUTTONDOWN:
    case WM_RBUTTONUP:          //hwnd116ToolTip   TTM_GETTEXT 
    case WM_NOTIFY :
      break;
      
    case WM_CHAR:
      {
        int sstop=0;
        switch ((TCHAR) wParam)
        {
        case K_INS :
          int sstop2=0;
          break;
        }
      }
      break;
    case WM_COMMAND:        
      {
        switch(LOWORD(wParam))
        {
        case WIN_GRAPH_116_CHECKBOX_35:
          {
            checkbox_intr=(BOOLEAN)::SendMessage(hwnd_intr, BM_GETCHECK, 0, 0L);
            ::SetFocus(hwnd);                    
          }
          break;
          //Bley 20.10.2000 Anfang
        case WIN_GRAPH_116_CHECKBOX_38:
          {
            checkbox_spiegel=(BOOLEAN)::SendMessage(hwnd_spiegel, BM_GETCHECK, 0, 0L);
            ::SetFocus(hwnd);
            list->spiegelbild();
            xvt_dwin_invalidate_rect(WIN_117,0); //Fenster updaten
            list->GetScrollDaten(&scr);
            display_prof116(&Edit_Win116[0]);			// GHJ
          }
          break;
          //Bley 20.10.2000 Ende
        case WIN_GRAPH_116_PUSHBUTTON_37:
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
        case WIN_GRAPH_116_PUSHBUTTON_39: //Gauss-Krüger-Interpolation
          {
            list->GaussKruegerInterpolieren(&scr);
            SaveProfilFile=TRUE;
            list->Koord_Update(0);
            
            list->GetScrollDaten(&scr);
            
            display_prof116(&Edit_Win116[0]);			// GHJ
            xvt_scr_set_focus_vobj(Edit_Win116[0]);//den Focus auf 1.Editfeld setzen
          }
          break;
        }
      }
      break;    
    default:
      break;
  }
  if (uMsg==nUeberfallbeiwertUpdateMsg)
  {
    char *temp;
    temp=(char*)lParam;
    strcpy(ueberfalbeiwert,temp);
    EVENT *xdEvent;
    xdEvent= new EVENT;
    xdEvent->type =E_USER;
    xdEvent->v.user.id=E_USER_UEBERFALLBEIWERT;
    xvt_win_dispatch_event(WIN_116,xdEvent);
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
    xvt_win_dispatch_event(WIN_116,xdEvent);
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
    xvt_win_dispatch_event(WIN_116,xdEvent);
    delete xdEvent;
  }
  
  return CallWindowProc(defWndProc, hwnd, uMsg, wParam, lParam);
}
/**************************************/


/*
Handler for window WIN_GRAPH_116 ("Grafisch / interaktiver Editor")
*/
long XVT_CALLCONV1 WIN_GRAPH_116_eh XVT_CALLCONV2( WINDOW xdWindow, EVENT* xdEvent )
{
  short xdControlId = xdEvent->v.ctl.id;
  
  if( xdEvent->type != E_UPDATE )
    stop116 = TRUE;

  switch( xdEvent->type ) 
  {
  case E_CREATE:
    {
      char buf[200],buf2[200];
      /*************   GHJ   *************/
      defWndProc = (WNDPROC)GetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC);
      SetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC, (LONG)&Win116WindowProc);
      /***********************************/
      xvt_menu_set_tree(xdWindow,main_menu_tree);
      
      xvt_menu_set_item_enabled(xdWindow, TASK_MENUBAR_8, TRUE);
      //	2 Edit_Fenster  disabled setzen -----------
      xvt_vobj_set_enabled(xvt_win_get_ctl(xdWindow, WIN_GRAPH_116_EDIT_2),FALSE);
      xvt_vobj_set_enabled(xvt_win_get_ctl(xdWindow, WIN_GRAPH_116_EDIT_9),FALSE);
      //--------------------------------------------
      set_menu_116(xdWindow,FALSE); // in :  DLL
      
      if(!(berechnen&&editieren))
      {
        xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_13_60,TRUE);   //Bearbeiten-Geländeverknüpfung
        xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_13_64,TRUE);   //Bearbeiten-Flächenberechnung
        xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_50,TRUE);
      } 
      else
      {
        xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_13_60,FALSE);   //Bearbeiten-Geländeverknüpfung
        xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_13_64,FALSE);   //Bearbeiten-Flächenberechnung
      } 
      
      xvt_menu_update(xdWindow);
      
      if(berechnen&&editieren)
      {
        xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_30,TRUE);   //Vergleichsdaten								
        xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_30_12,FALSE);//Tabelle
        xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_30_24,FALSE);//Laengsschnitt einsehen
        xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_1_86,FALSE);//Ergebnisse der Massenberechnung
        xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_30_84_87,FALSE);//Editor
        if( LWA_PROJEKT )
          xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_30_32,FALSE);//Auswerten(h)
      }
      else
      {				
        xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_30,FALSE);   //Vergleichsdaten
        xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_30_29,FALSE);//Ergebnisse(H)				
      }
      xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_50_127,FALSE); // Menu: Zustände
      xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_50_128,FALSE); // Menu: Zustand speichern als
      xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_13_42,FALSE); // Menu: Rauheiten ändern
      xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_14_16,FALSE); // Menu: Abflussdatei
      xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_14_17,FALSE); // Menu: Verlustdatei			
      xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_13_59,FALSE); // Menu:Interpolieren
      
      
      xvt_menu_update(xdWindow);            
      
      if( !LWA_PROJEKT )
      {
        MENU_ITEM *men;
        men=xvt_menu_get_tree(xdWindow);
        men[3].child[3].enabled=0;
        men[1].child[10].enabled=1;
        men[1].child[8].enabled=0;
        men[1].child[11].enabled=0;
        xvt_menu_set_tree(xdWindow,men);
      };
      
      hwnd116 =(HWND) xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW); 
      WIN_116 = xdWindow;
      zoom_info.level = 0;
      win_116_zoom_scroll=NULL_WIN;
      
      // Fensterskalierungsfaktor berechnen
      long faktor=xvt_vobj_get_attr(NULL_WIN,ATTR_CTL_STATIC_TEXT_HEIGHT);
      
      if (sf !=1)
      {
        RCT screen_rct;
        
        screen_rct.top=(short) (616*sf)-50;  /*OK*/
        screen_rct.left=(short)(792*sf);
        screen_rct.bottom= screen_rct.top+(short)(30*sf);
        screen_rct.right=  screen_rct.left+(short)(137*sf);
        xvt_vobj_move(xvt_win_get_ctl(xdWindow,WIN_GRAPH_116_PUSHBUTTON_26),&screen_rct);
        
        screen_rct.top= (short)(616*sf);  /*Abbruch*/
        screen_rct.left=(short)(792*sf);
        screen_rct.bottom= screen_rct.top +(short)(30*sf);
        screen_rct.right=  screen_rct.left+(short)(137*sf);
        xvt_vobj_move(xvt_win_get_ctl(xdWindow,WIN_GRAPH_116_PUSHBUTTON_27),&screen_rct);
      }
      
      sf=1.0;
      /*************  Static-Text Feld:  Titel 3.Spalte **************/
      xvt_vobj_get_outer_rect(xvt_win_get_ctl(xdWindow,WIN_GRAPH_116_TEXT_37),&rct_tmp); 
      
      xvt_rect_set(&rect_116,(short)(rct_tmp.right+1),(short)(rct_tmp.top),(short)(rct_tmp.right+251),(short)(rct_tmp.bottom));
      xvt_ctl_create(WC_TEXT, &rect_116, 0L, xdWindow,0L,0L,WIN116_EDIT_TYP);
      win116_typ=xvt_win_get_ctl(xdWindow,WIN116_EDIT_TYP);
      
      for (int e=0;e<=14;e++)
      {
        Edit_Win116[e]=NULL_WIN;
        Edit_Fehler_Feld[e]=FALSE;
      }
      
      new_profil = FALSE;
      is_profil_open=TRUE;
      
      if (ds_info[1] < 5)
        n_edit = ds_info[1];
        /********************************************************
        *             X-Y Editfenster erzeugen                 *
      ********************************************************/
      win_id = 0;
      for( int j = 0; j <= 1; j++ )
      {
        for( int i = 1; i <= 5; i++ )
        {
          short left = short( ( 528 + j * 135 ) * sf ); // x=528 ,Breite: 135
          short top = short( ( 24 + i * 30 ) * sf );  // Anfang: y=54
          short right = short( ( 663 + j * 135 ) * sf ); 
          short bottom = short( ( 54 + i * 30 ) * sf ); // left*top*right*bottom

          // das edit Fenster
          xvt_rect_set( &rect_116, left, top, right, bottom );
          Edit_Win116[win_id] = xvt_ctl_create( WC_EDIT, &rect_116, 0L, xdWindow,0L,0L,id + win_id );
          win_id++;
        }
      }
      /***********************************************************
      *             SCROLLBAR RECHTS 2-spaltig ´                *
      ************************************************************/
      xvt_rect_set(&rect_116,(short)(798*sf),       // left*top*right*bottom
        (short)(54 *sf),
        (short)(819*sf),
        (short)(204*sf));
      scroll_116 = xvt_ctl_create(WC_VSCROLL, &rect_116, 0L, xdWindow, 0L,0L,2050);
      
      xvt_sbar_set_range(scroll_116, HVSCROLL, 1, ds_info[1]-3);//Neu  5->3 2.07.98
      xvt_sbar_set_pos(scroll_116,HVSCROLL,1);
      
      scr.scrollpos=1;
      scr.anzahl=n_edit;
      scr.datensatz=1;
      
      for (int i=0;i<=49;i++)  draw_liste[i] =0;
      for (i=0;i<200;i++)  draw_typ_liste_allg[i] =0;
      draw_liste[0]=1;
      draw_liste[1]=1;
      
      list->GetMinMax(&pmm,scr.datensatz);
      
      list->GetScrollDaten(&scr);
      neue_datensatz_wahl = TRUE;
      xdedit10=xvt_win_get_ctl(WIN_116,WIN_GRAPH_116_EDIT_39); //P-typ
      
      // ***************  LISTBUTTON   **************************
      xdlist1=xvt_win_get_ctl(xdWindow,WIN_GRAPH_116_LISTBUTTON_33);
      win_list2 = xdlist1;   //für lösche Datensatz
      pWPL->window = xdlist1; //zum updaten von Listbutton33 in DLL
      //nach Geländeverknüpfung->Fkt:Connect_Prof_2Gel2()
      
      list->GetDateninfo(xdlist1);     //in:title_list
      
      
      /***********************************************************/
      // Grafikfenster 117  ausgeben  /Profil zeichnen
      
      
      WIN_117=xvt_win_create_res(WIN_GRAFIK_117, xdWindow, EM_ALL, WIN_GRAFIK_117_eh, 0L);
      if (WIN_117 == NULL_WIN)
        xvt_dm_post_error("Can't open _window117");
      
      mark_pos=0;
      paint->draw_marker(WIN_117,mark_pos);
      
      xdedit6=xvt_win_get_ctl(xdWindow,WIN_GRAPH_116_EDIT_6);   // Name
      xdedit7=xvt_win_get_ctl(xdWindow,WIN_GRAPH_116_EDIT_7);   // km
      xdedit8=xvt_win_get_ctl(xdWindow,WIN_GRAPH_116_EDIT_8);   //VZK
      xdedit9=xvt_win_get_ctl(xdWindow,WIN_GRAPH_116_EDIT_9);   //Profilkenng.
      
      xdedit11=xvt_win_get_ctl(xdWindow,WIN_GRAPH_116_EDIT_41);   //Zustand
      
      
      pt=xvt_slist_get_elt(header_profil,5,0);      //Name
      xvt_vobj_set_title(xdedit6,netz_dat[0]);
      test_line9(str);                              //Station
      xvt_vobj_set_title(xdedit7,str);
      
      pt=xvt_slist_get_elt(header_profil,4,0);     //vzk
      xvt_vobj_set_title(xdedit8,vzk);
      
      pt=xvt_slist_get_elt(header_profil,7,0);     //pk
      xvt_vobj_set_title(xdedit9,pk);
      
      xvt_vobj_set_title(xdedit11, zustand);    //Zustand
      
      xvt_res_get_str(STR_GAUSSRUECK,buf,sizeof(buf));
      xvt_res_get_str(STR_OFF_PROFIL,buf2,sizeof(buf2));
      
      if(GaussProfilMitRuecksprung)
        strcpy(temp,buf);
      else if (! list->GetSonderProfilTyp(&temp[0]))  // Profiltyp
        strcpy(temp,buf2);
      xvt_vobj_set_title(xdedit10,temp);
      create_116=TRUE;//Dick 21.04.99
      buf[0]='\0';
      xvt_res_get_str(STR_CHECKBOX_AUSG_DAT,buf,sizeof(buf));
      //Dick 23.07.99
      RECT  r;
      POINT point;
      point.x = 0;
      ctl_win = xvt_win_get_ctl(xdWindow, WIN_GRAPH_116_CHECKBOX_34);
      xvt_vobj_set_title(ctl_win,buf);//Dick 23.07.99
      ::GetWindowRect((HWND)xvt_vobj_get_attr(ctl_win,ATTR_NATIVE_WINDOW), &r);
      point.x = r.left;
      point.y = r.top;
      ::ScreenToClient(::GetParent((HWND)xvt_vobj_get_attr(ctl_win,ATTR_NATIVE_WINDOW)),&point);
      ::MoveWindow((HWND)xvt_vobj_get_attr(ctl_win,ATTR_NATIVE_WINDOW), point.x, point.y, r.right-r.left+30, r.bottom-r.top, TRUE);
      //
      xvt_ctl_set_checked(ctl_win, FALSE);
      checkbox=FALSE;
      xvt_scr_set_focus_vobj(Edit_Win116[0]);//den Focus auf dem SCROLLBAR setzen damit der Grafik sich aktualiesiert
      xvt_vobj_set_visible(ctl_win,TRUE);
      buf[0]='\0';
      xvt_res_get_str(STR_CHECKBOX_ALLE_DAT,buf,sizeof(buf));
      xvt_rect_set(&rect_116,          //checkbox alle;
        (short)(656),
        (short)(276),
        (short)(880),
        (short)(296));
      xvt_ctl_create(WC_CHECKBOX,&rect_116,buf,xdWindow,0,NULL,WIN_GRAPH_116_CHECKBOX_36);
      ctl_win_alle = xvt_win_get_ctl(xdWindow, WIN_GRAPH_116_CHECKBOX_36);
      checkbox_alle=FALSE;
      
      ////////CHECKBOX////////////
      
      buf[0]='\0';
      xvt_res_get_str(STR_CHECKBOX_Z_WERT,buf,sizeof(buf));
      
      hwnd_intr=CreateWindow("BUTTON",buf,BS_AUTOCHECKBOX  | BS_MULTILINE |WS_CHILD|WS_VISIBLE  ,304,86,104,42,
        hwnd116,(HMENU)WIN_GRAPH_116_CHECKBOX_35,(HINSTANCE)GetWindowLong(hwnd116,GWL_HINSTANCE),NULL);
      checkbox_intr=FALSE;
      
      //Info-Button    WIN_GRAPH_116_LISTBUTTON_33   (HINSTANCE)GetWindowLong(hwnd116,GWL_HINSTANCE)
      buf[0]='\0';
      RECT info_rect,w116_rect;
      HBITMAP info_bm;
      info_bm=(HBITMAP)LoadImage((HINSTANCE)GetWindowLong(hwnd116,GWL_HINSTANCE),MAKEINTRESOURCE(IDB_FRAGE1),IMAGE_BITMAP,0,0,LR_CREATEDIBSECTION);
      ::GetWindowRect((HWND)xvt_vobj_get_attr(xdlist1,ATTR_NATIVE_WINDOW), &info_rect);
      ::GetWindowRect(hwnd116, &w116_rect);
      
      hwnd_info=CreateWindow("BUTTON",buf,BS_PUSHBUTTON | BS_BITMAP |WS_CHILD  ,info_rect.right-w116_rect.left,info_rect.top-w116_rect.top-w116_rect.top/2,20,20,
        hwnd116,(HMENU)WIN_GRAPH_116_PUSHBUTTON_37,(HINSTANCE)GetWindowLong(hwnd116,GWL_HINSTANCE),NULL);
      HANDLE iimage=(HWND)SendMessage(hwnd_info,BM_SETIMAGE,(WPARAM)IMAGE_BITMAP,(LPARAM)(HANDLE)info_bm);
      //
      /****Interpolation Gauss-Krüger**********/
      buf[0]='\0';
      //                RECT info_rect,w116_rect;
      HBITMAP inter_bm;
      inter_bm=(HBITMAP)LoadImage((HINSTANCE)GetWindowLong(hwnd116,GWL_HINSTANCE),MAKEINTRESOURCE(IDB_INTERPOL1),IMAGE_BITMAP,0,0,LR_CREATEDIBSECTION);
      ::GetWindowRect((HWND)xvt_vobj_get_attr(xdlist1,ATTR_NATIVE_WINDOW), &info_rect);
      ::GetWindowRect(hwnd116, &w116_rect);
      
      hwnd_interpol=CreateWindow("BUTTON",buf,BS_PUSHBUTTON | BS_BITMAP |WS_CHILD  ,info_rect.right-w116_rect.left,info_rect.top-w116_rect.top-w116_rect.top/2,20,20,
        hwnd116,(HMENU)WIN_GRAPH_116_PUSHBUTTON_39,(HINSTANCE)GetWindowLong(hwnd116,GWL_HINSTANCE),NULL);
      iimage=(HWND)SendMessage(hwnd_interpol,BM_SETIMAGE,(WPARAM)IMAGE_BITMAP,(LPARAM)(HANDLE)inter_bm);
      
      
      /**********Ende Interpolation Gauss-Krüger******/
      
      /*jetzt aus XVT-Desing*/
      ctl_win_edit = xvt_win_get_ctl(xdWindow, WIN_GRAPH_116_CHECKBOX_50);
      xvt_ctl_set_checked(ctl_win_edit, FALSE);
      checkbox_edit=FALSE;
      if(berechnen&&editieren)
      {
        xvt_vobj_set_visible(ctl_win_edit,FALSE);
        xvt_vobj_set_visible(xvt_win_get_ctl(xdWindow, WIN_GRAPH_116_PUSHBUTTON_43),FALSE);//Punkt einf.
        xvt_vobj_set_visible(xvt_win_get_ctl(xdWindow, WIN_GRAPH_116_PUSHBUTTON_44),FALSE);//Datensatz neu
        xvt_vobj_set_visible(xvt_win_get_ctl(xdWindow, WIN_GRAPH_116_PUSHBUTTON_45),FALSE);//Profilverl.
        ShowWindow(hwnd_intr,SW_HIDE);
      }
      
      ctl_win_edit_v = xvt_win_get_ctl(xdWindow, WIN_GRAPH_116_CHECKBOX_51);
      xvt_ctl_set_checked(ctl_win_edit_v, FALSE);
      checkbox_edit_v=FALSE;
      xvt_vobj_set_visible(ctl_win_edit_v,FALSE);                   
      
      ctl_win_edit_h = xvt_win_get_ctl(xdWindow, WIN_GRAPH_116_CHECKBOX_52);
      xvt_ctl_set_checked(ctl_win_edit_h, FALSE);
      checkbox_edit_h=FALSE;
      xvt_vobj_set_visible(ctl_win_edit_h,FALSE);
      RECT rect;
      ChangeFontAndSize((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW));	// GHJ
      
      ::SystemParametersInfo(SPI_GETWORKAREA, 0, &rect, 0);
      if (rect.right-rect.left <= 800)
      {
        ::GetClientRect(::GetParent((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW)), &rect);
        ::MoveWindow((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), rect.left, rect.top, rect.right-rect.left, rect.bottom-rect.top, TRUE);
      }
      xvt_vobj_set_visible(xdWindow, TRUE);		// GHJ
      display_prof116(&Edit_Win116[0]);			// GHJ
      
      /* Blättern im Grafikeditor */
      dlg_166=NULL_WIN;
      if (!((berechnen)&& (editieren))) //d.h. Längsschnitt
        if ((xvt_dlg_create_res(WD_MODELESS,166, EM_ALL, DLG_CHANGE_STATION_eh,0L))==NULL_WIN)
          xvt_dm_post_error("Can't open dialog 166");
        xvt_scr_set_focus_vobj(xdWindow);
        
        tx_comment116=xvt_win_get_tx(xdWindow,WIN_GRAPH_116_TEDIT_54);
        SLIST_ELT e_c;
        char *line;
        if ((berechnen)&& (editieren)) //d.h. Längsschnitt
        {
          xvt_tx_destroy(tx_comment116);
          xvt_vobj_set_visible(xvt_win_get_ctl(xdWindow, WIN_GRAPH_116_TEXT_55),FALSE);//Comment dazu
        }
        if (!((berechnen)&& (editieren))) //d.h. Längsschnitt
        {
          if (slist_comment == NULL)
            slist_comment = xvt_slist_create();
          for (e_c=xvt_slist_get_first(slist_comment);e_c!=NULL;e_c=xvt_slist_get_next(slist_comment,e_c))
          {
            line = xvt_slist_get(slist_comment,e_c,0L);
            
            if (!xvt_tx_add_par(tx_comment116, USHRT_MAX,line ))
            {
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_ERROR_TXTEDIT_OUT,buf,sizeof(buf));
              xvt_dm_post_error("%s",buf);
              //xvt_dm_post_error("Fehler bei TXEDIT -Ausgabe");
            }
          }
        }
        
        
    }
    break;
  case E_DESTROY:
    /*			Window has been closed; last event sent to window.		*/
    {
      if (dlg_166!=NULL_WIN)
        xvt_vobj_destroy(dlg_166);
      if (dlg_sonderprofil != NULL_WIN)
      {
        xvt_vobj_destroy(dlg_sonderprofil);
        dlg_sonderprofil = NULL_WIN;
      }
      if (slist_comment!=NULL)
      {
        xvt_slist_destroy(slist_comment);
        slist_comment=NULL;
      }
      
      tx_comment116=NULL;
      
      win_list2 = NULL_WIN;
      pWPL->window =NULL;
      WIN_116 = NULL_WIN;
      
      editor_closed = TRUE;
      if (dlg_136 !=NULL_WIN)
        set_menu_116(Menu_Win,FALSE); // in :  DLL
      else
        set_menu_116(Menu_Win,TRUE); // in :  DLL
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_30,FALSE);   //Vergleichsdaten
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_30_29,TRUE);//Ergebnisse(H)
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_30_12,TRUE);//Tabelle
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_30_24,TRUE);//Laengsschnitt einsehen
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_1_86,TRUE);//Ergebnisse der Massenberechnung            
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_30_84_87,TRUE);//Editor
      if( LWA_PROJEKT )
        xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_30_32,TRUE);//Auswerten(h)
      
      if (dlg_136 !=NULL_WIN)
        xvt_vobj_set_visible(dlg_136,TRUE);
      else
        xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_13_39,TRUE); // Menu: Profildatei enabled setzen
      
      xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_50_127,TRUE); // Menu: Zustände
      xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_50_128,TRUE); // Menu: Zustand speichern als
      xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_13_42,TRUE); // Menu: Rauheiten ändern
      xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_14_16,TRUE); // Menu: Abflussdatei
      xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_14_17,TRUE); // Menu: Verlustdatei
      xvt_menu_set_item_enabled(xdWindow,TASK_MENUBAR_13_59,TRUE); // Menu:Interpolieren
      
      xvt_menu_update(Menu_Win);
      
      if(ber_var_text!=NULL)
      {
        xvt_slist_destroy(ber_var_text);
        ber_var_text=NULL;
      }	
      
    }
    berechnen=FALSE;
    editieren=FALSE;
    GaussProfilMitRuecksprung=FALSE;
    Einf=FALSE;
    Edit_Fehler=FALSE;
    Edit_Fehler_Y=FALSE;
    Edit_Fehler_Z=FALSE;
    DestroyWindow(hwnd_intr);
    hwnd_intr=NULL;
    DestroyWindow(hwnd_info);
    hwnd_info=NULL;
    DestroyWindow(hwnd_spiegel);
    hwnd_spiegel=NULL;
    hwnd116=NULL;
    return 0L;
    
  case E_FOCUS:
    {
    /*
    Window has lost or gained focus.
      */
      //if(druckvorgang)
      if (xdEvent->v.active)  {
      /*
      Window has gained focus
        */
        if(create_116==FALSE )//Dick 21.04.99
          GaussRuecksprungCheck();           
        
      } else {
      /*
      Window has lost focus
        */
        create_116=FALSE;//Dick 21.04.99
        
      }
    }
    break;
  case E_SIZE:
    {
    }
    break;
  case E_UPDATE:
    /*    	Window requires updating.		*/
    {
      xvt_dwin_clear(xdWindow,WspwinMainBackgroundColor);
      xvt_sbar_set_pos(scroll_116,HVSCROLL,scr.scrollpos);
      
      
    }
    break;
  case E_CLOSE:
    {
      if (dlg_sonderprofil != NULL_WIN)
      {
        xvt_vobj_destroy(dlg_sonderprofil);
        dlg_sonderprofil = NULL_WIN;
      }
      
      
      WIN_116 = NULL_WIN;
      xvt_vobj_destroy(xdWindow);
    }
    break;
  case E_CHAR:
    /* 	Character typed.	*/
    {
      switch(xdEvent->v.chr.ch)
      {
      case K_TAB:
      case ENTER:
        {
          if (ch_id_116 <= 4)                                // 0...4
            xvt_scr_set_focus_vobj(Edit_Win116[ch_id_116+5]);
          else
          {
            if((ch_id_116>=5)&&(ch_id_116<9))                   // 5..8
              xvt_scr_set_focus_vobj(Edit_Win116[ch_id_116-4]);
            else
            {
              if (ch_id_116 == 9)            // 9-->4
                xvt_scr_set_focus_vobj(Edit_Win116[4]);
              else
              {
                if (ch_id_116==14)
                {                     // 14 --> 4
                  if (ch_id_116 -10 + scr.scrollpos < ds_info[GELAENDEHOEHE])
                  {
                    //Wert in Edit-Feld 44 SICHERN
                    xdEvent->type =E_CONTROL;
                    xdEvent->v.ctl.id=2000 + ch_id_116;
                    xdEvent->v.ctl.ci.v.edit.focus_change = TRUE;
                    xdEvent->v.ctl.ci.v.edit.active = FALSE;
                    xvt_win_dispatch_event(xdWindow,xdEvent);
                    
                    scr.scrollpos++;
                    list->GetScrollDaten(&scr);
                    display_prof116(&Edit_Win116[0]);
                    
                  }
                  else   //NEU
                    
                  {
                    xvt_scr_set_focus_vobj(Edit_Win116[4]);
                    if(Edit_Z!=TRUE && ch_id_116 -10 + scr.scrollpos < ds_info[GELAENDEHOEHE]+1)//Dick 4.08.98
                    {
                      scr.scrollpos++;
                      list->GetScrollDaten(&scr);
                      display_prof116(&Edit_Win116[0]);
                    }
                  }
                }
                else     // 10..13
                {
                  if (ch_id_116-9 + scr.scrollpos > ds_info[1])
                  {  //NEU
                    xdEvent->type =E_CONTROL;
                    xdEvent->v.ctl.id=2000 + ch_id_116;
                    xdEvent->v.ctl.ci.v.edit.focus_change = TRUE;
                    xdEvent->v.ctl.ci.v.edit.active = FALSE;
                    xvt_win_dispatch_event(xdWindow,xdEvent);
                    xvt_scr_set_focus_vobj(Edit_Win116[ch_id_116-9]);
                    list->GetScrollDaten(&scr);
                    display_prof116(&Edit_Win116[0]);
                  }
                  else
                    xvt_scr_set_focus_vobj(Edit_Win116[ch_id_116+1]);
                  
                  
                }
              }
            }
          }
        }
        break;
      case K_INS:
        {
          int i=0;
          char buf[200];
          //Daten abspeichern ,wo zuletzt editiert wurde
          xdEvent->type =E_CONTROL;
          xdEvent->v.ctl.id=2000 + ch_id_116;
          xdEvent->v.ctl.ci.v.edit.focus_change = TRUE;
          xdEvent->v.ctl.ci.v.edit.active = FALSE;
          xvt_win_dispatch_event(xdWindow,xdEvent);
          //Daten einlesen
          list->GetScrollDaten(&scr);
          //Neue Eingabefeld einfügen
          for(i=14;i>edit_line;i--)
          {
            scr.x[i]=scr.x[i-1];
            scr.y[i]=scr.y[i-1];
            scr.z[i]=scr.z[i-1]; 
          }
          scr.x[i]=BCE_NAN;
          scr.y[i]=BCE_NAN;
          scr.z[i]=BCE_NAN;
          display_prof116(&Edit_Win116[0]);    
          xvt_vobj_set_title(Edit_Win116[edit_line],xvt_res_get_str(STR_NEU,buf,sizeof(buf)));
          xvt_scr_set_focus_vobj(Edit_Win116[edit_line]);
          Einf=TRUE;
        }
        break;
      case K_UP:
        {
          if (((zoom_info.level==0)&&(scr.scrollpos > 1)) ||
            ((zoom_info.level>0)&&(scr.scrollpos>zoom_info.pos_station_min)))
          {
            paint->draw_marker(WIN_117,mark_pos);                         
            scr.scrollpos --;
            if(zoom_info.level)
              mark_pos = scr.scrollpos +edit_line-zoom_info.pos_station_min;
            else mark_pos = scr.scrollpos-1 +edit_line;
            list->GetScrollDaten(&scr);
            display_prof116(&Edit_Win116[0]);
            if(zoom_info.level==0) //Neu 7.07.98
              xvt_sbar_set_range(scroll_116, HVSCROLL, 1, ds_info[1]-3);//Neu 2.07.98
            xvt_sbar_set_pos(scroll_116,HVSCROLL,scr.scrollpos);
            paint->draw_marker(WIN_117,mark_pos);
          }
          break;
        }
      case K_DOWN:
        {
          if  (((zoom_info.level==0)&&(scr.scrollpos < (ds_info[1]-4))) ||
            ((zoom_info.level>0)&&(scr.scrollpos < zoom_info.pos_station_max-4)))
          {
            paint->draw_marker(WIN_117,mark_pos);                         
            scr.scrollpos ++;
            if(zoom_info.level)
              mark_pos = scr.scrollpos +edit_line-zoom_info.pos_station_min;
            else mark_pos = scr.scrollpos-1 +edit_line;
            list->GetScrollDaten(&scr);
            display_prof116(&Edit_Win116[0]);
            if(zoom_info.level==0) //Neu 7.07.98
              xvt_sbar_set_range(scroll_116, HVSCROLL, 1, ds_info[1]-3);//Neu 2.07.98
            xvt_sbar_set_pos(scroll_116,HVSCROLL,scr.scrollpos);
            paint->draw_marker(WIN_117,mark_pos);
          }
          break;
        }
      case K_PREV:
        {
          {
            paint->draw_marker(WIN_117,mark_pos);
            if(zoom_info.level>0)
              if((scr.scrollpos-5)>=zoom_info.pos_station_min)
                scr.scrollpos =scr.scrollpos - 5;
              else 
                scr.scrollpos =zoom_info.pos_station_min;
              else if((scr.scrollpos-5)>=1) 
                scr.scrollpos =scr.scrollpos - 5;
              else 
                scr.scrollpos =1;
              
              if(zoom_info.level)
                mark_pos = scr.scrollpos +edit_line-zoom_info.pos_station_min;
              else mark_pos = scr.scrollpos-1 +edit_line;
              list->GetScrollDaten(&scr);
              display_prof116(&Edit_Win116[0]);
              if(zoom_info.level==0) //Neu 7.07.98
                xvt_sbar_set_range(scroll_116, HVSCROLL, 1, ds_info[1]-3);//Neu 2.07.98
              xvt_sbar_set_pos(scroll_116,HVSCROLL,scr.scrollpos);
              paint->draw_marker(WIN_117,mark_pos);
          }
          
          break;
        }
      case K_NEXT:
        {
          {
            paint->draw_marker(WIN_117,mark_pos);
            if(zoom_info.level>0)
            {
              if((scr.scrollpos + 5)<=(zoom_info.pos_station_max-4))
                scr.scrollpos =scr.scrollpos + 5;
              else 
                scr.scrollpos =max(zoom_info.pos_station_min,zoom_info.pos_station_max-4);
            }
            else if((scr.scrollpos+5)<=(ds_info[1]-4)) 
              scr.scrollpos =scr.scrollpos + 5;
            else 
              scr.scrollpos =ds_info[1]-4;
            if(zoom_info.level)
              mark_pos = scr.scrollpos +edit_line-zoom_info.pos_station_min;
            else mark_pos = scr.scrollpos-1 +edit_line;
            list->GetScrollDaten(&scr);
            display_prof116(&Edit_Win116[0]);
            if(zoom_info.level==0) //Neu 7.07.98
              xvt_sbar_set_range(scroll_116, HVSCROLL, 1, ds_info[1]-3);//Neu 2.07.98
            xvt_sbar_set_pos(scroll_116,HVSCROLL,scr.scrollpos);
            paint->draw_marker(WIN_117,mark_pos);
          }
          break;
        }
      case ESC:
        {
          if (new_ds.listbox_win !=NULL_WIN )
            xvt_vobj_destroy(new_ds.listbox_win);
          new_ds.listbox_win=NULL_WIN;
        }
        break;
      case K_BTAB:
        {
          if (ch_id_116 <= 4 && ch_id_116 > 0)  // 0...4
          {
            if(Edit_Win116[ch_id_116+9]!=NULL)
              xvt_scr_set_focus_vobj(Edit_Win116[ch_id_116+4]);
            else
              xvt_scr_set_focus_vobj(Edit_Win116[ch_id_116+4]);
          }
          else
          {
            if((ch_id_116>=5)&&(ch_id_116<9))                   // 5..8
              xvt_scr_set_focus_vobj(Edit_Win116[ch_id_116-5]);
            else
            {
              if (ch_id_116 == 9)
              {// 9-->4
                back_tab=TRUE; 
                xvt_scr_set_focus_vobj(Edit_Win116[4]);
              }
              else if(ch_id_116 == 0)
              {
                if(scr.scrollpos>1)
                {
                  scr.scrollpos--;
                  list->GetScrollDaten(&scr);
                  display_prof116(&Edit_Win116[0]);
                  if(Edit_Win116[10]!=NULL)
                    xvt_scr_set_focus_vobj(Edit_Win116[10]);
                  else
                    xvt_scr_set_focus_vobj(Edit_Win116[5]);
                }
              }
              else
              {
                if (ch_id_116==14)
                {                     // 14 --> 9
                  xvt_scr_set_focus_vobj(Edit_Win116[9]);	
                }
                else     // 10..13
                {
                  if (ch_id_116-9 + scr.scrollpos > ds_info[1])
                  {  //NEU
                    xdEvent->type =E_CONTROL;
                    xdEvent->v.ctl.id=2000 + ch_id_116;
                    xdEvent->v.ctl.ci.v.edit.focus_change = TRUE;
                    xdEvent->v.ctl.ci.v.edit.active = FALSE;
                    xvt_win_dispatch_event(xdWindow,xdEvent);
                    xvt_scr_set_focus_vobj(Edit_Win116[ch_id_116-5]);
                    list->GetScrollDaten(&scr);
                    display_prof116(&Edit_Win116[0]);
                  }
                  else
                    xvt_scr_set_focus_vobj(Edit_Win116[ch_id_116-5]);
                  
                  
                }
              }
            }
          } 
        }
        break;
        
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
  /*
  Mouse was pressed
    */
    {
      if (xdEvent->v.mouse.button ==1)  //rechte Maus-Taste
      {
      }
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
    /*		Mouse was moved		*/
    {
      int bbb=0;
    }
    break;
  case E_HSCROLL:
    {
    /*
    Horizontal scrollbar on frame was operated
      */
      switch (xdEvent->v.scroll.what) {
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
      switch (xdEvent->v.scroll.what) {
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
  case E_COMMAND:
  /*
  User issued command on window menu bar (menu bar at top of
  screen for Mac/CH).
    */
    {
      do_TASK_MENUBAR(xdWindow, xdEvent);
    }
    break;
  case E_CONTROL:
    /*			User operated control in window.		*/
    {
      switch(xdControlId)
      {
      case WIN_scroll_116: /* "Vertical Scrollbar:Datenblocktyp rechts" */
        {
          if(!Edit_Fehler&&!Einf&&!Edit_Fehler_Y&&!Edit_Fehler_Z)//Neu 3.07.98 Wenn Fehler bei der Eingabe -> sperren
            switch (xdEvent->v.ctl.ci.v.scroll.what)
          {
      case SC_LINE_UP:
        if (((zoom_info.level==0)&&(scr.scrollpos > 1)) ||
          ((zoom_info.level>0)&&(scr.scrollpos>zoom_info.pos_station_min)))
        {
          //alte Daten sichern
          if (ch_id_116 >0 && (ch_id_116_save!=ch_id_116 || Edit_X || Edit_Y || Edit_Z))   // Cursor auf Edit-Feld->Daten sichern
          {
            xdEvent->type =E_CONTROL;
            xdEvent->v.ctl.id=2000 + ch_id_116;
            xdEvent->v.ctl.ci.v.edit.focus_change = TRUE;
            xdEvent->v.ctl.ci.v.edit.active = FALSE;
            xvt_win_dispatch_event(xdWindow,xdEvent);
            ch_id_116_save=ch_id_116;
          }
          paint->draw_marker(WIN_117,mark_pos);
          
          scr.scrollpos --;
          if(zoom_info.level)
            mark_pos = scr.scrollpos +edit_line-zoom_info.pos_station_min;
          else mark_pos = scr.scrollpos-1 +edit_line;
          list->GetScrollDaten(&scr);
          display_prof116(&Edit_Win116[0]);
          if(zoom_info.level==0) //Neu 7.07.98
            xvt_sbar_set_range(scroll_116, HVSCROLL, 1, ds_info[1]-3);//Neu 2.07.98
          xvt_sbar_set_pos(scroll_116,HVSCROLL,scr.scrollpos);//-zoom_info.pos_station_min);
          paint->draw_marker(WIN_117,mark_pos);
        }
        break;
      case SC_LINE_DOWN:
        if  (((zoom_info.level==0)&&(scr.scrollpos < (ds_info[1]-4))) ||
          ((zoom_info.level>0)&&(scr.scrollpos < zoom_info.pos_station_max-4)))
        {
          
          //alte Daten sichern
          if (ch_id_116 >0 && (ch_id_116_save!=ch_id_116 || Edit_X || Edit_Y || Edit_Z))   // Cursor auf Edit-Feld->Daten sichern
          {
            xdEvent->type =E_CONTROL;
            xdEvent->v.ctl.id=2000 + ch_id_116;
            xdEvent->v.ctl.ci.v.edit.focus_change = TRUE;
            xdEvent->v.ctl.ci.v.edit.active = FALSE;
            xvt_win_dispatch_event(xdWindow,xdEvent);
            ch_id_116_save=ch_id_116;
          }
          
          paint->draw_marker(WIN_117,mark_pos);
          scr.scrollpos ++;
          if(zoom_info.level)
            mark_pos = scr.scrollpos +edit_line-zoom_info.pos_station_min;
          else mark_pos = scr.scrollpos-1 +edit_line;
          list->GetScrollDaten(&scr);
          display_prof116(&Edit_Win116[0]);
          if(zoom_info.level==0) //Neu 7.07.98
            xvt_sbar_set_range(scroll_116, HVSCROLL, 1, ds_info[1]-3);//Neu 2.07.98
          xvt_sbar_set_pos(scroll_116,HVSCROLL,scr.scrollpos);//-zoom_info.pos_station_min);
          paint->draw_marker(WIN_117,mark_pos);
        }
        break;
      case SC_PAGE_UP:
        {
          //alte Daten sichern
          if (ch_id_116 >0 && (ch_id_116_save!=ch_id_116 || Edit_X || Edit_Y || Edit_Z))   // Cursor auf Edit-Feld->Daten sichern
          {
            xdEvent->type =E_CONTROL;
            xdEvent->v.ctl.id=2000 + ch_id_116;
            xdEvent->v.ctl.ci.v.edit.focus_change = TRUE;
            xdEvent->v.ctl.ci.v.edit.active = FALSE;
            xvt_win_dispatch_event(xdWindow,xdEvent);
            ch_id_116_save=ch_id_116;
          }
          paint->draw_marker(WIN_117,mark_pos);
          if(zoom_info.level>0)
            if((scr.scrollpos-5)>=zoom_info.pos_station_min)
              scr.scrollpos =scr.scrollpos - 5;
            else 
              scr.scrollpos =zoom_info.pos_station_min;
            else if((scr.scrollpos-5)>=1) 
              scr.scrollpos =scr.scrollpos - 5;
            else 
              scr.scrollpos =1;
            
            if(zoom_info.level)
              mark_pos = scr.scrollpos +edit_line-zoom_info.pos_station_min;
            else mark_pos = scr.scrollpos-1 +edit_line;
            list->GetScrollDaten(&scr);
            display_prof116(&Edit_Win116[0]);
            if(zoom_info.level==0) //Neu 7.07.98
              xvt_sbar_set_range(scroll_116, HVSCROLL, 1, ds_info[1]-3);//Neu 2.07.98
            xvt_sbar_set_pos(scroll_116,HVSCROLL,scr.scrollpos);
            paint->draw_marker(WIN_117,mark_pos);
        }
        break;
      case SC_PAGE_DOWN:
        {
          //alte Daten sichern
          if (ch_id_116 >0 && (ch_id_116_save!=ch_id_116 || Edit_X || Edit_Y || Edit_Z))   // Cursor auf Edit-Feld->Daten sichern
          {
            xdEvent->type =E_CONTROL;
            xdEvent->v.ctl.id=2000 + ch_id_116;
            xdEvent->v.ctl.ci.v.edit.focus_change = TRUE;
            xdEvent->v.ctl.ci.v.edit.active = FALSE;
            xvt_win_dispatch_event(xdWindow,xdEvent);
            ch_id_116_save=ch_id_116;
          }
          paint->draw_marker(WIN_117,mark_pos);
          if(zoom_info.level>0)
          {
            if((scr.scrollpos + 5)<=(zoom_info.pos_station_max-4))
              scr.scrollpos =scr.scrollpos + 5;
            else 
              scr.scrollpos =max(zoom_info.pos_station_min,zoom_info.pos_station_max-4);
          }
          else if((scr.scrollpos+5)<=(ds_info[1]-4)) 
            scr.scrollpos =scr.scrollpos + 5;
          else 
            scr.scrollpos =ds_info[1]-4;
          if(zoom_info.level)
            mark_pos = scr.scrollpos +edit_line-zoom_info.pos_station_min;
          else mark_pos = scr.scrollpos-1 +edit_line;
          list->GetScrollDaten(&scr);
          display_prof116(&Edit_Win116[0]);
          if(zoom_info.level==0) //Neu 7.07.98
            xvt_sbar_set_range(scroll_116, HVSCROLL, 1, ds_info[1]-3);//Neu 2.07.98
          xvt_sbar_set_pos(scroll_116,HVSCROLL,scr.scrollpos);
          paint->draw_marker(WIN_117,mark_pos);
          
        }
        break;
      case SC_THUMB:
        {
          
          {
            //alte Daten sichern
            if (ch_id_116 >0 && (ch_id_116_save!=ch_id_116 || Edit_X || Edit_Y || Edit_Z))   // Cursor auf Edit-Feld->Daten sichern
            {
              xdEvent->type =E_CONTROL;
              xdEvent->v.ctl.id=2000 + ch_id_116;
              xdEvent->v.ctl.ci.v.edit.focus_change = TRUE;
              xdEvent->v.ctl.ci.v.edit.active = FALSE;
              xvt_win_dispatch_event(xdWindow,xdEvent);
              ch_id_116_save=ch_id_116;
            }
            paint->draw_marker(WIN_117,mark_pos);
            thumb = xvt_sbar_get_pos(scroll_116, HVSCROLL );
            scr.scrollpos = xdEvent->v.ctl.ci.v.scroll.pos;
            xvt_sbar_set_pos(scroll_116,HVSCROLL,scr.scrollpos);
            
            list->GetScrollDaten(&scr);
            display_prof116(&Edit_Win116[0]);
            if( zoom_info.level == 0 ) //Neu 7.07.98
              xvt_sbar_set_range( scroll_116, HVSCROLL, 1, ds_info[1] - 3 );//Neu 2.07.98
            if( zoom_info.level > 0 )
              mark_pos = scr.scrollpos + edit_line-zoom_info.pos_station_min;
            else 
              mark_pos = scr.scrollpos - 1 + edit_line;
            paint->draw_marker( WIN_117, mark_pos );
          }
        }
        
        break;
      case SC_THUMBTRACK:
        {
          {
            //alte Daten sichern
            if (ch_id_116 >0 && (ch_id_116_save!=ch_id_116 || Edit_X || Edit_Y || Edit_Z))   // Cursor auf Edit-Feld->Daten sichern
            {
              xdEvent->type =E_CONTROL;
              xdEvent->v.ctl.id=2000 + ch_id_116;
              xdEvent->v.ctl.ci.v.edit.focus_change = TRUE;
              xdEvent->v.ctl.ci.v.edit.active = FALSE;
              xvt_win_dispatch_event(xdWindow,xdEvent);
              ch_id_116=ch_id_116_save;
            }
            paint->draw_marker(WIN_117,mark_pos);
            thumb = xvt_sbar_get_pos(scroll_116, HVSCROLL );
            scr.scrollpos = xdEvent->v.ctl.ci.v.scroll.pos;
            xvt_sbar_set_pos(scroll_116,HVSCROLL,scr.scrollpos);
            
            list->GetScrollDaten(&scr);
            display_prof116(&Edit_Win116[0]);
            if(zoom_info.level==0) //Neu 7.07.98
              xvt_sbar_set_range(scroll_116, HVSCROLL, 1, ds_info[1]-3);//Neu 2.07.98
            if(zoom_info.level>0)
              mark_pos = scr.scrollpos +edit_line-zoom_info.pos_station_min;
            else mark_pos = scr.scrollpos-1 +edit_line;
            paint->draw_marker(WIN_117,mark_pos);
          }
        }
        break;
      default:
        break;
        }
      }
      break;
      
      
    case (WIN116_EDIT0):      // X - EDIT - FELDER 0..4   1.Spalte
      if((!Edit_Fehler && !Edit_Fehler_Y && !Edit_Fehler_Z)|| (Edit_Fehler&& Edit_Fehler_Feld[0])){
        ch_id_116=xdControlId-WIN116_EDIT0;     // WIN_116_EDIT0==2000
        edit_line=ch_id_116;
        if (xdEvent->v.ctl.ci.v.edit.focus_change)
        {
          if (xdEvent->v.ctl.ci.v.edit.active )
          {  	/*	+focus has entered the control		*/
            
            paint->draw_marker(WIN_117,mark_pos);//Letzter mark_pos in der Grafik deaktivieren!!!
            if(zoom_info.level)
              mark_pos = scr.scrollpos +ch_id_116-zoom_info.pos_station_min;
            else mark_pos = scr.scrollpos-1 +ch_id_116;
            paint->draw_marker(WIN_117,mark_pos);//Neuer mark_pos in der Grafik aktivieren!!!
            xvt_sbar_set_pos(scroll_116,HVSCROLL,scr.scrollpos);
            if(berechnen && editieren)  //Längsschnitt Schlüssel anzeigen
            {
              char station_txt [20];
              double station_wert=list->hole_station(ch_id_116,&scr);
              sprintf(station_txt,"%lf",station_wert);
              xvt_vobj_set_title(xdedit7,station_txt);
              char vzktxt[20];
              char pktxt[20];
              list->schluessel_holen((scr.scrollpos-1+ch_id_116),&temp[0],vzktxt,pktxt);
              xvt_vobj_set_title(xdedit10,temp);
              xvt_vobj_set_title(xdedit8,vzktxt);
              xvt_vobj_set_title(xdedit9,pktxt);
            }
            //BLEY 18.01.01 neu
            if(typ[scr.datensatz]==BUHNE)
            {
              list->GetScrollDaten(&scr);
              
              aktuelles_editfeld=list->GetBuhnenKoord((scr.scrollpos-1+ch_id_116),&scr);
              int merke=aktuelles_editfeld;
              
              
              temp[0]='\0';
              xvt_vobj_get_title(Edit_Win116[10],temp,14);
              is_digit = is_zahl(temp);
              if ( is_digit <=0)
              {//keine Eingabe
                temp[0]='\0';
                aktuelles_editfeld=0;
              }
              list->GetInfoline2(scr.datensatz,aktuelles_editfeld, &sonderbauwerk);  //Zeile2 Infoblock holen
					         list->Errechne_Buhnen_Neigung(scr.datensatz,aktuelles_editfeld, &sonderbauwerk);
                   
                   aktuelles_editfeld=merke;
                   if(dlg_sonderprofil==NULL_WIN)
                   {
                     if (!xvt_dlg_create_res(WD_MODELESS,DLG_337, EM_ALL, DLG_337_eh, 0L))
                       xvt_dm_post_error("Can't open dialog 337");
                   }
                   
                   xvt_vobj_set_title(edit_buhne[2],sonderbauwerk.hoehe_links);
                   xvt_vobj_set_title(edit_buhne[1],sonderbauwerk.neig_links_rueck);
                   xvt_vobj_set_title(edit_buhne[0],sonderbauwerk.neig_links_vorne);
                   
                   if(sonderbauwerk.lage[0]=='R')
                     xvt_ctl_check_radio_button(radio_buhne[1],radio_buhne,2);
                   else
                   {
                     xvt_ctl_check_radio_button(radio_buhne[0],radio_buhne,2);
                     strcpy(sonderbauwerk.lage,"L");
                   }
                   
                   
                   
            }
            //Ende
            if(ch_id_116!=last_edit_116)
            {
              last_edit_116 = ch_id_116;
              xvt_ctl_set_text_sel(Edit_Win116[ch_id_116],0,SHRT_MAX);
            }                  
          }
          else  //
          {
            //if (ch_id_116 + scr.scrollpos <= ds_info[1])   //Wert vorhanden
            if((zoom_info.level==0 && ch_id_116 + scr.scrollpos > ds_info[1])
              || (zoom_info.level>0)&&ch_id_116 + scr.scrollpos > zoom_info.pos_station_max)
              Einf=1;
            {//gehört nicht zu if
              xvt_vobj_get_title(Edit_Win116[0],temp,14);
              is_digit = is_zahl(temp);
              if ( is_digit ==-1)  //keine Eingabe
                temp[0]='\0';
              if ( is_digit ==0)
              {
                Edit_Fehler=TRUE;
                Edit_Fehler_Feld[0]=TRUE;
                char buf[200],buf2[200];//Dick 26.11.99
                LoadString(NULL,STR_ERROR_REALFORMAT2,buf,sizeof(buf));
                LoadString(NULL,STR_EINGABEFEHLER,buf2,sizeof(buf2));
                MessageBox(hwnd116,buf,buf2,MB_OK|MB_ICONWARNING);
                //MessageBox(hwnd116,"Ungültiges Zahlenformat !","Eingabefehler",MB_OK|MB_ICONWARNING);
                xvt_scr_set_focus_vobj(Edit_Win116[0]);
                
              }
              if(is_digit>0 || is_digit==-1)
              {
                double xd,yd;
                Edit_Fehler=FALSE;
                Edit_Fehler_Feld[0]=FALSE;
                Edit_X=FALSE;
                if(Einf && is_digit>0)
                {
                  xd = atof(temp);
                  if(!checkbox_intr)
                    yd = BCE_NAN;
                  else
                    yd=list->Koord_Interpolieren(xd,xdControlId-2000+scr.scrollpos);
                  
                  list->Koord_Einf(xd,yd,xdControlId-2000+scr.scrollpos);
                }
                else if(!Einf&&((zoom_info.level==0 && ch_id_116 + scr.scrollpos <= ds_info[1])
                  || ((zoom_info.level>0) && ch_id_116 + scr.scrollpos <= zoom_info.pos_station_max))) 
                  list->SaveScrollDaten(xdControlId,&scr,temp,K_X);
                if(zoom_info.level==0) //Neu 7.07.98
                  xvt_sbar_set_range(scroll_116, HVSCROLL, 1, ds_info[1]-3);//Neu 2.07.98
                Einf=FALSE;
                list->GetScrollDaten(&scr);
                display_prof116(&Edit_Win116[0]);
                if (scr.anzahl<5)
                  scr.anzahl++;
                else scr.anzahl =5;
                
                GaussRuecksprungCheck();
                GaussRuecksprungCheckVar=FALSE;
                xvt_dwin_invalidate_rect(WIN_117,0);
              }
            }
            //Einf=FALSE;
            
          }
                }
                else
                {		/*	Contents of control were changed	*/
                  SaveProfilFile = TRUE;
                  Edit_Fehler=TRUE;
                  Edit_Fehler_Feld[0]=TRUE;
                  GaussRuecksprungCheckVar=TRUE;
                  Edit_X=TRUE;
                }
                
      }
      break;
    case (WIN116_EDIT1):
      if((!Edit_Fehler && !Edit_Fehler_Y && !Edit_Fehler_Z)|| (Edit_Fehler&& Edit_Fehler_Feld[1])){
        ch_id_116=xdControlId-WIN116_EDIT0;     // WIN_116_EDIT0==2000
        edit_line=ch_id_116;
        if (xdEvent->v.ctl.ci.v.edit.focus_change)
        {
          if (xdEvent->v.ctl.ci.v.edit.active )
          {  	/*	+focus has entered the control		*/
            
            paint->draw_marker(WIN_117,mark_pos);//Letzter mark_pos in der Grafik deaktivieren!!!
            if(zoom_info.level)
              mark_pos = scr.scrollpos +ch_id_116-zoom_info.pos_station_min;
            else mark_pos = scr.scrollpos-1 +ch_id_116;
            paint->draw_marker(WIN_117,mark_pos);//Neuer mark_pos in der Grafik aktivieren!!!
            xvt_sbar_set_pos(scroll_116,HVSCROLL,scr.scrollpos);
            if(berechnen && editieren)  //Längsschnitt Schlüssel anzeigen
            {
              char station_txt [20];
              double station_wert=list->hole_station(ch_id_116,&scr);
              sprintf(station_txt,"%lf",station_wert);
              xvt_vobj_set_title(xdedit7,station_txt);
              char vzktxt[20];
              char pktxt[20];
              list->schluessel_holen((scr.scrollpos-1+ch_id_116),&temp[0],vzktxt,pktxt);
              xvt_vobj_set_title(xdedit10,temp);
              xvt_vobj_set_title(xdedit8,vzktxt);
              xvt_vobj_set_title(xdedit9,pktxt);
            }
            //BLEY 18.01.01 neu
            if(typ[scr.datensatz]==BUHNE)
            {
              list->GetScrollDaten(&scr);
              
              aktuelles_editfeld=list->GetBuhnenKoord((scr.scrollpos-1+ch_id_116),&scr);
              
              
              int merke=aktuelles_editfeld;
              
              temp[0]='\0';
              xvt_vobj_get_title(Edit_Win116[11],temp,14);
              is_digit = is_zahl(temp);
              if ( is_digit <=0)
              {//keine Eingabe
                temp[0]='\0';
                aktuelles_editfeld=0;
              }
              list->GetInfoline2(scr.datensatz,aktuelles_editfeld, &sonderbauwerk);  //Zeile2 Infoblock holen
					         list->Errechne_Buhnen_Neigung(scr.datensatz,aktuelles_editfeld, &sonderbauwerk);
                   
                   aktuelles_editfeld=merke;
                   if(dlg_sonderprofil==NULL_WIN)
                   {
                     if (!xvt_dlg_create_res(WD_MODELESS,DLG_337, EM_ALL, DLG_337_eh, 0L))
                       xvt_dm_post_error("Can't open dialog 337");
                     
                   }
                   xvt_vobj_set_title(edit_buhne[2],sonderbauwerk.hoehe_links);
                   xvt_vobj_set_title(edit_buhne[1],sonderbauwerk.neig_links_rueck);
                   xvt_vobj_set_title(edit_buhne[0],sonderbauwerk.neig_links_vorne);
                   if(sonderbauwerk.lage[0]=='R')
                     xvt_ctl_check_radio_button(radio_buhne[1],radio_buhne,2);
                   else
                   {
                     xvt_ctl_check_radio_button(radio_buhne[0],radio_buhne,2);
                     strcpy(sonderbauwerk.lage,"L");
                   }
                   
            }
            //Ende
            
            
            xvt_ctl_set_text_sel(Edit_Win116[ch_id_116],0,SHRT_MAX);
            last_edit_116 = ch_id_116;
            
          }
          else
          {
            if((zoom_info.level==0 && ch_id_116 + scr.scrollpos > ds_info[1])
              || (zoom_info.level>0)&&ch_id_116 + scr.scrollpos > zoom_info.pos_station_max)
              Einf=1;
            {//gehört nicht zu if
              xvt_vobj_get_title(Edit_Win116[1],temp,14);
              is_digit = is_zahl(temp);
              if ( is_digit ==-1)  //keine Eingabe
                temp[0]='\0';
              if ( is_digit ==0)
              {
                Edit_Fehler=TRUE;
                Edit_Fehler_Feld[1]=TRUE;
                char buf[200],buf2[200];//Dick 26.11.99
                LoadString(NULL,STR_ERROR_REALFORMAT2,buf,sizeof(buf));
                LoadString(NULL,STR_EINGABEFEHLER,buf2,sizeof(buf2));
                MessageBox(hwnd116,buf,buf2,MB_OK|MB_ICONWARNING);
                //MessageBox(hwnd116,"Ungültiges Zahlenformat !","Eingabefehler",MB_OK|MB_ICONWARNING);
                xvt_scr_set_focus_vobj(Edit_Win116[1]);
                
              }
              if(is_digit>0 || is_digit==-1)
              {
                double xd,yd;
                Edit_Fehler=FALSE;
                Edit_Fehler_Feld[1]=FALSE;
                Edit_X=FALSE;
                if(Einf && is_digit>0)
                {
                  xd = atof(temp);
                  if(!checkbox_intr)
                    yd = BCE_NAN;
                  else
                    yd=list->Koord_Interpolieren(xd,xdControlId-2000+scr.scrollpos);
                  list->Koord_Einf(xd,yd,xdControlId-2000+scr.scrollpos);
                }
                else if(!Einf&&((zoom_info.level==0 && ch_id_116 + scr.scrollpos <= ds_info[1])
                  || ((zoom_info.level>0) && ch_id_116 + scr.scrollpos <= zoom_info.pos_station_max))) 
                  list->SaveScrollDaten(xdControlId,&scr,temp,K_X);
                if(zoom_info.level==0) //Neu 7.07.98
                  xvt_sbar_set_range(scroll_116, HVSCROLL, 1, ds_info[1]-3);//Neu 2.07.98
                Einf=FALSE;
                list->GetScrollDaten(&scr);
                display_prof116(&Edit_Win116[0]);
                if (scr.anzahl<5)
                  scr.anzahl++;
                else scr.anzahl =5;
                
                GaussRuecksprungCheck();
                GaussRuecksprungCheckVar=FALSE;
                xvt_dwin_invalidate_rect(WIN_117,0);
              }
            }
            //Einf=FALSE;
            
          }
                }
                else
                {		/*	Contents of control were changed	*/
                  SaveProfilFile = TRUE;
                  Edit_Fehler=TRUE;
                  Edit_Fehler_Feld[1]=TRUE;
                  GaussRuecksprungCheckVar=TRUE;
                  Edit_X=TRUE;
                }
                
      }
      break;
      
    case (WIN116_EDIT2):
      if((!Edit_Fehler && !Edit_Fehler_Y && !Edit_Fehler_Z)|| (Edit_Fehler&& Edit_Fehler_Feld[2])){
        ch_id_116=xdControlId-WIN116_EDIT0;     // WIN_116_EDIT0==2000
        edit_line=ch_id_116;
        if (xdEvent->v.ctl.ci.v.edit.focus_change)
        {
          if (xdEvent->v.ctl.ci.v.edit.active )
          {  	/*	+focus has entered the control		*/
            
            paint->draw_marker(WIN_117,mark_pos);//Letzter mark_pos in der Grafik deaktivieren!!!
            if(zoom_info.level)
              mark_pos = scr.scrollpos +ch_id_116-zoom_info.pos_station_min;
            else mark_pos = scr.scrollpos-1 +ch_id_116;
            paint->draw_marker(WIN_117,mark_pos);//Neuer mark_pos in der Grafik aktivieren!!!
            xvt_sbar_set_pos(scroll_116,HVSCROLL,scr.scrollpos);
            if(berechnen && editieren)  //Längsschnitt Schlüssel anzeigen
            {
              char station_txt [20];
              double station_wert=list->hole_station(ch_id_116,&scr);
              sprintf(station_txt,"%lf",station_wert);
              xvt_vobj_set_title(xdedit7,station_txt);
              char vzktxt[20];
              char pktxt[20];
              list->schluessel_holen((scr.scrollpos-1+ch_id_116),&temp[0],vzktxt,pktxt);
              xvt_vobj_set_title(xdedit10,temp);
              xvt_vobj_set_title(xdedit8,vzktxt);
              xvt_vobj_set_title(xdedit9,pktxt);
            }
            //BLEY 18.01.01 neu
            if(typ[scr.datensatz]==BUHNE)
            {
              list->GetScrollDaten(&scr);
              aktuelles_editfeld=list->GetBuhnenKoord((scr.scrollpos-1+ch_id_116),&scr);
              
              int merke=aktuelles_editfeld;
              
              
              temp[0]='\0';
              xvt_vobj_get_title(Edit_Win116[12],temp,14);
              is_digit = is_zahl(temp);
              if ( is_digit <=0)
              {//keine Eingabe
                temp[0]='\0';
                aktuelles_editfeld=0;
              }
              list->GetInfoline2(scr.datensatz,aktuelles_editfeld, &sonderbauwerk);  //Zeile2 Infoblock holen
              list->Errechne_Buhnen_Neigung(scr.datensatz,aktuelles_editfeld, &sonderbauwerk);
              
              aktuelles_editfeld=merke;
              if(dlg_sonderprofil==NULL_WIN)
              {
                if (!xvt_dlg_create_res(WD_MODELESS,DLG_337, EM_ALL, DLG_337_eh, 0L))
                  xvt_dm_post_error("Can't open dialog 337");
                
              }
              xvt_vobj_set_title(edit_buhne[2],sonderbauwerk.hoehe_links);
              xvt_vobj_set_title(edit_buhne[1],sonderbauwerk.neig_links_rueck);
              xvt_vobj_set_title(edit_buhne[0],sonderbauwerk.neig_links_vorne);
              if(sonderbauwerk.lage[0]=='R')
                xvt_ctl_check_radio_button(radio_buhne[1],radio_buhne,2);
              else
              {
                xvt_ctl_check_radio_button(radio_buhne[0],radio_buhne,2);
                strcpy(sonderbauwerk.lage,"L");
              }
              
              
              
            }
            //Ende
            
            xvt_ctl_set_text_sel(Edit_Win116[ch_id_116],0,SHRT_MAX);
            last_edit_116 = ch_id_116;
            
          }
          else  //focus left controll
          {
            //if (ch_id_116 + scr.scrollpos <= ds_info[1])   //Wert vorhanden
            if((zoom_info.level==0 && ch_id_116 + scr.scrollpos > ds_info[1])
              || (zoom_info.level>0)&&ch_id_116 + scr.scrollpos > zoom_info.pos_station_max)
              Einf=1;
            {//gehört nicht zu if
              xvt_vobj_get_title(Edit_Win116[2],temp,14);
              is_digit = is_zahl(temp);
              if ( is_digit ==-1)  //keine Eingabe
                temp[0]='\0';
              if ( is_digit ==0)
              {
                Edit_Fehler=TRUE;
                Edit_Fehler_Feld[2]=TRUE;
                //temp[0]='\0';
                //xvt_vobj_set_title(Edit_Win116[last_edit_116],temp);
                //xvt_dm_post_note("Ungültiges Zahlenformat !");
                char buf[200],buf2[200];//Dick 26.11.99
                LoadString(NULL,STR_ERROR_REALFORMAT2,buf,sizeof(buf));
                LoadString(NULL,STR_EINGABEFEHLER,buf2,sizeof(buf2));
                MessageBox(hwnd116,buf,buf2,MB_OK|MB_ICONWARNING);
                //MessageBox(hwnd116,"Ungültiges Zahlenformat !","Eingabefehler",MB_OK|MB_ICONWARNING);
                xvt_scr_set_focus_vobj(Edit_Win116[2]);
                
              }
              if(is_digit>0 || is_digit==-1)
              {
                double xd,yd;
                Edit_Fehler=FALSE;
                Edit_Fehler_Feld[2]=FALSE;
                Edit_X=FALSE;
                if(Einf && is_digit>0)
                {
                  xd = atof(temp);
                  if(!checkbox_intr)
                    yd = BCE_NAN;
                  else
                    yd=list->Koord_Interpolieren(xd,xdControlId-2000+scr.scrollpos);
                  
                  list->Koord_Einf(xd,yd,xdControlId-2000+scr.scrollpos);
                }
                else if(!Einf&&((zoom_info.level==0 && ch_id_116 + scr.scrollpos <= ds_info[1])
                  || ((zoom_info.level>0) && ch_id_116 + scr.scrollpos <= zoom_info.pos_station_max))) 
                  list->SaveScrollDaten(xdControlId,&scr,temp,K_X);
                if(zoom_info.level==0) //Neu 7.07.98
                  xvt_sbar_set_range(scroll_116, HVSCROLL, 1, ds_info[1]-3);//Neu 2.07.98
                Einf=FALSE;
                list->GetScrollDaten(&scr);
                display_prof116(&Edit_Win116[0]);
                if (scr.anzahl<5)
                  scr.anzahl++;
                else scr.anzahl =5;
                
                GaussRuecksprungCheck();
                GaussRuecksprungCheckVar=FALSE;
                xvt_dwin_invalidate_rect(WIN_117,0);
              }
            }
            //Einf=FALSE;
            
          }
                }
                else
                {		/*	Contents of control were changed	*/
                  SaveProfilFile = TRUE;
                  Edit_Fehler=TRUE;
                  Edit_Fehler_Feld[2]=TRUE;
                  GaussRuecksprungCheckVar=TRUE;
                  Edit_X=TRUE;
                }
                
      }
      break;
    case (WIN116_EDIT3):
      if((!Edit_Fehler && !Edit_Fehler_Y && !Edit_Fehler_Z)|| (Edit_Fehler&& Edit_Fehler_Feld[3])){
        ch_id_116=xdControlId-WIN116_EDIT0;     // WIN_116_EDIT0==2000
        edit_line=ch_id_116;
        if (xdEvent->v.ctl.ci.v.edit.focus_change)
        {
          if (xdEvent->v.ctl.ci.v.edit.active )
          {  	/*	+focus has entered the control		*/
            
            paint->draw_marker(WIN_117,mark_pos);//Letzter mark_pos in der Grafik deaktivieren!!!
            if(zoom_info.level)
              mark_pos = scr.scrollpos +ch_id_116-zoom_info.pos_station_min;
            else mark_pos = scr.scrollpos-1 +ch_id_116;
            paint->draw_marker(WIN_117,mark_pos);//Neuer mark_pos in der Grafik aktivieren!!!
            xvt_sbar_set_pos(scroll_116,HVSCROLL,scr.scrollpos);
            if(berechnen && editieren)  //Längsschnitt Schlüssel anzeigen
            {
              char station_txt [20];
              double station_wert=list->hole_station(ch_id_116,&scr);
              //gcvt(station_wert,13,station_txt);
              sprintf(station_txt,"%lf",station_wert);
              xvt_vobj_set_title(xdedit7,station_txt);
              char vzktxt[20];
              char pktxt[20];
              list->schluessel_holen((scr.scrollpos-1+ch_id_116),&temp[0],vzktxt,pktxt);
              xvt_vobj_set_title(xdedit10,temp);
              xvt_vobj_set_title(xdedit8,vzktxt);
              xvt_vobj_set_title(xdedit9,pktxt);
            }
            //BLEY 18.01.01 neu
            if(typ[scr.datensatz]==BUHNE)
            {
              list->GetScrollDaten(&scr);
              aktuelles_editfeld=list->GetBuhnenKoord((scr.scrollpos-1+ch_id_116),&scr);
              int merke=aktuelles_editfeld;
              
              temp[0]='\0';
              xvt_vobj_get_title(Edit_Win116[13],temp,14);
              is_digit = is_zahl(temp);
              if ( is_digit <=0)
              {//keine Eingabe
                temp[0]='\0';
                aktuelles_editfeld=0;
              }
              list->GetInfoline2(scr.datensatz,aktuelles_editfeld, &sonderbauwerk);  //Zeile2 Infoblock holen
              list->Errechne_Buhnen_Neigung(scr.datensatz,aktuelles_editfeld, &sonderbauwerk);
              
              aktuelles_editfeld=merke;
              if(dlg_sonderprofil==NULL_WIN)
              {
                if (!xvt_dlg_create_res(WD_MODELESS,DLG_337, EM_ALL, DLG_337_eh, 0L))
                  xvt_dm_post_error("Can't open dialog 337");
                
              }
              xvt_vobj_set_title(edit_buhne[2],sonderbauwerk.hoehe_links);
              xvt_vobj_set_title(edit_buhne[1],sonderbauwerk.neig_links_rueck);
              xvt_vobj_set_title(edit_buhne[0],sonderbauwerk.neig_links_vorne);
              if(sonderbauwerk.lage[0]=='R')
                xvt_ctl_check_radio_button(radio_buhne[1],radio_buhne,2);
              else
              {
                xvt_ctl_check_radio_button(radio_buhne[0],radio_buhne,2);
                strcpy(sonderbauwerk.lage,"L");
              }
              
            }
            //Ende
            
            
            xvt_ctl_set_text_sel(Edit_Win116[ch_id_116],0,SHRT_MAX);
            last_edit_116 = ch_id_116;
            
          }
          else /*focus left controll*/
          {
            //if (ch_id_116 + scr.scrollpos <= ds_info[1])   //Wert vorhanden
            if((zoom_info.level==0 && ch_id_116 + scr.scrollpos > ds_info[1])
              || (zoom_info.level>0)&&ch_id_116 + scr.scrollpos > zoom_info.pos_station_max)
              Einf=1;
            {//gehört nicht zu if
              xvt_vobj_get_title(Edit_Win116[3],temp,14);
              is_digit = is_zahl(temp);
              if ( is_digit ==-1)  //keine Eingabe
                temp[0]='\0';
              if ( is_digit ==0)
              {
                Edit_Fehler=TRUE;
                Edit_Fehler_Feld[3]=TRUE;
                //temp[0]='\0';
                //xvt_vobj_set_title(Edit_Win116[last_edit_116],temp);
                //xvt_dm_post_note("Ungültiges Zahlenformat !");
                char buf[200],buf2[200];//Dick 26.11.99
                LoadString(NULL,STR_ERROR_REALFORMAT2,buf,sizeof(buf));
                LoadString(NULL,STR_EINGABEFEHLER,buf2,sizeof(buf2));
                MessageBox(hwnd116,buf,buf2,MB_OK|MB_ICONWARNING);
                //MessageBox(hwnd116,"Ungültiges Zahlenformat !","Eingabefehler",MB_OK|MB_ICONWARNING);
                xvt_scr_set_focus_vobj(Edit_Win116[3]);
                
              }
              if(is_digit>0 || is_digit==-1)
              {
                double xd,yd;
                Edit_Fehler=FALSE;
                Edit_Fehler_Feld[3]=FALSE;
                Edit_X=FALSE;
                if(Einf && is_digit>0)
                {
                  xd = atof(temp);
                  if(!checkbox_intr)
                    yd = BCE_NAN;
                  else
                    yd=list->Koord_Interpolieren(xd,xdControlId-2000+scr.scrollpos);
                  
                  list->Koord_Einf(xd,yd,xdControlId-2000+scr.scrollpos);
                }
                else if(!Einf&&((zoom_info.level==0 && ch_id_116 + scr.scrollpos <= ds_info[1])
                  || ((zoom_info.level>0) && ch_id_116 + scr.scrollpos <= zoom_info.pos_station_max))) 
                  list->SaveScrollDaten(xdControlId,&scr,temp,K_X);
                if(zoom_info.level==0) //Neu 7.07.98
                  xvt_sbar_set_range(scroll_116, HVSCROLL, 1, ds_info[1]-3);//Neu 2.07.98
                Einf=FALSE;
                list->GetScrollDaten(&scr);
                display_prof116(&Edit_Win116[0]);
                if (scr.anzahl<5)
                  scr.anzahl++;
                else scr.anzahl =5;
                
                GaussRuecksprungCheck();
                GaussRuecksprungCheckVar=FALSE;
                xvt_dwin_invalidate_rect(WIN_117,0);
              }
            }
            
            
          }
                }
                else
                {		/*	Contents of control were changed	*/
                  SaveProfilFile = TRUE;
                  Edit_Fehler=TRUE;
                  Edit_Fehler_Feld[3]=TRUE;
                  GaussRuecksprungCheckVar=TRUE;
                  Edit_X=TRUE;
                } 
                
      }
      break;
    case (WIN116_EDIT4):
      if((!Edit_Fehler && !Edit_Fehler_Y && !Edit_Fehler_Z) || (Edit_Fehler&& Edit_Fehler_Feld[4])){  /*	 Edit control was operated.	*/
        ch_id_116=xdControlId-WIN116_EDIT0;     // WIN_116_EDIT0==2000
        edit_line=ch_id_116;
        if (xdEvent->v.ctl.ci.v.edit.focus_change)
        {
          if (xdEvent->v.ctl.ci.v.edit.active )
          {  	/*	+focus has entered the control		*/
            paint->draw_marker(WIN_117,mark_pos);//Letzter mark_pos in der Grafik deaktivieren!!!
            if(zoom_info.level)
              mark_pos = scr.scrollpos +ch_id_116-zoom_info.pos_station_min;
            else mark_pos = scr.scrollpos-1 +ch_id_116;
            paint->draw_marker(WIN_117,mark_pos);//Neuer mark_pos in der Grafik aktivieren!!!
            xvt_sbar_set_pos(scroll_116,HVSCROLL,scr.scrollpos);
            if(berechnen && editieren)  //Längsschnitt Schlüssel anzeigen
            {
              char station_txt [20];
              double station_wert=list->hole_station(ch_id_116,&scr);
              //gcvt(station_wert,13,station_txt);
              sprintf(station_txt,"%lf",station_wert);
              xvt_vobj_set_title(xdedit7,station_txt);
              char vzktxt[20];
              char pktxt[20];
              list->schluessel_holen((scr.scrollpos-1+ch_id_116),&temp[0],vzktxt,pktxt);
              xvt_vobj_set_title(xdedit10,temp);
              xvt_vobj_set_title(xdedit8,vzktxt);
              xvt_vobj_set_title(xdedit9,pktxt);
            }
            //BLEY 18.01.01 neu
            if(typ[scr.datensatz]==BUHNE)
            {
              list->GetScrollDaten(&scr);
              aktuelles_editfeld=list->GetBuhnenKoord((scr.scrollpos-1+ch_id_116),&scr);
              int merke=aktuelles_editfeld;
              
              temp[0]='\0';
              xvt_vobj_get_title(Edit_Win116[14],temp,14);
              is_digit = is_zahl(temp);
              if ( is_digit <=0)
              {//keine Eingabe
                temp[0]='\0';
                aktuelles_editfeld=0;
              }
              
              list->GetInfoline2(scr.datensatz,aktuelles_editfeld, &sonderbauwerk);  //Zeile2 Infoblock holen
              list->Errechne_Buhnen_Neigung(scr.datensatz,aktuelles_editfeld, &sonderbauwerk);
              
              aktuelles_editfeld=merke;
              if(dlg_sonderprofil==NULL_WIN)
              {
                if (!xvt_dlg_create_res(WD_MODELESS,DLG_337, EM_ALL, DLG_337_eh, 0L))
                  xvt_dm_post_error("Can't open dialog 337");
                
              }
              xvt_vobj_set_title(edit_buhne[2],sonderbauwerk.hoehe_links);
              xvt_vobj_set_title(edit_buhne[1],sonderbauwerk.neig_links_rueck);
              xvt_vobj_set_title(edit_buhne[0],sonderbauwerk.neig_links_vorne);
              if(sonderbauwerk.lage[0]=='R')
                xvt_ctl_check_radio_button(radio_buhne[1],radio_buhne,2);
              else
              {
                xvt_ctl_check_radio_button(radio_buhne[0],radio_buhne,2);
                strcpy(sonderbauwerk.lage,"L");
              }
              
              
            }
            //Ende
            
            xvt_ctl_set_text_sel(Edit_Win116[ch_id_116],0,SHRT_MAX);
            
            last_edit_116 = ch_id_116;
            //Einf=FALSE;  //G_Neue
          } /* -focus has entered the control*/
          else
          {  /* focus has left the control			*/
            
            //if (ch_id_116 + scr.scrollpos <= ds_info[1])   //Wert vorhanden
            //if (ch_id_116 + scr.scrollpos > ds_info[1]) //Neuer Wert
            if((zoom_info.level==0 && ch_id_116 + scr.scrollpos > ds_info[1])
              || (zoom_info.level>0)&&ch_id_116 + scr.scrollpos > zoom_info.pos_station_max)
              Einf=1;
            {//gehört nicht zu if
              xvt_vobj_get_title(Edit_Win116[4],temp,14);
              is_digit = is_zahl(temp);
              if ( is_digit ==-1)  //keine Eingabe
                temp[0]='\0';
              if ( is_digit ==0)
              {
                Edit_Fehler=TRUE;
                Edit_Fehler_Feld[4]=TRUE;
                //temp[0]='\0';
                //xvt_vobj_set_title(Edit_Win116[last_edit_116],temp);
                //xvt_dm_post_note("Ungültiges Zahlenformat !");
                char buf[200],buf2[200];//Dick 26.11.99
                LoadString(NULL,STR_ERROR_REALFORMAT2,buf,sizeof(buf));
                LoadString(NULL,STR_EINGABEFEHLER,buf2,sizeof(buf2));
                MessageBox(hwnd116,buf,buf2,MB_OK|MB_ICONWARNING);
                //MessageBox(hwnd116,"Ungültiges Zahlenformat !","Eingabefehler",MB_OK|MB_ICONWARNING);
                xvt_scr_set_focus_vobj(Edit_Win116[4]);
                
              }
              if(is_digit>0 || is_digit==-1)
              {
                double xd,yd;
                Edit_Fehler=FALSE;
                Edit_Fehler_Feld[4]=FALSE;
                Edit_X=FALSE;
                if(Einf && is_digit>0)
                {
                  xd = atof(temp);
                  if(!checkbox_intr)
                    yd = BCE_NAN;
                  else
                    yd=list->Koord_Interpolieren(xd,xdControlId-2000+scr.scrollpos);
                  
                  list->Koord_Einf(xd,yd,xdControlId-2000+scr.scrollpos);
                }
                else if(!Einf&&((zoom_info.level==0 && ch_id_116 + scr.scrollpos <= ds_info[1])
                  || ((zoom_info.level>0) && ch_id_116 + scr.scrollpos <= zoom_info.pos_station_max))) 
                  list->SaveScrollDaten(xdControlId,&scr,temp,K_X);
                if(zoom_info.level==0) //Neu 7.07.98
                  xvt_sbar_set_range(scroll_116, HVSCROLL, 1, ds_info[1]-3);//Neu 2.07.98
                Einf=FALSE;
                list->GetScrollDaten(&scr);
                display_prof116(&Edit_Win116[0]);
                if (scr.anzahl<5)
                  scr.anzahl++;
                else scr.anzahl =5;
                
                GaussRuecksprungCheck();
                GaussRuecksprungCheckVar=FALSE;
                xvt_dwin_invalidate_rect(WIN_117,0);
              }
            }
            //Einf=FALSE;
          }   /*focus has left the control*/
       }
       else
       {		/*	Contents of control were changed	*/
         SaveProfilFile = TRUE;
         Edit_Fehler=TRUE;
         Edit_Fehler_Feld[4]=TRUE;
         GaussRuecksprungCheckVar=TRUE;
         Edit_X=TRUE;
       }
      }
      break;
      
    case (WIN116_EDIT5):      // Y - EDIT - FELDER 0..4   2.Spalte
    case (WIN116_EDIT6):
    case (WIN116_EDIT7):
    case (WIN116_EDIT8):
    case (WIN116_EDIT9):
      if(!Edit_Fehler&&!Edit_Fehler_Z)	{	/*	 	Edit control was operated.	  */
        ch_id_116=xdControlId-WIN116_EDIT0;     // WIN116_EDIT0==2000
        edit_line=ch_id_116-5;
        if(!Edit_Fehler_Y || Edit_Fehler_Y&&Edit_Fehler_Feld[edit_line])
        {
          if (xdEvent->v.ctl.ci.v.edit.focus_change)
          {
            if (xdEvent->v.ctl.ci.v.edit.active)
            {			/*	  +focus has entered the control		*/
              if(last_edit_116 != ch_id_116-5 && last_edit_116 != ch_id_116+5)
              {
                paint->draw_marker(WIN_117,mark_pos);
                if(zoom_info.level)
                  mark_pos = scr.scrollpos +ch_id_116-5-zoom_info.pos_station_min;
                else mark_pos = scr.scrollpos-1 +ch_id_116-5;
                paint->draw_marker(WIN_117,mark_pos);
              } 
              if(berechnen && editieren)  //Längsschnitt Schlüssel anzeigen
              {
                char station_txt [20];
                double station_wert=list->hole_station(ch_id_116-5,&scr);
                sprintf(station_txt,"%lf",station_wert);
                xvt_vobj_set_title(xdedit7,station_txt);
                char vzktxt[20];
                char pktxt[20];
                list->schluessel_holen((scr.scrollpos-1 +ch_id_116-5),&temp[0],vzktxt,pktxt);
                xvt_vobj_set_title(xdedit10,temp);
                xvt_vobj_set_title(xdedit8,vzktxt);
                xvt_vobj_set_title(xdedit9,pktxt);
              }
              
              
              //edit_position=0;
              //BLEY 18.01.01 neu
              if(typ[scr.datensatz]==BUHNE)
              {
                
                list->GetScrollDaten(&scr);
                aktuelles_editfeld=list->GetBuhnenKoord((scr.scrollpos-1+ch_id_116-5),&scr);
                int merke=aktuelles_editfeld;
                temp[0]='\0';
                //						 xvt_vobj_get_title(Edit_Win116[scr.scrollpos-1+ch_id_116],temp,10);
                xvt_vobj_get_title(Edit_Win116[ch_id_116+5],temp,14);
                is_digit = is_zahl(temp);
                if ( is_digit <=0)
                {//keine Eingabe
                  temp[0]='\0';
                  aktuelles_editfeld=0;
                }
                list->GetInfoline2(scr.datensatz,aktuelles_editfeld, &sonderbauwerk);  //Zeile2 Infoblock holen
                aktuelles_editfeld=merke;
                if(dlg_sonderprofil==NULL_WIN)
                {
                  if (!xvt_dlg_create_res(WD_MODELESS,DLG_337, EM_ALL, DLG_337_eh, 0L))
                    xvt_dm_post_error("Can't open dialog 337");
                  
                }
                xvt_vobj_set_title(edit_buhne[2],sonderbauwerk.hoehe_links);
                xvt_vobj_set_title(edit_buhne[1],sonderbauwerk.neig_links_rueck);
                xvt_vobj_set_title(edit_buhne[0],sonderbauwerk.neig_links_vorne);
                if(sonderbauwerk.lage[0]=='R')
                  xvt_ctl_check_radio_button(radio_buhne[1],radio_buhne,2);
                else
                {
                  xvt_ctl_check_radio_button(radio_buhne[0],radio_buhne,2);
                  strcpy(sonderbauwerk.lage,"L");
                }
                
              }
              //Ende
              
              
              xvt_ctl_set_text_sel(Edit_Win116[ch_id_116],0,SHRT_MAX);
              
              if ((last_edit_116 >= 0)&&(last_edit_116<=4))
                if (ch_id_116-5 != last_edit_116)
                  if (last_edit_116 + scr.scrollpos > ds_info[1])  // NEU
                  {
                    double xd,yd;
                    if (Edit_Win116[last_edit_116]!=NULL_WIN)
                      xvt_vobj_get_title(Edit_Win116[last_edit_116],temp,15);
                    is_digit = is_zahl(temp);
                    
                    if ( is_digit ==-1)  //keine Eingabe
                      temp[0]='\0';
                    if ( is_digit ==0)
                    {
                      char buf[200];//Dick 26.11.99
                      xvt_res_get_str(STR_ERROR_REALFORMAT2,buf,sizeof(buf));
                      xvt_dm_post_error("%s",buf);
                      //xvt_dm_post_note("Ungültiges Zahlenformat !");
                      xvt_scr_set_focus_vobj(Edit_Win116[last_edit_116]);
                    }
                    if (is_digit>0)  // GaussTest
                    {
                      xd = atof(temp);
                      yd = BCE_NAN;
                      if (scr.anzahl<5)
                        scr.anzahl++;
                      else scr.anzahl =5;
                      
                      list->GetScrollDaten(&scr);
                      
                      display_prof116(&Edit_Win116[0]);
                    }
                  }
                  last_edit_116 = ch_id_116;
                  
            }     /*  -focus has entered the control     */
            else
            {		/*			focus has left the control			*/
              if (ch_id_116-5 + scr.scrollpos > ds_info[1])  // NEU
              {
                temp[0]='\0';
                xvt_vobj_set_title(Edit_Win116[last_edit_116],temp);
                Edit_Y=FALSE;
                Edit_Fehler_Y=FALSE;
                Edit_Fehler_Feld[edit_line]=FALSE;
              }
              else      // y-Koordinate editiert
                if (ch_id_116 -5 + scr.scrollpos <= ds_info[1] )
                {
                  if (Edit_Win116[ch_id_116]!=NULL_WIN)
                    xvt_vobj_get_title(Edit_Win116[ch_id_116],temp,14);
                  is_digit = is_zahl(temp);
                  if ( is_digit ==0)
                  {
                    //temp[0]='\0';
                    //xvt_vobj_set_title(Edit_Win116[last_edit_116],temp);
                    char buf[200],buf2[200];//Dick 26.11.99
                    LoadString(NULL,STR_ERROR_REALFORMAT2,buf,sizeof(buf));
                    LoadString(NULL,STR_EINGABEFEHLER,buf2,sizeof(buf2));
                    Edit_Fehler_Feld[edit_line]=TRUE;
                    Edit_Fehler_Y=TRUE;
                    //xvt_dm_post_note("Ungültiges Zahlenformat !");
                    int save_ch=ch_id_116;
                    MessageBox(hwnd116,buf,buf2,MB_OK|MB_ICONWARNING);
                    xvt_scr_set_focus_vobj(Edit_Win116[save_ch]);
                  }
                  if ( is_digit ==-1)  //keine Eingabe
                    temp[0]='\0';
                  
                  if ((is_digit>0 || is_digit==-1)&&!Einf)
                  {
                    list->SaveScrollDaten(xdControlId,&scr,temp,K_Y);
                    Edit_Y=FALSE;
                    Edit_Fehler_Y=FALSE;
                    Edit_Fehler_Feld[edit_line]=FALSE;
                  }
                  
                }
                
                if (((xdControlId==WIN116_EDIT9)&&(ds_info[1]>=5)&& is_digit!=0 &&
                  ((ch_id_116-5 + scr.scrollpos <= ds_info[1] && zoom_info.level==0) ||
                  (ch_id_116-5 + scr.scrollpos < zoom_info.pos_station_max && zoom_info.level > 0)))&&!back_tab)//Neu 7.07.98
                {
                  scr.scrollpos ++;
                  list->GetScrollDaten(&scr);
                  display_prof116(&Edit_Win116[0]);
                }
                back_tab=FALSE;
            }
            xvt_dwin_invalidate_rect(WIN_117,0);
            last_edit_116 = ch_id_116;
       }
       else
       {		/*			Contents of control were changed		*/
         SaveProfilFile = TRUE;
         Edit_Y=TRUE;
         Edit_Fehler_Feld[edit_line]=TRUE;
         Edit_Fehler_Y=TRUE;
       }
             }
      }
      break;
      
    case (WIN116_EDIT10):      // Z - EDIT - FELDER 0..4   3.Spalte
    case (WIN116_EDIT11):
    case (WIN116_EDIT12):
    case (WIN116_EDIT13):
    case (WIN116_EDIT14):
      if(!Edit_Fehler&&!Edit_Fehler_Y){   /*  Edit control was operated.      */
        ch_id_116=xdControlId-WIN116_EDIT0;     //   WIN120_EDIT0 <=3000
        edit_line=ch_id_116-10;
        if(!Edit_Fehler_Z || Edit_Fehler_Z&&Edit_Fehler_Feld[edit_line])
        {
          if (xdEvent->v.ctl.ci.v.edit.focus_change)
          {
            if (xdEvent->v.ctl.ci.v.edit.active)
            {/*focus has entered the control	*/
              if(last_edit_116 != ch_id_116-5 && last_edit_116 != ch_id_116-10)
              {
                paint->draw_marker(WIN_117,mark_pos);
                if(zoom_info.level)
                  mark_pos = scr.scrollpos +ch_id_116-10-zoom_info.pos_station_min;
                else mark_pos = scr.scrollpos-1 +ch_id_116-10;
                paint->draw_marker(WIN_117,mark_pos);
              }
              
              if(berechnen && editieren)  //Längsschnitt Schlüssel anzeigen
              {
                char station_txt [20];
                double station_wert=list->hole_station(ch_id_116-10,&scr);
                //gcvt(station_wert,13,station_txt);
                sprintf(station_txt,"%lf",station_wert);
                xvt_vobj_set_title(xdedit7,station_txt);
                char vzktxt[20];
                char pktxt[20];
                list->schluessel_holen(scr.scrollpos-1 +ch_id_116-10,&temp[0],vzktxt,pktxt);
                xvt_vobj_set_title(xdedit10,temp);
                xvt_vobj_set_title(xdedit8,vzktxt);
                xvt_vobj_set_title(xdedit9,pktxt);
              }
              //BLEY 18.01.01 neu
              if(typ[scr.datensatz]==BUHNE)
              {
                
                list->GetScrollDaten(&scr);
                aktuelles_editfeld=list->GetBuhnenKoord((scr.scrollpos-1+ch_id_116-10),&scr);
                int merke=aktuelles_editfeld;
                temp[0]='\0';
                //						 xvt_vobj_get_title(Edit_Win116[scr.scrollpos-1+ch_id_116],temp,14);
                //						 xvt_vobj_get_title(Edit_Win116[ch_id_116+10],temp,14);
                xvt_vobj_get_title(Edit_Win116[ch_id_116],temp,14);
                is_digit = is_zahl(temp);
                if ( is_digit <=0)
                {//keine Eingabe
                  temp[0]='\0';
                  aktuelles_editfeld=0;
                }
                
                list->GetInfoline2(scr.datensatz,aktuelles_editfeld, &sonderbauwerk);  //Zeile2 Infoblock holen
                list->Errechne_Buhnen_Neigung(scr.datensatz,aktuelles_editfeld, &sonderbauwerk);
                
                aktuelles_editfeld=merke;
                if(dlg_sonderprofil==NULL_WIN)
                {
                  if (!xvt_dlg_create_res(WD_MODELESS,DLG_337, EM_ALL, DLG_337_eh, 0L))
                    xvt_dm_post_error("Can't open dialog 337");
                  
                }
                
                xvt_vobj_set_title(edit_buhne[2],sonderbauwerk.hoehe_links);
                xvt_vobj_set_title(edit_buhne[1],sonderbauwerk.neig_links_rueck);
                xvt_vobj_set_title(edit_buhne[0],sonderbauwerk.neig_links_vorne);
                if(sonderbauwerk.lage[0]=='R')
                  xvt_ctl_check_radio_button(radio_buhne[1],radio_buhne,2);
                else
                {
                  xvt_ctl_check_radio_button(radio_buhne[0],radio_buhne,2);
                  strcpy(sonderbauwerk.lage,"L");
                }
                
              }
              //Ende
              
              xvt_ctl_set_text_sel(Edit_Win116[ch_id_116],0,SHRT_MAX);
              
              last_edit_116 =ch_id_116;
              
            }
            else
            {  /*	focus has left the control	*/
              if (ch_id_116-10 + scr.scrollpos <= ds_info[1])
              {
                if (Edit_Win116[ch_id_116]!=NULL_WIN)
                  xvt_vobj_get_title(Edit_Win116[ch_id_116],temp,14);
                is_digit = is_zahl(temp);
                if ( is_digit ==0)
                {
                  char buf[200],buf2[200];//Dick 26.11.99
                  LoadString(NULL,STR_ERROR_REALFORMAT2,buf,sizeof(buf));
                  LoadString(NULL,STR_EINGABEFEHLER,buf2,sizeof(buf2));
                  Edit_Fehler_Feld[edit_line]=TRUE;
                  Edit_Fehler_Z=TRUE;
                  //xvt_dm_post_note("Ungültiges Zahlenformat !");
                  int save_ch=ch_id_116;
                  MessageBox(hwnd116,buf,buf2,MB_OK|MB_ICONWARNING);
                  xvt_scr_set_focus_vobj(Edit_Win116[save_ch]);
                }
                else
                  //	 if ( is_digit>0 )
                {
                  if ((is_digit>0 || is_digit==-1)&&!Einf)
                    list->SaveScrollDaten(xdControlId,&scr,temp,K_Z);
                  last_edit_116 =ch_id_116;
                  Edit_Z=FALSE;
                  Edit_Fehler_Z=FALSE;
                  Edit_Fehler_Feld[edit_line]=FALSE;
                  xvt_dwin_invalidate_rect(WIN_117,0);
                }
              }
              else       //Edit-Feld > ds_info
              {
                xvt_vobj_set_title(Edit_Win116[ch_id_116],"");
                xvt_scr_set_focus_vobj(Edit_Win116[ch_id_116-10]);
              }
              
            }
        }
        else
        { 	/*	Contents of control were changed	*/
          SaveProfilFile = TRUE;
          Edit_Z=TRUE;
          Edit_Fehler_Feld[edit_line]=TRUE;
          Edit_Fehler_Z=TRUE;
        }
                    }
      }
      break;
      
      
      
      /*****************************************************************************/
    case WIN_GRAPH_116_PUSHBUTTON_26: /* "OK" */
      {
        if (dlg_sonderprofil != NULL_WIN)
        {
          xvt_vobj_destroy(dlg_sonderprofil);
          dlg_sonderprofil = NULL_WIN;
        }
        
        int ds_tmp = scr.datensatz;  //Datensatz retten
        if (check_border())   // -> util.h
        {
          if( GetFeature( "wsp_nodemo" ) )
          {
            list->Koord_Update(0);
            save_profildatei(pWPL);
          }
          else
          {
            char buf[200];//Dick 26.11.99
            xvt_res_get_str(STR_DEMO_NOTE_3,buf,sizeof(buf));
            xvt_dm_post_note("%s",buf); 							   
            //xvt_dm_post_note("Speichern bei Demo nicht möglich");
          };
          
          WIN_116 = NULL_WIN;
          xvt_vobj_destroy(xdWindow);
          SaveProfilFile = FALSE;
          
        }
        else
        {
          scr.datensatz = ds_tmp;
          list->GetScrollDaten(&scr);
          SaveProfilFile = TRUE;
        }
        
      }
      break;
    case WIN_GRAPH_116_PUSHBUTTON_27: /* "Abbruch" */
      {
        if (dlg_sonderprofil != NULL_WIN)
        {
          xvt_vobj_destroy(dlg_sonderprofil);
          dlg_sonderprofil = NULL_WIN;
        }
        
        if (SaveProfilFile)
        {
          char buf[200],buf2[200],buf3[200],buf4[200];
          xvt_res_get_str(STR_JA,buf,sizeof(buf));
          xvt_res_get_str(STR_NEIN,buf2,sizeof(buf2));
          xvt_res_get_str(STR_ABBRECHEN,buf3,sizeof(buf3));
          xvt_res_get_str(STR_PLOT101_ASK,buf4,sizeof(buf4));
          switch(xvt_dm_post_ask(buf,buf2,buf3,buf4))
            //switch (xvt_dm_post_ask("Ja","Nein","Abbrechen","Sollen geänderte Daten\ngesichert werden ?"))
          {
          case RESP_DEFAULT:       //SICHERN
            {                
              int ds_tmp = scr.datensatz;  //Datensatz retten
              if (check_border())  // -> util.h  line 1211
              {
                if( GetFeature( "wsp_nodemo" ) )
                {
                  list->Koord_Update(0);
                  save_profildatei(pWPL);
                }
                else
                {
                  //xvt_dm_post_note("Speichern bei Demo nicht möglich");
                  char buf[200];//Dick 26.11.99
                  xvt_res_get_str(STR_DEMO_NOTE_3,buf,sizeof(buf));
                  xvt_dm_post_note("%s",buf);
                };
                
                WIN_116 = NULL_WIN;
                xvt_vobj_destroy(xdWindow);
                SaveProfilFile = FALSE;
              }
              else
              {
                scr.datensatz = ds_tmp;
                SaveProfilFile = TRUE;
                list->GetScrollDaten(&scr);
              }
            }
            break;
          case RESP_2:             // nicht sichere_datenblock
            {
              SaveProfilFile = FALSE;
              WIN_116 = NULL_WIN;
              xvt_vobj_destroy(xdWindow);
            }
            break;
          case RESP_3:        //Abbruch
            {
            }
            break;
          }
        }
        else
          xvt_vobj_destroy(xdWindow);
      }
      break;
      /**********************************************************************************/
    case WIN_GRAPH_116_LISTBUTTON_33:
      {
        SLIST slist;
        char *str;
        char ber_info_str[MAX_PATH];
        //scr.scrollpos=1;//Auskomm.Dick 7.07.98
        scr.datensatz=(xvt_list_get_sel_index(xdlist1) + 1  );
        //Neu Dick 15.12.99 Info-Button
        switch (typ[scr.datensatz]) 
        {
        case UK_BRUECKE:                             
        case OK_BRUECKE:
        case OK_WEHRS:
          ShowWindow(hwnd_info,SW_SHOW);
          break;
        case RECHTSWERT:
        case HOCHWERT:
          ShowWindow(hwnd_interpol,SW_SHOW);
          break;
        default:
          {
            ShowWindow(hwnd_info,SW_HIDE);
            ShowWindow(hwnd_interpol,SW_HIDE);
          }
        }
        //
        
        if (typ[scr.datensatz] == COMMENT)
        {
          win116_loesche_edit_felder();   //->dis_prof.h
          if (dlg_sonderprofil != NULL_WIN)
          {
            xvt_vobj_destroy(dlg_sonderprofil);
            dlg_sonderprofil = NULL_WIN;
          }
          
        }
        else
        {
          if(datensatz_aktuel!=scr.datensatz)//Dick  9.08.98
          {
            datensatz_aktuel=scr.datensatz; 
            list->Koord_Update(0);
          }
          if(zoom_info.level==0 && scr.scrollpos > ds_info[1]-4)//dick 7.08.98 Korrektur scr.scrollpos nach Update
          {
            if(ds_info[1]>4)
              scr.scrollpos=ds_info[1]-4;
            else
              scr.scrollpos=1;
          }
          else
            if((zoom_info.level>0)&&(zoom_info.pos_station_max > ds_info[1]-4))
            { 
              if(ds_info[1]>4)
              {
                
                zoom_info.pos_station_max=ds_info[1]-4;
                scr.scrollpos=zoom_info.pos_station_max-4;
                zoom_info.pos_station_min=zoom_info.pos_station_max-(zoom_info.max_old-zoom_info.min_old);
                list->Get_Num_Station_Zoom(&zoom_info,LEFT);
                xvt_sbar_set_pos(win_116_zoom_scroll,HVSCROLL,zoom_info.pos_station_min);
                list->Set_Zoom_Marker(&zoom_info);
              }
              else
              {
                scr.scrollpos=1;
                zoom_info.pos_station_min=1;
                zoom_info.pos_station_max=zoom_info.pos_station_max+(zoom_info.max_old-zoom_info.min_old);
                list->Get_Num_Station_Zoom(&zoom_info,LEFT);
                xvt_sbar_set_pos(win_116_zoom_scroll,HVSCROLL,zoom_info.pos_station_min);
                list->Set_Zoom_Marker(&zoom_info);
              }
            }
            if (ds_info[1] < 5)//dick 7.08.98 Korrektur scr.anzahl nach Update , wofür? aber vorsichtshalber
              scr.anzahl = ds_info[1];
            else
              scr.anzahl =5;
            list->GetScrollDaten(&scr);      // Daten in scr einlesen
            list->GetMinMax(&pmm,scr.datensatz);
            
            neue_datensatz_wahl = TRUE;
            display_prof116(&Edit_Win116[0]);   //Werte aus scr in x-y-z Felder ausgeben
            if(zoom_info.level==0)
              xvt_sbar_set_range(scroll_116, HVSCROLL, 1, ds_info[1]-3);
            else
              xvt_sbar_set_range(scroll_116, HVSCROLL,zoom_info.pos_station_min, zoom_info.pos_station_max-3);
            
            /*Neu Dick*/
            if(zoom_info.level>0)
            {
              list->Get_Num_Station_Zoom(&zoom_info,LEFT);
              xvt_sbar_set_pos(win_116_zoom_scroll,HVSCROLL,zoom_info.pos_station_min);
              list->Set_Zoom_Marker(&zoom_info);
            }
            /*ende Neu*/
            
            
            xvt_sbar_set_pos(scroll_116,HVSCROLL,scr.scrollpos);//Dich 8.08.98
            xvt_scr_set_focus_vobj(Edit_Win116[0]); //und den Focus daraf plazieren
            if (checkbox)        // alle Datensätze zeichnen
            {
              //2.10.96:
              draw_liste[1]=1; //damit Gel.höhe immer angezeigt und nicht letzter
              draw_liste[ scr.datensatz ] = 1;
              //		draw_liste[0]++;
              int gefunden=0;
              for(int i=1;i<=draw_typ_liste_allg[0];i++)
              {
                if(typ[scr.datensatz]==draw_typ_liste_allg[i])
                  gefunden=1;
              }
              if(gefunden==0)
              {
                draw_typ_liste_allg[0]++;
                draw_typ_liste_allg[draw_typ_liste_allg[0]]=typ[scr.datensatz];
              }
              gefunden=0;
              if(typ[scr.datensatz]==STATION || typ[scr.datensatz]==STATION_FIX ||
                typ[scr.datensatz]==SOHLHOEHE_2 || typ[scr.datensatz]==WASSERSPIEGEL_2 ||
                typ[scr.datensatz]==BOESCHUNG_LINKS_2 || typ[scr.datensatz]==BOESCHUNG_RECHTS_2)
              {
                if (ber_var_text == NULL)
                {                                                                            
                  if(list->ExtractInfo2String(scr.datensatz,ber_info_str))
                  {
                    ber_var_text = xvt_slist_create();
                    xvt_slist_add_at_pos(ber_var_text,0,ber_info_str,0L);
                  }
                }
                else
                {
                  if(list->ExtractInfo2String(scr.datensatz,ber_info_str))
                  {
                    SLIST_ELT e;
                    char *line;
                    for (e=xvt_slist_get_first(ber_var_text);e!=NULL;e=xvt_slist_get_next(ber_var_text,e))
                    {
                      line = xvt_slist_get(ber_var_text,e,0L);
                      if(xvt_str_match(ber_info_str,line,TRUE))
                      {
                        gefunden=1;
                      }
                    }
                    if(gefunden==0)
                    {
                      xvt_slist_add_at_elt(ber_var_text,NULL,ber_info_str,0L);
                    }
                  }
                }
              }
            }
            else
            {
              draw_liste[1]=scr.datensatz;
              //		draw_liste[0]=1;
            }
            /*neu*/    list->GetMinMax(&pmm,scr.datensatz);
            
            xvt_dwin_invalidate_rect(WIN_117,0);    //Grafik updaten
        } //-else
        /******  Titel 3.Spalte setzen: *******/
        xvt_vobj_set_title(win116_typ,"\0");
        
        if ((typ[scr.datensatz] >GELAENDEHOEHE)&&(typ[scr.datensatz] < MAUL))
        {
        }
        else
        {
          //Bley 30.10.2000
          
          //xvt_vobj_set_title(win116_typ,"\0");//bley weg 30.10.2000
          if(typ[scr.datensatz]!=LP_TEXT)
          {
            slist = xvt_slist_create();
            slist = xvt_list_get_sel(xdlist1);
            str 	= xvt_slist_get_elt(slist,0,0L);
            xvt_vobj_set_title(win116_typ,str);
          }
          //Ende Bley 30.10.2000
        }
        /***Ende  Titel 3.Spalte setzen: ****/
       }
       break;
       
       
    case WIN_116_LISTBOX_NEU:   /* "Listbox : Auswahl neuer Datensatz"  */
                                /* wird durch Auswahl im Hauptmenu aktiviert
siehe: add_new_datensatz in:util.cpp        */
      
      {/*	List box was operated.*/
        if (xdEvent->v.ctl.ci.v.lbox.dbl_click)
        { 	/*			double click			*/
        }
        else
        {   /*		single click			*/
          if (new_ds.listbox_win != NULL_WIN)
          {
            SLIST_ELT e;
            SLIST daten;
            daten = xvt_slist_create();
            long *id;
            char *chr;
            bool ax_da=FALSE,ay_da=FALSE,dp_da=FALSE;
            char buf[50];
            read_res_datentypen(daten,NEU);
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
              if (new_ds.listbox_win !=NULL_WIN )
                xvt_vobj_destroy(new_ds.listbox_win);
              new_ds.listbox_win=NULL_WIN;
              if (new_ds.id>0)
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
                    add_new_datensatz(xdlist1);   //in:util.cpp
                  }
                  if(!ay_da)
                  {
                    if((xvt_res_get_str(AYM+1000,buf,sizeof(buf)))==NULL)   //Kommentar
                      xvt_dm_post_error("Can't read string resource");
                    
                    new_ds.datensatz=buf;                                   
                    new_ds.id=AYM;                                                                      
                    add_new_datensatz(xdlist1);   //in:util.cpp
                  }
                  if(!dp_da)
                  {
                    if((xvt_res_get_str(DPM+1000,buf,sizeof(buf)))==NULL)   //Kommentar
                      xvt_dm_post_error("Can't read string resource");
                    
                    new_ds.datensatz=buf;                                   
                    new_ds.id=DPM;                                                                      
                    add_new_datensatz(xdlist1);   //in:util.cpp
                  }
                }
                else
                  add_new_datensatz(xdlist1);   //in:util.cpp
              }
              
              scr.scrollpos = 1;
              xvt_sbar_set_pos(scroll_116,HVSCROLL,1);
              
              if (ds_info[1]<15)
                scr.anzahl = ds_info[1];
              else	 scr.anzahl = anz;
              
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
              
              if(datensatz_aktuel!=scr.datensatz)//Dick  14.01.99
              {
                datensatz_aktuel=scr.datensatz; 
                list->Koord_Update(1);//Dick 3.04.2000 0->1 weil es kann passieren daß x koord. noch BCE_NAN sind
              }
              
              
              list->GetScrollDaten(&scr);
              
              xvt_vobj_set_title(win116_typ,"\0");  //Titel 3.Spalte löschen
              win116_loesche_edit_felder();     // 3.Spalte löschen
              neue_datensatz_wahl = TRUE;
              display_prof116(&Edit_Win116[0]);
              
              //Dick 31.05.99
              if ((typ[scr.datensatz] >GELAENDEHOEHE)&&(typ[scr.datensatz] < MAUL))
              {
              }
              //
              if (checkbox)        // alle Datensätze zeichnen
              {
                draw_liste[ scr.datensatz ] = 1;
                draw_liste[0]++;
              }
              else
              {
                draw_liste[1]=scr.datensatz;
                draw_liste[0]=1;
              }
              xvt_dwin_invalidate_rect(WIN_117,0);//Dick  14.01.99
              SaveProfilFile = TRUE;
            }
          }
        }
      }
      break;
    case WIN_116_LISTBOX_DEL:   /* "Listbox : Auswahl löschen Datensatz"  */
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
              {
                xvt_res_get_str(STR_DELETE_NODE,buf1,sizeof(buf1));
                xvt_dm_post_error("%s",buf);
                //xvt_dm_post_note("Dieser Datensatz kann nicht gelöscht werden!");
              }
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
                
                xvt_vobj_set_title(win116_typ,"\0");  //Titel 3.Spalte löschen
                win116_loesche_edit_felder();     // 3.Spalte löschen
                
                scr.datensatz =  1;
                
                if(datensatz_aktuel!=1)//Dick  14.01.99
                {
                  datensatz_aktuel=1; 
                  list->Koord_Update(0);
                }
                list->GetScrollDaten(&scr);
                display_prof116(&Edit_Win116[0]);
                xvt_dwin_invalidate_rect(WIN_117,0);//Dick  14.01.99
                SaveProfilFile = TRUE;
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
    case WIN_GRAPH_116_CHECKBOX_34: /* " ? alles darstellen" */
      {
        xvt_ctl_set_checked(ctl_win,!xvt_ctl_is_checked (ctl_win) );
        
        
        checkbox = !checkbox;
        //			  checkbox = FALSE;
        if (!checkbox)
        {
          draw_liste[0]=1;
          draw_liste[1]=1;
          for (int i=2;i<=49;i++)  draw_liste[i] =0;
          for (i=0;i<200;i++)  draw_typ_liste_allg[i] =0;
          if(ber_var_text!=NULL)
          {
            xvt_slist_destroy(ber_var_text);
            ber_var_text=NULL;
          }
        }
      }
      break;
    case WIN_GRAPH_116_CHECKBOX_36: /* " ? alles darstellen" */
      {
        xvt_ctl_set_checked(ctl_win_alle,!xvt_ctl_is_checked (ctl_win_alle) );
        
        
        checkbox_alle = !checkbox_alle;
        //			  checkbox = FALSE;
        if (!checkbox_alle)
        {
          draw_liste[0]=1;
          draw_liste[1]=1;
          for (int i=2;i<=49;i++)  draw_liste[i] =0;
          if(checkbox)
          {
            for(i=1;i<=ds_info[0];i++)
              for(int j=1;j<=draw_typ_liste_allg[0];j++)
              {
                if(typ[i]==draw_typ_liste_allg[j])
                {
                  if(typ[i]==STATION || typ[i]==STATION_FIX ||
                    typ[i]==SOHLHOEHE_2 || typ[i]==WASSERSPIEGEL_2 ||
                    typ[i]==BOESCHUNG_LINKS_2 || typ[i]==BOESCHUNG_RECHTS_2)
                  {
                    if(ber_var_text!=NULL)
                    {
                      char var_text[MAX_PATH];
                      char *line;
                      SLIST_ELT e;
                      if(list->ExtractInfo2String(i,var_text))
                      {
                        bool gefunden=FALSE;
                        for (e=xvt_slist_get_first(ber_var_text);e!=NULL;e=xvt_slist_get_next(ber_var_text,e))
                        {
                          line = xvt_slist_get(ber_var_text,e,0L);
                          if(xvt_str_match(var_text,line,TRUE))
                            gefunden=TRUE;
                        }
                        if(gefunden)
                          draw_liste[i]=1;
                        else
                          draw_liste[i]=0;
                      }
                      else
                        draw_liste[i]=0;
                    }
                    else
                      draw_liste[i]=0;                             
                  }
                  else
                    draw_liste[i]=1; 
                }
                //draw_liste[i] =1;
              }
              draw_liste[scr.datensatz]=1;
          }
          else
            draw_liste[1]=scr.datensatz;
          //for (i=0;i<200;i++)  draw_typ_liste_allg[i] =0;
          /*if(ber_var_text!=NULL)
          {
          xvt_slist_destroy(ber_var_text);
          ber_var_text=NULL;
        }*/
        }
        else
        {
          draw_liste[0]=1;
          draw_liste[1]=1;
          for (int i=2;i<=49;i++)  draw_liste[i] =1;
          //for (i=0;i<200;i++)  draw_typ_liste_allg[i] =0;
          
          /*if(ber_var_text!=NULL)
          {
          xvt_slist_destroy(ber_var_text);
          ber_var_text=NULL;
        }*/
        }
        xvt_dwin_invalidate_rect(WIN_117,0);
      }
      break;
      
    case WIN_GRAPH_116_CHECKBOX_50: /* "Edit " */
      {
        xvt_ctl_set_checked(ctl_win_edit,!xvt_ctl_is_checked (ctl_win_edit) );
        
        
        checkbox_edit = !checkbox_edit;
        
        if (!checkbox_edit)
        {
          xvt_ctl_set_checked(ctl_win_edit_v,FALSE);
          xvt_ctl_set_checked(ctl_win_edit_h,FALSE);
          checkbox_edit_v=FALSE;
          checkbox_edit_h=FALSE;
          xvt_vobj_set_visible(ctl_win_edit_v,FALSE);
          xvt_vobj_set_visible(ctl_win_edit_h,FALSE);
        }
        else
        {
          xvt_vobj_set_visible(ctl_win_edit_v,TRUE);
          xvt_vobj_set_visible(ctl_win_edit_h,TRUE);
        }
      }
      break;
    case WIN_GRAPH_116_CHECKBOX_51: /* "Edit Vertikal" */
      {
        xvt_ctl_set_checked(ctl_win_edit_v,!xvt_ctl_is_checked (ctl_win_edit_v) );
        
        
        checkbox_edit_v = !checkbox_edit_v;
        
        if (!checkbox_edit_v)
        {
          
        }
      }
      break;
    case WIN_GRAPH_116_CHECKBOX_52: /* "Edit Horizontal" */
      {
        xvt_ctl_set_checked(ctl_win_edit_h,!xvt_ctl_is_checked (ctl_win_edit_h) );
        
        
        checkbox_edit_h = !checkbox_edit_h;
        
        if (!checkbox_edit_h)
        {
          
        }
      }
      break;
    case WIN_GRAPH_116_CHECKBOX_35: /* "Interpolieren" */
      {
        xvt_ctl_set_checked(ctl_win_intr,!xvt_ctl_is_checked (ctl_win_intr) );
        
        
        checkbox_intr = !checkbox_intr;
        
        
      }
      break;
      //Bley 20.10.2000 Anfang
    case WIN_GRAPH_116_CHECKBOX_38: /* "Spieglbild" */
      {
        xvt_ctl_set_checked(ctl_win_spiegel,!xvt_ctl_is_checked (ctl_win_spiegel) );
        //				checkbox_spiegel = !checkbox_spiegel;
        list->spiegelbild();
        xvt_dwin_invalidate_rect(WIN_117,0); //Fenster updaten
        list->GetScrollDaten(&scr);
        display_prof116(&Edit_Win116[0]);			// GHJ
        //Bley 22.11.2000 Test
      }
      break;
      //Bley 20.10.2000 Ende
      
      /**********************Neu*********************************************************/
      
    case WIN_GRAPH_116_PUSHBUTTON_43: /* "Punkt einfügen" */
      {
        
        if(win122!=NULL_WIN)
        {
          xvt_vobj_destroy(win122);
          win122=NULL_WIN;
        }
        xdEvent->type=E_CHAR;
        xdEvent->v.chr.ch=K_INS;
        xvt_win_dispatch_event(WIN_116,xdEvent);
      }
      break;
    case WIN_GRAPH_116_PUSHBUTTON_44: /* "Datensatz neu einfügen" */
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
        new_ds = display_new_datensatz(WIN_116); //->util.cpp
      }
      break;
    case WIN_GRAPH_116_PUSHBUTTON_47: /* "Datensatz löschen" */
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
        delete_datensatz(WIN_116);
      }
      break;
    case WIN_GRAPH_116_PUSHBUTTON_42: /*  "Grafik drucken" */
      {
        LPVOID * lPtr;
        lPtr=NULL;//prevent compiler-warning
        DWORD threadId;
        CreateThread(NULL,
          4096,
          (LPTHREAD_START_ROUTINE)print_graphic,
          lPtr,
          0,
          &threadId
          );
      }
      break;
    case WIN_GRAPH_116_PUSHBUTTON_48 :    //  Zoomen....
      {
        if (WIN_117!= NULL_WIN)
        {
          ZOOM_IN = TRUE;
          dlg_zoom_1 = xvt_dlg_create_res(WD_MODELESS, ZOOM_1, EM_ALL, ZOOM_1_eh, 0L);
          if (dlg_zoom_1==NULL_WIN)
            xvt_dm_post_error("Can't open dialog Zoomen");
        }
      }
      break;
    case WIN_GRAPH_116_PUSHBUTTON_49:   /*Normal 100%*/
      {
        
        xdEvent->type =E_USER;
        xdEvent->v.user.id=EVENT_ZOOM_NORMAL;
        xvt_win_dispatch_event(WIN_117,xdEvent);
        xdEvent->v.user.id=E_USER_ZOOM_NORMAL;
        xvt_win_dispatch_event(WIN_116,xdEvent);
        
      }
      break;
    case WIN_GRAPH_116_PUSHBUTTON_45:  /* Profilverlängerung*/
      ExecProfilverlaengerung(pWPL);
      break;
    case WIN_GRAPH_116_PUSHBUTTON_46:  /* Punkt löschen*/
      {
        //Daten abspeichern ,wo zuletzt editiert wurde
        xdEvent->type =E_CONTROL;
        xdEvent->v.ctl.id=2000 + ch_id_116;
        xdEvent->v.ctl.ci.v.edit.focus_change = TRUE;
        xdEvent->v.ctl.ci.v.edit.active = FALSE;
        xvt_win_dispatch_event(xdWindow,xdEvent);
        
        xvt_scr_set_focus_vobj(Edit_Win116[edit_line]);
        xvt_vobj_set_title(Edit_Win116[edit_line],"\0");
        
        xdEvent->type =E_CONTROL;
        xdEvent->v.ctl.id=2000 + ch_id_116;
        xdEvent->v.ctl.ci.v.edit.focus_change = TRUE;
        xdEvent->v.ctl.ci.v.edit.active = FALSE;
        xvt_win_dispatch_event(xdWindow,xdEvent);
        
        SaveProfilFile = TRUE;
        
      }
      break;
      /*********************************************************************************/
    case WIN116_ZOOM_SCROLL: /* "Horizontal Scrollbar: ZOOMEM" */
      {
        switch (xdEvent->v.ctl.ci.v.scroll.what)
        {
        case SC_LINE_UP:
          {
            if ((zoom_info.pos_station_min > 1)&&(!limit_up))
            {
              //						 int old_level= zoom_info.level;
              //						 zoom_info.level =0;
              zoom_info.pos_station_min--;
              zoom_info.pos_station_max-- ;
              
              limit_up = list->Get_Num_Station_Zoom(&zoom_info,LEFT);
              limit_down=0;
              
              //						 zoom_info.level =old_level;
              
              xvt_sbar_set_pos(win_116_zoom_scroll,HVSCROLL,zoom_info.pos_station_min);
              
              list->Set_Zoom_Marker(&zoom_info);
              //scr.scrollpos = zoom_info.pos_station_min;
              xvt_sbar_set_range(scroll_116, HVSCROLL,zoom_info.pos_station_min, zoom_info.pos_station_max-3);
              scr.scrollpos--;//Neu Dick 12.07.98
              xvt_sbar_set_pos(scroll_116,HVSCROLL,scr.scrollpos);//Neu Dick 12.07.98
              list->GetScrollDaten(&scr);
              display_prof116(&Edit_Win116[0]);
              xvt_dwin_invalidate_rect(WIN_117,0); //Fenster updaten
              
            }
            else
              xvt_sbar_set_pos(win_116_zoom_scroll,HVSCROLL,zoom_info.pos_station_min);
          }
          break;
        case SC_LINE_DOWN:
          {
            if ((zoom_info.pos_station_max < ds_info[1])&&(!limit_down))
            {
              //						 int old_level= zoom_info.level;
              //						 zoom_info.level =0;
              zoom_info.pos_station_max++ ;
              zoom_info.pos_station_min++ ;
              
              limit_down = list->Get_Num_Station_Zoom(&zoom_info,RIGHT);
              limit_up=0;  //Eingentlich braucht man limit gar nicht mehr , aber ok
              
              //						 zoom_info.level =old_level;
              
              xvt_sbar_set_pos(win_116_zoom_scroll,HVSCROLL,zoom_info.pos_station_min);
              
              list->Set_Zoom_Marker(&zoom_info);
              //scr.scrollpos = zoom_info.pos_station_min;
              xvt_sbar_set_range(scroll_116, HVSCROLL,zoom_info.pos_station_min, zoom_info.pos_station_max-3);
              scr.scrollpos++;//Neu Dick 12.07.98
              xvt_sbar_set_pos(scroll_116,HVSCROLL,scr.scrollpos);//Neu Dick 12.07.98
              list->GetScrollDaten(&scr);
              display_prof116(&Edit_Win116[0]);
              xvt_dwin_invalidate_rect(WIN_117,0); //Fenster updaten
              //xvt_sbar_set_range(scroll_116, HVSCROLL,zoom_info.pos_station_min, zoom_info.pos_station_max-3);
            }
            else
              xvt_sbar_set_pos(win_116_zoom_scroll,HVSCROLL,zoom_info.pos_station_min);
          }
          break;
        case SC_PAGE_UP:
          {
            if ((zoom_info.pos_station_min > 1)&&(!limit_up))
            {
              //						 int old_level= zoom_info.level;
              //						 zoom_info.level =0;
              
              zoom_info.pos_station_min--;
              zoom_info.pos_station_max-- ;
              
              limit_up = list->Get_Num_Station_Zoom(&zoom_info,LEFT);
              limit_down=0;
              
              //						 zoom_info.level =old_level;
              
              xvt_sbar_set_pos(win_116_zoom_scroll,HVSCROLL,zoom_info.pos_station_min);
              
              list->Set_Zoom_Marker(&zoom_info);
              //scr.scrollpos = zoom_info.pos_station_min;
              xvt_sbar_set_range(scroll_116, HVSCROLL,zoom_info.pos_station_min, zoom_info.pos_station_max-3);
              scr.scrollpos--;//Neu Dick 12.07.98
              xvt_sbar_set_pos(scroll_116,HVSCROLL,scr.scrollpos);//Neu Dick 12.07.98
              list->GetScrollDaten(&scr);
              display_prof116(&Edit_Win116[0]);
              xvt_dwin_invalidate_rect(WIN_117,0); //Fenster updaten
              //xvt_sbar_set_range(scroll_116, HVSCROLL,zoom_info.pos_station_min, zoom_info.pos_station_max-3);
            }
            else
              xvt_sbar_set_pos(win_116_zoom_scroll,HVSCROLL,1);
          }
          break;
        case SC_PAGE_DOWN:
          {
            if ((zoom_info.pos_station_max < ds_info[1])&&(!limit_down))
            {
              //						 int old_level= zoom_info.level;
              //						 zoom_info.level =0;
              
              zoom_info.pos_station_max++ ;
              zoom_info.pos_station_min++ ;
              
              limit_down = list->Get_Num_Station_Zoom(&zoom_info,RIGHT);
              limit_up=0;
              //						 zoom_info.level =old_level;
              
              xvt_sbar_set_pos(win_116_zoom_scroll,HVSCROLL,zoom_info.pos_station_min);
              
              list->Set_Zoom_Marker(&zoom_info);
              //scr.scrollpos = zoom_info.pos_station_min;
              xvt_sbar_set_range(scroll_116, HVSCROLL,zoom_info.pos_station_min, zoom_info.pos_station_max-3);
              scr.scrollpos++;//Neu Dick 12.07.98
              xvt_sbar_set_pos(scroll_116,HVSCROLL,scr.scrollpos);//Neu Dick 12.07.98
              list->GetScrollDaten(&scr);
              display_prof116(&Edit_Win116[0]);
              xvt_dwin_invalidate_rect(WIN_117,0); //Fenster updaten
              //xvt_sbar_set_range(scroll_116, HVSCROLL,zoom_info.pos_station_min, zoom_info.pos_station_max-3);
            }
            else
              xvt_sbar_set_pos(win_116_zoom_scroll,HVSCROLL,zoom_info.pos_station_min);
          }
          break;
        case SC_THUMB:
          {
          }
          break;
        case SC_THUMBTRACK:
          {
          }
          break;
        default:
          break;
        }
      }
      break;
        case WIN_GRAPH_116_TEDIT_54:
          if (!((berechnen)&& (editieren))) //d.h. Längsschnitt
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
                  add_new_datensatz(xdlist1);   //in:util.cpp
                
                
              }
              
            }
            else if (xdEvent->v.ctl.ci.v.textedit.focus_change)
            {
            /*SLIST_ELT e_c;
            char *line;
            if(tx_comment116!=0)
            {
            xvt_tx_clear(tx_comment116);
            if (slist_comment == NULL)
            slist_comment = xvt_slist_create();
            xvt_tx_suspend(tx_comment116);
            for (e_c=xvt_slist_get_first(slist_comment);e_c!=NULL;e_c=xvt_slist_get_next(slist_comment,e_c))
            {
            line = xvt_slist_get(slist_comment,e_c,0L);
            
              if (!xvt_tx_add_par(tx_comment116, USHRT_MAX,line ))
              xvt_dm_post_error("Fehler bei TXEDIT -Ausgabe");
              }
              xvt_tx_resume(tx_comment116);
              
            }*/
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
                  t_pnum = xvt_tx_get_num_pars(tx_comment116);
                  /*		 t_lnum = xvt_tx_get_num_lines(tx_comment116); */
                  
                  if (tx_comment116 != BAD_TXEDIT)
                    for (i=0;i<=t_pnum-1;i++)
                    {
                      t_lpnum = xvt_tx_get_num_par_lines(tx_comment116,i);
                      for (j=0;j<=t_lpnum-1;j++)
                      {
                        xvt_tx_get_line(tx_comment116,i, A_LOCK,j,0);
                        
                        str = xvt_tx_get_line(tx_comment116,i, A_GET,j,&t_len);
                        strncpy(help,str,t_len);
                        help[t_len]='\0';
                        strcpy(line,"CC ");
                        strcat(line,help);
                        strcat(line,"\n");
                        xvt_slist_add_at_elt(slist_comment,NULL,(char*)help,0L);
                        
                        xvt_tx_get_line(tx_comment116,i, A_UNLOCK,j,0);
                      }
                    }
                    
                    ds_info[comment_ds_num] = xvt_slist_count(slist_comment);
                    
                    SaveProfilFile =TRUE;
                    //xvt_tx_destroy(tx_comment116);
                }
            }
            break;
            /*********************************************************************************/
        default:                
          break;
          
    }
    }
    break;
    
  case E_FONT:
  /*
  User issued font command on window menu bar (menu bar at top of
  screen for Mac/CH).
    */
    {
    }
    break;
  case E_TIMER:
  /*
  Timer associated with window went off.
    */
    {
    } 
    
  case E_USER:
    /*Application initiated.*/
    {
      switch (xdEvent->v.user.id) {
        
      case E_USER_AUTOSAVE:
        {
          if (dlg_sonderprofil != NULL_WIN)
          {
            xvt_vobj_destroy(dlg_sonderprofil);
            dlg_sonderprofil = NULL_WIN;
          }
          char buf[200],buf2[200],buf3[200];
          xvt_res_get_str(STR_JA,buf,sizeof(buf));
          xvt_res_get_str(STR_NEIN,buf2,sizeof(buf2));
          xvt_res_get_str(STR_PLOT101_ASK,buf3,sizeof(buf3));
          
          if (SaveProfilFile)
            switch(xvt_dm_post_ask(buf,buf2,0L,buf3))
            //switch (xvt_dm_post_ask("Ja","Nein",0L,"Sollen geänderte Daten\ngesichert werden ?"))
          {
      case RESP_DEFAULT:       //SICHERN
        {
          int ds_tmp = scr.datensatz;  //Datensatz retten
          if (check_border())  // -> util.h  line 1211
          {
            if( GetFeature( "wsp_nodemo" ) )
              save_profildatei(pWPL);
            else
            {
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_DEMO_NOTE_3,buf,sizeof(buf));
              xvt_dm_post_error("%s",buf);
              //xvt_dm_post_note("Speichern bei Demo nicht möglich");
            };
            
            SaveProfilFile = FALSE;
          }
          else
          {
            scr.datensatz = ds_tmp;
            SaveProfilFile = TRUE;
            list->GetScrollDaten(&scr);
          }
        }
        break;
      case RESP_2:             // nicht sichere_datenblock
        SaveProfilFile = FALSE;
        break;
          }
        }
        break;
        
        
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
                mmp.ds_nummer =scr.datensatz;
                xvt_list_set_sel(xdlist1,l-1,TRUE);
                
                neue_datensatz_wahl = TRUE;
                list->GetScrollDaten(&scr);      // Daten in scr einlesen
                display_prof116(&Edit_Win116[0]);
                last_edit_116 =ch_id_116;
                xvt_dwin_invalidate_rect(WIN_117,0);
              }
          }
          /* wenn Rauhigkeit vorhanden ist sie jetzt in jedem Fall ausgewählt */
          if ((typ[scr.datensatz] == RAUHIGKEIT)||(typ[scr.datensatz] == RAUHIGKEIT_KST))
          {
            mmp.ds_nummer =scr.datensatz;
            mmp.hor_m_down = pmm.minX;
            
            if (win_dlg_154 == NULL_WIN)
              //					  if ((ch_id_116 < 10)||(ch_id_116 > 14))  // hat Edit-Feld 3.Spalte Focus ?
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
              else
              {
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
          if ((typ[scr.datensatz] != AXM)&&(typ[scr.datensatz] != AYM)&&(typ[scr.datensatz] != DPM))
          {
            for (int l=0;l<=ds_info[0];l++)
              if ((typ[l]==AXM)||(typ[l]==AYM)||(typ[l]==DPM))
              {  //den ersten finden
                scr.datensatz=l;
                xvt_list_set_sel(xdlist1,l-1,TRUE);
                
                neue_datensatz_wahl = TRUE;
                list->GetScrollDaten(&scr);      // Daten in scr einlesen
                display_prof116(&Edit_Win116[0]);
                last_edit_116 =ch_id_116;
                xvt_dwin_invalidate_rect(WIN_117,0);
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
                xvt_list_set_sel(xdlist1,l-1,TRUE);
                
                neue_datensatz_wahl = TRUE;
                list->GetScrollDaten(&scr);      // Daten in scr einlesen
                display_prof116(&Edit_Win116[0]);
                last_edit_116 =ch_id_116;
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
      case E_USER_ZOOM:
        {
          /* Scrollbar  anlegen*/
          if (win_116_zoom_scroll==NULL_WIN)
          {
            RCT rc117;
            xvt_vobj_get_outer_rect(WIN_117,&rc117);
            xvt_rect_set(&rect_116,rc117.left,rc117.bottom,rc117.right,rc117.bottom+21);
            //xvt_rect_set(&rect_116,19,647,577,668);
            xvt_ctl_create(WC_HSCROLL, &rect_116, 0L, xdWindow,0L,0L,WIN116_ZOOM_SCROLL);
            win_116_zoom_scroll = xvt_win_get_ctl(xdWindow,WIN116_ZOOM_SCROLL);
          }
          xvt_sbar_set_range(win_116_zoom_scroll, HVSCROLL, 1,ds_info[1]+1-(zoom_info.pos_station_max-zoom_info.pos_station_min));
          xvt_sbar_set_pos(win_116_zoom_scroll,HVSCROLL,zoom_info.pos_station_min);
          xvt_sbar_set_range(scroll_116, HVSCROLL,zoom_info.pos_station_min,max(zoom_info.pos_station_min+1,zoom_info.pos_station_max-3));
          xvt_scr_set_focus_vobj(Edit_Win116[0]); //und den Focus daraf plazieren
        }
        break;
      case E_USER_ZOOM_NORMAL:
        {
          limit_up=0;
          limit_down=0;
          win_116_zoom_scroll = xvt_win_get_ctl(xdWindow,WIN116_ZOOM_SCROLL);
          if (win_116_zoom_scroll!= NULL_WIN)
          {
            xvt_vobj_destroy(win_116_zoom_scroll); //Scroller für Zoomen löschen
            win_116_zoom_scroll = NULL_WIN;
          }
          zoom_info.mouse_pos_left_x=zoom_info.mouse_pos_right_x=zoom_info.pos_station_min=zoom_info.pos_station_max=0;
          
          xvt_sbar_set_range(scroll_116, HVSCROLL, 1, ds_info[1]-3);
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

void GaussRuecksprungCheck(void)
{
  char *ProfTypStr=new char[40];
  
  if(list->check_sort() && !GaussProfilMitRuecksprung) // Sortierung der Stationen testen
  {            
    GaussProfilMitRuecksprung=TRUE;
    strcpy(ProfTypStr,"GaussRücksprung");
    if(WIN_116!=NULL_WIN)
      xvt_vobj_set_title(xdedit10,ProfTypStr);
    else
      if (WIN120!=NULL_WIN)
        xvt_vobj_set_title(win_120[48],ProfTypStr);
  }
  if(!list->check_sort() && GaussProfilMitRuecksprung) // Sortierung der Stationen testen
  {            
    GaussProfilMitRuecksprung=FALSE;
    strcpy(ProfTypStr,"offenes Profil");
    if(WIN_116!=NULL_WIN)
      xvt_vobj_set_title(xdedit10,ProfTypStr);
    else
      if (WIN120!=NULL_WIN)
        xvt_vobj_set_title(win_120[48],ProfTypStr);
  }
  delete [] ProfTypStr;
}

