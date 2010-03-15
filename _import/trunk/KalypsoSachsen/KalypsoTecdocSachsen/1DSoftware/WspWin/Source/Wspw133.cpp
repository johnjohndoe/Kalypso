/* wspw133.h: Dxf-Konfiguration */

#include <windows.h>
#include "xvt.h"

#include "wspwin.h"
#include "wsphilfe.h"
#include "resource.h"
#include "..\..\wspdlg\include\export.h"

#include "plot.h"
#include "bce_allg.h"


#define WIN_RES_ID WIN_LAYER
#define WIN_FLAGS 0x883L
#define WIN_CLASS ""
#define WIN_BORDER W_DOC

// globale Variablen
long c_tab[8] =
{   COLOR_RED,      //1...7, 8 nach Linienbreite
COLOR_YELLOW,
COLOR_GREEN,
COLOR_CYAN,
COLOR_BLUE,
COLOR_MAGENTA,
COLOR_BLACK,
COLOR_WHITE  };

char color_tab[8][10] =
{ "ROT",
"GELB",
"GRÜN",
"CYAN",
"BLAU",
"MAGENTA",
"SCHWARZ",
"LINIENTYP" } ;

long color_pos_top = 110,
color_pos_left= 370;
int top,left;
WINDOW lbox;
RCT color_rct;
PLOTTER plotter_def[32];
char lbox_line[25],*pchr;
short selected_color,w133_selected;
BOOLEAN save=FALSE;

extern  XVT_HELP_INFO hi;
extern BYTE  WindowsPlatformId; //Windows Version -->wspwin.cpp
extern double sf;
extern COLOR WspwinMainBackgroundColor; //globale Hintergrunndfarbe für alle Fenster


/*************   GHJ   *************/
static WNDPROC defWndProc;
LRESULT CALLBACK Win133WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  switch (uMsg)
  {
  case WM_HELP:
    {
      LPHELPINFO lphi = (LPHELPINFO)lParam;
      if (hi!=NULL_HELP_INFO)
      {
        xvt_help_display_topic(hi, HID_KAPITEL_6_7_1_3);
      }
    }
    break;
    
  default:
    break;
  }
  return CallWindowProc(defWndProc, hwnd, uMsg, wParam, lParam);
}
/**************************************/

/*	Handler for window WIN_LAYER ("Plotter-Konfiguration")*/
long XVT_CALLCONV1
#if XVT_CC_PROTO
WIN_LAYER_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
WIN_LAYER_eh XVT_CALLCONV2 (xdWindow, xdEvent)
WINDOW xdWindow;
EVENT *xdEvent;
#endif
{
  short xdControlId = xdEvent->v.ctl.id;
  
  switch (xdEvent->type) {
  case E_CREATE:
    {
      /*************   GHJ   *************/
      defWndProc = (WNDPROC)GetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC);
      SetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC, (LONG)&Win133WindowProc);
      /***********************************/ 
      lbox = xvt_win_get_ctl(xdWindow,WIN_LAYER_LBOX_1);
      NewSetFontFunc(lbox);
      
      /* Bildschirm anpassen*/
      RCT rct_win;
      
      if (WindowsPlatformId == VER_PLATFORM_WIN32_WINDOWS) //=Windows95
        color_pos_left= 370-45;
      
      xvt_vobj_get_outer_rect(xdWindow,&rct_win);
      rct_win.right=(short)(rct_win.right/sf);
      rct_win.bottom=(short)(rct_win.bottom/sf);
      xvt_vobj_move(xdWindow,&rct_win);
      
      
      for (int j=0;j<=3;j++)
        for (int i=0;i<2;i++)
        {
          CBRUSH brush;
          DRAW_CTOOLS ctools;
          xvt_dwin_get_draw_ctools(xdWindow,&ctools);
          top=color_pos_top;
          
          
          left=color_pos_left;
          
          brush.pat =   PAT_SOLID;
          brush.color = c_tab[j*2+i];
          xvt_dwin_set_cbrush (xdWindow,&brush);
          xvt_dwin_set_std_cpen(xdWindow,TL_PEN_BLACK);
          xvt_dwin_set_draw_mode(xdWindow,M_COPY);
          xvt_rect_set(&color_rct,left+i*40, top+j*40,
            left+(i+1)*40,top+(j+1)*40);
          xvt_dwin_draw_rect(xdWindow,&color_rct);
        }
        
        if(!read_plotter_cfg(plotter_def))     //->plot.cpp
          xvt_vobj_destroy(xdWindow);
        else
        {
          for (int i=0;i<32;i++)  // 32 Layer einlesen
          {
            memset(lbox_line,' ',25);  // -->WIN95
            strcpy(lbox_line,plotter_def[i].layer);
            pchr = strrchr(lbox_line,'\0');
            pchr[0]=' ';
            pchr = &lbox_line[16];//Dick 22.06.99 12->16
            strcpy(pchr,color_tab[plotter_def[i].color-1]);
            xvt_list_add(lbox,i,lbox_line);
          }
        } //-else
    }
    break;
  case E_DESTROY:
    {
    }
    return 0L;
  case E_FOCUS:
    {
      /*	Window has lost or gained focus.	*/
      if (xdEvent->v.active)  {
        /*	Window has gained focus	*/
      } else {
        /*	Window has lost focus*/
      }
    }
    break;
  case E_SIZE:
    {
    }
    break;
  case E_UPDATE:
    {
      xvt_dwin_clear(xdWindow,WspwinMainBackgroundColor);
      for (int j=0;j<=3;j++)
        for (int i=0;i<2;i++)
        {
          CBRUSH brush;
          DRAW_CTOOLS ctools;
          xvt_dwin_get_draw_ctools(xdWindow,&ctools);
          brush.pat =   PAT_SOLID;
          brush.color = c_tab[j*2+i];
          xvt_dwin_set_cbrush (xdWindow,&brush);
          xvt_dwin_set_std_cpen(xdWindow,TL_PEN_BLACK);
          xvt_dwin_set_draw_mode(xdWindow,M_COPY);
          xvt_rect_set(&color_rct,left+i*40, top+j*40,
            left+(i+1)*40,top+(j+1)*40);
          xvt_dwin_draw_rect(xdWindow,&color_rct);
        }
        
    }
    break;
  case E_CLOSE:
    {
      xvt_vobj_destroy(xdWindow);
    }
    break;
  case E_CHAR:
    {
    }
    break;
  case E_MOUSE_UP:
    {
      if ((xdEvent->v.mouse.where.h>color_pos_left)&&(xdEvent->v.mouse.where.h<color_pos_left+80)&&
        (xdEvent->v.mouse.where.v>color_pos_top)&&(xdEvent->v.mouse.where.v<color_pos_top+160))
      {
        short mx,my;
        mx =xdEvent->v.mouse.where.h;
        my =xdEvent->v.mouse.where.v;
        
        for (int j=0;j<=3;j++)
          for (int i=0;i<2;i++)
          {
            if ( (mx>color_pos_left+i*40)&&(mx<color_pos_left+(i+1)*40)&&
              (my>color_pos_top +j*40)&&(my<color_pos_top+(j+1)*40)   )
              selected_color=(i+2*j)+1;
          }
          if (w133_selected>=0)
          {
            plotter_def[w133_selected].color = selected_color;
            xvt_list_rem(lbox,w133_selected);
            memset(lbox_line,' ',25);
            strcpy(lbox_line,plotter_def[w133_selected].layer);
            pchr = strrchr(lbox_line,'\0');
            pchr[0]=' ';
            pchr = &lbox_line[16];//Dick 22.06.99 12->16
            strcpy(pchr,color_tab[plotter_def[w133_selected].color-1]);
            xvt_list_add(lbox,w133_selected,lbox_line);
            save = TRUE;
          }
          else
          {
            char buf[200];//Dick 26.11.99
            xvt_res_get_str(STR_WSPW133_NOTE_1,buf,sizeof(buf));
            xvt_dm_post_note("%s",buf);
            //xvt_dm_post_note("Bitte zuerst einen\nLayer wählen !");
          }
          
      }
    }
    break;
  case E_MOUSE_DOWN:
    {
    }
    break;
  case E_COMMAND:
    {
      /*	No menubar was associated with this window	*/
    }
    break;
  case E_CONTROL:
    {
      switch(xdControlId) {
      case WIN_LAYER_LBOX_1: /* "List Box 1" */
        {		/*	List box was operated.	*/
          if (xdEvent->v.ctl.ci.v.lbox.dbl_click)
          {/*	double click*/
          }
          else
          {	/*	single click*/
            w133_selected = xvt_list_get_sel_index(lbox);
          }
        }
        break;
      case WIN_LAYER_PB_CANCEL: /* "Abbrechen" */
        {
          if (save)
          {
            char buf[200],buf2[200],buf3[200],buf4[200];
            xvt_res_get_str(STR_JA,buf,sizeof(buf));
            xvt_res_get_str(STR_NEIN,buf2,sizeof(buf2));
            xvt_res_get_str(STR_ZURUECK,buf3,sizeof(buf3));
            xvt_res_get_str(STR_SICHERE_DATENBLOCK,buf4,sizeof(buf4));
            switch (xvt_dm_post_ask(buf,buf2,buf3,"%s",buf4))
              //switch (xvt_dm_post_ask("Ja","Nein","Zurück","Daten wurden geändert !\nSpeichern vor Schließen\ndes Fensters ?"))
            {
            case RESP_DEFAULT:       //SICHERN
              {
                save_plotter_cfg(plotter_def); //->plot.cpp
                xvt_vobj_destroy(xdWindow);
              }
              break;
            case RESP_2:
              xvt_vobj_destroy(xdWindow);
              break;             //zurück
            case RESP_3:break;
            }
          }
          else
            xvt_vobj_destroy(xdWindow);
        }
        break;
      case WIN_LAYER_PB_OK: /* "Ok" */
        {
          if (save)
            save_plotter_cfg(plotter_def); //->plot.cpp
          xvt_vobj_destroy(xdWindow);
        }
        break;
      default:
        break;
      }
    }
    break;
  case E_TIMER:
    /*	Timer associated with window went off.	*/
    {
    }
    break;
  case E_USER:
    /*	Application initiated.	*/
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
  xvt_tx_process_event(xdWindow, xdEvent);
  return 0L;
}
