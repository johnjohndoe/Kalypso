/*************************************************************
*				  WSPW117.CPP                                    *
*				  08.02.1995                                     *
*	      Darstellen der Grafik                               *
**************************************************************/

#include <windows.h>
#include "xvt.h"

#include "wsphilfe.h"
#include "wspwin.h"
#include "resource.h"

#include "global_types.h"
#include "global.h"
#include "typen.h"
#include "..\..\wspdlg\include\export.h"

#include "paint.h"
#include "printer.h"
#include "dis_prof.h"


/* 	Information about the window  */
/*
#define WIN_RES_ID WIN_GRAFIK_117
#define WIN_FLAGS 0x800L
#define WIN_CLASS ""
#define WIN_BORDER W_PLAIN
*/

WINDOW win_dlg_154;
WINDOW WIN_MOUSE_X,WIN_MOUSE_Y;
WINDOW dlg_zoom_1;
RCT rct_zoom;
RCT mouse_win;
struct _MMP mmp;
DRAW_CTOOLS rect_tools;
BOOLEAN win117_mouse_down = FALSE;
BOOLEAN win117_mouse_move = FALSE;
BOOLEAN win117_mouse_move_aktiv = FALSE;
int zoom_zustand = 0;
int pos_zustand = 0;
double factorX, factorY;
double m,n;
int hor_pnt, ver_pnt;//wie m,n nir int für dlg137
double hor_mouse_pos = 0.0, ver_mouse_pos = 0.0;
char hor[100],ver[100];
char strtemp[100];

extern DRAW_DIMENSIONS WspDrawDimensions;
extern WINDOW *dlg154_editwin,  win_dlg_110, *Dlg110_edit_win; //->Dialog 154:edit-Feld:Station Anfang,Ende,Offset...
extern PNT profil_pnt[STRANGANZAHL];
extern ZOOM zoom_info;
extern BOOLEAN ZOOM_IN;
extern MinMax pmm;
extern int mark_pos;
extern BOOLEAN is_open_dlg_137;
extern BOOLEAN Edit_Fehler, Einf, Edit_Fehler_Y, Edit_Fehler_Z;
extern WINDOW Edit_Win116[15], dlg_sonderprofil;
extern BOOLEAN fehl_korr; //Hilfsmerker für die Fehlerkorrektur beim Trennflächen im ZOOM-Modus
extern BOOLEAN berechnen, editieren;


/*************   GHJ   *************/
extern  XVT_HELP_INFO hi;
static WNDPROC defWndProc;
LRESULT CALLBACK Win117WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  switch (uMsg)
  {
  case WM_HELP:
    {
      LPHELPINFO lphi = (LPHELPINFO)lParam;
      if (hi!=NULL_HELP_INFO)
      {
        xvt_help_display_topic(hi, HID_KAPITEL_6_3_4);
      }
    }
    break;
    
  default:
    break;
  }
  return CallWindowProc(defWndProc, hwnd, uMsg, wParam, lParam);
}
/**************************************/

void draw_zoomrect(WINDOW win,RCT rct)
{
  PNT pnt;
  DRAW_CTOOLS rect_tools;
  
  if (rct.left<rct.right)
  {
    rct.top =40;
    rct.bottom=300;
    rect_tools.pen.width = 1;
    rect_tools.pen.pat = PAT_CROSS;
    rect_tools.pen.color = COLOR_BLACK;
    
    xvt_dwin_set_draw_ctools(win, &rect_tools);
    xvt_dwin_set_draw_mode(win,M_XOR);
    
    pnt.h = rct.left;
    pnt.v = rct.top;
    xvt_dwin_draw_set_pos(win, pnt);
    pnt.v = rct.bottom;
    xvt_dwin_draw_line(win, pnt);     //links
    
    xvt_dwin_draw_set_pos(win, pnt);
    pnt.h = rct.right;
    xvt_dwin_draw_line(win, pnt);     //unten
    
    xvt_dwin_draw_set_pos(win, pnt);
    pnt.v = rct.top;
    xvt_dwin_draw_line(win, pnt);     //rechts
    
    xvt_dwin_draw_set_pos(win, pnt);
    pnt.h = rct.left;
    xvt_dwin_draw_line(win, pnt);     //oben
  }
}; // draw_zoomrect

/*************************************************************/


/*  	Handler for window WIN_GRAFIK_117 ("")  */
long XVT_CALLCONV1
#if XVT_CC_PROTO
WIN_GRAFIK_117_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
WIN_GRAFIK_117_eh XVT_CALLCONV2 (xdWindow, xdEvent)
WINDOW xdWindow;
EVENT *xdEvent;
#endif
{
  short xdControlId = xdEvent->v.ctl.id;
  if(WspDrawDimensions.isPrinting==0)//Dick 27.08.99 sonst beim druckvorgang absturz
    switch (xdEvent->type) {
  case E_CREATE:
    {
      /*************   GHJ   *************/
      defWndProc = (WNDPROC)GetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC);
      SetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC, (LONG)&Win117WindowProc);
      /***********************************/
      
      WIN_117 = xdWindow;
      win_dlg_154=NULL_WIN;
      win_dlg_110=NULL_WIN;//Dick 18.04.99
      SetWin117Dim();
      
      for (int p=0;p<150;p++)  //initialisieren
        profil_pnt[p].v =profil_pnt[p].h =0;
      
      zoom_info.level = 0;
      zoom_info.min_old=0;
      zoom_info.max_old=0;
      
      rct_zoom.left=  0;
      rct_zoom.right= -1;
      ZOOM_IN = FALSE;
      zoom_info.mouse_pos_left_x=zoom_info.mouse_pos_right_x=0;
      
      xvt_app_get_default_ctools(&rect_tools);
      rect_tools.back_color = COLOR_WHITE;
      rect_tools.fore_color = COLOR_BLACK;
      
      xvt_dwin_set_draw_ctools(xdWindow, &rect_tools);
      xvt_dwin_set_back_color(xdWindow, COLOR_WHITE);
      
      mmp.mouse_down_v =0;
      mmp.mouse_down_h = mmp.mouse_h = WspDrawDimensions.DX;
      mmp.mouse_v =0;
      mmp.position_mouse_down =mmp.position_mouse_up=0;
      mmp.last_rct.top=mmp.last_rct.left=mmp.last_rct.bottom=mmp.last_rct.right=0;
      mmp.active=FALSE;
      //***** Static Text Fenster:Anzeige der Mouse Position *******
      
      /*********** GHJ ************/
      mouse_win.top    = (int)(res_factor*340);
      mouse_win.left   = (int)(res_factor*200);//Dick 20.04.99 250->230 //Dick 1.05.99 230->200
      mouse_win.bottom = (int)(res_factor*370);
      mouse_win.right  = (int)(res_factor*350);
      WIN_MOUSE_X = xvt_ctl_create(WC_TEXT, &mouse_win, 0L, xdWindow,
        0L,0L,2998);
      xvt_vobj_set_title(WIN_MOUSE_X,"");
      mouse_win.top    = (int)(res_factor*340);
      mouse_win.left   = (int)(res_factor*360);
      mouse_win.bottom = (int)(res_factor*370);
      mouse_win.right  = (int)(res_factor*470);  //Dick 20.04.99 460->470
      WIN_MOUSE_Y = xvt_ctl_create(WC_TEXT, &mouse_win, 0L, xdWindow,
        0L,0L,2999);
      
      paint->draw_min_max_werte(xdWindow);
      
      paint->draw_rect(xdWindow);        //  in paint->cpp
      
      paint->Paint117(WIN_117,&pmm);   //  Profillinie zeichnen
    }
    break;
  case E_DESTROY:
    {
      if (win_dlg_154 != NULL_WIN)
      {
        xvt_vobj_destroy(win_dlg_154);
        win_dlg_154=NULL_WIN;
      }
      if (win_dlg_110 != NULL_WIN)
      {
        xvt_vobj_destroy(win_dlg_110);
        win_dlg_110=NULL_WIN;
      }
      if (dlg_zoom_1 != NULL_WIN)
        xvt_vobj_destroy(dlg_zoom_1);
      WIN_117 = NULL_WIN;
      zoom_info.level =0;
    }
    return 0L;
  case E_FOCUS:
    {
      if (xdEvent->v.active)
      { 	/*  		Window has gained focus		*/
      }
      else
      {  /*			Window has lost focus		*/
      }
    }
    break;
    
    
  case E_UPDATE:
    /*   		Window requires updating.      	*/
    {
      xvt_dwin_clear(xdWindow,COLOR_WHITE);
      paint->draw_rect(xdWindow);
      paint->draw_117(xdWindow,&mmp);  /*andere Datensätze darstellen, incl. Profillinie */
      paint->draw_marker(xdWindow,mark_pos);
    }
    break;
  case E_CLOSE:
    {
      xvt_vobj_destroy(xdWindow);
    }
    break;
  case E_CHAR:
    /*	Character typed.	*/
    {
      switch(xdEvent->v.chr.ch)
      {
      case K_INS:     /*  Einfügen einer neuen Koordinate */
        {
          if (! is_open_dlg_137)
          {
            if (!xvt_dlg_create_res(WD_MODELESS, DLG_137, EM_ALL, DLG_137_eh, 0L))
              xvt_dm_post_error("Can't open DLG_137");
          }
        }
        break;
      }
    }
    break;
  case E_MOUSE_UP:
    if(!Edit_Fehler&&!Einf&&!Edit_Fehler_Y&&!Edit_Fehler_Z)	{
      win117_mouse_down=FALSE;
      win117_mouse_move=FALSE;//Dick 25.7.98
      win117_mouse_move_aktiv=FALSE;
      if ( ZOOM_IN)
      {
        zoom_zustand = 2;  //als Test für draw_zoomrect
        ZOOM_IN =FALSE;
        
        rct_zoom.left=  0;
        rct_zoom.right= -1;
        //rechten Stationswert ermitteln
        pos_zustand = paint->Get_Mouse_Position(xdEvent->v.mouse.where.h,xdEvent->v.mouse.where.v);   
        if (pos_zustand)
        {
          zoom_info.station_max = list->Get_Num_Station(1 ,&pos_zustand,1);
          zoom_info.pos_station_max = pos_zustand;
        }
        zoom_info.min_old = zoom_info.pos_station_min;
        zoom_info.max_old = zoom_info.pos_station_max;
        if (zoom_info.station_max==BCE_NAN)
        { //Fehler
          char buf[200];//Dick 26.11.99
          xvt_res_get_str(STR_ZOOM_ERROR_1,buf,sizeof(buf));
          xvt_dm_post_error("%s",buf);
          //xvt_dm_post_error("Fehler beim ermitteln der Zoomdaten !");
        }
        else
        { /*  sind mehr als 2 Stationen im Zoombereich*/
          if (zoom_info.pos_station_max-2 > zoom_info.pos_station_min)
          {
            zoom_info.mouse_pos_right_x = xdEvent->v.mouse.where.h;
            
            if (zoom_info.level==-1)
              zoom_info.level=1;
            else	zoom_info.level++;
            
            for (int p=0;p<150;p++)
              profil_pnt[p].v =profil_pnt[p].h =0;
            
            
            /* Scrollbar in WIN_116 anlegen*/
            xdEvent->type =E_USER;
            xdEvent->v.user.id=E_USER_ZOOM;
            xvt_win_dispatch_event(WIN_116,xdEvent);
            /* ENDE Scrollbar in WIN_116  */
            
            list->Set_Zoom_Marker(&zoom_info);
            scr.scrollpos = zoom_info.pos_station_min;
            list->GetScrollDaten(&scr);
            display_prof116(&Edit_Win116[0]);
            xvt_dwin_invalidate_rect(WIN_117,0); //Fenster updaten
            zoom_zustand = 0;
          }
          else /* zu wenig Stationen markiert */
          {
            char buf[200];//Dick 26.11.99
            xvt_res_get_str(STR_ZOOM_ERROR_1,buf,sizeof(buf));
            xvt_dm_post_error("%s",buf);
            //xvt_dm_post_note("Bitte markieren Sie mindestens 4 Stationen");
            zoom_zustand = 0;
            
            xdEvent->type =E_USER;
            xdEvent->v.user.id=EVENT_ZOOM_NORMAL;
            xvt_win_dispatch_event(WIN_117,xdEvent);
            xdEvent->v.user.id=E_USER_ZOOM_NORMAL;
            xvt_win_dispatch_event(WIN_116,xdEvent);
            xvt_dwin_invalidate_rect(WIN_117,0); //Fenster updaten
          }
        }
      }
      else if (!ZOOM_IN)
      {
        xvt_win_release_pointer();
        mmp.mouse_h=xdEvent->v.mouse.where.h;
        
        if ((typ[scr.datensatz]==RAUHIGKEIT)||(typ[scr.datensatz]==RAUHIGKEIT_KST)
          ||(typ[scr.datensatz]==AXM)||(typ[scr.datensatz]==AYM)||(typ[scr.datensatz]==DPM))//Dick 18.04.99
        {
          if (xdEvent->v.mouse.where.h>=544)   // Maus rechts von hor.Koord.-line
            xdEvent->v.mouse.where.h =545;
          mmp.position_mouse_up =paint->Get_MRPosition(xdEvent->v.mouse.where.h,xdEvent->v.mouse.where.v);
        }
        else
        {
          mmp.position_mouse_up = paint->Get_Mouse_Position(xdEvent->v.mouse.where.h,xdEvent->v.mouse.where.v);
          
          fehl_korr=TRUE;                                                                                     
          mmp.horizontal =list->Get_Num_Station(scr.datensatz ,&mmp.position,0);
          fehl_korr=FALSE;
        }
        
        
        if ((xdEvent->v.mouse.button ==0)&&(! is_open_dlg_137))
        {
          paint->edit_list_pnt(&mmp,&pmm);
          xvt_dwin_invalidate_rect(WIN_117,0);
        }
        
      }
    }
    break;
  case E_MOUSE_DOWN:
    if(!Edit_Fehler&&!Einf&&!Edit_Fehler_Y&&!Edit_Fehler_Z)
    {
      if(is_open_dlg_137 &&typ[scr.datensatz]==BUHNE)
      {
        for (int p=0;p<150;p++)  //initialisieren
          profil_pnt[p].v =profil_pnt[p].h =0;
      }
      if (ZOOM_IN)
      {
        win117_mouse_down=TRUE;
        win117_mouse_move=FALSE;//Dick 25.7.98
        if (dlg_sonderprofil != NULL_WIN)
        {
          xvt_vobj_destroy(dlg_sonderprofil);
          dlg_sonderprofil = NULL_WIN;
        }
        pos_zustand=0;
        
        if (dlg_zoom_1 != NULL_WIN)
        {
          zoom_zustand = 1;  //als Test für draw_zoomrect
          xvt_vobj_destroy(dlg_zoom_1);
          dlg_zoom_1=NULL_WIN;
          zoom_info.mouse_pos_left_x = xdEvent->v.mouse.where.h;
          
          //linken Stationswert ermitteln
          zoom_info.min_old = zoom_info.pos_station_min;
          zoom_info.max_old = zoom_info.pos_station_max;
          
          pos_zustand = paint->Get_Mouse_Position(xdEvent->v.mouse.where.h,xdEvent->v.mouse.where.v);
          //				pos_zustand = pos_zustand + zoom_info.min_old-1;
          if (pos_zustand)
          {
            zoom_info.station_min = list->Get_Num_Station(1 ,&pos_zustand,1);
            zoom_info.pos_station_min=pos_zustand;
          }
          if (zoom_info.station_min==BCE_NAN)
          { //Fehler
            char buf[200];//Dick 26.11.99
            xvt_res_get_str(STR_ZOOM_ERROR_1,buf,sizeof(buf));
            xvt_dm_post_error("%s",buf);
            //xvt_dm_post_error("Fehler beim ermitteln der Zoomdaten !");
            ZOOM_IN =FALSE;
          }
          else
          {
            rct_zoom.left= xdEvent->v.mouse.where.h;
          }
        }
      }   //  -if (ZOOM_IN)
      else if (zoom_zustand == 0)  //kein Zoom
      {
        win117_mouse_down=FALSE;
        xvt_win_trap_pointer(WIN_117);
        
        if (xdEvent->v.mouse.button ==1)  //rechte Maus-Taste
          if ((typ[scr.datensatz]!=RAUHIGKEIT)&&(typ[scr.datensatz]!=RAUHIGKEIT_KST)
            ||(typ[scr.datensatz]!=AXM)||(typ[scr.datensatz]!=AYM)||(typ[scr.datensatz]!=DPM))//Dick 18.04.99
          {
            if (! is_open_dlg_137 && !(berechnen && editieren))  // Dialog:Einfügen...
            {
              if (!xvt_dlg_create_res(WD_MODELESS, DLG_137, EM_ALL, DLG_137_eh, 0L))
                xvt_dm_post_error("Can't open DLG_137");
            }
          }
          if (is_open_dlg_137)   // =Modus :Einfügen einer neuen Koordinate
          { // über beide MausTasten möglich
            factorX= WspDrawDimensions.DELTA_X / pmm.distanceX;
            factorY= (220*res_factor) / pmm.distanceY;		// GHJ
            
            m=xdEvent->v.mouse.where.h;
            n=xdEvent->v.mouse.where.v;
            hor_pnt=xdEvent->v.mouse.where.h;
            ver_pnt=xdEvent->v.mouse.where.v;
            hor_mouse_pos = (  (m - WspDrawDimensions.DX) / factorX) + pmm.minX ;
            ver_mouse_pos =  -(((n - (255*res_factor) )/ factorY ) - pmm.minY);		// GHJ
            win117_mouse_down=TRUE;
            mmp.position_mouse_down  = paint->Get_Mouse_Position(xdEvent->v.mouse.where.h,xdEvent->v.mouse.where.v);//Test
            break;
          }
          if ((xdEvent->v.mouse.button ==0)&&(! is_open_dlg_137))
          { //  *****  linke Maus-Taste  ******
            if ((typ[scr.datensatz]==RAUHIGKEIT)||(typ[scr.datensatz]==RAUHIGKEIT_KST)
              ||(typ[scr.datensatz]==AXM)||(typ[scr.datensatz]==AYM)||(typ[scr.datensatz]==DPM))//Dick 18.04.99
              xvt_dwin_invalidate_rect(WIN_117,0);
            mmp.ds_nummer =scr.datensatz;
            mmp.active = TRUE;
            win117_mouse_down=1;
            if ((typ[scr.datensatz]!=RAUHIGKEIT)&&(typ[scr.datensatz]!=RAUHIGKEIT_KST)
              ||(typ[scr.datensatz]!=AXM)||(typ[scr.datensatz]!=AYM)||(typ[scr.datensatz]!=DPM))//Dick 18.04.99
            {
              if ((typ[scr.datensatz]==GELAENDEHOEHE)||
                (typ[scr.datensatz]==GELAENDE2)||
                (typ[scr.datensatz]==FLAECHE))
                mmp.position_mouse_down = paint->Get_Mouse_Position_GH(xdEvent->v.mouse.where.h,xdEvent->v.mouse.where.v);
              else
                mmp.position_mouse_down = paint->Get_Mouse_Position(xdEvent->v.mouse.where.h,xdEvent->v.mouse.where.v);
              if (mmp.position_mouse_down>0)//Dick 25.7.98
                mmp.hor_m_down =list->Get_Num_Station(scr.datensatz ,&mmp.position_mouse_down,1);
            }
            mmp.mouse_down_h =mmp.mouse_h = xdEvent->v.mouse.where.h;
            mmp.mouse_down_v =mmp.mouse_v = xdEvent->v.mouse.where.v;
            mmp.last_rct.left = mmp.mouse_h; // für editieren Trennflaechen
            
            if ((typ[scr.datensatz]==RAUHIGKEIT)||(typ[scr.datensatz]==RAUHIGKEIT_KST)
              ||(typ[scr.datensatz]==AXM)||(typ[scr.datensatz]==AYM)||(typ[scr.datensatz]==DPM))//Dick 18.04.99
            {
              mmp.active = TRUE;  // =alte Grafik löschen
              
              paint->edit_list_pnt(&mmp,&pmm);
              
              if (xdEvent->v.mouse.where.h<=30)   // Maus links von hor.Koord.-line
                xdEvent->v.mouse.where.h =31;
              
              mmp.position_mouse_down =paint->Get_MRPosition(xdEvent->v.mouse.where.h,xdEvent->v.mouse.where.v);
              
              mmp.hor_m_down =list->Get_Num_Station(scr.datensatz ,&mmp.position_mouse_down,1);
              
              if ((typ[scr.datensatz]==RAUHIGKEIT)||(typ[scr.datensatz]==RAUHIGKEIT_KST))//Dick 18.04.99
              {
                if (win_dlg_154 == NULL_WIN)
                {
                  if (!xvt_dlg_create_res(WD_MODELESS,DLG_154, EM_ALL, DLG_154_eh, 0L))
                    xvt_dm_post_error("Can't open dialog 154");
                  
                  mmp.mouse_down_h =mmp.mouse_h = xdEvent->v.mouse.where.h;
                }
                else
                {     // dlg154 bereits offen !
                  char hor[15];
                  WINDOW *pwin=dlg154_editwin;
                  sprintf(hor,"%.4lf",mmp.hor_m_down);
                  xvt_vobj_set_title(*pwin,hor);
                  xvt_vobj_set_title(*(++pwin),hor);
                }
              }
              if ((typ[scr.datensatz]==AXM)||(typ[scr.datensatz]==AYM)||(typ[scr.datensatz]==DPM))//Dick 18.04.99
              {
                if (win_dlg_110 == NULL_WIN)
                {
                  if (!xvt_dlg_create_res(WD_MODELESS,DLG_110, EM_ALL, DLG_110_eh, 0L))
                    xvt_dm_post_error("Can't open dialog 110");
                  
                  mmp.mouse_down_h =mmp.mouse_h = xdEvent->v.mouse.where.h;
                }
                else
                {     // dlg154 bereits offen !
                  char hor[15];
                  WINDOW *pwin=Dlg110_edit_win;
                  sprintf(hor,"%.4lf",mmp.hor_m_down);
                  xvt_vobj_set_title(*pwin,hor);
                  xvt_vobj_set_title(*(++pwin),hor);
                }
              }
            }
          } //xx
      }
    }
    break;
  case E_MOUSE_DBL:
    {
      m=xdEvent->v.mouse.where.h;
      n=xdEvent->v.mouse.where.v;
    }
    break;
  case E_MOUSE_MOVE:
    {
      win117_mouse_move=TRUE;//Dick 25.7.98
      xvt_win_set_cursor(WIN_117,CURSOR_PLUS);
      /* Display Mouse-Position [m] in Window117  */
      if(pmm.distanceX!=0)
        factorX= WspDrawDimensions.DELTA_X / pmm.distanceX;
      else
        xvt_dm_post_note("Distanz=0");
      factorY= (220*res_factor) / pmm.distanceY;		// GHJ
      
      mmp.mouse_v=xdEvent->v.mouse.where.v;
      
      mmp.horizontal = (  ((double)(xdEvent->v.mouse.where.h) - WspDrawDimensions.DX) / factorX) + pmm.minX ;//Dick 15.01.99
      mmp.vertikal =  -(((mmp.mouse_v - (255*res_factor) )/ factorY ) - pmm.minY);		// GHJ
      
      xvt_app_get_default_ctools(&rect_tools);
      rect_tools.fore_color = COLOR_RED;
      xvt_dwin_set_draw_ctools(xdWindow, &rect_tools);

      sprintf(hor,"%.4lf",mmp.horizontal);
      sprintf(ver,"%.4lf",mmp.vertikal);
      for( int i = 0; i < (INT)strlen( hor ); i++ )
      {
        if( hor[i] == '.' )
          hor[i + 4] = '\0';
      }

      for( i = 0; i < (INT)strlen( ver ); i++ )
      {
        if( ver[i]=='.') 
          ver[i + 4] = '\0';
      }
          
      strcpy(strtemp,"y=  ");
      strcat(strtemp,hor);
      strcat(strtemp,"m");
      xvt_vobj_set_title(WIN_MOUSE_X,strtemp);
      
      strcpy(strtemp,"z=  ");
      strcat(strtemp,ver);
      strcat(strtemp,"m");
      xvt_vobj_set_title(WIN_MOUSE_Y,strtemp);
      /* ENDE Display Mouse-Position [m] in Window117  */
          
      if ((ZOOM_IN)&&(zoom_zustand==1)&&(win117_mouse_down))  /*  ZOOMEN  */
      {
        draw_zoomrect(xdWindow,rct_zoom);  //altes Rechteck löschen
        if(zoom_info.level > -1)
          rct_zoom.right = xdEvent->v.mouse.where.h;
        draw_zoomrect(xdWindow,rct_zoom);  //Rechteck neuzeichnen
      }
      else
      {
        /************* Grafik  EDITIEREN ***********/
        if ((win117_mouse_down)&&(! is_open_dlg_137)&&(!ZOOM_IN))
        {
              mmp.mouse_h=xdEvent->v.mouse.where.h;
              mmp.ds_nummer =scr.datensatz;
              double pos_pruef; //Dick 30.04.99
              pos_pruef=mmp.position;
              if ((typ[scr.datensatz]==RAUHIGKEIT)||(typ[scr.datensatz]==RAUHIGKEIT_KST)
                ||(typ[scr.datensatz]==AXM)||(typ[scr.datensatz]==AYM)||(typ[scr.datensatz]==DPM))//Dick 18.04.99
              {
                if (xdEvent->v.mouse.where.h>=(int)(WspDrawDimensions.MAIN_DRAW_WIN_X - WspDrawDimensions.DYY-1))   // Maus rechts von hor.Koord.-line
                  xdEvent->v.mouse.where.h =(int)(WspDrawDimensions.DELTA_X + WspDrawDimensions.DX);
                if ((int)xdEvent->v.mouse.where.h<(int)(WspDrawDimensions.DX))   // Maus rechts von hor.Koord.-line
                  xdEvent->v.mouse.where.h =WspDrawDimensions.DX;
                
                mmp.position =paint->Get_MRPosition(xdEvent->v.mouse.where.h,xdEvent->v.mouse.where.v);
              }
              else
                if ((typ[scr.datensatz]==GELAENDEHOEHE)||(typ[scr.datensatz]==GELAENDE2)||(typ[scr.datensatz]==FLAECHE))
                  mmp.position = paint->Get_Mouse_Position_GH(xdEvent->v.mouse.where.h,xdEvent->v.mouse.where.v);            
                else
                  mmp.position = paint->Get_Mouse_Position(xdEvent->v.mouse.where.h,xdEvent->v.mouse.where.v);
                
                if(pos_pruef!=mmp.position) //Dick 30.04.99
                  win117_mouse_move_aktiv=FALSE;
                if(mmp.position < mmp.position_mouse_down && ((typ[scr.datensatz]==AXM)||(typ[scr.datensatz]==AYM)||(typ[scr.datensatz]==DPM)||(typ[scr.datensatz]==RAUHIGKEIT)||(typ[scr.datensatz]==RAUHIGKEIT_KST)))
                  mmp.position=mmp.position_mouse_down;//Dick 4.08.99
                if (mmp.position>0 )//Dick 25.7.98
                  mmp.horizontal =list->Get_Num_Station(scr.datensatz ,&mmp.position,!win117_mouse_move_aktiv);
                else                
                  mmp.position=mmp.position_mouse_down;//Dick 18.08.98
                win117_mouse_move_aktiv=TRUE;   
                if (win_dlg_154 != NULL_WIN) 
                  if (*dlg154_editwin!= NULL_WIN)   // =Dialog 154 geöffnet
                  {
                    
                    char hor[15];
                    WINDOW *pwin;
                    pwin = dlg154_editwin;
                    sprintf(hor,"%.4lf",mmp.horizontal);
                    xvt_vobj_set_title(*(++pwin),hor);  //Titel dlg154 setzen
                  }
                  if (win_dlg_110 != NULL_WIN) //Dick 18.04.99
                    if (*Dlg110_edit_win!= NULL_WIN 
                      && ((typ[scr.datensatz]==AXM)||(typ[scr.datensatz]==AYM)||(typ[scr.datensatz]==DPM)))   // =Dialog 110 geöffnet
                    {
                      
                      char hor[15];
                      WINDOW *pwin;
                      pwin = Dlg110_edit_win;
                      sprintf(hor,"%.4lf",mmp.horizontal);
                      xvt_vobj_set_title(*(++pwin),hor);  //Titel dlg154 setzen
                    }
                    
                    mmp.active = TRUE;   // =alte Grafik löschen
                    paint->edit_list_pnt(&mmp,&pmm);
                    list->GetScrollDaten(&scr);
                    display_prof116(&Edit_Win116[0]);
                    SaveProfilFile = TRUE;
            }
          }
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
    break;
  case E_CONTROL:
    {
      switch(xdControlId) {
      case -1:
      default:
        break;
      }
    }
    break;
  case E_FONT:
    {
    }
    break;
  case E_TIMER:
    {
    }
    break;
  case E_USER:
    {
      switch (xdEvent->v.user.id)
      {
      case EVENT_ZOOM_NORMAL:
        {
          zoom_info.level=0;
          zoom_info.min_old=0;
          zoom_info.max_old=0;
          zoom_zustand =  0;
          ZOOM_IN =   FALSE;
          
          rct_zoom.right=0;
          rct_zoom.left=0;
          
          if (dlg_zoom_1 !=NULL_WIN)
          {
            xvt_vobj_destroy(dlg_zoom_1);
            dlg_zoom_1 =NULL_WIN ;
          }
          list->GetMinMax(&pmm,scr.datensatz);
          list->GetScrollDaten(&scr);
          display_prof116(&Edit_Win116[0]);
          xvt_dwin_invalidate_rect(WIN_117,0);
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
