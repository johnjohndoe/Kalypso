/****************************************************************************
*             WSPW121 .CPP  :      Steuerdaten                              *
*          created:   14.10.1994    Andresen                                *
*          modified:  03.12.95      Andresen                                *
*                                                                           *
****************************************************************************/
#include <windows.h>
#include "xvt.h"

#include "wspwin.h"
#include "resource.h"
#include "wsphilfe.h"


#include "global_types.h"
#include "..\..\wspdlg\include\export.h"

#include "util.h"
#include "readprof.h"
#include "plot.h"

/*
Information about the window

  #define WIN_RES_ID WIN_121
  #define WIN_FLAGS 0x1883L
  #define WIN_CLASS ""
  #define WIN_BORDER W_DOC
*/

WINDOW WIN121;
short edit_nmb;
BOOLEAN control_were_changed = FALSE;
BOOLEAN DLG_122_WHAT = FALSE, dlg_cancel = FALSE;



extern WINDOW dlg_166, dlg_162, DLG203, dlg_sonderprofil, WIN120, WIN_116, Menu_Win;
extern BOOLEAN berechnen, editieren, stempel;
extern SLIST header_profil;
extern BOOLEAN Plot_DB;
extern BOOLEAN is_win_121_open;
extern BOOLEAN new_profil;
extern BOOLEAN is_profil_open;
extern COLOR WspwinMainBackgroundColor; //globale Hintergrunndfarbe für alle Fenster
extern BOOLEAN SaveProfilFile;
extern BOOLEAN data_changed;
extern WSP_PROFIL_LISTE* pWPL;

/*************   GHJ   *************/
extern  XVT_HELP_INFO hi;
static WNDPROC defWndProc;
LRESULT CALLBACK Win121WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  switch (uMsg)
  {
  case WM_HELP:
    {
      LPHELPINFO lphi = (LPHELPINFO)lParam;
      if (hi!=NULL_HELP_INFO)
        xvt_help_display_topic(hi, HID_KAPITEL_6_7_1_1_1);
    }
    break;
    
  default:
    break;
  }
  return CallWindowProc(defWndProc, hwnd, uMsg, wParam, lParam);
}
/**************************************/


/*	Handler for window WIN_121 ("Erfassung der Schlüsseldaten") */
long XVT_CALLCONV1
#if XVT_CC_PROTO
WIN_121_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
WIN_121_eh XVT_CALLCONV2 (xdWindow, xdEvent)
WINDOW xdWindow;
EVENT *xdEvent;
#endif
{
  short xdControlId = xdEvent->v.ctl.id;
  
  switch (xdEvent->type)
  {
  case E_CREATE:
    {
      /*************   GHJ   *************/
      defWndProc = (WNDPROC)GetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC);
      SetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC, (LONG)&Win121WindowProc);
      ChangeFontAndSize((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW));	// GHJ
      RECT rect;
      ::SystemParametersInfo(SPI_GETWORKAREA, 0, &rect, 0);
      if (rect.right-rect.left <= 800)
      {
        ::GetClientRect(::GetParent((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW)), &rect);
        ::MoveWindow((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), rect.left, rect.top, rect.right-rect.left, rect.bottom-rect.top, TRUE);
      }
      xvt_vobj_set_visible(xdWindow, TRUE);		// GHJ
      /***********************************/
      WIN121 = xdWindow;
      
      if (dlg_166!= NULL_WIN) // Stationen springen in WIN116
        xvt_vobj_set_visible(dlg_166,FALSE);
      
      if (dlg_162!= NULL_WIN) // Dick 2.12.98
        xvt_vobj_destroy(dlg_162);
      
      if (DLG203!= NULL_WIN) // Dick 2.12.98	
      {
        berechnen=editieren=TRUE;
        xvt_vobj_destroy(DLG203);
      }
      
      if (dlg_sonderprofil != NULL_WIN)
      {
        xvt_vobj_destroy(dlg_sonderprofil);
        dlg_sonderprofil = NULL_WIN;
      }
      if (WIN120 !=NULL_WIN)
        xvt_vobj_set_visible(WIN120,FALSE);
      else if(WIN_116 != NULL_WIN)
        xvt_vobj_set_visible(WIN_116,FALSE);
      
      if (header_profil==NULL)
      {
        char buf[200];//Dick 26.11.99
        xvt_res_get_str(STR_WSPW121_NOTE,buf,sizeof(buf));
        xvt_dm_post_error("%s",buf); //xvt_dm_post_error("Bitte erst ein Profil auswählen !");
        
        if (dlg_sonderprofil != NULL_WIN)
          xvt_vobj_set_visible(dlg_sonderprofil,TRUE);
        if (WIN120 !=NULL_WIN)
          xvt_vobj_set_visible(WIN120,TRUE);
        else if(WIN_116 != NULL_WIN)
          xvt_vobj_set_visible(WIN_116,TRUE);
        xvt_vobj_destroy(xdWindow);
      }
      else
      {
        wsp121_set_title(xdWindow); //--->util.cpp
        xvt_scr_set_focus_vobj(xvt_win_get_ctl(xdWindow,WIN_121_EDIT_16));
      }
    }
    break;
  case E_DESTROY:
    /*   Window has been closed; last event sent to window.*/
    {
      Plot_DB=FALSE;
      
      if (dlg_166!= NULL_WIN) // Stationen springen in WIN116
        xvt_vobj_set_visible(dlg_166,TRUE);
      
      if (WIN120 !=NULL_WIN)
        xvt_vobj_set_visible(WIN120,TRUE);
      else if(WIN_116 != NULL_WIN)
        xvt_vobj_set_visible(WIN_116,TRUE);
      
      WIN121=NULL;//Dick 30.09.99 muß vorher stehen
      if(!stempel)
      {
        xdEvent->type=E_COMMAND; 
        xdEvent->v.cmd.tag =TASK_MENUBAR_13_58_74;   //dlg 162
        xvt_win_dispatch_event(Menu_Win,xdEvent);
      }
      else
      {
        stempel=FALSE;
        berechnen=editieren=FALSE;
        xdEvent->type=E_COMMAND; 
        xdEvent->v.cmd.tag =TASK_MENUBAR_1_91;   //dlg 203
        xvt_win_dispatch_event(Menu_Win,xdEvent);
      }
      
      
    }
    return 0L;
  case E_FOCUS:
    {
    /*
    Window has lost or gained focus.
      */
      if (xdEvent->v.active)
      {      /* 	Window has gained focus	*/
        
        is_win_121_open=((is_profil_open)&&(!new_profil));
        
      }
      else
      {    /* 	Window has lost focus 	*/
        is_win_121_open = FALSE;
      }
    }
    break;
  case E_SIZE:
    {
    }
    break;
  case E_UPDATE:
		/*
    Window requires updating.
    */
    {
      xvt_dwin_clear(xdWindow,WspwinMainBackgroundColor);
    }
    break;
  case E_CLOSE:
    {
      xvt_vobj_destroy(xdWindow);
    }
    break;
  case E_CHAR:	/*			Character typed.		*/
    {
      if (xdEvent->v.chr.ch == K_TAB)
      {
        switch(edit_nmb)
        {
        case WIN_121_EDIT_16:
        case WIN_121_EDIT_17:
        case WIN_121_EDIT_18:
        case WIN_121_EDIT_19:
        case WIN_121_EDIT_20:
          xvt_scr_set_focus_vobj(xvt_win_get_ctl(xdWindow,edit_nmb+1));
          break;
        case WIN_121_EDIT_22:
          xvt_scr_set_focus_vobj(xvt_win_get_ctl(xdWindow,WIN_121_EDIT_27));
          break;
        case WIN_121_EDIT_24:
          xvt_scr_set_focus_vobj(xvt_win_get_ctl(xdWindow,WIN_121_EDIT_25));
          break;
        case WIN_121_EDIT_25:
          xvt_scr_set_focus_vobj(xvt_win_get_ctl(xdWindow,WIN_121_EDIT_16));
          break;
        case WIN_121_EDIT_27:
          xvt_scr_set_focus_vobj(xvt_win_get_ctl(xdWindow,WIN_121_EDIT_24));
          break;
        default:break;
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
    /*-------------------------------------------------------------------------*/
  case E_CONTROL:
    {
      switch(xdControlId)
      {
      case WIN_121_EDIT_16:
      case WIN_121_EDIT_17:
      case WIN_121_EDIT_18:
      case WIN_121_EDIT_19:
      case WIN_121_EDIT_20:
      case WIN_121_EDIT_21:
      case WIN_121_EDIT_22:
      case WIN_121_EDIT_24:
      case WIN_121_EDIT_25:
      case WIN_121_EDIT_27:
        {  /*	Edit control was operated.		*/
          if (xdEvent->v.ctl.ci.v.edit.focus_change)
          {
            if (xdEvent->v.ctl.ci.v.edit.active)
            {/*   focus has entered the control */
              edit_nmb = xdControlId;
              xvt_ctl_set_text_sel(xvt_win_get_ctl(xdWindow,xdControlId),0,0);
            }
            else
            { /*	focus has left the control 	*/
              char tmp[40],*ptr;
              /* führende blank's löschen  */
              xvt_vobj_get_title(xvt_win_get_ctl(xdWindow,xdControlId),tmp,36);
              ptr = &tmp[0];
              while ((ptr[0]==' ')&&(ptr[0]!='\0'))
                ptr++;
              xvt_vobj_set_title(xvt_win_get_ctl(xdWindow,xdControlId),ptr);
              
              edit_nmb = 0;
            }
          }
          else
          {  /* Contents of control were changed */
            control_were_changed = TRUE;
          }
        }
        break;
        /*-------------------------------------------------------------------------*/
      case WIN_121_PUSHBUTTON_38: /* "OK" */
        {
          // die Blattbezeichnung darf nicht leer sein
          WINDOW blattEdit = xvt_win_get_ctl( WIN121, WIN_121_EDIT_22 );
          if( blattEdit != NULL_WIN && ( !berechnen || !editieren ) )
          {
            char blattStr[256];
            xvt_vobj_get_title( blattEdit, blattStr, sizeof(blattStr) );
            if( strlen( blattStr ) == 0 )
            {
              char message[256]; // "Die Blattbezeichnung darf nicht leer sein"
              xvt_res_get_str( STR_WSPW121_BLATTBEZ, message, sizeof(message) );
              xvt_dm_post_error( "%s", message );
              break;
            } // if blattStr
          } // if blattEdit

          wsp121_save_title(); //--->util.cpp
          SaveProfilFile =TRUE;
          if(!Plot_DB)
          {                   
            save_profildatei(pWPL);//Profildatei wieder sichern Dick 2.12.98
          }
          else
          {
            save_Plot_DB(pWPL);//Plotterdatenbank sichern Dick 10.12.98
          }
          control_were_changed =0;
          xvt_vobj_destroy(xdWindow);
        }
        break;
      case WIN_121_PUSHBUTTON_39: /* "Abbruch" */
        {
          if ((control_were_changed)||(data_changed))
          {
            if (!xvt_dlg_create_res(WD_MODAL, DLG_122, EM_ALL, DLG_122_eh,0L))
              xvt_dm_post_error("Can't open dialog: DLG_122");
            if(DLG_122_WHAT)        //Schlüsseldaten sichern !
            {
              wsp121_save_title();
              SaveProfilFile =TRUE;
              if(!Plot_DB)
              {                          
                save_profildatei(pWPL);//Profildatei wieder sichern Dick 2.12.98 
              }
              else
              {
                save_Plot_DB(pWPL);//Plotterdatenbank sichern Dick 10.12.98
              }
            }
            else
              header_zusammensetzten(); //in util.cpp
            control_were_changed =0;
          }
          else
            header_zusammensetzten(); //in util.cpp
          
          xvt_vobj_destroy(xdWindow);
        }
        break;
      case WIN_121_PUSHBUTTON_40: /* "Steuerdaten" */
        {
          if (control_were_changed)
          {
            wsp121_save_title(); //--->util.cpp
            SaveProfilFile =TRUE;
          }
          else
            header_zusammensetzten(); //in util.cpp
          
          control_were_changed =0;
          dlg_cancel =FALSE;
          
          if (!xvt_dlg_create_res(WD_MODAL, PLOT_100, EM_ALL, PLOT_100_eh,0L))
            xvt_dm_post_error("Can't open dialog: PLOT_100");
          
          if ( !dlg_cancel)
            if (!xvt_dlg_create_res(WD_MODAL, PLOT_101, EM_ALL, PLOT_101_eh,0L))
              xvt_dm_post_error("Can't open dialog: PLOT_100");
            if ( !dlg_cancel)
              if (!xvt_win_create_res(WIN_130, TASK_WIN, EM_ALL, WIN_130_eh, 0L))
                xvt_dm_post_error("Can't open win130");
              
              if ( dlg_cancel)
                if (WIN121 != NULL_WIN)
                  xvt_vobj_set_visible(WIN121,TRUE);
        }
        break;
      default:
        break;
    }
    }
    break;
  }
  xvt_tx_process_event(xdWindow, xdEvent);
  return 0L;
}
