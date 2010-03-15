/////////////////
// wspd213.cpp //
/////////////////

/*    nur  BCE - VERSION   !!!!!!   für LWA: DLG_160 benutzen*/

// xvt-Design
#define DLG_RES_ID DLG_213
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODELESS


#include <windows.h>
#include "xvt.h"

#include "resource.h"

#include "wsphilfe.h"
#include "global_vars.h"
#include "..\..\wspdlg\include\export.h"

#include "wspwin.h"
#include "list.h"
#include "bce_allg.h"

// globale Variablen
extern WINDOW dlg_sonderprofil;
extern WEHR wehrdaten; // in wspd160.cpp

// lokale globale variablen
SLIST wehre_slist;
WINDOW listbutton213;

/*************   GHJ   *************/
static WNDPROC defWndProc;
LRESULT CALLBACK Dlg213WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  switch (uMsg)
  {
  case WM_HELP:
    {
      LPHELPINFO lphi = (LPHELPINFO)lParam;
      if (hi!=NULL_HELP_INFO)
      {
        xvt_help_display_topic(hi, HID_KAPITEL_5_2_2);
      }
    }
    break;
    
  default:
    break;
  }
  return CallWindowProc(defWndProc, hwnd, uMsg, wParam, lParam);
}
/**************************************/

/* 	Handler for dialog DLG_213 ("Wehrparameter")  */
long XVT_CALLCONV1
#if XVT_CC_PROTO
DLG_213_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
DLG_213_eh XVT_CALLCONV2 (xdWindow, xdEvent)
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
      if (WIN_116!=NULL_WIN)
        SetParent((HWND)xvt_vobj_get_attr(xdWindow, ATTR_NATIVE_WINDOW), (HWND)xvt_vobj_get_attr(WIN_116, ATTR_NATIVE_WINDOW));
      else if (WIN120!=NULL_WIN)
        SetParent((HWND)xvt_vobj_get_attr(xdWindow, ATTR_NATIVE_WINDOW), (HWND)xvt_vobj_get_attr(WIN120, ATTR_NATIVE_WINDOW));
      defWndProc = (WNDPROC)GetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC);
      SetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC, (LONG)&Dlg213WindowProc);
      ChangeFontAndSize((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW));	// GHJ
      RECT rect;
      POINT pt1, pt2;
      if (WIN_116!=NULL_WIN)
      {
        ::GetWindowRect(::GetDlgItem((HWND)xvt_vobj_get_attr(WIN_116,ATTR_NATIVE_WINDOW), WIN_GRAPH_116_LISTBUTTON_33), &rect);
        pt1.x = rect.left;
        pt1.y = rect.top;
        ::ScreenToClient((HWND)xvt_vobj_get_attr(WIN_116,ATTR_NATIVE_WINDOW), &pt1);
        pt1.y += 3*(rect.bottom-rect.top);
        pt1.x -= 10;
        ::GetWindowRect((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), &rect);
        pt2.x = pt1.x + rect.right-rect.left;
        pt2.y = pt1.y + rect.bottom-rect.top;
        ::MoveWindow((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), pt1.x, pt1.y, pt2.x-pt1.x, pt2.y-pt1.y, TRUE);
      }
      else if (WIN120!=NULL_WIN)
      {
        ::GetWindowRect(::GetDlgItem((HWND)xvt_vobj_get_attr(WIN120,ATTR_NATIVE_WINDOW), WIN_120_LISTBUTTON_33), &rect);
        pt1.x = rect.left;
        pt1.y = rect.top;
        ::ScreenToClient((HWND)xvt_vobj_get_attr(WIN120,ATTR_NATIVE_WINDOW), &pt1);
        pt1.y += 3*(rect.bottom-rect.top);
        pt1.x -= 10;
        ::GetWindowRect((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), &rect);
        pt2.x = pt1.x + rect.right-rect.left;
        pt2.y = pt1.y + rect.bottom-rect.top;
        ::MoveWindow((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), pt1.x, pt1.y, pt2.x-pt1.x, pt2.y-pt1.y, TRUE);
      }
      xvt_vobj_set_visible(xdWindow, TRUE);
      /***********************************/
      int gefunden=0;
      dlg_sonderprofil = xdWindow;
      
      wehrdaten.wehrtyp[0] = '\0';
      wehrdaten.bwert[0] = '\0';
      
      wehre_slist = xvt_slist_create();
      xvt_slist_add_at_elt(wehre_slist,NULL,"rundkronig",0L);
      xvt_slist_add_at_elt(wehre_slist,NULL,"breitkronig",0L);
      xvt_slist_add_at_elt(wehre_slist,NULL,"Beiwert",0L);
      xvt_slist_add_at_elt(wehre_slist,NULL,"scharfkantig",0L);
      
      listbutton213=xvt_win_get_ctl(xdWindow,DLG_213_LISTBUTTON_1);
      xvt_list_add(listbutton213,-1,(char*)wehre_slist);
      
      int index = list->GetInfoline2(scr.datensatz,&wehrdaten); //Zeile2 Infoblock holen
      
      xvt_vobj_set_title(xvt_win_get_ctl(xdWindow,DLG_213_EDIT_4),wehrdaten.bwert);
      
      xvt_list_set_sel( listbutton213, index ,TRUE );

      list->SaveInfoline2( scr.datensatz, &wehrdaten );   // und den default sichern
    }
    break;
  case E_DESTROY:
    {
      dlg_sonderprofil = NULL_WIN;
      xvt_slist_destroy(wehre_slist);
    }
    break;
  case E_CLOSE:
    {
      dlg_sonderprofil = NULL_WIN;
      xvt_vobj_destroy(xdWindow);
    }
    break;
  case E_CONTROL:
		/*
    User operated control in dialog.
    */
    {
      switch(xdControlId) 
      {
      case DLG_213_LISTBUTTON_1: /* "List Button 1" */
        {
          xvt_vobj_get_title(listbutton213,wehrdaten.wehrtyp,20);
          if(wehrdaten.wehrtyp[0]=='s')
          {
            wehrdaten.bwert[0]='\0';
            xvt_vobj_set_title(xvt_win_get_ctl(xdWindow,DLG_213_EDIT_4),"\0");
          }
        }
        break;
      case DLG_213_EDIT_4:
        {
          /* Edit control was operated.*/
          if (xdEvent->v.ctl.ci.v.edit.focus_change) 
          {
            if (xdEvent->v.ctl.ci.v.edit.active) 
            {
              /*			focus has entered the control			*/
              if((wehrdaten.wehrtyp[0]=='s')&& (wehrdaten.bwert[0]!='\0'))
              {
                char buf[200];//Dick 26.11.99
                xvt_res_get_str(STR_WSPD213_NOTE_1,buf,sizeof(buf));
                xvt_dm_post_note("%s",buf); //"Bei scharfkantigen Wehren kein Beiwert"
                wehrdaten.bwert[0]='\0';
                xvt_vobj_set_title(xvt_win_get_ctl(xdWindow,DLG_213_EDIT_4),"\0");
              }
            }
            else
              xvt_vobj_get_title(xvt_win_get_ctl(xdWindow,DLG_213_EDIT_4), wehrdaten.bwert,130);
          }
          else
          {	/*	Contents of control were changed	*/
          }
        }
        break;
      case DLG_213_PUSHBUTTON_11: /* "OK" */
        {
          xvt_vobj_get_title(listbutton213,wehrdaten.wehrtyp,20);
          if(wehrdaten.wehrtyp[0] != 's')
          {
            if ((strlen(wehrdaten.bwert))==0)
            {
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_WSPD213_NOTE_2,buf,sizeof(buf));
              xvt_dm_post_note("%s",buf);//"Bitte Wehrparameter eingeben !";
            }
            else
            {
              is_zahl(&wehrdaten.bwert[0]);
              list->SaveInfoline2(scr.datensatz,&wehrdaten);   //Zeile2 Infoblock sichern
              xvt_vobj_destroy(xdWindow);
            }
          }
          else
          {
            list->SaveInfoline2(scr.datensatz,&wehrdaten);   //Zeile2 Infoblock sichern
            xvt_vobj_destroy(xdWindow);
          }
        }
        break;
      case DLG_213_PUSHBUTTON_12: /* "Abbruch" */
        xvt_vobj_destroy(xdWindow);
        break;
      default:
        break;
      }
    }
    break;
  case E_USER:
		/*
    Application initiated.
    */
    {
      switch (xdEvent->v.user.id) 
      {
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
