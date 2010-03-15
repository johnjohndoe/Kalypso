/****************************************************************************
*             Dialog 155 : Anzeige KREISSEGMENT                             *
*             10.08.1995                                                    *
****************************************************************************/

#include <windows.h>
#include "xvt.h"

#include "wsphilfe.h"
#include "wspwin.h"
#include "resource.h"
#include "typen.h"

#include "global_vars.h"
#include "..\..\wspdlg\include\export.h"

#include "bce_allg.h"
#include "list.h"
#include "paint.h"

/*
#define DLG_RES_ID DLG_155
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODELESS
*/

// globale Variablen

WINDOW edit_dlg155[7];
BOOLEAN inhalt = TRUE;
char daten155[25];

// externe globale Variablen

extern XVT_HELP_INFO hi;
extern BOOLEAN sichere_datenblock;
extern WINDOW  dlg_sonderprofil;
extern int fehler;


/*************   GHJ   *************/
static WNDPROC defWndProc;
LRESULT CALLBACK Dlg155WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  switch (uMsg)
  {
  case WM_HELP:
    {
      LPHELPINFO lphi = (LPHELPINFO)lParam;
      if (hi!=NULL_HELP_INFO)
      {
        xvt_help_display_topic(hi, HID_KAPITEL_5_3_2_3);
      }
    }
    break;
    
  default:
    break;
  }
  return CallWindowProc(defWndProc, hwnd, uMsg, wParam, lParam);
}
/**************************************/

/*	Handler for dialog DLG_155 ("Kreissegment")*/
long XVT_CALLCONV1
#if XVT_CC_PROTO
DLG_155_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
DLG_155_eh XVT_CALLCONV2 (xdWindow, xdEvent)
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
      SetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC, (LONG)&Dlg155WindowProc);
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
      
      dlg_sonderprofil = xdWindow;
      char str[25];
      
      edit_dlg155[0]=xvt_win_get_ctl(xdWindow,DLG_155_EDIT_7);   // x-links
      edit_dlg155[1]=xvt_win_get_ctl(xdWindow,DLG_155_EDIT_11);  //x-Scheitel
      edit_dlg155[2]=xvt_win_get_ctl(xdWindow,DLG_155_EDIT_8);   // x-rechts
      
      edit_dlg155[3]=xvt_win_get_ctl(xdWindow,DLG_155_EDIT_14);  // y-links   *disabled
      edit_dlg155[4]=xvt_win_get_ctl(xdWindow,DLG_155_EDIT_12);  // y-Scheitel
      edit_dlg155[5]=xvt_win_get_ctl(xdWindow,DLG_155_EDIT_15);  // y-rechts  *disabled
      
      edit_dlg155[6]=xvt_win_get_ctl(xdWindow,DLG_155_EDIT_9);   // Gefälle
      
      for (int i=0;i<=6;i++)
      {
        if (scr.z[i] == BCE_NAN)
          str[0]='\0';
        else  sprintf(str,"%.4lf",scr.z[i]);//gcvt(scr.z[i],12,str);
        xvt_vobj_set_title(edit_dlg155[i],str);
      }
      if (hi!=NULL_HELP_INFO)
        xvt_help_set_win_assoc(hi, xdWindow, HID_KAPITEL_5_3_2_3, 0L);
    }
    break;
  case E_DESTROY:
    {
      dlg_sonderprofil= NULL_WIN;
    }
    break;
  case E_FOCUS:
    {
      if (xdEvent->v.active)  {
      /*
      Dialog has gained focus
        */
      } else {
      /*
      Dialog has lost focus
        */
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
  case E_CONTROL:
    {
      switch(xdControlId) 
      {
      case DLG_155_PUSHBUTTON_16: /* "OK" */
        {
          inhalt = TRUE;
          sichere_datenblock=TRUE;

          xvt_vobj_get_title(edit_dlg155[0],daten155,10);
          fehler = is_zahl(daten155);
          if(fehler==-1)
          {
            scr.z[0]=scr.z[3]=BCE_NAN;
            inhalt=FALSE;
          }
          else
          {
            scr.z[0]= atof(daten155);
            scr.z[3]=list->Get_Station_Hoehe(scr.z[0]);					      
          }
          xvt_vobj_get_title(edit_dlg155[2],daten155,10);
          fehler = is_zahl(daten155);
          if(fehler==-1)
          {
            scr.z[2]=scr.z[5]=BCE_NAN;
            inhalt=FALSE;
          }
          else
          {
            scr.z[2]= atof(daten155);
            scr.z[5]=list->Get_Station_Hoehe(scr.z[2]);					      
          }
          xvt_vobj_get_title(edit_dlg155[1],daten155,10);
          fehler = is_zahl(daten155);
          if(fehler==-1)
          {
            scr.z[1]=BCE_NAN;
            inhalt=FALSE;
          }
          else
            scr.z[1]= atof(daten155);
          xvt_vobj_get_title(edit_dlg155[4],daten155,10);
          fehler = is_zahl(daten155);
          if(fehler==-1)
          {
            scr.z[4]=BCE_NAN;
            inhalt=FALSE;
          }
          else
            scr.z[4]= atof(daten155);
          xvt_vobj_get_title(edit_dlg155[6],daten155,10);
          fehler = is_zahl(daten155);
          if(fehler==-1)
            scr.z[6] = 0.0;
          else
            scr.z[6]= atof(daten155);
          if (inhalt == FALSE)
          {
            char buf[200],buf2[200];
            xvt_res_get_str(STR_ABBRECHEN,buf,sizeof(buf));
            xvt_res_get_str(STR_WSPD155_NOTE_1,buf2,sizeof(buf2));
            switch(xvt_dm_post_ask("OK",buf,NULL,"%s",buf2))
            {
            case RESP_DEFAULT:       //SICHERN
              {
                
                for(int i=0;i<=6;i++)
                  scr.z[i]=BCE_NAN;
                list->SaveSonderprofildaten(&scr, KREISSEGM);
                if (WIN_117 != NULL_WIN)
                {
                  xvt_dwin_invalidate_rect(WIN_117,0);
                  paint->draw_kreis(WIN_117,KREISSEGM);
                }
                xvt_vobj_destroy(xdWindow);
              }
              break;
            case RESP_2:             // nicht sichere_datenblock                                 
              break;             //zurück
            case RESP_3:break;
            }
          }
          else
          {
            SaveProfilFile = TRUE;
            list->SaveSonderprofildaten(&scr, KREISSEGM);
            if (WIN_117 != NULL_WIN)
            {
              xvt_dwin_invalidate_rect(WIN_117,0);
              paint->draw_kreis(WIN_117,KREISSEGM);
            }
            xvt_vobj_destroy(xdWindow);
          }
      }
      break;
    case DLG_155_PUSHBUTTON_17: /* "Abbruch" */
      {
        inhalt = TRUE;
        char buf[200],buf2[200],buf3[200],buf4[200];
        xvt_res_get_str(STR_JA,buf,sizeof(buf));
        xvt_res_get_str(STR_NEIN,buf2,sizeof(buf2));
        xvt_res_get_str(STR_ABBRECHEN,buf3,sizeof(buf3));
        xvt_res_get_str(STR_SICHERE_DATENBLOCK,buf4,sizeof(buf4));
        
        if (sichere_datenblock)
        {
          switch (xvt_dm_post_ask(buf,buf2,buf3,"%s",buf4))
          {
          case RESP_DEFAULT:       //SICHERN
            {
              xvt_vobj_get_title(edit_dlg155[0],daten155,10);
              fehler = is_zahl(daten155);
              if(fehler==-1)
              {
                scr.z[0]=scr.z[3]=BCE_NAN;
                inhalt=FALSE;
              }
              else
              {
                scr.z[0]= atof(daten155);
                scr.z[3]=list->Get_Station_Hoehe(scr.z[0]);					      
              }
              xvt_vobj_get_title(edit_dlg155[2],daten155,10);
              fehler = is_zahl(daten155);
              if(fehler==-1)
              {
                scr.z[2]=scr.z[5]=BCE_NAN;
                inhalt=FALSE;
              }
              else
              {
                scr.z[2]= atof(daten155);
                scr.z[5]=list->Get_Station_Hoehe(scr.z[2]);					      
              }
              xvt_vobj_get_title(edit_dlg155[1],daten155,10);
              fehler = is_zahl(daten155);
              if(fehler==-1)
              {
                scr.z[1]=BCE_NAN;
                inhalt=FALSE;
              }
              else
                scr.z[1]= atof(daten155);
              xvt_vobj_get_title(edit_dlg155[4],daten155,10);
              fehler = is_zahl(daten155);
              if(fehler==-1)
              {
                scr.z[4]=BCE_NAN;
                inhalt=FALSE;
              }
              else
                scr.z[4]= atof(daten155);
              xvt_vobj_get_title(edit_dlg155[6],daten155,10);
              fehler = is_zahl(daten155);
              if(fehler==-1)
                scr.z[6] = 0.0;
              else
                scr.z[6]= atof(daten155);
              if (inhalt == FALSE)
              {
                
                xvt_res_get_str(STR_ABBRECHEN,buf,sizeof(buf));
                xvt_res_get_str(STR_WSPD155_NOTE_1,buf2,sizeof(buf2));
                switch(xvt_dm_post_ask("OK",buf,NULL,"%s",buf2))                         
                {
                case RESP_DEFAULT:       //SICHERN
                  {
                    
                    for(int i=0;i<=6;i++)
                      scr.z[i]=BCE_NAN;
                    list->SaveSonderprofildaten(&scr, KREISSEGM);
                    if (WIN_117 != NULL_WIN)
                    {
                      xvt_dwin_invalidate_rect(WIN_117,0);
                      paint->draw_kreis(WIN_117,KREISSEGM);
                    }
                    xvt_vobj_destroy(xdWindow);
                  }
                  break;
                case RESP_2:             // nicht sichere_datenblock                                 
                  break;             //zurück
                case RESP_3:break;
                }
              }
              else
              {
                list->SaveSonderprofildaten(&scr, KREISSEGM);
                if (WIN_117 != NULL_WIN)
                {
                  xvt_dwin_invalidate_rect(WIN_117,0);
                  paint->draw_kreis(WIN_117,KREISSEGM);
                }
                xvt_vobj_destroy(xdWindow);
              }
            }
            break;
          case RESP_2:             // nicht sichere_datenblock
            xvt_vobj_destroy(xdWindow);
            break;             //zurück
          case RESP_3:break;
          }
          //}
        }
        else
          xvt_vobj_destroy(xdWindow);
      }
      break;
    case DLG_155_EDIT_7:			{
    /*
				Edit control was operated.
      */
      if (xdEvent->v.ctl.ci.v.edit.focus_change) {
        if (xdEvent->v.ctl.ci.v.edit.active)
        {
          /*	focus has entered the control	*/
          xvt_vobj_get_title(edit_dlg155[0],daten155,10);
        }
        else
        {/*	focus has left the control		*/
          fehler = is_zahl(daten155);
          if (fehler ==-1) inhalt =FALSE;
          if (fehler ==0)
          {
            char buf[200];//Dick 26.11.99
            xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
            xvt_dm_post_error("%s",buf);
            //xvt_dm_post_error("Fehler in der Eingabe !");
            xvt_vobj_set_title(edit_dlg155[0],"\0");
          }
          else
          {
            xvt_vobj_set_title(edit_dlg155[0],daten155);
            scr.z[0]= atof(daten155);
            
            scr.z[3]=list->Get_Station_Hoehe(scr.z[0]);
            sprintf(daten155,"%lf",scr.z[3]);//gcvt(scr.z[3],12,daten155);
            xvt_vobj_set_title(edit_dlg155[3],daten155);
          }
        }
      } else {
      /*
      Contents of control were changed
        */
        xvt_vobj_get_title(edit_dlg155[0],daten155,10);
        sichere_datenblock = TRUE;
      }
                              }
      break;
    case DLG_155_EDIT_8:			{
    /*
				Edit control was operated.
      */
      if (xdEvent->v.ctl.ci.v.edit.focus_change) {
        if (xdEvent->v.ctl.ci.v.edit.active) {
        /*
        focus has entered the control
          */
          xvt_vobj_get_title(edit_dlg155[2],daten155,10);
        } else {
        /*
        focus has left the control
          */
          fehler = is_zahl(daten155);
          if (fehler ==-1) inhalt =FALSE;
          if (fehler ==0)
          {
            char buf[200];//Dick 26.11.99
            xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
            xvt_dm_post_error("%s",buf);
            //xvt_dm_post_error("Fehler in der Eingabe !");
            xvt_vobj_set_title(edit_dlg155[2],"\0");
          }
          else
          {
            xvt_vobj_set_title(edit_dlg155[2],daten155);
            scr.z[2]= atof(daten155);
            
            scr.z[5]=list->Get_Station_Hoehe(scr.z[2]) ;
            
            sprintf(daten155,"%lf",scr.z[5]);//gcvt(scr.z[5],12,daten155);
            xvt_vobj_set_title(edit_dlg155[5],daten155);
          }
        }
      } else {
      /*
      Contents of control were changed
        */
        xvt_vobj_get_title(edit_dlg155[2],daten155,10);
        sichere_datenblock = TRUE;
      }
                              }
      break;
    case DLG_155_EDIT_9:			{
      /*		Edit control: "GEFÄLLE" was operated.			*/
      if (xdEvent->v.ctl.ci.v.edit.focus_change) {
        if (xdEvent->v.ctl.ci.v.edit.active) {
        /*
        focus has entered the control
          */
          xvt_vobj_get_title(edit_dlg155[6],daten155,10);
        } else {
        /*
        focus has left the control
          */
        }
        fehler = is_zahl(daten155);
        if (fehler ==-1) inhalt =FALSE;
        if (fehler ==0)
        {
          char buf[200];//Dick 26.11.99
          xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
          xvt_dm_post_error("%s",buf);
          //xvt_dm_post_error("Fehler in der Eingabe !");
          xvt_vobj_set_title(edit_dlg155[6],"\0");
        }
        else
        {
          scr.z[6]= atof(daten155);
          xvt_vobj_set_title(edit_dlg155[6],daten155);
        }
      } else {
      /*
      Contents of control were changed
        */
        xvt_vobj_get_title(edit_dlg155[6],daten155,10);
        sichere_datenblock = TRUE;
      }
                              }
      break;
    case DLG_155_EDIT_11:			{
    /*
				Edit control was operated.
      */
      if (xdEvent->v.ctl.ci.v.edit.focus_change) {
        if (xdEvent->v.ctl.ci.v.edit.active) {
        /*
        focus has entered the control
          */
          xvt_vobj_get_title(edit_dlg155[1],daten155,10);
        } else {
        /*
        focus has left the control
          */
          fehler = is_zahl(daten155);
          if (fehler ==-1) inhalt =FALSE;
          if (fehler ==0)
          {
            char buf[200];//Dick 26.11.99
            xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
            xvt_dm_post_error("%s",buf);
            //xvt_dm_post_error("Fehler in der Eingabe !");
            xvt_vobj_set_title(edit_dlg155[1],"\0");
          }
          else
          {
            scr.z[1]= atof(daten155);
            xvt_vobj_set_title(edit_dlg155[1],daten155);
          }
        }
      } else {
      /*
      Contents of control were changed
        */
        xvt_vobj_get_title(edit_dlg155[1],daten155,10);
        sichere_datenblock = TRUE;
      }
                              }
      break;
    case DLG_155_EDIT_12:			{
    /*
				Edit control was operated.
      */
      if (xdEvent->v.ctl.ci.v.edit.focus_change) {
        if (xdEvent->v.ctl.ci.v.edit.active) {
        /*
        focus has entered the control
          */
          xvt_vobj_get_title(edit_dlg155[4],daten155,10);
        } else {
        /*
        focus has left the control
          */
          fehler = is_zahl(daten155);
          if (fehler ==-1) inhalt =FALSE;
          if (fehler ==0)
          {
            char buf[200];//Dick 26.11.99
            xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
            xvt_dm_post_error("%s",buf);
            //xvt_dm_post_error("Fehler in der Eingabe !");
            xvt_vobj_set_title(edit_dlg155[4],"\0");
          }
          else
          {
            scr.z[4]= atof(daten155);
            xvt_vobj_set_title(edit_dlg155[4],daten155);
          }
        }
      } else {
      /*
      Contents of control were changed
        */
        xvt_vobj_get_title(edit_dlg155[4],daten155,10);
        sichere_datenblock = TRUE;
      }
                              }
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
