
/* 	Dialog DLG_212 ("Brückenparameter Zeile2:Infoblock ") */

#include <windows.h>
#include <math.h>
#include "xvt.h"
#include "wspwin.h"
#include "resource.h"

#include "global_types.h"
#include "global_vars.h"
#include "..\..\wspdlg\include\export.h"

#include "list.h"
#include "bce_allg.h"

#include "strang.h"
#include "typen.h"

#include "global.h"

#include "wsphilfe.h"

extern XVT_HELP_INFO hi;

#define DLG_RES_ID DLG_212
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODELESS

extern WINDOW dlg_sonderprofil;
extern BOOLEAN sichere_datenblock;
extern BOOLEAN SaveProfilFile;

WINDOW edit_212[4];
WINDOW check_rehbock;
BRUECKE bruecke;

/*************   GHJ   *************/
static WNDPROC defWndProc;
LRESULT CALLBACK Dlg212WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  switch (uMsg)
  {
  case WM_HELP:
    {
      LPHELPINFO lphi = (LPHELPINFO)lParam;
      if (hi!=NULL_HELP_INFO)
      {
        xvt_help_display_topic(hi, HID_KAPITEL_5_1_1_1);
      }
    }
    break;
    
  default:
    break;
  }
  return CallWindowProc(defWndProc, hwnd, uMsg, wParam, lParam);
}
/**************************************/

/* 	Dialog DLG_212 ("Brückenparameter Zeile2:Infoblock ") */
long XVT_CALLCONV1
#if XVT_CC_PROTO
DLG_212_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
DLG_212_eh XVT_CALLCONV2 (xdWindow, xdEvent)
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
      SetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC, (LONG)&Dlg212WindowProc);
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
      sichere_datenblock = FALSE;
      dlg_sonderprofil = xdWindow;
      for (int i=0;i<=4;i++)
        edit_212[i]=xvt_win_get_ctl(xdWindow,DLG_212_EDIT_0+i);
      
      check_rehbock=xvt_win_get_ctl(xdWindow,DLG_212_CHECKBOX_12);
      bruecke.rehbock=0;
      if( !LWA_PROJEKT )
        xvt_vobj_set_visible(check_rehbock,FALSE);
      else
        xvt_ctl_set_checked(check_rehbock,TRUE);
      
      bruecke.sohle[0]='\0';   //init
      bruecke.breite[0]='\0';
      bruecke.rauheit[0]='\0';
      bruecke.beiwert[0]='\0';
      
      list->GetInfoline2(scr.datensatz,&bruecke);  //Zeile2 Infoblock holen

      if( LWA_PROJEKT )
      {
        xvt_ctl_set_checked(check_rehbock,bruecke.rehbock);
        xvt_vobj_set_enabled( edit_212[1], FALSE );
        xvt_vobj_set_enabled( edit_212[2], FALSE );
        if( strang_anfang )
        {
          STRANG* local_strang = strang_anfang;
          STRANG* local_vor_strang = NULL; //strang_anfang;
          
          while( local_strang && strcmp( file_spec.name, local_strang->name_anfang ) )
          {
            local_vor_strang = local_strang; 
            local_strang = local_strang->next;
          }
          
          if( local_strang )
          {
            double station = local_strang->anfang;
            double vorher = 0.0, nachher = 0.0;
            if( local_vor_strang && local_vor_strang->anfang != BCE_NAN )
              vorher = local_vor_strang->anfang;
            else
              vorher = station - 0.005; // 5 meter
            
            if( local_strang->next && local_strang->next->anfang != BCE_NAN )
              nachher = local_strang->next->anfang;
            else
              nachher = station + 0.005; // 5 Meter
            
            sprintf( bruecke.breite, "%.4lf", ( fabs( local_strang->anfang - vorher ) + 
              fabs( nachher - local_strang->anfang ) ) * 500.0 );
          }
        }
        Koord* dat_rauh = list->HoleDatensatz( RAUHIGKEIT );
        Koord* dat_trenn = list->HoleDatensatz( TRENNFLAECHEN );
        if( dat_rauh == NULL )            
          dat_rauh = list->HoleDatensatz( RAUHIGKEIT_KST );
        if( dat_trenn )
        {
          while( dat_rauh != NULL && dat_rauh->x <= dat_trenn->x )
            dat_rauh = dat_rauh->next_ds;
        }
        if( dat_rauh )
          sprintf( bruecke.rauheit, "%.4lf", dat_rauh->y );
        else
          sprintf( bruecke.rauheit, "%.4lf", 0.0 );
      };

      if (strlen(bruecke.sohle)< 1)   //default-Werte
        strcpy(bruecke.sohle,"0.0");
      if (strlen(bruecke.breite)< 1)
        strcpy(bruecke.breite,"0.0");
      if (strlen(bruecke.rauheit)< 1)
        strcpy(bruecke.rauheit,"0.0");
      if (strlen(bruecke.beiwert)< 1)
        strcpy(bruecke.beiwert,"0.0");
      
      list->SaveInfoline2(scr.datensatz,&bruecke);  //Zeile2 Infoblock sichern //Dick 30.09.99 default sichern
      xvt_vobj_set_title(edit_212[0],bruecke.sohle);
      xvt_vobj_set_title(edit_212[1],bruecke.breite);
      xvt_vobj_set_title(edit_212[2],bruecke.rauheit);
      xvt_vobj_set_title(edit_212[3],bruecke.beiwert);
      if (hi!=NULL_HELP_INFO)
        xvt_help_set_win_assoc(hi, xdWindow, HID_KAPITEL_5_1_1_1, 0L);
    }
    break;
  case E_DESTROY:
    {
      dlg_sonderprofil = NULL_WIN;
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
  case E_SIZE:
    {
    }
    break;
  case E_CLOSE:
    {
      dlg_sonderprofil = NULL_WIN;
      xvt_vobj_destroy(xdWindow);
    }
    break;
  case E_CHAR:
		/*
    Character typed.
    */
    {
    }
    break;
  case E_CONTROL:
    {
      switch(xdControlId) {
      case DLG_212_EDIT_0:  //Höhe Sohle im Unterwasser
        {
          /*				Edit control was operated.			*/
          if (xdEvent->v.ctl.ci.v.edit.focus_change) {
            if (xdEvent->v.ctl.ci.v.edit.active) {
              /*		focus has entered the control		*/
            } else
            {
              /*		focus has left the control			*/
              xvt_vobj_get_title(edit_212[0],bruecke.sohle,9);
              
            }
          } else {
            /*     Contents of control were changed	*/
            sichere_datenblock = TRUE;
          }
        }
        break;
      case DLG_212_EDIT_1:			{
      /*
      Edit control was operated.
        */
        if (xdEvent->v.ctl.ci.v.edit.focus_change) {
          if (xdEvent->v.ctl.ci.v.edit.active) {
          /*
          focus has entered the control
            */
          } else {
          /*
          focus has left the control
            */
            xvt_vobj_get_title(edit_212[1],bruecke.breite,9);
            
          }
        } else {
        /*
        Contents of control were changed
          */
          sichere_datenblock = TRUE;
        }
                                }
        break;
      case DLG_212_EDIT_2:			{
      /*
      Edit control was operated.
        */
        if (xdEvent->v.ctl.ci.v.edit.focus_change) {
          if (xdEvent->v.ctl.ci.v.edit.active) {
          /*
          focus has entered the control
            */
          } else {
          /*
          focus has left the control
            */
            xvt_vobj_get_title(edit_212[2],bruecke.rauheit,9);
            
          }
        } else {
        /*
        Contents of control were changed
          */
          sichere_datenblock = TRUE;
        }
                                }
        break;
      case DLG_212_EDIT_3:			{
      /*
      Edit control was operated.
        */
        if (xdEvent->v.ctl.ci.v.edit.focus_change) {
          if (xdEvent->v.ctl.ci.v.edit.active) {
          /*
          focus has entered the control
            */
          } else {
          /*
          focus has left the control
            */
            xvt_vobj_get_title(edit_212[3],bruecke.beiwert,9);
            
          }
        } else {
        /*
        Contents of control were changed
          */
          sichere_datenblock = TRUE;
        }
                                }
        break;
      case DLG_212_CHECKBOX_12: /* "nach Rehbock (sonst nach Yarnell)" */
        {

          if( LWA_PROJEKT )
          {
            xvt_ctl_set_checked(check_rehbock, !xvt_ctl_is_checked (check_rehbock));
            bruecke.rehbock=xvt_ctl_is_checked (check_rehbock);
          };
        }
        break;
        
      case DLG_212_PUSHBUTTON_10: /* "OK" */
        {
          int len[4],fehler=0;
          double bw;
          
          len[0]=is_zahl(bruecke.sohle);
          len[1]=is_zahl(bruecke.breite);
          len[2]=is_zahl(bruecke.rauheit);
          len[3]=is_zahl(bruecke.beiwert);
          
          bw=atof(bruecke.beiwert); /*wird beiwert =0 eingegeben soll in jedem Fall*/
          if( LWA_PROJEKT )
          {
            if (bw ==0.0)             /*als 5.Zahl Rehbock =0 gespeichert werden 31.01.96*/
              bruecke.rehbock = TRUE;
          };

          for(int i=0;i<=3;i++)
            if (len[i]<=0)
            {
              char buf[200],buf2[200];//Dick 26.11.99
              xvt_res_get_str(STR_WERT_IN_FELD,buf,sizeof(buf));
              xvt_res_get_str(STR_EINGEBEN,buf2,sizeof(buf2));
              xvt_dm_post_note("%s %i %s",buf,i,buf2);
              //xvt_dm_post_note("Bitte Wert in Feld %i eingeben",i);
              xvt_vobj_set_title(edit_212[i],"\0");
              fehler=1;
            }
            if (!fehler)
            {
              list->SaveInfoline2(scr.datensatz,&bruecke);  //Zeile2 Infoblock sichern
              SaveProfilFile = TRUE;
              xvt_vobj_destroy(xdWindow);
            }
        }
        break;
      case DLG_212_PUSHBUTTON_11: /* "Abbruch" */
        {
          char buf[200],buf2[200],buf3[200],buf4[200];
          xvt_res_get_str(STR_JA,buf,sizeof(buf));
          xvt_res_get_str(STR_NEIN,buf2,sizeof(buf2));
          xvt_res_get_str(STR_ABBRECHEN,buf3,sizeof(buf3));
          xvt_res_get_str(STR_SICHERE_DATENBLOCK,buf4,sizeof(buf4));
          if (sichere_datenblock)
            switch (xvt_dm_post_ask(buf,buf2,buf3,"%s",buf4))
            //switch (xvt_dm_post_ask("Ja","Nein","Abbrechen","Daten wurden geändert !\nSpeichern vor Schließen\ndes Fensters ?"))
          {
				  case RESP_DEFAULT:       //SICHERN
            {
              int len[4],fehler=0;
              
              len[0]=is_zahl(bruecke.sohle);
              len[1]=is_zahl(bruecke.breite);
              len[2]=is_zahl(bruecke.rauheit);
              len[3]=is_zahl(bruecke.beiwert);
              
              double bw=atof(bruecke.beiwert); /*wird beiwert =0 eingegeben soll in jedem Fall*/
              if( LWA_PROJEKT )
              {
                if (bw ==0.0)             /*als 5.Zahl Rehbock =0 gespeichert werden 31.01.96*/
                  bruecke.rehbock = TRUE;
              };

              for(int i=0;i<=3;i++)
                if (len[i]<=0)
                {
                  //Dick 26.11.99
                  xvt_res_get_str(STR_WERT_IN_FELD,buf,sizeof(buf));
                  xvt_res_get_str(STR_EINGEBEN,buf2,sizeof(buf2));
                  xvt_dm_post_note("%s %i %s",buf,i,buf2);
                  //xvt_dm_post_note("Bitte Wert in Feld %i eingeben",i);
                  xvt_vobj_set_title(edit_212[i],"\0");
                  fehler=1;
                }
                if (!fehler)
                {
                  list->SaveInfoline2(scr.datensatz,&bruecke);  //Zeile2 Infoblock sichern
                  SaveProfilFile = TRUE;
                  xvt_vobj_destroy(xdWindow);
                }
            }
            break;
          case RESP_2:             // nicht sichere_datenblock
            xvt_vobj_destroy(xdWindow);
            break;             //zurück
          case RESP_3:break;
          }
          xvt_vobj_destroy(xdWindow);
        }
        break;
      default:
        break;
    }
    }
    break;
  case E_TIMER:
		/*
    Timer associated with window went off.
    */
    {
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
