/****************************************************************************
*             Dialog 140 : Anzeige Durchst.Bereiche                         *
*             14.12.1994                                                    *
****************************************************************************/

#include <windows.h>
#include "xvt.h"

#include "wspwin.h"
#include "resource.h"
#include "wsphilfe.h"
#include "typen.h"

#include "global_types.h"
#include "global_vars.h"

#include "..\..\wspdlg\include\export.h"

#include "bce_allg.h"

#include "list.h"
#include "paint.h"


// Globale Variablen

extern BOOLEAN sichere_datenblock;

extern WINDOW dlg_sonderprofil;

extern WINDOW* ptr_win;

extern MinMax pmm;

// 'Lokale' Variablen

WINDOW edit_dlg140[4];
char daten[4][20];
int fehler140;


/*************   GHJ   *************/
static WNDPROC defWndProc;
extern XVT_HELP_INFO hi;

/*
#define DLG_RES_ID DLG_140
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODELESS
*/


LRESULT CALLBACK Dlg140WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  switch (uMsg)
  {
  case WM_HELP:
    {
      LPHELPINFO lphi = (LPHELPINFO)lParam;
      if (hi!=NULL_HELP_INFO)
      {
        xvt_help_display_topic(hi, HID_KAPITEL_4_4_3_4);
      }
    }
    break;
    
  default:
    break;
  }
  return CallWindowProc(defWndProc, hwnd, uMsg, wParam, lParam);
}
/**************************************/


/*	Handler for dialog DLG_140 ("durchströmte Bereiche") */
long XVT_CALLCONV1 DLG_140_eh XVT_CALLCONV2( WINDOW xdWindow, EVENT *xdEvent )
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
      SetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC, (LONG)&Dlg140WindowProc);
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
      
      char str[25];
      sichere_datenblock=FALSE;
      dlg_sonderprofil  = xdWindow;
      edit_dlg140[0]=xvt_win_get_ctl(xdWindow,DLG_140_EDIT_9);
      edit_dlg140[1]=xvt_win_get_ctl(xdWindow,DLG_140_EDIT_10);
      edit_dlg140[2]=xvt_win_get_ctl(xdWindow,DLG_140_EDIT_12);
      edit_dlg140[3]=xvt_win_get_ctl(xdWindow,DLG_140_EDIT_13);
      
      ptr_win = &edit_dlg140[0];
      
      if (scr.z[0] == BCE_NAN)
      {
        sprintf(str,"%.4lf",pmm.minX);
        scr.z[0]=pmm.minX;
        sichere_datenblock=TRUE;
      }
      else sprintf(str,"%.4lf",scr.z[0]);//gcvt(scr.z[0],6,str);
      xvt_vobj_set_title(edit_dlg140[0],str);
      strcpy(daten[0],str);
      
      if (scr.z[1] == BCE_NAN)
      {
        scr.z[1]=list->Get_Station_Hoehe(pmm.minX);
        sichere_datenblock=TRUE;
        if(scr.z[1]!=BCE_NAN)
          sprintf(str,"%.4lf",list->Get_Station_Hoehe(pmm.minX));//gcvt(list->Get_Station_Hoehe(pmm.minX),6,str);
        else
        {
          scr.z[1]=0;
          str[0]='0';
          str[1]='\0';
        }
      }
      else  sprintf(str,"%.4lf",scr.z[1]);//gcvt(scr.z[1],6,str);
      xvt_vobj_set_title(edit_dlg140[1],str);
      strcpy(daten[1],str);
      
      if (scr.z[2] == BCE_NAN)
				  {
        sprintf(str,"%.4lf",pmm.maxX);//gcvt(pmm.maxX,6,str);
        scr.z[2]=pmm.maxX;
        sichere_datenblock=TRUE;
				  }
      else  sprintf(str,"%.4lf",scr.z[2]);//gcvt(scr.z[2],6,str);
      xvt_vobj_set_title(edit_dlg140[2],str);
      strcpy(daten[2],str);
      
      if (scr.z[3] == BCE_NAN)
      {
        scr.z[3]=list->Get_Station_Hoehe(pmm.maxX);
        sichere_datenblock=TRUE;
        if(scr.z[3]!=BCE_NAN)
          sprintf(str,"%.4lf",list->Get_Station_Hoehe(pmm.maxX));//gcvt(list->Get_Station_Hoehe(pmm.maxX),6,str);
        else
        {
          scr.z[3]=0;
          str[0]='0';
          str[1]='\0';
        }
        
      }
      else  sprintf(str,"%.4lf",scr.z[3]);//gcvt(scr.z[3],6,str);
      xvt_vobj_set_title(edit_dlg140[3],str);
      strcpy(daten[3],str);
      
      
      if (sichere_datenblock)
      {
        list->SaveSonderprofildaten(&scr, DURCHST_BEREICH);
        sichere_datenblock=FALSE;
      }
      
      if (hi!=NULL_HELP_INFO)
        xvt_help_set_win_assoc(hi, xdWindow, HID_KAPITEL_4_4_3_4, 0L);
    }
    break;
  case E_DESTROY:
    {
      dlg_sonderprofil= NULL_WIN;
      *ptr_win = NULL_WIN;
    }
    break;
  case E_CLOSE:
    {
      char buf[200],buf2[200],buf3[200],buf4[200];
      xvt_res_get_str(STR_JA,buf,sizeof(buf));
      xvt_res_get_str(STR_NEIN,buf2,sizeof(buf2));
      xvt_res_get_str(STR_ABBRECHEN,buf3,sizeof(buf3));
      xvt_res_get_str(STR_SICHERE_DATENBLOCK,buf4,sizeof(buf4));
      
				 	
      if (sichere_datenblock)
      {
        switch( xvt_dm_post_ask(buf,buf2,buf3,"%s",buf4 ) ) //switch (xvt_dm_post_ask("Ja","Nein","Abbrechen","Daten wurden geändert !\nSpeichern vor Schließen\ndes Fensters ?"))
        {
        case RESP_DEFAULT:       //SICHERN
          {
            xvt_vobj_get_title(edit_dlg140[0],daten[0],19);
            xvt_vobj_get_title(edit_dlg140[1],daten[1],19);
            xvt_vobj_get_title(edit_dlg140[2],daten[2],19);
            xvt_vobj_get_title(edit_dlg140[3],daten[3],19);
            is_zahl(daten[0]);
            is_zahl(daten[1]);
            is_zahl(daten[2]);
            is_zahl(daten[3]);
            scr.z[0]= atof(daten[0]);
            scr.z[1]= atof(daten[1]);
            scr.z[2]= atof(daten[2]);
            scr.z[3]= atof(daten[3]);
            
            if (!list->ExistStation(scr.z[0],1))
            {
              char buf[200],buf2[200];//Dick 26.11.99
              xvt_res_get_str(STR_LINKER_Y,buf,sizeof(buf));
              xvt_res_get_str(STR_NOEXIST,buf2,sizeof(buf2));
              xvt_dm_post_error("%s %4.4lf m %s",buf,scr.z[0],buf2);
              //xvt_dm_post_error("Linker y-Wert %4.2lf m existiert nicht",scr.z[0]);
            }
            else
            {
              if (!list->ExistStation(scr.z[2],1))
              {
                char buf[200],buf2[200];//Dick 26.11.99
                xvt_res_get_str(STR_RECHTER_Y,buf,sizeof(buf));
                xvt_res_get_str(STR_NOEXIST,buf2,sizeof(buf2));
                xvt_dm_post_error("%s %4.4lf m %s",buf,scr.z[2],buf2);
                //	  xvt_dm_post_error("Rechter y-Wert %4.2lf m existiert nicht",scr.z[2]);
              }
              else
              {
                SaveProfilFile =TRUE;
                list->SaveSonderprofildaten(&scr, DURCHST_BEREICH);
                sichere_datenblock=FALSE;
                if (WIN_117 != NULL_WIN)
                {
                  xvt_dwin_invalidate_rect(WIN_117,0);
                  paint->DurchstBereiche(WIN_117, &pmm,scr.datensatz);
                }
                dlg_sonderprofil= NULL_WIN;
                sichere_datenblock=FALSE;
                xvt_vobj_destroy(xdWindow);
              }
            }
          }
        case RESP_2:             // nicht sichere_datenblock
          sichere_datenblock=FALSE;
          break;             //zurück
        case RESP_3:
          break;
          break;
        };
      }
      dlg_sonderprofil = NULL_WIN;
      xvt_vobj_destroy(xdWindow);
    }
    break;
  case E_CONTROL:
    {
      
      switch(xdControlId) {
      case DLG_140_PUSHBUTTON_6: /* "OK" */
        {
          xvt_vobj_get_title(edit_dlg140[0],daten[0],19);
          xvt_vobj_get_title(edit_dlg140[1],daten[1],19);
          xvt_vobj_get_title(edit_dlg140[2],daten[2],19);
          xvt_vobj_get_title(edit_dlg140[3],daten[3],19);
          is_zahl(daten[0]);
          is_zahl(daten[1]);
          is_zahl(daten[2]);
          is_zahl(daten[3]);
          scr.z[0]= atof(daten[0]);
          scr.z[1]= atof(daten[1]);
          scr.z[2]= atof(daten[2]);
          scr.z[3]= atof(daten[3]);
          if (!list->ExistStation(scr.z[0],1))
          {
            char buf[200],buf2[200];//Dick 26.11.99
            xvt_res_get_str(STR_LINKER_Y,buf,sizeof(buf));
            xvt_res_get_str(STR_NOEXIST,buf2,sizeof(buf2));
            xvt_dm_post_error("%s %4.4lf m %s",buf,scr.z[0],buf2);
            //xvt_dm_post_error("linker y-Wert %4.2lf m existiert nicht",scr.z[0]);
          }
          else
          {
            if (!list->ExistStation(scr.z[2],1))
            {
              char buf[200],buf2[200];//Dick 26.11.99
              xvt_res_get_str(STR_RECHTER_Y,buf,sizeof(buf));
              xvt_res_get_str(STR_NOEXIST,buf2,sizeof(buf2));
              xvt_dm_post_error("%s %4.4lf m %s",buf,scr.z[2],buf2);
              
              
            }
            //	  xvt_dm_post_error("rechter y-Wert %4.2lf m existiert nicht",scr.z[2]);
            else
            {
              scr.z[1]=list->Get_Station_Hoehe(scr.z[0]);
              scr.z[3]=list->Get_Station_Hoehe(scr.z[2]);
              
              if ( ! list->Exist_Station_Hoehe(scr.z[0],scr.z[1]) )
              {
                char buf[200],buf2[200];//Dick 26.11.99
                xvt_res_get_str(STR_LINKER_Z,buf,sizeof(buf));
                xvt_res_get_str(STR_NOEXIST,buf2,sizeof(buf2));
                xvt_dm_post_error("%s %4.4lf m %s",buf,scr.z[0],buf2);
                //xvt_dm_post_error("linker z-Wert %4.2lf m existiert nicht",scr.z[1]);
              }
              else
              {
                if ( ! list->Exist_Station_Hoehe(scr.z[2],scr.z[3]) )
                {
                  char buf[200],buf2[200];//Dick 26.11.99
                  xvt_res_get_str(STR_RECHTER_Z,buf,sizeof(buf));
                  xvt_res_get_str(STR_NOEXIST,buf2,sizeof(buf2));
                  xvt_dm_post_error("%s %4.4lf m %s",buf,scr.z[2],buf2);
                  //xvt_dm_post_error("rechter z-Wert %4.2lf m existiert nicht",scr.z[3]);
                }
                else
                {
                  SaveProfilFile =TRUE;
                  list->SaveSonderprofildaten(&scr, DURCHST_BEREICH);
                  sichere_datenblock=FALSE;
                  if((scr.z[0]!=BCE_NAN) &&
                    (scr.z[1]!=BCE_NAN) &&
                    (scr.z[2]!=BCE_NAN) &&
                    (scr.z[3]!=BCE_NAN))
                  {
                    if (WIN_117 != NULL_WIN)
                    {
                      xvt_dwin_invalidate_rect(WIN_117,0);
                      paint->DurchstBereiche(WIN_117, &pmm,scr.datensatz);
                    }
                  }
                  dlg_sonderprofil= NULL_WIN;
                  sichere_datenblock=FALSE;
                  xvt_vobj_destroy(xdWindow);
                }
              }
              
              
            }
          }
        }
        break;
      case DLG_140_PUSHBUTTON_11: /* "Abbruch" */
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
              xvt_vobj_get_title(edit_dlg140[0],daten[0],19);
              xvt_vobj_get_title(edit_dlg140[1],daten[1],19);
              xvt_vobj_get_title(edit_dlg140[2],daten[2],19);
              xvt_vobj_get_title(edit_dlg140[3],daten[3],19);
              is_zahl(daten[0]);
              is_zahl(daten[1]);
              is_zahl(daten[2]);
              is_zahl(daten[3]);
              scr.z[0]= atof(daten[0]);
              scr.z[1]= atof(daten[1]);
              scr.z[2]= atof(daten[2]);
              scr.z[3]= atof(daten[3]);
              if (!list->ExistStation(scr.z[0],1))
              {
                char buf[200],buf2[200];//Dick 26.11.99
                xvt_res_get_str(STR_LINKER_Y,buf,sizeof(buf));
                xvt_res_get_str(STR_NOEXIST,buf2,sizeof(buf2));
                xvt_dm_post_error("%s %4.4lf m %s",buf,scr.z[0],buf2); 
                //xvt_dm_post_error("linker y-Wert %4.2lf m existiert nicht",scr.z[0]);
              }
              else
              {
                if (!list->ExistStation(scr.z[2],1))
                  //					  xvt_dm_post_error("rechter y-Wert %4.2lf m existiert nicht",scr.z[2]);
                {
                  char buf[200],buf2[200];//Dick 26.11.99
                  xvt_res_get_str(STR_RECHTER_Y,buf,sizeof(buf));
                  xvt_res_get_str(STR_NOEXIST,buf2,sizeof(buf2));
                  xvt_dm_post_error("%s %4.4lf m %s",buf,scr.z[2],buf2);
                  
                  
                }
                else
                {
                  scr.z[1]=list->Get_Station_Hoehe(scr.z[0]);
                  scr.z[3]=list->Get_Station_Hoehe(scr.z[2]);
                  
                  if ( ! list->Exist_Station_Hoehe(scr.z[0],scr.z[1]) )
                  {
                    char buf[200],buf2[200];//Dick 26.11.99
                    xvt_res_get_str(STR_LINKER_Z,buf,sizeof(buf));
                    xvt_res_get_str(STR_NOEXIST,buf2,sizeof(buf2));
                    xvt_dm_post_error("%s %4.4lf m %s",buf,scr.z[0],buf2);
                    //xvt_dm_post_error("linker z-Wert %4.2lf m existiert nicht",scr.z[1]);
                  }
                  else
                  {
                    if ( ! list->Exist_Station_Hoehe(scr.z[2],scr.z[3]) )
                    {
                      char buf[200],buf2[200];//Dick 26.11.99
                      xvt_res_get_str(STR_RECHTER_Y,buf,sizeof(buf));
                      xvt_res_get_str(STR_NOEXIST,buf2,sizeof(buf2));
                      xvt_dm_post_error("%s %4.4lf m %s",buf,scr.z[2],buf2);
                      //xvt_dm_post_error("rechter z-Wert %4.2lf m existiert nicht",scr.z[3]);
                    }
                    else
                    {
                      SaveProfilFile =TRUE;
                      list->SaveSonderprofildaten(&scr, DURCHST_BEREICH);
                      sichere_datenblock=FALSE;
                      if((scr.z[0]!=BCE_NAN) &&
                        (scr.z[1]!=BCE_NAN) &&
                        (scr.z[2]!=BCE_NAN) &&
                        (scr.z[3]!=BCE_NAN))
                      {
                        if (WIN_117 != NULL_WIN)
                        {
                          xvt_dwin_invalidate_rect(WIN_117,0);
                          paint->DurchstBereiche(WIN_117, &pmm,scr.datensatz);
                        }
                      }
                      dlg_sonderprofil= NULL_WIN;
                      sichere_datenblock=FALSE;
                      xvt_vobj_destroy(xdWindow);
                      xdWindow = NULL_WIN;
                    }
                  }
                  
                  
                }
              }
            }
            break;
          case RESP_2:             // nicht sichere_datenblock
            sichere_datenblock=FALSE;
            break;             //zurück
          case RESP_3:break;
            break;
          };
          dlg_sonderprofil = NULL_WIN;
          if (xdWindow !=NULL_WIN)
            xvt_vobj_destroy(xdWindow);
      }
      break;
      
      
    case DLG_140_EDIT_9:			
      {
        if (xdEvent->v.ctl.ci.v.edit.focus_change)
        {
          if (xdEvent->v.ctl.ci.v.edit.active) 
          {
            /* focus has entered the control */
          }
          else
          {
            /* 			focus has left the control				*/
            xvt_vobj_get_title( edit_dlg140[0], daten[0], 19 );
            fehler140 = is_zahl( daten[0] );
            if( fehler140 == 0 )
            {
              char buf[200];//Dick 26.11.99
              xvt_res_get_str( STR_ERROR_INPUT, buf, sizeof(buf) );
              xvt_dm_post_error( "%s",buf ); // "Fehler in der Eingabe !"
              xvt_vobj_set_title( edit_dlg140[0], "\0" );
            }
            else
            {
              scr.z[0]= atof( daten[0] );
              if( !list->ExistStation( scr.z[0],1 ) )
              {
                char buf[200], buf2[200];//Dick 26.11.99
                xvt_res_get_str( STR_RECHTER_Y, buf, sizeof(buf) );
                xvt_res_get_str( STR_NOEXIST, buf2, sizeof(buf2) );
                xvt_dm_post_error( "%s %4.4lf m %s", buf, scr.z[0],buf2);
                //xvt_dm_post_error("linker y-Wert %4.2lf m existiert nicht",scr.z[0]);
              }
              else
              {
                scr.z[1] = list->Get_Station_Hoehe( scr.z[0] );
                sprintf( daten[1],"%.4lf",scr.z[1] );
                is_zahl(daten[1]);
                xvt_vobj_set_title(edit_dlg140[1],daten[1]);
                list->SaveSonderprofildaten(&scr, DURCHST_BEREICH);
                sichere_datenblock=FALSE;
                xvt_vobj_set_title(edit_dlg140[0],daten[0]);
                if((scr.z[0]!=BCE_NAN) &&
                  (scr.z[1]!=BCE_NAN) &&
                  (scr.z[2]!=BCE_NAN) &&
                  (scr.z[3]!=BCE_NAN))
                {
                  if (WIN_117 != NULL_WIN)
                  {
                    xvt_dwin_invalidate_rect(WIN_117,0);
                    paint->DurchstBereiche(WIN_117, &pmm,scr.datensatz);
                  }
                }
              }
            }
          }
        } 
        else 
        {
          /* Contents of control were changed */
          xvt_vobj_get_title(edit_dlg140[0],daten[0],19);
          sichere_datenblock = TRUE;
        }
      }
      break;
      
    case DLG_140_EDIT_10:
      {
        if (xdEvent->v.ctl.ci.v.edit.focus_change)
        {
          if (xdEvent->v.ctl.ci.v.edit.active)
          {	/*	focus has entered the control	*/
          }
          else
          {	/* 			focus has left the control				*/
            xvt_vobj_get_title(edit_dlg140[1],daten[1],19);
            fehler140 = is_zahl(daten[1]);
            if (fehler140 ==0)
            {
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
              xvt_dm_post_error("%s",buf);
              //xvt_dm_post_error("Fehler in der Eingabe !");
              xvt_vobj_set_title(edit_dlg140[1],"\0");
            }
            else
            {
              scr.z[1] = atof(daten[1]);
              if ( !list->Exist_Station_Hoehe(scr.z[0],scr.z[1]) )
              {
                char buf[200],buf2[200];//Dick 26.11.99
                xvt_res_get_str(STR_LINKER_Z,buf,sizeof(buf));
                xvt_res_get_str(STR_NOEXIST,buf2,sizeof(buf2));
                xvt_dm_post_error("%s %4.4lf m %s",buf,scr.z[0],buf2);
                //xvt_dm_post_error("linker z-Wert %4.2lf m existiert nicht",scr.z[1]);
              }
              else
              {
                list->SaveSonderprofildaten( &scr, DURCHST_BEREICH );
                sichere_datenblock = FALSE;
                xvt_vobj_set_title( edit_dlg140[1],daten[1] );
                if( (scr.z[0]!=BCE_NAN) &&
                  (scr.z[1]!=BCE_NAN) &&
                  (scr.z[2]!=BCE_NAN) &&
                  (scr.z[3]!=BCE_NAN))
                {
                  if (WIN_117 != NULL_WIN)
                  {
                    xvt_dwin_invalidate_rect(WIN_117,0);
                    paint->DurchstBereiche(WIN_117, &pmm,scr.datensatz);
                  }
                }
              }
            }
          }
        }
        else
        {	/*		Contents of control were changed		*/
          xvt_vobj_get_title(edit_dlg140[1],daten[1],19);
          sichere_datenblock = TRUE;
        }
      }
      break;
      
      
    case DLG_140_EDIT_12:
      {
        /*		Edit control was operated.	*/
        if (xdEvent->v.ctl.ci.v.edit.focus_change)
        {
          if (xdEvent->v.ctl.ci.v.edit.active)
          { /* focus has entered the control */
          }
          else 
          { /* focus has left the control */
            xvt_vobj_get_title( edit_dlg140[2], daten[2], 19 );
            fehler140 = is_zahl( daten[2] );
            if (fehler140 ==0 )
            {
              char buf[200];//Dick 26.11.99
              xvt_res_get_str( STR_ERROR_INPUT, buf, sizeof(buf) );
              xvt_dm_post_error( "%s",buf );
              //xvt_dm_post_error("Fehler in der Eingabe !");
              xvt_vobj_set_title( edit_dlg140[1],"\0" );
            }
            else
            {
              scr.z[2] = atof( daten[2] );
              if( !list->ExistStation( scr.z[2], 1 ) )
              {
                char buf[200],buf2[200];//Dick 26.11.99
                xvt_res_get_str(STR_RECHTER_Y,buf,sizeof(buf));
                xvt_res_get_str(STR_NOEXIST,buf2,sizeof(buf2));
                xvt_dm_post_error("%s %4.4lf m %s",buf,scr.z[2],buf2); //	  xvt_dm_post_error("rechter y-Wert %4.2lf m existiert nicht",scr.z[2]);
              }
              else
              {
                scr.z[3] = list->Get_Station_Hoehe( scr.z[2] );
                sprintf( daten[3], "%.4lf", scr.z[3] );
                is_zahl( daten[3] );
                xvt_vobj_set_title( edit_dlg140[3], daten[3] );
                
                list->SaveSonderprofildaten( &scr, DURCHST_BEREICH );
                sichere_datenblock = FALSE;
                xvt_vobj_set_title( edit_dlg140[2], daten[2] );
                if((scr.z[0]!=BCE_NAN) &&
                  (scr.z[1]!=BCE_NAN) &&
                  (scr.z[2]!=BCE_NAN) &&
                  (scr.z[3]!=BCE_NAN))
                {
                  if (WIN_117 != NULL_WIN)
                  {
                    xvt_dwin_invalidate_rect(WIN_117,0);
                    paint->DurchstBereiche(WIN_117, &pmm,scr.datensatz);
                  }
                }
              }
            }
          }
        } 
        else 
        {
        /*
        Contents of control were changed
          */
          xvt_vobj_get_title(edit_dlg140[2],daten[2],19);
          sichere_datenblock = TRUE;
        }
      }
      break;
      
    case DLG_140_EDIT_13:
      {
        if (xdEvent->v.ctl.ci.v.edit.focus_change)
        {
          if (xdEvent->v.ctl.ci.v.edit.active)
          {	/*	focus has entered the control	*/
          }
          else
          {	/* 			focus has left the control				*/
            xvt_vobj_get_title(edit_dlg140[3],daten[3],19);
            fehler140 = is_zahl(daten[3]);
            if (fehler140 ==0)
            {
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
              xvt_dm_post_error("%s",buf);
              //xvt_dm_post_error("Fehler in der Eingabe !");
              xvt_vobj_set_title(edit_dlg140[3],"\0");
            }
            else
            {
              scr.z[3]= atof(daten[3]);
              if ( !list->Exist_Station_Hoehe(scr.z[2],scr.z[3]) )
              {
                char buf[200],buf2[200];//Dick 26.11.99
                xvt_res_get_str(STR_RECHTER_Z,buf,sizeof(buf));
                xvt_res_get_str(STR_NOEXIST,buf2,sizeof(buf2));
                xvt_dm_post_error("%s %4.4lf m %s",buf,scr.z[2],buf2);
                //xvt_dm_post_error("rechter z-Wert %4.2lf m existiert nicht",scr.z[3]);
              }
              else
              {
                list->SaveSonderprofildaten(&scr, DURCHST_BEREICH);
                sichere_datenblock=FALSE;
                xvt_vobj_set_title(edit_dlg140[3],daten[3]);
                if((scr.z[0]!=BCE_NAN) &&
                  (scr.z[1]!=BCE_NAN) &&
                  (scr.z[2]!=BCE_NAN) &&
                  (scr.z[3]!=BCE_NAN))
                {
                  if (WIN_117 != NULL_WIN)
                  {
                    xvt_dwin_invalidate_rect(WIN_117,0);
                    paint->DurchstBereiche(WIN_117, &pmm,scr.datensatz);
                  }
                }
              }
            }
          }
        }
        else
        {	/*		Contents of control were changed		*/
          xvt_vobj_get_title(edit_dlg140[3],daten[3],19);
          sichere_datenblock = TRUE;
        }
      }
      break;
      
      
      
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
