#include <windows.h>
#include "xvt.h"
#include "wspwin.h"
#include "resource.h"

#include "global_types.h"
#include "..\..\wspdlg\Include\export.h"

#include "list.h"
#include "paint.h"
#include "typen.h"
#include "dis_prof.h"

#include "global.h"

#include "wsphilfe.h"

extern XVT_HELP_INFO hi;

#define DLG_RES_ID DLG_110
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODELESS

#define OFFSET 1
#define FAKTOR 2
#define NEU    3
#define ANFANG 4
#define ENDE   5

WINDOW win_dlg_110, win136 = NULL_WIN;
extern WINDOW dlg_sonderprofil,win122;
extern WINDOW Edit_Win116[15],win_120[100];
extern MMP mmp;
extern MinMax pmm;
WINDOW edit_win_110[5];
WINDOW *Dlg110_edit_win; //Dick 18.04.99
WINDOW ctl_win110;
extern BOOLEAN alle_stationen,
               bewuchs_alle;
extern char ax[13],ay[13],dp[13];

extern int db_auswahl;//Dick 28.11.98
WINDOW front_win;//Dick 28.11.98

extern DRAW_DIMENSIONS WspDrawDimensions;

int  wtyp=-1;
int  styp=-1;

/*************   GHJ   *************/
static WNDPROC defWndProc;
LRESULT CALLBACK Dlg110WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg)
	{
	case WM_HELP:
		{
			LPHELPINFO lphi = (LPHELPINFO)lParam;
			if (hi!=NULL_HELP_INFO)
			{
				if (typ[scr.datensatz]==AXM ||
					typ[scr.datensatz]==AYM ||
					typ[scr.datensatz]==DPM)
					xvt_help_display_topic(hi, HID_KAPITEL_5_4_1);
				else
					xvt_help_display_topic(hi, HID_KAPITEL_4_4_2_1);
			}
		}
		break;

	default:
		break;
	}
	return CallWindowProc(defWndProc, hwnd, uMsg, wParam, lParam);
}
/***********************************/



/*	Handler for dialog DLG_110 ("Bereiche editieren")*/
long XVT_CALLCONV1
#if XVT_CC_PROTO
DLG_110_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
DLG_110_eh XVT_CALLCONV2 (xdWindow, xdEvent)
WINDOW xdWindow;
EVENT *xdEvent;
#endif
{
	short xdControlId = xdEvent->v.ctl.id;

	switch (xdEvent->type) {
	case E_CREATE:
		{

         /*************   GHJ   *************/
		 if (WIN_116!=NULL_WIN)
			SetParent((HWND)xvt_vobj_get_attr(xdWindow, ATTR_NATIVE_WINDOW), (HWND)xvt_vobj_get_attr(WIN_116, ATTR_NATIVE_WINDOW));
		 else if (WIN120!=NULL_WIN)
			SetParent((HWND)xvt_vobj_get_attr(xdWindow, ATTR_NATIVE_WINDOW), (HWND)xvt_vobj_get_attr(WIN120, ATTR_NATIVE_WINDOW));
		 defWndProc = (WNDPROC)GetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC);
		 SetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC, (LONG)&Dlg110WindowProc);
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
         /**********************************/
         
		 ctl_win110 = xvt_win_get_ctl(xdWindow, DLG_110_CHECKBOX_15);
		 alle_stationen   =FALSE;
		 win_dlg_110      = xdWindow;
		 dlg_sonderprofil = xdWindow;

		 mmp.position_mouse_down = 0;
		 mmp.position_mouse_up = 0;

         //Dick 18.04.99
         mmp.mouse_down_v =0;
         mmp.mouse_down_h = mmp.mouse_h = WspDrawDimensions.DX;
         mmp.mouse_v =0;
         mmp.position_mouse_down =mmp.position_mouse_up=0;
         mmp.last_rct.top=mmp.last_rct.left=mmp.last_rct.bottom=mmp.last_rct.right=0;
         mmp.active=FALSE;
         //

		 for (int i=0;i<=4;i++)
			edit_win_110[i]=xvt_win_get_ctl(xdWindow,DLG_110_EDIT_8 +i);

         Dlg110_edit_win=&edit_win_110[0];

         char hor[15];
         if(WIN_117!=NULL)//Dick 9.09.99 29.09.99
             {
              mmp.position_mouse_down=1;
              mmp.hor_m_down =list->Get_Num_Station(scr.datensatz ,&mmp.position_mouse_down,1);
             }
		 sprintf(hor,"%.4lf",mmp.hor_m_down);
		 xvt_vobj_set_title(edit_win_110[0],hor);
		 xvt_vobj_set_title(edit_win_110[1],hor);


         if(win136 != NULL_WIN || (typ[scr.datensatz]!=AXM && typ[scr.datensatz]!=AYM && typ[scr.datensatz]!=DPM))
              xvt_vobj_set_visible(xvt_win_get_ctl(xdWindow, DLG_110_DB),FALSE);           
         else
             xvt_vobj_set_visible(xvt_win_get_ctl(xdWindow, DLG_110_DB),TRUE);
		
		 if (hi!=NULL_HELP_INFO)
			xvt_help_set_win_assoc(hi, xdWindow, HID_KAPITEL_5_4_1, 0L);
		}
		break;
	case E_DESTROY:
		{
		 win_dlg_110 = NULL_WIN;
		 dlg_sonderprofil = NULL_WIN;
         bewuchs_alle=FALSE;
		}
		break;
	case E_FOCUS:
		{
		if (xdEvent->v.active)  {
			/*				Dialog has gained focus			*/
		} else {
			/*				Dialog has lost focus			*/
		}
		}
		break;
	case E_CHAR:
		/*			Character typed.		*/
		{
		}
		break;
	case E_CONTROL:
		/*			User operated control in dialog.		*/
		{

		switch(xdControlId) {
		case DLG_110_PUSHBUTTON_13: /* "OK" */
			{
			 char temp[20],*p;
			 double wert;
			 int fehler =0;
			 int old_ds=scr.datensatz;

			 if (styp < ANFANG)
				 {
					int pos;
					xvt_vobj_get_title(edit_win_110[0],temp,10);
					if (strlen(temp)>0)
					{
					 if((p = strchr(temp,','))!=NULL)
						p[0]='.';
					 pos = list->ExistStation(atof(temp),1);
					 if (pos >0)
						mmp.position_mouse_down = pos;
					 else
					 {
                      char buf[200],buf2[200];
                      xvt_res_get_str(STR_Y_WERT,buf,sizeof(buf));
                      xvt_res_get_str(STR_ANFANG_UNGUELTIG,buf2,sizeof(buf2));
                      xvt_dm_post_note("%s:%s",buf,buf2);
					  //xvt_dm_post_note("y-Wert:Anfang ungültig");
					  xvt_vobj_set_title(edit_win_110[0],"\0");
					  fehler =1;
					 }
					}
					else fehler =1;

					xvt_vobj_get_title(edit_win_110[1],temp,10);
					if (strlen(temp)>0)
					{
					 if((p = strchr(temp,','))!=NULL)
						p[0]='.';
					 pos = list->ExistStation(atof(temp),1);
					 if (pos >0)
					  {
						if (!xvt_ctl_is_checked(ctl_win110))
						  mmp.position_mouse_up = pos;
					  }
					 else
					 {
                      char buf[200],buf2[200];
                      xvt_res_get_str(STR_Y_WERT,buf,sizeof(buf));
                      xvt_res_get_str(STR_ENDE_UNGUELTIG,buf2,sizeof(buf2));
                      xvt_dm_post_note("%s:%s",buf,buf2);
					  //xvt_dm_post_note("y-Wert:Ende ungültig");
					  xvt_vobj_set_title(edit_win_110[1],"\0");
					  fehler =2;
					 }
					}
					else  fehler =2;
				 }

			 if ((wtyp >0)&&(fehler==0))   // OFFSET,FAKTOR,NEU
				{
				 xvt_vobj_get_title(edit_win_110[wtyp+1],temp,10);
				 if((p = strchr(temp,','))!=NULL)
					 p[0]='.';
				 wert = atof(temp);

				 if ((strlen(temp)>0)&&(wtyp>0))
					 {
					  if (typ[scr.datensatz]==GELAENDEHOEHE)
						  list->Edit_Gelaende_Bereiche(scr.datensatz,wert,wtyp,&mmp);
					  else
						 {
						  if ((typ[scr.datensatz]==AXM)||(typ[scr.datensatz]==AYM)||(typ[scr.datensatz]==DPM))
							 {
                              if(bewuchs_alle) //Werte wurden in Win136 definiert
                                  {

                                  //bley 1.12.99
								  char buf[200],buf2[200],buf3[200];
								  xvt_res_get_str(STR_NEIN,buf,sizeof(buf));
							      xvt_res_get_str(STR_JA,buf2,sizeof(buf2));
                                  xvt_res_get_str(STR_SICHERE_BEWUCHSP,buf3,sizeof(buf3));
				 
				 	              switch (xvt_dm_post_ask(buf,buf2,NULL,"%s",buf3))
                                  //switch(xvt_dm_post_ask("Nein","Ja",NULL,"Sollen alle 3 Bewuchsparameter geändert werden ?"))
								 
                                      {
                                      case RESP_DEFAULT:
                                          switch (typ[scr.datensatz])
                                              {
                                              case AXM:
                                                  if(wtyp==NEU)
                                                      wert = atof(ax);
                                                  list->Edit_Gelaende_Bereiche(scr.datensatz,wert,wtyp,&mmp);
                                                  break;
                                              case AYM:
                                                  if(wtyp==NEU)
                                                      wert = atof(ay);
                                                  list->Edit_Gelaende_Bereiche(scr.datensatz,wert,wtyp,&mmp);
                                                  break;
                                              case DPM:
                                                  if(wtyp==NEU)
                                                      wert = atof(dp);
                                                  list->Edit_Gelaende_Bereiche(scr.datensatz,wert,wtyp,&mmp);
                                                  break;
                                              default:
                                                  wert = BCE_NAN;
                                                  break;
                                              }
                                          break;
                                              case RESP_2:
                                                  {
                                                  old_ds=scr.datensatz;
                                                  for (int l=0;l<=ds_info[0];l++)
                                                      {
                                                      scr.datensatz=l;
                                                      switch (typ[l])
                                                          {
                                                          case AXM:
                                                              if(wtyp==NEU)
                                                                  wert = atof(ax);
                                                              else
                                                                  wert = atof(temp);
                                                              list->Edit_Gelaende_Bereiche(scr.datensatz,wert,wtyp,&mmp);
                                                              break;
                                                          case AYM:
                                                              if(wtyp==NEU)
                                                                  wert = atof(ay);
                                                              else
                                                                  wert = atof(temp);
                                                              list->Edit_Gelaende_Bereiche(scr.datensatz,wert,wtyp,&mmp);
                                                              break;
                                                          case DPM:
                                                              if(wtyp==NEU)
                                                                  wert = atof(dp);
                                                              else
                                                                  wert = atof(temp);
                                                              list->Edit_Gelaende_Bereiche(scr.datensatz,wert,wtyp,&mmp);
                                                              break;
                                                          default:
                                                              wert = BCE_NAN;
                                                              break;
                                                          }
                                                      }
                                                  }
                                                  break;
                                      };  //-end switch post_ask...
                                  }//end bewuchs_alle
                              else
                                  {
                                   
                                   switch (typ[scr.datensatz])
                                              {
                                              case AXM:
                                                  xvt_vobj_get_title(edit_win_110[4],ax,6);
                                                  if(wtyp==NEU)
                                                      wert = atof(ax);
                                                  list->Edit_Gelaende_Bereiche(scr.datensatz,wert,wtyp,&mmp);
                                                  break;
                                              case AYM:
                                                  xvt_vobj_get_title(edit_win_110[4],ay,6);
                                                  if(wtyp==NEU)
                                                      wert = atof(ay);
                                                  list->Edit_Gelaende_Bereiche(scr.datensatz,wert,wtyp,&mmp);
                                                  break;
                                              case DPM:
                                                  xvt_vobj_get_title(edit_win_110[4],dp,6);
                                                  if(wtyp==NEU)
                                                      wert = atof(dp);
                                                  list->Edit_Gelaende_Bereiche(scr.datensatz,wert,wtyp,&mmp);
                                                  break;
                                              default:
                                                  wert = BCE_NAN;
                                                  break;
                                              }
                                  }
							 }
						 }
					  SaveProfilFile = TRUE;
					 }
				 scr.datensatz = old_ds;
				 list->GetScrollDaten(&scr);
				 if (WIN_116 !=NULL_WIN)
					 {
					  display_prof116(&Edit_Win116[0]);
					  xvt_dwin_invalidate_rect(WIN_117,0);  // Grafik updaten
					 }
				 else if (WIN120 !=NULL_WIN)
						 display_prof120(&win_120[0]);
				}
			 xvt_vobj_destroy(xdWindow);
			}
			break;
		case DLG_110_PUSHBUTTON_14: /* "Abbruch" */
			{
			 xvt_vobj_destroy(xdWindow);
			}
			break;
		case DLG_110_EDIT_8:
		  {
			if (xdEvent->v.ctl.ci.v.edit.focus_change)
			{
				if (xdEvent->v.ctl.ci.v.edit.active)
				{	/*	focus has entered the control	*/
				}
				else
				  {/*		focus has left the control		*/
					char temp[11],*p;
					int pos=0;
					xvt_vobj_get_title(edit_win_110[0],temp,10);
					if((p = strchr(temp,','))!=NULL)
						p[0]='.';
					pos = list->ExistStation(atof(temp),1);
					if (pos >0)
						{
						}
					else
					{
                     char buf[200],buf2[200];
                      xvt_res_get_str(STR_Y_WERT,buf,sizeof(buf));
                      xvt_res_get_str(STR_ANFANG_UNGUELTIG,buf2,sizeof(buf2));
                      xvt_dm_post_note("%s:%s",buf,buf2);
					 //xvt_dm_post_note("y-Wert: Anfang ungültig");
					 xvt_vobj_set_title(edit_win_110[0],"\0");
					 styp =-1;
					}
				  }
			}
			else
			 {	/*		Contents of control were changed		*/
			 }
		  }
		  break;
		case DLG_110_EDIT_9:
		  {
			if (xdEvent->v.ctl.ci.v.edit.focus_change)
			{
				if (xdEvent->v.ctl.ci.v.edit.active)
				{	/*	focus has entered the control	*/
				}
				else
				  {/*		focus has left the control		*/
					char temp[11],*p;
					int pos=0;
					xvt_vobj_get_title(edit_win_110[1],temp,10);
					if((p = strchr(temp,','))!=NULL)
						p[0]='.';
					pos = list->ExistStation(atof(temp),1);
					if (pos >0)
					  {
					  }
					else
					{
                      char buf[200],buf2[200];
                      xvt_res_get_str(STR_Y_WERT,buf,sizeof(buf));
                      xvt_res_get_str(STR_ENDE_UNGUELTIG,buf2,sizeof(buf2));
                      xvt_dm_post_note("%s:%s",buf,buf2);
					 //xvt_dm_post_note("y-Wert: Ende ungültig");
					 xvt_vobj_set_title(edit_win_110[1],"\0");
					 styp =-1;
					}

				  }
			}
			else
			 {	/*		Contents of control were changed		*/
			 }
		  }
		  break;
		case DLG_110_EDIT_10:
		  {
			if (xdEvent->v.ctl.ci.v.edit.focus_change)
			{
				if (xdEvent->v.ctl.ci.v.edit.active)
				{	/*	focus has entered the control	*/
				  xvt_vobj_set_title(edit_win_110[3],"\0");
				  xvt_vobj_set_title(edit_win_110[4],"\0");
				  wtyp= OFFSET;
				}
				else
				  {/*		focus has left the control		*/
				  }
			}
			else
			 {	/*		Contents of control were changed		*/
			 }
		  }
		  break;
		case DLG_110_EDIT_11:
		  {
			if (xdEvent->v.ctl.ci.v.edit.focus_change)
			{
				if (xdEvent->v.ctl.ci.v.edit.active)
				{	/*	focus has entered the control	*/
				  xvt_vobj_set_title(edit_win_110[2],"\0");
				  xvt_vobj_set_title(edit_win_110[4],"\0");
				  wtyp = FAKTOR;
				}
				else
				  {/*		focus has left the control		*/
				  }
			}
			else
			 {	/*		Contents of control were changed		*/
			 }
		  }
		  break;
		case DLG_110_EDIT_12: //neuer Wert
		  {
			if (xdEvent->v.ctl.ci.v.edit.focus_change)
			{
				if (xdEvent->v.ctl.ci.v.edit.active)
				{	/*	focus has entered the control	*/
				  xvt_vobj_set_title(edit_win_110[2],"\0");
				  xvt_vobj_set_title(edit_win_110[3],"\0");
				  wtyp = NEU;
				}
				else
				  {/*		focus has left the control		*/
				  if (typ[scr.datensatz]==AXM)
						xvt_vobj_get_title(edit_win_110[4],ax,6);
				  if (typ[scr.datensatz]==AYM)
						xvt_vobj_get_title(edit_win_110[4],ay,6);
				  if (typ[scr.datensatz]==DPM)
						xvt_vobj_get_title(edit_win_110[4],dp,6);
				  }
			}
			else
			 {	/*		Contents of control were changed		*/
			  wtyp = NEU;
			 }
		  }
		  break;
		case DLG_110_CHECKBOX_15: /* "alle Stationen" */
			{
			xvt_ctl_set_checked(ctl_win110, !xvt_ctl_is_checked (ctl_win110));

			alle_stationen=!alle_stationen;
			if (xvt_ctl_is_checked(ctl_win110))
			 {
			  char hor[25],*p;

			  sprintf(hor,"%4.4f",pmm.minX);
			  if(  (p=strchr(hor,'.'))!=NULL)
				  p[5]='\0';
			  xvt_vobj_set_title(edit_win_110[0],hor);

			  sprintf(hor,"%4.4f",pmm.maxX);
			  if(  (p=strchr(hor,'.'))!=NULL)
				  p[5]='\0';
			  xvt_vobj_set_title(edit_win_110[1],hor);

			  mmp.position_mouse_down=1;
			  mmp.position_mouse_up=ds_info[scr.datensatz];

			 }
			else
			 {
			  xvt_vobj_set_title(edit_win_110[0],"\0");
			  xvt_vobj_set_title(edit_win_110[1],"\0");
			 }



			}
			break;
        case DLG_110_DB:
            {
              if(win122!=NULL_WIN)
				  xvt_vobj_destroy(win122);
			 if (WIN120!=NULL_WIN)
					front_win = WIN120;
			 else if (WIN_116 !=NULL_WIN)
					front_win = WIN_116;
					else front_win = TASK_WIN;
             db_auswahl=2;
             HWND db_dlg=NULL;             
             db_dlg=DoDatabankDlg((HWND)xvt_vobj_get_attr(front_win,ATTR_NATIVE_WINDOW), start_dir,bewuchs);
                        }
            break;
		default:
			break;
		}
		}
		break;
	case E_TIMER:
		/*			Timer associated with window went off.		*/
		{
		}
		break;
	case E_USER:
		/*			Application initiated.		*/
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
