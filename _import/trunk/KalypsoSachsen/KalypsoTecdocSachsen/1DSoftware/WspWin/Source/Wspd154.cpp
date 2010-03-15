#include <windows.h>
#include "xvt.h"
#include "wspwin.h"
#include "resource.h"

#include "global_types.h"
#include "..\..\wspdlg\Include\export.h"

#include "list.h"
#include "dis_prof.h"
#include "paint.h"

#include "global.h"

#include "typen.h"

#include "wsphilfe.h"


extern XVT_HELP_INFO hi;

#define DLG_RES_ID DLG_154
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODELESS

#define OFFSET 1
#define FAKTOR 2
#define NEU    3
#define ANFANG 4
#define ENDE   5
extern Scroller scr;
extern WINDOW win_dlg_154, dlg_sonderprofil, win128 = NULL;
extern WINDOW Edit_Win116[15],WIN_116,win_120[15],WIN120,win122;
extern MMP mmp;
extern MinMax pmm;
extern BOOLEAN neue_datensatz_wahl ;
extern int db_auswahl;//Dick 28.11.98
extern WINDOW front_win;//Dick 28.11.98
extern DRAW_DIMENSIONS WspDrawDimensions;

WINDOW edit_win_154[5];
WINDOW *dlg154_editwin;
WINDOW ctl_win154;
BOOLEAN alle_stationen=FALSE;
int  werttyp=-1;
int stationstyp=-1;

/*************   GHJ   *************/
static WNDPROC defWndProc;
LRESULT CALLBACK Dlg154WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg)
	{
	case WM_HELP:
		{
			LPHELPINFO lphi = (LPHELPINFO)lParam;
			if (hi!=NULL_HELP_INFO)
			{
				xvt_help_display_topic(hi, HID_KAPITEL_4_4_3_2_1);
			}
		}
		break;

	default:
		break;
	}
	return CallWindowProc(defWndProc, hwnd, uMsg, wParam, lParam);
}
/**************************************/



/*	Handler for dialog DLG_154 ("Editieren Rauhigkeit") */
long XVT_CALLCONV1
#if XVT_CC_PROTO
DLG_154_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
DLG_154_eh XVT_CALLCONV2 (xdWindow, xdEvent)
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
		 SetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC, (LONG)&Dlg154WindowProc);
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
		    win_dlg_154 = xdWindow;
			ctl_win154 = xvt_win_get_ctl(xdWindow, DLG_154_CHECKBOX_15);
		
			  mmp.mouse_down_v =0;
			  mmp.mouse_down_h = mmp.mouse_h = WspDrawDimensions.DX;
			  mmp.mouse_v =0;
			  mmp.position_mouse_down =mmp.position_mouse_up=0;
			  mmp.last_rct.top=mmp.last_rct.left=mmp.last_rct.bottom=mmp.last_rct.right=0;
			  mmp.active=FALSE;
		
		 for (int i=0;i<=4;i++)
			edit_win_154[i]=xvt_win_get_ctl(xdWindow,DLG_154_EDIT_8 +i);
		 dlg154_editwin = &edit_win_154[0];

		 char hor[15];
         if(WIN_117==NULL)//Dick 11.08.98
             {
              mmp.position_mouse_down=1;
              mmp.hor_m_down =list->Get_Num_Station(scr.datensatz ,&mmp.position_mouse_down,1);
             }
		 sprintf(hor,"%.4lf",mmp.hor_m_down);//gcvt(mmp.hor_m_down,10,hor);
		 xvt_vobj_set_title(edit_win_154[0],hor);
		 xvt_vobj_set_title(edit_win_154[1],hor);

         if(win128 != NULL)             
              xvt_vobj_set_visible(xvt_win_get_ctl(xdWindow, DLG_154_DB),FALSE);           
         else
             xvt_vobj_set_visible(xvt_win_get_ctl(xdWindow, DLG_154_DB),TRUE);
		 if (hi!=NULL_HELP_INFO)
			xvt_help_set_win_assoc(hi, xdWindow, HID_KAPITEL_4_4_3_2_1, 0L);
		}
		break;
	case E_DESTROY:
		{
		 win_dlg_154 = NULL_WIN;
		 *dlg154_editwin = NULL_WIN;
       dlg_sonderprofil = NULL_WIN;
		}
		break;
	case E_FOCUS:
		{
		/*			Dialog has lost or gained focus.		*/
		if (xdEvent->v.active)  {
			/*			Dialog has gained focus	*/
		} else {
			/*			Dialog has lost focus	*/
		}
		}
		break;
	case E_SIZE:
		{
		}
		break;
	case E_UPDATE:
		{
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

		switch(xdControlId) {
		case DLG_154_PUSHBUTTON_13: /* "OK" */
			{
			 char temp[11],*p;
			 double wert;
			 int fehler =0;
             double anfang,ende;
//			 if (stationstyp < ANFANG)
				 {
					int pos;
					xvt_vobj_get_title(edit_win_154[0],temp,10);
					if((p = strchr(temp,','))!=NULL)
						p[0]='.';
                    anfang=atof(temp);
					pos = list->ExistStation(atof(temp),1);
//					pos = list->ExistStation(atof(temp),scr.datensatz);
					if (pos >0)
						mmp.position_mouse_down = pos;
					else
					{
                      char buf[200],buf2[200];
                      xvt_res_get_str(STR_Y_WERT,buf,sizeof(buf));
                      xvt_res_get_str(STR_ANFANG_UNGUELTIG,buf2,sizeof(buf2));
                      xvt_dm_post_note("%s:%s",buf,buf2);
					 //xvt_dm_post_note("y-Wert:Anfang ungültig");
					 xvt_vobj_set_title(edit_win_154[0],"\0");
					 fehler =1;
					}

					xvt_vobj_get_title(edit_win_154[1],temp,10);
					if((p = strchr(temp,','))!=NULL)
						p[0]='.';
                    ende=atof(temp);
					pos = list->ExistStation(atof(temp),1);
//					pos = list->ExistStation(atof(temp),scr.datensatz);

					 if (pos >0)
					  {
					  //	if (!xvt_ctl_is_checked(ctl_win154))  /*nicht alle Stationen:
						 //										 mmp.position_mouse_up=pos übernehmen*/
							mmp.position_mouse_up = pos;
					  }
					 else
					  {
                        char buf[200],buf2[200];
                        xvt_res_get_str(STR_Y_WERT,buf,sizeof(buf));
                        xvt_res_get_str(STR_ENDE_UNGUELTIG,buf2,sizeof(buf2));
                        xvt_dm_post_note("%s:%s",buf,buf2);
						//xvt_dm_post_note("y-Wert:Ende ungültig");
						xvt_vobj_set_title(edit_win_154[1],"\0");
						fehler =2;
					  }

				 }

			 if ((werttyp >0)&&(!fehler))   // OFFSET,FAKTOR,NEU
				{
				 xvt_vobj_get_title(edit_win_154[werttyp+1],temp,6);
				 if((p = strchr(temp,','))!=NULL)
					 p[0]='.';
				 wert = atof(temp);
				 if ((strlen(temp)>0)&&(werttyp>0))
                     {
                      if(WIN_117!=NULL) //Dick 5.08.98
					   paint->Edit_Rauhigkeit(wert,werttyp,&mmp);
                       else
                        paint->Edit_Rauhigkeit(wert,werttyp,anfang,ende,scr.datensatz); //Dick 5.08.98
                     }
				 list->GetScrollDaten(&scr);
                 if(WIN_116!=NULL) //Dick 5.08.98
				 display_prof116(&Edit_Win116[0]);
                 else                     
                  display_prof120(&win_120[0]);//Dick 5.08.98                 
                     
				 SaveProfilFile = TRUE;
				}
             if(WIN_117!=NULL)//Dick 5.08.98
                 {
			  mmp.mouse_down_h=WspDrawDimensions.DX;
			  mmp.mouse_h=WspDrawDimensions.DX;
			  paint->edit_list_pnt(&mmp,&pmm);

			 xvt_dwin_invalidate_rect(WIN_117,0);  // Grafik updaten
                 }
			 xvt_vobj_destroy(xdWindow);
			}
			break;
		case DLG_154_PUSHBUTTON_14: /* "Abbruch" */
			{
			 xvt_vobj_destroy(xdWindow);
			}
			break;
		case DLG_154_EDIT_8:   //Station Anfang
		  {
			/*			Edit control was operated.			*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change)
			  {
				if (xdEvent->v.ctl.ci.v.edit.active)
				{ 	/*	 focus has entered the control 	*/
				}
				else
				{ /*  focus has left the control		*/
					char temp[11],*p;
					int pos=0;
					xvt_vobj_get_title(edit_win_154[0],temp,10);
					if((p = strchr(temp,','))!=NULL)
						p[0]='.';
					pos = list->ExistStation(atof(temp),1);
//					pos = list->ExistStation(atof(temp),scr.datensatz);
					if (pos >0)
						mmp.position_mouse_down = pos;
					else
					{
                     char buf[200],buf2[200];
                      xvt_res_get_str(STR_Y_WERT,buf,sizeof(buf));
                      xvt_res_get_str(STR_ANFANG_UNGUELTIG,buf2,sizeof(buf2));
                      xvt_dm_post_note("%s:%s",buf,buf2);
					 //xvt_dm_post_note("y-Wert: Anfang ungültig");
					 xvt_vobj_set_title(edit_win_154[0],"\0");
					 stationstyp =-1;
					}
				}
			  }
			else
			  { /*		Contents of control were changed	*/
				stationstyp=ANFANG;
			  }
		  }
			break;
		case DLG_154_EDIT_9:  // Station Ende
		  {
			/*			Edit control was operated.			*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change)
			  {
				if (xdEvent->v.ctl.ci.v.edit.active)
				{ 	/*	 focus has entered the control 	*/
				}
				else
				{ /*  focus has left the control		*/
					char temp[11],*p;
					int pos=0;
					xvt_vobj_get_title(edit_win_154[1],temp,10);
					if((p = strchr(temp,','))!=NULL)
						p[0]='.';
					pos = list->ExistStation(atof(temp),1);
//					pos = list->ExistStation(atof(temp),scr.datensatz);
					if (pos >0)
						mmp.position_mouse_up = pos;
					else
					{
                     char buf[200],buf2[200];
                      xvt_res_get_str(STR_Y_WERT,buf,sizeof(buf));
                      xvt_res_get_str(STR_ENDE_UNGUELTIG,buf2,sizeof(buf2));
                      xvt_dm_post_note("%s:%s",buf,buf2);
					 //xvt_dm_post_note("y-Wert: Ende ungültig");
					 xvt_vobj_set_title(edit_win_154[1],"\0");
					 stationstyp =-1;
					}

				}
			  }
			else
			  { /*		Contents of control were changed	*/
				stationstyp=ENDE;
			  }
		  }
			break;
		case DLG_154_EDIT_10:			{
			/*
				Edit control: OFFSET was operated.
			*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change) {
				if (xdEvent->v.ctl.ci.v.edit.active) {
					/*
						focus has entered the control
					*/
				  xvt_vobj_set_title(edit_win_154[3],"\0");
				  xvt_vobj_set_title(edit_win_154[4],"\0");
				  werttyp= OFFSET;
				} else {
					/*
						focus has left the control
					*/
				}
			} else {
				/*
					Contents of control were changed
				*/
			}
			}
			break;
		case DLG_154_EDIT_11:			{
			/*
				Edit control: FAKTOR was operated.
			*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change) {
				if (xdEvent->v.ctl.ci.v.edit.active) {
					/*
						focus has entered the control
					*/
				  xvt_vobj_set_title(edit_win_154[2],"\0");
				  xvt_vobj_set_title(edit_win_154[4],"\0");
				  werttyp = FAKTOR;
				} else {
					/*

						focus has left the control
					*/
				}
			} else {
				/*
					Contents of control were changed
				*/
			}
			}
			break;
		case DLG_154_EDIT_12:			{
			/*
				Edit control: neuer Wert was operated.
			*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change) {
				if (xdEvent->v.ctl.ci.v.edit.active) {
					/*
						focus has entered the control
					*/
				  xvt_vobj_set_title(edit_win_154[2],"\0");
				  xvt_vobj_set_title(edit_win_154[3],"\0");
				  werttyp = NEU;
				} else {
					/*
						focus has left the control
					*/
				}
			} else {
				/*
					Contents of control were changed
				*/
			}
			}
			break;
		case DLG_154_CHECKBOX_15: /* "alle Stationen" */
			{
			xvt_ctl_set_checked(ctl_win154, !xvt_ctl_is_checked (ctl_win154));
			alle_stationen=!alle_stationen;

			if (xvt_ctl_is_checked(ctl_win154))
			 {
			  char hor[15],*p;
              if(WIN_117==NULL)//Dick 11.08.98
             {
              mmp.position_mouse_down=1;
              pmm.minX =list->Get_Num_Station(scr.datensatz ,&mmp.position_mouse_down,1);
              mmp.position_mouse_down=ds_info[1];
              pmm.maxX =list->Get_Num_Station(scr.datensatz ,&mmp.position_mouse_down,1);
             }
			  sprintf(hor,"%5.4f",pmm.minX);
			  if(  (p=strchr(hor,'.'))!=NULL)
				  p[5]='\0';
			  xvt_vobj_set_title(edit_win_154[0],hor);

			  sprintf(hor,"%5.4f",pmm.maxX);
			  if(  (p=strchr(hor,'.'))!=NULL)
				  p[5]='\0';
			  xvt_vobj_set_title(edit_win_154[1],hor);
			  mmp.position_mouse_down=1;
			  mmp.position_mouse_up=ds_info[scr.datensatz];

			  mmp.mouse_down_h=WspDrawDimensions.DX;
			  mmp.mouse_h=(INT) (WspDrawDimensions.DX+WspDrawDimensions.DELTA_X);
			  mmp.active = TRUE;
              if(WIN_117!=NULL) //Dick 5.08.98
			  paint->edit_list_pnt(&mmp,&pmm);
			 }
			else
			 {
			  xvt_vobj_set_title(edit_win_154[0],"\0");
			  xvt_vobj_set_title(edit_win_154[1],"\0");
			  mmp.mouse_down_h=mmp.mouse_h=WspDrawDimensions.DX;
			  mmp.active = TRUE;
              if(WIN_117!=NULL) //Dick 5.08.98
			  paint->edit_list_pnt(&mmp,&pmm);  //löschen
			 }
			}
			break;
        case DLG_154_DB:
            {
            if(win122!=NULL_WIN)
				  xvt_vobj_destroy(win122);
			 if (WIN120!=NULL_WIN)
					front_win = WIN120;
			 else if (WIN_116 !=NULL_WIN)
					front_win = WIN_116;
					else front_win = TASK_WIN;
             if(typ[scr.datensatz]==RAUHIGKEIT)
                db_auswahl=1;
             else
                 db_auswahl=0;
             HWND db_dlg=NULL;
             if(db_auswahl)
                 db_dlg=DoDatabankDlg((HWND)xvt_vobj_get_attr(front_win,ATTR_NATIVE_WINDOW), start_dir,rauheit_ks);
             else
                 db_dlg=DoDatabankDlg((HWND)xvt_vobj_get_attr(front_win,ATTR_NATIVE_WINDOW), start_dir,rauheit_kst);
            }
            break;
		default:
			break;
		}
		}
		break;
	case E_TIMER:
		{
		}
		break;
	case E_USER:
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
