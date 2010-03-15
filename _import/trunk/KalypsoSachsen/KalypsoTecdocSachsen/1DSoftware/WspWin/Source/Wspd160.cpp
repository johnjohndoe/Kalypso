/*   LWA - VERSION  20.10.95 */
#include <windows.h>
#include "xvt.h"
#include "wspwin.h"
#include "resource.h"

#include "global_types.h"
#include "..\..\wspdlg\Include\export.h"

#include "list.h"
#include "bce_allg.h"

#include "global.h"

#include "wsphilfe.h"


extern XVT_HELP_INFO hi;

#define DLG_RES_ID DLG_160
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODELESS


extern WINDOW dlg_sonderprofil;
extern BOOLEAN sichere_datenblock;
WINDOW listbutton160,
		  edit160[5],
          win_dlg_160=NULL_WIN;
WEHR wehrdaten;
extern HWND dlgbeiwert_db;

/*************   GHJ   *************/
static WNDPROC defWndProc;
LRESULT CALLBACK Dlg160WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg)
	{
	case WM_HELP:
		{
			LPHELPINFO lphi = (LPHELPINFO)lParam;
			if (hi!=NULL_HELP_INFO)
			{
				xvt_help_display_topic(hi, HID_KAPITEL_5_2_1_1);
			}
		}
		break;

	default:
		break;
	}
	return CallWindowProc(defWndProc, hwnd, uMsg, wParam, lParam);
}
/**************************************/

/*	Handler for dialog DLG_160 ("Wehrparameter")-------  LWA  ---------  */
long XVT_CALLCONV1
#if XVT_CC_PROTO
DLG_160_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
DLG_160_eh XVT_CALLCONV2 (xdWindow, xdEvent)
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
		 SetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC, (LONG)&Dlg160WindowProc);
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
         win_dlg_160=xdWindow;
		 wehrdaten.typ[0]='\0';
		 wehrdaten.wehrtyp[0]='\0';
		 wehrdaten.kote[0]='\0';
		 wehrdaten.breite[0]='\0';
		 wehrdaten.beiwert[0]='\0';
		 wehrdaten.hoehe[0]='\0';
		 wehrdaten.ausuferungshoehe[0]='\0';

		 edit160[0] = xvt_win_get_ctl(xdWindow,DLG_160_EDIT_16);
		 edit160[1] = xvt_win_get_ctl(xdWindow,DLG_160_EDIT_17);
		 edit160[2] = xvt_win_get_ctl(xdWindow,DLG_160_EDIT_20);
		 edit160[3] = xvt_win_get_ctl(xdWindow,DLG_160_EDIT_21);
		 edit160[4] = xvt_win_get_ctl(xdWindow,DLG_160_EDIT_23);

		 listbutton160=xvt_win_get_ctl(xdWindow,DLG_160_LISTBUTTON_2);

		 xvt_list_add(listbutton160,0,"STREICHWEHR");
		 xvt_list_add(listbutton160,1,"BREITKRONIG");
		 xvt_list_add(listbutton160,2,"DACHFÖRMIG");
		 xvt_list_add(listbutton160,3,"RUNDKRONIG");
		 xvt_list_add(listbutton160,4,"SCHARFKANTIG");

		 list->GetInfoline2(scr.datensatz,&wehrdaten);   //Zeile2 Infoblock holen
		 xvt_list_set_sel(listbutton160,(int)wehrdaten.wehrtyp[0],TRUE);

		 /* default Werte */
		 if (strlen(wehrdaten.kote)==0) strcpy(wehrdaten.kote,"0");
		 if (strlen(wehrdaten.breite)==0) strcpy(wehrdaten.breite,"0");
		 if (strlen(wehrdaten.beiwert)==0) strcpy(wehrdaten.beiwert,"0.5");//Dick 26.08.99 default auf 0.5
		 if (strlen(wehrdaten.hoehe)==0) strcpy(wehrdaten.hoehe,"0");
		 if (strlen(wehrdaten.ausuferungshoehe)==0) strcpy(wehrdaten.ausuferungshoehe,"0");

         list->SaveInfoline2(scr.datensatz,&wehrdaten);   //Zeile2 Infoblock sichern//Dick 30.09.99 default sichern

		 xvt_vobj_set_title(edit160[0],wehrdaten.kote);
		 xvt_vobj_set_title(edit160[1],wehrdaten.breite);
		 xvt_vobj_set_title(edit160[2],wehrdaten.beiwert);
		 xvt_vobj_set_title(edit160[3],wehrdaten.hoehe);
		 xvt_vobj_set_title(edit160[4],wehrdaten.ausuferungshoehe);
		 if (hi!=NULL_HELP_INFO)
			xvt_help_set_win_assoc(hi, xdWindow, HID_KAPITEL_5_2_1_1, 0L);
		}
		break;
	case E_DESTROY:
		{
		 dlg_sonderprofil = NULL_WIN;
         win_dlg_160=NULL_WIN;
		}
		break;
	case E_CLOSE:
		{
		xvt_vobj_destroy(xdWindow);
		}
		break;
	case E_CONTROL:
		{

		switch(xdControlId) {
		case DLG_160_LISTBUTTON_2: /* "List Button 1" */
			{
				  sichere_datenblock=TRUE;
			}
			break;
		case DLG_160_PUSHBUTTON_12: /* "OK" */
			{
			 int fehler,
				  ret=0;
			 xvt_vobj_get_title(listbutton160,wehrdaten.typ,15);

			 xvt_vobj_get_title(edit160[0],wehrdaten.kote,15);
			 fehler = is_zahl(wehrdaten.kote);
			 if (fehler<1 ) ret=1;

			 xvt_vobj_get_title(edit160[1],wehrdaten.breite,15);
			 fehler = is_zahl(wehrdaten.breite);
			 if (fehler<1 ) ret=1;

			 xvt_vobj_get_title(edit160[2],wehrdaten.beiwert,15);
			 fehler = is_zahl(wehrdaten.beiwert);
			 if (fehler<1 ) ret=1;

			 xvt_vobj_get_title(edit160[3],wehrdaten.hoehe,15);
			 fehler = is_zahl(wehrdaten.hoehe);

			 xvt_vobj_get_title(edit160[4],wehrdaten.ausuferungshoehe,15);
			 fehler = is_zahl(wehrdaten.ausuferungshoehe);
			 if (fehler<1 ) ret=1;

			 if (!ret)
			 {
			  list->SaveInfoline2(scr.datensatz,&wehrdaten);   //Zeile2 Infoblock sichern
			  sichere_datenblock=FALSE;
			  SaveProfilFile=TRUE;
			  xvt_vobj_destroy(xdWindow);
			 }
			 else 
                 {
                  char buf[200];//Dick 26.11.99
                   xvt_res_get_str(STR_ERROR_REALFORMAT,buf,sizeof(buf));
                   xvt_dm_post_error("%s",buf);
                  //xvt_dm_post_note("Fehler im Zahlenformat");
                 }
			}
			break;
		case DLG_160_PUSHBUTTON_13: /* "Abbruch" */
			{
			 char buf[200],buf2[200],buf3[200],buf4[200];
             xvt_res_get_str(STR_JA,buf,sizeof(buf));
             xvt_res_get_str(STR_NEIN,buf2,sizeof(buf2));
             xvt_res_get_str(STR_ABBRECHEN,buf3,sizeof(buf3));
             xvt_res_get_str(STR_SICHERE_DATENBLOCK,buf4,sizeof(buf4));
             if (sichere_datenblock)
                 switch (xvt_dm_post_ask(buf,buf2,buf3,"%s",buf4))
				//switch (xvt_dm_post_ask("Ja","Nein","Abbrechen","Wehrdaten wurden geändert !\nSpeichern vor Schließen\ndes Fensters ?"))
				 {
				  case RESP_DEFAULT:       //SICHERN
						 {
						  xvt_vobj_get_title(listbutton160,wehrdaten.typ,15);
						  xvt_vobj_get_title(edit160[0],wehrdaten.kote,15);
						  xvt_vobj_get_title(edit160[1],wehrdaten.breite,15);
						  xvt_vobj_get_title(edit160[2],wehrdaten.beiwert,15);
						  xvt_vobj_get_title(edit160[3],wehrdaten.hoehe,15);
						  xvt_vobj_get_title(edit160[4],wehrdaten.ausuferungshoehe,15);
						  list->SaveInfoline2(scr.datensatz,&wehrdaten);   //Zeile2 Infoblock sichern
						  sichere_datenblock=FALSE;
						  SaveProfilFile=TRUE;
						 }
					break;
				  case RESP_2:        // nicht sichere_datenblock
					break;             //zurück
				  case RESP_3:break;
				  break;
				 };
			 xvt_vobj_destroy(xdWindow);
			}
			break;
		case DLG_160_EDIT_16:			{
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
				}
			} else {
				/*
					Contents of control were changed
				*/
				  sichere_datenblock=TRUE;
			}
			}
			break;
		case DLG_160_EDIT_17:			{
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
				}
			} else {
				/*
					Contents of control were changed
				*/
				  sichere_datenblock=TRUE;
			}
			}
			break;
		case DLG_160_EDIT_20:			{
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
				}
			} else {
				/*
					Contents of control were changed
				*/
				  sichere_datenblock=TRUE;
			}
			}
			break;
		case DLG_160_EDIT_21:			{
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
				}
			} else {
				/*
					Contents of control were changed
				*/
				  sichere_datenblock=TRUE;
			}
			}
			break;
		case DLG_160_EDIT_23:			{
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
				}
			} else {
				/*
					Contents of control were changed
				*/
				  sichere_datenblock=TRUE;
			}
			}
			break;
            case DLG_160_PUSHBUTTON_DB: /* "Datenbank" */
            {
            WINDOW parent_win;
            HWND db_dlg=NULL;
            if (WIN_116!=NULL_WIN)
                parent_win=WIN_116;
            else if (WIN120!=NULL_WIN)
                parent_win=WIN120;
            db_dlg=DoDatabankDlg((HWND)xvt_vobj_get_attr(parent_win,ATTR_NATIVE_WINDOW), start_dir,ueberfallbeiwert);
            }
            break;


}

		}
		break;
	default:
		break;
	}
	return 0L;
}
