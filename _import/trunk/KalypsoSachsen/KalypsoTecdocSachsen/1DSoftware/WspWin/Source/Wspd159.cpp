#include <windows.h>
#include "xvt.h"
#include "wspwin.h"
#include "resource.h"

#include "global_types.h"
#include "..\..\wspdlg\include\export.h"

#include "list.h"

#include "typen.h"
#include "bce_allg.h"

#include "global.h"//Dick 8.12.98

#define DLG_RES_ID DLG_159
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODELESS

extern Scroller scr;
extern int typ[TYPE_SIZE];//Dick 8.12.98
extern BOOLEAN SaveProfilFile;
extern WINDOW dlg_sonderprofil;
extern WINDOW *ptr_win;
extern List *list;
extern MinMax pmm;
//extern int fehler;
//char daten159[20];
WINDOW edit_dlg159[6];

/*************   GHJ   *************/
#include "wsphilfe.h"
extern XVT_HELP_INFO hi;
static WNDPROC defWndProc;
LRESULT CALLBACK Dlg159WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg)
	{
	case WM_HELP:
		{
			LPHELPINFO lphi = (LPHELPINFO)lParam;
			if (hi!=NULL_HELP_INFO)
			{
//				xvt_help_display_topic(hi, HID_KAPITEL_5_1_1_1);
			}
		}
		break;

	default:
		break;
	}
	return CallWindowProc(defWndProc, hwnd, uMsg, wParam, lParam);
}
/**************************************/


/*	Handler for dialog DLG_159 ("Gauss-Krüger-Koordinaten")*/
long XVT_CALLCONV1
#if XVT_CC_PROTO
DLG_159_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
DLG_159_eh XVT_CALLCONV2 (xdWindow, xdEvent)
WINDOW xdWindow;
EVENT *xdEvent;
#endif
{
	short xdControlId = xdEvent->v.ctl.id;
	int fehler;
	char daten159[20];

	switch (xdEvent->type) {
	case E_CREATE:
		{
         /*************   GHJ   *************/
		 if (WIN_116!=NULL_WIN)
			SetParent((HWND)xvt_vobj_get_attr(xdWindow, ATTR_NATIVE_WINDOW), (HWND)xvt_vobj_get_attr(WIN_116, ATTR_NATIVE_WINDOW));
		 else if (WIN120!=NULL_WIN)
			SetParent((HWND)xvt_vobj_get_attr(xdWindow, ATTR_NATIVE_WINDOW), (HWND)xvt_vobj_get_attr(WIN120, ATTR_NATIVE_WINDOW));
		 defWndProc = (WNDPROC)GetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC);
		 SetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC, (LONG)&Dlg159WindowProc);
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
		 dlg_sonderprofil  = xdWindow;
		 edit_dlg159[0]=xvt_win_get_ctl(xdWindow,DLG_159_EDIT_1);   //Res-Id:14
		 edit_dlg159[1]=xvt_win_get_ctl(xdWindow,DLG_159_EDIT_2);   //Res-Id:12
		 edit_dlg159[2]=xvt_win_get_ctl(xdWindow,DLG_159_EDIT_3);   //Res-Id:13
		 edit_dlg159[3]=xvt_win_get_ctl(xdWindow,DLG_159_EDIT_4);   //Res-Id:17
		 edit_dlg159[4]=xvt_win_get_ctl(xdWindow,DLG_159_EDIT_5);   //Res-Id:15
		 edit_dlg159[5]=xvt_win_get_ctl(xdWindow,DLG_159_EDIT_6);   //Res-Id:16

		 if(scr.z[0]==BCE_NAN)
		  {
			sprintf(str,"%.4lf",pmm.minX);//gcvt(pmm.minX,6,str);
			scr.z[0]=pmm.minX;
		  }
		 if(scr.z[3]==BCE_NAN)
		  {
			sprintf(str,"%.4lf",pmm.maxX);//gcvt(pmm.maxX,6,str);
			scr.z[3]=pmm.maxX;
		  }
		 
		 for (int i=0;i<6;i++)
			  {
				 if (scr.z[i] == BCE_NAN)
					 str[0]='\0';
				 else  sprintf(str,"%.4lf",scr.z[i]);//gcvt(scr.z[i],10,str);
				 xvt_vobj_set_title(edit_dlg159[i],str);
			  }
		 ptr_win = edit_dlg159;

		}
		break;
	case E_DESTROY:
		{
		*ptr_win = NULL_WIN;
		dlg_sonderprofil = NULL_WIN;
		}
		break;
	case E_CLOSE:
		/*
			Request to close dialog; user operated "close" menu item on
			dialog system menu, or operated "close" control on dialog
			frame. Dialog not closed unless xvt_vobj_destroy is called.
		*/
		{
		xvt_vobj_destroy(xdWindow);
		}
		break;
	case E_CONTROL:
		/*
			User operated control in dialog.
		*/
		{

		switch(xdControlId) {
		case DLG_159_PUSHBUTTON_OK: /* "OK" */
			{
			 list->SaveSonderprofildaten(&scr,GAUSS);
			 SaveProfilFile = TRUE;
			 xvt_vobj_destroy(xdWindow);
			}
			break;
		case DLG_159_PUSHBUTTON_QUIT: /* "Abbrechen" */
			{
			 xvt_vobj_destroy(xdWindow);
			}
			break;
		case DLG_159_EDIT_1:
		{/*Edit control was operated.*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change)
			{
			 if (xdEvent->v.ctl.ci.v.edit.active)
				{ 	/*	focus has entered the control	*/
				}
			 else
				{/*	focus has left the control		*/
				 xvt_vobj_get_title(edit_dlg159[0],daten159,15);
				 fehler = is_zahl(daten159);
				 if (fehler ==0)
				  {
                   char buf[200];//Dick 26.11.99
                   xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                   xvt_dm_post_error("%s",buf);
					//xvt_dm_post_error("Fehler in der Eingabe !");
					xvt_vobj_set_title(edit_dlg159[0],"\0");
				  }
				 else
				  {
					scr.z[0]= atof(daten159);
					xvt_vobj_set_title(edit_dlg159[0],daten159);
				  }
				}
			}
			else
			  {/*	Contents of control were changed	*/
			  }
			}
			break;
		case DLG_159_EDIT_2:
		{/*Edit control was operated.*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change)
			{
			 if (xdEvent->v.ctl.ci.v.edit.active)
				{ 	/*	focus has entered the control	*/
				}
			 else
				{/*	focus has left the control		*/
				 xvt_vobj_get_title(edit_dlg159[1],daten159,15);
				 fehler = is_zahl(daten159);
				 if (fehler ==0)
				  {
                   char buf[200];//Dick 26.11.99
                   xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                   xvt_dm_post_error("%s",buf);
					//xvt_dm_post_error("Fehler in der Eingabe !");
					xvt_vobj_set_title(edit_dlg159[1],"\0");
				  }
				 else
				  {
					scr.z[1]= atof(daten159);
					xvt_vobj_set_title(edit_dlg159[1],daten159);
				  }
				}
			}
			else
			  {/*	Contents of control were changed	*/
			  }
			}
			break;
		case DLG_159_EDIT_3:
		{/*Edit control was operated.*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change)
			{
			 if (xdEvent->v.ctl.ci.v.edit.active)
				{ 	/*	focus has entered the control	*/
				}
			 else
				{/*	focus has left the control		*/
				 xvt_vobj_get_title(edit_dlg159[2],daten159,15);
				 fehler = is_zahl(daten159);
				 if (fehler ==0)
				  {
                   char buf[200];//Dick 26.11.99
                   xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                   xvt_dm_post_error("%s",buf);
					//xvt_dm_post_error("Fehler in der Eingabe !");
					xvt_vobj_set_title(edit_dlg159[2],"\0");
				  }
				 else
				  {
					scr.z[2]= atof(daten159);
					xvt_vobj_set_title(edit_dlg159[2],daten159);
				  }
				}
			}
			else
			  {/*	Contents of control were changed	*/
			  }
			}
			break;
		case DLG_159_EDIT_4:
		{/*Edit control was operated.*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change)
			{
			 if (xdEvent->v.ctl.ci.v.edit.active)
				{ 	/*	focus has entered the control	*/
				}
			 else
				{/*	focus has left the control		*/
				 xvt_vobj_get_title(edit_dlg159[3],daten159,15);
				 fehler = is_zahl(daten159);
				 if (fehler ==0)
				  {
                   char buf[200];//Dick 26.11.99
                   xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                   xvt_dm_post_error("%s",buf);
					//xvt_dm_post_error("Fehler in der Eingabe !");
					xvt_vobj_set_title(edit_dlg159[3],"\0");
				  }
				 else
				  {
					scr.z[3]= atof(daten159);
					xvt_vobj_set_title(edit_dlg159[3],daten159);
				  }
				}
			}
			else
			  {/*	Contents of control were changed	*/
			  }
			}
			break;
		case DLG_159_EDIT_5:
		{/*Edit control was operated.*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change)
			{
			 if (xdEvent->v.ctl.ci.v.edit.active)
				{ 	/*	focus has entered the control	*/
				}
			 else
				{/*	focus has left the control		*/
				 xvt_vobj_get_title(edit_dlg159[4],daten159,15);
				 fehler = is_zahl(daten159);
				 if (fehler ==0)
				  {
                   char buf[200];//Dick 26.11.99
                   xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                   xvt_dm_post_error("%s",buf);
					//xvt_dm_post_error("Fehler in der Eingabe !");
					xvt_vobj_set_title(edit_dlg159[4],"\0");
				  }
				 else
				  {
					scr.z[4]= atof(daten159);
					xvt_vobj_set_title(edit_dlg159[4],daten159);
				  }
				}
			}
			else
			  {/*	Contents of control were changed	*/
			  }
			}
			break;
		case DLG_159_EDIT_6:
		{/*Edit control was operated.*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change)
			{
			 if (xdEvent->v.ctl.ci.v.edit.active)
				{ 	/*	focus has entered the control	*/
				}
			 else
				{/*	focus has left the control		*/
				 xvt_vobj_get_title(edit_dlg159[5],daten159,15);
				 fehler = is_zahl(daten159);
				 if (fehler ==0)
				  {
                   char buf[200];//Dick 26.11.99
                   xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                   xvt_dm_post_error("%s",buf);
					//xvt_dm_post_error("Fehler in der Eingabe !");
					xvt_vobj_set_title(edit_dlg159[5],"\0");
				  }
				 else
				  {
					scr.z[5]= atof(daten159);
					xvt_vobj_set_title(edit_dlg159[5],daten159);
				  }
				}
			}
			else
			  {/*	Contents of control were changed	*/
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
