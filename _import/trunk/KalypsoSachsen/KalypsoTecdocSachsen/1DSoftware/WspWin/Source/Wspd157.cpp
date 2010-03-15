/****************************************************************************
*             Dialog 156 : Anzeige ARMCO71                                  *
*             19.08.1995                                                    *
****************************************************************************/
#include <windows.h>
#include "xvt.h"
#include "wspwin.h"
#include "strang.h"
#include "resource.h"

#include "global_types.h"
#include "..\..\wspdlg\include\export.h"

#include "list.h"
#include "paint.h"
#include "bce_allg.h"

#include "typen.h"

#include "wsphilfe.h"

extern XVT_HELP_INFO hi;

#define DLG_RES_ID DLG_157
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODELESS


extern Scroller scr;
extern int *typ;
extern BOOLEAN sichere_datenblock,
					SaveProfilFile;
extern WINDOW dlg_sonderprofil,
				  WIN_117,
				  *ptr_win;
extern List *list;
extern Paint *paint;
extern MinMax pmm;
//extern int fehler;
WINDOW edit_dlg157[10];

/*************   GHJ   *************/
extern WINDOW WIN_116, WIN120;
static WNDPROC defWndProc;
LRESULT CALLBACK Dlg157WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg)
	{
	case WM_HELP:
		{
			LPHELPINFO lphi = (LPHELPINFO)lParam;
			if (hi!=NULL_HELP_INFO)
			{
				xvt_help_display_topic(hi, HID_KAPITEL_5_3_2_5);
			}
		}
		break;

	default:
		break;
	}
	return CallWindowProc(defWndProc, hwnd, uMsg, wParam, lParam);
}
/**************************************/


/*	Handler for dialog DLG_157 ("ARMCO  71  -  Profil")*/
long XVT_CALLCONV1
#if XVT_CC_PROTO
DLG_157_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
DLG_157_eh XVT_CALLCONV2 (xdWindow, xdEvent)
WINDOW xdWindow;
EVENT *xdEvent;
#endif
{
	short xdControlId = xdEvent->v.ctl.id;
   char daten157[15];
   int fehler;

	switch (xdEvent->type) {
	case E_CREATE:
		{
         /*************   GHJ   *************/
		 if (WIN_116!=NULL_WIN)
			SetParent((HWND)xvt_vobj_get_attr(xdWindow, ATTR_NATIVE_WINDOW), (HWND)xvt_vobj_get_attr(WIN_116, ATTR_NATIVE_WINDOW));
		 else if (WIN120!=NULL_WIN)
			SetParent((HWND)xvt_vobj_get_attr(xdWindow, ATTR_NATIVE_WINDOW), (HWND)xvt_vobj_get_attr(WIN120, ATTR_NATIVE_WINDOW));
		 defWndProc = (WNDPROC)GetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC);
		 SetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC, (LONG)&Dlg157WindowProc);
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
		 for (int i=0;i<10;i++)
			  {
				 edit_dlg157[i]=xvt_win_get_ctl(xdWindow,DLG_157_EDIT_1+i);
				 if (scr.z[i] == BCE_NAN)
					 {
					  str[0]='0';
					  str[1]='\0';
					 }
				 else
				  sprintf(str,"%.4lf",scr.z[i]);//gcvt(scr.z[i],10,str);
				 xvt_vobj_set_title(edit_dlg157[i],str);
			  }
		 ptr_win = edit_dlg157;
		 if (hi!=NULL_HELP_INFO)
			xvt_help_set_win_assoc(hi, xdWindow, HID_KAPITEL_5_3_2_5, 0L);
		}
		break;
	case E_DESTROY:
		{
		*ptr_win = NULL_WIN;
		dlg_sonderprofil = NULL_WIN;
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
		case DLG_157_PUSHBUTTON_OK: /* "OK" */
			{
			 int i;
			 BOOLEAN falsch;
			 falsch=FALSE;
			 for (i=0;i<10;i++)
			  {
				xvt_vobj_get_title(edit_dlg157[i],daten157,10);
				fehler = is_zahl(daten157);
				if ((fehler ==0) || (daten157[0]=='\0'))
				 {
				  falsch=TRUE;
				  xvt_vobj_set_title(edit_dlg157[i],"0\0");
				 }
				else
				 scr.z[i]= atof(daten157);
				}
			 if(falsch)
                 {
                  char buf[200];//Dick 26.11.99
                  xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                  xvt_dm_post_error("%s",buf);
			      //xvt_dm_post_note("Fehler in der Eingabe");
                 }
			 else
			  {
				list->SaveSonderprofildaten(&scr, ARMCO71);
				SaveProfilFile = TRUE;
				xvt_vobj_destroy(xdWindow);
			  }
			}
			break;
		case DLG_157_PUSHBUTTON_QUIT: /* "Abbrechen" */
			{
			 xvt_vobj_destroy(xdWindow);
			}
			break;
		case DLG_157_EDIT_1:
		  {	/*	Edit control was operated.	*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change)
			 {
				if (xdEvent->v.ctl.ci.v.edit.active)
				{	/*	focus has entered the control	*/

				}
				else
				{	/*	focus has left the control		*/
				 xvt_vobj_get_title(edit_dlg157[0],daten157,10);
				 fehler = is_zahl(daten157);
				 if (fehler ==0)
				  {
                   char buf[200];//Dick 26.11.99
                   xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                   xvt_dm_post_error("%s",buf);
					//xvt_dm_post_error("Fehler in der Eingabe !");
					xvt_vobj_set_title(edit_dlg157[0],"\0");
				  }
				 else
					scr.z[0]= atof(daten157);
				}
			 }
			else
			 {	/*	Contents of control were changed	*/

			 }
		  }
		  break;
		case DLG_157_EDIT_2:
		  {	/*	Edit control was operated.	*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change)
			 {
				if (xdEvent->v.ctl.ci.v.edit.active)
				{	/*	focus has entered the control	*/

				}
				else
				{	/*	focus has left the control		*/
				 xvt_vobj_get_title(edit_dlg157[1],daten157,10);
				 fehler = is_zahl(daten157);
				 if (fehler ==0)
				  {
                   char buf[200];//Dick 26.11.99
                   xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                   xvt_dm_post_error("%s",buf);
					//xvt_dm_post_error("Fehler in der Eingabe !");
					xvt_vobj_set_title(edit_dlg157[1],"\0");
				  }
				 else
					scr.z[1]= atof(daten157);
				}
			 }
			else
			 {	/*	Contents of control were changed	*/

			 }
		  }
		  break;
		case DLG_157_EDIT_3:
		  {	/*	Edit control was operated.	*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change)
			 {
				if (xdEvent->v.ctl.ci.v.edit.active)
				{	/*	focus has entered the control	*/

				}
				else
				{	/*	focus has left the control		*/
				 xvt_vobj_get_title(edit_dlg157[2],daten157,10);
				 fehler = is_zahl(daten157);
				 if (fehler ==0)
				  {
                   char buf[200];//Dick 26.11.99
                   xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                   xvt_dm_post_error("%s",buf);
					//xvt_dm_post_error("Fehler in der Eingabe !");
					xvt_vobj_set_title(edit_dlg157[2],"\0");
				  }
				 else
					scr.z[2]= atof(daten157);
				}
			 }
			else
			 {	/*	Contents of control were changed	*/

			 }
		  }
		  break;
		case DLG_157_EDIT_4:
		  {	/*	Edit control was operated.	*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change)
			 {
				if (xdEvent->v.ctl.ci.v.edit.active)
				{	/*	focus has entered the control	*/

				}
				else
				{	/*	focus has left the control		*/
				 xvt_vobj_get_title(edit_dlg157[3],daten157,10);
				 fehler = is_zahl(daten157);
				 if (fehler ==0)
				  {
                   char buf[200];//Dick 26.11.99
                   xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                   xvt_dm_post_error("%s",buf);
					//xvt_dm_post_error("Fehler in der Eingabe !");
					xvt_vobj_set_title(edit_dlg157[3],"\0");
				  }
				 else
					scr.z[3]= atof(daten157);
				}
			 }
			else
			 {	/*	Contents of control were changed	*/

			 }
		  }
		  break;
		case DLG_157_EDIT_5:
		  {	/*	Edit control was operated.	*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change)
			 {
				if (xdEvent->v.ctl.ci.v.edit.active)
				{	/*	focus has entered the control	*/

				}
				else
				{	/*	focus has left the control		*/
				 xvt_vobj_get_title(edit_dlg157[4],daten157,10);
				 fehler = is_zahl(daten157);
				 if (fehler ==0)
				  {
                   char buf[200];//Dick 26.11.99
                   xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                   xvt_dm_post_error("%s",buf);
					//xvt_dm_post_error("Fehler in der Eingabe !");
					xvt_vobj_set_title(edit_dlg157[4],"\0");
				  }
				 else
					scr.z[4]= atof(daten157);
				}
			 }
			else
			 {	/*	Contents of control were changed	*/

			 }
		  }
		  break;
		case DLG_157_EDIT_RCO:
		  {	/*	Edit control was operated.	*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change)
			 {
				if (xdEvent->v.ctl.ci.v.edit.active)
				{	/*	focus has entered the control	*/

				}
				else
				{	/*	focus has left the control		*/
				 xvt_vobj_get_title(edit_dlg157[5],daten157,10);
				 fehler = is_zahl(daten157);
				 if (fehler ==0)
				  {
                   char buf[200];//Dick 26.11.99
                   xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                   xvt_dm_post_error("%s",buf);
					//xvt_dm_post_error("Fehler in der Eingabe !");
					xvt_vobj_set_title(edit_dlg157[5],"\0");
				  }
				 else
					scr.z[5]= atof(daten157);
				}
			 }
			else
			 {	/*	Contents of control were changed	*/

			 }
		  }
		  break;
		case DLG_157_EDIT_R1CO:
		  {	/*	Edit control was operated.	*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change)
			 {
				if (xdEvent->v.ctl.ci.v.edit.active)
				{	/*	focus has entered the control	*/

				}
				else
				{	/*	focus has left the control		*/
				 xvt_vobj_get_title(edit_dlg157[6],daten157,10);
				 fehler = is_zahl(daten157);
				 if (fehler ==0)
				  {
                   char buf[200];//Dick 26.11.99
                   xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                   xvt_dm_post_error("%s",buf);
					//xvt_dm_post_error("Fehler in der Eingabe !");
					xvt_vobj_set_title(edit_dlg157[6],"\0");
				  }
				 else
					scr.z[6]= atof(daten157);
				}
			 }
			else
			 {	/*	Contents of control were changed	*/

			 }
		  }
		  break;
		case DLG_157_EDIT_R2CO:
		  {	/*	Edit control was operated.	*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change)
			 {
				if (xdEvent->v.ctl.ci.v.edit.active)
				{	/*	focus has entered the control	*/

				}
				else
				{	/*	focus has left the control		*/
				 xvt_vobj_get_title(edit_dlg157[7],daten157,10);
				 fehler = is_zahl(daten157);
				 if (fehler ==0)
				  {
                   char buf[200];//Dick 26.11.99
                   xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                   xvt_dm_post_error("%s",buf);
					//xvt_dm_post_error("Fehler in der Eingabe !");
					xvt_vobj_set_title(edit_dlg157[7],"\0");
				  }
				 else
					scr.z[7]= atof(daten157);
				}
			 }
			else
			 {	/*	Contents of control were changed	*/

			 }
		  }
		  break;
		case DLG_157_EDIT_BCO:
		  {	/*	Edit control was operated.	*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change)
			 {
				if (xdEvent->v.ctl.ci.v.edit.active)
				{	/*	focus has entered the control	*/

				}
				else
				{	/*	focus has left the control		*/
				 xvt_vobj_get_title(edit_dlg157[8],daten157,10);
				 fehler = is_zahl(daten157);
				 if (fehler ==0)
				  {
                   char buf[200];//Dick 26.11.99
                   xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                   xvt_dm_post_error("%s",buf);
					//xvt_dm_post_error("Fehler in der Eingabe !");
					xvt_vobj_set_title(edit_dlg157[8],"\0");
				  }
				 else
					scr.z[8]= atof(daten157);
				}
			 }
			else
			 {	/*	Contents of control were changed	*/

			 }
		  }
		  break;
		case DLG_157_EDIT_FCRED:
		  {	/*	Edit control was operated.	*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change)
			 {
				if (xdEvent->v.ctl.ci.v.edit.active)
				{	/*	focus has entered the control	*/

				}
				else
				{	/*	focus has left the control		*/
				 xvt_vobj_get_title(edit_dlg157[9],daten157,10);
				 fehler = is_zahl(daten157);
				 if (fehler ==0)
				  {
                   char buf[200];//Dick 26.11.99
                   xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                   xvt_dm_post_error("%s",buf);
					//xvt_dm_post_error("Fehler in der Eingabe !");
					xvt_vobj_set_title(edit_dlg157[9],"\0");
				  }
				 else
					scr.z[9]= atof(daten157);
				}
			 }
			else
			 {	/*	Contents of control were changed	*/

			 }
		  }
		  break;
        case DLG_157_DB:
            {
             
             xvt_dlg_create_res(WD_MODAL, DLG_ARMCO, EM_ALL, DLG_ARMCO_eh,9L);
			 	 
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
