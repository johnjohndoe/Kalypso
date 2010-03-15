/****************************************************************************
*             Dialog 156 : Anzeige ARMCO84                                  *
*             19.08.1995                                                    *
****************************************************************************/
#include <windows.h>
#include "xvt.h"
#include "wspwin.h"
#include "resource.h"

#include "global_types.h"
#include "..\..\wspdlg\include\export.h"

#include "list.h"
#include "paint.h"

#include "bce_allg.h"
#include "typen.h"

#include "wsphilfe.h"

extern XVT_HELP_INFO hi;

#define DLG_RES_ID DLG_156
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
WINDOW edit_dlg156[11],
		 list_edit_156;

//char daten156[15];
//char dlg156_profiltyp[5];
//extern int fehler;

/*************   GHJ   *************/
extern WINDOW WIN_116, WIN120;
static WNDPROC defWndProc;
LRESULT CALLBACK Dlg156WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg)
	{
	case WM_HELP:
		{
			LPHELPINFO lphi = (LPHELPINFO)lParam;
			if (hi!=NULL_HELP_INFO)
			{
				xvt_help_display_topic(hi, HID_KAPITEL_5_3_2_7);
			}
		}
		break;

	default:
		break;
	}
	return CallWindowProc(defWndProc, hwnd, uMsg, wParam, lParam);
}
/**************************************/


/*	Handler for dialog DLG_156 ("ARMCO 84  -  Profil")*/
long XVT_CALLCONV1
#if XVT_CC_PROTO
DLG_156_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
DLG_156_eh XVT_CALLCONV2 (xdWindow, xdEvent)
WINDOW xdWindow;
EVENT *xdEvent;
#endif
{
	short xdControlId = xdEvent->v.ctl.id;
	int fehler;
	char daten156[15];
	char dlg156_profiltyp[5];
	switch (xdEvent->type) {
	case E_CREATE:
		{
         /*************   GHJ   *************/
		 if (WIN_116!=NULL_WIN)
			SetParent((HWND)xvt_vobj_get_attr(xdWindow, ATTR_NATIVE_WINDOW), (HWND)xvt_vobj_get_attr(WIN_116, ATTR_NATIVE_WINDOW));
		 else if (WIN120!=NULL_WIN)
			SetParent((HWND)xvt_vobj_get_attr(xdWindow, ATTR_NATIVE_WINDOW), (HWND)xvt_vobj_get_attr(WIN120, ATTR_NATIVE_WINDOW));
		 defWndProc = (WNDPROC)GetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC);
		 SetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC, (LONG)&Dlg156WindowProc);
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
		 int typ;
		 dlg_sonderprofil  = xdWindow;
		 ptr_win = edit_dlg156;

		 list_edit_156 =xvt_win_get_ctl(xdWindow,DLG_156_LISTEDIT);
		 xvt_list_add(list_edit_156,-1,"MA");
		 xvt_list_add(list_edit_156,-1,"MB");
		 xvt_list_add(list_edit_156,-1,"WA");
		 xvt_list_add(list_edit_156,-1,"WB");
		 xvt_list_add(list_edit_156,-1,"EA");
		 xvt_list_add(list_edit_156,-1,"EB");
		 xvt_list_add(list_edit_156,-1,"SE");
		 xvt_list_add(list_edit_156,-1,"SB");

		 typ = list->Infoline2(scr.datensatz,&dlg156_profiltyp[0],FALSE);
		 if ((typ<=0)||(typ>8))	  typ = 1;
		 xvt_list_set_sel(list_edit_156,typ-1,TRUE);

		 for (int i=0;i<11;i++)
			 edit_dlg156[i]=xvt_win_get_ctl(xdWindow,DLG_156_EDIT_1+i);

		 for (i=0;i<5;i++)
			{
			 if (scr.z[i] == BCE_NAN)
				 {	 str[0]='0'; str[1]='\0';  }
			 else 
				// gcvt(scr.z[i],10,str);
			  sprintf(str,"%.4lf",scr.z[i]);
				xvt_vobj_set_title(edit_dlg156[i],str);
			}
		 for (i=6;i<=11;i++)
			{
			 if (scr.z[i] == BCE_NAN)
				 {	 str[0]='0'; str[1]='\0';  }
			 else  
			//	 gcvt(scr.z[i],10,str);
			 sprintf(str,"%.4lf",scr.z[i]);
			 xvt_vobj_set_title(edit_dlg156[i-1],str);
			}
		 if (hi!=NULL_HELP_INFO)
			xvt_help_set_win_assoc(hi, xdWindow, HID_KAPITEL_5_3_2_7, 0L);
		}
		break;
	case E_DESTROY:
//		xdRemoveHelpAssoc( xdWindow );
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
		{

		switch(xdControlId) {
		case DLG_156_PUSHBUTTON_OK: /* "OK" */
			{
		 //	 if (strlen(dlg156_profiltyp)>0)
				{
				 int typ = xvt_list_get_sel_index(list_edit_156);

				 switch (typ+1)
					{
					 case 1:
					  strcpy(dlg156_profiltyp,"MA");	  break;
					 case 2:
					  strcpy(dlg156_profiltyp,"MB");	  break;
					 case 3:
					  strcpy(dlg156_profiltyp,"WA");	  break;
					 case 4:
					  strcpy(dlg156_profiltyp,"WB");	  break;
					 case 5:
					  strcpy(dlg156_profiltyp,"EA");	  break;
					 case 6:
					  strcpy(dlg156_profiltyp,"EB");	  break;
					 case 7:
					  strcpy(dlg156_profiltyp,"SE");	  break;
					 case 8:
					  strcpy(dlg156_profiltyp,"SB");	  break;
					 default:
					  strcpy(dlg156_profiltyp,"MA");	  break;
					}

				 BOOLEAN falsch=FALSE;
				 int i;
				 for(i=0;i<=10;i++)
				  {
					xvt_vobj_get_title(edit_dlg156[i],daten156,10);
					fehler = is_zahl(daten156);
					if ((fehler ==0)||(daten156[0]=='\0'))
					 {
					  falsch=TRUE;
					  xvt_vobj_set_title(edit_dlg156[i],"0\0");
					 }
					else
					 {
					  if(i<=4)
						scr.z[i]= atof(daten156);
					  else
						scr.z[i+1]=atof(daten156);
					 }
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
				 list->Infoline2(scr.datensatz,&dlg156_profiltyp[0],TRUE);
				 list->SaveSonderprofildaten(&scr, ARMCO84);
				 SaveProfilFile = TRUE;
				 xvt_vobj_destroy(xdWindow);
              }
				}
	  //		 else
				{
	  //			 xvt_dm_post_note("Bitte Profiltyp auswählen !");
				}
			}
			break;
		case DLG_156_PUSHBUTTON_QUIT: /* "Abbrechen" */
			{
			 sichere_datenblock=FALSE;
			 xvt_vobj_destroy(xdWindow);
			}
			break;
		case DLG_156_EDIT_1:
		  {	/*	Edit control was operated.	*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change)
			 {
				if (xdEvent->v.ctl.ci.v.edit.active)
				{	/*	focus has entered the control	*/

				}
				else
				{	/*	focus has left the control		*/
				 xvt_vobj_get_title(edit_dlg156[0],daten156,10);
				 fehler = is_zahl(daten156);
				 if (fehler ==0)
				  {
                   char buf[200];//Dick 26.11.99
                   xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                   xvt_dm_post_error("%s",buf);
					//xvt_dm_post_error("Fehler in der Eingabe !");
					xvt_vobj_set_title(edit_dlg156[0],"\0");
				  }
				 else
					scr.z[0]= atof(daten156);
				}
			 }
			else
			 {	/*	Contents of control were changed	*/
			  sichere_datenblock = TRUE;
			 }
		  }
		  break;
		case DLG_156_EDIT_2:
		  {	/*	Edit control was operated.	*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change)
			 {
				if (xdEvent->v.ctl.ci.v.edit.active)
				{	/*	focus has entered the control	*/

				}
				else
				{	/*	focus has left the control		*/
				 xvt_vobj_get_title(edit_dlg156[1],daten156,10);
				 fehler = is_zahl(daten156);
				 if (fehler ==0)
				  {
                   char buf[200];//Dick 26.11.99
                   xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                   xvt_dm_post_error("%s",buf);
					//xvt_dm_post_error("Fehler in der Eingabe !");
					xvt_vobj_set_title(edit_dlg156[1],"\0");
				  }
				 else
					scr.z[1]= atof(daten156);

				}
			 }
			else
			 {	/*	Contents of control were changed	*/
			  sichere_datenblock = TRUE;
			 }
		  }
		  break;
		case DLG_156_EDIT_3:
		  {	/*	Edit control was operated.	*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change)
			 {
				if (xdEvent->v.ctl.ci.v.edit.active)
				{	/*	focus has entered the control	*/

				}
				else
				{	/*	focus has left the control		*/
				 xvt_vobj_get_title(edit_dlg156[2],daten156,10);
				 fehler = is_zahl(daten156);
				 if (fehler ==0)
				  {
                   char buf[200];//Dick 26.11.99
                   xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                   xvt_dm_post_error("%s",buf);
					//xvt_dm_post_error("Fehler in der Eingabe !");
					xvt_vobj_set_title(edit_dlg156[2],"\0");
				  }
				 else
					scr.z[2]= atof(daten156);

				}
			 }
			else
			 {	/*	Contents of control were changed	*/
			  sichere_datenblock = TRUE;
			 }
		  }
		  break;
		case DLG_156_EDIT_4:
		  {	/*	Edit control was operated.	*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change)
			 {
				if (xdEvent->v.ctl.ci.v.edit.active)
				{	/*	focus has entered the control	*/

				}
				else
				{	/*	focus has left the control		*/
				 xvt_vobj_get_title(edit_dlg156[3],daten156,10);
				 fehler = is_zahl(daten156);
				 if (fehler ==0)
				  {
                   char buf[200];//Dick 26.11.99
                   xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                   xvt_dm_post_error("%s",buf);
					//xvt_dm_post_error("Fehler in der Eingabe !");
					xvt_vobj_set_title(edit_dlg156[3],"\0");
				  }
				 else
					scr.z[3]= atof(daten156);

				}
			 }
			else
			 {	/*	Contents of control were changed	*/
			  sichere_datenblock = TRUE;
			 }
		  }
		  break;
		case DLG_156_EDIT_5:
		  {	/*	Edit control was operated.	*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change)
			 {
				if (xdEvent->v.ctl.ci.v.edit.active)
				{	/*	focus has entered the control	*/

				}
				else
				{	/*	focus has left the control		*/
				 xvt_vobj_get_title(edit_dlg156[4],daten156,10);
				 fehler = is_zahl(daten156);
				 if (fehler ==0)
				  {
                   char buf[200];//Dick 26.11.99
                   xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                   xvt_dm_post_error("%s",buf);
					//xvt_dm_post_error("Fehler in der Eingabe !");
					xvt_vobj_set_title(edit_dlg156[4],"\0");
				  }
				 else
					scr.z[4]= atof(daten156);

				}
			 }
			else
			 {	/*	Contents of control were changed	*/
			  sichere_datenblock = TRUE;
			 }
		  }
		  break;
		case DLG_156_EDIT_R1:
		  {	/*	Edit control was operated.	*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change)
			 {
				if (xdEvent->v.ctl.ci.v.edit.active)
				{	/*	focus has entered the control	*/

				}
				else
				{	/*	focus has left the control		*/
				 xvt_vobj_get_title(edit_dlg156[5],daten156,10);
				 fehler = is_zahl(daten156);
				 if (fehler ==0)
				  {
                   char buf[200];//Dick 26.11.99
                   xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                   xvt_dm_post_error("%s",buf);
					//xvt_dm_post_error("Fehler in der Eingabe !");
					xvt_vobj_set_title(edit_dlg156[5],"\0");
				  }
				 else
					scr.z[6]= atof(daten156);

				}
			 }
			else
			 {	/*	Contents of control were changed	*/
			  sichere_datenblock = TRUE;
			 }
		  }
		  break;
		case DLG_156_EDIT_R2:
		  {	/*	Edit control was operated.	*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change)
			 {
				if (xdEvent->v.ctl.ci.v.edit.active)
				{	/*	focus has entered the control	*/

				}
				else
				{	/*	focus has left the control		*/
				 xvt_vobj_get_title(edit_dlg156[6],daten156,10);
				 fehler = is_zahl(daten156);
				 if (fehler ==0)
				  {
                   char buf[200];//Dick 26.11.99
                   xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                   xvt_dm_post_error("%s",buf);
					//xvt_dm_post_error("Fehler in der Eingabe !");
					xvt_vobj_set_title(edit_dlg156[6],"\0");
				  }
				 else
					scr.z[7]= atof(daten156);

				}
			 }
			else
			 {	/*	Contents of control were changed	*/
			  sichere_datenblock = TRUE;
			 }
		  }
		  break;
		case DLG_156_EDIT_R3:
		  {	/*	Edit control was operated.	*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change)
			 {
				if (xdEvent->v.ctl.ci.v.edit.active)
				{	/*	focus has entered the control	*/

				}
				else
				{	/*	focus has left the control		*/
				 xvt_vobj_get_title(edit_dlg156[7],daten156,10);
				 fehler = is_zahl(daten156);
				 if (fehler ==0)
				  {
                   char buf[200];//Dick 26.11.99
                   xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                   xvt_dm_post_error("%s",buf);
					//xvt_dm_post_error("Fehler in der Eingabe !");
					xvt_vobj_set_title(edit_dlg156[7],"\0");
				  }
				 else
					scr.z[8]= atof(daten156);

				}
			 }
			else
			 {	/*	Contents of control were changed	*/
			  sichere_datenblock = TRUE;
			 }
		  }
		  break;
		case DLG_156_EDIT_W1:
		  {	/*	Edit control was operated.	*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change)
			 {
				if (xdEvent->v.ctl.ci.v.edit.active)
				{	/*	focus has entered the control	*/

				}
				else
				{	/*	focus has left the control		*/
				 xvt_vobj_get_title(edit_dlg156[8],daten156,10);
				 fehler = is_zahl(daten156);
				 if (fehler ==0)
				  {
                   char buf[200];//Dick 26.11.99
                   xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                   xvt_dm_post_error("%s",buf);
					//xvt_dm_post_error("Fehler in der Eingabe !");
					xvt_vobj_set_title(edit_dlg156[8],"\0");
				  }
				 else
					scr.z[9]= atof(daten156);

				}
			 }
			else
			 {	/*	Contents of control were changed	*/
			  sichere_datenblock = TRUE;
			 }
		  }
		  break;
		case DLG_156_EDIT_W2:
		  {	/*	Edit control was operated.	*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change)
			 {
				if (xdEvent->v.ctl.ci.v.edit.active)
				{	/*	focus has entered the control	*/

				}
				else
				{	/*	focus has left the control		*/
				 xvt_vobj_get_title(edit_dlg156[9],daten156,10);
				 fehler = is_zahl(daten156);
				 if (fehler ==0)
				  {
                   char buf[200];//Dick 26.11.99
                   xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                   xvt_dm_post_error("%s",buf);
					//xvt_dm_post_error("Fehler in der Eingabe !");
					xvt_vobj_set_title(edit_dlg156[9],"\0");
				  }
				 else
					scr.z[10]= atof(daten156);

				}
			 }
			else
			 {	/*	Contents of control were changed	*/
			  sichere_datenblock = TRUE;
			 }
		  }
		  break;
		case DLG_156_EDIT_FCRED:
		  {	/*	Edit control was operated.	*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change)
			 {
				if (xdEvent->v.ctl.ci.v.edit.active)
				{	/*	focus has entered the control	*/

				}
				else
				{	/*	focus has left the control		*/
				 xvt_vobj_get_title(edit_dlg156[10],daten156,10);
				 fehler = is_zahl(daten156);
				 if (fehler ==0)
				  {
                   char buf[200];//Dick 26.11.99
                   xvt_res_get_str(STR_ERROR_INPUT,buf,sizeof(buf));
                   xvt_dm_post_error("%s",buf);
					//xvt_dm_post_error("Fehler in der Eingabe !");
					xvt_vobj_set_title(edit_dlg156[10],"\0");
				  }
				 else
					scr.z[11]= atof(daten156);

				}
			 }
			else
			 {	/*	Contents of control were changed	*/
			  sichere_datenblock = TRUE;
			 }
		  }
		  break;
		case DLG_156_LISTEDIT: /* "List Edit 25:Profiltyp" */
			{
			/*		Listedit control was operated.		*/
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
				/*	Contents of control were changed	*/
//			  xvt_vobj_get_title(list_edit_156,dlg156_profiltyp,4);
			}
			}
			break;

        case DLG_156_DB:
            {
             int typ = xvt_list_get_sel_index(list_edit_156);
             xvt_dlg_create_res(WD_MODAL, DLG_ARMCO, EM_ALL, DLG_ARMCO_eh, (long)(typ+1));
			 	 
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
