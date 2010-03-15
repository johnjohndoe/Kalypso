#include <windows.h>
#include "xvt.h"
#include "wspwin.h"

#include "global_types.h"
#include "..\..\wspdlg\Include\export.h"

#include "list.h"

#include "global.h"

#include "wsphilfe.h"
#include "bce_allg.h"

extern XVT_HELP_INFO hi;

#define DLG_RES_ID DLG_107
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODELESS

/*	Handler for dialog DLG_107 ("Überfallbeiwert Ok-Brücke") */

WINDOW win107_edit,win107_edit_6,
				   win107_edit_7,
				   win_dlg_107=NULL_WIN,
                   radios[3]  ;

BRUECKE uk_br;
extern WINDOW dlg_sonderprofil;
extern HWND dlgbeiwert_db;
extern BOOLEAN schuetz_flag;
/*************   GHJ   *************/
static WNDPROC defWndProc;
LRESULT CALLBACK Dlg107WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg)
	{
	case WM_HELP:
		{
			LPHELPINFO lphi = (LPHELPINFO)lParam;
			if (hi!=NULL_HELP_INFO)
			{
				xvt_help_display_topic(hi, HID_KAPITEL_5_1_3_1);
			}
		}
		break;

	default:
		break;
	}
	return CallWindowProc(defWndProc, hwnd, uMsg, wParam, lParam);
}
/**************************************/


long XVT_CALLCONV1
#if XVT_CC_PROTO
DLG_107_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
DLG_107_eh XVT_CALLCONV2 (xdWindow, xdEvent)
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
		 SetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC, (LONG)&Dlg107WindowProc);
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
         win_dlg_107=xdWindow;
		 win107_edit = xvt_win_get_ctl(xdWindow,DLG_107_EDIT);
         win107_edit_6 = xvt_win_get_ctl(xdWindow,DLG_107_EDIT_6);
         win107_edit_7 = xvt_win_get_ctl(xdWindow,DLG_107_EDIT_7);

		 strcpy(uk_br.beiwert,"0.5");
         strcpy(uk_br.abflusszahl,"0.8");
		 strcpy(uk_br.typ,"STANDARD");
		 strcpy(uk_br.neigung,"90");
		 xvt_vobj_set_visible(win107_edit_7,FALSE);
		 xvt_vobj_set_visible(win107_edit_6,TRUE);
		 xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_107_TEXT_2)),FALSE);
		 xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_107_TEXT_1)),TRUE);


		 list->GetInfoline2(scr.datensatz,&uk_br);
		 if (strlen(uk_br.beiwert)>0)
			 xvt_vobj_set_title(win107_edit,uk_br.beiwert);
         if (strlen(uk_br.abflusszahl)>0)
			 xvt_vobj_set_title(win107_edit_6,uk_br.abflusszahl);
         if (strlen(uk_br.neigung)>0)
			 xvt_vobj_set_title(win107_edit_7,uk_br.neigung);

		  radios[0]=xvt_win_get_ctl(xdWindow, DLG_107_RADIOBUTTON_1); //Standard
		  radios[1]=xvt_win_get_ctl(xdWindow, DLG_107_RADIOBUTTON_2); //Schmidt
		  radios[2]=xvt_win_get_ctl(xdWindow, DLG_107_RADIOBUTTON_3); //Knapp

		  if(uk_br.typ[1]=='T')
		  {//Standard
				xvt_ctl_check_radio_button(radios[0],radios,3);
			 xvt_vobj_set_visible(win107_edit_7,FALSE);
			 xvt_vobj_set_visible(win107_edit_6,TRUE);
		     xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_107_TEXT_2)),FALSE);
		     xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_107_TEXT_1)),TRUE);
          }
		  if(uk_br.typ[1]=='C')

		  {//Schmidt
			 xvt_ctl_check_radio_button(radios[1],radios,3);
 			 xvt_vobj_set_visible(win107_edit_7,TRUE);
			 xvt_vobj_set_visible(win107_edit_6,FALSE);
 		     xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_107_TEXT_2)),TRUE);
		     xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_107_TEXT_1)),FALSE);

		  }

		  if(uk_br.typ[1]=='N')
		  {//Knapp
				xvt_ctl_check_radio_button(radios[2],radios,3);
			 xvt_vobj_set_visible(win107_edit_7,TRUE);
			 xvt_vobj_set_visible(win107_edit_6,FALSE);
 		     xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_107_TEXT_2)),TRUE);
		     xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_107_TEXT_1)),FALSE);
		  }
		  if (schuetz_flag==TRUE)
		  {
 		     xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_107_TEXT)),FALSE);
 		     xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_107_EDIT)),FALSE);
 		     xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_107_RADIOBUTTON_1)),FALSE);
 		     xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_107_TEXT)),FALSE);
		     xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_107_TEXT_1)),FALSE);
 		     xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_107_EDIT_6)),FALSE);
 		     xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_107_PB_DB)),FALSE);
 		     xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_107_TEXT_2)),TRUE);
 		     xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_107_EDIT_7)),TRUE);
		     if(uk_br.typ[1]=='T')
			 {
			   xvt_ctl_check_radio_button(radios[1],radios,3); //Schmidt TRUE
			   strcpy(uk_br.typ,"SCHMIDT");
			 }

		  }
		 if (hi!=NULL_HELP_INFO)
			xvt_help_set_win_assoc(hi, xdWindow, HID_KAPITEL_5_1_3_1, 0L);
		}
		break;
	case E_DESTROY:
		{
		 dlg_sonderprofil = NULL_WIN;
         win_dlg_107=NULL_WIN;
		 schuetz_flag=FALSE;
		}
		break;
	case E_CLOSE:
		{
		xvt_vobj_destroy(xdWindow);
		schuetz_flag=FALSE;
		}
		break;
	case E_CONTROL:
		{

		switch(xdControlId) {
		case DLG_107_PB_OK: /* "Ok" */
			{
			 char str[25];
			 xvt_vobj_get_title(win107_edit,str,20);
			 if (is_zahl(&str[0])>0)
				{
				 strcpy(uk_br.beiwert,str);
				 //list->SaveInfoline2(scr.datensatz,&uk_br);
				}
			 else
				{

				 //uk_br.beiwert[0]='0';  //default-Wert: =0
				 //uk_br.beiwert[1]='\0';
				 //list->SaveInfoline2(scr.datensatz,&uk_br);
                strcpy(uk_br.beiwert,"0.5");
				}
             xvt_vobj_get_title(win107_edit_6,str,20);
			 if (is_zahl(&str[0])>0)
				{
				 strcpy(uk_br.abflusszahl,str);				
				}
			 else
				{
				 strcpy(uk_br.abflusszahl,"0.8");
				}

           if(uk_br.typ[1]!='T')
		   {
             xvt_vobj_get_title(win107_edit_7,str,20); //Neigungswinkel
			 if (is_zahl(&str[0])>0)
				{
				 strcpy(uk_br.neigung,str);				
				}
			 else
				{
				 strcpy(uk_br.neigung,"90");
				}
           }
             list->SaveInfoline2(scr.datensatz,&uk_br);
			 SaveProfilFile = TRUE;
		     schuetz_flag=FALSE;

			 xvt_vobj_destroy(xdWindow);
			}
			break;
		case DLG_107_PB_CANCEL: /* "Abbrechen" */
			{
       		 schuetz_flag=FALSE;
 			 xvt_vobj_destroy(xdWindow);
			}
			break;
		case DLG_107_EDIT:			{
			/*
				Edit control was operated.
			*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change) {
				if (xdEvent->v.ctl.ci.v.edit.active) {
					/*	focus has entered the control		*/
				} else {
					/*	focus has left the control			*/
				}
			} else {
				/*	Contents of control were changed		*/
			}
			}
			break;
        case DLG_107_EDIT_6:			{
			/*
				Edit control was operated.
			*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change) {
				if (xdEvent->v.ctl.ci.v.edit.active) {
					/*	focus has entered the control		*/
				} else {
					/*	focus has left the control			*/
				}
			} else {
				/*	Contents of control were changed		*/
			}
			}
			break;
        case DLG_107_EDIT_7: //Neigungswinkel
			{
			/*
				Edit control was operated.
			*/
			if (xdEvent->v.ctl.ci.v.edit.focus_change) {
				if (xdEvent->v.ctl.ci.v.edit.active) {
					/*	focus has entered the control		*/
				} else {
					/*	focus has left the control			*/
				}
			} else {
				/*	Contents of control were changed		*/
			}
			}
			break;

  
		case DLG_107_RADIOBUTTON_1: /* "Standardverfahren " */
			{
			 xdCheckRadioButton(xdWindow, DLG_107_RADIOBUTTON_1,
			 DLG_107_RADIOBUTTON_1, DLG_107_RADIOBUTTON_3);
			 strcpy(uk_br.typ,"STANDARD");
			 xvt_vobj_set_visible(win107_edit_7,FALSE);
			 xvt_vobj_set_visible(win107_edit_6,TRUE);
 		 xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_107_TEXT_1)),TRUE);
		 xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_107_TEXT_2)),FALSE);
			}
		break;
		
		case DLG_107_RADIOBUTTON_2: /* "Schmidt " */
			{
			 xdCheckRadioButton(xdWindow, DLG_107_RADIOBUTTON_2,
			 DLG_107_RADIOBUTTON_1, DLG_107_RADIOBUTTON_3);
			 strcpy(uk_br.typ,"SCHMIDT");
			 xvt_vobj_set_visible(win107_edit_7,TRUE);
			 xvt_vobj_set_visible(win107_edit_6,FALSE);
 			 xvt_vobj_set_title(win107_edit_7,"90");
 		 xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_107_TEXT_2)),TRUE);
		 xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_107_TEXT_1)),FALSE);

			}
		break;
		
		case DLG_107_RADIOBUTTON_3: /* "Knapp " */
			{
			 xdCheckRadioButton(xdWindow, DLG_107_RADIOBUTTON_3,
			 DLG_107_RADIOBUTTON_1, DLG_107_RADIOBUTTON_3);
			 strcpy(uk_br.typ,"KNAPP");
			 xvt_vobj_set_visible(win107_edit_7,TRUE);
			 xvt_vobj_set_visible(win107_edit_6,FALSE);
 			 xvt_vobj_set_title(win107_edit_7,"90");
 		 xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_107_TEXT_2)),TRUE);
		 xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_107_TEXT_1)),FALSE);

			}
		break;
		
		case DLG_107_PB_DB: /* "Datenbank" */
            {
            WINDOW parent_win;
            HWND db_dlg=NULL;
            if (WIN_116!=NULL_WIN)
                parent_win=WIN_116;
            else if (WIN120!=NULL_WIN)
                parent_win=WIN120;
            
             db_dlg=DoDatabankDlg((HWND)xvt_vobj_get_attr(parent_win,ATTR_NATIVE_WINDOW), start_dir,0);
             if(db_dlg!=NULL)
                 SetParent(db_dlg,(HWND)xvt_vobj_get_attr(parent_win,ATTR_NATIVE_WINDOW));
                 
            }
            break;
		default:
			break;
		}
		}
		break;
	case E_USER:
		/*	Application initiated.	*/
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
