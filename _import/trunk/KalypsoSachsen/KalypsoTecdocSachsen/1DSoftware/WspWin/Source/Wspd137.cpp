/*************************************************************
*				  WSPD137.CPP                                    *
*				  27.11.1994                                     *
*	  sortiertes Einfügen einer neuen Koordinate              *
*    in die Profildatei  für wspw116.cpp                     *
*                                                            *
**************************************************************/
#include <windows.h>
#include "xvt.h"
#include "wspwin.h"
#include "resource.h"

#include "global_types.h"
#include "global_vars.h"
#include "..\..\wspdlg\include\export.h"

#include "list.h"
#include "dis_prof.h"

#include "typen.h"

#include "global.h"//Dick 8.12.98
#include "bce_allg.h"
#include "paint.h"

#include "wsphilfe.h"

extern XVT_HELP_INFO hi;

/* Information about the dialog  */
#define DLG_RES_ID DLG_137
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODELESS

extern double hor_mouse_pos,ver_mouse_pos;
extern List *list;
extern Paint *paint;
extern BOOLEAN win117_mouse_down;
extern WINDOW WIN_117,WIN120,WIN_116;
extern WINDOW Edit_Win116[15],win_120[100];
long timer_id;
extern char hor[10],ver[10];
char Pos_nr_feld[10];
extern char temp[100];

BOOLEAN is_open_dlg_137;
WINDOW edit1,edit2,edit3;
int EinfPosition=0,Pos_nr;
double x,y;
extern BOOLEAN SaveProfilFile;
extern int fehler;
extern struct _MMP mmp;
extern int hor_pnt,ver_pnt;
extern int typ[TYPE_SIZE];//Dick 8.12.98

/*************   GHJ   *************/
static WNDPROC defWndProc;
LRESULT CALLBACK Dlg137WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg)
	{
	case WM_HELP:
		{
			LPHELPINFO lphi = (LPHELPINFO)lParam;
			if (hi!=NULL_HELP_INFO)
			{
				xvt_help_display_topic(hi, HID_KAPITEL_6_3_1);
			}
		}
		break;

	default:
		break;
	}
	return CallWindowProc(defWndProc, hwnd, uMsg, wParam, lParam);
}
/**************************************/


/* Handler for dialog DLG_137 ("NEUE KOORDINATE EINFÜGEN") */

long XVT_CALLCONV1
#if XVT_CC_PROTO
DLG_137_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
DLG_137_eh XVT_CALLCONV2 (xdWindow, xdEvent)
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
		 SetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC, (LONG)&Dlg137WindowProc);
		 ChangeFontAndSize((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW));	// GHJ
		 xvt_vobj_set_visible(xdWindow, TRUE);
         /***********************************/


		 is_open_dlg_137=TRUE;
		 win117_mouse_down=FALSE;
		 x=0.0;
		 y=0.0;
		 hor_mouse_pos=0.0;
		 ver_mouse_pos=0.0;
		 timer_id = xvt_timer_create(xdWindow,400);
		 edit1 = xvt_win_get_ctl(xdWindow,DLG_137_EDIT_6);
		 edit2 = xvt_win_get_ctl(xdWindow,DLG_137_EDIT_7);
         edit3 = xvt_win_get_ctl(xdWindow,DLG_137_EDIT_10);
		 xvt_scr_set_focus_vobj(edit1);
		 if (hi!=NULL_HELP_INFO)
			xvt_help_set_win_assoc(hi, xdWindow, HID_KAPITEL_6_3_1, 0L);
		}
		break;
	case E_DESTROY:
		/*	Dialog has been closed; last event sent to dialog.	*/
		{
		 xvt_timer_destroy(timer_id);
		 is_open_dlg_137=FALSE;
         //xvt_vobj_destroy(xdWindow);
		}
		break;
	case E_FOCUS:
		{
		/*  Dialog has lost or gained focus.	*/
		if (xdEvent->v.active)  {
			/*		Dialog has gained focus		*/
		} else {
			/*		Dialog has lost focus		*/
		}
		}
		break;
	case E_CLOSE:
		{
		 xvt_vobj_destroy(xdWindow);
		}
		break;
	case E_CHAR:
		/*		Character typed.	*/
		{
		}
		break;
	case E_CONTROL:
		/*		User operated control in dialog.	*/
		{
		switch(xdControlId) {
		case DLG_137_PUSHBUTTON_8: /* "Abbrechen" */
			{
			 xvt_vobj_destroy(xdWindow);
			}
			break;
		case DLG_137_EDIT_6:			{
			/*		Edit control was operated.		*/
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
			} else
				{  /*  	Contents of control were changed	*/
				 xvt_vobj_get_title(edit1,temp,10);
				 for (int i=0;i<=(INT)strlen(temp);i++)
					{
					 if (temp[i]=='.')
						temp[i+3]='\0';

					 if (temp[i]==',')
						{
						temp[i]='.' ;
						temp[i+3]='\0';
						}
					}

				}
			}
			break;
		case DLG_137_EDIT_7:			{
			/*	Edit control was operated.		*/
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
			} else
				{	/*  	Contents of control were changed	*/
				 xvt_vobj_get_title(edit2,temp,10);
				 for (int i=0;i<=(INT)strlen(temp);i++)
					{
					 if (temp[i]=='.')
						temp[i+3]='\0';

					 if (temp[i]==',')
						 {
						  temp[i]='.' ;
						  temp[i+3]='\0';
						 }
					}
				}
			}
			break;
        case DLG_137_EDIT_10:			{
			/*	Edit control was operated.		*/
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
			} else
				{	/*  	Contents of control were changed	*/
				 xvt_vobj_get_title(edit3,temp,10);
				 for (int i=0;i<=(INT)strlen(temp);i++)
					{
					 if (temp[i]=='.')
						temp[i+3]='\0';

					 if (temp[i]==',')
						 {
						  temp[i]='.' ;
						  temp[i+3]='\0';
						 }
					}
				}
			}
			break;
		case DLG_137_PUSHBUTTON_9: /* "OK" */
			{
			 xvt_vobj_get_title(edit1,temp,10);
			 fehler = is_zahl(temp);
			 if ((strlen(temp)>0)&&(fehler>=0))
			  {
				for (int i=0;i<=(INT)strlen(temp);i++)
					{
					 if ((temp[i]==',') || (temp[i]=='.'))
						 temp[i+3]='\0';
					}
				x = atof(temp);
			  }
			 else
			  {
				x=BCE_NAN;
                 char buf[200];//Dick 12.01.00
                 xvt_res_get_str(STR_ERROR_Y_WERT,buf,sizeof(buf));
                 xvt_dm_post_error("%s",buf);
				//xvt_dm_post_error("Fehler im y-Wert !");
                EinfPosition=0; //Dick GaussTest 26.07.98
			  }

			 xvt_vobj_get_title(edit2,temp,10);
			 fehler = is_zahl(temp);
			 if ((strlen(temp)>0)&&(fehler >=0))
				{
				 for (int i=0;i<=(INT)strlen(temp);i++)
					{
					 if ((temp[i]==',') || (temp[i]=='.'))
						 temp[i+3]='\0';
					}
				 y = atof(temp);
				}
			 else
				{
                 char buf[200];//Dick 12.01.00
                 xvt_res_get_str(STR_WSPD137_NOTE_2,buf,sizeof(buf));
                 xvt_dm_post_error("%s",buf);
				 //xvt_dm_post_error("Höhenwert wurde nicht angegeben!");
				 y=BCE_NAN;
				}

             xvt_vobj_get_title(edit3,temp,10);
			 fehler = is_zahl(temp);
			 if ((strlen(temp)>0)&&(fehler >=0))
				{
				 for (int i=0;i<=(INT)strlen(temp);i++)
					{
					 if ((temp[i]==',') || (temp[i]=='.'))
						 temp[i+3]='\0';
					}
				 Pos_nr = atoi(temp);
				}
			 else
				{
                  char buf[200];//Dick 12.01.00
                 xvt_res_get_str(STR_WSPD137_NOTE_3,buf,sizeof(buf));
                 xvt_dm_post_error("%s",buf);
				 //xvt_dm_post_error("Position wurde nicht angegeben!");
				 Pos_nr=0;
				}

			 if (x!=BCE_NAN && Pos_nr>0) //Dick GaussTest 26.7.98
			  {
                if(typ[scr.datensatz]==UK_BRUECKE||typ[scr.datensatz]==OK_BRUECKE ||
                    typ[scr.datensatz]==OK_WEHRS || typ[scr.datensatz]==OK_GELAENDE ||
					typ[scr.datensatz]==BUHNE)
						list->Koord_Einf(x,y,Pos_nr,typ[scr.datensatz]);
                else
					list->Koord_Einf(x,y,Pos_nr);
				//scr.scrollpos =1 ;
				list->GetScrollDaten(&scr);
				if  (WIN_116 !=NULL_WIN)
				 display_prof116(&Edit_Win116[0]) ;   //Werte aus scr in x-y-z Felder ausgeben
				else if  (WIN120 !=NULL_WIN)
						 display_prof120(&win_120[0]);
				xvt_dwin_invalidate_rect(WIN_117,0);
				SaveProfilFile =TRUE;
			  }
             else
                 {
                   char buf[200];//Dick 12.01.00
                   xvt_res_get_str(STR_WSPD137_NOTE_4,buf,sizeof(buf));
                   xvt_dm_post_error("%s",buf);
                  //xvt_dm_post_error("Sie müssen die Koordinaten auf der Grafik markieren!");
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
		/* Timer associated with window went off.	*/
		{
		 if ( win117_mouse_down )
			{
			 //gcvt(hor_mouse_pos,8,hor);
             if(hor_mouse_pos<1000. && hor_mouse_pos>-100.)
                 sprintf(hor,"%.4lf",hor_mouse_pos);
             else if(hor_mouse_pos<10000. && hor_mouse_pos>-1000.)
                 sprintf(hor,"%.3lf",hor_mouse_pos);
             else if(hor_mouse_pos<100000. && hor_mouse_pos>-10000.)
                 sprintf(hor,"%.2lf",hor_mouse_pos);
             else if(hor_mouse_pos<1000000. && hor_mouse_pos>-100000.)
                 sprintf(hor,"%.1lf",hor_mouse_pos);
             else if(hor_mouse_pos<10000000. && hor_mouse_pos>-1000000.)
                 sprintf(hor,"%.0lf",hor_mouse_pos);
             else
                 sprintf(hor,"%10.0lf",hor_mouse_pos);
				  /*for (int i=0;i<=(INT)strlen(hor);i++)
						if (hor[i]=='.')
							 hor[i+3]='\0';*/
			 //gcvt(ver_mouse_pos,8,ver);
             if(ver_mouse_pos<1000. && ver_mouse_pos>-100.)
                 sprintf(ver,"%.4lf",ver_mouse_pos);
             else if(ver_mouse_pos<10000. && ver_mouse_pos>-1000.)
                 sprintf(ver,"%.3lf",ver_mouse_pos);
             else if(ver_mouse_pos<100000. && ver_mouse_pos>-10000.)
                 sprintf(ver,"%.2lf",ver_mouse_pos);
             else if(ver_mouse_pos<1000000. && ver_mouse_pos>-100000.)
                 sprintf(ver,"%.1lf",ver_mouse_pos);
             else if(ver_mouse_pos<10000000. && ver_mouse_pos>-1000000.)
                 sprintf(ver,"%.0lf",ver_mouse_pos);
             else
                 sprintf(ver,"%10.0lf",ver_mouse_pos);
             EinfPosition=paint->Get_Einf_Position(hor_pnt,ver_pnt);//Dick GaussTest 26.7.98
             itoa(EinfPosition,Pos_nr_feld,10);
			 xvt_vobj_set_title(edit1,hor);
			 xvt_vobj_set_title(edit2,ver);
             xvt_vobj_set_title(edit3,Pos_nr_feld);
			 win117_mouse_down=FALSE;
             //xvt_dm_post_message("Position=%d",EinfPosition);
			}
			 xvt_vobj_get_title(edit1,temp,10);
				 for (int i=0;i<=(INT)strlen(temp);i++)
					{
					 if (temp[i]==',')
						  temp[i]='.' ;
					 if ((temp[i]==',') || (temp[i]=='.'))
						 temp[i+3]='\0';
					}
				 x = atof(temp);
			 xvt_vobj_get_title(edit2,temp,10);
				 for (i=0;i<=(INT)strlen(temp);i++)
					{
					 if (temp[i]==',')
						  temp[i]='.' ;
					 if ((temp[i]==',') || (temp[i]=='.'))
						 temp[i+3]='\0';
					}
				 y = atof(temp);
             xvt_vobj_get_title(edit3,temp,10);
				 for (i=0;i<=(INT)strlen(temp);i++)
					{
					 if (temp[i]==',')
						  temp[i]='.' ;
					 if ((temp[i]==',') || (temp[i]=='.'))
						 temp[i+3]='\0';
					}
				 Pos_nr = atoi(temp);
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
