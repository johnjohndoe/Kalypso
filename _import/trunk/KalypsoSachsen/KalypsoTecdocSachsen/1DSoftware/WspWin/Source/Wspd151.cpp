#include <windows.h>
#include "xvt.h"
#include "wspwin.h"
#include "resource.h"

#include "global_types.h"
#include "..\..\wspdlg\include\export.h"

#include "list.h"

#include "dis_prof.h"
#include "bce_allg.h"

#include "global.h"

#include "wsphilfe.h"

extern XVT_HELP_INFO hi;

#define DLG_RES_ID DLG_151
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODAL


extern WINDOW win_120[100], WIN_117;
extern WINDOW Edit_Win116[15],WIN120,WIN_116;

extern BOOLEAN SaveProfilFile;
extern char temp[100];
extern WINDOW edit1,edit2;
extern double x,y;

WINDOW dlg_151=NULL_WIN;

/*	Handler for dialog DLG_151 ("NEUE KOORDINATE  EINFÜGEN")*/
long XVT_CALLCONV1
#if XVT_CC_PROTO
DLG_151_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
DLG_151_eh XVT_CALLCONV2 (xdWindow, xdEvent)
WINDOW xdWindow;
EVENT *xdEvent;
#endif
{
	short xdControlId = xdEvent->v.ctl.id;

	switch (xdEvent->type) {
	case E_CREATE:
		{
		 dlg_151 = xdWindow;

		 edit1 = xvt_win_get_ctl(xdWindow,DLG_151_EDIT_6);
		 edit2 = xvt_win_get_ctl(xdWindow,DLG_151_EDIT_7);
		 xvt_scr_set_focus_vobj(edit1);

		 if (hi!=NULL_HELP_INFO)
			xvt_help_set_win_assoc(hi, xdWindow, HID_KAPITEL_6_3_1, 0L);

		}
		break;
	case E_DESTROY:
		{
		 dlg_151=NULL_WIN;
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
		case DLG_151_PUSHBUTTON_9: /* "OK" */
			{
			 int fehler;
			 xvt_vobj_get_title(edit1,temp,15);
			 fehler = is_zahl(temp);
			 if (fehler ==0)
				 {
                   char buf[200];//Dick 12.01.00
                   xvt_res_get_str(STR_ERROR_Y_WERT,buf,sizeof(buf));
                   xvt_dm_post_error("%s",buf);
				  //xvt_dm_post_error("Fehler im y-Wert !");
				  xvt_vobj_set_title(edit1,"\0");
				 }
			 else
				{
				 x = atof(temp);
				 xvt_vobj_get_title(edit2,temp,15);
				 fehler = is_zahl(temp);
				 if (fehler ==0)
					{
                      char buf[200];//Dick 12.01.00
                      xvt_res_get_str(STR_ERROR_H_WERT,buf,sizeof(buf));
                      xvt_dm_post_error("%s",buf);
					 //xvt_dm_post_error("Fehler im Höhenwert !");
					 xvt_vobj_set_title(edit2,"\0");
					}
				 else
					{
					 if (strlen(temp)>0)
						 y = atof(temp);
					 else y=BCE_NAN;

					 list->Sort(x,y,0);  // alle Datensätze !!
					 scr.scrollpos =1 ;

					 list->GetScrollDaten(&scr);
					 if (WIN_116!=NULL_WIN)
						 display_prof116(&Edit_Win116[0]);
					 else
						if (WIN120!=NULL_WIN)
							 display_prof120(&win_120[0]);

					 SaveProfilFile = TRUE;
					 if (WIN_117 != NULL_WIN)
					     xvt_dwin_invalidate_rect(WIN_117,0);
					 xvt_vobj_destroy(xdWindow);
					}
				}
			}
			break;
		case DLG_151_PUSHBUTTON_8: /* "Abbrechen" */
			{
			 xvt_vobj_destroy(xdWindow);
			}
			break;
		case DLG_151_EDIT_6:			{
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
			}
			}
			break;
		case DLG_151_EDIT_7:			{
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
			}
			}
			break;


		default:
			break;
		}
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
