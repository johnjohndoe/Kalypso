#include <windows.h>
#include "xvt.h"
#include "wspwin.h"

#include "global_types.h"

#include "list.h"

#include "global.h"

/*	Handler for dialog DLG_217 ("Auswahl Datenbank") */

//WINDOW db_radio[3];
int db_auswahl=0; //0=Manning-Strickler, 1=Darcy-Weisbach, 2=Bewuchs
BOOLEAN abbruch_217ff=FALSE;

#define DLG_RES_ID DLG_217
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODAL

long XVT_CALLCONV1
#if XVT_CC_PROTO
DLG_217_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
DLG_217_eh XVT_CALLCONV2 (xdWindow, xdEvent)
WINDOW xdWindow;
EVENT *xdEvent;
#endif
{
	short xdControlId = xdEvent->v.ctl.id;
	WINDOW db_radio[3];

	switch (xdEvent->type) {
	case E_CREATE:
		/*
			Dialog has been created; first event sent to newly-created
			dialog.
		*/
		{
		  db_radio[0]=xvt_win_get_ctl(xdWindow, DLG_217_RADIOBUTTON_1);
		  db_radio[1]=xvt_win_get_ctl(xdWindow, DLG_217_RADIOBUTTON_2);
		  db_radio[2]=xvt_win_get_ctl(xdWindow, DLG_217_RADIOBUTTON_3);

		  xvt_ctl_check_radio_button(db_radio[0],db_radio,3);
		  db_auswahl=0;

		}
		break;
	case E_DESTROY:
		/*
			Dialog has been closed; last event sent to dialog.
		*/
		{
		}
		break;
	case E_FOCUS:
		{
		/*
			Dialog has lost or gained focus.
		*/
		if (xdEvent->v.active)  {
			/*
				Dialog has gained focus
			*/
		} else {
			/*
				Dialog has lost focus
			*/
		}
		}
		break;
	case E_SIZE:
		/*
			Size of dialog has been set or changed; sent when dialog is
			created or subsequently resized by xvt_vobj_move.
		*/
		{
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
	case E_CHAR:
		/*
			Character typed.
		*/
		{
		}
		break;
	case E_CONTROL:
		/*
			User operated control in dialog.
		*/
		{

		switch(xdControlId) {
		case DLG_217_RADIOBUTTON_1: /* "Manning-Strickler" */
			{
			xdCheckRadioButton(xdWindow, DLG_217_RADIOBUTTON_1,
				DLG_217_RADIOBUTTON_1, DLG_217_RADIOBUTTON_3);
			db_auswahl=0;
			}
			break;
		case DLG_217_RADIOBUTTON_2: /* "Darcy-Weisbach" */
			{
			xdCheckRadioButton(xdWindow, DLG_217_RADIOBUTTON_2,
				DLG_217_RADIOBUTTON_1, DLG_217_RADIOBUTTON_3);
			db_auswahl=1;
			}
			break;
		case DLG_217_RADIOBUTTON_3: /* "Bewuchsparameter" */
			{
			xdCheckRadioButton(xdWindow, DLG_217_RADIOBUTTON_3,
				DLG_217_RADIOBUTTON_1, DLG_217_RADIOBUTTON_3);
			db_auswahl=2;
			}
			break;
		case DLG_217_PUSHBUTTON_5: /* "ABBRUCH" */
			{
			 abbruch_217ff=TRUE;
			 xvt_vobj_destroy(xdWindow);
			}
			break;
		case DLG_217_PUSHBUTTON_6: /* "OK" */
			{
			 abbruch_217ff=FALSE;
			 xvt_vobj_destroy(xdWindow);
			}
			break;

		default:
			break;
		}
		}
		break;
	case E_TIMER:
		/*
			Timer associated with window went off.
		*/
		{
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
