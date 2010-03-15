/*
	Handler for dialog DLG_222 ("Projektbezeichnung")
*/

#include <windows.h>
#include "xvt.h"
#include "wspwin.h"
#include "profpro2.h"

#include "..\..\wspdlg\include\export.h"

#include "wsphilfe.h"

extern XVT_HELP_INFO hi;
BOOLEAN stopit;
/* 
	Information about the dialog
*/
#define DLG_RES_ID DLG_222
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODELESS

/*
	Handler for dialog DLG_222 ("Projektbezeichnung")
*/
long XVT_CALLCONV1
#if XVT_CC_PROTO
DLG_222_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
DLG_222_eh XVT_CALLCONV2 (xdWindow, xdEvent)
WINDOW xdWindow;
EVENT *xdEvent;
#endif
{
	short xdControlId = xdEvent->v.ctl.id;

	char edittext[100];

	switch (xdEvent->type) {
	case E_CREATE:
		/*
			Dialog has been created; first event sent to newly-created
			dialog.
		*/
		{
		 edittext[0]='\0';
		 lese_projektbezeichnung(edittext); //in profpro2
		 xvt_vobj_set_title(xvt_win_get_ctl(xdWindow,DLG_222_EDIT_1),edittext);
		 if (hi!=NULL_HELP_INFO)
			xvt_help_set_win_assoc(hi, xdWindow, HID_KAPITEL_4_2_1, 0L);
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
		case DLG_222_EDIT_1:			{
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
		case DLG_222_PUSHBUTTON_3: /* "OK" */
			{
			  xvt_vobj_get_title(xvt_win_get_ctl(xdWindow,DLG_222_EDIT_1),edittext,60);
              edittext[60]='\0';
			  schreibe_projektbezeichnung(edittext); //in profpro2
              stopit=FALSE;
			  xvt_vobj_destroy(xdWindow);
			}
			break;
		case DLG_222_PUSHBUTTON_4: /* "Abbrechen" */
			{
			 xvt_vobj_destroy(xdWindow);
			 stopit=TRUE;
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
