/****************************************************************************
*             WSPD122 .CPP                                                  *
*             14.10.1994                                                    *
****************************************************************************/
#include <windows.h>
#include "xvt.h"
#include "wspwin.h"

#define DLG_RES_ID DLG_122
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODAL

extern BOOLEAN DLG_122_WHAT ;


/* 	Handler for dialog DLG_122 ("")    */

long XVT_CALLCONV1
#if XVT_CC_PROTO
DLG_122_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
DLG_122_eh XVT_CALLCONV2 (xdWindow, xdEvent)
WINDOW xdWindow;
EVENT *xdEvent;
#endif
{
	short xdControlId = xdEvent->v.ctl.id;

	switch (xdEvent->type) {
	case E_CREATE:
		/*
			Dialog has been created; first event sent to newly-created
			dialog.
		*/
		{

		}
		break;
	case E_DESTROY:
		/*
			Dialog has been closed; last event sent to dialog.
		*/
		//DLG_122_WHAT  = 1;//Dick 2.12.98 weg
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
		//DLG_122_WHAT  = 1;//Dick 2.12.98 weg
		xvt_vobj_destroy(xdWindow);
		}
		break;
	case E_CONTROL:		/*      User operated control in dialog.		*/
		{

		switch(xdControlId) {
		case DLG_122_PUSHBUTTON_4: /* "SICHERN" */
			{
			 DLG_122_WHAT  = 1;
			 xvt_vobj_destroy(xdWindow);
			}
			break;
		case DLG_122_PUSHBUTTON_3: /* "Abbrechen" */
			{
			 DLG_122_WHAT  =0;
			 xvt_vobj_destroy(xdWindow);
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
