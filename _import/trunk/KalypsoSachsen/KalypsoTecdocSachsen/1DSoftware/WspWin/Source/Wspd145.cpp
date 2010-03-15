/*********************************************************************
*     Auswahlbox :Lage der Trennflächen                              *
*     Aufruf bei neuen Datensatz anlegen -vor Dialog143              *
*            in file:dis_prof.cpp ->Funktion:display_prof120         *
*                                                                    *
*     Andresen   09.06.1995                                          *
*     in: Scroller scr :Lage der Trennflächen                        *
*																							*
*		links		scr.z[2] = 1 oder 3    1:Vorland , 3:Böschungsfuß     *
*     rechts   scr.z[3] = 2 oder 4    2:Vorland , 4:Böschungsfuß     *
*                                                                    *
*                                                                    *
*                                                                    *
*                                                                    *
**********************************************************************/
#include <windows.h>
#include "xvt.h"
#include "wspwin.h"

#include "global_types.h"
#include "..\..\wspdlg\include\export.h"

#include "list.h"
#include "paint.h"
#include "typen.h"

#include "global.h"

#include "wsphilfe.h"

extern XVT_HELP_INFO hi;

#define DLG_RES_ID DLG_145
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODAL

WINDOW radio_1[2],
		 radio_2[2];
/*	Handler for dialog DLG_145 ("Lage der Trennflächen")*/
long XVT_CALLCONV1
#if XVT_CC_PROTO
DLG_145_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
DLG_145_eh XVT_CALLCONV2 (xdWindow, xdEvent)
WINDOW xdWindow;
EVENT *xdEvent;
#endif
{
	short xdControlId = xdEvent->v.ctl.id;

	switch (xdEvent->type) {
	case E_CREATE:
		#if XVTWS == WINWS
		xdCheckRadioButton(xdWindow, DLG_145_RADIOBUTTON_4,
			DLG_145_RADIOBUTTON_4, DLG_145_RADIOBUTTON_5);
		#endif
		#if XVTWS == WINWS
		xdCheckRadioButton(xdWindow, DLG_145_RADIOBUTTON_6,
			DLG_145_RADIOBUTTON_6, DLG_145_RADIOBUTTON_7);
		#endif
		{
		 radio_1[0]=xvt_win_get_ctl(xdWindow,DLG_145_RADIOBUTTON_4);
		 radio_1[1]=xvt_win_get_ctl(xdWindow,DLG_145_RADIOBUTTON_5);
		 radio_2[0]=xvt_win_get_ctl(xdWindow,DLG_145_RADIOBUTTON_6);
		 radio_2[1]=xvt_win_get_ctl(xdWindow,DLG_145_RADIOBUTTON_7);

		 if (scr.z[2]==BCE_NAN)
			  scr.z[2]=1;  //:=linke Trennfläche auf Vorland
		 else //setzen
			 {
				if (scr.z[2]==1)
					xvt_ctl_check_radio_button(radio_1[0],radio_1,2);
				else
				  if (scr.z[2]==3)
					  xvt_ctl_check_radio_button(radio_1[1],radio_1,2);
			 }

		 if (scr.z[3]==BCE_NAN)
			  scr.z[3]=2;  //:=rechte Trennfläche auf Vorland
		 else //setzen
			 {
				if (scr.z[3]==2)
					xvt_ctl_check_radio_button(radio_2[0],radio_2,2);
				else
				  if (scr.z[3]==4)
					  xvt_ctl_check_radio_button(radio_2[1],radio_2,2);
			 }

		 if (hi!=NULL_HELP_INFO)
			xvt_help_set_win_assoc(hi, xdWindow, HID_KAPITEL_4_4_3_3, 0L);
		}
		break;
	case E_DESTROY:
		{
		}
		break;
	case E_FOCUS:
		{
		/*			Dialog has lost or gained focus.		*/
		if (xdEvent->v.active)  {
			/*				Dialog has gained focus			*/
		} else {
			/*				Dialog has lost focus			*/
		}
		}
		break;
	case E_CLOSE:
		{
		xvt_vobj_destroy(xdWindow);
		}
		break;
	case E_CHAR:
		{
		}
		break;
	case E_CONTROL:
		{

		switch(xdControlId) {
		case DLG_145_PUSHBUTTON_11: /* "Abbrechen" */
			{
			 xvt_vobj_destroy(xdWindow);
			}
			break;
		case DLG_145_RADIOBUTTON_4: /* "links Vorland" */
			{
			xdCheckRadioButton(xdWindow, DLG_145_RADIOBUTTON_4,
				DLG_145_RADIOBUTTON_4, DLG_145_RADIOBUTTON_5);
			 scr.z[2]=1;
			}
			break;
		case DLG_145_RADIOBUTTON_5: /* "links Böschungsfuß" */
			{
			xdCheckRadioButton(xdWindow, DLG_145_RADIOBUTTON_5,
				DLG_145_RADIOBUTTON_4, DLG_145_RADIOBUTTON_5);
			 scr.z[2]=3;
			}
			break;
		case DLG_145_RADIOBUTTON_6: /* "rechts Vorland" */
			{
			xdCheckRadioButton(xdWindow, DLG_145_RADIOBUTTON_6,
				DLG_145_RADIOBUTTON_6, DLG_145_RADIOBUTTON_7);
			 scr.z[3]=2;
			}
			break;
		case DLG_145_RADIOBUTTON_7: /* "rechts Böschungsfuß" */
			{
			xdCheckRadioButton(xdWindow, DLG_145_RADIOBUTTON_7,
				DLG_145_RADIOBUTTON_6, DLG_145_RADIOBUTTON_7);
			 scr.z[3]=4;
			}
			break;
		case DLG_145_PUSHBUTTON_10: /* "OK" */
			{
			 SaveProfilFile = TRUE;
       list->SaveSonderprofildaten(&scr, TRENNFLAECHEN); // die geänderten Werte übernehmen
			 xvt_vobj_destroy(xdWindow);
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
