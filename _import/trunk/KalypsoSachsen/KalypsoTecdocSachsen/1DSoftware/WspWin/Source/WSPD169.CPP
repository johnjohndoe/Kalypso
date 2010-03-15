/*   Dialog169  Vergleichslistendefinition / -auswahl  18.07.97*/
#include <windows.h>
#include "xvt.h"
#include "wspwin.h"
#include "vgl_list.h"
#include "..\..\wspdlg\include\export.h"

#include "wsphilfe.h"

extern XVT_HELP_INFO hi;

#define DLG_RES_ID DLG_169
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODAL



WINDOW dlg169_lbox;

extern long *Dlg169ListSel;  //-->vgl_list.cpp
extern BOOL punktbefehl;
extern Dlg169Canceled;

/*	Handler for dialog DLG_169 ("Vergleichslisten")  */
long XVT_CALLCONV1 DLG_169_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
{
	short xdControlId = xdEvent->v.ctl.id;

	switch (xdEvent->type) {
	case E_CREATE:
		{
		 char str[256];
		 dlg169_lbox = xvt_win_get_ctl(xdWindow,DLG_169_LBOX);
		 Dlg169Canceled = FALSE;
		 for (int id=1500;id<=1514;id++)
		 {
		  xvt_res_get_str(id,str,255);
		  xvt_list_add(dlg169_lbox,-1,(char*)&str);
		 }
		 if (hi!=NULL_HELP_INFO)
			xvt_help_set_win_assoc(hi, xdWindow, HID_KAPITEL_7_5_1_2, 0L);
		}
		break;
	case E_DESTROY:
		{
		}
		break;
	case E_CLOSE:
		{
		 Dlg169Canceled = TRUE;
		 xvt_vobj_destroy(xdWindow);
		}
		break;
	case E_CONTROL:
		{
		switch(xdControlId) {
		case DLG_169_LBOX: /* "List Box 1" */
			{ 	/*			List box was operated.			*/
			if (xdEvent->v.ctl.ci.v.lbox.dbl_click)
			  {  /*		double click			*/
			  }
			else
			  {  /*		single click			*/
			  }
			}
			break;
		case DLG_169_PUSHBUTTON_OK: /* "OK" */
			{
			 if (GetVglListSelectedItems( dlg169_lbox,Dlg169ListSel))
				 Dlg169Canceled = FALSE;
			 else Dlg169Canceled = TRUE;
			 xvt_vobj_destroy(xdWindow);
			}
			break;
		case DLG_169_PUSHBUTTON_CANCEL: /* "Abbrechen" */
			{
			 Dlg169Canceled = TRUE;
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
