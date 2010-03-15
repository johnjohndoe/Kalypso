/****************************************************************************
*             Dialog 153 : Anzeige Flaechedaten                             *
*             01.07.1996 Andresen                                           *
****************************************************************************/
#include <windows.h>
#include "xvt.h"
#include "wspwin.h"

#include "global_types.h"
#include "..\..\wspdlg\include\export.h"

#include "list.h"

#include "wsphilfe.h"

extern XVT_HELP_INFO hi;

#define DLG_RES_ID DLG_FLAECHE
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODELESS

extern WINDOW   dlg_sonderprofil;
extern Scroller scr;
extern WSP_PROFIL_LISTE *pWPL;

extern WINDOW WIN_116,WIN120;

/*************   GHJ   *************/
static WNDPROC defWndProc;
extern WINDOW main_win;
LRESULT CALLBACK Dlg153WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg)
	{
	case WM_HELP:
		{
			LPHELPINFO lphi = (LPHELPINFO)lParam;
			if (hi!=NULL_HELP_INFO)
			{
            //xvt_help_display_topic(hi, HID_KAPITEL_7_1_3);
			}
		}
		break;

	default:
		break;
	}
	return CallWindowProc(defWndProc, hwnd, uMsg, wParam, lParam);
}
/**************************************/


/* Handler for dialog DLG_153 ("Flächendaten")  */

long XVT_CALLCONV1
#if XVT_CC_PROTO
DLG_FLAECHE_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
DLG_FLAECHE_eh XVT_CALLCONV2 (xdWindow, xdEvent)
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
		 SetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC, (LONG)&Dlg153WindowProc);
		 ChangeFontAndSize((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW));	// GHJ
		 xvt_vobj_set_visible(xdWindow,TRUE);
         /***********************************/

		 S_FLAECHE *fl;//struct S_FLAECHE {char s[15],s_auf[15],s_ab[15]}

		 fl = new S_FLAECHE;
		 pWPL->PList->GetInfolineFlaeche(fl,scr.datensatz);
		 dlg_sonderprofil= xdWindow;
		 xvt_vobj_set_title(xvt_win_get_ctl(xdWindow,DLG_FLAECHE_EDIT_1),fl->s);
		 xvt_vobj_set_title(xvt_win_get_ctl(xdWindow,DLG_FLAECHE_EDIT_2),fl->s_auf);
		 xvt_vobj_set_title(xvt_win_get_ctl(xdWindow,DLG_FLAECHE_EDIT_3),fl->s_ab);
		 delete fl;
		 if (hi!=NULL_HELP_INFO)
			xvt_help_set_win_assoc(hi, xdWindow, HID_KAPITEL_7_2_4, 0L);
		}
		break;
	case E_DESTROY:
		{
		 dlg_sonderprofil= NULL_WIN;
		}
		break;
	case E_CLOSE:
		{
		 dlg_sonderprofil= NULL_WIN;
		}
		break;
	case E_CONTROL:
		{
		switch(xdControlId) {
		case DLG_FLAECHE_PB_OK: /* "OK" */
			{
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