#include <windows.h>
#include <windowsx.h>
#include "xvt.h"
#include "wspwin.h"

#include "global_types.h"
#include "..\..\wspdlg\include\export.h"

#include "list.h"

#include "dis_prof.h"

#include "global.h"

#include "wsphilfe.h"

extern XVT_HELP_INFO hi;

char dir150[251];
char driv150[25];
HWND hwnd150,h_LBox,h_CBox;
WINDOW dlg_150=NULL_WIN,edit150;
extern WINDOW main_win;
int CBoxSelNum=1;

/*	Handler for dialog DLG_151 ("NEUE KOORDINATE  EINFÜGEN")*/
long XVT_CALLCONV1
#if XVT_CC_PROTO
DLG_OPEN_FILE_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
DLG_OPEN_FILE_eh XVT_CALLCONV2 (xdWindow, xdEvent)
WINDOW xdWindow;
EVENT *xdEvent;
#endif
{
	short xdControlId = xdEvent->v.ctl.id;

	switch (xdEvent->type) {
	case E_CREATE:
		{
		 dlg_150 = xdWindow;
         hwnd150 =(HWND) xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW);
		 edit150 = xvt_win_get_ctl(xdWindow,DLG_150_EDIT_3);
         h_LBox=(HWND) xvt_vobj_get_attr(xvt_win_get_ctl(xdWindow,DLG_150_LBOX_4),ATTR_NATIVE_WINDOW);
         h_CBox=(HWND) xvt_vobj_get_attr(xvt_win_get_ctl(xdWindow,DLG_150_LISTEDIT_5),ATTR_NATIVE_WINDOW);
		 //edit2 = xvt_win_get_ctl(xdWindow,DLG_151_EDIT_7);
		 xvt_scr_set_focus_vobj(edit150);
         DlgDirList(hwnd150,dir150,DLG_150_LBOX_4,DLG_150_EDIT_3,DDL_DIRECTORY|DDL_EXCLUSIVE);
         DlgDirListComboBox(hwnd150,driv150,DLG_150_LISTEDIT_5,7/*NULL/*DLG_150_EDIT_3*/,DDL_DRIVES|DDL_EXCLUSIVE);
         ComboBox_SetCurSel(h_CBox, CBoxSelNum);

		 if (hi!=NULL_HELP_INFO)
			xvt_help_set_win_assoc(hi, xdWindow, HID_KAPITEL_6_3_1, 0L);
		}
		break;
	case E_DESTROY:
		{
		 dlg_150=NULL_WIN;
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
		case DLG_150_PUSHBUTTON_OK: /* "OK" */
			{
			 xvt_vobj_destroy(xdWindow);
			}
			break;                                                   
		case DLG_150_PUSHBUTTON_CANCEL: /* "Abbrechen" */
			{
			 xvt_vobj_destroy(xdWindow);
			}
			break;
		case DLG_150_EDIT_3:			{
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
		

        case DLG_150_LBOX_4:
            {
            /*
            ListBox control was operated.
            */
            if (xdEvent->v.ctl.ci.v.lbox.dbl_click)
                {
                 DlgDirList(hwnd150,dir150,DLG_150_LBOX_4,DLG_150_EDIT_3,DDL_DIRECTORY|DDL_EXCLUSIVE);
                }
            else
                {
                 //int numm=ListBox_GetCurSel(h_LBox);
                 //ListBox_GetText(h_LBox, ListBox_GetCurSel(h_LBox), dir150);
                 DlgDirSelectEx(hwnd150,dir150,251,DLG_150_LBOX_4);                 
                }
            }
            break;
        case DLG_150_LISTEDIT_5:			{
			/*
				Edit control was operated.
			*/
            int a;
			if (xdEvent->v.ctl.ci.v.listedit.focus_change) {
				if (xdEvent->v.ctl.ci.v.listedit.active) {
					/*
						focus has entered the control
					*/
                    a=0;
				} else {
					/*
						focus has left the control
					*/
                   a=1;
				}
			} else {
				/*
					Contents of control were changed
				*/
                a=2;
                a=ComboBox_GetCurSel(h_CBox);
                DlgDirSelectComboBoxEx(hwnd150,driv150,251,DLG_150_LISTEDIT_5);
                if(DlgDirListComboBox(hwnd150,driv150,DLG_150_LISTEDIT_5,7/*NULL/*DLG_150_EDIT_3*/,DDL_DRIVES|DDL_EXCLUSIVE) !=0)
                    {
                     CBoxSelNum=a; 
                     DlgDirList(hwnd150,driv150,DLG_150_LBOX_4,DLG_150_EDIT_3,DDL_DIRECTORY|DDL_EXCLUSIVE);
                     ComboBox_SetCurSel(h_CBox, CBoxSelNum);
                    }
                else
                    ComboBox_SetCurSel(h_CBox, CBoxSelNum);
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
