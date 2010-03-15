/*   Dialog168  Prüflistendefinition / -auswahl  18.07.97*/
#include <windows.h>
#include "xvt.h"
#include "wspwin.h"
#include "prueflst.h"
#include "resource.h"

#include "..\..\wspdlg\include\export.h"

#include "wsphilfe.h"

extern XVT_HELP_INFO hi;

#define DLG_RES_ID DLG_168
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODAL



WINDOW dlg168_lbox;

extern long *Dlg168ListSel;  //-->prueflst.cpp
extern BOOL punktbefehl;     //-->prueflst.cpp
extern Dlg168Canceled;
extern BOOL DefaultLesen;//-->prueflst.cpp

/*	Handler for dialog DLG_168 ("Prüflisten")  */
long XVT_CALLCONV1 DLG_168_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
{
	short xdControlId = xdEvent->v.ctl.id;

	switch (xdEvent->type) {
	case E_CREATE:
		{
		 char str[256];
         int def_list168[MAX_SEL_LIST_LENGTH+1];
         int pos=1;
		 dlg168_lbox = xvt_win_get_ctl(xdWindow,DLG_168_LBOX);
		 Dlg168Canceled = FALSE;
		 for (int id=1400;id<=1483;id++)
		 {
		  xvt_res_get_str(id,str,255);
		  xvt_list_add(dlg168_lbox,-1,(char*)&str);
		 }
         if(DefaultLesen)
         if(ReadDefPrueflistenDatei(def_list168))
             {
              while(def_list168[pos]!=-1)
                  {
                   xvt_list_set_sel(dlg168_lbox,def_list168[pos],TRUE);
                   pos++;
                  }
              xvt_list_set_sel(dlg168_lbox,0,xvt_list_is_sel(dlg168_lbox,0));
              if(def_list168[0]==1)//punktbefehl
                  xvt_ctl_set_checked(xvt_win_get_ctl(xdWindow,DLG_168_PUNKT),TRUE);
             }
		 if (hi!=NULL_HELP_INFO)
			xvt_help_set_win_assoc(hi, xdWindow, HID_KAPITEL_7_5_1_3, 0L);
		 ChangeFontAndSize((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW));	// GHJ
		 xvt_vobj_set_visible(xdWindow, TRUE);		// GHJ
		}
		break;
	case E_DESTROY:
		{
		}
		break;
	case E_CLOSE:
		{
		 Dlg168Canceled = TRUE;
		 xvt_vobj_destroy(xdWindow);
		}
		break;
	case E_CONTROL:
		{
		switch(xdControlId) {

		case DLG_168_PUNKT:
			{ // Checkbox: Punktbefehle
			}
			break;
		case DLG_168_LBOX: /* "List Box 1" */
			{ 	/*			List box was operated.			*/
			if (xdEvent->v.ctl.ci.v.lbox.dbl_click)
			  {  /*		double click			*/
			  }
			else
			  {  /*		single click			*/
			  }
			}
			break;
		case DLG_168_PUSHBUTTON_OK: /* "OK" */
			{
        if( !GetFeature( "wsp_nodemo" ) )
        {
          char buf[200];//Dick 26.11.99
          xvt_res_get_str(STR_DEMO_NOTE_1,buf,sizeof(buf));
          xvt_dm_post_note("%s",buf);
          //xvt_dm_post_note("Bei Demo nicht möglich");
          Dlg168Canceled = TRUE;
          xvt_vobj_destroy(xdWindow);
				break;
        };

			 if (xvt_ctl_is_checked(xvt_win_get_ctl(xdWindow,DLG_168_PUNKT)))
				punktbefehl = TRUE;
			 else punktbefehl = FALSE;
			 if (GetPrListSelectedItems( dlg168_lbox,Dlg168ListSel))
				 Dlg168Canceled = FALSE;
			 else Dlg168Canceled = TRUE;
			 xvt_vobj_destroy(xdWindow);
			}
			break;
		case DLG_168_PUSHBUTTON_CANCEL: /* "Abbrechen" */
			{
			 punktbefehl = FALSE;
			 Dlg168Canceled = TRUE;
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



