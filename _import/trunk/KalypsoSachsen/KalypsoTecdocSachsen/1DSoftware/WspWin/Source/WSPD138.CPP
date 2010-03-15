#include <windows.h>
#include "xvt.h"
#include "wspwin.h"

#include "global_defs.h"

#define DLG_RES_ID DLG_138
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODELESS

extern WINDOW id_dlg138;
WINDOW lwin_dlg138;
extern char  profilstr[100];
extern char  dateiname[15];
extern int pos;
SLIST dlg138_list;
extern SLIST prof_datei;
BOOLEAN dlg138_abbruch=FALSE;
/* Bitte eine Profildatei aus Profiltabelle  (Aufruf in: DLG136) auswählen */
/*	Handler for dialog DLG_138  */
long XVT_CALLCONV1
#if XVT_CC_PROTO
DLG_138_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
DLG_138_eh XVT_CALLCONV2 (xdWindow, xdEvent)
WINDOW xdWindow;
EVENT *xdEvent;
#endif
{
	short xdControlId = xdEvent->v.ctl.id;

	switch (xdEvent->type) {
	case E_CREATE:
		{
         int j=0;
         char *str;
         char strtemp[150];
         char temp[10];
         double s1,s2;
         int count=0;
		 id_dlg138 = xdWindow;
         lwin_dlg138= xvt_win_get_ctl(xdWindow,DLG_138_LISTBOX_1);
         /*if (prof_datei != NULL)
				  xvt_list_add(lwin_dlg138, -1, (char*)prof_datei);   //Ausgabe in Listbox
                  */
         if (prof_datei != NULL)
             {
              count = xvt_slist_count(prof_datei);
              dlg138_list=xvt_slist_create();
             while (j<count)
                 {
                 str = xvt_slist_get_elt(prof_datei, j, 0);
                 strcpy(strtemp,str);
                 for (int i=0;i<=7;i++)
                     temp[i]=strtemp[10+i];
                 temp[8]='\0';
                 s1 = atof(temp);
                 if(strlen(profilstr)>0)
                     s2 = atof(profilstr);
                 else
                     s2=BCE_NAN;
                 if (s1 == s2)
                     {
                      xvt_slist_add_at_elt(dlg138_list,NULL,strtemp,(long)(j+1));
                     }
                 j++;
                 }
             if (dlg138_list != NULL)
				  xvt_list_add(lwin_dlg138, -1, (char*)dlg138_list);   //Ausgabe in Listbox
              xvt_list_set_sel(lwin_dlg138,0,TRUE);
             
             }
		}
		break;
	case E_DESTROY:
		{
		 id_dlg138 = NULL_WIN;
         if (dlg138_list != NULL)
             {
              xvt_slist_destroy(dlg138_list);
              dlg138_list=NULL;
             }
		}
		break;
	case E_FOCUS:
		{
		/*  	Dialog has lost or gained focus.  */
		if (xdEvent->v.active)  {
			/*  	Dialog has gained focus		*/
		} else {
			/*		Dialog has lost focus	*/
		}
		}
		break;
	case E_CLOSE:
		{
         dlg138_abbruch=TRUE;
		xvt_vobj_destroy(xdWindow);
		}
		break;
	case E_CONTROL:
		/*		User operated control in dialog.		*/
		{
		switch(xdControlId) {
		case DLG_138_PUSHBUTTON_1: /* "Abbrechen" */
			{
             dlg138_abbruch=TRUE;
			 xvt_vobj_destroy(xdWindow);
			 
			}
			break;
        case DLG_138_PUSHBUTTON_2: /* "OK" */
			{
            char temp[100];
            char *str;
            long dlg138_data;
            int col=xvt_list_get_sel_index(lwin_dlg138);
            if(col>=0)
                {
                 SLIST dummy=xvt_list_get_sel(lwin_dlg138);
                str=xvt_slist_get_elt(dummy,0,0);
                strcpy(temp,str);
                str=xvt_slist_get_elt(dlg138_list,col,&dlg138_data);
                    
                pos=(int)dlg138_data-1;
                
                //xvt_list_get_elt(lwin_dlg138,xvt_list_get_sel_index(lwin_dlg138),temp,100);                
                for (int i=0;i<12;i++)
                    dateiname[i]=temp[i+44];
                dateiname[12]='\0';
                dlg138_abbruch=FALSE;
                }
            else
                dlg138_abbruch=TRUE;
             
			 xvt_vobj_destroy(xdWindow);
			 
			}
			break;
        case DLG_138_LISTBOX_1: /* "Listbox" */
			{
			 /*	List box was operated.*/
			 if (xdEvent->v.ctl.ci.v.lbox.dbl_click)
				{ 	/*			double click			*/
				}
			 else
				{   /*		single click			*/
                 }
			}
			break;
		}
		break;
	default:
		break;
	}
	}
	return 0L;
}
