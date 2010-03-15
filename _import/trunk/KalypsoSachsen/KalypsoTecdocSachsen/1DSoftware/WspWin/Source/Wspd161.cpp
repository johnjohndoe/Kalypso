/*	Handler for dialog DLG_161 ("Auswahl Zeichnen/nicht zeichnen")*/
#include <windows.h>
#include "xvt.h"
#include "wspwin.h"
#include "resource.h"

#include "global_types.h"
#include "global_defs.h"
#include "..\..\wspdlg\include\export.h"

#include "list.h"

#include "global.h"

#include "wsphilfe.h"

extern XVT_HELP_INFO hi;

#define DLG_RES_ID DLG_161
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODAL

extern int local_ds,
			  art;
extern char local_str[50];
extern struct _DATENSATZOPTIONEN datensatzoptionen;  // ->util.h + wspw130.cpp
extern int local_ds;

WINDOW list1,list2,
	   lb_p1,lb_p2;
SLIST werte;
SLIST_ELT ee1;
char *str107;
int *attribute;
int d161_counter,mem,
	 d161_l1=0,l2=0;
int *pos_list1,*pos_list2;
long popt;
WINDOW WIN161=NULL;

/*	Handler for dialog DLG_161 ("Auswahl Zeichnen/nicht zeichnen")*/

long XVT_CALLCONV1
#if XVT_CC_PROTO
DLG_161_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
DLG_161_eh XVT_CALLCONV2 (xdWindow, xdEvent)
WINDOW xdWindow;
EVENT *xdEvent;
#endif
{
	short xdControlId = xdEvent->v.ctl.id;
/*SLIST werte;
SLIST_ELT ee1;
char *str107;
int *attribute;
int d161_counter,mem,
	 d161_l1=0,l2=0;
int *pos_list1,*pos_list2;
long popt;*/

	switch (xdEvent->type) {
	case E_CREATE:
		{
         char buf[200];//Dick 26.11.99

         WIN161=xdWindow; 
		 werte =xvt_slist_create();

		 lb_p1=xvt_win_get_ctl(xdWindow,DLG_161_PB_NOTPAINT);
		 lb_p2=xvt_win_get_ctl(xdWindow,DLG_161_PB_PAINT);

		 xvt_vobj_set_title(xvt_win_get_ctl(xdWindow,DLG_161_EDIT_10),local_str);

		 if (werte ==NULL)
			xvt_vobj_destroy(xdWindow);


		 attribute = (int*)calloc( ds_info[local_ds]+1,sizeof(int) );

		 if(!list->Get_Plot_Options(local_ds,werte,art,&attribute[0]))
			 {
              //Dick 26.11.99
              xvt_res_get_str(STR_MEM_OUT,buf,sizeof(buf));
              xvt_dm_post_error("%s",buf);
			  //xvt_dm_post_error("Nicht genügend Speicher, um Anwendung auszuführen.");
			  xvt_vobj_destroy(xdWindow);
			 }
		 mem  = xvt_slist_count(werte);
		 pos_list1 = (int*)calloc(mem,sizeof(int));
		 pos_list2 = (int*)calloc(mem,sizeof(int));
		 if ((pos_list1==NULL)||(pos_list2==NULL)||(attribute ==NULL))
			 {
              //Dick 26.11.99
              xvt_res_get_str(STR_MEM_OUT,buf,sizeof(buf));
              xvt_dm_post_error("%s",buf);
			  //xvt_dm_post_error("Nicht genügend Speicher, um Anwendung auszuführen.");
			  xvt_vobj_destroy(xdWindow);
			 }


		 list1 =xvt_win_get_ctl(xdWindow,DLG_161_LBOX_1);
		 list2 =xvt_win_get_ctl(xdWindow,DLG_161_LBOX_4);

		 if (art==STATIONSWERTE)
             {
			  //xvt_vobj_set_title(xvt_win_get_ctl(xdWindow,DLG_161_EDIT_12),"y -Werte");
             xvt_vobj_set_title(xvt_win_get_ctl(xdWindow,DLG_161_EDIT_12),xvt_res_get_str(STR_Y_WERTE,buf,sizeof(buf)));
             }
		 else if (art == HOEHENWERTE)
             {
			  //xvt_vobj_set_title(xvt_win_get_ctl(xdWindow,DLG_161_EDIT_12),"Höhenwerte");
              xvt_vobj_set_title(xvt_win_get_ctl(xdWindow,DLG_161_EDIT_12),xvt_res_get_str(STR_HOEHENWERTE,buf,sizeof(buf)));
             }

		 d161_counter =0;
		 d161_l1 = l2 =0;
		 for (ee1=xvt_slist_get_first(werte);ee1!=NULL;ee1=xvt_slist_get_next(werte,ee1))
			 {
				str107 = xvt_slist_get(werte,ee1,&popt);
				if (attribute[d161_counter])
					{
					 xvt_list_add(list2,-1,str107);
					 pos_list2[l2] = d161_counter;
					 l2++;
					}
				else
					{
					 xvt_list_add(list1,-1,str107);
					 pos_list1[d161_l1] = d161_counter;
					 d161_l1++;
					}
			  d161_counter++;
			 }

		 xvt_list_set_sel(list1,-1,FALSE);
		 xvt_list_set_sel(list2,-1,FALSE);
		 if (hi!=NULL_HELP_INFO)
			xvt_help_set_win_assoc(hi, xdWindow, HID_KAPITEL_6_6, 0L);
		}
		break;
	case E_DESTROY:
		{
		 free(pos_list1);
		 free(pos_list2);
		 free(attribute);
		 xvt_slist_destroy(werte);
         WIN161=NULL;
		}
		break;
	case E_FOCUS:
		{
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
		case DLG_161_PB_OK: /* "OK" */
			{
			 list->Save_Plot_Options(local_ds,werte,art,&attribute[0]);
			 xvt_vobj_destroy(xdWindow);
			}
			break;
		case DLG_161_PB_CANCEL: /* "Abbrechen" */
			{
			 xvt_vobj_destroy(xdWindow);
			}
			break;
		case DLG_161_PB_NOTPAINT:  /* " nicht zeichnen -->" */
			{
			 for (int i=0;i<d161_l1;i++)
				if (xvt_list_is_sel(list1,i))
					attribute[pos_list1[i]] =1;
			 xvt_list_clear(list2);
			 xvt_list_clear(list1);

			 for (i=0;i<mem;i++)
				 pos_list1[i]=pos_list2[i]=0;

			 d161_l1=l2=0;
			 d161_counter=0;
			 for (ee1=xvt_slist_get_first(werte);ee1!=NULL;ee1=xvt_slist_get_next(werte,ee1))
			 {
				str107 = xvt_slist_get(werte,ee1,&popt);
				if (attribute[d161_counter])
					{
					 xvt_list_add(list2,-1,str107);
					 pos_list2[l2] = d161_counter;
					 l2++;
					}
				else
					{
					 xvt_list_add(list1,-1,str107);
					 pos_list1[d161_l1] = d161_counter;
					 d161_l1++;
					}
			  d161_counter++;
			 }
			 xvt_list_suspend(list1);
			 xvt_list_suspend(list2);
			 xvt_list_resume(list1);
			 xvt_list_resume(list2);
			 xvt_vobj_set_enabled(lb_p1,FALSE);
			 SaveProfilFile = TRUE;
			}
			break;
		case DLG_161_PB_PAINT:  /* "<-- zeichnen" */
			{
			 for (int i=0;i<l2;i++)
				if (xvt_list_is_sel(list2,i))
					attribute[pos_list2[i]] =0;
			 xvt_list_clear(list2);
			 xvt_list_clear(list1);

			 for (i=0;i<mem;i++)
				 pos_list1[i]=pos_list2[i]=0;

			 d161_l1=l2=0;
			 d161_counter=0;
			 for (ee1=xvt_slist_get_first(werte);ee1!=NULL;ee1=xvt_slist_get_next(werte,ee1))
			 {
				str107 = xvt_slist_get(werte,ee1,&popt);
				if (attribute[d161_counter])
					{
					 xvt_list_add(list2,-1,str107);
					 pos_list2[l2] = d161_counter;
					 l2++;
					}
				else
					{
					 xvt_list_add(list1,-1,str107);
					 pos_list1[d161_l1] = d161_counter;
					 d161_l1++;
					}
			  d161_counter++;
			 }

			 xvt_list_suspend(list1);
			 xvt_list_suspend(list2);
			 xvt_list_resume(list1);
			 xvt_list_resume(list2);
			 xvt_vobj_set_enabled(lb_p2,FALSE);
			 SaveProfilFile = TRUE;
			}
			break;

		case DLG_161_LBOX_1: /* "List Box 1" */
			{	/*	List box was operated.	*/
			if (xdEvent->v.ctl.ci.v.lbox.dbl_click)
			  {	/*	double click	*/
			  }
			else
			  {	/*	single click	*/
				if(xvt_list_count_sel(list1))
				  xvt_vobj_set_enabled(lb_p1,TRUE);
				else  xvt_vobj_set_enabled(lb_p1,FALSE);

			  }
			}
			break;
		case DLG_161_LBOX_4: /* "List Box 4" */
			{	/*	List box was operated.	*/
			if (xdEvent->v.ctl.ci.v.lbox.dbl_click)
			  {	/*	double click	*/
			  }
			else
			  {	/*	single click	*/
				if(xvt_list_count_sel(list2))
				  xvt_vobj_set_enabled(lb_p2,TRUE);
				else  xvt_vobj_set_enabled(lb_p2,FALSE);
			  }
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
