#include <windows.h>
#include "xvt.h"
#include "wspwin.h"

#include "..\..\wspdlg\include\export.h"

#include "util2.h"
#include "resource.h"
/*
	Information about the dialog
*/
#define DLG_RES_ID DLG_204
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODELESS

extern SLIST batch_list;
extern FILE_SPEC ber_spec;
extern BOOLEAN new_batch;
extern long timer_204;
extern WINDOW DLG204, DLG203;
extern int wsplist;

BOOLEAN destroy_DLG203=FALSE,
		  batch_list_offen=FALSE,
		  start_berechtigt=FALSE,
		  timer_an=FALSE,
          Save_Laengsschnitt=FALSE;
//WINDOW listbox204;
char *batch;
SLIST_ELT ebatch;
SLIST helplist204;
int batch_remove;


/*
	Handler for dialog DLG_204 ("Dialog 204")
*/
long XVT_CALLCONV1
#if XVT_CC_PROTO
DLG_204_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
DLG_204_eh XVT_CALLCONV2 (xdWindow, xdEvent)
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

		  DLG204=xdWindow;
		  new_batch=FALSE;
		  //listbox204 = xvt_win_get_ctl (xdWindow,DLG_204_LBOX_1);

		 xvt_list_add((xvt_win_get_ctl (xdWindow,DLG_204_LBOX_1)), -1, (char *)batch_list);   // Ausgabe in LISTBOX
		 xvt_list_set_sel((xvt_win_get_ctl (xdWindow,DLG_204_LBOX_1)), 0, TRUE);
		 if ((helplist204 = xvt_slist_create())==NULL)
			  xvt_dm_post_fatal_exit("SLIST Create Error -wspd203");
		 helplist204=xvt_list_get_sel(xvt_win_get_ctl (xdWindow,DLG_204_LBOX_1));
		 batch=xvt_slist_get_elt(helplist204,0,0);

         switch(xvt_vobj_get_data(xdWindow))
             {
              case 1L:
                  xvt_vobj_set_title(xvt_win_get_ctl (xdWindow,DLG_204_PUSHBUTTON_3),"Vergleich starten");
                  break;
             }

		}
		break;
	case E_DESTROY:
		/*
			Dialog has been closed; last event sent to dialog.
		*/
		{
			if (timer_an)
			{
			xvt_timer_destroy(timer_204);
			timer_an=FALSE;
			}

		}
		break;
	case E_FOCUS:
		{
		/*
			Dialog has lost or gained focus.
		*/
		if (xdEvent->v.active)  // Dialog has gained focus
		  {

		  }
		else {
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
		new_batch=TRUE;
        if (batch_list!=NULL)//Dick 5.03.99
		  {
			xvt_slist_destroy(batch_list);
            batch_list=NULL;
		  }
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
		case DLG_204_LBOX_1: /* "List Box 1" */
			{
			/*
				List box was operated.
			*/
			if (xdEvent->v.ctl.ci.v.lbox.dbl_click) {
				/*
					double click
				*/
			} else
			 {
			 /*single click */

			 helplist204=xvt_list_get_sel(xvt_win_get_ctl (xdWindow,DLG_204_LBOX_1));
			 batch=xvt_slist_get_elt(helplist204,0,0);
			 }
			}
			break;
		case DLG_204_PUSHBUTTON_3: /* "OK" */
			{
        char buf[200];//Dick 26.11.99
        if( !GetFeature( "wsp_nodemo" ) )
        {
          xvt_res_get_str(STR_DEMO_NOTE_2,buf,sizeof(buf));
          xvt_dm_post_note("%s",buf);
          //xvt_dm_post_note("Berechnungen bei Demo nicht möglich");
          if (batch_list!=NULL)
          {
            xvt_slist_destroy(batch_list);
            batch_list=NULL;
          }
          if(DLG203!=NULL_WIN)
            xvt_vobj_destroy(DLG203);
          xvt_vobj_destroy(xdWindow);
          new_batch=TRUE;
          destroy_DLG203=TRUE;
          break;
        };

			int x=xvt_slist_count(batch_list);
			if (x!=0)
				start_berechtigt=TRUE;
			new_batch=TRUE;
			batch_list_offen=TRUE;
			if (helplist204!=NULL)
			{
			 xvt_slist_destroy(helplist204);
			 helplist204=NULL;
			}
      if (!start_berechtigt)
      {
        xvt_res_get_str(STR_DEMO_NOTE_2,buf,sizeof(buf));
        xvt_dm_post_error("%s",buf);
        //xvt_dm_post_error("Sie haben keine Berechnungsdatei ausgewählt!");
      }
      else
      {
        if((wsplist!=1) && (wsplist!=2))
        {
          Save_Laengsschnitt=TRUE;
          schreibe_bat();
        }	
        batch_list_offen=FALSE;
        start_berechtigt=FALSE;
      }//else d.h. start_berechtigt

			if(DLG203!=NULL_WIN)
				xvt_vobj_destroy(DLG203);
			xvt_vobj_destroy(xdWindow);

			destroy_DLG203=TRUE;


			}
			break;
		case DLG_204_PUSHBUTTON_4: /* "LÖSCHEN" */
			{
			 helplist204=xvt_list_get_sel(xvt_win_get_ctl (xdWindow,DLG_204_LBOX_1));
			 batch_remove=xvt_list_get_sel_index(xvt_win_get_ctl (xdWindow,DLG_204_LBOX_1));
			 ebatch=xvt_slist_get_first(batch_list);
			 for (int h=1;h<=batch_remove;h++)
				ebatch=xvt_slist_get_next(batch_list,ebatch);
			 xvt_slist_rem(batch_list,ebatch);
			 timer_204=xvt_timer_create(DLG204,10);

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
		xvt_list_clear(xvt_win_get_ctl (xdWindow,DLG_204_LBOX_1));
		int slist_count=xvt_slist_count(batch_list);
		if (slist_count>0)
		 {
			xvt_list_add((xvt_win_get_ctl (xdWindow,DLG_204_LBOX_1)), -1, (char *)batch_list);   // Ausgabe in LISTBOX
			xvt_list_set_sel((xvt_win_get_ctl (xdWindow,DLG_204_LBOX_1)), 0, TRUE);
			helplist204=xvt_list_get_sel(xvt_win_get_ctl (xdWindow,DLG_204_LBOX_1));
			batch=xvt_slist_get_elt(helplist204,0,0);
			xvt_timer_destroy(timer_204);

		 }
		else
		 {
		  new_batch=TRUE;
		  if (batch_list!=NULL)
		  {
			xvt_slist_destroy(batch_list);
         batch_list=NULL;
		  }
		  batch_list_offen=FALSE;
		  if (helplist204!=NULL)
		  {
			xvt_slist_destroy(helplist204);
         helplist204=NULL;
		  }
		  timer_an=TRUE;
		  xvt_vobj_destroy(xdWindow);
		  start_berechtigt=FALSE;
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
