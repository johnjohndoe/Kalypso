#include <windows.h>
#include "xvt.h"
#include "resource.h"

#include "global_types.h"
#include "..\..\wspdlg\include\export.h"

#include "plot.h"

#include "wspwin.h"		// GHJ

#include "wsphilfe.h"
#include "bce_allg.h"

extern XVT_HELP_INFO hi;

#define DLG_RES_ID PLOT_101
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODAL



extern BOOLEAN data_changed,
		 dlg_cancel ;
LINE15 line_15;
WINDOW lb101[4],
		 edit_pl101[8],
		 check101_w,
		 check101_b;
extern SLIST header_profil;
extern BOOLEAN SaveProfilFile;
WINDOW PLOT101=NULL;

long XVT_CALLCONV1
#if XVT_CC_PROTO
PLOT_101_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
PLOT_101_eh XVT_CALLCONV2 (xdWindow, xdEvent)
WINDOW xdWindow;
EVENT *xdEvent;
#endif
{
	short xdControlId = xdEvent->v.ctl.id;

	switch (xdEvent->type) {
	case E_CREATE:
		{
          PLOT101=xdWindow;
		  data_changed=FALSE;
		  char temp[15];
		  temp[1]='\0';
		  lb101[0]=xvt_win_get_ctl(xdWindow,PLOT_101_LB_27);
		  lb101[1]=xvt_win_get_ctl(xdWindow,PLOT_101_LB_28);
		  lb101[2]=xvt_win_get_ctl(xdWindow,PLOT_101_LB_12);
		  lb101[3]=xvt_win_get_ctl(xdWindow,PLOT_101_LB_26);

		  edit_pl101[0]=xvt_win_get_ctl(xdWindow,PLOT_101_EDIT_8);
		  edit_pl101[1]=xvt_win_get_ctl(xdWindow,PLOT_101_EDIT_9);
		  edit_pl101[2]=xvt_win_get_ctl(xdWindow,PLOT_101_EDIT_10);
		  edit_pl101[3]=xvt_win_get_ctl(xdWindow,PLOT_101_EDIT_20);
		  edit_pl101[4]=xvt_win_get_ctl(xdWindow,PLOT_101_EDIT_21);
		  edit_pl101[5]=xvt_win_get_ctl(xdWindow,PLOT_101_EDIT_22);
		  edit_pl101[6]=xvt_win_get_ctl(xdWindow,PLOT_101_EDIT_23);
		  edit_pl101[7]=xvt_win_get_ctl(xdWindow,PLOT_101_EDIT_24);

		  check101_w=xvt_win_get_ctl(xdWindow,PLOT_101_CHECKBOX_14);
		  check101_b=xvt_win_get_ctl(xdWindow,PLOT_101_CHECKBOX_15);


		  get_line_15(&line_15,header_profil);    // Zeile 15 in:  struct LINE15 lesen

		  xvt_list_add(lb101[0],0,"2.00");
		  xvt_list_add(lb101[0],1,"3.00");
		  xvt_list_add(lb101[0],2,"4.00");
		  xvt_list_add(lb101[0],3,"5.00");
		  xvt_list_add(lb101[0],4,"6.00");
		  if (strlen(line_15.dist_schriftfeld)>0)
			  xvt_vobj_set_title(lb101[0],line_15.dist_schriftfeld);
		  else
			  xvt_vobj_set_title(lb101[0],"2.00");

		  xvt_list_add(lb101[1],0,"42.00");
		  xvt_list_add(lb101[1],1,"45.00");
		  xvt_list_add(lb101[1],2,"50.00");
		  xvt_list_add(lb101[1],3,"55.00");
		  xvt_list_add(lb101[1],4,"60.00");
		  if (strlen(line_15.paperlength)>0)
			  xvt_vobj_set_title(lb101[1],line_15.paperlength);
		  else
			  xvt_vobj_set_title(lb101[1],"42.00");

		  xvt_vobj_set_title(edit_pl101[0],line_15.bezugshoehe_NN);
		  xvt_vobj_set_title(edit_pl101[1],line_15.x_mass);
		  xvt_vobj_set_title(edit_pl101[2],line_15.y_mass);

		  xvt_list_add(lb101[2],0,"29.70");
		  xvt_list_add(lb101[2],1,"42.00");
		  if (strlen(line_15.paperhigh)>0)
			  xvt_vobj_set_title(lb101[2],line_15.paperhigh);
		  else
			  xvt_vobj_set_title(lb101[2],"29.70");

		  temp[0]=line_15.u_schriftfeld;  temp[1]='\0';
		  xvt_vobj_set_title(lb101[3],temp);
		  xvt_list_add(lb101[3],0,"0");
		  xvt_list_add(lb101[3],1,"1");
		  xvt_list_add(lb101[3],2,"2");
		  xvt_list_add(lb101[3],3,"3");
		  xvt_list_add(lb101[3],4,"4");
		  xvt_list_add(lb101[3],5,"5");

		  temp[0]=line_15.schriftfeldzeilen[0];
		  xvt_vobj_set_title(edit_pl101[3],temp);

		  temp[0]=line_15.schriftfeldzeilen[1];
		  xvt_vobj_set_title(edit_pl101[4],temp);

		  temp[0]=line_15.schriftfeldzeilen[2];
		  xvt_vobj_set_title(edit_pl101[5],temp);

		  temp[0]=line_15.schriftfeldzeilen[3];
		  xvt_vobj_set_title(edit_pl101[6],temp);

		  temp[0]=line_15.schriftfeldzeilen[4];
		  xvt_vobj_set_title(edit_pl101[7],temp);

      if (line_15.u_werte-48 == 1 )
        xvt_ctl_set_checked(check101_w, !xvt_ctl_is_checked (check101_w));
      else
        line_15.u_werte = '0';

      if (line_15.u_bzghoehe-48 == 1 )
        xvt_ctl_set_checked(check101_b, !xvt_ctl_is_checked (check101_b));
      else
        line_15.u_bzghoehe = '0';


		  ChangeFontAndSize((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW));	// GHJ
		  xvt_vobj_set_visible(xdWindow, TRUE);			// GHJ
          if (hi!=NULL_HELP_INFO)
             {
              xvt_help_set_win_assoc(hi, xdWindow, HID_KAPITEL_6_7_1_1_3, 0L);
             }
		}
		break;
	case E_DESTROY:
		{
         PLOT101=NULL;
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
		 case PLOT_101_PB_OK: /* "OK" */
		 {
         int error=FALSE;
			if (data_changed)
			{
			 int ret;
			 xvt_vobj_get_title(lb101[0],line_15.dist_schriftfeld,14);
			 ret = is_zahl(line_15.dist_schriftfeld);
			 if (ret<=0)
				{
				 error =TRUE;
				 xvt_vobj_set_title(lb101[0],"\0");
				 //xvt_dm_post_note("Falsche Zahleneingabe !");
                 char buf[200];//Dick 26.11.99
                 xvt_res_get_str(STR_REAL_FALSE,buf,sizeof(buf));
                 xvt_dm_post_note("%s",buf);
				}
			 xvt_vobj_get_title(lb101[1],line_15.paperlength,14);
			 ret = is_zahl(line_15.paperlength);
			 if (ret<=0)
				{
				 error =TRUE;
				 xvt_vobj_set_title(lb101[1],"\0");
				 //xvt_dm_post_note("Falsche Zahleneingabe !");
                 char buf[200];//Dick 26.11.99
                 xvt_res_get_str(STR_REAL_FALSE,buf,sizeof(buf));
                 xvt_dm_post_note("%s",buf);
				}

			 xvt_vobj_get_title(edit_pl101[0],line_15.bezugshoehe_NN,14);
			 ret = is_zahl(line_15.bezugshoehe_NN);
			 if (ret<=0)
				{
				 error =TRUE;
				 xvt_vobj_set_title(edit_pl101[0],"\0");
				 //xvt_dm_post_note("Falsche Zahleneingabe !");
                 char buf[200];//Dick 26.11.99
                 xvt_res_get_str(STR_REAL_FALSE,buf,sizeof(buf));
                 xvt_dm_post_note("%s",buf);
				}
			 xvt_vobj_get_title(edit_pl101[1],line_15.x_mass,14);
			 ret = is_zahl(line_15.x_mass);
			 if (ret<=0)
				{
				 error =TRUE;
				 xvt_vobj_set_title(edit_pl101[1],"\0");
				 //xvt_dm_post_note("Falsche Zahleneingabe !");
                 char buf[200];//Dick 26.11.99
                 xvt_res_get_str(STR_REAL_FALSE,buf,sizeof(buf));
                 xvt_dm_post_note("%s",buf);
				}
			 xvt_vobj_get_title(edit_pl101[2],line_15.y_mass,14);
			 ret = is_zahl(line_15.y_mass);
			 if (ret<=0)
				{
				 error =TRUE;
				 xvt_vobj_set_title(edit_pl101[2],"\0");
				 //xvt_dm_post_note("Falsche Zahleneingabe !");
                 char buf[200];//Dick 26.11.99
                 xvt_res_get_str(STR_REAL_FALSE,buf,sizeof(buf));
                 xvt_dm_post_note("%s",buf);
				}
			 xvt_vobj_get_title(lb101[2],line_15.paperhigh,14);
			 ret = is_zahl(line_15.paperhigh);
			 if (ret<=0)
				{
				 error =TRUE;
				 xvt_vobj_set_title(lb101[2],"\0");
				 //xvt_dm_post_note("Falsche Zahleneingabe !");
                 char buf[200];//Dick 26.11.99
                 xvt_res_get_str(STR_REAL_FALSE,buf,sizeof(buf));
                 xvt_dm_post_note("%s",buf);
				}

			 char anzahl[15];
			 xvt_vobj_get_title(lb101[3],anzahl,2);  //Unterteilung Schriftfeld
       if( anzahl[0] == '\0' || anzahl[0] == ' ' )
         line_15.u_schriftfeld = '0';
       else
         line_15.u_schriftfeld=anzahl[0];

			 line_15.schriftfeldzeilen[0]=
			 line_15.schriftfeldzeilen[1]=
			 line_15.schriftfeldzeilen[2]=
			 line_15.schriftfeldzeilen[3]=
			 line_15.schriftfeldzeilen[4]= ' ';

       switch(atoi(anzahl))
       {
       case 5:
         xvt_vobj_get_title(edit_pl101[7],anzahl,2);
         line_15.schriftfeldzeilen[4]=anzahl[0];
         
       case 4:
         xvt_vobj_get_title(edit_pl101[6],anzahl,2);
         line_15.schriftfeldzeilen[3]=anzahl[0];
         
       case 3:
         xvt_vobj_get_title(edit_pl101[5],anzahl,2);
         line_15.schriftfeldzeilen[2]=anzahl[0];
         
       case 2:
         xvt_vobj_get_title(edit_pl101[4],anzahl,2);
         line_15.schriftfeldzeilen[1]=anzahl[0];
         
       case 1:
         xvt_vobj_get_title(edit_pl101[3],anzahl,2);
         line_15.schriftfeldzeilen[0]=anzahl[0];
         /* !!   keine break's   !!!  */
       }
			 save_line_15(&line_15);
			 SaveProfilFile =TRUE;
			 }
			if (!error)
			  xvt_vobj_destroy(xdWindow);
		  }
		  break;
		case PLOT_101_PB_CANCEL: /* "Abbrechen" */
			{
			 dlg_cancel =TRUE;
			 if (data_changed)
				{
         char buf[200],buf2[200],buf3[200],buf4[200];//Dick 26.11.99
         
         xvt_res_get_str(STR_JA,buf,sizeof(buf));
         xvt_res_get_str(STR_NEIN,buf2,sizeof(buf2));
         xvt_res_get_str(STR_ZURUECK,buf3,sizeof(buf3));
         xvt_res_get_str(STR_PLOT101_ASK,buf4,sizeof(buf4));
         switch(xvt_dm_post_ask(buf,buf2,buf3,buf4))
         {
         case RESP_DEFAULT:
           {
             xdEvent->type =E_CONTROL;
             xdEvent->v.ctl.id=PLOT_101_PB_OK;  // OK-Button
             xvt_win_dispatch_event(xdWindow,xdEvent);
           }
         case RESP_2:
           xvt_vobj_destroy(xdWindow);
           break;
         case RESP_3:
           dlg_cancel=FALSE;
           break;
         }
       }
       else
				xvt_vobj_destroy(xdWindow);
			}
			break;
		case PLOT_101_CHECKBOX_14: /* "Unterdücken von Werten" */
			{
			 xvt_ctl_set_checked(check101_w, !xvt_ctl_is_checked (check101_w));
       if( xvt_ctl_is_checked (check101_w))
         line_15.u_werte = '1';
			 else 
         line_15.u_werte = '0';
			 data_changed=TRUE;
			}
			break;
		case PLOT_101_CHECKBOX_15: /* "Unterdrücken der Bezugshöhe" */
			{
			 xvt_ctl_set_checked(check101_b, !xvt_ctl_is_checked (check101_b));
			 if( xvt_ctl_is_checked (check101_b))
				line_15.u_bzghoehe = '1';
			 else line_15.u_bzghoehe = '0';
			 data_changed=TRUE;
			}
			break;
		case PLOT_101_LB_27: /* "List Edit 27" */
			{
			/*
				Listedit control was operated.
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
			 data_changed=TRUE;
			}
			}
			break;
		case PLOT_101_LB_28: /* "List Edit 28" */
			{
			/*
				Listedit control was operated.
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
			 data_changed=TRUE;
			}
			}
			break;
		case PLOT_101_EDIT_8:			{
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
			 data_changed=TRUE;
			}
			}
			break;
		case PLOT_101_EDIT_9:			{
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
			 data_changed=TRUE;
			}
			}
			break;
		case PLOT_101_EDIT_10:			{
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
			 data_changed=TRUE;
			}
			}
			break;
		case PLOT_101_LB_12:			{
			/*
				Listedit control was operated.
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
			 data_changed=TRUE;
			}
			}
			break;
		case PLOT_101_EDIT_20:			{
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
			 data_changed=TRUE;
			}
			}
			break;
		case PLOT_101_EDIT_21:			{
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
			 data_changed=TRUE;
			}
			}
			break;
		case PLOT_101_EDIT_22:			{
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
			 data_changed=TRUE;
			}
			}
			break;
		case PLOT_101_EDIT_23:			{
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
			 data_changed=TRUE;
			}
			}
			break;
		case PLOT_101_EDIT_24:			{
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
			 data_changed=TRUE;
			}
			}
			break;
		case PLOT_101_LB_26: /* "List Button 26" */
			{
			 data_changed=TRUE;
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
		/*			Application initiated.		*/
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
    xvt_tx_process_event(xdWindow, xdEvent);
	return 0L;

}
