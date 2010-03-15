#include <windows.h>
#include "xvt.h"
#include "plot.h"

#include "wspwin.h"		// GHJ
#include "resource.h"
#include "..\..\wspdlg\include\export.h"

#include "wsphilfe.h"

extern XVT_HELP_INFO hi;

#define DLG_RES_ID PLOT_100
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODAL


WINDOW lb[9],
		 edit_pl100[7];
extern WINDOW WIN_LINES,WIN121;
extern BOOLEAN data_changed,
		 dlg_cancel ;
int querfelder,
	 textzeilen[3],
	 schriftfeldzeilen[3];
LINE12 line12;
extern BOOLEAN SaveProfilFile;
WINDOW PLOT100=NULL;		

long XVT_CALLCONV1
#if XVT_CC_PROTO
PLOT_100_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
PLOT_100_eh XVT_CALLCONV2 (xdWindow, xdEvent)
WINDOW xdWindow;
EVENT *xdEvent;
#endif
{
	short xdControlId = xdEvent->v.ctl.id;

	switch (xdEvent->type) {
	case E_CREATE:
		{
          char buf[200];
          PLOT100=xdWindow;
		  data_changed=FALSE;
		  if (WIN121 != NULL_WIN)
			  xvt_vobj_set_visible(WIN121,FALSE);

		  lb[0]=xvt_win_get_ctl(xdWindow,PLOT_100_LB_9);
		  lb[1]=xvt_win_get_ctl(xdWindow,PLOT_100_LB_10);
		  lb[2]=xvt_win_get_ctl(xdWindow,PLOT_100_LB_31);
		  lb[3]=xvt_win_get_ctl(xdWindow,PLOT_100_LB_32);
		  lb[4]=xvt_win_get_ctl(xdWindow,PLOT_100_LB_33);
		  lb[5]=xvt_win_get_ctl(xdWindow,PLOT_100_LB_34);
		  lb[6]=xvt_win_get_ctl(xdWindow,PLOT_100_LB_35);
		  lb[7]=xvt_win_get_ctl(xdWindow,PLOT_100_LB_36);
		  lb[8]=xvt_win_get_ctl(xdWindow,PLOT_100_LB_37);

		  edit_pl100[0]=xvt_win_get_ctl(xdWindow,PLOT_100_EDIT_4);  //Blatt-Anlage
		  edit_pl100[1]=xvt_win_get_ctl(xdWindow,PLOT_100_EDIT_25); //Text Querfelder
		  edit_pl100[2]=xvt_win_get_ctl(xdWindow,PLOT_100_EDIT_26);
		  edit_pl100[3]=xvt_win_get_ctl(xdWindow,PLOT_100_EDIT_27);
		  edit_pl100[4]=xvt_win_get_ctl(xdWindow,PLOT_100_EDIT_28);
		  edit_pl100[5]=xvt_win_get_ctl(xdWindow,PLOT_100_EDIT_29);
		  edit_pl100[6]=xvt_win_get_ctl(xdWindow,PLOT_100_EDIT_30);

		  get_line_12(&line12);    // Zeile 12 in:  struct LINE12 lesen
          //Dick 30.09.99 
          char *temp_ch;          
          temp_ch=&line12.anlage[0];
          while(temp_ch[0]==' ')
              temp_ch=&temp_ch[1];
          if(temp_ch[0]=='\0')
              line12.anlage[0]='\0';
          else
              strcpy(line12.anlage,temp_ch);
          if(line12.anlage[0]=='\0')  //Dick 29.09.99
              strcpy(line12.anlage,"B-");
          //
		  xvt_vobj_set_title(edit_pl100[0],line12.anlage);

          
		  /*xvt_list_add(lb[0],0,"Dezimal mit 2 Stellen");    //Stationsbeschriftung
		  xvt_list_add(lb[0],1,"Betrag ohne Dezimalstellen");
		  xvt_list_add(lb[0],2,"Betrag mit 2 Dezimalstellen");
          */
          xvt_res_get_str(STR_PLOT100_STAT_1,buf,sizeof(buf));
          xvt_list_add(lb[0],0,buf);
          xvt_res_get_str(STR_PLOT100_STAT_2,buf,sizeof(buf));
          xvt_list_add(lb[0],1,buf);
          xvt_res_get_str(STR_PLOT100_STAT_3,buf,sizeof(buf));
		  xvt_list_add(lb[0],2,buf);
		  xvt_list_set_sel(lb[0],line12.stationsbeschr,TRUE);


		  /*xvt_list_add(lb[1],0,"kein Antragsteller- und Änderungsschild / keine Legende");    //Antagsteller
		  xvt_list_add(lb[1],1,"Antragsteller- und Änderungsschild /keine Legende");
		  xvt_list_add(lb[1],2,"nur Antragstellerschild /keine Legende");
		  xvt_list_add(lb[1],3,"nur Änderungsschild /keine Legende");
		  xvt_list_add(lb[1],4,"nur Legende -kein Antragsteller- Änderungsschild");
		  xvt_list_add(lb[1],5,"Antragsteller- Änderungsschild und Legende");
		  xvt_list_add(lb[1],6,"nur Antragstellerschild mit Legende");
		  xvt_list_add(lb[1],7,"nur Änderungsschild mit Legende");
          */
          xvt_res_get_str(STR_PLOT100_ANTR_1,buf,sizeof(buf));
          xvt_list_add(lb[1],0,buf);    //Antagsteller
          xvt_res_get_str(STR_PLOT100_ANTR_2,buf,sizeof(buf));
		  xvt_list_add(lb[1],1,buf);
          xvt_res_get_str(STR_PLOT100_ANTR_3,buf,sizeof(buf));
		  xvt_list_add(lb[1],2,buf);
          xvt_res_get_str(STR_PLOT100_ANTR_4,buf,sizeof(buf));
		  xvt_list_add(lb[1],3,buf);
          xvt_res_get_str(STR_PLOT100_ANTR_5,buf,sizeof(buf));
		  xvt_list_add(lb[1],4,buf);
          xvt_res_get_str(STR_PLOT100_ANTR_6,buf,sizeof(buf));
		  xvt_list_add(lb[1],5,buf);
          xvt_res_get_str(STR_PLOT100_ANTR_7,buf,sizeof(buf));
		  xvt_list_add(lb[1],6,buf);
          xvt_res_get_str(STR_PLOT100_ANTR_8,buf,sizeof(buf));
		  xvt_list_add(lb[1],7,buf);

		  xvt_list_set_sel(lb[1],line12.antragsteller,TRUE);


		  xvt_list_add(lb[2],0,"0"); //Anzahl Querfelder
		  xvt_list_add(lb[2],1,"1");
		  xvt_list_add(lb[2],2,"2");
		  xvt_list_add(lb[2],3,"3");
		  xvt_list_set_sel(lb[2],line12.querfelder,TRUE);

		  xvt_list_add(lb[3],0,"0");//Schriftfeldzeilen 1
		  xvt_list_add(lb[3],1,"1");
		  xvt_list_add(lb[3],2,"2");
		  xvt_list_add(lb[3],3,"3");
		  xvt_list_set_sel(lb[3],line12.schriftfeldzeilen[0],TRUE);

		  xvt_list_add(lb[4],0,"0");//Schriftfeldzeilen  2
		  xvt_list_add(lb[4],1,"1");
		  xvt_list_add(lb[4],2,"2");
		  xvt_list_add(lb[4],3,"3");
		  xvt_list_set_sel(lb[4],line12.schriftfeldzeilen[1],TRUE);

		  xvt_list_add(lb[5],0,"0");//Schriftfeldzeilen  3
		  xvt_list_add(lb[5],1,"1");
		  xvt_list_add(lb[5],2,"2");
		  xvt_list_add(lb[5],3,"3");
		  xvt_list_set_sel(lb[5],line12.schriftfeldzeilen[2],TRUE);

		  xvt_list_add(lb[6],0,"0");//Textzeilen 1
		  xvt_list_add(lb[6],1,"1");
		  xvt_list_add(lb[6],2,"2");
		  xvt_list_set_sel(lb[6],line12.textzeilen[0],TRUE);

		  xvt_list_add(lb[7],0,"0");//Textzeilen 2
		  xvt_list_add(lb[7],1,"1");
		  xvt_list_add(lb[7],2,"2");
		  xvt_list_set_sel(lb[7],line12.textzeilen[1],TRUE);

		  xvt_list_add(lb[8],0,"0");//Textzeilen 3
		  xvt_list_add(lb[8],1,"1");
		  xvt_list_add(lb[8],2,"2");
		  xvt_list_set_sel(lb[8],line12.textzeilen[2],TRUE);

          
		  for (int i=1;i<=6;i++)
			  xvt_vobj_set_title(edit_pl100[i],line12.text[i-1]);

		 set_options(&lb[0],&edit_pl100[0],&line12);
		  ChangeFontAndSize((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW));	// GHJ
		  xvt_vobj_set_visible(xdWindow, TRUE);		// GHJ
          if( hi != NULL_HELP_INFO )
             {
              xvt_help_set_win_assoc(hi, xdWindow, HID_KAPITEL_6_7_1_1_2, 0L);
             }
		}
		break;
	case E_DESTROY:
		{
         PLOT100=NULL;
		}
		break;
	case E_FOCUS:
		{
		/*
			Dialog has lost or gained focus.
		*/
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
		/*
			Request to close dialog; user operated "close" menu item on
			dialog system menu, or operated "close" control on dialog
			frame. Dialog not closed unless xvt_vobj_destroy is called.
		*/
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
		case PLOT_100_PB_OK: /* "OK" */
		 {
          data_changed=TRUE;//Dick 01.10.99 damit immer speichert
		  if (data_changed)
		  {
			 xvt_vobj_get_title(edit_pl100[0],line12.anlage,12);

			 line12.stationsbeschr =xvt_list_get_sel_index(lb[0]);
			 line12.antragsteller =xvt_list_get_sel_index(lb[1]);

			 line12.querfelder =xvt_list_get_sel_index(lb[2]);
			 switch (line12.querfelder)
				{
				 case 0:
				  line12.schriftfeldzeilen[0]=line12.schriftfeldzeilen[1]=line12.schriftfeldzeilen[2]=0;
				  line12.textzeilen[0] = line12.textzeilen[1] = line12.textzeilen[2] = 0;

				  line12.text[0][0]=line12.text[1][0]=line12.text[2][0]=
				  line12.text[3][0]=line12.text[4][0]=line12.text[5][0]='\0';
				  break;
				 case 1:
				  line12.schriftfeldzeilen[0] =xvt_list_get_sel_index(lb[3]);
				  line12.schriftfeldzeilen[1]=line12.schriftfeldzeilen[2]=0;

				  line12.textzeilen[0] =xvt_list_get_sel_index(lb[6]);
				  line12.textzeilen[1] = line12.textzeilen[2] = 0;

				  if(line12.textzeilen[0]==0)
					line12.text[0][0]=line12.text[1][0]='\0';
				  if(line12.textzeilen[0]==1)
					{xvt_vobj_get_title(edit_pl100[1],line12.text[0],12);
					 line12.text[1][0]='\0';}
				  else
					 {xvt_vobj_get_title(edit_pl100[1],line12.text[0],12);
					  xvt_vobj_get_title(edit_pl100[2],line12.text[1],12);}

				  line12.text[2][0]=line12.text[3][0]=
				  line12.text[4][0]=line12.text[5][0]='\0';
				  break;
				 case 2:
				  line12.schriftfeldzeilen[0] =xvt_list_get_sel_index(lb[3]);
				  line12.schriftfeldzeilen[1] =xvt_list_get_sel_index(lb[4]);
				  line12.schriftfeldzeilen[2]=0;

				  line12.textzeilen[0] =xvt_list_get_sel_index(lb[6]);
				  if(line12.textzeilen[0]==0)
					line12.text[0][0]=line12.text[1][0]='\0';
				  if(line12.textzeilen[0]==1)
					{xvt_vobj_get_title(edit_pl100[1],line12.text[0],12);
					 line12.text[1][0]='\0';}
				  else
					 {xvt_vobj_get_title(edit_pl100[1],line12.text[0],12);
					  xvt_vobj_get_title(edit_pl100[2],line12.text[1],12);}

				  line12.textzeilen[1] =xvt_list_get_sel_index(lb[7]);
				  if(line12.textzeilen[1]==0)
					line12.text[2][0]=line12.text[3][0]='\0';
				  if(line12.textzeilen[1]==1)
					{xvt_vobj_get_title(edit_pl100[3],line12.text[2],12);
					 line12.text[3][0]='\0';}
				  else
					 {xvt_vobj_get_title(edit_pl100[3],line12.text[2],12);
					  xvt_vobj_get_title(edit_pl100[4],line12.text[3],12);}

				  line12.text[4][0]=line12.text[5][0]='\0';

				  line12.textzeilen[2] = 0;
				  break;
				 case 3:
				  line12.schriftfeldzeilen[0] =xvt_list_get_sel_index(lb[3]);
				  line12.schriftfeldzeilen[1] =xvt_list_get_sel_index(lb[4]);
				  line12.schriftfeldzeilen[2] =xvt_list_get_sel_index(lb[5]);

				  line12.textzeilen[0] =xvt_list_get_sel_index(lb[6]);
				  if(line12.textzeilen[0]==0)
					line12.text[0][0]=line12.text[1][0]='\0';
				  if(line12.textzeilen[0]==1)
					{xvt_vobj_get_title(edit_pl100[1],line12.text[0],12);
					 line12.text[1][0]='\0';}
				  else
					 {xvt_vobj_get_title(edit_pl100[1],line12.text[0],12);
					  xvt_vobj_get_title(edit_pl100[2],line12.text[1],12);}

				  line12.textzeilen[1] =xvt_list_get_sel_index(lb[7]);
				  if(line12.textzeilen[1]==0)
					line12.text[2][0]=line12.text[3][0]='\0';
				  if(line12.textzeilen[1]==1)
					{xvt_vobj_get_title(edit_pl100[3],line12.text[2],12);
					 line12.text[3][0]='\0';}
				  else
					 {xvt_vobj_get_title(edit_pl100[3],line12.text[2],12);
					  xvt_vobj_get_title(edit_pl100[4],line12.text[3],12);}

				  line12.textzeilen[2] =xvt_list_get_sel_index(lb[8]);
				  if(line12.textzeilen[2]==0)
					line12.text[4][0]=line12.text[5][0]='\0';
				  if(line12.textzeilen[1]==1)
					{xvt_vobj_get_title(edit_pl100[5],line12.text[4],12);
					 line12.text[5][0]='\0';}
				  else
					 {xvt_vobj_get_title(edit_pl100[5],line12.text[4],12);
					  xvt_vobj_get_title(edit_pl100[6],line12.text[5],12);}

				  break;
				}
			 save_line_12(&line12);
			 SaveProfilFile =TRUE;
			 }//-if (data_changed)
			xvt_vobj_destroy(xdWindow);
		  }
		  break;
		case PLOT_100_PB_CANCEL: /* "Abbrechen" */
			{
			 dlg_cancel =TRUE;
			 if (data_changed)
				{
                 char buf[200],buf2[200],buf3[200],buf4[200];
                 xvt_res_get_str(STR_JA,buf,sizeof(buf));
                 xvt_res_get_str(STR_NEIN,buf2,sizeof(buf2));
                 xvt_res_get_str(STR_ZURUECK,buf3,sizeof(buf3));
                 xvt_res_get_str(STR_PLOT101_ASK,buf4,sizeof(buf4));
				 //switch(xvt_dm_post_ask("Ja","Nein","Zurück","Sollen geänderte Daten gesichert werden ?"))
                 switch(xvt_dm_post_ask(buf,buf2,buf3,buf4))
					{
					 case RESP_DEFAULT:
						  {
                            
							xdEvent->type =E_CONTROL;
							xdEvent->v.ctl.id=PLOT_100_PB_OK;  // OK-Button
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
		case PLOT_100_EDIT_4:			{
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
			} else
				{/*		Contents of control were changed		*/
				 data_changed=TRUE;
				}
			}
			break;
		case PLOT_100_LB_9:  /*Stationsbeschriftung*/
			{
				 data_changed=TRUE;
			}
			break;
		case PLOT_100_LB_10: /* "Antragsteller" */
			{
				 data_changed=TRUE;
			}
			break;
		case PLOT_100_LB_31: /* "Anzahl Querfelder" */
			{
			 querfelder =xvt_list_get_sel_index(lb[2]);
			 switch (querfelder)
				{
				 case 0:
					  xvt_vobj_set_enabled(lb[3],FALSE);
					  xvt_vobj_set_enabled(lb[4],FALSE);
					  xvt_vobj_set_enabled(lb[5],FALSE);
					  xvt_vobj_set_enabled(lb[6],FALSE);
					  xvt_vobj_set_enabled(lb[7],FALSE);
					  xvt_vobj_set_enabled(lb[8],FALSE);
					break;
				 case 1:
					  xvt_vobj_set_enabled(lb[3],TRUE);
					  xvt_vobj_set_enabled(lb[6],TRUE);
					  xvt_vobj_set_enabled(lb[4],FALSE);
					  xvt_vobj_set_enabled(lb[5],FALSE);
					  xvt_vobj_set_enabled(lb[7],FALSE);
					  xvt_vobj_set_enabled(lb[8],FALSE);
					break;
				 case 2:
					  xvt_vobj_set_enabled(lb[3],TRUE);
					  xvt_vobj_set_enabled(lb[4],TRUE);
					  xvt_vobj_set_enabled(lb[6],TRUE);
					  xvt_vobj_set_enabled(lb[7],TRUE);
					  xvt_vobj_set_enabled(lb[5],FALSE);
					  xvt_vobj_set_enabled(lb[8],FALSE);
					break;
				 case 3:
					  xvt_vobj_set_enabled(lb[3],TRUE);
					  xvt_vobj_set_enabled(lb[6],TRUE);
					  xvt_vobj_set_enabled(lb[4],TRUE);
					  xvt_vobj_set_enabled(lb[5],TRUE);
					  xvt_vobj_set_enabled(lb[7],TRUE);
					  xvt_vobj_set_enabled(lb[8],TRUE);
					break;
				}
				 xdEvent->type =E_CONTROL;
				 xdEvent->v.ctl.id=PLOT_100_LB_35;
				 xvt_win_dispatch_event(xdWindow,xdEvent);
				 xdEvent->v.ctl.id=PLOT_100_LB_36;
				 xvt_win_dispatch_event(xdWindow,xdEvent);
				 xdEvent->v.ctl.id=PLOT_100_LB_37;
				 xvt_win_dispatch_event(xdWindow,xdEvent);
				 data_changed=TRUE;
			}
			break;
		case PLOT_100_LB_32: /* "List Button 32" */
			{
				 data_changed=TRUE;
			}
			break;
		case PLOT_100_LB_33: /* "List Button 33" */
			{
				 data_changed=TRUE;
			}
			break;
		case PLOT_100_LB_34: /* "List Button 34" */
			{
				 data_changed=TRUE;
			}
			break;
		case PLOT_100_LB_35: /* "List Button 35" */
			{
			 textzeilen[0] =xvt_list_get_sel_index(lb[6]);
			 switch (textzeilen[0])
			  {
				case 0:
					xvt_vobj_set_enabled(edit_pl100[1],FALSE);
					xvt_vobj_set_enabled(edit_pl100[2],FALSE);
				  break;
				case 1:
					xvt_vobj_set_enabled(edit_pl100[1],TRUE);
					xvt_vobj_set_enabled(edit_pl100[2],FALSE);
				  break;
				case 2:
					xvt_vobj_set_enabled(edit_pl100[1],TRUE);
					xvt_vobj_set_enabled(edit_pl100[2],TRUE);
				  break;
			  };
				 data_changed=TRUE;
			}
			break;
		case PLOT_100_LB_36: /* "List Button 36" */
			{
			 textzeilen[1] =xvt_list_get_sel_index(lb[7]);
			 switch (textzeilen[1])
			  {
				case 0:
					xvt_vobj_set_enabled(edit_pl100[3],FALSE);
					xvt_vobj_set_enabled(edit_pl100[4],FALSE);
				  break;
				case 1:
					xvt_vobj_set_enabled(edit_pl100[3],TRUE);
					xvt_vobj_set_enabled(edit_pl100[4],FALSE);
				  break;
				case 2:
					xvt_vobj_set_enabled(edit_pl100[3],TRUE);
					xvt_vobj_set_enabled(edit_pl100[4],TRUE);
				  break;
			  } ;
				 data_changed=TRUE;
			}
			break;
		case PLOT_100_LB_37: /* "List Button 37" */
			{
			 textzeilen[2] =xvt_list_get_sel_index(lb[8]);
			 switch (textzeilen[2])
			  {
				case 0:
					xvt_vobj_set_enabled(edit_pl100[5],FALSE);
					xvt_vobj_set_enabled(edit_pl100[6],FALSE);
				  break;
				case 1:
					xvt_vobj_set_enabled(edit_pl100[5],TRUE);
					xvt_vobj_set_enabled(edit_pl100[6],FALSE);
				  break;
				case 2:
					xvt_vobj_set_enabled(edit_pl100[5],TRUE);
					xvt_vobj_set_enabled(edit_pl100[6],TRUE);
				  break;
			  }  ;
				 data_changed=TRUE;
			}
			break;
		case PLOT_100_EDIT_25:			{
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
		case PLOT_100_EDIT_26:			{
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
		case PLOT_100_EDIT_27:			{
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
		case PLOT_100_EDIT_28:			{
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
		case PLOT_100_EDIT_29:			{
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
		case PLOT_100_EDIT_30:			{
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
		default:
			break;
		}
		}
		break;
	case E_TIMER:
		/*			Timer associated with window went off.		*/
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
	return 0L;
}
