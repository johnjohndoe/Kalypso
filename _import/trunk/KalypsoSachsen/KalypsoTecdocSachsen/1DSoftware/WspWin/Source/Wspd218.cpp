/*
	Handler for dialog DLG_218 ("Plotinformationen")
*/

#include <windows.h>
#include "xvt.h"
#include "wspwin.h"
#include "resource.h"
#include "bce_allg.h"
#include "global_types.h"
#include "..\..\wspdlg\include\export.h"

#include "list.h"


#include "global.h"
#include "rauh.h"

#include "wsphilfe.h"

extern XVT_HELP_INFO hi;

extern BOOLEAN abbruch_217ff;
extern char dxf_ohne_verzeichnis[15];
WINDOW edit[3];
plot ploti;
BOOLEAN dateiexistiert=FALSE, ueberschreiben=FALSE;
/*
	Information about the dialog
*/
#define DLG_RES_ID DLG_218
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODAL

/*
	Handler for dialog DLG_218 ("Plotinformationen")
*/
long XVT_CALLCONV1
#if XVT_CC_PROTO
DLG_218_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
DLG_218_eh XVT_CALLCONV2 (xdWindow, xdEvent)
WINDOW xdWindow;
EVENT *xdEvent;
#endif
{
	short xdControlId = xdEvent->v.ctl.id;

	switch (xdEvent->type) {
	case E_CREATE:
		{
		 dateiexistiert=FALSE;
		 ueberschreiben=FALSE;
		 int back=masstab_holen(); //in rauh.cpp
		 if(back==0) //Inputdatei konnte in masstab_holen nicht geöffnet werden
		  {
			abbruch_217ff=TRUE;
			xvt_vobj_destroy(xdWindow);
		  }
		 else
		  {
			abbruch_217ff=FALSE;

			edit[0]=xvt_win_get_ctl(xdWindow,DLG_218_EDIT_3);
			edit[1]=xvt_win_get_ctl(xdWindow,DLG_218_EDIT_4);
			edit[2]=xvt_win_get_ctl(xdWindow,DLG_218_EDIT_8);

			int teste=access(ploti.name,00);
			if (teste!=0)  //DATEI EXISTIERT NOCH NICHT
			 {
			  xvt_vobj_set_visible(edit[2],FALSE);
			  xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_218_TEXT_7)),FALSE);
			  dateiexistiert=FALSE;
			 }
			else
			 {
			  xvt_vobj_set_title(edit[2],dxf_ohne_verzeichnis);
			  dateiexistiert=TRUE;
			 }
			xvt_vobj_set_title(edit[0],ploti.laenge);
			xvt_vobj_set_title(edit[1],ploti.hoehe);
		  } //else masstabholen hat geklappt
		 if (hi!=NULL_HELP_INFO)
			xvt_help_set_win_assoc(hi, xdWindow, HID_KAPITEL_6_7_1_2, 0L);
		  }
		break;
	case E_DESTROY:
		{
		}
		break;
	case E_CONTROL:
		{
		switch(xdControlId) {
		case DLG_218_EDIT_3:			{
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
/*					 int teste=is_zahl(ploti.laenge);
					 if(teste<=0)
					  {
						xvt_dm_post_note("Falsche Ziffer");
						strcpy(ploti.laenge,"1000");
						xvt_vobj_set_title(edit[0],ploti.laenge);
						xvt_scr_set_focus_vobj(edit[0]);
					  }
*/
				}
			} else {
				/*
					Contents of control were changed
				*/
			 xvt_vobj_get_title(edit[0],ploti.laenge,17);
			}
			}
			break;
		case DLG_218_EDIT_4:			{
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
/*					 int teste=is_zahl(ploti.hoehe);
					 if(teste<=0)
					  {
						xvt_dm_post_note("Falsche Ziffer");
						strcpy(ploti.hoehe,"100");
						xvt_vobj_set_title(edit[1],ploti.hoehe);
						xvt_scr_set_focus_vobj(edit[1]);
					  }
*/

				}
			} else {
				/*
					Contents of control were changed
				*/
			 xvt_vobj_get_title(edit[1],ploti.hoehe,17);
			}
			}
			break;
		case DLG_218_EDIT_8:			{
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

				 int len=strlen(dxf_ohne_verzeichnis);
				 int punkt=0;
				 for(int i=0; i<len; i++)
				  {
					if(dxf_ohne_verzeichnis[i]=='.')
					 punkt=1;
				  }
				 if((len>12) || (punkt==0) || (len==0))
				  {
                    char buf[200];//Dick 26.11.99
                    xvt_res_get_str(STR_FALSCHER_DATEINAME,buf,sizeof(buf));
                    xvt_dm_post_note("%s",buf);
					//xvt_dm_post_note("Falscher Dateiname");
					xvt_scr_set_focus_vobj(edit[2]);
				  }
				}
			} else {
				/*
					Contents of control were changed
				*/
			 xvt_vobj_get_title(edit[2],dxf_ohne_verzeichnis,13);
			}
			}
			break;
		case DLG_218_PUSHBUTTON_9: /* "OK" */
			{
//-----------NEU andresen 17.06.97
			 xvt_vobj_get_title(edit[0],ploti.laenge,17);
			 int teste=is_zahl(ploti.laenge);
			 if(teste<=0)
				{
                  char buf[200];//Dick 26.11.99
                  xvt_res_get_str(STR_WSPD218_NOTE_1,buf,sizeof(buf));
                  xvt_dm_post_note("%s",buf);
				 //xvt_dm_post_note("Falsche Ziffer in Längenmaßstab");
				 strcpy(ploti.laenge,"1000");
				 xvt_vobj_set_title(edit[0],ploti.laenge);
				 xvt_scr_set_focus_vobj(edit[0]);
			     break;
				}
			 xvt_vobj_get_title(edit[1],ploti.hoehe,17);
			 teste=is_zahl(ploti.hoehe);
			 if(teste<=0)
				{
                 char buf[200];//Dick 26.11.99
                 xvt_res_get_str(STR_WSPD218_NOTE_2,buf,sizeof(buf));
                 xvt_dm_post_note("%s",buf); 
				 //xvt_dm_post_note("Falsche Ziffer in Höhenmaßstab");
				 strcpy(ploti.hoehe,"100");
				 xvt_vobj_set_title(edit[1],ploti.hoehe);
				 xvt_scr_set_focus_vobj(edit[1]);
			     break;
			    }
             if(dateiexistiert)
			 {
			  xvt_vobj_get_title(edit[2],dxf_ohne_verzeichnis,13);
			  int len=strlen(dxf_ohne_verzeichnis);
			  int punkt=0;
			  for(int i=0; i<len; i++)
				  {
					if(dxf_ohne_verzeichnis[i]=='.')
					 punkt=1;
				  }
			  if((len>12) || (punkt==0) || (len==0))
				  {
                    char buf[200];//Dick 26.11.99
                    xvt_res_get_str(STR_FALSCHER_DATEINAME,buf,sizeof(buf));
                    xvt_dm_post_note("%s",buf);
					//xvt_dm_post_note("Falscher Dateiname");
					xvt_scr_set_focus_vobj(edit[2]);
			        break;
				  }
			 }
//-----------ende NEU andresen 17.06.97
			if(dateiexistiert==TRUE)
			  {
				//xvt_fsys_get_dir(&file_spec.dir);
				xvt_fsys_convert_dir_to_str(&file_spec.dir, ploti.name, 80);
				strcat(ploti.name,"\\");
				strcat(ploti.name,dxf_ohne_verzeichnis);
				int teste=access(ploti.name,00);
				if(teste==0)
				 {
				  ueberschreiben=FALSE;
                  char buf[200],buf2[200],buf3[200];//Dick 26.11.99
                  xvt_res_get_str(STR_JA,buf,sizeof(buf));
                  xvt_res_get_str(STR_NEIN,buf2,sizeof(buf2));
                  xvt_res_get_str(STR_WSPD218_ASK,buf3,sizeof(buf3));
                  switch (xvt_dm_post_ask(buf,buf2,NULL,"%s",buf3))
					//switch (xvt_dm_post_ask("Ja","Nein",NULL,"Es existiert schon eine Datei dieses Namens. Soll diese überschrieben werden?"))
					  {
						case RESP_DEFAULT:       //JA - überschreiben
						 {
							ueberschreiben=TRUE;
							abbruch_217ff=FALSE;
							xvt_vobj_destroy(xdWindow);
							break;

						 }  //case default
						case RESP_2:
						 {
						  xvt_scr_set_focus_vobj(edit[2]);
						  break;
						 } //case resp2
					  } //switch
				 }
				else
				 {
				  abbruch_217ff=FALSE;
				  xvt_vobj_destroy(xdWindow);
				 }
			  }
			 else
			  {
				abbruch_217ff=FALSE;
				xvt_vobj_destroy(xdWindow);
			  }
			}
			break;
		case DLG_218_PUSHBUTTON_10: /* "ABBRUCH" */
			{
			 abbruch_217ff=TRUE;
			 xvt_vobj_destroy(xdWindow);
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
