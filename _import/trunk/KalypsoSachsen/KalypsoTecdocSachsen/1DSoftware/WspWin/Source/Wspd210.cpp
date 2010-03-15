/*
	Handler for dialog DLG_210 ("Profildateien auswaehlen")
*/
#include <windows.h>
#include "xvt.h"
#include "wspwin.h"
#include "resource.h"

#include "global_types.h"
#include "..\..\wspdlg\include\export.h"

#include "list.h"
#include "bce_allg.h"

#include "global.h"
#include "profproj.h"
#include "profpro2.h"

#include "wsphilfe.h"

extern XVT_HELP_INFO hi;


#define DLG_RES_ID DLG_210
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODAL

WINDOW listbox210;
XVT_FNTID zz_font_id;

extern SLIST profile_slist2;
extern char *dateien; //dateien[350];
extern int ok_file;

SLIST auswahl_list,
		auswahl_list2;
SLIST_ELT elt210,
			 elt210b;
char *ptr_string;
BOOLEAN auswaehlen=TRUE;
int i210,j210, test1,test2,test3,count;

/*
	Handler for dialog DLG_210 ("Profildateien auswaehlen")
*/
long XVT_CALLCONV1
#if XVT_CC_PROTO
DLG_210_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
DLG_210_eh XVT_CALLCONV2 (xdWindow, xdEvent)
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
		 dateien[0]='\0';
		 if(auswahl_list!=NULL)
		  {
			xvt_slist_destroy(auswahl_list);
			auswahl_list=NULL;
		  }
		 if(auswahl_list2!=NULL)
		  {
			xvt_slist_destroy(auswahl_list2);
			auswahl_list2=NULL;
		  }
		 auswahl_list=xvt_slist_create();
		 auswahl_list2=xvt_slist_create();
		 listbox210=xvt_win_get_ctl(xdWindow,DLG_210_LBOX_1);
		 ermittle_profile_slist(); //IN PROFPRO2.cpp
		 xvt_list_add(listbox210,-1,(char*)profile_slist2);

		 int anzahl_slist=xvt_slist_count(profile_slist2);
		 if(anzahl_slist==0)
		  {
			dateien[0]='\0';
			ok_file=IDOK; //damit str. gespeichert wird
			xvt_vobj_destroy(xdWindow);
		  }

		 //Schriftart fixieren damit tabellarisch
		 NewSetFontFunc(listbox210);    //Schriftart für listbox210 ändern

     xvt_font_destroy(zz_font_id);

		 if (hi!=NULL_HELP_INFO)
			xvt_help_set_win_assoc(hi, xdWindow, HID_KAPITEL_6_2_1, 0L);
		}
		break;
	case E_DESTROY:
		/*
			Dialog has been closed; last event sent to dialog.
		*/
		{
		 if(profile_slist2!=NULL)
		  {
			xvt_slist_destroy(profile_slist2);
			profile_slist2=NULL;
		  }
		 if(auswahl_list!=NULL)
		  {
			xvt_slist_destroy(auswahl_list);
			auswahl_list=NULL;
		  }
/*		 if(auswahl_list2!=NULL)
		  {
			xvt_slist_destroy(auswahl_list2);
			auswahl_list2=NULL;
		  }
*/
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
		case DLG_210_LBOX_1: /* "List Box 1" */
			{
			/*
				List box was operated.
			*/
			if (xdEvent->v.ctl.ci.v.lbox.dbl_click) {
				/*
					double click
				*/
			} else {
				/*
					single click
				*/
			 auswahl_list=xvt_list_get_sel(listbox210);
			}
			}
			break;
		case DLG_210_PUSHBUTTON_3: /* "OK" */
		  {
			char auswahl[100];
			char auswahl2[100];
			char station_auswahl[15],
				  station_auswahl2[15],
				  vzk_auswahl[15],
				  vzk_auswahl2[15],
				  pk_auswahl[15],
				  pk_auswahl2[15],
				  profil_name_auswahl[15];

			count=xvt_slist_count(auswahl_list);
			if(count>25)
                {
                 char buf[200];//Dick 26.11.99
                 xvt_res_get_str(STR_WSPD210_NOTE_1,buf,sizeof(buf));
                 xvt_dm_post_note("%s",buf);
			     //xvt_dm_post_note("Maximal 25 Profile zulässig");
                }
			else
			 {
			 for(elt210=xvt_slist_get_first(auswahl_list);elt210!=NULL;
				  elt210=xvt_slist_get_next(auswahl_list,elt210))
			  {
				ptr_string=xvt_slist_get(auswahl_list,elt210,0);
				strcpy(auswahl,ptr_string);
				j210=0;
				for(i210=10;i210<=17;i210++)
				 {
				  if(auswahl[i210]!=' ')
					{
					 station_auswahl[j210]=auswahl[i210];
					 j210++;
					}
				 }
				 station_auswahl[j210]='\0';
				 j210=0;
				 for(i210=19;i210<=27;i210++)
				 {
				  if(auswahl[i210]!=' ')
					{
					 pk_auswahl[j210]=auswahl[i210];
					 j210++;
					}
				 }
				 pk_auswahl[j210]='\0';
				 if(pk_auswahl[0]!='0')
				  {
                   char buf[200];//Dick 26.11.99
                   xvt_res_get_str(STR_WSPD210_NOTE_2,buf,sizeof(buf));
                   xvt_dm_post_note("%s",buf);
					//xvt_dm_post_note("Es koennen keine mehrgliedrigen Profile auf diese Weise aufgenommen werden.\n"
					//					 "Waehlen sie die Option PROFIL AUFNEHMEN");
				  break;
				  }
				 j210=0;
				 for(i210=29;i210<=31;i210++)
				 {
				  if(auswahl[i210]!=' ')
					{
					 vzk_auswahl[j210]=auswahl[i210];
					 j210++;
					}
				 }
				 vzk_auswahl[j210]='\0';
				 if(vzk_auswahl[0]!='0')
				  {
                   char buf[200];//Dick 26.11.99
                   xvt_res_get_str(STR_WSPD210_NOTE_3,buf,sizeof(buf));
                   xvt_dm_post_note("%s",buf);
					//xvt_dm_post_note("Es koennen keine verzweigten Profile auf diese Weise aufgenommen werden.\n"
					//					 "Waehlen sie die Option PROFIL AUFNEHMEN");
				  break;
				  }

				count=xvt_slist_count(auswahl_list2);
				if((count==0) && (pk_auswahl[0]=='0') && (vzk_auswahl[0]=='0'))
				 xvt_slist_add_at_elt(auswahl_list2,NULL,auswahl,0L);
				if (count>0)
				 {
				  auswaehlen=TRUE;
				  for(elt210b=xvt_slist_get_first(auswahl_list2);elt210b!=NULL;
						elt210b=xvt_slist_get_next(auswahl_list2,elt210b))
					{
					 ptr_string=xvt_slist_get(auswahl_list,elt210b,0);
					 strcpy(auswahl2,ptr_string);
					 j210=0;
					 for(i210=10;i210<=17;i210++)
					  {
						if(auswahl2[i210]!=' ')
						 {
						  station_auswahl2[j210]=auswahl2[i210];
						  j210++;
						 }
					  }
					 station_auswahl2[j210]='\0';
					 j210=0;
					 for(i210=19;i210<=27;i210++)
					  {
						if(auswahl2[i210]!=' ')
						 {
						  pk_auswahl2[j210]=auswahl2[i210];
						  j210++;
						 }
					  }
					 pk_auswahl2[j210]='\0';
					 j210=0;
					 for(i210=29;i210<=31;i210++)
					  {
						if(auswahl2[i210]!=' ')
						 {
						  vzk_auswahl[j210]=auswahl2[i210];//Dick 5.08.98 auswahl2 statt auswahl
						  j210++;
						 }
					  }
					 vzk_auswahl[j210]='\0';

				 test1=xvt_str_compare_ignoring_case(station_auswahl,station_auswahl2);
				 test2=xvt_str_compare_ignoring_case(vzk_auswahl,vzk_auswahl2);
				 test3=xvt_str_compare_ignoring_case(pk_auswahl,pk_auswahl2);
				 if(((test1==0) && (test2==0) && (test3==0)) ||
						(pk_auswahl[0]!='0') || (vzk_auswahl[0]!='0'))
				  {
					auswaehlen=FALSE;
				  }
					} //FOR ELT AUSWAHL-LIST2
				if(auswaehlen)
				  xvt_slist_add_at_elt(auswahl_list2,NULL,auswahl,0L);
				 } //IF COUNT >0
			  } //FOR AUSWAHL-LIST
			 count=xvt_slist_count(auswahl_list2);

			 if(count>0)
			  {
				for(elt210=xvt_slist_get_first(auswahl_list2);elt210!=NULL;
				  elt210=xvt_slist_get_next(auswahl_list2,elt210))
				 {
				  ptr_string=xvt_slist_get(auswahl_list2,elt210,0);
				  strcpy(auswahl,ptr_string);
				  j210=0;
					 for(i210=44;i210<=55;i210++)
					  {
						if(auswahl[i210]!=' ')
						 {
						  profil_name_auswahl[j210]=auswahl[i210];
						  j210++;
						 }
					  }
					 profil_name_auswahl[j210]='\0';

				  haenge_zustand_an_zsd(profil_name_auswahl);
				  strcat(dateien,profil_name_auswahl);
				  strcat(dateien," ");
				 }
				ok_file=IDOK;
				xvt_vobj_destroy(xdWindow);
			  }
			 if(count==0)
			  {
				dateien[0]='\0';
				ok_file=IDOK;
				xvt_vobj_destroy(xdWindow);
			  }
			} //else
			/* delete[]auswahl;
			 delete[]auswahl2;
			 delete[]station_auswahl;
			 delete[]station_auswahl2;
			 delete[]vzk_auswahl;
			 delete[]vzk_auswahl2;
			 delete[]pk_auswahl;
			 delete[]pk_auswahl2;
			 delete[]profil_name_auswahl;
         */
			}
			break;
		case DLG_210_PUSHBUTTON_4: /* "ABBRUCH" */
			{
			 dateien[0]='\0';
			 ok_file=IDOK; //damit str. gespeichert wird
			 xvt_vobj_destroy(xdWindow);
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
