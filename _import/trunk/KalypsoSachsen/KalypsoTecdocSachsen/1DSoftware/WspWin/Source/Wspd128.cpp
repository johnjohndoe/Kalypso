/****************************************************************************
*                     WSPD128.CPP                                           *
*       Anlegen einer neuen Vernetzungsdatei / neues Projekt                *
*                                                                           *
****************************************************************************/
#include <dos.h>
#include <windows.h>
#include "xvt.h"

#include "global_types.h"
#include "..\..\wspdlg\include\export.h"

#include "wspwin.h"
#include "resource.h"

#include "list.h"


#include "global.h"
#include "bce_allg.h"

#include "wsphilfe.h"

extern XVT_HELP_INFO hi;

#define DLG_RES_ID DLG_128
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODAL

extern SLIST header_profil;
char str2[41],str3[16];
int l1,j=0,n1=4;

WINDOW win_128_edit[3];
BOOLEAN is_128_ok=TRUE;

/* Handler for dialog DLG_128 ("neue Vernetzungsdatei ")  */
long XVT_CALLCONV1
#if XVT_CC_PROTO
DLG_128_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
DLG_128_eh XVT_CALLCONV2 (xdWindow, xdEvent)
WINDOW xdWindow;
EVENT *xdEvent;
#endif
{
	short xdControlId = xdEvent->v.ctl.id;

	switch (xdEvent->type) {
	case E_CREATE:
		{
		 char str_datum[11],tmp[11];
		 netz_dat[0][0]='\0';
		 netz_dat[1][0]='\0';
		 netz_dat[2][0]='\0';
		 win_128_edit[0]=xvt_win_get_ctl(xdWindow,DLG_128_EDIT_4);
		 win_128_edit[1]=xvt_win_get_ctl(xdWindow,DLG_128_EDIT_5);
		 win_128_edit[2]=xvt_win_get_ctl(xdWindow,DLG_128_EDIT_6);
		 xvt_scr_set_focus_vobj(win_128_edit[0]);

     SYSTEMTIME systime;

     GetSystemTime(&systime);

		 itoa((char)systime.wDay,tmp,10);
		 strcpy(str_datum,tmp);
		 strcat(str_datum,".");

		 itoa((char)systime.wMonth,tmp,10);
		 strcat(str_datum,tmp);
		 strcat(str_datum,".");

		 itoa((int)systime.wYear,tmp,10);
		 strcat(str_datum,tmp);
		 xvt_vobj_set_title(win_128_edit[1],str_datum);
		 strcpy(netz_dat[1],str_datum);

		 if (hi!=NULL_HELP_INFO)
			xvt_help_set_win_assoc(hi, xdWindow, HID_KAPITEL_4_3_1, 0L);
		}
		break;
	case E_DESTROY:
		{

		}
		break;
	case E_CLOSE:
		{
		 SaveNetzFile = FALSE;
		 SaveStrangFile =FALSE;
		 str_netz[0]='\0';    //löschen
		 is_128_ok=FALSE;
		 xvt_vobj_destroy(xdWindow);
		}
		break;
	case E_CHAR:
		/*		Character typed.		*/
		{
		 switch(xdEvent->v.chr.ch)
			{
			 case K_BTAB:
				 {
				  xvt_scr_set_focus_vobj(win_128_edit[n1++ % 3]);
				 }
			}
		}
		break;
	case E_CONTROL:
	  {  	          /*   	User operated control in dialog.	*/
		switch(xdControlId) {
		case DLG_128_PUSHBUTTON_7: /* "OK" */
			{
			int is_128_str=TRUE;
			if (check_string(netz_dat[0])!=1)
				{
        char buf[200];//Dick 26.11.99
        xvt_res_get_str(STR_WSPD128_NOTE_1,buf,sizeof(buf));
        xvt_dm_post_error("%s",buf);
        //xvt_dm_post_note("Fehlerhafte Eingabe im Feld:\nGewässername\nBitte keine Umlaute oder Leerzeichen!");
        xvt_scr_set_focus_vobj(win_128_edit[0]);
        is_128_str=FALSE;
      }
      if (check_string(netz_dat[2])!=1)
      {
        char buf[200];//Dick 26.11.99
        xvt_res_get_str(STR_WSPD128_NOTE_2,buf,sizeof(buf));
        xvt_dm_post_error("%s",buf);
        //xvt_dm_post_note("Fehlerhafte Eingabe im Feld:\nZustand\nBitte keine Umlaute oder Leerzeichen!");
        xvt_scr_set_focus_vobj(win_128_edit[2]);
        is_128_str=FALSE;
      }
      
      
      if (is_128_str)
      {
        if (strlen(netz_dat[0])<2)
        {
          char buf[200];//Dick 26.11.99
          xvt_res_get_str(STR_WSPD128_NOTE_3,buf,sizeof(buf));
          xvt_dm_post_error("%s",buf);
          //xvt_dm_post_note("Fehlerhafte Eingabe im Feld:\n\nGewässername\n\n( mind. 2 Buchstaben eingeben )");
          xvt_scr_set_focus_vobj(win_128_edit[0]);
          is_128_ok=FALSE;
        }
        else
        {
          if ((strlen(netz_dat[1])<=5) || (!is_128_ok))
          {
            char buf[200];//Dick 26.11.99
            xvt_res_get_str(STR_WSPD128_NOTE_4,buf,sizeof(buf));
            xvt_dm_post_error("%s",buf);
            //xvt_dm_post_note("Falsche Eingabe im Feld:\n\nDatum !\n\n[ Beispiel: 2.8.94 ]");
            xvt_scr_set_focus_vobj(win_128_edit[1]);
            is_128_ok=FALSE;
          }
          else
          {
            if ((strlen(netz_dat[2])<=2) || (!is_128_ok))
            {
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_WSPD128_NOTE_5,buf,sizeof(buf));
              xvt_dm_post_error("%s",buf);
              //xvt_dm_post_note("Falsche Eingabe im Feld:\n\nZustand\n\n( mind. 3 Buchstaben eingeben )");
              xvt_scr_set_focus_vobj(win_128_edit[2]);
              is_128_ok=FALSE;
            }
          }
        }
        if (is_128_ok)
        {
          for (int i=0;i<=40;i++) str2[i]=' ';
          for (i=0;i<=99;i++)
            str_netz[i]=' ';
          l1 = strlen(netz_dat[0]);
          for (i=0;i<l1;i++)         //Gewässername
            str2[i]=netz_dat[0][i];
          
          l1=strlen(netz_dat[2]);    //Zustand
          for ( i=15;i<15+l1;i++)
            str2[i]=netz_dat[2][i-15];
          
          l1=strlen(netz_dat[1]);    //Datum
          for (i=30;i<30+l1;i++)
            str2[i]=netz_dat[1][i-30];
          
          str2[40]='\0';
          strcpy(str_netz,str2);
          str_netz[40]=' ';
          str_netz[73]=netz_dat[0][0];   //Gewässernameprefix
          str_netz[74]=netz_dat[0][1];
          str_netz[75]='0';              //Zustand : 00
          str_netz[76]='0';
          anzahl_str_dat_entries++;        //Zähler str-dateien erhöhen
          anzahl_str_dat_entries_abs++;    //abs. Zähler str-dateien erhöhen
          
          if (anzahl_str_dat_entries_abs>9998)
          {
            char buf[200];//Dick 26.11.99
            xvt_res_get_str(STR_WSPD128_NOTE_6,buf,sizeof(buf));
            xvt_dm_post_error("%s",buf);
            //xvt_dm_post_fatal_exit("Die max.Anzahl der zu verwaltenden Anzahl an Profildateien ist überschritten! Programm wird beendet!");
          }
          itoa(anzahl_str_dat_entries_abs,str2,10);  //Anzahl *.str-dateien+1
          strcat(str2,".str");
          l1=strlen(str2);
          if (l1<8)
            for (i=0;i<8-l1;i++)
              str3[i]='0';
            str3[i]='\0';
            strcat(str3,str2);
            l1=strlen(str3);
            for (i=0;i<l1;i++)
              str_netz[77+i]=str3[i];
            str_netz[85]='\0';
            
            for (i=73;i<=84;i++)
              STR_SPEC.name[i-73]=str_netz[i];
            
            //neu: weil teilweise Endung .strli auftrat
            STR_SPEC.name[12]='\0';
            
            SaveNetzFile = TRUE;
            SaveStrangFile =TRUE;
            xvt_vobj_destroy(xdWindow);
        }
        if (header_profil !=NULL)
        {
          xvt_slist_destroy(header_profil);
          header_profil =NULL;
        }
        header_profil = xvt_slist_create();
        for (int i=0;i<=5;i++)
          xvt_slist_add_at_elt(header_profil,NULL,"",i);
        xvt_slist_add_at_elt(header_profil,NULL,netz_dat[0],6);
        for (i=7;i<=9;i++)
          xvt_slist_add_at_elt(header_profil,NULL,"",i);
        xvt_slist_add_at_elt(header_profil,NULL,netz_dat[1],10);
        for (i=11;i<=14;i++)
          xvt_slist_add_at_elt(header_profil,NULL,"",i);
      }  // -if (is_128_str)
      }
      break;
    case DLG_128_PUSHBUTTON_8: /* "Abbrechen" */
      {
        SaveNetzFile = FALSE;
        SaveStrangFile =FALSE;
        str_netz[0]='\0';    //löschen
        netz_dat[0][0]='\0';
        netz_dat[1][0]='\0';
        netz_dat[2][0]='\0';
        is_128_ok=FALSE;
        xvt_vobj_destroy(xdWindow);
      }
      
      break;
    case DLG_128_EDIT_4:			{
      /*	Edit control was operated.		*/
      if (xdEvent->v.ctl.ci.v.edit.focus_change) {
        if (xdEvent->v.ctl.ci.v.edit.active) {
          /*   	focus has entered the control	*/
        } else
        { 	/*		focus has left the control		*/
          
        }
      }
      else
      {  	/*   Contents of control were changed		*/
        xvt_vobj_get_title(win_128_edit[0],netz_dat[0],16);
        netz_dat[0][9]='\0';
        is_128_ok = TRUE;
      }
                              }
      break;
    case DLG_128_EDIT_5:			{
      /*		Edit control was operated.		*/
      if (xdEvent->v.ctl.ci.v.edit.focus_change) {
        if (xdEvent->v.ctl.ci.v.edit.active) {
          /*	focus has entered the control		*/
        } else {
          /*		focus has left the control		*/
        }
      } else {
        /*		Contents of control were changed			*/
        xvt_vobj_get_title(win_128_edit[1],netz_dat[1],11);
        is_128_ok = TRUE;
      }
                              }
      break;
    case DLG_128_EDIT_6:			{
      /*		Edit control was operated.		*/
      if (xdEvent->v.ctl.ci.v.edit.focus_change) {
        if (xdEvent->v.ctl.ci.v.edit.active) {
          /*		focus has entered the control		*/
        } else {
          /*	focus has left the control		*/
        }
      } else {
        /*			Contents of control were changed		*/
        xvt_vobj_get_title(win_128_edit[2],netz_dat[2],16);
        is_128_ok = TRUE;
        
        
      }
                              }
      break;
    default:
      break;
    }
    }
    break;
  case E_USER:
    /*    Application initiated.	*/
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
