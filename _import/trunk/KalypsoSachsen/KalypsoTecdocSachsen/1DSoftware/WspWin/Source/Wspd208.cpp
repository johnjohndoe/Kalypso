/*	Handler for dialog DLG_208 ("Profilschluessel") */

#include <windows.h>
#include "xvt.h"
#include "resource.h"
#include "wspwin.h"

#include "global_types.h"
#include "global_vars.h"
#include "..\..\wspdlg\include\export.h"

#include "list.h"

#include "bce_allg.h"
#include "global.h"
#include "ctype.h"
#include "profproj.h"
#include "profpro2.h"
#include "util2.h"
#include "verzweig.h"

#include "wsphilfe.h"
#include "configuration.h"

extern XVT_HELP_INFO hi;

/*
	Information about the dialog
*/
#define DLG_RES_ID DLG_208
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODAL

extern BOOLEAN vergleich,
               profil_aufnehmen,
               istverzweigt,
               neukopieren,
               schluessel_aendern;//Dick 28.03.99

extern char profil_nr_string[8];

char save_station208[20];//Dick 28.03.99

/*externe Variablen:*/
char name208[20];
char profilnummer[15];
/***************/

int test208, test208b, test208c,len208, i208,j208;
BOOLEAN richtig208, richtig208b, richtig208c;
/*
	Handler for dialog DLG_208 ("Profilschluessel")
*/
long XVT_CALLCONV1
#if XVT_CC_PROTO
DLG_208_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
DLG_208_eh XVT_CALLCONV2 (xdWindow, xdEvent)
WINDOW xdWindow;
EVENT *xdEvent;
#endif
{
	short xdControlId = xdEvent->v.ctl.id;

	switch (xdEvent->type)
  {
  case E_CREATE:
    {
      strcpy(save_station208,station208);//Dick 28.03.99
      /*******NICHT PROFILAUFNEHMEN; D:H: NEUANLEGEN EINES PROFILES***********/

      if(!profil_aufnehmen)
      {
        station208[0]='\0';

        xvt_vobj_set_title((xvt_win_get_ctl(xdWindow,DLG_208_EDIT_9)),"0");
        vzk[0]='0';
        vzk[1]='\0';

        xvt_vobj_set_title((xvt_win_get_ctl(xdWindow,DLG_208_EDIT_10)),"0");
        pk[0]='0';
        pk[1]='\0';

        xvt_vobj_set_title((xvt_win_get_ctl(xdWindow,DLG_208_EDIT_8)),netz_dat[2]);
        xvt_vobj_get_title((xvt_win_get_ctl(xdWindow,DLG_208_EDIT_8)),zustand,20);
        strcat(zustand,"\0");

        profil_nr_ermitteln();
        xvt_vobj_set_title((xvt_win_get_ctl(xdWindow,DLG_208_EDIT_14)),profil_nr_string);
        xvt_vobj_get_title((xvt_win_get_ctl(xdWindow,DLG_208_EDIT_14)),profilnummer,10);
        strcat(profilnummer,"\0");
      }

      /*******PROFIL AUFNEHMEN IN WSPD136 gedrückt******************/
		  if(profil_aufnehmen)
			{
        xvt_vobj_set_title((xvt_win_get_ctl(xdWindow,DLG_208_EDIT_10)),pk);
        xvt_vobj_set_title((xvt_win_get_ctl(xdWindow,DLG_208_EDIT_9)),vzk);
        xvt_vobj_set_title((xvt_win_get_ctl(xdWindow,DLG_208_EDIT_7)),station208);
        xvt_vobj_set_title((xvt_win_get_ctl(xdWindow,DLG_208_EDIT_8)),zustand);

        if(profilnummer[0]!='\0')
          xvt_vobj_set_title((xvt_win_get_ctl(xdWindow,DLG_208_EDIT_14)),profilnummer);
        else
        {
          xvt_vobj_set_title((xvt_win_get_ctl(xdWindow,DLG_208_EDIT_14)),"1");
          strcpy(profilnummer,"1");
        }
        if(!neukopieren && !schluessel_aendern)//Dick 28,03.99
        {
          xvt_vobj_set_enabled((xvt_win_get_ctl(xdWindow,DLG_208_EDIT_7)),FALSE);
          xvt_vobj_set_enabled((xvt_win_get_ctl(xdWindow,DLG_208_EDIT_14)),FALSE);
        }
      }
      /*********ALLGEMEIN*****************************************/

      xvt_vobj_set_title((xvt_win_get_ctl(xdWindow,DLG_208_EDIT_6)),netz_dat[0]);
      xvt_vobj_set_enabled((xvt_win_get_ctl(xdWindow,DLG_208_EDIT_6)),FALSE);

      if( !LWA_PROJEKT )
      {
        xvt_vobj_set_enabled((xvt_win_get_ctl(xdWindow,DLG_208_EDIT_10)),FALSE);
        xvt_vobj_set_enabled((xvt_win_get_ctl(xdWindow,DLG_208_EDIT_9)),FALSE);
      };

      richtig208=TRUE;
      test208=1;
      test208b=1;
      abbruch208=FALSE;
      i208=0;
		  j208=0;
      if (hi!=NULL_HELP_INFO)
        xvt_help_set_win_assoc(hi, xdWindow, HID_KAPITEL_4_4_1_1, 0L);
    } // E_CREATE
    break;

	case E_CLOSE:
    {
      xvt_vobj_destroy(xdWindow);
    } // E_CLOSE
    break;

	case E_CONTROL:
    {
      switch(xdControlId) 
      {
      case DLG_208_EDIT_6:  //Name
        {
          if (xdEvent->v.ctl.ci.v.edit.focus_change) 
          {
          } 
          else
          {
            xvt_vobj_get_title((xvt_win_get_ctl(xdWindow,DLG_208_EDIT_6)),name208,20);
          }
        } // DLG_208_EDIT_6
        break;
        
      case DLG_208_EDIT_7:  //Station
        {
          if (xdEvent->v.ctl.ci.v.edit.focus_change) 
          {
          }
          else 
          { // Contents of control were changed
            xvt_vobj_get_title((xvt_win_get_ctl(xdWindow,DLG_208_EDIT_7)),station208,20);
          }
        }
        break;
        
      case DLG_208_EDIT_8: //Zustand
        {
        /*
        Edit control was operated.
          */
          if (xdEvent->v.ctl.ci.v.edit.focus_change) 
          {
            if (xdEvent->v.ctl.ci.v.edit.active) 
            {
            /*
            focus has entered the control
              */
            } else
            {
            /*
            focus has left the control
              */
            }
          } 
          else
          {
          /*
          Contents of control were changed
            */
            xvt_vobj_get_title((xvt_win_get_ctl(xdWindow,DLG_208_EDIT_8)),zustand,20);
            strcat(zustand,"\0");
          }
        } // DLG_208_EDIT_8
        break;
        
      case DLG_208_EDIT_9: //Verzweigungskennung
        {
        /*
        Edit control was operated.
          */
          if (xdEvent->v.ctl.ci.v.edit.focus_change)
          {
            if (xdEvent->v.ctl.ci.v.edit.active)
            {
            /*
            focus has entered the control
              */
            } 
            else
            {
            /*
            focus has left the control
              */
            }
          }
          else
          {
          /*
          Contents of control were changed
            */
            xvt_vobj_get_title((xvt_win_get_ctl(xdWindow,DLG_208_EDIT_9)),vzk,20);
            strcat(vzk,"\0");
          }
        } // DLG_208_EDIT_9
        break;
        
      case DLG_208_EDIT_10: //PK
        {
        /*
        Edit control was operated.
          */
          if (xdEvent->v.ctl.ci.v.edit.focus_change) 
          {
            if (xdEvent->v.ctl.ci.v.edit.active) 
            {
            /*
            focus has entered the control
              */
            } 
            else 
            {
            /*
            focus has left the control
              */
              if(strlen(pk)==0)//Dick 1.04.99
              {
                strcpy(pk,"0");
                xvt_vobj_set_title((xvt_win_get_ctl(xdWindow,DLG_208_EDIT_10)),"0");
              }
            }
          } 
          else
          {
          /*
          Contents of control were changed
            */
            xvt_vobj_get_title((xvt_win_get_ctl(xdWindow,DLG_208_EDIT_10)),pk,20);
            strcat(pk,"\0");
          }
        } // DLG_208_EDIT_10
        break;
        
      case DLG_208_EDIT_14: //Profilnummer
        {
        /*
        Edit control was operated.
          */
          if (xdEvent->v.ctl.ci.v.edit.focus_change) 
          {
            if (xdEvent->v.ctl.ci.v.edit.active) 
            {
            /*
            focus has entered the control
              */
            }
            else
            {
            /*
            focus has left the control
              */
            }
          }
          else
          {
          /*
          Contents of control were changed
            */
            xvt_vobj_get_title((xvt_win_get_ctl(xdWindow,DLG_208_EDIT_14)),profilnummer,10);
            strcat(profilnummer,"\0");
            if(strlen(profilnummer)>10)
              profilnummer[10]='\0';
          }
        }
        break;
        
      case DLG_208_PUSHBUTTON_11: /* "OK" */
        {
          /****führende Nullen entfernen*************/
          char help[20];
          int j=0;
          for(int i=0;i<=(INT)strlen(zustand);i++)    //ZUSTAND
          {
            if (zustand[i]!=' ')
            {
              help[j]=zustand[i];
              j++;
            }
          }
          strcpy(zustand,help);
          
          j=0;                                  //PK
          for(i=0;i<=(INT)strlen(pk);i++)
          {
            if (pk[i]!=' ')
            {
              help[j]=pk[i];
              j++;
            }
          }
          strcpy(pk,help);
          
          for(i=(INT)strlen(pk);i<20;i++)//Dick 1.04.99
          {				
            pk[i]='\0';				  
          }
          
          j=0;                                  //VZK
          for(i=0;i<=(INT)strlen(vzk);i++)
          {
            if (vzk[i]!=' ')
            {
              help[j]=vzk[i];
              j++;
            }
          }
          strcpy(vzk,help);
          //STATION
          j=0;
          for(i=0;i<=(INT)strlen(station208);i++)
          {
            if (station208[i]!=' ')
            {
              help[j]=station208[i];
              j++;
            }
          }
          strcpy(station208,help);             //Name
          j=0;
          for(i=0;i<=(INT)strlen(name208);i++)
          {
            if (name208[i]!=' ')
            {
              help[j]=name208[i];
              j++;
            }
          }
          strcpy(name208,help);
          
          if(strlen(zustand)>10)
            zustand[10]='\0';
          if(strlen(station208)>10) //Dick 18.08.98 8->10
            station208[10]='\0';
          if(strlen(pk)>3)
            pk[3]='\0';
          if (strlen(vzk)>3)
            vzk[3]='\0';
          
          richtig208=TRUE;
          
          test208=is_zahl(station208);
          if(test208<=0)
          {
            char buf[200];//Dick 26.11.99
            xvt_res_get_str(STR_FALSCHE_STATIONSANGABE,buf,sizeof(buf));
            xvt_dm_post_note("%s",buf);
            richtig208=FALSE;
          }
          else    //Dick 18.08.98 damit immer 4 nach der Kommastelle
          {
            double station208_f=atof(station208);
            if(station208_f<1000. && station208_f>-100.)
              sprintf(station208,"%.4lf",station208_f);
            else if(station208_f<10000. && station208_f>-1000.)
              sprintf(station208,"%.3lf",station208_f);
            else if(station208_f<100000. && station208_f>-10000.)
              sprintf(station208,"%.2lf",station208_f);
            else if(station208_f<1000000. && station208_f>-100000.)
              sprintf(station208,"%.1lf",station208_f);
            else if(station208_f<10000000. && station208_f>-1000000.)
              sprintf(station208,"%.0lf",station208_f);
            else
            {
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_STATIONSANGABE_ZU_GROSS,buf,sizeof(buf));
              xvt_dm_post_note("%s",buf);
              richtig208=FALSE;
            }
          }
          
          test208b=isdigit(vzk[0]);
          
          if (test208b<=0)
          {
            char buf[200];//Dick 26.11.99
            xvt_res_get_str(STR_FALSCHE_VERZ,buf,sizeof(buf));
            xvt_dm_post_note("%s",buf);
            richtig208=FALSE;
          }
          
          if(vzk[1]!='\0')
          {
            test208c=isdigit(vzk[1]);
            if (test208c<=0)
            {
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_FALSCHE_VERZ,buf,sizeof(buf));
              xvt_dm_post_note("%s",buf);
              richtig208=FALSE;
            }
          }
          
          if(richtig208)
          {
            int pos=-1; //Dick 1.04.99
            if(pk[1]=='0')//Dick 13.07.99
              pk[1]='\0';
            if(pk[0]=='0' && (pk[1]=='\0'))//||(pk[1]=='0' && pk[2]=='\0')))
              richtig208=TRUE;
            else
            {
              pos=strcspn(pk, "FLR" );
              if(pos==0 && pk[0]==pk[1] && strlen(pk)>=2)
              {
                char *pruef;
                int testdigit;
                pruef=&pk[2];
                
                if(strlen(pruef)!=0)
                {
                  for(i=0;i<=(INT)strlen(pruef)-1;i++)
                  {
                    if (!isdigit(pruef[i]))                                  
                      testdigit=0;                                  
                    else
                      testdigit=1;
                  }
                }
                else
                  testdigit=1;
                if(testdigit>0)
                  richtig208=TRUE;
                else
                  richtig208=FALSE;
              }
              else
                richtig208=FALSE;
            }
            if (!richtig208)
            {
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_FALSCHE_PK,buf,sizeof(buf));
              xvt_dm_post_note("%s",buf);
            }
          }
          
          if (richtig208)
          {
            teste_str_datei();  //IN PROFPROJ.cpp TEST OB SCHLUESSEL SCHON DA
            if(vergleich==FALSE)
            {
              if(!schluessel_aendern)//Dick 30.03.99
              {
                istverzweigt=FALSE;
                teste_str_verzweigt(); //IN VERZWEIG.cpp
                if((istverzweigt==TRUE) ||(vzk[0]!='0')|| (pk[0]!='0'))
                {
                  if ( GetSortVerzweigt() )
                  {
                    if(!xvt_dlg_create_res(WD_MODAL,DLG_211, EM_ALL, DLG_211_eh, 0L))
                      xvt_dm_post_error("Can't open dialog 211");                                     
                  }                                 
                }
              }
              xvt_vobj_destroy(xdWindow);
            }
            else
              vergleich=FALSE;
          }
        } // DLG_208_PUSHBUTTON_11: /* "OK" */
        break;
      
      case DLG_208_PUSHBUTTON_12: /* "ABBRUCH" */
        {
          xvt_vobj_destroy(xdWindow);
          abbruch208=TRUE;
        }
        break;
        
      default:
        break;
    } //  switch(xdControlId) 
  } // E_CONTROL
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
      switch (xdEvent->v.user.id) 
      {
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
