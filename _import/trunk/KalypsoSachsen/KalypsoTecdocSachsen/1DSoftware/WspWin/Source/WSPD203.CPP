/***************/
/* WspD203.cpp */
/***************/

/*	Handler for dialog DLG_203 ("Berechnungsvarianten")  */

#include <windows.h>
#include "xvt.h"

#include <fcntl.h>

#include "resource.h"

#include "global_types.h"
#include "global_vars.h"
#include "laengs1.h"
#include "..\..\wspdlg\Include\export.h"

#include "bce_allg.h"
#include "wsphilfe.h"
#include "util2.h"
#include "wspwin.h"
#include "read.h"
#include "rauh.h"
#include "strang.h"
#include "readprof.h"


#define DLG_RES_ID DLG_203
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODELESS


// globale Variablen

BOOLEAN plotoption = FALSE;
BOOLEAN dlg203offen = FALSE;
BOOLEAN new_batch = TRUE;
BOOLEAN stempel=FALSE;
BOOLEAN aendern;
WINDOW listbox203, DLG204;
SLIST batch_list, helplist;
FILE_SPEC ber_spec;
long timer_203, timer_204;

// externe globale Variablen

extern int wsplist, ok;

BOOLEAN abbruch206 = FALSE;

extern BOOLEAN editieren, berechnen, abbruch203, Save_Laengsschnitt, batch_list_offen, 
               var_dlg135, konvert, stop, ergebnis_tabelle_lesen, laengsschnitt;
extern WINDOW DLG203;
extern WINDOW main_win;
extern SLIST ber_list;
extern char ber_name_extern[MAX_PATH];
extern plot ploti;
extern WSP_PROFIL_LISTE *pWPL;
extern st_daten steuerdaten;

/*
Handler for dialog DLG_203 ("Berechnungsvarianten")
*/
long XVT_CALLCONV1
#if XVT_CC_PROTO
DLG_203_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
DLG_203_eh XVT_CALLCONV2 (xdWindow, xdEvent)
WINDOW xdWindow;
EVENT *xdEvent;
#endif
{
  BOOLEAN  anderewahl=FALSE,
		  timer_an_203=FALSE;
  
  int zaehle_batch=0;
  BOOLEAN is_dlg203ok=FALSE;
  
  char *temp;
  
  char str2[100];
  
  short xdControlId = xdEvent->v.ctl.id;
  temp =new char[MAX_PATH];
  //str2 = new char[100];
  switch (xdEvent->type) {
    
  case E_CREATE:
    {
      //button, der zuviel installiert wurde:
      
      if(!editieren && !berechnen && wsplist==0 && plotoption) // nur Plotten Dick 2.12.98
        xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_203_STEMPEL)),TRUE);
      if(((editieren) &&(berechnen)) ||  //Längsschnitt
        ((!editieren) && (!berechnen))) //Plotten
      {
        xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_203_PUSHBUTTON_9)),FALSE);
        xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_203_PUSHBUTTON_10)),FALSE);
        xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_203_PUSHBUTTON_12)),FALSE);
        //	  editieren=FALSE;
        //	  berechnen=FALSE;
        
        if((wsplist!=1) && (wsplist!=2))
        {
          xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_203_PUSHBUTTON_6)),FALSE);
          xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_203_PUSHBUTTON_11)),TRUE);
        }
        else
        {
          xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_203_PUSHBUTTON_6)),TRUE);
          xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_203_PUSHBUTTON_11)),FALSE);
        }
        
      }
      else
        xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_203_PUSHBUTTON_11)),FALSE);
      
      DLG203=xdWindow;
      dlg203offen=TRUE;
      listbox203 = xvt_win_get_ctl (xdWindow,DLG_203_LBOX_1);
      NewSetFontFunc(listbox203);
      xvt_list_add(listbox203, -1, (char *)ber_list);
      xvt_list_set_sel(listbox203, 0, TRUE);
      if ((helplist = xvt_slist_create())==NULL)
        xvt_dm_post_fatal_exit("SLIST Create Error -wspd203");
      helplist=xvt_list_get_sel(listbox203);
      char *str=xvt_slist_get_elt(helplist,0,0);
      for (int i=79;i<=91;i++)
        temp[i-79] = str[i];
      temp[12] = '\0';
      strcpy(ber_spec.name,temp);
      //neu Dick 1.02.99                     STR_BERVAR
      char title_203[200];
      xvt_res_get_str(STR_BERVAR,title_203,sizeof(title_203));
      //strcpy(title_203,"Berechnungsvarianten: ");
      strcat(title_203,": ");
      switch(xvt_vobj_get_data(xdWindow))
      {
      case 1L:
        char buf[200];
        xvt_res_get_str(STR_EDIT_BER,buf,sizeof(buf));
        //                  strcat(title_203,"Editieren und Berechnen");
        strcat(title_203,buf);
        break;
      case 2L:
        xvt_res_get_str(STR_LAENGS_SHOW,buf,sizeof(buf));
        //                  strcat(title_203,"Längsschnitt einsehen");
        strcat(title_203,buf);
        break;
      case 3L:
        xvt_res_get_str(STR_PLOT_LAENGS,buf,sizeof(buf));
        //                 strcat(title_203,"Plotten(Längsschnitt)");
        strcat(title_203,buf);
        break;
      case 4L:
        xvt_res_get_str(STR_INSERT_WATER,buf,sizeof(buf));
        //                 strcat(title_203,"Wasserspiegel einfügen");
        strcat(title_203,buf);
        break;
      case 5L:
        xvt_res_get_str(STR_DEL_WATER,buf,sizeof(buf));
        //                  strcat(title_203,"Wasserspiegel löschen");
        strcat(title_203,buf);
        break;
      case 6L:
        xvt_res_get_str(STR_COMPARE,buf,sizeof(buf));
        //                  strcat(title_203,"Vergleichsdaten aufnehmen");
        strcat(title_203,buf);
        break;
      case 7L:
        strcat(title_203,"Beispiellisten");
        break;
      case 8L:
        strcat(title_203,"Vergleichslisten");
        xvt_vobj_set_title(xvt_win_get_ctl (xdWindow,DLG_203_PUSHBUTTON_6),"Vergleichen");
        break;
      case 9L:
        strcat(title_203,"Prüflisten");
        break;
      case 10L:
        strcat(title_203,"Beispiellistenergebnisse: Profile pro Abfluss");
        break;
      case 11L:
        strcat(title_203,"Beispiellistenergebnisse: Abflüsse pro Station");
        break;
      case 12L:
        strcat(title_203,"Beispiellistenergebnisse: nur Qvoll pro Profil");
        break;
      case 13L:
        strcat(title_203,"Beispiellistenergebnisse: Längsschnitt in Excel-Format ");
        break;
      case 14L:
        strcat(title_203,"Prüflistenergebnisse");
        break;
      case 15L:
        strcat(title_203,"Vergleichslistenergebnisse");
        break;
      case 16L:
        xvt_res_get_str(STR_ERGEBNISDATEI,buf,sizeof(buf));
        //                 strcat(title_203,"Ergebnisdatei");
        strcat(title_203,buf);
        break;
      case 17L:
        xvt_res_get_str(STR_ZWISCHEN_ERG,buf,sizeof(buf));
        //                 strcat(title_203,"Zwieschenergebnisse");
        strcat(title_203,buf);
        break;
      default:
        break;
        
      }
      xvt_win_set_doc_title(xdWindow,title_203);
      //Ende neu
      if (hi!=NULL_HELP_INFO)
        xvt_help_set_win_assoc(hi, xdWindow, HID_KAPITEL_2_1, 0L);
    }
    break;
    
  case E_DESTROY:
		/*
    Dialog has been closed; last event sent to dialog.
    */
    {
      dlg203offen=FALSE;
      if (timer_an_203)
      {
        xvt_timer_destroy(timer_203);
        timer_an_203=FALSE;
      }
      DLG203=NULL_WIN;
      if( !GetFeature( "wsp_nodemo" ) )
      {
        if(wsplist!=3 && wsplist!=10&& !(berechnen&&editieren) && wsplist!=0)
        {
          abbruch203=TRUE;
          //xvt_dm_post_note("Bei Demo nicht möglich"); 
          char buf[200];//Dick 26.11.99
          xvt_res_get_str(STR_DEMO_NOTE_1,buf,sizeof(buf));
          xvt_dm_post_note("%s",buf);
        }
      };
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
      abbruch203=TRUE;
      is_dlg203ok=FALSE;
      ber_spec.name[0]='\0';
      if (helplist!=NULL)
      {
        xvt_slist_destroy(helplist);
        helplist=NULL;
      }
      if (ber_list!=NULL)
      {
        xvt_slist_destroy(ber_list);
        ber_list=NULL;
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
        
      case DLG_203_LBOX_1: /* "List Box 1" */
        {
          /*   	List box was operated.		*/
          helplist=xvt_list_get_sel(listbox203);
          char *str=xvt_slist_get_elt(helplist,0,0);
          if(str!=NULL)//Dick 28.01.99
          {
            for (int i=79;i<=91;i++)
              temp[i-79] = str[i];
            temp[12] = '\0';
            strcpy(ber_spec.name,temp);
          }
          
          if (xdEvent->v.ctl.ci.v.lbox.dbl_click)
          {	/*		double click		*/
            //	  is_dlg203ok=TRUE;
          }
          else
          {	/*		single click		*/
            
          }
        }
        break;
        
      case DLG_203_PUSHBUTTON_6: /* "Batch" */
        {
          if( !GetFeature( "wsp_nodemo" ) )
          {
            if(xvt_vobj_get_data(xdWindow)!=8L && xvt_vobj_get_data(xdWindow)!=7L) //Dick 13.10.99
            {
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_DEMO_NOTE_2,buf,sizeof(buf));
              xvt_dm_post_note("%s",buf);
              //xvt_dm_post_note("Berechnungen bei Demo nicht möglich");
            }
            if (batch_list!=NULL)
            {
              xvt_slist_destroy(batch_list);
              batch_list=NULL;
            }
            if(DLG203!=NULL_WIN)
              xvt_vobj_destroy(DLG203);            
            break;
          };

          //bley 3.12.98: 
          SLIST_ELT e;
          char *str;
          berechnen=TRUE;
          editieren=FALSE;
          
          helplist=xvt_list_get_sel(listbox203);
          if(xvt_slist_count(helplist)==0)
          {
            //xvt_dm_post_error("Sie haben keine Berechnungsdatei ausgewählt!");
            char buf[200];//Dick 26.11.99
            xvt_res_get_str(STR_DLG203_NOTE,buf,sizeof(buf));
            xvt_dm_post_note("%s",buf);
            break;
          }
          if(batch_list==NULL)//Dick 5.03.99 
            if ((batch_list = xvt_slist_create())==NULL)
              xvt_dm_post_fatal_exit("SLIST Create Error -wspd204");
            for (e=xvt_slist_get_first(helplist);e !=NULL;e=xvt_slist_get_next(helplist,e))
            {
              str=xvt_slist_get(helplist,e,0L);
              for (int i=79;i<=91;i++)
                temp[i-79] = str[i];
              temp[12] = '\0';
              strcpy(ber_spec.name,temp);
              xvt_slist_add_at_elt(batch_list,NULL,ber_spec.name,0);
            }
            if((wsplist!=1) && (wsplist!=2))
            {
              Save_Laengsschnitt=TRUE;

              if( LWA_PROJEKT )
                start_wspr();//für Windows LWA
              else
                schreibe_bat();//für DOS BCE & LWA
            }
            if(wsplist!=2)
              xvt_vobj_destroy(DLG203);
            else if(wsplist==2)//Dick 5.03.99 nur für Vergleichlisten
              if (new_batch)
              {
                if (batch_list_offen==TRUE)
                {
                  //xvt_slist_add_at_elt(batch_list,NULL,ber_spec.name,0);//Dick 5.03.99 weg
                  DLG204=xvt_dlg_create_res(WD_MODELESS,DLG_204, EM_ALL, DLG_204_eh, 0L);
                  if (DLG204==NULL_WIN)
                    xvt_dm_post_error("Can´t open dialog 204");
                  
                }
                else
                {
                  DLG204=xvt_dlg_create_res(WD_MODELESS,DLG_204, EM_ALL, DLG_204_eh, 1L);
                  if (DLG204==NULL_WIN)
                    xvt_dm_post_error("Can´t open dialog 204");
                }
                
              } //if new_batch
              else
              {
                //xvt_slist_add_at_elt(batch_list,NULL,ber_spec.name,0);//Dick 5.03.99 weg
                timer_204=xvt_timer_create(DLG204,10);
                
              }
              
        }
        break;
      case DLG_203_PUSHBUTTON_7: /*********************** "SCHLIESSEN" */
        {
          abbruch203=TRUE;
          is_dlg203ok=FALSE;
          ber_spec.name[0]='\0';
          if (helplist!=NULL)
          {
            xvt_slist_destroy(helplist);
            helplist=NULL;
          }
          if (ber_list!=NULL)
          {
            xvt_slist_destroy(ber_list);
            ber_list=NULL;
          }
          xvt_vobj_destroy(xdWindow);
        }
        break;
      case DLG_203_PUSHBUTTON_8: /* "andere Wahl..." */
        {
          
          char *file_str;
          file_str=new char[100];
          
          //Dick 15.07.99
          char str[MAX_PATH],*aendern_name;

          
          char help[MAX_PATH];
          FILE *ber_file;
          char *temp2;
          SLIST_ELT e;
          
          int i=0;
          unsigned int zaehler=0;
          aendern_name =new char[100];
          //
          
          
          str[0]='\0';
          
          var_dlg135=TRUE;
          anderewahl=TRUE;
          if (!xvt_dlg_create_res(WD_MODAL,DLG_135, EM_ALL, DLG_135_eh, 0L))
            xvt_dm_post_error("Can't open dialog 135");
          var_dlg135=FALSE;
          
          ok=read_start_berechnung();
          timer_203=xvt_timer_create(xdWindow,10);
          
          if (ok ==2)
          {                                  
            str2[0]='\0';
            strcpy(str2,STR_SPEC.name);
            int len=strlen(str2);
            str2[len-3]='\0';
            len=0;
            strcat(str2,"qwt");
            xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,file_str,79);
            strcat(file_str,"\\");
            strcat(file_str,str2);
            if((access(file_str,00))!=0)
            {
              qwert_fehlt=TRUE;
              //xvt_dm_post_note("Sie haben keine Abflussdatei erzeugt!");
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_KEINE_QWERTDATEI2,buf,sizeof(buf));
              xvt_dm_post_note("%s",buf);
              
              if (dlg203offen)
              {
                xvt_vobj_destroy(DLG203);
                dlg203offen=FALSE;
              }
            }
            else
            {
              if (qwert_fehlt==FALSE)
              {
                if( LWA_PROJEKT )
                {
				if( DoLWACalcDlg(Projektname_aktuell, STR_SPEC.name, str, (HWND)xvt_vobj_get_attr(main_win,ATTR_NATIVE_WINDOW),!GetFeature( "wsp_noDemo" ) ) )
                  {
                    read_varianten(str); // in util.cpp
                    aendern=FALSE;
                    konvert=FALSE;
                    strcpy(ber_name_extern,str);
                    if(stop)
                    {
                      remove(str);
                      for(e=xvt_slist_get_first(ber_list);
                      e!=NULL;
                      e=xvt_slist_get_next(ber_list,e))
                      {
                        temp2=xvt_slist_get(ber_list,e,0L);
                        for (i=79;i<=91;i++)
                          aendern_name[i-79]=temp2[i];
                        aendern_name[12]='\0';
                        if(xvt_str_match(aendern_name,ber_name_extern,0)==TRUE)
                          xvt_slist_rem(ber_list,e);
                      }
                      xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str,50);
                      strcat(str,"\\");
                      strcpy(str2,STR_SPEC.name);
                      str2[9]='\0';
                      strcat(str2,"BER");
                      strcat(str,str2);
                      
                      ber_file=fopen(str,"r");
                      fgets(help,10,ber_file);
                      zaehler = atoi(help);
                      fclose(ber_file);
                      ber_file=fopen(str,"w");
                      zaehler=zaehler-1;
                      fprintf(ber_file,"%d\n",zaehler);
                      
                      for(e=xvt_slist_get_first(ber_list);e!=NULL;
                      e=xvt_slist_get_next(ber_list,e))
                      {
                        temp2=xvt_slist_get(ber_list,e,0L);
                        strcpy(str,temp2);
                        for(i=0;i<=(INT)strlen(str);i++)
                        {
                          if(str[i]=='\n')
                            str[i]='\0';
                        }
                        fprintf(ber_file,"%s\n",temp2);
                      }
                      fclose(ber_file);
                      stop=FALSE;
                    }
                    ok = read_start_berechnung();
                  }
                  else if( ok == 2 )
                    ok = 0;
                }
                else
                {
    				if( DoBCECalcDlg(Projektname_aktuell, STR_SPEC.name, str, (HWND)xvt_vobj_get_attr(main_win,ATTR_NATIVE_WINDOW),!GetFeature( "wsp_nodemo" ) ) )
                      ok = read_start_berechnung(); //in read
                    else if( ok == 2 )//Dick 14.02.2000
                      ok = 0;
                };
                
                timer_203 = xvt_timer_create( xdWindow, 10 );
              }
            }
          } //ok=2
            
            if (ok==0)
              xvt_vobj_destroy(xdWindow);
            
            delete[]file_str;
            delete[]aendern_name;//Dick 15.07.99
       }
       break;
       
    case DLG_203_PUSHBUTTON_9: /*Ändern*/
      {
        //Dick 28.01.99 
        int num;
        helplist=xvt_list_get_sel(listbox203);
        num=xvt_slist_count(helplist);
        if(num==0)
        {
          //xvt_dm_post_error("Sie haben keine Berechnungsvariante ausgewählt!");
          char buf[200];//Dick 26.11.99
          xvt_res_get_str(STR_DLG203_NOTE,buf,sizeof(buf));
          xvt_dm_post_note("%s",buf);
          break;
        }
        else if(num > 1)
        {
          //xvt_dm_post_error("Sie können nur eine Berechnungsvariante fürs Editieren auswählen!");
          char buf[200];//Dick 26.11.99
          xvt_res_get_str(STR_DLG203_NOTE2,buf,sizeof(buf));
          xvt_dm_post_note("%s",buf);
          break;
        }
        //ende
        //bley 3.12.98
        editieren=TRUE;
        berechnen=FALSE;
        abbruch206=FALSE;
        xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str2,50);
        strcat(str2,"\\");
        strcat(str2,ber_spec.name);

        if( LWA_PROJEKT )
        {
		  if( DoLWACalcDlg(Projektname_aktuell, STR_SPEC.name, ber_spec.name, (HWND)xvt_vobj_get_attr(main_win,ATTR_NATIVE_WINDOW),!GetFeature( "wsp_nodemo" ) ) )
          {
            read_varianten(str2); // in util.cpp
            aendern=TRUE;
            konvert=FALSE;
            strcpy(ber_name_extern,ber_spec.name);
            timer_203=xvt_timer_create(xdWindow,10);
            read_start_berechnung();
          }
        }
        else
        {
			if( DoBCECalcDlg( Projektname_aktuell, STR_SPEC.name, ber_spec.name, (HWND)xvt_vobj_get_attr(main_win,ATTR_NATIVE_WINDOW),!GetFeature( "wsp_nodemo" ) ) )
          {
            timer_203=xvt_timer_create(xdWindow,10);
            read_start_berechnung();
          }
        };
        }
        break;
        
    case DLG_203_PUSHBUTTON_10: /*Neu*/
      {
        
        abbruch206=FALSE;
        
        char str[MAX_PATH],*aendern_name;
        str[0]='\0';
        int i=0;
        unsigned int zaehler=0;
        aendern_name =new char[100];

        char help[MAX_PATH];
        FILE *ber_file;
        char *temp2;
        SLIST_ELT e;

        if( LWA_PROJEKT )
        {
		if( DoLWACalcDlg( Projektname_aktuell, STR_SPEC.name, str, (HWND)xvt_vobj_get_attr(main_win,ATTR_NATIVE_WINDOW),
			!GetFeature( "wsp_nodemo" ) ) )
        {
          read_varianten(str); // in util.cpp
          aendern=FALSE;
          konvert=FALSE;
          strcpy(ber_name_extern,str);
          
          //falls Abbruch in DLG_209 (aufruf aus vzk_block) stop=true
          if(stop)
          {
            remove(str);
            for(e=xvt_slist_get_first(ber_list);e!=NULL;
            e=xvt_slist_get_next(ber_list,e))
            {
              temp2=xvt_slist_get(ber_list,e,0L);
              for (i=79;i<=91;i++)
                aendern_name[i-79]=temp2[i];
              aendern_name[12]='\0';
              if(xvt_str_match(aendern_name,ber_name_extern,0)==TRUE)
                xvt_slist_rem(ber_list,e);
            }
            xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str,50);
            strcat(str,"\\");
            strcpy(str2,STR_SPEC.name);
            str2[9]='\0';
            strcat(str2,"BER");
            strcat(str,str2);
            
            ber_file=fopen(str,"r");
            fgets(help,10,ber_file);
            zaehler = atoi(help);
            fclose(ber_file);
            ber_file=fopen(str,"w");
            zaehler=zaehler-1;
            fprintf(ber_file,"%d\n",zaehler);
            
            for(e=xvt_slist_get_first(ber_list);e!=NULL;
            e=xvt_slist_get_next(ber_list,e))
            {
              temp2=xvt_slist_get(ber_list,e,0L);
              strcpy(str,temp2);
              for(i=0;i<=(INT)strlen(str);i++)
              {
                if(str[i]=='\n')
                  str[i]='\0';
              }
              fprintf(ber_file,"%s\n",temp2);
            }
            fclose(ber_file);
            stop=FALSE;
          } 
          
          timer_203=xvt_timer_create(xdWindow,10);
          read_start_berechnung();
        }
        }
        else
        {
		  if( DoBCECalcDlg( Projektname_aktuell, STR_SPEC.name, str, (HWND)xvt_vobj_get_attr(main_win,ATTR_NATIVE_WINDOW),
			  !GetFeature( "wsp_nodemo" ) ) )
          {
            timer_203=xvt_timer_create(xdWindow,10);
            read_start_berechnung();
          }
        };
        delete []aendern_name;
      }
      break;
        
    case DLG_203_PUSHBUTTON_12: /* "LOESCHEN" */
      {
        char buf[200],buf2[200],buf3[200];//Dick 26.11.99
        int num;
        helplist=xvt_list_get_sel(listbox203);
        num=xvt_slist_count(helplist);
        if(num==0)
        {
          //xvt_dm_post_error("Sie haben keine Berechnungsvariante ausgewählt!");
          
          xvt_res_get_str(STR_DLG203_NOTE,buf,sizeof(buf));
          xvt_dm_post_note("%s",buf);
          break;
        }
        xvt_res_get_str(STR_JA,buf,sizeof(buf));
        xvt_res_get_str(STR_NEIN,buf2,sizeof(buf2));
        xvt_res_get_str(STR_BERVER_DEL_ASK,buf3,sizeof(buf3));
        switch (xvt_dm_post_ask(buf,buf2,NULL,buf3))
        {
        case RESP_DEFAULT:       //SICHERN
          {
            Delete_203();
          }
          break;
        case RESP_2:
          
          break;             //zurück
          
        }
        
      }
      break;
      
    case DLG_203_PUSHBUTTON_11: /*OK*/
      {
        int len;
        char ausgabe[100];
        char str_dat[100];//Dick 29.10.98
        helplist=xvt_list_get_sel(listbox203);
        char *str=xvt_slist_get_elt(helplist,0,0);
        for (int i=79;i<=91;i++)
          temp[i-79] = str[i];
        temp[12] = '\0';
        strcpy(ber_spec.name,temp);
        if(wsplist==0)
        {
          /**um Infokennung zu kriegen für wspinQuerprofile read_vari.**/
          xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str2,50);
          strcat(str2,"\\");
          strcat(str2,ber_spec.name);
          
          read_varianten(str2); // in util.cpp
          /**ende**/
          xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str2,50);
          strcpy(str_dat,str2);//Dick 29.10.98
          strcat(str_dat,"\\");//Dick 29.10.98
          strcat(str_dat,STR_SPEC.name);//Dick 29.10.98
          len=strlen(str2);
          str2[len-4]='\0';
          strcat(str2,"dath\\");
          strcpy(ausgabe,str2);

          if( LWA_PROJEKT )
          {
          temp[2]='l';
          temp[3]='p';
          }
          else
          {
            temp[2]='w';
            temp[3]='l';
          };

          strcat(str2,temp);
          if((access(str2,00))==0) //LPLOTDATEI vorhanden
          {
            int handle=open(str2,O_RDONLY);
            int length = filelength(handle);
            close(handle);
            if(((length!=0) && (length!=-1)) || (ergebnis_tabelle_lesen))
            {
              if( LWA_PROJEKT )
              {
                temp[2]='p';
                temp[3]='l';
              };
              strcat(ausgabe,temp);
              ergebnis_tabelle_lesen=FALSE;
              {
                if( LWA_PROJEKT )
                {
                  if((access(ausgabe,00))==0) //es existiert schlon Plot im BCE-Format
                  {
                    if((!berechnen) &&(!editieren))
                    {
                      strcpy(ploti.name,ausgabe);
                    }
                    else
                    {
                      char buf[200],buf2[200],buf3[200];//Dick 26.11.99
                      xvt_res_get_str(STR_JA,buf,sizeof(buf));
                      xvt_res_get_str(STR_NEIN,buf2,sizeof(buf2));
                      xvt_res_get_str(STR_LAENGS_ASK,buf3,sizeof(buf3));
                      switch (xvt_dm_post_ask(buf,buf2,NULL,buf3))
                      {
                      case RESP_DEFAULT:       //Ja
                        {
                          if ( GetFeature( "wsp_nodemo" ) )
                          {
                            int test=ConvertLaengsprofilLwaToBce(str2,ausgabe,pWPL,str_dat,steuerdaten.info,steuerdaten.q,steuerdaten.wsf_l); //Dick 29.10.98
                            //Bley 21.2.2001
                            if(steuerdaten.hmo) //umfunktioniert zu abstand da nicht mehr gebraucht
                            {
                              DIRECTORY dirp;
                              char strdir[100];
                              xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,strdir,90);
                              int len=strlen(strdir);
                              strdir[len-4]='\0';
                              strcat(strdir,"dath");
                              xvt_fsys_convert_str_to_dir(strdir,&dirp);                                            
                              
                              /**********/
                              if (strang_anfang != NULL)
                                destroy_strang_tabelle();
                              strang_anfang=NULL;
                              MakeNewStrang(STRANGANZAHL);
                              read_profil_dat(strang_anfang);
                              
                              
                              xvt_fsys_set_dir(&dirp);
                              strcpy(file_spec.name, temp); // obsolet?
                              laengsschnitt = TRUE; // obsolet?
                              if( !read_profildatei( pWPL, &STR_SPEC.dir, temp ) ) 											
                                Ersetze_Station_mit_Abstand_dll(pWPL,&dirp, temp, strang_anfang, anzahl_strang_entries);
                              
                              /*****6.3******/
                              if (strang_anfang != NULL)
                                destroy_strang_tabelle();
                              strang_anfang=NULL;
                              MakeNewStrang(STRANGANZAHL);
                              read_profil_dat(strang_anfang);
                              
                              /***********/
                              
                              save_profildatei(pWPL);	
                              
                              /*********/
                            }
                            //Bley 21.2.2001
                            
                            strcpy(ploti.name,ausgabe);//Dick 4.02.99
                          }
                          else
                          {
                            //xvt_dm_post_note("Speichern bei Demo nicht möglich");
                            xvt_res_get_str(STR_DEMO_NOTE_3,buf,sizeof(buf));
                            xvt_dm_post_note("%s",buf); 
                          };
                        break;
                      }
                    case RESP_2:             // nein
                      {
                        break;
                      }
                    } //switch
                  } //wenn berechnen & editieren
                }
                else
                {
                  int test=ConvertLaengsprofilLwaToBce(str2,ausgabe,pWPL,str_dat,steuerdaten.info,steuerdaten.q,steuerdaten.wsf_l);//Dick 29.10.98
                  strcpy(ploti.name,ausgabe);//Dick 6.08.98 Vorsichtshalber für den Fahl das ploti.name immer noch NULL-String ist
                }
              }
              else // Version == BCE
              {
                strcpy(ploti.name,ausgabe);
              };
            }
           } //if filelength !=0
           else
           {
             //if(berechnen && editieren || !berechnen && !editieren)	   
             //xvt_dm_post_note("Sie haben keine Längsschnittdatei bei der Berechnung erstellt.");
             char buf[200];//Dick 26.11.99
             xvt_res_get_str(STR_DLG203_NOTE3,buf,sizeof(buf));
             xvt_dm_post_note("%s",buf);
             //else
             //xvt_dm_post_note("Sie haben keine Querprofildatei bei der Berechnung erstellt.");
             abbruch203=TRUE;
           }
         } //lp-Datei da
         else
         {
           //if(berechnen && editieren || !berechnen && !editieren) 
           //xvt_dm_post_note("Sie haben keine Längsschnittdatei bei der Berechnung erstellt.");				 
           char buf[200];//Dick 26.11.99
           xvt_res_get_str(STR_DLG203_NOTE3,buf,sizeof(buf));
           xvt_dm_post_note("%s",buf);
           //else
           //xvt_dm_post_note("Sie haben keine Querprofildatei bei der Berechnung erstellt."); 
           abbruch203=TRUE;
         }
        }
        
        if (helplist!=NULL)
        {
          xvt_slist_destroy(helplist);
          helplist=NULL;
        }
        if (ber_list!=NULL)
        {
          xvt_slist_destroy(ber_list);
          ber_list=NULL;
        }
        xvt_vobj_destroy(xdWindow);
        
       }
       break;
       
        case DLG_203_STEMPEL:
          {
            int len;
            char ausgabe[100];
            char str_dat[100];
            helplist=xvt_list_get_sel(listbox203);
            char *str=xvt_slist_get_elt(helplist,0,0);
            for (int i=79;i<=91;i++)
              temp[i-79] = str[i];
            temp[12] = '\0';
            strcpy(ber_spec.name,temp);
            xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str2,50);
            strcat(str2,"\\");
            strcat(str2,ber_spec.name);
            
            read_varianten(str2); // in util.cpp
            /**ende**/
            xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str2,50);
            strcpy(str_dat,str2);
            strcat(str_dat,"\\");
            strcat(str_dat,STR_SPEC.name);
            len=strlen(str2);
            str2[len-4]='\0';
            strcat(str2,"dath\\");
            strcpy(ausgabe,str2);

            if( LWA_PROJEKT )
            {
              temp[2]='l';
              temp[3]='p';
            }
            else
            {
              temp[2]='w';
              temp[3]='l';
            };

            strcat(str2,temp);
            if((access(str2,00))==0) //LPLOTDATEI vorhanden
            {
              int handle=open(str2,O_RDONLY);
              int length=filelength(handle);
              close(handle);
              if((length!=0) && (length!=-1))
              {
                if( LWA_PROJEKT )
                {
                  temp[2]='p';
                  temp[3]='l';
                }

                char lpPath[MAX_PATH];
                strcpy( lpPath, ausgabe ); // den 7dath - Pfad merken
                strcat(ausgabe,temp);

                if( LWA_PROJEKT )
                {
                  if((access(ausgabe,00))==0) //es existiert schlon Plot im BCE-Format
                  {
                    if((!berechnen) &&(!editieren))
                    {
                      strcpy(ploti.name,ausgabe);
                    }
                    
                  }
                  else
                  {
                    int test=ConvertLaengsprofilLwaToBce(str2,ausgabe,pWPL,str_dat,steuerdaten.info,steuerdaten.q,steuerdaten.wsf_l);//Dick 29.10.98
                    strcpy(ploti.name,ausgabe);//Dick 6.08.98 Vorsichtshalber für den Fahl das ploti.name immer noch NULL-String ist
                  }
                }
                else
                  strcpy(ploti.name,ausgabe);

                if((access(ausgabe,00))==0) // Plot wurde erstellt
                {
                  for( int i = 0; i < 12; i++ )
                    file_spec.name[i] = temp[i];
                  file_spec.name[12]='\0';
  //                xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,directory,99);
                  xvt_fsys_convert_str_to_dir( lpPath, &file_spec.dir );
      //            xvt_fsys_set_dir(&file_spec.dir);
//                  laengsschnitt=TRUE; // obsolet?
                  if(read_profildatei( pWPL, &file_spec.dir, file_spec.name ) == 0 )   //Profildatei einlesen für file_spec.name
                  {
                    if (!xvt_win_create_res(WIN_121, TASK_WIN, EM_ALL, WIN_121_eh, 0L))                  
                      xvt_dm_post_error("Can't open win121");
                    else
                    {
                      stempel=TRUE;
                      if (helplist!=NULL)
                      {
                        xvt_slist_destroy(helplist);
                        helplist=NULL;
                      }
                      if (ber_list!=NULL)
                      {
                        xvt_slist_destroy(ber_list);
                        ber_list=NULL;
                      }
                    }
                    // laengsschnitt=FALSE;
                  }
                  //else
                  //  laengsschnitt=FALSE;
                }
              }
              else
              {
                
                //xvt_dm_post_note("Sie haben keine Längsschnittdatei bei der Berechnung erstellt.");
                char buf[200];//Dick 26.11.99
                xvt_res_get_str(STR_DLG203_NOTE3,buf,sizeof(buf));
                xvt_dm_post_note("%s",buf);
                //abbruch203=TRUE;
              }
            }
            else
            {
              
              //xvt_dm_post_note("Sie haben keine Längsschnittdatei bei der Berechnung erstellt.");
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_DLG203_NOTE3,buf,sizeof(buf));
              xvt_dm_post_note("%s",buf);
              //abbruch203=TRUE;
            }
            
            abbruch203=TRUE;
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
      xvt_list_clear(listbox203);
      int slist_count=xvt_slist_count(ber_list);
      
      if (slist_count>0)
      {
        /*******/
        //NewSetFontFunc(listbox203);
        xvt_list_add(listbox203, -1, (char *)ber_list);   // Ausgabe in LISTBOX
        xvt_list_set_sel(listbox203, 0, TRUE);
        helplist=xvt_list_get_sel(listbox203);
        char *str=xvt_slist_get_elt(helplist,0,0);
        for (int i=79;i<=91;i++)
          temp[i-79] = str[i];
        temp[12] = '\0';
        strcpy(ber_spec.name,temp);
        xvt_timer_destroy(timer_203);
      }
      else
      {
        is_dlg203ok=FALSE;
        ber_spec.name[0]='\0';
        if (helplist!=NULL)
        {
          xvt_slist_destroy(helplist);
          helplist=NULL;
        }
        if (ber_list!=NULL)
        {
          xvt_slist_destroy(ber_list);
          ber_list=NULL;
        }
        timer_an_203=TRUE;
        
        xvt_vobj_destroy(xdWindow);
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
  delete[]temp;
  //	delete[]str2;
  return 0L;
  
}
