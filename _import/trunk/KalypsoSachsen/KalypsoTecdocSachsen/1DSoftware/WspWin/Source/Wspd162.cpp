/*	Handler for dialog DLG_CHANGE_RAUH ("Rauheiten global ändern ")*/
#include <windows.h>
#include "xvt.h"
#include "wspwin.h"

#include "resource.h"

#include "global_types.h"
#include "global_vars.h"
#include "..\..\wspdlg\include\export.h"

#include "list.h"
#include "readprof.h"
#include "bce_allg.h"

#include "global.h"

#include "rauh.h"
#include "typen.h"

#include "wsphilfe.h"

extern XVT_HELP_INFO hi;

#define DLG_RES_ID DLG_CHANGE_RAUH
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODAL

#define IDC_KS          1027
#define IDC_KST         1028
#define IDC_RH_VOR      1029
#define IDC_DAT_DELETE  21
#define IDC_RAM162      1032


WINDOW win_162_listbox,
		 win_edit_l,
		 win_edit_m,
		 win_edit_r,
         dlg_162;
double rauheit_links,rauheit_mitte,rauheit_rechts;
FILE *protokoll_file;

extern WINDOW WIN123, main_win;;
extern int return_code;
extern BOOLEAN berechnen, editieren,abbruch_217ff;
extern plot ploti;
extern BOOLEAN  ueberschreiben;

extern WSP_PROFIL_LISTE *pWPL;
BOOLEAN Plot_DB=FALSE;





/*	Handler for dialog DLG_CHANGE_RAUH ("Rauheiten global ändern ")*/
long XVT_CALLCONV1
#if XVT_CC_PROTO
DLG_CHANGE_RAUH_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
DLG_CHANGE_RAUH_eh XVT_CALLCONV2 (xdWindow, xdEvent)
WINDOW xdWindow;
EVENT *xdEvent;
#endif
{
	short xdControlId = xdEvent->v.ctl.id;

	switch (xdEvent->type) {
	case E_CREATE:
		{
         
          dlg_162=xdWindow;
          xvt_fsys_set_dir(&STR_SPEC.dir);//Dick 27.07.99
	if(dlg_136!=NULL_WIN)
	{
		xvt_fsys_set_dir(&STR_SPEC.dir);
		save_str_datei();

	}
		
		if ((!berechnen) &&(!editieren))   //d.h. Plotten
		  {
		  xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_CHANGE_RAUH_PB_OK)),FALSE);
		  xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_CHANGE_RAUH_EDIT_5)),FALSE);
		  xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_CHANGE_RAUH_EDIT_6)),FALSE);
		  xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_CHANGE_RAUH_EDIT_7)),FALSE);
		  xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_CHANGE_RAUH_TEXT_8)),FALSE);
		  xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_CHANGE_RAUH_TEXT_9)),FALSE);
		  xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_CHANGE_RAUH_TEXT_10)),FALSE);
		  xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_CHANGE_RAUH_TEXT_11)),FALSE);

          xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,IDC_KS)),FALSE);
		  xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,IDC_KST)),FALSE);
		  xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,IDC_RH_VOR)),FALSE);
		  xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,IDC_DAT_DELETE)),FALSE);
		  xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,IDC_RAM162)),FALSE);

          xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_162_STEMPEL)),TRUE);
		  xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_162_STANDART_PLOT)),TRUE);
          xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_162_PLOTDB)),TRUE);
		  }
		 else
		  xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_CHANGE_RAUH_PB_PLOTTEN)),FALSE);

		 return_code=0;
		 win_162_listbox = xvt_win_get_ctl(xdWindow,DLG_CHANGE_RAUH_LBOX);


		 win_edit_l=xvt_win_get_ctl(xdWindow,DLG_CHANGE_RAUH_EDIT_5);
		 win_edit_m=xvt_win_get_ctl(xdWindow,DLG_CHANGE_RAUH_EDIT_6);
		 win_edit_r=xvt_win_get_ctl(xdWindow,DLG_CHANGE_RAUH_EDIT_7);
          WINDOW ctl[1];
          ctl[0]=xvt_win_get_ctl(xdWindow,IDC_RH_VOR);
         xvt_ctl_check_radio_button(xvt_win_get_ctl(xdWindow,IDC_RH_VOR),ctl,1);//Dick 20.10.98 vorselektieren

		 NewSetFontFunc(win_162_listbox);    //Schriftart für listbox86 ändern

		 read_profil_dat(strang_anfang);      //str-Datei einlesen,in:readprof.cpp

		 if (prof_datei != NULL)
				  xvt_list_add(win_162_listbox, -1, (char*)prof_datei);   //Ausgabe in Listbox
         xvt_list_set_sel(win_162_listbox,0,TRUE);
         xdEvent->type=E_CONTROL;
         xdControlId= xdEvent->v.ctl.id=DLG_CHANGE_RAUH_LBOX;
         xvt_win_dispatch_event(xdWindow,xdEvent);
         xvt_scr_set_focus_vobj(win_162_listbox);


         
         

		 if (hi!=NULL_HELP_INFO)
             {
              if ((!berechnen) &&(!editieren))
                  xvt_help_set_win_assoc(hi, xdWindow, HID_KAPITEL_6_7_1, 0L);
              else
                  xvt_help_set_win_assoc(hi, xdWindow, HID_KAPITEL_7_3_1, 0L);

             }
		}
		break;
	case E_DESTROY:
		{
         dlg_162=NULL_WIN;
		}
		break;
	case E_FOCUS:
		{
		/*	Dialog has lost or gained focus.	*/
		if (xdEvent->v.active)  {
			/*		Dialog has gained focus		*/
		} else {
			/*	Dialog has lost focus	*/
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
		case DLG_CHANGE_RAUH_PB_OK: /* "Ändern" */
			{
			 int anzahl,ok;
			 BOOLEAN cancel=FALSE;
			 SLIST select;
			 char *file_ptr,files[150],chr_li[15],chr_mi[15],chr_re[15];
			 char directory[100];
             char buf[200],buf2[200],buf3[200],buf4[200],buf5[200],buf6[200];//Dick 26.11.99
             BOOLEAN ks=FALSE,kst=FALSE,vordef=TRUE;
             int rauh_change_typ=0;

			 xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,directory,99);
			 xvt_fsys_convert_str_to_dir(directory,&file_spec.dir);
			 xvt_fsys_set_dir(&file_spec.dir);


			 xvt_vobj_get_title(win_edit_l,chr_li,10);
			 xvt_vobj_get_title(win_edit_m,chr_mi,10);
			 xvt_vobj_get_title(win_edit_r,chr_re,10);

             ks=xvt_ctl_is_checked (xvt_win_get_ctl(xdWindow,IDC_KS));
             kst=xvt_ctl_is_checked (xvt_win_get_ctl(xdWindow,IDC_KST));
             vordef=xvt_ctl_is_checked (xvt_win_get_ctl(xdWindow,IDC_RH_VOR));
             if(ks) rauh_change_typ=1;
             if(kst) rauh_change_typ=2;
             if(vordef) rauh_change_typ=0;

			 if ( (ok = is_zahl(&chr_li[0]))==0) cancel =TRUE;
			 if ( (ok = is_zahl(&chr_mi[0]))==0) cancel =TRUE;
			 if ( (ok = is_zahl(&chr_re[0]))==0) cancel =TRUE;

             if (strlen(chr_li)<=0 && strlen(chr_mi)<=0 && strlen(chr_re)<=0) 
                 cancel =TRUE;
			 /*if (strlen(chr_li)<=0) cancel =TRUE;
			 if (strlen(chr_mi)<=0) cancel =TRUE;
			 if (strlen(chr_re)<=0) cancel =TRUE;*///Dick 26.07.99 weg
             //Dick 26.07.99 neu
             if (strlen(chr_li)<=0)
                 rauheit_links=BCE_NAN;
             else
                 rauheit_links  = atof(chr_li);
             if (strlen(chr_mi)<=0)
                 rauheit_mitte=BCE_NAN;
             else
                 rauheit_mitte  = atof(chr_mi);
             if (strlen(chr_re)<=0)
                 rauheit_rechts = BCE_NAN;
             else
                 rauheit_rechts = atof(chr_re);
             //
			 anzahl = xvt_list_count_sel(win_162_listbox);
			 select=xvt_slist_create();
			 select = xvt_list_get_sel(win_162_listbox);
			 if ((anzahl)&&(!cancel))
			 {
         if( !GetFeature( "wsp_nodemo" ) )
         {
           xvt_res_get_str(STR_DEMO_NOTE_3,buf,sizeof(buf));
           xvt_dm_post_note("%s",buf);
				 //xvt_dm_post_note("Speichern bei Demo nicht möglich");
           break;
         };

			  protokoll_file = fopen("err_rauh.tmp","w");  // Protokolldatei
			  if (protokoll_file == NULL)
				 {
                  //Dick 26.11.99
                  xvt_res_get_str(STR_WSPD162_NOTE_1,buf,sizeof(buf));
                  xvt_dm_post_note("%s",buf);
				  //xvt_dm_post_note("Protokolldatei läßt sich nicht anlegen.Keine Änderungen möglich!");
				  break;
				 }
              xvt_res_get_str(STR_AENDPROT_F_RAUH,buf,sizeof(buf));
			  fprintf(protokoll_file,"********************************************************\n");
			  fprintf(protokoll_file,"          %s              \n",buf);
			  fprintf(protokoll_file,"********************************************************\n\n");

			  for (int f=0;f<anzahl;f++)
				{
				 file_ptr=xvt_slist_get_elt(select,f,0L);
				 strcpy(files,file_ptr);

				 /* Dateiname nach file_spec.name kopieren ,Name ab Position 44 in str[] */
				 for (int i=0;i<12;i++)
					file_spec.name[i] = files[i+44];
				 file_spec.name[12]='\0';
                                                      
                 xvt_res_get_str(STR_DATEI,buf,sizeof(buf));
				 fprintf(protokoll_file,"%s:\t%s\\%s\n",buf,directory,file_spec.name);
				 fprintf(protokoll_file,"--------------------------------------------------------\n\n");

                 xvt_res_get_str(STR_NEU_WERTE_FUER,buf,sizeof(buf));
                 xvt_res_get_str(STR_LINKES_VORLAND,buf2,sizeof(buf2));
                 xvt_res_get_str(STR_FLUSSSCHLAUCH,buf3,sizeof(buf3));
                 xvt_res_get_str(STR_RECHTES_VORLAND,buf4,sizeof(buf4));
                 xvt_res_get_str(STR_VON,buf5,sizeof(buf5));
                 xvt_res_get_str(STR_BIS,buf6,sizeof(buf6));
				 fprintf(protokoll_file,"%s:\n\n",buf);
				 fprintf(protokoll_file,"%s\t%s\t%s\n",buf2,buf3,buf4);
                 fprintf(protokoll_file,"%3.3lf\t\t\t%3.3lf\t\t\t%3.3lf\n",(rauheit_links>=0.0)?rauheit_links:-1,(rauheit_mitte>=0.0)?rauheit_mitte:-1,(rauheit_rechts>=0.0)?rauheit_rechts:-1);
				 fprintf(protokoll_file,"--------------------------------------------------------\n\n");
				 fprintf(protokoll_file,"%s:\t%s:\t\t%s:\t%s:\t\t%s:\t%s:\n\n",buf5,buf6,buf5,buf6,buf5,buf6);

				 if( read_profildatei( pWPL, &STR_SPEC.dir, file_spec.name ) == 0 )   //Profildatei einlesen für file_spec.name
				  {
					ok = list->change_rauheiten(protokoll_file,file_spec.name,rauheit_links,rauheit_mitte,rauheit_rechts,rauh_change_typ ); //->readprof.cpp
					if (ok)
						{
						 save_profildatei(pWPL);     //Profildatei wieder sichern
                          
                         xvt_res_get_str(STR_DATEI_SUCCESS_CHANGE,buf,sizeof(buf));
						 fprintf(protokoll_file,"--------------------------------------------------------\n");
						 fprintf(protokoll_file,"            %s               \n",buf);
						 fprintf(protokoll_file,"--------------------------------------------------------\n\n\n\n");
						}
					else // Fehler in change_rauheiten...
						{
                         xvt_res_get_str(STR_ERROR_CHANGE,buf,sizeof(buf));
						 fprintf(protokoll_file,"--------------------------------------------------------\n");
						 fprintf(protokoll_file,"     %s             \n",buf);
						 fprintf(protokoll_file,"--------------------------------------------------------\n\n\n\n");
						}
				  }
				}
			  fclose(protokoll_file);

              xvt_res_get_str(STR_JA,buf,sizeof(buf));
              xvt_res_get_str(STR_NEIN,buf2,sizeof(buf2));
              xvt_res_get_str(STR_PROTOKOLL,buf3,sizeof(buf3));
              xvt_res_get_str(STR_WSPD162_ASK_1,buf4,sizeof(buf4));
              switch (xvt_dm_post_ask(buf,buf2,buf3,"%s",buf4))
			  //switch(xvt_dm_post_ask("Ja","Nein","Protokoll","Datei(en) geändert.\nBearbeitung beenden ?"))
						  {
							case RESP_DEFAULT:
							 {
							  fclose(protokoll_file);
							  xvt_vobj_destroy(xdWindow);
							 }
							case RESP_2:
							break;
							case RESP_3:
							  {
								fclose(protokoll_file);
								xvt_vobj_destroy(xdWindow);
								return_code=E_AUTOLOAD;
							  }
							break;
						  }
			  fclose(protokoll_file);
			 }
			 else
				{
				 if (cancel)
                     {
                      xvt_res_get_str(STR_WSPD162_NOTE_2,buf,sizeof(buf));
                      xvt_dm_post_error("%s",buf);
					  //xvt_dm_post_error("Einer der Rauheitswerte hat ein falsches Zahlenformat.\nBitte Eingabe korrigieren !");
                     }
				 else if (anzahl ==0)
                     {
                      xvt_res_get_str(STR_WSPD162_NOTE_3,buf,sizeof(buf));
                      xvt_dm_post_error("%s",buf);
						 //xvt_dm_post_error("Bitte Profildateien auswählen !");
                     }

				}
             xvt_slist_destroy(select);
			}
			break;
		case DLG_CHANGE_RAUH_PB_CANCEL: /* "Abbrechen" */
			{
			 xvt_vobj_destroy(xdWindow);
			}
			break;

        case DLG_CHANGE_RAUH_PB_PLOTTEN: /* "Plotten" */
			{
			 int anzahl;//Dick 11.02.99 weg  ,ok;
			 BOOLEAN cancel=FALSE;
			 SLIST select;
			 char *file_ptr,files[150];//Dick 11.02.99 weg ,chr_li[15],chr_mi[15],chr_re[15];
			 //Dick 11.02.99 weg char directory[100];
              int back=1;
			 anzahl = xvt_list_count_sel(win_162_listbox);
				select=xvt_slist_create();
				select = xvt_list_get_sel(win_162_listbox);
                for(int pos=0;pos<anzahl;pos++)
                    {
                     file_ptr=xvt_slist_get_elt(select,pos,0L);
                     strcpy(files,file_ptr);
                     
                     // Dateiname nach ploti.name kopieren ,Name ab Position 44 in str[] 
                     xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, ploti.name, 80);
                     strcat(ploti.name,"\\");
                     int len=strlen(ploti.name);
                     for(int i=0;i<12;i++)
                         ploti.name[i+len]=files[i+44];
                     ploti.name[i+len]='\0';//Dick 1.10.99 +1 weg
                     /*for (int i=0;i<12;i++)
                     ploti.name[i] = files[i+44];
                     ploti.name[12]='\0';
                     */
                     return_code=1;
                     //neu Dick 2.12.98
                     int back=wahl();
                     if(anzahl==1)
                         {
                         if (!xvt_dlg_create_res(WD_MODAL,DLG_218, EM_ALL, DLG_218_eh, 0L))
                             xvt_dm_post_error("Can't open dialog 218");
                         }
                     else
                         {
                          ueberschreiben=TRUE;
                          abbruch_217ff=FALSE;
                          back=masstab_holen(); //in rauh.cpp
                         }
                     if(!abbruch_217ff && !GetFeature( "wsp_nodemo" ) == FALSE )
                       schreibe_input_plot_datei(pos); //in rauh.cpp //Inputdatei für Plotprogramm
                    }

                if( GetFeature( "wsp_nodemo" ) )
                {
                  if(!abbruch_217ff && back!=0)
                  {
                    starte_plot_programm(anzahl); //in rauh.cpp
                  }
                  else
                    abbruch_217ff=FALSE;
                }
                else
                {
                  char buf[200];//Dick 26.11.99
                  xvt_res_get_str(STR_DEMO_NOTE_4,buf,sizeof(buf));
                  xvt_dm_post_note("%s",buf);
                 //xvt_dm_post_note("Erstellung von Plotfiles bei Demo nicht möglich");
                };
                
                xvt_fsys_set_dir(&file_spec.dir);//Dick 25.08.99 
                xvt_slist_destroy(select);
			}
			break;
		case DLG_CHANGE_RAUH_LBOX: /* "List Box 1" */
			{
			 if (xdEvent->v.ctl.ci.v.lbox.dbl_click)
			 {		/*		double click		*/
			 }
			 else if (!((!berechnen) &&(!editieren)))   //Dick 25.08.99
       {		/*		single click	*/
         int anzahl,ok=FALSE,rauh_typ;
         SLIST select;
         char *string,str_tmp[MAX_PATH];
         WINDOW ctl[3];
         ctl[0]=xvt_win_get_ctl(xdWindow,IDC_KS);
         ctl[1]=xvt_win_get_ctl(xdWindow,IDC_KST);
         ctl[2]=xvt_win_get_ctl(xdWindow,IDC_RH_VOR);
         anzahl = xvt_list_count_sel(win_162_listbox);
         if(anzahl==1)
         {                  
           select=xvt_slist_create();
           select = xvt_list_get_sel(win_162_listbox);
           string=xvt_slist_get_elt(select,0,0L);
           strcpy(str_tmp,string);
           for (int i=0;i<12;i++)
             file_spec.name[i] = str_tmp[i+44];
           file_spec.name[12]='\0';
           if( read_profildatei( pWPL, &STR_SPEC.dir, file_spec.name ) == 0 )   //Profildatei einlesen für file_spec.name
           {
             double links,mitte,rechs;
             ok=list->Get_Rauheit(&links,&mitte,&rechs,&rauh_typ);
             if(ok)
             {
               char chr_li[15], chr_mi[15], chr_re[15];

               if( links != BCE_NAN )
               {
                 sprintf(chr_li,"%.4lf",links);
                 xvt_vobj_set_title(win_edit_l,chr_li);
               }
               else
                 xvt_vobj_set_title(win_edit_l,"");
               
               if(mitte!=BCE_NAN)
               {
                 sprintf(chr_mi,"%.4lf",mitte);
                 xvt_vobj_set_title(win_edit_m,chr_mi);
               }
               else
                 xvt_vobj_set_title(win_edit_m,"");
               
               if(rechs!=BCE_NAN)
               {
                 sprintf(chr_re,"%.4lf",rechs);
                 xvt_vobj_set_title(win_edit_r,chr_re);
               }
               else
                 xvt_vobj_set_title(win_edit_r,"");
               
               if(rauh_typ==RAUHIGKEIT)
                 xvt_ctl_check_radio_button(xvt_win_get_ctl(xdWindow,IDC_KS),ctl,3);
               else if(rauh_typ==RAUHIGKEIT_KST)
                 xvt_ctl_check_radio_button(xvt_win_get_ctl(xdWindow,IDC_KST),ctl,3);
               else
                 xvt_ctl_check_radio_button(xvt_win_get_ctl(xdWindow,IDC_RH_VOR),ctl,3);
             }
             else
             {
               xvt_vobj_set_title(win_edit_l,"");
               xvt_vobj_set_title(win_edit_m,"");
               xvt_vobj_set_title(win_edit_r,"");
               xvt_ctl_check_radio_button(xvt_win_get_ctl(xdWindow,IDC_RH_VOR),ctl,3);
             }
           }
           else
           {
             xvt_vobj_set_title(win_edit_l,"");
             xvt_vobj_set_title(win_edit_m,"");
             xvt_vobj_set_title(win_edit_r,"");
             xvt_ctl_check_radio_button(xvt_win_get_ctl(xdWindow,IDC_RH_VOR),ctl,3);
           }
           xvt_slist_destroy(select);
         }
         else if(anzahl==0)
         {
           xvt_vobj_set_title(win_edit_l,"");
           xvt_vobj_set_title(win_edit_m,"");
           xvt_vobj_set_title(win_edit_r,"");
           xvt_ctl_check_radio_button(xvt_win_get_ctl(xdWindow,IDC_RH_VOR),ctl,3);
         }
         
       }
			}
			break;
		case DLG_CHANGE_RAUH_EDIT_5:
		case DLG_CHANGE_RAUH_EDIT_6:
		case DLG_CHANGE_RAUH_EDIT_7:
			break;
        case IDC_DAT_DELETE://Dick 21.10.98
        {
        int anzahl;
        int ds_exist_nr=0;
        SLIST select;
        char *file_ptr,files[150];
        char directory[100];
        char buf[200],buf2[200],buf3[200],buf4[200];//Dick 26.11.99

        xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,directory,99);
        xvt_fsys_convert_str_to_dir(directory,&file_spec.dir);
        xvt_fsys_set_dir(&file_spec.dir);
        
        anzahl = xvt_list_count_sel(win_162_listbox);
        select=xvt_slist_create();
        select = xvt_list_get_sel(win_162_listbox);
        if (anzahl)
			 {
          if( !GetFeature( "wsp_nodemo" ) )
          {
            xvt_res_get_str(STR_DEMO_NOTE_3,buf,sizeof(buf));
            xvt_dm_post_note("%s",buf);
            //xvt_dm_post_note("Speichern bei Demo nicht möglich");
            break;
          };

			  protokoll_file = fopen("err_rauh.tmp","w");  // Protokolldatei
			  if (protokoll_file == NULL)
				 {
                  xvt_res_get_str(STR_WSPD162_NOTE_1,buf,sizeof(buf));
                  xvt_dm_post_note("%s",buf);
				  //xvt_dm_post_note("Protokolldatei läßt sich nicht anlegen.Keine Änderungen möglich!");
				  break;
				 }
              xvt_res_get_str(STR_AENDPROT_F_RAUH,buf,sizeof(buf));
              xvt_res_get_str(STR_LOESCHEN,buf2,sizeof(buf2));
			  fprintf(protokoll_file,"********************************************************\n");
			  fprintf(protokoll_file,"          %s(%s)     \n",buf,buf2);
			  fprintf(protokoll_file,"********************************************************\n\n");


              for (int f=0;f<anzahl;f++)
                  {
                  file_ptr=xvt_slist_get_elt(select,f,0L);
                  strcpy(files,file_ptr);
                  
                  /* Dateiname nach file_spec.name kopieren ,Name ab Position 44 in str[] */
                  for (int i=0;i<12;i++)
                      file_spec.name[i] = files[i+44];
                  file_spec.name[12]='\0';

                  xvt_res_get_str(STR_DATEI,buf,sizeof(buf));
                  fprintf(protokoll_file,"%s:\t%s\\%s\n",buf,directory,file_spec.name);
				 fprintf(protokoll_file,"--------------------------------------------------------\n\n");

                  if( read_profildatei( pWPL, &STR_SPEC.dir, file_spec.name  ) == 0 )   //Profildatei einlesen für file_spec.name
                      {
                      ds_exist_nr=list->ExistDatensatzTyp(RAUHIGKEIT);
                      if(ds_exist_nr>0)
                          list->DeleteNode(ds_exist_nr, (int*)ds_info, (int*)typ);
                      else
                          {
                          ds_exist_nr=list->ExistDatensatzTyp(RAUHIGKEIT_KST);
                          if(ds_exist_nr>0)
                              list->DeleteNode(ds_exist_nr, (int*)ds_info, (int*)typ);
                          else
                              {
                               xvt_res_get_str(STR_CHANGE_RAUH_4,buf,sizeof(buf));
                              fprintf(protokoll_file,"%s\n\n",buf);
                              }
                          } 
                      
                      if(ds_exist_nr>0 && 
                          !list->ExistDatensatzTyp(RAUHIGKEIT) && 
                          !list->ExistDatensatzTyp(RAUHIGKEIT_KST))
                          {
                          save_profildatei(pWPL);
                          xvt_res_get_str(STR_RAUH_SUCCESS_DELETE,buf,sizeof(buf));
                          fprintf(protokoll_file,"%s",buf);
                          }
                      else // Fehler in change_rauheiten...
                          {
                           xvt_res_get_str(STR_ERROR_CHANGE,buf,sizeof(buf));
                          fprintf(protokoll_file,"--------------------------------------------------------\n");
                          fprintf(protokoll_file,"     %s             \n",buf);
                          fprintf(protokoll_file,"--------------------------------------------------------\n\n\n\n");
                          }
                      }
                  }//for
              fclose(protokoll_file);
              xvt_res_get_str(STR_JA,buf,sizeof(buf));
              xvt_res_get_str(STR_NEIN,buf2,sizeof(buf2));
              xvt_res_get_str(STR_PROTOKOLL,buf3,sizeof(buf3));
              xvt_res_get_str(STR_WSPD162_ASK_1,buf4,sizeof(buf4));
              switch (xvt_dm_post_ask(buf,buf2,buf3,"%s",buf4))
			  //switch(xvt_dm_post_ask("Ja","Nein","Protokoll","Datei(en) geändert.\nBearbeitung beenden ?"))
						  {
							case RESP_DEFAULT:
							 {
							  fclose(protokoll_file);
							  xvt_vobj_destroy(xdWindow);
							 }
							case RESP_2:
							break;
							case RESP_3:
							  {
								fclose(protokoll_file);
								xvt_vobj_destroy(xdWindow);
								return_code=E_AUTOLOAD;
							  }
							break;
						  }
			  fclose(protokoll_file);
            } //if
        else
            {
             xvt_res_get_str(STR_WSPD162_NOTE_3,buf,sizeof(buf));
             xvt_dm_post_error("%s",buf);
             //xvt_dm_post_error("Bitte Profildateien auswählen !");            
            }
         xvt_slist_destroy(select);
        }
        break;
        case DLG_162_STEMPEL:
            {
			 int anzahl;  //Dick 11.02.99 weg ,ok;
			 
			 SLIST select;
			 char *file_ptr,files[150];//Dick 11.02.99 weg ,chr_li[15],chr_mi[15],chr_re[15];
			 char directory[100];
             char buf[200];

			 xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,directory,99);
			 xvt_fsys_convert_str_to_dir(directory,&file_spec.dir);
			 xvt_fsys_set_dir(&file_spec.dir);			 

			 anzahl = xvt_list_count_sel(win_162_listbox);
			 select=xvt_slist_create();
			 select = xvt_list_get_sel(win_162_listbox);
			// if (anzahl)//zur Zeit nicht möglich Dick 2.12.98
			// { //zur Zeit nicht möglich Dick 2.12.98
             if(anzahl!=1)
                 {
                  xvt_res_get_str(STR_WSPD162_NOTE_4,buf,sizeof(buf));
                  xvt_dm_post_error("%s",buf);
                    //xvt_dm_post_note("Bitte 1 Datei auswählen");
                 }
			 else
			  {
			  for (int f=0;f<anzahl;f++)
				{
				 file_ptr=xvt_slist_get_elt(select,f,0L);
				 strcpy(files,file_ptr);

				 /* Dateiname nach file_spec.name kopieren ,Name ab Position 44 in str[] */
				 for (int i=0;i<12;i++)
					file_spec.name[i] = files[i+44];
				 file_spec.name[12]='\0';

				 
				 if( read_profildatei( pWPL, &STR_SPEC.dir, file_spec.name ) == 0 )   //Profildatei einlesen für file_spec.name
				  {
                    if (!xvt_win_create_res(WIN_121, TASK_WIN, EM_ALL, WIN_121_eh, 0L))                  
                      xvt_dm_post_error("Can't open win121");
                    
				  }
				}
			 }
             xvt_slist_destroy(select);
			}
            break;
        case DLG_162_PLOTDB:
          { 
            Plot_DB=TRUE;
            if (read_Plot_DB( NULL, pWPL)==0)   // ist hier jemals exist_plot == TRUE?
            {
              if (!xvt_win_create_res(WIN_121, TASK_WIN, EM_ALL, WIN_121_eh, 0L))                  
                xvt_dm_post_error("Can't open win121");                    
            }
          }
            break;
        case DLG_162_STANDART_PLOT:
            {
             int anzahl,ok=0;
			 
			 SLIST select;
			 char *file_ptr,files[150];
			 char directory[100];
             char buf[200],buf2[200];
             xvt_res_get_str(STR_ABBRECHEN,buf,sizeof(buf));
             xvt_res_get_str(STR_WSPD162_ASK_2,buf2,sizeof(buf2));
             switch(xvt_dm_post_ask(buf,"OK",NULL,"%s",buf2)) // "Abbrechen","OK",NULL,"Wollen Sie wirklich\nin allen markierten\nProfilen die Voreinstellungen\nzum Plotten übernehmen?"
             {
             case RESP_DEFAULT:							 
               break;
             case RESP_2:
               ok = 1;
               break;
             }
             if(ok)
                 {
			 xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,directory,99);
			 xvt_fsys_convert_str_to_dir(directory,&file_spec.dir);
			 xvt_fsys_set_dir(&file_spec.dir);			 

			 anzahl = xvt_list_count_sel(win_162_listbox);
			 select=xvt_slist_create();
			 select = xvt_list_get_sel(win_162_listbox);
			 if (anzahl)           			 
			  {
			  for (int f=0;f<anzahl;f++)
				{
				 file_ptr=xvt_slist_get_elt(select,f,0L);
				 strcpy(files,file_ptr);

				 /* Dateiname nach file_spec.name kopieren ,Name ab Position 44 in str[] */
				 for (int i=0;i<12;i++)
					file_spec.name[i] = files[i+44];
				 file_spec.name[12]='\0';

				 
				 if( read_profildatei( pWPL, &STR_SPEC.dir, file_spec.name ) == 0 )   //Profildatei einlesen für file_spec.name
				  {					                                      
                    change_Plot_profildatei(pWPL);
                    save_profildatei(pWPL);     //Profildatei wieder sichern					
				  }
				}
			  
			  
			 }//anzahl             
			 else
				{
				 if(anzahl ==0)
                     {
                      xvt_res_get_str(STR_WSPD162_NOTE_3,buf,sizeof(buf));
                      xvt_dm_post_error("%s",buf);
						 //xvt_dm_post_error("Bitte Profildateien auswählen !");
                     }

				}
                  xvt_slist_destroy(select);
                 }//ok

             
            }
            break;
		default:
			break;
		}
		}
		break;
    

	case E_USER:
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
