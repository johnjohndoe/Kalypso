/****************************************************************************
*             WSPM001 .CPP                                                  *
*             10.03.1995                                                    *
****************************************************************************/

#include <windows.h>
#include "xvt.h"

#include <fcntl.h>

#include "resource.h"
#include "wspwin.h"

#include "global_types.h"
#include "global_vars.h"

#include "..\..\wspdlg\include\export.h"
#include "bce\include\wspwin_regentries.h"

#include "typen.h"
#include "L_typen.h"

#include "jabron1.h"

#include "flaeche.h"
#include "list.h"
#include "qlist.h"
#include "autosketchdlg.h"
#include "wspdde.h"
#include "rauh.h"
#include "util2.h"
#include "read_cfg.h"
#include "util.h"
#include "strang.h"
#include "readprof.h"
#include "profpro2.h"
#include "aufnehm.h"
#include "read.h"
#include "wsplist.h"
#include "volume1.h"
#include "dxf_edit.h"
#include "waspila.h"
#include "da66.h"
#include "laengs3.h"
#include "listausw.h"
#include "vgl_list.h"
#include "prueflst.h"
#include "wsplp2qp.h"
#include "verzweig.h"

// globale Variablen 

int wsplist;

FILE_SPEC alte_zustandsdatei, neue_zustandsdatei;

BOOLEAN var_dlg135 = FALSE, timermain = FALSE, berechnen, editieren, konvertierung, proj_rem = FALSE, 
  abbruch203;    // soll projekt gelöscht werden (DLG148)

BOOLEAN laengsschnitt;
BOOLEAN ergebnis_tabelle_lesen = FALSE;
long timer_main;
int timer_zaehler, ok, len, dlg135ref;      //Rückgabewerte aus Dialog 135

int  dlg147def = 0;//Definition für dialog 147
int return_code = 0;

int proj_archiv = 0;

// globale externe Variablen

extern bool map_object_command;//Dick 29.09.99

extern BOOLEAN Ber_edit_sperr;
extern BOOLEAN Einf;
extern BOOLEAN Edit_Fehler;
extern HANDLE hProc_dag_bat;

extern MENU_ITEM *main_menu_tree;

extern WINDOW dlg_sonderprofil, main_win;

BOOLEAN abbruch205 = FALSE;
extern BOOLEAN abbruch206, abbruch_217ff, is_128_ok,       // Rückgabewerte aus Dialogen
  plotoption; // für Plotoptionen in Dlg203

extern char str_zustand[16], str_gewaesser[16];

extern SLIST batch_list, profile_slist, probezlist; 
extern FILE_SPEC ber_spec;

extern struct _WSP_PROFIL_LISTE *pWPL;

extern plot ploti;

extern st_daten steuerdaten;

// Vorwärtsdeklarationen

extern long XVT_CALLCONV1 DLG_ABOUT_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent); // wspd102.cpp

void
#if XVT_CC_PROTO
do_TASK_MENUBAR (WINDOW xdWindow, EVENT *xdEvent)
#else
do_TASK_MENUBAR (xdWindow, xdEvent)
WINDOW xdWindow;
EVENT *xdEvent;
#endif
{
  MENU_TAG tag = xdEvent->v.cmd.tag;
  
  if(!Einf&&!Edit_Fehler&&hProc_dag_bat==NULL)
    switch (tag)
  {
    case TASK_MENUBAR_43_44: // Menu "Projekte speich. unter"
      {
        
        if(win122!=NULL_WIN)
        {
          xvt_vobj_destroy(win122);
          win122=NULL_WIN;
        }
        if (dlg_136 != NULL_WIN)
        {
          xvt_vobj_destroy(dlg_136);
          dlg_136 = NULL_WIN;
        }
        dlg147def=1;
        if (!xvt_dlg_create_res(WD_MODAL, DLG_147, EM_ALL, DLG_147_eh, 0L))
          xvt_dm_post_error("Can't open dialog");
        dlg147def=0;
        
      }
      break;
      
    case TASK_MENUBAR_43_45: /* Menu "PROJEKT Öffnen" Jetzt Projekte*/
      {
        if(win122!=NULL_WIN)
        {
          xvt_vobj_destroy(win122);
          win122=NULL_WIN;
        }
        if (dlg_136 != NULL_WIN)
        {
          xvt_vobj_destroy(dlg_136);
          dlg_136 = NULL_WIN;
        }
        is_projekt_neu = FALSE;
        proj_rem=FALSE;
        if( !xvt_dlg_create_res( WD_MODAL, DLG_148, EM_ALL, DLG_148_eh, 0L ) )
          xvt_dm_post_error("Can't open dialog148");
        if( dialog_ende )
        {
          xvt_menu_set_item_enabled( xdWindow,TASK_MENUBAR_6,TRUE ); // Menu: Konvertieren
          xvt_menu_set_item_enabled( xdWindow,TASK_MENUBAR_14_16,TRUE ); // Menu: Abflussdatei
          xvt_menu_set_item_enabled( xdWindow,TASK_MENUBAR_14_17,TRUE ); // Menu: Verlustdatei
          xvt_menu_update(xdWindow);
          
          if( win122 != NULL_WIN )
          {
            xvt_vobj_destroy( win122 );
            win122 = NULL_WIN;
          }
          int abbruch =FALSE;
          if( dlg_136 != NULL_WIN )
          {
            xvt_vobj_destroy( dlg_136 );
            dlg_136 = NULL_WIN;
          }
          if( !xvt_dlg_create_res( WD_MODAL,DLG_135, EM_ALL, DLG_135_eh, 0L ) )
            xvt_dm_post_error("Can't open dialog 135");
          			  
          if( anzahl_str_dat_entries > 0 )
          {
            if( strlen( STR_SPEC.name ) != 0 )
            {
              xvt_menu_set_item_enabled( Menu_Win,TASK_MENUBAR_14_16, TRUE ); // Menu: Abflußdatei
              xvt_menu_set_item_enabled( Menu_Win,TASK_MENUBAR_14_17, TRUE ); // Menu: Verlustdatei
              xvt_menu_update( Menu_Win );
            }
            
            if( dialog_ende )
            {
              if( win122 != NULL_WIN )
              {
                xvt_vobj_destroy( win122 );
                win122 = NULL_WIN;
              }
              CURSOR cur;
              cur = xvt_win_get_cursor( Menu_Win );		   //main_win
              xvt_win_set_cursor( Menu_Win, CURSOR_WAIT );

              if (!xvt_dlg_create_res( WD_MODELESS, DLG_136, EM_ALL, DLG_136_eh, 0L ) )
                xvt_dm_post_error( "Can't open dialog" );
              xvt_win_set_cursor( Menu_Win, cur );//Dick 17.08.98
            }
          }
        } //if dialogende
      }
      break;
      
    case TASK_MENUBAR_43_46_124: /* Menu "archivieren" */
      {
        if(win122!=NULL_WIN)
        {
          xvt_vobj_destroy(win122);
          win122=NULL_WIN;
        }
        if (dlg_136 != NULL_WIN)
        {
          xvt_vobj_destroy(dlg_136);
          dlg_136 = NULL_WIN;
        }
        proj_rem=FALSE;
        proj_archiv=1;
        if (!xvt_dlg_create_res(WD_MODAL, DLG_148, EM_ALL, DLG_148_eh, 0L))
          xvt_dm_post_error("Can't open dialog");
        proj_archiv=0;
      }
      break;
      
    case TASK_MENUBAR_43_46_125: /* Menu "zurückladen" */
      {
        if(win122!=NULL_WIN)
        {
          xvt_vobj_destroy(win122);
          win122=NULL_WIN;
        }
        if (dlg_136 != NULL_WIN)
        {
          xvt_vobj_destroy(dlg_136);
          dlg_136 = NULL_WIN;
        }
        proj_rem=FALSE;
        proj_archiv=2;
        if (!xvt_dlg_create_res(WD_MODAL, DLG_148, EM_ALL, DLG_148_eh, 0L))
          xvt_dm_post_error("Can't open dialog");
        proj_archiv=0;
      }
      break;
    case TASK_MENUBAR_43_11_21: /* Menu "Projekt aufnehmen" */
      {
        char szDirName[256];//Dick 6.01.99
        
        
        if(DoDirDlg((HWND)xvt_vobj_get_attr(main_win,ATTR_NATIVE_WINDOW), szDirName))
        {
          xvt_slist_add_at_pos(proj_list,0,szDirName,0L);
          if(probezlist==NULL)
            probezlist=xvt_slist_create();
          int count=xvt_slist_count(proj_list);
          for(int i=0;i<count; i++)
          {
            char *help=xvt_slist_get_elt(proj_list, i, 0L);
            strcpy(szDirName,help);
            xvt_slist_add_at_pos(probezlist, i, szDirName,0L);
          }
          // Projektdatei aktualisieren
          save_projekt();
          xvt_slist_destroy(probezlist);
          probezlist=NULL;
        }
      }
      break;
      
    case TASK_MENUBAR_43_11_23: /* Menu "Projekteintrag löschen" */
      {
        if( win122 != NULL_WIN )
        {
          xvt_vobj_destroy( win122 );
          win122 = NULL_WIN;
        };
        
        if( dlg_136 != NULL_WIN )
        {
          xvt_vobj_destroy( dlg_136 );
          dlg_136 = NULL_WIN;
        };
        
        proj_rem = TRUE;
        proj_archiv = 0;
        
        if( !xvt_dlg_create_res( WD_MODAL, DLG_148, EM_ALL, DLG_148_eh, 0L ) )
          xvt_dm_post_error( "Can't open dialog" );
        proj_rem = FALSE;
      }; // case TASK_MENUBAR_43_11_23
      break;
      
    case TASK_MENUBAR_43_48: /* Menu "Programm Beenden" */
      xvt_vobj_destroy(xdWindow);
      break;
      /*****************************************************************************/
      
    case TASK_MENUBAR_50_127: /* Menu "Zustände  */
      {
        if(win122!=NULL_WIN)
        {
          xvt_vobj_destroy(win122);
          win122=NULL_WIN;
        }
        int abbruch =FALSE;
        if (dlg_136 != NULL_WIN)
        {
          xvt_vobj_destroy(dlg_136);
          dlg_136 = NULL_WIN;
        }
        
        if (!xvt_dlg_create_res(WD_MODAL,DLG_135, EM_ALL, DLG_135_eh, 0L))
          xvt_dm_post_error("Can't open dialog 135");
        
        if (anzahl_str_dat_entries >0)
        {
          if(strlen(STR_SPEC.name)!=0)
          {
            xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_14_16,TRUE); // Menu: Abflußdatei
            xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_14_17,TRUE); // Menu: Verlustdatei
            xvt_menu_update(Menu_Win);
          }
          
          if(dialog_ende)
          {
            if (!xvt_dlg_create_res(WD_MODELESS, DLG_136, EM_ALL, DLG_136_eh, 0L))
              xvt_dm_post_error("Can't open dialog");
          }
        }
        
      }
      break;

    case TASK_MENUBAR_13_39: /* Menu "Profildatei öffnen" */
      {
        if(win122!=NULL_WIN)
        {
          xvt_vobj_destroy(win122);
          win122=NULL_WIN;
        }
        
        if (!xvt_dlg_create_res(WD_MODELESS, DLG_136, EM_ALL, DLG_136_eh, 0L))
          xvt_dm_post_error("Can't open dialog");
        
      }
      break;
      
    case TASK_MENUBAR_50_128: /* Menu "Zustand speichern als.." */
      {
        char *profil;
        SLIST_ELT ee;
        zustand_kopieren=TRUE;
        
        //testen ob in str_spec.name gültiger dateiename
        if (!dlg135ref)
        {
          var_dlg135=TRUE;
          if (!xvt_dlg_create_res(WD_MODAL,DLG_135, EM_ALL, DLG_135_eh, 0L))
            xvt_dm_post_error("Can't open dialog 135");
          var_dlg135=FALSE;
        }
        if (!dlg135ref)
        {
          zustand_kopieren=FALSE;
          break;
        }
        if(dlg_136!=NULL_WIN)//Dick 9.04.99
        {
          xvt_vobj_destroy(dlg_136);
          dlg_136=NULL_WIN;
        }
        
        strcpy (alte_zustandsdatei.name, STR_SPEC.name);
        xvt_fsys_set_dir(&STR_SPEC.dir);
        xvt_fsys_get_dir(&file_spec.dir);  //in fkt. profile_aufnehmen als quellpfad
        
        if (!xvt_dlg_create_res(WD_MODAL, DLG_128, EM_ALL, DLG_128_eh, 0L))
          xvt_dm_post_error("Can't open DLG_128");
        
        if(is_128_ok)
        {
          //zustandsdatei speichern gehört zu neuer Zustand /dlg 128
          char files[3];
          files[0]='\0';
          
          //aktuelle str=NEU
          strcpy (neue_zustandsdatei.name, STR_SPEC.name);
          
          anzahl_profil_dat_entries= get_profildatei_names(files);     // in: util.cpp
          xvt_slist_add_at_elt(cfg_list,NULL,str_netz,anzahl_str_dat_entries);
          destroy_strang_tabelle();
          MakeNewStrang(STRANGANZAHL);
          
          save_str_datei();
          read_profil_dat(strang_anfang);
          safe_cfg_dat();
          
          int new_anzahl_profile=anzahl_profil_dat_entries;
          int new_anzahl_straenge=anzahl_strang_entries;
          
          /////profile kopieren
          
          strcpy (STR_SPEC.name,alte_zustandsdatei.name);
          STRANG *alte_strang=NULL;
          
          alte_strang=MakeNewStrang(STRANGANZAHL,alte_strang);
          read_profil_dat(alte_strang);
          
          ermittle_loesch_profile(); //in profpro2.cpp //profile_slist rüchgabe
          if(profil_list!=NULL)
          {
            xvt_slist_destroy(profil_list);
            profil_list=NULL;
          }
          profil_list=xvt_slist_create();
          
          for (ee=xvt_slist_get_first(profile_slist);ee!=NULL;ee=xvt_slist_get_next(profile_slist,ee))
          {
            profil=xvt_slist_get(profile_slist,ee,0L);
            xvt_slist_add_at_elt(profil_list,NULL,profil,0L);
            
          } //umschreiben von profile_slist in profil_list, die in fkt.
          //profil_aufnehmen verwendet wird
          
          
          //alle globalen Variablen auf neue str-datei gesetzt,da in profile-aufnehmen verwendet:
          strcpy (STR_SPEC.name,neue_zustandsdatei.name);
          anzahl_profil_dat_entries=new_anzahl_profile;
          anzahl_strang_entries=new_anzahl_straenge;
          
          xvt_scr_set_busy_cursor();       
          strcpy(str_gewaesser, netz_dat[0]); // werden sonst falsch in die .STR datei geschrieben
          strcpy(str_zustand, netz_dat[2]);
          profile_aufnehmen(alte_strang);
          destroy_strang_tabelle(alte_strang);
          
          if(vergleich) //d.h schlüsseldaten vorhanden
          {
            zustand_kopieren=FALSE;
            loesche_alle_mit_strname();
            
            ee = xvt_slist_get_first(cfg_list);
            for  (int i=1;i<anzahl_str_dat_entries;i++)
              ee = xvt_slist_get_next(cfg_list,ee);
            xvt_slist_rem(cfg_list,ee);
            anzahl_str_dat_entries--;
            safe_cfg_dat();
            vergleich=FALSE;
            
            strcpy (STR_SPEC.name,alte_zustandsdatei.name);
            
            destroy_strang_tabelle();
            MakeNewStrang(STRANGANZAHL);
            make_sort_strangtabelle();    //neue Strangtabelle anlegen-sortieren
            read_profil_dat(strang_anfang);
            save_str_datei();
          }
          else
          {
            ////alle mit str_name kopieren
            
            if(profil_list!=NULL)       //profil_list neu Kreieren, um Dateien mit
            {                            //str_name reinzupacken, die dann kopiert werden müssen
              xvt_slist_destroy(profil_list);
              profil_list=NULL;
            }
            profil_list=xvt_slist_create(); 
            
            strcpy (STR_SPEC.name,alte_zustandsdatei.name);
            
            destroy_strang_tabelle();
            MakeNewStrang(STRANGANZAHL);
            read_profil_dat(strang_anfang);
            
            loesche_alle_mit_strname(); //in util2.cpp; packt dateien mit str-name in Profil_list;
            
            for (ee=xvt_slist_get_first(profil_list);ee!=NULL;ee=xvt_slist_get_next(profil_list,ee))
            {
              char old_file[200], new_file[200], new_file_name[20];
              xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,old_file,180);
              strcat(old_file,"\\");
              
              profil=xvt_slist_get(profil_list,ee,0L);
              strcat(old_file,profil);
              
              xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,new_file,180);
              strcpy(new_file_name,neue_zustandsdatei.name);
              for(int i=9;i<=11;i++)
                new_file_name[i]=profil[i];
              
              new_file_name[2]=profil[2];
              new_file_name[3]=profil[3];
              
              strcat(new_file,"\\");
              strcat(new_file,new_file_name);
              
              if( (((profil[9]=='s')  || (profil[9]=='S'))  && 
                ((profil[10]=='t') || (profil[10]=='T')) &&
                ((profil[11]=='r') || (profil[11]=='R'))) ||
                (((profil[9]=='m') || (profil[9]=='M')) && 
                ((profil[10]=='r') || (profil[10]=='R')) &&
                ((profil[11]=='k') || (profil[11]=='K'))))
              {
              }
              else
              {
                copy_fileb(old_file,new_file);
                
                if( ( (profil[9]=='b') && (profil[10]=='e') && (profil[11]=='r')) ||
                  ( (profil[9]=='B') && (profil[10]=='E') && (profil[11]=='R')))
                {
                  FILE *berfile_old, *berfile_new;
                  
                  if( ( (berfile_old=fopen(old_file,"r+") )!=NULL))
                  {
                    if(	( (berfile_new=fopen(new_file,"w+") )!=NULL))
                    {
                      char txtzeile[100];
                      
                      int zeilen_zaehler=1;
                      while(!feof(berfile_old))
                      {
                        int j, k, l, m, n, len2;
                        txtzeile[0]='\0';
                        fgets(txtzeile,100,berfile_old);
                        int len=strlen(txtzeile);
                        len2=strlen(neue_zustandsdatei.name);
                        if((len>2) && (zeilen_zaehler!=1))
                        {
                          
                          for(k=0; k<=12; k++)
                            if(txtzeile[len-k]=='.')
                              l=k;
                            
                            for (m=0; m<=12; m++)
                              if(neue_zustandsdatei.name[len2-m]=='.')
                                n=m;
                              
                              for(j=0;j<=8;j++)//Dick 18.03.99 7->8 weil auf '.' stand
                              {
                                txtzeile[len-j-l]=neue_zustandsdatei.name[len2-j-n];
                                
                              }
                              
                        }
                        fprintf(berfile_new,"%s",txtzeile);
                        zeilen_zaehler++;
                      } //while
                      fclose(berfile_new); 
                    } //berfile_new
                    fclose(berfile_old); 
                  } //berfile_old
                }  //wenn ber-datei
                
              } //else
            } //for profillist
            
            strcpy (STR_SPEC.name,neue_zustandsdatei.name);
            read_profil_dat(strang_anfang);
            
            //Neu Dick 12.02.99
            strcpy(Gewaessername_aktuell,netz_dat[0]);
            strcpy(Zustand_aktuell,netz_dat[2]);

            setTitle( Projektname_aktuell, Gewaessername_aktuell, Zustand_aktuell );
            
            //xvt_dm_post_note("Zustand wurde erfolgreich kopiert");
            CharBuffer buffer( 1000 );
            xvt_res_get_str( STR_ZUSTAND_COPY, buffer, 1000 );
            xvt_dm_post_note( "%s", buffer ); 
          } //if !=vergleich    
       }  //is 128 ok
       if(profile_slist!=NULL)
       {
         xvt_slist_destroy(profile_slist);
         profile_slist=NULL;
       }
       
       if(profil_list!=NULL)
       {
         xvt_slist_destroy(profil_list);
         profil_list=NULL;
       }
       
       zustand_kopieren=FALSE;
       
    }
    break;
    
  case TASK_MENUBAR_13_42: /* Rauheiten global ändern*/
    {
      //folgende 2 Var. werden genutzt, um zu unterscheiden
      //ob dlg162 zum Rauheiten ändern oder Plotten genutzt wird
      //damit nicht noch weitere externe Variablen angelegt werden.
      //daher auch bei Rauheiten-ändern
      editieren=TRUE;
      berechnen=FALSE;//Dick 28.06.99 sonst wird wie Längsschnitt behandelt
      if (!dlg135ref)
      {
        char buf[200];//Dick 26.11.99
        xvt_res_get_str(STR_ZUSTAND_WAHL,buf,sizeof(buf));
        xvt_dm_post_note("%s",buf);
        //xvt_dm_post_note("Wählen Sie zuerst eine Zustandsdatei aus.");
      }
      
      if(dlg135ref)
      {
        if (!xvt_dlg_create_res(WD_MODAL, DLG_CHANGE_RAUH, EM_ALL, DLG_CHANGE_RAUH_eh, 0L))
          xvt_dm_post_error("Can't open DLG_162");
        if (return_code==E_AUTOLOAD)
        {
          char szApp[] = "EDITOR";
          char szCommand[MAX_PATH];
          sprintf(szCommand,"[project(\"%s\",\"%s\")]",Projektname_aktuell,"prof\\err_rauh.tmp");
          if(!InitDDE(szApp,szCommand, NULL, NULL))
          {
            char buf[200];//Dick 26.11.99
            xvt_res_get_str(STR_EDITOR,buf,sizeof(buf));
            xvt_dm_post_note("%s",buf); // "Editor ist nicht vorhanden!"
          }
        }
      }
      berechnen=FALSE;
      editieren=FALSE;
    }
    break;

  case TASK_MENUBAR_13_59: /* Interpolieren*/
    {
      char szApp[] = "interp";
      char szCommand[MAX_PATH];
      char data[MAX_PATH];
      char str4[100];
      strcpy(str4,STR_SPEC.name);
      sprintf(szCommand,"[project(\"%s\",\"%s\",\"\")]",Projektname_aktuell,str4);
      data[0]='\n';
      
      if(!InitDDE(szApp,szCommand, "files", data, TRUE))
      { 
        //xvt_dm_post_note("Interpolationsprogramm ist nicht vorhanden!");
        char buf[200];//Dick 26.11.99
        xvt_res_get_str(STR_INTERPOLATION,buf,sizeof(buf));
        xvt_dm_post_note("%s",buf);
      }
      else
      {
        read_profil_dat(strang_anfang);
        if(dlg_136!=NULL_WIN)
        {
          EnableWindow((HWND)xvt_vobj_get_attr(dlg_136,ATTR_NATIVE_WINDOW),FALSE);
        }
      }
    }
    break;
    
  case TASK_MENUBAR_14_27: // Menu  "Sonderprogrammstart"
    StartAutoSketch(main_win,MODE_SONDERPROG_START);  // Optionendialog anzeigen)
    break;
    
  case TASK_MENUBAR_8_36: /* Menu "Info" */
    if ( !xvt_dlg_create_res( WD_MODAL, IDD_DLG_ABOUT, EM_ALL, DLG_ABOUT_eh, 0L ) )
      xvt_dm_post_error("Can't open dialog INFO-BOX");
    break;
    
  case TASK_MENUBAR_14_16: /* Menu "Abflussdatei " */
    {
      if(win122!=NULL_WIN)
      {
        xvt_vobj_destroy(win122);
        win122=NULL_WIN;
      }
      menue_alles_sperren(); // in read.cpp
      if (!dlg135ref)
      {
        var_dlg135=TRUE;
        if (!xvt_dlg_create_res(WD_MODAL,DLG_135, EM_ALL, DLG_135_eh, 0L))
          xvt_dm_post_error("Can't open dialog 135");
        var_dlg135=FALSE;
      }
      
      if (dlg135ref)
      {
        if (!xvt_win_create_res(QWERT_EDIT, TASK_WIN, EM_ALL, QWERT_EDIT_eh, 0L))
          xvt_dm_post_error("Can't open win122");
      }
      menue_alles_oeffnen(); // in read.cpp
    }
		  break;

  case TASK_MENUBAR_14_17: /* Menu "Verlustdatei" */
    {
      if(win122!=NULL_WIN)
      {
        xvt_vobj_destroy(win122);
        win122=NULL_WIN;
      }
      menue_alles_sperren(); // in read.cpp
      if (!dlg135ref)
      {
        var_dlg135=TRUE;
        if (!xvt_dlg_create_res(WD_MODAL,DLG_135, EM_ALL, DLG_135_eh, 0L))
          xvt_dm_post_error("Can't open dialog 135");
        var_dlg135=FALSE;
      }
      
      if (dlg135ref)
      {
        DoLossDlg(Projektname_aktuell, STR_SPEC.name,(HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW));
      }
      menue_alles_oeffnen(); // in read.cpp
    }
    break;
    
  case TASK_MENUBAR_30_7: /* Menu " Start Berechnung" */
    {
    	 FILE *qwert_file;
       char file_str[100];
       wsplist=0;//Dick 22.07.99 damit Unterschied von Auswertung verschaffen wird
       if(win122!=NULL_WIN)
       {
         xvt_vobj_destroy(win122);
         win122=NULL_WIN;
       }
       menue_alles_sperren(); // in read.cpp
       editieren=TRUE;
       berechnen=FALSE;
       abbruch205=FALSE;
       abbruch206=FALSE;
       if (!dlg135ref)
       {
         var_dlg135=TRUE;
         if (!xvt_dlg_create_res(WD_MODAL,DLG_135, EM_ALL, DLG_135_eh, 0L))
           xvt_dm_post_error("Can't open dialog 135");
         var_dlg135=FALSE;
       }
       
       if(dlg135ref)
       {
         char str2[15];
         zaehler_aus_ber_lesen();
         /*************/
         ok = read_start_berechnung(); //in read
         if (ok ==2) //neu
         {
           str2[0]='\0';
           strcpy(str2,STR_SPEC.name);
           len=strlen(str2);
           str2[len-3]='\0';
           len=0;
           strcat(str2,"qwt");
           strcpy(qwert_spec.name, str2);
           xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, str2, 50);
           xvt_fsys_convert_str_to_dir(str2,&qwert_spec.dir);
           str2[0]='\0';
           xvt_fsys_convert_dir_to_str(&qwert_spec.dir,file_str,79);
           strcat(file_str,"\\");
           strcat(file_str,qwert_spec.name);
           
           if ((qwert_file =fopen(file_str,"r+"))==NULL)
           {
             qwert_fehlt=TRUE;
             //xvt_dm_post_note("Sie haben keine Abflussdatei erzeugt!");
             char buf[200];
             xvt_res_get_str(STR_QWERT_CREATE,buf,sizeof(buf));
             xvt_dm_post_note("%s",buf);
           }
           else
           {
             fclose(qwert_file);
             
             if (qwert_fehlt==FALSE)
             {
               if( LWA_PROJEKT )
				 abbruch205=!DoLWACalcDlg(Projektname_aktuell, STR_SPEC.name, str2, (HWND)xvt_vobj_get_attr(main_win,ATTR_NATIVE_WINDOW), !GetFeature( "wsp_nodemo" ));
               else
				 abbruch205=!DoBCECalcDlg(Projektname_aktuell, STR_SPEC.name, str2, (HWND)xvt_vobj_get_attr(main_win,ATTR_NATIVE_WINDOW), !GetFeature( "wsp_nodemo" ));

               if(!abbruch205)
                 ok = read_start_berechnung(); //in read
             }
           }
         } //ok=2
         
         if ((qwert_fehlt==FALSE)&&(ok!=0)&& (abbruch205==FALSE) && (abbruch206==FALSE))
         {
           if (ptr_qwert_anfang!=NULL)
             Delete_Qwert_Datei();
           
           lese_qwert_datei();                   //DLG_203
           if (change_strang)//Dick 20.07.99 
           {
             //xvt_dm_post_note("Strangtabelle geändert, überprüfen Sie die Abflußdatei\n");
             char buf[200];//Dick 26.11.99
             xvt_res_get_str(STR_STRANG_CHANGE,buf,sizeof(buf));
             xvt_dm_post_note("%s",buf);
             change_strang=FALSE;                                
           }
           if (!change_strang && !qwert_fehlt)//Dick 10.02.99
           {
             /*DLG203=*/
             xvt_dlg_create_res(WD_MODAL, DLG_400, EM_ALL, DLG_203_eh, 1L);//Dick 1.02.99 1L
           }
         }
         
         if ((qwert_fehlt==TRUE)&&(ok!=0))
         {
           if (!xvt_win_create_res(QWERT_EDIT, TASK_WIN, EM_ALL, QWERT_EDIT_eh, 0L))
             xvt_dm_post_error("Can't open win122");
         }
         
       } //if dlg135ref
       menue_alles_oeffnen(); //in read.cpp
    }
    break;
    
     case TASK_MENUBAR_30_24:  //Längsschnitt einsehen
       {
         wsplist=0;
         if(win122!=NULL_WIN)
         {
           xvt_vobj_destroy(win122);
           win122=NULL_WIN;
         }
         if(dlg_136!=NULL_WIN)
         {
           xvt_vobj_destroy(dlg_136);
           dlg_136=NULL_WIN;
         }
         if(WIN_116!=NULL_WIN)
         {
           xvt_vobj_destroy(WIN_116);
           WIN_116=NULL_WIN;
         }
         menue_alles_sperren(); //in read.cpp
         berechnen=TRUE; //wenn beides true bedeutet dies Längsschnitt
         editieren=TRUE;
         
         if (!dlg135ref)
         {
           var_dlg135=TRUE;
           if (!xvt_dlg_create_res(WD_MODAL,DLG_135, EM_ALL, DLG_135_eh, 0L))
             xvt_dm_post_error("Can't open dialog 135");
           var_dlg135=FALSE;
         }
         if(dlg135ref)
         {
           char str3[100];
           xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str3,50);
           len=strlen(str3);
           str3[len-4]='\0';
           strcat(str3,"dath");
           xvt_fsys_convert_str_to_dir(str3,&file_spec.dir);
           zaehler_aus_ber_lesen();
           ok = read_start_berechnung(); //in read
           if(ok!=0)
           {
             abbruch203=FALSE;
             xvt_dlg_create_res(WD_MODAL, DLG_203, EM_ALL, DLG_203_eh, 2L);//Dick 1.02.99 2L
           }
           else
           {
             abbruch203=TRUE;
           }
           if((abbruch203==FALSE) && (ok!=0))
           {
             if( LWA_PROJEKT )
             {
               ber_spec.name[2]='p'; //in ber-spec.name steht Name der Ber. variante
               ber_spec.name[3]='l'; //=Längsschnittdatei
             }
             else
             {
               ber_spec.name[2]='w'; //in ber-spec.name steht Name der Ber. variante
               ber_spec.name[3]='l'; //=Längsschnittdatei
             };

             strcpy(file_spec.name,ber_spec.name);
             /**************			 xvt_dm_post_note("Graphik-Editor");*****************/
             
             /*****sicherheitshalber nochmal, weile Datei nicht geöffnet werden konnte in read_profildatei****/
             xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str3,50);
             len=strlen(str3);
             str3[len-4]='\0';
             strcat(str3,"dath");
             xvt_fsys_convert_str_to_dir(str3,&file_spec.dir);
             
             /*******************************/
//             xvt_fsys_set_dir(&file_spec.dir);
             int lesen_ok;
//             laengsschnitt=TRUE; //für lesen
             lesen_ok = read_profildatei(pWPL, &file_spec.dir, file_spec.name );
//             laengsschnitt=FALSE;
             if (lesen_ok ==0)
             {
               is_profil_open = TRUE;
               new_profil =FALSE;
               main_menu_tree=xvt_menu_get_tree(xdWindow);

               if( LWA_PROJEKT )
                 list->Mehrfeldoptimierung();

               if (!xvt_win_create_res(WIN_GRAPH_116, TASK_WIN, EM_ALL, WIN_GRAPH_116_eh, 0L))
                 xvt_dm_post_error("Can't open window116");
             }
             else
             {
               char buf[200];
               xvt_res_get_str(STR_READ_LAENGS,buf,sizeof(buf)); // "Längsschnittdatei kann nicht gelesen werden!");
               xvt_dm_post_note("%s",buf);
               editieren=FALSE;
               berechnen=FALSE;
             }
             
             /********************Ende Graphik-Editor************************************/
           } //abbruch203=FALSE;
           else //Dick 11.02.2000
           {
             editieren=FALSE;
             berechnen=FALSE;
           }
         }   //dlg135ref
         menue_alles_oeffnen(); // in read.cpp
   }
	  break;
    
    /********   Massenberechnung   ****************************************/
  case TASK_MENUBAR_13_60_62:  // Profildatei-Geländeverknüpfung-neu
    {
      if(WIN_116 !=NULL_WIN || WIN120 !=NULL_WIN)
      {
        xdEvent->v.cmd.tag =TASK_MENUBAR_1_68_70; // 
        xvt_win_dispatch_event(xdWindow,xdEvent);
        break;
      }
      xvt_scr_set_busy_cursor();
      Make_GelaendeVerknuepfung(1,0.0);  //action :=1 komplette str-Datei
      // in Dlg135 wird Menü disabled- daher hier wieder auf TRUE
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_13_60,TRUE);   //Profildatei-Geländeverknüpfung
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_13_64,TRUE);   //Profildatei-Flächenberechnung
      xvt_menu_update(Menu_Win);
      
    }
    break;

  case TASK_MENUBAR_13_60_63:  // Profildatei-Geländeverknüpfung-löschen
    {
    /*  Da Menüaufruf in DLG136 erfolgt ist in jedem Fall eine
    Vernetzungsdatei gewählt und SLIST prof_datei gültig
      */
      if(WIN_116 !=NULL_WIN || WIN120 !=NULL_WIN)
      {
        xdEvent->v.cmd.tag =TASK_MENUBAR_1_68_71; // 
        xvt_win_dispatch_event(xdWindow,xdEvent);
        break;
      }
      STR_DATA *str_data;
      CURSOR cursor;
      
      str_data=new  STR_DATA;
      InitStrData(str_data);
      
      if(Save_Global_StrData(str_data))  //globale Daten sichern
      {
        cursor = xvt_win_get_cursor(xdWindow);
        xvt_win_set_cursor(xdWindow,CURSOR_WAIT);
        
        DeleteGel2InStrData(str_data);
        Get_Global_StrData(str_data);
        xvt_win_set_cursor(xdWindow,cursor);
      }
      DeleteStrData(str_data);
      str_data=NULL;
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_13_60,TRUE);   //Profildatei-Geländeverknüpfung
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_13_64,TRUE);   //Profildatei-Flächenberechnung
      xvt_menu_update(Menu_Win);
    }
    break;

  case TASK_MENUBAR_13_64_65:  // Profildatei-Flächenberechnung-neu
    {
      
      if(WIN_116 !=NULL_WIN || WIN120 !=NULL_WIN)
      {
        xdEvent->v.cmd.tag =TASK_MENUBAR_1_69_72; // 
        xvt_win_dispatch_event(xdWindow,xdEvent);
        break;
      }
      BOOLEAN error=FALSE;
      STR_DATA *str_data;
      
      str_data=new  STR_DATA;
      InitStrData(str_data);
      
      char buf[200];//Dick 26.11.99
      
      if(Save_Global_StrData(str_data))  //globale Daten sichern
      {
        /* strang_anfang wird durch Save_Global_StrData() auf NULL gesetzt !!*/
        switch (Test_Flaechen_Data(str_data))
        {
        case 0:   // Ok
          Get_Global_StrData(str_data);
          if (Make_GelaendeVerknuepfung(1,0.0))
          {
            switch(Make_FlaechenBerechnung(str_data))
            {                              
            case 0:
              xvt_res_get_str(STR_FLAECHE_NOTE_10,buf,sizeof(buf));
              xvt_dm_post_error(buf);
              break;
            case 1:
              xvt_res_get_str(STR_FLAECHE_NOTE_11,buf,sizeof(buf));
              xvt_dm_post_note(buf);
              break;
            case 2:
              xvt_res_get_str(STR_FLAECHE_NOTE_12,buf,sizeof(buf));
              xvt_dm_post_error(buf);
              break;
            }
          }
          break;
        case 1:   // Abbruch bei löschen Fläche
        case 2:   //Abbruch bei löschen 2.Gelände
        case 3:   //keine Profile
          break;
        case 4:   //Gelände2 vorhanden- jedoch beibehalten
          Get_Global_StrData(str_data);
          
          switch(Make_FlaechenBerechnung(str_data))
          {
          case 0:
            xvt_res_get_str(STR_FLAECHE_NOTE_10,buf,sizeof(buf));
            xvt_dm_post_error(buf);
            break;
          case 1:
            xvt_res_get_str(STR_FLAECHE_NOTE_11,buf,sizeof(buf));
            xvt_dm_post_note(buf);
            break;
          case 2:
            xvt_res_get_str(STR_FLAECHE_NOTE_12,buf,sizeof(buf));
            xvt_dm_post_error(buf);
            break;
          }
          
          break;
        };
      }
      if (pWPL!=NULL)
        list = pWPL->PList;
      DeleteStrData(str_data);
      str_data=NULL;
      
      // in Dlg135 wird Menü disabled- daher hier wieder auf TRUE
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_13_60,TRUE);   //Bearbeiten-Geländeverknüpfung
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_13_64,TRUE);   //Bearbeiten-Flächenberechnung
      xvt_menu_update(Menu_Win);
    }
    break;
  case TASK_MENUBAR_13_64_66:  // Profildatei-Flächenberechnung-löschen
    {
    /*  Da Menüaufruf in DLG136 erfolgt ist in jedem Fall eine
    Vernetzungsdatei gewählt und SLIST prof_datei gültig
      */
      
      if(WIN_116 !=NULL_WIN || WIN120 !=NULL_WIN)
      {
        xdEvent->v.cmd.tag =TASK_MENUBAR_1_69_73; // 
        xvt_win_dispatch_event(xdWindow,xdEvent);
        break;
      }
      STR_DATA *str_data;
      CURSOR cursor;
      
      str_data=new  STR_DATA;
      InitStrData(str_data);
      
      if(Save_Global_StrData(str_data))  //globale Daten sichern
      {
        cursor = xvt_win_get_cursor(xdWindow);
        xvt_win_set_cursor(xdWindow,CURSOR_WAIT);
        
        DeleteFlaecheInStrData(str_data);
        
        Get_Global_StrData(str_data);
        xvt_win_set_cursor(xdWindow,cursor);
        //xvt_dm_post_note("Datensatz:Fläche wurde gelöscht!");
        char buf[200];
        xvt_res_get_str(STR_FLAECHE_NOTE_13,buf,sizeof(buf));
      }
      DeleteStrData(str_data);
      str_data=NULL;
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_13_60,TRUE);   //Profildatei-Geländeverknüpfung
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_13_64,TRUE);   //Profildatei-Flächenberechnung
      xvt_menu_update(Menu_Win);
    }
    break;
    
  case TASK_MENUBAR_1_68_70:  // Bearbeiten-Geländeverknüpfung-Datens. einfügen
    {  /*Aufruf aus Alpha-/Grafik- Editor*/
      CURSOR cursor;
      char tmp[10];
      double station;
      
      cursor = xvt_win_get_cursor(xdWindow);
      xvt_win_set_cursor(xdWindow,CURSOR_WAIT);
      
      test_line9(tmp);  //Station des aktuellen Profils
      station = atof(tmp);
      
      Make_GelaendeVerknuepfung(2,station);  // action :=	2 nur übergebene Station
      
      /******menü wurde nicht wieder aktiviert*******/
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_13_60,TRUE); //Geländeverkn. zustandsbez
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_13_64,TRUE); //Flächenberchnung zustandsbez
      xvt_menu_update(Menu_Win);
      
      /**********************************************/
      xvt_win_set_cursor(xdWindow,cursor);
    }
    break;

  case TASK_MENUBAR_1_68_71: 
    // Bearbeiten-Geländeverknüpfung-Datens. löschen
    {  /*
       Aufruf aus Alpha-/Grafik- Editor
       d.h. list-> ist gültig und zeigt auf aktuelles Profil
      */
      int ds;
      ds = list->ExistGelaende2Daten(); //gibt es Datensatz GELAENDE2
      if (ds)
      {
        list->DeleteNode(ds, (int*)ds_info, (int*)typ);  //Knoten löschen
        anzahl_ds--; //wird nicht mehr in DeleteNode gesetzt
        SaveProfilFile = TRUE;
        /*Listbutton in Alpha/Grafik aktualisieren*/
        if(pWPL->window !=NULL)
        {
          xvt_list_rem(pWPL->window,ds-1);
          xvt_list_set_sel(pWPL->window,0,TRUE);
        }
        if (dlg_sonderprofil !=NULL_WIN)
        {
          xvt_vobj_destroy(dlg_sonderprofil);
          dlg_sonderprofil = NULL_WIN;
        }
      }
      /******menü wurde nicht wieder aktiviert*******/
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_13_60,TRUE); //Geländeverkn. zustandsbez
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_13_64,TRUE); //Flächenberchnung zustandsbez
      xvt_menu_update(Menu_Win);
      xdEvent->type =E_CONTROL;
      xdEvent->v.ctl.id=WIN_GRAPH_116_LISTBUTTON_33;
      if(WIN_116 !=NULL_WIN)
        xvt_win_dispatch_event(WIN_116,xdEvent);
      else 
        if(WIN120 !=NULL_WIN)
          xvt_win_dispatch_event(WIN120,xdEvent);
        
    }
    break;

  case TASK_MENUBAR_1_69_72:  // Bearbeiten-Flächenberechnung-neu
    {  /*Aufruf aus Alpha-/Grafik- Editor*/
      CURSOR cursor;
      char tmp[10];
      double station;
      int result = 1;
      cursor = xvt_win_get_cursor(xdWindow);
      xvt_win_set_cursor(xdWindow,CURSOR_WAIT);
      
      test_line9(tmp);  //Station des aktuellen Profils
      station = atof(tmp);
      
      if (!pWPL->PList->ExistGelaende2Daten())
        result = Make_GelaendeVerknuepfung(2,station);  // action :=	2 nur übergebene Station
      else
      {/*aktuelle Daten sichern*/
        SaveGlobalData(pWPL->data);
      }
      if(result)
      {
        if( ProfilFlaechenBerechnung(pWPL->PList,station)==2)
        {
          //xvt_dm_post_note("Flächenberechnung fehlerhaft beendet");// Fehler abfangen falls keine Berechnung in Modul Leibo
          char buf[200];//Dick 26.11.99
          xvt_res_get_str(STR_FLAECHE_ERROR,buf,sizeof(buf));
          xvt_dm_post_note("%s",buf);
        }
        else  
        {
          //xvt_dm_post_note("Flächenberechnung erfolgreich beendet");
          char buf[200];//Dick 26.11.99
          xvt_res_get_str(STR_FLAECHE_SUCCESS,buf,sizeof(buf));
          xvt_dm_post_note("%s",buf);
        }
      }
      
      xvt_win_set_cursor(xdWindow,cursor);
      /******menü wurde nicht wieder aktiviert*******/
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_13_60,TRUE); //Geländeverkn. zustandsbez
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_13_64,TRUE); //Flächenberchnung zustandsbez
      xvt_menu_update(Menu_Win);
    }
    break;

  case TASK_MENUBAR_1_69_73: 
    // Bearbeiten-Flächenberechnung-löschen
    {  /* Aufruf aus Alpha-/Grafik- Editor
       d.h. list-> ist gültig und zeigt auf aktuelles Profil
      */
      int ds;
      ds = list->ExistFlaecheDaten(); //gibt es Datensatz GELAENDE2
      if (ds)
      {
        list->DeleteNode(ds, (int*)ds_info, (int*)typ);  //Knoten löschen
        anzahl_ds--;
        SaveProfilFile = TRUE;
        
        /*Listbutton in Alpha/Grafik aktualisieren*/
        if(pWPL->window !=NULL)
        {
          xvt_list_rem(pWPL->window,ds-1);
          xvt_list_set_sel(pWPL->window,0,TRUE);
        }
        if (dlg_sonderprofil !=NULL_WIN)
        {
          xvt_vobj_destroy(dlg_sonderprofil);
          dlg_sonderprofil = NULL_WIN;
        }
      }
      /******menü wurde nicht wieder aktiviert*******/
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_13_60,TRUE); //Geländeverkn. zustandsbez
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_13_64,TRUE); //Flächenberchnung zustandsbez
      xvt_menu_update(Menu_Win);
      xdEvent->type =E_CONTROL;
      xdEvent->v.ctl.id=WIN_GRAPH_116_LISTBUTTON_33;
      if(WIN_116 !=NULL_WIN)
        xvt_win_dispatch_event(WIN_116,xdEvent);
      else 
        if(WIN120 !=NULL_WIN)
          xvt_win_dispatch_event(WIN120,xdEvent);
    }
    break;

  case TASK_MENUBAR_2_3:  // Berechnen- Volumenberechnung
    {
      if( GetFeature( "wsp_nodemo" ) )
        MakeVolumenBerechnung(pWPL);
      else
      {
        char buf[200];//Dick 26.11.99
        xvt_res_get_str(STR_DEMO_NOTE_6,buf,sizeof(buf));
        xvt_dm_post_note("%s",buf);
        //xvt_dm_post_note("Massenberechnung bei Demo nicht möglich");
      };
      
    }
    break;

  case TASK_MENUBAR_1_86: //Ergebnisse der Massenberechnung
    {
      if(win122!=NULL_WIN)
      {
        xvt_vobj_destroy(win122);
        win122=NULL_WIN;
      }
      if (dlg_136!=NULL_WIN)
      {   // Dialog 136 schließen
        xdEvent->type =E_CONTROL;
        xdEvent->v.ctl.id=DLG_136_PUSHBUTTON_2;
        xvt_win_dispatch_event(dlg_136,xdEvent);
      }
      
      //Neue Editor Dick 10.02.99
      char szApp[] = "EDITOR";
      char szCommand[MAX_PATH];
      sprintf(szCommand,"[project(\"%s\",\"%s\")]",Projektname_aktuell,"dath\\masse.txt");
      if(!InitDDE(szApp,szCommand, NULL, NULL))
      {
        //xvt_dm_post_note("Editor ist nicht vorhanden!");
        char buf[200];//Dick 26.11.99
        xvt_res_get_str(STR_EDITOR,buf,sizeof(buf));
        xvt_dm_post_note("%s",buf);
      }
      //ende neue Editor*/
      xvt_fsys_set_dir(&STR_SPEC.dir);
    }
    break;
    /********  Ende Massenberechnung   **************************************/
    
  case TASK_MENUBAR_13_58_74: /* DXF:Querprof.  */
    {
      int back;
      if (!dlg135ref)
      {
        var_dlg135=TRUE;
        if (!xvt_dlg_create_res(WD_MODAL,DLG_135, EM_ALL, DLG_135_eh, 0L))
          xvt_dm_post_error("Can't open dialog 135");
        var_dlg135=FALSE;
      }
      if(dlg135ref)
      {
        char str3[100];
        xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str3,50);
        
        /*
        folgende 2 Var. werden genutzt, um zu unterscheiden
        ob dlg162 zum Rauheiten ändern oder Plotten genutzt wird
        damit nicht noch weitere externe Variablen angelegt werden.
        daher auch bei Rauheiten-ändern
        */
        ok=1;
        len=strlen(str3);
        str3[len-4]='\0';
        strcat(str3,"prof");
        xvt_fsys_convert_str_to_dir(str3,&file_spec.dir);
        berechnen=FALSE;
        editieren=FALSE;
        if(ok!=0)
        {
          if (!xvt_dlg_create_res(WD_MODAL, DLG_CHANGE_RAUH, EM_ALL, DLG_CHANGE_RAUH_eh, 0L))
            xvt_dm_post_error("Can't open DLG_162");
        }
        if(return_code==1)
          back=1;
        else
          back=0;
      } // if dlg135ref
    }
    break;

  case TASK_MENUBAR_1_91: /* DXF:Längs.  */
    {                                 
      int back;
      if (!dlg135ref)
      {
        var_dlg135=TRUE;
        if (!xvt_dlg_create_res(WD_MODAL,DLG_135, EM_ALL, DLG_135_eh, 0L))
          xvt_dm_post_error("Can't open dialog 135");
        var_dlg135=FALSE;
      }
      if(dlg135ref)
      {
        char str3[100];
        xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str3,50);
        
        berechnen=TRUE;
        editieren=FALSE;
        zaehler_aus_ber_lesen();
        ok = read_start_berechnung(); //in read
        
        len=strlen(str3);
        str3[len-4]='\0';
        strcat(str3,"dath");
        xvt_fsys_convert_str_to_dir(str3,&file_spec.dir);
        berechnen=FALSE;//d.h. Plooten 
        editieren=FALSE;           
        
        if(ok!=0)
        {
          plotoption=TRUE; 
          abbruch203=FALSE;
          xvt_dlg_create_res(WD_MODAL, DLG_203, EM_ALL, DLG_203_eh, 3L);//Dick 1.02.99 3L
          plotoption=FALSE;
        }
        else
          abbruch203=TRUE;//dick 19.07.99
        if(abbruch203==FALSE)
          back=1;
        else
          back=0;
        //break;
      }// if dlg135ref
      
      //   # endif
      
      if(back==1)
      {
        back=wahl();
        if (!xvt_dlg_create_res(WD_MODAL,DLG_218, EM_ALL, DLG_218_eh, 0L))
          xvt_dm_post_error("Can't open dialog 218");

        if( GetFeature( "wsp_nodemo" ) )
        {
          if(!abbruch_217ff)
          {
            schreibe_input_plot_datei(0); //in rauh.cpp //Inputdatei für Plotprogramm
            starte_plot_programm(1); //in rauh.cpp
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
      }
    }
    break;
    
  case TASK_MENUBAR_30_75: /* Konfiguration LAYER  */
    {
      if (!xvt_win_create_res(WIN_LAYER, TASK_WIN, EM_ALL, WIN_LAYER_eh, 0L))
        xvt_dm_post_error("Can't open _win_layer");
    }
    break;
    
  case TASK_MENUBAR_13_58_78:   /* Antragstellerschild ändern  */
    EditAntragsteller(TASK_WIN);
    break;
    
  case TASK_MENUBAR_43_6_26:  //WASPILA
    {
      char ausgewahl_dateien[AUFNEHM_BUFFER_SIZE];
      char name[15];
      int j_zaehler=0;
      
      char buf[200],buf2[200],buf3[200];//Dick 26.11.99
      
      menue_alles_sperren(); //in read.cpp
      berechnen=FALSE;
      editieren=FALSE;
      
      char test[100];
      test[0]='\0';
      
      loesche_alle_mit_log(); //in util2.cpp löscht alle Errorfiles
      
      xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,test,90);
      strcat(test,"\0");
      xvt_res_get_str(STR_ABBRECHEN,buf,sizeof(buf));
      xvt_res_get_str(STR_LWAKONV_NOTE_1,buf2,sizeof(buf2));
      xvt_res_get_str(STR_LWAKONV_NOTE_2,buf3,sizeof(buf3));
      switch(xvt_dm_post_ask("OK",buf,NULL,"%s%s%s",buf2,test,buf3))
      {
      case RESP_DEFAULT: //OK
        konvertierung=TRUE;
        break;
      case RESP_2:
        konvertierung=FALSE;
        break;
      }
      if (konvertierung)
      {
        konvertierung=FALSE; //sonst wird nach wsp-Endung gefiltert
        int back=auswahl2(ausgewahl_dateien,1,STR_START_DATEIEN);
        if(back==1)
        {
          back=prof_in_slist2(ausgewahl_dateien);
          int back2=1; 

          if( GetFeature( "wsp_nodemo" ) )
          {
            if ((back==1) && (back2==1))
            {
              if (!xvt_dlg_create_res(WD_MODAL,DLG_128, EM_ALL, DLG_128_eh, 0L))
                xvt_dm_post_error("Can't open dlg128");
              if (cfg_list==NULL)
                is_128_ok=FALSE;
              if(is_128_ok)
              {
                xvt_slist_add_at_elt(cfg_list,NULL,str_netz,0);
                int j2=0;
                for (int i=0; i<10; i++)
                {
                  if (str_netz[i]!=' ')
                  {
                    name[j2]=str_netz[i];
                    j2++;
                  }
                }
                name[j2]='\0';
                safe_cfg_dat();
              } //if is128 ok
              
              MakeNewStrang(STRANGANZAHL);
              for (int i=0;i<5;i++)          //Dateinamen für Strangtabelle
              {
                name_anfang[i][0]='\0';
                name_ende[i][0]='\0';
              }
              
              if (prof_datei)
                xvt_slist_destroy(prof_datei);
              
              if  ((prof_datei = xvt_slist_create())==NULL)
                xvt_dm_post_error(" Can't create _SLIST:prof_datei");
              
              int anzahl_files=lese_start_datei(ausgewahl_dateien);
              
              lese_geometrie(anzahl_files); //waspila.cpp
              
              
              xvt_fsys_set_dir(&STR_SPEC.dir);             
              
              is_Entry_in_SLIST=FALSE;
              
              for (j_zaehler=0;j_zaehler<=99;j_zaehler++)
                str_netz[j_zaehler]='\0';
              file_spec.name[0]='\0';
              
              if (prof_datei != NULL)
              {
                xvt_slist_destroy(prof_datei);
                prof_datei = NULL;
              }
              
              if(is_128_ok)
              {
                konvertierung=FALSE;
              }
              else
                is_128_ok=FALSE;
              
              char help_n[100];
              xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,help_n,50);
              xvt_fsys_convert_str_to_dir(help_n,&file_spec.dir);
              xvt_fsys_set_dir(&STR_SPEC.dir);
              
            } //if back==1
          }
          else
          {
            xvt_res_get_str(STR_DEMO_NOTE_5,buf,sizeof(buf));
            xvt_dm_post_note("%s",buf);
          };
        } //if back==1
      } //if konvertierung
      menue_alles_oeffnen(); //in read.cpp
      konvertierung=FALSE;
    }
    break;
    
    case TASK_MENUBAR_43_6_77: // Konvertierung WSPWIN->JABRON
      {
        if( GetFeature( "wsp_nodemo" ) )
        {
          BOOLEAN var_dlg135 = TRUE;   // 2 Buttons disabled setzen
          STR_DATA* str_data = new STR_DATA;
          
          InitStrData( str_data );
          
          if( !xvt_dlg_create_res(WD_MODAL,DLG_135, EM_ALL, DLG_135_eh, 0L) )
          {
            xvt_dm_post_error( "Can't open dialog 135" );
            break;
          }
          
          var_dlg135 = FALSE;
          
          if( strang_anfang != NULL )
            destroy_strang_tabelle();
          strang_anfang = NULL;
          MakeNewStrang( STRANGANZAHL );
          read_profil_dat( strang_anfang );  // Strangtabelle 1.Vernetzungsdatei lesen
          Save_Global_StrData( str_data );
          /* ab hier sollte erste Vernetzungsdatei vorhanden sein*/
          
          switch( JabConvertProfilToJabron( pWPL, str_data ) )
          {
          case 0: //Fehler
            {
              char buf[200];
              xvt_res_get_str(STR_JABRON_NOTE_1,buf,sizeof(buf));
              xvt_dm_post_note( "%s", buf );     //xvt_dm_post_note("Konvertierung wurde fehlerhaft beendet!");
            }
            break;
            
          case 1:  //OK
            {
              char buf[200];
              xvt_res_get_str(STR_JABRON_NOTE_2,buf,sizeof(buf));
              xvt_dm_post_note("%s",buf); //xvt_dm_post_note ("Konvertierung beendet!\nDaten in Datei:'jabron.pro' gespeichert.");
            }
            break;
          }; // switch
        } // if !Demo
        else
        {
          char buf[200];//Dick 26.11.99
          xvt_res_get_str(STR_DEMO_NOTE_5,buf,sizeof(buf));
          xvt_dm_post_note("%s",buf);
          //xvt_dm_post_note("Konvertierung bei Demo nicht möglich");
        };
      }
      break;
      
    case TASK_MENUBAR_43_6_95: // Konvertierung JABRON->WSPWIN
      {
        if( GetFeature( "wsp_nodemo" ) )
          JabConvertJabronToProfil(pWPL);
        else
        {
          char buf[200];//Dick 26.11.99
          xvt_res_get_str(STR_DEMO_NOTE_5,buf,sizeof(buf));
          xvt_dm_post_note("%s",buf);
          //xvt_dm_post_note("Konvertierung bei Demo nicht möglich");
        };
      }
      break;

    case TASK_MENUBAR_6_15: // Konvertierung LWA-Format
      {
        char ausgewahl_dateien[AUFNEHM_BUFFER_SIZE];
        char buf[200],buf2[200],buf3[200];//Dick 26.11.99
        if(win122!=NULL_WIN)
        {
          xvt_vobj_destroy(win122);
          win122=NULL_WIN;
        }
        menue_alles_sperren(); //in read.cpp
        berechnen=FALSE;
        editieren=FALSE;
        
        char test[100];
        test[0]='\0';
        
        loesche_alle_mit_log(); //in util2.cpp löscht alle Errorfiles
        
        xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,test,90);
        strcat(test,"\0");
        xvt_res_get_str(STR_ABBRECHEN,buf,sizeof(buf));
        xvt_res_get_str(STR_LWAKONV_NOTE_1,buf2,sizeof(buf2));
        xvt_res_get_str(STR_LWAKONV_NOTE_2,buf3,sizeof(buf3));
        switch(xvt_dm_post_ask("OK",buf,NULL,"%s%s%s",buf2,test,buf3))
        {
        case RESP_DEFAULT: //OK
          konvertierung=TRUE;
          break;
        case RESP_2:
          konvertierung=FALSE;
          break;
        }
        if (konvertierung)
        {
          int back=auswahl2(ausgewahl_dateien,1,40003);
          if(back==1)
          {
            back=prof_in_slist2(ausgewahl_dateien);
            int back2=name_doppelt_profile_schon_da(); //in read.cpp
            if( GetFeature( "wsp_nodemo" ) )
            {
              if ((back==1) && (back2==1))
              {
                start_lwa2b();
                if(is_128_ok)
                {
                  konvertierung=FALSE;
                  timer_main=FALSE;
                  timermain=FALSE;
                  timer_zaehler=0;
                  timer_main=xvt_timer_create(main_win,1000); //in wspwin.cpp
                }
                else
                  is_128_ok=FALSE;
                char help_n[100];
                xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,help_n,50);
                xvt_fsys_convert_str_to_dir(help_n,&file_spec.dir);
                xvt_fsys_set_dir(&STR_SPEC.dir);
              }
            }
            else
            {
              xvt_res_get_str(STR_DEMO_NOTE_5,buf,sizeof(buf));
              xvt_dm_post_note("%s",buf);
              //xvt_dm_post_note("Konvertierung bei DEMO nicht möglich");
            };
          } //if back==1
        }
        menue_alles_oeffnen(); //in read.cpp
        konvertierung=FALSE;
      }
      break;

    case TASK_MENUBAR_30_84_87: // Menu  "Editor" starten
      StartAutoSketch(main_win,MODE_EDITOR_START);  // Optionendialog anzeigen)
      break;
      // alt 	case TASK_MENUBAR_1_2: /* Ergebnis einsehen */
      
    case TASK_MENUBAR_30_12: /* jetzt Tabelle */
      {
        ergebnis_tabelle_lesen=TRUE;
        if(win122!=NULL_WIN)
        {
          xvt_vobj_destroy(win122);
          win122=NULL_WIN;
        }
        if (dlg_136!=NULL_WIN)
        {   // Dialog 136 schließen
          xdEvent->type =E_CONTROL;
          xdEvent->v.ctl.id=DLG_136_PUSHBUTTON_2;
          xvt_win_dispatch_event(dlg_136,xdEvent);
        }
        
        //Dick 10.02.99 Neue Editor
        int ok_file;
        char *path=new char[255];
        char str3 [150];
        int len;
        char file_name[256];
        
        xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,path,250);
        
        //**************************
        berechnen=FALSE; //Steuervariablen für dlg203 OK
        editieren=FALSE;//Steuervariablen für dlg203 OK
        if (!dlg135ref)
        {
          var_dlg135=TRUE;
          if (!xvt_dlg_create_res(WD_MODAL,DLG_135, EM_ALL, DLG_135_eh, 0L))
            xvt_dm_post_error("Can't open dialog 135");
          var_dlg135=FALSE;
        }
        if(dlg135ref)
        {
          xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str3,50);
          len=strlen(str3);
          str3[len-4]='\0';
          strcat(str3,"dath");
          
          
          xvt_fsys_convert_str_to_dir(str3,&file_spec.dir);
          
          zaehler_aus_ber_lesen();
          int ok = read_start_berechnung(); //in read
          
          wsplist=10;  //auf 10 gesetzt, damit abbricht in dlg203 ?????
          
          if(ok!=0)
          {
            abbruch203=FALSE;
            xvt_dlg_create_res(WD_MODAL, DLG_203, EM_ALL, DLG_203_eh, 16L);//Dick 1.02.99 16L
          }
          else
          {
            abbruch203=TRUE;
          }
          
          wsplist=0;
          ergebnis_tabelle_lesen=FALSE;
          
          if((abbruch203==FALSE) && (ok!=0))
          {
            ok_file=IDOK;
            strcpy(file_name,ber_spec.name);
            if( LWA_PROJEKT )
            {
              file_name[2]='e';
              file_name[3]='r';
            }
            else
            {
              file_name[2]='t';
              file_name[3]='b';
            };
            
            strcpy(str3,"dath\\");   
            strcat(str3,file_name);
            strcpy(file_name,str3);
          }
          
        } //if dlg135ref
        //***************************
        if (ok_file == IDOK && !abbruch203)
        {
          char szApp[] = "EDITOR";
          char szCommand[MAX_PATH];
          sprintf(szCommand,"[project(\"%s\",\"%s\")]",Projektname_aktuell,file_name);
          if(!InitDDE(szApp,szCommand, NULL, NULL))
          {
            //xvt_dm_post_note("Editor ist nicht vorhanden!");                      
            char buf[200];//Dick 26.11.99
            xvt_res_get_str(STR_EDITOR,buf,sizeof(buf));
            xvt_dm_post_note("%s",buf);
          }
          // }
          
        }
        delete[] path;
        //ende neue Editor */
        xvt_fsys_set_dir(&STR_SPEC.dir);
    }
    break;

    case TASK_MENUBAR_43_6_61: /* DA66 Konvertierung*/
      {
        BOOLEAN da66ja=FALSE;
        char ausgewahl_dateien[AUFNEHM_BUFFER_SIZE];
        char name[15];
        char help_n[100];
        char buf[200],buf2[200],buf3[200];//Dick 26.11.99
        
        if(win122!=NULL_WIN)
        {
          xvt_vobj_destroy(win122);
          win122=NULL_WIN;
        }
        menue_alles_sperren(); //in read.cpp
        
        help_n[0]='\0';
        
        xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,help_n,90);
        
        strcat(help_n,"\\");
        
        xvt_res_get_str(STR_ABBRECHEN,buf,sizeof(buf));
        xvt_res_get_str(STR_LWAKONV_NOTE_1,buf2,sizeof(buf2));
        xvt_res_get_str(STR_LWAKONV_NOTE_2,buf3,sizeof(buf3));
        switch(xvt_dm_post_ask("OK",buf,NULL,"%s%s%s",buf2,help_n,buf3))
        {
        case RESP_DEFAULT: //OK
          da66ja=TRUE;
          break;
        case RESP_2:
          da66ja=FALSE;
          break;
        }
        if (da66ja)
        {
          //Übergabe 0 heißt nur 1 Datei darf ausgewählt werden, dann in
          //ausgewahl_datei, Pfad+datei
          int back=auswahl2(ausgewahl_dateien, 0, 0L); //in aufnehm.cpp

          if ( GetFeature( "wsp_nodemo" ) )
          {
            if(back==IDOK)
            {
              
              if (!xvt_dlg_create_res(WD_MODAL,DLG_128, EM_ALL, DLG_128_eh, 0L))
                xvt_dm_post_error("Can't open dlg128");
              if (cfg_list==NULL)
                is_128_ok=FALSE;
              if(is_128_ok)
              {
                xvt_slist_add_at_elt(cfg_list,NULL,str_netz,0);
                int j2=0;
                for (int i=0; i<10; i++)
                {
                  if (str_netz[i]!=' ')
                  {
                    name[j2]=str_netz[i];
                    j2++;
                  }
                }
                name[j2]='\0';
                safe_cfg_dat();
              } //if is128 ok
            } //if back==IDOK
            else if(back==IDCANCEL)break;
            else break;
            
            xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,help_n,50);
            xvt_fsys_convert_str_to_dir(help_n,&file_spec.dir);
            xvt_fsys_set_dir(&STR_SPEC.dir);
            
            /*********neue str_datei**************/
            anzahl_profil_dat_entries = 0;
            if (is_128_ok)   // dlg128 wurde nicht abgebrochen
            {
              if (prof_datei != NULL)
              {
                xvt_slist_destroy(prof_datei);
                prof_datei = NULL;
              }
              if ((prof_datei=xvt_slist_create())==NULL)
                xvt_dm_post_error("Error while creation of SLIST:prof_datei\\util.cpp");
              
              if (prof_datei != NULL)
              {
                char dateien[350];
                memset(dateien,' ',350);
                xvt_slist_destroy(prof_datei);
                prof_datei = NULL;
                choice_neu = FALSE;
              }
            } //is 128ok
            file_spec.name[0] ='\0';
            
            destroy_strang_tabelle();
            MakeNewStrang(STRANGANZAHL);
            make_sort_strangtabelle();    //neue Strangtabelle anlegen-sortieren
            if(is_128_ok)
              save_str_datei();
            
            safe_cfg_dat();
            
            destroy_strang_tabelle();
            
            
            /*******ende neue str_datei************/
            strcat(help_n,"\\");
            
            da66(ausgewahl_dateien,help_n);
          }
          else
          {
            //xvt_dm_post_note("Konvertierung bei Demo nicht möglich");
            
            xvt_res_get_str(STR_DEMO_NOTE_5,buf,sizeof(buf));
            xvt_dm_post_note("%s",buf);
          };
        } //if da66ja
        menue_alles_oeffnen(); //in read.cpp
        konvertierung=FALSE;
     }
     break;
   
    case TASK_MENUBAR_30_84_88: /* Programm starten:Wehre.pif */
      {
        char wehr[250];
        strcpy(wehr,start_dir);
        strcat(wehr,"wehre.exe");
        unsigned fuCmdShow = WinExec(wehr,SW_SHOWNORMAL);
      }
      break;

    case TASK_MENUBAR_2_85_89: /* Programm starten:Gerinne.pif */
      {
        char gerinne[250];
        strcpy(gerinne,start_dir);
        strcat(gerinne,"gerinne.exe");
        unsigned fuCmdShow = WinExec(gerinne,SW_SHOWNORMAL);
      }
      break;
      
    case TASK_MENUBAR_30_90: /* Programm starten:Rohre.pif */
      {
        char rohre[250];
        strcpy(rohre,start_dir);
        strcat(rohre,"rohre.exe");
        unsigned fuCmdShow = WinExec(rohre,SW_SHOWNORMAL);
      }
      break;
      
    case TASK_MENUBAR_30_90_92: /* Programm starten:Strassenbaumodul*/
      {
        char strasse[250];
        strcpy(strasse,start_dir);
        strcat(strasse,"stra.exe");
        unsigned fuCmdShow = WinExec(strasse,SW_SHOWNORMAL);
      }
      break;
      
    case TASK_MENUBAR_30_29_93: /* Programm starten:Entlastungsbauwerke.pif */
      {
        char entl[250];
        strcpy(entl,start_dir);
        strcat(entl,"hymatv.exe");
        unsigned fuCmdShow = WinExec(entl,SW_SHOWNORMAL);
      }
      break;
      
    case TASK_MENUBAR_29_31_33:  /*Wasserspiegel in alle Querprofile einfügen*/
      {
        DIRECTORY dirp;
        char str3[100];
        char str_spec_name_save[20];
        
        map_object_command = FALSE;
        strcpy(str_spec_name_save,STR_SPEC.name);
        xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str3,50);
        len=strlen(str3);
        str3[len-4]='\0';
        strcat(str3,"dath");
        xvt_fsys_convert_str_to_dir(str3,&dirp);
        if(win122!=NULL_WIN)
        {
          xvt_vobj_destroy(win122);
          win122=NULL_WIN;
        }
        menue_alles_sperren(); //in read.cpp
        berechnen=FALSE; //Steuervariablen für dlg203 OK
        editieren=FALSE;//Steuervariablen für dlg203 OK
        if (!dlg135ref)
        {
          var_dlg135=TRUE;
          if (!xvt_dlg_create_res(WD_MODAL,DLG_135, EM_ALL, DLG_135_eh, 0L))
            xvt_dm_post_error("Can't open dialog 135");
          var_dlg135=FALSE;
        }
        if(dlg135ref)
        {
          xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str3,50);
          len=strlen(str3);
          str3[len-4]='\0';
          strcat(str3,"dath");
          xvt_fsys_convert_str_to_dir(str3,&file_spec.dir);
          zaehler_aus_ber_lesen();
          ok = read_start_berechnung(); //in read
          if(ok!=0)
          {
            abbruch203=FALSE;
            Ber_edit_sperr=TRUE;//Dick 11.02.99
            xvt_dlg_create_res(WD_MODAL, DLG_203, EM_ALL, DLG_203_eh, 4L);//Dick 1.02.99 4L
            Ber_edit_sperr=FALSE;//Dick 11.02.99
            if(map_object_command) //Dick 29.09.99
            {
              map_object_command=FALSE;                   
            }
            else
              strcpy(STR_SPEC.name,str_spec_name_save); //Dick 11.02.99
          }
          else
          {
            abbruch203=TRUE;
          }
          if((abbruch203==FALSE) && (ok!=0))
          {
            strcpy(file_spec.name,ploti.name);
            //**************			 xvt_dm_post_note("Graphik-Editor");*****************
            CURSOR cursor = xvt_win_get_cursor(main_win);
            xvt_win_set_cursor(main_win,CURSOR_WAIT);                                      
            
            xvt_fsys_set_dir(&file_spec.dir);
            if( LWA_PROJEKT )
            {
              ber_spec.name[2]='p'; //in ber-spec.name steht Name der Ber. variante
              ber_spec.name[3]='l'; //=Längsschnittdatei
            }
            else
            {
              ber_spec.name[2]='w'; //in ber-spec.name steht Name der Ber. variante
              ber_spec.name[3]='l'; //=Längsschnittdatei
            };

            int lesen_ok = read_profildatei( pWPL, &dirp, ber_spec.name );
            if (lesen_ok ==0)
            {
              InsertWspLpToQuerprof(pWPL,&dirp,ber_spec.name,steuerdaten.info,STATION);
              xvt_win_set_cursor(main_win,cursor);
              editieren=FALSE;
              berechnen=FALSE;
              char buf[200],buf2[200];//Dick 26.11.99
              xvt_res_get_str(STR_WS_EINFG_OK,buf,sizeof(buf));
              xvt_res_get_str(STR_WS_EINFG,buf2,sizeof(buf2));
              MessageBox(NULL,buf,buf2,MB_ICONINFORMATION|MB_OK); //	"Operation:Wasserspiegel einfügen...abgeschlossen",
            }
            else
            {
              //xvt_dm_post_error("Längsschnittdatei kann nicht gelesen werden!");
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_READ_LAENGS,buf,sizeof(buf));
              xvt_dm_post_note("%s",buf);
              editieren=FALSE;
              berechnen=FALSE;
            }
          } //abbruch203=FALSE;
        }   //dlg135ref
        menue_alles_oeffnen(); // in read.cpp
      }
      break;
      
    case TASK_MENUBAR_29_31_34:  /*Wasserspiegel in allen Querprofilen löschen*/
      {
        
        /*jetzt automatisch, aber später*/
        DIRECTORY dirp;
        char str3[100];
        char str_spec_name_save[20];//Dick 11.02.99  
        map_object_command=FALSE;//Dick 29.09.99
        strcpy(str_spec_name_save,STR_SPEC.name);//Dick 11.02.99
        xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str3,50);
        len=strlen(str3);
        str3[len-4]='\0';
        strcat(str3,"dath");
        xvt_fsys_convert_str_to_dir(str3,&dirp);
        
        
        ///**längsschnitt einsehen:**
        if(win122!=NULL_WIN)
        {
          xvt_vobj_destroy(win122);
          win122=NULL_WIN;
        }
        menue_alles_sperren(); //in read.cpp
        berechnen=FALSE; //Steuervariablen für dlg203 OK
        editieren=FALSE;//Steuervariablen für dlg203 OK
        if (!dlg135ref)
        {
          var_dlg135=TRUE;
          if (!xvt_dlg_create_res(WD_MODAL,DLG_135, EM_ALL, DLG_135_eh, 0L))
            xvt_dm_post_error("Can't open dialog 135");
          var_dlg135=FALSE;
        }
        if(dlg135ref)
        {
          xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str3,50);
          len=strlen(str3);
          str3[len-4]='\0';
          strcat(str3,"dath");
          xvt_fsys_convert_str_to_dir(str3,&file_spec.dir);
          zaehler_aus_ber_lesen();
          ok = read_start_berechnung(); //in read
          if(ok!=0)
          {
            abbruch203=FALSE;
            Ber_edit_sperr=TRUE;//Dick 11.02.99
            xvt_dlg_create_res(WD_MODAL, DLG_203, EM_ALL, DLG_203_eh, 5L);//Dick 1.02.99 5L
            Ber_edit_sperr=FALSE;//Dick 11.02.99
            if(map_object_command) //Dick 29.09.99
            {
              map_object_command=FALSE;                   
            }
            else
              strcpy(STR_SPEC.name,str_spec_name_save); //Dick 11.02.99
          }
          else
          {
            abbruch203=TRUE;
          }
          
          if((abbruch203==FALSE) && (ok!=0))
          {
//            strcpy(file_spec.name,ploti.name);
            ///**************			 xvt_dm_post_note("Graphik-Editor");****************
            CURSOR cursor = xvt_win_get_cursor(main_win);
            xvt_win_set_cursor(main_win,CURSOR_WAIT);
            xvt_fsys_set_dir(&file_spec.dir);
            if( LWA_PROJEKT )
            {
              ber_spec.name[2]='p'; //in ber-spec.name steht Name der Ber. variante
              ber_spec.name[3]='l'; //=Längsschnittdatei
            }
            else
            {
              ber_spec.name[2]='w'; //in ber-spec.name steht Name der Ber. variante
              ber_spec.name[3]='l'; //=Längsschnittdatei
            };

            int lesen_ok = read_profildatei( pWPL, &file_spec.dir, ber_spec.name );
            if (lesen_ok ==0)
            {
              DeleteWspInQuerprof(pWPL,&dirp,ber_spec.name,steuerdaten.info);
              xvt_win_set_cursor(main_win,cursor);
              editieren=FALSE;
              berechnen=FALSE;
              char buf[200],buf2[200];//Dick 26.11.99
              xvt_res_get_str(STR_WS_DEL_OK,buf,sizeof(buf));
              xvt_res_get_str(STR_WS_DEL,buf2,sizeof(buf2));
              MessageBox(NULL,buf,buf2,MB_ICONINFORMATION|MB_OK);
            }
            else
            {
              //xvt_dm_post_error("Längsschnittdatei kann nicht gelesen werden!");
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_READ_LAENGS,buf,sizeof(buf));
              xvt_dm_post_note("%s",buf);
              editieren=FALSE;
              berechnen=FALSE;
            }
            
            
          } //abbruch203=FALSE;
        }   //dlg135ref
        menue_alles_oeffnen(); // in read.cpp
        ///**Ende Längsschnitt einsehen**
    }
    break;
    
    case TASK_MENUBAR_1:  /*CAD-Programm*/
      StartAutoSketch(main_win,MODE_ASKETCH_START); // Pfad aus win.ini lesen und Autosketch starten
      break;
      
    case TASK_MENUBAR_30:      /*Vergleichsdaten aufnehmen*/
      if (berechnen && editieren) //d.h. Längsschnitt
      {  
        //Neu Dick 22.06.99
        int anzahl_datensatz_mit_2=0;
        for(int i=1;i<=ds_info[0];i++)
        {
          if(typ[i]==SOHLHOEHE_2)
            anzahl_datensatz_mit_2++;
        }
        if(anzahl_datensatz_mit_2>=10)
        {
          //xvt_dm_post_note("Sie können maximal nur 10 Vergleichsdaten aufnehmen!");
          char buf[200];//Dick 26.11.99
          xvt_res_get_str(STR_READ_LAENGS,buf,sizeof(buf));
          xvt_dm_post_note("%s",buf);
          break;
        }
        //Ende Neu
        char str_spec_name_save[20];//Dick 5.11.98
        map_object_command=FALSE;//Dick 29.09.99
        strcpy(str_spec_name_save,STR_SPEC.name);//Dick 5.11.98
        
        berechnen=FALSE;
        editieren=FALSE; //auf false gesetzt, damit in 203 verfährt, wie bei Plotten
        //d.h. als Rückgabe steht gewählte L.datei(pl-BCE-Format) in extern: ploti.name
        zaehler_aus_ber_lesen();
        ok = read_start_berechnung(); //in read_cfg
        
        if(ok!=0)
        {
          Ber_edit_sperr=TRUE;	 
          xvt_dlg_create_res(WD_MODAL, DLG_203, EM_ALL, DLG_203_eh, 6L);//Dick 1.02.99 6L
          berechnen=TRUE;
          editieren=TRUE;
          Ber_edit_sperr=FALSE;
          if(!abbruch203)
            int teste=MakeLaengsprofilVerknuepfung(pWPL,ploti.name);
        }
        if(map_object_command)//Dick 29.09.99
        {
          map_object_command=FALSE;                   
        }
        else
          strcpy(STR_SPEC.name,str_spec_name_save); //Dick 11.02.99
      }
      
      break;
      
    case TASK_MENUBAR_30_80:  //Liste 
      {
        char  str4[150];
        char str5[150],newfile[150],str3[100];
        char verzeichnis_erste_variante[150];
        FILE *in, *out;
        char helpstring[15];
        char * batch_elem;
        int anzahl_varianten, i;
        BOOLEAN fehler=FALSE;
        
        wsplist=1; //1 bedeutet Listenauswertung
        if(win122!=NULL_WIN)
        {
          xvt_vobj_destroy(win122);
          win122=NULL_WIN;
        }
        menue_alles_sperren(); //in read.cpp
        berechnen=FALSE; //Steuervariablen für dlg203 OK
        editieren=FALSE;//Steuervariablen für dlg203 OK
        
        
        if (!dlg135ref)
        {
          var_dlg135=TRUE;
          if (!xvt_dlg_create_res(WD_MODAL,DLG_135, EM_ALL, DLG_135_eh, 0L))
            xvt_dm_post_error("Can't open dialog 135");
          var_dlg135=FALSE;
        }
        if(dlg135ref)
        {
          xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str3,50);
          len=strlen(str3);
          str3[len-4]='\0';
          strcat(str3,"dath");
          
          strcpy(verzeichnis_erste_variante,str3);
          
          xvt_fsys_convert_str_to_dir(str3,&file_spec.dir);
          zaehler_aus_ber_lesen();
          ok = read_start_berechnung(); //in read
          if(ok!=0)
          {
            abbruch203=FALSE;
            xvt_dlg_create_res(WD_MODAL, DLG_400, EM_ALL, DLG_203_eh, 7L);//Dick 1.02.99 7L
          }
          
          if((abbruch203==FALSE) && (ok!=0))
          {
            
            anzahl_varianten=xvt_slist_count(batch_list);
            
            for(i=0; i<anzahl_varianten;i++)
            {
              batch_elem=xvt_slist_get_elt(batch_list,i,0L);
              strcpy(ber_spec.name,batch_elem);
              if(i==0)
              {
                strcat(str3,"\\");
                strcpy(helpstring,ber_spec.name);
                
                helpstring[2]='e';         //helpstring wird unten weiter
                //verwendet mit selbem Inhalt
                helpstring[3]='r';
                
                strcpy(newfile,str3);
                strcat(newfile,"backup.erg");
                
                strcat(str3,helpstring);
                
                
                if((access(str3,00))==0) //Ergebnis vorhanden
                {
                  int handle=open(str3,O_RDONLY|O_TEXT);
                  int laenge=filelength(handle);
                  close(handle);
                  if (laenge>0)
                  {
                    copy_fileb(str3,newfile);
                    if ((in = fopen(str3,"a+"))==NULL)
                    {
                      //xvt_dm_post_note("Datei %s läßt sich nicht öffnen",str3);
                      char buf[200],buf2[200];//Dick 26.11.99
                      xvt_res_get_str(STR_DATEI,buf,sizeof(buf));
                      xvt_res_get_str(STR_CANOTOPEN,buf2,sizeof(buf2));
                      xvt_dm_post_note("%s%s%s",buf,str3,buf2);
                    }
                    
                    strcpy(str5,str3);
                    xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str3,50);
                    len=strlen(str3);
                    str3[len-4]='\0';
                    strcat(str3,"dath\\");
                    strcat(str3, ber_spec.name);   //variable wird noch gebraucht nicht doppel besetzen
                  } //if laenge  >0
                } //access
                else
                {    
                  //xvt_dm_post_error("Ergebnisdatei nicht vorhanden!");
                  char buf[200];//Dick 26.11.99
                  xvt_res_get_str(STR_KEINE_ERGEBNISDAT,buf,sizeof(buf));
                  xvt_dm_post_note("%s",buf);
                  
                  editieren=FALSE;
                  berechnen=FALSE;
                  fehler=TRUE;
                  break;
                }
                
              } //i=0
              if(i>0)
              {
                ber_spec.name[2]='e';
                ber_spec.name[3]='r';
                xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str4,50);
                len=strlen(str4);
                str4[len-4]='\0';
                strcat(str4,"dath");
                
                strcat(str4,"\\");
                strcat(str4, ber_spec.name);
                if ((out = fopen(str4,"r+"))==NULL)
                {
                  //xvt_dm_post_note("Datei %s läßt sich nicht öffnen",str4);
                  char buf[200],buf2[200];//Dick 26.11.99
                  xvt_res_get_str(STR_DATEI,buf,sizeof(buf));
                  xvt_res_get_str(STR_CANOTOPEN,buf2,sizeof(buf2));
                  xvt_dm_post_note("%s%s%s",buf,str4,buf2);
                  fehler=TRUE;
                }
                else
                {
                  while(!(feof(out)))
                  {
                    fgets(str4,148,out);
                    fprintf(in,"%s",str4);
                  }
                  fclose(out);
                }
              } //i>0
            } //for anzahl batchlistelem
            xvt_slist_destroy(batch_list);
            batch_list=NULL;
            
            wsplist=1;
            
            
            if(!fehler)
            {
              if(in!=NULL)fclose(in);
              copy_fileb(newfile,str5);
              BOOLEAN test=WriteWsplistenDatei(str3); //in listausw.cpp
              if (test)
              {
                helpstring[2]='0';
                helpstring[3]='0';
                strcpy(ber_spec.name,helpstring);  //für Ausgabe, nächste Zeile auch
                xvt_fsys_convert_str_to_dir(verzeichnis_erste_variante,&STR_SPEC.dir);


                if(win_list(LISTE))
                  {
                    // Neue Editor Dick 4.03.99
                    char szApp[] = "EDITOR";
                    char szCommand[MAX_PATH];
                    //HWND hwndWin;
                    
                    for(int i=0;i<4;i++)
                    {
                      switch(i)
                      {
                      case 0:
                        
                        ber_spec.name[2]='w';
                        ber_spec.name[3]='k';
                        strcpy(str4,"\"dath\\");
                        strcat(str4, ber_spec.name);
                        strcat(str4, "\"");
                        break;
                      case 1:
                        ber_spec.name[2]='u';
                        ber_spec.name[3]='e';
                        strcat(str4, ",");
                        strcat(str4,"\"dath\\");
                        strcat(str4, ber_spec.name);
                        strcat(str4, "\"");
                        break;
                      case 2:
                        ber_spec.name[2]='m';
                        ber_spec.name[3]='a';
                        strcat(str4, ",");
                        strcat(str4,"\"dath\\");
                        strcat(str4, ber_spec.name);
                        strcat(str4, "\"");
                        break;
                      case 3:
                        ber_spec.name[2]='e';
                        ber_spec.name[3]='x';
                        strcat(str4, ",");
                        strcat(str4,"\"dath\\");
                        strcat(str4, ber_spec.name);
                        strcat(str4, "\"");
                        break;
                        
                      }
                    }//for
                    sprintf(szCommand,"[project(\"%s\",%s)]",Projektname_aktuell,str4);
                    
                    if(!InitDDE(szApp,szCommand, NULL, NULL))
                    {
                      //xvt_dm_post_note("Editor ist nicht vorhanden!");
                      char buf[200];//Dick 26.11.99
                      xvt_res_get_str(STR_EDITOR,buf,sizeof(buf));
                      xvt_dm_post_note("%s",buf);
                    }
                    //}
                    
                    //
                  }
              }
              editieren=FALSE;
              berechnen=FALSE;
            }
            
            
      } //abbruch203=FALSE;
     }   //dlg135ref
     
     //STR_SPEC von dath auf prof zurücksetzen
     static char str3_1[100];
     xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str3_1,50);
     len=strlen(str3_1);
     str3_1[len-4]='\0';
     strcat(str3_1,"prof");
     
     xvt_fsys_convert_str_to_dir(str3_1,&STR_SPEC.dir);
     
     /****/
     menue_alles_oeffnen(); // in read.cpp
     wsplist=0;
     
    }
    //Auswertung:Liste
    break;
    
    case TASK_MENUBAR_29_32_81: //Auswertung:Vergleich
      {
        wsplist=2; //2 bedeutet Vergleichslisten
        char str3[100];
        char helpstring[15];
        char BerVar1[100],BerVar2[100],BerVar3[100];
        int anzahl_varianten,i;
        char *batch_elem;
        char verzeichnis_erste_variante[150];
        BOOLEAN ergebnisse_da=1;
        int teste ;//, handle;
        char buf[200],buf2[200],buf3[200];//Dick 26.11.99
        
        if(win122!=NULL_WIN)
        {
          xvt_vobj_destroy(win122);
          win122=NULL_WIN;
        }
        menue_alles_sperren(); //in read.cpp
        berechnen=FALSE; //Steuervariablen für dlg203 OK
        editieren=FALSE;//Steuervariablen für dlg203 OK
        if (!dlg135ref)
        {
          var_dlg135=TRUE;
          if (!xvt_dlg_create_res(WD_MODAL,DLG_135, EM_ALL, DLG_135_eh, 0L))
            xvt_dm_post_error("Can't open dialog 135");
          var_dlg135=FALSE;
        }
        if(dlg135ref)
        {
          xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str3,50);
          len=strlen(str3);
          str3[len-4]='\0';
          strcat(str3,"dath");
          
          
          strcpy(verzeichnis_erste_variante,str3);
          
          xvt_fsys_convert_str_to_dir(str3,&file_spec.dir);
          strcat(str3,"\\");
          
          strcpy(BerVar1,str3);
          strcpy(BerVar2,str3);
          //	strcpy(BerVar3,str3);
          
          
          xvt_res_get_str(STR_VERGL_NOTE_3,buf,sizeof(buf));
          xvt_dm_post_note("%s",buf);
          //xvt_dm_post_note("Bitte wählen Sie 3 (mind. 2) Ergebnisdateien aus.");
          
          /*---->  *********************************************************************************/
          zaehler_aus_ber_lesen();
          ok = read_start_berechnung(); //in read
          if (ok==0)
          {
            //xvt_dm_post_note("Es existieren keine Berechnungsvarianten");
            break;
          }
          
          abbruch203=FALSE;
          xvt_dlg_create_res(WD_MODAL, DLG_400, EM_ALL, DLG_203_eh, 8L);//Dick 1.02.99 8L
          
          
          if((abbruch203==FALSE) && (ok!=0))
          {
            anzahl_varianten=xvt_slist_count(batch_list);
            if(anzahl_varianten<2)
            {
              //char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_VERGL_NOTE_1,buf,sizeof(buf));
              xvt_dm_post_note("%s",buf);
              //xvt_dm_post_note("Es müssen mindestens zwei Varianten gewählt werden");
              anzahl_varianten=0;
              wsplist=0;
              if(batch_list!=NULL)
              {
                xvt_slist_destroy(batch_list);
                batch_list=NULL;
              }
              break;
            }
            if (anzahl_varianten>3)
            {
              //xvt_dm_post_note("Es werden nur die ersten 3 Varianten berücksichtigt");
              xvt_res_get_str(STR_VERGL_NOTE_1,buf,sizeof(buf));
              xvt_dm_post_note("%s",buf);
              anzahl_varianten=3;
            }
            if(anzahl_varianten==2)
            {
              strcpy(BerVar3,"#");//Dick 8.5.98
            }
            
            for(i=0;i<anzahl_varianten;i++)
            {
              batch_elem=xvt_slist_get_elt(batch_list,i,0L);
              strcpy(ber_spec.name,batch_elem);
              ber_spec.name[2]='e';
              ber_spec.name[3]='r';
              if(i==0)
              {
                strcat(BerVar1,ber_spec.name);
                strcpy(helpstring,ber_spec.name); //für Ausgabe merken
              }
              if(i==1)
                strcat(BerVar2,ber_spec.name);
              if(i==2)
              {
                strcpy(BerVar3,str3);
                strcat(BerVar3,ber_spec.name);
              }
            }
          } //abbruch203=FALSE;
          else
          {
            wsplist=0;
            if(batch_list!=NULL)
            {
              xvt_slist_destroy(batch_list);
              batch_list=NULL;
            }
            break;
          }
          
          ergebnisse_da=1;
          for(int i=1; i<=anzahl_varianten;i++)
          {
            if(i==1)
              teste=access(BerVar1,00);
            if(i==2)
              teste=access(BerVar2,00);
            if(i==3)
              teste=access(BerVar3,00);
            
            if(teste==0) //Ergebnisdatei vorhanden
            {
            }
            else
            {
              //xvt_dm_post_note("Ergebnisdatei: %s nicht vorhanden oder noch keine Berechnung durchgeführt!",BerVar1);
              xvt_res_get_str(STR_ERGEBNISDATEI,buf,sizeof(buf));
              xvt_res_get_str(STR_NICHT_VORHANDEN,buf2,sizeof(buf2));
              xvt_res_get_str(STR_ODER_NOCH,buf3,sizeof(buf3));
              xvt_dm_post_error("%s%s%s%s",buf,BerVar1,buf2,buf3);
              ergebnisse_da=0;
              break;
            }
          } //for anzahl_varianten
          /*---->  *********************************************************************************/
          
          xvt_slist_destroy(batch_list);
          batch_list=NULL;
          
          if(ergebnisse_da!=0)
          {
            if (WriteVglListenDatei(BerVar1,BerVar2,BerVar3))
              
              helpstring[2]='0';
            helpstring[3]='0';
            strcpy(ber_spec.name,helpstring);  //für Ausgabe, nächste Zeile auch
            str3[strlen(str3)-1]='\0'; 
            xvt_fsys_convert_str_to_dir(str3,&STR_SPEC.dir);
            
            if(win_list(VERGLEICH)) //in listausw.cpp  write_bat_list
              {
                // Neue Editor Dick 4.03.99 
                char szApp[] = "EDITOR"; 
                char szCommand[MAX_PATH];
                //HWND hwndWin;
                char str4[100];
                
                strcpy(str4,"dath\\");                                                          
                strcat(str4, ber_spec.name);
                sprintf(szCommand,"[project(\"%s\",\"%s\")]",Projektname_aktuell,str4);
                
                if(!InitDDE(szApp,szCommand, NULL, NULL))
                {
                  //xvt_dm_post_note("Editor ist nicht vorhanden!");
                  //char buf[200];//Dick 26.11.99
                  xvt_res_get_str(STR_EDITOR,buf,sizeof(buf));
                  xvt_dm_post_note("%s",buf);
                }
                //}
                //
              }
              
          }
     }   //dlg135ref
     
     //STR_SPEC von dath auf prof zurücksetzen
     xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str3,50);
     len=strlen(str3);
     str3[len-4]='\0';
     strcat(str3,"prof");
     
     xvt_fsys_convert_str_to_dir(str3,&STR_SPEC.dir);
     
     /****/
     
     menue_alles_oeffnen(); // in read.cpp
     wsplist=0;
    }
    break;

    case TASK_MENUBAR_29_32_83_84: /* Prüflisten:Voreinstellung  */    
      WriteDefPrueflistenDatei();
      break;
      
    case TASK_MENUBAR_29_32_83_82: /* Prüflisten:Programmstart  */    
      {
        wsplist=3; //1 bedeutet Listenauswertung
        char str3[100];
        char verzeichnis[150];
        BOOLEAN bewuchs_var=FALSE;
        
        if(win122!=NULL_WIN)
        {
          xvt_vobj_destroy(win122);
          win122=NULL_WIN;
        }
        menue_alles_sperren(); //in read.cpp
        berechnen=FALSE; //Steuervariablen für dlg203 OK
        editieren=FALSE;//Steuervariablen für dlg203 OK
        if (!dlg135ref)
        {
          var_dlg135=TRUE;
          if (!xvt_dlg_create_res(WD_MODAL,DLG_135, EM_ALL, DLG_135_eh, 0L))
            xvt_dm_post_error("Can't open dialog 135");
          var_dlg135=FALSE;
        }
        if(dlg135ref)
        {
          xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str3,50);
          len=strlen(str3);
          str3[len-4]='\0';
          strcat(str3,"dath");
          
          
          xvt_fsys_convert_str_to_dir(str3,&file_spec.dir);
          
          zaehler_aus_ber_lesen();
          ok = read_start_berechnung(); //in read
          if(ok!=0)
          {
            abbruch203=FALSE;
            xvt_dlg_create_res(WD_MODAL, DLG_203, EM_ALL, DLG_203_eh, 9L);//Dick 1.02.99 9L
          }
          if((abbruch203==FALSE) && (ok!=0))
          {
            strcat(str3,"\\");
            strcpy(verzeichnis,str3);
            
            char helpstring[15];
            strcpy(helpstring,ber_spec.name);
            
            helpstring[2]='b';
            helpstring[3]='e';
            strcat(verzeichnis,helpstring);
            if((access(verzeichnis,00))==0) //Ergebnis vorhanden
              bewuchs_var=TRUE;
            else
              bewuchs_var=FALSE;
            
            helpstring[2]='e';
            helpstring[3]='r';
            strcat(str3,helpstring);
            
            
            if((access(str3,00))==0) //Ergebnis vorhanden
            {
              int handle=open(str3,O_RDONLY|O_TEXT);
              int laenge=filelength(handle);
              close(handle);
              if((laenge!=0) && (laenge!=-1))
              {
                xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str3,50);
                len=strlen(str3);
                str3[len-4]='\0';
                strcat(str3,"dath\\");
                strcat(str3, ber_spec.name);
                
                if (WritePrueflistenDatei(str3,bewuchs_var))    //in listausw.cpp
                {
                  if(win_list(PRUEFUNG)) //in listausw.cpp  write_bat_list
                    {
                      // Neue Editor Dick 4.03.99
                      char szApp[] = "EDITOR";
                      char szCommand[MAX_PATH];
                      char str4[100];
                      strcpy(str4,"dath\\");                                                          
                      strcat(str4, ber_spec.name);
                      
                      sprintf(szCommand,"[project(\"%s\",\"%s\")]",Projektname_aktuell,str4);
                      
                      if(!InitDDE(szApp,szCommand, NULL, NULL))
                      {
                        //xvt_dm_post_note("Editor ist nicht vorhanden!");
                        char buf[200];//Dick 26.11.99
                        xvt_res_get_str(STR_EDITOR,buf,sizeof(buf));
                        xvt_dm_post_note("%s",buf);
                      }
                    }
                }
              }
              editieren=FALSE;
              berechnen=FALSE;
            }
            else
            {
              //xvt_dm_post_error("Ergebnisdatei %s nicht vorhanden!",str3);
              char buf[200],buf2[200];//Dick 26.11.99
              xvt_res_get_str(STR_ERGEBNISDATEI,buf,sizeof(buf));
              xvt_res_get_str(STR_NICHT_VORHANDEN,buf2,sizeof(buf2));
              
              xvt_dm_post_error("%s%s%s!",buf,str3,buf2);
              editieren=FALSE;
              berechnen=FALSE;
            }
            
          } //abbruch203=FALSE;
     }   //dlg135ref
     
     //STR_SPEC von dath auf prof zurücksetzen
     xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str3,50);
     len=strlen(str3);
     str3[len-4]='\0';
     strcat(str3,"prof");
     
     xvt_fsys_convert_str_to_dir(str3,&STR_SPEC.dir);
     
     /****/
     
     menue_alles_oeffnen(); // in read.cpp
     wsplist=0;
     
    }
    break;
    
    case TASK_MENUBAR_13_67: // Menu  "Plotprogramm"(extern)
      {
        char szApp[MAX_PATH];
        char szPath[MAX_PATH];
        char szCommand[MAX_PATH];
        DWORD len, type;
        HKEY hKey;
        BOOL bOK = FALSE;
        if( RegOpenKey( HKEY_CURRENT_USER, "Software\\BCE\\WSPWIN Plotter\\Info", &hKey ) == ERROR_SUCCESS )
        {
          len = MAX_PATH;
          type = REG_SZ;
          if (RegQueryValueEx(hKey, "FilePath", NULL, &type, (LPBYTE)szPath, &len)==ERROR_SUCCESS)
          {
            if (type==REG_SZ)
            {
              strcpy(szApp, szPath);
              if(WIN_116!=NULL)
                sprintf(szCommand,"[project(\"%s\",\"%s\",\"%s\")]",Projektname_aktuell,STR_SPEC.name,file_spec.name);
              else
                sprintf(szCommand,"[project(\"%s\")]",Projektname_aktuell);
              if (Projektname_aktuell[0]=='\0')
                szCommand[0] = '\0';
              
              if(InitDDE(szApp,szCommand, NULL, NULL))
                bOK = TRUE;
            }
          }
        }
        if (!bOK)
        {
          //xvt_dm_post_note("Plotprogramm ist nicht vorhanden!");
          char buf[200],buf2[200],buf3[200];//Dick 26.11.99
          xvt_res_get_str(STR_PLOTPROGRAMM,buf,sizeof(buf));
          xvt_res_get_str(STR_IST,buf2,sizeof(buf2));
          xvt_res_get_str(STR_NICHT_VORHANDEN,buf3,sizeof(buf3));
          xvt_dm_post_note("%s%s%s!",buf,buf2,buf3);
        }
      }
      break;
      
    case TASK_MENUBAR_8_28://Kontext Hilfe  "?"
      {
        {
          POINT pt;
          
          ::GetCursorPos(&pt);
          LPARAM lParam = MAKELPARAM(pt.x, pt.y);
          ::SendMessage((HWND)xvt_vobj_get_attr(main_win, ATTR_NATIVE_WINDOW), WM_SYSCOMMAND, SC_CONTEXTHELP, lParam);
        }
      }
      break;

    case TASK_MENUBAR_8_9://Inhalt
      {
        {
          char path[MAX_PATH];
          strcpy(path,start_dir);
          strcat(path,"\\wspwin.hlp");
          BOOL ok_help=WinHelp((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW),path,HELP_FINDER ,0);
        }
      }
      break;

    case TASK_MENUBAR_8_20://Optionen
      //kim DoOptionsDlg( (HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), LWA_PROJEKT == TRUE, GetFeature( "Umk_Station" ) == TRUE );
		DoOptionsDlg( (HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), LWA_PROJEKT == TRUE,TRUE );
      break;

    case TASK_MENUBAR_43_35://Projektübersicht
      DoProjectSummaryDlg((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW));
      break;
      
    case TASK_MENUBAR_ACCESS: // Menu Fremddaten->Acces
      {
        if( dlg_136 != NULL_WIN )
        {
          save_str_datei();
          xvt_vobj_destroy( dlg_136 );
          dlg_136=NULL_WIN;
        };
        
        DoDatenimportAcces( Projektname_aktuell );
        
        read_cfg_dat();
      };
      break;
      
    case TASK_MENUBAR_43_6_38://CADDY
      {
        if( GetFeature( "wsp_nodemo" ) )
        {
          char cfg_neu_str[MAX_PATH],temp[MAX_PATH];
          cfg_neu_str[0]='\0';
          
          if(dlg_136!=NULL_WIN)
          {
            save_str_datei();
            xvt_vobj_destroy(dlg_136);
            dlg_136=NULL_WIN;
          }
          
          BOOL ok=DoCaddyConvertion(Projektname_aktuell, (HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW),cfg_neu_str);
          if(ok)
          {
            read_cfg_dat();
            sscanf(cfg_neu_str,"%s%s",Gewaessername_aktuell,Zustand_aktuell);
            for (int i=73;i<=84;i++)
              temp[i-73] = cfg_neu_str[i];
            temp[12] = '\0';
            strcpy(STR_SPEC.name,temp);
            read_profil_dat(strang_anfang);
            strcpy(netz_dat[0],Gewaessername_aktuell);
            strcpy(netz_dat[2],Zustand_aktuell);

            setTitle( Projektname_aktuell, Gewaessername_aktuell, Zustand_aktuell );
          }
        }
        else
        {
          //xvt_dm_post_note("Konvertierung bei DEMO nicht möglich");
          char buf[200];//Dick 26.11.99
          xvt_res_get_str(STR_DEMO_NOTE_5,buf,sizeof(buf));
          xvt_dm_post_note("%s",buf);
        }
      }
      break;

    case TASK_MENUBAR_50_5://Netzverknüpfung
      {
        if (!dlg135ref)
        {
          //xvt_dm_post_note("Wählen Sie zuerst eine Zustandsdatei aus.");
          char buf[200];//Dick 26.11.99
          xvt_res_get_str(STR_ZUSTAND_WAHL,buf,sizeof(buf));
          xvt_dm_post_note("%s",buf);
        }
        if (dlg135ref)
          vzk_block(); 
      }
      break;
      
    case TASK_MENUBAR_14_18://Teilgebiete (BCE-Version)
      {         
        teilgebiete=TRUE;
        if(win122!=NULL_WIN)
        {
          xvt_vobj_destroy(win122);
          win122=NULL_WIN;
        }
        menue_alles_sperren(); //in read.cpp
        if (!dlg135ref)
        {
          var_dlg135=TRUE;
          if (!xvt_dlg_create_res(WD_MODAL,DLG_135, EM_ALL, DLG_135_eh, 0L))
            xvt_dm_post_error("Can't open dialog 135");
          var_dlg135=FALSE;
        }
        
        if (dlg135ref)
        {
          if (!xvt_win_create_res(QWERT_EDIT, TASK_WIN, EM_ALL, QWERT_EDIT_eh, 0L))
            xvt_dm_post_error("Can't open win122");
        }
        menue_alles_oeffnen(); // in read.cpp
      }
      break;

    case TASK_MENUBAR_14_28://Kopf.txt editieren 
	   DoKopfTxtDlg( ( GetFeature( "wsp_knaufedit" ) ), (HWND)xvt_vobj_get_attr( xdWindow,ATTR_NATIVE_WINDOW ), start_dir );
      break;
      
    case TASK_MENUBAR_WSPWIN_MAPPER: 
      {
        HKEY hKey;
        BOOL bOK = FALSE;
        
        if ( RegOpenKey( HKEY_CURRENT_USER, WspWin::REG_PATH_MAPPER_INFO, &hKey ) == 
          ERROR_SUCCESS )
        {
          DWORD len = MAX_PATH;
          DWORD type = REG_SZ;
          BYTE pData[MAX_PATH * sizeof(TCHAR) ];
          
          if ( RegQueryValueEx( hKey, WspWin::REG_KEY_NAME_FILEPATH, NULL, &type, (LPBYTE)&pData, &len ) == 
            ERROR_SUCCESS )
          {
            if ( type == REG_SZ )
            {
              TCHAR tszPath[MAX_PATH];
              memcpy( tszPath, pData, len );
              if( InitDDE( tszPath, "\0", NULL, NULL ) )
                bOK = TRUE;
            }
          }
        }
        if (!bOK)
        {
          // "WspWin Mapper ist nicht vorhanden!" 
          char buf[200],buf2[200],buf3[200];//Dick 26.11.99
          xvt_res_get_str(STR_WSPWIN_MAPPER,buf,sizeof(buf));
          xvt_res_get_str(STR_IST,buf2,sizeof(buf2));
          xvt_res_get_str(STR_NICHT_VORHANDEN,buf3,sizeof(buf3));
          xvt_dm_post_note("%s%s%s!",buf,buf2,buf3);
        }
      }
      break;
      
      //-----------------------------------------------------------------------------
    default:
      {
      }
      break;
  }
  menue_alles_oeffnen(); // in read.cpp
}
