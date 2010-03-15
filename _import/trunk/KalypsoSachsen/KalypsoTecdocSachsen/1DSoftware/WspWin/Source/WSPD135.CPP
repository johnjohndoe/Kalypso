/*****************************************************************************
*              WSPD135.CPP                                                   *
*		  Vernetzungsdatei auswählen                                           *
*****************************************************************************/
#include <windows.h>
#include "xvt.h"
#include "wspwin.h"
#include "resource.h"

#include "global_types.h"
#include "global_vars.h"
#include "..\..\wspdlg\Include\export.h"
#include "list.h"

#include "util.h"
#include "read_cfg.h"
#include "strang.h"
#include "profpro2.h"
#include "util2.h"
#include "global.h"
#include "readprof.h"
#include "bce_allg.h"

#include "wsphilfe.h"

extern XVT_HELP_INFO hi;

#define DLG_RES_ID DLG_135
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODAL

#ifndef XVT_INCL_NATIVE
#define XVT_INCL_NATIVE
#endif

extern int dialog_ende,
anzahl_str_dat_entries,       // Anzahl der *.str-Dateien
anzahl_profil_dat_entries,
dlg135ref,
cfg_list_index;  //Position der ausgewählten *.str-Datei
//aus der SLIST:cfg_list
extern OPENFILENAME ofn;
extern SLIST cfg_list,
prof_datei;
extern char *str,
str_netz[100];
extern BOOLEAN SaveNetzFile,
var_dlg135,
SaveStrangFile,
new_profil,
is_128_ok,
is_profil_open,
choice_neu,
Ber_edit_sperr;  //Dick 5.11.98 dient für 2 Sachen
extern FILE_SPEC STR_SPEC,
file_spec;
extern WINDOW Menu_Win;

BOOLEAN delete_str=FALSE;

SLIST netzdatei;
int sel,
ok_file;
char temp[100];
WINDOW dlg135=NULL;


XVT_FNTID font_id;
char *dateien ; //dateien[350] ;  //extern Ref in dlg210
//extern char profprojdatei[100];
extern SLIST profile_slist;
BOOLEAN profile_loeschen;
char strspecnamesave[15];

/*	Handler for dialog DLG_135 ("Auswahl der Vernetzungsdatei")*/

long XVT_CALLCONV1 DLG_135_eh XVT_CALLCONV2( WINDOW xdWindow, EVENT* xdEvent )
{
  short xdControlId = xdEvent->v.ctl.id;
  
  switch (xdEvent->type)
  {
  case E_CREATE:
    {

      dlg135 = xdWindow;
      dateien = new char[350];

      xvt_fsys_set_dir( &STR_SPEC.dir );
      if( var_dlg135 )
      {
        xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_135_PUSHBUTTON_3)),FALSE);
        xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_135_PUSHBUTTON_4)),FALSE);
      }

      /******************  File: *.cfg-Konfiguration ermitteln  *******************
      *			wsp.cfg lesen und in SLIST: cfg_list schreiben                     *
      *			wenn OK enthält 'anzahl_vern_dateien' die Anzahl der *.str-Dateien */

      if( read_cfg_dat() == -1 )   //WSP.CFG ist hiernach gelocked
        xvt_vobj_destroy( xdWindow );

      setTitle( Projektname_aktuell, 0, 0 ); // den Titel des Hauptfensters setzen
   
      dlg135ref = FALSE;
      //listbox135 = xvt_win_get_ctl (xdWindow,DLG_135_LBOX_5);
      NewSetFontFunc(xvt_win_get_ctl (xdWindow,DLG_135_LBOX_5));    //Schriftart für listbox135 ändern
      xvt_list_add((xvt_win_get_ctl (xdWindow,DLG_135_LBOX_5)), -1, (char *)cfg_list);   // Ausgabe in LISTBOX
      int anzahl_cfg_slist=xvt_slist_count(cfg_list);
      if ((netzdatei = xvt_slist_create())==NULL)
        xvt_dm_post_fatal_exit("SLIST Create Error -DLG135");
    
      if(anzahl_cfg_slist>0)
      {
        xvt_list_set_sel((xvt_win_get_ctl (xdWindow,DLG_135_LBOX_5)), 0, TRUE);
        netzdatei=xvt_list_get_sel(xvt_win_get_ctl (xdWindow,DLG_135_LBOX_5));
        sel = xvt_list_get_sel_index(xvt_win_get_ctl (xdWindow,DLG_135_LBOX_5));
        str=xvt_slist_get_elt(netzdatei,0,0);
        cfg_list_index=xvt_list_get_sel_index(xvt_win_get_ctl (xdWindow,DLG_135_LBOX_5));
        strcpy(str_netz,str);       //str_netz enthält ausgewählte *.str-Datei
      }
      
      if (hi!=NULL_HELP_INFO)
        xvt_help_set_win_assoc(hi, xdWindow, HID_KAPITEL_6_2_3, 0L);
    }
    break;
  case E_DESTROY:
    {
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_13_60,FALSE);//Profildatei-Geländeverknüpfung
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_13_64,FALSE);//Profildatei-Flächenberechnung
      
      delete[] dateien;
      dlg135=NULL;
    }
    break;
  case E_FOCUS:
    {
      /* Dialog has lost or gained focus.	*/
      if (xdEvent->v.active)  {
        /*	Dialog has gained focus	*/
      } else {
        /* 	Dialog has lost focus*/
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
      dialog_ende =0;
      xvt_vobj_destroy(xdWindow);
    }
    break;
  case E_CONTROL:
    /* 	User operated control in dialog.	*/
    {
      
      switch(xdControlId) {
      case DLG_135_PUSHBUTTON_4: /* "Neu" */
        {
          char profprojdatei2[100];
          
          anzahl_profil_dat_entries = 0;
          if (!xvt_dlg_create_res(WD_MODAL, DLG_128, EM_ALL, DLG_128_eh, 0L))
            xvt_dm_post_error("Can't open DLG_128");
          
          //neuer string steht in: "str_netz"
          
          // erneut, da nach dlg128 verloren
          //listbox135 = xvt_win_get_ctl (xdWindow,DLG_135_LBOX_5);
          
          if (is_128_ok)   // dlg128 wurde nicht abgebrochen
          {
            //		if (!xvt_dlg_create_res(WD_MODAL, DLG_OPEN_FILE, EM_ALL, DLG_OPEN_FILE_eh, 0L))
            //				 xvt_dm_post_error("Can't open dialog 150");
            
            xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,profprojdatei2,50);
            strcat(profprojdatei2,"\\profproj.txt");
            int back=access(profprojdatei2,00);
            if(back!=0)  //PROFPROJDATEI NOCH NICHT VORHANDEN
            {
              ok_file=IDOK;
              dateien[0]='\0';
            }
            else
            {
              if (!xvt_dlg_create_res(WD_MODAL, DLG_210, EM_ALL, DLG_210_eh, 0L))
                xvt_dm_post_error("Can't open DLG_210");
            }
            if (ok_file == IDOK)
            {
              if (prof_datei != NULL)
              {
                xvt_slist_destroy(prof_datei);
                prof_datei = NULL;
              }
              if ((prof_datei=xvt_slist_create())==NULL)
                xvt_dm_post_error("Error while creation of SLIST:prof_datei\\util.cpp");
              
              anzahl_profil_dat_entries= get_profildatei_names(dateien);     // in: util.cpp
              xvt_slist_add_at_elt(cfg_list,NULL,str_netz,anzahl_str_dat_entries);
              
              xvt_list_set_sel((xvt_win_get_ctl (xdWindow,DLG_135_LBOX_5)), 0, TRUE);
              
              is_profil_open = FALSE;
              new_profil = TRUE;
              choice_neu =TRUE;
              
              dlg135ref=FALSE;
              
            } //if (ok_file==IDOK)
            
            if(ok_file==IDCANCEL)
            { // IDCANCEL
              if (prof_datei != NULL)
              {
                memset(dateien,' ',350);
                xvt_slist_destroy(prof_datei);
                prof_datei = NULL;
                choice_neu = FALSE;
              }
            } //IDCANCEL
          } //if is_128_ok
          
          file_spec.name[0] ='\0';
          xvt_list_clear(xvt_win_get_ctl (xdWindow,DLG_135_LBOX_5));
          xvt_list_add((xvt_win_get_ctl (xdWindow,DLG_135_LBOX_5)), -1, (char *)cfg_list);   // Ausgabe in LISTBOX
          xvt_list_set_sel((xvt_win_get_ctl (xdWindow,DLG_135_LBOX_5)), anzahl_str_dat_entries-1, TRUE);
          
          destroy_strang_tabelle();
          MakeNewStrang(STRANGANZAHL);
          make_sort_strangtabelle();    //neue Strangtabelle anlegen-sortieren
          if(is_128_ok)
            save_str_datei();
          
          safe_cfg_dat();
          
          destroy_strang_tabelle();
          
          xvt_scr_set_focus_vobj((xvt_win_get_ctl(xdWindow,DLG_135_PUSHBUTTON_1)));
        }
        break;
      case DLG_135_PUSHBUTTON_3: /* "Vernetzungsdatei löschen" */
        {
          /**sicherheitshalber:*********************************************/
          int anzahl_cfg_slist=xvt_slist_count(cfg_list);
          if(anzahl_cfg_slist>0)
          {
            SaveNetzFile=TRUE;
            netzdatei=xvt_list_get_sel(xvt_win_get_ctl (xdWindow,DLG_135_LBOX_5));
            sel = xvt_list_get_sel_index(xvt_win_get_ctl (xdWindow,DLG_135_LBOX_5));
            str=xvt_slist_get_elt(netzdatei,0,0);
            cfg_list_index=xvt_list_get_sel_index(xvt_win_get_ctl (xdWindow,DLG_135_LBOX_5));
            strcpy(str_netz,str);       //str_netz enthält ausgewählte *.str-Datei
            
            for (int i=73;i<=84;i++)
              temp[i-73] = str[i];
            temp[12]='\0';
            strcpy(STR_SPEC.name,temp);
            
            /****************************************************/
            delete_str=TRUE;
            if (delete_str)
            {
              char buf[200],buf2[200],buf3[200],buf4[200];//Dick 26.11.99
              xvt_res_get_str(STR_NEIN,buf,sizeof(buf));
              xvt_res_get_str(STR_JA,buf2,sizeof(buf2));
              xvt_res_get_str(STR_DELETE_STR,buf3,sizeof(buf3));
              xvt_res_get_str(STR_LOESCHEN2,buf4,sizeof(buf3));
              switch (xvt_dm_post_ask(buf,buf2,NULL,"%s\n%s\n%s",buf3,STR_SPEC.name,buf4))
                //					 switch (xvt_dm_post_ask("Nein","Ja",NULL,"Wollen Sie wirklich die Vernetzungsdatei:\n%s\nlöschen ?",STR_SPEC.name))
              {
              case RESP_2:       //Löschen !!!
                {
                  // lösche *.str-Datei
                  ermittle_loesch_profile(); //in profpro2.cpp
                  int anzahl_slist=xvt_slist_count(profile_slist);
                  if(anzahl_slist>0)
                  {
                    //switch (xvt_dm_post_ask("Ja","Nein",NULL,
                    //"Sollen die Profildateien, die nur in dieser"
                    //" Zustandsdatei referenziert sind, "
                    //" auch physisch gelöscht werden?"))
                    xvt_res_get_str(STR_PROFDAT_HARD_DEL_ASK2,buf3,sizeof(buf3));
                    switch (xvt_dm_post_ask(buf2,buf,NULL,"%s",buf3))
                    {
                    case RESP_DEFAULT:       //JA - löschen
                      {
                        profile_loeschen=TRUE;
                        break;
                      }
                    case RESP_2:             // NEIN - nicht löschen
                      {
                        profile_loeschen=FALSE;
                        break;
                      }
                    } //switch
                  } //anzahl_slist>0
                  if(anzahl_slist==0)
                    profile_loeschen=FALSE;
                  
                  strcpy(strspecnamesave,STR_SPEC.name);
                  loesche_alle_mit_strname(); //in util2.cpp
                  loesche_alle_in_dath();//Dick 17.12.99
                  xvt_list_rem((xvt_win_get_ctl (xdWindow,DLG_135_LBOX_5)),sel);
                  
                  SLIST_ELT elt = xvt_slist_get_first(cfg_list);
                  for  (int i=0;i<sel;i++)
                    elt = xvt_slist_get_next(cfg_list,elt);
                  xvt_slist_rem(cfg_list,elt);
                  
                  anzahl_str_dat_entries--;
                  
                  xvt_res_get_str(STR_WSPD135_NOTE_1,buf3,sizeof(buf3));
                  xvt_dm_post_note("%s",buf3);
                  //xvt_dm_post_note("Zustandsdatei erfolgreich gelöscht!");
                  
                  safe_cfg_dat();
                  sortiere_profproj_neu(); //in profproj.cpp
                  
                  
                  int anzahl_cfg_list=xvt_slist_count(cfg_list);
                  if(anzahl_cfg_list>0)
                    xvt_list_set_sel((xvt_win_get_ctl (xdWindow,DLG_135_LBOX_5)), 0, TRUE);
                  
                  SaveStrangFile = TRUE;
                  delete_str = FALSE;
                  
                  dialog_ende =0;
                  if(profile_slist!=NULL)
                  {
                    xvt_slist_destroy(profile_slist);
                    profile_slist=NULL;
                  }
                }
                break;
              }  //switch
            } //end :if (delete_str)...
            
          } //if anzahl cfg_list>0
          else
          {
            char buf[200];
            xvt_res_get_str(STR_WSPD135_NOTE_2,buf,sizeof(buf));
            xvt_dm_post_note("%s",buf); 
            //xvt_dm_post_note("Es existieren keine Vernetzungsdateien");
          }
          xvt_scr_set_focus_vobj((xvt_win_get_ctl(xdWindow,DLG_135_PUSHBUTTON_1)));
          
      }
      break;
    case DLG_135_PUSHBUTTON_2: /* "Abbrechen" */
      {
        dialog_ende =0;
        dlg135ref=FALSE;
        xvt_slist_destroy(netzdatei);
        xvt_vobj_destroy(xdWindow);
      }
      break;
    case DLG_135_PUSHBUTTON_1: /* "OK" */
      {
        if (!delete_str)
        {
          if (xvt_list_count_all(xvt_win_get_ctl (xdWindow,DLG_135_LBOX_5))>0)
          {
            netzdatei=xvt_list_get_sel(xvt_win_get_ctl (xdWindow,DLG_135_LBOX_5));
            str=xvt_slist_get_elt(netzdatei,0,0);
            sscanf(str,"%s%s",Gewaessername_aktuell,Zustand_aktuell);
            for (int i=73;i<=84;i++)
              temp[i-73] = str[i];
            temp[12] = '\0';
            strcpy(STR_SPEC.name,temp);
            
            dlg135ref=TRUE;
            SaveNetzFile=TRUE;
            xvt_slist_destroy(netzdatei);
          }
          else
          {
            strcpy(Gewaessername_aktuell," ");
            strcpy(Zustand_aktuell," ");
          }
          safe_cfg_dat();
          
          //strcpy(Gewaessername_aktuell,temp);
          if(!Ber_edit_sperr)
            setTitle( Projektname_aktuell, Gewaessername_aktuell, Zustand_aktuell );

          dialog_ende =1;
          xvt_vobj_destroy(xdWindow);
        }
        else
        {
          dialog_ende =1;
          delete_str=FALSE;
          xvt_vobj_destroy(xdWindow);
        }
        
      }
      break;
    case DLG_135_LBOX_5: /* "List Box 4" */
      {
        /* List box was operated.	*/
        SaveNetzFile=TRUE;
        netzdatei=xvt_list_get_sel(xvt_win_get_ctl (xdWindow,DLG_135_LBOX_5));
        sel = xvt_list_get_sel_index(xvt_win_get_ctl (xdWindow,DLG_135_LBOX_5));
        str=xvt_slist_get_elt(netzdatei,0,0);
        cfg_list_index=xvt_list_get_sel_index(xvt_win_get_ctl (xdWindow,DLG_135_LBOX_5));
        strcpy(str_netz,str);       //str_netz enthält ausgewählte *.str-Datei
        
        
        for (int i=73;i<=84;i++)
          temp[i-73] = str[i];
        temp[12] = '\0';
        strcpy(STR_SPEC.name,temp);
        
        
        if (xdEvent->v.ctl.ci.v.lbox.dbl_click)
        { 	/*	double click*/
          dlg135ref=TRUE;
          SaveNetzFile=TRUE;
          xvt_slist_destroy(netzdatei);
          if( !Ber_edit_sperr )
          {
            sscanf( str_netz,"%s%s", Gewaessername_aktuell, Zustand_aktuell );

            setTitle( Projektname_aktuell, Gewaessername_aktuell, Zustand_aktuell );
          }
          safe_cfg_dat();
          dialog_ende=1;
          xvt_vobj_destroy(xdWindow);
        }
        else
        { 		/* 	single click	*/
        }
      }
      break;
    default:
      break;
    }
    }
    break;
  case E_TIMER:
    /*		Timer associated with window went off.	*/
    {
    }
    break;
  case E_USER:
    /*		Application initiated.	*/
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
