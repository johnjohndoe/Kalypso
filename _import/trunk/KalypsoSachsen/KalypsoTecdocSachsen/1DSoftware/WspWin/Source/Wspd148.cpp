#include <windows.h>
#include <shlwapi.h>
#include <direct.h>

#include "xvt.h"
#include "wspwin.h"
#include "resource.h"

#include "global_types.h"
#include "global_vars.h"
#include "..\..\wspdlg\include\export.h"

#include "list.h"
#include "typen.h"
#include "bce_allg.h"

#include "util.h"
#include "read_cfg.h"

#include "global.h"

#include <ERRNO.H>

#include "wsphilfe.h"

extern XVT_HELP_INFO hi;

#define DLG_RES_ID DLG_148
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODAL

WINDOW dlg148_lbox,
dlg148_id;
extern WINDOW main_win;
extern BOOLEAN proj_rem;
extern int proj_archiv;//Dick 17.12.98
int dlg148_auswahl;
int anzahl_vern_dateien=0;
char *path,save_path[MAX_PATH];
SLIST selection=NULL, probezlist=NULL;
extern BOOLEAN stopit;
CURSOR cursor148;

// vorwärtsdeklaration
int remove_projekt( char *path );



bool open_projekt_dlg148()
{
  char hilfsstring[200], str[200];
  
  selection = xvt_list_get_sel( dlg148_lbox );

  char* pstr = xvt_slist_get_elt( selection, 0, 0 );
  char* qstr = strchr( pstr,'\n' );
  if( qstr != NULL )
    qstr[0] = '\0';
  
  strncpy( hilfsstring, pstr, 200 );
  int len = strlen( hilfsstring );
  
  for( int i = 0; i <= len; i++ )
  {
    if( hilfsstring[i] != ' ' )
      str[i] = hilfsstring[i];
    else
    {
      str[i] = '\0';
      break;
    }
  }
  
  strcpy( Projektname_aktuell, str ); 
  strcat( str, "\\PROF" );
  
  if( !xvt_fsys_convert_str_to_dir( str,&STR_SPEC.dir ) )//Dick 7.10.98
  {
    char buf[200],buf2[200];
    xvt_res_get_str(STR_FILEDLG_NOTE_3,buf,sizeof(buf));
    xvt_res_get_str(STR_FILEDLG_NOTE_6,buf2,sizeof(buf2));
    xvt_dm_post_error("%s %s",buf,buf2);
    //xvt_dm_post_error("Projekt existiert nicht mehr!");
    
    return false;
  }
  
  xvt_fsys_set_dir( &STR_SPEC.dir );
  
  xvt_menu_set_item_enabled( Menu_Win, TASK_MENUBAR_50, TRUE ); // Menu: Zustandsdatei enabled setzen
  xvt_menu_update( Menu_Win );
  new_strang = TRUE;
  new_profil = TRUE;
  choice_neu = TRUE;

  setTitle( Projektname_aktuell, 0, 0 );
  
  dialog_ende = 1;

  return true;
};





/*	Handler for dialog DLG_148 ("PROJEKTAUSWAHL")  */
long XVT_CALLCONV1 DLG_148_eh XVT_CALLCONV2( WINDOW xdWindow, EVENT* xdEvent )
{
  short xdControlId = xdEvent->v.ctl.id;
  
  switch (xdEvent->type) 
  {
  case E_CREATE:
    {
      char buf[200];
      
      dlg148_id =xdWindow;
      dlg148_lbox = xvt_win_get_ctl( xdWindow,DLG_148_LBOX_1 );
      
      probezlist = xvt_slist_create();
      FillProbezList( &proj_list, &probezlist );
      
      NewSetFontFunc(dlg148_lbox);    //Schriftart für listbox86 ändern
      
      if(proj_archiv==2)//d.h zurückladen
        xvt_list_add(dlg148_lbox,-1,(char*)archiv_list);
      else
        xvt_list_add(dlg148_lbox,-1,(char*)probezlist);
      
      if( LWA_PROJEKT )
        xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_148_PUSHBUTTON_5)),TRUE);
      else
        xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_148_PUSHBUTTON_5)),FALSE);
      
      if( proj_rem )
      {  //Projekt löschen
        xvt_res_get_str(STR_PROJEKT_DEL_TITLE,buf,sizeof(buf)); // "Projekteintrag löschen..."
        xvt_vobj_set_title( xdWindow, buf );
        xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,DLG_148_PUSHBUTTON_5)),FALSE);
        xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,IDC_PROJEKT_NEU)),FALSE);
        xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,IDC_PROJEKT_DEL)),FALSE);
      }
      else if(proj_archiv>0)
      {
        if(proj_archiv == 2)
          xvt_res_get_str(STR_PROJEKT_RELOAD_TITLE,buf,sizeof(buf));
        else
          xvt_res_get_str(STR_PROJEKT_ARCH_TITLE,buf,sizeof(buf)); // "Projekt archivieren..."
        xvt_vobj_set_title( xdWindow, buf );
        xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,IDC_PROJEKT_NEU)),FALSE);
        xvt_vobj_set_visible((xvt_win_get_ctl(xdWindow,IDC_PROJEKT_DEL)),FALSE);
      };
      
      // stets den ersten Eintrag selektieren
      xvt_list_set_sel(dlg148_lbox,0,TRUE);
      dlg148_auswahl = 0;
      
      if((selection=xvt_slist_create())==NULL)
        xvt_dm_post_error("Cant_create_slist:selection -dlg148");
      
      if (hi!=NULL_HELP_INFO)
        xvt_help_set_win_assoc(hi, xdWindow, HID_KAPITEL_4_2_2, 0L);
    }
    break;
    
  case E_DESTROY:
    {         
      xvt_slist_destroy(selection);
      xvt_slist_destroy(probezlist);
      probezlist=NULL;
      dlg148_id=NULL;
    }
    break;
  case E_FOCUS:
    {
      if (xdEvent->v.active)
      {
      /*
      Dialog has gained focus
        */
      }
      else 
      {
      /*
      Dialog has lost focus
        */
      }
    }
    break;
  case E_SIZE:
    {
    }
    break;
  case E_CLOSE:
    {
      dialog_ende=0;
      xvt_vobj_destroy(xdWindow);
    }
    break;
  case E_CHAR:
    {
    }
    break;
  case E_CONTROL:
    {
      switch(xdControlId) 
      {
      case DLG_148_LBOX_1: /* "List Box 1" */
        { 	/*			List box was operated.			*/
          dlg148_auswahl = xvt_list_get_sel_index( dlg148_lbox );

          if( xdEvent->v.ctl.ci.v.lbox.dbl_click)
          {  /*		double click			*/
            if( !proj_rem && proj_archiv == 0 )  // Projekt nicht löschen und nicht archivieren
            {
              if( open_projekt_dlg148() )
                xvt_vobj_destroy( xdWindow );
            }
            dialog_ende = 1;
          }
        }
        break;
        
      case DLG_148_PUSHBUTTON_3: /* "OK" */
        {
          char buf[200],buf2[200],buf3[200],buf4[200], hilfsstring[300], str[200];//Dick 26.11.99
          if (proj_rem)  // Projekteintrag löschen
          {
            SLIST_ELT elt, elt2;
            int geloescht=0;
            
            elt = xvt_slist_get_first(probezlist);
            elt2=xvt_slist_get_first(proj_list);
            
            for  (int i=0;i<dlg148_auswahl;i++)
              elt = xvt_slist_get_next(probezlist,elt);
            
            for  ( i=0;i<dlg148_auswahl;i++)
              elt2 = xvt_slist_get_next(proj_list,elt2);
            
            path=xvt_slist_get(probezlist,elt,0);
            
            strcpy(hilfsstring, path);
            int len=strlen(hilfsstring);
            //						   int i=0;
            for(i=0; i<=len; i++)
            {
              if(hilfsstring[i]!=' ')
                str[i]=hilfsstring[i];
              else
              {
                str[i]='\0';
                break;
              }
            }
            
            if (path !=NULL)
            {
              if(!strcmp(str,Projektname_aktuell))
              {
                xvt_res_get_str(STR_WSPD148_NOTE_1,buf,sizeof(buf));
                xvt_dm_post_note("%s",buf); //"Der Projekteintrag zum aktuellen Projekt darf nicht gelöscht werden!"
                break;
              }
              
              xvt_res_get_str(STR_NEIN,buf,sizeof(buf));
              xvt_res_get_str(STR_JA,buf2,sizeof(buf2));
              xvt_res_get_str(STR_WSPD148_ASK_1,buf3,sizeof(buf3));
              
              switch( xvt_dm_post_ask( buf, buf2, NULL, "%s", buf3 ) ) // "Nein","Ja",NULL,"Wollen Sie wirklich der Projekteintrag löschen?"
              {
              case RESP_2:
                {
                  if( (!xvt_slist_rem(proj_list,elt2)) ||(!xvt_slist_rem(probezlist, elt)))
                  {
                    xvt_res_get_str(STR_WSPD148_NOTE_2,buf,sizeof(buf));
                    xvt_dm_post_error("%s",buf); //"Projekteintrag kann nicht gelöscht werden"
                  }
                  xvt_list_rem(dlg148_lbox,dlg148_auswahl);
                  geloescht=1;
                }
                break;
              }
            }		
            
            dialog_ende=0;
            save_projekt();
          }
          else 
            if(proj_archiv>0)
            {
              SLIST_ELT elt;
              char szDirName[256];
              //Neu Dick 26.01.99
              int f;
              char temp_path[256],temp_path2[256],temp_path3[256],temp_path4[256],*p;
              FILE_SPEC ff;
              
              
              if(proj_archiv==1)//d.h. archivieren
              {
                elt = xvt_slist_get_first(proj_list);
                for  (int i=0;i<dlg148_auswahl;i++)
                  elt = xvt_slist_get_next(proj_list,elt);
                path=xvt_slist_get(proj_list,elt,0);
                
                strcpy(hilfsstring, path);
                int len=strlen(hilfsstring);
                int j=0;
                for(j=0; j<=len; j++)
                {
                  if(hilfsstring[j]!=' ')
                    save_path[j]=hilfsstring[j];
                  else
                  {
                    save_path[j]='\0';
                    break;
                  }
                }
                
                //    strcpy(save_path,path);
                xvt_res_get_str(STR_NEIN,buf,sizeof(buf));
                xvt_res_get_str(STR_JA,buf2,sizeof(buf2));
                xvt_res_get_str(STR_WSPD148_ASK_2,buf3,sizeof(buf3));
                xvt_res_get_str(STR_ARCHIV,buf4,sizeof(buf3));
                if (save_path !=NULL)
                  switch (xvt_dm_post_ask(buf,buf2,NULL,"%s\n%s\n%s ?",buf3,save_path,buf4))
                  //switch (xvt_dm_post_ask("Nein","Ja",NULL,"Wollen Sie wirklich das Projekt:\n%s\narchivieren ?",save_path))
                {
                           case RESP_2:       //archivieren
                             {
                               
                               xvt_res_get_str(STR_WSPD148_NOTE_3,buf,sizeof(buf));
                               xvt_res_get_str(STR_WSPD148_NOTE_4,buf2,sizeof(buf2));
                               xvt_dm_post_note("%s %s %s",buf,save_path,buf2);                                    
                               //xvt_dm_post_note("Geben Sie bitte das Verzeichnis ein,wo das Projekt %s archiviert werden soll.",save_path);
                               if(Verzeichnis_waehlen((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW),NULL,NULL,save_path,szDirName))
                               {
                                 if(strlen(szDirName)<4)
                                 {
                                   xvt_res_get_str(STR_WSPD148_NOTE_5,buf,sizeof(buf));
                                   xvt_dm_post_error("%s %s",szDirName,buf);
                                   //xvt_dm_post_error("%s ist kein Verzeichnis",szDirName);
                                   break;
                                 }
                                 strcpy(temp_path,save_path);
                                 strcat(temp_path,"\\dath");
                                 strcpy(temp_path2,save_path);
                                 strcat(temp_path2,"\\prof");
                                 strcpy(temp_path3,szDirName);
                                 strcat(temp_path3,"\\dath");
                                 strcpy(temp_path4,szDirName);
                                 strcat(temp_path4,"\\prof");
                                 if(_access( temp_path3,00)!=-1 || _access( temp_path4,00)!=-1)
                                 {
                                   xvt_res_get_str(STR_WSPD148_NOTE_6,buf,sizeof(buf));
                                   xvt_res_get_str(STR_WSPD148_NOTE_7,buf2,sizeof(buf2));
                                   xvt_dm_post_error("%s %s %s ",buf,szDirName,buf2);
                                   //xvt_dm_post_error("Im Verzeichnis %s existieren schon \\prof und \\dath!\nArchivieren abgebrochen! ",szDirName);
                                   break;
                                 }
                                 cursor148 = xvt_win_get_cursor(main_win);
                                 xvt_win_set_cursor(main_win,CURSOR_WAIT);
                                 if(!CopyDir(temp_path,temp_path3)&&!CopyDir(temp_path2,temp_path4))                                          
                                 {
                                   xvt_win_set_cursor(main_win,cursor148); 
                                   if(!xvt_slist_rem(proj_list,elt))
                                   {
                                     xvt_res_get_str(STR_WSPD148_NOTE_8,buf,sizeof(buf));
                                     xvt_dm_post_error("%s",buf);
                                     //xvt_dm_post_error("Projekteintrag kann nicht archiviert werden");
                                   }
                                   else
                                   {
                                     xvt_slist_add_at_pos(archiv_list,0,szDirName,0);
                                     xvt_list_rem(dlg148_lbox,dlg148_auswahl);
                                     save_projekt();
                                     xvt_res_get_str(STR_WSPD148_NOTE_9,buf,sizeof(buf));
                                     xvt_dm_post_error("%s",buf);
                                     //xvt_dm_post_note("Projekt erfolgreich archiviert !");                                     
                                   }
                                   
                                   strcpy(temp_path,save_path);
                                   strcat(temp_path,"\\dath");
                                   f=remove_projekt(temp_path);
                                   if(f==0)
                                   {
                                     strcpy(temp_path,save_path);
                                     strcat(temp_path,"\\prof");
                                     f=remove_projekt(temp_path);
                                   }
                                   if(f==0)
                                   {
                                     strcpy(temp_path,save_path);
                                     p = strrchr(temp_path, '\\' );
                                     p[0]='\0';
                                     if(strlen(temp_path)<3)// wenn temp=="C:"<-keine verzeichnis
                                       strcat(temp_path,"\\");
                                     xvt_fsys_convert_str_to_dir(temp_path,&ff.dir);
                                     xvt_fsys_set_dir(&ff.dir);   //set current directory
                                     _rmdir(save_path);
                                   }
                                   //ende
                                   if(f!=0)
                                   {
                                     xvt_res_get_str(STR_WSPD148_NOTE_10,buf,sizeof(buf));
                                     xvt_res_get_str(STR_WSPD148_NOTE_11,buf2,sizeof(buf2));
                                     xvt_dm_post_error("%s\n %s!\n%s ",buf,save_path,buf2);
                                     //xvt_dm_post_note("Es gab Probleme beim Löschen des Verzeichnises  %s! Die Reste müssen daher per Hand gelöscht werden!",save_path);
                                   }
                                 }
                                 else
                                 {
                                   xvt_win_set_cursor(main_win,cursor148);
                                   xvt_res_get_str(STR_WSPD148_NOTE_12,buf,sizeof(buf));
                                   xvt_dm_post_error("%s",buf);
                                   //xvt_dm_post_note("Es sind Fehler beim Archivieren aufgetreten!\nAktion abgebrochen!");                                           
                                 }
                               }
                             }
                             break;
                }  //switch
                     }
                     else if(proj_archiv==2)//d.h. zurückladen
                     {
                       elt = xvt_slist_get_first(archiv_list);
                       for  (int i=0;i<dlg148_auswahl;i++)
                         elt = xvt_slist_get_next(archiv_list,elt);
                       path=xvt_slist_get(archiv_list,elt,0);
                       if(path==NULL) break;
                       
                       strcpy(hilfsstring, path);
                       int len=strlen(hilfsstring);
                       int j=0;
                       for(j=0; j<=len; j++)
                       {
                         if(hilfsstring[j]!=' ')
                           save_path[j]=hilfsstring[j];
                         else
                         {
                           save_path[j]='\0';
                           break;
                         }
                       }
                       
                       
                       
                       
                       //      strcpy(save_path,path);
                       xvt_res_get_str(STR_NEIN,buf,sizeof(buf));
                       xvt_res_get_str(STR_JA,buf2,sizeof(buf2));
                       xvt_res_get_str(STR_WSPD148_ASK_2,buf3,sizeof(buf3));
                       xvt_res_get_str(STR_ZUERUECKLADEN,buf4,sizeof(buf3));
                       if (save_path !=NULL)
                         switch (xvt_dm_post_ask(buf,buf2,NULL,"%s\n%s\n%s ?",buf3,save_path,buf4))
                         //switch (xvt_dm_post_ask("Nein","Ja",NULL,"Wollen Sie wirklich das Projekt:\n%s\nzurückladen ?",save_path))
                       {
                           case RESP_2:       //zurückladen
                             {
                               
                               xvt_res_get_str(STR_WSPD148_NOTE_3,buf,sizeof(buf));
                               xvt_res_get_str(STR_WSPD148_NOTE_13,buf2,sizeof(buf2));
                               xvt_dm_post_note("%s %s %s",buf,save_path,buf2);
                               //xvt_dm_post_note("Geben Sie  bitte das Verzeichnis ein,wo das Projekt %s zurückgeladen werden soll.",save_path);
                               if(Verzeichnis_waehlen((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW),NULL,NULL,save_path,szDirName))
                               {
                                 if(strlen(szDirName)<4)
                                 {
                                   xvt_res_get_str(STR_WSPD148_NOTE_5,buf,sizeof(buf));
                                   xvt_dm_post_error("%s %s",szDirName,buf);                                            
                                   //xvt_dm_post_error("%s ist kein Verzeichnis",szDirName);
                                   break;
                                 }
                                 strcpy(temp_path,save_path);
                                 strcat(temp_path,"\\dath");
                                 strcpy(temp_path2,save_path);
                                 strcat(temp_path2,"\\prof");
                                 strcpy(temp_path3,szDirName);
                                 strcat(temp_path3,"\\dath");
                                 strcpy(temp_path4,szDirName);
                                 strcat(temp_path4,"\\prof");
                                 if(_access( temp_path3,00)!=-1 || _access( temp_path4,00)!=-1)
                                 {
                                   xvt_res_get_str(STR_WSPD148_NOTE_6,buf,sizeof(buf));
                                   xvt_res_get_str(STR_WSPD148_NOTE_14,buf2,sizeof(buf2));
                                   xvt_dm_post_error("%s %s %s ",buf,szDirName,buf2);                                            
                                   //xvt_dm_post_error("Im Verzeichnis %s existieren schon \\prof und \\dath!\nZurückladen abgebrochen! ",szDirName);
                                   break;
                                 }
                                 cursor148 = xvt_win_get_cursor(main_win);
                                 xvt_win_set_cursor(main_win,CURSOR_WAIT);
                                 if(!CopyDir(temp_path,temp_path3)&&!CopyDir(temp_path2,temp_path4))
                                 {
                                   xvt_win_set_cursor(main_win,cursor148);
                                   if(!xvt_slist_rem(archiv_list,elt))
                                   {
                                     xvt_res_get_str(STR_WSPD148_NOTE_15,buf,sizeof(buf));
                                     xvt_dm_post_error("%s",buf);                                                
                                     //xvt_dm_post_error("Projekteintrag kann nicht zurückgeladen werden");
                                   }
                                   else
                                   {
                                     xvt_slist_add_at_pos(proj_list,0,szDirName,0);
                                     xvt_list_rem(dlg148_lbox,dlg148_auswahl);
                                     save_projekt();
                                     xvt_res_get_str(STR_WSPD148_NOTE_16,buf,sizeof(buf));
                                     xvt_dm_post_note("%s",buf);                                                 
                                     //xvt_dm_post_note("Projekt erfolgreich zurückgeladen !");                                     
                                   }
                                   strcpy(temp_path,save_path);
                                   strcat(temp_path,"\\dath");
                                   f=remove_projekt(temp_path);
                                   if(f==0)
                                   {
                                     strcpy(temp_path,save_path);
                                     strcat(temp_path,"\\prof");
                                     f=remove_projekt(temp_path);
                                   }
                                   if(f==0)
                                   {
                                     strcpy(temp_path,save_path);
                                     p = strrchr(temp_path, '\\' );
                                     p[0]='\0';
                                     if(strlen(temp_path)<3)// wenn temp=="C:"<-keine verzeichnis
                                       strcat(temp_path,"\\");
                                     xvt_fsys_convert_str_to_dir(temp_path,&ff.dir);
                                     xvt_fsys_set_dir(&ff.dir);   //set current directory
                                     _rmdir(save_path);
                                   }
                                   //ende
                                   
                                   if(f!=0)
                                   {
                                     xvt_res_get_str(STR_WSPD148_NOTE_10,buf,sizeof(buf));
                                     xvt_res_get_str(STR_WSPD148_NOTE_11,buf2,sizeof(buf2));
                                     xvt_dm_post_note("%s\n %s!\n%s ",buf,save_path,buf2);                                                
                                     //xvt_dm_post_note("Es gab Probleme beim Löschen des Verzeichnises  %s! Die Reste müssen daher per Hand gelöscht werden!",save_path);
                                   }
                                 }
                                 else
                                 {
                                   xvt_win_set_cursor(main_win,cursor148); 
                                   xvt_win_set_cursor(main_win,cursor148);
                                   xvt_res_get_str(STR_WSPD148_NOTE_17,buf,sizeof(buf));
                                   xvt_dm_post_error("%s",buf);
                                   //xvt_dm_post_note("Es sind Fehler beim Zurückladen aufgetreten!\nAktion abgebrochen!");
                                 }
                               }
                             }
                             break;
                       }  //switch
                       
                     }
                }
                else  // Projekt auswählen*/
                {
                  if( xvt_list_count_all( dlg148_lbox ) > 0 )
                  {
                    if( open_projekt_dlg148() )
                      xvt_vobj_destroy( xdWindow );
                    else
                      break;
                  }
                  else //keine Projekte / Elemente da
                  {
                    xvt_res_get_str(STR_WSPD148_NOTE_18,buf,sizeof(buf));
                    xvt_dm_post_error("%s",buf);
                    //xvt_dm_post_note("Kein Projekt vorhanden.\nBitte neu anlegen!");
                    dialog_ende=0;
                  }
                }
                xvt_vobj_destroy(xdWindow);
      }
      break;
    case DLG_148_PUSHBUTTON_4: /* "Abbrechen" */
      {
        dialog_ende=0;
        xvt_vobj_destroy(xdWindow);
      }
      break;
    case DLG_148_PUSHBUTTON_5: /* "Projektbezeichung nur bei LWA" */
      {
        //Dick 31.05.99
        bool zurueksetzen=FALSE;
        int sel_index;
        char *projektbezeichnung, *text_zeile;
		      projektbezeichnung=new char [300];
          text_zeile=new char [200];
          FILE *probezfile;
          
          
          if(strlen(STR_SPEC.dir.path)==0)
            zurueksetzen=TRUE;
          //
          if (xvt_list_count_all(dlg148_lbox)>0)
          {
            char *pstr,*qstr,str[200], hilfsstring[200];
            selection = xvt_list_get_sel(dlg148_lbox);
            sel_index=xvt_list_get_sel_index(dlg148_lbox);
            
            pstr=xvt_slist_get_elt(selection,0,0);
            if(pstr!=NULL)
            {
              qstr=strchr(pstr,'\n');
              if (qstr != NULL) 
                qstr[0]='\0';
              
              //16.01.01 Bley
              strcpy(hilfsstring, pstr);
              int len=strlen(hilfsstring);
              int i=0;
              for(i=0; i<=len; i++)
              {
                if(hilfsstring[i]!=' ')
                  str[i]=hilfsstring[i];
                else
                {
                  str[i]='\0';
                  break;
                }
              }
              strcpy(hilfsstring,str); //fuer weiter unter projbez dranhaengen
              strcat(str,"\\PROF");
              
              xvt_fsys_convert_str_to_dir(str,&STR_SPEC.dir);
              xvt_fsys_set_dir(&STR_SPEC.dir);
              stopit=FALSE;
              if (!xvt_dlg_create_res(WD_MODAL, DLG_222, EM_ALL, DLG_222_eh, 0L))
                xvt_dm_post_error("Can't open dialog222");
              if(!stopit)
              {
                strcpy(projektbezeichnung,str);
                strcat(projektbezeichnung,"\\probez.txt");
                if((probezfile=fopen(projektbezeichnung,"r"))!=NULL)
                {
                  projektbezeichnung[0]='\0';
                  strcpy(projektbezeichnung,hilfsstring);
                  text_zeile[0]='\0';
                  fgets(text_zeile,100,probezfile);
                  int len=strlen(projektbezeichnung);
                  for(int i=len; i<=80; i++)
                    strcat(projektbezeichnung," ");
                  strcat(projektbezeichnung, " ");
                  strcat(projektbezeichnung,text_zeile);
                  xvt_slist_change_str(probezlist,projektbezeichnung,sel_index);
                  xvt_list_clear(dlg148_lbox);
                  xvt_list_add(dlg148_lbox,-1,(char*)probezlist);
                  xvt_list_set_sel(dlg148_lbox,sel_index,TRUE);
                  fclose(probezfile);
                }
              } //!stopit
            }  //if pstr!=NULL
            else
            {
              char buf[200];//Dick 12.01.00
              xvt_res_get_str(STR_WSPD148_NOTE_19,buf,sizeof(buf));
              xvt_dm_post_error("%s",buf);
              //xvt_dm_post_note("Bitte wählen Sie ein Projekt aus !");
            }
            if(zurueksetzen)//Dick 31.05.99
              STR_SPEC.dir.path[0]=0;
            
          }
          
          delete [] text_zeile;
          delete[] projektbezeichnung;
      }
      break;
    case IDC_PROJEKT_NEU:
      {
        xvt_list_suspend(dlg148_lbox);
        xvt_list_clear(dlg148_lbox);
        if (!xvt_dlg_create_res(WD_MODAL, DLG_147, EM_ALL, DLG_147_eh, 0L))
          xvt_dm_post_error("Can't open dialog");
        xvt_list_add(dlg148_lbox,-1,(char*)probezlist);
        xvt_list_resume(dlg148_lbox);
        xvt_list_set_sel(dlg148_lbox,0,TRUE);//Vorselektieren
        
      }
      break;
    case IDC_PROJEKT_DEL:
      {
        int geloescht=0;
        SLIST_ELT elt, elt2;
        char *pstr, hilfsstring[200], str[300];;
        
        elt = xvt_slist_get_first(probezlist);
        elt2=xvt_slist_get_first(proj_list);
        for  (int i=0;i<dlg148_auswahl;i++)
          elt = xvt_slist_get_next(probezlist,elt);
        for  (i=0;i<dlg148_auswahl;i++)
          elt2 = xvt_slist_get_next(proj_list,elt2);
        
        pstr=xvt_slist_get(probezlist,elt,0);
        path=xvt_slist_get(proj_list,elt2,0);
        
        strcpy(hilfsstring, pstr);
        int len=strlen(hilfsstring);
        int j=0;
        for(j=0; j<=len; j++)
        {
          if(hilfsstring[j]!=' ')
            str[j]=hilfsstring[j];
          else
          {
            str[j]='\0';
            break;
          }
        }
        
        if (pstr !=NULL)
        {
          
          if(!strcmp(str,Projektname_aktuell)) //Dick 12.02.99
          {
            char buf[200];//Dick 12.01.00
            xvt_res_get_str(STR_WSPD148_NOTE_20,buf,sizeof(buf));
            xvt_dm_post_error("%s",buf);
            //xvt_dm_post_note("Aktuelles Projekt darf nicht gelöscht werden!");
            break;
          }
          char buf[200],buf2[200],buf3[200],buf4[200];//Dick 26.11.99
          xvt_res_get_str(STR_NEIN,buf,sizeof(buf));
          xvt_res_get_str(STR_JA,buf2,sizeof(buf2));
          xvt_res_get_str(STR_PROJ_DEL_ASK,buf3,sizeof(buf3));
          xvt_res_get_str(STR_MOVE_TO_DUSTBIN,buf4,sizeof(buf3));
          switch (xvt_dm_post_ask(buf,buf2,NULL,"%s\n%s\n%s",buf3,str,buf4))
            //switch (xvt_dm_post_ask("Nein","Ja",NULL,"Wollen Sie wirklich das gesamte Projekt:\n%s\nlöschen ?",path))
          {
          case RESP_2:       //Löschen !!!
            {
              // jetzt die Dateien in den Papierkorb löschen ->Pfad in str
              LPSTR files = (LPSTR)malloc( strlen( str ) + 2 );
              strcpy( files, str );
              files[ strlen( str ) + 1 ] = '\0'; // mit einer doppel 0 abschliessen
              
              SHFILEOPSTRUCT foStruct;
              foStruct.hwnd = NULL;
              foStruct.wFunc = FO_DELETE;
              foStruct.pFrom = files;
              foStruct.pTo = NULL;
              foStruct.fFlags = FOF_ALLOWUNDO | FOF_NOCONFIRMATION;
              foStruct.lpszProgressTitle = NULL;
              
              int bResult = SHFileOperation( &foStruct );
              DWORD error = GetLastError();
              
              free( files );              
              
              if ( bResult == 0 )  // OK
              {
                if( ( !xvt_slist_rem( probezlist, elt ) ) || ( !xvt_slist_rem(proj_list, elt2 ) ) )
                { 
                  xvt_res_get_str( STR_WSPD148_NOTE_2, buf, sizeof(buf) );
                  xvt_dm_post_error("%s",buf); // "Projekteintrag kann nicht gelöscht werden"
                }
                else
                {
                  xvt_list_rem( dlg148_lbox, dlg148_auswahl );
                  xvt_res_get_str( STR_WSPD148_DELETE_SUCCES, buf, sizeof( buf ) );
                  xvt_dm_post_note( "%s", buf ); // "Projekt erfolgreich gelöscht !"
                  geloescht = 1;
                }
              }
              else
              {
                xvt_res_get_str( STR_NEIN, buf, sizeof(buf) );
                xvt_res_get_str(STR_JA,buf2,sizeof(buf2));
                xvt_res_get_str(STR_WSPD148_ASK_3,buf3,sizeof(buf3));
                
                switch (xvt_dm_post_ask(buf,buf2,NULL,"%s",buf3)) // "Nein","Ja",NULL,"Fehler beim löschen!\nProjekteintrag dennoch löschen?"
                {
                case RESP_2:
                  {
                    if((!xvt_slist_rem(probezlist,elt)) || (!xvt_slist_rem(proj_list,elt2)))
                    {
                      xvt_res_get_str(STR_WSPD148_NOTE_2,buf,sizeof(buf));
                      xvt_dm_post_error("%s",buf); // "Projekteintrag kann nicht gelöscht werden"
                    }
                    xvt_list_rem(dlg148_lbox,dlg148_auswahl);
                    geloescht=1;
                  }
                  break;
                }
              }
            }
            break;
          }  //switch
        } //if
        if(geloescht && !strcmp(str,Projektname_aktuell))
          setTitle( 0, 0, 0 );

        disable_menu(Menu_Win);
        dialog_ende=0;
        save_projekt();
        xvt_list_set_sel(dlg148_lbox,0,TRUE);
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

/***********************************************************
Rekursives löschen aller Dateien und Unterverzeichnisse
***********************************************************/
int remove_projekt(char *path)  // <==> deltree
{
  int fehler;
  SLIST filelist;
  SLIST_ELT e;
  char temp[250],*file,*p;
  FILE_SPEC f;
  
  /*  alle Dateien im Verzeichnis "path" und Unterverzeichnisse löschen  */
  strcpy(temp,path);
  if (temp[0]!='.')
  {
    xvt_fsys_convert_str_to_dir(temp,&f.dir);
    if((xvt_fsys_set_dir(&f.dir)))   //set current directory
    {
      if((filelist = xvt_fsys_list_files("","",FALSE))!=NULL)
      {
        for (e=xvt_slist_get_first(filelist);e!=NULL;
        e=xvt_slist_get_next(filelist,e))
        {
          file=xvt_slist_get(filelist,e,0L);
          strcpy(f.name,file);
          fehler = xvt_fsys_rem_file(&f);
        }
      }
      // Verzeichnis: path enthält nur noch directorys
      if((filelist = xvt_fsys_list_files("","",TRUE))!=NULL)
      {
        for (e=xvt_slist_get_first(filelist);e!=NULL;
        e=xvt_slist_get_next(filelist,e))
        {
          file=xvt_slist_get(filelist,e,0L);  // filelist enthält nur directory-namen
          if (file[0]!='.')
          {
            strcpy(temp,path);
            strcat(temp,"\\");
            strcat(temp,file);
            
            remove_projekt(temp);
          }
        }
      }
      strcpy(temp,path);
      p = strrchr(temp, '\\' );
      p[0]='\0';
      if(strlen(temp)<3)//Dick 13.01.99 wenn temp=="C:"<-keine verzeichnis
        strcat(temp,"\\");
      xvt_fsys_convert_str_to_dir(temp,&f.dir);
      xvt_fsys_set_dir(&f.dir);   //set current directory
      
      fehler= _rmdir(path);
      if (fehler ==-1)
      {
        if (errno==EACCES)
        {
          char buf[200],buf2[200];
          xvt_res_get_str(STR_FILEDLG_NOTE_1,buf,sizeof(buf));
          xvt_res_get_str(STR_FILEDLG_NOTE_2,buf2,sizeof(buf2));
          xvt_dm_post_error("%s%s%s",buf,temp,buf2);           
          //xvt_dm_post_error("Zugriff auf das Projekt:\n%s\nnicht gestattet!",temp);
        }
        if (errno==ENOENT)
        {
          char buf[200],buf2[200];
          xvt_res_get_str(STR_FILEDLG_NOTE_3,buf,sizeof(buf));
          xvt_res_get_str(STR_FILEDLG_NOTE_4,buf2,sizeof(buf2));
          xvt_dm_post_error("%s\n%s\n%s",buf,temp,buf2);           
          
          //xvt_dm_post_error("Projekt\n%s\nnicht gefunden!",temp);
        }
      }
    }
    else
    {
      strcpy(temp,path);
      p = strrchr(temp, '\\' );
      p[0]='\0'; 
      fehler =-1;
      char buf[200],buf2[200];
      xvt_res_get_str(STR_FILEDLG_NOTE_5,buf,sizeof(buf));
      xvt_res_get_str(STR_FILEDLG_NOTE_6,buf2,sizeof(buf2));
      xvt_dm_post_error("%s%s\n%s",buf,temp,buf2); 
      //xvt_dm_post_error("Gewähltes Projekt:\n%s\nexistiert nicht mehr !",temp);
    }
  }
  return fehler;
}