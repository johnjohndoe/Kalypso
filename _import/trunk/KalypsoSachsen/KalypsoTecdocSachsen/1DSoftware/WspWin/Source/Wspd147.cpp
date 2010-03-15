/*****************************************/
/* WspD147.cpp: Neues Projekt erstellen  */
/*****************************************/

#include <windows.h>
#include "xvt.h"
#include <direct.h>

#include "wspwin.h"
#include "wsphilfe.h"
#include "resource.h"

#include "global_vars.h"

#include "..\..\wspdlg\Include\export.h"

#include "bce_allg.h"
#include "read_cfg.h"
#include "util.h"


// globale Variablen

WINDOW edit_feld_id, static_feld_id, dlg147 = NULL;
char pfadname[200], fname[200];
int dir_status;
FILE* PROJ;

// externe Variablen 

extern int  dlg147def;//Definition für dialog 147
extern SLIST probezlist;
extern WINDOW main_win;

extern  XVT_HELP_INFO hi;

// Vorwärtsdeklaration

int verzeichnis_anlegen( char* );


/* 	Handler for dialog DLG_147 (" Neues Projekt erstellen") */
long XVT_CALLCONV1
#if XVT_CC_PROTO
DLG_147_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
#else
DLG_147_eh XVT_CALLCONV2 (xdWindow, xdEvent)
WINDOW xdWindow;
EVENT *xdEvent;
#endif
{
  short xdControlId = xdEvent->v.ctl.id;
  
  switch (xdEvent->type)
  {
  case E_CREATE:
    {
      dlg147=xdWindow;
      char initdir[256];
      edit_feld_id   = xvt_win_get_ctl (xdWindow,DLG_147_EDIT);
      static_feld_id = xvt_win_get_ctl (xdWindow,DLG_147_TEXT_2);
      xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,pfadname,140);
      xvt_vobj_set_title(static_feld_id, pfadname);
      
      if(!dlg147def)
      {
        GetPrivateProfileString("WSPWIN","HAUPTPATH","C:\\PROJEKT",initdir,256,"WSPWIN.INI");//Dick 6.01.99
        xvt_vobj_set_title(edit_feld_id,initdir);//Dick 6.01.99
        xvt_ctl_set_text_sel(edit_feld_id,SHRT_MAX,SHRT_MAX);
      }
      else
        xvt_win_set_doc_title(xdWindow,"Projekt speichern unter...");
      if (hi!=NULL_HELP_INFO)
        xvt_help_set_win_assoc(hi, xdWindow, HID_KAPITEL_6_1_1, 0L);
    }
    break;
  case E_DESTROY:
		/*
    Dialog has been closed; last event sent to dialog.
    */
    {
      dlg147=NULL;
    }
    break;
  case E_FOCUS:
    {
      /*			Dialog has lost or gained focus.		*/
      if (xdEvent->v.active)  {
        /*		Dialog has gained focus			*/
      } else {
        /*		Dialog has lost focus			*/
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
      
      switch(xdControlId) {
      case DLG_147_EDIT:			{
        /*		Edit control was operated.			*/
        if (xdEvent->v.ctl.ci.v.edit.focus_change) {
          if (xdEvent->v.ctl.ci.v.edit.active) {
            /*			focus has entered the control			*/
          } else {
            /*			focus has left the control				*/
          }
        } else {
          /*				Contents of control were changed		*/
        }
                              }
        break;
      case DLG_147_PUSHBUTTON_1: /* "OK" */
        {
          xvt_vobj_get_title(edit_feld_id,pfadname,149);
          if(!dlg147def)
          {
            if (check_string(pfadname)!=1)  //Fehler:Umlaut o.ä.
            {
              char buf[200];//dick 12.01.00
              xvt_res_get_str(STR_WSPD147_NOTE_1,buf,sizeof(buf));
              xvt_dm_post_note("%s",buf);
              //xvt_dm_post_note("Bitte keine Umlaute, Leerzeichen eingeben !");
            }
            else
            {
              char *p1,*p2,path[150];
              //Pfadname < 8 Buchstaben
              p1 = strrchr( pfadname,'\\');
              if (p1 ==NULL)
              {
                xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,path,140);
                strcat(path,"\\");
                strcat(path,pfadname);
                strcpy(pfadname,path);
              }
              else
              {
                p1++;
                strcpy(path,p1);
                p2 = strrchr(path,'.');
                if (p2)
                {	p2[0]='\0';
                p2++;    }

                if (p2)     //extension: .xyz
                {
                  if(strlen(p2)>3)
                    p2[3]='\0';
                  strcat(path,".");
                  strcat(path,p2);
                }
                p1[0]='\0';

                strcat(pfadname,path);
              }
              
              // Prüfen ob Pfad bereits existiert
              struct _finddata_t  data;
              long sHandle,sHandleTemp;
              BOOLEAN err_projekt_ist_da=FALSE;
              char pfadnametemp[256];
              strcpy(pfadnametemp,pfadname);
              strcat(pfadnametemp,"\\PROF");
              sHandleTemp=_findfirst(pfadname,&data);//Dick 6.01.99 für Verzeichnis
              sHandle=_findfirst(pfadnametemp,&data);//Dick 6.01.99 für Projekt
              
              if ((sHandle!=-1)&&(data.attrib==_A_SUBDIR))
              {
                char buf[200];//dick 12.01.00
                xvt_res_get_str(STR_WSPD147_NOTE_2,buf,sizeof(buf));
                xvt_dm_post_note("%s",buf);
                //xvt_dm_post_note("Projektname wurde bereits vergeben\nBitte andere Wahl");
                xvt_vobj_set_title(edit_feld_id,"\0");
                xvt_ctl_set_text_sel(edit_feld_id,SHRT_MAX,SHRT_MAX);
                err_projekt_ist_da=TRUE;
              }
              
              else  //Pfad neu anlegen
              {
                if(sHandleTemp==-1)
                  dir_status=verzeichnis_anlegen(pfadname); //Dick 12.08.98 Siehe Funktion unter
                else
                  dir_status=0;
                if (dir_status!=0)
                {
                  char buf[200];//dick 12.01.00
                  xvt_res_get_str(STR_WSPD147_NOTE_3,buf,sizeof(buf));
                  xvt_dm_post_note("%s",buf);
                  //xvt_dm_post_error("Projekt kann nicht angelegt werden");
                }
                else
                { // Verzeichnis wurde erstellt
                  strcpy(fname,pfadname);
                  strcat(fname,"\\PROF");
                  dir_status = _mkdir(fname);
                  
                  strcpy(fname,pfadname);
                  strcat(fname,"\\DATH");
                  dir_status = _mkdir(fname);
                  
                  strcpy(fname,pfadname);
                  strcat(fname,"\\PROF\\WSP.CFG");

                  // jetzt festlegen, welche Projektart es ist
                  char bcelwaflag = 'b';
                  BOOL usedefault = !GetFeature( "wsp_ignore_default_proj" ) || !GetFeature("wsp_lwa_version");
                  BOOL lwaProjekt = GetFeature( "wsp_default_lwa" );
                  if( usedefault ) // in diesem Fall den Standardwert übernehmen
                    bcelwaflag = lwaProjekt ? 'l' : 'b';
                  else // ansonsten den Bneutzer fragen
                  {
                    char frage[200], pasche[200], knauf[200];

                    xvt_res_get_str( STR_PROJEKT_NEU_FRAGE, frage, sizeof( frage ) );
                    xvt_res_get_str( STR_RECHENKERN_PASCHE, pasche, sizeof( pasche ) );
                    xvt_res_get_str( STR_RECHENKERN_KNAUF, knauf, sizeof( knauf ) );

                    switch( xvt_dm_post_ask( pasche, knauf, NULL, frage ) )
                    {
                    case RESP_DEFAULT:
                      bcelwaflag = 'b';
                      break;

                    case RESP_2:
                      bcelwaflag = 'l';
                      break;
                    };
                  };

                  
                  PROJ = fopen( fname, "w" );
                  fprintf( PROJ, "    0    0    0  %c", bcelwaflag );
                  fclose( PROJ );
                  
                  xvt_fsys_convert_str_to_dir(pfadname,&PROJ_SPEC.dir);
                  xvt_fsys_convert_str_to_dir(pfadname,&STR_SPEC.dir);
                  xvt_fsys_set_dir(&STR_SPEC.dir);
                  xvt_slist_add_at_pos(proj_list,0,(char *)pfadname,0L);
                  xvt_slist_add_at_pos(probezlist,0,(char *)pfadname,0L);
                  
                  save_projekt();
                }
              }
              if(!dir_status)//Dick 12.08.98
                dialog_ende=1;
              else
                dialog_ende=0;
              if(!err_projekt_ist_da)
                xvt_vobj_destroy(xdWindow);
       }//-if(check_string()....
            }//dlg147def
            else
            {
              if (check_string(pfadname)!=1)  //Fehler:Umlaut o.ä.
              {
                char buf[200];//dick 12.01.00
                xvt_res_get_str(STR_WSPD147_NOTE_1,buf,sizeof(buf));
                xvt_dm_post_note("%s",buf);
                //xvt_dm_post_note("Bitte keine Umlaute, Leerzeichen eingeben !");
              }
              else
              {
                if(!CopyDir(Projektname_aktuell,pfadname))
                {
                  // die probezlist erzeugen
                  probezlist = xvt_slist_create();
                  FillProbezList( &proj_list, &probezlist );

                  xvt_fsys_set_dir(&STR_SPEC.dir);
                  xvt_slist_add_at_pos(probezlist,0,(char *)pfadname,0L);
                  xvt_slist_add_at_pos(proj_list,0,(char *)pfadname,0L);
                  // Projektdatei aktualisieren
                        // die probezliste muss erzeugt werden

                  // die wsp.prj rausschreiben: save_projekt braucht die probezlist
                  save_projekt();

                  // und die probezlist wieder zerstören
                  xvt_slist_destroy( probezlist );
                  probezlist = NULL;

                  dialog_ende=1;
                }
                else
                {
                  dialog_ende=0;
                  char buf[200];//dick 12.01.00
                  xvt_res_get_str(STR_WSPD147_NOTE_1,buf,sizeof(buf));
                  xvt_dm_post_note("%s",buf);
                  //xvt_dm_post_note("Bitte keine Umlaute, Leerzeichen eingeben !");
                }
                xvt_vobj_destroy(xdWindow);
              }//-if(check_string()....
              
            }
      }
      break;
    case DLG_147_PUSHBUTTON_2: /* "Abbrechen" */
      {
        pfadname[0]='\0';
        is_projekt_neu = FALSE;
        dialog_ende=0;
        xvt_vobj_destroy(xdWindow);
      }
      break;
    case DLG_147_PUSHBUTTON_3:  //jetzt "Suchen..."
      {
        char szDirName[256];//Dick 6.01.99
        GetPrivateProfileString("WSPWIN","HAUPTPATH","C:\\PROJEKT",szDirName,MAX_PATH,"WSPWIN.INI");//Dick 6.01.99
        if(DoDirDlg((HWND)xvt_vobj_get_attr(main_win,ATTR_NATIVE_WINDOW), szDirName))
          xvt_vobj_set_title(edit_feld_id,szDirName);//Dick 6.01.99
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

int verzeichnis_anlegen(char *pfad)
{
  char *verzeichnis_knoten[20];//hier wird pfad nach Reihenfolge zerteilt
  unsigned int i, //Zyklusvariable 
    j=0,//für knote[]
    m=0,//für verzeichnis_knoten[]
    k_len=0,
    loesch_merker[20];//hier werden die Nummern von der neueingelegten Verzeichnisen gespeichert  
  char knote[100];//hier wird alles was zwieschen \ \ gespeichert
  int dir_stat;
  
  struct _finddata_t  data;
  long sHandle;
  
	 for(i=0;i<20;i++)
     loesch_merker[i]=0;
   for(i=0;i<strlen(pfad);i++)
   {
     if(pfad[i]!='\\')
     {   
       knote[j]=pfad[i];
       j++;
     }
     else
     {
       
       knote[j]='\0';
       j=0;
       verzeichnis_knoten[m]=new char[200];
       if(m==0)                 
         strcpy(verzeichnis_knoten[m],knote);//Laufwerk:\ überspringen                                 
       else
       {                    
         strcpy(verzeichnis_knoten[m],verzeichnis_knoten[m-1]);
         k_len=strlen(verzeichnis_knoten[m]);
         *(verzeichnis_knoten[m]+k_len)='\\';
         *(verzeichnis_knoten[m]+k_len+1)='\0';
         strcat(verzeichnis_knoten[m],knote);                                        
       }
       m++;                   
       
     }
     
   }
   //Letzte Knote anhängen
   knote[j]='\0';
   j=0;
   verzeichnis_knoten[m]=new char[200];
   strcpy(verzeichnis_knoten[m],verzeichnis_knoten[m-1]);
   k_len=strlen(verzeichnis_knoten[m]);
   *(verzeichnis_knoten[m]+k_len)='\\';
   *(verzeichnis_knoten[m]+k_len+1)='\0';
   strcat(verzeichnis_knoten[m],knote);
   
   
   for(i=1;i<m+1;i++)
   {
     sHandle=_findfirst(verzeichnis_knoten[i],&data);// Prüfen ob Pfad bereits existiert
     if ((sHandle!=-1)&&((data.attrib&_A_SUBDIR)==_A_SUBDIR))
       continue;
     else
     {
       if((dir_stat=mkdir(verzeichnis_knoten[i])))
         break;
       else
       {
         loesch_merker[i]=1;
       }
     }
   }
   if(dir_stat!=0)dir_stat=1;
   if(dir_stat)   //wenn mißlungen => neuangelegte Verzeichnise löschen
   {
     for(i=m;i>0;i--)
       if(loesch_merker[i]==1)
         _rmdir(verzeichnis_knoten[i]);
       
   }
   for(i=0;i<m+1;i++)
   { *verzeichnis_knoten[i]=NULL;
   delete[] verzeichnis_knoten[i];
   }
   _findclose(sHandle); 
   return dir_stat;//0-Ok sonst Fehler
   
}