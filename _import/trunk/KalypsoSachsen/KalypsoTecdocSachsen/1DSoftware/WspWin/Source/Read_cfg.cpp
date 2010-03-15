/****************************************************************************
*             READ_CFG .CPP  ->   WSP.CFG lesen                             *
*             10.03.1995                                                    *
****************************************************************************/
#include <windows.h>
#include <errno.h>

#include "xvt.h"

#include "resource.h"


#include "wspdlg/include/export.h"

#include "global_types.h"
#include "global_defs.h"
#include "global_vars.h"

#include "global.h"

#include "aufnehm.h"
#include "wspwin.h"

#define PROG "WSPWIN"
#define PATH "PRJPATH"


extern SLIST probezlist;

// Vorwärtsdeklarationen
int safe_cfg_dat();

int read_cfg_dat()
{
  char temp[PATH_LEN];

  if( cfg_list != NULL ) 
    xvt_slist_destroy( cfg_list );
  if( ( cfg_list = xvt_slist_create() ) == NULL )
  {
    xvt_dm_post_error( "Can't create _SLIST:read_cfg" );
    return -1;
  }
  xvt_fsys_convert_dir_to_str( &STR_SPEC.dir, temp, PATH_LEN - 10 );
  strncat( temp, "\\WSP.CFG", 10 );
  FILE* cfg = NULL;

  if( ( cfg = fopen( temp,"r+" ) ) == NULL )
	{
    char* ext = "cfg";

    char buf[200], buf2[200], buf3[200] ;//Dick 26.11.99
    xvt_res_get_str(STR_ERROR,buf,sizeof(buf)); 
    xvt_res_get_str(STR_WSPCONF_NOTE_1,buf2,sizeof(buf2));
    xvt_res_get_str(STR_VERZ_AUSWAHL,buf3,sizeof(buf3));
    xvt_dm_post_note("%s\n%s\n%s",buf,buf2,buf3); // "Fehler !\nWSP-Konfigurationsdatei nicht vorhanden\nBitte Verzeichnis auswählen !!"
    strcpy( STR_SPEC.type, ext );
    xvt_fsys_get_dir( &STR_SPEC.dir );
    
    buf[0]='\0';
    xvt_res_get_str( STR_WSPCONF_NOTE_3, buf, sizeof(buf) );
    switch( xvt_dm_post_file_open( &STR_SPEC,buf ) )
		
    {
    case FL_BAD:   
      {  MessageBox(NULL,"4","",1);
        char buf[200],buf2[200] ;//Dick 26.11.99
        xvt_res_get_str(STR_WSPCONF_NOTE_2,buf,sizeof(buf)); 
        xvt_res_get_str(STR_PROGRAMM_EXIT,buf2,sizeof(buf2));
        xvt_dm_post_error("%s\n%s",buf,buf2); 	 // "Kann WSP-Konfigurationsdatei nicht finden\nProgramm wird beendet !!"
        exit( -1 );
     }

    case FL_CANCEL:
      exit( -1 );

    case FL_OK:
      { 
        xvt_fsys_set_dir( &STR_SPEC.dir );
        xvt_fsys_convert_dir_to_str( &STR_SPEC.dir, temp, PATH_LEN - 40 );
        strncat( temp, "\\", 1 );
        strncat( temp, STR_SPEC.name, 39 );
        if( ( cfg = fopen( temp,"r+" ) ) == NULL )
        {
          char buf[200];//Dick 26.11.99
          xvt_res_get_str( STR_PROGRAMM_EXIT, buf, sizeof( buf ) ); 
          xvt_dm_post_error( "%s", buf );  // "Programm wird beendet !!";
          exit( -1 );
        }
        break;
      }
    } // switch
  } // if cfg
  char help[200];
  char str1[150];

  fgets( help, 200, cfg );    // 1. Zeile aus WSP.CFG lesen

  char bcelwaflag = 'b';
  bool bSaveCfg = false; // wenn die Projektart neu festgelegt wird, danach die cfg gleich wieder speichern

  int scanresult = sscanf( help, "%d %d %d %c", &anzahl_profil_dat_entries, &anzahl_str_dat_entries, &anzahl_str_dat_entries_abs, &bcelwaflag );

  switch( scanresult )
  {
  case 3:
    {           // Projektart noch nicht festgelegt
      BOOL setDefault = !GetFeature( "wsp_ignore_default_proj" ) || !GetFeature("wsp_lwa_version");
      BOOL default_lwa = GetFeature( "wsp_default_lwa" );
      if( setDefault) // in diesem Fall den Standardwert übernehmen
        bcelwaflag = default_lwa ? 'l' : 'b';
      else
      {
        // sonst den Benutzer fragen
        char frage[200], bcepasche[200], lwaknauf[200];
        xvt_res_get_str( STR_FRAGE_RECHENKERN_OLD_WSP, frage, sizeof( frage ) ); 
        xvt_res_get_str( STR_RECHENKERN_PASCHE, bcepasche, sizeof( bcepasche ) ); 
        xvt_res_get_str( STR_RECHENKERN_KNAUF, lwaknauf, sizeof( lwaknauf ) ); 

        switch( xvt_dm_post_ask( bcepasche, lwaknauf, NULL, frage ) )
        {
        case RESP_DEFAULT:
          bcelwaflag = 'b';
          break;

        case RESP_2:
          bcelwaflag = 'l';
          break;
        }
      };
      bSaveCfg = true;
    }
    break;

  case 4:  
    if( bcelwaflag == 'b' ||  bcelwaflag == 'l')
      break;
    // sonst Formatfehler, keni break
  default:
    {   
      char buf[200];//Dick 26.11.99
      xvt_res_get_str( STR_WSP_CFG_FORMAT_ERROR, buf, sizeof( buf ) ); 
      xvt_dm_post_error( "%s", buf );  // "Formatfehler in wsp.cfg. Programm wird beendet.";
      exit( -1 );
    }
  };
  // Abhängig vom Flag die Projektart setzen

  setProjectType( (bcelwaflag == 'l') );
  // die Namen der Strangdateien lesen
  for( int i = 1; i <= anzahl_str_dat_entries; i++ )
  {
	  if( !feof( cfg ) )
    {
      fgets( str1, 100, cfg );
      str1[strlen( str1 ) - 1] = '\0';
      xvt_slist_add_at_elt( cfg_list, NULL, str1, i - 1 );
    }
  }
  fclose( cfg );
  if( bSaveCfg )
    safe_cfg_dat();
  if( anzahl_str_dat_entries > 0 && fehler == 0 )
    fehler = anzahl_str_dat_entries;
  return fehler;
}

/******************************************************************************/
int safe_cfg_dat()  //neue *.str-Datei ans Ende anhängen
{
  char temp[PATH_LEN];
  xvt_fsys_convert_dir_to_str( &STR_SPEC.dir, temp, PATH_LEN - 10 );
  strncat( temp, "\\WSP.CFG", 10 );
  if( ( access( temp, 6 ) ) == -1 )
	{
    switch( errno )
    {
    case ENOENT:
      {
        //xvt_dm_post_fatal_exit("      Fehler\nKann WSP-Konfigurationsdatei nicht finden\nProgramm wird beendet !!");
        char buf[200],buf2[200],buf3[200] ;//Dick 26.11.99
        xvt_res_get_str(STR_ERROR,buf,sizeof(buf)); 
        xvt_res_get_str(STR_WSPCONF_NOTE_2,buf2,sizeof(buf2));
        xvt_res_get_str(STR_PROGRAMM_EXIT,buf3,sizeof(buf3));
        xvt_dm_post_fatal_exit("%s\n%s\n%s",buf,buf2,buf3); 
      }
      break;

    case EACCES:
      {
        //xvt_dm_post_error("      Fehler\nKann WSP-Konfigurationsdatei nicht finden\nProgramm wird beendet !!");
        char buf[200],buf2[200],buf3[200] ;//Dick 26.11.99
        xvt_res_get_str(STR_ERROR,buf,sizeof(buf)); 
        xvt_res_get_str(STR_WSPCONF_NOTE_2,buf2,sizeof(buf2));
        xvt_res_get_str(STR_PROGRAMM_EXIT,buf3,sizeof(buf3));
        xvt_dm_post_fatal_exit("%s\n%s\n%s",buf,buf2,buf3);
      }
      break;
    } // switch
  } //end if

  FILE* cfg = NULL;
  if( (cfg = fopen( temp, "w+" ) ) != NULL )
  {
    char bcelwaflag = LWA_PROJEKT ? 'l' : 'b';
    fprintf( cfg, "%5d%5d%5d %c\n", anzahl_profil_dat_entries, anzahl_str_dat_entries, anzahl_str_dat_entries_abs, bcelwaflag );
    
    if( xvt_slist_is_valid( cfg_list ) )
    {
      for( SLIST_ELT e = xvt_slist_get_first( cfg_list ); e != NULL; e = xvt_slist_get_next( cfg_list, e ) )
      {
        char* str2 = xvt_slist_get( cfg_list, e, 0L );
        fprintf( cfg, "%s\n", str2 );
      }
    }
     
    fclose( cfg );
    SaveNetzFile = FALSE;

    return 0;
  }
  else 
    return -1;
}

/******************************************************************************/
void lese_projektdatei(void)
{
  FILE *proj,*ini;
  char temp[155];
  char WINDOWS_DIRECTORY[151];
  int fehler;
  BOOL ExWinIni=FALSE;
	 
  if  ((proj_list = xvt_slist_create())==NULL)
  {
    xvt_dm_post_error(" Can't create _SLIST:proj_list");
    exit(-1);
  }
  
  if  ((archiv_list = xvt_slist_create())==NULL)
  {
    xvt_dm_post_error(" Can't create _SLIST:proj_list");
    exit(-1);
  }
  
  // WSPWIN.INI wird durch Installationsprogramm "SETUP" angelegt
  
  GetWindowsDirectory(WINDOWS_DIRECTORY,140);
  strcpy(temp,WINDOWS_DIRECTORY);
  strcat(temp,"\\");
  strcat( temp, INI_FILE_NAME );
  if ((ini=fopen(temp,"r"))==NULL)
  {
    strcpy(temp,WINDOWS_DIRECTORY);
    strcat(temp,"\\");
    strcat(temp,"WIN.INI");
    if ((ini=fopen(temp,"r"))==NULL)
    {
	     //xvt_dm_post_fatal_exit("WSPWIN.INI oder WIN.INI nicht vorhanden!\nProgramm wird beendet!");
      char buf[200],buf2[200];//Dick 26.11.99
      xvt_res_get_str(STR_INI_NOTEXIST,buf,sizeof(buf)); 
      xvt_res_get_str(STR_PROGRAMM_EXIT,buf2,sizeof(buf2));
      xvt_dm_post_fatal_exit("%s\n%s",buf,buf2);
    }
    else
    {	
      fclose(ini);
      ExWinIni =TRUE;  // es gibt eine WIN.INI
    }
  }
  else  fclose(ini); // es gibt eine WSPWIN.INI
  
  
  if ( ExWinIni)
    fehler = GetProfileString(PROG,PATH,"\0",WSP_START_VERZEICHNIS,150);
  else
    fehler = GetPrivateProfileString(PROG,PATH,PATH,WSP_START_VERZEICHNIS,150,INI_FILE_NAME);
  
  if (fehler < 1) // es wurden weniger wie 8 Zeichen gelesen,d.h nur "PRJPATH"
  {
    //xvt_dm_post_fatal_exit("Projekteintrag in WSPWIN.INI nicht vorhanden!\nProgramm wird beendet!");
    char buf[200],buf2[200];//Dick 26.11.99
    xvt_res_get_str(STR_INI_NOTEXIST2,buf,sizeof(buf)); 
    xvt_res_get_str(STR_PROGRAMM_EXIT,buf2,sizeof(buf2));
    xvt_dm_post_fatal_exit("%s\n%s",buf,buf2);
  }
  xvt_fsys_convert_str_to_dir(WSP_START_VERZEICHNIS,&PROJ_SPEC.dir);
  xvt_fsys_set_dir(&PROJ_SPEC.dir);
  strcpy(PROJ_SPEC.name, "WSP.PRJ");
  PROJ_SPEC.type[0]='\0';
  PROJ_SPEC.creator[0]='\0';
  //strcpy(PROJ_SPEC.type,"PRJ");
  //int j=sprintf(PROJ_SPEC.type,"prj");
  //PROJ_SPEC.type[++j]='\0';
  
  
  strcpy(Plot_Ini_Datei,WSP_START_VERZEICHNIS);
  strcat(Plot_Ini_Datei,"\\WSPPLOT.INI");
  
  strcpy(temp,WSP_START_VERZEICHNIS);// in WSPWIN.INI steht nur Pfad !!!
  strcat(temp,"\\WSP.PRJ");
  if ((proj= fopen(temp,"r+"))==NULL)
		{
    //xvt_dm_post_error("WSP-Projektdatei nicht vorhanden.\nBitte Verzeichnis auswählen !!");
    char buf[200],buf2[200];//Dick 26.11.99
    char ausgewahl_dateien[AUFNEHM_BUFFER_SIZE];
    xvt_res_get_str(STR_WSPPROJ_NOTEXIST,buf,sizeof(buf)); 
    xvt_res_get_str(STR_VERZ_AUSWAHL,buf2,sizeof(buf2));
    xvt_dm_post_error("%s\n%s",buf,buf2);
    buf[0]='\0';
    xvt_res_get_str(STR_PROJ_AUSWAHL,buf,sizeof(buf));
    //switch (xvt_dm_post_file_open(&PROJ_SPEC,buf))
    switch (auswahl2(ausgewahl_dateien, 0, 40004L))
			 {
       /*case FL_BAD:   {
       //xvt_dm_post_error("Kann WSP-Projektdatei nicht finden\nProgramm wird beendet !!");
       char buff[200],buf2[200] ;//Dick 26.11.99
       xvt_res_get_str(STR_WSPPROJ_NOTE_1,buff,sizeof(buff)); 
       xvt_res_get_str(STR_PROGRAMM_EXIT,buf2,sizeof(buf2));
       xvt_dm_post_error("%s\n%s",buff,buf2);
       exit(-1);
    }*/
				case IDCANCEL:{
									 //xvt_dm_post_error("Kann WSP-Projektdatei nicht finden\nProgramm wird beendet !!");
          char buff[200],buf2[200] ;//Dick 26.11.99
          xvt_res_get_str(STR_WSPPROJ_NOTE_1,buff,sizeof(buff)); 
          xvt_res_get_str(STR_PROGRAMM_EXIT,buf2,sizeof(buf2));
          xvt_dm_post_error("%s\n%s",buff,buf2);
									 exit(-1);
                      }
        case IDOK:    {
									 //xvt_fsys_set_dir(&PROJ_SPEC.dir);
          xvt_fsys_get_dir(&PROJ_SPEC.dir);
									 xvt_fsys_convert_dir_to_str(&PROJ_SPEC.dir,temp,140);
                   strcat(temp,"\\");
                   strcat(temp,PROJ_SPEC.name);
                   
                   if ((proj= fopen(temp,"r+"))==NULL)
                   {
                     //xvt_dm_post_error("Fehler\n\nProgramm wird beendet !!");
                     char buf[200],buf2[200] ;//Dick 26.11.99
                     xvt_res_get_str(STR_ERROR,buf,sizeof(buf)); 
                     xvt_res_get_str(STR_PROGRAMM_EXIT,buf2,sizeof(buf2));
                     xvt_dm_post_error("%s\n%s",buf,buf2);
                     exit (-1);
                   }
                   break;
                      }
			 }
  }
  while (!feof(proj))
	 {
    fgets(temp,150,proj);
    for (int i=strlen(temp);i>=0;i--)
      if  (temp[i]=='\n')
        temp[i] ='\0';
      if(!feof(proj))
        if (strlen(temp)>0)
          xvt_slist_add_at_elt(proj_list,NULL,temp,0L);
	 }
  fclose(proj);
  //Neu Dick 17.12.98
  strcpy(temp,WSP_START_VERZEICHNIS);// in WSPWIN.INI steht nur Pfad !!!
  strcat(temp,"\\WSP.ARH");
  if ((proj= fopen(temp,"a+"))==NULL)
		{
		  //xvt_dm_post_error("WSP-Archivdatei könnte nicht im Verzeichnis %s erstellt werden.\n Überprüfen Sie Ihre wspwin.ini Datei!!",WSP_START_VERZEICHNIS);
    char buf[200],buf2[200];//Dick 26.11.99
    xvt_res_get_str(STR_WSPPROJ_NOTE_2,buf,sizeof(buf)); 
    xvt_res_get_str(STR_WSPPROJ_NOTE_3,buf2,sizeof(buf2));
    xvt_dm_post_note("%s%s%s",buf,WSP_START_VERZEICHNIS,buf2);
		  exit (-1);
  }
  while (!feof(proj))
	 {
    fgets(temp,150,proj);
    for (int i=strlen(temp);i>=0;i--)
      if  (temp[i]=='\n')
        temp[i] ='\0';
      if(!feof(proj))
        if (strlen(temp)>0)
          xvt_slist_add_at_elt(archiv_list,NULL,temp,0L);
	 }
  fclose(proj);
  //Ende Neu
  xvt_fsys_convert_dir_to_str(&PROJ_SPEC.dir,temp,140);
}


/************************************************************************/
void save_projekt(void)
{
  FILE *proj;
  char temp[151], *pstr;
  SLIST_ELT e;
  char hilfsstring[200], str[200];
  
  strcpy(temp,WSP_START_VERZEICHNIS);
  strcat(temp,"\\WSP.PRJ");
  if ((proj= fopen(temp,"w"))==NULL)
  {
    //xvt_dm_post_error(" Fehler !\nWSP-Projektdatei nicht vorhanden !!");
    char buf[200],buf2[200];//Dick 26.11.99
    xvt_res_get_str(STR_ERROR,buf,sizeof(buf)); 
    xvt_res_get_str(STR_WSPPROJ_NOTEXIST,buf2,sizeof(buf2));
    xvt_dm_post_error("%s!\n%s!",buf,buf2);
  }
  else
  {
    //	 for (e=xvt_slist_get_first(proj_list);e!=NULL;
    //						  e=xvt_slist_get_next(proj_list,e))
    for (e=xvt_slist_get_first(probezlist);e!=NULL;
    e=xvt_slist_get_next(probezlist,e))
    {
      //			  pstr = xvt_slist_get(proj_list,e,0L);
      pstr = xvt_slist_get(probezlist,e,0L);
      
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
      
      //			  fprintf(proj,"%s\n",pstr);
      fprintf(proj,"%s\n",str);
			 }
    fclose(proj);
  }
  //Neu Dick 17.12.98
  strcpy(temp,WSP_START_VERZEICHNIS);
  strcat(temp,"\\WSP.ARH");
  if ((proj= fopen(temp,"w"))==NULL)
  {
    //xvt_dm_post_error(" Fehler !\nWSP-Archivdatei nicht vorhanden !!");
    char buf[200],buf2[200];//Dick 26.11.99
    xvt_res_get_str(STR_ERROR,buf,sizeof(buf)); 
    xvt_res_get_str(STR_WSPPROJ_NOTEXIST2,buf2,sizeof(buf2));
    xvt_dm_post_error("%s!\n%s!",buf,buf2);
  }
  else
  {
    for (e=xvt_slist_get_first(archiv_list);e!=NULL;
    e=xvt_slist_get_next(archiv_list,e))
			 {
      pstr = xvt_slist_get(archiv_list,e,0L);
      fprintf(proj,"%s\n",pstr);
			 }
    fclose(proj);
  }
  
  //Ende Neu
}
/*************************************************************************/

void min_max_in_cfg_list(void)
{
  SLIST_ELT e1;
  char *pstr=NULL;
  char max_cfg[15], min_cfg[15];
  char *cfg_str;  // andresen 07.10.96
  int x=0, i=0, j=1, zaehly=0, len=0;
  char strname_compare[15];
  BOOLEAN ende=FALSE;
  double anfangswert, endwert, laufwert;
  //geändert weile negative Werte nicht richtig einsortiert werden:
  
  
  //neu
  cfg_str = new char[150];
  
  if(list_anfang!=NULL)
    x=xvt_slist_count(list_anfang);
	 if(x!=0)
	  {
     pstr=xvt_slist_get_elt(list_anfang,0,0L);
     strcpy(min_cfg,pstr);
     anfangswert=atof(min_cfg);
	  }
	  else
      min_cfg[0]='\0';
    
    if(list_ende!=NULL)
      x=xvt_slist_count(list_ende);
    if(x!=0)
    {
      pstr=xvt_slist_get_elt(list_ende,x-1,0L);
      strcpy(max_cfg,pstr);
      endwert=atof(max_cfg);
    }
    else
      max_cfg[0]='\0';
    // check, ob auch wirklich min/max wg. z.B. Verzweigung
    
    
    
    for(e1=xvt_slist_get_first(list_anfang); e1!=NULL;
    e1=xvt_slist_get_next(list_anfang,e1))
    {
      pstr=xvt_slist_get(list_anfang,e1,0L);
      laufwert=atof(pstr);
      if (laufwert<anfangswert)
      {
        anfangswert=laufwert;
        strcpy(min_cfg,pstr);
      }
      if (laufwert>endwert)
      {
        endwert=laufwert;
        strcpy(max_cfg,pstr);
      }
    }
    
    
    
    //ende neu
    
    
    j=1;
    zaehly=0;
    int wieviel=xvt_slist_count(cfg_list);
    if(wieviel>0)
    {
      for(e1=xvt_slist_get_first(cfg_list);e1!=NULL;e1=xvt_slist_get_next(cfg_list,e1))
      {
        ende=FALSE;
        i=0;
        
        pstr=xvt_slist_get(cfg_list,e1,0L);
        for (x=73;x<=84;x++)
        {
          if((pstr[x]=='\n') || (pstr[x]=='\0'))
            ende=TRUE;
          if((pstr[x]!=' ') &&(!ende))
          {
            strname_compare[i]=pstr[x];
            i++;
          }
        }
        strname_compare[i]='\0';
        j=xvt_str_compare_ignoring_case(strname_compare,STR_SPEC.name);
        if(j==0)
        {
          if(min_cfg[0]!='\0')
          {
            x=54;
            len=strlen(min_cfg);
            len=len-1;
            for(i=len;i>=0;i--,x--)
            {
              pstr[x]=min_cfg[i];
            }
            for(i=40;i<=x;i++) //Dick 11.07.98 eventueles Restmüll z.B. '-' 
              pstr[i]=' ';
          }
          else
          {
            for(i=40;i<=54;i++)
              pstr[i]=' ';
          }
          if(max_cfg[0]!='\0')
          {
            x=69;
            len=strlen(max_cfg);
            len=len-1;
            for(i=len;i>=0;i--)
            {
              pstr[x]=max_cfg[i];
              x--;
            }
            for(i=55;i<=x;i++) //Dick 13.01.99 eventueles Restmüll z.B. '-' 
              pstr[i]=' ';
          }
          else
          {
            for(i=55;i<=69;i++)
              pstr[i]=' ';
          }
          //		 xvt_slist_change_str(cfg_list,pstr,zaehly);
          strcpy(cfg_str,pstr);
          xvt_slist_rem(cfg_list,e1);
          xvt_slist_add_at_pos(cfg_list,zaehly,cfg_str,0L);
          SLIST_ELT e=xvt_slist_get_first(cfg_list);
          for (int h=1;h<=zaehly;h++)
            e=xvt_slist_get_next(cfg_list,e);
          e1=e;
          //xvt_slist_change_str(cfg_list,cfg_str,zaehly);
          
        } //if j==0
        zaehly++;
      }  //for cfg_list
    }
    if(list_anfang!=NULL)
    {
      xvt_slist_destroy(list_anfang);
      list_anfang=NULL;
    }
    if(list_ende!=NULL)
    {
      xvt_slist_destroy(list_ende);
      list_ende=NULL;
    }
    //	xvt_slist_destroy(sorted_slist);
    delete[] cfg_str;
    
  }
  /*************************************************************************/
  
  void FillProbezList( SLIST* pProj_list, SLIST* pProbezlist )
    // liest die Projektbezeichnungen aus und schreibt sie in die ProbezList
    // Parameter:
    //        SLIST* pProj_list: Liste der Projekte
    //        SLIST* pProbezList: die zu füllende List ( wird geleert )
    // Rückgabewert:
  {
    for( SLIST_ELT e = xvt_slist_get_first( *pProj_list ); e != NULL; e = xvt_slist_get_next( *pProj_list, e ) )
    {
      char projektbezeichnung[300];
      FILE* probezfile;
      
      char* proj_element = xvt_slist_get( *pProj_list, e, 0L );
      
      strcpy( projektbezeichnung, proj_element );
      strcat( projektbezeichnung,"\\prof\\probez.txt" );
      
      if( ( probezfile = fopen( projektbezeichnung,"r" ) ) != NULL )
      {
        char text_zeile[200];
        
        projektbezeichnung[0] = '\0';
        strcpy( projektbezeichnung, proj_element );
        text_zeile[0] = '\0';
        fgets( text_zeile, 100, probezfile );
        int len = strlen( projektbezeichnung );
        for( int i = len; i <= 80; i++ )
          strcat( projektbezeichnung, " " );
        strcat( projektbezeichnung, " " );
        strcat( projektbezeichnung, text_zeile );
        xvt_slist_add_at_elt( *pProbezlist, NULL, projektbezeichnung, 0L );
        fclose( probezfile );
      }
      else
      {
        projektbezeichnung[0] = '\0';
        strcpy( projektbezeichnung, proj_element );
        xvt_slist_add_at_elt( *pProbezlist, NULL, projektbezeichnung, 0L );
      }; // if fopen
    }; // for e
  }; // FillProbezList