#include <windows.h>
#include "xvt.h"
#include "wspwin.h"
#include "resource.h"

#include "global_types.h"
#include "..\..\wspdlg\Include\export.h"


#include "global_vars.h"
#include "..\..\wspdlg\Include\export.h"

#include "list.h"
#include "read_cfg.h"

#include "global.h"
#include "read.h"

#include "cderr.h"

#include "process.h"	// GHJ


#ifndef Max_Files
#define Max_Files 1400
#endif

extern BOOLEAN var_dlg135, editieren, berechnen;
extern int  zaehler;
extern BOOLEAN is_128_ok, konvertierung;
SLIST ber_list=NULL;
SLIST neue_str_list, name_list;
char kon_verzeichnis[100];
char neue_str[13];
char kon_start[100];
char profil_verzeichnis[100];

extern BOOL bBreakBerechnung;	// GHJ
HANDLE hProc_Lwa2b;		// GHJ
extern DWORD exit_ausw;				// GHJ
extern WINDOW main_win;		// GHJ


/********************************************************************/

int read_start_berechnung(void)
{
 int i,ok=1,zaehler=0, k=0;
 //char str[100], str2[15], help[101];
 char *str,
		*str2,
		*help;
 FILE *ber_file;

 str=new char [100];
 str2=new char [15];
 help=new char [101];

 xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str,50);
 strcat(str,"\\");

strcpy(str2,STR_SPEC.name);
str2[9]='\0';
strcat(str2,"BER");
strcat(str,str2);
ber_file = fopen(str,"r+");

if((editieren) && (!berechnen))
 {
  while ((ber_file ==NULL)&&(ok==1))
	 {
      char buf[200],buf2[200],buf3[200],buf4[200];//Dick 26.11.99
      
      xvt_res_get_str(STR_NEU,buf,sizeof(buf));
      xvt_res_get_str(STR_ANDEREWAHL,buf2,sizeof(buf2));
      xvt_res_get_str(STR_ABBRECHEN,buf3,sizeof(buf3));
      xvt_res_get_str(STR_START_BER_ASK,buf4,sizeof(buf4)); 
	 // switch (xvt_dm_post_ask("Neu","andere Wahl","Abbrechen","Für den aktuellen Zustand sind keine\nBerechnungsdaten vorhanden\n"))
       switch (xvt_dm_post_ask(buf,buf2,buf3,buf4))
				 {
				  case RESP_DEFAULT:       //neu
						  ok=2;
						  break;
				  case RESP_2:             // anderes
						 {
						  var_dlg135=TRUE;
						  if (!xvt_dlg_create_res(WD_MODAL,DLG_135, EM_ALL, DLG_135_eh, 0L))
								xvt_dm_post_error("Can't open dialog 135");
						  var_dlg135=FALSE;

						  strcpy(str,STR_SPEC.name);
						  str[9]='\0';
						  strcat(str,"BER");
						  ber_file = fopen(str,"r+");
						  if(ber_file!=NULL)
						   ok=1;
						  else
							  ok=0;
						  break;
						 }
				  case RESP_3:
						  ok=0;
						  break;
				 } //switch
	 } /* end of while*/
	} //if editieren
	if(((berechnen) && (!editieren)) || ((berechnen) && (editieren))|| ((!berechnen) && (!editieren)))
	 {
	  if(ber_file==NULL)
		{
		 //xvt_dm_post_note("Es existieren keine Berechnungsvarianten");
         char buf[200];//Dick 26.11.99
          xvt_res_get_str(STR_BERVAR_NOTEXIST,buf,sizeof(buf));
          xvt_dm_post_error("%s",buf);
		 ok=0;
		}
	 }
	 if (ok==1)
	 {    // Datei ist geöffnet
	  fgets(help,10,ber_file);
	  for (i=0;i<=4;i++)
		  str[i]=help[i];
	  str[5]='\0';
	  zaehler = atoi(str);      // Anzahl Berechnunenldateien

	  if (ber_list !=NULL)
		{
		 xvt_slist_destroy(ber_list);
		 ber_list=NULL;
		}

	  if  ((ber_list = xvt_slist_create())==NULL)
			 {
			  xvt_dm_post_error(" Can't create _SLIST:ber_list");

			 }
	  else
		 {
		 for (i=1;i<=zaehler;i++)
			{
			if (!feof(ber_file))
			  {
				 fgets(help,100,ber_file);
				 if (help[0]!='\n')
				  {
					for (k=0;k<=(INT)strlen(help);k++)
					 {
					 if(help[k]=='\n')
						help[k] ='\0';
					 }
					strcat(help,"\0");
					xvt_slist_add_at_elt(ber_list,NULL,help,i-1);

				  }
				help[0]='\n';

			 } //if
		  } //for
		 } //else

	 } //if ok=1

	 if (ber_file!=NULL)
		  fclose(ber_file);

	delete[] str;
	delete[] str2;
	delete[] help;
	 return ok;
}
/********************************************************************************/
void read_anfang_ende(void)
{
  char *str,
    *help,
    *gewaesser_name,
    *str_zustand,
    *ap,
    *ep,
    *strang_text;
  FILE *in;
  int i=0, j=0;
  
  char *ap_ptr;
  char *ep_ptr;
  
  str = new char [100];
  help = new char [120];
  gewaesser_name = new char [20];
  str_zustand = new char[20];////Dick 17.06.99 12->20
  ap = new char [15];
  ep = new char [15];
  strang_text=new char [70];
  
  xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str,50);
  strcat(str,"\\");
  strcat(str,STR_SPEC.name);
  
  if ((in= fopen(str,"r+"))==NULL)
  {
    //xvt_dm_post_error(" Datei : %s läßt sich nicht öffnen !",str);
    char buf[200],buf2[200];//Dick 26.11.99
    xvt_res_get_str(STR_DATEI,buf,sizeof(buf));
    xvt_res_get_str(STR_CANOTOPEN,buf2,sizeof(buf2));
    xvt_dm_post_error("%s%s%s",buf,str,buf2);
    exit(-1);
  }
  else
  {
    fscanf(in,"%d",&anzahl_profil_dat_entries);
    fscanf(in,"%d",&anzahl_strang_entries);
    fscanf(in,"%s",gewaesser_name);
    fscanf(in,"%s",str_zustand);
    
    gewaesser_name[8]='\0';
    str_zustand[strlen(str_zustand)]='\0';
    fgets(help,110,in); // \n überlesen
    
    for (i=1;i<=anzahl_profil_dat_entries;i++)    //Profiltabelle lesen
    {
      if(!feof(in))
      {
        fgets(help,110,in);
      } // if !=eof
    }  // for
    
    fgets(help,110,in); //Leerzeile einlesen
    
    if((list_anfang=xvt_slist_create())==NULL)
      xvt_dm_post_error("Can´t create _SLIST:list_anfang");
    if((list_ende=xvt_slist_create())==NULL)
      xvt_dm_post_error("Can´t create _SLIST:list_ende");
    
    for (j=1;j<=anzahl_strang_entries;j++)
    {
      if(!feof(in))
      {
        fscanf(in,"%s",ap);
        fscanf(in,"%s",ep);
        fgets(strang_text,70,in);
        strang_text[strlen(strang_text)]='\0';
        if (strang_text[strlen(strang_text)-1]=='\n')
          strang_text[strlen(strang_text)-1]='\0';
        
        ap_ptr=&ap[0];
        ep_ptr=&ep[0];
        
        xvt_slist_add_at_elt(list_anfang,NULL,ap_ptr,0);  //Element in SLIST eintragen
        xvt_slist_add_at_elt(list_ende,NULL,ep_ptr,0);
      }
    } //for
    fclose(in);
  } //else
  
  delete[] str;
  delete[] help;
  delete[] gewaesser_name;
  delete[] str_zustand;
  delete[] ap;
  delete[] ep;
  delete[] strang_text;
  
} //read_anfang_ende

/*******************************************************/
 int name_doppelt_profile_schon_da(void) //in read.cpp
  {
	//AUFRUF AUS WSPM001.cpp - KONVERTIERE
	//TESTET OB BEI DEN ZUR KONVERTIERUNG AUSGEWÄHLTEN DATEIEN
	//DATEINAMEN DOPPELT SIND (ERSTE 5 BUCHSTABEN) ODER
	//PROFILE:PRF DIE KONVERTIERT WERDEN EVENTUELL SCHON DA SIND

  char *help_ptr;
  char str[15], str1[15], str2[15], datei[100], inhalt[80];
  int a,b,i,j,c,k, turn, rueckgabe, zaehler1, zaehler2;
  FILE *in;
  BOOLEAN meldung=FALSE;

  char buf[200];

  j=xvt_slist_count(profil_list);
  rueckgabe=1;
  if(j==0)
	rueckgabe=0;
  else
	{
	 for(a=0;a<j;a++)
	 {
	  help_ptr=xvt_slist_get_elt(profil_list,a,0l);
	  strcpy(str,help_ptr);
	  for(i=0;i<5;i++)
		str1[i]=str[i];
	  str1[5]='\0';
		  for(b=0;b<j;b++)
			{
			 help_ptr=xvt_slist_get_elt(profil_list,b,0L);
			 strcpy(str,help_ptr);
			 for(i=0;i<5;i++)
			  str2[i]=str[i];
			 str2[5]='\0';
			 turn=xvt_str_compare_ignoring_case(str1,str2);
			 if((turn==0) &&(a!=b))
			  {
				rueckgabe=0;
				//xvt_dm_post_note("Sie können nicht mehrere Profile mit den "
				//						" selben Dateinamen konvertieren (erste 5 "
				//						" Buchstaben entscheiden) ");
                
                xvt_res_get_str(STR_KONVERT_NOTE,buf,sizeof(buf));
                xvt_dm_post_note(buf);
			  }
			} //for b

	 xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,datei,80);
	 strcat(datei,"\\profproj.txt");
	 c=access(datei,00);
	 if(c==0)
	  {
		in=fopen(datei,"r");
		fscanf(in,"%d",&zaehler1);
		fscanf(in,"%d",&zaehler2);
		fgets(inhalt,79,in); //\n überlesen
		meldung=FALSE;
		for(i=1;i<=zaehler1;i++)
		 {
		  fgets(inhalt,77,in);
		  for(k=0;k<=(INT)strlen(inhalt);k++)
			{
			 if(inhalt[k]=='\n')
			  inhalt[k]='\0';
			}
		  for(k=44;k<49;k++)
			str2[k-44]=inhalt[k];
		  str2[5]='\0';
		  turn=xvt_str_compare_ignoring_case(str1,str2);
		  if(turn==0)
			{
			 rueckgabe=0;
			 meldung=TRUE;
			 //xvt_dm_post_note("Es existieren bereits Profile "
			 //						"mit dem selben Namen. Aendern Sie "
			 //						"den Namen der zu konvertierenden Datei ");
			}
		 } //for zaehler1
		if(meldung)
		 {
			//xvt_dm_post_note("Es existieren bereits Profile "
			//						"mit dem selben Namen. Aendern Sie "
			//						"den Namen der zu konvertierenden Datei ");
            xvt_res_get_str(STR_KONVERT_NOTE2,buf,sizeof(buf));
                xvt_dm_post_note(buf);
		 }
		fclose(in);
	  } //if c==0

	 } //for a
	} //else j!=0
	return rueckgabe;
  }

 /***************************************************************************/
void menue_alles_sperren(void)
{
  xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_50,FALSE);
  xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_13_39,FALSE); // Menu: Profildatei enabled setzen
  
  xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_14_16,FALSE); // Menu: Abflußdatei
  xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_14_17,FALSE); // Menu: Verlustdatei
  
  xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_30,FALSE);
  xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_6,FALSE);
  xvt_menu_update(Menu_Win);
}; // menue_alles_sperren
 /**************************************************************************/
void menue_alles_oeffnen(void)
{
  if(strlen(STR_SPEC.dir.path)!=0)
  {
    xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_50,TRUE);
    if(strlen(STR_SPEC.name)!=0)
    {
      if (dlg_136==NULL_WIN)
        xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_13_39,TRUE); // Menu: Profildatei enabled setzen
      
      if(dlg_136==NULL_WIN)
      {
        xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_14_16,TRUE); // Menu: Abflußdatei
        xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_14_17,TRUE); // Menu: Verlustdatei
      }
      
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_6,TRUE);
      xvt_menu_set_item_enabled(Menu_Win,TASK_MENUBAR_30_29,TRUE);//Ergebnisse(H)
    }
    xvt_menu_update(Menu_Win);
  }
}; // menu_alles_oeffnen
/****************************************************************************/

int prof_in_slist2(char auswahl_dateien[256])
// füllt die Profil_list mit den Profilen aus auswahl_dateien // NEIN insbesondere wird profil mit dem Namen belegt
// Rückgabewert: 0 bei Fehler, 1 bei Erfolg
// Paramter: char asuwahl_datein[256]: ...?
// Nebeneffekte:
//    Änderungen: char profil_verzeichnis[100]: 
//                SLIST profil_list:  wird neu gefüllt
{
  int i, result=0,j, help;
  char profil[13];
  BOOLEAN blank_da=FALSE;
  
  j=0;
  profil_verzeichnis[0]='\0';
  blank_da=FALSE;
  if (auswahl_dateien[0]==' ')
  {
    //xvt_dm_post_note("Keine Profildateien gewählt.\n Konvertierung nicht ausführbar!");
    char buf[200],buf2[200] ;//Dick 26.11.99
    xvt_res_get_str(STR_KONVERT_NOTE_1,buf,sizeof(buf)); 
    xvt_res_get_str(STR_KONVERT_NOTE_2,buf2,sizeof(buf2));
    xvt_dm_post_note("%s\n%s",buf,buf2);
    result=0;
  }
  else
  {
    result=1;
    if(profil_list!=NULL)
    {
      xvt_slist_destroy(profil_list);
      profil_list=NULL;
    }
    profil_list=xvt_slist_create();
    profil[0]='\0';
    
    for(i=0;i<=(INT)strlen(auswahl_dateien);i++)
    {
      if(auswahl_dateien[i]==' ')
        blank_da=TRUE;
    }
    if(!blank_da) //bei 1 Profil existiert kein blank zwischen verzeichnis und Profil
    {
      blank_da=FALSE; //var. blank_da für Anzeige von \\ benutzt
      j=0;
      help=strlen(auswahl_dateien);
      for(i=help;i>=0;i--)
      {
        if(!blank_da)
        {
          if(auswahl_dateien[i]=='\\')
          {
            blank_da=TRUE;
            auswahl_dateien[i]='\0';
            strcpy(profil_verzeichnis,auswahl_dateien);
            strcat(profil_verzeichnis,"\\");
            auswahl_dateien[i]='\\';
            j=i;
          }
        }
      } //for Länge auswahl_dateinen --;
      help=j+1;
      for(i=help; i<=(INT)strlen(auswahl_dateien);i++)
      {
        profil[i-help]=auswahl_dateien[i];
      }
      if(konvertierung)
      {
        int len=strlen(profil);
        if(((profil[len-1]=='p')||(profil[len-1]=='P'))
          &&((profil[len-2]=='s')|| (profil[len-2]=='S'))
          &&((profil[len-3]=='w') || (profil[len-3]=='W')))
          xvt_slist_add_at_elt(profil_list,NULL,profil,0);
        else
        {
          //xvt_dm_post_note("Datei %s hat nicht die Endung -wsp-.", profil);
          char buf[200],buf2[200] ;//Dick 26.11.99
          xvt_res_get_str(STR_DATEI,buf,sizeof(buf)); 
          xvt_res_get_str(STR_KONVERT_NOTE_3,buf2,sizeof(buf2));
          xvt_dm_post_note("%s%s%s",buf,profil,buf2);
        }
      }
      else
        xvt_slist_add_at_elt(profil_list,NULL,profil,0);
    }  //nur 1
    else  //mehrere Profile
    {
      blank_da=FALSE;
      
      for(i=0;i<=(INT)strlen(auswahl_dateien);i++)
      {
        if (blank_da==FALSE)
        {
          if(auswahl_dateien[i]!=' ')
            profil_verzeichnis[i]=auswahl_dateien[i];
        }
        if(blank_da)
        {
          if(auswahl_dateien[i]!=' ')
          {
            profil[j]=auswahl_dateien[i];
            j++;
          }
        }
        
        if ((auswahl_dateien[i]==' ') || (auswahl_dateien[i]=='\0'))
        {
          if (!blank_da)
          {
            profil_verzeichnis[i]='\0';
            strcat(profil_verzeichnis,"\\");
          }
          if (blank_da)
          {
            profil[j]='\0';
            /**********************/
            if(konvertierung)
            {
              int len=strlen(profil);
              if(((profil[len-1]=='p')||(profil[len-1]=='P'))
                &&((profil[len-2]=='s')|| (profil[len-2]=='S'))
                &&((profil[len-3]=='w') || (profil[len-3]=='W')))
                xvt_slist_add_at_elt(profil_list,NULL,profil,0);
              else
              {
                //xvt_dm_post_note("Datei %s hat nicht die Endung -wsp-.", profil);
                char buf[200],buf2[200] ;//Dick 26.11.99
                xvt_res_get_str(STR_DATEI,buf,sizeof(buf)); 
                xvt_res_get_str(STR_KONVERT_NOTE_3,buf2,sizeof(buf2));
                xvt_dm_post_note("%s%s%s",buf,profil,buf2);
              }
            }
            
            /**********************/
            else
              xvt_slist_add_at_elt(profil_list,NULL,profil,0);
            
            profil[0]='\0';
            j=0;
          }
          blank_da=TRUE;
        }
      } //FOR
    } //else mehrere Profile
    int len=xvt_slist_count(profil_list);
    if(len==0)
      result=0;
    
    
  } //ELSE überhaupt Profile
  return result;
} //FUNKTION

/**********************************************************************/
void zaehler_aus_ber_lesen(void)
{
  int i,ok=1,zaehler=0;
  char str[100],	str2[15], help[101], string[15];
  FILE *ber_file;
  
  xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str,50);
  strcat(str,"\\");
  strcpy(str2,STR_SPEC.name);
  str2[9]='\0';
  strcat(str2,"BER");
  strcat(str,str2);
  
  ber_file = fopen(str,"r+");
  if(ber_file!=NULL)
  {
    fgets(help,10,ber_file);
    for (i=0;i<=4;i++)
      string[i]=help[i];
    string[5]='\0';
    zaehler = atoi(string);      // Anzahl Berechnunenldateien
    fclose(ber_file);
  }
  if(zaehler<=0)
    remove(str);
}
/*******************************************************************/
// GHJ
void start_lwa2b(void)
// Bemerkung:
//    Inputdateiname darf nicht länger als 8+3 Zeichen sein
{
  FILE *in, *out;

  char name[10], kon_buffer[13], zst[13], *kon_ptr, daten_ausgabe[100], filename1[MAX_PATH],
    filename2[MAX_PATH], error_line[MAX_PATH];
  int zaehler;
  UINT ok=MAXINT;
  SLIST_ELT e;
  STARTUPINFO sui;
  PROCESS_INFORMATION pi;
  int i;
  char save_str_name[MAX_PATH];
  
  strcpy(save_str_name,STR_SPEC.name);
  
  hProc_Lwa2b = NULL;
  exit_ausw = 1;
  bBreakBerechnung = FALSE;
  xvt_fsys_set_dir(&STR_SPEC.dir);
  kon_start[0]='\0';
  xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, kon_start, 50);
  strcat(kon_start,"\\konhelp.bat");
  
  kon_start[0]='\0';
  xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, kon_start, 50);
  strcpy(daten_ausgabe,kon_start);
  daten_ausgabe[strlen(kon_start)-4]='\0';
  strcat(daten_ausgabe,"dath");
  strcat(daten_ausgabe,"\\");
  strcat(kon_start,"\\kon.bat");
  
  is_128_ok=TRUE;
  xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, kon_verzeichnis, 80);
  strcat(kon_verzeichnis,"\\");
  
  strcpy(filename1, daten_ausgabe);
  strcat(filename1, "error.log");
  DeleteFile(filename1);
  
  if(neue_str_list!=NULL)
  {
    xvt_slist_destroy(neue_str_list);
    neue_str_list=NULL;
  }
  neue_str_list=xvt_slist_create();
  zaehler=1;
  bool b_fehler=FALSE;
  for (e=xvt_slist_get_first(profil_list);e !=NULL;e=xvt_slist_get_next(profil_list,e))
  {
    if (is_128_ok)
    {
      kon_ptr = xvt_slist_get(profil_list,e,0L);
      strcpy(kon_buffer,kon_ptr);
      
      //xvt_dm_post_note("Konvertierung für Datei: %s",kon_buffer);
      char buf[200];//Dick 26.11.99
      xvt_res_get_str(STR_KOVERTFUERDATEI,buf,sizeof(buf));      
      xvt_dm_post_note("%s %s",buf,kon_buffer);
      
      if (!xvt_dlg_create_res(WD_MODAL,DLG_128, EM_ALL, DLG_128_eh, 0L))
        xvt_dm_post_error("Can´t open dialog 128");
      if (cfg_list==NULL)
        is_128_ok=FALSE;
      
      if(is_128_ok)
      {
        b_fehler=FALSE;
        DoProgressDlg((HWND)xvt_vobj_get_attr(main_win, ATTR_NATIVE_WINDOW));
        sprintf(filename1, "HYDRAWSP");
        SetProgressTitle(filename1);
        strcpy(filename1, "Konvertierungsprogramm läuft...");
        SetProgressText(filename1);
        IncProgress();
        if (bBreakBerechnung)
          break;
        xvt_slist_add_at_elt(cfg_list,NULL,str_netz,0);
        
        int j2=0;
        for (i=0; i<10; i++)
        {
          if (str_netz[i]!=' ')
          {
            name[j2]=str_netz[i];
            j2++;
          }
        }
        name[j2]='\0';
        
        char str_help[100];
        strcpy(str_help,profil_verzeichnis);
        strcat(str_help,kon_buffer);
        int teste=access(str_help,00);
        if(teste==0)
        {
          strcpy(filename1, profil_verzeichnis);
          strcat(filename1, kon_buffer);
          strcpy(filename2, kon_verzeichnis);
          strcat(filename2, kon_buffer);
          CopyFile(filename1, filename2, FALSE);
        }
        //neu Übergabe: Dateiname ohne extension:
        char kon_buffer_help[15];
        strcpy(kon_buffer_help,kon_buffer);
        kon_buffer_help[strlen(kon_buffer_help)-4]='\0';
        
        strcpy(filename1, kon_verzeichnis);
        strcat(filename1, "verluste.tmp");
        DeleteFile(filename1);
        
        strcpy(filename1, start_dir);
        strcat(filename1, "f77l3.eer");
        strcpy(filename2, kon_verzeichnis);
        strcat(filename2, "f77l3.eer");
        CopyFile(filename1, filename2, FALSE);
        strcpy(filename1, start_dir);
        strcat(filename1, "lf90.eer");
        strcpy(filename2, kon_verzeichnis);
        strcat(filename2, "lf90.eer");
        CopyFile(filename1, filename2, FALSE);
        strcpy(filename1, start_dir);
        strcat(filename1, "lwa2b.exe");
        
        FILE *lwa2b_ctr;
        char *lwa2b_ctr_datei_name;
        lwa2b_ctr_datei_name=new char[200];
        strcpy(lwa2b_ctr_datei_name, start_dir);
        strcat(lwa2b_ctr_datei_name,"lwa2b.ctr");
        lwa2b_ctr=fopen(lwa2b_ctr_datei_name,"w+");
        fprintf(lwa2b_ctr,"%s %s-%s",kon_buffer_help, name, netz_dat[2]);
        delete lwa2b_ctr_datei_name;
        fclose(lwa2b_ctr);
        
        sprintf( filename2, "\"%s\" %s %s-%s", filename1, kon_buffer_help, name, netz_dat[2] );
        ::GetStartupInfo(&sui);
        sui.lpReserved = NULL;
        sui.lpTitle = "LWA2B.EXE";
        sui.dwFlags |= STARTF_USESHOWWINDOW;
        sui.wShowWindow = SW_SHOWNORMAL;
        IncProgress();
        if (bBreakBerechnung)
          break;
        if (!::CreateProcess( NULL, filename2, NULL, NULL, TRUE, NORMAL_PRIORITY_CLASS, NULL, NULL, &sui, &pi))
          break;
        hProc_Lwa2b = pi.hProcess;
        i = 0;
        while (GetExitCodeProcess(pi.hProcess,&exit_ausw) && exit_ausw==STILL_ACTIVE)
        {
          if (!(i%1000))
            IncProgress();
          i++;
        }
        strcpy(filename1, kon_verzeichnis);
        strcat(filename1, "lwa2b.err");
        strcpy(filename2, daten_ausgabe);
        strcat(filename2, "error.log");
        if (zaehler==1)
        {
          DeleteFile(filename2);
          out= fopen(filename2,"w");
        }
        else
          out= fopen(filename2,"a+");
        if ((in = fopen(filename1, "r"))!=NULL)
        {
          fgets(error_line, MAX_PATH, in);		// ignore first line
          fgets(error_line, MAX_PATH, in);		// ignore second line
          while (!feof(in))
          {
            error_line[0] = '\0';
            fgets(error_line, MAX_PATH, in);
            if (error_line[0]!=0)
            {
              fprintf(out, "%s\n", error_line);
              b_fehler=TRUE;
            }
          }
          fclose(in);
        }
        fclose(out);
        
        IncProgress();
        if (bBreakBerechnung)
          break;
        
        hProc_Lwa2b = NULL;
        
        if (exit_ausw!=0)
          break;
        
        strcpy(zst,kon_buffer);
        /**21.2.96 neu:***/
        int len=strlen(zst);
        if(len<9)
        {
          for (int j=0; j<=len;j++)
          {
            if(zst[j]=='.')
              zst[j]='\0';
          }
          len=strlen(zst);
          for(j=len;j<5;j++)
            zst[j]='_';
          
        }
        
        zst[5]='.';
        zst[6]='\0';
        strcat(zst,"zst");
        for (int j=0;j<12;j++)
          neue_str[j]=str_netz[73+j];
        neue_str[12]='\0';
        xvt_slist_add_at_elt(neue_str_list,NULL,neue_str,0);
        strcpy(filename1, kon_verzeichnis);
        strcat(filename1, zst);
        strcpy(filename2, kon_verzeichnis);
        strcat(filename2, neue_str);
        CopyFile(filename1, filename2, FALSE);
        DeleteFile(filename1);

        if( !LWA_PROJEKT )
        {
          zst[(strlen(zst))-3]='\0';
          strcat(zst,"str");  //Steuerparameter bei Knauf
          strcpy(filename1, kon_verzeichnis);
          strcat(filename1, zst);
          DeleteFile(filename1);
          //sicherheitshalber, falls zst noch gebraucht:
          
          zst[(strlen(zst))-3]='\0'; //Protokolldatei bei Knauf
          strcat(zst,"log");
          strcpy(filename1, kon_verzeichnis);
          strcat(filename1, zst);
          DeleteFile(filename1);
          
          zst[(strlen(zst))-3]='\0';
          strcat(zst,"zst");
        }

        /***verlustdatei****/
        
        kon_buffer[strlen(kon_buffer)-3]='\0';
        strcat(kon_buffer,"log");
        strcpy(filename1, kon_verzeichnis);
        strcat(filename1, kon_buffer);
        strcpy(filename2, daten_ausgabe);
        strcat(filename2, kon_buffer);
        CopyFile(filename1, filename2, FALSE);
        
        kon_buffer[strlen(kon_buffer)-3]='\0';
        strcat(kon_buffer,"wsp");
        
        if( LWA_PROJEKT )
        {
          strcpy(filename1, kon_verzeichnis);
          strcat(filename1, "verluste.tmp");
          sprintf(filename2, "%stemp%d.psi", kon_verzeichnis, zaehler);
          CopyFile(filename1, filename2, FALSE);
        };

        strcpy(filename1, kon_verzeichnis);
        strcat(filename1, kon_buffer);
        strcpy(filename2, profil_verzeichnis);
        strcat(filename2, kon_buffer);
        if (stricmp(filename1, filename2)!=0)
          DeleteFile(filename1);
        
        zaehler++;
        EndProgressDlg();
        //if(!b_fehler)
        safe_cfg_dat();
        
      } // if 128_ok nach DLG-128
    } //if_128_ok in for Schleife
  } //for
  
  if(exit_ausw!=0)
  {
    
    strcpy(STR_SPEC.name,save_str_name);
  }

  if( !LWA_PROJEKT )
  {
    strcpy(filename1, kon_verzeichnis);
    strcat(filename1, "verluste.tmp");
    DeleteFile(filename1);
  };

  strcpy(filename1, kon_verzeichnis);
  strcat(filename1, "f77l3.eer");
  DeleteFile(filename1);
  kon_start[0]='\0';
  xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, kon_start, 50);
  strcat(kon_start,"\\konhelp.bat");
  
  kon_start[0]='\0';
  xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, kon_start, 50);
  strcat(kon_start,"\\kon.bat");
}

