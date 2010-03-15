/***************/
/* Aufnehm.cpp */
/***************/

#include <windows.h>
#include "xvt.h"

#include "global_types.h"
#include "global_vars.h"

#include "read.h"

#include "readprof.h"
#include "resource.h"
#include "profpro2.h"
#include "wspwin.h"
#include "profproj.h"
#include "util.h"
#include "util2.h"
#include "configuration.h"
#include "verzweig.h"
#include "strang.h"
#include "strang2.h"

#include "aufnehm.h"

// globale Variablen, welche ausschließlich in Aufnehm.cpp benutzt werden

char neuer_dateiname[15];  //Name der neuen Datei ohne Verzeichnis bei Kopieren
char kopierdatei[100]; //  Name der neuen Datei mit Verzeichnis bei Kopieren

// vorwärtsdeklarationen
void datei_namen_vergeben(void); // aufnehm.cpp

int profile_aufnehmen(STRANG *alte_strang)
{
  char *quelle_ptr;
  SLIST_ELT e;
  int back;
  char extension[5];
  int len;
  int anzahl_profile, profile_zaehler;
  
  char ziel[100]; //Zieldatei mit Verzeichnis
  char path[100]; //Quellpfad
  char quelle[100]; //Quelldatei mit Verzeichnis
  char dest [100]; //Zielpfad
  char zielname[15]; //Ziel- ==Quelldatei ohne Verzeichnis
  char auswahl_dateien[AUFNEHM_BUFFER_SIZE];
  
  auswahl_dateien[0]='\0';
  war_schon=FALSE;
  if(interp_prog) //Dick 22.03.99
  {
    strcpy(auswahl_dateien,lpszData);
    auswahl_dateien[strlen(auswahl_dateien)-1]='\0';
  }
  if(!zustand_kopieren && !interp_prog)  //8.2.99 bley //Dick 22.03.99
    back=auswahl2(auswahl_dateien, 1, 40001); //in aufnehm.cpp // 40001: .prf und .dat
  else                   //8.2.99 bley
    back=IDOK;         //8.2.99 bley
  
  if(back==IDOK)         
  {
    if(!zustand_kopieren || interp_prog)  //8.2.99 bley //Dick 22.03.99
      back=prof_in_slist2(auswahl_dateien); //in aufnehm.cpp
    else                   //8.2.99 bley
      back=1;         //8.2.99 bley
    
    xvt_fsys_get_dir(&file_spec.dir);
    xvt_fsys_convert_dir_to_str(&file_spec.dir, path, 80);
    if(back==1)
    {
      profile_zaehler=0;
      anzahl_profile=xvt_slist_count(profil_list);
      for (e=xvt_slist_get_first(profil_list);e !=NULL;e=xvt_slist_get_next(profil_list,e))
      {
        profile_zaehler++;
        profil_aufnehmen=TRUE;
        save_str_datei();
        read_profil_dat(strang_anfang);
        kopiert=FALSE;
        if (anzahl_strang_entries >=STRANGANZAHL-2)
        {
          //xvt_dm_post_error("Maximale Größe der Strangtabelle ist erreicht !");
          char buf[200];//Dick 26.11.99
          xvt_res_get_str(STR_STRANG_MAX,buf,sizeof(buf));
          xvt_dm_post_error("%s",buf);
        }
        else
        {
          xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,dest,100);   //Zielverzeichnis
          
          quelle_ptr = xvt_slist_get(profil_list,e,0L);
          strcpy(zielname,quelle_ptr);                             //Quelldatei m.V.
          strcpy(uebergabe_name,zielname); //weil in sort_new_prof als exern Var. gebraucht
          strcpy(quelle,path);
          strcat(quelle,"\\");
          strcat(quelle,zielname);
          
          abbruch208=FALSE;
          
          if(!zustand_kopieren && !interp_prog)  //8.2.99 bley //Dick 22.03.99
          {  //8.2.99 bley
            char buf[200],buf2[200],buf3[200],buf4[200], buf5[10], buf8[10], buf6[200], buf7[200];//Dick 26.11.99
            xvt_res_get_str(STR_KOPIEREN,buf,sizeof(buf)); //Bley 3.11.2000
            xvt_res_get_str(STR_VERKNUEPFEN,buf2,sizeof(buf2)); //Bley 3.11.2000
            xvt_res_get_str(STR_COPY_ASK_1,buf3,sizeof(buf3));
            xvt_res_get_str(STR_COPY_ASK_2,buf4,sizeof(buf4));
            xvt_res_get_str(STR_ABBRECHEN,buf6,sizeof(buf6)); //Bley 3.11.2000
            xvt_res_get_str(STR_QUIT_ALL,buf7,sizeof(buf7)); //Bley 3.11.2000
            xvt_res_get_str(STR_JA,buf5,sizeof(buf5)); //Bley 3.11.2000
            xvt_res_get_str(STR_NEIN,buf8,sizeof(buf8)); //Bley 3.11.2000
            
            switch (xvt_dm_post_ask(buf,buf2,buf6,"%s%s%s", buf3,zielname,buf4))
            {
            case RESP_DEFAULT:       //JA - kopieren
              {
                neukopieren=TRUE;
                schluessel_einlesen(quelle); //in profproj.cpp
                if (!xvt_dlg_create_res(WD_MODAL,DLG_208, EM_ALL, DLG_208_eh,0L))
                  xvt_dm_post_error("Can´t open dialog 208");
                
                if(!abbruch208)
                {
                  neukopieren=TRUE;
                  datei_namen_vergeben();     //in aufnehmen.cpp
                  strcpy(uebergabe_name, neuer_dateiname); // weil in sort gebraucht
                } //if(!abbruch208)
                else
                  neukopieren=FALSE;
                break;
              }  //case default
            case RESP_2:
              {
                neukopieren=FALSE;
                break;
              } //case resp2
            case RESP_3:             //Abbrechen Bley 3.11.2000
              {
                switch (xvt_dm_post_ask(buf5,buf8,NULL,"%s", buf7))
                {
                case RESP_DEFAULT:       // Ja-Alles Abbrechen
                  {
                    e=NULL;
                    break;
                  }  //case default
                case RESP_2:             //Nein nicht abbrechen
                  {
                    abbruch208=TRUE;
                    neukopieren=FALSE;
                    break;
                  } //case resp2
                }
                abbruch208=TRUE;
                neukopieren=FALSE;
                break;
              } //case resp2
            } //switch
          } //ende zustand_kopieren   8.2.99 bley
          else if(zustand_kopieren) //Dick 22.03.99
          {
            neukopieren=TRUE;
            schluessel_einlesen(quelle); //in profproj.cpp
            zustand[0]='\0';
            strcpy(zustand,netz_dat[2]);
            teste_projekt_profile(0);
            if(!vergleich)
            {
              datei_namen_vergeben();     //in aufnehmen.cpp
              strcpy(uebergabe_name, neuer_dateiname); // weil in sort gebraucht
            }  //if !=vergleich
            //		 }  ehemals abbruch
            else  //if vergleich
            {
              zustand_kopieren=FALSE;
              break;
              
            }  //else vergleich
          }  //else zustand kopieren
          else //Dick 22.03.99 Interpolation
          {
            neukopieren=FALSE;
          }
          
          len=strlen(quelle);
          extension[0]=quelle[len-3];
          extension[1]=quelle[len-2];
          extension[2]=quelle[len-1];
          extension[3]='\0';
          
          if(!xvt_str_match(extension,"prf",FALSE))
            if(!xvt_str_match(extension,"dat",FALSE))
            {
              char buf[200];//Dick 26.11.99
              xvt_res_get_str(STR_DAT_KEIN_PROF,buf,sizeof(buf));
              xvt_dm_post_error("%s",buf);
              //xvt_dm_post_error("Datei ist keine Profildatei");
              break;
            }
            
            if(!abbruch208)
            {
              /****1. Quellverzeichnis ungleich Zielverzeichnis****/
              if (xvt_str_compare_ignoring_case(path,dest)!=0) //Quell-und Zielverzeichnis ungleich
              {
                if(neukopieren)
                {
                  copy_fileb(quelle,kopierdatei);
                  schluessel_nach_profil(kopierdatei); //in profprof2.cpp
                }
                else
                {
                  strcpy(ziel,dest);
                  strcat(ziel,"\\");
                  strcat(ziel,zielname);
                  
                  int back=access(ziel,00);
                  if (back==0)
                  {
                    //xvt_dm_post_error("Quelldatei:%s\nexistiert bereits im Zielverzeichnis",zielname);
                    char buf[200],buf2[200];//Dick 26.11.99
                    xvt_res_get_str(STR_QUELLDATEI,buf,sizeof(buf));
                    xvt_res_get_str(STR_EXIST_IN_ZIEL,buf2,sizeof(buf2));
                    xvt_dm_post_error("%s%s\n%s",buf,zielname,buf2);
                    break;
                  }
                  else
                    copy_fileb(quelle,ziel);
                }
                xvt_fsys_set_dir(&STR_SPEC.dir);
                
                if(neukopieren)
                {
                  strcpy(dateiname,neuer_dateiname); //wg. Fkt. teste_...
                  teste_projekt_profile(0);
                  if(!vergleich)
                    teste_projekt_profile(1);
                }
                
                if(neukopieren)
                  get_profildatei_names(&neuer_dateiname[0]); //neue prf.-Datei in Proftab.
                else
                  get_profildatei_names(&zielname[0]);
                kopiert=TRUE;
                if(!neukopieren)
                {
                  ermittle_schluessel_aus_profildatei(zielname);
                  schluessel_nach_profil(zielname);
                }
                
                if(!vergleich)
                {
                  BOOL strang_vorwaerts = GetSortStrangVorwaerts();
                  istverzweigt=FALSE;
                  teste_str_verzweigt(); //IN VERZWEIG.cpp
                  if((istverzweigt==FALSE) &&(vzk[0]=='0') && (pk[0]=='0'))
                    sort_new_profil( &strang_anfang, &strang_ende, station208, vzk, pk, uebergabe_name, strang_vorwaerts );
                  else
                  {
                    if ( GetSortVerzweigt() )
                    {
                      if(!xvt_dlg_create_res(WD_MODAL,DLG_211, EM_ALL, DLG_211_eh, 0L))
                        xvt_dm_post_error("Can't open dialog 211");
                      verzweigtes_profil_sortieren( 0, strang_vorwaerts );
                    }
                    else
                      verzweigtes_profil_sortieren( 1, strang_vorwaerts );
                  }
                  if(alte_strang)//Dick 24.11.99
                    wandle_abstand_in_string(alte_strang);
                  else
                    wandle_abstand_in_string();
                }
                
              } // quelle !=Ziel
              
              else /******** ZIEL- UND 	QUELLVERZEICHNIS GLEICH********/
              {
                if(neukopieren)
                {
                  copy_fileb(quelle,kopierdatei);
                  schluessel_nach_profil(kopierdatei);
                }
                xvt_fsys_set_dir(&STR_SPEC.dir);
                
                if(neukopieren)
                {
                  get_profildatei_names(&neuer_dateiname[0]);
                  kopiert=FALSE;
                  ermittle_schluessel_aus_profildatei(neuer_dateiname); //in profproj.cpp
                  teste_projekt_profile(0);
                }
                else
                {
                  get_profildatei_names(&zielname[0]);
                  kopiert=FALSE;
                  ermittle_schluessel_aus_profildatei(zielname); //in profproj.cpp
                  if(!vergleich)
                  {
                    nicht_posten=TRUE;
                    teste_projekt_profile(0);
                    nicht_posten=FALSE;
                    if(!vergleich)
                    {
                      /***/
                      schluessel_nach_profil(zielname);
                      strcpy(dateiname,zielname);
                      teste_projekt_profile(1);
                      char help[100];
                      char *help_ptr;
                      int i,h;
                      SLIST_ELT e2, ee;
                      zeige_slist(prof_datei);
                      for(ee=xvt_slist_get_first(prof_datei);
                      ee!=NULL;ee=xvt_slist_get_next(prof_datei,ee))
                      {
                        if((e2=xvt_slist_get_next(prof_datei,ee))==NULL)
                        {
                          help_ptr=xvt_slist_get(prof_datei,ee,0L);
                          if(strlen(help_ptr)==0)
                            help_ptr=" ";
                          strcpy(help,help_ptr);
                          xvt_slist_rem(prof_datei,ee);
                          for(i=0;i<(INT)strlen(zustand);i++)
                            help[33+i]=zustand[i];

                          h=26;
                          for(i=(INT)(strlen(pk)-1);i>=0;h--,i--)             //pk
                            help[h]=pk[i];   //Neu Dick 19->26 und minus 10.07.98
                          
                          h=31;
                          for(i=(INT)(strlen(vzk)-1);i>=0;h--,i--)            //vzk
                            help[h]=vzk[i];  //Neu Dick 29->31 und minus 10.07.98
                          
                          
                          xvt_slist_add_at_elt(prof_datei,NULL,help,0L);
                          break;//Dick 16.04.99 sonst Absturz
                        }
                      }
                      /**/
                      istverzweigt=FALSE;
                      teste_str_verzweigt(); //IN VERZWEIG.cpp
                      BOOL strang_vorwaerts = GetSortStrangVorwaerts();
                      if((istverzweigt==FALSE) &&(vzk[0]=='0') && (pk[0]=='0'))
                        sort_new_profil( &strang_anfang, &strang_ende, station208, vzk, pk, uebergabe_name, strang_vorwaerts );
                      else
                      {
                        if ( GetSortVerzweigt() )
                        {
                          if(!xvt_dlg_create_res(WD_MODAL,DLG_211, EM_ALL, DLG_211_eh, 0L))
                            xvt_dm_post_error("Can't open dialog 211");
                          verzweigtes_profil_sortieren( 0, strang_vorwaerts );
                        }
                        else
                          verzweigtes_profil_sortieren( 1, strang_vorwaerts );
                      }
                      if(alte_strang)//Dick 24.11.99
                        wandle_abstand_in_string(alte_strang);
                      else
                        wandle_abstand_in_string();
                      vergleich=TRUE;
                      /***/
                      //xvt_dm_post_note("Die Profile müssen über WSPWIN registriert sein");
                      //break;
                      
                    }
                    else
                      vergleich=FALSE;
                  }
                }
                if(!vergleich)
                {
                  istverzweigt=FALSE;
                  teste_str_verzweigt(); //IN VERZWEIG.cpp
                  BOOL strang_vorwaerts = GetSortStrangVorwaerts();
                  if((istverzweigt==FALSE) &&(vzk[0]=='0') && (pk[0]=='0'))
                    sort_new_profil( &strang_anfang, &strang_ende, station208, vzk, pk, uebergabe_name, strang_vorwaerts ); //in strang.cpp
                  else
                  {
                    if(!zustand_kopieren)
                    {
                      BOOL sortVerzweigt = GetSortVerzweigt();
                      if(!neukopieren)
                      {                                                                          
                        if ( sortVerzweigt )
                        {
                          if(!xvt_dlg_create_res(WD_MODAL,DLG_211, EM_ALL, DLG_211_eh, 0L))                                         
                            xvt_dm_post_error("Can't open dialog 211");                                     
                        }
                      }
                      if ( sortVerzweigt )
                        verzweigtes_profil_sortieren( 0, strang_vorwaerts );
                      else
                        verzweigtes_profil_sortieren( 1, strang_vorwaerts );
                    }
                    else
                    {
                      if(war_schon==FALSE)
                        alte_str_in_slist();
                      copy_old_to_new_strverweigt(quelle_ptr, neuer_dateiname); //in verwezig.cpp                          if(profile_zaehler==anzahl_profile)
                      if(profile_zaehler==anzahl_profile)
                      {
                        schreibe_slist_verzweigt();
                        xvt_slist_destroy(verzweigt_new);
                        copy_vzk_default_datei(quelle, ziel);  //verzweig.cpp
                      }
                      
                    }
                  }
                  
                  if(alte_strang)//Dick 24.11.99
                    wandle_abstand_in_string(alte_strang);
                  else
                    wandle_abstand_in_string();
                  if(!neukopieren)
                    haenge_zustand_an_zsd(zielname); //in profproj.cpp
                  else
                  {
                    strcpy(dateiname,neuer_dateiname);
                    teste_projekt_profile(1);
                  }
                } //if (!vergleich)
          }  //else Verzeichnisse gleich
          
          if((vergleich) && (neukopieren))
          {
            remove(kopierdatei);
            SLIST_ELT ee,e2;
            int hilfszaehler=0;
            int hilfszaehler2;
            for(ee=xvt_slist_get_first(prof_datei);
            ee!=NULL;ee=xvt_slist_get_next(prof_datei,ee))
            {
              if((e2=xvt_slist_get_next(prof_datei,ee))==NULL)
                //xvt_slist_rem(prof_datei,ee);
                hilfszaehler2=hilfszaehler;
              hilfszaehler++;
            }
            ee=xvt_slist_get_first(prof_datei);
            for(int i=0;i<hilfszaehler2;i++)
              ee=xvt_slist_get_next(prof_datei,ee);
            xvt_slist_rem(prof_datei,ee);						 
            anzahl_profil_dat_entries--;
          } //IF VERGLEICH UND NEUKOPIEREN
          
          if(!zustand_kopieren&& (!interp_prog || (interp_prog && dlg_136!=NULL) ) ) //8.2.99 bley //Dick 22.03.99
          {                         //8.2.99 bley
            xvt_list_clear(lwin);
            xvt_list_add(lwin, -1, (char*)prof_datei);   //Ausgabe in Listbox
            dlg136_get_daten(scroll_position);
            int numberslist=xvt_slist_count(prof_datei);//Dick 22.03.99
            if(numberslist>0)
            {
              xvt_list_set_sel(lwin, 0,  TRUE);
              // Listbox vorselektieren
              selektion(); //in aufnehm.cpp
            }
          }                         //8.2.99 bley
          else
          {
            if(war_schon==FALSE) //d.h. nicht verzweigt, damit nicht falsch übeschrieben
              save_str_datei();        //8.2.99 bley
          }
          vergleich=FALSE;
          
         } //!abbruch208
         
        } //else Stranganzahl überschritten
        
      } //for profil_list
   } //if back==1
   return IDOK;
  } //if back==1 aus auswahl
  
  if(profil_list!=NULL)
		{
    xvt_slist_destroy(profil_list);
    profil_list=NULL;
		}
  return IDCANCEL;
}
 /*************************************************************************/
 /*********************************************************************/
 void datei_namen_vergeben(void)
 {
   /******DATEINAMEN VERGEBEN*********************************/
   profil_nr_ermitteln();  //in profprofj.cpp
   for(int i=2;i<=7;i++)
     neuer_dateiname[i]='0';
   neuer_dateiname[0]=netz_dat[0][0];
   neuer_dateiname[1]=netz_dat[0][1];
   neuer_dateiname[8]='.';
   neuer_dateiname[9]='p';
   neuer_dateiname[10]='r';
   neuer_dateiname[11]='f';
   neuer_dateiname[12]='\0';
   //	strcat(neuer_dateiname,".prf");
   
   int back=0;
   while(back==0)
   {
     int j=7;
     for (i=strlen(profil_nr_string);i>=1;i--)
     {
       neuer_dateiname[j] = profil_nr_string[i-1];
       j--;
     }
     xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,kopierdatei,50);
     /****weil strcat nicht funktionierte****/
     int len1=strlen(kopierdatei);
     kopierdatei[len1]='\\';
     kopierdatei[len1+1]='\0';
     len1=strlen(kopierdatei);
     for(i=0;i<=((INT)strlen(neuer_dateiname));i++)
       kopierdatei[len1+i]=neuer_dateiname[i];
     //	  strcat(kopierdatei,"\\");
     //	  strcat(kopierdatei,neuer_dateiname);
     back=access(kopierdatei,00);
     if(back==0)
     {
       int tempint=atoi(profil_nr_string);
       tempint++;
       itoa(tempint,profil_nr_string,10);
     }
	  } //while back==0
   
   /*********ENDE DATEINAMEN VERGEBEN************/
   
 }
 /*******************************************************************/
void selektion(void)
// liest daten aus dem aktuell selektierten Eintrag von lwin
//
// Nebeneffekte:
//          Ändert: int dlg136_select
//                  FILE_SPEC file_spec
//                  char zustand[20]
//                  char vzk[20]
//                  char pk[20]
{
  SLIST selection;
  char *str_ptr;
  char prstr[100];

  selection = xvt_list_get_sel(lwin);
  
  dlg136_select = xvt_list_get_sel_index(lwin);
  
  str_ptr=xvt_slist_get_elt(selection,0,0);
  strcpy(prstr,str_ptr);
  
  if (strlen(prstr)>0)
  {
    for (int i=0;i<12;i++)
      file_spec.name[i] = prstr[i+44];
    file_spec.name[12]='\0';
    /******ZUSTAND****/
    int j=0;
    for (i=0;i<11;i++)
    {
      if(prstr[i+33]!=' ')
      {
        zustand[j] = prstr[i+33];
        j++;
      }
    }
    zustand[j]='\0';
    /********VZK********/
    j=0;
    for (i=0;i<4;i++)
    {
      if(prstr[i+29]!=' ')
      {
        vzk[j]=prstr[i+29];
        j++;
      }
    }
    vzk[j]='\0';
    /**********PK**************/
    j=0;
    for (i=0;i<10;i++)
    {
      if(prstr[19+i]!=' ')
      {
        pk[j]=prstr[19+i];
        j++;
      }
    }
    pk[j]='\0';
  }//if prstr>0
  
  xvt_slist_destroy(selection);
}
 
int auswahl2( char *szFiles, int multi, int stringtable )
// erfragt Dateinamen vom Benutzer per CommonFileDialog
// 
// Rückgabewert: IDOK falls Dateien ordentlich ausgewählt wurden, sonst IDCANCEL
// Parameter:
//        char* szFiles: hier werden die ausgewälten Dateien ( durch ' ' getrennt abgelegt )
//                        der buffer muss vorher schon ausreichend? speciher allokiert haben
//        int multi:  eigentlich BOOLEAN: falls TRUE ist mehrfachauswahl erlaubt
//        int stringtable: gibt an welche Dateiendungen erlaubt sind ( siehe switch Anweisung )
// Nebeneffekte: ?
{
  char szDirName[256];
  int j;
  UINT i;
  size_t buffer_groesse;
  char title[200];
  
 
  memset(&ofn,0,sizeof(OPENFILENAME));
  
  char buf[200],buf2[200],buf3[200],buf3_2[200],buf4[200], buf5[200];
  xvt_res_get_str(STR_PROF_DAT,buf,sizeof(buf));
  xvt_res_get_str(STR_ALL_DAT,buf2,sizeof(buf2));
  xvt_res_get_str(STR_LAENGS_DAT,buf3,sizeof(buf3));
  xvt_res_get_str(STR_AUSWAEHLEN,buf3_2,sizeof(buf3_2));
  xvt_res_get_str(STR_WSPD162_NOTE_3,title,sizeof(title));
  xvt_res_get_str(STR_START_WAEHLEN,buf5,sizeof(buf5));
  
  szDirName[0]='\0';
  
  switch(stringtable)
  {
  case 40001:
    j  = sprintf( buf4,"%s (*.prf)",buf);
    buf4[++j]='\0';
    j += sprintf( buf4 + j, "*.prf");
    buf4[++j]='\0';
    j += sprintf( buf4 + j, "%s (*.dat)",buf);
    buf4[++j]='\0';
    j += sprintf( buf4 + j, "*.dat");
    buf4[++j]='\0';
    j += sprintf( buf4 + j, "%s(*.*)",buf2);
    buf4[++j]='\0';
    j += sprintf( buf4 + j, "*.*");
    buf4[++j]='\0';
    break;
  case 40002:
    j  = sprintf( buf4,     "%s (*.wsl)",buf3);
    buf4[++j]='\0';
    j += sprintf( buf4 + j, "*.wsl");
    buf4[++j]='\0';
    j += sprintf( buf4 + j, "%s (*.prf)",buf);
    buf4[++j]='\0';
    j += sprintf( buf4 + j, "*.prf");
    buf4[++j]='\0';         
    break;
  case 40003:
    j  = sprintf( buf4,"%s (*.wsp)",buf);
    buf4[++j]='\0';
    j += sprintf( buf4 + j, "*.wsp");
    buf4[++j]='\0';         
    break;
  case 40004:
    xvt_res_get_str(STR_PROJEKT_DAT,buf,sizeof(buf));
    xvt_res_get_str(STR_PROJ_AUSWAHL,title,sizeof(title));
    j  = sprintf( buf4,"%s (wsp.prj)",buf);
    buf4[++j]='\0';
    j += sprintf( buf4 + j, "wsp.prj");
    buf4[++j]='\0';         
    break;
  case STR_START_DATEIEN:
    j = sprintf( buf4, "%s (*.dat)",buf5);
    buf4[++j]='\0';
    j += sprintf( buf4 + j, "*.dat");
    buf4[++j]='\0';
    j += sprintf( buf4 + j, "%s(*.*)",buf2);
    buf4[++j]='\0';
    j += sprintf( buf4 + j, "*.*");
    buf4[++j]='\0';
    strcpy(title,"Start-Datei auswählen");
    break;
  default:
    j  = sprintf( buf4,"%s (*.*)",buf2);
    buf4[++j]='\0';
    j += sprintf( buf4 + j, "*.*");                           
    buf4[++j]='\0';
    break;
  }

  // Buffer für die Dateinamen
  char* files = new char[AUFNEHM_BUFFER_SIZE];
  memset( files, '\0', AUFNEHM_BUFFER_SIZE );//=Voraussetzung für for-Schleife 

  ofn.lpstrFilter=buf4;                              
  ofn.lStructSize = sizeof(OPENFILENAME);
  ofn.hwndOwner = (HWND)xvt_vobj_get_attr(Menu_Win,ATTR_NATIVE_WINDOW);
  ofn.lpstrFile = files;
  ofn.nMaxFile = AUFNEHM_BUFFER_SIZE - 1;
  ofn.lpstrFileTitle=NULL;
  ofn.nMaxFileTitle = 0;
  ofn.lpstrTitle =title;//"Profildateien auswählen";
  ofn.lpstrInitialDir =szDirName;
  ofn.lpstrDefExt = "prf" ;
  if (multi)
    ofn.Flags = OFN_ALLOWMULTISELECT | OFN_FILEMUSTEXIST | OFN_PATHMUSTEXIST 
    | OFN_HIDEREADONLY|OFN_EXPLORER;
  else
    ofn.Flags = OFN_FILEMUSTEXIST | OFN_PATHMUSTEXIST | OFN_HIDEREADONLY | OFN_EXPLORER ;
  
  
  if( GetOpenFileName(&ofn) == 0)
  {
    if (CommDlgExtendedError()==(DWORD)FNERR_BUFFERTOOSMALL)
    {
      //xvt_dm_post_note("Zu viele Dateien ausgewählt !");
      char buf[200];//Dick 26.11.99
      xvt_res_get_str(STR_BUFFERTOOSMALL,buf,sizeof(buf));
      xvt_dm_post_note("%s",buf);
    }
    buffer_groesse=sizeof(ofn.lpstrFile);
    
    ofn.lpstrFile[0]='\0';
    szFiles[0]='\0';
    delete[] files;
    return IDCANCEL;
  }
  else
  { 
  /* In ofn.lpstrFile steht der gewählte Pfad und die gewählte(n) Datei(en) 
  durch '\0' getrennt.
  Nachfolgende for-Schleife dient dazu aus ofn.lpstrFile einen kompletten 
  String zu machen.
  Voraussetzung: ofn.lpstrFile wurde mit 0 initialisiert(:=memset(files,'\0',256))   */
    for( i = 0; i < AUFNEHM_BUFFER_SIZE - 1; i++ )
    {
      if((files[i]=='\0')&&(files[i+1]!='\0'))
        files[i]=' ';
    }
    strcpy(szFiles,files);
    delete[] files;
    return IDOK;
  }
}
 
