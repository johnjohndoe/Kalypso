/* Lese_str.cpp */

#include <windows.h>
#include "xvt.h"


#include "resource.h"

#include "global_types.h"
#include "global_vars.h"

#include "wspwin.h"
#include "util2.h"
#include "profpro2.h"
#include "qlist.h"
#include "..\..\wspdlg\Include\export.h"

#include "lese_str.h"


SLIST stations_liste, abfluss_liste, muss_liste, kann_liste, prof_slist;
char type[9], zeile2[200], info_hq[10];

int schleife = 0; //test=0 
BOOLEAN blank_true = FALSE, kalmin_wahr = FALSE, nasabs_wahr = FALSE, konvert = TRUE;


st_daten steuerdaten;
extern SLIST neue_str_list;
extern char kon_verzeichnis[100], neue_str[13];
TABELLE* nasim_anfang = NULL, *nasim_ptr, *nasim_ende; 

extern long timer_main;
extern BOOLEAN timermain;
extern BOOLEAN aendern;

// vorwärtsdeklarationen
void abfluss_datei_schreiben(void);
void typ_testen(void);
void steuerdaten_besetzen(void);
void oeffne_kann_liste(void);


/*********************************************************************/
/***DIESE DATEI ENTHÄLT FUNKTIONEN ZUR KONVERTIERUNG LWA-FORMAT*******/


void lese_str(void)
{
  char abfluss_string[10];
  char stations_wert[9];
  char stations_wert2[9];
  char kon_buffer2[13];
  char datei_anfang2[13];
  char datei_ende2[13];
  
  char buf[200];
  xvt_res_get_str(STR_LESE_STR_NOTE,buf,sizeof(buf));
  xvt_dm_post_note("%s",buf); //" Hinweis: Zur Zeit wird nur der erste Abflußzustand übernommen.");
  
  char dat_anf_mit_verz[100], profil_str[100], dat_end_mit_verz[100], help[100], randbedingung[20],
    vergleich[9], abfluss_string_alt[10];
  
  FILE *profil_file, *str_in;
  char *kon_ptr, *str_ptr;
  int i, j, len, zaehler, i_comp=1, j_comp=0, anzahl_profil_list=0;
  
  char alteverzweigungsdatei[100];
  char neueverzweigungsdatei [100];
  /************************************/
  
  Init_Steuerdaten_LWA(&steuerdaten);//Dick 19.07.99 Initialisierung
  
  anzahl_profil_list=xvt_slist_count(profil_list);
  for (schleife=1;schleife<=anzahl_profil_list;schleife++)
  {
    kon_ptr = xvt_slist_get_elt(profil_list,schleife-1,0);
    str_ptr = xvt_slist_get_elt(neue_str_list,schleife-1,0);
    strcpy(kon_buffer2,kon_ptr);
    strcpy(neue_str,str_ptr);
    xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, alteverzweigungsdatei, 50);
    strcat(alteverzweigungsdatei,"\\");
    char help_kon_ptr[15];
    strcpy(help_kon_ptr,kon_buffer2);
    char* helpString = strchr( help_kon_ptr, '.' ); // Punkt finden
    if( helpString )
      helpString[0] = '\0'; // die Endung abschneiden
    help_kon_ptr[5]='\0'; // aber dennoch maximal 5 Zeichen erlauben
    int laenge = strlen( help_kon_ptr );
    if( laenge < 5 )
    {
      for(int hilf=laenge;hilf<5;hilf++)
        help_kon_ptr[hilf]='_';
    }
    strcat(help_kon_ptr,".zvk");
    strcat(alteverzweigungsdatei,help_kon_ptr);
    
    xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, neueverzweigungsdatei, 50);
    strcat(neueverzweigungsdatei,"\\");
    char help_neue_str[15];
    strcpy(help_neue_str,neue_str);
    help_neue_str[strlen(help_neue_str)-4]='\0';
    strcat(help_neue_str,".vzk");
    strcat(neueverzweigungsdatei,help_neue_str);
    
    int vzktestda=access(alteverzweigungsdatei,4);
    if(vzktestda==0)
    {
      copy_fileb(alteverzweigungsdatei,neueverzweigungsdatei);
      DeleteFile( alteverzweigungsdatei ); // und löschen
    }; // if vzktestda
    
    /***24.3.97***********/
    /***neu 21.2.96***/
    len=strlen(kon_buffer2);
    if(len<9)
    {
      for (j=0; j<=len;j++)
      {
        if(kon_buffer2[j]=='.')
          kon_buffer2[j]='\0';
      }
      len=strlen(kon_buffer2);
      for(j=len;j<5;j++)
        kon_buffer2[j]='_';
      
    }
    
    /*****************/
    strcpy(profil_str,kon_verzeichnis);
    len=strlen(profil_str);
    for (i=0;i<5;i++)
      profil_str[len+i]=kon_buffer2[i];
    profil_str[len+i]='\0';
    strcat(profil_str,".str");
    
    if ( (str_in=fopen(profil_str,"r"))==NULL)
    {
      //xvt_dm_post_note("Datei %s kann nicht geöffnet werden", profil_str);
      char buf[200],buf2[200];//Dick 26.11.99
      xvt_res_get_str(STR_DATEI,buf,sizeof(buf));
      xvt_res_get_str(STR_CANOTOPEN,buf2,sizeof(buf2));
      xvt_dm_post_note("%s%s%s",buf,profil_str,buf2);
      exit(-1);
    }
    zaehler=1;
    for(i=0;i<7;i++) /*hat Knauf am 17.8 auf 7 Zeilen erhöht*/
    {
      zeile2[0]='\0';
      fgets(zeile2,140,str_in);
      /****neu 17.9.96***********/
      if (i==1)
      {
        schreibe_projektbezeichnung(zeile2);
      }
      if(i==3)
      {
        zeile2[57]='\0';
        strcpy(steuerdaten.info,zeile2);
      }
      /****ende neu 17.9.96*********/
      if((i==4) && (zeile2[0]!='\0'))
      {
        steuerdaten.epsh[0]='\0';
        steuerdaten.epsv[0]='\0';
        steuerdaten.rny[0]='\0';
        steuerdaten.cwr[0]='\0';
        
        sscanf(zeile2,"%d %d %d %d %d %d %d %d",
          &steuerdaten.ia,&steuerdaten.nhyd,&steuerdaten.ncar,
          &steuerdaten.idat,&steuerdaten.iauto,&steuerdaten.nfrou,
          &steuerdaten.ifp, &steuerdaten.idr);
        
      }
      if ((i==5) && (zeile2[0]!='\0'))
      {
        char temp1[20],temp2[20],temp3[20],temp4[20],temp5[20];
        temp1[0]=temp2[0]=temp3[0]=temp4[0]=temp5[0]='\0';
        sscanf(zeile2,"%s %s %s %s",steuerdaten.epsh,steuerdaten.epsv,
          steuerdaten.rny,steuerdaten.cwr,
          temp1,temp2,temp3,temp4,temp5);
        if(steuerdaten.epsh[0]=='\0')
          strcpy(steuerdaten.epsh,"0.005");
        if(steuerdaten.epsv[0]=='\0')
          strcpy(steuerdaten.epsv,"0.01");
        if(steuerdaten.rny[0]=='\0')
          strcpy(steuerdaten.rny,"1.31");
        if(steuerdaten.cwr[0]=='\0')
          strcpy(steuerdaten.cwr,"1.5");
        //Dick 28.09.99
        if(temp1[0]=='\0')
          steuerdaten.dhwmax=2.0;
        else
          steuerdaten.dhwmax=atof(temp1);
        
        if(temp2[0]=='\0')
          steuerdaten.vfmax=8.0;
        else
          steuerdaten.vfmax=atof(temp2);
        
        if(temp3[0]=='\0')
          steuerdaten.hzvmax=1.0;
        else
          steuerdaten.hzvmax=atof(temp3);
        
        if(temp4[0]=='\0')
          steuerdaten.faklhg=5.0;
        else
          steuerdaten.faklhg=atof(temp4);
        
        if(temp5[0]=='\0')
          steuerdaten.ffmax=5000.0;
        else
          steuerdaten.ffmax=atof(temp5);
        //
      }
    }
    
    i_comp=1;
    
    stations_liste=xvt_slist_create();
    abfluss_liste=xvt_slist_create();
    abfluss_string_alt[0]='\0';
    if(nasim_anfang!=NULL)
      destroy_tabelle();
    
    if(nasim_anfang==NULL)
    {
      nasim_anfang=MakeNewTabelle(100);
      nasim_ptr=nasim_anfang;
    }
    
    while(i_comp!=0)
    {
      zeile2[0]='\0';
      vergleich[0]='\0';
      fgets(zeile2,190,str_in);                                    //zeile2 LESEN
      for (i=0;i<7;i++)
        vergleich[i]=zeile2[i];
      randbedingung[0]='\0';
      strcat(randbedingung,"RANDBED");                      // ? RANDBEDINGUNG
      i_comp=strncmp(vergleich,randbedingung,7);
      
      if (i_comp!=0)
      {
        datei_anfang2[0]='\0';                                //ANFANGSSTATION
        for(i=9;i<21;i++)
          datei_anfang2[i-9]=zeile2[i];
        datei_anfang2[12]='\0';
        
        dat_anf_mit_verz[0]='\0';
        strcat(dat_anf_mit_verz, kon_verzeichnis);
        strcat(dat_anf_mit_verz, datei_anfang2);
        
        profil_file=fopen(dat_anf_mit_verz,"r");        //PROFILDATEI OEFFNEN
        for (i=0; i<8;i++)
        {
          help[0]='\0';
          fgets(help,100,profil_file);
        }
        help[0]='\0';
        fscanf(profil_file,"%s",help); //"Station"
        fscanf(profil_file,"%s",help); //"km"
        fscanf(profil_file,"%s",stations_wert);             //STATIONSWERT
        strcat(stations_wert,"\0");
        stations_wert[strlen(stations_wert)]='\0';
        fclose(profil_file);
        
        if (zaehler==1)                                           //ST.ANFANG
          steuerdaten.anfang=atof(stations_wert);
        datei_ende2[0]='\0';
        for (i=23;i<35;i++)
          datei_ende2[i-23]=zeile2[i];
        datei_ende2[12]='\0';
        
        /**************Stationswert Endstation holen********/
        dat_end_mit_verz[0]='\0';
        strcat(dat_end_mit_verz, kon_verzeichnis);
        strcat(dat_end_mit_verz, datei_ende2);
        
        profil_file=fopen(dat_end_mit_verz,"r");        //PROFILDATEI OEFFNEN
        for (i=0; i<8;i++)
        {
          help[0]='\0';
          fgets(help,100,profil_file);
        }
        help[0]='\0';
        fscanf(profil_file,"%s",help); //"Station"
        fscanf(profil_file,"%s",help); //"km"
        fscanf(profil_file,"%s",stations_wert2);             //STATIONSWERT
        strcat(stations_wert2,"\0");
        stations_wert2[strlen(stations_wert2)]='\0';
        fclose(profil_file);
        /****************************************************/
        if((zeile2[77]=='K')||(zeile2[78]=='K'))     //30.3.99
        {
          nasim_ptr->anfang=atof(stations_wert);
          nasim_ptr->ende=atof(stations_wert2);
          nasim_ptr=nasim_ptr->next;
        }
        kalmin_wahr=FALSE;
        nasabs_wahr=FALSE;
        /*******ABFLUSSDATEI******/
        j=0;
        //			for (i=68;i<76;i++)                            // SPÄTER
        for (i=61;i<69;i++)
        {
          if (zeile2[i]!=' ')
          {
            abfluss_string[j]=zeile2[i];
            j++;
          }
        }
        abfluss_string[j]='\0';
        if (zaehler==1)
        {
          xvt_slist_add_at_elt(abfluss_liste,NULL,abfluss_string,0);
          xvt_slist_add_at_elt(stations_liste,NULL,stations_wert,0);
          strcpy(abfluss_string_alt,abfluss_string);
        }
        else
        {
          j_comp=strcmp(abfluss_string, abfluss_string_alt);
          if (j_comp!=0)
          {
            xvt_slist_add_at_elt(abfluss_liste,NULL,abfluss_string,0);
            xvt_slist_add_at_elt(stations_liste,NULL,stations_wert,0);
          }
          strcpy(abfluss_string_alt,abfluss_string);
        }
        
        /*************************/
      } //IF I_COMP !=0
      zaehler++;
    } //WHILE
    
    steuerdaten.ende=atof(stations_wert2);                            //ST.ENDE
    
    fgets(zeile2,140,str_in);                                        // CCZEILE
    
    muss_liste=xvt_slist_create();
    kann_liste=xvt_slist_create();
    
    while(!feof(str_in))
    {
      fgets(zeile2,140,str_in);
      if (!feof(str_in))
      {
        for (i=25;i<=31;i++)
        {
          if (zeile2[i]!=' ')
            type[i-25]=zeile2[i];
          else
            type[i-25]='\0';
        }
        
        if((type[3]=='-') || (type[5]=='-'))
          steuerdaten.schiess=1;
        else
          steuerdaten.schiess=0;
        type[6]='\0';
        typ_testen();
      } //IF !FEOF
      
    } // WHILE !FEOF
    
    fclose(str_in);
    abfluss_datei_schreiben();
    steuerdaten_besetzen();
    if (stations_liste!=NULL)
    {
      xvt_slist_destroy(stations_liste);
      stations_liste=NULL;
    }
    if (abfluss_liste!=NULL)
    {
      xvt_slist_destroy(abfluss_liste);
      abfluss_liste=NULL;
    }
    if (muss_liste!=NULL)
    {
      xvt_slist_destroy(muss_liste);
      muss_liste=NULL;
    }
    if (kann_liste!=NULL)
    {
      xvt_slist_destroy(kann_liste);
      kann_liste=NULL;
    }
 } // FOR SLIST ELEM
 
 if (profil_list!=NULL)
 {
   xvt_slist_destroy(profil_list);
   profil_list=NULL;
 }
 SLIST_ELT ee;
 for (ee=xvt_slist_get_first(neue_str_list);ee!=NULL;
 ee=xvt_slist_get_next(neue_str_list,ee))
 {
			char *help_ptr=xvt_slist_get(neue_str_list,ee,0L);
      strcpy(STR_SPEC.name,help_ptr);
      profile_aus_str_ermitteln(); //in lese_str.cpp
      if((anzahl_profil_dat_entries<2) || (anzahl_strang_entries<1))
      {
        char *abfluss_datei;//char abfluss_datei[150];
        abfluss_datei =  new char[150];
        
        strcpy(abfluss_datei,kon_verzeichnis);
        strcat(abfluss_datei,help_ptr);
        abfluss_datei[strlen(abfluss_datei)-3]='\0';
        strcat(abfluss_datei,"qwt");
        if((access(abfluss_datei,00))==0)
          remove(abfluss_datei);    //Abflussdatei löschen
        abfluss_datei[strlen(abfluss_datei)-3]='\0';
        strcat(abfluss_datei,"ber");
        if((access(abfluss_datei,00))==0)
          remove(abfluss_datei); // Berechnungsvariantenverknüpfungsdatei löschen
        abfluss_datei[strlen(abfluss_datei)-3]='\0';
        strcat(abfluss_datei,"psi");
        if((access(abfluss_datei,00))==0)
          remove(abfluss_datei); // Berechnungsvariantenverknüpfungsdatei löschen
        
        delete[] abfluss_datei;
      }
      profile_nach_profproj_schreiben(); //in profpro2.cpp
 }
 
 if (neue_str_list!=NULL)
 {
   xvt_slist_destroy(neue_str_list);
   neue_str_list=NULL;
 }
 
 //Neu Dick 14.06.99
 
 strcpy(Gewaessername_aktuell,netz_dat[0]);
 strcpy(Zustand_aktuell,netz_dat[2]);

 setTitle( Projektname_aktuell, Gewaessername_aktuell, Zustand_aktuell );


 
 strcpy(help,"error.log");
 pruefe_errorlog( help ); //in util2.cpp
 
 xvt_timer_destroy(timer_main);
 timermain=FALSE;
 
} //FUNKTION

/****************************************************************************/
void abfluss_datei_schreiben(void)
{
  char *abfluss_datei,//char abfluss_datei[100];
    *abfluss_ptr,
    *station_ptr,
    *abfluss_string2,
    *stations_wertb;
  FILE *abfluss_file;
  int zaehler_hq=0, zaehler_slist=0, i=0;
  /*******************************/
  abfluss_string2=new char[10];
  abfluss_datei =  new char[100];
  stations_wertb=new char[9];
  int anzahl_werte=xvt_slist_count(abfluss_liste);
  if(anzahl_werte>0)
  {
    abfluss_datei[0]='\0';
    strcat(abfluss_datei,kon_verzeichnis);
    strcat(abfluss_datei,neue_str);
    
    abfluss_datei[strlen(abfluss_datei)-3]='\0';
    strcat(abfluss_datei,"qwt");
    
    abfluss_file=fopen(abfluss_datei,"w");
    zaehler_hq=1;
    info_hq[0]='\0';
    itoa(zaehler_hq,info_hq,10);
    strcat(info_hq,"HQ"); //DAMIT MIN 2 BUCHSTABEN MUSS SEIN
    zaehler_slist=xvt_slist_count(abfluss_liste);
    fprintf(abfluss_file,"%s ",info_hq);
    fprintf(abfluss_file,"%d\n",zaehler_slist);
    abfluss_string2[0]='\0';
    stations_wertb[0]='\0';
    for (i=0;i<zaehler_slist;i++)
    {
      station_ptr=xvt_slist_get_elt(stations_liste,i,NULL);
      strcpy(stations_wertb,station_ptr);
      stations_wertb[strlen(stations_wertb)]='\0';
      fprintf(abfluss_file,"%s ",stations_wertb);
      abfluss_ptr=xvt_slist_get_elt(abfluss_liste,i,NULL);
      strcpy(abfluss_string2,abfluss_ptr);
      abfluss_string2[strlen(abfluss_string2)]='\0';
      fprintf(abfluss_file,"%s\n",abfluss_string2);
    }
    
    fclose(abfluss_file);
  }
  
  delete[] abfluss_datei;
  delete[] abfluss_string2;
  delete[] stations_wertb;
} //FUNKTION

/*******************************************************************/
void typ_testen(void)
{
  char *gebe_in_slist;//char gebe_in_slist[35];
  int i, j;
  
  gebe_in_slist = new char[35];
  
  if ((type[0]=='E') ||
    //	  ((type[0]=='H') && (type[1]=='G') && (type[5]=='Z')) ||
    ((type[0]=='H') && (type[1]=='G') && (type[4]=='N')) ||
    ((type[0]=='H') && (type[1]=='N') && (type[5]=='\0')) ||
    ((type[0]=='W') && (type[1]=='S')))
  {
    gebe_in_slist[0]='\0';
    strcat (gebe_in_slist, type);
    strcat (gebe_in_slist, " ");
    j=strlen(gebe_in_slist);
    for (i=51; i<=74;i++)
    {
      gebe_in_slist[j]=zeile2[i];
      j++;
    }
    gebe_in_slist[j]='\0';
    xvt_slist_add_at_elt(muss_liste,NULL,gebe_in_slist,0);
  }
  
  if (((type[0]=='H') && (type[1]=='G') && (type[5]=='L')) ||
    ((type[0]=='H') && (type[1]=='N') && (type[5]=='A')) ||
    ((type[0]=='W') && (type[1]=='E')) ||
    ((type[0]=='N') && (type[5]=='L')) ||
    (type[1]=='P') ||
    (type[1]=='L') ||
    ((type[0]=='W') && (type[1]=='T')))
  {
    gebe_in_slist[0]='\0';
    strcat (gebe_in_slist, type);
    xvt_slist_add_at_elt(kann_liste, NULL, gebe_in_slist,0);
  }
  if ((type[0]=='W') && (type[1]=='Q'))
  {
    gebe_in_slist[0]='\0';
    strcat (gebe_in_slist,type);
    strcat (gebe_in_slist," ");
    j=strlen(gebe_in_slist);
    for (i=51; i<=74;i++)
    {
      gebe_in_slist[j]=zeile2[i];
      j++;
    }
    gebe_in_slist[j]='\0';
    xvt_slist_add_at_elt(kann_liste,NULL,gebe_in_slist,0);
  }
  
  if (type[0]=='K')
    kalmin_wahr=TRUE;
  
  if ((type[0]=='N') && (type[5]=='S'))
    nasabs_wahr=TRUE;
  
  delete[] gebe_in_slist;
} //FUNKTION

/*******************************************************************/
void steuerdaten_besetzen(void)
{
  int anzahl_liste, i, j, z,x;
  char *char_ptr;
  BOOLEAN blank_true=FALSE;
  
  /*char text[35];
  hilfs_text[35]; */
  char *text,
    *hilfs_text;
  
  text      = new char[35];
  hilfs_text= new char[35];
  
  
  anzahl_liste=xvt_slist_count(muss_liste);
  if (anzahl_liste>0)
  {
    for (x=1;x<=anzahl_liste;x++)
    {
      char_ptr=xvt_slist_get_elt(muss_liste,x-1,NULL);
      strcpy(text,char_ptr);
      strcat(text,"\0");
      text[strlen(text)]='\0';
      
      if (text[0]=='E')                                //EICHFL-EICHVO
      {
        //steuerdaten.info[0]='\0';
        steuerdaten.eich[0]='\0';
        if (text[4]=='F')
          strcat(steuerdaten.eich,"EICHFL");
        if (text[4]=='V')
          strcat(steuerdaten.eich,"EICHVO");
        i=0;
        blank_true=FALSE;
        while (blank_true==FALSE)  //type UEBERLESEN
        {
          if (text[i]!=' ')
            blank_true=FALSE;
          if (text[i]==' ')
            blank_true=TRUE;
          i++;
        }
        
        for (z=0;z<=7;z++,i++)
          hilfs_text[z]=text[i];
        
        for (z=0,j=0;z<=7;z++)
        {
          if (hilfs_text[z]!=' ')
          {
            steuerdaten.hoehe[j]=hilfs_text[z];
            j++;
          }
        }
        steuerdaten.hoehe[j]='\0';
        for (z=0;z<=7;z++,i++)
          hilfs_text[z]=text[i];
        
        for (z=0,j=0;z<=7;z++)
        {
          if (hilfs_text[z]!=' ')
          {
            steuerdaten.he[j]=hilfs_text[z];
            j++;
          }
        }
        steuerdaten.he[j]='\0';
        steuerdaten.wasser[0]='\0';
        strcat(steuerdaten.wasser,"WSP");
        steuerdaten.gefaelle[0]='\0';
        steuerdaten.sel_index=0;
        steuerdaten.q[0]='\0';
        strcat(steuerdaten.q,info_hq);
      } // IF EICHFL
      
      //		if ((text[0]=='H') && (text[1]=='G') && (text[5]=='Z'))    //HGRENZ
      {
        if ((text[0]=='H') && (text[1]=='G') && (text[3]=='E'))
          //steuerdaten.info[0]='\0';
          steuerdaten.eich[0]='\0';
        steuerdaten.he[0]='\0';
        steuerdaten.wasser[0]='\0';
        strcat(steuerdaten.wasser,"HGRENZ");
        steuerdaten.hoehe[0]='\0';
        steuerdaten.gefaelle[0]='\0';
        steuerdaten.sel_index=0;
        steuerdaten.q[0]='\0';
        strcat(steuerdaten.q,info_hq);
      } // HGRENZ
      
      if ((text[0]=='H') && (text[1]=='N') && (text[5]!='A'))    //HNORM
      {
        //steuerdaten.info[0]='\0';
        steuerdaten.eich[0]='\0';
        steuerdaten.he[0]='\0';
        steuerdaten.wasser[0]='\0';
        strcat(steuerdaten.wasser,"HNORM");
        steuerdaten.hoehe[0]='\0';
        i=0;
        blank_true=FALSE;
        while (blank_true==FALSE)  //type UEBERLESEN
        {
          if (text[i]!=' ')
            blank_true=FALSE;
          if (text[i]==' ')
            blank_true=TRUE;
          i++;
        }
        
        for (z=0;z<=7;z++,i++)
          hilfs_text[z]=text[i];
        
        for (z=0;z<=7;z++,i++)
          hilfs_text[z]=text[i];
        
        for (z=0;z<=7;z++,i++)
          hilfs_text[z]=text[i];
        
        for (z=0,j=0;z<=7;z++)
        {
          if (hilfs_text[z]!=' ')
          {
            steuerdaten.gefaelle[j]=hilfs_text[z];
            j++;
          }
        }
        steuerdaten.gefaelle[j]='\0';
        
        steuerdaten.sel_index=0;
        steuerdaten.q[0]='\0';
        strcat(steuerdaten.q,info_hq);
      } //HNORM
      
      
      if ((text[0]=='W') && (text[1]=='S'))                   //WSP
      {
        //steuerdaten.info[0]='\0';
        steuerdaten.eich[0]='\0';
        steuerdaten.he[0]='\0';
        steuerdaten.wasser[0]='\0';
        strcat(steuerdaten.wasser,"WSP");
        steuerdaten.gefaelle[0]='\0';
        i=0;
        blank_true=FALSE;
        while (blank_true==FALSE)  //type UEBERLESEN
        {
          if (text[i]!=' ')
            blank_true=FALSE;
          if (text[i]==' ')
            blank_true=TRUE;
          i++;
        }
        
        for (z=0;z<=7;z++,i++)
          hilfs_text[z]=text[i];
        
        for (z=0,j=0;z<=7;z++)
        {
          if (hilfs_text[z]!=' ')
          {
            steuerdaten.hoehe[j]=hilfs_text[z];
            j++;
          }
        }
        steuerdaten.hoehe[j]='\0';
        
        steuerdaten.sel_index=0;
        steuerdaten.q[0]='\0';
        strcat(steuerdaten.q,info_hq);
      }
      
      oeffne_kann_liste();
      steuerdaten.kalmin[0]='\0';
      steuerdaten.nasabs[0]='\0';
      
      if (x==1)
      {
        if (kalmin_wahr)
        {
          steuerdaten.kalmin[0]='\0';
          strcat(steuerdaten.kalmin,"KALMIN");
        }
        if (nasabs_wahr)
        {
          steuerdaten.nasabs[0]='\0';
          strcat(steuerdaten.nasabs,"NASABS");
        }
      }
      
      if (x!=1)
      {
        if(nasim_anfang!=NULL)
          destroy_tabelle();
        MakeNewTabelle(100);
        steuerdaten.nasabs[0]='\0';
        steuerdaten.kalmin[0]='\0';
      }
      steuerdaten.qwv[0]='\0';
      strcpy(STR_SPEC.name,neue_str);
      konvert=TRUE;
      aendern=FALSE;//Dick 19.07.99
      savedat(&steuerdaten);
      
      if(nasim_anfang!=NULL)
        destroy_tabelle();
      
   } //FOR ANZAHL LISTE
  } //IF > 0
  
  delete[] text;
  delete[] hilfs_text;
} //FUNKTION

/*******************************************************************/
void oeffne_kann_liste(void)
{
  int anzahl_elem, i,z;
  char *list_ptr;
  BOOLEAN a,b,c,d,e,f,g,h,j,k;
  SLIST_ELT element;
  
  // char list_char[35], hilfs_text[35];
  char *list_char,
    *hilfs_text;
  
  list_char  =new char[35];
  hilfs_text =new char[35];
  
  anzahl_elem=xvt_slist_count(kann_liste);
  a=FALSE;
  b=FALSE;
  c=FALSE;
  d=FALSE;
  e=FALSE;
  f=FALSE;
  g=FALSE;
  h=FALSE;
  j=FALSE;
  k=FALSE;
  
  if (anzahl_elem>0)
  {
    for(element=xvt_slist_get_first(kann_liste);element!=NULL;element=xvt_slist_get_next(kann_liste,element))
    {
      if (anzahl_elem>0)
      {
        list_ptr=xvt_slist_get(kann_liste,element,0);
        strcpy(list_char,list_ptr);
        list_char[strlen(list_char)]='\0';
        strcat(list_char,"\0");
        
        if ((list_char[0]=='H') && (list_char[1]=='G'))
        {
          if (!a)
          {
            steuerdaten.hgralle[0]='\0';
            strcat(steuerdaten.hgralle,"HGRALL");
            a=TRUE;
            xvt_slist_rem(kann_liste,element);
            anzahl_elem=xvt_slist_count(kann_liste);
          }
        }
        if ((list_char[0]=='H') && (list_char[1]=='N'))
        {
          if (!b)
          {
            steuerdaten.normalle[0]='\0';
            strcat(steuerdaten.normalle,"HNORMA");
            b=TRUE;
            xvt_slist_rem(kann_liste,element);
            anzahl_elem=xvt_slist_count(kann_liste);
          }
        }
        if ((list_char[0]=='W') && (list_char[1]=='E'))
        {
          if (!c)
          {
            steuerdaten.wehran[0]='\0';
            strcat(steuerdaten.wehran,"WEHRAN");
            c=TRUE;
            xvt_slist_rem(kann_liste,element);
            anzahl_elem=xvt_slist_count(kann_liste);
          }
        }
        if ((list_char[0]=='N') && (list_char[1]=='A'))
        {
          if (!d)
          {
            steuerdaten.nasall[0]='\0';
            strcat(steuerdaten.nasall,"NASALL");
            d=TRUE;
            xvt_slist_rem(kann_liste,element);
            anzahl_elem=xvt_slist_count(kann_liste);
          }
        }
        if ((list_char[0]=='L') && (list_char[1]=='P'))
        {
          if (!e)
          {
            steuerdaten.lplot[0]='\0';
            strcat(steuerdaten.lplot,"LPLOT");
            e=TRUE;
            xvt_slist_rem(kann_liste,element);
            anzahl_elem=xvt_slist_count(kann_liste);
          }
        }
        if ((list_char[0]=='Q') && (list_char[1]=='P'))
        {
          if (!f)
          {
            steuerdaten.qplot[0]='\0';
            strcat(steuerdaten.qplot,"QPLOT");
            f=TRUE;
            xvt_slist_rem(kann_liste,element);
            anzahl_elem=xvt_slist_count(kann_liste);
          }
        }
        if ((list_char[0]=='Q') && (list_char[1]=='L'))
        {
          if (!g)
          {
            steuerdaten.qplot[0]='\0';
            strcat(steuerdaten.qplot,"QPLOT");
            steuerdaten.lplot[0]='\0';
            strcat(steuerdaten.lplot,"LPLOT");
            g=TRUE;
            xvt_slist_rem(kann_liste,element);
            anzahl_elem=xvt_slist_count(kann_liste);
          }
        }
        if ((list_char[0]=='W') && (list_char[1]=='T'))
        {
          if (!h)
          {
            steuerdaten.wtau[0]='\0';
            strcat(steuerdaten.wtau,"WTAU");
            h=TRUE;
            xvt_slist_rem(kann_liste,element);
            anzahl_elem=xvt_slist_count(kann_liste);
          }
        }
        
        if ((list_char[0]=='W') && (list_char[1]=='Q'))
        {
          if (!k)
          {
            steuerdaten.wqbez[0]='\0';
            strcat(steuerdaten.wqbez,"WQBEZ");
            k=TRUE;
            blank_true=FALSE;
            i=0;
            while (blank_true==FALSE)  //type UEBERLESEN
            {
              if (list_char[i]!=' ')
                blank_true=FALSE;
              if (list_char[i]==' ')
                blank_true=TRUE;
              i++;
            }
            
            for (z=0;z<=7;z++,i++)
              hilfs_text[z]=list_char[i];
            
            for (z=0,j=0;z<=7;z++)
            {
              if (hilfs_text[z]!=' ')
              {
                steuerdaten.qmin[j]=hilfs_text[z];
                j++;
              }
            }
            steuerdaten.qmin[j]='\0';
            
            for (z=0;z<=7;z++,i++)
              hilfs_text[z]=list_char[i];
            
            for (z=0,j=0;z<=7;z++)
            {
              if (hilfs_text[z]!=' ')
              {
                steuerdaten.qstep[j]=hilfs_text[z];
                j++;
              }
            }
            steuerdaten.qstep[j]='\0';
            
            for (z=0;z<=7;z++,i++)
              hilfs_text[z]=list_char[i];
            
            for (z=0,j=0;z<=7;z++)
            {
              if (hilfs_text[z]!=' ')
              {
                steuerdaten.qmax[j]=hilfs_text[z];
                j++;
              }
            }
            steuerdaten.qmax[j]='\0';
            
            xvt_slist_rem(kann_liste,element);
            anzahl_elem=xvt_slist_count(kann_liste);
          }
        }
        
        
   } //IF ANZAHL>0
   } //FOR
  } //IF ANZAHL>0
  
  delete[] list_char;
  delete[] hilfs_text;
 } //FUNKTION
 
 /*****************************************************************/
 void profile_aus_str_ermitteln(void)
 {
   //DIE FUNKTION PACKT DIE PROFILTABELLE EINER KONVERTIERTEN STR-DATEI
   //IN EINE SLIST
   //AUFRUF AUS WSPWIN.cpp -TIMER BEI KONVERTIERE AUS FKT. LESE_STR
   
   FILE *str_file;
   int i;
   unsigned int j;
   /*	char str_datei[100];
   char help[120];*/
   
   char *str_datei,
     *help;
   str_datei = new char[100];
   help = new char[120];
   
   xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str_datei,50);
   strcat(str_datei,"\\");
   strcat(str_datei,STR_SPEC.name);
   
   if(prof_slist!=NULL)
   {
     xvt_slist_destroy(prof_slist);
     prof_slist=NULL;
   }
   
   prof_slist=xvt_slist_create();
   if ( (str_file=fopen(str_datei,"r"))==NULL)
   {
     //	xvt_dm_post_note("Datei %s kann nicht geöffnet werden", str_datei);
     char buf[200],buf2[200];//Dick 26.11.99
     xvt_res_get_str(STR_DATEI,buf,sizeof(buf));
     xvt_res_get_str(STR_CANOTOPEN,buf2,sizeof(buf2));
     xvt_dm_post_note("%s%s%s",buf,str_datei,buf2);
   }
   else
   {
     fscanf(str_file,"%d",&anzahl_profil_dat_entries);
     fscanf(str_file,"%d",&anzahl_strang_entries);
     fgets(help,110,str_file);
     if(anzahl_profil_dat_entries>0)
     {
       for(i=1;i<=anzahl_profil_dat_entries;i++)
       {
         fgets(help,110,str_file);
         for(j=0;j<=strlen(help);j++)
         {
           if(help[j]=='\n')
             help[j]='\0';
         }
         xvt_slist_add_at_elt(prof_slist,NULL,help,0L);
       } //for anzahl_prof_dat_entries
     } //if anzahl_prof_dat_entries>0
     fclose(str_file);
   } //else str_datei da
   
   delete[] help;
   delete[] str_datei;
 }
