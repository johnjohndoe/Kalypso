/******FUNKTIONEN IM ZUS.MIT EINER DATEI, IN DER ALLE PROFILE EINES****/
/******PROJEKTES REFERRENZIERT WERDEN*********************************/

//#include <stdlib.h>
//#include <string.h>

#include <windows.h>
#include "xvt.h"
#include "wspwin.h"
#include "resource.h"

#include "profproj.h"

#include "global_types.h"

#include "global_vars.h"

#include "list.h"

#include "util.h"
#include "read_cfg.h"
#include "dis_prof.h"
#include "typen.h"
#include "strang.h"
#include "global.h"

#include "profpro2.h"

extern char name208[20],
				station208[20],
				vzk[20],
				zustand[20],
				pk[20],
				dateiname[15],
                save_station208[20];//Dick 28.03.99
extern BOOLEAN nicht_posten,
               schluessel_aendern;//Dick 28.03.99  

BOOLEAN bce=FALSE;

FILE *profprojfile,
	  *str_datei_file;
SLIST schreibe_list;
/****************************************************/

void teste_projekt_profile(int drucken) //AUFRUF AUS WSPW120.cpp
{
  char profprojdatei[100];
  int i, j, k, zaehler1,zaehler2, back, teste,h;
  BOOLEAN nameda,stationda,pkda,vzkda,zustandda;
  SLIST_ELT ee;

  char	*druckstring,
  			*help,
        *name208_vgl,
        *station208_vgl,
        *vzk_vgl,
        *zustand_vgl,
        *pk_vgl,
        *str_referenz;

  char *hilfs_ptr;
 /********************/
  druckstring = new char[200];
  help = new char[200];
  name208_vgl = new char [20];
  station208_vgl= new char [20];
  vzk_vgl= new char [20];
  zustand_vgl= new char [20];
  pk_vgl= new char [20];
  str_referenz= new char [50];

 /************************/

 xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,profprojdatei,50);
 strcat(profprojdatei,"\\profproj.txt");
 back=access(profprojdatei,00);

 if((back!=0) && (drucken==1)) //PROFPROJDATEI NOCH NICHT VORHANDEN
 {
   strcpy(name208,netz_dat[0]);
   vergleich=FALSE;
   profprojfile=fopen(profprojdatei,"w");
   for (i=0;i<98;i++)
     druckstring[i]=' ';
   druckstring[98]='\0';
   for(i=0;i<(INT)strlen(name208);i++)
     druckstring[i]=name208[i];
   for(i=0;i<(INT)strlen(station208);i++)
     druckstring[i+10]=station208[i];
   h=26;
   for(i=(INT)(strlen(pk)-1);i>=0;h--,i--)             //pk
     druckstring[h]=pk[i];   //Neu Dick 19->26 und minus 10.07.98
   
   h=31;
   for(i=(INT)(strlen(vzk)-1);i>=0;h--,i--)            //vzk
     druckstring[h]=vzk[i];  //Neu Dick 29->31 und minus 10.07.98
   
   for(i=0;i<(INT)strlen(zustand);i++)
     druckstring[i+33]=zustand[i];
   for(i=0;i<(INT)strlen(dateiname);i++)
     druckstring[i+44]=dateiname[i];
   
   fprintf(profprojfile,"1 1\n");
   fprintf(profprojfile,"%s\n",druckstring);
   
   fprintf(profprojfile,"\n");
   
   /*****FOLGENDES WURDE EINGEFUEGT WEIL TEILWEISE DER NICHT*******
   ******NAHCHZUVOLLZIEHENDE FEHLER AUFTRAT DASS IN DER ENDUNG*****
   ******STRLI STATT STR AUFTRAT BEI STR_SPEC.NAME*****************/
   int len=strlen(STR_SPEC.name);
   if(STR_SPEC.name [len-1]!='r')
   {
     STR_SPEC.name[len-1]='\0';
     char buf[200];
     xvt_res_get_str(STR_PROFPROJ_ERR,buf,sizeof(buf));
     xvt_dm_post_note("%s",buf); 
     //xvt_dm_post_note("Evt. Fehler in profproj.txt bzw. .str-Datei");
   }
   if(STR_SPEC.name [len-2]!='t')
   {
     STR_SPEC.name[len-2]='\0';
     char buf[200];
     xvt_res_get_str(STR_PROFPROJ_ERR,buf,sizeof(buf));
     xvt_dm_post_note("%s",buf);
     //xvt_dm_post_note("Evt. Fehler in profproj.txt bzw. .str-Datei");
   }
   STR_SPEC.name[12]='\0';
   fprintf(profprojfile,"%s %s\n",dateiname, STR_SPEC.name);
   fclose(profprojfile);
   
 } // IF PROFPROJDATEI EXISTIERT NOCH NICHT
 
 if(back==0) //PROFPROJDATEI EXISTIERT
	{
   strcpy(name208,netz_dat[0]);
   schreibe_list=xvt_slist_create();
   profprojfile=fopen(profprojdatei,"r");
   fscanf(profprojfile,"%d",&zaehler1);
   fscanf(profprojfile,"%d",&zaehler2);
   fgets(help,101,profprojfile); //\n überlesen
   vergleich=FALSE;
   for(i=1;i<=zaehler1;i++)
	  {
     fgets(help,101,profprojfile);
     for(j=0;j<=(INT)strlen(help);j++)
     {
       if(help[j]=='\n')
         help[j]='\0';
     }
     xvt_slist_add_at_elt(schreibe_list,NULL,help,0L);
     
     /***********name*******/
     k=0;
     nameda=FALSE;
     for(j=0;j<9;j++)
     {
       if(help[j]!=' ')
       {
         name208_vgl[k]=help[j];
         k++;
       }
     }
     name208_vgl[k]='\0';
     teste=xvt_str_compare_ignoring_case(name208_vgl,name208);
     if(teste==0)
       nameda=TRUE;
     else
       nameda=FALSE;
     /**********name********/
     /*****station**********/
     k=0;
     stationda=FALSE;
     for(j=10;j<18;j++)
     {
       if(help[j]!=' ')
       {
         station208_vgl[k]=help[j];
         k++;
       }
     }
     station208_vgl[k]='\0';
     teste=xvt_str_compare_ignoring_case(station208_vgl,station208);
     if(teste==0)
       stationda=TRUE;
     else
       stationda=FALSE;
     
     /*********station******/
     /*********pk***********/
     k=0;
     pkda=FALSE;
     for(j=19;j<28;j++)
     {
       if(help[j]!=' ')
       {
         pk_vgl[k]=help[j];
         k++;
       }
     }
     pk_vgl[k]='\0';
     teste=xvt_str_compare_ignoring_case(pk_vgl,pk);
     if(teste==0)
       pkda=TRUE;
     else
       pkda=FALSE;
     /*************pk********/
     /********vzk************/
     k=0;
     vzkda=FALSE;
     for(j=29;j<32;j++)
     {
       if(help[j]!=' ')
       {
         vzk_vgl[k]=help[j];
         k++;
       }
     }
     vzk_vgl[k]='\0';
     teste=xvt_str_compare_ignoring_case(vzk_vgl,vzk);
     if(teste==0)
       vzkda=TRUE;
     else
       vzkda=FALSE;
     /*******vzk*************/
     /*********zustand*******/
     k=0;
     zustandda=FALSE;
     for(j=33;j<43;j++)
     {
       if(help[j]!=' ')
       {
         zustand_vgl[k]=help[j];
         k++;
       }
     }
     zustand_vgl[k]='\0';
     teste=xvt_str_compare_ignoring_case(zustand_vgl,zustand);
     if(teste==0)
       zustandda=TRUE;
     else
       zustandda=FALSE;
     /************zustand************/
     if((nameda) &&(stationda) &&(pkda) &&(vzkda) &&(zustandda))
     {
       if(!nicht_posten)
       {
         //xvt_dm_post_note("Es existiert bereits ein Profil "
         //				"mit den selben Schlüsseldaten "
         //				"in diesem Projekt");
         char buf[200];
         xvt_res_get_str(STR_PROFIL_SCHON_DA,buf,sizeof(buf));
         xvt_dm_post_note("%s",buf);
       }
       
       vergleich=TRUE;
       
     } //WENN SCHON DA
     
  } //FOR ANZAHL ZAEHLER1
  
  if(vergleich==FALSE)
  {
    if(drucken==1)
    {
      for (i=0;i<98;i++)
        druckstring[i]=' ';
      druckstring[98]='\0';
      for(i=0;i<(INT)strlen(name208);i++)
        druckstring[i]=name208[i];
      for(i=0;i<(INT)strlen(station208);i++)
        druckstring[i+10]=station208[i];
      for(i=0;i<(INT)strlen(pk);i++)
        druckstring[i+19]=pk[i];
      for(i=0;i<(INT)strlen(vzk);i++)
        druckstring[i+29]=vzk[i];
      for(i=0;i<(INT)strlen(zustand);i++)
        druckstring[i+33]=zustand[i];
      for(i=0;i<(INT)strlen(dateiname);i++)
        druckstring[i+44]=dateiname[i];
      xvt_slist_add_at_elt(schreibe_list,NULL,druckstring,0L);
      
      fgets(help,101,profprojfile); //\n überlesen
      help[0]='\0';
      help[1]='\0';
      xvt_slist_add_at_elt(schreibe_list,NULL,help,0L);
      for(i=1;i<=zaehler2;i++)
      {
        fgets(help,101,profprojfile);
        for(j=0;j<=(INT)strlen(help);j++)
        {
          if(help[j]=='\n')
            help[j]='\0';
        }
        xvt_slist_add_at_elt(schreibe_list,NULL,help,0L);
      }
      strcpy(str_referenz,dateiname);
      strcat(str_referenz," ");
      
      /****Wegen Fehler strli Endung der nicht nachvollziehbar***********/
      int len=strlen(STR_SPEC.name);
      if(STR_SPEC.name [len-1]!='r')
      {
        STR_SPEC.name[len-1]='\0';
        char buf[200];
        xvt_res_get_str(STR_PROFPROJ_ERR,buf,sizeof(buf));
        xvt_dm_post_note("%s",buf);
        xvt_dm_post_note("Evt. Fehler in profproj.txt bzw. .str-Datei");
      }
      if(STR_SPEC.name [len-2]!='t')
      {
        STR_SPEC.name[len-2]='\0';
        char buf[200];
        xvt_res_get_str(STR_PROFPROJ_ERR,buf,sizeof(buf));
        xvt_dm_post_note("%s",buf);
        xvt_dm_post_note("Evt. Fehler in profproj.txt bzw. .str-Datei");
      }
      /******************************************************************/
      STR_SPEC.name[12]='\0';
      strcat(str_referenz,STR_SPEC.name);
      xvt_slist_add_at_elt(schreibe_list,NULL,str_referenz,0L);
    } //IF DRUCKEN
    
  } //IF VERGLEICH==FALSE
  
	 fclose(profprojfile);
   if((vergleich==FALSE) && (drucken==1))
	  {
     profprojfile=fopen(profprojdatei,"w");
     zaehler1=zaehler1+1;
     zaehler2=zaehler2+1;
     fprintf(profprojfile,"%d ",zaehler1);
     fprintf(profprojfile,"%d\n",zaehler2);
     for(ee=xvt_slist_get_first(schreibe_list);
     ee!=NULL;ee=xvt_slist_get_next(schreibe_list,ee))
     {
       hilfs_ptr=xvt_slist_get(schreibe_list,ee,0L);
       druckstring[0]='\0';
       strcpy(druckstring,hilfs_ptr);
       fprintf(profprojfile,"%s\n",druckstring);
     }
     fclose(profprojfile);
   }
   
   if(schreibe_list!=NULL)
	  {
     xvt_slist_destroy(schreibe_list);
     schreibe_list=NULL;
	  }
  } //IF PROFPROJDATEI EXISTIERT
  
  delete[] help;
  delete[] druckstring;
  delete[] name208_vgl;
  delete[] station208_vgl;
  delete[] vzk_vgl;
  delete[] zustand_vgl;
  delete[] pk_vgl;
  delete[] str_referenz;
}

/************************************************************************/
void teste_str_datei(void) //AUFRUF AUS WSPDLG208.cpp
{                         //UND AUS ERMITTLE-SCHLUESSEL_AUS-PROFILDATEI s.u.
  int i,j,a, teste;
  BOOLEAN stationda,pkda,vzkda,
    station_change=FALSE;//Dick 28.03.99
  double station208_vgl_d, station208_double;
  
  char *help_str,
    *str_datei,
    *station208_vgl,
    *vzk_vgl,
    *pk_vgl;
  
  help_str 		= new char[200];
  str_datei		= new char[100];
  station208_vgl = new char [20];
  vzk_vgl 			= new char [20];
  pk_vgl 			= new char [20];
  /*********/
  xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str_datei,50);
  strcat(str_datei,"\\");
  STR_SPEC.name[12]='\0';
  strcat(str_datei,STR_SPEC.name);
  str_datei_file=fopen(str_datei,"r");
  fscanf(str_datei_file,"%d",&anzahl_profil_dat_entries);
  fscanf(str_datei_file,"%d",&anzahl_strang_entries);
  fscanf(str_datei_file,"%s",help_str);
  fscanf(str_datei_file,"%s",help_str);
  fgets(help_str,110,str_datei_file);   //  '\n' ueberlesen
  vergleich=FALSE;
  if(anzahl_profil_dat_entries>0)
  {
    for (a=1;a<=anzahl_profil_dat_entries;a++)    //Profiltabelle lesen
    {
      fgets(help_str,110,str_datei_file);
      /****STATION TESTEN****/
      j=0;
      stationda=FALSE;
      for(i=10;i<18;i++)
      {
        if(help_str[i]!=' ')
        {
          station208_vgl[j]=help_str[i];
          j++;
        }
      }
      station208_vgl[j]='\0';
      station208_double=atof(station208);
      station208_vgl_d=atof(station208_vgl);
      if(station208_vgl_d==station208_double)
        stationda=TRUE;
      else
        stationda=FALSE;
      
      /*********station******/
      /*********pk***********/
      j=0;
      pkda=FALSE;
      for(i=19;i<28;i++)
      {
        if(help_str[i]!=' ')
        {
          pk_vgl[j]=help_str[i];
          j++;
        }
      }
      pk_vgl[j]='\0';
      if(pk_vgl[0]=='0' && pk_vgl[0]==pk[0])
        pkda=TRUE;
      else
      {
        teste=xvt_str_compare_ignoring_case(pk_vgl,pk);
        if(teste==0)
          pkda=TRUE;
        else
          pkda=FALSE;
      }
      /*************pk********/
      /********vzk************/
      j=0;
      vzkda=FALSE;
      for(i=29;i<32;i++)
      {
        if(help_str[i]!=' ')
        {
          vzk_vgl[j]=help_str[i];
          j++;
        }
      }
      vzk_vgl[j]='\0';
      teste=xvt_str_compare_ignoring_case(vzk_vgl,vzk);
      if(teste==0)
        vzkda=TRUE;
      else
        vzkda=FALSE;
      /*******vzk*************/
      if(schluessel_aendern)
      {
        //teste=xvt_str_compare_ignoring_case(save_station208,station208);
        if(atof(save_station208)==atof(station208))//Dick 6.04.99
          teste=0;
        else
          teste=-1;
        if(teste==0)
          station_change=TRUE;
        else
          station_change=FALSE;
      }
      else
        station_change=FALSE;
      if((stationda) && (vzkda) &&(pkda) && !station_change)
      {
        char buf[200];
        xvt_res_get_str(STR_KEY_EXIST,buf,sizeof(buf));
        xvt_dm_post_note("%s",buf);
        //xvt_dm_post_note("Schlüsseldaten existieren bereits");
        vergleich=TRUE;
      }
    } //FOR ANZAHL PROF DAT ENTRIES ++
    
  } // ANZAHL PROF DAT ENTRIES >0
  fclose(str_datei_file);
  
  delete[] help_str;
  delete[] str_datei;
  delete[]	station208_vgl;
  delete[]	vzk_vgl;
  delete[]pk_vgl;
} //  teste_str_datei()

/*************************************************************************/
void ermittle_schluessel_aus_profildatei(char fss_name[15])
{
  //AUFRUF AUS WSPDLG136.cpp - PROFIL AUFNEHMEN
  //char help[100], profil_uebergeben[100];
  char *help_ptr;
  int i,test,h;
  SLIST_ELT e2, ee;
  
  char *help, *profil_uebergeben;
  
  help = new char[100];
  profil_uebergeben = new char[100];
  
  help_ptr= new char[300];
  
  /***********/
  
  xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,profil_uebergeben,50);
  strcat(profil_uebergeben,"\\");
  strcat(profil_uebergeben,fss_name);
  
  schluessel_einlesen(profil_uebergeben);
	 test=xvt_str_compare_ignoring_case(netz_dat[0],name208);
	  if(test!=0)
    {
      vergleich=TRUE;
      char buf[200],buf2[200],buf3[200];//Dick 26.11.99
      xvt_res_get_str(STR_GEWAESSERNAME,buf,sizeof(buf));
      xvt_res_get_str(STR_GEWAESSERNAME2,buf2,sizeof(buf2));
      xvt_res_get_str(STR_GEWAESSERNAME3,buf3,sizeof(buf3));
      xvt_dm_post_note("%s%s%s%s%s",buf,name208,buf2,netz_dat[0],buf3);
      //xvt_dm_post_note("Gewässername: %s der Profildatei "
      //						"entspricht nicht dem "
      //						"Gewässernamen: %s der Zustandsdatei ",
      //						 name208,netz_dat[0]);
      if(kopiert)
        remove(profil_uebergeben);
      for(ee=xvt_slist_get_first(prof_datei);
      ee!=NULL;ee=xvt_slist_get_next(prof_datei,ee))
      {
        if((e2=xvt_slist_get_next(prof_datei,ee))==NULL)
          xvt_slist_rem(prof_datei,ee);
      }
      anzahl_profil_dat_entries--;
    }//IF TEST!=0,D.H.FALSCHER GEW.NAME
    
    /*****************************/
    if(test==0)
    {
      vergleich=FALSE;
      profil_aufnehmen=TRUE;
      
      if(bce)
      {
        pk[0]='1';
        pk[1]='\0';
        vzk[0]='0';
        vzk[1]='\0';
      }
      teste_str_datei(); //IN PROFPROJ.cpp
      anzahl_profil_dat_entries++; //WEIL IN TESTE-STR WIEDER ERNIEDRIGT
      if(vergleich)
      {
        if(kopiert)
          remove(profil_uebergeben);
        int hilfszaehler=0;
        int hilfszaehler2;
        for(ee=xvt_slist_get_first(prof_datei);
        ee!=NULL;ee=xvt_slist_get_next(prof_datei,ee))
        {
          if((e2=xvt_slist_get_next(prof_datei,ee))==NULL)
            //  xvt_slist_rem(prof_datei,ee);
            hilfszaehler2=hilfszaehler;
          hilfszaehler++;
        }
        ee=xvt_slist_get_first(prof_datei); 
        help_ptr=xvt_slist_get(prof_datei,ee,0L);
        for(i=0; i<hilfszaehler2; i++)
        {
          ee=xvt_slist_get_next(prof_datei,ee);
          help_ptr=xvt_slist_get(prof_datei,ee,0L);
          
        }
        xvt_slist_rem(prof_datei,ee);
        anzahl_profil_dat_entries--;
        zeige_slist(prof_datei);
      } //if vergleich d.h. Profil dieses Schluessels schon da
      
      if((!vergleich) && (kopiert))
      {
        dateiname[0]='\0';
        strcpy(dateiname,fss_name);
        teste_projekt_profile(1);
        if(vergleich)
        {
          if(kopiert)
            remove(profil_uebergeben);
          for(ee=xvt_slist_get_first(prof_datei);
          ee!=NULL;ee=xvt_slist_get_next(prof_datei,ee))
          {
            if((e2=xvt_slist_get_next(prof_datei,ee))==NULL)
              xvt_slist_rem(prof_datei,ee);
          }
          anzahl_profil_dat_entries--;
        } // if vergleich zwischenzeitlich
        // d.h. Profil dieses Schluessels schon da
      } //if !vergeleich &kopiert
      
      if(!vergleich)
      {
        for(ee=xvt_slist_get_first(prof_datei);
        ee!=NULL;ee=xvt_slist_get_next(prof_datei,ee))
        {
          if((e2=xvt_slist_get_next(prof_datei,ee))==NULL)
          {
            help_ptr=xvt_slist_get(prof_datei,ee,0L);
            if(strlen(help_ptr)==0)//Dick 22.09.2000 sonst in strcpy abbruch
              break;
            strcpy(help,help_ptr);
            xvt_slist_rem(prof_datei,ee);
            
            for(i=0;i<(INT)strlen(zustand);i++)
              help[33+i]=zustand[i];
              /* for(i=0;i<(INT)strlen(vzk);i++)
              help[31-i]=vzk[i]; //Neu Dick 29->31 und minus 10.07.98
              for(i=0;i<(INT)strlen(pk);i++)
              help[26-i]=pk[i]; //Neu Dick 19->26 und minus 10.07.98
            */
            h=26;
            for(i=(INT)(strlen(pk)-1);i>=0;h--,i--)             //pk
              help[h]=pk[i];   //Neu Dick 19->26 und minus 10.07.98
            
            h=31;
            for(i=(INT)(strlen(vzk)-1);i>=0;h--,i--)            //vzk
              help[h]=vzk[i];  //Neu Dick 29->31 und minus 10.07.98
            
            
            xvt_slist_add_at_elt(prof_datei,NULL,help,0L);
          }
        }
      }
      profil_aufnehmen=FALSE;
    } //test==0
    delete[] help;
    delete[] profil_uebergeben;
    
   }
   /*********************************************************/
   void haenge_zustand_an_zsd(char profil_name[15])
   {
     //SCHREIBT IN DATEI PROFPROJ NEUE KOMBINATION PROFIL-ZUSTAND
     //AUFRUF AUS WSPD210.cpp (PROFILE AUFNEHMEN FÜR NEUE STR-DATEI)
     //AUFRUF AUS WSPD136.cpp (PROFIL AUFNEHMEN OHNE ZU KOPIEREN)
     
     FILE *zsdfile ;
     SLIST zsd_list;
     int counter1, counter2,i,j,teste, back;
     SLIST_ELT ee;
     char *hilfs_ptr;
     BOOLEAN schonda=FALSE;
     
     
     char *zsdstr,*help, *druckstring;//char zsdstr[100],help[150], druckstring[150];
     
     zsdstr      = new char[100];
     help        = new char[150];
     druckstring = new char[150];
     
     /**************************/
     teste=1;
     
     zsd_list=xvt_slist_create();
     xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,zsdstr,50);
     strcat(zsdstr,"\\profproj.txt");
     back=access(zsdstr,00);
     if(back!=0)  //zsd-DATEI NOCH NICHT VORHANDEN
     {
       //xvt_dm_post_note("Die aufgenommene Datei ist nicht registriert -"
       //				  "Es kann zu Komplikationen kommen");
       char buf[200];
       xvt_res_get_str(STR_DATEI_NOT_REG,buf,sizeof(buf));
       xvt_dm_post_note("%s",buf);
     }
     else
     {
       zsdfile=fopen(zsdstr,"r");
       fscanf(zsdfile,"%d",&counter1);
       fscanf(zsdfile,"%d",&counter2);
       fgets(help,10,zsdfile); //\n UEBERLESEN
       
       for(i=1;i<=counter1;i++)
       {
         fgets(help,101,zsdfile);
         for(j=0;j<=(INT)strlen(help);j++)
         {
           if(help[j]=='\n')
             help[j]='\0';
         }
         xvt_slist_add_at_elt(zsd_list,NULL,help,0L);
       }
       fgets(help,101,zsdfile);
       
       xvt_slist_add_at_elt(zsd_list,NULL,"\0",0L);
       
       strcpy(druckstring,profil_name);
       strcat(druckstring," ");
       STR_SPEC.name[12]='\0';
       strcat(druckstring,STR_SPEC.name);
       
       schonda=FALSE;
       for(i=1;i<=counter2;i++)
       {
         fgets(help,101,zsdfile);
         for(j=0;j<=(INT)strlen(help);j++)
         {
           if(help[j]=='\n')
             help[j]='\0';
         }
         teste=xvt_str_compare_ignoring_case(druckstring,help);
         if(teste==0)
         {
           // xvt_dm_post_note("Die Profildatei ist schon "
           //						 "in der Zustandsdatei referenziert");
           char buf[200];//Dick 26.11.99
           xvt_res_get_str(STR_PROFDAT_IST_DA,buf,sizeof(buf));
           xvt_dm_post_note("%s",buf);
           schonda=TRUE;
         }
         
         xvt_slist_add_at_elt(zsd_list,NULL,help,0L);
         
       } //for counter2
       
       
       fclose(zsdfile);
       
       zsdfile=fopen(zsdstr,"w");
       if(!schonda)
       {
         counter2=counter2+1;
         xvt_slist_add_at_elt(zsd_list,NULL,druckstring,0L);
       }
       fprintf(zsdfile,"%d %d\n",counter1,counter2);
       for(ee=xvt_slist_get_first(zsd_list);
       ee!=NULL;ee=xvt_slist_get_next(zsd_list,ee))
       {
         hilfs_ptr=xvt_slist_get(zsd_list,ee,0L);
         strcpy(druckstring,hilfs_ptr);
         fprintf(profprojfile,"%s\n",druckstring);
       }
       
       fclose(zsdfile);
     } //else profproj existiert
     if(zsd_list!=NULL)
     {
       xvt_slist_destroy(zsd_list);
       zsd_list=NULL;
     }
     delete[] zsdstr;
     delete[] help  ;
     delete[] druckstring;
 }
 /************************************************************************/
 void teste_physisch_loeschen(char name[15])
 {
   //AUFRUF AUS WSPDLG136
   //FKT: TESTET OB PROFILDATEI NOCH IN ANDEREN ZUSTANDSDATEIEN REFERENZIERT
   //WIRD
   char profprojdatei[100];
   int test,k,l, test2,test3,i,j, zaehler1,zaehler2;
   BOOLEAN blank=FALSE, loeschen;
   char *hilfs_ptr;
   SLIST_ELT ee;
   
   /*char vergleichsstring[100],
		 profilnamevgl[15],
     strnamevgl[15],
     help[200],
     druckstring[200],
		 loeschdatei[100]; */
   
   char *vergleichsstring,
     *profilnamevgl,
     *strnamevgl,
     *help,
     *druckstring,
     *loeschdatei;
   
   vergleichsstring  = new char[100];
   profilnamevgl     = new char[15];
   strnamevgl        = new char[15];
   help              = new char[200];
   druckstring       = new char[200];
   loeschdatei       = new char[200];
   /****************************************************/
   loeschen=TRUE;
   if(schreibe_list!=NULL)
   {
     xvt_slist_destroy(schreibe_list);
     schreibe_list=NULL;
   }
   schreibe_list=xvt_slist_create();
   xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,profprojdatei,50);
   strcat(profprojdatei,"\\profproj.txt");
   profprojfile=fopen(profprojdatei,"r");
   fscanf(profprojfile,"%d",&zaehler1);
   fscanf(profprojfile,"%d",&zaehler2);
   fgets(help,101,profprojfile); //\n überlesen
   for(i=1;i<=zaehler1;i++)
   {
     fgets(help,101,profprojfile);
     for(j=0;j<=(INT)strlen(help);j++)
     {
       if(help[j]=='\n')
         help[j]='\0';
     }
     xvt_slist_add_at_elt(schreibe_list,NULL,help,0L);
   }
   fgets(help,101,profprojfile);
   xvt_slist_add_at_elt(schreibe_list,NULL,"\0",0L);
   strcpy(vergleichsstring,name);
   strcat(vergleichsstring," ");
   STR_SPEC.name[12]='\0';
   strcat(vergleichsstring,STR_SPEC.name);
   for(i=1;i<=zaehler2;i++)
   {
     fgets(help,101,profprojfile);
     for(j=0;j<=(INT)strlen(help);j++)
     {
       if(help[j]=='\n')
         help[j]='\0';
     }
     
     blank=FALSE;
     k=0;
     l=0;
     for(j=0;j<(INT)strlen(help);j++)
     {
       if(help[j]==' ')
         blank=TRUE;
       if(!blank)
       {
         profilnamevgl[l]=help[j];
         l++;
       }
       if((blank) && (help[j]!=' '))
       {
         strnamevgl[k]=help[j];
         k++;
       }
     }
     strnamevgl[k]='\0';
     profilnamevgl[l]='\0';
     test=xvt_str_compare_ignoring_case(vergleichsstring,help);
     if(test!=0)
       xvt_slist_add_at_elt(schreibe_list,NULL,help,0L);
     test2=xvt_str_compare_ignoring_case(profilnamevgl,name);
     STR_SPEC.name[12]='\0';
     test3=xvt_str_compare_ignoring_case(strnamevgl,STR_SPEC.name);
     if((test2==0) && (test3!=0))
       loeschen=FALSE;
   } //FOR ANZAHL UNTERER TEIL PROFPROJ
   if(!loeschen)
   {
	    //xvt_dm_post_note("Die Profildatei wird noch in anderen "
     //				  "Zustandsdateien referenziert. Sie wird "
     //				  "nicht physisch gelöscht.");
     char buf[200];//Dick 26.11.99
     xvt_res_get_str(STR_PROFDAT_NOT_HARD_DEL,buf,sizeof(buf));
     xvt_dm_post_note("%s",buf);
   }
   else
   {
     char buf[200],buf2[200],buf3[200];//Dick 26.11.99
     xvt_res_get_str(STR_JA,buf,sizeof(buf));
     xvt_res_get_str(STR_NEIN,buf2,sizeof(buf2));
     xvt_res_get_str(STR_PROFDAT_HARD_DEL_ASK,buf3,sizeof(buf3));
     switch (xvt_dm_post_ask(buf,buf2,NULL,buf3))
				 {
     case RESP_DEFAULT:       //JA - löschen
       {
         test=1;
         xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,loeschdatei,50);
         strcat(loeschdatei,"\\");
         strcat(loeschdatei,name);
         remove(loeschdatei);
         //AUS OBEREM TEIL VON PROFPROJ LOESCHEN
         
         int x=0,x_rem=-1;
         
         for(ee=xvt_slist_get_first(schreibe_list);
								 ee!=NULL;ee=xvt_slist_get_next(schreibe_list,ee))
                 {
                   hilfs_ptr=(CHAR*)xvt_slist_get(schreibe_list,ee,0L);
                   loeschdatei[0]='\0';
                   
                   strcpy(loeschdatei,hilfs_ptr);
                   
                   if((strlen(loeschdatei))>=40)
                   {
                     j=0;
                     for(i=44;i<=55;i++)
                     {
                       if(loeschdatei[i]!=' ')
                       {
                         profilnamevgl[j]=loeschdatei[i];
                         j++;
                       }
                     }
                     profilnamevgl[j]='\0';
                     test=xvt_str_compare_ignoring_case(profilnamevgl,name);
                     if(test==0)
                     {
                       x_rem=x;
                       //xvt_slist_rem(schreibe_list,ee); // nicht in for-Schleife löschen
                     }
                   } //if strlen loeschdatei>40
                   x++;
                 } //for slist_elem
                 
                 if (x_rem>-1)
                 {
                   x=0;	 
                   /***auskomm***
                   for(ee=xvt_slist_get_first(schreibe_list);
                   ee!=NULL;ee=xvt_slist_get_next(schreibe_list,ee))
                   if(x_rem==x)
                   xvt_slist_rem(schreibe_list,ee);
                   ****ende auskomm.*/
                   ee=xvt_slist_get_first(schreibe_list);
                   x=xvt_slist_count(schreibe_list);
                   for(x=0;x<x_rem;x++)
                   {
                     ee=xvt_slist_get_next(schreibe_list,ee);
                   }
                   xvt_slist_rem(schreibe_list,ee);
                   //  x++; auskomm.
                   
                 }
                 
                 zaehler1=zaehler1-1;
                 
                 break;
       }
     case RESP_2:             // NEIN - nicht löschen
						 {
               break;
             }
     } //switch
     
   } //else Datei wird nicht mehr referenziert
   
   fclose(profprojfile);
   //PROFPROJ NEU SCHREIBEN AUS SLIST SCHREIBE-LIST:
   
   zaehler2=zaehler2-1;
   if(zaehler1!=0)
   {
     profprojfile=fopen(profprojdatei,"w");
     fprintf(profprojfile,"%d ",zaehler1);
     fprintf(profprojfile,"%d\n",zaehler2);
     for(ee=xvt_slist_get_first(schreibe_list);
     ee!=NULL;ee=xvt_slist_get_next(schreibe_list,ee))
     {
       hilfs_ptr=xvt_slist_get(schreibe_list,ee,0L);
       druckstring[0]='\0';
       strcpy(druckstring,hilfs_ptr);
       fprintf(profprojfile,"%s\n",druckstring);
     }
     fclose(profprojfile);
   }
   else
   {
     remove(profprojdatei);
   }
   
   
   if(schreibe_list!=NULL)
   {
     xvt_slist_destroy(schreibe_list);
     schreibe_list=NULL;
   }
   
   
   delete[] vergleichsstring;
   delete[] profilnamevgl   ;
   delete[] strnamevgl;
   delete[] help;
   delete[] druckstring;
   delete[] loeschdatei;
   
 } //Fkt.
 /**************************************************************************/
 
