#include <windows.h>
#include <windowsx.h>

#include "xvt.h"
#include "wspwin.h"
#include "resource.h"


#include <commdlg.h>
#include <dlgs.h>
#include <winuser.h>
#include <stdlib.h>


#include "global_types.h"
#include "global_vars.h"
#include "..\..\wspdlg\Include\export.h"
#include "list.h"
#include "readprof.h"

#include "util.h"
#include "typen.h"
#include "strang.h"

#include "global.h"
#include "read_cfg.h"
#include "bce_allg.h"

#define XVT_INCL_NATIVE

#ifndef WIN_120_LISTBOX_NEU
#define WIN_120_LISTBOX_NEU 3049
#endif
#ifndef WIN_120_LISTBOX_DEL
#define WIN_120_LISTBOX_DEL 3051
#endif
#ifndef datensatzanzahl       // Anzahl der Datensätze in wspdlg.rc bzw. typen.h
#define datensatzanzahl 100
#endif
#ifndef tabellenlaenge
#define tabellenlaenge 500 //Dick 7.02.00 100->500
#endif
#define RES_ID_NEU  1015 //von 1011 auf 1016 Bley 9.1.2001
#define RES_ID_READ 1015
#define NEU  0
#define READ 1
#define PLOT 2 

/**********   UTILITIES   *******************/
extern char newstr[15];
extern OPENFILENAME ofn;

extern WINDOW win_list2,
				  scroll_120;
extern BOOLEAN aendern;
neuer_datensatz new_ds;  //struct: neuer_datensatz  -->util.h

extern char zustand[20];
extern char pk[20];
extern char vzk[20];
extern char station208[20];
extern BOOLEAN bce, editieren, berechnen;
extern WINDOW win_list2;
extern SLIST header_profil;
extern WINDOW win120_typ,win116_typ;

extern MinMax pmm;


extern int verzeichnis_anlegen(char*);

bool map_object_command=FALSE;


void convert_profil_str(char profilstrutil[100])
/* konvertiert Profilstring aus der Vernetzungsdatei
	(listbox 86 in wspd136) in profilstring für Edit-Felder
	in wspd136                                            */
{
 int i,j;
 BOOLEAN alpha=FALSE,blank =FALSE;

 for (i=0;i<=10;i++)
	  newstr[i]=' ';

 for (i=0;i<=7;i++)         // Station
	  {
		newstr[i] = profilstrutil[10+i];
		if (newstr[i]!=' ') alpha = TRUE;

		if ((newstr[i]==' ')&&(!alpha)) blank++;
	  }
 newstr[8]='\0';
/**** führende Blank's entfernen ****/
 if (blank >0)
	{
	 for (i=1;i<=blank;i++)
		{
		 for(j=0;j<=8;j++)
			 newstr[j]=newstr[j+1];
		}
	}
}

int compare_station(char *station)
{
 char *str;
 char temp[10];
 char strtemp[150];
 int count=0,j=0,pos=-1;
 double s1,s2;
 BOOLEAN gefunden =FALSE;

 dateiname[0]='\0';
 count = xvt_slist_count(prof_datei);
 while ((j<count)&&(!gefunden))
	{
	 str = xvt_slist_get_elt(prof_datei, j, 0);
	 strcpy(strtemp,str);
	 for (int i=0;i<=7;i++)
			temp[i]=strtemp[10+i];
	 temp[8]='\0';
	 s1 = atof(temp);
     if(strlen(station)>0)
	 s2 = atof(station);
     else
         s2=BCE_NAN;
	 if (s1 == s2)
		  {
			pos = j;
			gefunden = TRUE;
			for (int i=0;i<12;i++)
			  dateiname[i]=str[i+44];
			dateiname[12]='\0';
		  }
	 j++;
	}
  return pos;
}
int anzahl_station(char *station)
{
 char *str;
 char temp[10];
 char strtemp[150];
 int count=0,j=0,pos=0;
 double s1,s2;
 

 dateiname[0]='\0';
 count = xvt_slist_count(prof_datei);
 while (j<count)
	{
	 str = xvt_slist_get_elt(prof_datei, j, 0);
	 strcpy(strtemp,str);
	 for (int i=0;i<=7;i++)
			temp[i]=strtemp[10+i];
	 temp[8]='\0';
	 s1 = atof(temp);
     if(strlen(station)>0)
	 s2 = atof(station);
     else
         s2=BCE_NAN;
	 if (s1 == s2)
		  {
			pos++;			
			
		  }
	 j++;
	}
  return pos;
}
/****************************************************************/
int get_profildatei_names(char *lpstrFile)
{
 char *str,  //str[350]
		*temp, //[120]
		*str1;  //[100]
 char station_hilfe[20];

 char *match="STATION";
 int ok,zaehler=0,i=0,j=0,k=0,lv,l;
 double wert,min=MAXDOUBLE,max=BCE_NAN;

 str = new char[350];
 str1= new char[100];
 temp = new char[120];

 strcpy(str,lpstrFile);
 ok=0;
 if (strlen(lpstrFile)<=11)                               //für DLG_150
	 ok = 1;
// ok = xvt_str_match(str,"keine*Profildatei*",FALSE);   //für OpenFileDialog
 if ( !ok )
 {
  while (j < (INT)strlen(lpstrFile))
	{
	 for (i=0;i<=99;i++) //str1 initialisieren
		  str1[i]=' ';
	 for (i=0;i<=99;i++) //str initialisieren
		  str[i]=' ';

	 lv=0;
	 while ((lpstrFile[j]!=' ')&&(lpstrFile[j]!='\0'))    //Dateinamen aus ofn nach str[]
		{
		 str[lv]=lpstrFile[j];
		 lv++;
		 j++;
		}
	 str[lv]='\0';
	 zaehler++;

   FILE* in;
	 if ((in =fopen(str,"r+"))==NULL)
		 {
		  //xvt_dm_post_error("Profildatei:\n%s\nläßt sich nicht öffnen !",str);
          char buf[200],buf2[200];//Dick 26.11.99
          xvt_res_get_str(STR_PROFILDATEI,buf,sizeof(buf));
          xvt_res_get_str(STR_CANOTOPEN,buf2,sizeof(buf2));
          xvt_dm_post_error("%s%s%s",buf,str,buf2);
		  lpstrFile[0]='\0';
		  fclose(in);
		 }
	 else
		{
        for(i=1;i<=3;i++)        //Zeile 3 einlesen Zustand
			 fgets(temp,110,in);
		temp[strlen(temp)-1]='\0';
		k=0;
		l=33;
		/******NEU****/
		  BOOLEAN nichtsmehr=FALSE;
		  for(i=0;i<=36;i++)
			{
			 if(temp[i]=='\0')
			  nichtsmehr=TRUE;
			}
		  if(!nichtsmehr)
			{
			 for(i=40;i<=48;i++)
			  {
				 if((temp[i]!=' ') &&(temp[i]!='\n') &&(temp[i]!='\0'))
				  {
					str1[l]=temp[i];
					zustand[k]=temp[i];
					l++;
					k++;
				  }
				 if((temp[i]=='\0') || (temp[i]=='\n'))
				  i=49;
			  }
			 zustand[k]='\0';
			}
		  if(nichtsmehr)
			{
			 for(k=0;k<(INT)strlen(netz_dat[2]);k++)
			  str1[k+33]=netz_dat[2][k];
			 strcpy(zustand,netz_dat[2]);
			}
		/**********/
		for(i=4;i<=5;i++)        //Zeile 5 einlesen vzk
			 fgets(temp,110,in);
		temp[strlen(temp)-1]='\0';

		k=0;
		l=31;  //Neu Dick 13.07.98
		nichtsmehr=FALSE;
		  for(i=0;i<=36;i++)
			{
			 if(temp[i]=='\0')
			  nichtsmehr=TRUE;
			}
		  if(!nichtsmehr)
			{
			 for(i=40;i<=43;i++)
			  {
				 if((temp[i]!=' ') &&(temp[i]!='\n') &&(temp[i]!='\0'))
				  {
					str1[l]=temp[i];
					vzk[k]=temp[i];
					l--;      //Neu Dick 13.07.98
					k++;
				  }
				 if((temp[i]=='\0') || (temp[i]=='\n'))
				  i=44;
			  }
			 vzk[k]='\0';
			}
		  if(nichtsmehr)
			{
			 str1[31]='0';//Neu 29->31  Dick  13.07.98 
			 vzk[0]='\0';
			 strcat(vzk,"0");
			}

		/******************/
		fgets(temp,110,in);		 //Zeile 6 aus Profildatei einlesen =Gewässername

		temp[strlen(temp)-1]='\0';
		  k=0;
		  /*NEU*/
		  nichtsmehr=FALSE;
		  for(i=0;i<=36;i++)
			{
			 if(temp[i]=='\0')
			  nichtsmehr=TRUE;
			}
		  if(!nichtsmehr)
			{
			 for(i=40;i<=48;i++)
			  {
				 if((temp[i]!=' ') &&(temp[i]!='\n') &&(temp[i]!='\0'))
				  {
					str1[k]=temp[i];
					k++;
				  }
				 if((temp[i]=='\0') || (temp[i]=='\n'))
				  i=49;
			  }
			}
		  if(nichtsmehr)
			{
			 for(k=0;k<(INT)strlen(netz_dat[0]);k++)
			  str1[k]=netz_dat[0][k];
			}
		  /****/
		/********************/
		fgets(temp,110,in); 	//Zeile 7 PK

		temp[strlen(temp)-1]='\0';
		 k=0;
		 l=26;//Neu Dick 13.07.98
		/****/
		 nichtsmehr=FALSE;
		  for(i=0;i<=36;i++)
			{
			 if(temp[i]=='\0')
			  nichtsmehr=TRUE;
			}
		  if(!nichtsmehr)
			{
			 for(i=40;i<=48;i++)
			  {
				 if((temp[i]!=' ') &&(temp[i]!='\n') &&(temp[i]!='\0'))
				  {
					str1[l]=temp[i];
					pk[k]=temp[i];
					k++;
					l--; //Neu Dick 13.07.98
				  }
				 if((temp[i]=='\0') || (temp[i]=='\n'))
				  i=49;
			  }
			 pk[k]='\0';
			}
		  if(nichtsmehr)
			{
			 str1[26]='1';//Neu 19->26  Dick  13.07.98
			 pk[0]='0';
			 pk[1]='\0';
			}
	  /*****/
		fgets(temp,110,in);    //Zeile 8
		fgets(temp,110,in); 	 //Zeile 9 aus Profildatei einlesen =Station

		ok=xvt_str_compare_ignoring_case(match,temp);
		if (ok)
		 {
		  BOOLEAN found=FALSE;

		  int a=17;                                  //Station
		  for (i=0;i<=(INT)strlen(temp);i++)
			{
			 if(temp[i]=='\n')
			  temp[i]='\0';
			}

			int len=strlen(temp);
			BOOLEAN wert_war_da=FALSE;
			k=0;

			for (i=10;i<=len-1;i++)
			 {
			  if((temp[i]=='\0') || (temp[i]=='\n'))
				found=TRUE;
			  if((wert_war_da) && (temp[i]==' '))
				found=TRUE;
			  if((temp[i]!='\0') &&(temp[i]!='\n') &&(!found) &&(temp[i]!=' '))
				{
				 station208[k]=temp[i];
				 wert_war_da=TRUE;
				 k++;
				}
			 }
			station208[k]='\0';
			wert = atof(station208);
             //Neu ,Dick 6.07.99
            double st_wert;
            sscanf(station208,"%lf",&st_wert);
            if(st_wert<1000. && st_wert>-100.)
                sprintf(station208,"%.4lf",st_wert);//Dick 18.08.98
            else if(st_wert<10000. && st_wert>-1000.)
                sprintf(station208,"%.3lf",st_wert);//Dick 18.08.98
            else if(st_wert<100000. && st_wert>-10000.)
                sprintf(station208,"%.2lf",st_wert);
            else if(st_wert<1000000. && st_wert>-100000.)
                sprintf(station208,"%.1lf",st_wert);
            else 
                sprintf(station208,"%.0lf",st_wert);
			 a=17;
			 len=strlen(station208);  //neu len von station208

			 for(i=len-1;i>=0;i--)
			  {
				if((station208[i]!=' ') && (station208[i]!='\0') && (station208[i]!='\n'))
				 {
				  str1[a]=station208[i];
				  a--;
				 }
			  }
/*			 for(i=len-1;i>=10;i--)
			  {
				if((temp[i]!=' ') && (temp[i]!='\0') && (temp[i]!='\n'))
				 {
				  str1[a]=temp[i];
				  a--;
				 }
				}
*/
		if (wert < min)
		 min = wert;

		if (wert > max)
		 max = wert;
		 } //if (0k)

		for (i=0;i<=11;i++)    //Filename
			 str1[i+44]=str[i];
		str1[56]='\0';
		xvt_slist_add_at_elt(prof_datei,NULL,str1,NULL);  //Element in SLIST eintragen
		i=0;
		j++;
		SaveStrangFile = TRUE;
		fclose(in);
		}
	  }
	  anzahl_profil_dat_entries = anzahl_profil_dat_entries + zaehler;
	  if(min!=BCE_NAN)
		{
	      sprintf(station_hilfe,"%.4lf",min);
		  //gcvt(min,13,station_hilfe);
		  /*
		  for (i=0;i<strlen(station_hilfe);i++)
			 str_netz[40+i]=station_hilfe[i];
		 */
		}
	 if(max!=BCE_NAN)
	  {
        sprintf(station_hilfe,"%.4lf",max);
//		gcvt(max,13,station_hilfe);
		/*
		for (i=0;i<strlen(station_hilfe);i++)
			 str_netz[55+i]=station_hilfe[i];
	  */
	  }
		delete[] str;
		delete[] str1;
		delete[] temp;
	  return zaehler;


  }
  else
	 {
		delete[] str;
		delete[] str1;
		delete[] temp;
	  return	 0;
	 }
 }

/**************************************************************************/

//******************************************************************************
neuer_datensatz display_new_datensatz(WINDOW win)
/*  Generieren einer Listbox im Editor und Anzeige der noch nicht verwendeten Datensätze
Aufruf aus Hauptmenu
*/
{
  RCT rct;
  SLIST auswahl;
  SLIST datenblocktypen;
  SLIST_ELT e;
  WINDOW win_listbox;
  short neue_typen[100];
  int i,j,temp[100];
  char *str;
  
  xvt_rect_set(&rct ,660,285,960,455);     //left,top,right,bottom
  win_listbox = xvt_ctl_create(WC_LBOX, &rct, 0L, win,0L,0L, WIN_120_LISTBOX_NEU);
  ChangeFontAndSize((HWND)xvt_vobj_get_attr(win_listbox,ATTR_NATIVE_WINDOW));	// GHJ
  
  for (i=0;i<=datensatzanzahl-1;i++)     // initialisieren
  {
    temp[i]=0;
    neue_typen[i]=0;
    new_ds.typen[i] =0;
  }
  
  for (i=1; i<=ds_info[0]; i++)
    temp[typ[i]] = 1;
  
  
  /****    ausschliessen von Datenblöcken   ********/
  int  sonderprofil =0;
  if (temp[RAUHIGKEIT]) temp[RAUHIGKEIT_KST]=1;	//Rauhigkeit ks - Rauhigkeit kst
  else if (temp[RAUHIGKEIT_KST])
  {
    temp[RAUHIGKEIT]=1;    //+Rauhigkeit kst   -ks
    temp[AXM]=1;   // -AX,AY,DP
    temp[AYM]=1;
    temp[DPM]=1;
  }
  
  if ((temp[UK_BRUECKE]) || (temp[OK_BRUECKE]))     // +UK-Bruecke || +OK-Bruecke
  {
    temp[OK_WEHRS]=1;  // - OK-Wehr
    temp[TRENN_WEHR]=1;   // Trennl.Wehr
  }
  else if ((temp[TRENN_WEHR]) || (temp[OK_WEHRS]))     // +Trennl.Wehr || +OK Wehr
  {
    temp[UK_BRUECKE]=1;  // -Uk-Bruecke
    temp[OK_BRUECKE]=1;   // -OK-Bruecke
  }
      
  if (temp[SVA_WERT])
  {
    temp[AXM]=1;  // - AXM
    temp[AYM]=1;   // - AYM
    temp[DPM]=1;   // - DPM
  }
  else if ((temp[AXM])||(temp[AYM])||(temp[DPM]))
    temp[SVA_WERT]=1;  // - SVA-Werte
        
  for (int k=MAUL;k<=TRAPEZ;k++)
  {
    if (temp[k]==1)
      sonderprofil = k;
  };
  
  if (sonderprofil)
  {
    for(k=MAUL;k<=TRAPEZ;k++)
      temp[k]=1;
  };
            
  temp[MODELLGRENZEN] = 1;
  temp[BUHNE] = 1;

  // die folgenden Datenblöcke werden durch anderen Mechanismen erzeugt, 
  // der Benutzer soll sie nicht selbst anlegen
  temp[GELAENDE2] = 1;
  temp[FLAECHE] = 1;
  //temp[SCHUETZ] = 1;
  temp[COMMENT] = 1;


  // Generell die Datenblöcke "2.Geländehöhe, Fläche und Schütz auschliessen

  /****   ende  ausschliessen von Datenblöcken   ********/
  
  /*Datensatz WSP-Höhe (= Station) mehrmals zulassen*/
  if (temp[STATION]) temp[STATION]=0;
  
  
  j=0;
  for (i=1; i<=datensatzanzahl-1; i++)
  {
    if (temp[i]==0)
    {
      neue_typen[j] = i;
      j++;
    }
  }
    
  if ((auswahl = xvt_slist_create())==NULL)
  {
    xvt_dm_post_error(" Can't create slist:auswahl ");
  }
  
  if ((datenblocktypen = xvt_slist_create())==NULL)
  {
    xvt_dm_post_error(" Can't create SLIST:datenblocktypen ");
  }
  
  // Datenblocktypen aus Resource:"wspdlg.rc" lesen
  read_res_datentypen(datenblocktypen,NEU);  //in: util.cpp
  
    
  for (i=0; i<=j-1; i++)
  {
    long *data;
    for (e=xvt_slist_get_first(datenblocktypen);e !=NULL;e=xvt_slist_get_next(datenblocktypen,e))
    {
      data = xvt_slist_get_data(e);
      if (neue_typen[i]== *data)
      {
        str = xvt_slist_get(datenblocktypen,e,0L);
        new_ds.typen[i]= neue_typen[i];
        if (strlen(str)>0)
          xvt_slist_add_at_elt(auswahl,NULL,str,i);
      }
    }
    
  }
  xvt_list_add(win_listbox, 0, (char *)auswahl);   // Ausgabe in LISTBOX
  xvt_list_set_sel(win_listbox, 0, TRUE)  ;       // 1.Listeneintrag auswählen
  new_ds.listbox_win = win_listbox;
  new_ds.id=0;
  xvt_slist_destroy(datenblocktypen);
  xvt_slist_destroy(auswahl);
  return new_ds;
}
//******************************************************************************

void read_res_datentypen(SLIST datenblocktypen,int zustand)
// Datenblocktypen aus Resource:"wspdlg.rc" lesen
{
  int res_max=0;
  
  if (zustand==NEU)
    res_max = RES_ID_NEU;
  // else
  if (zustand==READ || zustand==PLOT)
    res_max = RES_ID_READ;
  
  char buf[50];
  if((editieren) && (berechnen))  //=Längsprofil
  {
    if( LWA_PROJEKT )
    {
    for (int res_id=1101;res_id<=1114;res_id++)
      if((xvt_res_get_str(res_id,buf,sizeof(buf)))==NULL)
        xvt_dm_post_error("Can't read string resource");
      else
        xvt_slist_add_at_elt(datenblocktypen,NULL,buf,res_id-1000);
      
      for (res_id=1131;res_id<=1144;res_id++)
        if((xvt_res_get_str(res_id,buf,sizeof(buf)))==NULL)
          xvt_dm_post_error("Can't read string resource");
        else
          xvt_slist_add_at_elt(datenblocktypen,NULL,buf,res_id-1000);
        //Dick 14.12.98
        if((xvt_res_get_str(1097,buf,sizeof(buf)))==NULL)   //Kommentar
          xvt_dm_post_error("Can't read string resource");
        else
          xvt_slist_add_at_elt(datenblocktypen,NULL,buf,1097-1000);
        //
        //Dick 04.02.99
        if((xvt_res_get_str(1120,buf,sizeof(buf)))==NULL)   //Wasserspiegelfixirung
          xvt_dm_post_error("Can't read string resource");
        else
          xvt_slist_add_at_elt(datenblocktypen,NULL,buf,1120-1000);
        //
        //Dick 23.09.99 neue Datensätze
        for (res_id=1121;res_id<=1125;res_id++) //Bley 8.11.2000 von 1124 auf 1125 geändert
          if((xvt_res_get_str(res_id,buf,sizeof(buf)))==NULL)   
            xvt_dm_post_error("Can't read string resource");
          else
            xvt_slist_add_at_elt(datenblocktypen,NULL,buf,res_id-1000);
          // 
    }
    else
    {
          for (int res_id=1101;res_id<=1117;res_id++)
            if((xvt_res_get_str(res_id,buf,sizeof(buf)))==NULL)
              xvt_dm_post_error("Can't read string resource");
            else
              xvt_slist_add_at_elt(datenblocktypen,NULL,buf,res_id-1000);
            
            for (res_id=1131;res_id<=1144;res_id++)
              if((xvt_res_get_str(res_id,buf,sizeof(buf)))==NULL)
                xvt_dm_post_error("Can't read string resource");
              else
                xvt_slist_add_at_elt(datenblocktypen,NULL,buf,res_id-1000);
              
              //Dick 14.12.98
              if((xvt_res_get_str(1097,buf,sizeof(buf)))==NULL)   //Kommentar
                xvt_dm_post_error("Can't read string resource");
              else
                xvt_slist_add_at_elt(datenblocktypen,NULL,buf,1097-1000);
              //
              //Dick 04.02.99
              if((xvt_res_get_str(1120,buf,sizeof(buf)))==NULL)   //Wasserspiegelfixirung
                xvt_dm_post_error("Can't read string resource");
              else
                xvt_slist_add_at_elt(datenblocktypen,NULL,buf,1120-1000);
    };
  }
  else  //Querprofil
  {
    if( LWA_PROJEKT )
    {
    for (int res_id=1001;res_id<=res_max;res_id++)
      if((xvt_res_get_str(res_id,buf,sizeof(buf)))==NULL)
        xvt_dm_post_error("Can't read string resource");
      else
        xvt_slist_add_at_elt(datenblocktypen,NULL,buf,res_id-1000);
      
      if((xvt_res_get_str(1016,buf,sizeof(buf)))==NULL)      //Schütz
        xvt_dm_post_error("Can't read string resource");
      else
        xvt_slist_add_at_elt(datenblocktypen,NULL,buf,res_id-1000);
      
      for (res_id=1020;res_id<=1028;res_id++)
        if((xvt_res_get_str(res_id,buf,sizeof(buf)))==NULL)
          xvt_dm_post_error("Can't read string resource");
        else if(res_id!=1025&&res_id!=1026&&res_id!=1023) //Dick 9.12.98 Gauss weg  24.02.2000 Kastenprofil weg
          xvt_slist_add_at_elt(datenblocktypen,NULL,buf,res_id-1000);
        
        for (res_id=1060;res_id<=1062;res_id++)
          if((xvt_res_get_str(res_id,buf,sizeof(buf)))==NULL)
            xvt_dm_post_error("Can't read string resource");
          else
            xvt_slist_add_at_elt(datenblocktypen,NULL,buf,res_id-1000);
          
          for (res_id=1070;res_id<=1074;res_id++)
            if((xvt_res_get_str(res_id,buf,sizeof(buf)))==NULL)
              xvt_dm_post_error("Can't read string resource");
            else if(zustand!=PLOT || (zustand==PLOT && res_id!=1073 && res_id!=1074))//Dick 9.12.98
              xvt_slist_add_at_elt(datenblocktypen,NULL,buf,res_id-1000);
            
            if(zustand!=PLOT)//Dick 9.12.98
              if((xvt_res_get_str(1097,buf,sizeof(buf)))==NULL)
                xvt_dm_post_error("Can't read string resource");
              else
                xvt_slist_add_at_elt(datenblocktypen,NULL,buf,97);
              
              if((xvt_res_get_str(1041,buf,sizeof(buf)))==NULL)
                xvt_dm_post_error("Can't read string resource");
              else
                xvt_slist_add_at_elt(datenblocktypen,NULL,buf,41);
              
              if((xvt_res_get_str(1045,buf,sizeof(buf)))==NULL)//Dick 23.08.99
                xvt_dm_post_error("Can't read string resource");
              else
                xvt_slist_add_at_elt(datenblocktypen,NULL,buf,45);
              
              if((xvt_res_get_str(1047,buf,sizeof(buf)))==NULL)//Dick 5.02.99
                xvt_dm_post_error("Can't read string resource");
              else
                xvt_slist_add_at_elt(datenblocktypen,NULL,buf,47);
              
              if(zustand!=PLOT)//Dick 9.12.98
                if((xvt_res_get_str(1104,buf,sizeof(buf)))==NULL)
                  xvt_dm_post_error("Can't read string resource");
                else
                  xvt_slist_add_at_elt(datenblocktypen,NULL,buf,104);
    }
    else
    {
      char buf[50];
      for (int res_id=1001;res_id<=res_max;res_id++)
        if((xvt_res_get_str(res_id,buf,sizeof(buf)))==NULL)
                    xvt_dm_post_error("Can't read string resource");
                  else
                    xvt_slist_add_at_elt(datenblocktypen,NULL,buf,res_id-1000);
                  
                  for (res_id=1027;res_id<=1028;res_id++)//Dick 24.08.99
                    if((xvt_res_get_str(res_id,buf,sizeof(buf)))==NULL)
                      xvt_dm_post_error("Can't read string resource");
                    else 
                      xvt_slist_add_at_elt(datenblocktypen,NULL,buf,res_id-1000);
                    for (res_id=1040;res_id<=1047;res_id++)   //Dick 10.06.99
                      if((xvt_res_get_str(res_id,buf,sizeof(buf)))==NULL)
                        xvt_dm_post_error("Can't read string resource");
                      else
                        xvt_slist_add_at_elt(datenblocktypen,NULL,buf,res_id-1000);
                      
                      for (res_id=1060;res_id<=1062;res_id++)
                        if((xvt_res_get_str(res_id,buf,sizeof(buf)))==NULL)
                          xvt_dm_post_error("Can't read string resource");
                        else
                          xvt_slist_add_at_elt(datenblocktypen,NULL,buf,res_id-1000);
                        
                        if((xvt_res_get_str(1080,buf,sizeof(buf)))==NULL)
                          xvt_dm_post_error("Can't read string resource");
                        else
                          xvt_slist_add_at_elt(datenblocktypen,NULL,buf,80);
                        
                        //  1097= Kommentar für Konvertierung JABRON->BCE
                        if (res_max == RES_ID_READ)
                        {
                          if((xvt_res_get_str(1097,buf,sizeof(buf)))==NULL)
                            xvt_dm_post_error("Can't read string resource");
                          else
                            xvt_slist_add_at_elt(datenblocktypen,NULL,buf,97);
                        }
    };                        
  } //else nicht Längsschnitt
  }
//*****************************************************************************
void add_new_datensatz(WINDOW win_list2)
{
 int z;
 xvt_list_add(win_list2,-1,new_ds.datensatz);    // '-1':= an Ende der Liste anhängen
 ds_info[0]++;
 anzahl_ds=ds_info[0];
 typ[anzahl_ds]=new_ds.id;
 ds_info[anzahl_ds]=0;
 xvt_list_set_sel(win_list2,anzahl_ds-1,TRUE);
 list->MakeNewNode(ds_info[0]);

 switch (typ[anzahl_ds])
	 {
	  case RAUHIGKEIT:
	  case RAUHIGKEIT_KST:
			{
			list->MakeNewKoord(ds_info[1]);
			list->CopyStation(anzahl_ds);  // Stationswerte in neuen Datensatz kopieren
			ds_info[anzahl_ds]=ds_info[1];
			}
		 break;
	  case DURCHST_BEREICH:
		{
			ds_info[ds_info[0]]=2;
			 for (z=0;z<14;z++)
				 scr.z[z]= BCE_NAN;
		}
			break;
	  case AXM:
	  case AYM:
	  case DPM:
		  {
			list->MakeNewKoord(ds_info[1]);
			list->CopyStation(anzahl_ds);  // Stationswerte in neuen Datensatz kopieren
			ds_info[anzahl_ds]=ds_info[1];
			list->InitKoord(anzahl_ds,0.0);  // mit 0.0 initialisieren

		  }
		break;
      case OK_WEHRS:
		  {
            Koord *gel,*wehr;
			list->MakeNewKoord(ds_info[1]);
			list->CopyStation(anzahl_ds);  // Stationswerte in neuen Datensatz kopieren
			ds_info[anzahl_ds]=ds_info[1];
            list->WriteTypDaten(anzahl_ds,new_ds.id,NULL);
            gel=list->HoleDatensatz(GELAENDEHOEHE);
            wehr=list->HoleDatensatz(OK_WEHRS);
			list->CopyDatenL(wehr,gel);
		  }
          break;
	  case TRENNFLAECHEN:
  		{
			 for (z=0;z<=14;z++)
				 scr.z[z]= BCE_NAN;
			 ds_info[anzahl_ds]=2;   
		 }
			break;

	  case BORDVOLL:
    case MODELLGRENZEN:
		{
			 for (z=0;z<=14;z++)
				 scr.z[z]= BCE_NAN;
       ds_info[anzahl_ds]=2;
		 }
			break;
    if( LWA_PROJEKT )
    {
	  case OK_BRUECKE:
			{
			 for (z=0;z<=14;z++)
				 scr.z[z]= BCE_NAN;
			 ds_info[anzahl_ds]=1;
			}
		  break;

	  case SCHUETZ:
			{
			 for (z=0;z<1;z++)
			 {
				 scr.y[z]=0;
				 scr.z[z]= 0;
			 }
			 ds_info[anzahl_ds]=0;
			}
		  break;
    };
	  case KREISSEGM:
			{
			 for (z=0;z<=14;z++)
				 scr.z[z]= BCE_NAN;
			 ds_info[anzahl_ds]=3;
			}
		  break;
	  case KASTEN:
			{
			 for (z=0;z<=14;z++)
				 scr.z[z]= BCE_NAN;
			 ds_info[anzahl_ds]=4;
			}
		  break;
	  case MAUL:
			{
			 for (z=0;z<=4;z++)
				 scr.z[z]= BCE_NAN;
			 ds_info[anzahl_ds]=0;
			}
		  break;
	  case NWRINNE:
	  case ARMCO84:
	  case ARMCO71:
			 ds_info[anzahl_ds]=0;

		  break;
	  case EIPROFIL:
			{
			 for (z=0;z<=4;z++)
				 scr.z[z]= BCE_NAN;
			 ds_info[anzahl_ds]=0;
			}
		  break;
	  case KREIS:
			{
			 for (z=0;z<4;z++)
				 scr.z[z]= BCE_NAN;
			 ds_info[anzahl_ds]=0;
			}
		  break;
	  case TRAPEZ:
			{
			 int i;
			for(i=0;i<6;i++)
			 scr.z[i]=BCE_NAN;
			ds_info[anzahl_ds]=0;
			}
		  break;
      case GAUSS:
		  int i;
		  for(i=0;i<6;i++)
			 scr.z[i]=BCE_NAN;
		  ds_info[anzahl_ds]=3;
          break;
	  case COMMENT:
			 {
			  slist_comment = xvt_slist_create();
			 }
		  break;
	  default:
		  {
			list->MakeNewKoord(ds_info[1]);
			list->CopyStation(anzahl_ds);  // Stationswerte in neuen Datensatz kopieren
			ds_info[anzahl_ds]=ds_info[1];
		  }
		  break;
	 }
  list->WriteTypDaten(anzahl_ds,new_ds.id,NULL);
}
//******************************************************************************
void delete_datensatz(WINDOW win)
{
/*  Generieren einer Listbox im Alpha-Editor und
	 Anzeige der  verwendeten Datensätze zum löschen
	 Aufruf aus Hauptmenu
*/
 SLIST typliste;
 RCT rct;
 WINDOW win_listbox;


 //xvt_rect_set(&rct ,580,236,880,388);
 xvt_rect_set(&rct ,660,285,960,455);//Dick 11.11.98  
 win_listbox = xvt_ctl_create(WC_LBOX, &rct, 0L, win,0L,0L, WIN_120_LISTBOX_DEL);
 ChangeFontAndSize((HWND)xvt_vobj_get_attr(win_listbox,ATTR_NATIVE_WINDOW));	// GHJ

 new_ds.listbox_win = win_listbox;
 new_ds.id=0;

 if ((typliste = xvt_slist_create())==NULL)
		xvt_dm_post_error(" Can't create slist:typliste ");

 typliste = xvt_list_get_all(win_list2);

 xvt_list_add(win_listbox, 0, (char *)typliste);   // Ausgabe in LISTBOX

/*
 for (int i=1;i<=anzahl_ds;i++)
  {
	if (typ[i]==1) // Datensatz:Gelände Höhe aus Liste löschen
		xvt_list_rem(win_listbox,i-1);
  }
*/
 xvt_list_set_sel(win_listbox, 0, FALSE)  ;       // 1.Listeneintrag auswählen
 xvt_slist_destroy(typliste);

}
/****************************************************************************
*             SET121 .CPP                                                  *
*             14.10.1994                                                    *
****************************************************************************/
/*SET124*/
WINDOW radio1[3];
WINDOW radio2[3];
WINDOW radio3[4];
WINDOW radio4[4];
WINDOW radio5[3];
WINDOW radio6[3];
WINDOW radio7[4];
WINDOW radio8[5];
char line3[31];

WINDOW xdedit121[25];
extern SLIST header_profil;
extern char temp[100];
char			header_prof_z3[30],  //von extern geändert
				header_prof_z5[30],  //von 25 auf 30
				header_prof_z6[30],
				header_prof_z7[30];
extern char str[25];
char *ptr1;
char line[100];


void wsp121_set_title(WINDOW win)
	{
	 int strlg,i;
	 char help[100];
		  xdedit121[0]=xvt_win_get_ctl(win,WIN_121_EDIT_16);//Auftraggeber 1
		  xdedit121[1]=xvt_win_get_ctl(win,WIN_121_EDIT_17);//Auftraggeber 2
		  xdedit121[2]=xvt_win_get_ctl(win,WIN_121_EDIT_18);//Projektbez.1
		  xdedit121[3]=xvt_win_get_ctl(win,WIN_121_EDIT_19);//Projektbez.2
		  xdedit121[4]=xvt_win_get_ctl(win,WIN_121_EDIT_20);//Projektbez.3
		  xdedit121[5]=xvt_win_get_ctl(win,WIN_121_EDIT_21);//Gewässername
		  xdedit121[6]=xvt_win_get_ctl(win,WIN_121_EDIT_22);//Blattbez.1
		  xdedit121[7]=xvt_win_get_ctl(win,WIN_121_EDIT_23);//Station      :disabled
		  xdedit121[8]=xvt_win_get_ctl(win,WIN_121_EDIT_24);//Projektnummer
		  xdedit121[9]=xvt_win_get_ctl(win,WIN_121_EDIT_25);//Datum
		  xdedit121[10]=xvt_win_get_ctl(win,WIN_121_EDIT_27);//Zeichnugsüberschrift

		  ptr1=xvt_slist_get_elt(header_profil,1,0);  //Zeile 2 Profildatei
		  xvt_vobj_set_title(xdedit121[0],ptr1);

		  ptr1=xvt_slist_get_elt(header_profil,2,0);  //Zeile 3 Profildatei
		  strcpy(help,ptr1);
		  strlg=strlen(help);
		  if(strlg>40)
			for(i=40;i<=strlg;i++)
			 header_prof_z3[i-40]=help[i];
		  if(help[0]=='\0')
			{
			 help[0]=' ';
			 help[1]='\0';
			}
		  help[39]='\0';
		  xvt_vobj_set_title(xdedit121[1],help);

		  ptr1=xvt_slist_get_elt(header_profil,3,0); //Zeile 4 Profildatei
		  xvt_vobj_set_title(xdedit121[2],ptr1);

		  ptr1=xvt_slist_get_elt(header_profil,4,0); //Zeile 5 Profildatei
		  strcpy(help,ptr1);
		  strlg=strlen(help);
		  if(strlg>40)
			for(i=40;i<=strlg;i++)
			 header_prof_z5[i-40]=help[i];

		  if(help[0]=='\0')
			{
			 help[0]=' ';
			 help[1]='\0';
			}
		  help[39]='\0';
		  xvt_vobj_set_title(xdedit121[3],help);

		  ptr1=xvt_slist_get_elt(header_profil,5,0);  //Zeile 6 Profildatei:Gewässername
		  strcpy(help,ptr1);
		  strlg=strlen(help);
		  if(strlg>40)
			for(i=40;i<=strlg;i++)
			 header_prof_z6[i-40]=help[i];

		  if(help[0]=='\0')
			{
			 help[0]=' ';
			 help[1]='\0';
			}
		  help[39]='\0';
		  xvt_vobj_set_title(xdedit121[4],help);

		  ptr1=xvt_slist_get_elt(header_profil,6,0);  //Zeile 7 Profildatei:Blattbez.
		  strcpy(help,ptr1);
		  strlg=strlen(help);
		  if(strlg>40)
			for(i=40;i<=strlg;i++)
			 header_prof_z7[i-40]=help[i];
		  if(help[0]=='\0')
			{
			 help[0]=' ';
			 help[1]='\0';
			}
		  help[39]='\0';
		  xvt_vobj_set_title(xdedit121[5],help);

		  if((berechnen) &&(editieren))
			{
			 xvt_vobj_set_enabled(xdedit121[6],FALSE);
			}
		  else
          xvt_vobj_set_enabled(xdedit121[6],TRUE);

		  ptr1=xvt_slist_get_elt(header_profil,7,0); //Zeile 8 Profildatei :Blattbez.
      if( _strnicmp( ptr1, "QUERPROFIL ", strlen( "QUERPROFIL " ) ) == 0 )
        ptr1 += strlen( "QUERPROFIL " );
		  xvt_vobj_set_title(xdedit121[6],ptr1);

		  if((berechnen) &&(editieren))
			{
			 help[0]=' ';
			 help[1]='\0';
			 xvt_vobj_set_title(xdedit121[7],help);    //Zeile 9:Station km
			}
		  else
			{
			 test_line9(str);
			 xvt_vobj_set_title(xdedit121[7],str);    //Zeile 9:Station km
			}
		  zeige_slist(header_profil);
		  ptr1=xvt_slist_get_elt(header_profil,9,0); //Zeile 10:Projektnummer
		  xvt_vobj_set_title(xdedit121[8],ptr1);

		  zeige_slist(header_profil);
		  ptr1=xvt_slist_get_elt(header_profil,10,0);  //Zeile 11:Datum
		  xvt_vobj_set_title(xdedit121[9],ptr1);

		  ptr1=xvt_slist_get_elt(header_profil,12,0); //Zeile13   :Zeichnungsüberschrift
		  xvt_vobj_set_title(xdedit121[10],ptr1);

	  }

/********************************************************************/
void wsp121_save_title(void)
{
		 //Window 121 Edit-Felder setzen: Schlüsseldaten
		  
		  char str1[100],*p1;
		  /*   Funktion: "xvt_slist_change_str()" in :util.h !!
				 keine xvt-Funktion  !!! */

		  xvt_vobj_get_title(xdedit121[0],temp,36);  //Zeile 2:Auftraggeber1
		  xvt_slist_change_str(header_profil,temp,1);

		  xvt_vobj_get_title(xdedit121[1],temp,36);  //Zeile 3:Auftraggeber2
		  for (int g=0;g<100;g++) str1[g]=' ';
		  strcpy(str1,temp);
		  p1 = strchr(str1,'\0');
		  p1[0]=' ';
		  p1= &str1[40];
		  strcpy(p1,header_prof_z3);
		  xvt_slist_change_str(header_profil,str1,2);

		  xvt_vobj_get_title(xdedit121[2],temp,36);  //Zeile 4:Projektbez.1
		  xvt_slist_change_str(header_profil,temp,3);

		  xvt_vobj_get_title(xdedit121[3],temp,36);  //Zeile 5:Projektbez.2
		  for (g=0;g<100;g++) str1[g]=' ';
		  strcpy(str1,temp);
		  p1 = strchr(str1,'\0');
		  p1[0]=' ';
		  p1= &str1[40];
		  strcpy(p1,header_prof_z5);
		  xvt_slist_change_str(header_profil,str1,4);

		  xvt_vobj_get_title(xdedit121[4],temp,36);//Zeile 6: Projektbez. 3
		  for (g=0;g<100;g++) str1[g]=' ';
		  strcpy(str1,temp);
		  p1 = strchr(str1,'\0');
		  p1[0]=' ';
		  p1= &str1[40];
		  strcpy(p1,header_prof_z6);
		  xvt_slist_change_str(header_profil,str1,5);

		  /*Zeile 7 */
		  xvt_vobj_get_title(xdedit121[5],temp,36);  //Zeile 7:Gewässername/Zustand
		  for (g=0;g<100;g++) str1[g]=' ';
		  strcpy(str1,temp);
		  p1 = strchr(str1,'\0');
		  p1[0]=' ';
		  p1= &str1[40];
		  strcpy(p1,header_prof_z7);
		  xvt_slist_change_str(header_profil,str1,6);
		  /*Zeile 7 */

		  xvt_vobj_get_title(xdedit121[6],temp,36);  //Zeile 8 Profildatei :Blattbez.2
		  xvt_slist_change_str(header_profil,temp,7);

		  xvt_vobj_get_title(xdedit121[8],temp,16);  //Zeile 10:Projektnummer
		  xvt_slist_change_str(header_profil,temp,9);

		  xvt_vobj_get_title(xdedit121[9],temp,16);  //Zeile 11: Datum
		  xvt_slist_change_str(header_profil,temp,10);


		  xvt_vobj_get_title(xdedit121[10],temp,36);  //Zeile 13:Zeichnungsüberschrift
		  xvt_slist_change_str(header_profil,temp,12);
}
/*********************************************************************/
 void header_zusammensetzten(void)
  {
	/*In W121 werden Schlüsselbegriffe zuerst aus header entfernt, bei
	 Abbruch werden diese nicht gesichert, daher müssen sie wieder drangesetzt
	 werden*/
		  int i;
		  char str1[100],*p1;

		  p1=xvt_slist_get_elt(header_profil,2,0);  //Zeile 3 Profildatei
		  strcpy(str1,p1);
		  if(strlen(str1)<40)
			{
			 for(i=strlen(str1);i<40;i++)
			  str1[i]=' ';
			}
		  str1[40]='\0';
		  strcat(str1,header_prof_z3);
		  xvt_slist_change_str(header_profil,str1,2);


		  p1=xvt_slist_get_elt(header_profil,4,0);  //Zeile 5 Profildatei
		  strcpy(str1,p1);
		  if(strlen(str1)<40)
			{
			 for(i=strlen(str1);i<40;i++)
			  str1[i]=' ';
			}
		  str1[40]='\0';
		  strcat(str1,header_prof_z5);
		  xvt_slist_change_str(header_profil,str1,4);

		  p1=xvt_slist_get_elt(header_profil,5,0);  //Zeile 6 Profildatei
		  strcpy(str1,p1);
		  if(strlen(str1)<40)
			{
			 for(i=strlen(str1);i<40;i++)
			  str1[i]=' ';
			}
		  str1[40]='\0';
		  strcat(str1,header_prof_z6);
		  xvt_slist_change_str(header_profil,str1,5);

		  p1=xvt_slist_get_elt(header_profil,6,0);  //Zeile 7 Profildatei
		  strcpy(str1,p1);
		  if(strlen(str1)<40)
			{
			 for(i=strlen(str1);i<40;i++)
			  str1[i]=' ';
			}
		  str1[40]='\0';
		  strcat(str1,header_prof_z7);
		  xvt_slist_change_str(header_profil,str1,6);
  }
/********************************************************************/
//#endif

int check_border(void)
{
  int check_border=TRUE;
  int anz_bewuchsparm=0;
  bool trennflaechen_da=FALSE,
       durchst_da=FALSE,
       rauh_da=FALSE;
  char buff[50];
  for (int ds=1;ds<=ds_info[0];ds++)
  {
    if (typ[ds]==KASTEN)
    {
      list->Delete_Min_Koord(ds);
      if	(ds_info[ds]>4)
      {
        //xvt_dm_post_error("Im Kastenprofil sind mehr als 4 Geländehöhen vorhanden!\nBitte korrigieren!");
        char buf[200],buf2[200];
        xvt_res_get_str(STR_KASTEN_NOTE_1,buf,sizeof(buf));
        xvt_res_get_str(STR_KORR,buf2,sizeof(buf2));
        xvt_dm_post_note("%s\n%s",buf,buf2); 
        check_border=FALSE;
      }
      if (ds_info[ds]<4)
      {
        //xvt_dm_post_error("Im Kastenprofil sind weniger als 4 Geländehöhen vorhanden!\nBitte korrigieren!");
        char buf[200],buf2[200];
        xvt_res_get_str(STR_KASTEN_NOTE_2,buf,sizeof(buf));
        xvt_res_get_str(STR_KORR,buf2,sizeof(buf2));
        xvt_dm_post_note("%s\n%s",buf,buf2);
        check_border=FALSE;
      }
    }
    if ((typ[ds]==AXM)||(typ[ds]==AYM)||(typ[ds]==DPM))
      anz_bewuchsparm++;

    if( LWA_PROJEKT )
    {
    {
      if (typ[ds]==OK_BRUECKE || typ[ds]==UK_BRUECKE)
      {
        list->Delete_Min_Koord(ds);
        if (ds_info[ds]<2 && ds_info[ds]>0)
        {
          char buf[200],buf2[200],buf3[200];//Dick 26.11.99
          
          xvt_res_get_str(STR_JA,buf,sizeof(buf));
          xvt_res_get_str(STR_NEIN,buf2,sizeof(buf2));
          xvt_res_get_str(STR_CHECKBORD_NOTE,buf3,sizeof(buf3));
          
          //	switch(xvt_dm_post_ask("Ja","Nein",NULL,"In OK/UK-Brücke sind weniger als 2 Geländehöhen vorhanden!\nGeländepunkte bei der OK-Brücke müssen gleiche Höhen aufweisen!\nWollen Sie es korrigieren?"))
          switch(xvt_dm_post_ask(buf,buf2,NULL,buf3))
          {
          case RESP_DEFAULT:
            check_border=FALSE;
            break;
          }
        }
        else if(ds_info[ds]<=0)
        {
          char buf[200],buf2[200],buf3[200];//Dick 26.11.99
          
          xvt_res_get_str(STR_NEIN,buf,sizeof(buf));
          xvt_res_get_str(STR_JA,buf2,sizeof(buf2));
          xvt_res_get_str(STR_CHECKBORD_NOTE2,buf3,sizeof(buf3));
          //switch(xvt_dm_post_ask("Nein","Ja",NULL,"In OK/UK-Brücke sind keine Geländehöhen vorhanden!\nWenn Sie fortfahren, wird leere Datensatz gelöscht.\nFortfahren?"))
          switch(xvt_dm_post_ask(buf,buf2,NULL,buf3))
          {
          case RESP_DEFAULT:
            check_border=FALSE;
            break;
          }
        }
      }
    }
    
    if (typ[ds]==OK_WEHRS)
    {
      list->Delete_Min_Koord(ds);
      if (ds_info[ds]<2 && ds_info[ds]>0)
      {
        char buf[200],buf2[200],buf3[200];//Dick 26.11.99
        
        xvt_res_get_str(STR_JA,buf,sizeof(buf));
        xvt_res_get_str(STR_NEIN,buf2,sizeof(buf2));
        xvt_res_get_str(STR_CHECKBORD_NOTE3,buf3,sizeof(buf3));
        
        //switch(xvt_dm_post_ask("Ja","Nein",NULL,"In OK-Wehr sind weniger als 2 Geländehöhen vorhanden!\nGeländepunkte bei der OK-Wehr müssen gleiche Höhen aufweisen!\nWollen Sie es korrigieren?"))
        switch(xvt_dm_post_ask(buf,buf2,NULL,buf3))
        {
        case RESP_DEFAULT:
          check_border=FALSE;
          break;
          
        }
      }
      else if(ds_info[ds]<=0)
      {
        char buf[200],buf2[200],buf3[200];//Dick 26.11.99
        
        xvt_res_get_str(STR_NEIN,buf,sizeof(buf));
        xvt_res_get_str(STR_JA,buf2,sizeof(buf2));
        xvt_res_get_str(STR_CHECKBORD_NOTE4,buf3,sizeof(buf3));
        
        //switch(xvt_dm_post_ask("Nein","Ja",NULL,"In OK-Wehr sind keine Geländehöhen vorhanden!\nWenn Sie fortfahren, wird leere Datensatz gelöscht.\nFortfahren?"))
        switch(xvt_dm_post_ask(buf,buf2,NULL,buf3))
        {
        case RESP_DEFAULT:
          check_border=FALSE;
          break;
        }
      }					
    }
    }
    else
    {
    if ((typ[ds]==OK_BRUECKE)||(typ[ds]==UK_BRUECKE))
    {
      list->Delete_Min_Koord(ds);
      
      if(ds_info[ds]<3)
      {
        //xvt_dm_post_error("In Ok / Uk-Brücke sind weniger als 3 Geländehöhen vorhanden!\nBitte korrigieren!");
        char buf[200],buf2[200];
        xvt_res_get_str(STR_BRUECKE_NOTE,buf,sizeof(buf));
        xvt_res_get_str(STR_KORR,buf2,sizeof(buf2));
        xvt_dm_post_note("%s\n%s",buf,buf2);
        
        check_border=FALSE;
      }
    }
    };
    
    if (typ[ds]==TRENNFLAECHEN)
      trennflaechen_da=TRUE;
    if (typ[ds]==DURCHST_BEREICH)
      durchst_da=TRUE;
    if (typ[ds]==RAUHIGKEIT || typ[ds]==RAUHIGKEIT_KST)
      rauh_da=TRUE;
  } // for ds
  if( ( !trennflaechen_da || !durchst_da || !rauh_da ) && !( berechnen && editieren ) )
  {
    char buf[200],buf2[200],buf3[200];//Dick 26.11.99
    
    xvt_res_get_str(STR_JA,buf,sizeof(buf));
    xvt_res_get_str(STR_NEIN,buf2,sizeof(buf2));
    xvt_res_get_str(STR_CHECKBORD_NOTE5,buf3,sizeof(buf3));
    
    //switch (xvt_dm_post_ask("Ja","Nein",NULL,"Es müssen mindenstens\n Geländehöhe, Trennflächen, Durchströmte Bereiche und Rauhheit angegeben werden!\n"
    //                                        "Wollen Sie die fehlenden Parameter automatisch anlegen lassen?"))
    switch(xvt_dm_post_ask(buf,buf2,NULL,buf3))
    {
    case RESP_DEFAULT:       //Ja
      {
        if(!trennflaechen_da)
        {
          if((xvt_res_get_str(TRENNFLAECHEN+1000,buff,sizeof(buff)))==NULL)   //Kommentar
            xvt_dm_post_error("Can't read string resource");
          
          new_ds.datensatz=buf;                                   
          new_ds.id=TRENNFLAECHEN;                                                                      
          add_new_datensatz(win_list2);   //in:util.cpp
          list->GetMinMax(&pmm,1);
          if (scr.z[0] == BCE_NAN)
          {
            scr.z[0]=pmm.minX;
            scr.z[2]=1.0;
          }
          if (scr.z[1] == BCE_NAN)
          {
            scr.z[1]=pmm.maxX;
            scr.z[3]=2.0;
          }
          scr.datensatz=ds_info[0];                                   
          list->SaveSonderprofildaten(&scr, TRENNFLAECHEN);
        }
        if(!durchst_da)
        {
          if((xvt_res_get_str(DURCHST_BEREICH+1000,buff,sizeof(buff)))==NULL)   //Kommentar
            xvt_dm_post_error("Can't read string resource");
          
          new_ds.datensatz=buff;                                   
          new_ds.id=DURCHST_BEREICH;                                                                      
          add_new_datensatz(win_list2);   //in:util.cpp
          list->GetMinMax(&pmm,1);                                   
          if (scr.z[0] == BCE_NAN)
            scr.z[0]=pmm.minX;
          if (scr.z[1] == BCE_NAN)
            scr.z[1]=list->Get_Station_Hoehe(pmm.minX);
          if(scr.z[1]==BCE_NAN)
            scr.z[1]=0;
          if (scr.z[2] == BCE_NAN)
            scr.z[2]=pmm.maxX;
          if (scr.z[3] == BCE_NAN)
            scr.z[3]=list->Get_Station_Hoehe(pmm.maxX);
          if(scr.z[3]==BCE_NAN)
            scr.z[3]=0;
          scr.datensatz=ds_info[0];
          list->SaveSonderprofildaten(&scr, DURCHST_BEREICH);
        }
        if(!rauh_da)
        {
          char rauh_mode[MAX_PATH];
          int rauhtyp=0;
          GetPrivateProfileString("WSPWIN","AUTORAUHEIT","NEIN",rauh_mode,MAX_PATH,"WSPWIN.INI"); 
          if(strcmp(rauh_mode,"KS")==0)
            rauhtyp=RAUHIGKEIT;
          else
            if(strcmp(rauh_mode,"KST")==0)
              rauhtyp=RAUHIGKEIT_KST;
            else
            {
              //xvt_dm_post_note("Rauheit wurde nicht erstellt!\nGeben Sie unter dem Menüpunkt \"Extras-Optionen...\" an,\nob der Datensatz KS oder KST Rauheiten automatisch mit erstellt werden soll!");
              char buf[200];
              xvt_res_get_str(STR_RAUH_NOTE,buf,sizeof(buf));
              xvt_dm_post_note("%s",buf);
              break;
            }
            if((xvt_res_get_str(rauhtyp+1000,buff,sizeof(buff)))==NULL)   //Kommentar
              xvt_dm_post_error("Can't read string resource");
            
            new_ds.datensatz=buff;                                   
            new_ds.id=rauhtyp;                                                                      
            add_new_datensatz(win_list2);   //in:util.cpp
        }
      }
      break;
      
    case RESP_2:             // nein
      {
        check_border=TRUE;
      }
      break;
    }
  }
  if ((anz_bewuchsparm>0)&&(anz_bewuchsparm<3))
  {
    //xvt_dm_post_note("Es müssen alle Bewuchsparameter: AX, AY, DP angegeben werden!");
    char buf[200];
    xvt_res_get_str(STR_BEWUCHS_NOTE,buf,sizeof(buf));
    xvt_dm_post_note("%s",buf); 
    check_border=FALSE;
  }
  return check_border;
} // int check_border(void)

/********************************************************************/
/*
 *  Fkt. write_datplt aus main_dll herausgenommen, da es beim Aufruf unter NT4.0
 * zu einer  Schutzverletzung kommt 
 * Nochmal geändert: es wird jetzt nichts rausgeschrieben, keiner will die .plt Datei haben...
 * aber sonst bleibt die Funktion gleich (überliest Schrott aus den .prf Dateien)
 */
int write_datplt(FILE *in,char *filename,BOOLEAN* exist_plot ,int read_first_line)
{
  /*****        Sichern des Plotterdatensatzes    ******/
//  FILE *temporaer;
  
//  char name[15];
  int first_line=0;
  
//  strcpy( name,filename );
//  name[  strlen(name)-3 ] ='\0';
//  strcat(name,"plt");
//  if ((temporaer =fopen(name,"w"))==NULL)
//  {
//    //	xvt_dm_post_error("Fehler:Kann Datei %s nicht öffnen !",filename);
//    char buf[200],buf2[200],buf3[200];
//    xvt_res_get_str(STR_DATEI,buf,sizeof(buf));
//    xvt_res_get_str(STR_CANOTOPEN,buf2,sizeof(buf2));
//    xvt_res_get_str(STR_ERROR,buf3,sizeof(buf3));
//    xvt_dm_post_error("%s:%s:%s",buf3,buf,name,buf2);
//  }
  
  while( !feof( in ) )
  {
    char temp[LENLINE + 1];
    fgets( temp, LENLINE, in );
    
    if( !read_first_line ) // Zeile "datplt" wurde bereits gelesen
    {
      //fputs( "datplt", temporaer );
      first_line = 1;
    }

    if( !first_line )
    {
      while( ( !feof( in ) ) &&( temp[0] == '\n' ) )
        fgets( temp, LENLINE, in );
      
      if( ( !feof( in ) ) && ( xvt_str_match( temp,"datplt*", FALSE ) ) )
      {
        first_line = 1;
      }
      else
      {
//        //xvt_dm_post_note("Datei enthält nicht lesbare Daten!\nDiese werden in Datei\n%s\nabgelegt !",name);
//        char buf[200],buf2[200];
//        xvt_res_get_str(STR_DATPLT_L_1,buf,sizeof(buf));
//        xvt_res_get_str(STR_DATPLT_L_2,buf2,sizeof(buf2));
//        xvt_dm_post_note("%s%s%s",buf,name,buf2);
//        return 0;
      }
    }
//    fputs(temp,temporaer);
  }
//  fclose(temporaer);
  *exist_plot = FALSE;
  return 1;
}
/********************************************************************/
void zeige_slist(SLIST list)
{
  // zeigt SLIST list in liste[] an,nur intern verwenden
  SLIST_ELT e;
  char *str;
  char liste[50][200];
  int i=0;
  for (e=xvt_slist_get_first(list);e !=NULL;e=xvt_slist_get_next(list,e))
  {
    if (i<49)
			 {
      str = xvt_slist_get(list,e,0L);
      strcpy(liste[i],str);
      i++;
			 }
  }
  strcpy(liste[i],"ende");
  i=0;  //dient nur als breakpoint !!
}

BOOL CopyDir(char* quelle,char* ziel)
{
  BOOL fehler=FALSE;
  SLIST filelist;
  SLIST_ELT e;
  char temp_q[250],temp_z[250],temp_q2[250],temp_z2[250],*file;//,*p;
  FILE_SPEC f;
  
  /*  alle Dateien im Verzeichnis "quelle" und Unterverzeichnisse nach "ziel" kopieren  */
  
  strcpy(temp_q,quelle);
  strcpy(temp_z,ziel);
  
  if((access(temp_q,00))!=0)
    return TRUE;
  
  if((access(temp_z,00))!=0)
  {
    if(verzeichnis_anlegen(temp_z))
      return TRUE;
  }
  
  if (temp_q[0]!='.')
  {
    xvt_fsys_convert_str_to_dir(temp_q,&f.dir);
    if((xvt_fsys_set_dir(&f.dir)))   //set current directory
    {
      if((filelist = xvt_fsys_list_files("","",TRUE))!=NULL)
      {
        for (e=xvt_slist_get_first(filelist);e!=NULL;
        e=xvt_slist_get_next(filelist,e))
        {
          file=xvt_slist_get(filelist,e,0L);
          if (file[0]!='.')
          {
            strcpy(f.name,file);
            f.type[0] = '\0';
            f.creator[0] = '\0';
            if (xvt_fsys_get_file_attr( &f, XVT_FILE_ATTR_DIRECTORY ))
            {
              strcpy(temp_q,quelle);
              strcat(temp_q,"\\");
              strcat(temp_q,file);
              strcpy(temp_z,ziel);
              strcat(temp_z,"\\");
              strcat(temp_z,file);
              CopyDir(temp_q,temp_z);
            }
            else
            {
              strcpy(temp_q2,quelle);
              strcat(temp_q2,"\\");
              strcat(temp_q2,file);
              strcpy(temp_z2,ziel);
              strcat(temp_z2,"\\");
              strcat(temp_z2,file);
              CopyFile(temp_q2,temp_z2,TRUE);
            }
          }
        }
      }
    }
    else
    {
      fehler =TRUE;
      //xvt_dm_post_error("Gewähltes Projekt:\n%s\nexistiert nicht mehr !",quelle);
      char buf[200],buf2[200];
      xvt_res_get_str(STR_FILEDLG_NOTE_5,buf,sizeof(buf));
      xvt_res_get_str(STR_FILEDLG_NOTE_6,buf2,sizeof(buf2));
      xvt_dm_post_error("%s%s\n%s",buf,quelle,buf2); 
    }
  }
  return fehler;  
}



typedef struct _COMDLG32
{
  char Verz[80];		// a test buffer containing the file selected
  // a test buffer containing the file path
} COMDLG32, FAR * LPCOMDLG32;

COMDLG32 sCOMDLG32;
LPTSTR lpStr;
TCHAR filt[256];
#define IDD_DIRECTORY 116
#define IDD_COMDLG32  29505
#define IDE_SELECTED  1022
#define IDE_PATH      102
#define MAX_PATH             260
UINT APIENTRY OpenSaveHookProc(HWND hwnd, UINT msg, UINT wParam, LONG lParam) ;
void ProcessCDError(DWORD dwErrorCode, HWND hWnd);
BOOL NEAR PASCAL TestNotify(HWND hDlg, LPOFNOTIFY pofn);
//UINT nF_OK;

BOOLEAN Verzeichnis_waehlen(HWND hwnd,HINSTANCE hInst,char *szTempName,char *szInitialDir,char *szDirName)
{
  //LPOPENFILENAME po;     
  OPENFILENAME po;
  BOOLEAN bRet;
  //HANDLE hRes,hDialog ;
  int nInc=0,i;
  //hRes = FindResource( GetModuleHandle("wspwin.exe"), MAKEINTRESOURCE(IDD_COMDLG32), RT_DIALOG) ;
  //hDialog = LoadResource( GetModuleHandle("wspwin.exe"), hRes) ;
  //nF_OK = RegisterWindowMessage((LPTSTR) FILEOKSTRING) ;
  //filter   
  for(i=0;i<256;i++)
    filt[i]=0;
  lpStr=filt;
  lstrcpy(lpStr,"Verzeichnise");
  nInc+=lstrlen(lpStr) + 1;
  lpStr=&filt[nInc];
  lstrcpy(lpStr,"1.1");
  nInc+=lstrlen(lpStr) + 1;
  lpStr=&filt[nInc];
  filt[nInc]='\0';
  //
  szDirName[0]='\0';
  po.lStructSize          = sizeof(OPENFILENAME) ;
  po.hwndOwner            = hwnd ;
  po.hInstance            =(HINSTANCE)GetWindowLong(hwnd,GWL_HINSTANCE);//(HINSTANCE)hDialog;//GetWindowLong(hwnd,GWL_HINSTANCE);
  po.lpstrFilter          =filt ;
  po.lpstrCustomFilter    =NULL ;
  po.nMaxCustFilter       = 40 ;
  po.nFilterIndex         = 0;
  po.lpstrFile            = (LPSTR)szDirName ;
  po.nMaxFile             = _MAX_PATH ;
  po.lpstrFileTitle       = NULL ;
  po.nMaxFileTitle        = 0 ;
  po.lpstrInitialDir      = szInitialDir ;                                      //HANDLE
  po.lpstrTitle           = "Verzeichnis wählen" ;
  po.Flags                =  OFN_EXPLORER |OFN_ENABLEHOOK | OFN_ENABLETEMPLATE | OFN_SHOWHELP | OFN_CREATEPROMPT | OFN_NOVALIDATE | OFN_NOCHANGEDIR;//| OFN_NOVALIDATE | OFN_NODEREFERENCELINKS;//| OFN_EXPLORER;// OFN_SHOWHELP | OFN_EXTENSIONDIFFERENT |OFN_NOVALIDATE;//|OFN_LONGNAMES |  OFN_ENABLEHOOK | OFN_ENABLETEMPLATE ;
  po.nFileOffset          = 0 ;
  po.nFileExtension       = 0 ;
  po.lpstrDefExt          = NULL;
  po.lCustData            = (LPARAM)&sCOMDLG32 ;
  po.lpfnHook             = OpenSaveHookProc ;
  po.lpTemplateName       = MAKEINTRESOURCE(IDD_COMDLG32);
  bRet = GetOpenFileName(&po) ;
  DWORD err=CommDlgExtendedError();
  if(err==0 && bRet)
  {
    strcpy(szDirName,sCOMDLG32.Verz);
    return 1;  
  }
  return 0; 
}
/************************************************************************

  Function: OpenSaveHookProc(HWND, UINT, UINT, LONG) ;
  
    Purpose:
    
      This function is the hook function for the GetOpenFileName() function.
      If GetOpenFileName() is called with the OFN_ENABLEHOOK flag, this
      function will be called before the normal GetOpenFileName() dialog
      function is called.
      
        Returns: FALSE to pass the message on to the normal GetOpenFileName()
        logic, TRUE to discard the message.
        
          Comments:
          
            To enable this function in this program, enter the value for
            OFN_ENABLEHOOK in the "Flags" edit box.
            
************************************************************************/
//HWND h_LBoxC;

UINT APIENTRY OpenSaveHookProc(HWND hwnd, UINT msg, UINT wParam, LONG lParam)
{
  LPOPENFILENAME pOfn ;
  //TCHAR szMsg[50] ;
  //int index1;
  //char verz[256];
  
  switch(msg)
  {
    
  case WM_INITDIALOG:
    
    pOfn = (LPOPENFILENAME) lParam ;
    
    // Save off the long pointer to the OPENFILENAME structure.
    SetWindowLong(hwnd, DWL_USER, lParam);
    RECT rect;
    GetWindowRect(GetParent(hwnd),&rect);
    MoveWindow(GetParent(hwnd),200,200,rect.right-rect.left,rect.bottom-rect.top,FALSE);
    //h_LBoxC=GetDlgItem(hwnd,lst2);
    /* During initialization, if there is a hook proc, the getopen()
    code will send pointer to the OPENFILENAME strucure in the
    lParam.  To demonstrate this, pop up a message box if this
    structure has a non zero value in the lCustData structure member */
    
    
    /*if (pOfn->lCustData != 0L)
    {
    wsprintf(szMsg, TEXT("OPENFILENAME->lCustData is: %ld"), pOfn->lCustData) ;
    
      MessageBox(hwnd, szMsg, TEXT("lCustData Sent!"), MB_OK) ;
  } */
    
    // SetWindowText(hwnd, TEXT("Open Hook Proc Dialog")) ;
    
    break ;
    
    
    /* use the WM_CTLCOLOR* messages to change the color of the Open
    dialog */
    
    /*    case WM_CTLCOLORDLG:
    
      if (!hBrushDlg)
      hBrushDlg = GetStockObject(LTGRAY_BRUSH) ;
      
        return (UINT) hBrushDlg ;
        
          break ;
          
            
              case WM_CTLCOLORBTN:
              
                SetBkMode((HDC) wParam, TRANSPARENT) ;   //sets background color
                //for push and check box
                //buttons...
                
                  if (!hBrushButton)
                  hBrushButton = GetStockObject(LTGRAY_BRUSH) ;
                  
                    return (UINT) hBrushButton ;
                    
                      break ;
                      
                        
                          case WM_CTLCOLORSTATIC:
                          
                            SetTextColor((HDC) wParam, RGB(0x00, 0xff, 0x00)) ;  //green
                            SetBkMode((HDC) wParam, TRANSPARENT) ;               //transparent text
                            
                              if (!hBrushDlg)
                              hBrushDlg = GetStockObject(LTGRAY_BRUSH) ;
                              
                                return (UINT) hBrushDlg ;
                                
                                  break ;
    */
    
  case WM_DESTROY:
    {
      LPOPENFILENAME lpOFN = (LPOPENFILENAME)GetWindowLong(hwnd, DWL_USER);
      LPCOMDLG32 psCOMDLG32 = (LPCOMDLG32)lpOFN->lCustData;
      
      GetDlgItemText(hwnd, IDE_PATH, psCOMDLG32->Verz, sizeof(psCOMDLG32->Verz));
      
    }
    break;
    /* case WM_COMMAND:
    switch (LOWORD(wParam))
    {
    case IDOK:
    GetDlgItemText(hwnd, stc1, verz, 256);
    SetDlgItemText(hwnd, edt1, verz) ;
    MessageBox(hwnd, TEXT("Read-Only button clicked..."),
    TEXT("Open"), MB_OK | MB_APPLMODAL) ;
    SetWindowLong(hwnd, DWL_MSGRESULT, 1L);
    DestroyWindow(hwnd);
    break ;
    
      case IDCANCEL:
      MessageBox(hwnd, TEXT("File Preview Button Clicked"),
      TEXT("Open"), MB_OK | MB_APPLMODAL) ;
      break ;
      case lst2:
      //index1=ListBox_GetCurSel(h_LBoxC);
      //ListBox_GetText(h_LBoxC, index1, verz);
      GetDlgItemText(hwnd, stc1, verz, 256);
      SetDlgItemText(hwnd, edt1, verz) ;
      break;
      case stc1:
      //index1=ListBox_GetCurSel(h_LBoxC);
      //ListBox_GetText(h_LBoxC, index1, verz);
      GetDlgItemText(hwnd, stc1, verz, 256);
      SetDlgItemText(hwnd, edt1, verz) ;
      break;
      
        default: break ;
        }
    break ;*/
  case WM_NOTIFY:
    TestNotify(hwnd, (LPOFNOTIFY)lParam);
    
  default:
    
  /*if (msg == nOpenShareVMsg)
  {
  // MessageBox(hwnd, TEXT("The SHAREVSTRING message is here!"),
  //                TEXT("Open"),
  //              MB_ICONEXCLAMATION | MB_OK | MB_APPLMODAL) ;
  
    return OFN_SHAREWARN ;
      } */
      /*if(GetFocus()==GetDlgItem(GetParent(hwnd),lst2))
      CommDlg_OpenSave_SetControlText(GetParent(hwnd),IDOK,"Öffnen");
    else*/
    CommDlg_OpenSave_SetControlText(GetParent(hwnd),IDOK,"OK");
    
    CommDlg_OpenSave_SetControlText(GetParent(hwnd),edt1,"a");
    /* if (msg == nF_OK)
    {
    
      //MessageBox(hwnd, TEXT("File Preview Button Clicked"),
      //             TEXT("Open"), MB_OK | MB_APPLMODAL) ;
      }*/ 
    
    break ;
  }
  
  return FALSE ;   //send msg to the common dialog code
}


//
//   FUNCTION: TestNotify( HWND hDlg, LPOFNOTIFY pofn)
//
//  PURPOSE:  Processes the WM_NOTIFY message notifications that is sent
//    to the hook dialog procedure for the File Open common dialog.
//
//
BOOL NEAR PASCAL TestNotify(HWND hDlg, LPOFNOTIFY pofn)
{
  switch (pofn->hdr.code)
  {
    
    // The selection has changed.            
		case CDN_SELCHANGE:
      {
        char szFile[MAX_PATH];             
        
        
        CommDlg_OpenSave_HideControl(GetParent(hDlg),edt1);
        CommDlg_OpenSave_HideControl(GetParent(hDlg),stc3);
        CommDlg_OpenSave_HideControl(GetParent(hDlg),cmb1);
        CommDlg_OpenSave_HideControl(GetParent(hDlg),stc2);
        CommDlg_OpenSave_HideControl(GetParent(hDlg),chx1);
        
        
        //CommDlg_OpenSave_SetControlText(GetParent(hDlg),IDOK,"Öffnen");
        /* h_LBoxC=GetDlgItem(GetParent(hDlg),lst2);
        int index1=ListBox_GetCurSel(h_LBoxC);
        ListBox_GetText(h_LBoxC, index1, verz);
        */
        // Get the file specification from the common dialog.
        /*	if (CommDlg_OpenSave_GetSpec(GetParent(hDlg),
        szFile, sizeof(szFile)) <= sizeof(szFile))
        {
        // Set the dialog item to reflect this.
        SetDlgItemText(hDlg, IDE_SELECTED, szFile);
      }*/
        if (CommDlg_OpenSave_GetFolderPath(GetParent(hDlg),
          szFile, sizeof(szFile)) <= sizeof(szFile))
        {
          // Set the dialog item to reflect this.
          SetDlgItemText(hDlg, IDE_PATH, szFile);
          //SetDlgItemText(hDlg, edt1, szFile);
          //SetWindowLong(hDlg, DWL_MSGRESULT, 1L);
        }
        
        
        // Get the path of the selected file.
        /*	if (CommDlg_OpenSave_GetFilePath(GetParent(hDlg),
        szFile, sizeof(szFile)) <= sizeof(szFile))
        {
        // Display this path in the appropriate box.
        SetDlgItemText(hDlg, IDE_PATH, szFile);
      }*/
        
      }
      break;
      
      // A new folder has been opened.
    case CDN_FOLDERCHANGE:
      {
        char szFile[MAX_PATH];
        
        if (CommDlg_OpenSave_GetFolderPath(GetParent(hDlg),
          szFile, sizeof(szFile)) <= sizeof(szFile))
        {
          // Display this new path in the appropriate box.
          SetDlgItemText(hDlg, IDE_PATH, szFile);                
        }
        //CommDlg_OpenSave_SetControlText(GetParent(hDlg),IDOK,"OK");
      }
      break;
      
      // The "Help" pushbutton has been pressed.
    case CDN_HELP:
      {   
        
      }
      break;       
      // The 'OK' pushbutton has been pressed.
    case CDN_FILEOK:
      // Update the appropriate box.
      //SetDlgItemText(hDlg,IDE_PATH, pofn->lpOFN->lpstrFile);
      //SetWindowLong(hDlg, DWL_MSGRESULT, 1L);
      break;
      
      // Received a sharing violation.
    case CDN_SHAREVIOLATION:
      // Update the appropriate box.
      SetDlgItemText(hDlg, IDE_PATH, pofn->pszFile);
      MessageBox(hDlg, "Got a sharing violation notify.", "WSPWIN", MB_OK);
      break;
      /* default:
      if(pofn->hdr.code == nF_OK)
      {
      SetDlgItemText(hDlg,IDE_PATH, pofn->lpOFN->lpstrFile);
      SetWindowLong(hDlg, DWL_MSGRESULT, 1L);
      }
      
      break;*/
  }
  
  return(TRUE);
}



// string constants

#define IDS_DIALOGFAILURE     1
#define IDS_STRUCTSIZE        2
#define IDS_INITIALIZATION    3
#define IDS_NOTEMPLATE        4
#define IDS_NOHINSTANCE       5
#define IDS_LOADSTRFAILURE    6
#define IDS_FINDRESFAILURE    7
#define IDS_LOADRESFAILURE    8
#define IDS_LOCKRESFAILURE    9
#define IDS_MEMALLOCFAILURE  10
#define IDS_MEMLOCKFAILURE   11
#define IDS_NOHOOK           12
#define IDS_SETUPFAILURE     13
#define IDS_PARSEFAILURE     14
#define IDS_RETDEFFAILURE    15
#define IDS_LOADDRVFAILURE   16
#define IDS_GETDEVMODEFAIL   17
#define IDS_INITFAILURE      18
#define IDS_NODEVICES        19
#define IDS_NODEFAULTPRN     20
#define IDS_DNDMMISMATCH     21
#define IDS_CREATEICFAILURE  22
#define IDS_PRINTERNOTFOUND  23
#define IDS_NOFONTS          24
#define IDS_SUBCLASSFAILURE  25
#define IDS_INVALIDFILENAME  26
#define IDS_BUFFERTOOSMALL   27
#define IDS_FILTERSTRING     28
#define IDS_UNKNOWNERROR     29

// constants


//
//  FUNCTION: ProcessCDError(DWORD) 
//
//  PURPOSE: Processes errors from the common dialog functions.
//
//  COMMENTS:
//
//        This function is called whenever a common dialog function
//        fails.  The CommonDialogExtendedError() value is passed to
//        the function which maps the error value to a string table.
//        The string is loaded and displayed for the user. 
//
//
void ProcessCDError(DWORD dwErrorCode, HWND hWnd)
{
  WORD  wStringID;
  TCHAR  buf[MAX_PATH];
  
  switch(dwErrorCode)
  {
	 case CDERR_DIALOGFAILURE:   wStringID=IDS_DIALOGFAILURE;   break;
   case CDERR_STRUCTSIZE:      wStringID=IDS_STRUCTSIZE;      break;
   case CDERR_INITIALIZATION:  wStringID=IDS_INITIALIZATION;  break;
   case CDERR_NOTEMPLATE:      wStringID=IDS_NOTEMPLATE;      break;
   case CDERR_NOHINSTANCE:     wStringID=IDS_NOHINSTANCE;     break;
   case CDERR_LOADSTRFAILURE:  wStringID=IDS_LOADSTRFAILURE;  break;
   case CDERR_FINDRESFAILURE:  wStringID=IDS_FINDRESFAILURE;  break;
   case CDERR_LOADRESFAILURE:  wStringID=IDS_LOADRESFAILURE;  break;
   case CDERR_LOCKRESFAILURE:  wStringID=IDS_LOCKRESFAILURE;  break;
   case CDERR_MEMALLOCFAILURE: wStringID=IDS_MEMALLOCFAILURE; break;
   case CDERR_MEMLOCKFAILURE:  wStringID=IDS_MEMLOCKFAILURE;  break;
   case CDERR_NOHOOK:          wStringID=IDS_NOHOOK;          break;
   case PDERR_SETUPFAILURE:    wStringID=IDS_SETUPFAILURE;    break;
   case PDERR_PARSEFAILURE:    wStringID=IDS_PARSEFAILURE;    break;
   case PDERR_RETDEFFAILURE:   wStringID=IDS_RETDEFFAILURE;   break;
   case PDERR_LOADDRVFAILURE:  wStringID=IDS_LOADDRVFAILURE;  break;
   case PDERR_GETDEVMODEFAIL:  wStringID=IDS_GETDEVMODEFAIL;  break;
   case PDERR_INITFAILURE:     wStringID=IDS_INITFAILURE;     break;
   case PDERR_NODEVICES:       wStringID=IDS_NODEVICES;       break;
   case PDERR_NODEFAULTPRN:    wStringID=IDS_NODEFAULTPRN;    break;
   case PDERR_DNDMMISMATCH:    wStringID=IDS_DNDMMISMATCH;    break;
   case PDERR_CREATEICFAILURE: wStringID=IDS_CREATEICFAILURE; break;
   case PDERR_PRINTERNOTFOUND: wStringID=IDS_PRINTERNOTFOUND; break;
   case CFERR_NOFONTS:         wStringID=IDS_NOFONTS;         break;
   case FNERR_SUBCLASSFAILURE: wStringID=IDS_SUBCLASSFAILURE; break;
   case FNERR_INVALIDFILENAME: wStringID=IDS_INVALIDFILENAME; break;
   case FNERR_BUFFERTOOSMALL:  wStringID=IDS_BUFFERTOOSMALL;  break;
     
   case 0:   //User may have hit CANCEL or we got a *very* random error
	    return;
      
   default:
	    wStringID=IDS_UNKNOWNERROR;
  }
  
  LoadString(NULL, wStringID, buf, sizeof(buf));
  MessageBox(hWnd, buf, NULL, MB_OK);
  return;
}
void ProjektZustandProfilOpen(char *strProjekt,char *strZustand,char *strProfil)
{
  char str[200],str_vergl[200];
  extern WINDOW dlg_136,WIN_116,WIN120,dlg_162,WIN121,PLOT100,PLOT101,WIN130,
    WIN161,dlg135,main_win,win122,DLG203,dlg147,dlg148_id;
  extern WSP_PROFIL_LISTE *pWPL;
  extern int scroll_position,dlg135ref;
  extern char str_zustand[16],
    str_gewaesser[16];
  extern BOOLEAN Edit_Fehler,dlg_cancel,abbruch203;//um Menü zu sperren
  extern BOOLEAN timermain,timermain2,berechnen,editieren;
  extern DWORD exit_ausw;
  extern HANDLE hProc_dag_bat;
  extern HWND hwndWinDDE;
  extern int dialog_ende;
  HWND focus_win,hMainWin;
  hMainWin=(HWND)xvt_vobj_get_attr(main_win, ATTR_NATIVE_WINDOW);
  
  if(timermain || timermain2 || exit_ausw!=0||hProc_dag_bat!=NULL)
    return;
  strcpy(str,strProjekt);                     
  strcat(str,"\\PROF");
  strcpy(str_vergl,strProjekt);
  strcat(str_vergl,"\\PROF");
  Edit_Fehler=TRUE;
  dlg_cancel=TRUE;
  abbruch203=TRUE;
  dialog_ende=0;
  
  focus_win=GetWindow(hMainWin,GW_CHILD);
  while(focus_win)
  {
    
    if(IsChild(hMainWin,focus_win) && focus_win!=hwndWinDDE)
      DestroyWindow(focus_win);
    focus_win=GetNextWindow(focus_win,GW_HWNDNEXT);
    
  }
  
  BringWindowToTop(hMainWin);
  if(dlg148_id!=NULL)
  {
    if(dlg147!=NULL)
    {           
      xvt_vobj_destroy(dlg147);
    }
    xvt_vobj_destroy(dlg148_id);
  }
  if(WIN120!=NULL)
    xvt_vobj_destroy(WIN120);
  if(dlg135!=NULL)
    xvt_vobj_destroy(dlg135);
  if(WIN121!=NULL)
  {
    if(PLOT100!=NULL)
      xvt_vobj_destroy(PLOT100);
    if(PLOT101!=NULL)
      xvt_vobj_destroy(PLOT101);
    if(WIN130!=NULL)
    {
      if(WIN161!=NULL)
      {           
        xvt_vobj_destroy(WIN161);
      }
      xvt_vobj_destroy(WIN130);
    }
    xvt_vobj_destroy(WIN121);
  }
  if(dlg_162!=NULL)
    xvt_vobj_destroy(dlg_162);
  if(win122!=NULL)
    xvt_vobj_destroy(win122);
  if(DLG203!=NULL)
    xvt_vobj_destroy(DLG203);
  Edit_Fehler=FALSE;
  if(strcmp(STR_SPEC.name,strZustand)!=0)
  {
    if(dlg_136!=NULL)
    {
      xvt_fsys_set_dir(&STR_SPEC.dir);             
      dlg136_save_daten(scroll_position);             
      save_str_datei();
      xvt_vobj_destroy(dlg_136);
    }
  }
  if(strcmp(STR_SPEC.dir.path,str_vergl)!=0)
  {
    if(!xvt_fsys_convert_str_to_dir(str,&STR_SPEC.dir))//Dick 7.10.98
    {
      //xvt_dm_post_error("Projekt existiert nicht mehr!");
      char buf[200],buf2[200];
      xvt_res_get_str(STR_FILEDLG_NOTE_3,buf,sizeof(buf));
      xvt_res_get_str(STR_FILEDLG_NOTE_6,buf2,sizeof(buf2));
      xvt_dm_post_error("%s %s",buf,buf2);
      return;
    }
    if (read_cfg_dat()==-1)
      return;
    
    strcpy(Projektname_aktuell,strProjekt);
    
  }
  map_object_command=TRUE;
  if(strcmp(STR_SPEC.name,strZustand)!=0)
  {
    
    strcpy(STR_SPEC.name,strZustand);
    if(dlg_136==NULL)
      if (!xvt_dlg_create_res(WD_MODELESS, DLG_136, EM_ALL, DLG_136_eh, 0L))
        xvt_dm_post_error("Can't open dialog");
      strcpy(Gewaessername_aktuell,str_gewaesser);
      strcpy(Zustand_aktuell,str_zustand);
      dlg135ref=TRUE;
  }
  else if(dlg_136==NULL)
    if (!xvt_dlg_create_res(WD_MODELESS, DLG_136, EM_ALL, DLG_136_eh, 0L))
      xvt_dm_post_error("Can't open dialog");
    if(strcmp(file_spec.name,strProfil)!=0 || WIN_116==NULL)
    {
      if(WIN_116!=NULL)
        xvt_vobj_destroy(WIN_116);
      strcpy(file_spec.name,strProfil); // obsolet?
      xvt_fsys_set_dir(&STR_SPEC.dir);
      int lesen_ok = read_profildatei(pWPL, &STR_SPEC.dir, strProfil );
      choice_neu =FALSE;
      
      if (lesen_ok ==0)
      {
        is_profil_open = TRUE;
        new_profil =FALSE;
        if(dlg_136)
          xvt_vobj_set_visible(dlg_136,FALSE);
        berechnen=editieren=FALSE;
        if (!xvt_win_create_res(WIN_GRAPH_116, TASK_WIN, EM_ALL, WIN_GRAPH_116_eh, 0L))
          xvt_dm_post_error("Can't open window116");
      }
      else 
      {
        //xvt_dm_post_error("Profildatei kann nicht gelesen werden!");
        char buf[200];
        xvt_res_get_str(STR_PROF_READ_NOTE,buf,sizeof(buf));
        xvt_dm_post_error("%s",buf);
      }
    }

    setTitle( Projektname_aktuell, Gewaessername_aktuell, Zustand_aktuell );

    BringWindowToTop( hMainWin ); 
}
