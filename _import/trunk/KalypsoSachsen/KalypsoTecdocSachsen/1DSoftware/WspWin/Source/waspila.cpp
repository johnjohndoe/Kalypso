#include <windows.h>
#include "xvt.h"
#include "wspwin.h"
#include "resource.h"

#include "global_types.h"
#include "..\..\wspdlg\include\export.h"

#include "list.h"

#include "read_cfg.h"
#include "readprof.h"
#include "profpro2.h"

#include "global.h"
#include "cderr.h"

#include "typen.h"
#include "process.h"	// GHJ
#include "waspila.h"
#include "profproj.h"
#include "strang.h"
#include "configuration.h"
#include "bce_allg.h"

extern SLIST header_profil;

extern MMP mmp;
extern char name208[20];
extern char station208[20];
extern char vzk[20];
extern char zustand[20];
extern char pk[20];
extern char profilnummer[15];
extern char profil_nr_string[8];

extern char dateiname[15];
extern BOOLEAN vergleich;
extern WSP_PROFIL_LISTE *pWPL;
extern char profilstr [100];
extern char uebergabe_name[15];

SLIST geometrie, rauheit, charakter;
char directory [200];

int lese_start_datei (char* start_datei)
{
  FILE* start_file=NULL;
  int ok=1, ok_rauh=1, ok_charakter=1, i=0, j=0, anzahl_profile=0;

  char* datei;
  datei=new char [200];

  char* str_hier;
  str_hier=new char [200];


  geometrie=xvt_slist_create();
  rauheit=xvt_slist_create();
  charakter=xvt_slist_create();

  if ((start_file=fopen(start_datei,"r+"))!=NULL)
  {
    while(!feof(start_file))
	{
	 ok=1;
	 ok_charakter=1;
	 ok_rauh=1;
     str_hier[0]='\0';
	 fgets (str_hier,190,start_file);
	 for(int j=0; j<=(int)strlen(str_hier);j++)
	 {
		 if(str_hier[j]=='\n')
			 str_hier[j]='\0';
		 if(str_hier[j]==' ')       //nachgestellte Blanks entfernen
			 str_hier[j]='\0';
	 }
	 if(i==0)
		 strcpy(directory,str_hier);
	 //ok=xvt_str_compare_ignoring_case(str,str_to_compare_geo);
     
     //if(ok==0)
      if(xvt_str_match(str_hier,"GEOMETRIE",TRUE))
	 {
       anzahl_profile=0;
	   fgets(str_hier, 190, start_file);
/*	   for(int j=0; j<=strlen(str);j++)
		   if(str[j]==' ')          //führende Blanks entfernen
			   str[j]=str[j+1];
*/
	   sscanf(str_hier, "%d", &anzahl_profile);

       for(j=1; j<=anzahl_profile; j++)
	   {
	    fgets(str_hier, 190, start_file);
		sscanf(str_hier,"%s",datei);
        xvt_slist_add_at_elt(geometrie, NULL,datei,0L);
	   }
	 }
//	 ok_charakter=xvt_str_compare_ignoring_case(str,str_to_compare_char);
//     if(ok_charakter==0)
      if(xvt_str_match(str_hier,"CHARAKTERI",TRUE))

	  {
       anzahl_profile=0;
	   fgets(str_hier, 190, start_file);
	   sscanf(str_hier, "%d", &anzahl_profile);
       for(j=1; j<=anzahl_profile; j++)
	   {
	    fgets(str_hier, 190, start_file);
		sscanf(str_hier,"%s",datei);
        xvt_slist_add_at_elt(charakter, NULL,datei,0L);
	   }
	 }
//	 ok_rauh=xvt_str_compare_ignoring_case(str,str_to_compare_rauh);
//     if(ok_rauh==0)
      if(xvt_str_match(str_hier,"RAUHIGKEIT",TRUE))
	  {
       anzahl_profile=0;
	   fgets(str_hier, 190, start_file);
	   sscanf(str_hier, "%d", &anzahl_profile);
       for(j=1; j<=anzahl_profile; j++)
	   {
	    fgets(str_hier, 190, start_file);
		sscanf(str_hier,"%s",datei);
        xvt_slist_add_at_elt(rauheit, NULL,datei,0L);
	   }
	 }
     i++;
	} //while !feof
  } //fopen
  fclose(start_file);

  delete [] str_hier;
  delete [] datei;
 return anzahl_profile;
}
/***************************************************************/
void lese_geometrie(int anzahl_profs)
{
  int i=0, ok;
  char *element;
  char element_string [200];
  char charakter_string [200];
  char geometrie_datei[200];
  

  for (i=1; i<=anzahl_profs; i++)
  {
	element = xvt_slist_get_elt(geometrie,i-1,0);
    strcpy(element_string,element);
    strcpy(geometrie_datei,directory);
	strcat(geometrie_datei,"GEOMETRI\\");
	strcat(geometrie_datei,element_string);
    ok=lese_profil(geometrie_datei, anzahl_profs);

	element = xvt_slist_get_elt(charakter,i-1,0);
    strcpy(element_string,element);
    strcpy(geometrie_datei,directory);
	strcat(geometrie_datei,"KARAKTER\\");
	strcat(geometrie_datei,element_string);
    strcpy(charakter_string,geometrie_datei);
	
	element = xvt_slist_get_elt(rauheit,i-1,0);
    strcpy(element_string,element);
    strcpy(geometrie_datei,directory);
	strcat(geometrie_datei,"RAUHEIT\\");
	strcat(geometrie_datei,element_string);

	ok=lese_charakter_und_rauheits_profil(charakter_string,geometrie_datei);

	/**********18.12.00 in str speicher*/
	char tmp[15];
	test_line9(tmp);

	for (int h=0;h<100;h++)   //init.
		profilstr[h]=' ';
	profilstr[99]='\0';
	
	for (h=0;h<(INT)strlen(netz_dat[0]);h++)            //Gewässer
		{
			if ((netz_dat[0][h]=='\n')||(netz_dat[0][h]=='\0'))
				profilstr[h]=' ';
			else
				profilstr[h]=netz_dat[0][h];
		}
	
	int a=17;                                  //Station
	int len=strlen(tmp);
		len=len-1;
	for(h=len;h>=0;h--,a--)
		profilstr[a]=tmp[h];

    a=26;
	
	for(h=(INT)(strlen(pk)-1);h>=0;h--,a--)             //pk
		profilstr[a]=pk[h];   //Neu Dick 19->26 und minus 10.07.98

    a=31;
	for(h=(INT)(strlen(vzk)-1);h>=0;h--,a--)            //vzk
		profilstr[a]=vzk[h];  //Neu Dick 29->31 und minus 10.07.98

	for(h=0;h<(INT)strlen(zustand);h++)
		profilstr[33+h]=zustand[h];

	for (h=0;h<=11;h++)          // Filename
		{
			profilstr[44+h]=dateiname[h];
			uebergabe_name[h]=dateiname[h];
		}
	 uebergabe_name[12]='\0';

   if( GetFeature( "wsp_nodemo" ) )
   {
     char *stat_str,*dummy_str;
     float st_km_new,st_km_test;
     SLIST_ELT e;
     int kol=0,pos_gefunden=-1;
     dummy_str=new char[10];
     sscanf(profilstr,"%s%f",dummy_str,&st_km_new);
     
     for(e=xvt_slist_get_first(prof_datei);e!=NULL;e=xvt_slist_get_next(prof_datei,e))
     {
       stat_str=xvt_slist_get(prof_datei,e,0L);
       sscanf(stat_str,"%s%f",dummy_str,&st_km_test);
       if(st_km_test > st_km_new)
       {
         xvt_slist_add_at_pos(prof_datei,kol,profilstr,0L);
         pos_gefunden=kol;
         break;
       }
       else
         kol++;
     }
     if(pos_gefunden<0)
       xvt_slist_add_at_elt(prof_datei,NULL,profilstr,0L);
     delete []dummy_str;
     //xvt_slist_add_sorted(prof_datei,profilstr,0L,FALSE,TRUE);//Neu dick 10.07.98
   };

    anzahl_profil_dat_entries++;

    vzk[0]='0';
	pk[0]='0';
	sort_new_profil( &strang_anfang, &strang_ende, station208, vzk, pk, uebergabe_name, GetSortStrangVorwaerts() );

	wandle_abstand_in_string();
    StrangUpdateIndex();

	/*****************18.12.00 ende in str. speicher*/

  strcpy(pWPL->data->file.name, uebergabe_name );
  pWPL->data->file.dir = STR_SPEC.dir;

	save_profildatei(pWPL);      /*  Profildatei sichern  */

  }

 save_str_datei();
 xvt_slist_destroy(rauheit);
 xvt_slist_destroy(geometrie);
 xvt_slist_destroy(charakter);

}

/*************************************************************/
int lese_profil(char* profil_name, int anzahl_profs)
{
	FILE *start_profil;
    int i=0, anzahl_punkte=0, durchs1=0, durchs2=0, dummy;
	double station;
	char temp_len[200];
	Scroller xy;

    char* str=NULL;
    str=new char [200];
	char *p;

	if ((start_profil=fopen(profil_name,"r+"))!=NULL)
	{
	    if(!feof(start_profil))
			{
				for(i=0;i<=2;i++)
				{
					fgets (str,190,start_profil);
					if(i==0)
						strcpy(name208,str);
					if(i==2)
						{
							sscanf(str,"%lf %d %d %d %d", &station, &durchs1, &durchs2, &dummy, &anzahl_punkte);
							station=station /1000;
							sprintf(station208,"%lf",station);
						}
				}

/*************neu*****************/
		 xvt_slist_destroy(header_profil);
		 header_profil = xvt_slist_create();

		 for (i=0;i<14;i++)
		  xvt_slist_add_at_elt(header_profil,NULL,"\0",0L);
		 xvt_slist_add_at_elt(header_profil,NULL,"0 0 0 0 0 0 0 0 0",0L);

		 list->DeleteList();                         // neue Profildatei anlegen !
		 list->MakeNewNode(1);
		 list->MakeNewKoord(anzahl_punkte);
		 list->LeseWerteInPtr(start_profil,anzahl_punkte);
		// range = 0;
		// scr.scrollpos = 1;
		 anzahl_ds = 1;
		 ds_info[0] = 1;
		 ds_info[1] = anzahl_punkte;
		 scr.anzahl=1;
		 scr.datensatz=1;

		 for (i=0;i<=TYPE_SIZE-1;i++) typ[i]=0;//Dick 8.12.98   //Datenblocktypliste init.
		 typ[1] = 1;

		 list->WriteTypDaten(typ[1],GELAENDEHOEHE,NULL);

/***************Durchströmte bereiche einfuegen***************/

		ds_info[0]++;
		anzahl_ds=ds_info[0];
		typ[anzahl_ds]=DURCHST_BEREICH;//siehe typen.h
		ds_info[anzahl_ds]=2;
		list->MakeNewNode(ds_info[0]);
		list->WriteTypDaten(anzahl_ds,DURCHST_BEREICH,NULL);
        xy.z[0]=list->Get_Num_Station(1,&durchs1,0);
		xy.z[1]=list->Get_Station_Hoehe(xy.z[0]);
		xy.z[2]=list->Get_Num_Station(1,&durchs2,0);
		xy.z[3]=list->Get_Station_Hoehe(xy.z[2]);

		list->SaveSonderprofildaten(&xy, DURCHST_BEREICH);

/***************************************/
			 
		 p=&netz_dat[0][0];

		 strcpy(pk,"0\0");
		 strcpy(vzk,"0\0");
		 /*NEU*/
		 char zufuegen[80];
		 for (int i=0;i<40;i++)
		  zufuegen[i]=' ';
		 zufuegen[40]='\0';
		 strcat(zufuegen,netz_dat[0]);
		 xvt_slist_change_str(header_profil,zufuegen,5);
		 /*****/


		 /********Zeile 9 schreibe: Station*****/
		 write_line9(station208);
         
		 /*NEU**/
		 for (i=0;i<40;i++)
		  zufuegen[i]=' ';
		 zufuegen[40]='\0';
		 strcat(zufuegen,zustand);
		 xvt_slist_change_str(header_profil,zufuegen,2);   // Zustand
		 /******/

		 /***NEU***VZK*/
		 for (i=0;i<40;i++)
		  zufuegen[i]=' ';
		 zufuegen[40]='\0';
		 strcat(zufuegen,vzk);
		 xvt_slist_change_str(header_profil,zufuegen,4);   // VZK


		 /*****Neu***Querprofil*/
		 profil_nr_ermitteln();
		 zufuegen[0]='\0';
		 strcat(zufuegen,"Querprofil ");
		 strcat(zufuegen,profilnummer);
		 xvt_slist_change_str(header_profil,zufuegen,7);


		 /*NEU**PK***/
		 for (i=0;i<40;i++)
		  zufuegen[i]=' ';

		 zufuegen[40]='\0';
		 strcat(zufuegen,pk);


		 for (i=0;i<(INT)strlen(netz_dat[0]);i++)
		  zufuegen[i]=netz_dat[0][i];
		 int j=10;

		 /*18.12.00:*/
		 for (i=0;i<(INT)strlen(netz_dat[2]);i++)
		  zustand[i]=netz_dat[2][i];

		 for (i=0;i<(INT)strlen(zustand);i++,j++)
		  zufuegen[j]=zustand[i];
         
		 /***********/

		 xvt_slist_change_str(header_profil,zufuegen,6);   // Zustand

		 //neuen Profildateinamen erzeugen:
		 for (i=0;i<15;i++)
			 dateiname[i] ='0';
		 dateiname[0]=p[0];
		 dateiname[1]=p[1];
		 dateiname[8]= '.';
		 dateiname[9]= 'p';
		 dateiname[10]= 'r';
		 dateiname[11]= 'f';
		 dateiname[12]= '\0';

		 /*************DAGMAR EINGEFUEGT*****************/
		 profil_nr_ermitteln();
		 strcpy(temp_len,profil_nr_string);

		 /***NEU*****/
		 int back=0;
		 while(back==0)
			 {
			  j=7;
			  for (i=strlen(temp_len);i>=1;i--)
				 {
				  dateiname[j] = temp_len[i-1];
				  j--;
				 }

			  char testedatei[200];
			  xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,testedatei,50);
			  strcat(testedatei,"\\");
			  strcat(testedatei,dateiname);
			  back=access(testedatei,00);
			  if(back==0)
				 {
				  int tempint=atoi(temp_len);
				  tempint++;
				  itoa(tempint,temp_len,10);
				 }
			 } //while back==0

		 vergleich=FALSE;
         int druck;
		 druck=0;
		 teste_projekt_profile(druck);   //vergleich wird geändert

         new_profil=TRUE;
		 char *directory_name;
		 directory_name=new char[255];
		 xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, directory_name,250);
         xvt_fsys_convert_str_to_dir(directory_name,&file_spec.dir);


         delete [] directory_name;
		 druck=1;
		 teste_projekt_profile(druck);   //vergleich wird geändert

		/*************/

				/***************ende neu*******************/
			}
  fclose(start_profil);

	} //if fopen

delete[]str;

  return 1;
}
/*******************************************************/

/*****************************************************/
int lese_charakter_und_rauheits_profil(char *charakter_name, char *profil_name)
{
	FILE *start_profil_charakter, *start_profil_rauheit;
    int i=0, anzahl_punkte=0, dummy=0, j=0, jr=0, bewuchs_init=0, rauh_typ=0, verfahren=0;
	double k=0, ax=0, ay=0, dp=0, y1, y2;

	char typ [200];
	char rauh_werte[4] [15];

    char* str_lokal;
    str_lokal=new char [200];

	if ((start_profil_charakter=fopen(charakter_name,"r+"))!=NULL)
	{
	  if ((start_profil_rauheit=fopen(profil_name,"r+"))!=NULL)
	  {
			while(!feof(start_profil_charakter))
			{
			  if(j==0)
			  {
				for(i=0;i<=7;i++)
				{
					fgets (str_lokal,190,start_profil_charakter);
					j++;
				}
			  } //j==0

              if(jr==0)
			  {
			   for(i=0;i<3;i++)
				{
					fgets (str_lokal,190,start_profil_rauheit);
					jr++;
				}
			  } //jr==0

			   if(!feof(start_profil_charakter))
					{
						fgets(str_lokal,190,start_profil_charakter);
						sscanf(str_lokal,"%s %lf %lf", typ, &y1, &y2);
						j++;
					 }
				if((typ[0]!='T')&&(!feof(start_profil_rauheit)))
				{
					fgets (str_lokal,190,start_profil_rauheit);
					if(jr==3)
						{
							char verfahren_string [5];
							verfahren_string[0]=str_lokal[5];
							verfahren_string[1]='\0';
							verfahren=atoi(verfahren_string);
							if((verfahren==1) || (verfahren==2))
								rauh_typ=4; //Kst-rauheit
							else
								rauh_typ=3; //ks-rauheit

							fgets (str_lokal,190,start_profil_rauheit);
							fgets (str_lokal,190,start_profil_rauheit);
							jr=jr+2;
						
					        /*******Rauheitsdatensatz anlegen********/
							ds_info[0]++;
							anzahl_ds=ds_info[0];
							typ[anzahl_ds]=rauh_typ;//siehe typen.h
							ds_info[anzahl_ds]=0;
							list->MakeNewNode(ds_info[0]);
							list->MakeNewKoord(ds_info[1]);
							list->CopyStation(anzahl_ds);  // Stationswerte in neuen Datensatz kopieren
							ds_info[anzahl_ds]=ds_info[1];
							list->InitKoord(anzahl_ds,0.0);  // mit 0.0 initialisieren
							list->WriteTypDaten(anzahl_ds,rauh_typ,NULL);
							/*******Ende Rauheitsdatensatz anlegen***/
					/**/} //if jr==3
 //                    jr++;
					 int strich1_da=0;
					 int strich2_da=0;
					 int strich3_da=0;
					 int blank_da=0;
					 int a=0,b=0,c=0,d=0, pos=0, ahelp=0, e=0, eingelesen=0;
                     
					 for(e=0; e<4; e++)
						 rauh_werte[e][0]='\0';

					 for(a=0; a<=(int)strlen(str_lokal); a++)
						{

						    if((rauh_werte[0][0]!='\0') && (str_lokal[a]==' ') && (!eingelesen))
							{
								sscanf(str_lokal,"%s %s %s %s", rauh_werte[0], rauh_werte[1], rauh_werte[2], rauh_werte[3]); 
                                eingelesen=1;
							}
							else
								{
								  if(!eingelesen)
								  {
									if((str_lokal[a]!='-') && (!strich1_da))
									if(str_lokal[a]!=' ')
										rauh_werte[0][a-blank_da]=str_lokal[a];
									else
										blank_da++;
                             
									if((str_lokal[a]=='-') && (!strich1_da))
										{
											strich1_da=1;
											ahelp=a-blank_da;
											a++;
											b=0;
										}
									if((strich1_da) &&(strich2_da==0))
										{
											if(str_lokal[a]!='-') 
												{
													rauh_werte[1][b]=str_lokal[a];
													b++;
												}
											if(str_lokal[a]=='-') 
												{
													strich2_da=1;
													a++;
													c=0;
												}
										}
									if((strich2_da) &&(strich3_da==0))
										{
											if(str_lokal[a]!='-')
												{
													rauh_werte[2][c]=str_lokal[a];
													c++;
												}
											else
												{
													a++;
													strich3_da=1;
													d=0;
												}
										}
									if(strich3_da)
										if(str_lokal[a]!='-')
											{
												rauh_werte[3][d]=str_lokal[a];
												d++;
											}
									}//!=eingelsen		
								} //else
					 }  //for (a..a<strlen(str)

                     if(!eingelesen)
					 {
					  for(int in=0; in<=3; in++)
						{
							if(in==0)
								rauh_werte[in][ahelp]='\0';
							if(in==1)
								rauh_werte[in][b]='\0';
							if(in==2)
								rauh_werte[in][c]='\0';
							if(in==3)
								rauh_werte[in][d]='\0';
						}
					 }   
					 k=atof(rauh_werte[0]);
					 dp=atof(rauh_werte[1]);
					 ax=atof(rauh_werte[2]);
					 ay=atof(rauh_werte[3]);
						
	
 					 pos = list->ExistStation(y1,1);
					 mmp.position_mouse_down = pos;

 					 pos = list->ExistStation(y2,1);
					 mmp.position_mouse_up = pos;
                    

//					 list->Edit_Gelaende_Bereiche(2,k,rauh_typ,&mmp);
					 list->Edit_Gelaende_Bereiche(3,k,3,&mmp);
					
                    if(!((dp==1) && (ax==1) && (ay==1)))
					{	
						/********Datensätze DP, AX, AY 1x anlegen********/
						if(bewuchs_init==0)
							{
								/***DP*****/
								ds_info[0]++;
								anzahl_ds=ds_info[0];
								typ[anzahl_ds]=10;//siehe typen.h  //=dp
								ds_info[anzahl_ds]=0;
								list->MakeNewNode(ds_info[0]);
								list->MakeNewKoord(ds_info[1]);
								list->CopyStation(anzahl_ds);  // Stationswerte in neuen Datensatz kopieren
								ds_info[anzahl_ds]=ds_info[1];
								list->InitKoord(anzahl_ds,0.0);  // mit 0.0 initialisieren
								list->WriteTypDaten(anzahl_ds,DPM,NULL);

								/*****AX********/
								ds_info[0]++;
								anzahl_ds=ds_info[0];
								typ[anzahl_ds]=8;//siehe typen.h  //=dp
								ds_info[anzahl_ds]=0;
								list->MakeNewNode(ds_info[0]);
								list->MakeNewKoord(ds_info[1]);
								list->CopyStation(anzahl_ds);  // Stationswerte in neuen Datensatz kopieren
								ds_info[anzahl_ds]=ds_info[1];
								list->InitKoord(anzahl_ds,0.0);  // mit 0.0 initialisieren
								list->WriteTypDaten(anzahl_ds,AXM,NULL);

								/*****AY********/
								ds_info[0]++;
								anzahl_ds=ds_info[0];
								typ[anzahl_ds]=9;//siehe typen.h  //=dp
								ds_info[anzahl_ds]=0;
								list->MakeNewNode(ds_info[0]);
								list->MakeNewKoord(ds_info[1]);
								list->CopyStation(anzahl_ds);  // Stationswerte in neuen Datensatz kopieren
								ds_info[anzahl_ds]=ds_info[1];
								list->InitKoord(anzahl_ds,0.0);  // mit 0.0 initialisieren
								list->WriteTypDaten(anzahl_ds,AYM,NULL);
							bewuchs_init=1;
						} //bewuchs_init=0

						/********Datensätze DP, AX, AY 1x anlegen********/

						list->Edit_Gelaende_Bereiche(ds_info[0]-2,dp,3,&mmp); //DP
						list->Edit_Gelaende_Bereiche(ds_info[0]-1,ax,3,&mmp); //8=AX
	
						list->Edit_Gelaende_Bereiche(ds_info[0],ay,3,&mmp);  //9=AY

					}  //if dp!=1 AX!=1, AY!=1, d.h. Bewuchs vorhanden

					jr++;
				}  //wenn bei charakter_string kein T, d.h. Rauheitsbereich
//              jr++;

			}  //while !=feof start_profile_charakter
       fclose(start_profil_rauheit);
	  }
	fclose(start_profil_charakter);
  }
    	
	delete[]str_lokal;
	return 1;
}