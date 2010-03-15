#include <windows.h>
#include "xvt.h"
#include "wspwin.h"
#include "resource.h"


#include "global_types.h"

#include "global_vars.h"

#include "list.h"
#include "readprof.h"
#include "read_cfg.h"
#include "strang.h"
#include "global.h"

#include "profpro2.h"

#include "lese_str.h"

extern char name208[20],
				station208[20],
				vzk[20],
				zustand[20],
				profilnummer[15],
				pk[20],
				dateiname[15],
				strspecnamesave[15];
extern BOOLEAN profile_loeschen,
					neukopieren;
extern SLIST prof_slist,
				 schreibe_list;
extern FILE *profprojfile;

SLIST profile_slist;  //extern in D135
SLIST profile_slist2; //extern in D210
//FILE *prof_file;
extern BOOLEAN zustand_kopieren;  //8.2.99 bley

/***************************************************************/

void profil_nr_ermitteln(void)
 {
  char profprojdatei[100];
  int back;
  int zaehler1;
//  char *profil_nr_string;
//  profil_nr_string = new char[8];

  xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,profprojdatei,50);
  strcat(profprojdatei,"\\profproj.txt");
  back=access(profprojdatei,00);
  if(back==0) //PROFPROJDATEI VORHANDEN
	{
	  if((profprojfile=fopen(profprojdatei,"r"))==NULL)
          {
		    //xvt_dm_post_note("PROFPROJDATEI fehlt");
            char buf[200];
           xvt_res_get_str(STR_PROFPROJ_FEHLT,buf,sizeof(buf));
           xvt_dm_post_note("%s",buf);
          }
	  else
		{
		 fscanf(profprojfile,"%d",&zaehler1);
		 zaehler1=zaehler1+1;
		 itoa(zaehler1,profil_nr_string,10);
		 fclose(profprojfile);
		}
	} //PROFPROJDATEI VORHANDEN
  if(back!=0) //PROFPROJDATEI FEHLT
	{
	 profil_nr_string[0]='1';
	 profil_nr_string[1]='\0';
	}
//  delete[]profil_nr_string;

 }
 /************************************************************************/
 void ermittle_profile_slist(void)
  {
	//DIE FUNKTION LIEST ALLE PROFILE IN PROFPROJ.TXT IN EINE SLIST EIN
	//DIESE WIRD DANN IN WSPDLG210 (AUSWAHL VON PROFILE BEIM NEUANLEGEN
	//EINER ZUSTANDSDATEI) ANGEZEIGT
	//AUFRUF AUS WSPD210.cpp

	//char gewaessernamevgl[15], help[200];
	char profprojdatei[100];
	char *gewaessernamevgl, *help;
	int i,j,k, pruefe=1, zaehler1,zaehler2;
	/***********************/
	gewaessernamevgl =new char [15];
	help = new char [200];

	if(profile_slist2!=NULL)
	 {
	  xvt_slist_destroy(profile_slist2);
	  profile_slist2=NULL;
	 }

	profile_slist2=xvt_slist_create();

	xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,profprojdatei,50);
	strcat(profprojdatei,"\\profproj.txt");
	if((profprojfile=fopen(profprojdatei,"r"))==NULL)
	 {
	 }
//		xvt_dm_post_note("PROFPROJDATEI fehlt");
	else
		{
		 fscanf(profprojfile,"%d",&zaehler1);
		 fscanf(profprojfile,"%d",&zaehler2);
		 fgets(help,101,profprojfile); //\n UEBERLESEN
		 for(i=1;i<=zaehler1;i++)
		  {
			fgets(help,101,profprojfile);
			for(j=0;j<=(INT)strlen(help);j++)
			 {
			  if(help[j]=='\n')
				help[j]='\0';
			 }
			k=0;
			for(j=0;j<=8;j++)
			 {
			  if(help[j]!=' ')
				{
				 gewaessernamevgl[k]=help[j];
				 k++;
				}
			 }
			 gewaessernamevgl[k]='\0';
			 pruefe=xvt_str_compare_ignoring_case(gewaessernamevgl,netz_dat[0]);

			 if(pruefe==0)
			  xvt_slist_add_at_elt(profile_slist2,NULL,help,0L);

		  }// FOR ANZAHL PROFILE (ZAEHLER 1)
		 fclose(profprojfile);
		}// ELSE PROFPROJFILE DA
	 delete [] gewaessernamevgl;
	 delete [] help;
  }

  /***********************************************************************/
  void ermittle_loesch_profile(void)
	{
	 //AUFRUF AUS WSPDLG135: ZUSTANDSDATEI LOESCHEN
	 //ERMITTELT WELCHE PROFILE IN ZUSTANDSDATEI

/*	 char zustandsdatei[100],
			strhelp[120],*/
	 char *zustandsdatei,
			*strhelp,
			profil[15];
	 FILE *zustandsfile;
	 int i,j,k;

	 zustandsdatei = new char[100];
	 strhelp       = new char[120];

	 /********************/

/*	 if(profile_slist!=NULL)
	  {
		xvt_slist_destroy(profile_slist);
		profile_slist=NULL;
	  }
*/
	 profile_slist=xvt_slist_create();
	 xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,zustandsdatei,50);
	 strcat(zustandsdatei,"\\");
	 strcat(zustandsdatei,STR_SPEC.name);
	 if((zustandsfile=fopen(zustandsdatei,"r"))==NULL)
         {
	      //xvt_dm_post_note("Zustandsdatei %s existiert nicht",STR_SPEC.name);
          char buf[200],buf2[200];//Dick 26.11.99
          xvt_res_get_str(STR_ZUSTANDSDATEI,buf,sizeof(buf));
          xvt_res_get_str(STR_NOEXIST,buf2,sizeof(buf2));
          xvt_dm_post_note("%s%s%s",buf,STR_SPEC.name,buf2);
         }
	 else
	  {
		fscanf(zustandsfile,"%d",&anzahl_profil_dat_entries);
		fscanf(zustandsfile,"%d",&anzahl_strang_entries);
		fgets(strhelp,110,zustandsfile);
		if(anzahl_profil_dat_entries>0)
		 {
		  for(i=1;i<=anzahl_profil_dat_entries;i++)
			{
			 k=0;
			 fgets(strhelp,110,zustandsfile);
			 for(j=44;j<=55;j++)
			  {
				if((strhelp[j]!='\0') && (strhelp[j]!='\n') &&(strhelp[j]!=' '))
				 {
				  profil[k]=strhelp[j];
				  k++;
				 }
				if((strhelp[j]=='\0') || (strhelp[j]=='\n'))
				 j=56;
			  }
			 profil[k]='\0';
			 xvt_slist_add_at_elt(profile_slist,NULL,profil,0L);
			}//for anzahl_profil_dat_entries
		 }//if anzahl_profil_dat_entries>0

		fclose(zustandsfile);
		if(!zustand_kopieren)  //8.2.99 bley

		test_auf_andere_zustandsdatei();  //IN PROFPRO2.cpp
	  } //else Zustandsdatei existiert noch

	 delete[] zustandsdatei;
	 delete[] strhelp     ;
	}  //FUNKTION

	/***********************************************************************/
	void test_auf_andere_zustandsdatei(void)
	 {
      SLIST_ELT ee2;
		//AUFRUF AUS FKT ERMITTLE_LOESCH_PROFILE (s.o)
	  //TESTET OB PROFILE EINER ZUSTANDSDATEI DIE GELÖSCHT WERDEN SOLL
	  //NOCH IN ANDEREN ZUSTANDSDATEIEN REFERENZIERT SIND

	  // char profilausslist[15], profilauspp[15], strauspp[15];
	  //char help[200];
	    char profprojdatei[100];
		 char *profilausslist,
			 *profilauspp,
			 *strauspp,
			 *hilfs_ptr,
			 *help;
	  int i,j,k,l,teststr,testp,zaehler1,zaehler2,ihelp;
	  BOOLEAN blank,entfernen;
	  SLIST_ELT ee;
      int anzahl_slist;

	  help = new char[200];
	  profilausslist = new char [15];
	  profilauspp = new char[15];
	  strauspp = new char [15];
	  /**************************/

	  anzahl_slist=xvt_slist_count(profile_slist);
	  if(anzahl_slist>0)
	  {
	   ee=xvt_slist_get_first(profile_slist);
	   for(ihelp=0;ihelp<anzahl_slist;i++)
	    {
		//  for(ee=xvt_slist_get_first(profile_slist);
		// ee!=NULL;ee=xvt_slist_get_next(profile_slist,ee))
		//  {
           if(profile_slist!=NULL && ee!=NULL)
		   {
		   hilfs_ptr=xvt_slist_get(profile_slist,ee,0L);
			profilausslist[0]='\0';
			strcpy(profilausslist,hilfs_ptr);

	  xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,profprojdatei,50);
	  strcat(profprojdatei,"\\profproj.txt");
	  if((profprojfile=fopen(profprojdatei,"r"))==NULL)
          {
		   //xvt_dm_post_note("PROFPROJDATEI fehlt");
           char buf[200];
           xvt_res_get_str(STR_PROFPROJ_FEHLT,buf,sizeof(buf));
           xvt_dm_post_note("%s",buf);
          }
	  else
		{
		 entfernen=FALSE;
		 fscanf(profprojfile,"%d",&zaehler1);
		 fscanf(profprojfile,"%d",&zaehler2);
		 fgets(help,101,profprojfile); //\n UEBERLESEN
		 for(i=1;i<=zaehler1;i++)
		  {
			fgets(help,101,profprojfile);
		  }
		 fgets(help,101,profprojfile);
		 for(i=1;i<=zaehler2;i++)
		  {
			blank=FALSE;
			fgets(help,101,profprojfile);
			/*************/
			k=0;
			l=0;
			for(j=0;j<(INT)strlen(help);j++)
			 {
			 if(help[j]==' ')
			  blank=TRUE;
			 if(!blank)
			  {
				profilauspp[l]=help[j];
				l++;
			  }
			 if(blank)
			  {
				if((help[j]!=' ') && (help[j]!='\0') && (help[j]!='\n'))
				 {
				  strauspp[k]=help[j];
				  k++;
				 }
			  }
			 } //for strlen(help)
			strauspp[k]='\0';
			profilauspp[l]='\0';
			testp=xvt_str_compare_ignoring_case(profilauspp,profilausslist);
			teststr=xvt_str_compare_ignoring_case(strauspp,STR_SPEC.name);
			if((testp==0) && (teststr!=0))
			 entfernen=TRUE;
			/************/
		  } //for zaehler2

		 fclose(profprojfile);
		 } //else profprojdatei da
		 if(entfernen)
		  {
             ee2=xvt_slist_get_next(profile_slist,ee);
			 xvt_slist_rem(profile_slist,ee);
			 ee=ee2;
		  }
	     else
		  ee=xvt_slist_get_next(profile_slist,ee);	
		   } //if 1=NULL
        ihelp++;
	   } //FOR FIRST-LAST PROFILE-SLIST
	  }
	  delete[] help;
	  delete[] profilausslist;
	  delete[] profilauspp;
	  delete[] strauspp;
	 } //FUNKTION

	 /*****************************************************************/
	 void sortiere_profproj_neu(void)
	 {
	  //AUFRUF AUS WSPDLG135:ZUSTANDSDATEI LOESCHEN
	  //DIE FUNKTION AKTUALISIERT DIE PROFPROJDATEI UND LOESCHT EVENTUELL
	  //PROFILE BEIM LOESCHEN EINER ZUSTANDSDATEI

	  // char profilausp[15],profilauss[15], strausp[15];
	  //char dateiloeschen[100], druckstring[200], help[200];

	    char profprojdatei[100];
		char *profilausp,
			 *profilauss,
			 *strausp,
			 *dateiloeschen,
			 *druckstring,
			 *help;
	  int i,j,k,l, z,testcomp, helpzaehler1,helpzaehler2, zaehler1, zaehler2;
	  BOOLEAN in_slist, blank;
	  char *hilfs_ptr;
	  SLIST_ELT ee;

	  profilausp	= new char[15];
	  profilauss	= new char [15];
	  strausp		= new char [15];
	  dateiloeschen = new char[100];
	  druckstring   = new char[200];
	  help          = new char[200];

	  /**********************/

	  if(schreibe_list!=NULL)
		{
		 xvt_slist_destroy(schreibe_list);
		 schreibe_list=NULL;
		}
	  schreibe_list=xvt_slist_create();

	  xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,profprojdatei,50);
	  strcat(profprojdatei,"\\profproj.txt");
	  if((profprojfile=fopen(profprojdatei,"r"))==NULL)
          {
		   //xvt_dm_post_note("PROFPROJDATEI fehlt");
           char buf[200];
           xvt_res_get_str(STR_PROFPROJ_FEHLT,buf,sizeof(buf));
           xvt_dm_post_note("%s",buf);
           
           return;//Dick 20.04.99
          }
	  else
		{
		 fscanf(profprojfile,"%d",&zaehler1);
		 fscanf(profprojfile,"%d",&zaehler2);
		 helpzaehler1=zaehler1;
		 helpzaehler2=zaehler2;
		 fgets(help,101,profprojfile); //\n UEBERLESEN

		 for(i=1;i<=zaehler1;i++)
		  {
			in_slist=FALSE;
			fgets(help,101,profprojfile);
			 for(z=0;z<=(INT)strlen(help);z++)
			  {
				if(help[z]=='\n')
				 help[z]='\0';
			  }
			k=0;
			for(j=44;j<=55;j++)
			 {
			  if((help[j]=='\0') || (help[j]=='\n'))
				j=56;
			  if((help[j]!=' ') && (help[j]!='\0') && (help[j]!='\n'))
				{
				 profilausp[k]=help[j];
				 k++;
				} //if !=' '
			 } //for  name
			profilausp[k]='\0';
			 if(profile_loeschen)
			  {
				for(ee=xvt_slist_get_first(profile_slist);
				 ee!=NULL;ee=xvt_slist_get_next(profile_slist,ee))
				  {
					hilfs_ptr=xvt_slist_get(profile_slist,ee,0L);
					profilauss[0]='\0';
					strcpy(profilauss,hilfs_ptr);
					testcomp=xvt_str_compare_ignoring_case(profilauss,profilausp);
					if(testcomp==0)
					 {
					  in_slist=TRUE;
					  helpzaehler1=helpzaehler1-1;
					  xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,dateiloeschen,50);
					  strcat(dateiloeschen,"\\");
					  strcat(dateiloeschen,profilausp);
					  remove(dateiloeschen);
					 }
				  } //for ee profile_slist
			  } //if profile_loschen

			if (!in_slist)
			 xvt_slist_add_at_elt(schreibe_list,NULL,help,0L);
		  }  //for zaehler1

		 fgets(help,101,profprojfile); //Leerz. UEBERLESEN
		 xvt_slist_add_at_elt(schreibe_list,NULL,"\0",0L);
		 for(i=1;i<=zaehler2;i++)
		  {
			fgets(help,101,profprojfile);
			 for(z=0;z<=(INT)strlen(help);z++)
			  {
				if(help[z]=='\n')
				 help[z]='\0';
			  }

				l=0;
				k=0;
				blank=FALSE;
				for(j=0;j<(INT)strlen(help);j++)
				 {
					if(help[j]==' ')
						blank=TRUE;
					if(!blank)
					 {
					  profilausp[l]=help[j];
					  l++;
					 }
					if(blank)
					 {
					  if((help[j]!=' ') && (help[j]!='\0') && (help[j]!='\n'))
						{
						 strausp[k]=help[j];
						 k++;
						}
					 }
				  } //for strlen(help)
			strausp[k]='\0';
			profilausp[l]='\0';
			testcomp=xvt_str_compare_ignoring_case(strausp,strspecnamesave);
			if(testcomp!=0)
			 xvt_slist_add_at_elt(schreibe_list,NULL,help,0L);
			if (testcomp==0)
			 helpzaehler2=helpzaehler2-1;
		  }  //for zaehler 2
		 fclose(profprojfile); //lesen zu
		} //else profprojda

//	  if((helpzaehler1>0) && (helpzaehler2>0))
//	  if(helpzaehler1>0)
		{
		 profprojfile=fopen(profprojdatei,"w");
		 fprintf(profprojfile,"%d ",helpzaehler1);
		 fprintf(profprojfile,"%d\n",helpzaehler2);
		 for(ee=xvt_slist_get_first(schreibe_list);
		  ee!=NULL;ee=xvt_slist_get_next(schreibe_list,ee))
			{
			hilfs_ptr=xvt_slist_get(schreibe_list,ee,0L);
			druckstring[0]='\0';
			strcpy(druckstring,hilfs_ptr);
			fprintf(profprojfile,"%s\n",druckstring);
		  }
		 fclose(profprojfile); //schreiben
		}
	  if((helpzaehler1==0) && (helpzaehler2==0))
		{
		 remove(profprojdatei);
		}
	  if(schreibe_list!=NULL)
		{
		 xvt_slist_destroy(schreibe_list);
		 schreibe_list=NULL;
		}

	  delete[] profilausp;
	  delete[] profilauss;
	  delete[] strausp;
	  delete[] dateiloeschen;
	  delete[] druckstring  ;
	  delete[] help         ;

	 }

	 /******************************************************************/
	 void profile_nach_profproj_schreiben(void)
	  {
		//DIE FUNKTION AKTUALISIERT DIE PROFPROJDATEI IM HINBLICK AUF
		//NEU KONVERTIERTE PROFILE
		//AUFRUF AUS WSPWIN.cpp TIMER BEI KONVERTIEREN aus LESE-STR

		//char profilstrpro2[15],  dazustring[50];
		/*char druckstring[200],   help[200];*/
		  char profprojdatei[100];
			char *druckstring,
			  *help,
			  *profilstrpro2,
			  *dazustring;
		int i,j,k,teste, zaehler1,zaehler2;
		BOOLEAN profprojda=TRUE;
		char *hilfs_ptr;
		SLIST_ELT ee, evorher;

		druckstring = new char[200];
		help        = new char[200];
		profilstrpro2 = new char [15];
		dazustring = new char [50];
		/****************************************************/
		if(schreibe_list!=NULL)
		{
		 xvt_slist_destroy(schreibe_list);
		 schreibe_list=NULL;
		}
		schreibe_list=xvt_slist_create();

		xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,profprojdatei,50);
		strcat(profprojdatei,"\\profproj.txt");
		if((profprojfile=fopen(profprojdatei,"r"))==NULL)
		  {
			zaehler1=0;
			zaehler2=0;
			profprojda=FALSE;
		  }
		if(profprojda)
		{
		 fscanf(profprojfile,"%d",&zaehler1);
		 fscanf(profprojfile,"%d",&zaehler2);
		 fgets(help,101,profprojfile); //\n UEBERLESEN

		 /*****oberen Teil einlesen und neue Profile vergleichen****/

		 for(i=1;i<=zaehler1;i++)
		  {
			fgets(help,101,profprojfile);
			for(j=0;j<=(INT)strlen(help);j++)
			 {
			  if(help[j]=='\n')
				help[j]='\0';
			 }
			/**********/
			k=0;
				for(j=44;j<=55;j++)
				 {
				  if((help[j]!=' ') &&(help[j]!='\0') &&
						(help[j]!='\n'))
					{
					 profilstrpro2[k]=help[j];
					 k++;
					}
				  if((help[j]=='\0') || (help[j]=='\n'))
					j=56;
				 } //for j44-j55 druckstring
				profilstrpro2[k]='\0';
			/**********/
            evorher = NULL;
			for(ee=xvt_slist_get_first(prof_slist);
				 ee!=NULL;ee=xvt_slist_get_next(prof_slist,ee))
			  {
				teste=1;
				hilfs_ptr=xvt_slist_get(prof_slist,ee,0L);
				druckstring[0]='\0';
				strcpy(druckstring,hilfs_ptr);
				k=0;
				for(j=44;j<=55;j++)
				 {
				  if((druckstring[j]!=' ') &&(druckstring[j]!='\0') &&
						(druckstring[j]!='\n'))
					{
					 dateiname[k]=druckstring[j];
					 k++;
					}
				  if((druckstring[j]=='\0') || (druckstring[j]=='\n'))
					j=56;
				 } //for j44-j55 druckstring
				dateiname[k]='\0';
				teste=xvt_str_compare_ignoring_case(dateiname,profilstrpro2);
				if(teste==0)
                    {
				    xvt_slist_rem(prof_slist,ee);
                    ee = evorher;
                    }
                evorher = ee;
			  } //for slist prof_slit

			xvt_slist_add_at_elt(schreibe_list,NULL,help,0L);
		  } //for zaehler 1
		 } //if(profprojda)
		 teste=xvt_slist_count(prof_slist);
		 if(teste>0)
		  {
			for(ee=xvt_slist_get_first(prof_slist);
				 ee!=NULL;ee=xvt_slist_get_next(prof_slist,ee))
			  {
				hilfs_ptr=xvt_slist_get(prof_slist,ee,0L);
				druckstring[0]='\0';
				strcpy(druckstring,hilfs_ptr);
				xvt_slist_add_at_elt(schreibe_list,NULL,druckstring,0L);
				zaehler1=zaehler1+1;
			  }
		  }

		 /*********/
		  if(profprojda)
			  fgets(help,101,profprojfile); // Leerzeile
		  xvt_slist_add_at_elt(schreibe_list,NULL,"\0",0L);
		 /*********/
		 profile_aus_str_ermitteln(); //in lese_str.cpp

	  if(profprojda)
		{
		 for(i=1;i<=zaehler2;i++)
		  {
			fgets(help,101,profprojfile);
			for(j=0;j<=(INT)strlen(help);j++)
			 {
			  if(help[j]=='\n')
				help[j]='\0';
			 }
			 xvt_slist_add_at_elt(schreibe_list,NULL,help,0L);
		  } //for zaehler2
		 } //if(profprojda)

		 /****neue Profile in unteren Teil v. profproj****/

		 teste=xvt_slist_count(prof_slist);
		 if(teste>0)
		  {
			for(ee=xvt_slist_get_first(prof_slist);
				 ee!=NULL;ee=xvt_slist_get_next(prof_slist,ee))
			  {
				hilfs_ptr=xvt_slist_get(prof_slist,ee,0L);
				druckstring[0]='\0';
				strcpy(druckstring,hilfs_ptr);
				k=0;
				for(j=44;j<=55;j++)
				 {
				  if((druckstring[j]!=' ') &&(druckstring[j]!='\0') &&
						(druckstring[j]!='\n'))
					{
					 dateiname[k]=druckstring[j];
					 k++;
					}
				  if((druckstring[j]=='\0') || (druckstring[j]=='\n'))
					j=56;
				 } //for j44-j55 druckstring
				dateiname[k]='\0';
				for(i=0;i<(INT)strlen(dateiname);i++)
				 {
				  dazustring[i]=dateiname[i];
				 }
				 dazustring[i]=' ';
				 dazustring[i+1]='\0';
				 strcat(dazustring,STR_SPEC.name);
				 xvt_slist_add_at_elt(schreibe_list,NULL,dazustring,0L);
				 zaehler2=zaehler2+1;
			  } //for prof_slit
		  } //if teste>0
		 /******/
		if(profprojda)
		 fclose(profprojfile);

		/**********schreiben*****************/
		if(zaehler1>0)
		 {
		  xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,profprojdatei,50);
		  strcat(profprojdatei,"\\profproj.txt");
		  if((profprojfile=fopen(profprojdatei,"w"))==NULL)
              {
			    //xvt_dm_post_note("Profprojdatei kann nicht geschrieben werden");
               char buf[200],buf2[200];
               xvt_res_get_str(STR_PROFPROJDATEI,buf,sizeof(buf));
               xvt_res_get_str(STR_CANNOT_WRITE2,buf2,sizeof(buf2));
               xvt_dm_post_note("%s%s",buf);
              }
		  else
			{
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
			 fclose(profprojfile); //schreiben
			}
		 } //if zaehler1>0

		 if(schreibe_list!=NULL)
		  {
			xvt_slist_destroy(schreibe_list);
			schreibe_list=NULL;
		  }

		 if(prof_slist!=NULL)
		  {
			xvt_slist_destroy(prof_slist);
			prof_slist=NULL;
		  }


		delete[] druckstring;
		delete[] help       ;
		delete [] profilstrpro2;
		delete [] dazustring;

	  }

/*********************************************************************/
void schluessel_nach_profil( char zielname[100] )
{
  //DIE FUNKTION AKTUALISIERT EINE PROFILDATEI DIE KOPIERT WIRD
  //MIT NEUEN SCHLUESSELDATEN
  //AUFRUF AUS WSPD136 PROFIL AUFNEHMEN FALL KOPIEREN
//  int i,j, len;
//  char *hilfs_ptr;
//  SLIST_ELT ee;

  if( schreibe_list != NULL )
  {
    xvt_slist_destroy( schreibe_list );
    schreibe_list = NULL;
  }
  schreibe_list = xvt_slist_create();


  FILE* prof_file = fopen( zielname, "r" );
  if( prof_file == NULL )
  {
    char buf[200],buf2[200];//Dick 26.11.99
    xvt_res_get_str(STR_DATEI,buf,sizeof(buf));
    xvt_res_get_str(STR_CANOTOPEN,buf2,sizeof(buf2));
    xvt_dm_post_note("%s%s%s",buf,zielname,buf2); // "Datei %s kann nicht geöffnet werden"
  }
  else
  {
    CharBuffer helpi( 250 ), druckstring( 300 );

    for( int i = 1; i < 10; i++ )
    {
      if( fgets( helpi, 98, prof_file ) == NULL )
      {
        char buf[200];
        xvt_res_get_str(STR_NOT_READ,buf,sizeof(buf));
        xvt_dm_post_note("%s",buf); //xvt_dm_post_note("Nicht gelesen");
      }

      int len = strlen( helpi );
      if( helpi[len - 1] == '\n' )
        len = len - 1;

      if( i == 3 )
      {
        if(len<40)
        {
          for( int j = len; j < 40; j++ )
            helpi[j]=' ';
          helpi[40]='\0';
          strcat(helpi,zustand);
        }
        else
        {
          for( int j = 40;j < len; j++ )
            helpi[j]=' ';
          helpi[40]='\0';
          strcat(helpi,zustand);
        }
      }
			
      if (i==5)
      {
        if(len<40)
        {
          for( int j = len; j < 40; j++ )
            helpi[j]=' ';
          helpi[40]='\0';
          strcat(helpi,vzk);
        }
        else
        {
          for( int j = 40; j < len; j++ )
            helpi[j]=' ';
          helpi[40]='\0';
          strcat(helpi,vzk);
        }
      }

      if(i==6)
      {
        if(len<40)
        {
          for( int j=len;j<40;j++)
            helpi[j]=' ';
          helpi[40]='\0';
          strcat(helpi,netz_dat[0]);
        }
        else
        {
          for( int j=40;j<len;j++)
            helpi[j]=' ';
          helpi[40]='\0';
          strcat(helpi,netz_dat[0]);
        }
      }

      if(i==7)
      {
        if(len<40)
        {
          for( int j=len;j<40;j++)
            helpi[j]=' ';
          helpi[40]='\0';
          strcat(helpi,pk);
        }
        else
        {
          for( int j=40;j<len;j++)
            helpi[j]=' ';
          helpi[40]='\0';
          strcat(helpi,pk);
        }
      }

      if(i==8)
      {
        helpi[0]='\0';
        strcat(helpi,"QUERPROFIL ");
        strcat(helpi,profilnummer);
      }

      if(i==9)
      {
        helpi[0]='\0';
        strcat(helpi,"STATION KM ");
        strcat(helpi,station208);
      }
      
      for( int j = 0; j <= (INT)strlen(helpi); j++ )
      {
        if(helpi[j]=='\n')
          helpi[j]='\0';
      }
      xvt_slist_add_at_elt(schreibe_list,NULL,helpi,0L);
    } // for i

    int a = 0;

    while( !feof( prof_file ) )
    {
      helpi[0] = '\0';
      fgets( helpi, 250, prof_file );//Dick 11.02.99  150 ->250

      for( int j = 0; j <= (INT)strlen( helpi ); j++ )
      {
        if(helpi[j] == '\n')
          helpi[j]='\0';
        if( (unsigned char)helpi[j] > 127 ) // sonst stürzt xvt_slist_add_elt ab 
          helpi[j] = ' ';
      };
      
      xvt_slist_add_at_elt( schreibe_list, NULL, helpi, 0L );
    }
    fclose(prof_file);

    prof_file = fopen( zielname,"w");
    if( prof_file == NULL )
    {
      char buf[200],buf2[200];//Dick 26.11.99
      xvt_res_get_str(STR_DATEI,buf,sizeof(buf));
      xvt_res_get_str(STR_CANOTOPEN,buf2,sizeof(buf2));
      xvt_dm_post_note("%s%s%s",buf,zielname,buf2);    // "Datei %s kann nicht geöffnet werden"
    }
    else
    {
      for( SLIST_ELT ee = xvt_slist_get_first( schreibe_list ); ee != NULL; ee = xvt_slist_get_next( schreibe_list, ee ) )
      {
        char* hilfs_ptr = xvt_slist_get( schreibe_list, ee, 0L );
        druckstring[0]='\0';
        strcpy( druckstring, hilfs_ptr );
        fprintf( prof_file, "%s\n", druckstring );
      }
      fclose(prof_file); //schreiben
    }
  }
}

/*********************************************************/
/*********************************************************/
/*********************************************************/

void schluessel_einlesen(char namederdatei[100])
{
	 FILE *prof_file;
	 int i,j,k;
	 BOOLEAN ende, dlganzeigen;

	 char *help;
	 help = new char[200];


   if((prof_file=fopen(namederdatei,"r"))==NULL)
   {
     //xvt_dm_post_note("Datei %s kann nicht geöffnet werden",namederdatei);
     char buf[200],buf2[200];//Dick 26.11.99
     xvt_res_get_str(STR_DATEI,buf,sizeof(buf));
     xvt_res_get_str(STR_CANOTOPEN,buf2,sizeof(buf2));
     xvt_dm_post_note("%s%s%s",buf,namederdatei,buf2);
   }
   else
   {
     dlganzeigen=FALSE;
     for(i=1;i<10;i++)
     {
       fgets(help,140,prof_file);
       
       /************ZUSTAND************************/
       if (i==3)
       {
         ende=FALSE;
         k=0;
         /***NEU***/
         BOOLEAN nichtsmehr=FALSE;
         for(j=0;j<=38;j++)
         {
           if((help[j]=='\0')||(help[j]=='\n'))
             nichtsmehr=TRUE;
         }
         if(!nichtsmehr)
         {
           zustand[0]='\0';
           for(j=40;j<=49;j++)
           {
             if((help[j]!=' ') &&(help[j]!='\n') &&(help[j]!='\0'))
             {
               zustand[k]=help[j];
               k++;
             }
             if((help[j]=='\0') || (help[j]=='\n'))
               j=50;
           }
           zustand[k]='\0';
           
           /***falls doch mehr wie 40 Zeichen und nur Blanks drin:***/
           if(zustand[0]=='\0')
           {
             strcpy(zustand,netz_dat[2]);
             dlganzeigen=TRUE;
           }
           /******************************/
         }
         if(nichtsmehr)
         {
           strcpy(zustand,netz_dat[2]);
           dlganzeigen=TRUE;
         }
       }
       /*****/
       /*************VZK**************************/
       if (i==5)
       {
         ende=FALSE;
         k=0;
         /***NEU***/
         BOOLEAN nichtsmehr=FALSE;
         for(j=0;j<=38;j++)
         {
           if((help[j]=='\0')||(help[j]=='\n'))
             nichtsmehr=TRUE;
         }
         if(!nichtsmehr)
         {
           vzk[0]='\0';
           for(j=40;j<=42;j++)
           {
             if((help[j]!=' ') &&(help[j]!='\n') &&(help[j]!='\0'))
             {
               vzk[k]=help[j];
               k++;
             }
             if((help[j]=='\0') || (help[j]=='\n'))
               j=43;
           }
           vzk[k]='\0';
           if(vzk[0]=='\0')
           {
             strcat(vzk,"0");
             dlganzeigen=TRUE;
           }
         }
         if(nichtsmehr)
         {
           vzk[0]='\0';
           strcat(vzk,"0");
           dlganzeigen=TRUE;
         }
       }
       /***********GEW-NAME**********************/
       if (i==6)
       {
         ende=FALSE;
         k=0;
         /****/
         BOOLEAN nichtsmehr=FALSE;
         for(j=0;j<=38;j++)
         {
           if((help[j]=='\0')||(help[j]=='\n'))
             nichtsmehr=TRUE;
         }
         if(!nichtsmehr)
         {
           name208[0]='\0';
           for(j=40;j<=48;j++)
           {
             if((help[j]!=' ') &&(help[j]!='\n') &&(help[j]!='\0'))
             {
               name208[k]=help[j];
               k++;
             }
             if((help[j]=='\0') || (help[j]=='\n'))
               j=49;
           }
           name208[k]='\0';
           if(name208[0]=='\0')
           {
             strcpy(name208,netz_dat[0]);
             dlganzeigen=TRUE;
           }
         }
         if(nichtsmehr)
         {
           strcpy(name208,netz_dat[0]);
           dlganzeigen=TRUE;
         }
         /****/
       }
       /************PK***************************/
       if (i==7)
       {
         ende=FALSE;
         k=0;
         /****neu***/
         BOOLEAN nichtsmehr=FALSE;
         for(j=0;j<=38;j++)
         {
           if((help[j]=='\0')||(help[j]=='\n'))
             nichtsmehr=TRUE;
         }
         if(!nichtsmehr)
         {
           pk[0]='\0';
           for(j=40;j<=48;j++)
           {
             if((help[j]!=' ') &&(help[j]!='\n') &&(help[j]!='\0'))
             {
               pk[k]=help[j];
               k++;
             }
             if((help[j]=='\0') || (help[j]=='\n'))
               j=49;
           }
           pk[k]='\0';
           if(pk[0]=='\0')
           {
             strcat(pk,"0");
             dlganzeigen=TRUE;
           }
         }
         if(nichtsmehr)
         {
           pk[0]='\0';
           strcat(pk,"0");
           dlganzeigen=TRUE;
         }
       }
       /********PROFILNUMMER********************************/
       if(i==8)
       {
         k=0;
         for(j=10;j<(INT)strlen(help);j++)
         {
           if((help[j]!=' ') && (help[j]!='\0') && (help[j]!='\n'))
           {
             profilnummer[k]=help[j];
             k++;
           }
         }
         profilnummer[k]='\0';
       }
       /**********STATION***********************/
       if (i==9)
       {
         ende=FALSE;
         BOOLEAN wert_war_da=FALSE;
         k=0;
         for(j=11;j<25;j++)
         {
           if((help[j]=='\0') || (help[j]=='\n'))
             ende=TRUE;
           if((help[j]==' ') && (wert_war_da))
             ende=TRUE;
           if((help[j]!='\0') && (help[j]!='\n') &&(!ende) &&(help[j]!=' '))
           {
             station208[k]=help[j];
             wert_war_da=TRUE;
             k++;
           }
         }
         station208[k]='\0';
       }
    } //FOR ERSTE 9 ZEILEN
    fclose(prof_file);
    if((dlganzeigen) &&(!neukopieren))
      if(!xvt_dlg_create_res(WD_MODAL,DLG_208, EM_ALL, DLG_208_eh, 0L))
        xvt_dm_post_error("Can't open dialog 208");
      
  } //else
  delete[] help;
}

  /*****************************************************************/
  void lese_projektbezeichnung(char *text) //aus wspdlg222
	{
	 char dateiname[100];
	 FILE *in;

	 xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,dateiname,50);
	 strcat(dateiname,"\\probez.txt");
	 if((in=fopen(dateiname,"r"))==NULL)
	  {
		text[0]='\0';
	  }
	 else
	  {
		 fgets(text,60,in); //
		 fclose(in);
	  }
	}
/***********************************************************/
 void  schreibe_projektbezeichnung(char *text) //aus WSPDLG222
  {
	 char dateiname[100];
	 FILE *out;

	 xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,dateiname,50);
	 strcat(dateiname,"\\probez.txt");
	 if((out=fopen(dateiname,"w+"))==NULL)
	  {
		//xvt_dm_post_note("Projektbezeichung kann nicht geschrieben werden.");
        char buf[200],buf2[200];
        xvt_res_get_str(STR_PROJEKTBEZ,buf,sizeof(buf));
        xvt_res_get_str(STR_CANNOT_WRITE2,buf2,sizeof(buf2));
        xvt_dm_post_note("%s%s",buf,buf2);
	  }
	 else
	  {
		 fprintf(out,"%s",text);
		 fclose(out);
	  }
  }
