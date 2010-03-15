#include <windows.h>
#include "xvt.h"
#include "wspwin.h"
#include "resource.h"

#include "global_types.h"

#include "list.h"
#include "read_cfg.h"


#include "global.h"
#include "util2.h"
#include "cderr.h"
#include "rauh.h"

#ifndef Max_Files
#define Max_Files 1400
#endif

SLIST db_slist;
extern plot ploti;
char dxf_ohne_verzeichnis[15];
extern BOOLEAN dateiexistiert, ueberschreiben;

void lese_datenbank_in_slist(char *db_pfadn)
 {
	FILE *in;
	char *text_zeile;
	text_zeile = new char [100];//Dick 12.01.99 90->100
	in=fopen(db_pfadn,"r");

    memset(text_zeile,0,100);//Dick 12.01.99 90->100

	while(!feof(in))
		{
	     fgets(text_zeile,100,in);
		 slashn_entfernenb(text_zeile); //in util2.cpp
		 xvt_slist_add_at_elt(db_slist,NULL,text_zeile,0L);
		}
	fclose(in);
    delete[] text_zeile;
 }
/*************************************************************/
void schreibe_datenbank(char *db_pfadptr)
 {
  FILE *out;
  char *str_ptr;
  char *str_zeile;
  str_zeile=new char [100];//Dick 12.01.99 80->100
	if((out=fopen(db_pfadptr,"w+"))==NULL)
	  xvt_dm_post_error("Datenbank kann nicht gesichert werden");
	else
	 {
	  int count=xvt_slist_count(db_slist);
	  int zaehler=0;
	  for(zaehler=0;zaehler<=count-1;zaehler++) 
		{
		 str_ptr=xvt_slist_get_elt(db_slist,zaehler,0L);
		 strcpy(str_zeile,str_ptr);
		 if(zaehler==count-1)
		  fprintf(out,"%s",str_zeile);
		 else
		  fprintf(out,"%s\n",str_zeile);
		}
	 fclose(out);
	 }
  delete[] str_zeile;
 }
/**********************************************************************/

void parameter_aus_selected_bew(char *selected2, char*bemerkung2,char *wert1, char *wert2, char *wert3)
 {
  //DIE FUNKTION ZERGLIEDERT DAS SELEKTIERTE ELEMENT "SELECTED" EINER LISTBOX
  //IN DIE EINZELNEN PARAMETER BEMERKUNG,WERT1,WERT2,WERT3
  //AUFRUF AUS WSPW129: Bewuchsdatenbank

  if((selected2[0]=='n') &&
	  (selected2[1]=='e') &&
	  (selected2[2]=='u') &&
	  (selected2[3]=='\0'))
	{
	 bemerkung2[0]='\0';
	 wert1[0]='\0';
	 wert2[0]='\0';
	 wert3[0]='\0';
	}
  else
	{  
    //neu Dick 14.01.99
       char *tmp;
       double a1,a2,a3; 
       tmp=&selected2[41];
       sscanf(tmp,"%lf%lf%lf",&a1,&a2,&a3);
       sprintf(wert1,"%lf",a1);
       sprintf(wert2,"%lf",a2);
       sprintf(wert3,"%lf",a3);
    //ende neu
	 for(int i=0;i<40;i++)
	  bemerkung2[i]=selected2[i];
	 bemerkung2[40]='\0';
	 BOOLEAN ende=FALSE;          //nachgestellte Blanks weg
	 for(i=39;i>=0;i--)
	  {
		if(bemerkung2[i]!=' ')
		 ende=TRUE;
		if((bemerkung2[i]==' ') && (!ende))
		 bemerkung2[i]='\0';
	  }

	 /*************** //Dick 14.01.99 alles durch neu ersetzt
	 for(i=41;i<51;i++)
	  wert1[i-41]=selected2[i];
	 wert1[10]='\0';
	 ende=FALSE;          //nachgestellte Blanks weg
	  for(i=9;i>=0;i--)
		{
		 if(wert1[i]!=' ')
		  ende=TRUE;
		 if((wert1[i]==' ') && (!ende))
		  wert1[i]='\0';
		}
	/************
	  for(i=52;i<62;i++)
		wert2[i-52]=selected2[i];
	  wert2[10]='\0';
	  ende=FALSE;          //nachgestellte Blanks weg
	  for(i=9;i>=0;i--)
		{
		 if(wert2[i]!=' ')
		  ende=TRUE;
		 if((wert2[i]==' ') && (!ende))
		  wert2[i]='\0';
		}
	/*************
		for(i=63;i<=(INT)strlen(selected2);i++)
		 wert3[i-63]=selected2[i];
		ende=FALSE;          //nachgestellte Blanks weg
		int len=strlen(wert3);
		for(i=len-1;i>=0;i--)
		 {
		  if(wert3[i]!=' ')
			ende=TRUE;
		  if((wert3[i]==' ') && (!ende))
			wert3[i]='\0';
		  }*/
		 } //else

  }

/*****************************************************************************/
//FUNTIONEN ZUM PLOTTEN:

void starte_plot_programm(int anzahl)
 {
  FILE *out;
  char name[100], verzeichnis [150], bat_file_text[150];
  //char *name, *verzeichnis, *bat_file_text;
  
  /*name=new char [100];
  verzeichnis=new char [150];
  bat_file_text=new char [150];
  */

  
  xvt_fsys_set_dir_startup();
  int z,pos;

  strcpy(name,start_dir);
  strcat(name,"plot.bat");

  out=fopen(name,"w+");
/****/
			 strcpy(verzeichnis, start_dir);
			 verzeichnis[2]='\0'; 						//SICHERHEITSHALBER C:
			 fprintf(out,"%s\n",verzeichnis);
			 verzeichnis[2]='\\';
			 verzeichnis[(strlen(verzeichnis)-1)]='\0'; //letzter Backslash weg
		/******************************************************/
		fprintf(out,"@echo off\n");
		  fprintf(out,"cd\\\n");
		  bat_file_text[0]='\0';

//		  for(z=2;z<(INT)strlen(verzeichnis);z++) // CD START-DIRECTORY
		  for(z=3;z<(INT)strlen(verzeichnis);z++)
		   {
				int y=0;
				bat_file_text[0]='\0';

			//	while (verzeichnis[z]!='\\')
			while ((verzeichnis[z]!='\\') && (verzeichnis[z]!='\0')
				   && (verzeichnis[z]!='\n'))
			  {
					bat_file_text[y]=verzeichnis[z];
					z++;
					y++;
				 }
			  bat_file_text[y]='\0';
			  if (bat_file_text[0]!='\0')
				  {
					fprintf(out,"cd ");
					fprintf(out,"%s\n",bat_file_text);
				  }
			  }


/****/
		  for( pos = 0; pos < anzahl; pos++ )
			  fprintf( out,"%spro.exe < %sinput%d.tmp\n", start_dir, start_dir, pos );
		  for( pos = 0; pos < anzahl; pos++ )
			  fprintf( out,"if exist %sinput%d.tmp del %sinput%d.tmp\n",
				  start_dir,pos, start_dir,pos );
		  fprintf(out,"@echo on\n");
		  fprintf(out,"@echo SCHLIESSEN SIE JETZT DIE DOS-BOX\n");
		  fprintf(out,"PAUSE\n");
		  fclose(out);
		  xvt_fsys_set_dir_startup();
		  WinExec(name,SW_SHOWMAXIMIZED);
 }


 int wahl(void)
 {
	  int i, j,help, help2;

	  strcpy(ploti.ursprungsname,ploti.name);
	  help=strlen(ploti.name);
	  help2=0;
	  for(i=help;i>=0;i--)
		{
		 if(help2==0)
		  {
			if(ploti.name[i]=='\\')
			 {
			  j=i;
			  help2=1;
			 }
		  }
		}
	  help2=j+1;
	  for (i=help2; i<=help;i++)
		 dxf_ohne_verzeichnis[i-help2]=ploti.name[i];

	  ploti.name[strlen(ploti.name)-3]='\0';
	  strcat(ploti.name,"dxf");
	  dxf_ohne_verzeichnis[strlen(dxf_ohne_verzeichnis)-3]='\0';
	  strcat(dxf_ohne_verzeichnis,"dxf");
  return 1;
 }

 /***********************************************************************/
 void schreibe_input_plot_datei(int pos)
  {
	char name[100];
	//char *name;
    char tmp[100];
	//name=new char [100];
	FILE *out;
    sprintf(tmp,"input%d.tmp",pos);
	strcpy(name,start_dir);
	//strcat(name,"input.tmp");
    strcat(name,tmp);

	out=fopen(name,"w+");
	fprintf(out,"j\n");
	fprintf(out,"%s\n",ploti.ursprungsname);
	fprintf(out,"j\n");
	fprintf(out,"%s\n",ploti.laenge);
	fprintf(out,"%s\n",ploti.hoehe);
	fprintf(out,"n\n");
	fprintf(out,"n\n");
	if(dateiexistiert)
	 {
	  if(ueberschreiben)
		fprintf(out,"j\n");
	  else
		{
		 fprintf(out,"n\n");
		 fprintf(out,"%s\n",ploti.name);
		}
	 }

	fclose(out);
	//delete[] name;
  }

  /***************************************************/
  int masstab_holen(void)
	{
	 FILE *in;

	 char *temp;
	 SLIST  neu;
	 char help[4][30];

	temp=new char[200];
	if((in=fopen(ploti.ursprungsname,"r+"))==NULL)
	 {
	  //xvt_dm_post_note("Plotinputdatei %s läßt sich nicht öffnen", ploti.ursprungsname);
      char buf[200],buf2[200];
     xvt_res_get_str(STR_PLOT_NOTE_4,buf,sizeof(buf));
     xvt_res_get_str(STR_PLOT_NOTE_5 ,buf2,sizeof(buf2));
     xvt_dm_post_error("%s\n%s\n%s",buf,ploti.ursprungsname,buf2);
	  return 0;
	 }
	else
	 {
	  neu=xvt_slist_create();
	  for (int i=0;i<=14;i++)
		{
		 fgets(temp,198,in);
		 temp[strlen(temp)-1]='\0';
		 xvt_slist_add_at_elt(neu,NULL,temp,i);
		}
	  sscanf(temp,"%s %s %s %s %s",&help[0], &help[1], &help[2], &ploti.laenge, &ploti.hoehe);

	  fclose(in);
	  if(neu!=NULL)
		{
		 xvt_slist_destroy(neu);
		 neu=NULL;
		}
	  return 1;
	}

  delete[] temp;
 }
