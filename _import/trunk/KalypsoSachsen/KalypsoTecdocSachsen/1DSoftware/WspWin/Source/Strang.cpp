//** strang.cpp **//

/************************************************************
*      Dialog136                                            *
*      Verwaltung der Strangtabelle                         *
*		 Verzeigerte Liste												*
*                                                           *
*  struct STRANG                                            *
*	  {                                                      *
*		int nummer;                                           *
*		float anfang,ende;                                    *
*		char[11] abstand_links, abstand_rechts, abstand_fluss;*
*		STRANG *next;                                         *
*	  }                                                      *
*                                                           *
*************************************************************/

#include <windows.h>
#include "xvt.h"

#include <math.h>

#include "resource.h"

#include "global_types.h"
#include "global_vars.h"


void MakeNewStrang(int anzahl)
{
 for (int i=1;i<=anzahl;i++)
  {
	strang_ptr = new STRANG;
	strang_ptr->next = NULL;
	strang_ptr->nummer = i;
	strang_ptr->anfang = BCE_NAN;
	strang_ptr->ende   = BCE_NAN;
	strang_ptr->strang_vzk=0;
	strang_ptr->strang_pk=0;//Dick 31.03.99
	strang_ptr->abstand_links[0] ='\0';
	strang_ptr->abstand_rechts[0]='\0';
	strang_ptr->abstand_fluss[0] ='\0';
	strang_ptr->name_anfang[0]='\0';
	strang_ptr->name_ende[0]='\0';
	strang_ptr->abstand=BCE_NAN;
	if (!strang_anfang)
		 strang_anfang = strang_ptr;
	else
		strang_ende->next = strang_ptr;
	strang_ende = strang_ptr;
	strang_ende->next = NULL;
  }
}
STRANG* MakeNewStrang(int anzahl,STRANG *strang)
{
 STRANG *hilfe_strang=strang=NULL;
 for (int i=1;i<=anzahl;i++)
  {
	strang_ptr = new STRANG;
	strang_ptr->next = NULL;
	strang_ptr->nummer = i;
	strang_ptr->anfang = BCE_NAN;
	strang_ptr->ende   = BCE_NAN;
	strang_ptr->strang_vzk=0;
	strang_ptr->strang_pk=0;//Dick 31.03.99
	strang_ptr->abstand_links[0] ='\0';
	strang_ptr->abstand_rechts[0]='\0';
	strang_ptr->abstand_fluss[0] ='\0';
	strang_ptr->name_anfang[0]='\0';
	strang_ptr->name_ende[0]='\0';
	strang_ptr->abstand=BCE_NAN;
    if (!strang)
        {
         strang = strang_ptr;
         //strang->next=hilfe_strang;
        }
    else        
         hilfe_strang->next=strang_ptr;
    hilfe_strang=strang_ptr;
    //hilfe_strang=strang_ptr;
    hilfe_strang->next=NULL;
  }
 return strang;
}

/***************************************************/
void dlg136_get_daten(int position)
// liets die aktuelle Strangtabelle aus und füllt die 'Grid'-control mit Daten
// Parameter:
//          int position: ab welcher Position in der Strangtabelle soll ausgegeben werden
// Nebeneffekte:
//        Liest:  STRANG* strang_anfang   // Zeigt auf den akt. Strang
//        Ändert: STRANG* strang_ptr      // nur hilfsvariable
//                char name_anfang[5}[15] // die Namen der sichtbaren Anfangsprofile
//                char name_ende[5][15]   // die Namen der sichtbaren Endprofile
//                WINDOW dlg136_edit:     // die Editcontrols für die Strangtabelle in dlg136
{
  char temp[15];
  
  strang_ptr = strang_anfang;
  
  for (int i=1;i<position;i++)
    if (strang_ptr !=NULL)
      strang_ptr = strang_ptr->next;
    
  for (i=0;i<=24;i=i+6)
  {
    if (strang_ptr !=NULL)
    {
      // NUMMER
      itoa(strang_ptr->nummer,temp,10);
      xvt_vobj_set_title(dlg136_edit[i],temp);
      // Anfangsprofil
      if (strang_ptr->anfang == BCE_NAN)
        temp[0]='\0';
      else
        sprintf(temp,"%.4lf",strang_ptr->anfang); 
      xvt_vobj_set_title(dlg136_edit[i+1],temp);
      // Endprofil
      if (strang_ptr->ende == BCE_NAN)
        temp[0]='\0';
      else
        sprintf(temp,"%.4lf",strang_ptr->ende); 
      xvt_vobj_set_title(dlg136_edit[i+2],temp);
      // Profilabstand
      xvt_vobj_set_title(dlg136_edit[i+3],strang_ptr->abstand_links);
      xvt_vobj_set_title(dlg136_edit[i+4],strang_ptr->abstand_fluss);
      xvt_vobj_set_title(dlg136_edit[i+5],strang_ptr->abstand_rechts);
      strang_ptr = strang_ptr->next;
    }
  } // for i
  strang_ptr = strang_anfang;
  for (i=1;i<position;i++)
    if (strang_ptr !=NULL)
      strang_ptr = strang_ptr->next;
  for (i=0;i<5;i++)
  {
    if (strang_ptr !=NULL)
    {
      strcpy(name_anfang[i],strang_ptr->name_anfang);
      strcpy(name_ende[i],strang_ptr->name_ende);
      strang_ptr = strang_ptr->next;
    }
  }
}

/***************************************************/
void dlg136_save_daten( int position )
{
  char temp[15];
  
  strang_ptr = strang_anfang;
  
  for (int i=1;i<position;i++)
    if (strang_ptr !=NULL)
      strang_ptr = strang_ptr->next;
    
  for (i=0;i<=24;i=i+6)
  {
    if (strang_ptr !=NULL)
    {
      // Anfangsprofil
      xvt_vobj_get_title(dlg136_edit[i+1],temp,10);
      if (strlen(temp)==0)
        strang_ptr->anfang=BCE_NAN;
      else
        strang_ptr->anfang=atof(temp);
      // Endprofil
      xvt_vobj_get_title(dlg136_edit[i+2],temp,10);
      if (strlen(temp)==0)
        strang_ptr->ende=BCE_NAN;
      else
        strang_ptr->ende=atof(temp);
      // Profilabstand
      xvt_vobj_get_title(dlg136_edit[i+3],strang_ptr->abstand_links,10);
      xvt_vobj_get_title(dlg136_edit[i+4],strang_ptr->abstand_fluss,10);
      xvt_vobj_get_title(dlg136_edit[i+5],strang_ptr->abstand_rechts,10);
      strang_ptr = strang_ptr->next;
    }
    else
    {
      //xvt_dm_post_error("Fehler beim Speichern der Strangtabelle");
      char buf[200];
      xvt_res_get_str(STR_SAVE_STRANG_NOTE,buf,sizeof(buf));
      
      xvt_dm_post_error("%s",buf);
    }
  }
  
  strang_ptr = strang_anfang;
  for (i=1;i<position;i++)
    if (strang_ptr !=NULL)
      strang_ptr = strang_ptr->next;
    for (i=0;i<5;i++)
    {
      if (strang_ptr !=NULL)
      {
        strcpy(strang_ptr->name_anfang,name_anfang[i]);
        strcpy(strang_ptr->name_ende,name_ende[i]);
        strang_ptr = strang_ptr->next;
      }
    }
}
/*************************************************************************/
void destroy_strang_tabelle(void)
{
 while (strang_anfang)
  {
	strang_ptr = strang_anfang;
	strang_anfang = strang_anfang->next;
	delete strang_ptr;
  }
 strang_anfang = NULL;
 strang_ptr=NULL;
}

/*************************************************************************/
void destroy_strang_tabelle(STRANG *strang)
{
 while (strang)
  {
	strang_ptr = strang;
	strang = strang->next;
	delete strang_ptr;
  }
 strang = NULL;
 strang_ptr=NULL;
}


/****************************************************************/
void make_sort_strangtabelle(void)
{
 double *station; //station[STRANGANZAHL];
 char name[STRANGANZAHL][15];
 char *str;
 char temp[15];
 char strtemp[150];
 int count=0,j=0;
 double x;

 station = new double[STRANGANZAHL];

 for (int i=0;i<STRANGANZAHL;i++)
	  {
		station[i]=0.0;
		name[i][0]='\0';
	  }
 count = xvt_slist_count(prof_datei);

if (count < STRANGANZAHL)
 {
  for (int i=0;i<count;i++)
	{
	 str = xvt_slist_get_elt(prof_datei, i, 0);
	 strcpy(strtemp,str);
	 for (int j=0;j<=7;j++)
			temp[j]=strtemp[10+j];
	 temp[8]='\0';
	 station[i] = atof(temp);

	 for (j=0;j<=11;j++)         //Dateiname
			name[i][j]=strtemp[44+j];
	 name[i][12]='\0';
	}

 for (j=0;j<count;j++)                  //sortieren
  {
	for (i=count-1;i>j;i--)
	{
	 if (station[i-1] > station[i])
		 {
		  x=station[i];
		  station[i]=station[i-1];
		  station[i-1]=x;
		  strcpy(temp,name[i]);
		  strcpy(name[i],name[i-1]);
		  strcpy(name[i-1],temp);

		 }
	}
  }
 strang_ptr = strang_anfang;

 for (i=0;i<count-1;i++)
  {
	strang_ptr->anfang = station[i];
	strang_ptr->ende   = station[i+1];
	//gcvt(	(station[i+1]-station[i]),10, temp);
	sprintf(temp,"%.4lf",(station[i+1]-station[i]));
	strcpy(strang_ptr->abstand_links, temp);
	strcpy(strang_ptr->abstand_rechts,temp);
	strcpy(strang_ptr->abstand_fluss, temp);
	strcpy(strang_ptr->name_anfang,name[i]   );
	strcpy(strang_ptr->name_ende,  name[i+1] );

	if (strang_ptr !=NULL)
		strang_ptr = strang_ptr->next;
	anzahl_strang_entries = count -1;
  }
 }
 delete[] station;
}
/******************************************************************************/
void sort_new_profil( STRANG** strang_anfang, STRANG** strang_ende, 
                      char stationStr[20], char vzkStr[20], char pkStr[20], char name[15], 
                      BOOL vorwaerts )
//
// sortiert ein Profil in die Strangtabelle ein
//
// Parameter:
//        char stationStr[20],
//        char vzkStr[20],
//        char pkStr[20]
//        char name[15]:     Daten des Profils
//        int anzahl_profil_dat_entries: Länge des Strangs
//        STRANG* strang_anfang: zeigt auf den Anfang der Strangtabelle
//                              in diese Tabelle wird ein neuer STRANG eingefügt und das
//                              letzte Element gelöscht ( damit die Länge der verketteten Liste konstant ( = 1000 ) bleibt
//        STRANG* strang_ende: zeigt auf das Ende der Strangtabelle, wird entsprechend aktualisiert
{
  // Profildaten konvertieren
  double station = atof( stationStr );
  int vzk = atoi( vzkStr );
  unsigned pk;

  switch(pkStr[0])
  {
  case '0':
    pk = 0;
    break;
  
  case 'L':
    pk = 100;
    if( pkStr[2] != '\0')
      pk += atoi( &pkStr[2] );
    break;
  
  case 'F':
    pk = 200;
    if( pkStr[2] != '\0' )
      pk += atoi( &pkStr[2] );
    break;

  case 'R':
    pk = 300;
    if( pkStr[2] != '\0' )
      pk += atoi( &pkStr[2] );
    break;

  default:
    pk = 0;
    break;
  };

  STRANG* strang_prev = NULL; // zeigt auf das Element nach welchem das neue einsortiert wird ( NULL falls das erste )
  STRANG* strang_next = NULL; // zeigt auf das Element vor welchem das neue einsortiert wird
  STRANG* strang_help; // Hilfszeiger

  strang_next = *strang_anfang;
  while ( strang_next->anfang != BCE_NAN && strang_next->next != NULL )
  {
    if ( ( vorwaerts && station <= strang_next->anfang )  ||
         ( !vorwaerts && station >= strang_next->anfang ) 
       )
      break;
    strang_prev = strang_next;
    strang_next = strang_next->next;
  };

  // Strangnummerierung auffrischen und das Ende suchen
  strang_help = strang_next;
  while ( strang_help->next->next != NULL )
  {
    strang_help->nummer++;
    strang_help = strang_help->next;
  };
  // strang_help zeigt jetzt auf das vorletzte Element
  
  // 'neues' ( d.h. das letzte ) Element neu verknüpfen
  if ( strang_prev == NULL )
    *strang_anfang = strang_help->next;
  else
    strang_prev->next = strang_help->next;

  if ( strang_next->next != NULL )
    strang_help->next->next = strang_next;
  strang_help->next = NULL;

  *strang_ende = strang_help; // Ende des Strangs neu setzen

  if ( strang_prev == NULL )
    strang_help = *strang_anfang;
  else
    strang_help = strang_prev->next;

  // jetzt neues Element, sowie Vorgänger und Nachfolger mit Daten füllen
  if ( strang_prev != NULL )
  {
    strang_prev->ende = station;
    strang_prev->abstand = fabs( strang_prev->ende - strang_prev->anfang );
    strcpy( strang_prev->name_ende, name );

    strang_help->nummer = strang_prev->nummer + 1;
  }
  else
    strang_help->nummer = 1;


  strang_help->anfang = station;
  strcpy( strang_help->name_anfang, name );
  strang_help->strang_vzk = vzk;
  strang_help->strang_pk = pk;
  strang_help->abstand_fluss[0] = '\0';
  strang_help->abstand_links[0] = '\0';
  strang_help->abstand_rechts[0] = '\0';

  if ( strang_next && strang_next->anfang != BCE_NAN )
  {
    strang_help->ende = strang_next->anfang;
    strcpy( strang_help->name_ende, strang_next->name_anfang );
    strang_help->abstand = fabs( strang_help->anfang - strang_next->anfang );
  }
  else
  {
    strang_help->ende = BCE_NAN;
    strang_help->name_ende[0] = '\0';
  };
}; // sort_new_profil


 /*******************************************************/
 void vzk_einlesen(void) //AUFRUF IN FKT:READ_PROFIL_DAT IN READPROF.CPP
  {
	int anzahl_prof_datei, i, j,k;
	char *slist_help;
	char slist_inhalt[100], station[15],name[15], vzk_einlesen[5];
	char pk_einlesen[10];
	double station_double;
	int string_compare=1, int_vzk;
	int int_pk;//Dick 31.03.99
  /***********************/

  string_compare=1;
  anzahl_prof_datei=xvt_slist_count(prof_datei);

  if(anzahl_prof_datei>0)
	{
	 for (i=0; i<anzahl_prof_datei;i++)
	  {
		slist_help=xvt_slist_get_elt(prof_datei,i,0L);
		strcpy(slist_inhalt,slist_help);
		for(j=10;j<=17;j++)
			station[j-10]=slist_inhalt[j];
		station[j-9]='\0';
		station_double=atof(station);

		for(j=44;j<=55;j++)
			name[j-44]=slist_inhalt[j];
		name[12]='\0';

		for(j=29;j<=31;j++)
		  vzk_einlesen[j-29]=slist_inhalt[j];
		int_vzk=atoi(vzk_einlesen);
        k=0;//Dick 31.03.99
		for (j=19;j<=27;j++)//Dick 31.03.99
            {
             if(slist_inhalt[j]!=' ')//Dick 31.03.99
                 {
			      pk_einlesen[k]=slist_inhalt[j];
                  k++;
                 }
            }
		//int_pk=atoi(pk_einlesen);
        switch(pk_einlesen[0])//Dick 31.03.99
            {
            case '0':
                int_pk=0;
                break;
            case 'L':
                int_pk=100;
                if(pk[2]!='\0')
                    int_pk+=atoi(&pk[2]);
                break;
            case 'F':
                int_pk=200;
                if(pk[2]!='\0')
                    int_pk+=atoi(&pk[2]);
                break;
            case 'R':
                int_pk=300;
                if(pk[2]!='\0')
                    int_pk+=atoi(&pk[2]);
                break;
            default:
                int_pk=0;
                break;
            }
		string_compare=xvt_str_compare_ignoring_case(name,strang_ptr->name_anfang);

		if((station_double==strang_ptr->anfang) && (string_compare==0))
			{
			 strang_ptr->strang_vzk=int_vzk;
			 strang_ptr->strang_pk=int_pk;//Dick 31.03.99
			}
	  } //FOR SLIST
	} //IF ANZAHL PROF-DATEI >0


  }
  /**********************************************************/
  void anhaengen(void)  //AUFRUF AUS WSPDLG 136
  {
  STRANG *helpstrang;
  FILE *str_dt;
  char strdatei[100];
  char inhalt_str[120];
  char var[15];
  int i=0;

  strang_ptr=strang_anfang;
  while(strang_ptr->next->anfang!=BCE_NAN)
	strang_ptr=strang_ptr->next;
  if((strang_ptr->anfang!=BCE_NAN) &&
		(strang_ptr->ende!=BCE_NAN))
  {
  helpstrang=new STRANG;
  helpstrang->anfang=strang_ptr->ende;
  helpstrang->ende=BCE_NAN;
  helpstrang->nummer=strang_ptr->nummer+1;
  helpstrang->strang_vzk=0;
  helpstrang->strang_pk=0;//Dick 31.03.99
  helpstrang->abstand_links[0]='\0';
  helpstrang->abstand_rechts[0]='\0';
  helpstrang->abstand_fluss[0]='\0';
  for (int i=0; i<(INT)strlen(strang_ptr->name_ende);i++)
	 helpstrang->name_anfang[i]=strang_ptr->name_ende[i];
  helpstrang->name_anfang[i]='\0';
  helpstrang->name_ende[0]='\0';
  helpstrang->next=strang_ptr->next;
  strang_ptr->next=helpstrang;
  strang_ptr=strang_ptr->next;
  vzk_einlesen(); //in strang.cpp //Dick 31.03.99 vzk und pk in der letzte strang aktualisieren
  strang_ptr=strang_ptr->next;
  while(strang_ptr!=NULL)
  {
	strang_ptr->nummer=strang_ptr->nummer+1;
	strang_ptr=strang_ptr->next;
  }
  } //if !=BCE_NAN

  if((anzahl_profil_dat_entries==1) && (anzahl_strang_entries==0))
	{
		xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,strdatei,50);
		strcat(strdatei,"\\");
		strcat(strdatei,STR_SPEC.name);

		if ((str_dt= fopen(strdatei,"r"))==NULL)
			 {
			  //xvt_dm_post_error(" Datei : %s läßt sich nicht öffnen !",strdatei);
              char buf[200],buf2[200];
              xvt_res_get_str(STR_DATEI,buf,sizeof(buf));
              xvt_res_get_str(STR_CANOTOPEN,buf2,sizeof(buf2));
              xvt_dm_post_error("%s%s%s",buf,strdatei,buf2);
			  exit(-1);
			 }
		else
		 {
		  fgets(inhalt_str,110,str_dt);
		  fgets(inhalt_str,110,str_dt);
		  for(i=10;i<18;i++)
			 var[i-10]=inhalt_str[i];
		  var[i-10]='\0'; //Dick 30.03.99
		  strang_ptr->anfang=atof(var);
		  for(i=19;i<18;i++)
			var[i-19]=inhalt_str[i];
		  var[i-19]='\0';    //Dick 30.03.99
//		  strang_ptr->strang_pk=atoi(var);
		  for(i=29;i<32;i++)
			var[i-29]=inhalt_str[i];
		  var[i-29]='\0';//Dick 30.03.99
//		  strang_ptr->strang_pk=atoi(var);
		  for (i=44;i<56;i++)
			var[i-44]=inhalt_str[i];
		  var[i-44]='\0'; //Dick 30.03.99
		  for(i=0; i<(INT)strlen(var);i++)
			strang_ptr->name_anfang[i]=var[i];
		  strang_ptr->name_anfang[i]='\0';
		  strang_ptr->name_ende[0]='\0';
		  fclose(str_dt);
		 } //ELSE
	} //IF NUR  1 PROFILDATEI UND KEIN STRANG

  }
/*************************************************************/

void wandle_abstand_in_string(void)
{
  //DIESE FUNKTION WURDE NUR EINGEFÜGT WEIL GCVT FEHLSCHLUG IN
  //FUNKTION SORT_NEW_PROFIL - WANDELT IN STRANGLISTE DOUBLE ABSTAND
  //IN STRING UM
  // AUFRUF HINTER FKT SORT-NEW-PROFIL AUS WSPW120 & WSPDLG136
  
  if(strang_anfang!=NULL)
  {
    strang_ptr=strang_anfang;
    while((strang_ptr->anfang!=BCE_NAN) && (strang_ptr->ende!=BCE_NAN)
      && (strang_ptr->next!=NULL))
    {
      if(strang_ptr->abstand!=BCE_NAN)
      {
        sprintf(strang_ptr->abstand_links,"%.4lf",strang_ptr->abstand);
        sprintf(strang_ptr->abstand_fluss,"%.4lf",strang_ptr->abstand);
        sprintf(strang_ptr->abstand_rechts,"%.4lf",strang_ptr->abstand);
        strang_ptr->abstand=BCE_NAN;
      }
      strang_ptr=strang_ptr->next;
    }
    
    if((strang_ptr->anfang!=BCE_NAN) && (strang_ptr->ende!=BCE_NAN)
      && (strang_ptr->next==NULL))
    {
      if(strang_ptr->abstand!=BCE_NAN)
      {
        sprintf(strang_ptr->abstand_links,"%.4lf",strang_ptr->abstand);
        sprintf(strang_ptr->abstand_fluss,"%.4lf",strang_ptr->abstand);
        sprintf(strang_ptr->abstand_rechts,"%.4lf",strang_ptr->abstand);
      }
    }
  } //IF STRANG-ANFANG !=NULL
}

/*************************************************************/
void wandle_abstand_in_string(STRANG *alt_strang)
{
  //für update von Abständen von dem alten Profil
  
  if(strang_anfang!=NULL)
  {
    strang_ptr=strang_anfang;
    while((strang_ptr->anfang!=BCE_NAN) && (strang_ptr->ende!=BCE_NAN)
      && (strang_ptr->next!=NULL))
    {
	     if(alt_strang!=NULL)
       {
         strcpy(strang_ptr->abstand_links,alt_strang->abstand_links);
         strcpy(strang_ptr->abstand_fluss,alt_strang->abstand_fluss);
         strcpy(strang_ptr->abstand_rechts,alt_strang->abstand_rechts);
         strang_ptr->abstand=BCE_NAN;
       }
       strang_ptr=strang_ptr->next;
       if(alt_strang)
         alt_strang=alt_strang->next;
    }
    
    if((strang_ptr->anfang!=BCE_NAN) && (strang_ptr->ende!=BCE_NAN)
      && (strang_ptr->next==NULL))
    {
      if(alt_strang!=NULL)
      {
        strcpy(strang_ptr->abstand_links,alt_strang->abstand_links);
        strcpy(strang_ptr->abstand_fluss,alt_strang->abstand_fluss);
        strcpy(strang_ptr->abstand_rechts,alt_strang->abstand_rechts);
      }
    }
  } //IF STRANG-ANFANG !=NULL
}

/***************************************************************/
void delete_strang_entry_neu(void)
// Löscht einen Strangeintrag aus der Strangliste
//
// Seiteneffekte:
//      ändert:
//          STRANG* strang_ptr, strang_anfang; // zeiger auf den Strang
//          
//      liest:
//          FILE_SPEC file_spec; // .name enthält den Dateinamen des zu löschenden Profils

{
  STRANG* help;
  int str_vergleichende = 1, str_vergleichanfang;
  int anfang_testen = 1;
  BOOLEAN gefunden;
  //-------------------------//
  
  strang_ptr = strang_anfang;
  str_vergleichende = 1;
  str_vergleichanfang = 1;
  anfang_testen = 1;
  gefunden = FALSE;
  
  while ( ( strang_ptr->nummer <= STRANGANZAHL ) && ( !gefunden ) && ( strang_ptr->anfang != BCE_NAN ) )
  {
    str_vergleichende = xvt_str_compare_ignoring_case( file_spec.name,strang_ptr->name_ende );
    str_vergleichanfang = xvt_str_compare_ignoring_case( file_spec.name, strang_ptr->next->name_anfang );
    if( ( str_vergleichanfang == 0 ) && ( str_vergleichende == 0 ) )
    {
      gefunden = TRUE;
      strang_ptr->ende = strang_ptr->next->ende;
      strcpy( strang_ptr->name_ende, strang_ptr->next->name_ende );
      if( strang_ptr->ende != BCE_NAN )
        strang_ptr->abstand = fabs( strang_ptr->ende - strang_ptr->anfang );
      else
      {
        strang_ptr->abstand = BCE_NAN;
        strang_ptr->abstand_links[0]='\0';
        strang_ptr->abstand_rechts[0]='\0';
        strang_ptr->abstand_fluss[0]='\0';
      }
      help = strang_ptr->next;
      strang_ptr->next = strang_ptr->next->next;
      strang_ptr = strang_ptr->next;
      delete help;

      while( ( strang_ptr->nummer <= STRANGANZAHL ) && ( strang_ptr->next != NULL ) )
      {
        strang_ptr->nummer = strang_ptr->nummer - 1;
        strang_ptr = strang_ptr->next;
      }
      if( strang_ptr->next == NULL )
      {
        strang_ptr->nummer = strang_ptr->nummer - 1;
        if( ( strang_ptr->nummer == STRANGANZAHL - 1 ) && ( strang_ptr->next == NULL ) )
        {
          help = new STRANG;
          help->next = NULL;
          help->nummer = strang_ptr->nummer+1;
          help->anfang = BCE_NAN;
          help->ende = BCE_NAN;
          help->strang_vzk=0;
          help->strang_pk=0;
          help->abstand_links[0] = '\0';
          help->abstand_rechts[0]= '\0';
          help->abstand_fluss[0] = '\0';
          help->name_anfang[0] = '\0';
          help->name_ende[0] = '\0';
          help->abstand = BCE_NAN;
          strang_ptr->next = help;
        }
      }
    } //vergleichanfang &&vergleichende

    anfang_testen = xvt_str_compare_ignoring_case( file_spec.name, strang_ptr->name_anfang );
    if( ( !gefunden ) && ( anfang_testen == 0) && ( strang_ptr == strang_anfang ) )
    {
      gefunden = TRUE;
      help = strang_anfang;
      strang_anfang = strang_ptr->next;
      strang_ptr = strang_anfang;
      delete help;
      while( ( strang_ptr->nummer <= STRANGANZAHL ) && ( strang_ptr->next != NULL ) )
      {
        strang_ptr->nummer = strang_ptr->nummer - 1;
        strang_ptr = strang_ptr->next;
      }
      if( strang_ptr->next == NULL )
      {
        strang_ptr->nummer = strang_ptr->nummer - 1;
        if( ( strang_ptr->nummer == STRANGANZAHL - 1 ) && ( strang_ptr->next == NULL ) )
        {
          help = new STRANG;
          help->next = NULL;
          help->nummer = strang_ptr->nummer + 1;
          help->anfang = BCE_NAN;
          help->ende   = BCE_NAN;
          help->strang_vzk = 0;
          help->strang_pk=0;
          help->abstand_links[0] ='\0';
          help->abstand_rechts[0]='\0';
          help->abstand_fluss[0] ='\0';
          help->name_anfang[0] = '\0';
          help->name_ende[0] = '\0';
          help->abstand = BCE_NAN;
          strang_ptr->next = help;
        }
      }
    }

    if( ( !gefunden ) && ( anfang_testen == 0 ) && ( strang_ptr != strang_anfang ) )
    {
      char buf[200], buf2[200];
      xvt_res_get_str( STR_VERZ_SYSTEM, buf, sizeof(buf) );   // "Sie haben ein verzweigtes System aufgebaut" 
      xvt_res_get_str( STR_STRANG_EDIT, buf2, sizeof(buf2) ); // "Die Strangtabelle ist von Hand zu editieren");
      xvt_dm_post_note( "%s\n%s", buf, buf2 );
    }
    if( !gefunden )
      strang_ptr = strang_ptr->next;
  } //while nicht gefunden
  if( gefunden )
    wandle_abstand_in_string();
} // delete_strang_entry_neu()

/***************************************************/
void StrangUpdateIndex(void)
{//Strang-Nummer beim Beenden von Win120 neu nummerieren
 int i=1;
 strang_ptr = strang_anfang;
 while (strang_ptr)
  {
	strang_ptr->nummer = i++;
	strang_ptr = strang_ptr->next;
  }
}
/***************************************************/
 /***************************************************************/
 void change_strang_entry(void)
  {
	//STRANG *help;
	int str_vergleichende=1, str_vergleichanfang;
	int anfang_testen=1;
	BOOLEAN gefunden;
	/*********************/

	strang_ptr=strang_anfang;
	str_vergleichende=1;
	str_vergleichanfang=1;
	anfang_testen=1;
	gefunden=FALSE;

	while((strang_ptr->nummer<=STRANGANZAHL) && (!gefunden)
			  && (strang_ptr->anfang!=BCE_NAN))
	 {
	  str_vergleichende=xvt_str_compare_ignoring_case(file_spec.name,strang_ptr->name_ende);
	  str_vergleichanfang=xvt_str_compare_ignoring_case(file_spec.name,strang_ptr->next->name_anfang);
	  if((str_vergleichanfang==0) && (str_vergleichende==0))
		{
		 gefunden=TRUE;
		 strang_ptr->ende=atof(station208);
         strang_ptr->next->anfang=atof(station208);
		 strang_ptr->next->strang_vzk=atoi(vzk);//Dick 31.03.99 next->
         switch(pk[0])//Dick 31.03.99
            {
            case '0':
                strang_ptr->next->strang_pk=0;
                break;
            case 'L':
                strang_ptr->next->strang_pk=100;
                if(pk[2]!='\0')
                    strang_ptr->next->strang_pk+=atoi(&pk[2]);
                break;
            case 'F':
                strang_ptr->next->strang_pk=200;
                if(pk[2]!='\0')
                    strang_ptr->next->strang_pk+=atoi(&pk[2]);
                break;
            case 'R':
                strang_ptr->next->strang_pk=300;
                if(pk[2]!='\0')
                    strang_ptr->next->strang_pk+=atoi(&pk[2]);
                break;
            default:
                strang_ptr->next->strang_pk=0;
                break;
            }
		 if(strang_ptr->ende!=BCE_NAN)
			strang_ptr->abstand=strang_ptr->ende-strang_ptr->anfang;
		 else
		  {
			strang_ptr->abstand=BCE_NAN;
			strang_ptr->abstand_links[0]='\0';
			strang_ptr->abstand_rechts[0]='\0';
			strang_ptr->abstand_fluss[0]='\0';
		  }		 


		} //vergleichanfang &&vergleichende
		anfang_testen=xvt_str_compare_ignoring_case(file_spec.name,strang_ptr->name_anfang);
		if((!gefunden) &&(anfang_testen==0) && (strang_ptr==strang_anfang))
		 {
		  gefunden=TRUE;
		  strang_ptr->anfang=atof(station208);
          strang_ptr->strang_vzk=atoi(vzk);
		  switch(pk[0])//Dick 31.03.99  und 24.08.99
            {
            case '0':
                strang_ptr->strang_pk=0;
                break;
            case 'L':
                strang_ptr->strang_pk=100;
                if(pk[2]!='\0')
                    strang_ptr->strang_pk+=atoi(&pk[2]);
                break;
            case 'F':
                strang_ptr->strang_pk=200;
                if(pk[2]!='\0')
                    strang_ptr->strang_pk+=atoi(&pk[2]);
                break;
            case 'R':
                strang_ptr->strang_pk=300;
                if(pk[2]!='\0')
                    strang_ptr->strang_pk+=atoi(&pk[2]);
                break;
            default:
                strang_ptr->strang_pk=0;
                break;
            }		 

		 }
		if((!gefunden) &&(anfang_testen==0) && (strang_ptr!=strang_anfang))
            {
		      //xvt_dm_post_note("Sie haben ein verzweigtes System aufgebaut"
				//				 "Die Strangtabelle ist von Hand zu editieren");
             char buf[200],buf2[200];
             xvt_res_get_str(STR_VERZ_SYSTEM,buf,sizeof(buf));
             xvt_res_get_str(STR_STRANG_EDIT,buf2,sizeof(buf2));
             xvt_dm_post_note("%s\n%s",buf,buf2);
            }
		if(!gefunden)
			strang_ptr=strang_ptr->next;
	 } //while nicht gefunden
	if(gefunden)
	 wandle_abstand_in_string();
  }

/***************************************************/
