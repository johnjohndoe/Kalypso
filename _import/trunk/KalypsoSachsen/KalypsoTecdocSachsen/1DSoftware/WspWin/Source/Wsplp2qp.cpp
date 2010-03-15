/*-------------------------------------------------------------------------
WSPLP2QP.CPP												02.04.97
-------------------------------------------------------------------------*/

#include <windows.h>
#include "xvt.h"

#include <math.h>

#include "resource.h"

#include "typen.h"
#include "global_types.h"

#include "laengs1.h"

#include "list.h"
#include "wsplist.h"
#include "readprof.h"
#include "strang.h"

extern FILE_SPEC file_spec, STR_SPEC;
extern int ds_info[TYPE_SIZE];
extern int typ[TYPE_SIZE];
extern STRANG* strang_anfang;
extern int anzahl_strang_entries;      // Anzahl  Einträge in der Strangtabelle

/*******************************************************
Wasserspiegel aus Längsschnittdatei
in alle Querprofile übertragen

  DIRECTORY *dirp := Pfad der Längsschnittdatei
  char * filename := Dateiname der Längsschnittdatei
  
    Rückgabe: 1 = OK
    0 = Fehler
*******************************************************/
int InsertWspLpToQuerprof( WSP_PROFIL_LISTE* pWPL, DIRECTORY* dirp, char* filename, char* BerVariante, int datensatz )
{
  pWPL = Init_Profil_Liste( pWPL );//neues WPL-Element anhängen

  WSP_PROFIL_LISTE* tmpWPL = pWPL->PListNext ? pWPL->PListNext : 0;
  if( !read_profildatei( tmpWPL, dirp, filename ) )  // Längsprofil einlesen
  {
    // Vernetzungsdatei einlesen
    if( strang_anfang )
      destroy_strang_tabelle();
    strang_anfang = NULL;
    MakeNewStrang( STRANGANZAHL );
    read_profil_dat( strang_anfang );

    pWPL->PListNext;

    int pos = 1;
    int n_vzk = -1;
    double station_f = BCE_NAN;
    double wsp_fix, wsp;

    while( tmpWPL->PList->ListWspLpData( pos, &station_f, &wsp, &n_vzk, &wsp_fix ) > 0 )
    {
      int str = 1;
      station_f = station_f * -0.001; //Strangtabelle:[km] <--> Längsprofil:[m]

      char station_ch[20];//Dick 3.08.98
      sprintf( station_ch, "%.4lf", station_f );//Dick 3.08.98
      sscanf( station_ch, "%.4lf", &station_f );//Dick 3.08.98

      STRANG* pstrang = strang_anfang;
      while( str <= anzahl_strang_entries && pstrang )
      {
        LPSTR ptr2 = 0;

        if( fabs( pstrang->anfang-station_f ) < PRECISION_KRD && pstrang->strang_vzk == n_vzk )
            ptr2 = pstrang->name_anfang; // ptr2 zeigt auf Dateinamen
        // Sonderfall für letzten Strang: auch das Ende prüfen
        else if( str == anzahl_strang_entries )
        {
          if( fabs( pstrang->ende - station_f ) < PRECISION_KRD && pstrang->strang_vzk == n_vzk )
             ptr2 = pstrang->name_ende; // ptr2 zeigt auf Dateinamen
        }
        
        if( ptr2 )
        {
          tmpWPL = Init_Profil_Liste( tmpWPL );//neues WPL-Element anhängen
          
          WSP_PROFIL_LISTE* tmpQpWPL = tmpWPL->PListNext ? tmpQpWPL = tmpWPL->PListNext : 0;
          
          xvt_fsys_set_dir( &file_spec.dir );
          strcpy( file_spec.name, ptr2 );
          if( !read_profildatei( tmpQpWPL, &STR_SPEC.dir, ptr2, FALSE ) )  // Querprofil einlesen
          { // Daten nach Querprofil
            int anzahlDs = tmpQpWPL->data->ds_info[0] + 1;

            tmpQpWPL->data->ds_info[0]++;
            tmpQpWPL->data->typ[anzahlDs] = datensatz;
            tmpQpWPL->data->ds_info[anzahlDs] = 2;
            tmpQpWPL->data->anzahl_ds++;
            
            // jetzt wsp-Höhe in Profil kopieren
            if( datensatz == STATION_FIX )
              wsp = wsp_fix;
            if( wsp != BCE_NAN )
            {
              int anzCrd = tmpQpWPL->PList->InsertQpWspInLp( anzahlDs, wsp, filename, BerVariante, datensatz );
              if( anzCrd > 0 )
              {
                tmpQpWPL->data->ds_info[anzahlDs] = anzCrd;
                save_profildatei( tmpQpWPL, false );  
              };
            }; // if wsp != BCE_NAN
          }
          Delete_Profil_Liste( tmpWPL->PListNext );
          tmpWPL->PListNext = NULL;
        }

        pstrang = pstrang->next;
        str++;
      }
      pos++;
    }
  }
  else 
    return 0;

  Delete_Profil_Liste( pWPL->PListNext );
  pWPL->PListNext = NULL;
  
  return 0;
}

/*******************************************************
Wasserspiegel (aus Längsschnittdatei:Referenz Dateiname)
in allen Querprofilen löschen

DIRECTORY *dirp := Pfad der Längsschnittdatei
char * filename := Dateiname der Längsschnittdatei
  
Rückgabe: 1 = OK
0 = Fehler
*******************************************************/

int DeleteWspInQuerprof(WSP_PROFIL_LISTE *pWPL,DIRECTORY *dirp,char* filename,char *BerVariante)
{
  char *StrFile = new char[255];
  char *station_ch=new char[20];//Dick 3.08.98
  LPSTR ptr2;//ptr1,
  WSP_PROFIL_LISTE *tmpWPL,*tmpQpWPL;
  double station_f,wsp,
    station_save=BCE_NAN,
    wsp_fix;
  STRANG    *pstrang;
  int str, ds, pos =1,n_vzk=-1,
    gleiche_laengs_station=1;
  BOOL gefunden;
  
  
  pWPL=Init_Profil_Liste(pWPL);//neues WPL-Element anhängen
  if (pWPL->PListNext !=NULL)
    tmpWPL = pWPL->PListNext;
  
  xvt_fsys_set_dir(dirp);
  strcpy(file_spec.name,filename); // obsolet?
  if(!read_profildatei( tmpWPL, dirp, filename ) )  // Längsprofil einlesen
  {
    // Vernetzungsdatei einlesen
    if (strang_anfang != NULL)
      destroy_strang_tabelle();
    strang_anfang=NULL;
    MakeNewStrang(STRANGANZAHL);
    read_profil_dat(strang_anfang);
    
    while(tmpWPL->PList->ListWspLpData(pos,&station_f,&wsp,&n_vzk,&wsp_fix)>0)
    {
      
      if (fabs(station_save-station_f)<1.e-5)
        gleiche_laengs_station++;
      else
        gleiche_laengs_station=1;
      station_save=station_f;
      
      gefunden = FALSE;
      pstrang = strang_anfang;
      str=1;
      station_f = station_f*(-0.001); //Strangtabelle:[km] <--> Längsprofil:[m]
      sprintf(station_ch,"%.4lf",station_f); //Dick 3.08.98 
      sscanf(station_ch,"%.4lf",&station_f); //Dick 3.08.98
      int gleiche_prof_station=0;
      while ((str<=anzahl_strang_entries)&&(pstrang !=NULL))
      {
        if( str == anzahl_strang_entries )
        {                                   //Dick 3.08.98
          if ((fabs(pstrang->anfang-station_f)<1.e-5)&&(pstrang->strang_vzk==n_vzk))//||(fabs(diff)<0.0001000001)) //Strangtabelle:[km] <--> Längsprofil:[m]
          {
            gleiche_prof_station++;
            if(gleiche_prof_station==gleiche_laengs_station)
            {
              ptr2 = pstrang->name_anfang; // ptr2 zeigt auf Dateinamen
              gefunden = TRUE;                  
            }
          }                                   //Dick 3.08.98
          else if ((fabs(pstrang->ende-station_f)<1.e-5)&&(pstrang->strang_vzk==n_vzk))//||(fabs(diff2)<0.0001000001)) //Strangtabelle:[km] <--> Längsprofil:[m]
          {
            gleiche_prof_station++;
            if(gleiche_prof_station==gleiche_laengs_station)
            {
              ptr2 = pstrang->name_ende; // ptr2 zeigt auf Dateinamen
              gefunden = TRUE;                  
            }
          }
        }                                        //Dick 3.08.98
        else if ((fabs(pstrang->anfang-station_f)<1.e-5)&&(pstrang->strang_vzk==n_vzk))//||(fabs(diff)<0.0001000001)) //Strangtabelle:[km] <--> Längsprofil:[m]
        {
          gleiche_prof_station++;
          if(gleiche_prof_station==gleiche_laengs_station)
          {
            ptr2 = pstrang->name_anfang; // ptr2 zeigt auf Dateinamen
            gefunden = TRUE;                  
          }
        }
        if (gefunden)
        {
          
          tmpWPL=Init_Profil_Liste(tmpWPL);//neues WPL-Element anhängen
          if (tmpWPL->PListNext !=NULL)
            tmpQpWPL = tmpWPL->PListNext;
          xvt_fsys_set_dir(&file_spec.dir);
          strcpy(file_spec.name,ptr2);
          if( !read_profildatei( tmpQpWPL, &STR_SPEC.dir, ptr2 ) )  // Querprofil einlesen
          { 
            ds = tmpQpWPL->PList->FindWspQpData(filename,BerVariante);
            if (ds)
            {
              tmpQpWPL->PList->DeleteNode( ds, (int*)ds_info, (int*)typ );
              
              for (int i=ds;i<TYPE_SIZE-1;i++)//Dick 8.12.98
                tmpQpWPL->data->typ[i]=tmpQpWPL->data->typ[i+1];
              save_profildatei(tmpQpWPL);  
            }
            
          }
          Delete_Profil_Liste(tmpWPL->PListNext);
          tmpWPL->PListNext = NULL;
          pstrang = NULL;
          
        }
        else 
        {
          pstrang = pstrang->next;
          str++;
        }
      }
      pos++;
    }
  }
  else return 0;
  
  delete[] StrFile;
  delete[] station_ch;//Dick 3.08.98
  Delete_Profil_Liste(pWPL->PListNext);
  pWPL->PListNext =NULL;
  return 0;
  
}

void deleteAllDatablocksFromProfile( int datensatz, WSP_PROFIL_LISTE* tmpWPL, STRANG* aktStr, bool bEnde )
{
	// nur falls die station (mit dieser vzk) auch im längsschnitt vorkommt, löschen
	double station_f;
	double wsp;
	double wsp_fix;
	int n_vzk;
	
	double station = bEnde ? aktStr->ende : aktStr->anfang;
    bool gefunden = false;
	int pos = 1;
    while( tmpWPL->PList->ListWspLpData( pos, &station_f, &wsp, &n_vzk, &wsp_fix ) > 0 )
    {
		pos++;

		station_f = station_f * ( -0.001 ); //Strangtabelle:[km] <--> Längsprofil:[m]

		// runden
		char* station_ch = new char[20];//Dick 3.08.98
		sprintf( station_ch, "%.4lf", station_f );
		sscanf( station_ch, "%.4lf", &station_f );
		delete[] station_ch;

		if( fabs( station - station_f ) < 1.e-5 && aktStr->strang_vzk == n_vzk )
		{
			gefunden = true;
			break;
		}
	}
	
	if( gefunden == false )
		return;


	WSP_PROFIL_LISTE* tmpQpWPL = NULL;

	LPSTR profilFile = bEnde ? aktStr->name_ende : aktStr->name_anfang;

	tmpWPL = Init_Profil_Liste( tmpWPL );//neues WPL-Element anhängen
	if( tmpWPL->PListNext !=NULL )
		tmpQpWPL = tmpWPL->PListNext;
	
	xvt_fsys_set_dir( &file_spec.dir );
	strcpy( file_spec.name, profilFile );
	
	if( !read_profildatei( tmpQpWPL, &STR_SPEC.dir, profilFile ) )  // Querprofil einlesen
	{ 
		int ds = tmpQpWPL->PList->ExistDatensatzTyp( datensatz );
		if( ds )
		{
			do
			{
				tmpQpWPL->PList->DeleteNode(ds,(int*)ds_info, (int*)typ );
                
				for( int i = ds; i < TYPE_SIZE - 1; i++ )
					tmpQpWPL->data->typ[i] = tmpQpWPL->data->typ[i + 1];
			}
			while( ( ds = tmpQpWPL->PList->ExistDatensatzTyp( datensatz ) ) != 0 );
			save_profildatei( tmpQpWPL );  
		}
	}
	
	Delete_Profil_Liste(tmpWPL->PListNext);
	tmpWPL->PListNext = NULL;
}

/*******************************************************
Alle Wasserspiegel (aus Längsschnittdatei:Referenz Dateiname)
in allen Querprofilen löschen

DIRECTORY *dirp := Pfad der Längsschnittdatei
char * filename := Dateiname der Längsschnittdatei
  
Rückgabe: 1 = OK
0 = Fehler
*******************************************************/
int DeleteAlleWspInQuerprof(WSP_PROFIL_LISTE *pWPL,DIRECTORY *dirp,char* filename,int datensatz)
{
  char* StrFile = new char[255];
  char strname[15];

  LPSTR ptr1;
  WSP_PROFIL_LISTE *tmpWPL;
  
  pWPL = Init_Profil_Liste( pWPL );//neues WPL-Element anhängen
  if( pWPL->PListNext !=NULL )
    tmpWPL = pWPL->PListNext;
  
  if( !read_profildatei( tmpWPL, dirp, filename ) )  // Längsprofil einlesen
  {
    //aus Längsprofilname  Namen der *.str-Datei bauen
    strcpy(strname,filename);
    if ( ptr1 = strrchr(strname,'.') )
    {
      ptr1[0]='\0';
      strcat(strname,".str");
      strname[2] = strname[3] = '0';
      strcpy(STR_SPEC.name,strname);
      
      xvt_fsys_convert_dir_to_str(dirp,StrFile,255);
      ptr1=strrchr(StrFile,'\\');
      if (ptr1) ptr1[1]='\0';
      strcat(StrFile,"prof");
      xvt_fsys_convert_str_to_dir(StrFile,&STR_SPEC.dir);
    }
    
    // Vernetzungsdatei einlesen
    if (strang_anfang != NULL)
      destroy_strang_tabelle();
    
	strang_anfang=NULL;
    
	MakeNewStrang(STRANGANZAHL);
    
	read_profil_dat(strang_anfang);
    

	// alles neu:
	// durch strang iterieren und bei JEDER datei alles löschen
	STRANG* pStrang;
	int strCount;
	for( strCount = 0, pStrang = strang_anfang; 
		 strCount < anzahl_strang_entries && pStrang != 0; 
		 strCount++, pStrang = pStrang->next)
		 {
			 deleteAllDatablocksFromProfile( datensatz, tmpWPL, pStrang, false );

			 if( strCount + 1 == anzahl_strang_entries )
				 deleteAllDatablocksFromProfile( datensatz, tmpWPL, pStrang, true );
		 }
  }
  else 
	  return 0;
  
  delete[] StrFile;
  Delete_Profil_Liste(pWPL->PListNext);
  pWPL->PListNext =NULL;
  return 1;
  
}

/*******************************************************
Erstellen eine UG-datei(aus Längsschnittdatei:Referenz Dateiname)

DIRECTORY *dirp := Pfad der Längsschnittdatei
char * filename := Dateiname der Längsschnittdatei
    
Rückgabe: 1 = OK
0 = Fehler (wenn Profil oder im Profil die Datensetze RECHTSWERT und HOCHWERT nicht gefunden wurden
*******************************************************/

int WspToUGrenze(WSP_PROFIL_LISTE *pWPL,DIRECTORY *dirp,char* filename)
{
  char *StrFile = new char[255];
  char strname[15],strname_ug[15];
  char *station_ch=new char[20];//Dick 3.08.98
  LPSTR ptr1,ptr2;
  Koord *dat1,*dat2;
  WSP_PROFIL_LISTE *tmpWPL,*tmpQpWPL;
  double station_f,wsp,wsp_fix;//,wspdat[500];
  STRANG    *pstrang;
  int str,i=0,k, pos =1,n_vzk=-1,fehler_hmo=0;
  BOOL gefunden;
  FILE *f;
  
  double ug_wsp_x1=BCE_NAN,ug_wsp_x2=-BCE_NAN,ug_rechtswert[2],ug_hochwert[2];
  
  pWPL=Init_Profil_Liste(pWPL);//neues WPL-Element anhängen
  if (pWPL->PListNext !=NULL)
    tmpWPL = pWPL->PListNext;
  
  xvt_fsys_set_dir(dirp);
  strcpy(file_spec.name,filename); // obsolet?
  if(!read_profildatei( tmpWPL, &STR_SPEC.dir, filename ))  // Längsprofil einlesen
  {
    //aus Längsprofilname  Namen der *.str-Datei bauen
    int len=strlen(filename);
    
    strcpy(strname,filename);
    strcpy(strname_ug,strname);
    strname_ug[2]='u';
    strname_ug[3]='g';
    if ( ptr1 = strrchr(strname,'.') )
    {
      ptr1[0]='\0';   
      strcat(strname,".str");
      strname[2] = strname[3] = '0';   
      strcpy(STR_SPEC.name,strname);
      xvt_fsys_convert_dir_to_str(dirp,StrFile,255);
      ptr1=strrchr(StrFile,'\\');
      if (ptr1) ptr1[1]='\0';
      strcat(StrFile,"prof");
      xvt_fsys_convert_str_to_dir(StrFile,&STR_SPEC.dir);
    }
    
    
    if ((f = fopen(strname_ug,"w+"))==NULL)
    {    
      char buf[200],buf2[200];//Dick 26.11.99
      xvt_res_get_str(STR_UEBERSCHW_DATEI,buf,sizeof(buf));
      xvt_res_get_str(STR_PLOT_NOTE_5,buf2,sizeof(buf2));
      xvt_dm_post_note("%s %s",buf,buf2);
      //xvt_dm_post_note("Überschwämungsgrenze-Datei läßt sich nicht öffnen");
      return 1;
    }
    
    // Vernetzungsdatei einlesen
    if (strang_anfang != NULL)
      destroy_strang_tabelle();
    strang_anfang=NULL;
    MakeNewStrang(STRANGANZAHL);
    read_profil_dat(strang_anfang);
    
    while(tmpWPL->PList->ListWspLpData(pos,&station_f,&wsp,&n_vzk,&wsp_fix)>0)
    {
      gefunden = FALSE;
      pstrang = strang_anfang;
      str=1;
      station_f = station_f*(-0.001); //Strangtabelle:[km] <--> Längsprofil:[m]
      sprintf(station_ch,"%.4lf",station_f); //Dick 3.08.98 
      sscanf(station_ch,"%.4lf",&station_f); //Dick 3.08.98
      while ((str<=anzahl_strang_entries)&&(pstrang !=NULL))
      {
        
        
        if (str==anzahl_strang_entries)
        {                                   //Dick 3.08.98
          if ((fabs(pstrang->anfang-station_f)<1.e-5)&&(pstrang->strang_vzk==n_vzk))//||(fabs(diff)<0.0001000001)) //Strangtabelle:[km] <--> Längsprofil:[m]
          {
            ptr2 = pstrang->name_anfang; // ptr2 zeigt auf Dateinamen
            gefunden = TRUE;
          }                                   //Dick 3.08.98
          else if ((fabs(pstrang->ende-station_f)<1.e-5)&&(pstrang->strang_vzk==n_vzk))//||(fabs(diff2)<0.0001000001)) //Strangtabelle:[km] <--> Längsprofil:[m]
          {
            ptr2 = pstrang->name_ende; // ptr2 zeigt auf Dateinamen
            gefunden = TRUE;
          }
        }                                        //Dick 3.08.98
        else if ((fabs(pstrang->anfang-station_f)<1.e-5)&&(pstrang->strang_vzk==n_vzk))//||(fabs(diff)<0.0001000001)) //Strangtabelle:[km] <--> Längsprofil:[m]
        {
          ptr2 = pstrang->name_anfang; // ptr2 zeigt auf Dateinamen
          gefunden = TRUE;
        }
        if (gefunden)
        {
          
          tmpWPL=Init_Profil_Liste(tmpWPL);//neues WPL-Element anhängen
          if (tmpWPL->PListNext !=NULL)
            tmpQpWPL = tmpWPL->PListNext;
          xvt_fsys_set_dir(&file_spec.dir);
          strcpy(file_spec.name,ptr2);
          if( !read_profildatei( tmpQpWPL, &STR_SPEC.dir, ptr2 ) )  // Querprofil einlesen
          {
            dat1 = tmpQpWPL->PList->HoleDatensatz(RECHTSWERT);
            dat2 = tmpQpWPL->PList->HoleDatensatz(HOCHWERT);
            
            if(!tmpQpWPL->PList->GetUGWsp(ds_info[0],wsp,&ug_wsp_x1,&ug_wsp_x2))
            {
              pos++;
              continue;
            }                     
            
            
            if (dat1!=NULL && dat2!=NULL)
            {
              k=0;
              if(!tmpQpWPL->PList->GetInterpKoord(ug_wsp_x1,&ug_rechtswert[0],RECHTSWERT))
              {
                ug_rechtswert[0]=BCE_NAN;
              }
              if(!tmpQpWPL->PList->GetInterpKoord(ug_wsp_x1,&ug_hochwert[0],HOCHWERT))
              {
                ug_hochwert[0]=BCE_NAN;
              }
              if(!tmpQpWPL->PList->GetInterpKoord(ug_wsp_x2,&ug_rechtswert[1],RECHTSWERT))
              {
                ug_rechtswert[1]=BCE_NAN;
              }
              if(!tmpQpWPL->PList->GetInterpKoord(ug_wsp_x2,&ug_hochwert[1],HOCHWERT))
              {
                ug_hochwert[1]=BCE_NAN;
              }
              
              fprintf(f,"%.4lf %14lf %14lf %14lf %14lf\n",
                station_f,ug_rechtswert[0],ug_hochwert[0],ug_rechtswert[1],ug_hochwert[1]);
              
            }
            else
            {
              fprintf(f,"Es exsistiert kein RECHTSWERT oder HOCHWERT in Station %lf\n",station_f);
              fehler_hmo=1;
            }
            
            
          }
          Delete_Profil_Liste(tmpWPL->PListNext);
          tmpWPL->PListNext = NULL;
          pstrang = NULL;
          
        }
        else 
        {
          pstrang = pstrang->next;
          str++;
        }
      }
      if(!gefunden)
      {
        fprintf(f,"Station %lf nicht gefunden\n",station_f);
        fehler_hmo=1;
      }
      pos++;
    }
  }
  else return 1;
  
  
  fclose(f);
  
  
  delete[] StrFile;
  delete[] station_ch;//Dick 3.08.98
  Delete_Profil_Liste(pWPL->PListNext);
  pWPL->PListNext =NULL;
  return fehler_hmo;
  
}

