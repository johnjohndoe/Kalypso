/*---------------------------------  3a -------------------------------------
Liesst da66 Profile von der beim Benutzer abgefragten Datei

  66 (Profilpunkte Station,Hoehe 4 in einer Zeile)
  66 (weitere Profilpunktzeilen moeglich)
  88 (letzte 66-Zeile des Profils beginnt notwendigerweise mit einer 88 )
  
    und schreibt auf eine Ausgabedatei in pro.h-Format
---------------------------------------------------------------------------*/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <stdlib.h>
#include <time.h>

#include <windows.h>
#include "xvt.h"
#include "resource.h"

#include "global_defs.h"
#include "global_types.h"

#include "list.h"
#include "global.h"

#include "da66.h"


struct Dpoint3d
{
  double x,y,z;
};

struct BCEProfil
{
  Dpoint3d P[KOORDINPROFILANZAHL];               /*Profilpunkte*/

  int mPn;                          /*Profilpunktezaehler*/

  double station;
};


//char input [100];
//char ou[100];


// Vorwärtsdeklarationen
void bce_Strcut( const char*, char[100], int,int ); // nur hier
void bce_ProfilDateiAusgeben( BCEProfil*, int, char*, char* ); // nur hier
FILE* bce_Profil_datei_erzeugen ( BCEProfil*, int, char*, char* ); // nur hier


/********************************************************************************/

void da66(char *path, char *ausgabepfad)
{
  int iP = 0;               /*Zaehler Profile*/
  
  /*-------------------------------------------------------------------------------*/
  
  FILE* Dt = fopen( path,"r" );
  if( Dt == NULL )
  {
    //xvt_dm_post_note("Konnte da66 Datei nicht oeffnen\n");
    char buf[200];//Dick 26.11.99
    xvt_res_get_str(STR_DA66_NOTE_1,buf,sizeof(buf));
    xvt_dm_post_note("%s",buf);
    return;
  }
  
  /*-------------------------------------------------------------------------------*/
  if( feof(Dt) != 1 )
  {
    BOOLEAN fehler = FALSE;
    BCEProfil data;
    data.mPn = 0;

    bool bContinue = true; // solange weitermachen, bis bContinue = false
    bool bFirst = true; // Flag, ob die allererste Zeile ist

    double lastStation;

    while( bContinue )
    {
      int DaT = 0;                /* Datenart                                           */
      double StA66;// StA,            /* Gewaesser Kilometrierung                     */

      // eine Zeile lesen
      char input[100];
      char ou[100];
      if( fgets( input, 100, Dt ) == NULL )
        bContinue = false; // falls die Datei zuende ist, jetzt nicht mehr weitermachen
      else
      {
        // ansonsten Datenart und Station ermitteln
        bce_Strcut( input, ou, 1, 2 );
        sscanf( ou, "%d", &DaT ); /*DatenArT ermitteln*/

        bce_Strcut( input, ou, 10, 18 );       /*Station lesen:*/
        sscanf( ou,"%lf",&StA66 );
        if( bFirst )
        {
          lastStation = StA66; // damit kein neues Profil angelegt wird
          bFirst = false;
        }
      }

      // falls es einen Stationswechsel gab oder das Ende der Datei ereicht ist
      // jetzt das aktuelle Profil rausschreiben und resetten
      if( bContinue == false || fabs( lastStation - StA66 ) > 0.0001 )
      {

        if( !fehler && data.mPn <= KOORDINPROFILANZAHL )
          bce_ProfilDateiAusgeben( &data, iP + 1, ausgabepfad, path ); 
        else
        {
          //xvt_dm_post_note("max. moegl. Anzahl Profilpunkte %i ueberschritten\n"
          //"Zuviele Profilpunkte  Km= %lf \n",KOORDINPROFILANZAHL,data[0].station);
          char buf[200], buf2[200];//Dick 26.11.99
          xvt_res_get_str( STR_DA66_NOTE_4_1,buf,sizeof(buf) );
          xvt_res_get_str( STR_DA66_NOTE_4_2,buf2,sizeof(buf2) );
          xvt_dm_post_note( "%s%i%s%lf \n", buf, KOORDINPROFILANZAHL, buf2, data.station );
        }
        iP++;
        data.mPn = 0;
        
        fehler = FALSE;
        if( iP > STRANGANZAHL )
        {
          //xvt_dm_post_note("mehr als %i Profile nicht zulaessig\n",STRANGANZAHL);
          char buf[200],buf2[200];//Dick 26.11.99
          xvt_res_get_str(STR_DA66_NOTE_5_1,buf,sizeof(buf));
          xvt_res_get_str(STR_DA66_NOTE_5_2,buf2,sizeof(buf2));
          xvt_dm_post_note("%s%i%s\n",buf,STRANGANZAHL,buf2);
          return;
        }
      }; // if bContinue == false


      // jetzt die aktuelle Zeile parsen und die Daten dem Profil hinzufügen
      if( DaT == 66 || DaT == 88 )  /*Zeile im Datenformat 66(88)*/
      {
        data.station = StA66 / 1000 / 1000;
        
        double R,H;             /*  Profilstrecke und Höhe                            */
        double Q;               /*  Hilfsvariable                                     */

        int k = 0;                   /*Profilpunkte aus der Zeile lesen:*/
        while( k <= 3 )
        {
          bce_Strcut( input, ou, 22 + 15 * k, 28 + 15 * k );
          
          double r_help;
          if( sscanf( ou, "%lf" , &r_help ) != EOF )
            R = r_help;
          else
          {
            R = R - 1000000.;
            k = 5;
          }
          
          bce_Strcut( input, ou, 29 + 15 * k, 35 + 15 * k );
          H = 0;
          sscanf( ou,"%lf", &H );
          
          k++;
          
          if( data.mPn == 0 )
            Q = R;

          if( R >= Q ) /*Abfrage, ob Profilpunkte fortlaufend stationiert*/
          {
            int n = data.mPn;
            data.P[n].x = R;          /*Profilpunkte 2d speichern*/
            data.P[n].y = 0.0;
            data.P[n].z = H;
            data.mPn++;                    /* Profilpunktzaehler */
            if( data.mPn > KOORDINPROFILANZAHL )
              fehler=TRUE;
            Q = R;
          }/*ende if R>=Q*/
        }/*ende while k<=3*/
      }/*ende if DaT == 66*/
      
      lastStation = StA66;
    }/*ende while( fgets*/
  }/*ende if(feof*/
  
  /*und tschuess*/
  fclose( Dt );

  //xvt_dm_post_note("%d Profile angelegt",iP);
  char buf[200];//Dick 26.11.99
  xvt_res_get_str( STR_DA66_NOTE_2, buf, sizeof(buf) );
  xvt_dm_post_note("%d %s", iP ,buf );
  return;
} // da66

/*---------------------------------  4  -------------------------------------
unser eigener STRing - CUTter
---------------------------------  4  -------------------------------------*/
void bce_Strcut( const char* input, char output[100], int L,int R )
{
  int length = min( 99, R - L + 1 );
  strncpy( output, input + L -  1, length );
  output[length] = '\0'; // auf jeden Fall mit 0 abschliessen
}
/*---------------------------------  5  -------------------------------------
---------------------------------  5  -------------------------------------*/

void bce_ProfilDateiAusgeben( BCEProfil* profil, int nr, char *ausgabepfad, char* path )
{
  FILE *infile;
  int i, count=0;//j,Pnum,
  double THICK=10000.;
  double  blatthoehe=0.0;
  char string[80], satz[100];
  
  
  infile = bce_Profil_datei_erzeugen ( profil, nr, ausgabepfad, path );
  if (infile != NULL)
  {
    /* 14. Zeile */
    sprintf (string, "1 %d", profil->mPn);
    fprintf (infile, "%s\n", string);
    /* 15 Zeile */
    blatthoehe = (blatthoehe+3*THICK)/1000.0;
    if (blatthoehe<29.7) blatthoehe = 29.7;
    else if (blatthoehe<42.0) blatthoehe = 42.0;
    else if (blatthoehe<59.4) blatthoehe = 59.4;
    else                 blatthoehe = 84.1;
    sprintf(string,"0 0 0 0 0 0 0");
    fprintf (infile, "%s\n", string);
    /* 16 Zeile */
    strcpy (string, "GELAENDE-");
    fprintf (infile, "%s\n", string);
    /* 17. Zeile */
    strcpy (string, "HOEHE");
    fprintf (infile, "%s\n", string);
    /* 18. zeile */
    strcpy (string, "1 0 0  0 1 0 0 0 0");
    fprintf (infile, "%s\n", string);
    
    /* ------ Schreiben der X - Daten, das sind die ProfilStationen ---------------- */
    count = 0;
    strcpy (string, "\0");
    strcpy (satz, "\0");
    for (i=0; i < profil->mPn; i++)
    {
      if ( profil->P[i].y==0.0 )
      {
        sprintf (string, "0 %7.2lf ", profil->P[i].x/1000.0);
        strcat (satz, string);
        count++;
        if (count==8)
        {
          fprintf (infile, "%s\n", satz);
          count=0;
          strcpy (satz, "\0");
          strcpy (string, "\0");
        }
      }
    }
    if (count>0) fprintf (infile, "%s\n", satz);
    
    /* ------ Schreiben der Höhen ---------------- */
    count = 0;
    strcpy (string, "\0");
    strcpy (satz, "\0");
    for (i=0; i < profil->mPn; i++)
    {
      if (profil->P[i].y==0.0)
      {
        sprintf (string, "0 %7.2lf ", profil->P[i].z/1000.0);
        strcat (satz, string);
        count++;
        if (count==8)
        {
          fprintf (infile, "%s\n", satz); /*naechste Zeile ins file*/
          count=0;
          strcpy (satz, "\0");
          strcpy (string, "\0");
        }
      }
    }
    if (count>0) fprintf (infile, "%s\n", satz);  /*letzte Zeile ins file*/
    
    fclose (infile);
  }/*ende if infile != NULL*/
  return;
}

/*---------------------------------  6  -------------------------------------
---------------------------------  6  -------------------------------------*/
FILE* bce_Profil_datei_erzeugen ( BCEProfil* profil, int nr, char* ausgabepfad, char* path )
{
  FILE *infile;
  char     ProfDatei[256];
  char text[80];  //, jahr[5];
  struct tm xzeit;
  struct tm *tzeit=&xzeit;
  char itext [10];
  
  strcpy (text, "\0");
  strcpy (ProfDatei, "\0");
  if (strlen(ProfDatei)==0)
  {
    strcat(ProfDatei,ausgabepfad);
    if(nr<10)
      strcat(ProfDatei,"profil_");
    if((nr<100) &&(nr>=10))
      strcat(ProfDatei,"profil");
    if(nr>=100)
      strcat(ProfDatei,"profi");
    
    itoa(nr,itext,10);
    strcat(ProfDatei,itext);
    strcat(ProfDatei,".dat");
    remove (ProfDatei);
  }
  if ((infile= fopen(ProfDatei, "wt"))==NULL)
  {
    //xvt_dm_post_note ("Kann die ProfilDatei nicht oeffnen\n");
    char buf[200];//Dick 26.11.99
    xvt_res_get_str(STR_DA66_NOTE_3,buf,sizeof(buf));
    xvt_dm_post_note("%s",buf);
    return NULL;
  }else{
    // printf ("ProfilDatei %s wird beschrieben\n",ProfDatei);
  }
  /* 1. Zeile */
  fprintf (infile , "%s\n", path);
  /* 2. u. 3.  Zeile */
  LoadString(NULL, STR_AUFTRAGGEBER, text, sizeof(text));
  //strcpy (text, "Auftraggeber");
  fprintf (infile, "%s\n", text);
  fprintf (infile, "%s\n", text);
  /* 4,, 5. u. 6. Zeilen */
  LoadString(NULL, STR_PROJEKTBEZEICHNUNG, text, sizeof(text));
  //strcpy (text, "Projektbezeichnung");
  fprintf (infile, "%s\n", text);
  fprintf (infile, "%s\n", text);
  fprintf (infile, "%s\n", text);
  /* 7., 8. u. 9. Zeilen */
  LoadString(NULL, STR_BLATTBEZEICHNUNG, text, sizeof(text));
  //strcpy (text, "Blattbezeichnung");
  fprintf (infile, "%s\n", text);
  strcpy( text, "Querprofil");
  fprintf (infile, "%s %d\n",text,nr );
  strcpy(text, "Station km");
  fprintf (infile, "%s %8.4lf\n",text, profil->station);//Dick 10.01.00 8.2 -> 8.4
  /* 10.Zeile */
  LoadString(NULL, STR_PROJEKT_NUMMER, text, sizeof(text));
  //strcpy (text, "Projektnummer");
  fprintf (infile, "%s\n", text);
  /* 11. Zeile*/
  LoadString(NULL, STR_DATUM_EINGEBEN, text, sizeof(text));
  //strcpy (text, "Hier Datum eintragen");
  fprintf (infile, "%s\n", text);
  /* 12. Zeile */
  strcpy (text, "B- ");
  fprintf (infile, "%s\n", text);
  /* 13. Zeile */
  LoadString(NULL, STR_ZEICHN_UEBERSCHRIFT, text, sizeof(text));
  //strcpy (text, "Zeichnungsüberschrift");
  strcpy (text, "\0");
  char buf[80];
  LoadString(NULL, STR_GEWPROF_AN_STAT, buf, sizeof(buf));
  sprintf(text,"%s %8.4lf",buf,profil->station);
  //sprintf(text,"Gewaesserquerprofil an Station Km %8.2lf",data[0].station);
  fprintf (infile, "%s\n", text);
  
  return infile;
}
