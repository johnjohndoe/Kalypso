/**********************************************************************

	 MODUL:  Konvertieren von BCE-Profil --> JABRON Format
   
     CLASS   Jabron
     21.07.96 Andresen
     
       
**********************************************************************/
#include <windows.h>
#include <math.h>
#include "xvt.h"

#include "resource.h"
#include "typen.h"

#include "global_types.h"

#include "slist.h"
#include "list.h"

#include "jabron.h"


#define T_LINKS  1
#define T_RECHTS 2

Jabron::Jabron( )
{
  ptr_profil = ptr_ende = ptr_anfang;
  ptr_anfang->ds_nummer=1;
  ptr_anfang->datensatz->ds_nr=1;
}
Jabron::~Jabron(void)
{
}


UINT Jabron::JabWriteJab( FILE* out, const UINT nr, LPCSTR name, PROFILDATA* profilData, double station, double gefaelle, bool bInterpolBruecke )
// schreibt dieses Profil als Jabron Daten raus
// Parameter:
//        FILE* out: die ( zum schreiben geöffnete Ausgabedatei )
//        const UINT nummer: die aktuelle Nummer des Profil
//        LPCSTR name: Name des Profils
//        int* ds_info: das unnütze ds_info!!!
//        double station: aktuelle Station des Profils
//        double gefaelle: Gefaelle
//        bool bInterpolBruecke: falls TRUE, wird das Gelände der Brücke als Zwischenprofil vor und nach der Brücke rausgeschrieben
// Rückgabewert:
//          UINT: Anzahl der geschriebenen Profile ( bei Sonderprofilen mehr als eines )
{
  UINT count = 0; // Anzahl der geschriebenen Profile

  // zuerst den Profiltyp rausfinden und abhängig davon verschiedene Profile rausschreiben
  if( ExistDatensatzTyp( UK_BRUECKE ) )
  {
    // zuerst mal die Breite der Brücke ermitteln
    Profildatei* ptrUK = HoleDatenblock( UK_BRUECKE );
    double uwHoehe;
    double breite;
    double rauheit;
    double beiwert;
    
    LPSTR string = ptrUK->daten_info[1];
    sscanf( string, "%lf %lf %lf %lf", &uwHoehe, &breite, &rauheit, &beiwert );

    double sohltiefe = GetSohltiefe(); // der tiefste Punkt des Profils
    double deltaY = sohltiefe - uwHoehe; // das erste Profil soll auf die uwHoehe gesetzt werden

    // eine Brücke: es werden im Abstand von +/- Brückenbreite das Profil als Gerinne rausgeschrieben und dazwischen
    // ein geschlosseses Profil
    if( bInterpolBruecke )
    {
      JabWriteGerinne( out, nr, name, profilData, station - breite / 1000, gefaelle, deltaY, "Extrapoliertes Brückenprofil" );
      count++;
    }

    JabWriteGeschlossen( out, nr + count, name, profilData, station, gefaelle, breite );
    count++;

    if( bInterpolBruecke )
    {
      JabWriteGerinne( out, nr + count, name, profilData, station + breite / 1000, gefaelle, -deltaY, "Extrapoliertes Brückenprofil" );
      count++;
    };
  }
  else if( ExistDatensatzTyp( KREIS ) )
  {
    const double breite = 2.5; // Länge des Kreis in m

    // Fallunterscheidung: bei Kreisen mit vernünftigen Gelände ( mehr als drei Punkte )
    // wird der Kreis um 2.5m ins Unterwasser versetzt, und das Gelände als Gerinne ausgegeben
    if( profilData->ds_info[1] > 3 )
    {
      JabWriteKreis( out, nr , name, profilData, station - breite / 1000, breite, false );
      JabWriteGerinne( out, nr + 1, name, profilData, station, gefaelle, 0.0, "Extrapoliertes Kreisprofil" );

      count = 2;
    }
    else
    {
      JabWriteKreis( out, nr + count, name, profilData, station, breite, true );
      count = 1;
    }; // if profilData->ds_info[3] > 3
  }
  else
  {
    // ein ganz Normales Profil
    JabWriteGerinne( out, nr, name, profilData, station, gefaelle, 0.0 );

    count = 1;
  };

  return count;
} // JabWriteJab

void Jabron::JabWriteKreis( FILE* out, UINT nr, LPCSTR prof_name, PROFILDATA* profData, double station, double laenge, const bool bKS )
// schreibt ein JabronKreisprofil raus
{
  // ein paar Daten ermitteln
  Profildatei* pKreis = HoleDatenblock( KREIS );
  Koord* krdKreis = pKreis->datensatz;

  if( krdKreis == NULL )
    return;
  double durchm = krdKreis->x;
  krdKreis = krdKreis->next_ds;
  if( krdKreis == NULL )
    return;
  double sohlGef = krdKreis->x / 1000;

  // die erste Rauheit holen
  double rauheit = 50;

  // falls bKS gesetzt, die erste Koordinate des Datenblocks RAUHIGKEIT (KST) nehmen
  if( bKS )
  {
    Koord* rKrd = HoleDatensatz( RAUHIGKEIT );
    if( rKrd == NULL )
      rKrd = HoleDatensatz( RAUHIGKEIT_KST );
    if( rKrd != NULL )
      rauheit = rKrd->y;
  };

  // Strassenhöhe = niedrigste Geländekoordinate
  Koord* gelKrd = HoleDatensatz( GELAENDEHOEHE );
  if( gelKrd == NULL )
    return;
  double strasse = -1e36;
  while( gelKrd != NULL )
  {
    strasse = max( strasse, gelKrd->y );
    gelKrd = gelKrd->next_ds;
  } // while gelKrd

  // den ProfilHeader rauschreiben
  fprintf( out, "$-------------------   neues Profil   --------------------------------\n" );
  fprintf( out, "PROFIL    %i %s\n", nr, prof_name );
  fprintf( out, "KREIS   %8.2lf   %8.2lf   %8.4lf\n", durchm, rauheit, sohlGef );
  fprintf( out, "KILOMETER %8.3lf\n", station );
  fprintf( out, "STRASSENHOEHE %8.2lf\n", strasse );
  fprintf( out, "LAENGE  %8.2lf\n", laenge );

  // auch noch den Kommentar rausschreiben
  JabWriteComment( out, profData );
} // JabWriteKreis

void Jabron::JabWriteGeschlossen( FILE* out, UINT nr, LPCSTR prof_name, PROFILDATA* profData, double station, double gefaelle, double laenge )
// schreibt dieses Profil als geschlossenes Profil raus
{
  Koord* ukKrd = HoleDatensatz( UK_BRUECKE );
  Koord* gelKrd = HoleDatensatz( GELAENDEHOEHE );
  Koord* rKrd = HoleDatensatz( RAUHIGKEIT );
  if( rKrd == NULL )
    rKrd = HoleDatensatz( RAUHIGKEIT_KST );

  if( ukKrd == NULL || gelKrd == NULL || rKrd == NULL )
    return;

  // die Position der ersten BrueckenKoordinate suchen, die mit einer Geländekoordinate übereinstimmt
  Koord* fstUKKrd = NULL;
  Koord* fstGelKrd = NULL;

  Koord* aktGelKrd = gelKrd;
  while( aktGelKrd != NULL && fstUKKrd == NULL )
  {
    Koord* aktUKKrd = ukKrd;
    while( aktUKKrd != NULL )
    {
      if( fabs( aktUKKrd->x - aktGelKrd->x ) < 0.00001 && fabs( aktUKKrd->y - aktGelKrd->y ) < 0.00001 )
      {
        fstUKKrd = aktUKKrd;
        fstGelKrd = aktGelKrd;
        break;
      };

      aktUKKrd = aktUKKrd->next_ds;
    } // while aktUKKrd

    aktGelKrd = aktGelKrd->next_ds;
  } // while aktGelKrd

  if( fstUKKrd == NULL || fstGelKrd == NULL )
    return;

  // sind mehrere hintereinander gleich, alle überspringen
  while( fstUKKrd->next_ds != NULL && fstGelKrd->next_ds != NULL )
  {
    if( fabs( fstUKKrd->next_ds->x - fstGelKrd->next_ds->x ) < 0.00001 && 
      fabs( fstUKKrd->next_ds->y - fstGelKrd->next_ds->y ) < 0.00001 )
    {
      fstUKKrd = fstUKKrd->next_ds;
      fstGelKrd = fstGelKrd->next_ds;
    }
    else
      break;
  }

  // die Position der nächsten Brückenkoordinate suchen, welche mit dem Gelände übereinstimmt
  Koord* lstUKKrd = NULL;
  Koord* lstGelKrd = NULL;

  aktGelKrd = fstGelKrd->next_ds;
  while( aktGelKrd != NULL && lstUKKrd == NULL )
  {
    Koord* aktUKKrd = fstUKKrd->next_ds;
    while( aktUKKrd != NULL )
    {
      if( fabs( aktUKKrd->x - aktGelKrd->x ) < 0.00001 && fabs( aktUKKrd->y - aktGelKrd->y ) < 0.00001 )
      {
        lstUKKrd = aktUKKrd;
        lstGelKrd = aktGelKrd;
        break;
      };

      aktUKKrd = aktUKKrd->next_ds;
    } // while aktUKKrd

    aktGelKrd = aktGelKrd->next_ds;
  } // while aktGelKrd

  if( lstUKKrd == NULL || lstGelKrd == NULL )
    return;


  int count = lstGelKrd->ds_nr - fstGelKrd->ds_nr;
  count += lstUKKrd->ds_nr - fstUKKrd->ds_nr;

  // jetzt diese Koordinaten auslesen
  double* xKrds = new double[count]; // die y-Koordinaten 
  double* yKrds = new double[count]; // die x-Koordinaten
  double* rKrds = new double[count]; // die Rauheiten

  // zur richtigen Rauheitskoordinate vorrücken
  Koord* aktRKrd = rKrd;
  while( aktRKrd != NULL && aktRKrd->ds_nr < fstGelKrd->ds_nr )
    aktRKrd = aktRKrd->next_ds;

  int i = 0;
  Koord* aktKrd = NULL;
  for( aktKrd = fstGelKrd; aktKrd != lstGelKrd; aktKrd = aktKrd->next_ds )
  {
    xKrds[i] = aktKrd->x;
    yKrds[i] = aktKrd->y;
    
    
    if( aktRKrd != NULL )
    {
      rKrds[i] = aktRKrd->y;
      aktRKrd = aktRKrd->next_ds;
    }
    else
      rKrds[i] = 60;

    i++;
  } // for i

  for( aktKrd = lstUKKrd; aktKrd != fstUKKrd; aktKrd = aktKrd->pre_ds )
  {
    xKrds[i] = aktKrd->x;
    yKrds[i] = aktKrd->y;
    rKrds[i] = 60; // Standardwert Beton für die Brücke selbst
    i++;
  }

  // die Koordinaten so umsortieren, dass der FirstPunkt ganz vorne ist
  int maxPos = 0;
  double maxY = -1e36;
  for( i = 0; i < count; i++ )
  {
    if( yKrds[i] > maxY )
      maxPos = i;
  }; // for i

  double* newXKrds = new double[count];
  double* newYKrds = new double[count];
  double* newRKrds = new double[count];
  
  int cnt = 0;
  for( i = maxPos; i < count; i++ )
  {
    newXKrds[cnt] = xKrds[i];
    newYKrds[cnt] = yKrds[i];
    newRKrds[cnt] = rKrds[i];

    cnt++;
  }; // for i
  for( i = 0; i < maxPos; i++ )
  {
    newXKrds[cnt] = xKrds[i];
    newYKrds[cnt] = yKrds[i];
    newRKrds[cnt] = rKrds[i];

    cnt++;
  }; // for i

  delete[] xKrds;
  delete[] yKrds;
  delete[] rKrds;

  xKrds = newXKrds;
  yKrds = newYKrds;
  rKrds = newRKrds;

  // jetzt haben wir alle Daten

  // zuerst den ProfilHeader rauschreiben
  fprintf( out, "$-------------------   neues Profil   --------------------------------\n" );
  fprintf( out, "PROFIL    %i %s\n", nr, prof_name );
  fprintf( out, "GESCHLOSSEN   %i\n", count );  //Anzahl der Datensätze von GELAENDE
  fprintf( out, "KILOMETER %8.3lf\n", station );
  fprintf( out, "$ Gefaelle für Wsp-Berechnung auf Standardwert gesetzt.\n" );
  fprintf( out, "GEFAELLE %8.3lf\n", gefaelle );
  fprintf( out, "$\n" );

  // und jetzt die Koordinaten
  fprintf( out, "X-KOO " );
  for( i = 0; i < count; i++ )
  {
    fprintf( out, "%8.2lf", xKrds[i] ); // max. 8 char
    if( ( i + 1 ) % 8 == 0 )
      fprintf( out, "\n" );
  } // for i
  if( i % 8 != 0 )
    fprintf( out, "\n" );

  fprintf( out, "Y-KOO " );
  for( i = 0; i < count; i++ )
  {
    fprintf( out, "%8.2lf", yKrds[i] ); // max. 8 char
    if( ( i + 1)  % 8 == 0 )
      fprintf( out, "\n" );
  } // for i
  if( i % 8 != 0 )
    fprintf( out, "\n" );

  fprintf( out, "KS " );
  for( i = 0; i < count - 1; i++ )
  {
    fprintf( out, "%8.2lf", rKrds[i] ); // max. 8 char
    if( ( i + 1)  % 8 == 0 )
      fprintf( out, "\n" );
  } // for i
  if( i % 8 != 0 )
    fprintf( out, "\n" );

   
  // die Laenge rausschreiben
  fprintf( out, "LAENGE  %8.2lf\n", laenge );

  // die Strassenhöhe rausschreiben
  // dazu den niedrigsten Punkt der DKOK nehmen ( ausser ersten / letzten Punkt )
  double strasse = 1e36;
  Profildatei* pOK = HoleDatenblock( OK_BRUECKE );
  if( pOK == NULL )
    return;

  Koord* okKrd = pOK->datensatz;
  if( okKrd == NULL )
    return;

  if( profData->ds_info[pOK->ds_nummer] < 3 )
    strasse = okKrd->y;
  else
  {
    okKrd = okKrd->next_ds; // den ersten überspringen
    while( okKrd != NULL && okKrd->next_ds != NULL )
    {
      if( strasse > okKrd->y )
        strasse = okKrd->y;
      okKrd = okKrd->next_ds;
    } // while okKrd
  } // if ds_info
  fprintf( out, "STRASSENHOEHE %8.2lf\n", strasse );

  delete[] xKrds;
  delete[] yKrds;

  // auch noch einen Kommentar
  JabWriteComment( out, profData );
}; // JabWriteGeschlossen

void Jabron::JabWriteGerinne( FILE* out, UINT nr, LPCSTR prof_name, PROFILDATA* profData, double station, double gefaelle, double deltaY, LPCSTR commentStr /* = NULL */ )
// schreibt dieses Profil als GERINNE raus
// Parameter:
//        double deltaY: um diesen Betrag werden alle YKoordinaten abgesenkt
{
  // zuerst den ProfilHeader rausschreiben
  fprintf( out, "$-------------------   neues Profil   --------------------------------\n" );
  fprintf( out, "PROFIL    %i %s\n", nr, prof_name );
  fprintf( out, "GERINNE   %i\n", profData->ds_info[1] );  //Anzahl der Datensätze von GELAENDE
  fprintf( out, "KILOMETER %8.3lf\n", station );
  fprintf( out, "$ Gefaelle für Wsp-Berechnung auf Standardwert gesetzt.\n" );
  fprintf( out, "GEFAELLE %8.3lf\n", gefaelle );
  fprintf( out, "$\n" );

  // jetzt die Datenblöcke in Jabron Daten konvertieren
  int ds_nr = 1;
  Koord *help;
  ptr_profil = ptr_anfang;
  
  while (ptr_profil !=NULL)
  {
    help = ptr_profil->datensatz;
    switch (ptr_profil->profiltyp)
    {
    case GELAENDEHOEHE:
      JabWriteXY( out, help, deltaY );
      break;
    case TRENNFLAECHEN:
      JabWriteTrennfl(out,ptr_anfang,ptr_profil->ds_nummer);
      break;
    case RAUHIGKEIT:
    case RAUHIGKEIT_KST:
        JabWriteKS( out, help, profData->ds_info[1] - 1 );
      break;
    case DURCHST_BEREICH:
      JabWriteDurchstBereiche( out, ptr_anfang->datensatz, help );
      break;

    case BORDVOLL:
      JabWriteBordvoll( out, ptr_anfang->datensatz, help );
      break;

    case COMMENT:
    case OK_GELAENDE:
    case UK_BRUECKE:
    case OK_BRUECKE:
    case AXM:
    case AYM:
    case DPM:
    case OK_WEHRS:
    case KREISSEGM:
    case SVA_WERT:
    case KASTEN:
    case LWA_FELDER:
    case GAUSSRUECK:
    case GAUSS:
    case PUNKT_NR:
    case STATION:
    case WASSERSP1:
    case WASSERSP100:
    case WASSERSP5:
    case MODELLGRENZEN:
    case TRENN_WEHR:
    case MAUL:
    case EIPROFIL:
    case KREIS:
    case ARMCO84:
    case ARMCO71:
    case NWRINNE:
    case FUELLHOEHE:
    case NAU_MED:
    case TRAPEZ:
    case GELAENDE2:
    case FLAECHE:
    case UNKNOWN:
      break;

    default:
      break;

    }; //-switch
    ds_nr++;
    ptr_profil = ptr_profil->next;
  }


  // auch noch den Bewuchs ausgeben
  JabWriteBewuchs( out, profData->ds_info );

  // und zuletzt noch einen Kommentar
  JabWriteComment( out, profData, commentStr );
}

void Jabron::JabWriteComment( FILE* out, PROFILDATA* profilData, LPCSTR commentStr /* = NULL */ )
// Schreibt einen evtl. vorhandenen Kommentar, sowie einen zusätzlichen Text raus
// Parameter:
//        FILE* out: ( zum Schreiben geöffnete ) Ausgabedatei
//        PROFILDATA* profData: die blöden Zusatzdaten dieses Profils
//        LPCSTR commentStr = NULL: falls ungleich NULL die zweite Kommentarzeile; ansonsten wird noch der Komment-
//              tardatenblock angehängt, falls vorhanden
{
  int lineCount = 0;

  // Anzahl der Kommentarzeilen
  if( profilData->exist_comment )
    lineCount += WspSListCountElem( profilData->slist_comment );
  
  if( commentStr != NULL )
    lineCount++;

  if( lineCount == 0 )
    return;

  // jetzt den Kommentar rrausschreiben
  fprintf( out, "TEXT %d\n", lineCount );

  if( commentStr != NULL )
    fprintf( out, "%s\n", commentStr );
  
  if( profilData->exist_comment )
  {
    WSP_SLIST* sList = profilData->slist_comment;
    while( sList  )
    {
      fprintf( out, "%s\n", sList->string );
      sList = sList->next;
    }; // while sList
  }; // if exist_comment
  
  fprintf( out, "\n" ); // nach dem TEXT muss immer eine Leerzeile folgen, sonst absturz bei Jabron
} // JabWriteText

void Jabron::JabWriteBewuchs( FILE* out, int* ds_info )
{
  int dsAX = ExistDatensatzTyp( AXM );
  int dsAY = ExistDatensatzTyp( AYM );
  int dsDP = ExistDatensatzTyp( DPM );
  int dsGel = ExistDatensatzTyp( GELAENDEHOEHE );
  int dsTrenn = ExistDatensatzTyp( TRENNFLAECHEN );

  // die Positionen der Trennflächen rausfinden: innerhalb der TF darf kein Bewuchs sein
  Koord* krdTrenn = HoleDatensatz( TRENNFLAECHEN );
  Koord* krdGel = HoleDatensatz( GELAENDEHOEHE );
  if( krdTrenn == NULL || krdGel == NULL )
    return;

  int left = 0, 
    right = 0; // linke und rechte Position
  
  while( krdGel != NULL )
  {
    if( fabs( krdGel->x - krdTrenn->x ) < 0.00001 && left == 0 )
      left = krdGel->ds_nr;
    if( fabs( krdGel->x - krdTrenn->next_ds->x ) < 0.00001 && right == 0 )
      right = krdGel->ds_nr;
    krdGel = krdGel ->next_ds;
  } // while krdGel

  if( left == 0 || right == 0 )
    return; // wenn eine der Trennflaechen nicht gefunden wurde, raus


  // nur wenn alle Datensaetze existieren und die gleiche Anzahl an Koordinaten haben rausschreiben
  if( dsGel > 0 && dsAX > 0 && dsAY && dsDP > 0 && 
    ds_info[dsGel] == ds_info[dsAX] &&
    ds_info[dsGel] == ds_info[dsAY] &&
    ds_info[dsGel] == ds_info[dsDP] )
  {
    Koord* krdAX = HoleDatensatz( AXM );
    Koord* krdAY = HoleDatensatz( AYM );
    Koord* krdDP = HoleDatensatz( DPM );

    // für alle bis auf den letzten die Parameter ausgeben
    int count = 1;
    while( krdAX != NULL && krdAY != NULL && krdDP != NULL && krdAX->next_ds != NULL )
    {
      double ax = krdAX->y;
      double ay = krdAY->y;
      double dp = krdDP->y;

      // nur wenn alle drei ungleich null
      if( ax != 0.0 && ay != 0.0 && dp != 0.0 && ( count < left || count >= right ) )
        fprintf( out, "DVWK-BEWUCHS-ZONE %d %d %8.3lf %8.3lf %8.3lf \n", count, count + 1, ax, ay, dp );

      krdAX = krdAX->next_ds;
      krdAY = krdAY->next_ds;
      krdDP = krdDP->next_ds;

      count++;
    } // while krdAX != NULL ...
  }
} // JabWriteBewuchs


/*************************************************************************/
void Jabron::JabWriteXY( FILE* out, Koord* profil, double deltaY )
// Parameter:
//        double deltaY: um diesen Betrag werden alle YKoordinaten abgesenkt
{
  int k;
  Koord *pp;
  
  /* die maximale Zeilenlänge für JABRON darf 80 Zeichen/Zeile nicht überschreiten,
	 mit "x-KOO "= 6 + 8*8 = 70 Zeichen klappt dies */
  fprintf(out,"X-KOO ");
  pp=profil;
  k=0;
  
  while (pp !=NULL)
  {
    if ((pp->x != BCE_NAN) && (pp->y != BCE_NAN))
    {
      k++;
      fprintf(out,"%8.2lf", pp->x); // max. 8 char
      pp = pp->next_ds;
      if ((k % 8)==0)
        fprintf(out,"\n");
    }
    else pp = pp->next_ds;
  }
  if ((k % 8)!=0)
    fprintf(out,"\n");
  fprintf(out,"Y-KOO ");
  pp=profil;
  k=0;
  while (pp != NULL)
  {
    if ( (pp->x !=BCE_NAN) && (pp->y != BCE_NAN))
				{
      k++;
      fprintf( out,"%8.2lf", pp->y - deltaY ); //max. 8 char
      pp = pp->next_ds;
      if ((k % 8)==0)
        fprintf(out,"\n");
				}
    else pp = pp->next_ds;
  }
  if ((k % 8)!=0)
    fprintf(out,"\n");
}
/**************************************************************************/
void Jabron::JabWriteKS( FILE*out, Koord* profil, const int count )
// Schreibt einen Datenblock als 'KS' raus
// Parameter:
//        FILE* out: die ( zum schreiben geöffnete ) Ausgabedatei
//        Koord* profil: die y-Werte dieses Datensatzes werden rausgeschrieben
//        const int count: genau soviele koordinaten werden rausgeschrieben; ist count
//                  kleiner als die anzahl der Koordinaten von profil, werden nur die ersten 
//                  count geschrieben
//                  ist count grösser als die Anzahl der Koordinaten, wird die letzte so oft
//                  wiederholt, bis count erreicht ist
{
  /* die maximale Zeilenlänge für JABRON darf 80 Zeichen/Zeile nicht überschreiten,
	 mit "x-KOO "= 6 + 8*8 = 70 Zeichen klappt dies */
  fprintf(out,"KS    ");
  
  Koord* pp = profil;
  double rauh = 0.0;

  for( int i = 0; i < count; i++ )
  {
    if( pp != NULL )
    {
      if( pp->x != BCE_NAN && pp->y != BCE_NAN )
        rauh = pp->y;
      pp = pp->next_ds;
    } // if pp != NULL

    fprintf( out, "%8.2lf", rauh ); // max. 8 char
    if( ( ( i + 1 ) % 8 ) == 0 )
      fprintf( out,"\n" );
  } // for i
  
  // wenn zuletzt kein Zeilenumbruch geschrieben wurde, dies jetzt tun
  if( ( i % 8 ) != 0 )
    fprintf(out,"\n");
}
/**************************************************************************/
void Jabron::JabWriteTrennfl(FILE *out,Profildatei *ptr_profil,int ds_nr)
{
  Profildatei *ptr_tmp;
  Koord *trennfl,*help;
  int left=0,right=0;
  
  ptr_tmp = ptr_profil;
  help = ptr_profil->datensatz;
  
  while ((ptr_tmp !=NULL)&&(ptr_tmp->ds_nummer<ds_nr))
    ptr_tmp = ptr_tmp->next;
  //ptr_tmp zeigt auf Trennfläche
  trennfl = ptr_tmp->datensatz;
  
  while (help!=NULL)
  {
    if ((help->x == trennfl->x)&&(left == 0))
      left = help->ds_nr;
    if (help->x == trennfl->next_ds->x)
      right = help->ds_nr;
    help = help->next_ds;
  }
  
  fprintf(out,"LINKS  %i\n",left);
  // fprintf(out,"SOHLE  %i %i\n",left,right);
  fprintf(out,"RECHTS %i\n",right);
  
}
/**************************************************************************/
void Jabron::JabWriteDurchstBereiche( FILE *out, Koord* gelKoord, Koord* durchstKoord )
// erzeugt Daten aus den durchst. Bereichen
// Parameter:
//      FILE* out: Ausgabedatei ( muss zum schreiben geöffnet sein )
//      Koord* gelKoord: zeigt auf die Geländekoordinaten
//      Koord* durchstKoord: zeigt auf die Koordinaten der durchst. Bereiche
{
  // hier werden die Schlüssenworte NICHT-ABFLUSSWIRKSAM rausgeschrieben
  // und zwar 1. Profilpunkt bis linke Begrenzung
  //          rechte Begrenzung bis letzter Profilpunkt


  // zuerst die Nummern der Profilpunkte rausfinden, an denen die durchst. Bereiche sitzen
  int left = 0, 
    right = 0,
    last = 0;

  Koord* leftDB = durchstKoord;
  if( leftDB == NULL )
    return;

  Koord* rightDB = durchstKoord->next_ds;
  if( rightDB == NULL )
    return;
  
  Koord* krd = gelKoord;
  while( krd != NULL )
  {
    last = krd->ds_nr;

    if( krd->x == leftDB->x && left == 0 )
      left = krd->ds_nr;
    if( krd->x == rightDB->x )
      right = krd->ds_nr;
    krd = krd->next_ds;
  }

  if( left == 0 || right == 0 || last == 0 )
    return;

  // nur wenn ein ganzer Bereich nicht abflusswirksam ist, diesen auch rausschreiben
  if( left > 1 )
    fprintf( out, "NICHT-ABFLUSSWIRKSAM %d %d\n", 1, left );
  if( right < last )
    fprintf( out, "NICHT-ABFLUSSWIRKSAM %d %d\n", right, last );
}

void Jabron::JabWriteBordvoll( FILE* out, Koord* gelKoord, Koord* bordKoord )
// schreibt den Datensatz BORDVOLL
// Parameter:
//      FILE* out: Ausgabedatei, muss zum schreiben geöffnet sein
//      Koord* bordKoord: zeigt auf die Koordinaten des Datensatzes BORDVOLL
{
  if( out == NULL )
    return;

  // linker Bordvollhöhe
  Koord* krd = gelKoord;
  Koord* brdKrd = bordKoord;

  for( int i = 0; i < 2; i++ )
  {
    if( brdKrd == NULL )
      return;

    while( krd != NULL )
    {
      if( fabs( krd->x - brdKrd->x ) < 0.00001 )
      {
        if( i == 0 )
          fprintf( out, "L-GELAENDE %8.2lf\n", krd->y );
        else
          fprintf( out, "R-GELAENDE %8.2lf\n", krd->y );
      break;
      }; // if i == 0
      krd = krd->next_ds;
    }; // while krd

    brdKrd = brdKrd->next_ds;
  }; // for i
}; // JabWriteBordvoll

/**************************************************************************/


/*************************************************************************
Jabron ---> BCE

**************************************************************************/
int Jabron::WriteJabXKoord(char *string,int ds)
{
  char *ptr,
    str[15];
  int bruecke=FALSE,
    i=0;
  double wert,
		  max=-MAXDOUBLE;
  
  ptr = string;
  ptr_profil = ptr_anfang;
  while ((ptr_profil !=NULL)&&(ptr_profil->ds_nummer < ds))
    ptr_profil = ptr_profil->next;
  ptr_profil->profiltyp = GELAENDEHOEHE;
  pp = ptr_profil->datensatz;
  
  if (ptr_profil->status == 1 ) //Daten anhängen (status wird mit -1 initialisiert)
  {
    while ((pp!=NULL)&&(pp->attr >=1))
      pp = pp->next_ds;
  }
  
  if (ptr_profil->status == 3 ) //Daten anhängen (status wird mit -1 initialisiert)
  {
    while ((pp!=NULL)&&(pp->attr ==3))
      pp = pp->next_ds;
  }
  
  if (ptr_profil->status == 2 ) //Daten anhängen (status wird mit -1 initialisiert)
    ptr_profil->status = 3;
  
  if (ptr_profil->status == -1 )	  ptr_profil->status = 1;
  
  
  while (((ptr[0]==' ')||(ptr[0]=='\t'))&&(ptr[0]!='\0')) // erste 'blanks' überlesen
    ptr++;
  while ((ptr[0]!='\n')&&(ptr[0]!='\0'))
  {
    i=0;
    while ( (ptr[0]!=' ')&&(ptr[0]!='\t')&&(ptr[0]!='\n')&&(ptr[0]!='\0'))
    {
      str[i++]=ptr[0];
      ptr++;
    }
    while (((ptr[0]==' ')||(ptr[0]=='\t'))&&(ptr[0]!='\0')) // evt 'blanks' überlesen
      ptr++;
    str[i]='\0';
    wert = atof(str);
    if (wert>=max)
      max = wert;
    else
      bruecke=TRUE;
    if (!pp)
      NewKoord(1);
    if (pp)
    {
      pp->x = wert;
      pp->pre_x=0;
      pp->attr = pp->attr+1;
      pp = pp->next_ds;
    }
    
  }
  return bruecke;
}
/**************************************************************************/
int Jabron::WriteJabYKoord(char *string,int ds)
{
  char *ptr,
    str[15];
  int i=0;
  double wert;
  
  ptr = string;
  ptr_profil = ptr_anfang;
  while ((ptr_profil !=NULL)&&(ptr_profil->ds_nummer < ds))
    ptr_profil = ptr_profil->next;
  ptr_profil->profiltyp = GELAENDEHOEHE;
  pp = ptr_profil->datensatz;
  
  
  
  if (ptr_profil->status == 2 ) //Daten anhängen (status wird mit -1 initialisiert)
  {
    while ((pp!=NULL)&&(pp->attr >=2))
      pp = pp->next_ds;
  }
  
  if (ptr_profil->status == 3 ) //Daten anhängen (status wird mit -1 initialisiert)
  {
    while ((pp!=NULL)&&(pp->attr ==3))
      pp = pp->next_ds;
  }
  if (ptr_profil->status == 1 )
    ptr_profil->status = 3;
  
  if (ptr_profil->status == -1 )	  ptr_profil->status = 2;
  
  
  while (((ptr[0]==' ')||(ptr[0]=='\t'))&&(ptr[0]!='\0')) // erste 'blanks' überlesen
    ptr++;
  while ((ptr[0]!='\n')&&(ptr[0]!='\0'))
  {
    i=0;
    while ((ptr[0]!=' ')&&(ptr[0]!='\t')&&(ptr[0]!='\n')&&(ptr[0]!='\0'))
    {
      str[i++]=ptr[0];
      ptr++;
    }
    while (((ptr[0]==' ')||(ptr[0]=='\t'))&&(ptr[0]!='\0')) // evt 'blanks' überlesen
      ptr++;
    str[i]='\0';
    wert = atof(str);
    if (!pp)
      NewKoord(1);
    if (pp)
    {
      pp->y = wert;
      pp->pre_y=0;
      pp->attr = pp->attr+2;
      pp = pp->next_ds;
    }
  }
  return 1;
}
/*******************************************************************************/
int Jabron::WriteTrennflaechen(char *string,int ds,int typ)
{
  Profildatei *ptr_gel;
  Koord *gel;
  char *ptr,
    str[15];
  int x,i=0;
  double wert;
  
  ptr_gel=ptr_anfang;
  while ((ptr_gel !=NULL)&&(ptr_gel->profiltyp != GELAENDEHOEHE))
    ptr_gel = ptr_gel->next;
  if ((ptr_gel !=NULL)&&(ptr_gel->profiltyp == GELAENDEHOEHE))
    gel = ptr_gel->datensatz;
  else
  {
    char buf[200];
    xvt_res_get_str(STR_JABRON_DLL_NOTE_2,buf,sizeof(buf));
    xvt_dm_post_note("%s",buf);
    //xvt_dm_post_note("Zum Konvertieren von Trennflächen ins WSP-Format muß zuerst eine Geländehoehe existieren.");
    return 0;
  }
  
  ptr_profil = ptr_anfang;
  while ((ptr_profil !=NULL)&&(ptr_profil->ds_nummer < ds))
    ptr_profil = ptr_profil->next;
  ptr_profil->profiltyp = TRENNFLAECHEN;
  pp = ptr_profil->datensatz;
  
  ptr = string;
  
  
  while (((ptr[0]==' ')||(ptr[0]=='\t'))&&(ptr[0]!='\0')) // erste 'blanks' überlesen
    ptr++;
  while ((ptr[0]!=' ')&&(ptr[0]!='\t')&&(ptr[0]!='\n')&&(ptr[0]!='\0'))
		{
    str[i++]=ptr[0];
    ptr++;
		}
  str[i]='\0';
  x = atoi(str);
  i=1;
  while ((gel)&&(i<x))
  {
    gel = gel->next_ds;
    i++;
  }
  if(gel)
    wert = gel->x;
  
  if ((pp!=NULL)&&(typ ==T_LINKS))
  {
    pp->x = wert;
    pp->y = 1.0;
  }
  else     // RECHTS
  {
    pp=pp->next_ds;
    if ((pp!=NULL)&&(typ ==T_RECHTS))
    {
      pp->x = wert;
      pp->y = 2.0;
    }
  }
  return 1;
}
/*******************************************************************************/
int Jabron::WriteDurchstBer(char *string,int ds,int typ)
{
  Profildatei *ptr_gel;
  Koord *gel;
  char *ptr,
    str[15];
  int i=0;
  double wert;
  
  ptr_gel=ptr_anfang;
  while ((ptr_gel !=NULL)&&(ptr_gel->profiltyp != GELAENDEHOEHE))
    ptr_gel = ptr_gel->next;
  if ((ptr_gel !=NULL)&&(ptr_gel->profiltyp == GELAENDEHOEHE))
    gel = ptr_gel->datensatz;
  else
  {
    char buf[200];
    xvt_res_get_str(STR_JABRON_DLL_NOTE_3,buf,sizeof(buf));
    xvt_dm_post_note("%s",buf);
    
    //	 xvt_dm_post_note("Zum Konvertieren von durchst.Bereichen ins WSP-Format muß zuerst eine Geländehoehe existieren.");
    return 0;
  }
  
  ptr_profil = ptr_anfang;
  while ((ptr_profil !=NULL)&&(ptr_profil->ds_nummer < ds))
    ptr_profil = ptr_profil->next;
  ptr_profil->profiltyp = DURCHST_BEREICH;
  pp = ptr_profil->datensatz;
  
  ptr = string;
  
  
  while (((ptr[0]==' ')||(ptr[0]=='\t'))&&(ptr[0]!='\0')) // erste 'blanks' überlesen
    ptr++;
  while ((ptr[0]!=' ')&&(ptr[0]!='\t')&&(ptr[0]!='\n')&&(ptr[0]!='\0'))
		{
    str[i++]=ptr[0];
    ptr++;
		}
  str[i]='\0';
  wert = atof(str);
  
  while ((gel)&&(gel->y !=wert))
    gel = gel->next_ds;
  
  if (gel!=NULL)
  {
    if (gel->y == wert)
    {
      if ((pp!=NULL)&&(typ ==T_LINKS))
      {
        pp->x = gel->x;
        pp->y = wert;
      }
      else if(pp!=NULL)     // RECHTS
      {
        if(pp->x==gel->x)
        {
          gel = gel->next_ds;
          if(gel!=NULL)
            while ((gel->next_ds)&&(gel->y !=wert))
              gel = gel->next_ds;
        }
        pp=pp->next_ds;
        if(gel->next_ds!=NULL || (gel->next_ds==NULL && gel->y ==wert))
        {
          if ((pp!=NULL)&&(typ ==T_RECHTS))
          {
            pp->x = gel->x;
            pp->y = wert;
          }
        }
        else
        {
          char buf[200];
          xvt_res_get_str(STR_JABRON_DLL_NOTE_4,buf,sizeof(buf));
          xvt_dm_post_note("%s",buf);
          //xvt_dm_post_note("Es wurde kein passender y-Wert zu duchst.Bereichen gefunden");
          if ((pp!=NULL)&&(typ ==T_RECHTS))
          {
            pp->x = gel->x;
            pp->y = wert;
          }
        }
      }
    }
    else
    {
      if ((pp!=NULL)&&(typ ==T_LINKS))
      {
        pp->x = ptr_anfang->datensatz->x;	 pp->y = ptr_anfang->datensatz->y;
      }
      if ((pp->next_ds!=NULL)&&(typ ==T_RECHTS))
      {
        gel = ptr_gel->datensatz;
        while (gel->next_ds!=NULL)
          gel = gel->next_ds;
        pp->next_ds->x = gel->x;	 pp->next_ds->y = gel->y;
      }
      char buf[200];
      xvt_res_get_str(STR_JABRON_DLL_NOTE_4,buf,sizeof(buf));
      xvt_dm_post_note("%s",buf);
      
      //xvt_dm_post_note("Es wurde kein passender y-Wert zu duchst.Bereichen gefunden");
    }
  }
  else
  {
    if ((pp!=NULL)&&(typ ==T_LINKS))
    {
      pp->x = ptr_anfang->datensatz->x;	 pp->y = ptr_anfang->datensatz->y;
    }
    if ((pp->next_ds!=NULL)&&(typ ==T_RECHTS))
    {
      gel = ptr_gel->datensatz;
      while (gel->next_ds!=NULL)
        gel = gel->next_ds;
      pp->next_ds->x = gel->x;	 pp->next_ds->y = gel->y;
    }
    char buf[200];
    xvt_res_get_str(STR_JABRON_DLL_NOTE_4,buf,sizeof(buf));
    xvt_dm_post_note("%s",buf);
    
    //xvt_dm_post_note("Es wurde kein passender y-Wert zu duchst.Bereichen gefunden");
  }
  return 1;
  
}
/*******************************************************************************/
int Jabron::WriteJabKS(char *string,int ds,int erst)
{
  char *ptr,
    str[15];
  int anzahl=0,
    i=0;
  double wert;
  
  ptr = string;
  
  ptr_profil = ptr_anfang;/*Anzahl der Tupel in GELAENDE ermitteln*/
  while ((ptr_profil !=NULL)&&(ptr_profil->profiltyp != GELAENDEHOEHE))
    ptr_profil = ptr_profil->next;
  if ((ptr_profil !=NULL)&&(ptr_profil->profiltyp == GELAENDEHOEHE))
  {
    pp = ptr_profil->datensatz;
    while (pp)
    {
      anzahl++;
      pp=pp->next_ds;
    }
  }
  else anzahl =0;
  
  
  ptr_profil = ptr_anfang;
  while ((ptr_profil !=NULL)&&(ptr_profil->ds_nummer < ds))
    ptr_profil = ptr_profil->next;
  ptr_profil->profiltyp = RAUHIGKEIT;
  pp = ptr_profil->datensatz;
  
  if (ptr_profil->status == 1 ) //Daten anhängen (status wird mit -1 initialisiert)
  {
    while ((pp!=NULL)&&(pp->status==1))
      pp = pp->next_ds;
  }
  else ptr_profil->status = 1 ;
  
  
  p_t=pp;
  if(erst)
  {
    if (anzahl)
    {
      //MakeNewKoord(anzahl-1);
      MakeNewKoord(anzahl);//Dick
      CopyStation(ds);
    }
    pp = ptr_profil->datensatz;
  }
  
  while (((ptr[0]==' ')||(ptr[0]=='\t'))&&(ptr[0]!='\0')) // erste 'blanks' überlesen
    ptr++;
  while ((ptr[0]!='\n')&&(ptr[0]!='\0'))
  {
    i=0;
    while ((ptr[0]!=' ')&&(ptr[0]!='\t')&&(ptr[0]!='\n')&&(ptr[0]!='\0'))
    {
      str[i++]=ptr[0];
      ptr++;
    }
    while (((ptr[0]==' ')||(ptr[0]=='\t'))&&(ptr[0]!='\0')) // evt 'blanks' überlesen
      ptr++;
    str[i]='\0';
    wert = atof(str);
    
    if (pp)
    {
      pp->y = wert;
      pp->pre_y=0;
      pp->status =1;
      pp = pp->next_ds;
    }
  }
  if(pp!=NULL)//Dick 10.06.99
    if (pp->next_ds == NULL)//Letzte Element => dann Rauhigkeit vom vorletzten Element
    {
      pp->y = wert;
      pp->pre_y=0;
      pp->status =1;
    }
    return 1;
}
/*******************************************************************************/
int Jabron::WriteJabKSG(char *string,int ds)
{
  char *ptr,
    str[15];
  int anzahl=0,
    i=0;
  double wert;
  
  ptr = string;
  
  ptr_profil = ptr_anfang;/*Anzahl der Tupel in GELAENDE ermitteln*/
  while ((ptr_profil !=NULL)&&(ptr_profil->profiltyp != GELAENDEHOEHE))
    ptr_profil = ptr_profil->next;
  if ((ptr_profil !=NULL)&&(ptr_profil->profiltyp == GELAENDEHOEHE))
  {
    pp = ptr_profil->datensatz;
    while (pp)
    {
      anzahl++;
      pp=pp->next_ds;
    }
  }
  else anzahl =0;
  
  
  ptr_profil = ptr_anfang;
  while ((ptr_profil !=NULL)&&(ptr_profil->ds_nummer < ds))
    ptr_profil = ptr_profil->next;
  ptr_profil->profiltyp = RAUHIGKEIT;
  pp = ptr_profil->datensatz;
  p_t=pp;
  if (anzahl)
  {
    MakeNewKoord(anzahl-1);
    CopyStation(ds);
  }
  pp = ptr_profil->datensatz;
  
  while (((ptr[0]==' ')||(ptr[0]=='\t'))&&(ptr[0]!='\0')) // erste 'blanks' überlesen
    ptr++;
  while ((ptr[0]!='\n')&&(ptr[0]!='\0'))
  {
    i=0;
    while ((ptr[0]!=' ')&&(ptr[0]!='\t')&&(ptr[0]!='\n')&&(ptr[0]!='\0'))
    {
      str[i++]=ptr[0];
      ptr++;
    }
    while (((ptr[0]==' ')||(ptr[0]=='\t'))&&(ptr[0]!='\0')) // evt 'blanks' überlesen
      ptr++;
    str[i]='\0';
    wert = atof(str);
    
    while (pp)
    {
      pp->y = wert;
      pp->pre_y=0;
      pp = pp->next_ds;
    }
    break;
  }
  return 1;
}
/*******************************************************************************/
int Jabron::WriteJabKSL(char *string,int ds)
{
  char *ptr,
    str[15];
  int  i=0;
  double wert;
  double trL;
  
  ptr = string;
  
  /*linke Trennfläche suchen*/
  ptr_profil = ptr_anfang;
  while ((ptr_profil !=NULL)&&(ptr_profil->profiltyp != TRENNFLAECHEN))
    ptr_profil = ptr_profil->next;
  if ((ptr_profil !=NULL)&&(ptr_profil->profiltyp == TRENNFLAECHEN))
    trL = ptr_profil->datensatz->x;
  else trL=BCE_NAN;
  
  ptr_profil = ptr_anfang;
  while ((ptr_profil !=NULL)&&(ptr_profil->ds_nummer < ds))
    ptr_profil = ptr_profil->next;
  ptr_profil->profiltyp = RAUHIGKEIT;
  pp = ptr_profil->datensatz;
  
  while (((ptr[0]==' ')||(ptr[0]=='\t'))&&(ptr[0]!='\0')) // erste 'blanks' überlesen
    ptr++;
  while ((ptr[0]!='\n')&&(ptr[0]!='\0'))
  {
    i=0;
    while ((ptr[0]!=' ')&&(ptr[0]!='\t')&&(ptr[0]!='\n')&&(ptr[0]!='\0'))
    {
      str[i++]=ptr[0];
      ptr++;
    }
    while (((ptr[0]==' ')||(ptr[0]=='\t'))&&(ptr[0]!='\0')) // evt 'blanks' überlesen
      ptr++;
    str[i]='\0';
    wert = atof(str);
    
    while ((pp)&&(pp->x<=trL))
    {
      pp->y = wert;
      pp->pre_y=0;
      pp = pp->next_ds;
    }
    break;
  }
  return 1;
}
/*******************************************************************************/
int Jabron::WriteJabKSR(char *string,int ds)
{
  char *ptr,
    str[15];
  int  i=0;
  double wert;
  double trR;
  
  ptr = string;
  
  /*rechte Trennfläche suchen*/
  ptr_profil = ptr_anfang;
  while ((ptr_profil !=NULL)&&(ptr_profil->profiltyp != TRENNFLAECHEN))
    ptr_profil = ptr_profil->next;
  if ((ptr_profil !=NULL)&&(ptr_profil->profiltyp == TRENNFLAECHEN))
    trR = ptr_profil->datensatz->next_ds->x;
  else trR=BCE_NAN;
  
  ptr_profil = ptr_anfang;
  while ((ptr_profil !=NULL)&&(ptr_profil->ds_nummer < ds))
    ptr_profil = ptr_profil->next;
  ptr_profil->profiltyp = RAUHIGKEIT;
  pp = ptr_profil->datensatz;
  
  
  while (((ptr[0]==' ')||(ptr[0]=='\t'))&&(ptr[0]!='\0')) // erste 'blanks' überlesen
    ptr++;
  while ((ptr[0]!='\n')&&(ptr[0]!='\0'))
  {
    i=0;
    while ((ptr[0]!=' ')&&(ptr[0]!='\t')&&(ptr[0]!='\n')&&(ptr[0]!='\0'))
    {
      str[i++]=ptr[0];
      ptr++;
    }
    while (((ptr[0]==' ')||(ptr[0]=='\t'))&&(ptr[0]!='\0')) // evt 'blanks' überlesen
      ptr++;
    str[i]='\0';
    wert = atof(str);
    
    while ((pp)&&(pp->x <trR))
      pp = pp->next_ds;
    
    while (pp)
    {
      pp->y = wert;
      pp->pre_y=0;
      pp = pp->next_ds;
    }
    break;
  }
  return 1;
}
/*******************************************************************************/
/*******************************************************************************/
int Jabron::WriteJabKSLU(char *string,int ds,int l_sohle)
{
  char *ptr,
    str[15];
  int  i=0;
  double wert;
  double trL =BCE_NAN,
		  trLU=BCE_NAN;
  
  ptr = string;
  
  /* Stationswert von trLU ermitteln*/
  ptr_profil = ptr_anfang;
  while ((ptr_profil !=NULL)&&(ptr_profil->profiltyp != GELAENDEHOEHE))
    ptr_profil = ptr_profil->next;
  if ((ptr_profil !=NULL)&&(ptr_profil->profiltyp == GELAENDEHOEHE))
	 {
    pp = ptr_profil->datensatz;
    while ((pp)&&(pp->ds_nr < l_sohle))
      pp=pp->next_ds;
    if (pp)  trLU=pp->x;
	 }
  
  /*linke Trennfläche suchen*/
  ptr_profil = ptr_anfang;
  while ((ptr_profil !=NULL)&&(ptr_profil->profiltyp != TRENNFLAECHEN))
    ptr_profil = ptr_profil->next;
  if ((ptr_profil !=NULL)&&(ptr_profil->profiltyp == TRENNFLAECHEN))
    trL = ptr_profil->datensatz->x;
  else trL=BCE_NAN;
  
  /* zu Rauhigkeit gehen */
  ptr_profil = ptr_anfang;
  while ((ptr_profil !=NULL)&&(ptr_profil->ds_nummer < ds))
    ptr_profil = ptr_profil->next;
  ptr_profil->profiltyp = RAUHIGKEIT;
  pp = ptr_profil->datensatz;
  
  while (((ptr[0]==' ')||(ptr[0]=='\t'))&&(ptr[0]!='\0')) // erste 'blanks' überlesen
    ptr++;
  while ((ptr[0]!='\n')&&(ptr[0]!='\0'))
  {
    i=0;
    while ((ptr[0]!=' ')&&(ptr[0]!='\t')&&(ptr[0]!='\n')&&(ptr[0]!='\0'))
    {
      str[i++]=ptr[0];
      ptr++;
    }
    while (((ptr[0]==' ')||(ptr[0]=='\t'))&&(ptr[0]!='\0')) // evt 'blanks' überlesen
      ptr++;
    str[i]='\0';
    wert = atof(str);
    
    while ((pp)&&(pp->x <trL))
      pp = pp->next_ds;
    while ((pp)&&(pp->x <trLU))
    {
      pp->y = wert;
      pp->pre_y=0;
      pp = pp->next_ds;
    }
    break;
  }
  return 1;
}
/*******************************************************************************/
/*******************************************************************************/
int Jabron::WriteJabKSRU(char *string,int ds,int r_sohle)
{
  char *ptr,
    str[15];
  int  i=0;
  double wert;
  double trR =BCE_NAN,
		  trRU=BCE_NAN;
  
  ptr = string;
  
  
  /* Stationswert von trRU ermitteln*/
  ptr_profil = ptr_anfang;
  while ((ptr_profil !=NULL)&&(ptr_profil->profiltyp != GELAENDEHOEHE))
    ptr_profil = ptr_profil->next;
  if ((ptr_profil !=NULL)&&(ptr_profil->profiltyp == GELAENDEHOEHE))
	 {
    pp = ptr_profil->datensatz;
    while ((pp)&&(pp->ds_nr < r_sohle))
      pp=pp->next_ds;
    if (pp)  trRU=pp->x;
	 }
  
  /*rechte Trennfläche suchen*/
  ptr_profil = ptr_anfang;
  while ((ptr_profil !=NULL)&&(ptr_profil->profiltyp != TRENNFLAECHEN))
    ptr_profil = ptr_profil->next;
  if ((ptr_profil !=NULL)&&(ptr_profil->profiltyp == TRENNFLAECHEN))
    trR = ptr_profil->datensatz->next_ds->x;
  else trR=BCE_NAN;
  
  /* zu Rauhigkeit gehen */
  ptr_profil = ptr_anfang;
  while ((ptr_profil !=NULL)&&(ptr_profil->ds_nummer < ds))
    ptr_profil = ptr_profil->next;
  ptr_profil->profiltyp = RAUHIGKEIT;
  pp = ptr_profil->datensatz;
  
  while (((ptr[0]==' ')||(ptr[0]=='\t'))&&(ptr[0]!='\0')) // erste 'blanks' überlesen
    ptr++;
  while ((ptr[0]!='\n')&&(ptr[0]!='\0'))
  {
    i=0;
    while ((ptr[0]!=' ')&&(ptr[0]!='\t')&&(ptr[0]!='\n')&&(ptr[0]!='\0'))
    {
      str[i++]=ptr[0];
      ptr++;
    }
    while (((ptr[0]==' ')||(ptr[0]=='\t'))&&(ptr[0]!='\0')) // evt 'blanks' überlesen
      ptr++;
    str[i]='\0';
    wert = atof(str);
    
    while ((pp)&&(pp->x <trRU))
      pp = pp->next_ds;
    while ((pp)&&(pp->x <trR))
    {
      pp->y = wert;
      pp->pre_y=0;
      pp = pp->next_ds;
    }
    break;
  }
  return 1;
}
/*******************************************************************************/
/*******************************************************************************/
int Jabron::WriteJabKS_Sohle(char *string,int ds,int l_sohle,int r_sohle)
{
  char *ptr,
    str[15];
  int  i=0;
  double wert;
  double trLU =BCE_NAN,
		  trRU=BCE_NAN;
  
  ptr = string;
  
  /* Stationswert von trLU / trRU ermitteln*/
  ptr_profil = ptr_anfang;
  while ((ptr_profil !=NULL)&&(ptr_profil->profiltyp != GELAENDEHOEHE))
    ptr_profil = ptr_profil->next;
  if ((ptr_profil !=NULL)&&(ptr_profil->profiltyp == GELAENDEHOEHE))
	 {
    pp = ptr_profil->datensatz;
    while ((pp)&&(pp->ds_nr < l_sohle))
      pp=pp->next_ds;
    if (pp)  trLU=pp->x;
    
    while ((pp)&&(pp->ds_nr < r_sohle))
      pp=pp->next_ds;
    if (pp)  trRU=pp->x;
	 }
  
  /* zu Rauhigkeit gehen */
  ptr_profil = ptr_anfang;
  while ((ptr_profil !=NULL)&&(ptr_profil->ds_nummer < ds))
    ptr_profil = ptr_profil->next;
  ptr_profil->profiltyp = RAUHIGKEIT;
  pp = ptr_profil->datensatz;
  
  while (((ptr[0]==' ')||(ptr[0]=='\t'))&&(ptr[0]!='\0')) // erste 'blanks' überlesen
    ptr++;
  while ((ptr[0]!='\n')&&(ptr[0]!='\0'))
  {
    i=0;
    while ((ptr[0]!=' ')&&(ptr[0]!='\t')&&(ptr[0]!='\n')&&(ptr[0]!='\0'))
    {
      str[i++]=ptr[0];
      ptr++;
    }
    while (((ptr[0]==' ')||(ptr[0]=='\t'))&&(ptr[0]!='\0')) // evt 'blanks' überlesen
      ptr++;
    str[i]='\0';
    wert = atof(str);
    
    while ((pp)&&(pp->x <trLU))
      pp = pp->next_ds;
    while ((pp)&&(pp->x <trRU))
    {
      pp->y = wert;
      pp->pre_y=0;
      pp = pp->next_ds;
    }
    break;
  }
  return 1;
}

/*******************************************************************************/
int Jabron::DoGeschlProfil(int maxDatensatz)
{
/* Ist das zu konvertierende Jabron Profil ein 'geschlossenes' Profil,
so werden die x-Werte ab einem bestimmten Punkt wieder kleiner.
Daher werden diese Werte in umgekehrter Reihenfolge in den neuen Datensatz
Unterkante Brücke kopiert und im Datensatz GELAENDEHOEHE gelöscht.
  */
  Profildatei *bruecke;
  Koord *gel,*max;
  
  ptr_profil = ptr_anfang;
  while ((ptr_profil !=NULL)&&(ptr_profil->profiltyp !=GELAENDEHOEHE))
    ptr_profil = ptr_profil->next;
  if (ptr_profil->profiltyp ==GELAENDEHOEHE)
  {
    gel = ptr_profil->datensatz;
    max = gel;
    
    while ((gel)&&(max->x <=gel->x))
    {
      max = gel;
      gel = gel->next_ds;
    }
    while (gel->next_ds)
      gel=gel->next_ds;
    
    if (gel)
    {
      MakeNewNode(maxDatensatz);
      WriteTypDaten(maxDatensatz,UK_BRUECKE,NULL);
      bruecke = ptr_anfang;
      while ((bruecke !=NULL)&&(bruecke->profiltyp !=UK_BRUECKE))
        bruecke = bruecke->next;
      if (bruecke->profiltyp ==UK_BRUECKE)
      {
        pp = bruecke->datensatz;
        p_t=pp;
        while ((gel)&&(gel != max))
        {
          if (!pp)
            NewKoord(maxDatensatz);
          pp->x= gel->x;
          pp->y= gel->y;
          gel = gel->pre_ds;
          pp  = pp->next_ds;
        }
        if ((gel)&&(gel == max))
        {
          if (!pp)
            NewKoord(maxDatensatz);
          pp->x= gel->x;
          pp->y= gel->y;
        }
      }
      
      if (max)		// jetzt noch löschen
      {
        gel = max->next_ds;
        max->next_ds=NULL;
        while(gel->next_ds)
        {
          max = gel;
          gel = gel->next_ds;
          delete max;
        }
        
      }
      
    }
  }
  return 1;
}
/*******************************************************************************/
int Jabron::BuildDsInfo(WSP_PROFIL_LISTE *wpl)
{
  int nr, ds = 1;
  
  wpl->data->typ[0] = 0;  //immer
  ptr_profil = ptr_anfang;
  while (ptr_profil )
  {
    if(ptr_profil->profiltyp!=COMMENT)
    {
      pp = ptr_profil->datensatz;
      wpl->data->typ[ds]=ptr_profil->profiltyp;
      nr =0;
      while (pp)
      {
        nr++;
        pp = pp->next_ds;
      }
      wpl->data->ds_info[ds]=nr;
    }
    ds++;
    ptr_profil = ptr_profil->next;
  }
  wpl->data->ds_info[0]=ds-1;
  return 1;
}
/*******************************************************************************/
/*******************************************************************************/

