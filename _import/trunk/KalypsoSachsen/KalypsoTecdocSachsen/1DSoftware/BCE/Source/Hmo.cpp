// Hmo.cpp: Implementierung der Klasse Hmo.
//
///////////////////////////////////////////////////////////////////////////
// Klasse zur Modellierung und zum Arbeiten mit BCE-Hoehenmodellen (HMO)
//
// 
//
///////////////////////////////////////////////////////////////////////////

#include <stdio.h>
#include <windows.h>

#include <iostream>

#define REAL double
#include "triangle.h"

#include "shapeFile.h"
#include "dbfFile.h"

#include "Hmo.h"

using namespace BCE;

///////////////////////////////////////////////////////////////////////
// Hilfsklasse Indexlist
///////////////////////////////////////////////////////////////////////
// zum verwalten von int -> int Abbildungen
///////////////////////////////////////////////////////////////////////
Hmo::IndexList::IndexList( const unsigned int grow )
// Parameter:
//        const unsigned int grow: zuwachsFaktor für IndexListe
{
  this->grow = grow;
  indexList = NULL;
  maxIndex = 0;
};

Hmo::IndexList::~IndexList()
{
  free( indexList );
};

unsigned int& Hmo::IndexList::operator[] ( unsigned int from )
{
  if ( from >= maxIndex )
  {
    indexList = (unsigned int*)realloc( indexList, ( from + grow ) * sizeof( unsigned int ) );
    ZeroMemory( &indexList[maxIndex], ( from + grow - maxIndex ) * sizeof( unsigned int ) );

    maxIndex = from + grow;
  }; // if from >= maxIndex

  return indexList[from];
}; // operator[]

unsigned int Hmo::IndexList::operator[] ( unsigned int from ) const
{
    if ( from >= maxIndex )
    return 0;
  else
    return indexList[from];
}; // operator[]


void Hmo::IndexList::Set( const unsigned int from, const unsigned int to )
// Parameter:
//        const unsigned int from, to: an der from. Stelle wird to Eingetragen. d.h: from |-> to
{
  this[from] = to;
}; // Set

unsigned int Hmo::IndexList::Get( const unsigned int from ) const
// Parameter:const unsigned int from: das bild dieses Index ist gesucht
// Rückgabewert: indexList[from] oder 0 bei Fehler
{
  return (*this)[from];  
}; // Get


//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

Hmo::Hmo(int zuwachs /* = 10000 */)
{
  knotenListe = NULL;
  knotenAnzahl = 0;
  kantenListe = NULL;
  kantenAnzahl = 0;
  dreiecksListe = NULL;
  dreiecksAnzahl = 0;
  maxKnoten = 0;
  maxKanten = 0;
  maxDreiecke = 0;
  zuwachsFaktor = zuwachs;
};

Hmo::~Hmo()
{
  DeleteContents();
};

void Hmo::DeleteContents()
{
  DeleteKnoten();
  DeleteKanten();
  DeleteDreiecke();
};


//////////////////////////////////////////////////////////
// 'Attribute' 
//////////////////////////////////////////////////////////

int Hmo::AddKnoten( double x, double y, double z )
//
// fügt einen neuen Knoten hinzu
//
// Parameter: double x, y, z: Koordinaten des knotens
// Rückgabewert: Nummer des hinzugefügten Knotens
{
  if (knotenAnzahl >= maxKnoten)
  {
    maxKnoten += zuwachsFaktor;
    knotenListe = (Knoten*)realloc(knotenListe, sizeof(Knoten) * maxKnoten);
    if (!knotenListe)
      return 1;
  };

  knotenListe[knotenAnzahl].x = x;
  knotenListe[knotenAnzahl].y = y;
  knotenListe[knotenAnzahl].z = z;
  return knotenAnzahl++;
}; // AddKnoten

void Hmo::DeleteKnoten()
{
  if (knotenListe)
  {
    free(knotenListe);
    knotenListe = NULL;
    knotenAnzahl = 0;
    maxKnoten = 0;
  };
};

int Hmo::AddKante( int i1, int i2 )
// fügt eine neue Kante ein
// Parameter:
//        int i1, i2: die Endpunkte der Kante
// Rückgabewert: Fehlerkode:
//                    0 kein Fehler
//                    1 nicht genügend Speicher
{
  if ( kantenAnzahl >= maxKanten )
  {
    maxKanten += zuwachsFaktor;
    kantenListe = (Kante*)realloc( kantenListe, maxKanten * sizeof( Kante ) );
    if ( !kantenListe )
      return 1;
  }; // if kantenAnzahl >= maxKanten

  kantenListe[kantenAnzahl].i1 = i1;
  kantenListe[kantenAnzahl].i2 = i2;
  kantenAnzahl++;

  return 0;
}; // AddKanten

void Hmo::DeleteKanten()
{
  free( kantenListe );
  kantenListe = NULL;
  kantenAnzahl = 0;
  maxKanten = 0;
}; // DeleteKanten

int Hmo::AddDreieck( int i1, int i2, int i3 )
//
// fügt ein neues Dreieck hinzu
//
// Parameter: int i1, i2, i3: Indices der Dreiecksecken
// Rückgabewert: Fehlerkode:
//                    0 kein Fehler
//                    1 nicht genügend Speicher
{
  if (dreiecksAnzahl >= maxDreiecke)
  {
    maxDreiecke += zuwachsFaktor;
    dreiecksListe = (Dreieck*)realloc(dreiecksListe, sizeof(Dreieck) * maxDreiecke);
    if ( !dreiecksListe )
      return 1;
  };

  dreiecksListe[dreiecksAnzahl].i1 = i1;
  dreiecksListe[dreiecksAnzahl].i2 = i2;
  dreiecksListe[dreiecksAnzahl].i3 = i3;
  dreiecksAnzahl++;
  return 0;
}; // AddDreieck

void Hmo::DeleteDreiecke()
// löscht alle Dreiecke und befreit die Dreiecksliste
{
  if (dreiecksListe)
  {
    free(dreiecksListe);
    dreiecksListe = NULL;
    dreiecksAnzahl = 0;
    maxDreiecke = 0;
  };
}; // DeleteDreiecke


/////////////////////////////////////////////
// Import / Export
/////////////////////////////////////////////

int Hmo::ReadFromFile( const char* fileName )
//
// liest Höhenmodel - Daten aus einer Datei
//
// Parameter:
//        LPCTSTR fileName: Name der Eingabedatei
// Rückgabewert:
//        int: Fehlerkode:  0: kein Fehler
//                          1: Fehler beim Öffnen der Eingabedatei
//                          2: Fehler im Eingabeformat der Punkte
//                          3: Fehler im Eingabeformat der Dreiecke
//                       >200: Fehler beim Zufügen eines Dreiecks (siehe AddDreieck)
//
// Bemerkung: die unterschiedlichen Eingabeformate werden anhand der Dateiendung ermittelt
// zur Zeit werden erkannt: - '.2DM' SMS Format
//                          - '.HMO' HMO Format
//                          - '.NET' TIN Format
//                          - '.RAS' Raster Daten (pro Zeile x y z, das ist alles)
//   eine unbekannte Endung wird versucht als hmo einzulesen
{
  FILE *file = fopen( fileName, "r");
  if (!file)
    return 1; // Fehler beim Öffnen der Eingabedatei

  LPTSTR ext = strchr( fileName, '.');

  bool read = false;
  int error = 0;
  if ( ext )
  {
    ext++;
    if (_stricmp(ext, "2DM") == 0)
    {
      error = ReadSmsFile(file);
      read = true;
    }
    else if (_stricmp(ext, "NET") == 0)
    {
      error = ReadNetFile(file);
      read = true;
    }
    else if (_stricmp(ext, "RAS") == 0)
    {
      error = ReadRasFile(file);
      read = true;
    };
  };
  if (!read)
    error = ReadHmoFile(file);  // im Zweifelsfall stets hmo lesen

  fclose(file);

  if (error)
    return error;

  return 0;
}; // ReadFromFile

int Hmo::ReadSmsFile(FILE* file)
//
// liest SMS-Daten aus einer Datei  (SMS = '.2dm')
//
// Parameter:
//        FILE* file: geöffnete Eingabedatei
// Rückgabewert:
//        int: Fehlerkode:  0: kein Fehler
//                          1: Fehler beim Lesen der Datei
//                          2: Fehler im Eingabeformat der Punkte
//                          3: Fehler im Eingabeformat der Dreiecke
//                          4: Fehler: unbekannter Knoten bei Dreiecken
//                       >200: Fehler beim Zufügen eines Dreiecks (siehe AddDreieck)
//
// Bemerkung:
//   vorhande Knoten, Kanten und Dreiecke werden gelöscht
{
  DeleteContents();

  TCHAR buffer[1000];

  IndexList knotenMap( zuwachsFaktor );
  int kIndex = 0;        // aktueller Index des gerade gelesenen knotens

  while ( fgets( (LPSTR)buffer, 1000, file) )
  {
    int n, n1, n2, n3, n4, dummy;
    double x, y, z;

    if ( sscanf( (LPSTR)buffer, "ND %d %lf %lf %lf", &n, &x, &y, &z) == 4 )
    {
      knotenMap[n] = ++kIndex;
      AddKnoten( x, y, z );
      continue;
    };

    if (sscanf( (LPSTR)buffer, "E3T %d %d %d %d %d", &n, &n1, &n2, &n3, &dummy ) == 5 )
    {
      int error = AddDreieck( n1, n2, n3 );
      if ( error )
        return error + 200;

      continue;
    };

    if ( sscanf( buffer, "E4Q %d %d %d %d %d %d", &n, &n1, &n2, &n3, &n4, &dummy ) == 4 )
      continue;
  }; // while fgets

  // jetzt die Dreiecksknoten umbiegen

  for ( int i = 0; i < GetDreieckCount(); i++ )
  {
    Dreieck* dreieck = GetDreieck( i );
    int i1 = knotenMap[dreieck->i1];
    int i2 = knotenMap[dreieck->i2];
    int i3 = knotenMap[dreieck->i3];
    if ( i1 == 0 || i2 == 0 || i3 == 0 )
      return 4;

    dreieck->i1 = i1 - 1;
    dreieck->i2 = i2 - 1;
    dreieck->i3 = i3 - 1;
  }; // for i

  if (feof(file))
    return 0;
  else
    return 1;
};

int Hmo::ReadRasFile(FILE* file)
//
// liest Raster-Daten aus einer Datei: Format: x y z 
//
// Parameter:
//        FILE* file: geöffnete Eingabedatei
// Rückgabewert:
//        int: Fehlerkode:  0: kein Fehler
//                       >100: Fehler beim Zufügen eines Knoten (siehe AddKnoten)
//
{
  double x, y, z;
  while (!feof(file))
  {
    if (fscanf(file, "%lf %lf %lf\n", &x, &y, &z) == 3)
      AddKnoten( x, y, z );
  }; // while !eof
  return 0;
};

int Hmo::ReadHmoFile(FILE* file)
//
// liest hmo-Daten aus einer Datei
//
// Parameter:
//        FILE* file: geöffnete Eingabedatei
// Rückgabewert:
//        int: Fehlerkode:  0: kein Fehler
//                          2: Fehler im Eingabeformat der Punkte
//                          3: Fehler im Eingabeformat der Dreiecke
//                          4: Unbekannter Knoten bei den Dreiecken
//                       >200: Fehler beim Zufügen eines Dreiecks (siehe AddDreieck)
//
{
  IndexList knotenMap( zuwachsFaktor );
  int kIndex = 0;        // aktueller Index des gerade gelesenen knotens

  int knotenNummer;
  double x, y, z;

  int dreieckNummer, k1, k2, k3;

  TCHAR dummy[1000];

  int typ;
  while ((typ = fgetc(file)) != EOF)
  {
    switch(typ)
    {
    case 'P':
      {
        if (fscanf(file, ": %d %lf %lf %lf\n", &knotenNummer, &x, &y, &z) != 4)
          return 2;

        knotenMap[knotenNummer] = ++kIndex;

        AddKnoten( x, y, z );
      }; // case 'P'
      break;

    case 'D':
      {
        if(fscanf(file, ": %d %d %d %d\n", &dreieckNummer, &k1, &k2, &k3) != 4)
          return 3;

        int error = AddDreieck( k1, k2, k3 );
        if ( error )
          return error + 200;
      }; // case 'D'
    break;

    default:
        fgets( (LPSTR)dummy, 1000, file );
    }; // switch typ
  }; // while !eof

  // jetzt noch die Dreiecke umnummerieren
  for ( int i = 0; i < GetDreieckCount(); i++ )
  {
    Dreieck* dreieck = GetDreieck( i );
    unsigned int i1 = knotenMap[dreieck->i1];
    unsigned int i2 = knotenMap[dreieck->i2];
    unsigned int i3 = knotenMap[dreieck->i3];
    
    if ( i1 == 0 || i2 == 0 || i3 == 0 )
      return 4;
    // TODO: Messagebox stattdessen!!!

    dreieck->i1 = i1 - 1;
    dreieck->i2 = i2 - 1;
    dreieck->i3 = i3 - 1;
  }; // for i

  return 0;
};

int Hmo::ReadNetFile(FILE* file)
//
// liest ungeneratet Tin-Daten (.net) aus einer Datei
//
// Parameter:
//        FILE* file: geöffnete Eingabedatei
// Rückgabewert:
//        int: Fehlerkode:  0: kein Fehler
//                          1: Fehler beim Lesen der Datei
//                          2: Fehler im Eingabeformat der Punkte
//                          3: unbekannter Knoten in den Kanten
//                          4: unbekannte Kante in den Dreiecken
//                       >200: Fehler beim Zufügen einer Kante (siehe AddKante)
//                       >300: Fehler beim Zufügen eines Dreiecks (siehe AddDreieck)
//
{
  IndexList knotenMap( zuwachsFaktor ), kantenMap( zuwachsFaktor ); // zum umindizieren der Kanten und Knoten
  unsigned int knotenIndex = 0, kantenIndex = 0; // die jeweils aktuellen indices
  
  CHAR buffer[1000];
  char aktuell = ' ';

  int n, k1, k2, k3;
  double x, y, z;

  while (fgets( (LPSTR)buffer, 1000, file))
  {

    switch (buffer[0])
    {
    case 'N':
      aktuell = 'N';
      continue;

    case 'E':
      aktuell = 'E';
      continue;

    case 'T':
      aktuell = 'T';
      continue;
    };

    switch (aktuell)
    {
    case 'N':
      if (sscanf( (LPSTR)buffer, "%d %lf %lf %lf", &n, &x, &y, &z) == 4)
      {
        knotenMap[n] = ++knotenIndex;  // um eins erhöht: 0 ist Fehler

        AddKnoten( x, y, z );
      };
      continue;

    case 'E':
      if ( sscanf( (LPSTR)buffer, "%d %d %d %d", &n, &k1, &k2, &k3 ) == 4 )
      {
        kantenMap[n] = ++kantenIndex; // um eins erhöht: 0 ist Fehler

        int error = AddKante( k1, k2 );
        if ( error )
          return error + 200;
      };
      continue;

    case 'T':
      if (sscanf( (LPSTR)buffer, "%d %d %d %d", &n, &k1, &k2, &k3) == 4)
      {
        int error = AddDreieck( k1, k2, k3 );
        if ( error )
          return error + 300;
      };
      continue;
    }; // switch
  };

  // jetzt die Kantenknoten umbiegen
  for ( int i = 0; i < GetKantenCount(); i++ )
  {
    Kante* kante = GetKante( i );
    unsigned int i1 = knotenMap[kante->i1];
    unsigned int i2 = knotenMap[kante->i2];
    if ( i1 == 0 || i2 == 0 )
      return 3; // unbekannter Knoten in den Kanten
    kante->i1 = i1 - 1;
    kante->i2 = i2 - 1;
  }; // i

  // jetzt die Dreiecke statt mit Kantennummer mit Knotennummern indizieren
  for ( i = 0; i < GetDreieckCount(); i++ )
  {
    Dreieck* dreieck = GetDreieck( i );

    unsigned int i1 = kantenMap[dreieck->i1];
    unsigned int i2 = kantenMap[dreieck->i2];
    unsigned int i3 = kantenMap[dreieck->i3];

    if ( i1 == 0 || i2 == 0 || i3 == 0 )
      return 4; // unbekannte Kante in den Dreiecken

    Kante* k1 = GetKante( i1 - 1 );
    Kante* k2 = GetKante( i2 - 1 );

    dreieck->i1 = k1->i1;
    dreieck->i2 = k1->i2;
    if ( k1->i1 == k2->i1 || k1->i2 == k2->i1 )
      dreieck->i3 = k2->i2;
    else
      dreieck->i3 = k2->i1;
  }; // for i

  return 0;
}; // ReadNetFile

int Hmo::WriteToFile( const char* hmoFileName)
//
// schreibt Daten in eine HMO Datei
//
// Rückgabewert: Errorcode:
//                0 kein Fehler
//                1 Fehler beim Öffnen der Ausgabedatei
//                2 Fehler beim schreiben
{
  FILE* outFile = fopen( hmoFileName, "w");
  if (!outFile)
    return 1;

  int error = 0;
  for ( int i = 0; i < knotenAnzahl; i++ )
  {
    Knoten* knoten = &(knotenListe[i]);
    error  = fprintf( outFile, "P: %10d %15.3lf %15.3lf %15.3lf\n", i + 1, knoten->x, knoten->y, knoten->z );
    if ( error < 0 )
      break;
  };
  for( i = 0; i < dreiecksAnzahl; i++ )
  {
    Dreieck* dreieck = &(dreiecksListe[i]);
    error = fprintf( outFile, "D: %10d %10d %10d %10d\n", i + 1, dreieck->i1 + 1, dreieck->i2  + 1, dreieck->i3 + 1 );
    if( error < 0 )
      break;
  };
  fclose( outFile );

  if( error < 0 )
    return 2;
  else
    return 0;
}; // WriteToFile


/**
  * legt HMO - Daten als ESRI shapes ab
  *
  * @param shapeTitle Pfad der Shape Dateien ohne Endung
  * @param hasZ bei true werden die z Werte mit abgelegt, in ein Z-Polygon
  *
  * @return Fehlerkode:
  *                    0 keine Fehler
  *                    1 Fehler beim Öffnen der ShapeFiles
  *                    2 Fehler beim Öffnen des DbfFiles
  *                    3 Fehler beim Zufügen eines Feldes zur Dbf- 
  *                    4 Ein Knoten wurde nicht gefunden
  */
int Hmo::WriteToPolygonShape( const char* shapeTitle, bool hasZ)
{
  double eckkoord[12];
  
  int shapeType;
  if (hasZ)
    shapeType = SHPT_POLYGONZ;
  else
    shapeType = SHPT_POLYGON;
  
  ShapeFile shapeFile( shapeTitle, shapeType );
  if (!shapeFile.GetHandle())
    return 1;
  
  // Dbffile öffnen
  TCHAR dbfFileName[MAX_PATH];
  strcpy(dbfFileName, shapeTitle );
  strcat(dbfFileName, ".dbf");
  DbfFile dbfFile(dbfFileName);
  if (!dbfFile.GetHandle())
    return 2;
  if (!(dbfFile.AddField("Dummy", DbfFile::FTInteger, 1, 0)))  // zur Probe erstmal so
    return 3;

  int count = 0;
  int parts = 0;
  int dummy = 1;
  Knoten* k1;
  Knoten* k2;
  Knoten* k3;
  
  for( int i = 0; i < dreiecksAnzahl; i++ )
  {
    k1 = GetKnoten( dreiecksListe[i].i1 );
    k2 = GetKnoten( dreiecksListe[i].i2 );
    k3 = GetKnoten( dreiecksListe[i].i3 );

    eckkoord[0] = k1->x;
    eckkoord[1] = k1->y;
    eckkoord[2] = k1->z;
    eckkoord[3] = k2->x;
    eckkoord[4] = k2->y;
    eckkoord[5] = k2->z;
    eckkoord[6] = k3->x;
    eckkoord[7] = k3->y;
    eckkoord[8] = k3->z;
    eckkoord[9] = eckkoord[0];
    eckkoord[10] = eckkoord[1];
    eckkoord[11] = eckkoord[2];

    shapeFile.WriteVertices( 4, 1, &parts, eckkoord );
    dbfFile.WriteAttribute( count, 0, &dummy );
    count++;
  };
  
  shapeFile.Close();
  dbfFile.Close();
  return 0;
}; // WritePolygonShape


/////////////////////////////////////////
// Operationen
/////////////////////////////////////////

int Hmo::Triangulate( double quality /* = 0.0 */ )
// trianguliert mithilfe des Programmpaketes triangle
// Parameter:
//        int quality: falls ungleich 0.0, wird die triangulations so verfeinert, dass
//                      alle Winkel ( aller Dreiecke ) mindestens qualitiy gross sind ( sollte zwischen 0 und 33.7 liegen )
// Rückgabewert:
//          Fehlerkode: 0 kein Fehler
//                      1 Ausnahme beim Triangulieren
//
{
  // Datenstrukturen anlegen
  triangulateio tIN;
  triangulateio tOUT;
  
  ZeroMemory( &tIN, sizeof( tIN ) ); // erstmal alles null setzen
  ZeroMemory( &tOUT, sizeof( tOUT ) );

  // knoten
  tIN.numberofpoints = GetKnotenCount();
  tIN.pointlist = (double*)malloc( GetKnotenCount() * 2 * sizeof( double ) );

  tIN.numberofpointattributes = 1;
  tIN.pointattributelist = (double*)malloc( GetKnotenCount() * sizeof( double ) );
  
  // Datenstruktur füllen
  for( int i = 0; i < GetKnotenCount(); i++ )
  {
    Knoten* k = GetKnoten( i );
    tIN.pointlist[2 * i] = k->x;
    tIN.pointlist[2 * i + 1] = k->y;
    tIN.pointattributelist[i] = k->z;
  };

  if ( GetKantenCount() > 0 )
  {
    tIN.numberofsegments = GetKantenCount();
    tIN.segmentlist = (int*)malloc( GetKantenCount() * 2 * sizeof( int ) );
    for ( int i = 0; i < GetKantenCount(); i++ )
    {
      Kante* kante = GetKante( i );
      tIN.segmentlist[i * 2] = kante->i1;
      tIN.segmentlist[i * 2 + 1] = kante->i2;
    }; // for i
  }; // if GetKantenCount > 0
  
  // kommandozeile erzeugen und triangulieren
  CHAR commandLine[256];
  strcpy( commandLine, "-z" );

  if ( GetKantenCount() > 0 ) // falls Kanten vorhanden, alles als einen planar straight line graph triangulieren, sonst einfach die convexe Hülle der Punkte
    strcat( commandLine, "p" );

  if ( quality != 0.0 )
  {
    CHAR helpStr[100];
    sprintf( helpStr, "q%lf", quality );
    strcat( commandLine, helpStr );
  };

  int error = 0;
/*  __try
  {
    triangulate( commandLine, &tIN, &tOUT, NULL );
  }
  __except( EXCEPTION_EXECUTE_HANDLER )
  {
    error = 1;
  };
*/
  try
  {
    triangulate( commandLine, &tIN, &tOUT, NULL );
  }
  catch( ... )
  {
    error = 1;
  };


  // Ergebnisse auslesen
  DeleteKnoten();
  DeleteDreiecke();

  if( error == 0 )
  {
    for ( i = 0; i < tOUT.numberofpoints; i++ )
      AddKnoten( tOUT.pointlist[i * 2], tOUT.pointlist[i * 2 + 1], tOUT.pointattributelist[i] );
    for ( i = 0; i < tOUT.numberoftriangles; i++ )
      AddDreieck( tOUT.trianglelist[i * 3], tOUT.trianglelist[i * 3 + 1], tOUT.trianglelist[i * 3 + 2] );
  };

  // Datenstrukturen wieder freigeben

  if ( tIN.pointlist )
    free( tIN.pointlist );

  if ( tIN.pointattributelist )
    free( tIN.pointattributelist );

  if ( tIN.pointmarkerlist )
    free( tIN.pointmarkerlist );

  if ( tIN.segmentlist )
    free( tIN.segmentlist );

  if ( tOUT.pointlist )
    free( tOUT.pointlist );

  if ( tOUT.pointattributelist )
    free( tOUT.pointattributelist );

  if ( tOUT.pointmarkerlist )
    free( tOUT.pointmarkerlist );

  if ( tOUT.trianglelist )
    free( tOUT.trianglelist );

  if ( tOUT.triangleattributelist )
    free( tOUT.triangleattributelist );

  if ( tOUT.trianglearealist )
    free( tOUT.trianglearealist );

  if ( tOUT.neighborlist )
    free( tOUT.neighborlist );

  if ( tOUT.segmentlist )
    free( tOUT.segmentlist );

  if ( tOUT.segmentmarkerlist )
    free( tOUT.segmentmarkerlist );

  if ( tOUT.holelist )
    free( tOUT.holelist );

  if ( tOUT.regionlist )
    free( tOUT.regionlist );

  if ( tOUT.edgelist )
    free( tOUT.edgelist );

  if ( tOUT.edgemarkerlist )
    free( tOUT.edgemarkerlist );
  
  if ( tOUT.normlist )
    free( tOUT.normlist );

  return error;
}; // Triangulate


/*!
 * Schreibt alle Daten per TRACE raus
 *
 * @param none
 *
 * @return void  : 
 */
void Hmo::DumpTrace( std::ostream& os )
{
  using namespace std;

  os.setf( ios_base::fixed, ios_base::floatfield );
  
  // erstmal die Kerndaten
  os << "Dies ist ein HMO" << endl;
  os << "Zuwachsfaktor: " << zuwachsFaktor << endl;;
  os << "Anzahl Knoten: " << knotenAnzahl << endl;
  os << "MaxKnoten: " << maxKnoten << endl;
  os << "Anzahl Kanten: " << kantenAnzahl << endl;
  os << "MaxKanten: " << maxKanten << endl;
  os << "Anzahl Dreiecke: " << dreiecksAnzahl << endl;
  os << "MaxDreiecke: " << maxDreiecke << endl;

  os << "die Knoten:" << endl;
  for( int k = 0; k < knotenAnzahl; k++ )
    os << "#: " << k << "  x: " << knotenListe[k].x << " y: " << knotenListe[k].y << " z: " << knotenListe[k].z << endl;

  os << "die Kanten:" << endl;
  for( int v = 0; v < kantenAnzahl; v++ )
    os << "#: " << v << "  i1: " << kantenListe[v].i1 << " i2: " << kantenListe[v].i2 << endl;

  os << "die Dreiecke:" << endl;
  for( int d = 0; d < dreiecksAnzahl; d++ )
    os << "#: " << d << "  i1: " << dreiecksListe[d].i1 << " i2: " << dreiecksListe[d].i2 << " i3: " << dreiecksListe[d].i3 << endl;
} // TraceDump


/**
  * Schreibt die Kanten als Linienshape raus.
  *
  * @param shapeTitle Pfad der Shape Dateien ohne Endung.
  * @param hasZ bei true werden die z Werte mit abgelegt, in eine Z-Polyline.
  *
  * @return Fehlerkode:
  *                    0 keine Fehler
  *                    1 Fehler beim Öffnen der ShapeFiles
  *                    2 Fehler beim Öffnen des DbfFiles
  *                    3 Fehler beim Zufügen eines Feldes zur Dbf- 
  *                    4 Ein Knoten wurde nicht gefunden
  */
int Hmo::WriteKantenAsLineShape( const char* shapeTitle, bool hasZ )
{

  double eckkoord[6];
  
  int shapeType;
  if( hasZ )
    shapeType = SHPT_POLYLINEZ;
  else
    shapeType = SHPT_POLYLINE;
  
  ShapeFile shapeFile( shapeTitle, shapeType );
  if (!shapeFile.GetHandle())
    return 1;
  
  // Dbffile öffnen
  TCHAR dbfFileName[MAX_PATH];
  strcpy(dbfFileName, shapeTitle );
  strcat(dbfFileName, ".dbf");
  DbfFile dbfFile(dbfFileName);
  if (!dbfFile.GetHandle())
    return 2;
  if (!(dbfFile.AddField("Dummy", DbfFile::FTInteger, 1, 0)))  // zur Probe erstmal so
    return 3;

  int count = 0;
  int parts = 0;
  int dummy = 1;
  
  const int kantenCount = GetKantenCount();
  for( int i = 0; i < kantenCount; i++ )
  {
	  Kante* kante = GetKante( i );
	  Knoten* k1 = GetKnoten( kante->i1 );
	  Knoten* k2 = GetKnoten( kante->i2 );
	  
	  eckkoord[0] = k1->x;
	  eckkoord[1] = k1->y;
	  eckkoord[2] = k1->z;
	  eckkoord[3] = k2->x;
	  eckkoord[4] = k2->y;
	  eckkoord[5] = k2->z;
	  
	  shapeFile.WriteVertices( 2, 1, &parts, eckkoord );
	  dbfFile.WriteAttribute( count, 0, &dummy );
	  count++;
  };
  
  shapeFile.Close();
  dbfFile.Close();
  return 0;
}
