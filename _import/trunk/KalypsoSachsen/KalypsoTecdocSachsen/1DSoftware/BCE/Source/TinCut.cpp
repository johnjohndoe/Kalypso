// tell the compiler to shut up: vector<ostream> gibt ziemlich lange Symbole
#pragma warning(disable:4786)

#include <minmax.h>
#include <stdarg.h>
#include <io.h>
#include <fcntl.h>
#include <math.h>
#include <ctime>

#include "Resource.h"

#include "DbfFile.h"
#include "ShapeFile.h"
#include "StringResource.h"

#include "TinCutExc.h"
#include "tincut.h"

using namespace BCE;

// Globale Konstanten initialisieren

const double TinCut::eps = 0.00001;
const double TinCut::epst = 0.00000001;
const int TinCut::SUCCESS = 0;

const StringResource TinCut::stringRes = StringResource();

// Konstruktor
TinCut::TinCut() : Observable()
{
  m_sd[0] = -1;
  m_sd[1] = -1;
  m_sd[2] = -1;
  
  m_verboseLevel = 0;
  
  // Dynamische Speicherverwaltung: Index 0: untere HMO, Index 1:obere HMO
  m_pointCapas[0] = 0; // aktuelle Kapazität am Anfang 0
  m_pointCapas[1] = 0;
  
  m_pointMinCapas[0] = 50000;
  m_pointMinCapas[1] = 50000;
  
  m_pointGrow[0] = 100000;
  m_pointGrow[1] = 100000;
  
  m_trnglCapas[0] = 0;
  m_trnglCapas[1] = 0;
  
  m_trnglMinCapas[0] = 100000;
  m_trnglMinCapas[1] = 100000;
  
  m_trnglGrow[0] = 150000;
  m_trnglGrow[1] = 150000;
  
  m_arcCapas[0] = 0;
  m_arcCapas[1] = 0;
  
  m_arcGrow[0] = 1000;
  m_arcGrow[1] = 1000;
  
  setObsMaxProcess( 5 ); // 5 Schritte bei TinCut
}; // Standardkonstruktor

unsigned int TinCut::run( void* pParam )
{
  RunParam* rParam = (RunParam*)pParam;
  TinCut* tinCut = rParam->tinCut;
  
  // es darf nur einmal gestartet werden
  if( tinCut->getObsState() != OBS_STATE_UNKNOWN )
    return tinCut->getObsReturnCode();
  
  // observable status setzen
  tinCut->setObsState( OBS_STATE_RUNNING );
  
  tinCut->setObsReturnCode( tinCut->cut( rParam->lowerHmo, rParam->upperHmo, rParam->shapeBase, rParam->b3D ) );
  
  tinCut->setObsState( OBS_STATE_FINISHED );
  return tinCut->getObsReturnCode();
};  // muss vom abgeleiteten Objekt implementiert werden

const char* TinCut::getIDString( const unsigned int stringID ) const
// Gibt den zu einer ID gehörenden String zurück
// Parameter:
//        const int stringID: die ID des Strings
// Rückgabewert:
//        string: der gefundene String
// Throws
//      TinCutException wenn die ID nicht gefunden wurde
{
  return stringRes.getString( stringID );
}; // getIDString

// Logging

// Fügt einen neuen Stream hinzu in welchen Log-Infos geschrieben werden
// Parameter:
//        const ostream& stream: der Stream; muss offen zum schreiben sein
void TinCut::addLogStream( const std::ostream& stream )
{
  m_logStreams.push_back( &stream );
}; // addLogStream

// Löscht einen Stream aus der Liste der Log-Streams
// Parameter:
//        const ostream& stream: der Stream
// Rückgabewert:
//        bool: true, falls der Stream gefunden und gelöscht wurde
// Bemerkung:
//        löscht höchstens einen Stream, wurde ein Stream mehrfach hinzugefügt muss er
//        auch mehrfach gelöscht werden
bool TinCut::removeLogStream( std::ostream& stream )
{
  for( int i = 0; i < m_logStreams.size(); i++ )
  {
    if( m_logStreams[i] == &stream )
    {
      // den Stream flushen und aus der Liste entfernen
      stream.flush(); 
      m_logStreams.erase( m_logStreams.begin() + i );
      return true;
    }; // if
  }; // for i
  
  // stream nicht gefunden
  return false;
}; // removeLogStream

void TinCut::log( const int vLevel, const int stringID, ... ) const
// wie vLog, der Formatstring wird nur aus einer ID erzeugt
// Parameter:
//        const int vLevel: der Verbose Level
//        const int stringID: die ResourcenID des FormatStrings
//        ...: beliebige wietere Parameter -> siehe printf
{
  std::string s = getIDString( stringID );
  
  va_list pData;
  va_start( pData, stringID );
  
  vLog( vLevel, s, pData );
  
  va_end( pData );
}; // log



void TinCut::log( const int vLevel, const char* text, ... ) const
// wie vLog, nur freie ParameterListe
// Parameter:
//        const int vLevel: der Verbose Level
//        const char* text: der FormatString: bei const string& funktioniert va_start nicht...
//        ...: beliebige weitere Parameter -> siehe printf
{
  va_list pData;
  va_start( pData, text );
  
  vLog( vLevel, text, pData );
  
  va_end( pData );
}; // log

void TinCut::vLog( const int vLevel, const std::string& text, const va_list argPtr ) const
// gibt Text auf die console und in die Protokolldatei aus
// Parameter:
//        const int vLevel: Verbose Level: Der Text wird nur ausgegeben, falls vLevel kleiner gleich 
//                    dem aktuellen Verbose Level ist
//        const string& text: der Formatstring für printf
//        const va_list argPtr: Liste der Aegumente für printf
{
  if ( vLevel <= getVerboseLevel() )
  {
    char buffer[1000]; // hoffentlich hat kein Ausgabestring eine Länge länger als 1000
    vsprintf( buffer, std::string( text + "\n" ).c_str(), argPtr );
    
    for( int i = 0; i < m_logStreams.size(); i++ )
    {
      std::ostream stream = *m_logStreams[i];
      stream << buffer;
    }; // for i
  }; // if vLevel ...
}; // vLog

void TinCut::allocatePoint( Point** pt, const int index, const int size )
// sichert, dass im übergebenen Array genug Platz reserviert wurde
// Parameter:
//        Point** p: zeigt auf das gewünschte Array ( wird ggfls. verändert )
//        const int index: index welche Parameter benutzt werden ( in den Arrays m_..pointCapa... )
//        const int size: die gewünschte Anzahl Punkte
// Bemerkung:
//      es werden mindestens soviele Punkte reservieert wie in m_pointMinCapas angegeben
// Throws: TinCutException
{
  if( size + 10 > m_pointCapas[index] )
  {
    // ein vielfaches der GrowSize hinzufügen
    int setSize = m_trnglCapas[index];
    while( setSize < size + 10 )
      setSize += m_trnglGrow[index];
    
    log( 1, IDS_TINCUT_ALLOCATE_POINT, setSize );
    *pt = (Point*)realloc( *pt, sizeof( Point ) * setSize );
    if( *pt == NULL )
      throw TinCutException( IDS_TINCUT_ERROR_MEM );
    
    m_pointCapas[index] = setSize;
    p[index] = *pt; // auch p ändern
  }; // wishedSize > pointCapas
}; // allocatePoint

void TinCut::allocateTriangle( Triangle** tr, const int index, const int size )
// sichert, dass im übergebenen Array genug Platz reserviert wurde
// Parameter:
//        Triangle** t: zeigt auf das gewünschte Array ( wird ggfls. verändert )
//        const int index: index welche Parameter benutzt werden ( in den Arrays m_..pointCapa... )
//        const int size: die gewünschte Anzahl Dreiecke
// Bemerkung:
//      es werden mindestens soviele Dreiecke reservieert wie in m_pointMinCapas angegeben
// Throws: TinCutException
{
  if( size + 10 > m_trnglCapas[index] )
  {
    // ein vielfaches der GrowSize hinzufügen
    int setSize = m_trnglCapas[index];
    while( setSize < size + 10 )
      setSize += m_trnglGrow[index];
    
    log( 1, IDS_TINCUT_ALLOCATE_TRNGL, setSize );
    *tr = (Triangle*)realloc( *tr, sizeof( Triangle ) * setSize );
    if( *tr == NULL )
    {
      throw TinCutException( IDS_TINCUT_ERROR_MEM );
    };

    m_trnglCapas[index] = setSize;
    t[index] = *tr;
  }; // wishedSize > pointCapas
}; // allocateTriangle

void TinCut::allocateArc( Arc** arc, const int index, const int size )
// sichert, dass im übergebenen Array genug Platz reserviert wurde
// Parameter:
//        Triangle** t: zeigt auf das gewünschte Array ( wird ggfls. verändert )
//        const int index: index welche Parameter benutzt werden ( in den Arrays m_..pointCapa... )
//        const int size: die gewünschte Anzahl Dreiecke
// Bemerkung:
//      es werden mindestens soviele Dreiecke reservieert wie in m_pointMinCapas angegeben
// Throws: TinCutException
{
  if( size + 10 > m_arcCapas[index] )
  {
    // ein vielfaches der GrowSize hinzufügen
    int setSize = m_arcCapas[index];
    while( setSize < size + 10 )
      setSize += m_arcGrow[index];
    
    log( 1, IDS_TINCUT_ALLOCATE_ARC, setSize );
    *arc = (Arc*)realloc( *arc, sizeof( Arc ) * setSize );
    if( *arc == NULL )
      throw TinCutException( IDS_TINCUT_ERROR_MEM );
    m_arcCapas[index] = setSize;
  }; // wishedSize > pointCapas
}; // allocateTriangle


// Verschneidet zwei Triangulationen ( als HMO - Dateien ) miteinander
// Parameter:
//        const string& lowerHmo, upperHmo: absolute Pfade auf die beiden Dateien
//        const string& outputPath: Basisdateiname für alle Ausgabedateien
//        const bool b3D: falls true, wird zusätzlich ein 3D Shape als Ausgabe ausgegeben
// Rückgabewert:
//        int: 0 Fehler, 1 kein Fehler
// Throws: TinCutException
int TinCut::cut( const char* lowerHmo, const char* upperHmo, const char* outputPath, const bool b3D )
{  
  MaxMinPoint e[2];

  time_t startTime;
  time( &startTime );
  
  int i;
  int fErgebnis;

  p = NULL;
  t = NULL;
  Arc** a = NULL; // Zeigt auf die Kanten
  Arc** g = NULL; // Zeigt auf die bereinigten Kanten
  int** o = NULL; // Zeigt auf die Umrandungslinie
  
  try
  {
    //////////////////////////////////////////
    // 1. Teil: Einlesen der Eingabedateien //
    //////////////////////////////////////////
    // Input: In lowerHmo und upperHmo stehen die Namen der Eingabedateien
    // Output: die Datenstrukturen p und t werden allokiert und gefüllt
    ///////////////////////////////////////////
    log( 0, "\n1. Teil :  Lesen der Eingabedateien\n" );
    setObsText( "Teil 1:  Eingabedateien werden gelesen." );
    
    // Speicher für Punkte und Dreiecke bereitstellen
    p = (Point**)malloc( sizeof( Point* ) * 2 );
    if( p == NULL )
      throw new TinCutException( IDS_TINCUT_ERROR_MEM );
    p[0] = (Point*)malloc( sizeof( Point ) * m_pointMinCapas[0] );
    p[1] = (Point*)malloc( sizeof( Point ) * m_pointMinCapas[1] );
    if( p[0] == NULL || p[1] == NULL )
      throw TinCutException( IDS_TINCUT_ERROR_MEM );
    
    t = (Triangle**)malloc( sizeof( Triangle* ) * 2 );
    if( t == NULL )
      throw TinCutException( IDS_TINCUT_ERROR_MEM );
    t[0] = (Triangle*)malloc( sizeof( Triangle ) * m_trnglMinCapas[0] );
    t[1] = (Triangle*)malloc( sizeof( Triangle ) * m_trnglMinCapas[1] );
    if( t[0] == NULL || t[1] == NULL )
      throw TinCutException( IDS_TINCUT_ERROR_MEM );
    
    // die Dateien einlesen
    if( lbo_input( upperHmo, lowerHmo, p, t ) == -1 )
      throw TinCutException( IDS_TINCUT_ERROR_INPUT );

    if( p[0][0].n < 3 || p[1][0].n < 3 )
      throw TinCutException( IDS_TINCUT_ERROR_INPUT );
    
    stepObsProcess();
    
    ////////////////////////////////////////////////////////////////////////////////////
    // 2. Teil: e wird initialisiert und zusätzliche Parameter für t werden berechnet //
    //          a wird eingelesen                                                     //
    ////////////////////////////////////////////////////////////////////////////////////
    // Input: in p und t beschreiben zwei Triangulationen
    // Output: zusätzliche Parameter von t sind initialisiert sowie e
    ////////////////////////////////////////////////////////////////////////////////////
    
    log( 0, "\n2.Teil : Parameterberechnung und Ueberpruefung der Eingabedaten\n" ); 
    setObsText( "Teil 2: Daten werden ueberprueft." );
    
    // MaxMinPoint e[2] mit Werten füllen
    for ( i = 0; i < 2; i++ )
    {
      if( lbo_MaxMinKoordinaten( i, e, p ) == -1 )
        return 0; // Extremwerte: n,x,y,z 
    };
    
    // Koordinaten in die Nähe des Ursprungs schieben 
    if ( lbo_KoordinatenVerschieben( &e[0], p, -1 ) == -1 )
      return 0; //x-xmin und y-ymin
    
    // Punkte nach Bastiansennummer sortieren
    for( i = 0; i < 2; i++ )
      lbo_QuickSortStructPointNachNummer(1,p[i][0].n-1,p[i]);
    
    // Auf Grund der Punktsortierung schreibt den Eckpunkten des Dreiecks laufende Punktnummer:
    // t.i1,t.i2,t.i3;
    for( i = 0; i < 2; i++ )
    {
      if( lbo_DreiecksLaufendeNummer(p[i],t[i]) != SUCCESS )
        return 0;
    };
    
    // Parameter t.xc, t.yc, t.area der Structur t berechnen;
    // Orientiert Dreiecke gegen den Uhrzeigersinn
    // Berechnet Parameter für a1,a2,a3:
    // a.bot,a.top,a.lft=nn (Dreiecksnummer), a.rgt=0,a.xc,a.yc
    for ( i = 0; i < 2; i++ )
    {
      if( lbo_TriangleArea(p[i],t[i], 1 ) != SUCCESS )
        return 0;
    };
    
    for( i = 0; i < 2; i++ )
    {
      if( lbo_RadiusVektorDesDreiecks( p[i], t[i] ) != SUCCESS )
        return 0;
    };
    // Findet in jedem Dreieck maximale X-, Y- und Z-Komponente
    // aus drei Vektoren, die den Schwerpunkt mit Ecken verbinden.
    
    if( lbo_PunktProjektionen( p, t ) != SUCCESS )
      return 0;
    // Die Koordinaten < p > werden nach X
    // sortiert und auf ein entsprechendes Dreieck
    // der anderen Eben projeziert. Man muss weiter nur
    // die inneren Punkte bearbeiten,
    // fuer die gilt p->Pz != HUGE_VAL
    // Punkte werden nach der Bastiaansen-Nummer sortiert 
    
    if( getVerboseLevel() > 0 )
    {
      lbo_ShapePunkte( outputPath, "-gelesene-Punkte-oben", p[0], &e[0]);
      lbo_ShapePunkte( outputPath, "-gelesene-Punkte-unten", p[1], &e[0]);
      lbo_ShapeFlaeche( outputPath, "-gelesene-Flaeche-oben", p[0], t[0], &e[0]);
      lbo_ShapeFlaeche( outputPath, "-gelesene-Flaeche-unten", p[1], t[1], &e[0]);
    };
    
    
    int MaxTn = t[0][0].n > t[1][0].n ? t[0][0].n : t[1][0].n; // Maximale Anzahl von Dreiecken
    a = allocateArcs( 3 * MaxTn, 3 * MaxTn );
    
    for ( i = 0; i < 2; i++ )
    {
      if( lbo_KantenDoppelt( t[i], a[i], 0 ) != SUCCESS )
        return 0;
    };
    // Bildet 3 Kanten fuer jedes (gültige) Dreieck.
    // Es gibt vorlaeufig Doppelkanten innerhalb der Gebiete
    // Fuer Kanten wird dreimal mehr Platz angefordert als fuer Dreiecke
    if( getVerboseLevel() > 6 )
    {
      if (a[0][0].n > 1)
        lbo_ShapeLinie( outputPath, "-gelesene-Kanten-doppelt-oben", p[0], a[0], &e[0] );
      if (a[1][0].n > 1)
        lbo_ShapeLinie( outputPath, "-gelesene-Kanten-doppelt-unten", p[1], a[1], &e[0] );
    };
    
    for (i = 0; i < 2; i++)
      lbo_QuickSortKantenNachX(1,a[i][0].n-1,a[i]); 
    // Sortiert alle Kanten nach a.xc
    
    for (i = 0; i < 2; i++)
      lbo_KantenEinzelnNachX( a[i], 1 );
    // Doppelte Kannten wurden eliminiert und geprüft, ob mehrfache Kanten vorhanden sind
    
    if( getVerboseLevel() > 4 )
    {
      if (a[0][0].n > 1)
        lbo_ShapeLinie( outputPath, "-gelesene-Kanten-einzeln-oben", p[0], a[0], &e[0]);
      if (a[1][0].n > 1)
        lbo_ShapeLinie( outputPath, "-gelesene-Kanten-einzeln-unten", p[1], a[1], &e[0]);
    };
    
    
    stepObsProcess();
    
    ////////////////////////////////////////////////////////////////////////////////////
    // 3. Teil: beide Triangulationen auf die Schnittmenge reduzieren
    //         
    ////////////////////////////////////////////////////////////////////////////////////
    // Input: t und p und a
    // Output: 
    ////////////////////////////////////////////////////////////////////////////////////
    
    log( 0, "\n3.Teil : Berechnung der Schnittmenge der Projektionsgebiete\n" );
    setObsText( "Teil 3: Schnittmenge der Flaechen wird berechnet." );
    
    
    g = allocateArcs( a[0][0].n + 10, a[1][0].n + 10 ); // alle Grenzkanten: maximal soviele wie alle Kanten
    m_arcCapas[0] = a[0][0].n + 10;
    m_arcCapas[1] = a[1][0].n + 10;
    
    for (i = 0; i < 2; i++)
    {
      if( lbo_GrenzKanten(g[i],a[i]) != SUCCESS )
        return 0;
    };
    // Berechnet (d.h. Kopiert) Grenzkanten

    // die anderen Kanten gleich freigeben
    freeArcs( a );
    a = NULL;
    
    for(i = 0; i < 2; i++)
    {
      lbo_QuickSortKantenNachX(1,g[i][0].n-1,g[i]); // Sortiert nach g.xc
    };
    
    MaxMinArc ea[2]; 
    
    for (i = 0; i < 2; i++)
    {
      if( lbo_MaxKantenLaenge(g[i],p[i],&ea[i]) != SUCCESS )
        return 0;
    };
    // Berechnet maximale Komponente(n) der Menge von Vektoren(Grenzkanten)
    
    if( getVerboseLevel() > 4 )
    {
      if (g[0][0].n > 1)
        lbo_ShapeLinie( outputPath, "-grenzkanten-oben", p[0], g[0], &e[0] );
      if (g[1][0].n > 1)
        lbo_ShapeLinie( outputPath, "-grenzkanten-unten", p[1], g[1], &e[0] );
    };
    
    if ( lbo_NeuePunkteUndKanten( p, g, e, ea ) != SUCCESS )
      return 0;
    // Berechnet Schnittpunkte der Grenzen 
    // von zwei verschiedenen Flaechen
    // Konstruiert neue Kanten in zwei Ebenen
    
    if( getVerboseLevel() > 4 )
    {
      if (g[0][0].n > 1)
        lbo_ShapeLinie( outputPath, "-neue-Grenzkanten-oben", p[0], g[0], &e[0] );
      if (g[1][0].n > 1)
        lbo_ShapeLinie( outputPath, "-neue-Grenzkanten-unten", p[1], g[1], &e[0] );
      
      lbo_ShapePunkte( outputPath, "-neue-Punkte-oben", p[0], &e[0] );
      lbo_ShapePunkte( outputPath, "-neue-Punkte-unten", p[1], &e[0] );
    };
    
    //for(i=0; i<2; i++) if(lbo_KleineKantenDerGrenzeWeg(g[i],p[i]) != SUCCESS) return -1;  
    //Kleine Kanten wurden aus der Grenzlinie abfiltriert.
    
    for (i = 0; i < 2; i++)
      lbo_QuickSortKantenNachX( 1, g[i][0].n - 1, g[i] ); // Sortiert nach g.xc 
    
    for( i = 0; i < 2; i++)
    {
      if( lbo_MaxKantenLaenge( g[i], p[i], &ea[i] ) != SUCCESS )
        return 0;
    };
    // Berechnet maximale Komponente der Menge von Vektoren(Grenzkanten)
    
    for( i = 0; i < 2; i++ )
    {
      if( lbo_OuterArcsAway( g[i],p[i] ) != SUCCESS )
        return 0;
    };
    //Die Kanten, die die Schnittmenge nicht bergrenzen, werden eliminiert
    
    if( getVerboseLevel() > 4 )
    {
      if (g[0][0].n > 1)
        lbo_ShapeLinie( outputPath, "-innere-Grenzkanten-oben", p[0], g[0], &e[0] );
      if (g[1][0].n > 1)
        lbo_ShapeLinie( outputPath, "-innere-Grenzkanten-unten", p[1], g[1], &e[0] );
    };
    
    MaxMinTriangle et[2];
    
    for ( i = 0; i < 2; i++ )
    {
      if( lbo_ExtremDreiecksWerte( t[i], &et[i] ) != SUCCESS )
        return 0;
    };
    // Maximale Dreiecksnummer und den maximale Elipsoidhuelle gefunden.
    
    for(i = 0; i < 2; i++)
    {
      if ( g[i][0].n > 1 )
        lbo_QuickSortKantenNachX( 1, g[i][0].n-1, g[i] ); // Sortiert nach g.xc 
    };
    
    for(i = 0; i < 2; i++)
    {
      g[i][0].bot = g[i][0].top = g[i][0].lft = g[i][0].rgt = 0;
    };
    
    if( lbo_NeuePunkteUndDreieckeAnDerPolygonGrenze( p, g, t, ea, e, et ) != SUCCESS )
      return 0;
    // Nach g.xc sortierte Kanten werden fuer jedes Dreieck gewaehlt und geprueft, ob sie
    // die Dreiecksseiten schneiden. Neue Punkte und Dreiecke werden konstruiert.
    // Doppelte Punkte erscheinen(!). Sie muessen spaeter abfiltriert werden.

    // g wird jetzt nicht mehr gebraucht
    freeArcs( g );
    g = NULL;
    
    if( getVerboseLevel() > 4 )
    {
      lbo_ShapePunkte( outputPath, "-ganz-neue-Punkte-oben", p[0], &e[0] );
      lbo_ShapePunkte( outputPath, "-ganz-neue-Punkte-unten", p[1], &e[0] );
      
      lbo_ShapeFlaeche( outputPath, "-neue-Dreiecke-oben", p[0], t[0], &e[0] );
      lbo_ShapeFlaeche( outputPath, "-neue-Dreiecke-unten", p[1], t[1], &e[0] );
    };
    
    lbo_ProjektionenDesDreiecksZentrum(t,p); // Voraussetzung für lbo_OuterTrianglAway
    if( lbo_OuterTrianglAway( p, t ) != SUCCESS )
      return 0;
    // Die zu der Schnittmenge nicht gehörenden Dreiecke werden eliminiert 
    
    if( getVerboseLevel() > 2 )
    {
      if (g[0][0].n > 1)
        lbo_ShapeLinie( outputPath, "-Grenze-des-Schnitts-oben", p[0], g[0], &e[0] );
      if (g[1][0].n > 1)
        lbo_ShapeLinie( outputPath, "-Grenze-des-Schnitts-unten", p[1], g[1], &e[0] );
      lbo_ShapeFlaeche( outputPath, "-Schnittfläche-oben", p[0], t[0], &e[0] );
      lbo_ShapeFlaeche( outputPath, "-Schnittfläche-unten", p[1], t[1], &e[0] );
    };
    
    
    stepObsProcess();
    
    /////////////
    // 4. Teil //
    /////////////
    
    log( 0, "4.Teil : Berechnung neuer Auf-und Abtragsdreiecke im Raum\n");
    setObsText( "Teil 4: Die Modelle werden verschnitten." );
    
    for (i = 0; i < 2; i++)
    {
      if(lbo_ExtremDreiecksWerte(t[i],&et[i]) != SUCCESS)
        return 0;
    };
    // die Dreieckextremwerte neu berechnen, da sie durch lbo_NeuePunkteUndDreieckeAnDerPolygonGrenze nicht mehr aktuell sind
    
    for(i = 0; i < 2; i++)
    {
      lbo_QuickSortDreieckeNachX(1,t[i][0].n-1,t[i]); // Sortiert nach t.xc 
    };
    
    if( lbo_AufUndAbtragsPunkteGrenzeDreiecke( p, t, e, et ) != SUCCESS )
      return 0;
    // Findet Schnittstrecke zwischen Dreiecken verschiedener Ebenen.
    // Konstruiert neue Dreiecke und Punkte 
    
    if( lbo_DreieckeMitNegativenNummernWeg(t,et) == -1 )
      return 0;
    // Findet minimale Dreiecksflaeche
    // Eliminiert ueberfluessige Dreiecke aus dem PGM <<lbo_AufUndAbtragsPunkteGrenzeDreiecke>>
    
    MaxTn = t[0][0].n > t[1][0].n ? t[0][0].n : t[1][0].n;
    
    // Kanten neu allockieren
    a = allocateArcs( 3 * MaxTn, 3 * MaxTn );
    
    for(i = 0; i<2; i++)
    {
      if( lbo_Initializierung( 0, NULL, 3 * MaxTn, a[i], 0 , NULL , i ) == -1 )
        return 0;
      a[i][0].n = 1;
    };
    // Die Parameter für a1,a2,a3: a.bot,a.top,a.lft=nn (Dreiecksnummer), a.rgt=0,a.xc,a.yc
    // wurden fuer neue Dreiecke nicht berechnet !!!
    
    for( i = 0; i < 2; i++ )
    {
      if(lbo_TriangleArea(p[i],t[i], 0 ) != SUCCESS)
        return 0;
    };
    // Berechnet Parameter der Structur: t.xc, t.yc, t.area;
    // Orientiert Dreiecke gegen den Uhrzeigersinn
    // Berechnet Parameter für a1,a2,a3:
    //                a.bot,a.top,a.lft=nn (Dreiecksnummer), a.rgt=0,a.xc,a.yc
    
    for(i = 0; i < 2; i++)
    {
      if(lbo_KantenDoppelt(t[i],a[i],0) != SUCCESS)
        return 0;
    };
    // Bildet 3 Kanten fuer jedes Dreieck.
    // Es gibt vorlaeufig Doppelkanten innerhalb der Gebiete
    
    for(i =0; i < 2; i++)
    {
      lbo_QuickSortKantenNachX(1,a[i][0].n-1,a[i]); 
    };
    // Sortiert alle Kanten nach a.xc
    
    for(i = 0; i < 2; i++)
    {
      lbo_KantenEinzelnNachX(a[i]);
    };
    // Doppelte Kannten wurden eliminiert
    
    g = allocateArcs( a[0][0].n + 1, a[1][0].n + 1 );
    m_arcCapas[0] = a[0][0].n + 1;
    m_arcCapas[1] = a[0][0].n + 1;
    
    for(i = 0; i < 2; i++)
    {
      if(lbo_GrenzKanten(g[i],a[i]) != SUCCESS)
        return 0;
    };  // for i
    // Grenzkanten fuer Auf- und Abtragsgebiete sind gefunden
    
    // a wird nicht mehr gebraucht
    freeArcs( a );
    a = NULL;
    
    stepObsProcess();
    
    
    log( 0, "5.Teil : Doppelte Punkte werden abfiltriert\n"); 
    setObsText( "Teil 5: Auftragsflaeche wird ermittelt und gespeichert." );
    
    int MaxPn = p[0][0].n > p[1][0].n ? p[0][0].n : p[1][0].n; // Maximale Anzahl von Punkten
    
    o = (int**)malloc( sizeof( int* ) * 2 );		
    if( o == NULL )
      throw TinCutException( IDS_TINCUT_ERROR_MEM );
    
    o[0] = (int*)malloc( sizeof( int ) * MaxPn );
    o[1] = (int*)malloc( sizeof( int ) * MaxPn );
    if( o[0] == NULL || o[1] == NULL )
      throw TinCutException( IDS_TINCUT_ERROR_MEM );
    
    // Fordert den Speicherplatz fuer Hilfsnummern an
    
    for(i = 0; i < 2; i++)
    {
      if(lbo_Initializierung(p[i][0].n, o[0], 0, NULL, 0, NULL, i ) == -1)
        return 0; // o[i] = i	
      lbo_QuickSortKoordinatenNachX(1, p[i][0].n-1, p[i], o[0]);
      if( lbo_UmkehrNummern(p[i],o[0],o[1]) != SUCCESS )
        return 0;
      int j;
      for (j = 1; j < o[0][0]; j++)
      {
        if(j != o[1][o[0][j]] && 0 != o[1][o[0][j]]) 
        {
          log( 0, "Fehler: Kann nicht sein\n" );
          log( 0, "%d %d\n",j,o[1][o[0][j]] );
          return 0;
        }
      }; // for j
      // Input ins Programm besteht aus Punktnummern o[0] <-> Output - aus o[1]     
      
      if( lbo_DreiecksNummernAenderung(p[i],t[i],o[1]) == -1 )
        return 0; // wegen Sortieren
      if( lbo_GrenzkantenNummernAendern(p[i],g[i],o[1]) == -1 )
        return 0; // wegen Sortieren
      if(lbo_Initializierung(p[i][0].n,o[0],0,NULL,0,NULL, i ) == -1)
        return 0; // o[i] = i	
      // Wir gehen davon aus, daß alle Punkte nach X sortiert wurden
      if( lbo_PunktFiltration(p[i],o[0],i) != SUCCESS )
        return 0;
      if(lbo_DreiecksNummernAenderung(p[i],t[i],o[0]) == -1)
        return 0; // wegen Filtration
      if(lbo_GrenzkantenNummernAendern(p[i],g[i],o[0]) == -1)
        return 0; // wegen Filtration 
    }; // for i
    
    // g wird nicht mehr gebraucht
    freeArcs( g );
    g = NULL;

    // o wird nicht mehr gebraucht
    free( o[0] );
    o[0] = NULL;
    free( o[1] );
    o[1] = NULL;
    free( o );
    o = NULL;

    if( getVerboseLevel() > 2 )
    {
      lbo_ShapePunkte( outputPath, "-neue-Punkte-oben", p[0], &e[0] );
      lbo_ShapePunkte( outputPath, "-neue-Punkte-unten", p[1], &e[0] );
      lbo_ShapeFlaeche( outputPath, "-zerlegte-Fläche-oben", p[0], t[0], &e[0] );
      lbo_ShapeFlaeche( outputPath, "-zerlegte-Fläche-unten", p[1], t[1], &e[0] );
    };

    // Jetzt haben die Dateien keine doppelten Punkte und kein Dreieck der Auftragsflaeche
    // schneidet sich mit den Dreiecken der Abtragsflaeche und umgekehrt. Alle Auftragsdreiecke
    // liegen hoeher als die Abtragsdreiecke...
    
    lbo_ProjektionenDesDreiecksZentrum(t,p);
    // In diesem Programm werden die Projektionen des Dreieckszentrum auf die andere Ebene 
    // gefunden : zc -> zP.
    
    for(i = 0; i < 2; i++)
    {
      // Diese Funktion sucht die Auftrags- und Abtragsdreiecke  
      if(lbo_AbUndAuftragsDreiecke(p[i],t[i],i) == -1)
        return 0;
    };
    
    for( i = 0; i < 2; i++ )
    {
      if( lbo_TriangleArea( p[i],t[i], 0 ) != SUCCESS)
        return 0;
    };
    // Berechnet Parameter der Structur: t.xc, t.yc, t.area;
    // Orientiert Dreiecke gegen den Uhrzeigersinn
    // Berechnet Parameter für a1,a2,a3:
    //                a.bot,a.top,a.lft=nn (Dreiecksnummer), a.rgt=0,a.xc,a.yc   
    
    // Berechnung jetzt Fertig, mann muss nur noch die Dreiecke einer ebene gleichen
    // Vorzeichens aussortieren und das ist das Ergebnis
    if( getVerboseLevel() > 1 )
    {
      lbo_ShapeAuftragAbtrag( outputPath, "-oben", p[0], t[0], &e[0] );
      lbo_ShapeAuftragAbtrag( outputPath, "-unten", p[1], t[1], &e[0] );
    };
    
    
    // der neue workaround : die triangulierte Abtragsfläche ausgeben
    // als 2d und als 3d ausgeben
    if( b3D == true )
      fErgebnis = ( lbo_ShapeFlaeche( outputPath, "-3D", p[1], t[1], &e[0], -1, false ) == 0 );

    fErgebnis = ( lbo_ShapeFlaeche( outputPath, "", p[1], t[1], &e[0], -1, true ) == 0 );
    
    stepObsProcess();
    
    /*  
    // hier kommt jetzt noch eine Überprüfung der Ergebnisse
    log( 0, "\n");
    int UpDown; 
    polygon q[2];
    char string[256]; 
    int von[2], bis[2];
    double Parea,volumen;
    Parea = volumen = 0.;
    
      MaxTn=t[0][0].n > t[1][0].n ? t[0][0].n : t[1][0].n;
      int fErgebnis = FALSE;
      
        for(UpDown = -1; UpDown < 2; UpDown++)
        { 
        char helpStr[100];
        if (verbose > 0)
        {
        if(UpDown==-1) ausgabe( 0, "\nAuftragsvolumen\n"); 
        if(UpDown== 0) ausgabe( 0, "\nVolumendifferenz\n"); 
        if(UpDown== 1) ausgabe( 0, "\nAbtragsvolumen\n"); 
        };
        
          for(i = 0; i < 2; i++)
          {
          if(lbo_Initializierung(0,NULL,3*MaxTn,a[i],0,NULL) == -1) 
          return FALSE;
          a[i][0].n = 1;
          };
          
            // hier werden tatsächlich die Grenzen der Abtrags- / Auftragsgebiete gefunden (durch den subtilen 3. Parameter (fischt i.A. nur Dreiecke mit positiver Bastiansennummer raus)
            if( lbo_KantenDoppelt( t[0],a[0],-UpDown ) == -1 )
            return FALSE;
            if( lbo_KantenDoppelt( t[1],a[1], UpDown ) == -1 )
            return FALSE;
            
              if ( verbose > 6 )
              {
              sprintf( helpStr, "-Teil6-Grenzen-doppelt-%2d-oben", UpDown );
              if (a[0][0].n > 1)
              lbo_ShapeLinie(argv[3], helpStr, p[0], a[0], &e[0]);
              sprintf( helpStr, "-Teil6-Grenzen-doppelt-%2d-unten", UpDown );
              if (a[1][0].n > 1)
              lbo_ShapeLinie(argv[3], helpStr, p[1], a[1], &e[0]);
              };
              
                for(i = 0; i < 2; i++)
                lbo_QuickSortKantenNachX(1,a[i][0].n-1,a[i]); 
                
                  for(i = 0; i < 2; i++)
                  lbo_KantenEinzelnNachX(a[i]);
                  
                    if (verbose > 5)
                    {
                    sprintf( helpStr, "-Teil6-Grenzen-einzeln-%2d-oben", UpDown );
                    if (a[0][0].n > 1)
                    lbo_ShapeLinie(argv[3], helpStr, p[0], a[0], &e[0]);
                    sprintf( helpStr, "-Teil6-Grenzen-einzeln-%2d-unten", UpDown );
                    if (a[1][0].n > 1)
                    lbo_ShapeLinie(argv[3], helpStr, p[1], a[1], &e[0]);
                    };
                    
                      for(i = 0; i < 2; i++) 
                      {
                      if(lbo_GrenzKanten(g[i],a[i]) == -1)
                      return FALSE;
                      };
                      
                        if (verbose > 4)
                        {
                        sprintf( helpStr, "-Teil6-Grenzen-g-%2d-oben", UpDown );
                        if (g[0][0].n > 1)
                        lbo_ShapeLinie(argv[3], helpStr, p[0], g[0], &e[0]);
                        sprintf( helpStr, "-Teil6-Grenzen-g-%2d-unten", UpDown );
                        if (g[1][0].n > 1)
                        lbo_ShapeLinie(argv[3], helpStr, p[1], g[1], &e[0]);
                        };
                        
                          von[0] = von[1] = 1;
                          bis[0] = g[0][0].n-1;
                          bis[1] = g[1][0].n-1;
                          
                            if( lbo_VolumenUndFlaechen(FALSE,von,bis,g,p,t,q,e,-UpDown) == SUCCESS )
                            {
                            if(fabs(q[0].Parea-q[1].Parea) > eps)
                            {
                            //ausgabe( 0, "Bezugsflaechen falsch berechnet\n"); return -1;
                            }
                            else 
                            {
                            if(UpDown==-1) 
                            {
                            Parea=Parea+q[0].Parea;
                            volumen=volumen-q[1].volume + q[0].volume;
                            }
                            else if(UpDown== 0) 
                            {
                            Parea=Parea-q[0].Parea;
                            volumen=volumen+q[1].volume - q[0].volume;
                            }
                            else if(UpDown== 1)
                            {
                            Parea=Parea+q[0].Parea;
                            volumen=volumen-q[1].volume + q[0].volume;
                            if(fabs(Parea) > eps) 
                            {
                            //ausgabe( 0, "Bezugsflaeche falsch berechnet\n"); return -1;
                            }
                            if(fabs(volumen) > eps) 
                            {
                            //ausgabe( 0, "Volumen nicht praezis\n"); return -1;
                            }
                            }
                            if (verbose > 0)
                            {
                            for( i = 0; i < 2; i++ )
                            {
                            sprintf(string,"%d.Ebene\nBezugsflaeche=%f qm\n      Flaeche=%f qm\n      Volumen=%f cbm\n      Umfang=%f m\n",i,-q[i].Parea,q[i].area,-q[i].volume,q[i].perimeter);
                            ausgabe(0,string); 
                            } 
                            };
                            }; // if fabs ...
                            }; // if lbo_VolumenUndFlaeche
                            
                              // 1 <= o[i] < o[0]   sind Polygonnummern. o[i]=0 bedeutet den Uebergang zum nechsten
                              // Polygon oder das Ende. 
                              int grenze0 = -1;
                              int grenze1 = -1;
                              if ( UpDown == 1 )
                              {
                              grenze0 = lbo_GrenzLinieAusKanten(g[0],o[0],p[0]);
                              grenze1 = lbo_GrenzLinieAusKanten(g[1], o[1], p[1]);
                              
                                if (verbose > 4)
                                {
                                sprintf( helpStr, "-Teil6-Grenzen-g-%2d-sortiert-oben", UpDown );
                                if (g[0][0].n > 1)
                                lbo_ShapeLinie(argv[3], helpStr, p[0], g[0], &e[0]);
                                sprintf( helpStr, "-Teil6-Grenzen-g-%2d-sortiert-unten", UpDown );
                                if (g[1][0].n > 1)
                                lbo_ShapeLinie(argv[3], helpStr, p[1], g[1], &e[0]);
                                };
                                };
                                
                                  if ( grenze1 != -1 )
                                  {
                                  if ( UpDown == 1 )
                                  {
                                  lbo_ShapeGrenzlinien( argv[3], "", g[1], o[1], p[1], &e[0], TRUE );
                                  fErgebnis = TRUE;
                                  };
                                  }
                                  else if( grenze0 != -1 )
                                  {
                                  if ( UpDown == 1 )
                                  {
                                  lbo_ShapeGrenzlinien( argv[3], "", g[0], o[0], p[0], &e[0], TRUE );
                                  fErgebnis = TRUE;
                                  };
                                  }
                                  
                                    
                                      if (verbose > 0)
                                      {
                                      if (UpDown == -1)
                                      {
                                      if ( grenze0 != -1 )
                                      lbo_ShapeGrenzlinien(argv[3], "-Auftragsgrenze-oben", g[0], o[0], p[0], &e[0]);
                                      if ( grenze1 != -1 )
                                      lbo_ShapeGrenzlinien(argv[3], "-Auftragsgrenze-unten", g[1], o[1], p[1], &e[0]);
                                      }
                                      else if (UpDown == 1)
                                      {
                                      if ( grenze0 != -1 )
                                      lbo_ShapeGrenzlinien(argv[3], "-Abtragsgrenze-oben", g[0], o[0], p[0], &e[0]);
                                      if ( grenze1 != -1 )
                                      lbo_ShapeGrenzlinien(argv[3], "-Abtragsgrenze-unten", g[1], o[1], p[1], &e[0]);
                                      };
                                      sprintf(string,"Volumendifferenz(Ebene 1 - Ebene 0) = %f cbm\n\n", -q[1].volume + q[0].volume);
                                      ausgabe(0, string);
                                      };
                                      }; //for(UpDown=-1; UpDown < 2; UpDown++) { 
          */

    
  }
  catch( TinCutException e )
  {
    log( 0, getIDString( e.getErrorCode() ) );
    fErgebnis = 0;
  }; // try-catch


  // allen Speicher wieder freigeben falls nicht schon geschehen
  if( p != NULL )
  {
    free( p[0] );
    p[0] = NULL;
    free( p[1] );
    p[1] = NULL;
  }; // if p 
  free( p );
  p = NULL;

  if( t != NULL )
  {
    free( t[0] );
    t[0] = NULL;

    free( t[1] );
    t[1] = NULL;
  }; // if t
  free( t );
  t = NULL;

  freeArcs( a );
  a = NULL;
  freeArcs( g );
  g = NULL;
  if( o != NULL )
  {
    free( o[0] );
    o[0] = NULL;
    free( o[1] );
    o[1] = NULL;
  };
  free( o );
  o = NULL;
  
	 
  if ( fErgebnis == 1 )
    log( 0, "Flaechen erfolgreich verschnitten\n"); 
  else
    log( 0, "Flaechen konnten nicht verschnitten werden\n" );
  
  time_t stopTime;
  time( &stopTime );
  
  time_t elapsedTime = stopTime - startTime;
  tm* elapsedTm = gmtime( &elapsedTime );
  
  char buffer[1000];
  strftime( (char*)&buffer, 1000, "TinCut lief %H Stunden %M Minuten und %S Sekunden.", elapsedTm );

  log( 0, buffer );

  return fErgebnis;
}; // cut

TinCut::Arc** TinCut::allocateArcs( unsigned int size0, unsigned int size1 )
// Reserviert Speicher für die Kanten-Daten
// Parameter:
//        unsigned int size: die Anzahl der zu allokierenden Kanten pro Model
// Rückgabewert:
//        Arc**: das allokierte Objekt
// Throws 
//    TinCutException wenn der Speicher nicht allokiert werden konnte
{
  Arc** arcs = (Arc**)malloc( sizeof( Arc* ) * 2 );
  if( arcs == NULL )
    throw TinCutException( IDS_TINCUT_ERROR_MEM );
  
  arcs[0] = (Arc *)malloc( sizeof( Arc ) * size0 );
  arcs[1] = (Arc *)malloc( sizeof( Arc ) * size1 );
  
  if( arcs[0] == NULL || arcs[1] == NULL )
    throw TinCutException( IDS_TINCUT_ERROR_MEM );
  
  log( 1, "Platz für %d und %d Kanten reserviert\n", size0, size1 ); 
  
  return arcs;
}; // allocateArcs

void TinCut::freeArcs( Arc** a )
{
  if( a != NULL )
  {
    free( a[0] );
    a[0] = NULL;

    free( a[1] );
    a[1] = NULL;
  };
  free( a );
}; // freeArcs

int TinCut::lbo_input( const std::string& upperHmo, const std::string& lowerHmo, Point** p, Triangle** t )
{
  char d1, d2;
  int j, r;
  
  std::vector<const std::string*> strings;
  strings.push_back( &upperHmo );
  strings.push_back( &lowerHmo );
  
  const int BCE_P = 14928;  // = 'P' + 256 * ':'
  const int BCE_D = 14916;  // = 'D' + 256 * ':'
  
  for( j = 0; j < 2; j++ )
  {
    int fd = open( strings[j]->c_str(), O_RDONLY );
    if( fd == -1 )
    {
      log( 0, "%s: %s\n", strings[j]->c_str(), strerror( errno ) );
      return -1;
    }
    
    FILE* file;
    if( ( file = _fdopen( fd, "r") ) == NULL )
    {
      log( 0, "Input-Datei %s nicht geoeffnet\n", strings[j]->c_str() );
      return -1;
    }
    else
    {
      if(j == 0)
        log( 0, "\n");
      log( 0, "%d.Input-Datei %s\n", j + 1, strings[j]->c_str() );
    }
    
    int i = 1;
    int e = 1;
    
    char in[81];
    while( fgets( in, 80, file ) != NULL )
    {
      // sicherstellen, dass genug Speicher für Punkte und Dreiecke da ist
      allocatePoint( &p[j], j, i + 1 );
      allocateTriangle( &t[j], j, e + 1 );
      
      switch( in[0] + 256 * in[1] )
      {
      case( BCE_P ):
        {
          p[j][i].n = INT_MAX;
          p[j][i].x = p[j][i].y = p[j][i].z = p[j][i].Pz=HUGE_VAL;
          
          r = sscanf( in, "%c%c%d%lf%lf%lf", &d1, &d2, &p[j][i].n, &p[j][i].x, 
            &p[j][i].y, &p[j][i].z );
          
          if( r != 6 || p[j][i].n  == INT_MAX || p[j][i].x==HUGE_VAL ||
            p[j][i].y  == HUGE_VAL || p[j][i].z == HUGE_VAL) 
          {
            log( 0, "\nFehler beim Lesen von Punkt %d\n", i );
            return -1;
          }
          
          i++;
        }
        break;
        
      case( BCE_D ):
        {
          t[j][e].n = t[j][e].i1 = t[j][e].i2 = t[j][e].i3 = INT_MAX;
          
          r = sscanf( in, "%c%c%d%d%d%d", &d1, &d2, &t[j][e].n, &t[j][e].i1, 
            &t[j][e].i2, &t[j][e].i3 );
          
          if( r != 6 || t[j][e].n == INT_MAX || t[j][e].i1 == INT_MAX ||
            t[j][e].i2 == INT_MAX || t[j][e].i3 == INT_MAX )
          {
            log(0," \nFehler beim Lesen von Dreieck %d\n", e );
            return -1;
          };
          
          t[j][e].xc = t[j][e].yc = HUGE_VAL;
          t[j][e].x = t[j][e].y = t[j][e].area = HUGE_VAL;
          t[j][e].a1.n = t[j][e].a2.n = t[j][e].a3.n = INT_MAX;
          t[j][e].a1.bot = t[j][e].a2.bot = t[j][e].a3.bot = INT_MAX;
          t[j][e].a1.top = t[j][e].a2.top = t[j][e].a3.top = INT_MAX;
          t[j][e].a1.lft = t[j][e].a2.lft = t[j][e].a3.lft = INT_MAX;
          t[j][e].a1.rgt = t[j][e].a2.rgt = t[j][e].a3.rgt = INT_MAX;
          t[j][e].a1.xc = t[j][e].a2.xc = t[j][e].a3.xc = HUGE_VAL;
          t[j][e].a1.yc = t[j][e].a2.yc = t[j][e].a3.yc = HUGE_VAL;
          
          e++;
        }
        break;
      } // switch
    } // while
    
    p[j][0].n = i;
    p[j][0].x = p[j][0].y = p[j][0].z = HUGE_VAL;
    t[j][0].n = e;
    t[j][0].i1 = t[j][0].i2 = t[j][0].i3 = INT_MAX;
    
    fclose( file );
  }; // for j
  return 0;
}; // lbo_input

int TinCut::lbo_MaxMinKoordinaten( int j, MaxMinPoint* e, Point **p )
{
  e[j].xmax=e[j].xmin=p[j][1].x;
  e[j].ymax=e[j].ymin=p[j][1].y;
  e[j].zmax=e[j].zmin=p[j][1].z;
  e[j].nmax=e[j].nmin=p[j][1].n;
  
  for(int i=2; i<p[j][0].n; i++){
    if(p[j][i].x > e[j].xmax) e[j].xmax=p[j][i].x;
    if(p[j][i].x < e[j].xmin) e[j].xmin=p[j][i].x;
    if(p[j][i].y > e[j].ymax) e[j].ymax=p[j][i].y;
    if(p[j][i].y < e[j].ymin) e[j].ymin=p[j][i].y;
    if(p[j][i].z > e[j].zmax) e[j].zmax=p[j][i].z;
    if(p[j][i].z < e[j].zmin) e[j].zmin=p[j][i].z;
    if(p[j][i].n > e[j].nmax) e[j].nmax=p[j][i].n;
    if(p[j][i].n < e[j].nmin) e[j].nmin=p[j][i].n;
  }
  if(e[j].xmax == HUGE_VAL || e[j].xmin == HUGE_VAL || 
	   e[j].ymax == HUGE_VAL || e[j].ymin == HUGE_VAL ||
     e[j].ymax == HUGE_VAL || e[j].ymin == HUGE_VAL ||
     e[j].nmax ==  INT_MAX || e[j].nmin ==  INT_MAX   ) {
	   log(0, "Extremalwerte sind nicht definierbar\n"); return -1;
  }
  
  return 0;
}; // lbo_MaxMinKoordinaten

int TinCut::lbo_KoordinatenVerschieben( MaxMinPoint* e, Point** p,int sign )
{
  if(sign != 1 && sign != -1)
    return -1;
  
  for(int j=0; j<2; j++)
  {
    for(int i=1; i<p[j][0].n; i++)
    {
      if(sign == -1)
      {
        p[j][i].x -= e->xmin;
        p[j][i].y -= e->ymin;
      }
      else
      {
        p[j][i].x += e->xmin;
        p[j][i].y += e->ymin;
      }
    }
  }
  return 0;
}; // lbo_KoordinatenVerschieben


void TinCut::lbo_QuickSortDreieckeNachX( int lt,int rt,Triangle *t )
{
  int i,j; double xc; Triangle h;
  i=lt;  j=rt;  xc=t[(lt+rt)/2].xc;
  do
  {
    while (t[i].xc  < xc) i++; while (xc < t[j].xc ) j--; 
    if (i<=j)
    {
      if(t != NULL) {h=t[i]; t[i]=t[j]; t[j]=h;}
      
      i++; j--;
    }
  } while (i<=j);
  if (lt<j ) lbo_QuickSortDreieckeNachX(lt,j ,t); 
  if (i <rt) lbo_QuickSortDreieckeNachX(i ,rt,t); 
}; // lbo_QuickSortDreieckeNachX

void TinCut::lbo_QuickSortStructPointNachNummer( int lt, int rt, Point* p )
{
  Point s;
  
  int i = lt;
  int j = rt;
  int m = p[( lt + rt ) / 2].n;
  do
  {
    while( p[i].n  < m )
      i++;
    
    while( m < p[j].n ) 
      j--;

    if( i <= j )
    {
      if( p != NULL )
      {
        s = p[i];
        p[i] = p[j];
        p[j] = s;
      }
      
      i++; j--;
    }
  } 
  while( i <= j );
  
  if( lt < j )
    lbo_QuickSortStructPointNachNummer( lt, j, p ); 

  if( i < rt )
    lbo_QuickSortStructPointNachNummer( i, rt, p );
}; // lbo_QuickSortStructPointNachNummer

void TinCut::lbo_QuickSortKantenNachX( int lt, int rt, Arc *a )
// sortiert die Kanten a im Bereich lt bis rt
{
  int i, j;
  double xc;
  Arc h;
  
  i = lt;
  j = rt;
  xc = a[( lt + rt ) / 2].xc;
  do
  {
    while( a[i].xc  < xc )
      i++;
    while( xc < a[j].xc )
      j--;
    if ( i <= j )
    {
      if( a != NULL )
      {
        h = a[i];
        a[i] = a[j];
        a[j] = h;
      }
      
      i++;
      j--;
    }
  } while ( i <= j );
  
  if ( lt < j )
    lbo_QuickSortKantenNachX( lt, j ,a );
  if ( i <rt )
    lbo_QuickSortKantenNachX( i , rt, a );
}; // lbo_QuickSortKantenNachX

void TinCut::lbo_QuickSortKoordinatenNachX( int lt,int rt, Point* p, int* o )
{
  int i, j, m;
  double xP; 
  Point h;
  
  i = lt;
  j = rt;
  xP = p[(lt+rt)/2].x;
  do
  {
    while (p[i].x  < xP)
      i++;
    while (xP < p[j].x )
      j--; 
    if (i<=j)
    {
      h = p[i];
      p[i] = p[j];
      p[j] = h; 
      if(o != NULL)
      {
        m = o[i];
        o[i] = o[j];
        o[j] = m;
      };
      i++; j--;
    };
  } while (i<=j);
  
  if (lt < j ) lbo_QuickSortKoordinatenNachX(lt, j ,p, o);
  if (i < rt) lbo_QuickSortKoordinatenNachX(i, rt, p, o); 
}; // lbo_QuickSortKoordinatenNachX

int TinCut::lbo_DreiecksLaufendeNummer( Point* p, Triangle* t )
{
  int i,i1,i2,i3; 
  
  for(i=1; i<t[0].n; i++) {
    i1=0;
    lbo_QuickFindStructPointNachNummer(&i1,1,p[0].n,t[i].i1,p); if(i1 > 0) t[i].i1=i1;
    else {log(0, "Findet den Punkt nicht\n"); return -1;}
    i2=0;
    lbo_QuickFindStructPointNachNummer(&i2,1,p[0].n,t[i].i2,p); if(i2 > 0) t[i].i2=i2;
    else {log(0, "Findet den Punkt nicht\n"); return -1;}
    i3=0;
    lbo_QuickFindStructPointNachNummer(&i3,1,p[0].n,t[i].i3,p); if(i3 > 0) t[i].i3=i3;
    else {log(0, "Findet den Punkt nicht\n"); return -1;}
  }
  return 0;
}; // lbo_DreiecksLaufendeNummer

int TinCut::lbo_ParameterEinesDreiecks( int i, Point* p, Triangle* t, bool meldung )
// Parameter:
//         bool log: falls TRUE, gibts bei fehlerhaftem Dreieck eine Fehlermeldung
{
  int i1, i2, i3, d;
  double x1, y1, z1, x2, y2, z2, x3, y3, z3;
  
  i1 = t->i1; 
  i2 = t->i2;
  i3 = t->i3;
  
  x1 = p[i1].x; y1 = p[i1].y; z1=p[i1].z;
  x2 = p[i2].x; y2 = p[i2].y; z2=p[i2].z;
  x3 = p[i3].x; y3 = p[i3].y; z3=p[i3].z;
  
  t->xc = ( x1 + x2 + x3 ) / 3.;
  t->yc = ( y1 + y2 + y3 ) / 3.;
  t->zc = ( z1 + z2 + z3 ) / 3.; 
  
  t->area = ( ( x2 - x1 ) * ( y2 + y1 ) + ( x3 - x2 ) * ( y3 + y2 ) + ( x1 - x3 ) * ( y1 + y3 ) ) / 2.;
  if( -epst <= t->area  && t->area <= epst )
  {
    if ( meldung == 1 )
    {
      log( 0, "Das Dreieck mit Nummer %d ist zu klein: Flaeche %lf \n", t->n, t->area );
      log( 1, "x1,x2,x3 = %f %f %f\n",x1,x2,x3);
      log( 1, "y1,y2,y3 = %f %f %f\n",y1,y2,y3);
      
      if( i1 == i2 || i1 == i3 || i2 == i3 )
        log( 0, "Seine Eckpunkte : i1= %d i2= %d i3= %d sind falsch!\n", i1, i2, i3 );
      log( 0, "\n" );
    };
    t->area = 0.0;  // => das Dreieck ist 'nicht gültig'
  };
  
  if(t->area > 0.)
  {
    d = t->i2;
    t->i2 = t->i3;
    t->i3 = d;
    t->area = -t->area;
  };
  
  i1 = t->a1.bot = t->i1;
  i2 = t->a1.top = t->i2;
  t->a1.lft = i;
  t->a1.rgt = 0;
	 
  t->a1.xc = (p[i1].x+p[i2].x)/2.; 
  t->a1.yc = (p[i1].y+p[i2].y)/2.;
  
  i2 = t->a2.bot = t->i2;
  i3 = t->a2.top = t->i3;
  t->a2.lft = i; 
  t->a2.rgt = 0;
  t->a2.xc = (p[i2].x+p[i3].x)/2.;
  t->a2.yc = (p[i2].y+p[i3].y)/2.;
  
  i3 = t->a3.bot = t->i3;
  i1 = t->a3.top = t->i1;
  t->a3.lft = i;
  t->a3.rgt = 0;
  t->a3.xc = (p[i3].x + p[i1].x)/2.;
  t->a3.yc = (p[i3].y + p[i1].y)/2.;
  
  return 0;
}

int TinCut::lbo_TriangleArea( Point* p,Triangle* t, bool meldung )
// Parameter:
//        bool meldung: falls TRUE, bekommen Fehlerhafte Dreiecke eine Fehlermeldung
{
  for(int i = 1;i < t[0].n; i++)
  {
    if(lbo_ParameterEinesDreiecks( i, p, &t[i], meldung ) != SUCCESS)
      return -1;
  };
  return 0;
}; // lbo_TriangleArea

int TinCut::lbo_RadiusVektor( Point* p, Triangle* t )
{
  if (t->area == 0.0)
    return 0; // ungültiges Dreieck
  
  bool isValid = true;
  
  double x1, y1, z1, x2, y2, z2, x3, y3, z3, xc, yc, zc; 
  
  xc = t->xc;
  x1 = p[t->i1].x;
  x2 = p[t->i2].x;
  x3 = p[t->i3].x; 
  
  x1 = fabs(xc - x1);
  x2 = fabs(xc - x2);
  x3 = fabs(xc - x3);
  
  t->x = x1 > x2 ? x1 : x2;
  t->x = t->x > x3 ? t->x : x3;
  
  if(t->x < eps)
  {
    log( 1, "Radiusvektor ist zu klein\n");
    //    return -1;
    //    isValid = FALSE;
  }
  
  yc = t->yc;
  y1 = p[t->i1].y;
  y2 = p[t->i2].y;
  y3 = p[t->i3].y;
  
  y1 = fabs(yc - y1);
  y2 = fabs(yc - y2);
  y3 = fabs(yc - y3);
  
  t->y = y1 > y2 ? y1 : y2;
  t->y = t->y > y3 ? t->y : y3;
  if(t->y < eps)
  {
    log( 1, "Radiusvektor ist zu klein\n");
    //    return -1;
    //    isValid = FALSE;
  }
  
  zc = t->zc;
  z1 = p[t->i1].z;
  z2 = p[t->i2].z;
  z3 = p[t->i3].z;
  
  z1 = fabs(zc - z1);
  z2 = fabs(zc - z2);
  z3 = fabs(zc - z3);
  
  t->z = z1 > z2 ? z1 : z2;
  t->z = t->z > z3 ? t->z : z3;
  
  if( isValid == false )
    t->area = 0.0; 
  
  return 0;
}; // lbo_RadiusVektor

int TinCut::lbo_RadiusVektorDesDreiecks( Point* p, Triangle* t )
{
  for(int i = 1; i < t[0].n; i++)
  {
    if(lbo_RadiusVektor(p,&t[i]) != SUCCESS)
      return -1;
  }
  return 0;
}; // lbo_RadiusVektorDesDreiecks

int TinCut::lbo_PunktProjektionen( Point** p, Triangle** t )
{
  double xl, xr, yl, yr;
  int lt, rt, n, k, r;
  
  for(int i = 0; i < 2; i++)
  {
    k = (i+1)%2;
    //if(k == 1) lbo_time("Projektion : Bezug --> Modell\n");
    //if(k == 0) lbo_time("Projektion : Modell ->  Bezug\n"); 
    
    lbo_QuickSortKoordinatenNachX(1, p[k][0].n-1, p[k], NULL);
    
    for(int j = 1; j < t[i][0].n; j++)
    { 
      if (t[i][j].area == 0.0)
        continue;  // Dreieck ungültig
      
      lt = 1;
      rt = p[k][0].n-1;
      
      xl = t[i][j].xc - t[i][j].x;
      xr = t[i][j].xc + t[i][j].x;
      
      lbo_QuickFindStructPointNachX(&lt, &rt, xl, xr, p[k]);
      if(rt == 0) 
        continue;
      
      yl = t[i][j].yc - t[i][j].y;
      yr = t[i][j].yc + t[i][j].y;
      
      n = (lt+rt)/2;
      while( true )
      {
        if(yl <= p[k][n].y && p[k][n].y <= yr && p[k][n].Pz == HUGE_VAL)
        {
          r = lbo_pointInOutDreieck(&p[k][n], p[i], &t[i][j]);
          if(r == -1)
          {
            log( 1, "Dreieck nn= %d steht Kopf\n",t[i][j].n);
            //           return 0; // -1
          };
          //  if(k==0 && r==1)log("t[i][j].n= %d r= %d p[k][n].Pz= %f\n",
          //	 t[i][j].n,r,p[k][n].Pz); 
        };
        --n;
        if(n < 1 || p[k][n].x < xl) 
          break;
      }; // while TRUE
      
      n = (lt + rt) / 2;
      while( true )
      {
        ++n;
        if(n > p[k][0].n || p[k][n].x > xr) 
          break;
        
        if(yl <= p[k][n].y && p[k][n].y <= yr && p[k][n].Pz == HUGE_VAL)
        {
          r=lbo_pointInOutDreieck(&p[k][n], p[i], &t[i][j]);
          if(r == -1 && r ==1)
          {
            log( 1, "Dreieck nn= %d steht Kopf\n",t[i][j].n); 
            //            return 0; // -1
          };
          //	if(k==0 && r == 1) log("t[i][j].n= %d r= %d p[k][n].Pz= %f\n",
          //		t[i][j].n,r,p[k][n].Pz); 
        } ;
      }; // while TRUE
    }; // for j
    
    lbo_QuickSortStructPointNachNummer(1,p[k][0].n-1,p[k]);
    // Die Funktion wieder sortiert Punkte nach dem Bastiaansen-Nummer
    // Benutzung des Vektors o[i] ist jetzt nicht moeglich
  }; // for i
  return 0;
}; // lbo_PunktProjektionen

int TinCut::lbo_ShapePunkte( const char* ausgabePfad, const char* ausgabeerweiterung, Point* p, MaxMinPoint* e )
// gibt die Punkte als Punkte-Z Shape aus
{
  char dateiPfad[500];
  
  sprintf(dateiPfad, "%s%s", ausgabePfad, ausgabeerweiterung);
  
  ShapeFile shapeFile( dateiPfad, SHPT_POINTZ );
  if ( !shapeFile.GetHandle() )
    return -1;
  
  strcat(dateiPfad, ".dbf");
  DbfFile dbfFile( dateiPfad );
  if( !dbfFile.GetHandle() )
    return -1;
  
  dbfFile.AddField( "Pz", DbfFile::FTDouble, 12, 3 );
  dbfFile.AddField( "n", DbfFile::FTDouble, 12, 3 );
  dbfFile.AddField( "i", DbfFile::FTDouble, 12, 3 );
  
  double eckkoord[3];
  double Pz;
  int nParts = 0; // gesamtzahl der geschriebenen Teile (Parts)
  int count = 0;  // Gesamtzahl der geschriebenen Objekte
  
  for(int i = 1; i < p[0].n; i++) 
  { 
    eckkoord[0] = p[i].x + e->xmin;
    eckkoord[1] = p[i].y + e->ymin;
    eckkoord[2] = p[i].z;
    Pz = p[i].Pz;
    
    if( Pz == HUGE_VAL )
      Pz = -9999.9;
    
    dbfFile.WriteAttribute( count, 0, &Pz );
    
    double attr = (double)p[i].n;
    dbfFile.WriteAttribute( count, 1, &attr );
    
    attr = (double)i;
    dbfFile.WriteAttribute( count, 2, &attr );
    
    shapeFile.WriteVertices( 1, 1, &nParts, eckkoord );
    
    count++;
  }; // for i
  
  shapeFile.Close();
  dbfFile.Close();
  
  return 0;  // return 1 ist fehler
}; // lbo_ShapePunkte

int TinCut::lbo_ShapeLinie( const char* ausgabePfad, const char* ausgabeerweiterung, Point* p, Arc* a, MaxMinPoint* e )
// schreibt Arc's als Linien raus
{
  char dateiPfad[500];
  
  sprintf(dateiPfad, "%s%s", ausgabePfad, ausgabeerweiterung);
  
  ShapeFile shpFile( dateiPfad, SHPT_POLYLINEZ );
  if( !shpFile.GetHandle() )
    return -1;
  
  strcat(dateiPfad, ".dbf");
  DbfFile dbfFile( dateiPfad );
  if(!dbfFile.GetHandle() )
    return -1;
  
  dbfFile.AddField( "i", DbfFile::FTDouble, 10, 3 );
  dbfFile.AddField( "n", DbfFile::FTDouble, 10, 3);
  dbfFile.AddField( "bot", DbfFile::FTDouble, 10, 3);
  dbfFile.AddField( "top", DbfFile::FTDouble, 10, 3);
  dbfFile.AddField( "lft", DbfFile::FTDouble, 10, 3);
  dbfFile.AddField( "rgt", DbfFile::FTDouble, 10, 3);
  dbfFile.AddField( "xc", DbfFile::FTDouble, 10, 3);
  dbfFile.AddField( "yc", DbfFile::FTDouble, 10, 3);
  
  double eckkoord[6];
  int number, bot, top, lft, rgt;
  double xc, yc;
  
  
  int nParts = 0; // gesamtzahl der geschriebenen Teile
  int count = 0;
  int kantenzahl = 0;
  
  for(int i = 1; i < a[0].n; i++) 
  { 
    number = a[i].n;
    bot = a[i].bot;
    top = a[i].top;
    lft = a[i].lft;
    rgt = a[i].rgt;
    xc = a[i].xc;
    yc = a[i].yc;
    
    eckkoord[0] = p[bot].x + e->xmin;
    eckkoord[1] = p[bot].y + e->ymin;
    eckkoord[2] = p[bot].z;
    eckkoord[3] = p[top].x + e->xmin;
    eckkoord[4] = p[top].y + e->ymin;
    eckkoord[5] = p[top].z;
    
    shpFile.WriteVertices( 2, 1, &nParts, eckkoord );
    
    dbfFile.WriteIntegerAttribute( count, 0, i );
    dbfFile.WriteIntegerAttribute( count, 1, number);
    dbfFile.WriteIntegerAttribute( count, 2, bot);
    dbfFile.WriteIntegerAttribute( count, 3, top);
    dbfFile.WriteIntegerAttribute( count, 4, lft);
    dbfFile.WriteIntegerAttribute( count, 5, rgt);
    dbfFile.WriteAttribute( count, 6, &xc );
    dbfFile.WriteAttribute( count, 7, &yc );
    
    count++;
  }; // for i
  
  shpFile.Close();
  dbfFile.Close();
  
  return 0;  // return 1 ist fehler
}; // int lbo_ShapeLinie( char* ausgabePfad, const char* ausgabeerweiterung, Point* p, Arc* a, MaxMinPoint* e )

int TinCut::lbo_ShapeFlaeche( const char* ausgabePfad, const char* ausgabeerweiterung, Point* p, Triangle* t, MaxMinPoint* e, int sort /* = 10 */, bool b2D /* = false */ )
// schreibt Dreiecke t als polygon shape raus
{
  char dateiPfad[500];

  int type = ( b2D == true ) ? SHPT_POLYGON : SHPT_POLYGONZ;
  
  sprintf(dateiPfad, "%s%s", ausgabePfad, ausgabeerweiterung);
  ShapeFile shpFile( dateiPfad, type );
  if(!shpFile.GetHandle() )
    return -1;
  
  strcat(dateiPfad, ".dbf");
  DbfFile dbfFile( dateiPfad );
  if( !dbfFile.GetHandle() )
    return -1;
  
  dbfFile.AddField( "n", DbfFile::FTInteger, 10, 0);
  dbfFile.AddField( "i1", DbfFile::FTInteger, 10, 0);
  dbfFile.AddField( "i2", DbfFile::FTInteger, 10, 0);
  dbfFile.AddField( "i3", DbfFile::FTInteger, 10, 0);
  dbfFile.AddField( "area", DbfFile::FTDouble, 15, 8);
  //  DBFAddField(hDBF, "xc", FTDouble, 11, 4);
  //  DBFAddField(hDBF, "yc", FTDouble, 11, 4);
  //  DBFAddField(hDBF, "zc", FTDouble, 11, 4);
  //  DBFAddField(hDBF, "zP", FTDouble, 11, 4);
  
  int anzKoord = ( b2D == true ) ? 8 : 12;

  double eckkoord[12];

  int dNummer; // Bastiansennummer des akt. Dreiecks
  int i1, i2, i3;
  double area;
  double xc, yc, zc, zP;
  
  int nParts = 0; // gesamtzahl der geschriebenen Teile
  int count = 0;
  int kantenzahl = 0;
  
  for(int i = 1; i < t[0].n; i++) 
  { 
    dNummer = t[i].n;
    i1 = t[i].i1;
    i2 = t[i].i2;
    i3 = t[i].i3;
    area = t[i].area;
    xc = t[i].xc;
    yc = t[i].yc;
    zc = t[i].zc;
    zP = HUGE_VAL;
    zP = t[i].zP;
    
    if (sort != 10)
    { // ginge einfacher mit der signum Funktion für integer
      if (sort < 0 && dNummer > 0)
        continue;
      if (sort > 0 && dNummer < 0)
        continue;
      if (sort == 0 && dNummer != 0)
        continue;
    };
    
    int anzKrd = 0;

    eckkoord[anzKrd++] = p[i1].x + e->xmin;
    eckkoord[anzKrd++] = p[i1].y + e->ymin;
    if( !b2D )
      eckkoord[anzKrd++] = p[i1].z;

    eckkoord[anzKrd++] = p[i2].x + e->xmin;
    eckkoord[anzKrd++] = p[i2].y + e->ymin;
    if( !b2D )
      eckkoord[anzKrd++] = p[i2].z;

    eckkoord[anzKrd++] = p[i3].x + e->xmin;
    eckkoord[anzKrd++] = p[i3].y + e->ymin;
    if( !b2D )
      eckkoord[anzKrd++] = p[i3].z;

    eckkoord[anzKrd++] = p[i1].x + e->xmin;
    eckkoord[anzKrd++] = p[i1].y + e->ymin;
    if( !b2D )
      eckkoord[anzKrd++] = p[i1].z;
    
    shpFile.WriteVertices( 4, 1, &nParts, eckkoord );
    
    dbfFile.WriteIntegerAttribute( count, 0, dNummer );
    dbfFile.WriteIntegerAttribute( count, 1, i1);
    dbfFile.WriteIntegerAttribute( count, 2, i2);
    dbfFile.WriteIntegerAttribute( count, 3, i3);
    dbfFile.WriteAttribute( count, 4, &area );
    
    count++;
  }; // for i
  
  shpFile.Close();
  dbfFile.Close();

  return 0;  // return 1 ist fehler
}; // lbo_ShapeFlaeche

int TinCut::lbo_ShapeAuftragAbtrag( const char* ausgabePfad, const char* ausgabeerweiterung, Point* p, Triangle* t, MaxMinPoint* e )
// sortiert Dreiecke nach positiver negativer Bastiansennummer und schreibt diese dann als shape raus
{
  char dummy[500];
  sprintf(dummy, "-Auftrag%s", ausgabeerweiterung);
  int error1 = lbo_ShapeFlaeche(ausgabePfad, dummy, p, t, e, 1);
  sprintf(dummy, "-Abtrag%s", ausgabeerweiterung);
  int error2 = lbo_ShapeFlaeche(ausgabePfad, dummy, p, t, e, -1);
  return error1 + error2;
}; // lbo_ShapeAuftragAbtrag

int TinCut::lbo_ShapeGrenzlinien( const char* ausgabePfad, char* kommentar, Arc* g, int* o, 
                                 Point* p, MaxMinPoint* e, bool poly )
                                 // schreibt die Auftrags/Abtraggebiete als polygon shapes raus
                                 // Parameter:
                                 //          char* ausgabePfad: Name der Ausgabedatei(en): es werden .shp, .dbf, .shx erzeugt
                                 //          char* kommentar: wird an ausgabePfad angehängt
                                 //          arg* g, int* o, Point* p: die Grenzlinie
                                 //          MaxMinPoint* e: ?? nicht benutzt ??
                                 //          bool poly: falls true, werden die Grenzkanten als Polygon rausgeschrieben
                                 // Rückgabewert:
                                 //            0 kein Fehler
                                 //           -1 Fehler
{
  char dateiPfad[500];
  
  if (o[0] == 1) // Grenze ist leer
    return 0;
  
  sprintf( dateiPfad, "%s%s", ausgabePfad, kommentar );
  
  int shapeType = poly ? SHPT_POLYGONZ : SHPT_POLYLINEZ;
  ShapeFile shpFile( dateiPfad, shapeType );
  if ( !shpFile.GetHandle() )
    return -1;
  
  strcat( dateiPfad, ".dbf" );
  DbfFile dbfFile( dateiPfad );
  if( !dbfFile.GetHandle() )
    return -1;
  
  dbfFile.AddField( "Knoten", DbfFile::FTInteger, 10, 0 );
  dbfFile.AddField( "I", DbfFile::FTInteger, 10, 0 );
  
  double* eckkoord = NULL;
  
  int nParts = 0; // gesamtzahl der geschriebenen Teile
  int objectCount = 0;
  int knotenzahl = 0;
  
  int i = 1;
  while ( i < o[0] )
  {
    while ( o[i] != 0 )
    {
      eckkoord = (double*)realloc(eckkoord, sizeof(double) * 3 * (knotenzahl + 1));
      eckkoord[knotenzahl * 3] = p[o[i]].x + e->xmin;
      eckkoord[knotenzahl * 3 + 1] = p[o[i]].y + e->ymin;
      eckkoord[knotenzahl * 3 + 2] = p[o[i]].z;
      i++;
      knotenzahl++;
    };
    
    shpFile.WriteVertices( knotenzahl, 1, &nParts, eckkoord );
    
    dbfFile.WriteIntegerAttribute( objectCount, 0, knotenzahl );
    dbfFile.WriteIntegerAttribute( objectCount, 1, objectCount + 1 );
    
    objectCount++;
    knotenzahl = 0;
    free(eckkoord);
    eckkoord = NULL;
    i++;
  };
  
  shpFile.Close();
  dbfFile.Close();
  
  return 0;  // return 1 ist fehler
}; // lbo_ShapeGrenzlinien

int TinCut::lbo_KantenDoppelt( Triangle* t, Arc* a, int j, bool nullDreiecke )
// Parameter:
//          bool nulldreiecke: falls TRUE, werden Dreiecke mit Fläche < eps nicht mit
//                              einbezogen
{
  int k=1;
  
  for(int i = 1; i < t[0].n; i++)
  {
    if( j*t[i].n > 0 || ( nullDreiecke && fabs( t[i].area ) < eps * eps ) )
      continue;  
    a[k++] = t[i].a1;
    //    if(k > MEL*3)
    //      return -1; 
    a[k++] = t[i].a2;
    //    if(k > MEL*3)
    //      return -1;
    a[k++] = t[i].a3;
    //    if(k > MEL*3)
    //      return -1;
  }; // for i
  a[0].n = k;
  return 0;
}; // lbo_KantenDoppelt

void TinCut::lbo_KantenEinzelnNachX( Arc* a, int meldung )
// sortiert doppelte Kanten aus
{
  double deltax, deltay;
  
  for( int i = 1; i < a[0].n-1; i++ )
  {
    for( int j = i + 1; j < a[0].n; j++ )
    {
      deltax = a[i].xc - a[j].xc;
      deltay = a[i].yc - a[j].yc;
      
      if( sqrt( deltax * deltax + deltay * deltay ) < eps )
      {
        if ( meldung == 1 && a[i].rgt != 0 )
          log( 0, "Die Kante %d-%d im Dreieck %d ist bereits in den Dreiecken %d und %d vorhanden\n",
          a[j].bot, a[j].top, a[j].lft, a[i].rgt, a[i].lft );
        
        a[i].rgt = a[j].lft;
        a[j].bot = a[j].top = a[j].lft = a[j].rgt = 0;
      };
      if(a[i].xc < a[j].xc - eps)
        break;
    }; // for j
  }; // for i
  
  i=1;
  for (int j = 1; j < a[0].n; j++)
  {
    if(a[j].rgt ==  0 && a[j].bot > 0)
      a[i++] = a[j]; 
  };
  a[0].n=i;
  for(i = 1; i < a[0].n; i++)
    a[i].n = i;
  
  return;
}; // lbo_KantenEinzelnNachX

int TinCut::lbo_GrenzKanten( Arc* g, Arc* a )
// Kopiert Kanten a nach Kanten g
{
  int j = 1;
  
  for( int i = 1; i < a[0].n; i++ )
  {
    if( a[i].bot > 0 )
      g[j++] = a[i];
  };
  g[0].n = j;
  
  //if(g[0].n > MEL) 
  //{
  //  log( 0, "Fuer Grenzkanten war zu wenig Platz angefordert\n" );
  //  return -1;
  //};
  return 0;
}; // lbo_GrenzKanten

int TinCut::lbo_MaxKantenLaenge( Arc* g, Point* p, MaxMinArc *ea )
// initialisiert ea (d.h. setzt ea->xl auf längste Kantenausdehnung in x - Richtung (ebenso für y)
// zusätlich: prüft ob winzige Kanten vorhanden sind und steigt sonst aus
{
  int i, b, t; 
  double xb, xt, yb, yt, xl, yl, d;
  
  b = g[1].bot;
  t = g[1].top;
  xb = p[b].x;
  xt = p[t].x;
  yb = p[b].y;
  yt = p[t].y;
  
  ea->xl = fabs(xt - xb);
  ea->yl = fabs(yt - yb);
  
  for( i = 2; i < g[0].n; i++ )
  {
    b = g[i].bot; 
    t = g[i].top;
    xb = p[b].x;
    xt = p[t].x;
    yb = p[b].y;
    yt = p[t].y;
    
    xl = fabs(xt - xb);
    yl = fabs(yt - yb); 
    d = sqrt(xl * xl + yl * yl);
    
    if( d < eps )
    {
      log( 1, "Kante nicht in Ordnung %f\n",d);
      //return -1;
    };
    if(xl > ea->xl)
      ea->xl = xl;
    if(yl > ea->yl)
      ea->yl = yl;
  }; // for i
  
  return 0;
}; // lbo_MaxKantenLaenge

int TinCut::lbo_HoehePunkteKante( int i, int j, double xb, double yb, double xt, double yt,
                                 double Xb, double Yb, double Xt, double Yt,
                                 double Xx, double Yy, Point **p, Arc **g, MaxMinPoint *e )
                                 // fügt neue Punkte und Kanten ein
                                 //
                                 // Parameter:
                                 //
                                 // Rückgabewert: -1 bei Fehler
{
  // i = Kantennummer der 0.Ebene und j = Kantennummer der 1.Ebene
  
  double zZ,Zz,zb,zt,Zb,Zt;
  int k,n;
  
  Zz = zZ = HUGE_VAL;
  if ( j > 0 )
  {
    Zb = p[1][g[1][j].bot].z;
    Zt = p[1][g[1][j].top].z;
    if( lbo_HoeheInderStrecke( Xb, Yb, Zb, Xt, Yt, Zt, Xx, Yy, &Zz ) != 0)
    {
      log( 0, "Grenzkante %d der 1.Ebene ist ein Doppelpunkt\n", j );
      return -1;
    };
  };
  
  if ( i > 0 )
  {
    zb = p[0][g[0][i].bot].z; 
    zt = p[0][g[0][i].top].z; 
    if( lbo_HoeheInderStrecke( xb, yb, zb, xt, yt, zt, Xx, Yy, &zZ ) != 0)
    {
      log( 0, "Grenzkante %d der 0.Ebene ist ein Doppelpunkt\n", i );
      return -1;
    };
  };
  
  if( i > 0 )
  {  
    // Ein neuer Punkt auf der Ebene 0 wird konstruiert
    n = p[0][0].n;
    p[0][n].x = Xx;
    p[0][n].y = Yy;
    p[0][n].z = zZ;
    p[0][n].Pz = Zz;
    p[0][n].n = ++e[0].nmax;
    p[0][0].n++;
    
    allocatePoint( &p[0], 0, p[0][0].n );
    //    if( p[0][0].n > NKNOT )
    //    {
    //      perror("Punktezahl zu gross\n");
    //      return -1;
    //    };
    
    // Zwei neue Grenzkanten auf der Ebene 0 werden aus einer alten Kante gebildet
    k = g[0][0].n;
    allocateArc( &g[0], 0, g[0][0].n );
    g[0][i].bot = g[0][i].bot;
    g[0][k].top = g[0][i].top;
    g[0][i].top = g[0][k].bot = n;
    g[0][i].lft = g[0][i].rgt = g[0][k].lft = g[0][k].rgt = 0;
    g[0][i].n = 0;
    g[0][k].n = 0;
    g[0][i].xc = ( xb + Xx ) / 2.;
    g[0][i].yc = ( yb + Yy ) / 2.;
    g[0][k].xc = ( Xx + xt ) / 2.;
    g[0][k].yc = ( Yy + yt ) / 2.;
    g[0][0].n++; 
  }; // if i > 0
  
  if( j > 0 )
  {
    // Ein neuer Punkt auf der Ebene 1 wird konstruiert
    n = p[1][0].n;
    p[1][n].x = Xx;
    p[1][n].y = Yy;
    p[1][n].z = Zz;
    p[1][n].Pz = zZ;
    p[1][n].n = ++e[1].nmax;
    p[1][0].n++;
    
    allocatePoint( &p[1], 1, p[1][0].n );
    //if( p[1][0].n > NKNOT ) 
    //{
    //  perror("Punktezahl zu gross\n");
    //  return -1;
    //} 
    
    // Zwei neue Grenzkanten auf der Ebene 1 werden aus einer alten Kante gebildet
    k = g[1][0].n;
    allocateArc( &g[1], 1, g[1][0].n );
    g[1][j].bot = g[1][j].bot; 
    g[1][k].top = g[1][j].top;
    g[1][j].top = g[1][k].bot = n;
    g[1][j].lft = g[1][j].rgt = g[1][k].lft = g[1][k].rgt = 0;
    g[1][j].n = 0; g[1][k].n = 0;
    g[1][j].xc = (Xb + Xx)/2.;
    g[1][j].yc = (Yb+Yy)/2.;
    g[1][k].xc = (Xx + Xt)/2.;
    g[1][k].yc = (Yy + Yt)/2.;
    g[1][0].n++;
    lbo_QuickSortKantenNachX( 1, g[1][0].n-1, g[1] );
  }; // if j > 0
  
  return 0;
}; // lbo_HoehePunkteKante

int TinCut::lbo_NeuePunkteUndKanten( Point** p, Arc** g, MaxMinPoint* e, MaxMinArc* ea )
// Bemerkung: wird ausschließlich einmal in der main aufgerufen
{
  int i, m, n, b0, t0, b1, t1, lt, rt, ix; 
  double xl, xr, s0, s1, xb, xt, yb, yt, Xb, Yb, Xt, Yt, Xx, Yy, Zz = 0.;
  
  s1 = ea[1].xl / 2.;
  
  for( i = 1; i < g[0][0].n; i++ )
  {
    b0 = g[0][i].bot;
    xb = p[0][b0].x;
    yb = p[0][b0].y;
    
    t0 = g[0][i].top;
    xt = p[0][t0].x;
    yt = p[0][t0].y;
    
    s0 = fabs( xb - xt ) / 2.; // wöfür ist das gut?
    xl = g[0][i].xc - s0 - s1;
    xr = g[0][i].xc + s0 + s1;
    
    lt = 1; 
    rt = g[1][0].n - 1;
    lbo_QuickFindEineKantenFuerStrecke( &lt, &rt, xl, xr, g[1] );
    if ( lt == 0 )
      continue; // keine Kante gefunden
    
    n = ( lt + rt ) / 2;
    m = i;
    while( true )
    {
      b1 = g[1][n].bot;
      Xb = p[1][b1].x;
      Yb = p[1][b1].y;
      
      t1 = g[1][n].top;
      Xt = p[1][t1].x;
      Yt = p[1][t1].y; 
      
      ix = lbo_SchnittpunktZweierGeraden( xb, yb, xt, yt, Xb, Yb, Xt, Yt, &Xx, &Yy );
      
      if(ix == 2)
      {
        if(lbo_HoehePunkteKante(0,n,xb,yb,xt,yt,Xb,Yb,Xt,Yt,Xx,Yy,p,g,e) != SUCCESS)
          return -1;
        break;
      };
      
      if(ix == 3)
      {
        if(lbo_HoehePunkteKante(i,0,xb,yb,xt,yt,Xb,Yb,Xt,Yt,Xx,Yy,p,g,e) != SUCCESS)
          return -1;
        i--;
        break;
      };
      
      if(ix == 4)
      {
        if(lbo_HoehePunkteKante(i,n,xb,yb,xt,yt,Xb,Yb,Xt,Yt,Xx,Yy,p,g,e) != SUCCESS)
          return -1;
        i--;
        break;
      }
      --n;
      if(n < 1 || g[1][n].xc < xl) 
        break;
    }; // while true
    
    if( m > i ) // falls ein punkt hinzugefügt wurde, sicherheitshalber nochmal prüfen
      continue; 
    
    n = ( lt + rt ) / 2;
    while( true )
    {
      ++n;
      if(n >= g[1][0].n || g[1][n].xc > xr)
        break;
      
      b1 = g[1][n].bot;
      Xb = p[1][b1].x;
      Yb = p[1][b1].y;
      t1 = g[1][n].top; 
      Xt = p[1][t1].x;
      Yt = p[1][t1].y; 
      
      ix = lbo_SchnittpunktZweierGeraden(xb,yb,xt,yt,Xb,Yb,Xt,Yt,&Xx,&Yy);
      
      if(ix == 2)
      {
        if(lbo_HoehePunkteKante(0,n,xb,yb,xt,yt,Xb,Yb,Xt,Yt,Xx,Yy,p,g,e) != SUCCESS)
          return -1;
        // Eins der Ende der 1. Linienstueck beruert das 2. Linienstueck
        // d.h. 0-Ebene bekommt kein Extrapunkt und keine Extrakante und
        //      1-Ebene hat jetzt ein Punkt und zwei Kanten mehr.
        break;
      };
      
      if(ix==3)
      {
        if(lbo_HoehePunkteKante(i,0,xb,yb,xt,yt,Xb,Yb,Xt,Yt,Xx,Yy,p,g,e) != SUCCESS)
          return -1;
        // Eins der Ende der 2. Linienstueck beruert das 1. Linienstueck
        // d.h. 1-Ebene bekommt kein Extrapunkt und keine Extrakante und
        //      0-Ebene hat jetzt ein Punkt und zwei Kanten mehr.
        // Eine Kante wird auf der Stelle von i-Kante gespeichert und die andere am Ende
        // der Kantenvektor. Kanten werden wieder nach g.xc sortiert.
        i--;
        break;
      };
      
      if(ix==4)
      {
        if(lbo_HoehePunkteKante(i,n,xb,yb,xt,yt,Xb,Yb,Xt,Yt,Xx,Yy,p,g,e) != SUCCESS)
          return -1;
        // Linienstuecke schneiden sich im inneren Bereich
        // d.h. 0-Ebene hat jetzt ein Punkt und zwei Kanten mehr und
        //      1-Ebene hat jetzt ein Punkt und zwei Kanten mehr.
        // Eine Kante der 0-Ebene wird auf der Stelle von i-Kante gespeichert
        // und die andere am Ende der Kantenvektor.
        // Kanten der 1-Ebene werden wieder nach g.xc sortiert.
        i--; 
        break;
      };
    }; // while true
  }; // for i
  
  return 0;
}; // lbo_NeuePunkteUndKanten

int TinCut::lbo_OuterArcsAway( Arc* g, Point* p )
// schmeisst jene Kanten weg, bei welchen ein Punkt bei Pz HUGE_VAL ist
//
// Bemerkung: wird in der main genau zweimal ( an der gleichen Stelle ) aufgerufen
{
  int i,j = 1; 
  
  for(i = 1; i < g[0].n; i++) 
  {
    if( p[g[i].bot].Pz == HUGE_VAL || p[g[i].top].Pz == HUGE_VAL )
      continue;
    
    g[j++] = g[i];
  };
  
  g[0].n = j;
  return 0;
}; // lbo_OuterArcsAway

int TinCut::lbo_ExtremDreiecksWerte( Triangle* t, MaxMinTriangle* et )
// initialisiert et durch Werte aus t
//
// Parameter: Triangle* t: eine Dreiecksliste
//            MaxMinTriangle* et: enthält bei Rückkehr die max. Ausdehnungen 
//                              der Dreiecke ( wird verändert )
{
  et->nmax = -INT_MAX;
  et->xmax = et->ymax = et->zmax = -HUGE_VAL;
  
  for( int i = 1; i < t[0].n; i++)
  {
    if( t[i].n == INT_MAX || t[i].n == 0)
    {
      log( 0 ,"Dreiecksnummer ist falsch\n" );
      return -1;
    }
    if( et->nmax < t[i].n )
      et->nmax = t[i].n;
    if( et->xmax < t[i].x )
      et->xmax = t[i].x;
    if( et->ymax < t[i].y )
      et->ymax = t[i].y;
    if( et->zmax < t[i].z ) 
      et->zmax = t[i].z;
  }
  return 0;
}; // lbo_ExtremDreiecksWerte

int TinCut::lbo_DreieckeUndPunkteVomSchnitt( int Tn,Triangle *t,Point *pbot,Point *ptop,Point *pt,MaxMinPoint *e,MaxMinTriangle *et, int index )
{
  
  int i[3],j,j1,j2,j3,J[3],k,r,i1,i2,i3,i4[3];
  double x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,xo,yo,zo; 
  
  int R[3];
  
  x1=pbot->x; y1=pbot->y; z1=pbot->z; x2=ptop->x; y2=ptop->y; z2=ptop->z;
  
  i[0]=t[Tn].i1; i[1]=t[Tn].i2; i[2]=t[Tn].i3; 
  k=0;
  for(j=0; j<3; j++){
    
    x3=pt[i[ j     ]].x; y3=pt[i[ j     ]].y; z3=pt[i[ j     ]].z;
    x4=pt[i[(j+1)%3]].x; y4=pt[i[(j+1)%3]].y; z4=pt[i[(j+1)%3]].z;
    
    if((r=lbo_SchnittpunktZweierGeraden(x1,y1,x2,y2,x3,y3,x4,y4,&xo,&yo)) == -1) return -1; 
    
    if( r == 4 || r == 2)
    { 
      R[k] = r;
      J[k] = j;
      i4[k] = pt[0].n++; 
      
      allocatePoint( &pt, index, pt[0].n );
      //if( pt[0].n > NKNOT )
      //{
      //  log( 0, "Zuwenig Speicher für Knoten reserviert\n"); 
      //  return -1;
      //}
      
      if(lbo_HoeheInderStrecke(x3,y3,z3,x4,y4,z4,xo,yo,&zo) != SUCCESS) return -1;
      pt[i4[k]].z =zo; 
      if(lbo_HoeheInderStrecke(x1,y1,z1,x2,y2,z2,xo,yo,&zo) != SUCCESS) return -1;
      pt[i4[k]].Pz=zo;
      pt[i4[k]].n=++(e->nmax); pt[i4[k]].x=xo; pt[i4[k]].y=yo;
      k++;
      if(k > 2) 
      {
        log( 0, "Kante kann ein Dreieck nur zweimal schneiden k=%d\n",k);
        for(int ii=0; ii<k; ii++) log( 0, "R %d\n",R[ii]);
        return -1;
      }
    }
	   
  }
  if(k == 0) return 0;
  
  else if(k == 1) {
    
    
    
    // Zwei neue Dreiecke werden konstruiert
    
    j1=J[k-1];
    i1=i[j1]; i2=i[(j1+1)%3]; i3=i[(j1+2)%3];
    
    
    // 1. Dreieck
    
    //   t[Tn    ].n=t[Tn].n;  
    t[Tn    ].i1=i1; t[Tn    ].i2=i4[k-1]; t[Tn    ].i3=i3;
    if(lbo_ParameterEinesDreiecks(Tn,pt,&t[Tn]) != SUCCESS) return -1;
    if(lbo_RadiusVektor(pt,&t[Tn]) != SUCCESS) return -1;
    
    // 2. Dreieck
    
    t[t[0].n].n=++(et->nmax);   t[t[0].n].i1=i2; t[t[0].n].i2=i3; t[t[0].n].i3=i4[k-1];
    if(lbo_ParameterEinesDreiecks(t[0].n,pt,&t[t[0].n]) != SUCCESS) return -1;
    if(lbo_RadiusVektor(pt,&t[t[0].n]) != SUCCESS) return -1;
    t[0].n++;
    
    allocateTriangle( &t, index, t[0].n );
    // if(t[0].n > MEL) {log( 0, "Zuwenig Speicher für Dreiecke reserviert\n"); return -1;}
  }
  
  else if(k == 2) {
    j1=J[k-2]; j2=J[k-1]; j3=(2*(j1+j2))%3;
    
    // 1. Dreieck
    
    //   t[Tn].n=t[Tn].n;
    t[Tn].i1=i4[k-2]; t[Tn].i2=i4[k-1]; t[Tn].i3=i[(j2+1)%3];
    
    if(lbo_ParameterEinesDreiecks(Tn,pt,&t[Tn]) != SUCCESS) return -1;
    if(lbo_RadiusVektor(pt,&t[Tn]) != SUCCESS) return -1;
    
    // 2. Dreieck
    
    t[t[0].n].n=++(et->nmax);
    t[t[0].n].i1=i[j2]; t[t[0].n].i2=i4[k-1]; t[t[0].n].i3=i4[k-2];
    
    if(lbo_ParameterEinesDreiecks(t[0].n,pt,&t[t[0].n]) != SUCCESS) return -1;
    if(lbo_RadiusVektor(pt,&t[t[0].n]) != SUCCESS) return -1;
    t[0].n++;
    
    allocateTriangle( &t, index, t[0].n );
    //if(t[0].n > MEL) {log( 0, "Zuwenig Speicher für Dreiecke reserviert\n"); return -1;}
    
    // 3. Dreieck
    
    t[t[0].n].n=++(et->nmax);
    t[t[0].n].i1=i4[k-2];t[t[0].n].i2=i[j3];t[t[0].n].i3=i[(j3+1)%3];
    
    if(lbo_ParameterEinesDreiecks(t[0].n,pt,&t[t[0].n]) != SUCCESS) return -1;
    if(lbo_RadiusVektor(pt,&t[t[0].n]) != SUCCESS) return -1;
    t[0].n++;
    
    allocateTriangle( &t, index, t[0].n );
    //if(t[0].n > MEL) {log( 0, "Zuwenig Speicher für Dreiecke reserviert\n"); return -1;}
  }
  
  return 0;
}; // lbo_DreieckeUndPunkteVomSchnitt

int TinCut::lbo_DreieckeVomPunkt( int Tn, Triangle* t, Point* pi, Point* pt, MaxMinPoint* e,MaxMinTriangle* et, const int index ) 
{
  int i4;
  i4 = pt[0].n++;
  
  allocatePoint( &pt, index, pt[0].n );
  //if( pt[0].n > NKNOT)
  //{
  // log( 0, "Zuwenig Speicher für Knoten reserviert\n"); 
  //return -1;
  //}
  pt[i4].x=pi->x; pt[i4].y=pi->y; pt[i4].z=pi->Pz; pt[i4].Pz=pi->z; pt[i4].n=++(e->nmax); 
  int i1,i2,i3;
  i1=t[Tn].i1; i2=t[Tn].i2; i3=t[Tn].i3; 
  
  
  //typedef struct Triangle {int n,i1,i2,i3; Arc a1,a2,a3; double xc,yc,zc,zP,x,y,z,area;} Triangle;
  
  
  // 1. Dreieck
  
  // t[Tn    ].n=t[Tn].n; 
  t[Tn    ].i1=i1; t[Tn    ].i2=i2; t[Tn    ].i3=i4;
  if(lbo_ParameterEinesDreiecks(Tn,pt,&t[Tn]) != SUCCESS) return -1;
  if(lbo_RadiusVektor(pt,&t[Tn]) != SUCCESS) return -1;
  
  // 2. Dreieck
  t[t[0].n].n=++(et->nmax);   t[t[0].n].i1=i2; t[t[0].n].i2=i3; t[t[0].n].i3=i4;
  if(lbo_ParameterEinesDreiecks(t[0].n,pt,&t[t[0].n]) != SUCCESS) return -1;
  if(lbo_RadiusVektor(pt,&t[t[0].n]) != SUCCESS) return -1;
  t[0].n++;
  
  allocateTriangle( &t, index, t[0].n );
  //if(t[0].n > MEL) {log( 0, "Zuwenig Speicher für Dreiecke reserviert\n"); return -1;}
  
  // 3. Dreieck
  t[t[0].n].n=++(et->nmax);   t[t[0].n].i1=i3; t[t[0].n].i2=i1; t[t[0].n].i3=i4;
  if(lbo_ParameterEinesDreiecks(t[0].n,pt,&t[t[0].n]) != SUCCESS) return -1;
  if(lbo_RadiusVektor(pt,&t[t[0].n]) != SUCCESS) return -1;
  t[0].n++;
  
  allocateTriangle( &t, index, t[0].n );
  //if(t[0].n > MEL) {log( 0, "Zuwenig Speicher für Dreiecke reserviert\n"); return -1;}
  
  return 0;
}; // lbo_DreieckeVomPunkt

int TinCut::lbo_ArcCutTrngl( int t0i, Arc *g, int Tn, Triangle* t, Point *pg, Point *pt, MaxMinPoint* e, MaxMinTriangle* et, const int index )
{
  int j, jb = 0, jt = 0, rbot, rtop;
  // Tn wird bearbeitet. t[0].n wird vergroessert.
  Point pbot,ptop; pbot=pg[g->bot]; ptop=pg[g->top];
  
  // Findet neue Dreiecke von innerhalbliegenden Kantenenden _
  //                                                          |
  //                                                          V
  rbot =  lbo_pointInOutDreieck(&pbot,pt,&t[Tn]);  // in Dreieck Tn
  if(rbot == 1)
  {
    lbo_DreieckeVomPunkt(Tn,t,&pbot,pt,e,et, index );
    t = this->t[index];
    pt = this->p[index];
    //!!
  }
  else if(rbot ==-1)
    return -1;
  
  rtop = lbo_pointInOutDreieck(&ptop,pt,&t[Tn]);  // in Dreieck Tn
  if(rtop == 1)
  {
    lbo_DreieckeVomPunkt(Tn,t,&ptop,pt,e,et, index);
    t = this->t[index];
    pt = this->p[index];
    //!!
  }
  else if(rtop ==-1)
    return -1;
  
  if(rbot == 1 && rtop == 1) 
    return 0;
  
  if(rbot != 1)
  {
    for(j = t0i; j < t[0].n; j++)
    { 
      if (t[j].area == 0.0)
        continue;
      jb = 0;
      rbot = lbo_pointInOutDreieck(&pbot,pt,&t[j]);//in Dreieck t0i<=j<t[0].n
      if(rbot == 1)
      {
        lbo_DreieckeVomPunkt(j,t,&pbot,pt,e,et, index);
        t = this->t[index];
        pt = this->p[index];
        //!!
        jb = j;
        break;
      } 
      else if(rbot ==-1) 
        return -1;
    }
  }
  
  if(rtop != 1)
  {
    for(j = t0i; j < t[0].n; j++)
    {
      if (t[j].area == 0.0)
        continue;
      jt = 0;
      rtop = lbo_pointInOutDreieck(&ptop,pt,&t[j ]);//in Dreieck t0i<=j<t[0].n
      if(rtop == 1)
      {
        lbo_DreieckeVomPunkt(j,t,&ptop,pt,e,et, index);
        t = this->t[index];
        pt = this->p[index];
        //!!
        jt = j;
        break;
      }
      else if(rtop ==-1)
        return -1;
    }
  }
  
  if(jb > 0 && jt == jb)
    return 0; // Kante liegt auf der Dreiecksseite
  
  // Findet weitere Dreiecke, wenn die Kante schneidet eine Seite des Dreiecks _
  //                                                                            |
  //                                                                            V
  
  if(lbo_DreieckeUndPunkteVomSchnitt(Tn,t,&pbot,&ptop,pt,e,et, index ) == -1)
    return -1;
  t = this->t[index];
  pt = this->p[index];
  


  int tEnde = t[0].n;
  for( j = t0i; j < tEnde; j++ )
  {
	   if(lbo_DreieckeUndPunkteVomSchnitt(j ,t,&pbot,&ptop,pt,e,et, index ) == -1)
       return -1;
     t = this->t[index];
     pt = this->p[index];
  };
	 
  return 0;
}; // lbo_ArcCutTrngl

int TinCut::lbo_KanteIstWeitVomDreieck( Arc* g, Triangle* t, Point* p )
{
  double xd,yd,xg,yg; 
  
  //typedef struct Triangle {int n,i1,i2,i3; Arc a1,a2,a3; double xc,yc,zc,zP,x,y,z,area;} Triangle;
  
  //  Hier wird geprueft, ob das noch nicht geschnittene Dreieck eine bestimmte Kante
  //  ueberhaupt schneiden kann.
  
  xg=p[g->bot].x - p[g->top].x;  if(xg < 0.) xg=-xg;  // X-Projektionslaenge der Kante <g>
  yg=p[g->bot].y - p[g->top].y;  if(yg < 0.) yg=-yg;  // Y-Projektionslaenge der Kante <g>
  
  xd=g->xc - t->xc; if(xd <0.) xd=-xd; // X-Strecke zwischen Dreiecks- und Kantenzentrum
  yd=g->yc - t->yc; if(yd <0.) yd=-yd; // Y-Strecke zwischen Dreiecks- und Kantenzentrum
  
  if(xd > xg/2.+t->x || yd > yg/2.+t->y) return 0; else return 1;	
  // t->x;  X-groeßte Projektionslaenge des Vektors vom Dreieckszentrum zum Eckknoten
  // t->y;  Y-groeßte Projektionslaenge des Vektors vom Dreieckszentrum zum Eckknoten
}; // lbo_KanteIstWeitVomDreieck

int TinCut::lbo_NeuePunkteUndDreieckeAnDerPolygonGrenze( Point** p, Arc** g, Triangle** t, MaxMinArc* ea,
                                                        MaxMinPoint* e, MaxMinTriangle* et )
{
  int i, j, k, n, lt, rt, t0n, t0i;
  double xl, xr;
  Triangle tij;
  
  for( i = 0; i < 2; i++ )
  {
    t0n = t[i][0].n;
    k = (i + 1) % 2;
    
    if ( g[k][0].n <= 1 )
      continue; // Grenze existiert nicht
    
    for(j = 1; j < t0n; j++)
    { 
      tij = t[i][j];
      if (tij.area == 0.0)
        continue;
      
      t0i = t[i][0].n; 
      
      // t0i = Anzahl von Dreiecken bevor j-Dreieck geteilt wird
      
      if(t[i][j].n < 0)
        t[i][j].n = -t[i][j].n;  //else continue;                        
      
      xl = t[i][j].xc - t[i][j].x - ea[k].xl/2.;
      xr = t[i][j].xc + t[i][j].x + ea[k].xl/2.;
      
      lt = 1;
      rt = g[k][0].n-1;
      lbo_QuickFindEineKantenFuerStrecke( &lt, &rt, xl, xr, g[k] );
      if(lt==0)
        continue;
      n = (lt+rt)/2; 
      
      while(true)
      { 
        // PGM : j = aktive Dreiecksnummer; n = aktive Kantennummer 
        
        if(lbo_KanteIstWeitVomDreieck(&g[k][n],&tij,p[k]) == 1) 
        {
          if(lbo_ArcCutTrngl(t0i,&g[k][n],j,t[i],p[k],p[i],&e[i],&et[i], i ) != SUCCESS)
            return -1;
        }
        --n;
        if(n < 1 || g[k][n].xc < xl) 
          break;
      }; // while TRUE
      
      n = (lt+rt)/2; 
      while(true)
      {
        ++n;
        if(n >= g[k][0].n || g[k][n].xc > xr)
          break;
        
        // PGM : j = aktive Dreiecksnummer; n = aktive Kantennummer 
        if(lbo_KanteIstWeitVomDreieck(&g[k][n],&tij,p[k]) == 1)
        {
          if(lbo_ArcCutTrngl(t0i,&g[k][n],j,t[i],p[k],p[i],&e[i],&et[i], i ) != SUCCESS)
            return -1;
        }
      }; // while TRUE
    }; // for j
  }; // for i
  return 0;
}; // lbo_NeuePunkteUndDreieckeAnDerPolygonGrenze 

int TinCut::lbo_ProjektionenDesDreiecksZentrum( Triangle** t, Point**p )
{
  // In diesem Programm werden die Projektionen des Dreieckszentrum auf die andere Ebene 
  // gefunden : zc -> zP. 
  
  double xl,xr,yl,yr;
  int i,j,lt,rt,n,k,r;
  Point q;
  
  for( i = 0; i < 2; i++) 
  {
    for( j = 1; j < t[i][0].n; j++)
    {
      t[i][j].zP = HUGE_VAL;
    }
  }
  
  for( i = 0; i < 2; i++)
  {
    k = (i+1)%2;
    
    lbo_QuickSortDreieckeNachX(1,t[k][0].n-1,t[k]);
    
    for( j = 1; j < t[i][0].n; j++)
    { 
      if (t[i][j].area == 0.0)
        continue;
      
      lt = 1;
      rt = t[k][0].n-1;
      
      xl = t[i][j].xc - t[i][j].x;
      xr = t[i][j].xc + t[i][j].x;
      
      lbo_QuickFindEinDreieckFuerStrecke(&lt,&rt,xl,xr,t[k]);
      if(rt == 0) 
        continue;
      
      yl = t[i][j].yc - t[i][j].y;
      yr = t[i][j].yc + t[i][j].y;
      
      n = (lt + rt) / 2;
      while(true)
      {
        if(yl <= t[k][n].yc && t[k][n].yc <= yr && t[k][n].zP == HUGE_VAL)
        {
          q.x = t[k][n].xc;
          q.y = t[k][n].yc;
          q.z = t[k][n].zc;
          q.Pz = t[k][n].zP;
          
          //printf("%f %f %f\n",q.x,q.y,q.z);
          
          if((r = lbo_pointInOutDreieck(&q,p[i],&t[i][j]))==-1)
          {
            log( 1, "Dreieck nn= %d steht Kopf\n",t[i][j].n); 
            r = 2; //            return 0; // -1
          }
          if(r != 2)
            t[k][n].zP = q.Pz;
        }
        
        --n;
        if(n < 1 || t[k][n].xc < xl) 
          break;
      }; // while TRUE
      
      
      n = (lt + rt)/2;
      while(true)
      {
        ++n;
        if(n > t[k][0].n || t[k][n].xc > xr) 
          break;
        
        if(yl <= t[k][n].yc && t[k][n].yc <= yr && t[k][n].zP == HUGE_VAL)
        {
          q.x = t[k][n].xc; 
          q.y = t[k][n].yc; 
          q.z = t[k][n].zc; 
          q.Pz = t[k][n].zP;
          
          if((r = lbo_pointInOutDreieck(&q,p[i],&t[i][j]))==-1){
            log( 1, "Dreieck nn= %d steht Kopf\n",t[i][j].n); 
            r = 2; //          return 0; // -1
          }
          if(r != 2)
            t[k][n].zP = q.Pz;
        } 
      }; // while TRUE
    }; // for j
  }; // for i
  
     /*
     for(i = 0; i < 2; i++) 
     {
     for(j = 1; j < t[i][0].n; j++)
     {
     if(t[i][j].area != 0.0 && t[i][j].zP==HUGE_VAL)
     {
     log( 0, "Das %d.Dreieck in der %d.Ebene wurde nicht projiziert\n",i,j);
     return -1;
     }
     }
     }; // for i
  */
  
  return 0;
}; // lbo_ProjektionenDesDreiecksZentrum

int TinCut::lbo_OuterTrianglAway( Point** p, Triangle** t ) // , Arc **g,maxminarc *ea )
// entfernt Dreiecke aus t, deren .zP - Wert gleich HUGE_VAL ist
// Parameter:
//        Point** p, Triangle** t: die beiden Dreiecksebenen
//        
{
/* // alt Leibo
int i,j,k,r0,r1,n0,n1; 

  // Funktioniert nur dann, wenn g[] (Grenzstuecke) nach x sortiert sind und wenn die ganze 
  // Grenze gegen den Uhrzeigersinn orientiert ist.
  
    for( j = 0; j < 2; j++ )
    {
    k = 1;
    for( i = 1; i < t[j][0].n; i++ )
    {
    r0 = lbo_PointInOutPolygon( t[j][i].xc, t[j][i].yc, p[0], g[0], &ea[0], &n0 );
    if( r0 == -1 )
    return -1;
    r1 = lbo_PointInOutPolygon( t[j][i].xc, t[j][i].yc, p[1], g[1], &ea[1], &n1 );
    if( r1 == -1 )
    return -1;
    
      if( ( n0 + n1 ) %2 == 0 )
      continue; 
      else 
      t[j][k++] = t[j][i];
      
        }; // for i
        t[j][0].n = k; 
        }; // for j
        
          return 0;
  */
  
  // neu: jetzt unabhängig von den grenzkanten: da garantiert ist, dass sich keine zwei 
  // Dreiecke von oben und unten ueberlappen, reicht es lbo_ProjektionenDesDreiecksZentrum(t,p)
  // aufzurufen um .zP jeden Dreieckes zu errechnen. Dreiecke deren .pZ == HUGE_VAL ist
  // liegen ausserhalb des Schnitts, fertig
  
  for ( int j = 0; j < 2; j++ )
  {
    int count = 1;
    for ( int i = 1; i < t[j][0].n; i++ )
    {
      if ( t[j][i].zP != HUGE_VAL )
        t[j][count++] = t[j][i];
    }; // for i
    t[j][0].n = count;
    
  }; // for j
  return 0;
}; // lbo_OuterTrianglAway

int TinCut::lbo_3TrianglesFromInsidePoint( int Tn, Triangle *tj, Point *po, Point *pj, MaxMinPoint *e,
                                          MaxMinTriangle *et, const int index )
{
  int i4 = pj[0].n++;
  
  allocatePoint( &pj, index, pj[0].n );
  //if( pj[0].n > NKNOT )
  //{
  //  log( 0, "Zu viele Punkte\n" );
  //  return -1;
  //}
  
  pj[i4].x = po->x;
  pj[i4].y = po->y;
  pj[i4].z = po->Pz;
  pj[i4].Pz = po->z;
  pj[i4].n = ++( e->nmax );
  
  int i1 = tj[Tn].i1;
  int i2 = tj[Tn].i2;
  int i3 = tj[Tn].i3;
  
  // testen, ob po einer der Eckpunkte ist, wenn ja, zurück
  if ( ( pj[i4].x == pj[i1].x && pj[i4].y == pj[i1].y ) ||
    ( pj[i4].x == pj[i2].x && pj[i4].y == pj[i2].y ) ||
    ( pj[i4].x == pj[i3].x && pj[i4].y == pj[i3].y ) )
    return 0;
  
  if( fabs(pj[i4].z - pj[i4].Pz) > eps )
  {
    log( 0, "Punkt ist falsch projiziert: %f %f\n", pj[i4].z, pj[i4].Pz );
    return -1;
  }
  
  double area0, area1, area2, area3; 
  area0 = tj[Tn].area;
  
  // 1. Dreieck
  tj[Tn].i1 = i1;
  tj[Tn].i2 = i2;
  tj[Tn].i3 = i4;
  if( lbo_ParameterEinesDreiecks( Tn, pj, &tj[Tn] ) != SUCCESS )
    return -1;
  if( lbo_RadiusVektor( pj, &tj[Tn] ) != SUCCESS )
    return -1;
  
  area1 = tj[Tn].area;
  
  // 2. Dreieck
  tj[tj[0].n].n = ++(et->nmax);
  tj[tj[0].n].i1 = i2;
  tj[tj[0].n].i2 = i3;
  tj[tj[0].n].i3 = i4;
  if( lbo_ParameterEinesDreiecks( tj[0].n, pj, &tj[tj[0].n] ) != SUCCESS )
    return -1;
  if( lbo_RadiusVektor( pj, &tj[tj[0].n] ) != SUCCESS )
    return -1;
  area2 = tj[tj[0].n].area;
  tj[0].n++;
  
  allocateTriangle( &tj, index, tj[0].n );
  //if( tj[0].n > MEL ) 
  //{ 
  //  log( 0, "Zuviele Dreiecke\n");
  //  return -1;
  //}
  
  // 3. Dreieck
  tj[tj[0].n].n = ++(et->nmax);
  tj[tj[0].n].i1 = i3;
  tj[tj[0].n].i2 = i1;
  tj[tj[0].n].i3 = i4;
  if( lbo_ParameterEinesDreiecks( tj[0].n, pj, &tj[tj[0].n] ) != SUCCESS
    ) return -1;
  if( lbo_RadiusVektor( pj, &tj[tj[0].n] ) != SUCCESS )
    return -1;
  area3 = tj[tj[0].n].area;
  tj[0].n++;
  
  allocateTriangle( &tj, index, tj[0].n );
  //if( tj[0].n > MEL )
  //{ 
  //  log( 0, "Zuviele Dreiecke\n"); 
  //  return -1;
  //}
  
  if( area0 - ( area1 + area2 + area3 ) < -eps*eps ||
    area0 - ( area1 + area2 + area3 ) > eps*eps )
  {
    log( 1, "Dreiecke falsch konstruiert\n" ); 
  }
  
  return 0;
} // lbo_3TrianglesFromInsidePoint

int TinCut::lbo_3TrianglesFromBoundaryPoints( int Tn, Triangle* tj, Point* op, Point* pj,
                                             MaxMinPoint* e, MaxMinTriangle* et, int* sd, const int index )
{
  // sd[...] = 0,1,2 entspricht t.i1 t.i2 t.i3
  
  if( sd[2] != -1)
    return -1;
  
  sd[2] = ( 2 * ( sd[0] + sd[1] ) ) % 3; // Findet immer dritte Seite des Dreiecks
  
  int i4 = pj[0].n++;
  
  allocatePoint( &pj, index, pj[0].n );
  // if( pj[0].n > NKNOT ) 
  //{
  //  log( 0, "Zuwenig Speicher für Knoten reserviert\n"); 
  //  return -1;
  //}
  
  pj[i4].x=op[sd[0]].x; pj[i4].y=op[sd[0]].y; pj[i4].z=op[sd[0]].Pz; pj[i4].Pz=op[sd[0]].z;
  pj[i4].n=++(e->nmax); 
  
  if(fabs(pj[i4].z - pj[i4].Pz) > eps) 
  {
    log( 0, "Punkt ist falsch projeziert: %f %f\n",pj[i4].z,pj[i4].Pz); return -1;
  }
  
  int i5 = pj[0].n++;
  
  allocatePoint( &pj, index, pj[0].n );
  //if(pj[0].n > NKNOT) {log( 0, "Zuwenig Speicher für Knoten reserviert\n"); return -1;}
  
  pj[i5].x=op[sd[1]].x; pj[i5].y=op[sd[1]].y; pj[i5].z=op[sd[1]].Pz; pj[i5].Pz=op[sd[1]].z;
  pj[i5].n=++(e->nmax); 
  
  if(fabs(pj[i5].z - pj[i5].Pz) > eps) {
    log( 0, "Punkt ist falsch projeziert: %f %f\n",pj[i5].z,pj[i5].Pz); return -1;
  }
  
  int it[3]; it[0]=tj[Tn].i1; it[1]=tj[Tn].i2; it[2]=tj[Tn].i3;
  
  //typedef struct Triangle {int n,i1,i2,i3; Arc a1,a2,a3; double xc,yc,zc,zP,x,y,z,area;} Triangle;
  
  
  // 1. Dreieck
  
  tj[Tn].i1=i4; tj[Tn].i2=i5; tj[Tn    ].i3=it[sd[0]];
  if(lbo_ParameterEinesDreiecks(Tn,pj,&tj[Tn]) != SUCCESS) return -1;
  if(lbo_RadiusVektor(pj,&tj[Tn]) != SUCCESS) return -1;
  
  // 2. Dreieck
  
  tj[tj[0].n].n=++(et->nmax);
  tj[tj[0].n].i1=i5; tj[tj[0].n].i2=i4; tj[tj[0].n].i3=it[(sd[0]+1)%3];;
  if(lbo_ParameterEinesDreiecks(tj[0].n,pj,&tj[tj[0].n]) != SUCCESS) return -1;
  if(lbo_RadiusVektor(pj,&tj[tj[0].n]) != SUCCESS) return -1;
  
  tj[0].n++;
  
  allocateTriangle( &tj, index, tj[0].n );
  //if(tj[0].n > MEL) { 0, log( 0, "Zuwenig Speicher für Dreiecke reserviert\n"); return -1;}
  
  // 3. Dreieck
  
  tj[tj[0].n].n=++(et->nmax);
  tj[tj[0].n].i1=i5; tj[tj[0].n].i2=it[sd[2]]; tj[tj[0].n].i3=it[(sd[2]+1)%3];
  if(lbo_ParameterEinesDreiecks(tj[0].n,pj,&tj[tj[0].n]) != SUCCESS) return -1;
  if(lbo_RadiusVektor(pj,&tj[tj[0].n]) != SUCCESS) return -1;
  
  tj[0].n++; 
  
  allocateTriangle( &tj, index, tj[0].n );
  //if(tj[0].n > MEL) { 0, log( 0, "Zuwenig Speicher für Dreiecke reserviert\n"); return -1;}
  
  return 0;
}; // lbo_3TrianglesFromBoundaryPoints

int TinCut::lbo_2TrianglesFromBoundaryPoint( int Tn, Triangle* tj, Point* op, Point* pj, MaxMinPoint* e,
                                            MaxMinTriangle* et, int sd, const int index  )
{
  int i4 = pj[0].n++;
  
  allocatePoint( &pj, index, pj[0].n );
  //if(pj[0].n > NKNOT) { 0, log( 0, "Zuwenig Speicher für Knoten reserviert\n"); return -1;}
  
  pj[i4].x=op[sd].x; pj[i4].y=op[sd].y; pj[i4].z=op[sd].Pz; pj[i4].Pz=op[sd].z;
  pj[i4].n=++(e->nmax); 
  
		if(fabs(pj[i4].z - pj[i4].Pz) > eps) {
      log( 0, "Punkt ist falsch projeziert: %f %f\n",pj[i4].z,pj[i4].Pz); return -1;
    }
    int it[3]; it[0]=tj[Tn].i1; it[1]=tj[Tn].i2; it[2]=tj[Tn].i3;
    
    //typedef struct Triangle {int n,i1,i2,i3; Arc a1,a2,a3; double xc,yc,zc,zP,x,y,z,area;} Triangle;
    
    
    // 1. Dreieck
    tj[Tn].i1=i4; tj[Tn].i2=it[(sd+1)%3]; tj[Tn    ].i3=it[(sd+2)%3];
    if(lbo_ParameterEinesDreiecks(Tn,pj,&tj[Tn]) != SUCCESS) return -1;
    if(lbo_RadiusVektor(pj,&tj[Tn]) != SUCCESS) return -1;
    
    // 2. Dreieck
    tj[tj[0].n].n=++(et->nmax);
    tj[tj[0].n].i1=it[(sd+2)%3]; tj[tj[0].n].i2=it[(sd+3)%3]; tj[tj[0].n].i3=i4;
    if(lbo_ParameterEinesDreiecks(tj[0].n,pj,&tj[tj[0].n]) != SUCCESS) return -1;
    if(lbo_RadiusVektor(pj,&tj[tj[0].n]) != SUCCESS) return -1;
    
    tj[0].n++;
    
    allocateTriangle( &tj, index, tj[0].n );
    //if(tj[0].n > MEL) { log( 0, "Zuwenig Speicher für Dreiecke reserviert\n"); return -1;}
    
    return 0;
}; // lbo_2TrianglesFromBoundaryPoint

int TinCut::lbo_StreckenLaenge( Point *p1, Point *p2 )
// testet, ob die Länge der Strecke p1 - p2 kleiner als eps ist oder nicht
//
// Rückgabewert:
//            TRUE, falls die Strecke länger als oder gleich lang wie eps ist
//            FALSE, falls die Strecke kürzer als eps ist
{
  double s;
  s = sqrt( pow( p2->x - p1->x, 2. ) + pow( p2->y - p1->y, 2. ) + pow( p2->z - p1->z, 2. ) ); 
  if( s < eps )
    return 0;
  else 
    return 1;
}; // lbo_StreckenLaenge

int TinCut::lbo_LineCutsSidesOfTheTriangle( int i, Point *pj, Triangle *tj, Point *po, Point *op, int *sd )
{ 
  double x[3],y[3],z[3],b1[3],b2[3],d1[3],d2[3],beta1[3],beta2[3],s[3];
  int j,k,c,r,r1,r2;
  Point p1,p2;
  
  x[0]=pj[tj[i].i1].x; x[1]=pj[tj[i].i2].x; x[2]=pj[tj[i].i3].x;
  y[0]=pj[tj[i].i1].y; y[1]=pj[tj[i].i2].y; y[2]=pj[tj[i].i3].y;
  z[0]=pj[tj[i].i1].z; z[1]=pj[tj[i].i2].z; z[2]=pj[tj[i].i3].z;
  
  b1[0]=po[0].x; d1[0]=po[1].x-po[0].x;
  b1[1]=po[0].y; d1[1]=po[1].y-po[0].y;
  b1[2]=po[0].z; d1[2]=po[1].z-po[0].z;  // beta1 gehoert der Linie << po >>
  
  c=0; sd[0]=sd[1]=sd[2]=-1;
  
  for( k = 0; k < 3; k++ )
  {
    b2[0]=x[k]; d2[0]=x[(k+1)%3]-x[k];
    b2[1]=y[k]; d2[1]=y[(k+1)%3]-y[k];
    b2[2]=z[k]; d2[2]=z[(k+1)%3]-z[k];     // beta2 gehoert der Dreiecksseite
    
    r =  lbo_ILL3(&beta1[k],b1,d1,&beta2[k],b2,d2,s);
    if(r == 1)
    { 
      if( eps <  beta2[k] && beta2[k] < 1.+eps && -eps <  beta1[k] && beta1[k] < 1.+eps   ) 
      { 
        sd[c++] = k;
        op[k].x = s[0];
        op[k].y = s[1];
        op[k].z = op[k].Pz = s[2];
      }
    }
  } // for k
  
  if(c == 0)
    return 0;
  else if(c == 1) 
  {
    // printf("c= %d %d %d %d\n",c,sd[0],sd[1],sd[2]);
    p1.x = x[(sd[0]+0)%3];
    p1.y = y[(sd[0]+0)%3];
    p1.z = z[(sd[0]+0)%3];
    
    p2.x = x[(sd[0]+1)%3]; 
    p2.y = y[(sd[0]+1)%3]; 
    p2.z = z[(sd[0]+1)%3];
    
    r1 = lbo_StreckenLaenge(&p1,&op[sd[0]]);
    r2 = lbo_StreckenLaenge(&p2,&op[sd[0]]); 
    if(r1 == 0 || r2 == 0 ) 
      return 0;
    else {
      // printf("c= %d %d %d %d\n",++count,sd[0],sd[1],sd[2]);
      return 1;
    }
  }
  else if(c == 2) 
  {
    for( j = 0; j < c; j++ )
    {
      p1.x = x[(sd[j]+0)%3];
      p1.y = y[(sd[j]+0)%3];
      p1.z = z[(sd[j]+0)%3];
      p2.x = x[(sd[j]+1)%3]; 
      p2.y = y[(sd[j]+1)%3];
      p2.z = z[(sd[j]+1)%3];
      
      r1 = lbo_StreckenLaenge(&p1,&op[sd[j]]);
      r2 = lbo_StreckenLaenge(&p2,&op[sd[j]]); 
      
      if(r1 == 0 || r2 == 0 ) 
        sd[j] = -1; 
    }
    
    if(sd[0] == -1 && sd[1] == -1) 
      return 0;
    if(sd[0] >= 0 && sd[1] == -1) 
      return 1;
    if(sd[1] >=  0 && sd[0] == -1) 
    { 
      sd[0] = sd[1];
      return 1;
    }
    
    if(sd[0] >=  0 && sd[1] >=  0) 
    {
      //	printf("c= %d %d %d %d\n",c,sd[0],sd[1],sd[2]);
      return 2;
    }
    
    log( 1, "Darf nie sein %d %d %d %d\n",c,sd[0],sd[1],sd[2]);
    return 0;
				
  }
  else 
  { 
    log( 1, "Fehler in lbo_LineCutsSidesOfTheTriangle c= %d\n", c );	
    return 0;
  }
}; // lbo_LineCutsSidesOfTheTriangle

int TinCut::lbo_PointLiesInPlain( int i, Point *pj, Triangle *tj, Point *po )
// berechnet jeweils den Abstand zwischen den durch po gegebenen 3 Punkten
// und dem i. Dreieck in tj / pj
// gibt eine Meldung aus falls dieser zu gross ist ( > 0.001 )
//
// Parameter:
//          int i: Index ( in tj ) des zu testenden Dreiecks
//          Point* pj: Zeiger auf die Punkte einer Ebene
//          Triangle* tj: Zeiger auf die Dreiecke einer ebene
//          Point* po: Zeiger auf 3 ( drei ) Punkte ( diese sind zu testen )
//
// Rückgabewert: ursprünglich -1 bei Fehler ( d.h. abstand zu gross ) jetzt aber stets 0 = kein Fehler
// 
// Bemerkung: wird ausschließlich von lbo_AdditionalPointsAndTriangles aufgerufen
{ 
  int i1, i2, i3, j;
  double x0, y0, z0, zo, x1, y1, z1, x2, y2, z2, x3, y3, z3, nx, ny, nz, k;
  
  i1 = tj[i].i1;
  i2 = tj[i].i2;
  i3 = tj[i].i3;
  
  x1 = pj[i1].x;
  y1 = pj[i1].y;
  z1 = pj[i1].z;
  
  x2 = pj[i2].x;
  y2 = pj[i2].y;
  z2 = pj[i2].z;
  
  x3 = pj[i3].x;
  y3 = pj[i3].y;
  z3 = pj[i3].z;
  
  nx = ( y2 - y1 ) * ( z3 - z2 ) - ( y3 - y2 ) * ( z2 - z1 );
  ny = ( x3 - x2 ) * ( z2 - z1 ) - ( x2 - x1 ) * ( z3 - z2 );
  nz = ( x2 - x1 ) * ( y3 - y2 ) - ( x3 - x2 ) * ( y2 - y1 );
  
  if( -eps*eps < nz && nz < eps*eps)
  {
    log( 1, "Dreieck steht Kopf: nx=%f ny=%f nz=%f\n", nx, ny, nz );
    return 0;
  };
  
  k = nx * x1 + ny * y1 + nz * z1;
  
  for( j = 0; j < 2; j++ )
  {
    x0 = po[j].x;
    y0 = po[j].y;
    z0 = po[j].z;
    
    zo = ( k - nx * x0 - ny * y0 ) / nz;
    
    if( !(-0.001 < z0-zo && z0-zo < 0.001) )
    {
      log( 3,  "%d. Punkt liegt nicht in der Dreiecksschraegebene: z0-zo=%f\n", j, z0-zo );
    }
  }
  
  return 0;
} // lbo_Point_liesInPlain

int TinCut::lbo_AdditionalPointsAndTriangles( int mT, Point *pj, Triangle *tj, Point *po, MaxMinPoint *e,
                                             MaxMinTriangle *et, const int index )
                                             // ?? fügt neue Punkte ein ??
                                             //
                                             // Bemerkung: wird auschließlich von lbo_AufUndAbtragsPunkteGrenzeDreiecke aufgerufen
{
  int i, j, k, r, Tm, rLine, sd[3];
  Point op[3];
  
  j = k = 0;
  
  Tm = tj[0].n;
  for( i = mT; i < Tm; i++ )
  {
    if ( tj[i].area == 0.0 )
      continue;
    if( lbo_PointLiesInPlain( i, pj, tj, po ) == -1 )
      return -1;
    r = lbo_pointInOutDreieck( &po[0], pj, &tj[i] );
    if(r == -1)
      return -1;
    
    if(r == 1)
    {
      if( lbo_3TrianglesFromInsidePoint( i, tj, &po[0], pj, e, et, index ) == -1 )
        return -1;
      tj = this->t[index];
      pj = this->p[index];
      j = i;
      break;
    }
  }; // for i
  
  Tm = tj[0].n;
  for( i = mT; i < Tm; i++ )
  {
    if ( tj[i].area == 0.0 )
      continue;
    if( lbo_PointLiesInPlain(i,pj,tj,po)==-1 )
      return -1;
    r = lbo_pointInOutDreieck( &po[1], pj, &tj[i] );
    if(r == -1)
      return -1;
    if(r == 1) 
    {
      if(lbo_3TrianglesFromInsidePoint( i, tj, &po[1], pj, e, et, index ) == -1 )
        return -1;
      tj = this->t[index];
      pj = this->p[index];
      //!!
      k = i;
      break;
    }	
  }; // for i
  
  //if(k != 0 && (k==j || k == Tm-1 || k == Tm-2)) return 0;
  
  Tm = tj[0].n;
  for( i = mT; i < Tm; i++ )
  {
    if ( tj[i].area == 0.0 )
      continue;
    if( lbo_StreckenLaenge( &po[0], &pj[tj[i].i1] ) == 0 ||
      lbo_StreckenLaenge( &po[0], &pj[tj[i].i2] ) == 0 ||
      lbo_StreckenLaenge( &po[0], &pj[tj[i].i3] ) == 0 )
    {
      continue;
    }
    if( lbo_PointLiesInPlain(i,pj,tj,po) == -1 )
      continue; // return -1
    
    r = lbo_pointInOutDreieck(&po[0],pj,&tj[i]);
    if(r == -1)
      continue; // return -1;
    
    if(r == 1) 
    {
      log( 1, "Darf nicht innerhalb der Grenze liegen\n"); 
      //return -1;
      continue;
    }
    
    if(r == 0)
    {
      if( m_sd[0] > -1 && m_sd[1] == -1 && m_sd[2] == -1 ) 
      {
        sd[0] = m_sd[0];
        op[sd[0]] = po[0];
      }
      else if( m_sd[0] == -1 && m_sd[1] > -1 && m_sd[2] == -1 )
      {
        sd[0] = m_sd[1];
        op[sd[0]] = po[0];
      }
      else if( m_sd[0] == -1 && m_sd[1] == -1 && m_sd[2] > -1 )
      {
        sd[0] = m_sd[2];
        op[sd[0]] = po[0];
      }
      else
      {
        log( 1, "Darf nicht sein Global %d %d %d\n", m_sd[0], m_sd[1], m_sd[2] );
        continue;
        //return -1;
      }
      
      if( lbo_2TrianglesFromBoundaryPoint( i, tj, op, pj, e, et, sd[0], index ) == -1 )
        return -1;  
      tj = this->t[index];
      pj = this->p[index];
      //!!
    }	
  }; // for i
  
  Tm = tj[0].n;
  for( i = mT; i < Tm; i++ )
  {
    if (tj[i].area == 0.0)
      continue;
    if(lbo_PointLiesInPlain( i, pj, tj, po) == -1 )
      return -1;
    r = lbo_pointInOutDreieck( &po[1], pj, &tj[i] );
    if(r == -1)
      return -1;
    
    if(r == 0)
    {
      if( lbo_StreckenLaenge( &po[1], &pj[tj[i].i1] ) == 0 )
        continue;
      if(lbo_StreckenLaenge( &po[1], &pj[tj[i].i2] ) == 0 )
        continue;
      if(lbo_StreckenLaenge( &po[1], &pj[tj[i].i3] ) == 0 )
        continue;
      
      if(m_sd[0] > -1 && m_sd[1] == -1 && m_sd[2] == -1)
      {
        sd[0] = m_sd[0];
        op[sd[0]] = po[1];
      }
      else if(m_sd[0] == -1 && m_sd[1] > -1 && m_sd[2] == -1)
      {
        sd[0] = m_sd[1];
        op[sd[0]] = po[1];
      }
      else if(m_sd[0] == -1 && m_sd[1] == -1 && m_sd[2] > -1)
      {
        sd[0] = m_sd[2];
        op[sd[0]] = po[1];
      }
      else
      { 
        printf("Darf nicht sein Global %d %d %d\n",m_sd[0],m_sd[1],m_sd[2]);
        continue;
        //return -1;
      }
      
      if(lbo_2TrianglesFromBoundaryPoint(i,tj,op,pj,e,et,sd[0], index ) == -1) 
        return -1;  
      tj = this->t[index];
      pj = this->p[index];
      //!!
    }	
  }; // for i
  
  //if(tj[0].n > Tm) printf("Tm= %d tj[0].n= %d\n",Tm,tj[0].n);
  
  
  Tm = tj[0].n;
  for( i = mT; i < Tm; i++ )
  {
    if( ( rLine = lbo_LineCutsSidesOfTheTriangle( i, pj, tj, po, op, sd ) ) == -1)
      return -1;
    else if( rLine == 1 ) 
    { // Seite  sd[0] -> (sd[0]+1)%3 : op
      if( lbo_2TrianglesFromBoundaryPoint(i,tj,op,pj,e,et,sd[0], index ) == -1)
        return -1;  
      tj = this->t[index];
      pj = this->p[index];
      //!!
    }
  }	     
  
  
  Tm = tj[0].n;
  for( i = mT; i < Tm; i++ )
  {
    if( ( rLine = lbo_LineCutsSidesOfTheTriangle( i, pj, tj, po, op, sd ) ) == -1)
      return -1;
    else if( rLine == 2 )
    { // Seite  sd[0] -> (sd[0]+1)%3 : op
      if( lbo_3TrianglesFromBoundaryPoints( i, tj, op, pj, e, et, sd, index ) == -1 )
        return -1;  
      tj = this->t[index];
      pj = this->p[index];
      //!!
    }
  }
  
  return 0;
}; // lbo_AdditionalPointsAndTriangles

int	TinCut::lbo_TriangleSidesAndSlantPlaneCut( Point *pj, Triangle *tji, Point *pk, Triangle *tkn, 
                                              Point *po)
                                              // findet die Schnittpunkte eines Dreiecks mit der von einem anderen Dreieck
                                              // aufgespannten Ebene
                                              //
                                              // Rückgabewert: 0, falls kein Schnittpunkt gefunden wurde
                                              //               1 bei Erfolgreicher Schnittpunkt suche
                                              //
                                              //
                                              // Parameter:
                                              //          Point* pj, Triangle* tij: erstes Dreieck mit den zugehörigen Punkten ( spannt die Ebene auf )
                                              //          Point* pk, Triangle* tkn: zweites Dreieck, ebenso ( schneidet die Ebene )
                                              // Bemerkung:
                                              //        geschnitten werden in Wirklichkeit jeweils die drei Seiten ( nur als Strecken )
                                              //        des einen mit der vom anderen Dreieck aufgespannten Ebene
{
  // Zuerst werden Schnittpunkte gefunden: 
  // Die Seiten des Dreiecks < tkn > schneiden die Schraegebene, in welcher 
  // das Dreieck < tji > liegt -> < po > sind Schnittpunkte.
  
  int i, r, r1, r2, r3, n;
  Line l[3];
  double alfa[3];
  
  l[0].p1 = pk[tkn->i1]; 
  l[1].p1 = pk[tkn->i2]; 
  l[2].p1 = pk[tkn->i3];
  
  l[0].p2 = pk[tkn->i2];
  l[1].p2 = pk[tkn->i3]; 
  l[2].p2 = pk[tkn->i1];
  
  n = 0;
  for( i = 0; i < 3; i++)
  {
    l[i].alfa = HUGE_VAL;
    r = lbo_LineAndSlantPlaneCut( &l[i], tji, pj );
    
    if( r ==  1) 
    {
      alfa[n] = l[i].alfa;
      po[n].n = n;
      po[n].x = l[i].p1.x + l[i].alfa * ( l[i].p2.x - l[i].p1.x );
      po[n].y = l[i].p1.y + l[i].alfa * ( l[i].p2.y - l[i].p1.y );
      po[n].z = l[i].p1.z + l[i].alfa * ( l[i].p2.z - l[i].p1.z );
      po[n].Pz = po[n].z;
      n++;
    }
    else if( r == -1 )
      return -1;	
  } // for i
  
  if( n == 0 )
    return  0;
  else if( n == 1 )
  {
    log( 3, "Dieser Fehler darf nie auftreten\n");
    log( 3, "%d %f %f %f %f\n",n-1,alfa[n-1],po[n-1].x,po[n-1].y,po[n-1].z);
    return 0;
  }
  else if( n == 2 )
    return 1;
  else if( n == 3 )
  { // das Dreieck liegt vollständig in der Ebene
    r1 = lbo_StreckenLaenge( &po[0], &po[1] );
    r2 = lbo_StreckenLaenge( &po[0], &po[2] );
    r3 = lbo_StreckenLaenge( &po[1], &po[2] );
    
    log(3, "Drei Punkte in der Schnittebene: Dreiecke %d und %d\n", tji->n, tkn->n ); 
    if( r1 == 1 && ( r2 == 0 || r3 == 0 ) )
    { //nichts tun
    }
    else if( r2 == 1 && ( r1 == 0 || r3 == 0 ) )
    {
      po[1] = po[2];
    }
    else if( r3 == 1 && ( r2 == 0 || r1 == 0 ) )
    {
      po[0] = po[2];
    }
    else
    {
      log(3, "Dieser Fehler darf hier nie auftreten\n"); 
      return 0;
    }
    
    return 1;
  }
  return 0;
} // lbo_TriangleSidesAndSlantPlaneCut

int TinCut::lbo_DieZweiDreieckeSchneidenNicht( Triangle *tji, Triangle *tkn )
// testet schnell auf eine notwendige Bedingung für den Schnitt ( die Umkreise Berühren sich nicht )
//
// Rückgabewert:
//            TRUE, falls sich die Dreiecke definitiv nicht schneiden
//            FALSE falls sie möglicherweise tun
// Parameter: Triangle* tij und Triangle* tkn: die beiden zu testenden Dreiecke
// 
{
  double epsilon, delta;
  
  epsilon = tji->xc - tkn->xc;
  delta = tji->x + tkn->x;
  if( epsilon > delta || epsilon < -delta )
    return 1;
  
  epsilon = tji->yc - tkn->yc;
  delta = tji->y + tkn->y;
  if( epsilon >  delta || epsilon < -delta )
    return 1;
  
  epsilon = tji->zc - tkn->zc;
  delta = tji->z + tkn->z;
  if( epsilon >  delta || epsilon < -delta )
    return 1;
  
  return 0;
} // lbo_DieZweiDreieckeSchneidenNicht

int TinCut::lbo_AufUndAbtragsPunkteGrenzeDreiecke( Point** p, Triangle** t, MaxMinPoint* e, MaxMinTriangle* et )
// Zerlegt die Dreiecksebene an ihren 'Durchdringungspunkten'
//
{
  int i,j,k,n,lt,rt,r;
  double xl,xr,area0;
  int TriNum[2],mT; 
  Point po[3];
  Triangle tji; 
  
  //  a[0][0].n = 1;
  //  a[1][0].n = 1; // Im Struct-Vektor a{...} werden Grenzstuecke gespeichert
  
  // Neue Dreiecke haben laufende Nummern, die groesser als TriNum[j] sind
  
  TriNum[0] = t[0][0].n;
  TriNum[1] = t[1][0].n;
  
  for( j = 0; j < 2; j++ )
  {
    k = (j + 1) % 2;
    
    for( i = 1; i < TriNum[j]; i++ )
    {
      xl = t[j][i].xc - t[j][i].x - et[k].xmax;
      xr = t[j][i].xc + t[j][i].x + et[k].xmax;
      
      lt = 1;
      rt = TriNum[k] - 1; 
      lbo_QuickFindEinDreieckFuerStrecke( &lt, &rt, xl, xr, t[k] );
      
      if( lt == 0 )
        continue;
      
      mT = t[j][0].n++;
      if(mT <= 0)
      {
        log( 0, "Dreiecksanzahl muss positiv sein\n");
        return -1;
      }
      
      t[j][mT] = t[j][i]; 
      t[j][mT].n = ++(et[j].nmax);
      tji = t[j][mT];
      
      area0=t[j][i].area;
      n = (lt + rt) / 2;         
      while( true ) 
      {
        if( lbo_DieZweiDreieckeSchneidenNicht( &tji, &t[k][n] ) == 0 ) 
        {
          r = lbo_TriangleSidesAndSlantPlaneCut( p[j], &tji, p[k], &t[k][n], po );
          if(r == -1)
            return -1;
          if(r ==  1) 
          {  
            if( lbo_AdditionalPointsAndTriangles( mT, p[j], t[j], po, e, et, j ) == -1 )
              return -1;
          }					 
        }				                
        --n;
        if(n < 1 || t[k][n].xc < xl) 
          break;
      }
      
      n=(lt+rt)/2; 
      
      while( true ) 
      {
        ++n;
        if(n >= t[k][0].n || t[k][n].xc > xr) 
          break;
        
        if(lbo_DieZweiDreieckeSchneidenNicht(&tji,&t[k][n]) == 0 ) 
        {
          r=lbo_TriangleSidesAndSlantPlaneCut(p[j],&tji,p[k],&t[k][n],po);
          if(r == -1)
            return -1;
          if(r ==  1) 
          {
            if(lbo_AdditionalPointsAndTriangles(mT,p[j],t[j],po,e,et, j ) == -1)
              return -1; 	  
          }
        }
      }
      
      if( mT == t[j][0].n-1 )
        t[j][0].n--;
      else if( area0 != t[j][i].area )
      {
        log( 0, "Kann nicht sein\n" );  
        return -1;
      }
      else
        t[j][i].n = -t[j][i].n;
    }
  }
  return 0;
} // lbo_AufUndAbtragsPunkteGrenzeDreiecke

int TinCut::lbo_DreieckeMitNegativenNummernWeg( Triangle** t, MaxMinTriangle* et )
{
  int i,j,k; 
  
  for(j=0; j<2; j++)
  {
    k=1;         
    for(i=1; i<t[j][0].n; i++) 
    {
      if(t[j][i].n > 0)
      {
        t[j][k++]=t[j][i];
        /*
        if(t[j][i].area > -eps*eps) {
        printf("%d.Dreieck in der %d.Ebene hat Flaeche %f\n",i,j,t[j][i].area);
        return -1;
        }
        */		
      }
    }
    
    t[j][0].n=k; 
  };
  
  for(j=0; j<2; j++) {                 et[j].minarea=-HUGE_VAL;       
  for(i=1; i<t[j][0].n; i++) {                      
    
    if(t[j][i].area > et[j].minarea) et[j].minarea=t[j][i].area;
  }
  if(et[j].minarea > -eps*eps)
    log( 1, "Das minimale Flaeche in der %d.Ebene /eps/eps = %f\n",j,et[j].minarea/eps/eps);
  }
  return 0;
}; // lbo_DreieckeMitNegativenNummernWeg

int TinCut::lbo_Initializierung( int no, int* o, int na, Arc* a, int np, Point* pt, const int index )
// initialisiert o: o[i] = i
//               a: a[i].alles = 0
//               p: p[i].n = i
{
  int i;
  if(no > 0)
  {
    o[0] = no;
    if(o[0] < 4 && o[0] > 0)
    {
      log( 0, "Es gibt nicht genug Punkte\n");
      return -1;
    };
    for(i = 1; i < no; i++)
    {
      o[i] = i;
    };
  };
  
  //	if( na > MEL ) 
  //  {
  //		log( 0, "Variablen-Initializierung: MEL zu klein\n");
  //    return -1;
  //	};
  
  for(i = 1; i < na; i++)
  {
    a[i].n = a[i].bot = a[i].top = a[i].lft = a[i].rgt = 0;
  };
  
  allocatePoint( &pt, index, np );
  //if(np > NKNOT)
  //{
  //	log( 0, "Variablen-Initializierung: NKNOT zu klein\n");
  //  return -1;
  //};
  
  for( i = 1; i < np; i++ )
    pt[i].n = i;
  
  return 0;
}; // lbo_Initializierung

int TinCut::lbo_UmkehrNummern( Point* p,int* o, int* O )
{
  // Input ins Programm besteht aus Punktnumern o[0] <=> Output - aus o[1]
  int i;
  for(    i=1; i<o[0]; i++){
    if(o[i] == 0){
      log( 0, "Umkehrfunktion kann nicht definiert werden %d %d\n",o[0],o[i]); return -1;
    }
    O[o[i]]=i; 
  }
  
  
  return 0;
}; // lbo_UmkehrNummern

int TinCut::lbo_DreiecksNummernAenderung( Point* p, Triangle* t, int* o )
{
  int i;
  for(i = 1; i < t[0].n; i++)
  {
    t[i].i1 = o[t[i].i1];
    t[i].i2 = o[t[i].i2];
    t[i].i3 = o[t[i].i3];
  };
  return 0;
}; // lbo_DreiecksNummernAenderung

int TinCut::lbo_GrenzkantenNummernAendern( Point* p, Arc* g, int* o )
{
  int i;
  for(i = 1; i < g[0].n; i++)
  {
    g[i].bot = o[g[i].bot];
    g[i].top = o[g[i].top];
  };
  return 0;
}; // lbo_GrenzkantenNummernAendern

int TinCut::lbo_PunktFiltration( Point* p, int* o, int ebene )
{
  int i, j; //,p0n;
  // Wir gehen davon aus, daß alle Punkte nach X sortiert wurden und
  
  //	p0n = p[0].n;
  for(i = 1; i < p[0].n-1; i++)
  {
    if(p[i].n < 0)
      continue;			
    for(j = i+1; j < p[0].n; j++)
    {
      if(p[j].n < 0)
        continue;
      if(sqrt(pow(p[j].x - p[i].x,2.) + pow(p[j].y - p[i].y,2.)) < eps)
      {
        p[j].n = -p[j].n;
        o[j] = i;
      };
      if(p[j].x - p[i].x > eps)
      {
        i = j - 1;
        break;
      };
    }; // for j
  }; // for i
  
  j=0;
  for(i = 1; i < p[0].n; i++)
  {
    if(p[i].n > 0)
    {
      p[++j]=p[i];
      o[i] = j;
    }
    else
    {
      o[i] = o[o[i]];
    };
    
  };
  
  p[0].n=++j;
  
  //if(p[0].n != p0n)
		//printf("%d doppelte Punkte der %d.Ebene werden abfiltriert\n",p0n-p[0].n,ebene);
  return 0;
}; // lbo_PunktFiltration

int TinCut::lbo_AbUndAuftragsDreiecke( Point* p, Triangle* t, int j )
{
  int i, null, plus, minus;
  double zc;
  
  null = plus = minus = 0;
  
  for(i = 1; i < t[0].n; i++)
  {
    if(t[i].n <= 0)
    {
      log( 0, "t[i].n <= 0  %d\n",i);
      return -1;
    }
    
    zc = t[i].zc - t[i].zP; 
    
    if(zc ==  0.0)
    {
      t[i].n = 0;
      null++;
    }
    else if(zc >   0.0)
    {
      t[i].n = +t[i].n;
      plus++; 
    } // Auftrag
    else if(zc <   0.0)
    {
      t[i].n = -t[i].n;
      minus++;
    } // Abtrag 
    else 
    {
      log( 0, "Darf nicht sein! << lbo_AbUndAuftragsDreiecke >> \n"); return -1;
    };
    
    //if(null != 0) printf("null %d\n",null);
  };
  if(t[0].n-1 != null + plus + minus) 
  {
    log( 0, "Fehler: << lbo_AbUndAuftragsDreiecke >> \n"); 
    return -1;
  };
  return 0;
}; // lbo_AbUndAuftragsDreiecke

void TinCut::lbo_QuickFindStructPointNachNummer( int* i, int lt, int rt, int n, Point* p )
{
  *i = (lt+rt)/2;
  
  int m=p[*i].n; 
  
  if(n == p[lt].n)
  {
    *i = lt;
    return;
  };
		
  if(n ==p[rt].n)
  {
    *i=rt;
    return;
  }
  
  if(n == m)
    return;
		
  if(n < m)
  {
    rt = *i;
    lbo_QuickFindStructPointNachNummer(i,lt,rt,n,p);
  };
  
  if(n > m)
  {
    lt=*i;
    lbo_QuickFindStructPointNachNummer(i,lt,rt,n,p);
  };
}; // lbo_QuickFindStructPointNachNummer

void TinCut::lbo_QuickFindStructPointNachX( int* lt, int* rt, double xl, double xr, Point* p )
{
  int i = (*lt+*rt)/2;
  double x = p[i].x; 
  
  if(xl <= x && x <= xr) 
    return;
  
  if (*lt >= *rt)
  {
    *rt = 0; // zeigt einen Fehler an
    return;
  };
  
  if(x < xl)
  {
    *lt = max(i, *lt + 1);  // mindestens ein x-Wert weiter
    lbo_QuickFindStructPointNachX(lt,rt,xl,xr,p);
  };
  
  if(x > xr) 
  {
    *rt = min(i, *rt - 1); // mindestens einen Wert weniger
    lbo_QuickFindStructPointNachX(lt,rt,xl,xr,p);
  };
}; // lbo_QuickFindStructPointNachX

void TinCut::lbo_QuickFindEineKantenFuerStrecke( int* lt, int* rt, double xl, double xr, Arc* a )
{ 
  int i = ( *lt + *rt ) / 2;
  
  double x = a[i].xc; 
  
  if(xl <= x && x <= xr) 
    return;
  
  if (*lt >= *rt)
  {
    *lt = 0; // nicht gefunden
    return;
  };
  
  if(x < xl) 
  {
    *lt = max(i, *lt + 1);  // mindestens eins nach rechts
    lbo_QuickFindEineKantenFuerStrecke(lt,rt,xl,xr,a);
  }
  if(x > xr)
  {
    *rt = min(i, *rt - 1); // mindestens eins nach links
    lbo_QuickFindEineKantenFuerStrecke(lt,rt,xl,xr,a);
  };
}; // lbo_QuickFindEineKantenFuerStrecke

void TinCut::lbo_QuickFindEinDreieckFuerStrecke ( int* lt, int* rt, double xl, double xr, Triangle* t )
// schränkt einen Suchbereich für Dreiecke ein
//
// Parameter:
//        int* lt, rt: Bereich von Dreiecken welcher zur Verfügung steht
//        double xl, xr: linke und rechte Grenze
//        trngl* t: Dreiecke
// Rückgabewert:
//        keiner; *lt == 0 falls kein Dreieck im Bereich [lt, rt] gefunden wurde
// Bemerkung:
//        schränkt lt und rt auf den kleinsten Bereich ein, in welchem
//        das Zentrum aller Dreiecke sicher zwischen xl und xr liegt
{ 
  int i = ( *lt + *rt ) / 2;
  
  double x = t[i].xc; 
  
  if( xl <= x && x <= xr )
    return;
  
  if( xr < xl ) 
  {
    *lt = 0; // darf nicht sein -> kein Punkt gefunden
    return;
  };
  
  if (*lt >= *rt)
  {
    *lt = 0; // kein Punkt gefunden
    return; 
  };
  
  if(x < xl)
  {
    *lt = max(i, *lt + 1); // mindestens eins nach rechts
    lbo_QuickFindEinDreieckFuerStrecke(lt,rt,xl,xr,t);
  };
	 
  if(x > xr)
  {
    *rt = min(i, *rt - 1); // mindestens eins nach links
    lbo_QuickFindEinDreieckFuerStrecke(lt,rt,xl,xr,t);
  };
}; // lbo_QuickFindEinDreieckFuerStrecke 


int TinCut::lbo_pointInOutDreieck( Point* q, Point* p, Triangle* t )
// Testet, ob ein Punkt innerhalb oder ausserhalb eines Dreiecks liegt
// Parameter:
//          point* q: der Testpunkt, sein Member Pz wird gesetzt
//          point* p, trngl* t: das Dreieck
// Rückgabewert:
//            2: der Punkt liegt außerhalb des Dreiecks
//            0: der Punkt liegt an einer Seite des Dreiecks
//            1: der Punkt liegt innerhalb des Dreiecks
//           -1: das Dreiecks ist fehlerhaft
// Seiteneffekte:
//            int m_sd[3]: 
//          
// Bemerkung:
//  hier gibt es Probleme mit Rundungsfehlern!!!
{
  double x0, y0, x1, y1, x2, y2, x3, y3, a0, a1, a2, a3, z1, z2, z3, nx, ny, nz, k;
  int i1, i2, i3;
  int r = -1;
  
  i1 = t->i1;
  i2 = t->i2;
  i3 = t->i3;
  
  x0 = q->x;
  y0 = q->y;
  x1 = p[i1].x;
  y1 = p[i1].y;
  x2 = p[i2].x;
  y2 = p[i2].y;
  x3 = p[i3].x;
  y3 = p[i3].y;
  
  // neu, erst Testen, obs einer der Drei Punkte ist ( zwar langsamer, verhindert
  // aber Rundungsfehler )
  double dx01 = x0 - x1;
  double dy01 = y0 - y1;
  double dx02 = x0 - x2;
  double dy02 = y0 - y2;
  double dx03 = x0 - x3;
  double dy03 = y0 - y3;
  double dx12 = x1 - x2;
  double dy12 = y1 - y2;
  double dx13 = x1 - x3;
  double dy13 = y1 - y3;
  double dx23 = x2 - x3;
  double dy23 = y2 - y3;
  
  m_sd[0] = m_sd[1] = m_sd[2] = -1;
  
  if ( dx01 == 0.0 && dy01 == 0.0 )
  {
    r = 0;
    m_sd[0] = 0;
  }
  else if ( dx02 == 0.0 && dy02 == 0.0 )
  {
    r = 0;
    m_sd[1] = 1;
  }
  else if ( dx03 == 0.0 && dy03 == 0.0 )
  {
    r = 0;
    m_sd[2] = 2;
  };
  
  a0 = ( ( -dx12 ) * ( y2 + y1 ) + 
    ( -dx23 ) * ( y3 + y2 ) + 
    (  dx13 ) * ( y1 + y3 ) ) / 2.0;
  if( eps * eps < a0 )
  {
    log( 3, "Dreieck Nummer %d ist falsch orientiert: a0 = %lf\n", t->n, a0 );
    // return -1;
    r = 2;
  }
  if( eps * eps >= a0 && a0 >= -eps * eps && r != -1 )
  {
    log( 3, "Dreieck Numer %d hat die Flaeche kleiner als eps\n", t->n );
    r = 2;
  };
  
  a1 = ( ( -dx12 ) * ( y2 + y1 ) + 
    (  dx02 ) * ( y0 + y2 ) + 
    ( -dx01 ) * ( y1 + y0 ) ) / 2.0; 
  a2 = ( ( -dx23 ) * ( y3 + y2 ) +
    (  dx03 ) * ( y0 + y3 ) +
    ( -dx02 ) * ( y2 + y0 ) ) / 2.0; 
  a3 = ( (  dx13 ) * ( y1 + y3 ) +
    (  dx01 ) * ( y0 + y1 ) +
    ( -dx03 ) * ( y3 + y0 ) ) / 2.0; 
  
  if ( r == -1 )
  {
    if( a1 < -eps * eps && a2 < -eps * eps && a3 < -eps * eps )
      r = 1; 
    else if( a1 >  eps*eps || a2 >  eps*eps || a3 >  eps*eps )
      return 2;
    else if( a1 <= eps * eps && a1 >= -eps * eps )
    {
      r = 0;
      m_sd[0] = 0;
    }
    else if( a2 <= eps * eps && a2 >= -eps * eps )
    {
      r = 0;
      m_sd[1] = 1;
    }
    else if( a3<= eps * eps && a3 >= -eps * eps )
    { 
      r = 0;
      m_sd[2]=2;
    }
    else 
    {
      log( 1, "1.Logische Programmierungsfehler in lbo_pointInOutDreieck\n" );
      return -1;
    }
  }; // if r == -1
  
  //     Vektorgleichung einer Fläche : (nx,ny,nz)*v=k 
  
  z1 = p[i1].z;
  z2 = p[i2].z;
  z3 = p[i3].z; 
  
  nx = (y2-y1)*(z3-z2)-(y3-y2)*(z2-z1);
  ny = (x3-x2)*(z2-z1)-(x2-x1)*(z3-z2);
  nz = (x2-x1)*(y3-y2)-(x3-x2)*(y2-y1);
  if( nx * nx + ny * ny + nz * nz <= eps * eps )
  { 
    log( 1, "Normalenvektor ist zu klein (Dreieck %d)\n", t->n );
  };
  
  if( nz == 0.0 && nx * nx + ny * ny > eps*eps )
  {
		  log( 1, "Dreieck steht Kopf: nx=%f ny=%f nz=%f\n", nx, ny, nz );
  };
  
  k = nx * x1 + ny * y1 + nz * z1;
  
  q->Pz = ( k - nx * x0 - ny * y0 ) / nz;
  
  return r;
}; // lbo_pointInOutDreieck

int TinCut::lbo_HoeheInderStrecke( double x, double y, double z, double X, double Y, double Z, 
                                  double xX,double yY, double* zZ )
                                  // berechnet die Höhe eines Projektionspunktes in einer Strecke
                                  //
                                  // Parameter:
                                  //          double x, y, z, X, Y, Z: anfangs und Endpunkt der Strecke
                                  //          double xX, yY: ein Punkt in der Projektion der Strecke
                                  //          double* zZ: hier wird die erechnete höhe des Punktes gespeichert ( verändert )
                                  // Rückgabewert: int
                                  //          -1, falls die Projektion des anfangs und endpunkts gleich sind
                                  //           0 sonst
{
  ////////////////////////////////////////////////////////////
  //                                                        //
  // Nach der Aehnlichkeit der Dreiecke :  (*zZ-z)/(Z-z)=s/S //
  //                                                        //
  ////////////////////////////////////////////////////////////
  
  double S;
  if( ( S = sqrt( pow( X - x, 2.) + pow( Y - y, 2.) ) ) == 0. )
    return -1;
  
  double s = sqrt( pow( xX - x, 2. ) + pow( yY - y, 2. ) );
  *zZ = s / S * ( Z - z ) + z;
  
  return  0;
}; // lbo_HoeheInderStrecke


   /*
   int lbo_SchnittpunktZweierGeraden1
   (double x1,double y1,double x2,double y2,double x3,double y3,double x4,double y4,
   double *Xx,double *Yy) {
   
     double alfa, beta, nenner;
     // beta ?
     // x1+alfa*(x2-x1) = x3+beta*(x4-x3) = *Xx
     // y1+alfa*(y2-y1) = y3+beta*(y4-y3) = *Yy
     // x1*(y2-y1)+alfa*(x2-x1)*(y2-y1) = x3*(y2-y1)+beta*(x4-x3)*(y2-y1)
     // -
     // y1*(x2-x1)+alfa*(y2-y1)*(x2-x1) = y3*(x2-x1)+beta*(y4-y3)*(x2-x1)
     
       // alfa ?
       // x1+alfa*(x2-x1) = x3+beta*(x4-x3) = *Xx
       // y1+alfa*(y2-y1) = y3+beta*(y4-y3) = *Yy
       // x1*(y4-y3)+alfa*(x2-x1)*(y4-y3) = x3*(y4-y3)+beta*(x4-x3)*(y4-y3)
       // -
       // y1*(x4-x3)+alfa*(y2-y1)*(x4-x3) = y3*(x4-x3)+beta*(y4-y3)*(x4-x3) 
       
         nenner=(x4-x3)*(y2-y1)-(y4-y3)*(x2-x1);
         if(fabs(nenner)<eps) return 0;
         beta=((x1-x3)*(y2-y1)-(y1-y3)*(x2-x1))/nenner; if(beta > 1.+eps || beta < 0.-eps) return 0;
         
           nenner=(x2-x1)*(y4-y3)-(y2-y1)*(x4-x3);
           if(fabs(nenner)<eps) return 0;
           alfa=((x3-x1)*(y4-y3)-(y3-y1)*(x4-x3))/nenner; if(alfa > 1.+eps || alfa < 0.-eps) return 0;
           
             *Xx=x3+beta*(x4-x3); *Yy=y3+beta*(y4-y3);
             double Ox,Oy; Ox=x1+alfa*(x2-x1);  Oy=y1+alfa*(y2-y1);
             
               if(fabs(*Xx - Ox) > eps || fabs(*Yy - Oy) > eps)
               {printf("Schnittpunkt ist falsch gefunden\n");}
               
                 
                   if((fabs(alfa-1.)<eps||fabs(alfa)<eps)&&(fabs(beta-1.)<eps||fabs(beta)<eps)) return 1;//-
                   //                                                                                       |
                   // Beide Linienstuecke beruehren sich an Endpunkten                                    <-
                   
                     else if( fabs(alfa-1.)<eps||fabs(alfa)<eps)                                       return 2;//-
                     //                                                                                       |
                     // Eins der Ende der 1. Linienstueck beruert das 2. Linienstueck im inneren Bereich    <-
                     
                       else if(                                      fabs(beta-1.)<eps||fabs(beta)<eps)  return 3;//-
                       //                                                                                       |
                       // Eins der Ende der 2. Linienstueck beruert das 1. Linienstueck im inneren Bereich    <-
                       
                         return 4;//-
                         //                                                                                       |
                         // Linienstuecke schneiden sich im inneren Bereich                                     <-
                         }
                         
*/

int TinCut::lbo_SchnittpunktZweierGeraden( double x1, double y1, double x2, double y2, 
                                          double x3, double y3, double x4, double y4, double *Xx, double *Yy )
                                          // berechnet den Schnittpunkt zweier Strecken in der Ebene
                                          //
                                          // Parameter: double x1, y1, x2, y2: Anfangs und Endpunkt der 1. Strecke
                                          //            double x3, y3, x4, y4: dito 2. Strecke
                                          //            double* Xx, Yy: Koordinaten des Schnittpunkts ( verändert )
                                          // Rückgabewert:
                                          //            0 = Strecken schneiden sich nicht ( Xx und Yy undefiniert )
                                          //            1 = zwei Endpunkte berühren sich
                                          //            2 = 1. Strecke berührt das Innere der zweiten
                                          //            3 = 2. Strecke berührt das innere der ersten
                                          //            4 = der Schnittpunkt liegt im Inneren beider Strecken
{
  double alfa, beta, nenner;
  // beta ?
  // x1+alfa*(x2-x1) = x3+beta*(x4-x3) = *Xx
  // y1+alfa*(y2-y1) = y3+beta*(y4-y3) = *Yy
  // x1*(y2-y1)+alfa*(x2-x1)*(y2-y1) = x3*(y2-y1)+beta*(x4-x3)*(y2-y1)
  // -
  // y1*(x2-x1)+alfa*(y2-y1)*(x2-x1) = y3*(x2-x1)+beta*(y4-y3)*(x2-x1)
  
  // alfa ?
  // x1+alfa*(x2-x1) = x3+beta*(x4-x3) = *Xx
  // y1+alfa*(y2-y1) = y3+beta*(y4-y3) = *Yy
  // x1*(y4-y3)+alfa*(x2-x1)*(y4-y3) = x3*(y4-y3)+beta*(x4-x3)*(y4-y3)
  // -
  // y1*(x4-x3)+alfa*(y2-y1)*(x4-x3) = y3*(x4-x3)+beta*(y4-y3)*(x4-x3) 
  
  nenner = ( x4 - x3 ) * ( y2 - y1 ) - ( y4 - y3 ) * ( x2 - x1 );
  if( fabs(nenner) < eps )
    return 0;
  beta = ( ( x1 - x3 ) * ( y2 - y1 ) - ( y1 - y3 ) * ( x2 - x1 ) ) / nenner; 
  
  nenner = ( x2 - x1 ) * ( y4 - y3 ) - ( y2 - y1 ) * ( x4 - x3 );
  if( fabs(nenner) < eps )
    return 0;
  alfa = ( ( x3 - x1 ) * ( y4 - y3 ) - ( y3 - y1 ) * ( x4 - x3 ) ) / nenner; 
  
  *Xx = x3 + beta * ( x4 - x3 );
  *Yy = y3 + beta * ( y4 - y3 );
  int A0 = alfa < -eps || alfa > 1. + eps;       // alfa > 1 or alfa < 0
  int B0 = beta < -eps || beta > 1. + eps;       // beta > 1 or beta < 0
  int A1 = -eps <=     alfa &&     alfa <= eps; // alfa ~ 0
  int A2 = -eps <= 1.- alfa && 1.- alfa <= eps; // alfa ~ 1
  int B1 = -eps <=     beta &&     beta <= eps; // beta ~ 0
  int B2 = -eps <= 1.- beta && 1.- beta <= eps; // beta ~ 1
  int A3 =  eps < alfa && alfa < 1.- eps;       // 0 < alfa < 1
  int B3 =  eps < beta && beta < 1.- eps;       // 0 < beta < 1
  
  if( A0||B0)              return 0;   // Schnittpunkt im aeusseren Bereich
  if((A1||A2) && (B1||B2)) return 1;   // Beide Linienstuecke beruehren sich an Endpunkten
  if((A1||A2) &&  B3)      return 2;  
  
  // Eins der Ende der 1. Linienstueck beruert das 2. Linienstueck im inneren Bereich  
  
  if((B1||B2) &&  A3)      return 3;                                 
  // Eins der Ende der 2. Linienstueck beruert das 1. Linienstueck im inneren Bereich  
  
  if(A3 && B3) return 4;  // Linienstuecke schneiden sich im inneren Bereich
  
  log( 0, "Fehler: lbo_SchnittpunktZweierGeraden\n" );
  return -1;  
}; // lbo_SchnittpunktZweierGeraden

int TinCut::lbo_ILL3( double* beta1, double* b1, double* d1,
                     double* beta2, double* b2, double* d2, double* s)
                     // Point of intersection of two lines in 3 dimentions
{
/*
s[i] = b1[i] + ß1*d1[i] = b2[i] + ß2*d2[i] 
s = Schnittvektor, b[i] = Anfangspunk einer Strecke, d[i] = Richtungsvektor

  Es gibt nur zwei unbekannte Skalare ß1 und ß2 in drei Gleichungen. Das heißt,daß 
  nur zwei Gleichungen linearunabhängig sind.
  */
  
  int i, j;
  double delta, atled;
  
  // Finden das beste Paar von Gleichungen : j und (j+1)%3
  
  j = 0;
  delta = 0.;
  
  for( i = 0; i < 3; i++ )
  {
    atled = d1[i] * d2[( i + 1 ) % 3] - d1[( i + 1 ) % 3] * d2[i];
    if( fabs(atled) > fabs(delta) )
    {
      delta = atled;
      j = i;
    }
  }; // for i
  
  if( -eps < delta && delta < eps )
    return 0;
  else
  {
    *beta1 = ( d2[( j + 1 ) % 3] * ( b2[j] - b1[j] ) - d2[j] * ( b2[( j + 1 ) % 3] - 
      b1[( j + 1 ) % 3] ) );
    *beta2 = ( d1[( j + 1 ) % 3] * ( b2[j] - b1[j] ) - d1[j] * ( b2[( j + 1 ) % 3] - 
      b1[( j + 1 ) % 3] ) );
    
    *beta1 = *beta1 / delta;
    *beta2 = *beta2 / delta;
    
    double dif = b1[( j + 2 ) % 3] + *beta1 * d1[( j + 2 ) % 3] - b2[( j + 2 ) % 3] -
      *beta2 * d2[( j + 2 ) % 3];
    
    if( !( -eps < dif && dif < eps ) )
    {
      log( 1, "Schnittpunkt ist fehlerhaft: beta1=%f beta2=%f dif=%f delta=%f\n",
        *beta1, *beta2, dif, delta );
      return -1;
    }
    else
    {
      for( i = 0; i < 3; i++)
        s[i] = b1[i] + *beta1 * d1[i];
      return 1;
    }
  }
}; // lbo_ILL3

int TinCut::lbo_LineAndSlantPlaneCut( Line* l, Triangle* t, Point* p )
// Berechnet den Schnittpunkt einer Ebene und einer Strecke
//
// Parameter:
//        trngl* t und point* p: t spannt die Ebene auf, p sind die zugehörigen Punkte
//        line* l: die Strecke, durch alpha wird hier der Schnittpunkt spezifiziert
// Rückgabewert:
//          0 die Strecke schneidet nicht die Ebene ( d.h. entweder schneidet die gerade
//              die Ebene oder der Schnittpunkt liegt ausserhalb der Strecke
// Bemerkung: wird ausschließlich von lbo_TriangleSidesAndSlantPlaneCut aufgerufen
// 
{
  double x1, y1, z1, x2, y2, z2, x3, y3, z3, nx, ny, nz, ns, k, d1, d2;
  
  x1 = p[t->i1].x;
  y1 = p[t->i1].y;
  z1 = p[t->i1].z;
  
  x2 = p[t->i2].x;
  y2 = p[t->i2].y;
  z2 = p[t->i2].z;
  
  x3 = p[t->i3].x;
  y3 = p[t->i3].y;
  z3 = p[t->i3].z;
  
  //     Vektorgleichung einer Fläche : (nx,ny,nz)*v=k : Vektor-Punkt <v> liegt auf der Ebene
  
  nx = (y2-y1)*(z3-z2)-(y3-y2)*(z2-z1);
  ny = (x3-x2)*(z2-z1)-(x2-x1)*(z3-z2);
  nz = (x2-x1)*(y3-y2)-(x3-x2)*(y2-y1);
  
  ns = sqrt( nx * nx + ny * ny + nz * nz );
  if( fabs(ns) < eps )
  {
    log( 1, "Dreiecksflaeche ist 0\n");
    return 0;
  };
  
  nx = nx / ns;
  ny = ny / ns;
  nz = nz / ns;
  
  if( nz == 0. ) 
  {
    log( 1, "Dreieck steht Kopf: nx=%f ny=%f nz=%f\n", nx, ny, nz );
    return 0;
  };
  
  k = nx * ( x1 + x2 + x3 ) / 3. + ny * ( y1 + y2 + y3 ) / 3. + nz * ( z1 + z2 + z3 ) / 3.;
  
  //  Linie l=p1 + alfa*(p2 - p1) : <p1> und <p2> sind Vektoren, die Punkte im Raum definieren
  //                                <alfa> ist eine Zahl von -HUGE_VAL bis +HUGE_VAL
  //  Wenn alfa > 0  und  alfa < 1, dann liegt Punkt-Vektor <l> zwischen <p1> und <p2>
  
  x1 = l->p1.x;
  y1 = l->p1.y;
  z1 = l->p1.z;
  
  x2 = l->p2.x;
  y2 = l->p2.y;
  z2 = l->p2.z;
  
  d1 = nx * ( x2 - x1 ) + ny * ( y2 - y1 ) + nz * ( z2 - z1 );
  
  if( fabs(d1) < eps )
    return 0;
  
  d2 = nx * x1 + ny * y1 + nz * z1; 
  
  //  Bei einem <alfa> : <l> == <v> : Linie schneidet die Ebene.
  
  double alfa, dif;
  
  alfa = l->alfa = ( k - d2 ) / d1;
  
  double xi, yi, zi;
  
  xi = x1 + alfa * ( x2 - x1 );
  yi = y1 + alfa * ( y2 - y1 );
  zi = z1 + alfa * ( z2 - z1 );
  
  if( ( dif = fabs( nx * xi + ny * yi + nz * zi - k ) ) > eps )
  {
    log( 0, "Fehler: dif=%f\n", dif ); 
    return -1;
  }
  
  if( l->alfa > -eps && l->alfa < 1.0 + eps )
  {
    return 1;
  }
  else
    return 0;
} // lbo_LineAndSlantPlaneCut