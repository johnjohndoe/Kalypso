// Include Datei für Programme, welche die TinCut Lib benutzen

#ifndef _TINCUT_H_INCLUDED_
#define _TINCUT_H_INCLUDED_

#include <ostream>
#include <vector>

#include "observable.h"

namespace BCE
{ 
  class StringResource;

  class TinCut : public Observable
  {
    // Parameter für die run-Funktion ( für TinCut als Thread )
    struct RunParam
    {
      TinCut* tinCut;
      const char* lowerHmo;
      const char* upperHmo;
      const char* shapeBase;

      bool b3D;
    }; // struct RunParam

    ////////////////////////
    // globale Konstanten //
    ////////////////////////

  private:
    static const double eps;  // = 0.00001;
    static const double epst; // = 0.00000001; // spezielles epsilon für Test der Eingangadaten
    static const int SUCCESS; //  = 0; // Rückgabewert der meissten Funktionen

    static const StringResource stringRes;

    /////////////////////////////
    // globale Typdefinitionen //
    /////////////////////////////

    // n=Nummer;bot,top=Anfangs- and Endknoten;lft,rgt=links- und rechtsanliegende Elemente.
    typedef struct Arc
    {
      int n, bot, top, lft, rgt; 
      double xc, yc; 
    }; // struct Arc

    // xl,yl=maximale Projektionen aller Kanten (xl,yl muessen nicht der gleichen Kante gehoeren)
    typedef struct MaxMinArc
    {
      double xl, yl; 
    }; // struct MaxMinArc

    // x,y,z = Koordinaten,Pz = Projektion auf die andere Ebene
    typedef struct Point 
    {
      int n; 
      double x, y, z, Pz;
    }; // struct Point

    // alfa :: po=p1+alfa*(p2-p1)
    typedef struct Line
    {
      int n; 
      Point p1, p2; 
      double alfa;
    }; // struct Line

    // Extremale Koordinaten aller Punkte
    typedef struct MaxMinPoint
    { 
      int nmax, nmin; 
      double xmax, ymax, zmax, xmin, ymin, zmin; 
    }; // struct MaxMinPoint


    // i = Eckknoten, a = Kanten, ...c = Koordinaten des Zentralpunktes,
    // x,y,z = maximale Projektionen von drei Vektoren (Zentrum -> Ecke), die nicht unbedingt
    // einem Vektor gehoeren
    typedef struct Triangle
    {
      int n, i1, i2, i3;
      Arc a1, a2, a3;
      double xc, yc, zc, zP, x, y, z, area;
    }; // struct Triangle

    // Maximale Nummer und Projektionen des obenerwaehnten Vektor, die nicht dem gleichen Dreieck
    // gehoeren muessen
    typedef struct MaxMinTriangle 
    {
      int nmax; 
      double xmax, ymax, zmax; 
      double minarea; 
    }; // struct MaxMinTriangle

    // area = Flaecheninhalt, Parea = Projektionsflaecheninhalt
    typedef struct Polygon 
    {
      int n; 
      double Parea, area, volume, perimeter;
    }; // struct Polygon


    /////////////
    // Members //
    /////////////

    // Konstruktor
  public:
    TinCut();

    // Attribute
  public:
    // Die Log-Streams: in diese Streams werden alle Daten geschrieben
    void addLogStream( const std::ostream& stream );
    bool removeLogStream( std::ostream& stream );

    //  Verbose - Level: je höher die Zahl desto mehr Informationen werden angezeigt
    //  0: Standard
    //  1: Eingabedateien werden zu shape konvertiert
    //  2: einige Zwischenergebnisse
    //  3: ab hier erfolgen zusätzliche Debug-Meldungen in der Log-Datei
    int getVerboseLevel() const { return m_verboseLevel; };
    void setVerboseLevel( const int level ) { m_verboseLevel = level; };

  private:
    const char* getIDString( const unsigned int stringID ) const;

    std::vector<const std::ostream*> m_logStreams;

    int m_sd[3];

    unsigned short m_verboseLevel; // das VerboseLevel

    int m_pointCapas[2]; // Kapazitäten der Punkte Arrays
    int m_pointMinCapas[2]; // minimale Kapazitäten der Punkte Arrays
    int m_pointGrow[2]; // Grössen des Zuwachses der Punkte Arrays
    
    int m_trnglCapas[2]; // Kapazitäten der Dreieck Arrays
    int m_trnglMinCapas[2]; // minimale Kapazitäten der Punkte Arrays
    int m_trnglGrow[2]; // Grössen des Zuwachses der Dreiecke Arrays

    int m_arcCapas[2]; // Kapazitäten der Dreieck Arrays
    int m_arcGrow[2]; // Grössen des Zuwachses der Dreiecke Arrays

    Point** p; // wird globals verwendet
    Triangle** t;

    // Operationen
  public:
    int cut( const char* lowerHmo, const char* upperHmo, const char* outputPath, const bool b3D );
    static unsigned int _cdecl run( void* pParam );

  private:
    void log( const int vLevel, const int stringID, ... ) const;
    void log( const int vLevel, const char* text, ... ) const;
    void vLog( const int vLevel, const std::string& text, const va_list argPtr ) const;
    
    void allocatePoint( Point** p, const int index, const int size );
    void allocateTriangle( Triangle** t, const int index, const int size );
    void allocateArc( Arc** arc, const int index, const int size );

    // Hilfsfuntionen
  private:
    Arc** allocateArcs( unsigned int size0, unsigned int size1 );
    void freeArcs( Arc** a );

    int lbo_input( const std::string& upperHmo, const std::string& lowerHmo, Point **p, Triangle **t );
    
    int lbo_MaxMinKoordinaten( int j, MaxMinPoint* e, Point** p );
    
    int lbo_KoordinatenVerschieben( MaxMinPoint* e, Point** p,int sign );
    
    void lbo_QuickSortDreieckeNachX( int lt,int rt,Triangle *t );
    void lbo_QuickSortStructPointNachNummer( int lt, int rt, Point* p );
    void lbo_QuickSortKantenNachX( int lt, int rt, Arc *a );
    void lbo_QuickSortKoordinatenNachX( int lt,int rt, Point* p, int* o );
    
    int lbo_DreiecksLaufendeNummer( Point* p, Triangle* t );
    
    int lbo_ParameterEinesDreiecks( int i, Point* p, Triangle* t, bool meldung = 0 );
    int lbo_TriangleArea( Point* p,Triangle* t, bool meldung );
    
    int lbo_RadiusVektor( Point* p, Triangle* t );
    int lbo_RadiusVektorDesDreiecks( Point* p, Triangle* t );
    
    int lbo_PunktProjektionen( Point** p, Triangle** t );
    
    int lbo_ShapePunkte( const char *ausgabePfad, const char* ausgabeerweiterung, Point* p, MaxMinPoint* e );
    int lbo_ShapeLinie( const char* ausgabePfad, const char* ausgabeerweiterung, Point* p, Arc* a, MaxMinPoint* e );
    int lbo_ShapeFlaeche( const char* ausgabePfad, const char* ausgabeerweiterung, Point* p, Triangle* t, MaxMinPoint* e, int sort = 10, bool b2D = false );
    int lbo_ShapeAuftragAbtrag( const char* ausgabePfad, const char* ausgabeerweiterung, Point* p, Triangle* t, MaxMinPoint* e );
    int lbo_ShapeGrenzlinien( const char* ausgabePfad, char* kommentar, Arc* g, int* o, 
      Point* p, MaxMinPoint* e, bool poly = false );

    int lbo_KantenDoppelt( Triangle* t, Arc* a, int j, bool nullDreiecke = false );
    
    void lbo_KantenEinzelnNachX( Arc* a, int meldung = 0 );
    
    int lbo_GrenzKanten( Arc* g, Arc* a );
    
    int lbo_MaxKantenLaenge( Arc* g, Point* p, MaxMinArc *ea );

    int lbo_HoehePunkteKante( int i, int j, double xb, double yb, double xt, double yt,
      double Xb, double Yb, double Xt, double Yt,
      double Xx, double Yy, Point **p, Arc **g, MaxMinPoint *e );
    int lbo_NeuePunkteUndKanten( Point** p, Arc** g, MaxMinPoint* e, MaxMinArc* ea );

    int lbo_OuterArcsAway( Arc* g, Point* p );
    int lbo_OuterTrianglAway( Point** p, Triangle** t );

    int lbo_ExtremDreiecksWerte( Triangle* t, MaxMinTriangle* et );

    int lbo_DreieckeUndPunkteVomSchnitt( int Tn,Triangle *t,Point *pbot,Point *ptop,Point *pt,
      MaxMinPoint *e,MaxMinTriangle *et, int index );
    int lbo_DreieckeVomPunkt( int Tn, Triangle* t, Point* pi, Point* pt, MaxMinPoint* e,MaxMinTriangle* et, const int index );
    int lbo_ArcCutTrngl( int t0i, Arc *g, int Tn, Triangle *t, Point *pg, Point *pt, MaxMinPoint* e,
                    MaxMinTriangle* et, const int index );
    int lbo_KanteIstWeitVomDreieck( Arc* g, Triangle* t, Point* p );
    int lbo_NeuePunkteUndDreieckeAnDerPolygonGrenze( Point** p, Arc** g, Triangle** t, MaxMinArc* ea,
      MaxMinPoint* e, MaxMinTriangle* et );

    int lbo_ProjektionenDesDreiecksZentrum( Triangle** t, Point**p );

    int lbo_3TrianglesFromInsidePoint( int Tn, Triangle *tj, Point *po, Point *pj, MaxMinPoint *e,
                                   MaxMinTriangle *et, const int index );
    int lbo_3TrianglesFromBoundaryPoints( int Tn, Triangle* tj, Point* op, Point* pj,
      MaxMinPoint* e, MaxMinTriangle* et, int* sd, const int index );
    int lbo_2TrianglesFromBoundaryPoint( int Tn, Triangle* tj, Point* op, Point* pj, MaxMinPoint* e,
                                                 MaxMinTriangle* et, int sd, const int index );
    int lbo_StreckenLaenge( Point *p1, Point *p2 );
    int lbo_LineCutsSidesOfTheTriangle( int i, Point *pj, Triangle *tj, Point *po, Point *op, int *sd );
    int lbo_PointLiesInPlain( int i, Point *pj, Triangle *tj, Point *po );
    int lbo_AdditionalPointsAndTriangles( int mT, Point *pj, Triangle *tj, Point *po, MaxMinPoint *e,
                                      MaxMinTriangle *et, const int index );
    int	lbo_TriangleSidesAndSlantPlaneCut( Point *pj, Triangle *tji, Point *pk, Triangle *tkn, 
                                       Point *po);
    int lbo_DieZweiDreieckeSchneidenNicht( Triangle *tji, Triangle *tkn );

    int lbo_AufUndAbtragsPunkteGrenzeDreiecke( Point** p, Triangle** t, MaxMinPoint* e, MaxMinTriangle* et );

    int lbo_DreieckeMitNegativenNummernWeg( Triangle** t, MaxMinTriangle* et );

    int lbo_Initializierung( int no, int* o, int na, Arc* a, int np, Point* p, const int index );

    int lbo_UmkehrNummern( Point* p,int* o, int* O );

    int lbo_DreiecksNummernAenderung( Point* p, Triangle* t, int* o );

    int lbo_GrenzkantenNummernAendern( Point* p, Arc* g, int* o );
    
    int lbo_PunktFiltration( Point* p, int* o, int ebene );

    int lbo_AbUndAuftragsDreiecke( Point* p, Triangle* t, int j );
    
    void lbo_QuickFindStructPointNachNummer( int* i, int lt, int rt, int n, Point* p );
    void lbo_QuickFindStructPointNachX( int* lt, int* rt, double xl, double xr, Point* p );
    void lbo_QuickFindEineKantenFuerStrecke( int *lt, int *rt, double xl, double xr, Arc *a );
    void lbo_QuickFindEinDreieckFuerStrecke ( int* lt, int* rt, double xl, double xr, Triangle* t );

    int lbo_pointInOutDreieck( Point* q, Point* p, Triangle* t );

    int lbo_HoeheInderStrecke( double x, double y, double z, double X, double Y, double Z, 
                           double xX,double yY, double* zZ );

    int lbo_SchnittpunktZweierGeraden( double x1, double y1, double x2, double y2, 
      double x3, double y3, double x4, double y4, double *Xx, double *Yy);

    int lbo_ILL3( double* beta1, double* b1, double* d1,
             double* beta2, double* b2, double* d2, double* s);

    int lbo_LineAndSlantPlaneCut( Line* l, Triangle* t, Point* p );
  }; // class TinCut
}; // NameSpace BCE


#endif // _TINCUT_H_INCLUDED_
