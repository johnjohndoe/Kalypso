// Hmo.h: Schnittstelle für die Klasse Hmo.
//
//////////////////////////////////////////////////////////////////////
//
// Die Klasse Hmo dient der der Manipulation von
// digitalen Geländemodellen (DGMs).
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_BCEHMO_H__FFD6E4A4_FDA1_11D4_BDD6_00104BB3E525__INCLUDED_)
#define AFX_BCEHMO_H__FFD6E4A4_FDA1_11D4_BDD6_00104BB3E525__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

namespace BCE
{
  class Hmo
  {
    // Typdefinitionen
  public:
    typedef struct Knoten
    {
      double x, y, z; // Koordinaten
    };
    
    typedef struct Dreieck
    {
      int i1, i2, i3;   // Indices der drei Ecken
    };
    
    typedef struct Kante
    {
      int i1, i2;
    };
    
    
    class IndexList
    {
    public:
      IndexList( const unsigned int grow );
      ~IndexList();
      
      void Set( const unsigned int from, const unsigned int to );
      unsigned int Get( const unsigned int from ) const ;
      unsigned int& operator[] ( unsigned int from );
      unsigned int operator[] ( unsigned int from ) const;
      
    private:
      unsigned int* indexList; // Array der Indices
      unsigned int grow;   // zuwachsFaktor
      unsigned int maxIndex; // bisher allokierte Einträge
    }; // class IndexList
    
    
    // Konstruktion / Destruktion
    public:
      Hmo( int zuwachs = 10000 );
      virtual ~Hmo();
      void DeleteContents();
      
      
      // Attribute
    public:
      int AddKnoten( double x, double y, double z );
      Knoten* GetKnoten( int i ) { return &(knotenListe[i]); };
      int GetKnotenCount() { return knotenAnzahl; };
      void DeleteKnoten();
      
      int AddKante( int i1, int i2 );
      Kante* GetKante( int i ) { return &(kantenListe[i]); };
      int GetKantenCount() { return kantenAnzahl; };
      void DeleteKanten();
      
      int AddDreieck( int i1, int i2, int i3 );
      Dreieck* GetDreieck( int i ) { return &(dreiecksListe[i]); };
      int GetDreieckCount() { return dreiecksAnzahl; };
      void DeleteDreiecke();
      
    private:
      Knoten* knotenListe;
      int knotenAnzahl;
      int maxKnoten;
      
      Kante* kantenListe;
      int kantenAnzahl;
      int maxKanten;
      
      Dreieck* dreiecksListe;
      int dreiecksAnzahl;
      int maxDreiecke;
      
      int zuwachsFaktor;
      
      // Operationen
    public:
      int ReadFromFile( const char* fileName );
      int WriteToFile( const char* hmoFileName );
      int WriteToPolygonShape( const char* shapeTitle, bool hasZ );

	  /** Schreibt die aktuiell Definierten (Bruch-)kanten als Linienshape raus. */
	  int WriteKantenAsLineShape( const char* shapeTitle, bool hasZ );
      
      int Triangulate( double quality = 0 );

      void DumpTrace( std::ostream& os );
      
    protected:
      int ReadHmoFile( FILE* file );
      int ReadSmsFile( FILE* file );
      int ReadNetFile( FILE* file );
      int ReadRasFile( FILE* file );
  }; // class Hmo
}; // namespace BCE


#endif // !defined(AFX_BCEHMO_H__FFD6E4A4_FDA1_11D4_BDD6_00104BB3E525__INCLUDED_)
