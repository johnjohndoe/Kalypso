// DataBlck.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef DATABLCK_H
#define DATABLCK_H

#include "..\..\CommonMfc\CommonMfc.h"
#include "dtypes.h"

#define DB_SP_COMMENT 17
#define DB_SP_BEREICHE 23


class Profil;
class Coord;
class TextBlock;

class DataBlock : public CObject
{
public:
  struct Bereich
  {
    double from;
    double to;
    UINT style;
    COLORREF color;
    LONG hatch;
    CString text;
  };

public:
   DataBlock( Profil* pProfil, int type = DST_UNKNOWN );
	 ~DataBlock();
   
   DataBlock* Clone( Profil* pProfil );
   
   Profil* GetProfil();
   int GetType() const;
   int GetNumCoords() const;
   CString GetName( const int n ) const;
   void SetName( const CString& name, const int n );//23.09.2000
   CString GetName( const int n, const int type );
   CString GetDesc(int n);
   double GetDVar(int n);
   int GetNVar(int n);
   CString GetLine( const int n ) const;
   int GetLineCount() const;
   
   void SetType(int type);
   void SetCoordNum(int n);
   
   void AddCoord(Coord* crd);
   void RemoveCoord(Coord* crd);
   int GetCurrentCoordIndex();
   int GetCoordIndex(double x, double toleranz = 0.0000001);
   void InsertCoordAt(int n, Coord* crd);
   Coord* GetCoordAt(int n);
   void SetCoordAt(int n, Coord* crd);
   Coord* GetCoord(double ycrd, double dy);
   double GetDYAt( double dx, bool bExtrapolLinear );
   
   Coord* GetFirstCoord();
   Coord* GetNextCoord();
   Coord* GetLastCoord();
   Coord* GetPrevCoord();
   
   int GetControlData( const int index ) const{ return m_nControlData[index]; };
   void SetControlData(int position, int value);
   
   int GetSonderProfilType() const { return GetControlData( 8 ); };
   
   void SetProfil( Profil* profil );

   int GetImageType();
   static int GetImageType( int nType );
   
   // für DatenBlock "TEXT"
   const TextBlock* GetTextBlock() const { return m_textBlock; };
   TextBlock* GetTextBlock() { return m_textBlock; };
   void AddTextBlock( const int dbIndex, const int stationIndex, const CStringArray& strings );
   
   CString GetWSPName() const;
   void SetWSPName( const CString& name );
   
   // für DatenBlock "Comment"
   int GetCommentCount() const { return m_lines.GetSize(); };
   void SetCommentLine( const CString& commentLine ); //neue Kommentarlinie einfügen
   CString GetCommentLine( const int index ) const;

   
   // für DatenBlock "KREIS"
   double GetKreisDurchmesser() const { return m_dVars[0]; };
   double GetKreisSohlgefaelle() const { return m_dVars[1]; };
   double GetKreisY() const { return m_dVars[2]; };
   double GetKreisZ() const { return m_dVars[3]; };

   // für Datenblock "BEREICH"
   Bereich& GetBereich( const int n ) { return m_bereiche[n]; }; 
   void RemoveBereich( const int n ) { m_bereiche.RemoveAt( n ); };
   const int GetBereichCount() const { return m_bereiche.GetSize(); };

   CRect GetExtent();

   void FlipHorizontal();

   TextBlock* getTextBlock()   { return m_textBlock;  }
   
   // Operationen
public:
  void Paint( CDC* dc, CRect* extent );
  
  void SortCoordsByXs( BOOL bDeleteXs = TRUE );
  BOOL CheckData( CString& errorString );
  BOOL CheckAllYZero();
  
  friend istream& operator>>(istream& is, DataBlock &dat);
  friend ostream& operator<<(ostream& os, DataBlock &dat);
  
  
protected:
  void FindType();
  
protected:
  Profil* m_pProfil;
  int m_nType;
  int m_nCoord;
  
  // 1.-2. Zeilen
  CString m_name[2];
  
  // 3. Zeile
  int m_nControlData[9];
  
  // folgende Zeilen
  CTypedPtrArray<CObArray, Coord*> m_Coords;
  int m_nCoordIndex;
  
  // nur fuer Besonderprofile:
  CArray<double, double> m_dVars;
  CArray<int, int> m_nVars;
  CArray<CString, LPCSTR> m_lines;

private:
  CArray<Bereich, Bereich&> m_bereiche;
  TextBlock* m_textBlock;
};

class DataBlockArray : public CTypedPtrArray<CPtrArray, DataBlock*> {};

#endif // DATABLCK_H