// Profil.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef PROFIL_H
#define PROFIL_H

#include "bce/include/csv.h"

class Section;
class DataBlock;
class DataBlockArray;
class Coord;
class TripleArray;

class Profil : public CObject
{
public:
  Profil::Profil( Section* pOwner = NULL );
	 ~Profil();
   
   Section* GetOwner();
   CString GetOriginalFile() const;
   CString GetClient( const int n ) const;
   CString GetProjectDesc( const int n ) const;
   CString GetPageDesc( const int n ) const;
   CString GetProjectNum() const;
   COleDateTime GetDate() const;
   CString GetPageNum() const;
   CString GetDrawingTitle() const;
   int GetNum() const;
   double GetAbstandShriftfeld() const;
   double GetBlattlaenge() const;
   double GetBezugshoehe() const;
   double GetXMaszstab() const;
   double GetYMaszstab() const;
   double GetBlatthoehe() const;
   int GetSchriftfeldUT() const;
   int GetWertenUD() const;
   int GetBezugshoeheUD() const;
   int GetSchriftfeldZUT() const;
   
   void SetModified();
   
   void SetOwner( Section* section );
   void SetOriginalFile( const CString& str );
   void SetClient( const int n, const CString& str );
   void SetProjectDesc( const int n, const CString& str );
   void SetPageDesc( const int n, const CString& str );
   void SetProjectNum( const CString& str );
   void SetDate( const COleDateTime& date );
   void SetPageNum( const CString& str );
   void SetNum( const int n );
   
   CString GetStateName() const;
   CString GetWaterName() const;
   void SetStateAndWaterName( LPCTSTR stateName, LPCTSTR waterName );
   void SetStateName( LPCTSTR stateName );
   void SetWaterName( LPCTSTR waterName );
   void SetStation( const double station );
   
   BOOL Load( const CString& filename );
   BOOL Save( const CString& filename );
   Profil* Clone(Section* sec);
   
   void AddDataBlock( DataBlock* db );
   void RemoveDataBlock( DataBlock* db );
   DataBlock* GetDataBlockByIndex( const int index );
   DataBlock* GetFirstDataBlock();
   DataBlock* GetNextDataBlock();
   DataBlock* GetDataBlock( const int type, const int index = 1 ); //Typ, Index des Datentyps
   DataBlock* GetDataBlock( const int type , const int index = 1 ) const;
   int GetDataBlockNum( const int type ) const;
   int GetNumDataBlocks() const;
   int GetDataBlockIndex( DataBlock* db );
   POSITION GetDataBlockPos() const;
   void SetDataBlockPos( const POSITION pos );
   double GetGelaendehoehe( const double yKrd );
   Coord* GetSchnittpunkt( Coord* aufpunkt, const double neigung, const BOOL vorwaerts );

   UINT DeleteDataBlocks( const CArray<int, int>& dbTypes );
   void Constrain( const double from, const double to );
   
   void AddComment( const CString& text );
   CString GetComment();
   
   CRect GetExtent() const;
   
   void Paint( CDC* dc, CRect* extent );
   
   Coord* GetSohlHoehe(Section* cs, bool DurchstroemterBereich);
   
   void GetSortedDBs( DataBlockArray* dbArray ) const;

   void FlipHorizontal();

   static Profil* CreateFromTriple( const TripleArray& tripArray );

 public:
   /** 
	* Writes Datablocks of this section in a CVS-Table
	* All values will be added to table, existings values are eventually overwritten, but nothing will be deleted.
	*/
	 void DumpCSV( BCE::CSV<double, CString, double>& table, const CString& columnPostfix );
   
protected:
  friend istream& operator>>(istream& is, Profil &prof);
  friend ostream& operator<<(ostream& os, const Profil &prof );
  
  friend ostream& operator<<(ostream& os, const CString &string );
  
protected:
  Section* m_pOwner;
  // 1. Zeile
  CString m_strOriginFile;
  // 2.-3. Zeilen
  CString m_strClient[2];
  // 4.-6. Zeilen
  CString m_strProjectDesc[3];
  // 7.-9. Zeilen
  CString m_strPageDesc[3];
  // 10. Zeile
  CString m_strProjectNum;
  // 11. Zeile
  COleDateTime m_date;
  // 12. Zeile
  CString m_strPageNum;
  int m_nControlSizes[6];
  CString m_strCrossRef[3];
  // 13. Zeile
  CString m_zeichnungsueberschrift;
  // 14. Zeile
  // Anzahl der Datensaetze u.s.w.
  // Wir brauchen diese Daten im class nicht aufnehmen.
  // 15. Zeile
  double m_dControlData[6];
  int m_nControlData[4];
  // folgende Zeilen
  CTypedPtrList<CObList, DataBlock*> m_DataBlocks;
  POSITION m_DBlockPos;
  int m_nNr;
  BOOL m_bModified;
  
};

#endif // PROFIL_H