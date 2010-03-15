//////////////////////////////////////////////////////////////////////
// zuordnung.h: Schnittstelle für die Klassen CZListe und CZTable
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_ZUORDNUNG_H__37A62C93_AB3B_11D5_BE84_00104BB3E525__INCLUDED_)
#define AFX_ZUORDNUNG_H__37A62C93_AB3B_11D5_BE84_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000


/////////////////////////////////////////
// Klasse CZTable ( ZurdnungsTabelle ) //
/////////////////////////////////////////

class CZTable
{
  // der Datentype Zuordnung
  struct Zuordnung
  {
    int m_von, m_bis; // Klasseneinteilung des Definitionsbereichs
    CArray<double, double> data;
    CString comment; // Kommentar
  }; // struct Zuordnung

public:
	CZTable( const CString& fileName );
	~CZTable();

  // Operationen
public:
  BOOL Load();
	BOOL Save();

protected:
  void DeleteContents();

  // Attribute
public:
  int AddColumn( const CString& name );
  void RemoveColumn( const CString& name );
  void RemoveAllColumns();

  int AddRow( const int von, const int bis );
  void RemoveRow( const int index ) { m_zuordnungen.RemoveAt( index ); };
  void RemoveAllRows();
  
  BOOL SetValue( const int index, const int columnIndex, const double value );
  double GetValue( const int columnIndex, const int index ) const;
  double GetValue( int type, const CString& columnStr );
  
  int GetRowIndex( const int type ) const;
  int GetColumnIndex( const CString& name ) const;

  void SetComment( const int index, const CString& comment ) { m_zuordnungen[index]->comment = comment; };
  CString GetComment( const int index ) const { return m_zuordnungen[index]->comment; };

  void SetVon( const int index, const int von ) { m_zuordnungen[index]->m_von = von; };
  int GetVon( const int index ) const { return m_zuordnungen[index]->m_von; };
  void SetBis( const int index, const int bis ) { m_zuordnungen[index]->m_bis = bis; };
  int GetBis( const int index ) const { return m_zuordnungen[index]->m_von; };

  int GetRowCount() const { return m_zuordnungen.GetSize(); };
  int GetColumnCount() const { return m_size; };
  CString GetColumnHeader( const int index ) const { return dataNames[index]; };

  CString GetFileName() const { return m_fileName; };
  CString GetName() const;

protected:
  CString m_fileName;
  CTypedPtrArray<CPtrArray, Zuordnung*> m_zuordnungen;
  int m_size; // Anzahl der zugeordneten Werte
  CStringArray dataNames;
}; // class CZTable


///////////////////////////////////////
// Klasse CZListe ( ZurdnungsListe ) //
///////////////////////////////////////

class CZList
{
public:
	CZList( CString directory );
	~CZList();

  // operationen
public:
	BOOL Load();
	BOOL Save();

  int AddZTable( CZTable* zt ) { return m_tables.Add( zt ); };
  int AddZTable( const CString& name );
  void RemoveZTable( int index ) { m_tables.RemoveAt( index ); };
  CZTable* GetZTable( int index ) const { return m_tables[index]; };
  int GetZTableCount() const { return m_tables.GetSize(); };

protected:
  void DeleteContents();

  // Attribute
protected:
  CString m_directory;
  CTypedPtrArray<CPtrArray, CZTable*> m_tables;
};

#endif // !defined(AFX_ZUORDNUNG_H__37A62C93_AB3B_11D5_BE84_00104BB3E525__INCLUDED_)
