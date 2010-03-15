// zuordnung.cpp: Implementierung der Klasse CZListe.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "commonMFC/include/mfcHelper.h"

#include "zuordnung.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif


/////////////////////////////////////////
// Klasse CZTable ( ZurdnungsTabelle ) //
/////////////////////////////////////////

CZTable::CZTable( const CString& fileName )
{
  m_fileName = fileName;
  m_size = 0;
};

CZTable::~CZTable()
{
  DeleteContents();
};


CString CZTable::GetName() const
{
  return BCE::MfcHelper::GetFileTitle( m_fileName ); 
};

void CZTable::DeleteContents()
// löscht den Inhalt der Zuordnungstabelle
{
  RemoveAllRows();

  m_size = 0;
  dataNames.RemoveAll();
}; // DeleteContents

void CZTable::RemoveAllRows()
{
  for( int i = 0; i < m_zuordnungen.GetSize(); i++ )
    delete m_zuordnungen[i];
  
  m_zuordnungen.RemoveAll();
}; // löscht alle Zeilen aus der Tabelle

BOOL CZTable::Load()
// lädt die Zuordnungstabelle
// Bemerkung:
//        in jedem Fall enthält die Tabelle nach dieser Funktion keine Daten mehr
{
  BOOL bReturn = TRUE;

  // mögliche Daten zerstören
  DeleteContents();

  try
  {
    CString helpStr; 

    CStdioFile file( m_fileName, CFile::modeRead );

    // zuerst tabellenweiten Daten lesen
    file.ReadString( helpStr ); // den Tabellennamen und Anzahl einträge auslesen und vergessen
    file.ReadString( helpStr );
    file.ReadString( helpStr ); // Leerzeile

    // jetzt den Tabellenkopf lesen und Spalten erzeugen
    file.ReadString( helpStr );
    helpStr.TrimLeft();
    int count = 0;
    while( !helpStr.IsEmpty() )
    {
      count++;
      int find = helpStr.Find( TEXT(' ') );
      if ( find == -1 )
        find = helpStr.GetLength(); // falls kein Leerzeichen gefunden, alles nehmen
      CString found = helpStr.Left( find );
      helpStr = helpStr.Mid( find ); // den HilfsString um den gerade gelesenen Eintrag kürzen
      helpStr.TrimLeft();

      if( count > 2 && !helpStr.IsEmpty() ) // die beiden ersten Tabelleneinträge: von, bis nicht aufnehmen, sowie den letzten: Text auch nicht
        AddColumn( found );
    }; // while !helpStr.IsEmpty()

    // jetzt zuordnungen lesen
    while( file.ReadString( helpStr ) )
    {
      int von, bis;

      if( sscanf( helpStr, "%d %d", &von, &bis ) != 2 )
        return FALSE;

      int index = AddRow( von, bis );

      // den String um die beiden gerade eingelesenen Parameter lürzen
      helpStr = helpStr.Mid( helpStr.Find( TEXT(' ') ) );
      helpStr.TrimLeft();
      helpStr = helpStr.Mid( helpStr.Find( TEXT(' ') ) );
      helpStr.TrimLeft();
      
      for( int j = 0; j < m_size; j++ )
      {
        double value;
        if( sscanf( helpStr, "%lf", &value ) != 1 )
          return FALSE;

        SetValue( index, j, value );

        helpStr = helpStr.Mid( helpStr.Find( TEXT(' ') ) );
        helpStr.TrimLeft();
      }; // for j

      // jetzt den Rest des Strings als Text interpretieren
      SetComment( index, helpStr );
    }; // while file.ReadString
  }
  catch( CFileException* e )
  {
    e->Delete();
    bReturn = FALSE;
  }; // try-catch

  return bReturn;
}; // Load

BOOL CZTable::Save()
// speichert die Zuordnungstabelle
{
  BOOL bReturn = TRUE;

  try
  {
    CString helpStr; // Hilfsstring für formatierte Ausgabe

    CStdioFile file( m_fileName, CFile::modeWrite | CFile::modeCreate );
    
    // zuerst Tabellenweiten Daten rausschreiben
    file.WriteString( BCE::MfcHelper::GetFileTitle( m_fileName ) + "\n" );
    helpStr.Format( TEXT("Einträge: %d\n"), m_size );
    file.WriteString( helpStr );
    file.WriteString( TEXT("\n") );

    // jetzt den Tabellenkopf rausschreiben
    helpStr.Empty();
    helpStr.Format( "%14.14s %14.14s ", TEXT("von"), TEXT("bis") );
    for( int i = 0; i < m_size; i++ )
    {
      CString formatStr;
      formatStr.Format( "%14.14s ", dataNames[i] );
      helpStr += formatStr;
    }; // for i 
    helpStr += TEXT( "Text\n" );
    
    file.WriteString( helpStr );

    // jetzt die einzelnen Zuordnungen rausschreiben
    for( i = 0; i < GetRowCount(); i++ )
    {
      helpStr.Format( "%14.14d %14.14d ", GetVon( i ), GetBis( i ) );

      for( int j = 0; j < m_size; j++ )
      {
        CString str;
        str.Format( "%14.4lf ", GetValue( j, i ) );
        helpStr += str;
      }; // for j
      helpStr += GetComment( i );
      file.WriteString( helpStr + "\n" );
    }; // for i
  }
  catch( CFileException* e )
  {
    e->Delete();
    bReturn = FALSE;
  }; // try-catch

  return bReturn;
}; // Save
	
  // Attribute
int CZTable::AddColumn( const CString& name )
// fügt eine neue Spalte in die Tabelle ein
// Parameter:
//          const CString& name: die Spaltenüberschrift
// Rückgabewert:
//          int: -1, falls dieser Name schon vorhanden ist, sonst der Index der neuen Spalte
{
  dataNames.SetAtGrow( m_size++, name );
  return m_size - 1;
}; // AddColumn

void CZTable::RemoveColumn( const CString& name )
// löscht eine Spalte komplett
// Parameter:
//      const CString& name: die Spaltenüberschrift
{
  int index = GetColumnIndex( name );
  if( index != -1 )
  {
    dataNames.RemoveAt( index );
    m_size--;

    // auch die eigentlichen Daten löschen
    for( int i = 0; GetRowCount(); i++ )
    {
      Zuordnung* z = m_zuordnungen[i];
      z->data.RemoveAt( index );
    };
  }; // if index != -1
}

void CZTable::RemoveAllColumns()
// löscht alle Spalten dieser Tabelle
{
  while( GetColumnCount() > 0 )
    RemoveColumn( GetColumnHeader( 0 ) );
} // RemoveAllColumns

int CZTable::AddRow( const int von, const int bis )
// fügt einen neuen Eintrag in die Tabelle ein
{
  Zuordnung* zo = new Zuordnung;
  zo->m_von = von;
  zo->m_bis = bis;
  return m_zuordnungen.Add( zo );
}; // AddEntry

BOOL CZTable::SetValue( const int index, const int columnIndex, const double value )
{
  Zuordnung* zo = m_zuordnungen[index];
  if( columnIndex > m_size )
    return FALSE;
  else
  {
    zo->data.SetAtGrow( columnIndex, value );
    return TRUE;
  }; // if columnIndex
}; // SetValue

double CZTable::GetValue( const int columnIndex, const int index ) const
// gibt den Wert der Spalte mit Überschrift dataName zurück, in der index. Zeile
{
  if( columnIndex >= 0 && columnIndex < m_size )
    return m_zuordnungen[index]->data[(int)columnIndex];
  else
    return 0.0;
}; // GetValue

double CZTable::GetValue( int type, const CString& columnStr )
// ordnet einem Typ type den Wert der Spalte mit Überschrift columnStr zu
// Parameter:
//        int type: zuzuordnender Wert
//        const CString& columnStr: die Spaltenüberschrift
// Rückgabewert:
//        double, der zugeordnete Wert, 0.0 bei Fehler
{
  // zuerst den SpaltenIndex suchen
  int i = GetColumnIndex( columnStr );
  int j = GetRowIndex( type );
  if( i == -1 || j == -1 )
    return 0.0;
  else
    return GetValue( i, j );
}; // GetValue

int CZTable::GetColumnIndex( const CString& name ) const
// gibt den Index der Spalte mit Überschrift name zurück
// Parameter:
//        const CString& name: Überschrift der gesuchten Spalte
// Rückgabewert:
//        int: Index der gefundenen Spalte, -1 bei Misserfolg
{
  for( int i = 0; i < GetColumnCount(); i++ )
  {
    if( name.CompareNoCase( GetColumnHeader( i ) ) == 0 )
      return i;
  }; // for i

  return -1;
}; // GetcolumnIndex

int CZTable::GetRowIndex( const int type ) const
// findet die Zeile, welche dem Typ type zugeordnet ist
// d.h. die erste Zeile für welche gilt: von < type < bis
// Parameter:
//        const int type: der gesuchte Wert
// Rückgabewert:
//        int. Index der gefundenen Zeile, -1 bei Misserfolg
{
  for( int j = 0; j < GetRowCount(); j++ )
  {
    if( type >= GetVon( j ) && type <= GetBis( j ) )
      return j;
  }; // for j

  return -1; // nichts gefunden
}; // GetIndex

///////////////////////////////////////
// Klasse CZListe ( ZurdnungsListe ) //
///////////////////////////////////////

CZList::CZList( CString directory )
{
  m_directory = directory;
};

CZList::~CZList()
{
  DeleteContents();
};

void CZList::DeleteContents()
// löscht alle Daten
{
  for( int i = 0; i < GetZTableCount(); i++ )
    delete m_tables[i];
  m_tables.RemoveAll();
}; // DeleteContents

BOOL CZList::Load()
// Lädt alle Tabellen aus dem Tabellenverzeichnis
{
  // testen, ob das Verzeichnis vorhanden ist
  CFileStatus fileStatus;
  CFile::GetStatus( m_directory, fileStatus );
  if( !( fileStatus.m_attribute | CFile::directory ) )
    return FALSE;

  // das Verzeichnis existiert, jetzt alle Dateien, welche die Endung .ztb ( Zurordnungstabelle ) haben, öffnen
  CFileFind finder;
  if( finder.FindFile( m_directory + "\\*.ztb" ) )
  {
    BOOL bNotLast;
    do
    {
      bNotLast = finder.FindNextFile();// while 
      CZTable* zTable = new CZTable( finder.GetFilePath() );
      if( zTable->Load() )
        AddZTable( zTable );
      else
        delete zTable;
    }
    while( bNotLast );

  }; // if finder.Findfile

  return TRUE;
}; // Load

BOOL CZList::Save()
{
  // zuerst das Verzeichnis erzeugen, falls es noch nicht existiert
  CreateDirectory( m_directory, NULL );

  // jetzt einfach alle Tabellen rauschreiben
  BOOL bReturn = TRUE;
  for( int i = 0; i < GetZTableCount(); i++ )
    bReturn &= m_tables[i]->Save();

  return bReturn;
}; // Save

int CZList::AddZTable( const CString& name )
// erzeugt eine Neue Tabelle namens 'name' und fügt sie der Liste hinzu
{
  CZTable* zTable = new CZTable( m_directory + "\\" + name + ".ztb" );
  return AddZTable( zTable );
}; // AddZTable