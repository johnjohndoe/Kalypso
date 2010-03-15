// Version.cpp: Implementierung der Klasse CVersion.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "Version.h"

//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

CVersion::CVersion( HMODULE hModule )
// Erzeugt die Versionsinformation zum angegebenen Modul
{
  // zuerst rausfinden, wie lang der Buffer sein muss
  const int strLength = 1000; // hoffentlich ist der Pfad nie länger als 1000 Zeichen
  LPTSTR strFile = (LPTSTR)malloc( strLength * sizeof( TCHAR ) ); 
  DWORD length = GetModuleFileName( hModule, strFile, strLength );
  if( length == 0 )
  {
    free( strFile );
    TRACE( "CVersion: unable to find Module-Filename for Module %X. Error Code = %d.\n", hModule, GetLastError() );
    return;
  };

  m_fileName = CString( strFile );

  DWORD hDummy; // Dummy für GetVileVersionInfoSize
  DWORD infoSize = GetFileVersionInfoSize( strFile, &hDummy );
   if( infoSize == 0 )
  {
    free( strFile );
    TRACE( "CVersion: InfoSize could not be retreaved. Error Code = %d.\n", GetLastError() );
    return;
  }

  // Speicher für info reservieren
  LPVOID pBlock = (LPVOID)malloc( infoSize );
  if( GetFileVersionInfo( strFile, hDummy, infoSize, pBlock ) == FALSE )
  {
    free( strFile );

    TRACE( "CVersion: VersionInfo could not be retreaved. Error Code = %d.\n", GetLastError() );
    return;
  };

  // noch die Zeit der Dateierzeugung auslesen
  HANDLE hFile = hFile = CreateFile( strFile, GENERIC_READ, FILE_SHARE_READ, NULL, 
    OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL );
  if( hFile == INVALID_HANDLE_VALUE ) 
  { 
    TRACE( "Could not open module file.");   // process error 
    return;
  }

  BY_HANDLE_FILE_INFORMATION fileInfo;
  GetFileInformationByHandle( hFile, &fileInfo );
  m_time = CTime( fileInfo.ftLastWriteTime );


  // strFile nicht mehr benötigt
  free( strFile );
  strFile = NULL;


  // jetzt die Versioninfos auslesen
  LPVOID lpBuffer = NULL;
  UINT len = 0;
  if( VerQueryValue( pBlock, TEXT("\\"), &lpBuffer, &len ) == FALSE )
  {
    delete lpBuffer;
    TRACE( "CVersion: Could not retreave Version-Info. Error Code = %d.\n", GetLastError() );
    return;
  }

  // das FileInfo kopieren
  memcpy( &m_fileInfo, lpBuffer, sizeof( VS_FIXEDFILEINFO ) );

  // lpBuffer nicht mehr benötigt
  lpBuffer = NULL;
  
  // pBlock nicht mehr benötigt
  free( pBlock );
  pBlock = NULL;

} // Konstruktor



/*!
 * Gibt die Productnummer als formatierten String zurück.
 * Die Produktnummer ist stets dreistellig.
 * Ist eine der Subnummer gleich 65535, so wird diese durch ein 'a'
 * ersetzt ( für Alpha Versionen )
 *
 * @return CString  : die Formatierte Versionsnummer
 */
CString CVersion::GetProductNr() const
{
  int nr[4];
  nr[0] = HIWORD(m_fileInfo.dwProductVersionMS);
  nr[1] = LOWORD(m_fileInfo.dwProductVersionMS);
  nr[2] = HIWORD(m_fileInfo.dwProductVersionLS);
  nr[3] = LOWORD(m_fileInfo.dwProductVersionLS);

  CString strNr[4];
  for( int i = 0; i < 4; i++ )
  {
    if( nr[i] == 65535 )
      strNr[i] = TEXT( "a" );
    else
      strNr[i].Format( TEXT( "%d" ), nr[i] );
  }

  CString vNummer;
  vNummer.Format( TEXT( "%s.%s.%s" ), strNr[0], strNr[1], strNr[2] );

  return vNummer;
} // GetProductNr

CString CVersion::GetFileNr() const
{
  CString nr;
  nr.Format( "%d.%d.%d.%d", HIWORD(m_fileInfo.dwFileVersionMS), LOWORD(m_fileInfo.dwFileVersionMS), 
    HIWORD(m_fileInfo.dwFileVersionLS), LOWORD(m_fileInfo.dwFileVersionLS) );

  return nr;
} // GetProductNr

CString CVersion::GetTimeString() const
// gibt Datum in der Form "September 2002" zurueck
{
  TCHAR buffer[1000];
  tm* time = GetTime().GetGmtTm( NULL );
  _tcsftime( (TCHAR*)(&buffer), 1000, "%B %Y", time );

  return CString( buffer );
}; // gibt das Datum in der Form "September 1993" zurück

CString CVersion::GetFileName() const
{
	return m_fileName;
}
