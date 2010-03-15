// MfcHelper.cpp: Implementierung der Klasse MfcHelper.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "MfcHelper.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

CString BCE::MfcHelper::GetPathAsRelative( const CString& baseDir, const CString& absolutePath )
{
  // if absolutePath starts with baseDir, we can make it relative
  CString absoluteUP = absolutePath;
  absoluteUP.MakeUpper();
  CString baseUP = baseDir;
  baseUP.MakeUpper();


  if( absoluteUP.Find( baseUP ) == 0 )
    return absolutePath.Mid( baseDir.GetLength() );
  else
    return absolutePath;
}

CString BCE::MfcHelper::CreateAbsolutePath( const CString& baseDir, const CString& relativePath )
{
  if ( relativePath.Find( TCHAR(':') ) == -1 )
    return baseDir + relativePath;
  else
    return relativePath;
}

///////////////////////////////////////////////////////////////////////////
// File name handling
//
CString BCE::MfcHelper::GetFileDirectory( const CString& path )
{
  ASSERT(path.GetLength());
  int pos = path.ReverseFind('\\');
  if (pos >= 0)
    return path.Left(pos);
  return "";
}

CString BCE::MfcHelper::GetFileName( const CString& path )
{
  const int pos = path.ReverseFind( TCHAR( '\\' ) );
  if( pos >= 0 )
    return path.Right( path.GetLength() - pos - 1 );
  else
    return path;
}

CString BCE::MfcHelper::GetFileExt( const CString& path )
{
  const int pos = path.ReverseFind( TCHAR( '.' ) );
  if( pos >= 0 )
    return path.Right( path.GetLength() - pos - 1 );
  else
    return CString();
}

CString BCE::MfcHelper::GetFileTitle( const CString& path )
{
  CString strResult = GetFileName( path );
  const int pos = strResult.ReverseFind( TCHAR('.') );
  if( pos >= 0 )
    return strResult.Left( pos );
  else
    return strResult;
}

CString BCE::MfcHelper::GetFileWOExt( const CString& path )
{
  const int pos = path.ReverseFind( TCHAR( '.' ) );
  if( pos >= 0 )
    return path.Left( pos );
  else
    return path;
};

CString BCE::MfcHelper::GetFileDrive( const CString& path )
{
  const int pos = path.Find( TCHAR( ':' ) );
  if( pos >= 0 )
    return path.Left( pos );
  else
    return CString();
};

/**
 *	@param makeShpConform If true, the filename will be changed so that ArcView can read it (replace '.' with '_' etc.
 */
CString BCE::MfcHelper::GetUnusedFileName( const CString& base, const CString& ext, const BOOL makeShpConform /* = true */)
{
  CFileStatus fileStatus;
  
  CString fileName( base );
  if( makeShpConform )
	fileName.Replace( '.', '_' );
  
  int count = 0;
  while ( CFile::GetStatus( fileName + ext, fileStatus ) )
    fileName.Format( "%s-%d", base, ++count );
  
  return fileName;
};
