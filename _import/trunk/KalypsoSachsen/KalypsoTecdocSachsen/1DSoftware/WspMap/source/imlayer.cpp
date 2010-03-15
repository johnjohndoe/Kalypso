#include "stdafx.h"

#include "commonMFC/include/mfcHelper.h"

#include "improps.h"

#include "imlayer.h"

  ////////////////////////////
  //  Klasse  CImageLayer
  ///////////////////////////

IMPLEMENT_SERIAL(CImageLayer, CLayer, VERSIONABLE_SCHEMA|2)

CImageLayer::CImageLayer() : CLayer()
{
  SetType(image);
}

CImageLayer::CImageLayer( const CString& strBaseDirectory ) : CLayer( strBaseDirectory )
{
  SetType(image);
}

CImageLayer::CImageLayer( const CString& strBaseDirectory, const LPDISPATCH pDispatch ) : 
    CLayer( strBaseDirectory, pDispatch, FALSE )
{
	m_imageLayer.AttachDispatch(pDispatch);
  SetType(image);
}

CImageLayer::CImageLayer( const CString& strBaseDirectory, const CImageLayer& dispatchSrc ) :
    CLayer( strBaseDirectory, dispatchSrc, FALSE )
{
	m_imageLayer.AttachDispatch(dispatchSrc.m_imageLayer.m_lpDispatch);
  SetType(image);
}

CImageLayer::~CImageLayer()
{
}


CLayer* CImageLayer::Copy( const CString& newFileTitle )
// Kopiert den Layer in eine neue Datei und legt einen neuen Layer an
//
// Parameter:
//        const CString& fileName: absoluter oder relativer Pfad + Titel der neuen Datei
// Rückgabewert:
//        der neue ( kopierte ) Layer, NULL bei Misserfolg
// Bemerkung:
//        die Dateiendung der Datei wird beibehalten 
//        ( d.h. an newFileTitle angehängt )
{
  if ( LoadIsValid() )
  {
    CString newFilePath = newFileTitle + BCE::MfcHelper::GetFileExt( m_strGeoDatasetName );
    CString newFullPath;
    if ( newFilePath.Find( TCHAR(':') ) == -1 )
      newFullPath = m_strBaseDirectory + newFilePath;
    else
      newFullPath = newFilePath;
    if ( CopyFile( GetFullGeoDatasetName(), newFilePath, TRUE ) )
    {
      CImageLayer* newLayer = new CImageLayer;
      newLayer->SetGeoDatasetName( newFilePath );
      if ( newLayer->LoadIsValid() )
        return newLayer;
    };
  };

  return NULL;
}; // Copy


BOOL CImageLayer::CreateDispatch(LPCTSTR lpszProgID, COleException* pError)
{
	BOOL bRet;

	bRet = m_imageLayer.CreateDispatch(lpszProgID, pError);
	if (bRet)
		m_dispatchDriver.AttachDispatch(LPDISPATCH(m_imageLayer), FALSE);
	return bRet;
}

/* virtual */
void CImageLayer::SerializeProperties( CArchive& ar, BOOL bOnlyProps )
{
  if( ar.IsStoring() )
	{
    if( bOnlyProps )
      ar << GetRuntimeClass()->m_wSchema;

		ar << (WORD)GetTransparent();
		ar << (DWORD)GetTransparentColor();
	}
	else
	{
    int nVersion;

    if( bOnlyProps )
      ar >> nVersion;
    else
      nVersion = ar.GetObjectSchema();

    ar.SetObjectSchema( nVersion );
    switch( nVersion )
    {
    case 0:
    case 1:
    case 2:
      {
        WORD wTemp;
        DWORD dwTemp;
    
        ar >> wTemp;
        ar >> dwTemp;

        if( LoadIsValid() )
        {
          SetTransparent( (BOOL)wTemp );
          SetTransparentColor( (COLORREF)dwTemp );
        };
      }; 
      break;
      
    default:
      AfxThrowArchiveException(CArchiveException::badSchema);
      break;
    }
  }
}

BOOL CImageLayer::SetGeoDatasetName( const CString& path )
// lädt einen CMoImageLayer aus den durch path spezifizierten Raster - Daten
//
//  Rückgabe:
//        FALSE, falls die Datei nicht geladen werden konnte
// Parameter:
//        CString &path: absoluter oder relativer Pfad auf die Rasterdaten
{
 	VERIFY(CreateDispatch(TEXT("MapObjects2.ImageLayer")));

  CString relPath = BCE::MfcHelper::GetPathAsRelative( m_strBaseDirectory, path );

  if( relPath.Find( TCHAR(':') ) == -1 )
    m_imageLayer.SetFile( m_strBaseDirectory + relPath );
  else
    m_imageLayer.SetFile( relPath );

  if( !GetValid() )
  {
    m_strGeoDatasetName = "";
    return FALSE;
  };

  m_strGeoDatasetName = relPath;

  return TRUE;
}

BOOL CImageLayer::GetVisible()
{
	return m_imageLayer.GetVisible();
}

void CImageLayer::SetVisible(BOOL propVal)
{
	m_imageLayer.SetVisible(propVal);
}

CString CImageLayer::GetName()
{
	return m_imageLayer.GetName();
}

void CImageLayer::SetName(LPCTSTR propVal)
{
	m_imageLayer.SetName(propVal);
}

CMoRectangle CImageLayer::GetExtent()
{
	return m_imageLayer.GetExtent();
}

void CImageLayer::SetExtent(LPDISPATCH propVal)
{
	m_imageLayer.SetExtent(propVal);
}

CString CImageLayer::GetFile()
{
	return m_imageLayer.GetFile();
}

void CImageLayer::SetFile(LPCTSTR propVal)
{
	m_imageLayer.SetFile(propVal);
}

long CImageLayer::GetLayerType()
{
	return m_imageLayer.GetLayerType();
}

CString CImageLayer::GetTag()
{
	return m_imageLayer.GetTag();
}

void CImageLayer::SetTag(LPCTSTR propVal)
{
	m_imageLayer.SetTag(propVal);
}

BOOL CImageLayer::GetValid()
{
	return m_imageLayer.GetValid();
}

void CImageLayer::SetValid(BOOL propVal)
{
	m_imageLayer.SetValid(propVal);
}

BOOL CImageLayer::GetUpdateWhileDrawing()
{
	return m_imageLayer.GetUpdateWhileDrawing();
}

void CImageLayer::SetUpdateWhileDrawing(BOOL propVal)
{
	m_imageLayer.SetUpdateWhileDrawing(propVal);
}

BOOL CImageLayer::GetTransparent()
{
	return m_imageLayer.GetTransparent();
}

void CImageLayer::SetTransparent(BOOL propVal)
{
	m_imageLayer.SetTransparent(propVal);
}

unsigned long CImageLayer::GetTransparentColor()
{
	return m_imageLayer.GetTransparentColor();
}

void CImageLayer::SetTransparentColor(unsigned long propVal)
{
	m_imageLayer.SetTransparentColor(propVal);
}

/* virtual */
BOOL CImageLayer::ShowPropertyDialog( CMoMap& pMap, CWnd* pWnd )
{
  CImageProperties dlg( this );
  return dlg.DoModal() == IDOK;
}
