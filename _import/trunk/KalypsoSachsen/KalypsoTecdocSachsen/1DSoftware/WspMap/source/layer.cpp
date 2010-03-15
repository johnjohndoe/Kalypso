#include "stdafx.h"

//#include "..\..\wspprj\wspprj.h"
#include "commonMfc/include/mfcHelper.h"

#include "mapdoc.h"
#include "layer.h"

  ////////////////////////////
  //  Klasse  CLayer
  ///////////////////////////

IMPLEMENT_SERIAL(CLayer, CObject, VERSIONABLE_SCHEMA|2)

CLayer::CLayer()
{
};

CLayer::CLayer( const CString& strBaseDirectory )
{
  m_strBaseDirectory = strBaseDirectory;
  m_strGeoDatasetName = "";
}

CLayer::CLayer( const CString& strBaseDirectory, const LPDISPATCH pDispatch, const BOOL bAutoRelease)
{
	m_dispatchDriver.AttachDispatch(pDispatch, bAutoRelease);
  m_strGeoDatasetName = "";
  m_strBaseDirectory = strBaseDirectory;
}

CLayer::CLayer( const CString& strBaseDirectory, const CLayer& dispatchSrc, const BOOL bAutoRelease )
{
	m_dispatchDriver.AttachDispatch(dispatchSrc.m_dispatchDriver.m_lpDispatch, bAutoRelease);
  m_strBaseDirectory = strBaseDirectory;
  m_strGeoDatasetName = "";
}

CLayer::~CLayer()
{
}


void CLayer::Serialize(CArchive& ar)
{
	CObject::Serialize(ar);
	if (ar.IsStoring())
	{
		ar << GetGeoDatasetName();
		ar << (WORD)m_nType;
		ar << GetName();
    ar << GetTag();  // Belger November 2000
		ar << (WORD)GetVisible();
	}
	else
	{
    int nVersion = ar.GetObjectSchema();
    ar.SetObjectSchema( nVersion );

    if ( ar.m_pDocument )
      SetBaseDirectory( ((CMapDoc*)ar.m_pDocument)->GetMapPath() );

    switch (nVersion)
    {
    case 0:
    case 1:
    case 2:
      {
        CString str; //,str2;
        WORD wTemp;           
        
        ar >> str;
        str.MakeLower();
        
        SetGeoDatasetName(str);
        ar >> wTemp; 
        m_nType = (LayerType)wTemp;

        CString name, tag;
        ar >> name;
        ar >> tag;
        ar >> wTemp; 
        
        if( LoadIsValid() )
        {
          SetName( name );
          SetTag( tag );
          SetVisible( (BOOL)wTemp );
        };
      }; // case 0/1
      break;
      
    default:
      AfxThrowArchiveException(CArchiveException::badSchema);
      break;
    }
  }

  // jetzt die Properties der abgleiteten Layers serialisieren
  SerializeProperties( ar, FALSE );
}

CString CLayer::GetName()
{
	CString result;
	if (GetLayerType()==moMapLayer)
		m_dispatchDriver.GetProperty(0x1, VT_BSTR, (void*)&result);
	else
		m_dispatchDriver.GetProperty(0x2, VT_BSTR, (void*)&result);
	return result;
}

void CLayer::SetName(LPCTSTR propVal)
{
	if (GetLayerType()==moMapLayer)
		m_dispatchDriver.SetProperty(0x1, VT_BSTR, propVal);
	else
		m_dispatchDriver.SetProperty(0x2, VT_BSTR, propVal);
}

CMoRectangle CLayer::GetExtent()
{
	LPDISPATCH result;
	if (GetLayerType()==moMapLayer)
		m_dispatchDriver.GetProperty(0x4, VT_DISPATCH, (void*)&result);
	else
		m_dispatchDriver.GetProperty(0x5, VT_DISPATCH, (void*)&result);
	return result;
}

void CLayer::SetExtent(LPDISPATCH propVal)
{
	if (GetLayerType()==moMapLayer)
		m_dispatchDriver.SetProperty(0x4, VT_DISPATCH, propVal);
	else
		m_dispatchDriver.SetProperty(0x5, VT_DISPATCH, propVal);
}

BOOL CLayer::GetVisible()
{
	BOOL result;
	if (GetLayerType()==moMapLayer)
		m_dispatchDriver.GetProperty(0x3, VT_BOOL, (void*)&result);
	else
		m_dispatchDriver.GetProperty(0x1, VT_BOOL, (void*)&result);
	return result;
}

void CLayer::SetVisible(BOOL propVal)
{
	if (GetLayerType()==moMapLayer)
		m_dispatchDriver.SetProperty(0x3, VT_BOOL, propVal);
	else
		m_dispatchDriver.SetProperty(0x1, VT_BOOL, propVal);
}

long CLayer::GetLayerType()
{
	USES_CONVERSION;
	short		layerType = -1;
	LPCOLESTR	lpOleStr = T2COLE(TEXT("LayerType"));
	DISPID		dispID = -1;

	if (SUCCEEDED(LPDISPATCH(m_dispatchDriver)->GetIDsOfNames(IID_NULL,
		(LPOLESTR*)&lpOleStr, 1, 0, &dispID)))
	{
		m_dispatchDriver.GetProperty(dispID, VT_I2, &layerType);
	}

	return layerType;
}

CString CLayer::GetTag()
{
	CString result;
	if (GetLayerType()==moMapLayer)
		m_dispatchDriver.GetProperty(0x2, VT_BSTR, (void*)&result);
	else
		m_dispatchDriver.GetProperty(0x3, VT_BSTR, (void*)&result);
	return result;
}

void CLayer::SetTag(LPCTSTR propVal)
{
	if (GetLayerType()==moMapLayer)
		m_dispatchDriver.SetProperty(0x2, VT_BSTR, propVal);
	else
		m_dispatchDriver.SetProperty(0x3, VT_BSTR, propVal);
}

BOOL CLayer::GetValid()
{
	BOOL result;
	if (GetLayerType()==moMapLayer)
		m_dispatchDriver.GetProperty(0xc, VT_BOOL, (void*)&result);
	else
		m_dispatchDriver.GetProperty(0x8, VT_BOOL, (void*)&result);
	return result;
}

void CLayer::SetValid(BOOL propVal)
{
	if (GetLayerType()==moMapLayer)
		m_dispatchDriver.SetProperty(0xc, VT_BOOL, propVal);
	else
		m_dispatchDriver.SetProperty(0x8, VT_BOOL, propVal);
}

const CString CLayer::GetFullGeoDatasetName()
// gibt den vollen Pfad auf die unterliegende Datei
// ist der Pfad relativ, so wird das kartenverzeichnis vorne angehängt
// Rückgabewert:
//            CString: ein absoluter Pfad
{
  return BCE::MfcHelper::CreateAbsolutePath( m_strBaseDirectory, m_strGeoDatasetName );
};


void CLayer::LoadProperties( const CString& path, CMoMap& map )
{
  CFile file( path, CFile::modeRead );
  CArchive ar( &file, CArchive::load );

  SerializeProperties( ar, TRUE );

  if( GetLayerType() == moMapLayer )
    ((CMapLayer*)this)->ApplyRenderer( map, TRUE );
}

void CLayer::SaveProperties( const CString& path )
{
  CFile file( path, CFile::modeCreate | CFile::modeWrite );
  CArchive ar( &file, CArchive::store );

  SerializeProperties( ar, TRUE );
}
