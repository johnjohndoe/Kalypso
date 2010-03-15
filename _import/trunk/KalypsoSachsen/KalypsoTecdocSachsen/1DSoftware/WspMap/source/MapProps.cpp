// MapData.cpp: Implementierung der Klasse CMapData.
//
//////////////////////////////////////////////////////////////////////
//
// Serialisierbare Daten von CMapDoc: eigene Klasse nötig für Vorlagen
// Eine vorlage soll einfach ein einzelnes serialisiertes CMapData 
// Object sein
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "resource.h"

#include "dtypes.h"

#include "mapLayout.h"
#include "layerdata.h"

#include "MapProps.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif


IMPLEMENT_SERIAL( CMapProperties, CObject, VERSIONABLE_SCHEMA | 4 )

//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

CMapProperties::CMapProperties()
{
  windowRect = CRect( 0, 0, 100, 100 );
  windowState = SW_SHOWMAXIMIZED;
  m_mapLayout = NULL;
}

CMapProperties::~CMapProperties()
{
  DeleteContents();
}


//////////////////////////////////////////////////////////////////////
// Operationen
//////////////////////////////////////////////////////////////////////

void CMapProperties::DeleteContents()
{
  for ( int i = 0; i < m_layerData.GetSize(); i++ )
  {
    if ( m_layerData[i] )
      delete m_layerData[i];
  }; // for i
  m_layerData.RemoveAll();

  delete m_mapLayout;
}; // DeleteContents

void CMapProperties::Serialize( CArchive &ar )
{
  // zuerst m_layerData serialisieren
  if ( ar.IsStoring() )
  {
    // jetzt die Daten laden
    ar << windowRect;
    ar << windowState;
    dockState.Serialize( ar );

    ar << barRects.GetCount();
    POSITION pos = barRects.GetStartPosition();
    while ( pos )
    {
      UINT barID;
      CRect barRect;
      barRects.GetNextAssoc( pos, barID, barRect );
      ar << barID << barRect;
    }; // while pos

    // auch das MapLayout serialisieren
    ar.WriteObject( m_mapLayout );

  } // IsStoring
  else
  {
    // Vorsichtsheitshalber auf jeden Fall das MapLayout zerstören
    delete m_mapLayout;
    m_mapLayout = NULL;
    
    // dier Versionsnummer holen
    UINT nVersion = ar.GetObjectSchema();

    // Früher wurden die LayerDaten gespeichert; jetzt aber nicht mehr
    if( nVersion < 4 )
    {
      int size;
      ar >> size;
      for ( int i = 0; i < size; i++ )
      {
        int index;
        ar >> index;
        CLayerData* lD;
        ar >> lD;
        
        // die Layerdaten nur aus dem Archiv lesen, die Daten aber nicht mehr benutzen, soll durch Initialize gesetzt werden
        delete lD;
        //m_layerData.SetAtGrow( index, lD );
      }; // for i
    }

    if ( nVersion > 1 )
    {
      ar >> windowRect;
      ar >> windowState;
      dockState.Serialize( ar );

      int size;
      ar >> size;
      for ( int i = 0; i < size; i++ )
      {
        UINT barID;
        CRect barRect;
        ar >> barID;
        ar >> barRect;

        barRects[barID] = barRect;
      };
    }; // nVersion >= 2

    if( nVersion > 2 )
      m_mapLayout = (CMapLayout*)ar.ReadObject( RUNTIME_CLASS(CMapLayout) );
  }; // IsLoading
}; // Serialize

void CMapProperties::Initialize()
// Initialisiert alle Daten mit Standardwerten
// Überschreibt vorhandene Daten nicht
{
  // MapLayout Initialisieren
  if( m_mapLayout == NULL )
    m_mapLayout = new CMapLayout();
  m_mapLayout->Initialize();

  // LayerData initialisieren
  int size = m_layerData.GetSize();
  CLayerData* lD;
  CLayer::LayerType type;

  //
  // image
  //
  type = CLayer::image;
  if ( size < type + 1 || !m_layerData[type] )
  {
    lD = new CLayerData;
    m_layerData.SetAtGrow( type, lD );
    //lD->layerName = "";
    //lD->fileName = "";
    //lD->bRandomColor;
    //lD->color;
    //lD->datablock;
    lD->bKopierbar = FALSE;
    //lD->kopierterTyp;
    //lD->bKopiereTag;
    lD->bTinCut = FALSE;
    //lD->bObjectGeometryEditable;
    lD->bCutToProfileLines = FALSE;
    lD->bUmrandungZeichnen = FALSE;
    lD->nProfilBezug = -1;
    //bLiesOnPoint = FALSE;
    //lD->bAdaptHeight = FALSE;
  };

  //
  // ProfilLines
  //
  type = CLayer::profilLines;
  if ( size < type || !m_layerData[type] )
  {
    lD = new CLayerData;
    m_layerData.SetAtGrow( type, lD );
    lD->layerName = CString(MAKEINTRESOURCE(IDS_PROFILE_LINES));
    lD->fileName = CString(MAKEINTRESOURCE(IDS_PROFILES));
    lD->bRandomColor = FALSE;
    lD->color = moGreen;
    //lD->datablock;
    lD->bKopierbar = TRUE;
    lD->kopierterTyp = CLayer::user_RW;
    lD->bKopiereTag = FALSE;
    lD->bTinCut = FALSE;
    lD->bObjectGeometryEditable = FALSE;
    lD->bCutToProfileLines = FALSE;
    lD->bUmrandungZeichnen = FALSE;
    lD->nProfilBezug = -1;
    //bLiesOnPoint = FALSE;
    //lD->bAdaptHeight = FALSE;
  };
    
  //
  // ProfilPoints
  //
  type = CLayer::profilPoints;
  if ( size < type + 1 || !m_layerData[type] )
  {
    lD = new CLayerData;
    m_layerData.SetAtGrow( type, lD );
    lD->layerName = CString(MAKEINTRESOURCE(IDS_PROFILE_POINTS));
    lD->fileName = CString(MAKEINTRESOURCE(IDS_POINTS));
    lD->bRandomColor = FALSE;
    lD->color = moRed;
    //lD->datablock;
    lD->bKopierbar = TRUE;
    lD->kopierterTyp = CLayer::user_RW;
    lD->bKopiereTag = FALSE;
    lD->bTinCut = FALSE;
    lD->bObjectGeometryEditable = FALSE;
    lD->bCutToProfileLines = FALSE;
    lD->bUmrandungZeichnen = FALSE;
    lD->nProfilBezug = 0;
    lD->bLiesOnPoint = FALSE;
    lD->bAdaptHeight = TRUE;
  };
    
  //
  // Trennflaechen
  //
  type = CLayer::trennflaechen;
  if ( size < type + 1 || !m_layerData[type] )
  {
    lD = new CLayerData;
    m_layerData.SetAtGrow( type, lD );
    lD->layerName = CString(MAKEINTRESOURCE(IDS_SEP_AREA));
    lD->fileName = CString(MAKEINTRESOURCE(IDS_SEP));
    lD->bRandomColor = FALSE;
    lD->color = moDarkGreen;
    lD->datablock = DST_TRENNFLAECHEN;
    lD->bKopierbar = TRUE;
    lD->kopierterTyp = CLayer::user_RW;
    lD->bKopiereTag = FALSE;
    lD->bTinCut = FALSE;
    lD->bObjectGeometryEditable = TRUE;
    lD->bCutToProfileLines = FALSE;
    lD->bUmrandungZeichnen = FALSE;
    lD->nProfilBezug = 2;
    lD->bLiesOnPoint = TRUE;
    lD->bAdaptHeight = TRUE;
  };
    
  //
  // Durchst_Bereiche
  //
  type = CLayer::durchst_bereiche;
  if ( size < type + 1 || !m_layerData[type] )
  {
    lD = new CLayerData;
    m_layerData.SetAtGrow( type, lD );
    lD->layerName = CString(MAKEINTRESOURCE(IDS_FLOW_AREA));
    lD->fileName = CString(MAKEINTRESOURCE(IDS_FLOW));
    lD->bRandomColor = FALSE;
    lD->color = moCyan;
    lD->datablock = DST_DURCHST_BEREICH;
    lD->bKopierbar = TRUE;
    lD->kopierterTyp = CLayer::user_RW;
    lD->bKopiereTag = FALSE;
    lD->bTinCut = FALSE;
    lD->bObjectGeometryEditable = TRUE;
    lD->bCutToProfileLines = FALSE;
    lD->bUmrandungZeichnen = FALSE;
    lD->nProfilBezug = 2;
    lD->bLiesOnPoint = TRUE;
    lD->bAdaptHeight = TRUE;
  };

  //
  // Modellgrenzen
  //
  type = CLayer::modellgrenzen;
  if ( size < type + 1 || !m_layerData[type] )
  {
    lD = new CLayerData;
    m_layerData.SetAtGrow( type, lD );
    lD->layerName = CString(MAKEINTRESOURCE(IDS_MODELL_BOUNDS));
    lD->fileName = CString(MAKEINTRESOURCE(IDS_MODELL_SHP));
    lD->bRandomColor = FALSE;
    lD->color = moBrown;
    lD->datablock = DST_MODELLGRENZEN;
    lD->bKopierbar = TRUE;
    lD->kopierterTyp = CLayer::user_RW;
    lD->bKopiereTag = FALSE;
    lD->bTinCut = FALSE;
    lD->bObjectGeometryEditable = TRUE;
    lD->bCutToProfileLines = FALSE;
    lD->bUmrandungZeichnen = FALSE;
    lD->nProfilBezug = 2;
    lD->bLiesOnPoint = FALSE;
    lD->bAdaptHeight = TRUE;
  };

  //
  // Boeschung
  //
  type = CLayer::bordvoll;
  if ( size < type + 1 || !m_layerData[type] )
  {
    lD = new CLayerData;
    m_layerData.SetAtGrow( type, lD );
    lD->layerName = CString(MAKEINTRESOURCE(IDS_BANK));
    lD->fileName = CString(MAKEINTRESOURCE(IDS_BANK_SHP));
    lD->bRandomColor = FALSE;
    lD->color = moGray;
    lD->datablock = DST_BORDVOLL;
    lD->bKopierbar = TRUE;
    lD->kopierterTyp = CLayer::user_RW;
    lD->bKopiereTag = FALSE;
    lD->bTinCut = FALSE;
    lD->bObjectGeometryEditable = TRUE;
    lD->bCutToProfileLines = FALSE;
    lD->bUmrandungZeichnen = FALSE;
    lD->nProfilBezug = 2;
    lD->bLiesOnPoint = TRUE;
    lD->bAdaptHeight = TRUE;
  };

  //
  // WaterLines
  //
  type = CLayer::waterLines;
  if ( size < type + 1 || !m_layerData[type] )
  {
    lD = new CLayerData;
    m_layerData.SetAtGrow( type, lD );
    // lD->layerName;
    lD->fileName = CString(MAKEINTRESOURCE(IDS_WLINES));
    lD->bRandomColor = FALSE;
    lD->color = moBlue;
    //lD->datablock;
    lD->bKopierbar = TRUE;
    lD->kopierterTyp = CLayer::user_RW;
    lD->bKopiereTag = FALSE;
    lD->bTinCut = FALSE;
    lD->bObjectGeometryEditable = FALSE;
    lD->bCutToProfileLines = TRUE;
    lD->bUmrandungZeichnen = FALSE;
    lD->nProfilBezug = -1;
    //lD->bLiesOnPoint = FALSE;
    //lD->bAdaptHeight = FALSE;
  };
    
  //
  // User_RO
  //
  type = CLayer::user_RO;
  if ( size < type + 1 || !m_layerData[type] )
  {
    lD = new CLayerData;
    m_layerData.SetAtGrow( type, lD );
    // lD->layerName;
    //lD->fileName;
    lD->bRandomColor = TRUE;
    //lD->color;
    //lD->datablock;
    lD->bKopierbar = TRUE;
    lD->kopierterTyp = CLayer::user_RW;
    lD->bKopiereTag = FALSE;
    lD->bTinCut = FALSE;
    lD->bObjectGeometryEditable = FALSE;
    lD->bCutToProfileLines = TRUE;
    lD->bUmrandungZeichnen = FALSE;
    lD->nProfilBezug = -1;
    //lD->bLiesOnPoint = FALSE;
    lD->bAdaptHeight = FALSE;
  };

  //
  // User_RW
  //
  type = CLayer::user_RW;
  if ( size < type + 1 || !m_layerData[type] )
  {
    lD = new CLayerData;
    m_layerData.SetAtGrow( type, lD );
    lD->layerName = CString(MAKEINTRESOURCE(IDS_USER_RW));
    lD->fileName = CString(MAKEINTRESOURCE(IDS_USER_RW_SHP));
    lD->bRandomColor = TRUE;
    //lD->color;
    //lD->datablock;
    lD->bKopierbar = TRUE;
    lD->kopierterTyp = CLayer::user_RW;
    lD->bKopiereTag = FALSE;
    lD->bTinCut = FALSE;
    lD->bObjectGeometryEditable = TRUE;
    lD->bCutToProfileLines = TRUE;
    lD->bUmrandungZeichnen = FALSE;
    lD->nProfilBezug = -1;
    //lD->bLiesOnPoint = FALSE;
    lD->bAdaptHeight = FALSE;
  };

  //
  // WaterLevel
  //
  type = CLayer::waterLevel;
  if ( size < type + 1 || !m_layerData[type] )
  {
    lD = new CLayerData;
    m_layerData.SetAtGrow( type, lD );
    lD->layerName = CString(MAKEINTRESOURCE(IDS_WATER_LEVEL));
    lD->fileName = CString(MAKEINTRESOURCE(IDS_WLEVEL));
    lD->bRandomColor = TRUE;
    //lD->color;
    //lD->datablock;
    lD->bKopierbar = TRUE;
    lD->kopierterTyp = CLayer::waterLevelC;
    lD->bKopiereTag = TRUE;
    lD->bTinCut = TRUE;
    lD->bObjectGeometryEditable = TRUE;
    lD->bCutToProfileLines = FALSE;
    lD->bUmrandungZeichnen = FALSE;
    lD->nProfilBezug = 0;
    lD->bLiesOnPoint = FALSE;
    lD->bAdaptHeight = TRUE;
  };

  //
  // Hmo
  //
  type = CLayer::hmo;
  if ( size < type + 1 || !m_layerData[type] )
  {
    lD = new CLayerData;
    m_layerData.SetAtGrow( type, lD );
    //lD->layerName;
    //lD->fileName;
    lD->bRandomColor = TRUE;
    //lD->color;
    //lD->datablock;
    lD->bKopierbar = TRUE;
    lD->kopierterTyp = CLayer::user_RW;
    lD->bKopiereTag = FALSE;
    lD->bTinCut = FALSE;
    lD->bObjectGeometryEditable = FALSE;
    lD->bCutToProfileLines = FALSE;
    lD->bUmrandungZeichnen = FALSE;
    lD->nProfilBezug = -1;
    //lD->bLiesOnPoint = FALSE;
    //lD->bAdaptHeight = FALSE;
  };
  
  //
  // Buhnen
  //
  type = CLayer::buhnen;
  if ( size < type + 1 || !m_layerData[type] )
  {
    lD = new CLayerData;
    m_layerData.SetAtGrow( type, lD );
    lD->layerName = CString(MAKEINTRESOURCE(IDS_BUHNE));
    lD->fileName = CString(MAKEINTRESOURCE(IDS_BUHNEN));
    lD->bRandomColor = FALSE;
    lD->color = moGray;
    lD->datablock = DST_BUHNEN;
    lD->bKopierbar = TRUE;
    lD->kopierterTyp = CLayer::user_RW;
    lD->bKopiereTag = FALSE;
    lD->bTinCut = FALSE;
    lD->bObjectGeometryEditable = TRUE;
    lD->bCutToProfileLines = FALSE;
    lD->bUmrandungZeichnen = FALSE;
    lD->nProfilBezug = -1;
    lD->bLiesOnPoint = FALSE;
    lD->bAdaptHeight = TRUE;
  };

  //
  // Festpunkte
  //
  type = CLayer::festpunkte;
  if ( size < type + 1 || !m_layerData[type] )
  {
    lD = new CLayerData;
    m_layerData.SetAtGrow( type, lD );
    lD->layerName = CString(MAKEINTRESOURCE(IDS_PIVOT_POINTS));
    lD->fileName = CString(MAKEINTRESOURCE(IDS_PIVOT_SHP));
    lD->bRandomColor = FALSE;
    lD->color = moYellow;
    //lD->datablock;
    lD->bKopierbar = TRUE;
    lD->kopierterTyp = CLayer::user_RW;
    lD->bKopiereTag = FALSE;
    lD->bTinCut = FALSE;
    lD->bObjectGeometryEditable = FALSE;
    lD->bCutToProfileLines = FALSE;
    lD->bUmrandungZeichnen = FALSE;
    lD->nProfilBezug = 1;
    lD->bLiesOnPoint = FALSE;
    lD->bAdaptHeight = TRUE;
  };

  //
  // WaterLevelC
  //
  type = CLayer::waterLevelC;
  if ( size < type + 1 || !m_layerData[type] )
  {
    lD = new CLayerData;
    m_layerData.SetAtGrow( type, lD );
    //lD->layerName;
    //lD->fileName;
    //lD->bRandomColor;
    //lD->color;
    //lD->datablock;
    lD->bKopierbar = TRUE;
    lD->kopierterTyp = CLayer::waterLevelC;
    lD->bKopiereTag = TRUE;
    lD->bTinCut = TRUE;
    lD->bObjectGeometryEditable = TRUE;
    lD->bCutToProfileLines = FALSE;
    lD->bUmrandungZeichnen = TRUE;
    lD->nProfilBezug = 0;
    lD->bLiesOnPoint = FALSE;
    lD->bAdaptHeight = FALSE;
  };

  //
  // Flussachse
  //
  type = CLayer::flussachse;
  if ( size < type + 1 || !m_layerData[type] )
  {
    lD = new CLayerData;
    m_layerData.SetAtGrow( type, lD );
    //lD->layerName = //CString( MAKEINTRESOURCE(IDS_RIVER_AXIS) );
    //lD->fileName = //CString(MAKEINTRESOURCE(IDS_RIVER_AXIS_SHP));
    lD->bRandomColor = FALSE;
    lD->color = moBlue;
    //lD->datablock;
    lD->bKopierbar = TRUE;
    lD->kopierterTyp = CLayer::user_RW;
    lD->bKopiereTag = FALSE;
    lD->bTinCut = FALSE;
    lD->bObjectGeometryEditable = TRUE;
    lD->bCutToProfileLines = TRUE;
    lD->bUmrandungZeichnen = FALSE;
    lD->nProfilBezug = 0;
    lD->bLiesOnPoint = FALSE;
    lD->bAdaptHeight = FALSE;
  };
}; // Initialize


BOOL CMapProperties::SafeToFile( LPCTSTR filePath )
// speichert die Karteneigenschaften in die angegebene Datei
{
  CFile file;

  if( !file.Open( filePath, CFile::modeCreate | CFile::modeWrite ) )
    return FALSE;
  
  CArchive ar( &file, CArchive::store );
  Serialize( ar );

  return TRUE;
}; // SafeToFile

BOOL CMapProperties::LoadFromFile( LPCTSTR filePath )
// lädt die Karteneigenschaften aus der angegebenen Datei
{
  CFile file;

  if (!file.Open( filePath, CFile::modeRead ) )
    return FALSE;

  CArchive ar( &file, CArchive::load );

  try
  {
    Serialize( ar );
  }
  catch( CException* e )
  {
    // die Properties konnten nicht geladen werden
    e->Delete();
    return FALSE;
  };
    

  return TRUE;
}; // LoadFromFile

CRect CMapProperties::GetWindowRect( UINT barID ) const
{
  CRect barRect;
  barRects.Lookup( barID, barRect );
  return barRect;
}; // GetWindowRect

void CMapProperties::SetWindowRect( const UINT barID, const CRect& barRect )
{
  barRects[barID] = barRect;
}; // SetWindowRect
