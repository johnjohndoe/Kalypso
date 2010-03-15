// MapDoc.cpp : Implementierung der Klasse CMapDoc
//

#pragma warning(disable:4786)
#pragma warning(disable:4503)

#include "stdafx.h"

#include <fstream>
#include <process.h>
#include <algorithm>
#include <iostream>

#include "maphelper.h"

#include "bce/include/tincut.h"
#include "bce/include/hmo.h"
#include "bce/include/WSPFeatures.h"
#include "bce/include/simpleCache.h"

#include "commonMfc\include\mfcHelper.h"
#include "commonMfc\include\ItemChooser.h"
#include "commonMFc\include\StringInputDlg.h"
#include "commonMFc\include\helper.h"
#include "commonMFc\include\executeExtern.h"
#include "commonMfc/include/variant_helper.h"
#include "commonMfc/include/version.h"

#include "wspprj\wspprj.h"
#include "wspprj\include\triple.h"

#include "progressdlg.h"
#include "mapview.h"
#include "mainfrm.h"
#include "wspmap.h"
#include "mapproj.h"
#include "mapdoc.h"
#include "layerdata.h"
#include "childfrm.h"
#include "zuordnung.h"
#include "progressDlg.h"
#include "tincutdlg.h"
#include "extendprofiledlg.h"
#include "mapprops.h"
#include "maplayer.h"
#include "imlayer.h"
#include "observerdialog.h"
#include "mapPreview.h"
#include "markMapObject.h"
#include "profilModel.h"
#include "volumedlg.h"
#include "MapStateProfiles.h"
#include "profileDistancer.h"
#include "strangDistanceDlg.h"


#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CMapDoc

IMPLEMENT_DYNCREATE(CMapDoc, CDocument)  // sollte eigentlich IMPLEMENT_SERIAL sein, geht aber auch so

/////////////////////////////////////////////////////////////////////////////
// CMapDoc Konstruktion/Destruktion

CMapDoc::CMapDoc()
{
  // Karteneinstellungen setzen 
  m_mapProperties = NULL;
  m_nMapUnit = muMeters;
  m_nScaleUnit = suKM;
  m_nScale = 5000;
  m_nScreenUnit = suInches;
  m_pOverviewLayer = 0;
  m_pActiveLayer = NULL;
  VERIFY(m_extent.CreateDispatch(TEXT("MapObjects2.Rectangle")));
  m_extent.SetLeft(0.0);
  m_extent.SetRight(1.0);
  m_extent.SetBottom(0.0);
  m_extent.SetTop(1.0);
  m_bShowOverview = FALSE;
  m_activeProfileID = -1;
  m_fShowActiveProfile = TRUE;
  m_netz = NULL;
  m_profilAuswahl = NULL;
}

CMapDoc::~CMapDoc()
{
  DeleteContents();
}

void CMapDoc::DeleteContents()
{
  for (int i = 0; i < m_layers.GetSize(); i++)
    delete m_layers[i];
  m_layers.RemoveAll();
  
  delete m_mapProperties;
  m_mapProperties = NULL;
  
  CDocument::DeleteContents();
}


/////////////////////////////////////////////////////
// Befehlsbehandlungsroutinen
/////////////////////////////////////////////////////

BEGIN_MESSAGE_MAP(CMapDoc, CDocument)
//{{AFX_MSG_MAP(CMapDoc)
ON_COMMAND(ID_VIEW_OVERVIEW, OnViewOverview)
ON_UPDATE_COMMAND_UI(ID_VIEW_OVERVIEW, OnUpdateViewOverview)
//}}AFX_MSG_MAP

ON_COMMAND(ID_SAFE_DEFAULT_PROPERTIES, OnSafeDefaultProperties)
END_MESSAGE_MAP()



void CMapDoc::OnSafeDefaultProperties()
{
  CString filePath = theApp.GetAppDir() + "\\wspmap.wpt";
  
  CString string;
  string.FormatMessage( IDS_SAVE_MAP_TEMPLATE, filePath );
  if ( AfxMessageBox( string, MB_OKCANCEL ) == IDOK )
  {
    GetWindowState();
    
    SafeMapProperties( filePath );
  }; // if AfxMessageBox
  
}; // OnSafeDefaultProperties


BOOL CMapDoc::OnNewDocument()
// überschrieben, um neue Dokumente mit den Standard MapProperties zu initialisieren
{
  CDocument::OnNewDocument();
  
  // neue MapProperties erzeugen und aus standardvorlage laden
  ASSERT( !m_mapProperties ); // müssen in CDocument::OnNewdocument durch DeleteContents gelöscht worden sein
  m_mapProperties = new CMapProperties;
  
  CString filePath = theApp.GetAppDir() + "\\wspmap.wpt";
  if ( !LoadMapProperties( filePath ) )
    AfxMessageBox( "Standardkartenvorlage nicht gefunden" );
  m_mapProperties->Initialize(); // auf jeden Fall standardwerte initialisieren
  
  return TRUE;
};

BOOL CMapDoc::OnOpenDocument( LPCTSTR lpszPathName ) 
// überladen, um den Pfadnamen des Dokuments schon vor dem serialisieren zu setzten
// das wird gebraucht, damit die Layer ihren BasisPfad finden
{
  m_strPathName = lpszPathName;
  m_mapDir = BCE::MfcHelper::GetFileDirectory( m_strPathName ) + "\\";

  if( CDocument::OnOpenDocument( lpszPathName ) )
  {
    m_mapProperties->Initialize();
    return TRUE;
  }
  else
    return FALSE;
}; // OnOpenDocument

HMENU CMapDoc::GetDefaultMenu()
{
  HINSTANCE hInstance = theApp.m_hInstance;
  
  HMENU hMainMenu = ::CreateMenu();
  
  // Ansicht
  CHelper::AppendMenu( hInstance, hMainMenu, IDR_MAPTYPE );
  CHelper::AppendMenu( hInstance, hMainMenu, IDR_VIEW );
  CHelper::AppendMenu( hInstance, hMainMenu, IDR_MAP );
  CHelper::AppendMenu( hInstance, hMainMenu, IDR_THEME );
  CHelper::AppendMenu( hInstance, hMainMenu, IDR_OBJECT );
  CHelper::AppendMenu( hInstance, hMainMenu, IDR_TOOLS );
  CHelper::AppendMenu( hInstance, hMainMenu, IDR_EXTRAS );
  CHelper::AppendMenu( hInstance, hMainMenu, IDR_WINDOW );
  CHelper::AppendMenu( hInstance, hMainMenu, IDR_HELP );

  return hMainMenu;
}; // GetDefaultMenu

///////////////
// Attribute //
///////////////

CMapLayout* CMapDoc::GetMapLayout()
{
  if( m_mapProperties )
    return m_mapProperties->GetMapLayout();
  else
    return NULL;
} // GetMapLayout

void CMapDoc::SetProject( Project* project )
{
  if( project )
    m_mapDir = project->GetMapDir();
  if( m_profilAuswahl )
    m_profilAuswahl->SetDocument( this, project );
}; // SetProject



CMoMap* CMapDoc::GetMap()
// gibt die map der ersten View zurück, NULL bei Misserfolg
{
  POSITION pos = GetFirstViewPosition();
  if( pos )
  {
    CMapView* mapView = (CMapView*)GetNextView( pos );
    if( mapView )
      return &(mapView->m_map);
  }; // if pos
  
  return NULL; // Misserfolg
}; // GetMap

void CMapDoc::SetActiveLayer( LPDISPATCH layer )
{
  CLayer* newLayer = m_layers.FindLayer( layer );

  if( m_pActiveLayer != newLayer )
  {
    m_pActiveLayer = newLayer;
    FireMapDocChanged( IMapDocListener::ACTIVE_THEME_CHANGED, newLayer );
    SetModifiedFlag( TRUE );
  }

}

void CMapDoc::AddLayer( CLayer* layer, const bool bRefreshViews )
{
  if( !layer )
    return;
  
  // es kann vorkommen, dass der layer schon da ist (wg. serialisierung)
  // dann nicht erneut hinzufügen
  m_layers.Add( layer );

  GetMap()->GetLayers().Add( layer->GetDispatch() );

  SetModifiedFlag( TRUE );

  if( bRefreshViews )
    FireMapDocChanged( IMapDocListener::THEME_ADDED, layer );
}

void CMapDoc::RemoveLayer( CLayer* layer, const bool bRefreshViews )
// löscht den Layer aus der Layerliste, welcher den gleichen dispatch hat wie 'layer'
//
// Input:
//      CLayer* layer: zu löschender Layer, falls NULL oder der Layer nicht in der Liste ist passiert nix
{
  // falls der Layer der Aktive Layer ist, letzteren auf NULL setzen
  if( layer == GetActiveLayer() )
    SetActiveLayer( NULL );

  if( layer == GetOverviewLayer() )
    SetOverview( LPDISPATCH( layer ) );
  
  CMoLayers layers( GetMap()->GetLayers() );

  LPDISPATCH lDisp = layer->GetDispatch();

  for( short lid = 0; lid < layers.GetCount(); lid++ )
  {
    LPDISPATCH disp = layers.Item( COleVariant( lid ) );
    if( disp == lDisp )
      layers.Remove(lid--);
    disp->Release();
  };

  // Layer im Dokument löschen
  for( int i = 0; i < m_layers.GetSize(); i++ )
  {
    CLayer* pLayer = GetLayer( i );
    if( pLayer == layer )
    {
      m_layers.RemoveAt( i );
      break;
    }
  };

  SetModifiedFlag( TRUE );

  if( bRefreshViews )
    FireMapDocChanged( IMapDocListener::THEME_REMOVED, 0 );

}; // RemoveLayer


CLayerData* CMapDoc::GetLayerData( CLayer::LayerType type )
// gibt die Layerdaten einen speziellen Layer-Typs zurück
// Parameter: CLayer::LayerType type: der gewünschte Typ
// Rückgabewert: Zeiger auf die Layerdaten oder NULL bei Fehler
{
  if ( m_mapProperties && type >= 0 && type < m_mapProperties->m_layerData.GetSize() )
    return m_mapProperties->m_layerData[type];
  else return NULL;
}; // GetLayerData

CMapLayer* CMapDoc::GetMapLayer(LPDISPATCH lpDispatch)
{
  int i;
  
  for (i = 0; i < m_layers.GetSize(); i++)
  {
    if (m_layers[i]->GetLayerType() == moMapLayer)
    {
      if (m_layers[i]->GetDispatch() == lpDispatch)
        return (CMapLayer*)m_layers[i];
    }
  }
  
  return NULL;
}

CImageLayer* CMapDoc::GetImageLayer(LPDISPATCH lpDispatch)
{
  int i;
  
  for (i = 0; i < m_layers.GetSize(); i++)
  {
    if (m_layers[i]->GetLayerType() == moImageLayer)
    {
      if (m_layers[i]->GetDispatch() == lpDispatch)
        return (CImageLayer*)m_layers[i];
    }
  }
  
  return NULL;
}

CLayer* CMapDoc::GetLayer(int i)
{
  if (i >= 0 && i < m_layers.GetSize())
    return m_layers[i];
  
  return NULL;
}

int CMapDoc::GetLayerCount()
{
  return m_layers.GetSize();
}

void CMapDoc::EditLayerProperties(CLayer *layer)
{
  if( !layer )
    return;
  
  if( layer->ShowPropertyDialog( *GetMap(), theApp.m_pMainWnd ) )
  {
    SetModifiedFlag( TRUE );
    FireMapDocChanged( CMapView::THEME_PROPS_CHANGED, layer );
  }
}

BOOL CMapDoc::SaveModified() 
{
	// AutoSave der Profile veranlassen -> im Frame, da wo die Profilauswahl gedockt ist
	POSITION pos = GetFirstViewPosition();
	while ( pos )
	{
		CMapView* pView = (CMapView*)GetNextView( pos );
		CChildFrame* pFrame = (CChildFrame*)pView->GetParentFrame();
		
		// ebenfalls die Karteneigenschaften jetzt aktualisieren ( nötig für die Fenstereinstellungen
		GetWindowState();
		
		break; // es gibt nur eine View
	}; // while pos
	
	// stets speichern ohne zu fragen

	// den Titel merken, da DoFileSave ihn auf den Dateinamen setzt
	CString title( GetTitle() );
	BOOL result = DoFileSave();
	SetTitle( title );
	return result;
}; // SaveModified


///////////////////////////////////////////////////////////////////////////
// Layer Management
//


/*!
 * Erzeugt ein neues Thema, fügt es in die Karte ein und gibt eine Referenz darauf zurück.
 * Existiert ein Layer dieses Typs bereits und darf nur ein einziger davon existieren, wird eine
 * Referenz auf den bereits existierenden zurückgegeben.
 *
 * @param typ : der Typ des neuen Layers
 *
 * @return CMapLayer*  : Referenz auf den neuen (oder bereits vorhandenen) Layer
 */
CMapLayer* CMapDoc::CreateAndAddLayer( const CLayer::LayerType& typ, const CString& strFileNameSuffix, const bool bRefreshViews )
{
  CLayerArray* pLayers = GetLayers();
  
  ASSERT( pLayers != 0 );
  if( !pLayers )
    return 0;
  
  switch( typ )
  {
    // hier kann ich nichts erzeugen
  case CLayer::image:
    return 0; // gleich zurück
    
    // diese dürfen nur einmal vorkommen
  case CLayer::profilLines:
  case CLayer::profilPoints:
  case CLayer::trennflaechen:
  case CLayer::durchst_bereiche:
  case CLayer::modellgrenzen:
  case CLayer::bordvoll:
  case CLayer::buhnen:
  case CLayer::festpunkte:
  case CLayer::waterLevelC:
  case CLayer::flussachse:
    {
      CMapLayer* pMapLayer = pLayers->FindFirstLayer( typ );
      if( pMapLayer )
        return pMapLayer;
    };
    break;
    
    // diese dürfen mehrfach vorkommen
  case CLayer::waterLines:
  case CLayer::user_RO:
  case CLayer::user_RW:
  case CLayer::waterLevel:
  case CLayer::hmo:
    break; // einfach weiter
  }; // switch typ
  
  // wenn wir hier ankommen, wirklich einen neuen erzeugen
  CMapLayer* pMapLayer = CreateMapLayer( typ, strFileNameSuffix );
  
  // und hinzufügen
  AddLayer( pMapLayer, bRefreshViews );
  
  return pMapLayer;
}; // CreateAndAddLayer


/** neuen Layer vom Typ typ erzeugen und Zeiger darauf zurückgeben
 * 
 * @param typ Typ des anzulegenden Layer
 * @return Zeiger auf den erzeugten Layer, NULL bei Misserfolg
 */
CMapLayer* CMapDoc::CreateMapLayer( const CLayer::LayerType typ, const CString& strFileSuffix )
{
  CLayerData* layerData = GetLayerData( typ );
  if ( !layerData )
    return NULL;
  
  COLORREF borderColor = moBlack;
  short style;
  short shapeType;
  CMoTableDesc tableDesc;
  BOOL visible = TRUE; // soll der Layer am Anfang sichtbar oder unsichtbar sein?
  
  VERIFY(tableDesc.CreateDispatch(TEXT("MapObjects2.TableDesc")));
  
  CString str;
  
  switch ( typ )
  {
  case CLayer::profilLines:
    shapeType = moLine;
    style = moSolidLine;
    
    tableDesc.SetFieldCount( 7 );
    
    tableDesc.SetFieldName( 0, MO2_FIELD_STATION );
    tableDesc.SetFieldType( 0, moDouble );
    tableDesc.SetFieldPrecision( 0, 15 );
    tableDesc.SetFieldScale( 0, 4 );	// decimal places
    
    tableDesc.SetFieldName( 1, MO2_FIELD_FILE );
    tableDesc.SetFieldType( 1, moString );
    tableDesc.SetFieldLength( 1, 50 );
    
    tableDesc.SetFieldName( 2, MO2_FIELD_STATE );
    tableDesc.SetFieldType( 2, moString );
    tableDesc.SetFieldLength( 2, 50 );
    
    tableDesc.SetFieldName( 3, MO2_FIELD_DELTAY );
    tableDesc.SetFieldType( 3, moDouble );
    tableDesc.SetFieldPrecision( 3, 15 );
    tableDesc.SetFieldScale( 3, 4 );
    
    tableDesc.SetFieldName( 4, MO2_FIELD_MFB );
    tableDesc.SetFieldType( 4, moLong );
    tableDesc.SetFieldPrecision( 4, 10 );
    tableDesc.SetFieldScale( 4, 0 );	// decimal places
    
    tableDesc.SetFieldName( 5, MO2_FIELD_VZK );
    tableDesc.SetFieldType( 5, moLong );
    tableDesc.SetFieldPrecision( 5, 10 );
    tableDesc.SetFieldScale( 5, 0 );	// decimal places
    
    tableDesc.SetFieldName( 6, MO2_FIELD_PK );
    tableDesc.SetFieldType( 6, moString );
    tableDesc.SetFieldLength( 6, 4 );
    break;
    
  case CLayer::profilPoints:
    shapeType = moPoint;
    style = moCrossMarker;
    visible = FALSE;
    
    tableDesc.SetFieldCount( 9 );
    
    tableDesc.SetFieldName( 0,MO2_FIELD_PROFILID );
    tableDesc.SetFieldType( 0, moLong );
    tableDesc.SetFieldPrecision( 0, 10 );
    
    tableDesc.SetFieldName( 1, MO2_FIELD_YKRD );
    tableDesc.SetFieldType( 1, moDouble );
    tableDesc.SetFieldPrecision( 1, 15 );
    tableDesc.SetFieldScale( 1, 4 );	// decimal places
    
    tableDesc.SetFieldName( 2, MO2_FIELD_HEIGHT );
    tableDesc.SetFieldType( 2, moDouble );
    tableDesc.SetFieldPrecision( 2, 15 );
    tableDesc.SetFieldScale( 2, 4 );	// decimal places
    
    tableDesc.SetFieldName( 3, MO2_FIELD_RAUHEIT );
    tableDesc.SetFieldType( 3, moDouble );
    tableDesc.SetFieldPrecision( 3, 15 );
    tableDesc.SetFieldScale( 3, 4 );	// decimal places
    
    tableDesc.SetFieldName( 4, MO2_FIELD_RAUHEITKST );
    tableDesc.SetFieldType( 4, moDouble );
    tableDesc.SetFieldPrecision( 4, 15 );
    tableDesc.SetFieldScale( 4, 4 );	// decimal places
    
    tableDesc.SetFieldName( 5, MO2_FIELD_AXM );
    tableDesc.SetFieldType( 5, moDouble );
    tableDesc.SetFieldPrecision( 5, 15 );
    tableDesc.SetFieldScale( 5, 4 );	// decimal places
    
    tableDesc.SetFieldName( 6, MO2_FIELD_AYM );
    tableDesc.SetFieldType( 6, moDouble );
    tableDesc.SetFieldPrecision( 6, 15 );
    tableDesc.SetFieldScale( 6, 4 );	// decimal places
    
    tableDesc.SetFieldName( 7, MO2_FIELD_DPM );
    tableDesc.SetFieldType( 7, moDouble );
    tableDesc.SetFieldPrecision( 7, 15 );
    tableDesc.SetFieldScale( 7, 4 );	// decimal places
    
    tableDesc.SetFieldName( 8, MO2_FIELD_NUMBER );
    tableDesc.SetFieldType( 8, moLong );
    tableDesc.SetFieldPrecision( 8, 10 );
    tableDesc.SetFieldScale( 8, 0 );	// decimal places
    break;
    
  case CLayer::festpunkte:
    shapeType = moPoint;
    style = moCircleMarker;
    
    tableDesc.SetFieldCount(1);
    
    tableDesc.SetFieldName(0, MO2_FIELD_PROFILID);
    tableDesc.SetFieldType(0, moLong);
    tableDesc.SetFieldPrecision(0, 10);
    break;
    
  case CLayer::modellgrenzen:
    shapeType = moPoint;
    style = moCircleMarker;
    
    tableDesc.SetFieldCount( 3 );
    
    tableDesc.SetFieldName(0, MO2_FIELD_PROFILID);
    tableDesc.SetFieldType(0, moLong);
    tableDesc.SetFieldPrecision(0, 10);
    
    tableDesc.SetFieldName(1, MO2_FIELD_YKRD);
    tableDesc.SetFieldType(1, moDouble);
    tableDesc.SetFieldPrecision(1, 15);
    tableDesc.SetFieldScale(1, 4);	// decimal places

    tableDesc.SetFieldName(2, MO2_FIELD_HEIGHT);
    tableDesc.SetFieldType(2, moDouble);
    tableDesc.SetFieldPrecision(2, 15);
    tableDesc.SetFieldScale(2, 4);	// decimal places
    break;
    
  case CLayer::trennflaechen:
    shapeType = moPoint;
    style = moCircleMarker;
    
    tableDesc.SetFieldCount(3);
    
    tableDesc.SetFieldName(0,MO2_FIELD_PROFILID);
    tableDesc.SetFieldType(0, moLong);
    tableDesc.SetFieldPrecision(0, 10);
    
    tableDesc.SetFieldName(1, MO2_FIELD_YKRD);
    tableDesc.SetFieldType(1, moDouble);
    tableDesc.SetFieldPrecision(1, 15);
    tableDesc.SetFieldScale(1, 4);	// decimal places
    
    tableDesc.SetFieldName(2, MO2_FIELD_TRENNSPEZIAL);
    tableDesc.SetFieldType(2, moLong);
    tableDesc.SetFieldPrecision(2, 10);
    break;
    
  case CLayer::durchst_bereiche:
    shapeType = moPoint;
    style = moCircleMarker;
    
    tableDesc.SetFieldCount(3);
    
    tableDesc.SetFieldName(0, MO2_FIELD_PROFILID);
    tableDesc.SetFieldType(0, moLong);
    tableDesc.SetFieldPrecision(0, 10);
    
    tableDesc.SetFieldName(1, MO2_FIELD_YKRD);
    tableDesc.SetFieldType(1, moDouble);
    tableDesc.SetFieldPrecision(1, 15);
    tableDesc.SetFieldScale(1, 4);	// decimal places
    
    tableDesc.SetFieldName(2, MO2_FIELD_HEIGHT);
    tableDesc.SetFieldType(2, moDouble);
    tableDesc.SetFieldPrecision(2, 15);
    tableDesc.SetFieldScale(2, 4);	// decimal places
    break;
    
  case CLayer::bordvoll:
    shapeType = moPoint;
    style = moCircleMarker;
    
    tableDesc.SetFieldCount( 3 );
    
    tableDesc.SetFieldName(0,MO2_FIELD_PROFILID);
    tableDesc.SetFieldType(0, moLong);
    tableDesc.SetFieldPrecision(0, 10);
    
    tableDesc.SetFieldName(1, MO2_FIELD_YKRD);
    tableDesc.SetFieldType(1, moDouble);
    tableDesc.SetFieldPrecision(1, 15);
    tableDesc.SetFieldScale(1, 4);	// decimal places

    tableDesc.SetFieldName(2, MO2_FIELD_HEIGHT);
    tableDesc.SetFieldType(2, moDouble);
    tableDesc.SetFieldPrecision(2, 15);
    tableDesc.SetFieldScale(2, 4);	// decimal places
    break;
    
  case CLayer::buhnen:
    shapeType = moPoint;
    style = moTriangleMarker;
    
    tableDesc.SetFieldCount(7);
    
    tableDesc.SetFieldName(0,MO2_FIELD_PROFILID);
    tableDesc.SetFieldType(0, moLong);
    tableDesc.SetFieldPrecision(0, 10);
    
    tableDesc.SetFieldName(1, MO2_FIELD_YKRD);
    tableDesc.SetFieldType(1, moDouble);
    tableDesc.SetFieldPrecision(1, 15);
    tableDesc.SetFieldScale(1, 4);	// decimal places
    
    tableDesc.SetFieldName(2, MO2_FIELD_HEIGHT);
    tableDesc.SetFieldType(2, moDouble);
    tableDesc.SetFieldPrecision(2, 15);
    tableDesc.SetFieldScale(2, 4);	// decimal places
    
    tableDesc.SetFieldName(3, MO2_FIELD_RELI);
    tableDesc.SetFieldType(3, moString);
    tableDesc.SetFieldLength(3, 3);
    
    tableDesc.SetFieldName(4, MO2_FIELD_HEADHEIGHT);
    tableDesc.SetFieldType(4, moDouble);
    tableDesc.SetFieldPrecision(4, 15);
    tableDesc.SetFieldScale(4, 4);	// decimal places
    
    tableDesc.SetFieldName(5, MO2_FIELD_BACKSLOPE);
    tableDesc.SetFieldType(5, moLong);
    tableDesc.SetFieldPrecision(5, 10);
    
    tableDesc.SetFieldName(6, MO2_FIELD_FRONTSLOPE);
    tableDesc.SetFieldType(6, moLong);
    tableDesc.SetFieldPrecision(6, 10);
    break;
    
  case CLayer::waterLevel:
    {
      shapeType = moPoint;
      style = moCrossMarker;
      
      tableDesc.SetFieldCount( 5 );
      
      tableDesc.SetFieldName( 0,MO2_FIELD_PROFILID );
      tableDesc.SetFieldType( 0, moLong );
      tableDesc.SetFieldPrecision( 0, 10 );
      
      tableDesc.SetFieldName( 1,MO2_FIELD_NEXTID );
      tableDesc.SetFieldType( 1, moLong );
      tableDesc.SetFieldPrecision( 1, 10 );
      
      tableDesc.SetFieldName( 2, MO2_FIELD_YKRD );
      tableDesc.SetFieldType( 2, moDouble );
      tableDesc.SetFieldPrecision( 2, 15 );
      tableDesc.SetFieldScale( 2, 4 );	// decimal places
      
      tableDesc.SetFieldName( 3, MO2_FIELD_HEIGHT ); // hier Höhe des Wasserstandes
      tableDesc.SetFieldType( 3, moDouble );
      tableDesc.SetFieldPrecision( 3, 15 );
      tableDesc.SetFieldScale( 3, 4 );	// decimal places
      
      tableDesc.SetFieldName( 4, MO2_FIELD_VARIANT );
      tableDesc.SetFieldType( 4, moString );
      tableDesc.SetFieldLength( 4, 30 );
    };
    break;
    
  case CLayer::waterLines:
    shapeType = moPolygon;
    style = moTransparentFill;
    borderColor = moBlue;
    
    // mindestens ein Attribut vergeben, da sonst dieses Shape nicht in ArcView geöffnet werden kann
    tableDesc.SetFieldCount( 1 );

    tableDesc.SetFieldName( 0, "dummy" );
    tableDesc.SetFieldType( 0, moLong );
    tableDesc.SetFieldPrecision( 0, 10 );
    break;
    
    // falls der Layer-Typ user_RW ist default-mässig einen Punkte layer erzeugen ( benötogt
    // für CMapDoc::OnShapeToProfile
  case CLayer::user_RW:
    shapeType = moPoint;
    style = moCircleMarker;
    
    tableDesc.SetFieldCount( 0 );
    break;
    
  default:
    return NULL; // kann nicht erzeugt werden
  }; // switch(type)
  
  // gültigen Dateinamnen erzeuegen, notfalls Nummern anfügen
  CString file = GetTitle() + GetLayerData( typ )->fileName;
  CString suffix = strFileSuffix.SpanIncluding( "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghicklmnopqrstuvwxyz0123456789_" ); // nur diese Zeichen sollen im Dateinamen vorkommen dürfen
  file = BCE::MfcHelper::GetUnusedFileName( GetMapPath() + file, suffix + ".shp") + suffix + ".shp";
  file = file.Mid( GetMapPath().GetLength() );
  
  CMapLayer *layer = new CMapLayer( GetMapPath() );
  layer->SetGeoDatasetName(file, shapeType, tableDesc);
  
  // eindeutigen Namen erzeugen, notfalls Nummern anfügen
  CString name = GetLayerData( typ )->layerName;
  int i = 0;
  CString count = "";
  while (m_layers.FindFirstLayerByName(name + count))
  {
    i++;
    count.Format(" %i", i);
  };
  name += count;
  
  layer->SetName(name);
  layer->SetType(typ);
  layer->SetVisible( visible );
  
  CMoSymbol symbol(layer->GetSymbol());
  symbol.SetStyle(style);
  if ( !layerData->bRandomColor )
    symbol.SetColor( layerData->color );
  if (shapeType != moLine && symbol.GetOutline())
    symbol.SetOutlineColor(borderColor);
  
  return layer;
};



/////////////////////////////////////////////////////////////////////////////
// CMapDoc Serialisierung

void CMapDoc::Serialize(CArchive& ar)
{
  if( ar.IsStoring() )
  {
    GetLayers()->MatchLayerOrder( *GetMap() );
    
    ar << DWORD(4);  // die aktuelle Datei-Version

    // den Kartenauschnitt speichern
    CMoRectangle extent = GetExtent();
    ar << extent.GetLeft();
    ar << extent.GetTop();
    ar << extent.GetRight();
    ar << extent.GetBottom();

    ar.WriteObject( m_mapProperties );
    ar << GetTitle();
    ar << m_layers.GetSize();
    for( int i = 0; i < m_layers.GetSize(); i++ )
    {
      ar << m_layers[i]->GetLayerType();
      ar << m_layers[i];
    };
  }
  else
  {
    DWORD nVersion;
    ar >> nVersion;
    if ( m_mapProperties )
    {
      delete m_mapProperties;
      m_mapProperties = NULL;
    };

    BOOL bSetExtent = TRUE;
    double left, right, top, bottom;
    
    switch (nVersion)
    {
    case 4:
      {
        // liest den Kartenextent
        ar >> left;
        ar >> top;
        ar >> right;
        ar >> bottom;
        bSetExtent = FALSE; // den Extent nicht mehr auf Maximum setzen
      }
    case 3:
      {
        CRuntimeClass* mapPropertiesRClass = RUNTIME_CLASS( CMapProperties );
        ASSERT( mapPropertiesRClass );

        m_mapProperties = (CMapProperties*)ar.ReadObject( mapPropertiesRClass );
        ASSERT( m_mapProperties );
      }; // case 3:
    case 2:
      {
        CString title;
        ar >> title;
        // SetTitle( title ); wird durch OpenDocument eh wieder zersört; wird in CMapProjekt::OpenMap gesetzt
        int size;
        ar >> size;
        for( int i = 0; i < size; i++ )
        {
          long type;
          ar >> type;
          switch( type )
          {
          default:
          case moMapLayer:
          {
            CMapLayer* mapLayer;
            ar >> mapLayer;
            if( mapLayer->LoadIsValid() )
              m_layers.Add( mapLayer );
            else
              delete mapLayer;
          }
          break;

          case moImageLayer:
          {
            CImageLayer* imageLayer;
            ar >> imageLayer;
            if( imageLayer->LoadIsValid() )
              m_layers.Add( imageLayer );
            else
              delete imageLayer;
          }
          break;
          } // switch type
        }; // for i

        if( bSetExtent )
          m_extent = m_layers.GetExtent();
        else
        {
          m_extent.SetLeft( left );
          m_extent.SetTop( top );
          m_extent.SetRight( right );
          m_extent.SetBottom( bottom );
        }

      };
      break;
      
    default:
      AfxThrowArchiveException(CArchiveException::badSchema);
      break;
    }; // switch nVersion
    
    if ( !m_mapProperties )
      m_mapProperties = new CMapProperties; // sollte nur im Fall alter Karten ( d.h Version < 3 ) passieren
  } // ar.isLoading
}

/////////////////////////////////////////////////////////////////////////////
// CMapDoc Diagnose

#ifdef _DEBUG
void CMapDoc::AssertValid() const
{
  CDocument::AssertValid();
}

void CMapDoc::Dump(CDumpContext& dc) const
{
  CDocument::Dump(dc);
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CMapDoc Befehle


/* Zeigt einen Layer im Übersichtsfenster
 * Falls das Fenster bisher geschlossen war, wird es aktiviert
 */
void CMapDoc::SetOverview( LPDISPATCH layer )
{
  CLayer* newLayer = m_layers.FindLayer( layer );

  if( m_pOverviewLayer != newLayer )
  {
    m_pOverviewLayer = newLayer;

    if( !GetShowOverview() && newLayer )
      SetShowOverview( TRUE );
    
    SetModifiedFlag( TRUE );

    FireMapDocChanged( IMapDocListener::OVERVIEW_CHANGED, 0 );
  }
}

int CMapDoc::GetLayerIndex(CLayer* layer)
// findet Nummer des Layers layer
// gibt -1 zurück falls Suche erfolglos war
{
  return m_layers.GetLayerIndex(layer);
}

void CMapDoc::SetScaleUnit(short n)
{
  m_nScaleUnit = n;
  FireMapDocChanged( IMapDocListener::SCALE_CHANGED, NULL );
};

void CMapDoc::SetScreenUnit(short n)
{
  m_nScreenUnit = n;
  FireMapDocChanged( IMapDocListener::SCALE_CHANGED, NULL );
}

void CMapDoc::OnViewOverview() 
{
  SetShowOverview(!GetShowOverview());
}

void CMapDoc::OnUpdateViewOverview(CCmdUI* pCmdUI) 
{
  pCmdUI->Enable( TRUE );
  pCmdUI->SetCheck(GetShowOverview());
}

void CMapDoc::SetShowOverview(BOOL b)
{
  if( GetShowOverview() != b )
  {
    m_bShowOverview = b; 
    FireMapDocChanged( IMapDocListener::OVERVIEW_CHANGED, NULL );  // Legende neu zeichnen
  };
}

CMoRectangle CMapDoc::GetExtent()
{
  return GetMap()->GetExtent();
}

CMoRectangle CMapDoc::GetFullExtent() 
// gibt den Maximalen Auschnitt der Karte zurück
{
  CMoMap* map = GetMap();
  if( map )
    return map->GetFullExtent();
  else
    return CMoRectangle();
}

void CMapDoc::SetExtent(CMoRectangle extent)
{
  if( !LPDISPATCH( extent ) )
    return;

  GetMap()->SetExtent( extent );

  SetModifiedFlag( TRUE );

  FireMapDocChanged( IMapDocListener::EXTENT_CHANGED, NULL );
}

CMapLayer* CMapDoc::MakeUGrenzenFromWaterLevel( CMapLayer* levelLayer )
// generiert aus einem CLayer::waterLevel - layer ein Linien-Shape
// Parameter:
//            CMapLayer* levelLayer: der Wasserspiegel, aus welchem die Grenzen erstellet werden sollen
// Rückgabewert: der neu erzeugte Layer (im \\map Verzeichnis) oder NULL bei Misserfolg
{
  if( !levelLayer || !levelLayer->GetValid() )
    return NULL;
  
  CMapLayer* grenzenLayer = m_layers.FindFirstLayerByTagAndType( levelLayer->GetTag(), CLayer::waterLines );
  if ( grenzenLayer )
    grenzenLayer->ClearRecordset();
  else
  {
    CString tag = levelLayer->GetTag();
    int find = tag.Find('@');
    CString suffix = find != -1 ? tag.Left( find ) : "";

    grenzenLayer = CreateMapLayer( CLayer::waterLines, suffix );
    if (!grenzenLayer)
      return NULL;
    grenzenLayer->SetName(levelLayer->GetName());
    grenzenLayer->SetTag( tag );
    grenzenLayer->SetColor( levelLayer->GetColor() );
    grenzenLayer->SetOutlineColor( levelLayer->GetColor() );
    AddLayer( grenzenLayer, FALSE );
  }; // if grenzenLayer
  
  CMoPolyArray polyArray;
  levelLayer->GetBorderPolygon( polyArray );
  
  // die Polygone als Objekte zum grenzenLayer hinzufügen
  CMoRecordset grenzenRecords( grenzenLayer->GetRecords() );
  if( !LPDISPATCH( grenzenRecords ) || !grenzenRecords.GetUpdatable() )
    return NULL;
  CMoFields grenzenFields( grenzenRecords.GetFields() );
  if( !LPDISPATCH(grenzenFields) )
    return NULL;
  CMoField grenzenShapeField( grenzenFields.Item( CComVariant( MO2_FIELD_SHAPE ) ) );
  if( !LPDISPATCH(grenzenShapeField) )
    return NULL;
  
  for( int pCount = 0; pCount < polyArray.GetSize(); pCount++ )
  {
    CMoPolygon poly( polyArray[pCount] );
    if( !LPDISPATCH(poly) )
      continue;
    
    grenzenRecords.AddNew();
    if( grenzenRecords.GetEditMode() == moEditAdd )
    {
      grenzenShapeField.SetValue( CComVariant( poly ) );
      grenzenRecords.Update();
    }; // if EditMode == moEditAdd
  }; // for pCount
  
  return grenzenLayer;
}; // MakeUGrenzenFromWaterLevel


/*
 * fügt Profilbezogene Daten zu den Layern hinzu, erzeugt ggfls. neue Layer
 *
 * @param pCrossSections Zeiger auf die hinzuzufügenden Profildaten, bereits geladen!
 * @param pStates der jeweilige Zustand unter welchen hinzugefügt werden soll
 * @param bZoomFull = TRUE: falls TRUE, wird nach der Operation der Ausschnitt der Karte maximiert
 * 
 * @return Ein bitset entsprechend der Eingangsarrays, ob die Profile geladen werden konnten oder nicht
 */
std::vector<bool> CMapDoc::AddProfiles( CrossSectionArray* cSections, StatesArray* states )
{
  // erstmal Voraussetzungen prüfen und nötige Daten holen
  ASSERT( cSections && states && cSections->GetSize() == states->GetSize() );
  
  CMapProperties* mapProps = GetMapProperties();
  CLayerArray* layers = GetLayers();
  CMainFrame* pMainFrame = (CMainFrame*)theApp.GetMainWnd();

  std::vector<bool> returnArray( cSections->GetSize() );
  
  // müssen alle vorhanden sein
  if( !pMainFrame || !mapProps || !layers )
    return returnArray;
  
  CTypedPtrArray<CPtrArray, CLayerData*>* layerData = mapProps->GetLayerData();
  if( !layerData )
    return returnArray;
  
  // das Hinzufügen der Profile dauert eine Weile
  CWaitCursor wait;
  CString progressTitle( MAKEINTRESOURCE( IDS_LOAD_PROFILES ) );
  CProgressCtrl* progressCtrl = pMainFrame->CreateStatusBarProgress( progressTitle );
  if( progressCtrl )
    progressCtrl->SetRange(0, cSections->GetSize());
  
  
  CTypedPtrArray<CPtrArray, CMapLayer*> mapLayers;
  for( int i = 0; i < layerData->GetSize(); i++ )
  {
    CLayerData* lData = layerData->GetAt( i );
    if( lData->nProfilBezug >= 0 || i == CLayer::profilLines )
    {
      CMapLayer* mapLayer = layers->FindFirstLayer( i );
      
      // falls der Layer nocht nicht existiert, erzeugen und gleich hinzufügen
      if( !mapLayer )
      {
        mapLayer = CreateMapLayer( (CLayer::LayerType)i, "" );
        AddLayer( mapLayer, FALSE );
      }; // if !mapLayer
      
      // falls es jetzt einen Layer gibt, das Recordset vorinitialisieren
      if( mapLayer )
      {
        CMoRecordset records( mapLayer->GetRecords() );
        if( LPDISPATCH( records ) && records.GetUpdatable() )
          mapLayers.SetAtGrow( i, mapLayer );
      }; // if mapLayer
    }; // if profilBezogen
  }; // for i
  
  
  // Hilfsvariablen für Mehrfeldbrücken
  CStringArray mfbStates; // der Zustandsname dieser MFB-Nr
  CArray<double, double> mfbStation; // die Station dieser MFB-Nr
  CUIntArray mfbVZK; // die VZK dieser MFB-Nr
  
  // auslesen, welche MBF-Nummern bereits vergeben sind, zu welchen Kennungen
  CUIntArray mfbNrs;
  {
    CMapLayer* lineLayer = mapLayers[CLayer::profilLines];
    if( lineLayer )
    {
      CMoRecordset lineRecords( lineLayer->GetRecords() );
      // es gibt höchstens soviele MFB-nummern wie Profile
      
      if( LPDISPATCH(lineRecords) )
      {
        CMoFields lineFields( lineRecords.GetFields() );
        if( LPDISPATCH(lineFields) )
        {
          CMoField mfbField( lineFields.Item( CComVariant( MO2_FIELD_MFB ) ) );
          CMoField stateField( lineFields.Item( CComVariant( MO2_FIELD_STATE ) ) );
          CMoField stationField( lineFields.Item( CComVariant( MO2_FIELD_STATION ) ) );
          CMoField vzkField( lineFields.Item( CComVariant( MO2_FIELD_VZK ) ) );
          if( LPDISPATCH( mfbField ) && LPDISPATCH(stateField) && LPDISPATCH(stationField) && LPDISPATCH(vzkField ) )
          {
            while( !lineRecords.GetEof() )
            {
              int mfbNr = int( COleVariantEx( mfbField.GetValue() ) );
              if( mfbNr >= 0 )
              {
                const CString state( COleVariantEx( stateField.GetValue() ) );
                const double station = COleVariantEx( stationField.GetValue() );
                const int vzk = COleVariantEx( vzkField.GetValue() );
                
                mfbStates.SetAtGrow( mfbNr, state );
                mfbStation.SetAtGrow( mfbNr, station );
                mfbVZK.SetAtGrow( mfbNr, vzk );
              }; // if mfbNr
              
              lineRecords.MoveNext();
            }; // while lineRecords
          }; // if mfbField
        }; // if lineFields
      }; // if lineRecords
    }; // if lineLayer
  }; // MFB-Nummern auslesen
  
  // benötigt, um Profilbezogene Punkte auf die Profillinie zu projizieren
  CArray<double, double> rWerte; // die Rechtswerte
  CArray<double, double> hWerte; // die Hochwerte
  CArray<double, double> yWerte; // die y Koordinaten
  CMap<double, double, int, int> yIndex; // yKoord -> index in yWerte

  CString strNichtGeladeneProfile;
  
  // Profildaten hinzufügen
  for( i = 0; i < cSections->GetSize(); i++ )
  {
    // erstmal alte Hilfswerte löschen
    rWerte.RemoveAll();
    hWerte.RemoveAll();
    yWerte.RemoveAll();
    yIndex.RemoveAll();
    
    // Querprofil, Zustand und Profil holen
    CrossSection* cs = cSections->GetAt( i );
    State* st = states->GetAt( i );
    if( !cs )
      continue;
    Profil* profil = cs->GetProfil();
    if ( !profil ) // sicherheitshalber nochmal prüfen ob das Profil geladen ist
      continue;
    
    // Daten der CrossSection auslesen
    CString csFile = cs->GetFileName();
    CString stFile;
    if( st )
      stFile = st->GetFileName();
    double csStation = cs->GetStation();
    UINT csVZK = cs->GetVZK();
    CString csPK = cs->GetPK();
    
    long mfb = -1;
    // falls es eine Mehrfeldbrücke ist, die MFB-Nr rausfinden
    if( csPK.CompareNoCase( TEXT("0") ) != 0 )
    {
      // suchen, ob es schon eine Nr zu dieser Kennung gibt
      mfb = -1;
      for( int nr = 0; nr < mfbStates.GetSize(); nr++ )
      {
        if( stFile.CompareNoCase( mfbStates[nr] ) == 0 && 
          fabs( csStation - mfbStation[nr] ) < 0.00001 &&
          csVZK == mfbVZK[nr] )
        {
          mfb = nr;
          break;
        }; // falls gefunden
      }; // for nr
      
      // falls nicht gefunden, neue Nr anlegen
      if( mfb == -1 )
      {
        // die kleinste freie MFB-Nummer rausfinden, die noch nicht vergeben wurde
        int k = 0;
        for( ; k < mfbStates.GetSize(); k++ )
        {
          // ist Leer, weil CStringArray unbenutzte Elemente mit "" vorinitialisiert
          if( mfbStates[k].IsEmpty() )
            break;
        }; // for k
        if( k == mfbStates.GetSize() )
        {
          mfb = mfbStates.Add( stFile );
          mfbStation.Add( csStation );
          mfbVZK.Add( csVZK );
        }
        else
        {
          mfb = k;
          mfbStates.SetAt( k, stFile );
          mfbStation.SetAt( k, csStation );
          mfbVZK.SetAt( k, csVZK );
        }; // if k == mfbStates.GetSize
      }; // mfb == -1
    }; // if PK != "0"
    
    // den Text der ProgressCtrl setzen
    pMainFrame->SetStatusBarProgressText( csFile );
    
    // alle interessanten Datenblöcke holen
    DataBlock* dbRw = profil->GetDataBlock( DST_RECHTSWERT );
    DataBlock* dbHw = profil->GetDataBlock( DST_HOCHWERT );
    DataBlock* dbGel = profil->GetDataBlock( DST_GELAENDEHOEHE );
    
    DataBlock* dbZusatz[5];
    dbZusatz[0] = profil->GetDataBlock(DST_RAUHIGKEIT);
    dbZusatz[1] = profil->GetDataBlock(DST_RAUHIGKEIT_KST);
    dbZusatz[2] = profil->GetDataBlock(DST_AXM);
    dbZusatz[3] = profil->GetDataBlock(DST_AYM);
    dbZusatz[4] = profil->GetDataBlock(DST_DPM);
    
    DataBlock* dbModell = profil->GetDataBlock( DST_MODELLGRENZEN );
    DataBlock* dbTrenn = profil->GetDataBlock( DST_TRENNFLAECHEN );
    DataBlock* dbDurch = profil->GetDataBlock( DST_DURCHST_BEREICH );
    DataBlock* dbBoeschung = profil->GetDataBlock( DST_BORDVOLL );
    DataBlock* dbBuhne = profil->GetDataBlock( DST_BUHNEN );
    

    // falls einer der Datenblöcke nicht ok ist, lieber nicht einlesen
    if( dbRw == NULL || dbHw == NULL || dbGel == NULL || dbRw->GetNumCoords() != dbHw->GetNumCoords() ||
      dbRw->GetNumCoords() != dbGel->GetNumCoords() || dbRw->GetNumCoords() < 2 )
    {
      CString str;
      str.Format( "%.4lf, ", cs->GetStation() );

      strNichtGeladeneProfile += str;

      continue; // ohne passende Rechtswert, Hochwert und Geländehöhe geht hier nichts
    }

    for( int z = 0; z < 5; z++ )
    {
      if( dbZusatz[z] && dbZusatz[z]->GetNumCoords() != dbGel->GetNumCoords() )
        dbZusatz[z] = NULL;
    }; // for z
    
    
    double deltay; // y-Koordinatenversatz für punkte innerhalb der Linie (= yKrd des 1.Punktes)
    long profilID;
    
    { // ProfilPoints und profilLines
      Coord* pRwCrd = dbRw->GetFirstCoord();
      Coord* pHwCrd = dbHw->GetFirstCoord();
      Coord* pGelCrd = dbGel->GetFirstCoord();
      
      Coord* pZusatz[5];
      for( int z = 0; z < 5; z++ )
      {
        if( dbZusatz[z] )
          pZusatz[z] = dbZusatz[z]->GetFirstCoord();
        else
          pZusatz[z] = NULL;
      }; // for z
      
      CMapLayer* profilPointLayer = mapLayers[CLayer::profilPoints];
      CMapLayer* profilLineLayer = mapLayers[CLayer::profilLines];
      
      CMoRecordset profilPointRecords( profilPointLayer->GetRecords() );
      profilPointRecords.SetAutoFlush( FALSE );
      CMoFields profilPointFields(profilPointRecords.GetFields());
      
      CMoRecordset profilLineRecords(profilLineLayer->GetRecords());
      profilLineRecords.SetAutoFlush( FALSE );
      profilID = profilLineRecords.GetCount() + 1;
      CMoFields profilLineFields(profilLineRecords.GetFields());
      
      CMoLine profilLine; // die zu erzeugende Profillinie
      MO2CREATE( profilLine, "Line" );
      CMoParts lineParts( profilLine.GetParts() );
      CMoPoints linePoints;
      MO2CREATE( linePoints, "Points" );
      
      deltay = pGelCrd->dx;
      
      LONG count = 0;
      while( pRwCrd && pHwCrd && pGelCrd )
      {
        CMoPoint point;
        MO2CREATE( point, "Point" );
        
        point.SetX(pRwCrd->dy);
        point.SetY(pHwCrd->dy);
        
        CMoField fZusatz[5];
        fZusatz[0] = CMoField( profilPointFields.Item( COleVariant( MO2_FIELD_RAUHEIT ) ) );
        fZusatz[1] = CMoField( profilPointFields.Item( COleVariant( MO2_FIELD_RAUHEITKST ) ) );
        fZusatz[2] = CMoField( profilPointFields.Item( COleVariant( MO2_FIELD_AXM ) ) );
        fZusatz[3] = CMoField( profilPointFields.Item( COleVariant( MO2_FIELD_AYM ) ) );
        fZusatz[4] = CMoField( profilPointFields.Item( COleVariant( MO2_FIELD_DPM ) ) );
        
        profilPointRecords.AddNew();
        SetValue( profilPointFields, MO2_FIELD_SHAPE, point );
        SetValue( profilPointFields, MO2_FIELD_PROFILID, profilID );
        SetValue( profilPointFields, MO2_FIELD_YKRD, pGelCrd->dx );
        SetValue( profilPointFields, MO2_FIELD_HEIGHT, pGelCrd->dy );
        SetValue( profilPointFields, MO2_FIELD_NUMBER, count++ ); // nötig, um die Reihenfolge der Punkte später wieder richtig herzustellen
        for( int z = 0; z < 5; z++ )
        {
          if( pZusatz[z] )
            fZusatz[z].SetValue( CComVariant( pZusatz[z]->dy ) );
        }; // for z
        profilPointRecords.Update();
        
        linePoints.Add( point );
        
        // Rechts, Hoch und yWerte merken
        yIndex[pGelCrd->dx] = yWerte.Add( pGelCrd->dx ) ;
        rWerte.Add( pRwCrd->dy );
        hWerte.Add( pHwCrd->dy );
        
        
        pRwCrd = dbRw->GetNextCoord();
        pHwCrd = dbHw->GetNextCoord();
        pGelCrd = dbGel->GetNextCoord();
        
        for( z = 0; z < 5; z++ )
        {
          if( dbZusatz[z] )
            pZusatz[z] = dbZusatz[z]->GetNextCoord();
        }; // for z
      };
      
      lineParts.Add( linePoints );
      
      profilLineRecords.AddNew();
      SetValue( profilLineFields, MO2_FIELD_SHAPE, profilLine );
      SetValue( profilLineFields, MO2_FIELD_STATION, csStation );
      SetValue( profilLineFields, MO2_FIELD_FILE, csFile );
      SetValue( profilLineFields, MO2_FIELD_STATE, stFile );
      SetValue( profilLineFields, MO2_FIELD_DELTAY, deltay );
      SetValue( profilLineFields, MO2_FIELD_MFB, mfb ); // 0: kein MFB, sonst > 0: MFB
      SetValue( profilLineFields, MO2_FIELD_VZK, (long)csVZK );
      SetValue( profilLineFields, MO2_FIELD_PK, csPK );
      profilLineRecords.Update();
    }; // profilPoints und profilLines
    
    { // Festpunkte
      CMapLayer* pivotLayer = mapLayers[CLayer::festpunkte];
      CMoRecordset pivotRecords(pivotLayer->GetRecords());
      pivotRecords.SetAutoFlush( FALSE );
      CMoFields pivotFields(pivotRecords.GetFields());
      
      CMoPoint point( YKoordToGeokoords( rWerte, hWerte, yWerte, yIndex, 0.0 ));
      if ( LPDISPATCH(point) )
      {
        pivotRecords.AddNew();
        SetValue( pivotFields, MO2_FIELD_SHAPE, LPDISPATCH(point) );
        SetValue( pivotFields, MO2_FIELD_PROFILID, profilID );
        pivotRecords.Update();
      };
    }; // Festpunkte
    
    // Wasserstaende
    {
      for (int w = 0; w < profil->GetDataBlockNum(DST_WSP_HOEHE); w++)
      {
        DataBlock *db_wsp = profil->GetDataBlock(DST_WSP_HOEHE, w + 1);
        
        CString tag = db_wsp->GetName(1);
        tag = tag.Mid( 99 );
        tag.TrimRight();
        
        CMapLayer* waterLayer = layers->FindFirstLayerByTagAndType( tag, CLayer::waterLevel );
        
        if( !waterLayer ) // falls er nicht schon existiert, erzeugen
        { // neuen Layer erzeugen
          int find = tag.Find('@');
          CString suffix = find != -1 ? tag.Left( find ) : "";

          waterLayer = CreateMapLayer( CLayer::waterLevel, suffix );
          waterLayer->SetTag(tag);
          waterLayer->SetName( waterLayer->GetName() + " " + suffix );

          AddLayer( waterLayer, FALSE );
        }; // if !waterLayer
        
        ASSERT(waterLayer);
        
        CMoRecordset records(waterLayer->GetRecords());
        records.SetAutoFlush( FALSE );
        CMoFields fields(records.GetFields());
        
        Coord* wspCrd = db_wsp->GetFirstCoord();
        while( wspCrd )
        {
          CMoPoint wspPoint( YKoordToGeokoords( rWerte, hWerte, yWerte, yIndex, wspCrd->dx ) );
          if( LPDISPATCH( wspPoint ) )
          {
            records.AddNew();
            SetValue( fields, MO2_FIELD_SHAPE, LPDISPATCH(wspPoint) );
            SetValue( fields, MO2_FIELD_PROFILID, profilID );
            SetValue( fields, MO2_FIELD_NEXTID, 0 );
            SetValue( fields, MO2_FIELD_YKRD, wspCrd->dx );
            SetValue( fields, MO2_FIELD_HEIGHT, wspCrd->dy );
            SetValue( fields, MO2_FIELD_VARIANT, tag );
            records.Update();
          }; // wspPoint
          
          wspCrd = db_wsp->GetNextCoord();
        }; // while wspCrd
        //        }; // if crdL && crdR
      }; // for w
    }; // wasserstaende
    
    // modellgrenzen
    if ( dbModell )
    {
      CMapLayer* modellLayer = mapLayers[CLayer::modellgrenzen];
      CMoRecordset modellRecords(modellLayer->GetRecords());
      modellRecords.SetAutoFlush( FALSE );
      CMoFields modellFields(modellRecords.GetFields());
      
      Coord *pMCrd = dbModell->GetFirstCoord();
      while (pMCrd)
      {
        double yKrd = pMCrd->dx;
        CMoPoint point( YKoordToGeokoords( rWerte, hWerte, yWerte, yIndex, yKrd ));
        if (LPDISPATCH(point))
        {
          modellRecords.AddNew();
          SetValue(modellFields, MO2_FIELD_SHAPE, LPDISPATCH(point));
          SetValue(modellFields, MO2_FIELD_PROFILID, profilID);
          SetValue(modellFields, MO2_FIELD_YKRD, yKrd );
          modellRecords.Update();
        };
        pMCrd = dbModell->GetNextCoord();
      }; // while
    }; // trennflaechen
    
    // trennflaechen
    if (dbTrenn)
    {
      CMapLayer* trennLayer = mapLayers[CLayer::trennflaechen];
      CMoRecordset trennRecords(trennLayer->GetRecords());
      trennRecords.SetAutoFlush( FALSE );
      CMoFields trennFields(trennRecords.GetFields());
      
      Coord *pTCrd = dbTrenn->GetFirstCoord();
      while (pTCrd)
      {
        double yKrd = pTCrd->dx;
        CMoPoint point( YKoordToGeokoords( rWerte, hWerte, yWerte, yIndex, yKrd ) );
        if (LPDISPATCH(point))
        {
          trennRecords.AddNew();
          SetValue( trennFields, MO2_FIELD_SHAPE, LPDISPATCH(point));
          SetValue( trennFields, MO2_FIELD_PROFILID, profilID);
          SetValue( trennFields, MO2_FIELD_YKRD, yKrd );
          SetValue( trennFields, MO2_FIELD_TRENNSPEZIAL, pTCrd->dy );
          trennRecords.Update();
        };
        pTCrd = dbTrenn->GetNextCoord();
      }; // while
    }; // trennflaechen
    
    //    durchst_bereiche       
    if (dbDurch)
    {
      CMapLayer* durchstLayer = mapLayers[CLayer::durchst_bereiche];
      CMoRecordset durchstRecords(durchstLayer->GetRecords());
      durchstRecords.SetAutoFlush( FALSE );
      CMoFields durchstFields(durchstRecords.GetFields());
      
      int count = 1;
      Coord *pDCrd = dbDurch->GetFirstCoord();
      while (pDCrd)
      {
        double yKrd = pDCrd->dx;
        CMoPoint point( YKoordToGeokoords( rWerte, hWerte, yWerte, yIndex, yKrd ) );
        if (LPDISPATCH(point))
        {
          durchstRecords.AddNew();
          SetValue(durchstFields, MO2_FIELD_SHAPE, LPDISPATCH(point));
          SetValue(durchstFields, MO2_FIELD_PROFILID, profilID);
          SetValue(durchstFields, MO2_FIELD_YKRD, yKrd );
          SetValue(durchstFields, MO2_FIELD_HEIGHT , pDCrd->dy );
          durchstRecords.Update();
          count++;
        };
        pDCrd = dbDurch->GetNextCoord();
      }; // while
    }; // durchst_Bereiche
    
    // boeschungskante       
    if( dbBoeschung )
    {
      CMapLayer* boeschungsLayer = mapLayers[CLayer::bordvoll];
      CMoRecordset boeschungsRecords(boeschungsLayer->GetRecords());
      boeschungsRecords.SetAutoFlush( FALSE );
      CMoFields boeschungsFields(boeschungsRecords.GetFields());
      
      Coord *pBoCrd = dbBoeschung->GetFirstCoord();
      while (pBoCrd)
      {
        double yKrd = pBoCrd->dx;
        CMoPoint point( YKoordToGeokoords( rWerte, hWerte, yWerte, yIndex, yKrd ));
        if (LPDISPATCH(point))
        {
          boeschungsRecords.AddNew();
          SetValue(boeschungsFields, MO2_FIELD_SHAPE, LPDISPATCH(point));
          SetValue(boeschungsFields, MO2_FIELD_PROFILID, profilID);
          SetValue(boeschungsFields, MO2_FIELD_YKRD, yKrd );
          boeschungsRecords.Update();
        };
        pBoCrd = dbBoeschung->GetNextCoord();
      }; // while
    }; // durchst_Bereiche
    
    // buhnen
    if (dbBuhne && (dbBuhne->GetNumCoords() % 3) == 0)
    {
      CString name = dbBuhne->GetName(1);
      name.MakeUpper();
      
      int count = 1;
      Coord *pB1Crd = dbBuhne->GetFirstCoord();
      
      while (pB1Crd)
      {
        CMapLayer* buhnenLayer = mapLayers[CLayer::buhnen];
        CMoRecordset buhnenRecords(buhnenLayer->GetRecords());
        buhnenRecords.SetAutoFlush( FALSE );
        CMoFields buhnenFields(buhnenRecords.GetFields());
        
        Coord* pBCrd = dbBuhne->GetNextCoord();
        Coord* pB2Crd = dbBuhne->GetNextCoord();
        char reli = 'R';
        
        if ( name.GetLength() >= 2 )
        {
          reli = name[1];
          name = name.Mid(2);
        };
        
        double x = pBCrd->dx;
        double y = pBCrd->dy;
        double x1 = pB1Crd->dx;
        double y1 = pB1Crd->dy;
        double x2 = pB2Crd->dx;
        double y2 = pB2Crd->dy;
        
        double yKrd = x;
        CMoPoint point(YKoordToGeokoords( rWerte, hWerte, yWerte, yIndex, yKrd ));
        if (LPDISPATCH(point))
        {
          // Keine rundungsmöglichkeiten für float -> integer in C++ ? -> dirty Tricks via scanf
          double dRuecken = (x - x1) / (y - y1); // Kehrwerte!
          double dVorderseite = (x2 - x) / (y2 - y);
          CString str;
          str.Format( "%10.0lf %10.0lf", dRuecken, dVorderseite );
          int ruecken, vorderseite;
          sscanf( str, "%d %d", &ruecken, &vorderseite );
          
          if (reli == 'R') // rücken und vorderseite vertauschen
          {
            int tmp = ruecken;
            ruecken = vorderseite;
            vorderseite = tmp;
          }
          else // Vorzeichen umdrehen
          {
            ruecken = -ruecken;
            vorderseite = -vorderseite;
          };
          
          buhnenRecords.AddNew();
          SetValue(buhnenFields, MO2_FIELD_SHAPE, LPDISPATCH(point));
          SetValue(buhnenFields, MO2_FIELD_PROFILID, profilID);
          SetValue(buhnenFields, MO2_FIELD_HEIGHT, 0.0);
          SetValue(buhnenFields, MO2_FIELD_RELI, CString(reli));
          SetValue(buhnenFields, MO2_FIELD_YKRD, x);
          SetValue(buhnenFields, MO2_FIELD_HEADHEIGHT, y);
          SetValue(buhnenFields, MO2_FIELD_BACKSLOPE, (long)ruecken);
          SetValue(buhnenFields, MO2_FIELD_FRONTSLOPE, (long)vorderseite);
          buhnenRecords.Update();
        };
        pB1Crd = dbBuhne->GetNextCoord();
      }; // while
    }; // if db_buhnen
    
    if( progressCtrl )
      progressCtrl->StepIt();

    // erst jetzt als geladen markieren
    returnArray[i] = true;
  }; // for i
  
  pMainFrame->DestroyStatusBarProgress();
  progressCtrl = NULL;
  
  // die WspVernetzung wieder auffrischen
  UpdateWSPVernetzung();
  
  // leere Layer wieder zerstören und Autoflush auf TRUE setzen
  FlushLayers();

  // Fehlermeldung ausgeben, wenn manche Profile nicht geladen werden konnten
  if( strNichtGeladeneProfile.GetLength() > 1 )
  {
    strNichtGeladeneProfile = strNichtGeladeneProfile.Left( strNichtGeladeneProfile.GetLength() - 2 ); // das letzte Komma wegschmeissen
    CString msg;
    msg.Format( IDS_FEHLER_PROFILE_LADEN_RWHW, strNichtGeladeneProfile );
    AfxMessageBox( msg, MB_OK | MB_ICONEXCLAMATION );
  }
  
  // falls bisher noch kein aktives Profil gesetzt wurde, dies jetzt nachholen
  if( m_activeProfileID < 1 )  
    SetActiveProfile( 1, FALSE );

  FireMapDocChanged( IMapDocListener::THEME_GEOMETRY_CHANGED | 
                     IMapDocListener::THEME_DATA_CHANGED | 
                     IMapDocListener::THEME_ADDED |
                     IMapDocListener::ACTIVE_PROFILE_CHANGED, 0 );

  return returnArray;
}; // AddProfiles


/**
 * Löscht Profillinine aus der Karte aus dem Shape und
 * aktualisiert die ReferenzIDs
 * Die ProfilAuswahl wird nicht beeinflusst.
 * Sollte nur durch die ProfilAuswahl aufgerufen werden
 * 
 */
void CMapDoc::RemoveProfiles( const CStringArray& fileNames )
{
  // ein paar Voraussetzungen prüfen
  CMapLayer* profilLines = m_layers.FindFirstLayer( CLayer::profilLines );
  if ( !LPDISPATCH(profilLines) || !profilLines->GetValid() || fileNames.GetSize() == 0 )
    return;
  
  // zuerst die alten IDs merken
  std::map<long, CString> idMap = profilLines->GetIDToFileMap();
  std::map<CString, long> fileMap = profilLines->GetFileToIDMap();

  long oldActive = m_activeProfileID;
  
  // removeIDs vorinitialisieren; die höchste ID ist idMap.GetSize(), weil die FeatureID immer von 1 - size besetzt werden
  CArray<BOOL, BOOL> removeIDs;
  for( int i = 0; i < idMap.size() + 1; i++ )
    removeIDs.SetAtGrow( i, FALSE );
  
  for( i = 0; i < fileNames.GetSize(); i++ )
  {
    long id = fileMap[fileNames.GetAt( i )];
    if( id != 0 )
      removeIDs.SetAtGrow( id, TRUE );
  }; // for i
  
  // den Dateinamen des aktiven Layers holen, ausser er wird gelöscht
  CString activeStr;
  if( oldActive >= 0 && oldActive < removeIDs.GetSize() && !removeIDs[oldActive] )
    activeStr = idMap[oldActive];
  
  // jetzt tatsächlich die Daten löschen
  RemoveDataFromMap( removeIDs );
  
  // und die IDs aller Profilbezogenen Daten wieder richtig setzen
  const int newActive = ChangeProfilIDs( idMap, activeStr );
  
  // die Wasserspiegelvernetung auffrischen
  UpdateWSPVernetzung();
  
  // das active Profil wieder setzen; 
  SetActiveProfile( max( 0, newActive ), FALSE ); // im Zweifelsfall Nr 1 selektieren
  
  // zuletzte alle nicht mehr benötigten Layer löschen
  FlushLayers();
  
  // und alles schöne neu anzeigen
  FireMapDocChanged( IMapDocListener::THEME_REMOVED | 
                     IMapDocListener::THEME_DATA_CHANGED | 
                     IMapDocListener::THEME_GEOMETRY_CHANGED |
                     IMapDocListener::ACTIVE_PROFILE_CHANGED, 0 );

  SetModifiedFlag( TRUE );
}; // RemoveProfile

void CMapDoc::RemoveDataFromMap( const CArray<BOOL, BOOL>& removeIDs )
// löscht alle Profilbezogenen Daten und ProfilLinien, die in removeIds angegeben sind
// Parameter:
//        const CArray<BOOL, BOOL>& removeIDs: ists TRUE, wird gelöscht
{
  CLayerArray* layers = GetLayers();
  CMapProperties* mapProps = GetMapProperties();
  
  if( !layers || !mapProps )
    return;
  
  int maxId = removeIDs.GetSize();
  
  for( int i = 0; i < layers->GetSize(); i++ )
  {
    // den Layer holen und schauen ob alles in Ordnung
    CLayer* layer = layers->GetAt( i );
    if( !layer || layer->GetLayerType() != moMapLayer )
      continue;
    
    CMapLayer* mapLayer =  (CMapLayer*)layer;
    CLayer::LayerType layerType = mapLayer->GetType();
    CLayerData* layerData = GetLayerData( layerType );
    
    if( !layerData || ( layerData->nProfilBezug == -1 && layerType != CLayer::profilLines ) )
      continue;
    
    // recordset und felder holen: bei ProfilLinien die FeatureID, sonst die ProfilID
    CMoRecordset records( mapLayer->GetRecords() );
    if( !LPDISPATCH(records) || !records.GetUpdatable() )
      continue;
    
    records.SetAutoFlush( FALSE );
    
    CMoFields fields( records.GetFields() );
    if( !LPDISPATCH(fields) )
      continue;
    
    CString fieldName;
    if( layerType == CLayer::profilLines )
      fieldName = CString( MO2_FIELD_FEATUREID );
    else
      fieldName = CString( MO2_FIELD_PROFILID );
    CMoField idField( fields.Item( CComVariant( fieldName ) ) );
    
    if ( !LPDISPATCH(idField) )
      continue;
    
    // jetzt die records durchgehen und löschen
    while ( !records.GetEof() )
    {
      const int id = COleVariantEx( idField.GetValue() );
      if( id < maxId && removeIDs[id] ) // diese id sol gelöscht werden, also auf gehts!
        records.Delete();
      
      records.MoveNext();
    }; // while !records.GetEof()
    
    records.SetAutoFlush( TRUE );
  }; // for i
  
}; // RemoveDataFromMap

void CMapDoc::UpdateWSPVernetzung()
// Vernetzt die Wasserstände
{
  // die allgemeine Vorarbeit
  CLayerArray* layers = GetLayers();
  if( !layers )
    return;
  CMapLayer* profilLines = layers->FindFirstLayer( CLayer::profilLines );
  if( !profilLines )
    return;
  
  // zuerst die Vernetzung der geladenen Profile holen
  WSPVernetzungsInfo* netz = GetProfileNet();
  if( !netz )
    return;
  
  // um später Vernetzen zu können, müssen wir ein Paar Daten aus den ProfilLinien auslesen
  // wir brauchen eine Liste aller Profile und jeweils dazu einen SQL-String, den wir für spätere Abfragen brauchen
  CMoRecordset lineRecords( profilLines->GetRecords() );
  if( !LPDISPATCH(lineRecords) )
    return;
  CMoFields lineFields( lineRecords.GetFields() );
  if( !LPDISPATCH(lineFields) )
    return;
  CMoField idField( lineFields.Item( CComVariant(MO2_FIELD_FEATUREID) ) );
  CMoField mfbField( lineFields.Item( CComVariant(MO2_FIELD_MFB) ) );
  CMoField profilField( lineFields.Item( CComVariant(MO2_FIELD_FILE) ) );
  if( !idField || !profilField )
    return;
  
  CMapStringToString mapNameToSql; // Ordnet jedem ProfilNamen den Sql-Suchstring zu
  CStringArray mapMfbToSql;        // Ordnet jeder Mfb-Nummer den Sql-Suchstring zu, der alle Profile mit dieser Kennung findet
  CStringArray mapProfilIDToName; // Ordnet jeder ProfilId das ProfilFile zu
  while( !lineRecords.GetEof() )
  {
    const int profilID = COleVariantEx( idField.GetValue() ); 
    const CString profilFile( COleVariantEx( profilField.GetValue() ) );
    
    int mfbNr = -1; // d.h keine Mehrfeldbrücke
    if( LPDISPATCH( mfbField ) ) // Kompatibilität zu älteren Versionen
      mfbNr = COleVariantEx( mfbField.GetValue() );
    
    // die mapProfilIDToName setzen
    mapProfilIDToName.SetAtGrow( profilID, profilFile );
    
    // falls es ein normales Profil ist einfach den SQL String erzeugen und fertig
    if( mfbNr < 0 )
    {
      // keine Mehrfeldbrücke: derAbfragestring hat die Form: "ProfilId = d"
      CString sqlStr;
      sqlStr.Format( "%s = %d", MO2_FIELD_PROFILID, profilID );
      mapNameToSql[profilFile] = sqlStr;
    }
    else
    {
      // Mehrfeldbrücke: der Abfragestring hat die Form: "( ProfilID = d1 ) or ( ProfilID = d2 ) ... "
      CString sqlStr;
      if( mfbNr < mapMfbToSql.GetSize() )
        sqlStr = mapMfbToSql[mfbNr]; // kann auch leer sein ( durch SetAtGrow erzeugt )
      
      if( !sqlStr.IsEmpty() )
        sqlStr += " or ";
      
      CString newSqlStr;
      newSqlStr.Format( "( %s = %d )", MO2_FIELD_PROFILID, profilID );
      sqlStr += newSqlStr;
      
      mapMfbToSql.SetAtGrow( mfbNr, sqlStr );
      
      // und in der mapNameToSql markieren, dass hier noch was zu tun ist
      CString marker;
      marker.Format( "*%d", mfbNr );
      mapNameToSql[profilFile] = marker;
    }; // if mfbNr == -1
    
    
    lineRecords.MoveNext();
  }; // while lineRecords
  
  // jetzt den Profilen, welche Mehrfeldbrücken sind, doch den richtigen Sql-String zuordnen
  POSITION pos = mapNameToSql.GetStartPosition();
  while( pos )
  {
    CString sqlStr, profilFile;
    
    mapNameToSql.GetNextAssoc( pos, profilFile, sqlStr );
    
    // falls Mehrfeldbrückenkennung, jetzt den String ersetzen
    if( sqlStr[0] == '*' && sqlStr.GetLength() > 1 )
    {
      sqlStr = sqlStr.Mid( 1 ); // das erste Zeichen abschneiden
      int mfbNr = atoi( sqlStr );
      if( mfbNr >= 0 && mfbNr < mapMfbToSql.GetSize() )
      {
        sqlStr = mapMfbToSql[mfbNr];
        mapNameToSql[profilFile] = sqlStr;
      }; // if mfbNr ...
    }; // if sqlStr[0] = '*'
  }; // while pos
  
  
     /*
     // Debug
     pos = netz->GetStartPosition();
     while( pos )
     {
     CString fileName;
     CStringArray* strings;
     netz->GetNextAssoc( pos, fileName, strings );
     
       CString s;
       s.Format( "File: %s; Vorg1: %s, Vorg2: %s, Nachf1: %s Nachf2: %s", fileName, strings->GetAt( 0 ), strings->GetAt( 1 ),
       strings->GetAt( 2 ), strings->GetAt( 3 ) );
       AfxMessageBox( s );
       }; // while pos
  */
  
  // jetzt für jeden Wasserspiegel die Vernetzung erstellen
  POSITION waterPos = NULL;
  CMapLayer* waterLayer = layers->FindFirstLayer( CLayer::waterLevel, &waterPos );
  while( waterLayer )
  {
    // um später Vernetzen zu können, lesen wir zwei Zuordnungen aus den Daten ab:
    // - ProfilName |-> int[2]: Orndet jedem Profil zwei IDs von Wasserpiegeln zu, den linken und den rechten
    // - WspFeatureIDs |-> { "L", "R", "-" }. Ordnet jedem Waserspiegel zu, ob er der Linke, der Rechte oder irgendeiner in der Mitte ist
    
    CMapStringToPtr lrMap; // Ordnet jedem Profilnamen die beiden IDs des linkesten und rechtesten Wasserspiegel zu: LOWORD( arg ) ist ID des linken, HIWORD( arg ) die ID des rechten
    CByteArray mapIDToLR; // Ordnet jeder ID eines Wasserspiegels ein Byte zu: 0 = "-"; 1 = "L"; 2 = "R"
    
    // für jedes Profil nun all seine Wasserspiegel anschauen
    POSITION pos = mapNameToSql.GetStartPosition();
    while( pos )
    {
      CString sqlStr, profilName;
      mapNameToSql.GetNextAssoc( pos, profilName, sqlStr );
      
      // das Recordset und die Felder initialisieren
      CMoRecordset wspRecords( waterLayer->SearchExpression( sqlStr ) ); // liefert alle Wasserspiegel zu diesem Profil; bei Mehrfeldbrücken sogar alle aus dieser Mehrfeldbrücke
      if( !LPDISPATCH(wspRecords) )
        continue;
      
      CMoFields wspFields( wspRecords.GetFields() );
      if( !LPDISPATCH(wspFields) )
        continue;
      
      CMoField idField( wspFields.Item( CComVariant( MO2_FIELD_FEATUREID ) ) );
      CMoField yField( wspFields.Item( CComVariant( MO2_FIELD_YKRD ) ) );
      if( !LPDISPATCH(idField) || !LPDISPATCH(yField) )
        continue;
      
      // jetzt die einzelnen Punkte abarbeiten
      int lID = 0; // Id des linkesten
      int rID = 0; // Id des rechtesten
      double yLeft = 1e36; // sehr gross
      double yRight = -1e36; // sehr klein
      while( !wspRecords.GetEof() )
      {
        const int id = COleVariantEx( idField.GetValue() );
        const double yKrd = COleVariantEx( yField.GetValue() );
        
        if( yKrd < yLeft )
        {
          // die alten Infos löschen
          mapIDToLR.SetAtGrow( lID, 0 );
          yLeft = yKrd;
          lID = id;
        }; // yKrd < left
        
        if( yKrd > yRight )
        {
          mapIDToLR.SetAtGrow( rID, 0 );
          yRight = yKrd;
          rID = id;
        }; // if yKrd > yRight
        
        // immer neu setzen
        mapIDToLR.SetAtGrow( lID, 1 );
        mapIDToLR.SetAtGrow( rID, 2 );
        
        // auch noch die lpMap setzen:
        void* lrIDs = (void*)MAKELONG( lID, rID );
        lrMap[profilName] = lrIDs;
        
        wspRecords.MoveNext();
      }; // while wspRecords
      
    }; // while pos
    
    // jetzt endlich die Vernetzung eintragen: dazu alle Punkte durchgehen, die NextID ermitteln und fertig
    CMoRecordset wspRecords( waterLayer->GetRecords() );
    if( !LPDISPATCH(wspRecords) )
      continue;
    CMoFields wspFields( wspRecords.GetFields() );
    if( !LPDISPATCH(wspFields) )
      continue;
    CMoField idField( wspFields.Item( CComVariant( MO2_FIELD_FEATUREID ) ) );
    CMoField profilIDField( wspFields.Item( CComVariant( MO2_FIELD_PROFILID ) ) );
    CMoField nextField( wspFields.Item( CComVariant( MO2_FIELD_NEXTID ) ) );
    if( !LPDISPATCH(idField) || !LPDISPATCH(nextField) )
      continue;
    
    // einfach alle Records durchgehen
    while( !wspRecords.GetEof() )
    {
      const int id = COleVariantEx( idField.GetValue() );
      const int profilID = COleVariantEx( profilIDField.GetValue() );
      int nextId = 0; // Standardwert für die NextID
      CString profilFile; // der ProfilName
      if( 0 <= profilID && profilID < mapProfilIDToName.GetSize() )
        profilFile = mapProfilIDToName[profilID];
      
      // es wird folgendermassen verknüpft: ein linker Punkt immer mit dem linken punkt des folgenden Profils
      // rechts genau umgekehrt; bei Verzweigungen kehrt sich das um, falls man aus dem rechten Zweig kommt und gerade
      // links sucht bzw. umgekehrt
      
      BOOL bLinks = TRUE; // suchen wir gerade links oder rechts ( FALSE )
      CString suchProfil = profilFile;
      BOOL bSuchen = FALSE; // sol überhaupt gesucht werden?
      int alternativNext; // falls nicht gefunden wird ( d.h man ist am Strangende ) muss man innerhalb des Profils vernetzen
      
      void* lrInfo = 0;
      lrMap.Lookup( profilFile, lrInfo );
      
      if( id >= 0 && id < mapIDToLR.GetSize() )
      {
        switch( mapIDToLR[id] )
        {
        case 1: // ist linksaussen: also die ID des linkesten des Nachfolgenden Profils suchen
          bSuchen = TRUE;
          bLinks = TRUE;
          alternativNext = HIWORD( lrInfo );
          break;
          
        case 2:
          bSuchen = TRUE;
          bLinks = FALSE;
          alternativNext = LOWORD( lrInfo );
          break;
        }; // switch mapIDToLR
        
        while( bSuchen )
        {
          // zuerst die Daten des nächsten Profils holen
          CStringArray* suchStrings = NULL; // die ProfilNamen der folgenden und vorhergehenden Profile
          if( suchProfil.IsEmpty() || !netz->Lookup( suchProfil, suchStrings ) || suchStrings == NULL )
          {
            // falls kein nächstes Profil da ist oder gefunden werden kann, die Suche abbrechen, wir sind hoffentlich am Ende des Strangs
            bSuchen = FALSE;
            nextId = alternativNext;
            break;
          };
          
          CString vorLinksName = suchStrings->GetAt( 0 );
          CString vorRechtsName = suchStrings->GetAt( 1 );
          CString nachLinksName = suchStrings->GetAt( 2 );
          CString nachRechtsName = suchStrings->GetAt( 3 );
          
          CString nextProfil;
          if( bLinks )
          {
            if( !nachLinksName.IsEmpty() )
              nextProfil = nachLinksName;
            else
              nextProfil = nachRechtsName; // in jedem Fall zuordnen, möglicherweise ist suchProfil = "" und die suche ist fertig
          }
          else
          {
            if( !vorRechtsName.IsEmpty() )
              nextProfil = vorRechtsName;
            else
              nextProfil = vorLinksName;
          }; // if bLinks
          
          // jetzt checken, ob wird die Suchrichtung wegen einer Verzweigung umdrehen müssen
          CStringArray* nextStrings = NULL;
          if( nextProfil.IsEmpty() || !netz->Lookup( nextProfil, nextStrings ) || nextStrings == NULL )
          {
            suchProfil.Empty();
            continue; // die gleiche abfrage führt beim nächsten schleifendurchgang zum abbruch
          }; // if nextProfil
          
          CString nextVorLinksName = nextStrings->GetAt( 0 );
          CString nextVorRechtsName = nextStrings->GetAt( 1 );
          CString nextNachLinksName = nextStrings->GetAt( 2 );
          CString nextNachRechtsName = nextStrings->GetAt( 3 );
          
          
          if( bLinks && !nextVorLinksName.IsEmpty() && nextVorRechtsName.CompareNoCase( suchProfil ) == 0 )
          {
            nextProfil = nextVorLinksName;
            bLinks = FALSE;
          }
          else if( !bLinks && !nextNachRechtsName.IsEmpty() && nextNachLinksName.CompareNoCase( suchProfil ) == 0 )
          {
            nextProfil = nextNachRechtsName;
            bLinks = TRUE;
          };
          
          // so jetzt wissen wir, in welchem Profil wir nach einem Wasserstand suchen sollen
          // tun wirs also
          if( lrMap.Lookup( nextProfil, lrInfo ) )
          {
            if( bLinks && LOWORD(lrInfo) != 0 )
            {
              nextId = LOWORD(lrInfo);
              break;
            }; // if bLinks
            if( !bLinks && HIWORD(lrInfo) != 0 )
            {
              nextId = HIWORD(lrInfo);
              break;
            }; 
          }; // if lrMap
          
          // falls keine Wasserstände gefunden wurden: weitersuchen
          suchProfil = nextProfil;
        }; // while bSuchen
      }; // if id
      
      
      // die nextID in jedem Falle setzen
      wspRecords.Edit();
      if( wspRecords.GetEditMode() == moEditInProgress )
      {
        nextField.SetValue( CComVariant(nextId) );
        wspRecords.Update();
      }; // if EditMode = moEditInProgress
      
      wspRecords.MoveNext();
    }; // while wspRecords
    
    // zuletzt noch das Polygonthema der Überschwemmungsgrenzen erstllen
    MakeUGrenzenFromWaterLevel( waterLayer );
    
    waterLayer = layers->FindNextLayer( CLayer::waterLevel, &waterPos );
  }; // while waterLayer
};

BOOL CMapDoc::SaveProfileModifications( CrossSectionArray* cSections )
// speichere Daten aus den Layer in die Profile
// Parameter:
//        CrossSectionArray* cSections: die zu speichernden Profile
// Rückgabewert:
//        BOOL: FALSE bei kritischen Fehler oder Abbruch durch den Benutzer;
//                in diesem Falle sollten die Datnblöcke nicht gespeichert werden
// Bemerkung: die Profile müssen bereits geladen sein
{
  CLayerArray* layers = GetLayers();
  CMainFrame* mainFrame = (CMainFrame*)theApp.GetMainWnd();
  if( !mainFrame || !layers || !cSections )
    return FALSE;
  
  // desweiteren brauchen wir eine Zuordnung ProfilID -> Id der Crosssection in cSections
  // deswegen mal die ProfilLinien auslesen
  CMapLayer* profilLayer = layers->FindFirstLayer( CLayer::profilLines );
  if( !profilLayer )
    return FALSE;
  
  // weils lange dauert brauchen wir eine ProgressCtrl
  CProgressCtrl* progress = mainFrame->CreateStatusBarProgress( CString(MAKEINTRESOURCE(IDS_WRITE_MAP_TO_PROFILES)) );
  if( !progress )
    return FALSE;
  
  // zuerst mal zählen, wie viele Daten wir haben
  int recordCount = 0;
  for( int i = 0; i < layers->GetSize(); i++ )
  {
    CLayer* layer = layers->GetAt( i );
    if( layer && layer->GetLayerType() == moMapLayer )
    {
      CMapLayer* mapLayer = (CMapLayer*)layer;
      
      CLayerData* lData = GetLayerData( layer->GetType() );
      CMoRecordset records( mapLayer->GetRecords() );
      
      if( LPDISPATCH(records) && lData && ( lData->nProfilBezug >= 0 || layer->GetType() == CLayer::profilPoints ) )
        recordCount += records.GetCount();
    }; // if layer
  }; // for i
  
  progress->SetRange32( 0, recordCount );
  
  // die ProfilLinien auslesen
  CStringArray profilNames; // profilID |-> Dateiname der Cs
  profilLayer->GetFieldValues( CString(MO2_FIELD_FILE), profilNames );

  // ein neues CrossSectionArray erstellen, in welchem profilID mit Array Index übereinstimmt
  // insbesondere sind dort nur diejenigen drin, welche in den ProfilLinien vorhanden sind
  CrossSectionArray sortedCsArray; // profilID |-> CrossSection
  sortedCsArray.SetSize( profilNames.GetSize() );

  for( i = 0; i < profilNames.GetSize(); i++ )
  {
    CString file = profilNames[i];
    // die entsprechende CrossSection suchen
    for( int j = 0; j < cSections->GetSize(); j++ )
    {
      CrossSection* cs = cSections->GetAt( j );
      if( !cs )
        continue;
      
      if( file.CompareNoCase( cs->GetFileName() ) == 0 )
      {
        sortedCsArray.SetAtGrow( i, cs );
        break;
      }; // if file == cs.GetFileName
    }; // for j
  }; // for i

  // es wird einfach der Reihe nach durch die vorhandenen Layer gegangen und alle Punkte abgearbeitet
  // für jeden Layer, die durch ihn gespeicherten Datenblöcke holen
  // und danach den Profilen hinzufügen
  for( i = 0; i < layers->GetSize(); i++ )
  {
    CLayer* layer = layers->GetAt( i );
    
    if( !layer || layer->GetLayerType() != moMapLayer )
      continue;
    
    CMapLayer* mapLayer = (CMapLayer*)layer;
    CLayer::LayerType layerType = mapLayer->GetType();
    
    CLayerData* layerData = GetLayerData( layerType );
    if( !layerData )
      continue;

    // aus dem Layer jetzt alle Datenblöcke holen, die wir kriegen können
    CTypedPtrMap<CMapWordToPtr, WORD, DataBlockArray*> dataBlocks;
    mapLayer->GetDatablocks( dataBlocks, progress );

    // und die neuen DatenBlöcke der Liste von QuerProfilen hinzufügen
    CString dataErrors;
    BOOL bContinue = sortedCsArray.ChangeDataBlocks( dataBlocks, dataErrors );

    if( !dataErrors.IsEmpty() )
    {
      CString message;
      message.Format( IDS_DATA_ERROR_MSG, dataErrors );
      AfxMessageBox( message, MB_ERROR );
    };

    // alle übriggebliebenen Datenblöcke löschen
    POSITION pos = dataBlocks.GetStartPosition();
    while( pos )
    {
      DataBlockArray* dbArray;
      WORD dbType;
      dataBlocks.GetNextAssoc( pos, dbType, dbArray );
      if( dbArray != NULL )
      {
        for( int i = 0; i < dbArray->GetSize(); i++ )
          delete dbArray->GetAt( i );

        delete dbArray;
      } // if dbArray
    } // while pos

    // wurde bei ChangeDataBlocks ein kritischer Fehler gefunden oder durch den
    // Benutzer abgebrochen, dann jetzt abbrechen
    if( !bContinue )
    {
      mainFrame->DestroyStatusBarProgress();
      return FALSE;
    }
  }; // for i

  mainFrame->DestroyStatusBarProgress();
  progress = NULL;

  return TRUE;
}; // SaveProfileModifications

BOOL CMapDoc::SafeMapProperties( LPCTSTR filePath )
{
  return m_mapProperties->SafeToFile( filePath );
};

BOOL CMapDoc::LoadMapProperties( LPCTSTR filePath )
{
  return m_mapProperties->LoadFromFile( filePath );
};

BOOL CMapDoc::ParseCommand( NMPROJECTMNG* command )
// Befehl aus dem Projektmanager parsen
// Bemerkung: wird gleich an die ProfilAuswahl weitergeleitet
{
  if ( m_profilAuswahl && m_profilAuswahl->GetSafeHwnd() )
    return m_profilAuswahl->ParseCommand( command );
  else
    return FALSE;
}; // ParseCommand

void CMapDoc::GetWindowState()
// aktualisiert die MapProperties, was die Fesntergrössen betrifft
{
  POSITION pos = GetFirstViewPosition();
  while ( pos )
  {
    CView* pView = GetNextView( pos );
    CFrameWnd* pFrame = pView->GetParentFrame();
    
    WINDOWPLACEMENT* wPlace = new WINDOWPLACEMENT;
    pFrame->GetWindowPlacement( wPlace );
    m_mapProperties->windowRect = CRect( wPlace->rcNormalPosition );
    m_mapProperties->windowState = wPlace->showCmd;
    
    pFrame->GetDockState( m_mapProperties->dockState );
    CDockState* dockState = m_mapProperties->GetDockState();
    for ( int i = 0; i < dockState->m_arrBarInfo.GetSize(); i++ )
    {
      CControlBarInfo* pInfo = (CControlBarInfo*)dockState->m_arrBarInfo[i];
      CControlBar* bar = pFrame->GetControlBar( pInfo->m_nBarID );
      CRect barRect;
      bar->GetWindowRect( barRect );
      m_mapProperties->SetWindowRect( pInfo->m_nBarID, barRect );
    };
    
    delete wPlace;
    
    break; // es gibt nur eineView
  };
}; // GetWindowState

void CMapDoc::MoveObjectsOutwards( CMapLayer* layer )
// schiebt Profilbezogene Daten ans Ende der Profillinie
// Parameter:
//        CMapLayer* layer: alle Daten dieses Layers werden verschoben
// Bemerkung:
{
  ASSERT( layer && LPDISPATCH(layer) );
  
  // durch die Profillinien iterieren
  CLayerArray* layers = GetLayers();
  CMainFrame* mainFrame = (CMainFrame*)AfxGetApp()->GetMainWnd();
  if ( !layers || !mainFrame )
    return;

  CMapLayer* profilLayer = layers->FindFirstLayer( CLayer::profilLines );
  CMapLayer* pointLayer = layers->FindFirstLayer( CLayer::profilPoints );
  if( !profilLayer || !pointLayer )
    return;

  // Daten der ProfilLinien
  CMoRecordset profilRecords( profilLayer->GetRecords() );
  CMoFields profilFields( profilRecords.GetFields() );
  CMoField profilShapeField( profilFields.Item( COleVariant( MO2_FIELD_SHAPE ) ) );
  CMoField profilFeatureIDField( profilFields.Item( COleVariant( MO2_FIELD_FEATUREID ) ) );

  // Daten des Themas
  CMoRecordset records( layer->GetRecords() );
  CMoFields fields( records.GetFields() );
  CMoField profilIDField( fields.Item( COleVariant( MO2_FIELD_PROFILID ) ) );
  CMoField yKrdField( fields.Item( COleVariant( MO2_FIELD_YKRD ) ) );
  CMoField shapeField( fields.Item( COleVariant( MO2_FIELD_SHAPE ) ) );

  CWaitCursor wait; // das kann dauern...
  CProgressCtrl* pCtrl = mainFrame->CreateStatusBarProgress( CString(MAKEINTRESOURCE(IDS_MOVE_OBJECT_OUT) ) );
  if( pCtrl )
    pCtrl->SetRange32( 0, profilRecords.GetCount() + records.GetCount() );

  while( !profilRecords.GetEof() )
  {
    CMoLine profilLine( profilShapeField.GetValue().pdispVal );
    CMoParts profilParts( profilLine.GetParts() );
    if ( profilParts.GetCount() != 1 )
    {
      profilRecords.MoveNext();
      continue;
    };
    
    CMoPoints profilPoints( profilParts.Item( CComVariant( 0 ) ) );
    if ( profilPoints.GetCount() < 2 )
    {
      profilRecords.MoveNext();
      break;
    };
    
    CMoPoint firstPoint( profilPoints.Item( CComVariant( 0 ) ) );
    CMoPoint lastPoint( profilPoints.Item( CComVariant( profilPoints.GetCount() - 1 ) ) );
    
    const long profilID = COleVariantEx( profilFeatureIDField.GetValue() );
    
    CMoRecordset records( layer->GetRecords() );
    if ( LPDISPATCH( records ) )
    {
      CMoFields fields( records.GetFields() );
      CMoField shapeField( fields.Item( COleVariant( MO2_FIELD_SHAPE ) ) );
      CMoField featureField( fields.Item( COleVariant( MO2_FIELD_FEATUREID ) ) );
      CMoField profilIDField( fields.Item( COleVariant( MO2_FIELD_PROFILID ) ) );
      
      double minDistFirst = 1e36; // kleinste abstände jeweils zum ersten bzw.
      double minDistLast = 1e36; // letzten Profilpunkt
      long firstID = 0; // featureID des Punktes mit kleinstem Abstand zum ersten Punkt
      long lastID = 0; // featureID des Punktes mit kleinstem Abstand zum letzten Punkt
      
      // in der ersten Runde die Abstände zu den Endpunkten auslesen und rausfinden,
      // welcher Punkt wohin verschoben werden soll
      while ( !records.GetEof() )
      {
        const long pID = COleVariantEx( profilIDField.GetValue() );
        if( pID == profilID )
        {
          const long featureID = COleVariantEx( featureField.GetValue() );
          CMoPoint point( shapeField.GetValue().pdispVal );
          
          double distFirst = point.DistanceTo( firstPoint );
          double distLast = point.DistanceTo( lastPoint );
          
          if ( distFirst < minDistFirst )
          {
            minDistFirst = distFirst;
            firstID = featureID;
          };
          
          if ( distLast < minDistLast )
          {
            minDistLast = distLast;
            lastID = featureID;
          };
        }; // if pID == profilID
        
        records.MoveNext();
      }; // while records.GetEof
      
      // in der zweiten Runde wirklich verschieben:
      // der Punkt mit dem kleinsten Abstand zum ersten Profilpunkt wird auf den ersten,
      // der Punkt mit dem kleinsten Abstand zum letzten Profilpunkt wird auf den letzten verschoben
      records.MoveFirst();
      while ( !records.GetEof() )
      {
        const long featureID = COleVariantEx( featureField.GetValue() );
        
        if( featureID == lastID )
        {
          records.Edit();
          shapeField.SetValue( CComVariant(lastPoint) );
          records.Update();
        };
        
        if( featureID == firstID )
        {
          records.Edit();
          shapeField.SetValue( CComVariant(firstPoint) );
          records.Update();
        };
        
        records.MoveNext();
      }; // while records
      
    }; // if records
    
    if( pCtrl )
      pCtrl->StepIt();
    
    profilRecords.MoveNext();
  }; // while profilRecords

  // am Schluss nochmal alle yKrd richtig setzen
  while( !records.GetEof() )
  {
    const long profilID = COleVariantEx( profilIDField.GetValue() );
    CMoPoint point( shapeField.GetValue().pdispVal );
    
    CMap<CString, LPCSTR, CComVariant, CComVariant&> attribute;
    if( pointLayer->FindNextPoint( point, profilID, attribute ) )
    {
      CComVariant yVar;
      attribute.Lookup( MO2_FIELD_YKRD, yVar );

      records.Edit();
      if( records.GetEditMode() == moEditInProgress )
      {
        yKrdField.SetValue( yVar );
        records.Update();
      }; // if editMode

    }; // if FindNextPoint

    if( pCtrl )
      pCtrl->StepIt();
    
    records.MoveNext();
  }; // while !records.GetEof

  mainFrame->DestroyStatusBarProgress();
  
  FireMapDocChanged( IMapDocListener::THEME_GEOMETRY_CHANGED, NULL );
}; // MoveObjectsOutwards

BOOL CMapDoc::CutThemeToProfilLines( CMapLayer* mapLayer, const CLayer::LayerType type, const CString& feature, const CString& constraint, const BOOL bDeletePoints, const CString& reli /* = TEXT("RE") */ )
// verschneidet ein Thema mit den Profillinien und erzeugt Profilbezogene Daten wie z.B. Trennflächen
// Parameter:
//        CMapLayer* mapLayer: das zu verschneidende Thema
//        const CLayer::LayerType type; das zu erzeugende Thema
//        const CString& feature: mögliche Einschränkung der Verschneidung: falls ungleich "", werden
//                          nur dijenigen Objekte von mapLayer mit den ProfilLinien verschnitten,
//                          deren Attribut Namens 'feature' gleich 'constraint' ist.
//        const CString& constraint: siehe feature
//        const BOOL bDeleteTheme: falls TRUE, werden alle Daten des Themas vom Typ type vorher gelöscht
// Rückgabewert: Erfolg der Operation
{
  CWaitCursor wait;
  bool bCreated = false; // ob der Layer neu erzeugt wurde
  
  CMainFrame* mainFrame = (CMainFrame*)AfxGetApp()->GetMainWnd();
  CLayerArray* layers = GetLayers();
  CLayerData* lData = GetLayerData( type );
  
  if( !layers || !mainFrame || !lData )
    return FALSE;

  CMapLayer* profilLayer = layers->FindFirstLayer( CLayer::profilLines );
  if( !profilLayer )
  {
    AfxMessageBox( IDS_CUT_THEME_NO_LINES, MB_ERROR );
    return FALSE;
  };

  CMapLayer* pointLayer = layers->FindFirstLayer( CLayer::profilPoints );
  if( lData->bLiesOnPoint && !pointLayer )
  {
    pointLayer = CreateMapLayer( CLayer::profilPoints, "" );
    if( pointLayer )
      AddLayer( pointLayer, TRUE );
    else
    {
      AfxMessageBox( IDS_ERROR_CREATE_LAYER, MB_ERROR );
      return FALSE;
    };
  }; // if !pointLayer

  // sql - Einschränkung erzeugen
  CString sqlStr;
  if( !feature.IsEmpty() )
    sqlStr.Format( "%s = %s", feature, constraint );
  
  // die Fortschrittsanzeige initialisieren
  CProgressCtrl* pCtrl = mainFrame->CreateStatusBarProgress( CString(MAKEINTRESOURCE(IDS_CUT_LINES_CUTTING)) );

  // die Schnittpunkte holen
  CMapObjectArray schnittPunkte( true );
  profilLayer->GetSchnittpunkte( schnittPunkte, mapLayer, sqlStr, pCtrl );

  if( schnittPunkte.GetSize() == 0 )
  {
    AfxMessageBox( IDS_NO_POINTS_FOUND );
    return FALSE;
  };

  // die Schnittpunkte zum ausgewählten Thema sowie möglicherweise zu den Profilpunkten hinzufügen
  BOOL fUpdateView = FALSE; // war die Operation erfolgreich und soll die View jetzt aufgefrischt werden?

  // Layer holen, falls nötig erzeugen
  CMapLayer* layer = NULL;
  if( type != CLayer::user_RW )
    layer = GetLayers()->FindFirstLayer( type );

  if( !layer )
  {
    layer = CreateMapLayer( type, "" );
    bCreated = true;
  }

  if( !layer )
  {
    AfxMessageBox( IDS_ERROR_CREATE_LAYER );
    return FALSE;
  }

  if( type == CLayer::user_RW )
  { // Farbe des Layers kopieren
    CMoSymbol pointSymbol( layer->GetSymbol() );
    CMoSymbol mapSymbol( mapLayer->GetSymbol() );
    pointSymbol.SetColor( mapSymbol.GetColor() );
  }; // if type == user_RW

  // Kopie der Schnittpunkte
  // die nächste Operation löscht möglicherweise ein paar und erstellt dann eine neu Kopie
  CMapObjectArray newSchnittObjects( schnittPunkte );

  // bei Beschränkten Layern jetzt evtl. Schnittunkte weglassen oder vorhandene Punkte löschen
  if( lData->nProfilBezug > 0 )
  {
    // die vorhandenen Punkte auslesen ( nur bei beschränkten Layern )
    
    CMapObjectArray presentObjects( true );
    layer->GetObjects( presentObjects, pCtrl );

    CMapObjectArrays presentObjectsByID;
    CMapObjectArrays schnittObjectsByID;
    CMapObject::SortByProfilID( presentObjects, presentObjectsByID );
    CMapObject::SortByProfilID( schnittPunkte, schnittObjectsByID );

    CMapObjectArray deletePoints( false ); // bekommt die Objekte, welche gelöscht werden sollen
    CMapObjectArray lostPoints( false ); // bekommt die Objekte, welche nicht hinzugefügt werden konnten ( zuviele pro Profil )

    presentObjectsByID.SetSize( schnittObjectsByID.GetSize() ); // damit Indices immer funktionieren
    
    for( int i = 0; i < schnittObjectsByID.GetSize(); i++ )
    {
      // wieviele Punkte sind zuviel?
      int zuviele = presentObjectsByID[i].GetSize() + schnittObjectsByID[i].GetSize() - lData->nProfilBezug;
      
      // falls Punkte gelöscht werden dürfen, zuerst aus den vorhandenen löschen
      if( bDeletePoints )
      {
        while( zuviele > 0 && presentObjectsByID[i].GetSize() > 0 )
        {
          // je nach Uferlinie denjenigen mit der kleinsten, bzw grössten yKrd löschen
          int index = -1;
          double kleinY = HUGE_VAL;
          double grossY = -HUGE_VAL;
          for( int j = 0; j < presentObjectsByID[i].GetSize(); j++ )
          {
            CMapObject* obj = presentObjectsByID[i][j];
            CComVariant yVar;
            if( obj->Lookup( MO2_FIELD_YKRD, yVar ) )
            {
              double y = yVar.dblVal;
              if( reli == "LI" && y < kleinY )
              {
                kleinY = y;
                index = j;
              }
              else if( reli == "RE" && y > grossY )
              {
                grossY = y;
                index = j;
              }
            } // if Lookup
          } // for j
          
          if( index == -1 )
            index = 0;
          
          deletePoints.Add( presentObjectsByID[i][index] );
          presentObjectsByID[i].RemoveAt( index );
          
          zuviele--;
        } // while zuviele > 0
      } // if deletePoints
      
      // wenn jetzt noch zuviele Punkte vorhanden sind, Schnittpunkte löschen
      while( zuviele > 0 )
      {
        lostPoints.Add( schnittObjectsByID[i][0] );
        schnittObjectsByID[i].RemoveAt( 0 );
        zuviele--;
      }
      
    } // for i
    
    // aus den noch vorhanden Schnittpunkten ein neues SchnittpunktArray herstellen
    newSchnittObjects.RemoveAll();
    CMapObject::AggregateArrays( schnittObjectsByID, newSchnittObjects );
    
    // für diejenigen Punkte, die nicht hinzugefügt werden konnten
    // eine Fehlermeldung generieren
    if( lostPoints.GetSize() > 0 )
    {
      profilLayer->GetProfilData( lostPoints, NULL ); // die Station auslesen
      CMapObjectArrays lostPointsById;
      CMapObject::SortByProfilID( lostPoints, lostPointsById );
      
      CString errorStations;
      BOOL bFirst = TRUE;
      for( i = 0; i < lostPointsById.GetSize(); i++ )
      {
        if( lostPointsById[i].GetSize() > 0 )
        {
          CMapObject* obj = lostPointsById[i][0];
          CComVariant stationVar;
          if( obj->Lookup( MO2_FIELD_STATION, stationVar ) )
          {
            if( bFirst )
              bFirst = FALSE;
            else
              errorStations += ", ";
            
            double station = stationVar.dblVal;
            CString stationString;
            stationString.Format( "%8.3lf", station );
            errorStations += stationString;
          }; // if Lookup
        }// if lostPointById[i].Size > 0
      }; // for i
      
      if( !errorStations.IsEmpty() )
      {
        CString message;
        message.Format( IDS_ERROR_CUT_COUNT_COORDS, lData->nProfilBezug, errorStations );
        if( AfxMessageBox( message, MB_OKCANCEL ) == IDCANCEL )
        {
          mainFrame->DestroyStatusBarProgress();
          if( bCreated )
            delete layer;
          return FALSE;
        };
      };
    }; // if lostPoints.Size() > 0

    // falls erforderlich alte Punkte löschen
    if( bDeletePoints )
    {
      mainFrame->SetStatusBarProgressText( CString( MAKEINTRESOURCE(IDS_CUT_LINES_DELETE_POINTS) ) );
      layer->RemoveObjects( deletePoints, pCtrl );
    }
  } // if lData->nProfilbezug > 0
      
  // jetzt enthält newSchnittObjecte genau die Punkte die hinzugefügt werden sollen

  // von diesen Punkten jetzt alle möglichen Daten ausrechnen ( yKrd, Höhe etc. )
  mainFrame->SetStatusBarProgressText( CString( MAKEINTRESOURCE(IDS_CUT_LINES_READING_DATA) ) );
  pointLayer->GetProfilData( newSchnittObjects, pCtrl );

  // Extrawurst für Wasserspiegellage: 
  // der Benutzer kann einen Typ angeben
  if( type == CLayer::waterLevel )
  {
    CStringInputDlg dlg( CString(MAKEINTRESOURCE(IDS_CUT_LINES_WSP_QUEST_TITLE)), CString(MAKEINTRESOURCE(IDS_CUT_LINES_WSP_QUEST_TEXT)), "HQ-", mainFrame );
    dlg.DoModal(); // kann nicht abgebrochen werden

    CString calcName = dlg.GetInput();
    CComVariant value = CComVariant( calcName );
    for( int i = 0; i < newSchnittObjects.GetSize(); i++ )
    {
      CMapObject* pObject = newSchnittObjects.GetAt( i );
      pObject->SetAt( MO2_FIELD_VARIANT, value );
    }; // for i

    // auch den Namen des Layers entsprechend setzen
    if( layer && !calcName.IsEmpty() )
    {
      layer->SetName( calcName );
      layer->SetTag( calcName );
    };
  }; // if type

  // Falls die neuen Punkte auf Profilpunkten liegen müssen, jetzt neue Profilpunkte 
  // erzeugen
  if( lData->bLiesOnPoint )
  {
    mainFrame->SetStatusBarProgressText( CString( MAKEINTRESOURCE(IDS_CUT_LINES_ADDING_PROFILE_POINTS) ) );
    pointLayer->AddObjects( newSchnittObjects, pCtrl );
  }

  mainFrame->SetStatusBarProgressText( CString( MAKEINTRESOURCE(IDS_CUT_LINES_ADDING_POINTS) ) );
  layer->AddObjects( newSchnittObjects, pCtrl );

  if( pCtrl )
    mainFrame->DestroyStatusBarProgress();

  if( bCreated )
    AddLayer( layer, FALSE );

  FireMapDocChanged( IMapDocListener::THEME_GEOMETRY_CHANGED |
                     IMapDocListener::THEME_DATA_CHANGED |
                     IMapDocListener::THEME_ADDED, 0 );

  return TRUE;
} // CutThemeToProfilLines

BOOL CMapDoc::CutThemeToProfilPoints( CMapLayer* mapLayer, const CString& feature, CZTable* zTable )
// verschneidet ein Thema mit den ProfilPunkten und belegt spezielle Attribute der Punkte mit
// Werten aus dem Thema. Die Werte werden mittels einer Zurodnungstabelle aus einem Attribut 
// des Themas ermittelt
// Parameter:
//        CMapLayer* mapLayer: das Thema, welches mit den Profilpunkten verschnitten wird
//        const CString& feature: in diesem Atrtribut stehen die Werte
//        CZTable* zTable: die Zuordnungstabelle
// Rückgabewert:
//        Erfolg der Operation
{
  CMainFrame* mainFrame = (CMainFrame*)theApp.GetMainWnd();
  if( !mainFrame )
    return FALSE;

  CMoRecordset mapRecords( mapLayer->GetRecords() );
  if( !LPDISPATCH(mapRecords) )
    return FALSE;
  
  CMoFields mapFields( mapRecords.GetFields() );
  if( !LPDISPATCH(mapFields) )
    return FALSE;
  
  CMoField mapFeatureField( mapFields.Item( COleVariant( feature ) ) );
  CMoField mapShapeField( mapFields.Item( COleVariant( MO2_FIELD_SHAPE ) ) );
  
  CMapLayer* profilLayer = GetLayers()->FindFirstLayer( CLayer::profilPoints );
  
  // ProfilLayer, ZTable und bestimmte Attribute im MapLayer müssen existieren
  if( !profilLayer || !zTable || !LPDISPATCH(mapFeatureField) || !LPDISPATCH(mapShapeField) )
    return FALSE;
  
  // die zuzuordnenden Attribute
  CStringArray zNames;
  for( int i = 0; i < zTable->GetColumnCount(); i++ )
  {
    CString name = zTable->GetColumnHeader( i );
    if( name.CompareNoCase( MO2_FIELD_RAUHEIT ) == 0 ||
      name.CompareNoCase( MO2_FIELD_RAUHEITKST ) == 0 ||
      name.CompareNoCase( MO2_FIELD_AXM ) == 0 ||
      name.CompareNoCase( MO2_FIELD_AYM ) == 0 ||
      name.CompareNoCase( MO2_FIELD_DPM ) == 0 )
      zNames.Add( name );
  } // for i
    
  // einen Vortschrittsdialog aufmachen
  CProgressCtrl* pCtrl = mainFrame->CreateStatusBarProgress( CString(MAKEINTRESOURCE(IDS_VERSCHNEID_PROGRESS)) );
  if( pCtrl != NULL )
    pCtrl->SetRange32( 0, mapRecords.GetCount() );
  CWaitCursor wait;
  
  // alle Objekte des Themas durchgehen
  while( !mapRecords.GetEof() )
  {
    LPDISPATCH disp = mapShapeField.GetValue().pdispVal; // den dispatch sichern
    
    // welcher Wert soll zugeordnet werden?
    const int typeValue = COleVariantEx( mapFeatureField.GetValue() );
    
    // teste, ob dieser Wert ( typeValue ) in der Zuordnungstebelle vorhanden ist
    // sonst nichts zuordnen
    if( zTable->GetRowIndex( typeValue ) != -1 )
    {
      CMoRecordset profilRecords( profilLayer->SearchShape( disp, moContaining, TEXT("") ) );
      disp->Release(); // den Dispatch loswerden
      disp = NULL;
      
      if( LPDISPATCH( profilRecords ) && profilRecords.GetUpdatable() )
      {
        profilRecords.SetAutoFlush( FALSE );
        CMoFields profilFields( profilRecords.GetFields() );

        CTypedPtrArray<CPtrArray, CMoField*> zFields; // die zu den Parameter passenden Felder
        CArray<double, double> typeValues; // die zugeordneten Werte

        // die Felder initialisieren
        for( int z = 0; z < zNames.GetSize(); z++ )
        {
          CString name = zNames[z];
          CMoField* field = new CMoField( profilFields.Item( COleVariant( name ) ) );
          if( LPDISPATCH(field) )
          {
            zFields.Add( field );
            typeValues.Add( zTable->GetValue( typeValue, name ) );
          }
          else
            delete field;
        }; // for z
        
        while( !profilRecords.GetEof() )
        {
          // jetzt zuordnen
          profilRecords.Edit();
          for( z = 0; z < zFields.GetSize(); z++ )
            zFields[z]->SetValue( CComVariant( typeValues[z] ) );
          
          profilRecords.Update();
          profilRecords.MoveNext();
        }; // while profilRecords

        // die Felder wieder freigeben
        for( z = 0; z < zFields.GetSize(); z++ )
          delete zFields[z];
      }; // if profilRecords 
    }; // falls type gefunden
    
    if( pCtrl )
      pCtrl->StepIt();
    
    mapRecords.MoveNext();
  }; // while mapRecords

  // Die Fortschrittsanzeige wieder zerstören
  mainFrame->DestroyStatusBarProgress();

  FireMapDocChanged( IMapDocListener::THEME_DATA_CHANGED | IMapDocListener::THEME_GEOMETRY_CHANGED, 0 );
  
  return TRUE;
}; // CutThemeToProfilPoints


/**
 * Erzeugt ein neues Querprofil anhand einer Linie und eines Geländemodells.
 *
 * @param profilLine : die Lage des neuen Profils
 * @param hmoLayer : aus diesem Geländemodell kommt die Kontur
 * @param stateName: ists ungleich null, wird in diesen Zustand importiert und nicht gefragt
 * @param station : ists ungleich null, wird diese Station fest gesetzt und mögl nicht gefragt
 * @throws cancel_action : falls bei der Frage nach doppelten Schnittpunkten Abbrechen gedrückt wird
 */
void CMapDoc::GenerateProfile( CMoLine profilLine, CMapLayer* hmoLayer, bool bReportNoPoints, int* pLastCutAnswer, State* state, const double* station, const CString& text )
{
  CMoMap* pMap = GetMap();
  CLayerArray* pLayers = GetLayers();

  ASSERT( pMap && pLayers && hmoLayer );
  if( !pMap || !pLayers || !hmoLayer )
    return;

  // jetzt die richtige Flusslinie holen
  CMapLayer* pLayer = (CMapLayer*)pLayers->FindFirstLayer( CLayer::flussachse );
  
  CMoLine riverLine;
  int vzk = 0;
  if( pLayer )
  {
    CMapObject riverObject = pLayer->SearchFirstObject( profilLine, moEdgeTouchOrAreaIntersect, 0 );
    CComVariant dispVar = riverObject.GetAt( MO2_FIELD_SHAPE );
    COleVariantEx vzkVar( riverObject.GetAt( MO2_FIELD_VZK ) );
    if( dispVar.vt != VT_EMPTY && dispVar.pdispVal != 0 && vzkVar.vt != VT_EMPTY )
    {
      riverLine = CMoLine( dispVar.pdispVal ); // einfach das erste Objekt, welches die ProfilLinie schneidet
      vzk = int( vzkVar );
    }
    else
      return; // die Flussachse schneidet dieses Profil nicht
  }
  else
  {
    // die Handgezogene Flusslinie finden
    CMoTrackingLayer trackLayer = pMap->GetTrackingLayer();
    if( !LPDISPATCH(trackLayer ) )
      return;

    CMoGeoEvent riverEvent = trackLayer.FindEvent( MO2_TRACKTAG_RIVER );
    if( LPDISPATCH(riverEvent) )
    {
      riverLine = riverEvent.GetShape();
      if( !LPDISPATCH(riverLine) )
        trackLayer.RemoveEvent( riverEvent.GetIndex() );
    }
  }; // if pLayer

  if( !LPDISPATCH(riverLine) )
  {
    AfxMessageBox( IDS_GENPROFILE_DIGITRIVER, MB_OK | MB_ICONWARNING );
    throw cancel_action(); // damit wirklich abgebrochen wird
  };

  // überprüfen, ob es bereits einen Layer profilLines und profilPoints gibt, sonst diese beiden anlegen
  CMapLayer* pProfilLines = CreateAndAddLayer( CLayer::profilLines, "", FALSE );
  CreateAndAddLayer( CLayer::profilPoints, "", FALSE );

  // das objekt markieren und am Ende der funktion wieder unmarkieren
  CMarkMapObject m0( LPDISPATCH(profilLine), pProfilLines, pMap->GetExtent(), pMap->GetTrackingLayer(), MO2_TRACKSYMBOL_GENERATEPROFILE );

  // mit dieser Linie, der ProfilLinie und dem Geländemodell ein neues Profil erzeugen
  GenerateProfile( riverLine, profilLine, hmoLayer, bReportNoPoints, pLastCutAnswer, state, station, &vzk, text );
}


BOOL CMapDoc::GenerateProfile( CMoLine& river, CMoLine& line, CMapLayer* hmoLayer, bool bReportNoPoints, int* pLastCutAnswer, State* state, const double* station, const int* vzk, const CString& comment )
// verschneidet eine Linie mit einem Geländemodell und generiert so ein neues Profil
// die Position des Profils innerhalb der Strangtabelle wird versucht mithilfe des Digitalisierten Flusses zu ermitteln
// Parameter:
//        const CMoLine& river: eine Linie, die dem Fluss entsprechen sollte( Flussmitte ) und das 
//                              zu erzeugende Profil schneiden muss
//                              oder ein NULL-Dispatch: dann wird das neue Profil ans Strangende sortiert
//        const CMoLine& line: die zu verschneidende Linie
//        int* pLastCutAnswer: die letzte Antwort auf die Frage, was passieren soo, wenn die Profillinie die Flusslinie mehrfach schneidet:
//                              IDNEVER, IDCANCEL: Abbruch, IDALWAYS: fortfahren: sonst Fragen
//                             die Variable wird dann entsprechend neu gesetzt
// Rückgabewert:
//        Erfolg, Misserfolg der Operation
// Throw: cancel_action: falls bei einer Frage Abbrechen gedrückt wurde
{
  // Voraussetzungen überprüfen
  CMoMap* pMap = GetMap();
  CMainFrame* pMainFrm = (CMainFrame*)theApp.GetMainWnd();
  CLayerArray* layers = GetLayers();
  
  if( !pMap || !pMainFrm || !layers || !LPDISPATCH(line) || !hmoLayer )
    return FALSE;

  // testen, wie oft das Profil den Fluss schneidet
  long cutCount = 0;
  if( LPDISPATCH(river ) )
    cutCount = CMoPoints( river.GetCrossings( line ) ).GetCount();
  switch( cutCount )
  {
  case 0: // kein Schnittpunkt oer river gar nicht gültig
    AfxMessageBox( IDS_ERROR_RIVER_CUTS_PROFILE );
    return FALSE; // mit Fehler zurück

  case 1: // so muss es sein
    break;

  default:
    {
      // Abhängig davon, was da letzte Mal geantwortet wurde
      if( !pLastCutAnswer )
        return FALSE; // im Zweifelsfall einfach raus

      if( *pLastCutAnswer != IDNEVER && *pLastCutAnswer != IDALWAYS )
      {
        CMessageBox5 dlg( CString(MAKEINTRESOURCE(IDS_ERROR_RIVER_CUTS_MULTIPLE_TITLE)), CString(MAKEINTRESOURCE(IDS_ERROR_RIVER_CUTS_PROFILE_MULTIPLE)), pMainFrm );
        *pLastCutAnswer = dlg.DoModal();
        pMainFrm->UpdateWindow();
      };

      if( *pLastCutAnswer == IDNEVER || *pLastCutAnswer == IDNO )
        return FALSE;
      else if( *pLastCutAnswer == IDCANCEL )
        throw cancel_action(); // ganz hart raus
    }; // default
    break;
  }; // switch count

  // das Profil mit dem Geländemodell verschneiden
  // kann ein bisschen dauern: deswegen ProgressCtrl und WaitCursor
  CWaitCursor wait;
  
  CProgressCtrl* progressCtrl = pMainFrm->CreateStatusBarProgress( CString(MAKEINTRESOURCE(IDS_CUT_LINE_WITH_DGM)) );
  TripleArray* tripArray = hmoLayer->CutWithLine( line, CStringArray(), NULL, progressCtrl );
  pMainFrm->DestroyStatusBarProgress();
  progressCtrl = NULL;
  
  // ergebniss des verschneidens überprüfen
  if( !tripArray || tripArray->size() < 1 )
  {
    if( tripArray && tripArray->size() == 1 ) // den einzigen Punkt löschen 
      delete tripArray->at( 0 );

    // nur eine Meldug geben, wenn dies gewünscht ist
    if( bReportNoPoints == true )
      AfxMessageBox( IDS_PROFILE_EXTEND_NO_POINTS, MB_ICONEXCLAMATION );
    return FALSE;
  }; // if !tripArray

  // falls andere ProfilLinien vorhanden sind, versuchen, die Position des neuen Profils zu bestimmen
  bool bRet = true;
  if( state != 0 && vzk != 0 && station != 0 )
    m_profilAuswahl->AddNewProfile( tripArray, true, state, *station, *vzk, "0", comment );
  else
  {
    CString profilFiles[2]; // vorhergehendes, neues, und nachfolgendes Profil ( "" heisst immer gibts nicht )
    CString stateFiles[2]; // die entsprechenden Zustände
    double profilAbstaende[2]; // abstand zum vorhergehenden und nachfolgenden
    
    CMapLayer* profilLines = layers->FindFirstLayer( CLayer::profilLines );
    if( profilLines && profilLines->GetValid() )
    {
      // die Schnittpunkte des Profils mit den Profillinien holen
      CStringArray fieldNames;
      fieldNames.Add( MO2_FIELD_FILE );
      fieldNames.Add( MO2_FIELD_STATE );
      
      TripleArray* schnittPunkte = profilLines->CutWithLine( river, fieldNames, line ); // progressCtrl );
      
      // falls schnittpunkte gefunden wurden, die benachtbarten Profile suchen und die Abstände zu diesen ausrechnen
      if( schnittPunkte )
      {
        // die drei gesuchten Positionen im TrippleArray: das Profil davor, das Profil danach und das neue Profil
        int pos[3] = { -1, -1, -1 };
        
        for( int i = 0; i < schnittPunkte->size(); i++ )
        {
          Triple* triple = schnittPunkte->at( i );
          
          if( triple->attribute[0].CompareNoCase( MO2_EXTRA_OBJECT ) == 0 )
          {
            pos[0] = i - 1;
            pos[1] = i;
            if( i + 1 != schnittPunkte->size() ) // falls keins mehr kommt
              pos[2] = i + 1;
            break;
          };
        }; // for i
        
        // die drei Informationen auslesen
        Triple* triples[3] = { NULL, NULL, NULL };
        CString stateAttributes[3];
        CString fileAttributes[3];
        double yAbst[3] = { 0.0, 0.0, 0.0 };
        for( i = 0; i < 3; i++ )
        {
          if( pos[i] != -1 )
          {
            triples[i] = schnittPunkte->at( pos[i] );
            fileAttributes[i] = triples[i]->attribute[0];
            stateAttributes[i] = triples[i]->attribute[1];
            yAbst[i] = triples[i]->breite;
          };
        }; // for i
        
        profilAbstaende[0] = fabs( yAbst[1] - yAbst[0] );
        profilAbstaende[1] = fabs( yAbst[2] - yAbst[1] );
        profilFiles[0] = fileAttributes[0];
        profilFiles[1] = fileAttributes[2];
        stateFiles[0] = stateAttributes[0];
        stateFiles[1] = stateAttributes[2];
        
        // und das Tripple-Array löschen, wird ncht mehr benötigt
        for( i = 0; i < schnittPunkte->size(); i++ )
          delete schnittPunkte->at( i );
        delete schnittPunkte;
        schnittPunkte = NULL;
      }; // if schnittPunkte
    }; // if profilLines

    bRet = m_profilAuswahl->AddNewProfile( tripArray, profilFiles, stateFiles, profilAbstaende, true );

    FireMapDocChanged( IMapDocListener::THEME_DATA_CHANGED | IMapDocListener::THEME_GEOMETRY_CHANGED, 0 );
  }
  
  // das TrippleArray löschen: wird nicht mehr benötigt
  for( int i = 0; i < tripArray->size(); i++ )
    delete tripArray->at( i );
  delete tripArray;
  tripArray = 0; // damit der Zeiger sicher nicht mehr benutzt wird

  return bRet;
}

/**
 * verlängert ein Profil, indem es eine Linie mit einem Höhenmodell verschneidet
 * @param line die zu verschneidende Linie
 * @param csFile Dateiname des Profils
 * @param stateFile Dateiname des Zustandes, unter welchem das Profil geladen ist
 */
BOOL CMapDoc::ExtendProfile( CMoLine& line, const int profilID )
{
  CMainFrame* pMainFrm = (CMainFrame*)theApp.GetMainWnd();
  CLayerArray* layers = GetLayers();
  
  if( !LPDISPATCH(line) || !pMainFrm || !layers )
    return FALSE;
  
  // zuerst erfragen, mit welchem Geländemodell verschnitten werden soll
  CDgmDlg dlg( layers );
  if( dlg.DoModal() != IDOK )
    return FALSE;
  
  // jetzt die Linie mit dem DGM verschneiden
  CMapLayer* hmoLayer = dlg.GetLayer();
  if( !hmoLayer )
    return FALSE;
  
  // kann ein bisschen dauern: deswegen ProgressCtrl und WaitCursor
  CProgressCtrl* progressCtrl = pMainFrm->CreateStatusBarProgress( CString(MAKEINTRESOURCE(IDS_CUT_LINE_WITH_DGM)) );
  CWaitCursor* pWait = new CWaitCursor();
  TripleArray* tripArray = hmoLayer->CutWithLine( line, CStringArray(), NULL, progressCtrl );
  delete pWait;
  pWait = 0;
  pMainFrm->DestroyStatusBarProgress();
  progressCtrl = NULL;
  
  if( tripArray && tripArray->size() > 0 )
  {
    // falls der Abstand des ersten Punktes = 0 ist, diesen löschen ( der Anfangspunkt der linie lag über dem
    // Geländemodell, dieser Punkt ist aber schon im Profil vorhanden )
    Triple* tripl = (Triple*)tripArray->at( 0 );
    if( fabs( tripl->breite ) < 0.0001 )
    {
      tripArray->RemoveAt( 0, false );
      delete tripl;
      tripl = NULL;
    }; // if abstand == 0
  }; // if tripArray
  if( !tripArray || tripArray->size() < 1 )
  {
    delete tripArray;
    AfxMessageBox( IDS_PROFILE_EXTEND_NO_POINTS, MB_ICONEXCLAMATION );
    return FALSE;
  };
  
  BOOL bRet = FALSE;
  
  Profil* profil = Profil::CreateFromTriple( *tripArray );
  if( profil )
  {
    CString titel( MAKEINTRESOURCE(IDS_EXTENDPROFILE_TITEL) );
    CString okText( MAKEINTRESOURCE(IDS_EXTENDPROFILE_OKTEXT) );

    CExtendProfileDlg extendDlg( titel, okText, profil, tripArray, pMainFrm );
    int modalRet = extendDlg.DoModal();
    
    // das Profil löschen
    delete profil;
    profil = NULL; // wurde auschliesslich für die Vorschau benötigt
    
    if( modalRet == IDOK )
    {
      // jetzt die neuen Punkte dem Profil hinzufügen
      CMapLayer* profilLayer = layers->FindFirstLayer( CLayer::profilLines );
      if( profilLayer )
      {
        // für AddTrippleToProfile ist es wichtig, den Punkt der Profillinie zu übergeben, an welchen angehängt werden soll
        CMoPoints linePoints( GetPointsFromObject( COleDispatchDriverEx( line ) ) );
        if( LPDISPATCH(linePoints) && linePoints.GetCount() > 0 )
        {
          CMoPoint startPoint( linePoints.Item( CComVariant( 0 ) ) );
          if( LPDISPATCH(startPoint) )
          {
            // vor AddTrippleToProfil müssen die alten ProfilIDs gemerkt werden
            std::map<long, CString> idMap = profilLayer->GetIDToFileMap();
            const int oldActive = GetActiveProfile();
            const CString activeStr = idMap[ oldActive ]; // den Namen des aktiven Profils holen und merken
            
            // kann ein bisschen dauern, deswegen WaitCursor und Fortschrittsanzeige
            CWaitCursor wait;
            progressCtrl = pMainFrm->CreateStatusBarProgress( CString(MAKEINTRESOURCE(IDS_ADD_POINTS_TO_MAP)) );
            
            bRet = AddTrippleToProfile( tripArray, profilID, startPoint, progressCtrl );
            
            // danach müssen die ID neu gesetzt werden
            pMainFrm->SetStatusBarProgressText( CString(MAKEINTRESOURCE(IDS_UPDATE_DATA)) );
            const int newActive = ChangeProfilIDs( idMap, activeStr, progressCtrl );
            
            // und das aktive Profil wieder setzen
            SetActiveProfile( newActive, FALSE );
            
            pMainFrm->DestroyStatusBarProgress();
            progressCtrl = NULL;
          }; // if startPoint
        }; // if linePoints
      }; // if profilLayer
    }; // if IDOK
  };
  
  // gleich das TrippleArray löschen: wird nicht mehr benötigt
  for( int i = 0; i < tripArray->size(); i++ )
    delete tripArray->at( i );
  delete tripArray;
  tripArray = NULL; // damit der Zeiger sicher nicht mehr benutzt wird
  
  // zuletzt noch die ansicht auffrischen
  if( bRet ) // sonst hat sich nichts verändert
    FireMapDocChanged( IMapDocListener::THEME_GEOMETRY_CHANGED | IMapDocListener::ACTIVE_PROFILE_CHANGED, 0 );
  
  return bRet;
}; // ExtendProfile


/**
 * fügt durch ein TrippleArray gegebene Punkte einem gegebenen Profil hinzu
 * 
 * @param tripArray Koordinaten der hinzuzufügenden Punkte
 * @param csFile Namen des Profils, an welches hinzugefügt werden soll
 * @param startPoint dieser Punkt muss einem der Endpunkte der Profillinie entsprechen; anhand dieses 
 *        Punktes wird entschieden, wo angehängt wird
 *
 * @return Erfolg der operation
 *
 * Bemerkung: Verändert die FeatureIds der ProfilLinien -> ProfilIDs müssen umnummeriert werden
 *            Zeigt ein Fortschrittsanzeige an
 */
BOOL CMapDoc::AddTrippleToProfile( TripleArray* tripArray, const int profilID, CMoPoint& startPoint, CProgressCtrl* progressCtrl /* = MULL */ )
{
  CLayerArray* layers = GetLayers();
  if( !layers || !tripArray )
    return FALSE;
  
  // ProfilLinienlayer holen, ggfls. erzeugen
  CMapLayer* profilLines = layers->FindFirstLayer( CLayer::profilLines );
  CMapLayer* profilPunkte = layers->FindFirstLayer( CLayer::profilPoints );
  if( !profilLines || !profilPunkte ) // es kann nur an bereits bestehende Profile angehängt werden
    return FALSE;
  
  // zuerst die ProfilLinie selbst holen
  VARIANT var = profilLines->GetFieldValByID( profilID, MO2_FIELD_SHAPE );
  if( var.vt != VT_DISPATCH )
    return FALSE;
  
  var.pdispVal->AddRef();
  COleDispatchDriverEx object( var.pdispVal );
  if( long( COleVariantEx( object.GetProperty( "ShapeType" ) ) ) != moShapeTypeLine )
    return FALSE;
  
  CMoLine line( object );
  CMoParts lineParts( line.GetParts() );
  if( !LPDISPATCH(lineParts) || lineParts.GetCount() != 1 )
    return FALSE;
  CMoPoints linePoints( lineParts.Item( CComVariant( 0 ) ) );
  if( !LPDISPATCH(linePoints) || linePoints.GetCount() < 2 )
    return FALSE;
  
  // feststellen an welcher Seite angefügt werden soll und mit welchen y-Versatz
  CMoPoint anfPoint( linePoints.Item( CComVariant( 0 ) ) );
  CMoPoint endPoint( linePoints.Item( CComVariant( linePoints.GetCount() - 1 ) ) );
  if( !LPDISPATCH(anfPoint) || !LPDISPATCH(endPoint) )
    return FALSE;
  
  BOOL bVorneEinfuegen;
  double signum; // zum Ausrechnen des y-Abstandes
  CMoPoint point;
  if( anfPoint.DistanceTo( startPoint ) < endPoint.DistanceTo( startPoint ) )
  {
    bVorneEinfuegen = TRUE;
    point = anfPoint;
    signum = -1.0;
  }
  else
  {
    bVorneEinfuegen = FALSE;
    point = endPoint;
    signum = 1.0;
  }; // distanceTo
  
  // die Records und Felder der Profilpunkte
  CMoRecordset pointRecords( profilPunkte->GetRecords() );
  if( !LPDISPATCH(pointRecords) || !pointRecords.GetUpdatable() )
    return FALSE;
  
  CMoFields pointFields( pointRecords.GetFields() );
  if( !LPDISPATCH(pointFields) )
    return FALSE;
  CMoField pointShapeField( pointFields.Item( CComVariant( MO2_FIELD_SHAPE ) ) );
  CMoField pointProfilIDField( pointFields.Item( CComVariant( MO2_FIELD_PROFILID ) ) );
  if( !LPDISPATCH(pointShapeField) || !LPDISPATCH(pointProfilIDField) )
    return FALSE;
  CMoField pointYField( pointFields.Item( CComVariant( MO2_FIELD_YKRD ) ) );
  CMoField pointHeightField( pointFields.Item( CComVariant( MO2_FIELD_HEIGHT ) ) );
  CMoField pointNumberField( pointFields.Item( CComVariant( MO2_FIELD_NUMBER ) ) );
  
  // die ProgressCtrl setzten
  int progressCount = pointRecords.GetCount() + tripArray->size();
  if( bVorneEinfuegen )
    progressCount += pointRecords.GetCount();
  if( progressCtrl )
  {
    progressCtrl->SetRange( 0, progressCount );
    progressCtrl->SetPos( 0 );
  }; // if progressCtrl
  
  // zuerst den y-Versatz des Startpunktes und dei maximale Punkt ID finden
  double deltaY = 0.0; // falls sich Wert nicht feststellen lässt
  double maxDist = 1e36; // hoffentlich haben nie alle Punkte einen grösseren Abstand zum startPunkt
  int maxPointNumber = 0; // 0 falls sich Wert nicht feststellen lässt
  
  while( !pointRecords.GetEof() )
  {
    if( progressCtrl )
      progressCtrl->StepIt();
    
    if( profilID == long( COleVariantEx( pointProfilIDField.GetValue() ) ) )
    {
      VARIANT var = pointShapeField.GetValue();
      if( var.vt == VT_DISPATCH && var.pdispVal )
      {
        CMoPoint profilPoint( var.pdispVal );
        var.pdispVal->AddRef();
        
        double distance = point.DistanceTo( profilPoint  ); // Abstand des aktuellen Punktes zum startPunkt
        if( distance < maxDist && LPDISPATCH(pointYField) )
        {
          maxDist = distance;
          deltaY = COleVariantEx( pointYField.GetValue() );
        }; // distance < maxDist
        
        if( LPDISPATCH(pointNumberField) )
        {
          COleVariantEx var( pointNumberField.GetValue() );
          if( var.vt != VT_ERROR )
          {
            long pointNumber = long( var );
            maxPointNumber = max( maxPointNumber, pointNumber );
          }
        }; // pointNumberField
        
      }; // if vt
    }; // if profilID
    pointRecords.MoveNext();
  }; // while pointRecords
  
  // falls vorne eingefügt wird, müssen die Punktnummer^n der Punkte verändert werden
  if( bVorneEinfuegen && LPDISPATCH(pointNumberField) )
  {
    pointRecords.SetAutoFlush( FALSE );
    pointRecords.MoveFirst();
    while( !pointRecords.GetEof() )
    {
      if( progressCtrl )
        progressCtrl->StepIt();
      
      if( profilID == long( COleVariantEx( pointProfilIDField.GetValue() ) ) )
      {
        pointRecords.Edit();
        
        if( pointRecords.GetEditMode() == moEditInProgress )
          pointNumberField.SetValue( CComVariant( (int)(tripArray->size() + int( COleVariantEx( pointNumberField.GetValue() ) ) ) ) );
        
        pointRecords.Update();
      }; // if profilID == ...
      
      pointRecords.MoveNext();
    }; // while pointRecords
    
    pointRecords.SetAutoFlush( TRUE ); // damit Änderungen wirksam werden vor dem AddNew
  }; // if bVorneEinfügen
  
  // jetzt der Profillinie die neuen Punkte hinzufügen,
  // ebenso dem PunkteLayer neue Punkte hinzufügen
  pointRecords.SetAutoFlush( FALSE );
  for( int i = 0; i < tripArray->size(); i++ )
  {
    if( progressCtrl )
      progressCtrl->StepIt();
    
    Triple* tripl = tripArray->at( i );
    CMoPoint point;
    MO2CREATE( point, "Point" );
    if( !LPDISPATCH(point) )
      continue;
    
    double y = deltaY + signum * tripl->breite; // je nachdem ob vorne oder hinten angefügt wird
    
    point.SetX( tripl->rw );
    point.SetY( tripl->hw );
    
    pointRecords.AddNew();
    if( pointRecords.GetEditMode() == moEditAdd )
    {
      pointShapeField.SetValue( CComVariant( point ) );
      pointProfilIDField.SetValue( CComVariant( profilID ) );
      if( LPDISPATCH(pointYField) )
        pointYField.SetValue( CComVariant( y ) );
      if( LPDISPATCH(pointHeightField) )
        pointHeightField.SetValue( CComVariant(tripl->hoehe) );
      
      if( LPDISPATCH(pointNumberField) )
      {
        // die neue Nummer des Punktes ausrechnen
        int newNumber;
        if( bVorneEinfuegen )
          newNumber = tripArray->size() - i - 1;
        else
          newNumber = maxPointNumber + i + 1;
        
        pointNumberField.SetValue( CComVariant( newNumber ) );
      }; // if pointnumberField
      
      pointRecords.Update();
    }; // mode == moEditAdd
    
    
    // auch noch der Linie einen punkt hinzufügen
    if( bVorneEinfuegen )
      linePoints.Insert( 0, point );
    else
      linePoints.Add( point );
    
  }; // for i
  
  // den Punkte Layer freigeben, weil er in der Nächsten Funktion benutzt wird
  pointRecords.SetAutoFlush( TRUE );
  pointRecords.ReleaseDispatch();

  // falls vorne angefügt wurde, auch noch das deltaY der Linie verändern
  // muss vor ändern der Shape passieren, da sonst die profilID nicht mehr stimmt
  if( bVorneEinfuegen )
  {
    Triple* lastTrip = tripArray->at( tripArray->size() - 1 );
    double y = deltaY + signum * lastTrip->breite;
    profilLines->SetFieldValByID( profilID, MO2_FIELD_DELTAY, CComVariant( y ) );
  };
  
  // jetzt die ProfilLinie ändern: -> Umbiegen der ProfilIDs nötig 
  profilLines->SetFieldValByID( profilID, MO2_FIELD_SHAPE, CComVariant( line ) );
  
  return TRUE;
}; // AddTrippleToProfile



/**
 * korrigiert die ProfilIDs aller Layer anhand des ProfilDateinamens, gibt für einen speziellen Dateinamen die neue
 * Nummer zurück
 *
 * @param const std::map<long, CString>& zurodnung der alten IDs zu den Dateinamen
 * @param const CString& profilFile für diesen Namen wird die Nummer zurückgegeben
 * @param CProgressCtrl* falls ungleich NULL, wird auch noch diese Forschrittsanzeige weitergeführt
 *
 * @return neue ID von profilFile, -1 bei Fehler
 *
 * Bemerkung;
 *       es wird Festgestellt, zu welchem Datienamen welche ProfilID gehört, dann werden alte IDs auf Dateinamen
 *       und Dateinamen auf neue IDs abgebilded
 */
int CMapDoc::ChangeProfilIDs( const std::map<long, CString>& idMap, const CString& profilFile, CProgressCtrl* progressCtrl )
{
  CLayerArray* layers = GetLayers();
  CMapProperties* mapProps = GetMapProperties();
  if( !layers || !mapProps )
    return -1;
  
  // die Beziehung Dateiname |-> neue ID generieren
  CMapLayer* lines = layers->FindFirstLayer( CLayer::profilLines );
  if( !lines || !lines->GetValid() )
    return -1;
  
  std::map<CString, long> fileMap = lines->GetFileToIDMap();
  
  if( progressCtrl )
  {
    progressCtrl->SetRange( 0, layers->GetSize() );
    progressCtrl->SetPos( 0 );
  }; // if progressCtrl
  
  // alle Layer durchgehen, diejenigen welche ein Feld ProfilID haben umbenennen
  for( int i = 0; i < layers->GetSize(); i++ )
  {
    if( progressCtrl )
      progressCtrl->StepIt();
    
    CLayer* layer = layers->GetAt( i );
    if( !layer || layer->GetLayerType() != moMapLayer )
      continue;
    CMapLayer* mapLayer = (CMapLayer*)layer;
    
    CLayerData* layerData = GetLayerData( layer->GetType() );
    if( !layerData || layerData->nProfilBezug == -1 )
      continue;
    
    // Layer ist ok und Profilbezogen, Id sollten also umbenannt werden
    CMoRecordset records( mapLayer->GetRecords() );
    if( !LPDISPATCH(records) || !records.GetUpdatable() )
      continue;
    CMoFields fields( records.GetFields() );
    if( !LPDISPATCH(fields) )
      continue;
    CMoField idField( fields.Item( CComVariant( MO2_FIELD_PROFILID ) ) );
    if( !LPDISPATCH(idField) )
      continue;
    
    // jetzt alle records durchgehen und umnummerieren
    records.SetAutoFlush( FALSE );
    while( !records.GetEof() )
    {
      const UINT oldId = int( COleVariantEx( idField.GetValue() ) );

      std::map<long, CString>::const_iterator pos = idMap.find( oldId );

      if( pos != idMap.end() )
      {
        const CString& fileName = (*pos).second;

        const long newID = fileMap[fileName];
        if( newID != 0 && newID != oldId )
        {
          records.Edit();
          if( records.GetEditMode() == moEditInProgress )
            idField.SetValue( CComVariant( newID ) );
          records.Update();
        }; // if fileMap
      }
          
      records.MoveNext();
    }; // while records
    records.SetAutoFlush( TRUE );
  }; // for i
  
  // auch noch für das spezielle File die ID suchen
  long returnVal = fileMap[profilFile];
  if( returnVal == 0 )
    returnVal = -1;
  
  return returnVal;
}; // ChangeProfilIDs

void CMapDoc::FlushLayers()
// löscht alle leeren Layer und deren Shapefiles und setzt alle Layer auf AutoFlush
// und forciert das Indexing
{
  CStringArray fileNames; // diese ShapeFiles sollen gelöscht werden
  CLayerArray* layers = GetLayers();
  if( !layers )
    return; // nichts zu tun
  
  for ( int i = 0; i < layers->GetSize(); i++ )
  {
    CLayer* layer = layers->GetAt( i );
    if( layer && layer->GetLayerType() == moMapLayer )
    {
      CMapLayer* mapLayer = (CMapLayer*)layer;
      mapLayer->BuildIndex( TRUE );
      
      CMoRecordset records( mapLayer->GetRecords() );
      if( LPDISPATCH(records) )
      {
        records.SetAutoFlush( TRUE );
        
        if( records.GetCount() == 0 )
        {
          RemoveLayer( mapLayer, FALSE );
          fileNames.Add( GetMapPath() + mapLayer->GetGeoDatasetName() );
          delete mapLayer;
          i--;
        }; // Count == 0
      }; // if records
    }; // layerType == moMapLayer
  }; // for i
  
  // die Dateien der gelöschten Layer löschen
  DeleteShapeFiles( fileNames );
}; // FlushLayers

void CMapDoc::SetProfileNet( WSPVernetzungsInfo* netz )
{
  m_netz = netz; 
}; // SetProfileNetz

void CMapDoc::GetDataBlockTypes( CArray<int, int>& dbArray  )
// gibt zurück, welche Datenblocktypen potentiell in der Karte vorhanden sind
// Rückgabewert:
//          CArray<int, int>*: einfache Liste der vorhandenen DatenblockTypen; NULL bei Fehler
// Bemerkung: das Array muss von der aufrufenden Funktion per delete zerstört werden
{
  CLayerArray* layers = GetLayers();
  if( !layers || !&dbArray)
    return;
  
  // ale Layer durchgehen, und die DatenBlöcke hinzufügen, welche man aus diesen gewinnen kann
  for( int i = 0; i < layers->GetSize(); i++ )
  {
    CLayer* layer = layers->GetAt( i );
    if( !layer )
      continue;
    
    int type = layer->GetType();
    switch( type )
    {
    case CLayer::trennflaechen:
      dbArray.Add( DST_TRENNFLAECHEN );
      break;
      
    case CLayer::durchst_bereiche:
      dbArray.Add( DST_DURCHST_BEREICH );
      break;
      
    case CLayer::modellgrenzen:
      dbArray.Add( DST_MODELLGRENZEN );
      break;
      
    case CLayer::bordvoll:
      dbArray.Add( DST_BORDVOLL );
      break;
      
    case CLayer::buhnen:
      dbArray.Add( DST_BUHNEN );
      break;
      
    case CLayer::profilPoints:
      {
        CMapLayer* profilLayer = (CMapLayer*)layer;
        
        CStringArray fieldNames;
        profilLayer->GetTableNames( fieldNames );
        for( int f = 0; f < fieldNames.GetSize(); f++ )
        {
          CString name = fieldNames[f];
          if( name.CompareNoCase( MO2_FIELD_YKRD ) == 0 )
          {
            dbArray.Add( DST_GELAENDEHOEHE );
            dbArray.Add( DST_RECHTSWERT );
            dbArray.Add( DST_HOCHWERT );
          }; // if MO2_FIELD_YKRD
          if( name.CompareNoCase( MO2_FIELD_RAUHEIT ) == 0 )
            dbArray.Add( DST_RAUHIGKEIT );
          if( name.CompareNoCase( MO2_FIELD_RAUHEITKST ) == 0 )
            dbArray.Add( DST_RAUHIGKEIT_KST );
          if( name.CompareNoCase( MO2_FIELD_AXM ) == 0 )
            dbArray.Add( DST_AXM );
          if( name.CompareNoCase( MO2_FIELD_AYM ) == 0 )
            dbArray.Add( DST_AYM );
          if( name.CompareNoCase( MO2_FIELD_DPM ) == 0 )
            dbArray.Add( DST_DPM );
        }; // for f
      }; // case CLayer::profilPoints
      break;
    }; // switch type
  }; // for i
  
}; // GetDataBlockTypes

void CMapDoc::GetProfileNames( CStringArray& profilNames, CStringArray& stateNames )
// gibt eine Liste der ProfilDateien und eine Liste der zugehörigen Zustandsdateien zurück
// welche zur Zeit in der Karte sind
{
  CLayerArray* layers = GetLayers();
  if( !layers )
    return;
  CMapLayer* profilLayer = layers->FindFirstLayer( CLayer::profilLines );
  if( profilLayer )
  {
    CStringArray fields;
    profilLayer->GetFieldValues( MO2_FIELD_FILE, profilNames );
    profilLayer->GetFieldValues( MO2_FIELD_STATE, stateNames );
  }; // if profilLayer
}; // GetProfileNames


void CMapDoc::EditProfile( const long profilID )
// startet den Graphikeditor von WspWin und editiert das angegebene Profil
{
  if( m_profilAuswahl )
  {
    CString csFile = GetLayers()->GetProfilFile( profilID );
    CString stFile = GetLayers()->GetProfilState( profilID );
    m_profilAuswahl->EditProfile( csFile, stFile );
  }
}; // EditProfile


BOOLEAN CMapDoc::CreateHmoFromWsp( CMapLayer* wspLayer, const CString& hmoName, const CString& shapeName )
{
  BCE::Hmo hmo;

  // Umrandungsinformation auslesen
  CList<long, long> pointsLeft;  // Liste der noch nicht bearbeitete Punkte
  CArray<CMoPoint, CMoPoint&> punkteListe;
  CArray<double,double> pointYListe;
  CUIntArray nextIDs;  // featureID -> featureID
  CTypedPtrArray<CPtrArray, CUIntArray*> pointsByProfil; // nur Dummy, benötigt für GetBorderInfo

  wspLayer->GetBorderInfo( pointsLeft, punkteListe, pointYListe, nextIDs, pointsByProfil );
  
  // die Punkte dem Hmo als Knoten hinzufügen
  CMap<long, long, long, long> knotenIDs; // Ordnet PunkteIDs den KnotenIDs im Hmo zu
  POSITION pos = pointsLeft.GetHeadPosition();
  while( pos )
  {
    long pointID = pointsLeft.GetNext( pos );
    CMoPoint point = punkteListe[pointID];

    knotenIDs[pointID] = hmo.AddKnoten( point.GetX(), point.GetY(), point.GetZ() );
  }; // while pos

  // die Zwangslinien definieren: von jedem Punkt zu seinem Nachfolger
  pos = pointsLeft.GetHeadPosition();
  while( pos )
  {
    long pointID1 = pointsLeft.GetNext( pos );
    long pointID2 = nextIDs[pointID1];
    
    long knotenID1, knotenID2;
    if( knotenIDs.Lookup( pointID1, knotenID1 ) && knotenIDs.Lookup( pointID2, knotenID2 ) )
      hmo.AddKante( knotenID1, knotenID2 );
  }; // while pos


  // zuletzt noch die Profillinien ( d.h. die Verbindung zwischen den beiden äussersten Wasserspiegeln )
  // als Zwangslinien hinzufügen )
  for( int i = 0; i < pointsByProfil.GetSize(); i++ )
  {
    CUIntArray* pointArray = pointsByProfil[i];
    if( pointArray != NULL && pointArray->GetSize() == 2 )
    {
      long pointID1 = pointArray->GetAt( 0 );
      long pointID2 = pointArray->GetAt( 1 );

      long y1 = pointYListe[pointID1];
      long y2 = pointYListe[pointID2];

      if( y1 > y2 )
      {
        long tmp = pointID1;
        pointID1 = pointID2;
        pointID2 = tmp;

        long tmpY = y1;
        y1 = y2;
        y2 = tmp;
      }

      CMoPoint point1 = punkteListe[pointID1];
      double wspHoehe = point1.GetZ();

      long knotenID1, knotenID2;
      if( knotenIDs.Lookup( pointID1, knotenID1 ) && knotenIDs.Lookup( pointID2, knotenID2 ) )
      {

        long lastKnotenID = knotenID1;

        // Misst wir brauchen noch den x-Wert jeden Knotens!

        // das Profil auslesen
        std::auto_ptr<CProfilModel> profilModel( ReadProfil( i ) );

        // Voraussetzung, die Grenzen kommen nach x sortiert an
        CProfilModel::GrenzenIterator gIt = profilModel->GetGrenzenBegin();
		const CProfilModel::Grenze* lastUsedGrenze = 0;
        while( gIt != profilModel->GetGrenzenEnd() )
        {
          const CProfilModel::Grenze& g = *(gIt++);

		  // Immer mit Abständen Vergleichen, sonst fügen wir
		  // Punkte mit 0er Abstand hinzu, das gibt Probleme beim Triangulieren
          if( y1 < g.x + 0.5 && g.x < y2 + 0.5 && ( lastUsedGrenze != 0 && fabs( g.x - lastUsedGrenze->x ) > 0.5 ) )
          {
            try
            {
              CProfilModel::ProfilPoint pt = profilModel->GetInterpolPointAt( g.x );

              long neueKnotenID = hmo.AddKnoten( pt.rw, pt.hw, wspHoehe );
			
			  TRACE( "Füge Knoten hinzu: %ld %ld\n", lastKnotenID, neueKnotenID );
              hmo.AddKante( lastKnotenID, neueKnotenID );

              lastKnotenID = neueKnotenID;
            }
            catch( CProfilModel::InterpolException )
            {
				// ignore this x
				TRACE( "Hier gabs ne Interpolexyception\n" );
            }

			lastUsedGrenze = &g;
          }
        }

        // den letzten mit dem allerletzten verbinden
		TRACE( "Füge Knoten hinzu: %ld %ld\n", lastKnotenID, knotenID2 );
        hmo.AddKante( lastKnotenID, knotenID2 );
      }
    }
  }; // for i

  // die Daten von GetBorderInfo löschen
  wspLayer->GetBorderInfo( pointsLeft, punkteListe, pointYListe, nextIDs, pointsByProfil, TRUE );

    // als Shape schreiben
  if( shapeName.GetLength() > 0 )
  {
	  const CString kantenShapeName = shapeName + "_kanten";
	  hmo.WriteKantenAsLineShape( kantenShapeName, true );
	  
	  CMapLayer* bruchkantenLayer = new CMapLayer( GetMapPath() );
  
	  if( bruchkantenLayer->SetGeoDatasetName( kantenShapeName ) )
	  {
		  AddLayer( bruchkantenLayer, TRUE );
		  bruchkantenLayer->SetType( CLayer::user_RW );
		  SetExtent( bruchkantenLayer->GetExtent() );
	  }
	  else
		  delete bruchkantenLayer;
  }

    // das Hmo Triangulieren
  int error = hmo.Triangulate( 0.0 );
  if( error != 0 )
  {
    CString msg( MAKEINTRESOURCE( IDS_ERROR_HMO_TRIANG ) );
    ::MessageBox( NULL, msg, NULL, MB_OK | MB_ICONERROR );
    return FALSE;
  }; // if error = 1;


  // Wasseroberfläche als HMO und als Shape rausschreiben, shape anzeigen ( nur DEBUG )
  if( hmo.WriteToFile( GetMapPath() + hmoName ) )
  {
    AfxMessageBox( IDS_ERROR_WRITING_TMP_FILE );
    return FALSE;
  };

  // als Shape schreiben
  if( shapeName.GetLength() > 0 )
  {
    hmo.WriteToPolygonShape( shapeName, TRUE );
  
    CMapLayer* wasserTinLayer = new CMapLayer( GetMapPath() );
  
    if( wasserTinLayer->SetGeoDatasetName( shapeName ) )
    {
      AddLayer( wasserTinLayer, TRUE );
      wasserTinLayer->SetType( CLayer::user_RW );
      SetExtent( wasserTinLayer->GetExtent() );
    }
    else
      delete wasserTinLayer;
  }

  return TRUE;
}

void CMapDoc::ShowVolumeDialog( const CString& logFileName )
{
	TRY
	{   
		CString searchStart( "  <message>Total volume of flooded area is:" );
		CString searchEnd( "</message>" );
		CStdioFile file( logFileName, CFile::modeRead );
		CString line;
		while( file.ReadString( line ) )
		{
			int start = line.Find( searchStart );
			int end = line.Find( searchEnd );

			if( start != -1 && end != -1 )
			{
				CString volStr = line.Mid( searchStart.GetLength(), end - searchStart.GetLength() );
				CVolumeDlg dlg( volStr );
				dlg.DoModal();

				break;
			}
		}
	}
	CATCH( CFileException, e )
	{
		TRACE( "File could not be opened " + logFileName + "\n" );
	}
	END_CATCH
}


/** 
	Verschneidet einen Wasserspiegel-Layer mit einem Geländemodell
	
	@param wspLayer der WasserspiegelLayer
	@return Erfolg der Operation
*/
BOOL CMapDoc::TinCutLayer( CMapLayer* wspLayer )
{
  CMainFrame* mainFrame = (CMainFrame*)theApp.GetMainWnd();
  
  // Voraussetzungen prüfen
  if( mainFrame == NULL || !wspLayer || 
      ( wspLayer->GetType() != CLayer::waterLevel && wspLayer->GetType() != CLayer::waterLevelC ) )
    return FALSE;
  
  // temporäres Verzeichnis im 'map' Verzeichnis anlegen (falls nicht schon vorhanden)
  CString mapPfad = GetMapPath();
  CString tempPfad =  "tmp";
  CFileStatus fileStatus;
  if ( CFile::GetStatus( mapPfad + tempPfad, fileStatus ) )
  {
    if ( !( fileStatus.m_attribute & CFile::directory ) || 
      ( fileStatus.m_attribute & CFile::readOnly ) )
    {
      CString message;
      message.FormatMessage( IDS_ERROR_ACCESS_TMP_DIR, mapPfad + tempPfad );
      AfxMessageBox( message );
      return FALSE;
    };
  }
  else if ( !CreateDirectory( mapPfad + tempPfad, NULL ) )
  {
    CString message;
    message.FormatMessage( IDS_ERROR_CREATE_TMP_DIR, mapPfad + tempPfad );
    AfxMessageBox( message );
    return FALSE;
  };
  
  tempPfad += "\\";

  // eine Triangulation der Wasseroberfläche erstellen
  CString wasserFileBase = tempPfad + "triangout";
  CString wasserHmoFile = wasserFileBase + ".hmo";

  CString wasserShapeFile = "";
#if _DEBUG
  wasserShapeFile = BCE::MfcHelper::GetUnusedFileName( mapPfad + wasserFileBase, ".shp" );
#endif

  if( !CreateHmoFromWsp( wspLayer, wasserHmoFile, wasserShapeFile ) )
     return FALSE;

  // jetzt mit Geländemodell verschneiden
  CDgmDlg dlg( GetLayers() );
  if ( dlg.DoModal() == IDOK )
  {
    // warteCursor und alles neu zeichnen lassen
    CWaitCursor wait;
    mainFrame->UpdateWindow();
    
    CString layerPath = BCE::MfcHelper::GetUnusedFileName( 
      BCE::MfcHelper::GetFileWOExt( mapPfad + wspLayer->GetGeoDatasetName() ) + "-tincut", ".shp" );
    layerPath = layerPath.Mid( mapPfad.GetLength() ); // den Kartenpfad wieder löschen

    CString lowerHmo = BCE::MfcHelper::CreateAbsolutePath( GetMapPath(), dlg.GetHmoPath() );
    
	//check, if pfad noch da
	CFileStatus lowerStatus;
	if( !CFile::GetStatus( lowerHmo, lowerStatus ) )
	{
		AfxMessageBox( "Der unterliegende Pfad des Geländemodells ist nicht mehr gültig:\n" + lowerHmo + "\nImportieren Sie das Geländemodell erneut.", MB_ICONEXCLAMATION );
		return FALSE;
	}
	
	CString upperHmo = mapPfad + wasserHmoFile;
    CString shapeBase = mapPfad + layerPath;
    CString logFileName = mapPfad + tempPfad + "tincut.log";


	CString wspmap_exe_name = theApp.GetVersion()->GetFileName();
	CString wspmap_exe_path = BCE::MfcHelper::GetFileDirectory( wspmap_exe_name );

    CString cmdLine = wspmap_exe_path + "\\wspfli.exe -d \"" + lowerHmo + "\" -w \"" + upperHmo + "\" -s \"" + shapeBase + "\" -l \"" + logFileName + "\"";
    
    // nur wenn die Option an ist, die Fliesstiefen aktivieren
	// wird nicht mehr benutzt, option ist immer an -> die globale Option ausbauen
//    if( WSPFeatures::Instance()->isEnabled("MAPPER","map_fliti"))
//      cmdLine += " --fliTi034";

    CExecuteExtern waitForExtern( "Fliesstiefenermittlung", cmdLine, mainFrame );
    waitForExtern.DoModal();

	ShowVolumeDialog( logFileName );

    CMapLayer* mapLayer = new CMapLayer( mapPfad );
    if( mapLayer->SetGeoDatasetName( layerPath + ".shp" ) )
    {
      mapLayer->RenderRamp( "Name" );

      mapLayer->SetType( CLayer::user_RO );
      mapLayer->SetName( wspLayer->GetTag() );

      mapLayer->ApplyRenderer( *GetMap(), TRUE );

      AddLayer( mapLayer, TRUE );

      return TRUE;
    }
    else
    {
      delete mapLayer;
      return FALSE;
    }; // if mapLayer
  } // if dlg.DoModal == IDOK
  else
    return FALSE;
}; // TinCutLayer

void CMapDoc::OnCloseDocument() 
{
  CDocument::OnCloseDocument();
}



/*!
 * Stellt sicher, dass eine Flusslinie existiert. Ist weder ein entsprechender Layer
 * da, noch ien TrackingLayer, wird jetzt eine vom Benutzer digitalisiert.
 *
 * @param none
 *
 * @return int : Was ist passiert:
 *                0: alles ok, eine Flusslinie war schon da
 *                1: alles ok, es wurde gerade eine flusslinie digitalisiert
 *                2: Fehler: keine Flusslinie vorhanden oder sonstiger Fehler
 */
int CMapDoc::CreateRiverLine()
{
  // falls ein FlussLayer da ist, ist eh schon alles ok
  if( GetLayers()->FindFirstLayer( CLayer::flussachse ) )
    return 0;  // nichts wurde digitalisiert, die Linie ist da
  
  CMoMap* pMap = GetMap();
  CMainFrame* pMainFrame = (CMainFrame*)theApp.GetMainWnd();
  CLayerArray* pLayers = GetLayers();
  ASSERT( pMainFrame && pMap && pLayers );
  if( !pMainFrame || !pMap || !pLayers )
    return 2;

  CMoTrackingLayer trackLayer( pMap->GetTrackingLayer() );
  if( !LPDISPATCH(trackLayer) )
    return 2;

  CMoGeoEvent riverEvent( trackLayer.FindEvent( MO2_TRACKTAG_RIVER ) );
  // falls kein FlussTrackEvent da ist, jetzt einen zeichnen lassen
  if( !LPDISPATCH(riverEvent) )
  {
    // falls die Fluss-Hilfslinie noch nicht vorhanden ist, diese ziehen lassen
    pMainFrame->SetStatusBarProgressText( CString(MAKEINTRESOURCE(IDS_TRACK_RIVER)) );
    CMoLine riverLine( pMap->TrackLine() );
    pMainFrame->SetStatusBarProgressText( CString(MAKEINTRESOURCE(AFX_IDS_IDLEMESSAGE)) );
    
    if( LPDISPATCH(riverLine) )
    {
      // erstmal testen, ob sie kein Profil zweimal schneidet
      CMapLayer* profilLines = pLayers->FindFirstLayer( CLayer::profilLines );
      if( profilLines && profilLines->GetValid() )
      {
        CMoRectangle fullExtent( pMap->GetFullExtent() ); // für schnittoperationen
        CMoRecordset records( profilLines->GetRecords() );
        if( LPDISPATCH(fullExtent) && LPDISPATCH(records) )
        {
          CMoFields fields( records.GetFields() );
          if( LPDISPATCH(fields) )
          {
            CMoField shapeField( fields.Item(  CComVariant( MO2_FIELD_SHAPE ) ) );
            if( LPDISPATCH(shapeField) )
            {
              BOOL bDoppelt = FALSE;
              while( !records.GetEof() )
              {
                VARIANT var = shapeField.GetValue();
                if( var.vt == VT_DISPATCH )
                {
                  CMoLine profile( var.pdispVal );
                  var.pdispVal->AddRef();
                  
                  CMoPoints schnitt( riverLine.GetCrossings( profile ) );
                  if( LPDISPATCH(schnitt) && schnitt.GetCount() > 1 )
                  {
                    bDoppelt = TRUE;
                    break;
                  }; // if points.GetCount > 1
                  
                }; // if var.vt
                records.MoveNext();
              }; // while records
              if( bDoppelt )
              {
                AfxMessageBox( IDS_ERROR_RIVER_CUTS, MB_ICONERROR );
                return 2;
              }; 
            }; // if shapeField
          }; // if fields
        }; // if fullExtent && records
      }; // if profileLines
      
      // falls eine Linie erzeugt wurde, diese auf der Karte als TrackingLayer anzeigen
      CMoGeoEvent event( trackLayer.AddEvent( riverLine, MO2_TRACKSYMBOL_RIVER ) );
      if( LPDISPATCH(event) )
        event.SetTag( MO2_TRACKTAG_RIVER ); // als Fluss-Hilfslinie markieren
    }; // if riverLine
    
    return 1; // es wure eine Linie digitalisiert
  } // if riverEvent
  else
    return 0; // es wurde keine Linie digitalisiert, d.h. sie ist schon da
}; // CreateriverLine

void CMapDoc::GenerateProfiles( CMapLayer* pMapLayer )
{
  CLayerArray* pLayers = GetLayers();
  CMainFrame* pMainFrame = (CMainFrame*)theApp.GetMainWnd();

  CProfilAuswahl* pProfilauswahl = GetProfilAuswahl();

  if( !pMainFrame || !pLayers || !pMapLayer || !pProfilauswahl || 
    !pMapLayer->GetValid() || !(pMapLayer->GetShapeType() == moShapeTypeLine) )
    return;
  
  Project* pProject = pProfilauswahl->GetProject();
  if( !pProject )
    return;

  // welches Geländemodell soll benutzt werden?
  CDgmDlg dlg( pLayers, pMainFrame );
  if( dlg.DoModal() != IDOK )
    return;
  
  CMapLayer* hmoLayer = dlg.GetLayer();
  if( !hmoLayer )
    return;

  // den Benutzer fragen, was zu tun ist.
  // TODO: jetzt nur Schmalspur: es wird als Station das Feld 'Station' ausgelesen
  CString stationFieldName = MO2_FIELD_STATION;

  // die Objekte der Reihe nach durchgehen
  CMoRecordset records( pMapLayer->GetRecords() );
  if( !LPDISPATCH(records) )
    return;

  CMoFields fields( records.GetFields() );
  if( !LPDISPATCH(fields) )
    return;

  CMoField shapeField( fields.Item( CComVariant( MO2_FIELD_SHAPE ) ) );
  CMoField stationField( fields.Item( CComVariant( MO2_FIELD_STATION ) ) );
  CMoField textField( fields.Item( CComVariant( MO2_FIELD_TEXT ) ) );
  if( !LPDISPATCH( shapeField ) )
    return;

//  if( !LPDISPATCH(stationField) )
//  {
//    AfxMessageBox( IDS_GENERATE_PROFILES_NO_STATION_ATTRIBUTE,  MB_OK | MB_ICONEXCLAMATION );
//    return;
//  };

  // Abfragen in welchen Zustand importiert werden soll
  StateChooser stateChooser( pProject, pMainFrame );
  if( stateChooser.DoModal() != IDOK )
    return;
  
  State* state = stateChooser.GetCurrentState();
  if( !state )
    return;

  // den State Dialog verstecken
  pMainFrame->UpdateWindow();

  //int lastCutAnswer = IDYES; // für GenerateProfile
  int lastCutAnswer = LPDISPATCH(stationField) ? IDYES : IDCANCEL; // für GenerateProfile
  
  while( !records.GetEof() )
  {
    VARIANT dispVar = shapeField.GetValue();
    if( dispVar.vt == VT_DISPATCH )
    {
      CMoLine line( dispVar.pdispVal );

      CString text;
      if( LPDISPATCH(textField) )
      {
        VARIANT textVar = textField.GetValue();
        if( textVar.vt == VT_BSTR )
          text = textVar.bstrVal;
      };

      // falls ein Stationsfeld da ist dieses nehmen, sonst 0 übergeben
      // -> der Benutzer wird gefragt
      double dummyStation = 0.0;
      double* station = 0;

      if( LPDISPATCH(stationField) )
      {
        dummyStation = COleVariantEx( stationField.GetValue() );
        station = &dummyStation;
      }

      LPCTSTR strText = 0;
      if( !text.IsEmpty() )
        strText = LPCTSTR(text);
      
      // try-catch: falls ein Abbruch kommt
      try { GenerateProfile( line, hmoLayer, false, &lastCutAnswer, state, station, strText ); }
      catch( cancel_action ) { return; }; // einfach abbrechen
    }; // if dispVar.vt

    records.MoveNext();
  }; // while records

  // eine Erfolgsmeldung ausgeben
  AfxMessageBox( IDS_GENERATE_PROFILES_SUCCESS, MB_OK | MB_ICONINFORMATION );

  // jetzt erst das Projekt abspeichern
  //GetProfilAuswahl()->GetProject()->Save();

  return;
}


/*!
 * Erstellt eine Kopie eines Themas
 * und fügt diesem zur Karte hinzu.
 *
 * @param pLayer : der zu kopierende Layer
 */
void CMapDoc::CopyLayer( CLayer* pLayer )
{
  CMainFrame *pMainWnd = (CMainFrame*)theApp.m_pMainWnd;

  ASSERT( pLayer && pMainWnd );
  if( !pLayer || !pMainWnd )
    return;

  CLayerData* layerData = GetLayerData( pLayer->GetType() );
  ASSERT( layerData );
  
  CString oldFileName = pLayer->GetGeoDatasetName();
  CString oldFileExt = BCE::MfcHelper::GetFileExt( oldFileName );
  CString newFileName = GetMapPath() + BCE::MfcHelper::GetFileTitle( oldFileName ) + "-" + CString(MAKEINTRESOURCE(IDS_COPY));
  newFileName = BCE::MfcHelper::GetUnusedFileName( newFileName, "." + oldFileExt ) +  "." + oldFileExt;
  if ( newFileName.Find( GetMapPath() ) == 0 )
    newFileName = newFileName.Mid( GetMapPath().GetLength() );

  CLayer* newLayer = NULL;
  switch ( pLayer->GetLayerType() )
  {
  case moImageLayer:
    newLayer = ((CImageLayer*)pLayer)->Copy( newFileName );
    break;
    
  case moMapLayer:
    {
      CMapLayer::LayerType newType = layerData->kopierterTyp;
      // falls das ZielThema benutzerdefiniert ist, fragen, ob die Kopie einen bestimmten
      // Typ haben soll.
      CMapLayer* oldLayer = (CMapLayer*)pLayer;
      if( layerData->kopierterTyp == CMapLayer::user_RW )
      {
        CItemChooser<CLayer::LayerType>::TypeMap typeMap;
        typeMap.SetAt( CString(MAKEINTRESOURCE(IDS_RIVER_AXIS)), CLayer::flussachse );
        typeMap.SetAt( CString(MAKEINTRESOURCE(IDS_USER_RW)), CLayer::user_RW );

        CItemChooser<CLayer::LayerType> dlg( typeMap, CString(MAKEINTRESOURCE(IDS_COPY_LAYER_CHOOSER_TITLE)), CString(MAKEINTRESOURCE(IDS_COPY_LAYER_CHOOSER_TEXT)), pMainWnd );
        if( dlg.DoModal() != IDOK )
          return;
        newType = dlg.GetSelectedItem();

        // Testen, ob der Layer die spezifikation des neuen Typs erfüllt
        if( oldLayer->TestLayerType( newType ) == false )
        {
          AfxMessageBox( IDS_TEST_LAYER_TYPE_FAILED, MB_OK | MB_ICONEXCLAMATION );
          return;
        }; // if TestLayerType
      }; // if kopierterTyp == user_RW

      // testen, ob der gewünschte Typ mehrfach vorkommen darf
      CLayerArray* pLayers = GetLayers();
      if( pLayers && newType == CMapLayer::flussachse && pLayers->FindFirstLayer( CMapLayer::flussachse ) != 0 )
      {
        AfxMessageBox( IDS_COPY_LAYER_UNIQUE_AXIS, MB_OK | MB_ICONEXCLAMATION );
        return;
      };

      CMapLayer* mapLayer = oldLayer->Copy( newFileName );
      if( mapLayer )
      { // Layer initialisieren
        mapLayer->SetName( pLayer->GetName() + TEXT(" (") + CString(MAKEINTRESOURCE(IDS_COPY)) + TEXT(")") );
        if ( layerData->bKopiereTag )
          mapLayer->SetTag( pLayer->GetTag() );
        else
          mapLayer->SetTag( TEXT("") );
        
        CMoSymbol symbol( ((CMapLayer*)pLayer)->GetSymbol() );
        mapLayer->SetSymbol( symbol );
        mapLayer->SetType( newType );

        newLayer = mapLayer;
      };
    };
    break;      
  };
  
  if ( newLayer )
    AddLayer( newLayer, TRUE );
  else
    AfxMessageBox( IDS_LAYER_COPY_FAILED, MB_OK | MB_ICONEXCLAMATION );
}; // CopyLayer


CProfilModel* CMapDoc::GetActiveProfilModel()
{
  return ReadProfil( m_activeProfileID );
}

/*!
 * Liest die GIS Daten für ein Profil aus aus und erzeugt daraus ein ProfilModel
 * Gibt ein neues Objekt zurück
 *
 */
CProfilModel* CMapDoc::ReadProfil( const int profilID )
{
  if( profilID == 0 || profilID == -1 )
    return 0;

  CString sqlExpr;  

  CLayerArray* layers = GetLayers();
  if( !layers )
    return 0;
  
  CMapLayer* profilLineLayer = layers->FindFirstLayer( CLayer::profilLines );
  CMapLayer* profilLayer = layers->FindFirstLayer( CLayer::profilPoints );
  if( !profilLineLayer || !profilLayer )
    return 0;

  // erstmal rausfinden, ob es eine Mehrfeldbrücke ist, falls ja entsprechend handeln
  int mfbNr = -1;
  COleVariantEx var = profilLineLayer->GetFieldValByID( profilID, MO2_FIELD_MFB );
  if( var.vt != VT_ERROR )
    mfbNr = var;

  CUIntArray profilIDs; // Liste der darzustellenden Profile in der richtigen Reihenfolge
  CUIntArray pkS; // entsprechende Liste der PKs

  // falls es eine Mehrfeldbrücke ist, die Daten der Linien holen
  if( mfbNr > -1 )
  {
    CString searchString;
    searchString.Format( "%s = %d", MO2_FIELD_MFB, mfbNr );
    CMoRecordset lineRecords( profilLineLayer->SearchExpression( searchString ) );
    if( !LPDISPATCH(lineRecords) )
      return 0;
    CMoFields lineFields( lineRecords.GetFields() );
    if( !LPDISPATCH(lineFields) )
      return 0;
    CMoField idField( lineFields.Item( CComVariant(MO2_FIELD_FEATUREID) ) );
    CMoField pkField( lineFields.Item( CComVariant(MO2_FIELD_PK) ) );

    while( !lineRecords.GetEof() )
    {
      const int id = COleVariantEx( idField.GetValue() );
      const CString pk( COleVariantEx( pkField.GetValue() ) );
      UINT pkNum; // zum sortieren der pks

      switch( pk[0] )
      {
      case 'L':
        pkNum = 100;
        break;

      case 'F':
        pkNum = 200;
        break;

      case 'R':
        pkNum = 300;
        break;

      default:
        pkNum = 0;
      }; // switch pk[0]

      if( pk.GetLength() > 2 )
        pkNum += atoi( pk.Mid( 2 ) );

      // jetzt die ProfilId in die Liste an der richtigen Stelle einsortieren
      int pos = -1;
      for( int i = 0; i < profilIDs.GetSize(); i++ )
      {
        if( pkNum < pkS[i] )
        {
          pos = i;
          break;
        }; // if profilIDs[i] < pkNum
      }; // for i
      if( pos == -1 )
      {
        profilIDs.Add( id );
        pkS.Add( pkNum );
      }
      else
      {
        profilIDs.InsertAt( pos, id );
        pkS.InsertAt( pos, pkNum );
      }; // if pos == -1

      lineRecords.MoveNext();
    }; // while lineRecords
  }
  else
  {
    profilIDs.Add( profilID );
    pkS.Add( 0 );
  }; // if mfbNr == -1

  CProfilModel* model = new CProfilModel;

  for( int pid = 0; pid < profilIDs.GetSize(); pid++ )
  {
    CMap<unsigned long, unsigned long, CProfilModel::ProfilPoint, CProfilModel::ProfilPoint&> profilPoints;
    unsigned long minId = 4000000000;
    unsigned long maxId = 0;

    int pID = profilIDs[pid]; // die aktuell zu lesende ID
    sqlExpr.Format( "%s = %d", MO2_FIELD_PROFILID, pID );
  
    CMoRecordset profilRecords(profilLayer->SearchExpression(sqlExpr));
    if( !LPDISPATCH(profilRecords) )
      continue;

    CMoFields profilFields(profilRecords.GetFields());
    if( !LPDISPATCH(profilFields) )
      continue;

    CMoField profilShapeField(profilFields.Item(COleVariant(MO2_FIELD_SHAPE)));
    CMoField profilHeightField(profilFields.Item(COleVariant(MO2_FIELD_HEIGHT)));
    CMoField profilYField(profilFields.Item(COleVariant(MO2_FIELD_YKRD)));
    CMoField profilNumberField(profilFields.Item(COleVariant(MO2_FIELD_NUMBER)));
    CMoField profilIDField(profilFields.Item(COleVariant(MO2_FIELD_FEATUREID)));
    if( !LPDISPATCH(profilShapeField) || !LPDISPATCH(profilHeightField) || !LPDISPATCH(profilYField) ||
        !LPDISPATCH(profilIDField) )
      continue;

    while( !profilRecords.GetEof() )
    {
      long number;
      if( LPDISPATCH(profilNumberField) ) // aus Kombatibilitätsgründen auch noch Sortierung nach der FeatureID zulassen 
        number = COleVariantEx( profilNumberField.GetValue() );
      else
        number = COleVariantEx( profilIDField.GetValue() );
      const double height = COleVariantEx( profilHeightField.GetValue() );
      const double ykrd = COleVariantEx( profilYField.GetValue() );
      
      CMoPoint profilPoint( profilShapeField.GetValue().pdispVal );

      double rw = profilPoint.GetX();
      double hw = profilPoint.GetY();

      if( number != -1 )
      {
        minId = min( minId, number );
        maxId = max( maxId, number );
        profilPoints[number] = CProfilModel::ProfilPoint( ykrd, height, rw, hw );
      }; // if number != -1
        
      profilRecords.MoveNext();
    }; // while profilRecords

    // profilPunkte in der Reihenfolde der id nach m_profilPoints schreiben  
    for( unsigned long i = minId; i <= maxId; i++ )
    {
      CProfilModel::ProfilPoint point;
      if( profilPoints.Lookup( i, point ) )
      {
        model->AddPoint( point );

        // falls mehr als ein Profil gleichzeitig angezeigt wird ( Mehrfeldbrücke ), noch eine Trennlinie einfügen
        if( i == minId && pid != 0 )
           model->AddMfb( point.x );
      }; // if Lookup
    }; // for i

    // weitere Profilbezogene Daten auslesen
    ReadAsTrenntype( layers->FindFirstLayer( CLayer::durchst_bereiche ), sqlExpr, model, CProfilModel::db );
    ReadAsTrenntype( layers->FindFirstLayer( CLayer::trennflaechen ), sqlExpr, model, CProfilModel::tf );
    ReadAsTrenntype( layers->FindFirstLayer( CLayer::bordvoll ), sqlExpr, model, CProfilModel::bv );
    
    // wasserstände auslesen
    POSITION wspPos = NULL;
    CMapLayer* wspLayer = layers->FindFirstLayer( CLayer::waterLevel, &wspPos );
    while( wspLayer )
    {
      CMoRecordset wspRecords(wspLayer->SearchExpression(sqlExpr));
      CMoFields wspFields(wspRecords.GetFields());
      CMoField yField(wspFields.Item(COleVariant(MO2_FIELD_YKRD)));
      CMoField heightField(wspFields.Item(COleVariant(MO2_FIELD_HEIGHT)));

      CMoSymbol wspSymbol( wspLayer->GetSymbol() );

      while( !wspRecords.GetEof() )
      {
        const double yKrd1 = COleVariantEx( yField.GetValue() );
        const double height1 = COleVariantEx( heightField.GetValue() );
        
        wspRecords.MoveNext();
        if ( !wspRecords.GetEof() )
        {
          const double yKrd2 = COleVariantEx( yField.GetValue() );
          const double height2 = COleVariantEx( heightField.GetValue() );

          CProfilModel::WSP wsp;
          wsp.color = wspSymbol.GetColor();
          wsp.entry = CProfilModel::WspEntry( CDoublePoint( yKrd1, height1 ), CDoublePoint( yKrd2, height2 ) );

          model->AddWsp( wsp );

          wspRecords.MoveNext();
        }; // if !wspRecords.GetEof
      }; // while wspRecords.MoveNext

      wspLayer = layers->FindNextLayer( CLayer::waterLevel, &wspPos );
    }; // while wspLayer
  }; // for pid

  // Station etc. auslesen
  model->SetStation( COleVariantEx( profilLineLayer->GetFieldValByID( profilID, MO2_FIELD_STATION ) ) );

  return model;
}

void CMapDoc::ReadAsTrenntype( CMapLayer* layer, const CString& sqlExpr, CProfilModel* model, const int type )
{
  if( layer )
  {
    CMoRecordset records( layer->SearchExpression( sqlExpr ) );
    CMoFields fields( records.GetFields() );
    CMoField yField( fields.Item( COleVariant( MO2_FIELD_YKRD ) ) );
    while( !records.GetEof() )
    {
      const double yKrd = COleVariantEx( yField.GetValue() );
      model->AddGrenze( yKrd, (CProfilModel::Type)type );
      records.MoveNext();
    };
      
    CMoSymbol symbol( layer->GetSymbol() );
    model->SetTypeColor( (CProfilModel::Type)type, symbol.GetColor() );
  };
}

void CMapDoc::SetActiveProfile( const long featureID, const bool bRefreshViews )
{
  const long oldID = m_activeProfileID;

  if( !m_layers.FindFirstLayer( CLayer::profilLines ) )
    m_activeProfileID = 0;
  else
    m_activeProfileID = featureID;

  if( bRefreshViews && m_activeProfileID != oldID )
  {
    m_profilCursorPos = -99999.99; // damit der Profilcurosr nach Profilwechsel erst mal ausgeblendet ist
    FireMapDocChanged( IMapDocListener::ACTIVE_PROFILE_CHANGED, 0 );
  }
};

void CMapDoc::SetProfilCursor( const double cursorPos )
{
  m_profilCursorPos = cursorPos;

  FireMapDocChanged( IMapDocListener::PROFILE_CURSOR_CHANGED, 0 );
}

void CMapDoc::SetFullExtent()
{
  CMoRectangle fullExtent( GetLayers()->GetExtent() );
  if( LPDISPATCH(fullExtent) )
    SetExtent( fullExtent );
};


// Publisher Pattern
void CMapDoc::AddMapDocListener( IMapDocListener* listener )
{
  m_listeners.insert( listener );
}

void CMapDoc::RemoveMapDocListener( IMapDocListener* listener )
{
  m_listeners.erase( listener );
}

void CMapDoc::FireMapDocChanged( const long type, CLayer* layer )
{
  for( ListenerSet::const_iterator lIt = m_listeners.begin(); lIt != m_listeners.end(); lIt++ )
    (*lIt)->MapDocChanged( type, layer );

  // Trick: speichert nach ändern der Karte
  if( type & ( IMapDocListener::THEME_ADDED | IMapDocListener::THEME_REMOVED | CMapView::THEME_PROPS_CHANGED ) )
    SaveModified();
}

void CMapDoc::RecalcProfileDistances( CMapLayer* lineLayer )
{
	// erst mal den Layer der Profillinien holen
	CMapLayer* profilLayer = GetLayers()->FindFirstLayer( CLayer::profilLines );
	CLayerArray* layers = GetLayers();
	if( !profilLayer || !layers )
	{
		// TOOD: Fehlermeldung
		return;
	}

	// Auswahl des Strangs aus allen geladenen Strängen
	CProfilAuswahl* pa = GetProfilAuswahl();
	CStateProfilesMap stateProfilesMap( pa->GetProfilInfo() );

	CMainFrame* pMainFrame = (CMainFrame*)theApp.GetMainWnd();
	State* state = stateProfilesMap.ChooseStrang( pMainFrame );
	if( !state )
		return;

	CMoRectangle mapExtent( GetMap()->GetExtent() );

	CMapObjectArray profilLineArray( true );
	profilLayer->GetObjects( profilLineArray, 0 );
	CMapObjectIndex profilLineIndex( MO2_FIELD_FILE, profilLineArray );

	CString mapPfad = GetMapPath();
	CString logFile = mapPfad + "/tmp/profilabstände.log";
	std::ofstream logger( logFile );

	CProfileDistancer distancer( logger );

	SimpleCache<long, CProfilModel> modelMap( true );

	CString progressTitle( "Profilabstände werden ermittelt..." );
	CProgressCtrl* progressCtrl = pMainFrame->CreateStatusBarProgress( progressTitle );
	if( progressCtrl )
		progressCtrl->SetRange(0, state->GetNumConnections() );

	Connection* conn = state->GetFirstConnection();
	while( conn )
	{
		CString anfProfileName = conn->GetAnfProf();
		CString endProfileName = conn->GetEndProf();

		// finde entsprechende geladene Profile
			// profillinien holen
		CMapObject* anfObject = profilLineIndex.GetAt( CComVariant( anfProfileName ) );
		CMapObject* endObject = profilLineIndex.GetAt( CComVariant( endProfileName ) );

		if( !anfObject || !endObject )
		{
			TRACE( "Strang: " + anfProfileName + " - " + endProfileName + "  ist nicht in der Karte enthalten\n" );

			conn = state->GetNextConnection();

			if( progressCtrl )
				progressCtrl->StepIt();
			
			continue;
		}

		long anfID = anfObject->GetAt( MO2_FIELD_FEATUREID ).lVal;
		long endID = endObject->GetAt( MO2_FIELD_FEATUREID ).lVal;

		CProfilModel* anfModel = modelMap.Get( anfID );
		if( !anfModel )
			anfModel = modelMap.Add( anfID, ReadProfil( anfID ) );
		CProfilModel* endModel = modelMap.Get( endID );
		if( !endModel )
			endModel = modelMap.Add( endID, ReadProfil( endID ) );


		// finde alle objekte, die beide profillinien schneiden
		CComVariant anfShape = anfObject->GetAt( MO2_FIELD_SHAPE );

		CMoRecordset crossLines = lineLayer->SearchShape( anfShape.pdispVal, moLineCross, "" );
		CMoFields crossFields = crossLines.GetFields();
		CMoField crossShapeField = crossFields.Item( CComVariant( MO2_FIELD_SHAPE ) );
		CMoField crossIdField = crossFields.Item( CComVariant( MO2_FIELD_FEATUREID ) );
		while( !crossLines.GetEof() )
		{
			CComVariant crossShape = crossShapeField.GetValue();
			const long crossID = crossIdField.GetValue().lVal;

			distancer.AddDistances( conn, crossID, crossShape.pdispVal, anfObject, endObject, anfModel, endModel, layers );

			crossLines.MoveNext();
		}

		conn = state->GetNextConnection();

		if( progressCtrl )
			progressCtrl->StepIt();
	}

	pMainFrame->DestroyStatusBarProgress();

	logger.close();


	const CProfileDistancer::ConnMap& connMap = distancer.GetConnMap();
	CStrangDistanceDlg dlg( state, connMap, logFile, pMainFrame );
	if( dlg.DoModal() == IDOK )
	{
		// Messagebox zur Sicherheit?
		if( AfxMessageBox( "Strangdatei wird überschrieben.\nSchließen Sie das WspWin Basisprogramm, bevor Sie mit OK bestätigen.", MB_OKCANCEL ) == IDOK )
		{
			dlg.ApplyChanges();
			state->Save();
		}
	};
}
