#pragma warning(disable:4786)
#pragma warning(disable:4503)

#include "stdafx.h"

#include "bce\include\WSPFeatures.h"
#include "bce/include/hmo.h"
#include "commonMfc/include/mfcHelper.h"
#include "commonMfc/include/variant_helper.h"

#include "maphelper.h"

#include "mainfrm.h"
#include "childfrm.h"
#include "obqrydlg.h"
#include "newldlg.h"
#include "layerdata.h"
#include "verschneidDlg.h"

#include "wspmap.h"
#include "mapdoc.h"
#include "mapproj.h"
#include "mapprops.h"
#include "imlayer.h"
#include "mappreview.h"
#include "mapLayout.h"

#include "imapcontroller.h"
#include "multimapcontroller.h"
#include "groupcontroller.h"
#include "nilController.h"
#include "FlipProfileController.h"
#include "selectProfilController.h"
#include "extendProfileController.h"
#include "panController.h"
#include "zoomController.h"
#include "newObjectController.h"
#include "moveObjectController.h"
#include "generateProfileController.h"
#include "queryObjectController.h"
#include "deleteObjectController.h"
#include "clipObjectController.h"
#include "movePointController.h"
#include "deletePointController.h"
#include "insertPointController.h"
#include "zoomOutController.h"
#include "followProfileControler.h"
#include "showCoordsController.h"
#include "followCursorControler.h"

#include "mapview.h"

extern VARIANT nullVariant;
extern CWSPMapApp theApp;

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CMapView
//
// MapController.cpp: Implementierung der Klasse CMapController.
//
//////////////////////////////////////////////////////////////////////

IMPLEMENT_DYNCREATE(CMapView, CFormView)

CMapView::CMapView() : CFormView(CMapView::IDD)
{
	//{{AFX_DATA_INIT(CMapView)
	//}}AFX_DATA_INIT

  m_legend = NULL;
  m_profilEditor = NULL;

  m_toolController = std::auto_ptr<CGroupController>( new CGroupController( this ) );
  m_mapController = std::auto_ptr<CMultiMapController>( new CMultiMapController( this ) );

  m_toolController->AddController( ID_TOOLS_NONE, IMapController_ptr( new CNilController( this ) ) );
  m_toolController->AddController( ID_TOOLS_EDIT_PROFIL, IMapController_ptr( new CSelectProfilController( this ) ) );
  m_toolController->AddController( ID_TOOLS_EXTEND_PROFILE, IMapController_ptr( new CExtendProfileController( this ) ) );
  m_toolController->AddController( ID_TOOLS_FLIP_PROFILE, IMapController_ptr( new CFlipProfileController( this ) ) );
  m_toolController->AddController( ID_TOOLS_ZOOMIN, IMapController_ptr( new CZoomController( this ) ) );
  m_toolController->AddController( ID_OBJECT_NEW, IMapController_ptr( new CNewObjectController( this ) ) );
  m_toolController->AddController( ID_OBJECT_MOVE, IMapController_ptr( new CMoveObjectController( this ) ) );
  m_toolController->AddController( ID_TOOLS_GENERATE_PROFILE, IMapController_ptr( new CGenerateProfileController( this ) ) );
  m_toolController->AddController( ID_OBJECT_QUERY, IMapController_ptr( new CQueryObjectController( this ) ) );
  m_toolController->AddController( ID_OBJECT_DELETE, IMapController_ptr( new CDeleteObjectController( this ) ) );
  m_toolController->AddController( ID_OBJECT_CLIP, IMapController_ptr( new CClipObjectController( this ) ) );
  
  m_toolController->AddController( ID_POINT_MOVE, IMapController_ptr( new CMovePointController( this ) ) );
  m_toolController->AddController( ID_POINT_DELETE, IMapController_ptr( new CDeletePointController( this ) ) );
  m_toolController->AddController( ID_POINT_INSERT, IMapController_ptr( new CInsertPointController( this ) ) );

  m_mapController->AddController( 0, IMapController_ptr( new CZoomOutController( this ) ) );
  m_mapController->AddController( ID_TOOLS_FOLLOW_PROFILE, IMapController_ptr( new CFollowProfileControler( this ) ) );
  m_mapController->AddController( ID_TOOLS_SHOW_CURSOR, IMapController_ptr( new CFollowCursorControler( this ) ) );
  m_mapController->AddController( 1, IMapController_ptr( new CShowCoordsController( this ) ) );
  m_mapController->AddController( 2, IMapController_ptr( new CPanController( this, 2, 0 ) ) );
}

CMapView::~CMapView() {};

void CMapView::DoDataExchange(CDataExchange* pDX)
{
  CFormView::DoDataExchange(pDX);
  //{{AFX_DATA_MAP(CMapView)
  DDX_Control(pDX, IDC_MAP1, m_map);
  //}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CMapView, CFormView)
//{{AFX_MSG_MAP(CMapView)
ON_WM_SIZE()
ON_COMMAND(ID_FILE_PRINT_PREVIEW, OnFilePrintPreview)
ON_WM_CREATE()
//}}AFX_MSG_MAP

ON_WM_MOUSEWHEEL()

ON_UPDATE_COMMAND_UI(ID_INDICATOR_XCOORD, OnUpdateIndicator)
ON_UPDATE_COMMAND_UI(ID_INDICATOR_YCOORD, OnUpdateIndicator)
ON_COMMAND(ID_FILE_PRINT, OnFilePrint)
ON_COMMAND(ID_FILE_PRINT_DIRECT, OnFilePrint)
ON_COMMAND(ID_FILE_PRINT_SETUP, OnFilePrintSetup)
ON_UPDATE_COMMAND_UI(ID_FILE_PRINT, OnUpdateSingle)
ON_UPDATE_COMMAND_UI(ID_FILE_PRINT_DIRECT, OnUpdateSingle)

// Scalebar-Menu Kommandos
ON_COMMAND_EX(ID_VIEW_MAPUNITS_DEGREES, OnViewScalebarUnits)
ON_COMMAND_EX(ID_VIEW_MAPUNITS_FEET, OnViewScalebarUnits)
ON_COMMAND_EX(ID_VIEW_MAPUNITS_METERS, OnViewScalebarUnits)
ON_COMMAND_EX(ID_VIEW_SCALEUNITS_MILES, OnViewScalebarUnits)
ON_COMMAND_EX(ID_VIEW_SCALEUNITS_FEET, OnViewScalebarUnits)
ON_COMMAND_EX(ID_VIEW_SCALEUNITS_METERS, OnViewScalebarUnits)
ON_COMMAND_EX(ID_VIEW_SCALEUNITS_KM, OnViewScalebarUnits)
ON_COMMAND_EX(ID_VIEW_SCREENUNITS_CM, OnViewScalebarUnits)
ON_COMMAND_EX(ID_VIEW_SCREENUNITS_INCHES, OnViewScalebarUnits)

ON_UPDATE_COMMAND_UI(ID_VIEW_MAPUNITS_DEGREES, OnUpdateViewUnits)
ON_UPDATE_COMMAND_UI(ID_VIEW_MAPUNITS_FEET, OnUpdateViewUnits)
ON_UPDATE_COMMAND_UI(ID_VIEW_MAPUNITS_METERS, OnUpdateViewUnits)
ON_UPDATE_COMMAND_UI(ID_VIEW_SCALEUNITS_FEET, OnUpdateViewUnits)
ON_UPDATE_COMMAND_UI(ID_VIEW_SCALEUNITS_KM, OnUpdateViewUnits)
ON_UPDATE_COMMAND_UI(ID_VIEW_SCALEUNITS_METERS, OnUpdateViewUnits)
ON_UPDATE_COMMAND_UI(ID_VIEW_SCALEUNITS_MILES, OnUpdateViewUnits)
ON_UPDATE_COMMAND_UI(ID_VIEW_SCREENUNITS_CM, OnUpdateViewUnits)
ON_UPDATE_COMMAND_UI(ID_VIEW_SCREENUNITS_INCHES, OnUpdateViewUnits)

// Thema - Menü kommandos
ON_COMMAND_EX( ID_LAYER_ZOOM, OnLayerCommand )
ON_COMMAND_EX( ID_LAYER_MOVETOTOP, OnLayerCommand )
ON_COMMAND_EX( ID_LAYER_MOVEFORWARD, OnLayerCommand )
ON_COMMAND_EX( ID_LAYER_MOVEBACK, OnLayerCommand )
ON_COMMAND_EX( ID_LAYER_MOVETOBACK, OnLayerCommand )
ON_COMMAND_EX( ID_LAYER_OVERVIEW, OnLayerCommand )
ON_COMMAND_EX( ID_LAYER_PROPERTIES, OnLayerCommand )
ON_COMMAND_EX( ID_LAYER_MOVE_OUTWARDS, OnLayerCommand )
ON_COMMAND_EX( ID_LAYER_TINCUT, OnLayerCommand )
ON_COMMAND_EX( ID_EXTRAS_PROFILEDISTANCE, OnLayerCommand )
ON_COMMAND( ID_LAYER_INSERT, OnLayerInsert )
ON_COMMAND( ID_LAYER_REMOVE, OnLayerRemove )
ON_COMMAND( ID_LAYER_HMO_IMPORT, OnLayerHmoImport )
ON_COMMAND( ID_LAYER_NEW, OnLayerNew )
ON_COMMAND( ID_LAYER_SHAPE_TO_PROFILE, OnLayerVerschneid )
ON_COMMAND_EX( ID_LAYER_GENERATE_PROFILES, OnLayerCommand )
ON_COMMAND_EX( ID_LAYER_COPY, OnLayerCommand )
ON_COMMAND_EX( ID_LAYER_PROPERTIES_IMPORT, OnLayerCommand )
ON_COMMAND_EX( ID_LAYER_PROPERTIES_EXPORT, OnLayerCommand )

ON_UPDATE_COMMAND_UI( ID_LAYER_HMO_IMPORT, OnUpdateLayer )
ON_UPDATE_COMMAND_UI( ID_LAYER_ZOOM, OnUpdateLayer )
ON_UPDATE_COMMAND_UI( ID_LAYER_MOVETOTOP, OnUpdateLayer )
ON_UPDATE_COMMAND_UI( ID_LAYER_MOVEFORWARD, OnUpdateLayer )
ON_UPDATE_COMMAND_UI( ID_LAYER_MOVEBACK, OnUpdateLayer )
ON_UPDATE_COMMAND_UI( ID_LAYER_MOVETOBACK, OnUpdateLayer )
ON_UPDATE_COMMAND_UI( ID_LAYER_OVERVIEW, OnUpdateLayer )
ON_UPDATE_COMMAND_UI( ID_LAYER_INSERT, OnUpdateLayer )
ON_UPDATE_COMMAND_UI( ID_LAYER_REMOVE, OnUpdateLayer )
ON_UPDATE_COMMAND_UI( ID_LAYER_PROPERTIES, OnUpdateLayer )
ON_UPDATE_COMMAND_UI( ID_LAYER_MOVE_OUTWARDS, OnUpdateLayer )
ON_UPDATE_COMMAND_UI( ID_LAYER_NEW, OnUpdateLayer )
ON_UPDATE_COMMAND_UI( ID_LAYER_TINCUT, OnUpdateLayer )
ON_UPDATE_COMMAND_UI( ID_LAYER_SHAPE_TO_PROFILE, OnUpdateLayer )
ON_UPDATE_COMMAND_UI( ID_LAYER_GENERATE_PROFILES, OnUpdateLayer )
ON_UPDATE_COMMAND_UI( ID_LAYER_COPY, OnUpdateLayer)
ON_UPDATE_COMMAND_UI( ID_LAYER_PROPERTIES_EXPORT, OnUpdateLayer)
ON_UPDATE_COMMAND_UI( ID_LAYER_PROPERTIES_IMPORT, OnUpdateLayer)
ON_UPDATE_COMMAND_UI( ID_EXTRAS_PROFILEDISTANCE, OnUpdateLayer)

// Menu 'Karte'
ON_COMMAND( ID_MAP_CLIPBOARD, OnMapClipboard )
ON_COMMAND( ID_MAP_SAVE_AS, OnMapSaveAs )
ON_UPDATE_COMMAND_UI( ID_MAP_CLIPBOARD, OnUpdateMap )
ON_UPDATE_COMMAND_UI( ID_MAP_SAVE_AS, OnUpdateMap )

// Kommandos aus den Menues 'Geoobjekte' und 'Tools'
ON_COMMAND_EX(ID_OBJECT_CLIP, OnToolsCommand )
ON_COMMAND_EX(ID_OBJECT_NEW, OnToolsCommand )
ON_COMMAND_EX(ID_OBJECT_QUERY, OnToolsCommand )
ON_COMMAND_EX(ID_OBJECT_MOVE, OnToolsCommand )
ON_COMMAND_EX(ID_OBJECT_DELETE, OnToolsCommand )
ON_COMMAND_EX(ID_POINT_INSERT, OnToolsCommand )
ON_COMMAND_EX(ID_POINT_DELETE, OnToolsCommand )
ON_COMMAND_EX(ID_POINT_MOVE, OnToolsCommand )
ON_COMMAND_EX(ID_TOOLS_ZOOMIN, OnToolsCommand )
ON_COMMAND_EX(ID_TOOLS_PAN, OnToolsCommand )
ON_COMMAND_EX(ID_TOOLS_NONE, OnToolsCommand )
ON_COMMAND_EX(ID_TOOLS_EDIT_PROFIL, OnToolsCommand )
ON_COMMAND_EX(ID_TOOLS_GENERATE_PROFILE, OnToolsCommand )
ON_COMMAND_EX(ID_TOOLS_EXTEND_PROFILE, OnToolsCommand )
ON_COMMAND_EX(ID_TOOLS_FLIP_PROFILE, OnToolsCommand )

ON_UPDATE_COMMAND_UI(ID_OBJECT_CLIP, OnUpdateTools)
ON_UPDATE_COMMAND_UI(ID_OBJECT_NEW, OnUpdateTools)
ON_UPDATE_COMMAND_UI(ID_OBJECT_QUERY, OnUpdateTools)
ON_UPDATE_COMMAND_UI(ID_OBJECT_MOVE, OnUpdateTools)
ON_UPDATE_COMMAND_UI(ID_OBJECT_DELETE, OnUpdateTools)
ON_UPDATE_COMMAND_UI(ID_POINT_INSERT, OnUpdateTools)
ON_UPDATE_COMMAND_UI(ID_POINT_DELETE, OnUpdateTools)
ON_UPDATE_COMMAND_UI(ID_POINT_MOVE, OnUpdateTools)
ON_UPDATE_COMMAND_UI(ID_TOOLS_ZOOMIN, OnUpdateTools)
ON_UPDATE_COMMAND_UI(ID_TOOLS_PAN, OnUpdateTools)
ON_UPDATE_COMMAND_UI(ID_TOOLS_NONE, OnUpdateTools)
ON_UPDATE_COMMAND_UI(ID_TOOLS_EDIT_PROFIL, OnUpdateTools)
ON_UPDATE_COMMAND_UI(ID_TOOLS_GENERATE_PROFILE, OnUpdateTools)
ON_UPDATE_COMMAND_UI(ID_TOOLS_EXTEND_PROFILE, OnUpdateTools)
ON_UPDATE_COMMAND_UI(ID_TOOLS_FLIP_PROFILE, OnUpdateTools)

// Kommandos, welche eine einzelne aktion ausführen, unabhängig davon, ob es
// ein aktives Thema gibt oder nicht
ON_COMMAND_EX( ID_TOOLS_FULL_EXTENT, OnSingleCommand )
ON_COMMAND_EX( ID_TOOLS_FOLLOW_PROFILE, OnSingleCommand )
ON_COMMAND_EX( ID_TOOLS_SHOW_CURSOR, OnSingleCommand )
ON_COMMAND_EX( ID_TOOLS_MARK_PROFILE, OnSingleCommand )
ON_COMMAND_EX( ID_ESCAPE, OnSingleCommand )

ON_UPDATE_COMMAND_UI( ID_TOOLS_FOLLOW_PROFILE, OnUpdateSingle )
ON_UPDATE_COMMAND_UI( ID_TOOLS_SHOW_CURSOR, OnUpdateSingle )
ON_UPDATE_COMMAND_UI( ID_TOOLS_MARK_PROFILE, OnUpdateSingle )

END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CMapView 


BOOL CMapView::OnMouseWheel( UINT nFlags, short zDelta, CPoint pt )
{
  const double faktor = zDelta < 0 ? 0.7 : 1.3;

  
  CView::ScreenToClient( &pt );

  CMoPoint cursor( GetDocument()->GetMap()->ToMapPoint( pt.x, pt.y ) );

  Zoom( faktor, cursor );

  return TRUE;
}

BOOL CMapView::OnViewScalebarUnits( UINT nID )
{
  CMapDoc *pDoc = GetDocument();

  switch ( nID )
  {
  case ID_VIEW_MAPUNITS_DEGREES:
    pDoc->SetMapUnit(muDecimalDegrees);
    break;

  case ID_VIEW_MAPUNITS_FEET:
    pDoc->SetMapUnit(muFeet);    
    break;

  case ID_VIEW_MAPUNITS_METERS:
    pDoc->SetMapUnit(muMeters);
    break;

  case ID_VIEW_SCALEUNITS_MILES:
    pDoc->SetScaleUnit(suMiles);
    break;

  case ID_VIEW_SCALEUNITS_FEET:
    pDoc->SetScaleUnit(suFeet);
    break;

  case ID_VIEW_SCALEUNITS_METERS:
    pDoc->SetScaleUnit(suMeters);
    break;

  case ID_VIEW_SCALEUNITS_KM:
    pDoc->SetScaleUnit(suKM);    
    break;

  case ID_VIEW_SCREENUNITS_CM:
    pDoc->SetScreenUnit(suCentimeters);
    break;

  case ID_VIEW_SCREENUNITS_INCHES:
    pDoc->SetScreenUnit(suInches);
    break;

  default:
    return FALSE; // unbekannte ID -> Kommando weiterverarbeiten
  }; // switch
  
  GetDocument()->FireMapDocChanged( CMapView::SCALE_CHANGED, NULL );
  return TRUE;
}; // OnViewScalebarUnits

void CMapView::OnUpdateViewUnits(CCmdUI* pCmdUI)
// Handler für CommandUI Nachrichten im Masstabs-Popup-Menu
{
  CMapDoc *pDoc = GetDocument();
  BOOL bCheck = FALSE;

  
  switch( pCmdUI->m_nID )
  {
		case ID_VIEW_MAPUNITS_DEGREES:
      if (pDoc->GetMapUnit()==muDecimalDegrees)
        bCheck = TRUE;
      break;
      
    case ID_VIEW_MAPUNITS_FEET:
      if (pDoc->GetMapUnit()==muFeet)
        bCheck = TRUE;
      break;
      
    case ID_VIEW_MAPUNITS_METERS:
      if (pDoc->GetMapUnit()==muMeters)
        bCheck = TRUE;
      break;
      
    case ID_VIEW_SCALEUNITS_MILES:
      if (pDoc->GetScaleUnit()==suMiles)
        bCheck = TRUE;
      break;
      
    case ID_VIEW_SCALEUNITS_FEET:
      if (pDoc->GetScaleUnit()==suFeet)
        bCheck = TRUE;
      break;
      
    case ID_VIEW_SCALEUNITS_METERS:
      if (pDoc->GetScaleUnit()==suMeters)
        bCheck = TRUE;
      break;
      
    case ID_VIEW_SCALEUNITS_KM:
      if (pDoc->GetScaleUnit()==suKM)
        bCheck = TRUE;
      break;
      
    case ID_VIEW_SCREENUNITS_CM:
      if (pDoc->GetScreenUnit()==suCentimeters)
        bCheck = TRUE;
      break;
      
    case ID_VIEW_SCREENUNITS_INCHES:
      if (pDoc->GetScreenUnit()==suInches)
        bCheck = TRUE;
      break;
  }
  
  pCmdUI->SetCheck(bCheck);
}; // OnUpdateViewUnits

void CMapView::OnLayerVerschneid()
// verschneidet ein Thema mit den Profillinien oder Profilpunkten
// und erzeugt neue Daten
{
  CMapDoc* pDoc = GetDocument();
  CLayer* activeLayer = pDoc->GetActiveLayer();
  if( !activeLayer || activeLayer->GetLayerType() != moMapLayer )
    return;

  CMapLayer* mapLayer = (CMapLayer*)activeLayer;
  
  // zuerst rausfinden, was genau der Benutzer machen will
  CVerschneidDlg dlg( theApp.GetNutzklassDir(), mapLayer, this );

  dlg.AddEntry( CString(MAKEINTRESOURCE(IDS_USER_DEFINED)), CLayer::user_RW );
  CMapProperties* pMapProps = pDoc->GetMapProperties();
  for( int i = 0; i < pMapProps->GetLayerData()->GetSize(); i++ )
  {
    CLayerData* layerData = pMapProps->GetLayerData()->GetAt( i );
    if ( layerData->bObjectGeometryEditable && layerData->nProfilBezug >= 0 && 
         !layerData->layerName.IsEmpty() )
      dlg.AddEntry( layerData->layerName, static_cast<CLayer::LayerType>( i ) );
  }; // for i

  if ( dlg.DoModal() != IDOK )
    return;

  // jetzt die Aktion ausführen
  if( dlg.GetAction() )
    pDoc->CutThemeToProfilLines( mapLayer, dlg.GetLayerType(), dlg.GetFeature(), dlg.GetConstraint(),
                                  dlg.GetDeleteTheme(), dlg.GetReli() );
  else
    pDoc->CutThemeToProfilPoints( mapLayer, dlg.GetFeature(), dlg.GetZTable() );
}; // OnLayerVerschneid

void CMapView::OnLayerHmoImport() 
{ 
	if (!WSPFeatures::Instance()->isEnabled("MAPPER","map_nodemo"))
	{
		std::string TheText = std::string(WSPFeatures::Instance()->GetDataStr("HEAD", "DEMO_INFO"));
		TheText.append("\n\n");
		TheText.append(WSPFeatures::Instance()->GetDataStr("MAPPER", "map_nodemo"));
		AfxMessageBox(TheText.c_str() ,MB_ICONINFORMATION,0);
		return;
	}
  CMapDoc* pDoc = GetDocument();
     
  CString filter;
  filter.LoadString( IDS_HMO_IMPORT_FILTER );

  CFileDialog dlg( TRUE, NULL, NULL, OFN_FILEMUSTEXIST | OFN_HIDEREADONLY, (LPCTSTR)filter, this );
  if( dlg.DoModal() == IDOK )
  {
    BOOL originalDatei = FALSE;  // ob die original Datei benutzt werden soll
    CString inputFileName = dlg.GetPathName();
    CString inputFileExt = BCE::MfcHelper::GetFileExt( inputFileName );
    inputFileExt.MakeUpper();
    if( inputFileExt == "HMO" )
      originalDatei = TRUE;
    // Eingabedatei einlesen
    CWaitCursor wait;  // das Einlesen + shape-schreiben kann länger dauern
    BCE::Hmo hmo;

    int error = hmo.ReadFromFile( inputFileName );
    if( error )
    {
      AfxMessageBox( IDS_ERROR_READ_DTM );
      return;
    };
  
    // Falls keine Dreiecke vorhanden, fragen ob trianguliert werden soll
    if( hmo.GetDreieckCount() == 0 )
    {
      originalDatei = FALSE; // muss auf jeden Fall als neue HMO rausgeschrieben werden

      int error;
      switch ( AfxMessageBox(IDS_NO_TRIANGLES, MB_OKCANCEL ) )
      {
      case IDOK:
        error = hmo.Triangulate();
        break;
      
      default:
        return;
      };

      if (error)
      {
        CString message;
        message.LoadString( IDS_TRIANGULATON_ERROR );
        AfxFormattedMessageBox( message, error );
        return;
      };
    };

    // Daten als HMO rausschreiben (falls nicht schon vorher so gewesen)
    CString hmoFileName;
    if( !originalDatei )
    {
      hmoFileName = 
        BCE::MfcHelper::GetUnusedFileName(pDoc->GetMapPath() + BCE::MfcHelper::GetFileTitle(inputFileName), ".hmo") + ".hmo";
      int error = hmo.WriteToFile( hmoFileName );
      if( error > 0 )
      {
        AfxMessageBox( "Fehler beim Schreiben der HMO - Datei" );
        return;
      };
    }
    else
      hmoFileName = inputFileName;
    
    // HMO Datei als Shape rausschreiben und anzeigen
    CString shapeFileName = BCE::MfcHelper::GetUnusedFileName( pDoc->GetMapPath() + BCE::MfcHelper::GetFileTitle(inputFileName), ".shp" );

    error = hmo.WriteToPolygonShape( shapeFileName, TRUE );
    if( error )
    {
      CString str;
      str.Format( "Error writing Shape-File: %d", error );
      AfxMessageBox( str );
      return;
    };

    CMapLayer* layer = new CMapLayer( pDoc->GetMapPath() );
    if( layer->SetGeoDatasetName( shapeFileName + ".shp" ) )
    {
      layer->SetType( CLayer::hmo );
      layer->SetName( BCE::MfcHelper::GetFileName( inputFileName ) );
      layer->SetTag( BCE::MfcHelper::GetPathAsRelative( pDoc->GetMapPath(), hmoFileName ) );

      pDoc->AddLayer( layer, TRUE );
    }
    else
    {
      delete layer;
      AfxMessageBox("Error adding hmo-layer");
    };
  };
}; // OnLayerHmoImport()

void CMapView::OnLayerInsert() 
{
  CMapDoc* pDoc = (CMapDoc*)GetDocument();

  CString resString;
  CString fileFilter;

  fileFilter += "ESRI Shape (.shp)|*.shp|";
  
  fileFilter += "ESRI Coverages|aat.adf;pat.adf;nat.adf;txt.adf;*.tat;*.pat;*.rat|";
  
  fileFilter += "ESRI GRID|hdr.adf|";
  
  resString.LoadString( IDS_CAD_DRAWINGS );
  fileFilter += resString + " (.dwg;.dxf)|*.dwg;*.dxf|";

  fileFilter += "VPF (.pft;.lft;.aft;.tft)|*.pft;*.lft;*.aft;*.tft|";

  resString.LoadString( IDS_ALL_VECTOR_FORMATS );
  fileFilter += resString + "|*.shp;aat.adf;pat.adf;nat.adf;txt.adf;*.tat;*.pat;*.rat;*.pft;*.lft;*.aft;*.tft;*.dwg;*.dxf|";

  resString.LoadString( IDS_ALL_IMAGE_FORMATS );
  CString imageExts = "|*.bmp;*.dib;*.tif;*.jpg;*.jff;*.bil;*.bip;*.bsq;*.gis;*.lan;*.rlc;*.sid;*.sun;*.rs;*.ras;*svf;*.img;*.gif;*.ovr;*.ntf|"; // wird später nochmal benutzt
  fileFilter += resString + imageExts;

  resString.LoadString( IDS_ALL_FILES );
  fileFilter += resString + " (*.*)|*.*|";

  fileFilter += '|';
 
  CFileDialog dlg(TRUE, NULL, NULL, 
    OFN_ALLOWMULTISELECT | OFN_FILEMUSTEXIST | OFN_HIDEREADONLY, fileFilter, NULL);
  
  // standardmässig den Filter auf 'alle Dateien' setzen
  dlg.m_ofn.nFilterIndex = 8;

  // den Dateibuffer für ::GetOpenFileName vergrössern, damit man mehr
  // als nur ca. 12 Dateien selektieren kann
  const UINT nMaxFile = 5 * MAX_PATH;
  LPTSTR lpstrFile = (LPTSTR)malloc( nMaxFile * sizeof( TCHAR ) );
  lpstrFile[0] = '\0';
  dlg.m_ofn.lpstrFile = lpstrFile;
  dlg.m_ofn.nMaxFile = nMaxFile;
  
  if (dlg.DoModal() == IDOK)
	{
		POSITION pos = dlg.GetStartPosition();

    BOOL zugefuegt = FALSE;
    CMoRectangle fullExtent;  // voller Extent aller geladener Layer
    while(pos)
    {
      CString fileFullPath = dlg.GetNextPathName(pos);
      fileFullPath.MakeUpper();
      CString fileExt = BCE::MfcHelper::GetFileExt(fileFullPath);
      CString fileTitle = BCE::MfcHelper::GetFileTitle(fileFullPath);
      CString filePath = BCE::MfcHelper::GetFileDirectory(fileFullPath);
      CString fileBase = BCE::MfcHelper::GetFileDirectory(filePath);
      CString fileBaseTitle = BCE::MfcHelper::GetFileTitle(filePath);

      BOOL vector = FALSE;

      CStringArray layerPaths;
      // je nach Endung entscheiden, was passieren soll
      if (fileExt == "SHP")
      {
        layerPaths.Add(fileFullPath);
        vector = TRUE;
      }
      else if (fileExt == "PAT" || fileExt == "RAT" || fileExt == "TAT")
      {
        layerPaths.Add("[arc]" + fileBase + "\\" + fileBaseTitle + "." + fileExt + fileTitle);
        vector = TRUE;
      }
      else if (fileExt == "ADF")
      {
        if (fileTitle == "HDR")
        {
          layerPaths.Add(fileFullPath);
        }
        else if (fileTitle == "AAT" || fileTitle == "PAT" || fileTitle == "NAT" ||
                 fileTitle == "TXT")
        {
          layerPaths.Add("[arc]" + fileBase + "\\" + fileBaseTitle + "." + fileTitle);
          vector = TRUE;
        };
      }
      else if (fileExt == "DWG" || fileExt == "DXF")
      {
        layerPaths.Add("[CADArea]" + fileFullPath);
        layerPaths.Add("[CADLine]" + fileFullPath);
        layerPaths.Add("[CADPoint]" + fileFullPath);
        layerPaths.Add("[CADText]" + fileFullPath);
        vector = TRUE;
      }
      else if (fileExt == "AFT" || fileExt == "LFT" || fileExt == "PFT" || fileExt == "TFT")
      {
        layerPaths.Add("[vpf]" + fileFullPath);
        vector = TRUE;
      }
      else // ist es ein Rasterformat?
      {
        imageExts.MakeUpper();
        if ( imageExts.Find( fileExt ) != -1 )
          layerPaths.Add( fileFullPath );
      };

      for( int i = 0; i < layerPaths.GetSize(); i++ )
      {
        if( vector )
        {
          CMapLayer* mapLayer = new CMapLayer( pDoc->GetMapPath() );
          if( mapLayer->SetGeoDatasetName( layerPaths[i] ) )
          {
            CMoRecordset records( mapLayer->GetRecords() );
            if( records.GetCount() == 0 )
              delete mapLayer;
            else
            {
				// TODO: immer RW setzen besser? was ist mit coverages etc.?
              //mapLayer->SetType( CLayer::user_RW );
			  mapLayer->SetType( CLayer::user_RO );
              mapLayer->SetVisible( FALSE );
              
              pDoc->AddLayer( mapLayer, TRUE );
            };
          }
          else
          {
            CString message;
            message.FormatMessage(IDS_LOAD_LAYER_ERROR, layerPaths[i]);
            AfxMessageBox(message);
            delete mapLayer;
          };
        }
        else
        {
          CImageLayer *imLayer = new CImageLayer( pDoc->GetMapPath() );
          if (imLayer->SetGeoDatasetName(layerPaths[i]))
          {
            imLayer->SetVisible( FALSE );
            pDoc->AddLayer( imLayer, TRUE );
          }
          else
          {
            CString message;
            message.FormatMessage(IDS_LOAD_LAYER_ERROR, layerPaths[i]);
            AfxMessageBox(message);
            delete imLayer;
          };
        }; // if vector
      }; // for i

    }; // while pos
	} // dlg.DoModal()
  else
  {
    if ( ::CommDlgExtendedError() == FNERR_BUFFERTOOSMALL )
      AfxMessageBox( IDS_TOO_MANY_FILES_SELECTED );
  };

  free( lpstrFile );
	return;
}; // OnLayerInsert

void CMapView::OnLayerRemove()
// löscht ein Thema aus der Karte
// falls es ein Shape ist und im Kartenverzeichnis liegt wird noch gefragt, ob die Dateien
// gelöscht werden sollen
{
  CMapDoc* pDoc = (CMapDoc*)GetDocument();
  CLayer *layer = pDoc->GetActiveLayer();
  if (layer)
  {
    // erst mal schauen, ob die Dateien potentiell gelöscht werden könnnen
    CString fileName = layer->GetGeoDatasetName();
    fileName.MakeUpper();
    
    int pointIndex = fileName.Find( ".SHP" );
    fileName = pDoc->GetMapPath() + fileName;

    BOOL bDelete = FALSE;

    // jetzt abfragen, ob gelöscht werden soll
    if ( layer->GetType() != CLayer::user_RO && pointIndex != -1 && 
         AfxMessageBox( IDS_DELETE_SHAPES, MB_YESNO ) == IDYES )
      bDelete = TRUE;

    pDoc->SetActiveLayer( NULL );

    pDoc->RemoveLayer( layer, TRUE );
    delete layer;

    if ( bDelete )
    {
      CStringArray fileNames;
      fileNames.Add( fileName );
      DeleteShapeFiles( fileNames );
    };

  };
}; // OnLayerRemove

void CMapView::OnLayerNew() 
{
  CMapDoc* pDoc = (CMapDoc*)GetDocument();
  CMapLayer *pLayer;

  CString file;
  int nType, i = 1;
  CFileStatus rStatus;
  CMainFrame *pMainWnd = (CMainFrame*)theApp.m_pMainWnd;
  
  CNewLayerDialog dlg(pMainWnd);

  CString hStr( MAKEINTRESOURCE(IDS_THEME1) );
  hStr = pDoc->GetTitle() + hStr;

  file.Format("%s%d%s", hStr, i++, ".shp");
  while (CFile::GetStatus( pDoc->GetMapPath() + file, rStatus))
    file.Format("%s%d%s", hStr, i++, ".shp");
  dlg.m_file = pDoc->GetMapPath() + file;

  hStr.LoadString( IDS_THEME2 );
  dlg.m_name.FormatMessage( hStr, i - 1 );
		
  if (dlg.DoModal()==IDOK)
  {
    switch (dlg.m_type)
    {
    case 0:
      nType = moPoint;
      break;
      
    case 1:
      nType = moLine;
      break;
      
    case 2:
      nType = moPolygon;
      break;
      
    default:
      ASSERT(FALSE);
      break;
    }
    
    // Define the geodataset
    CMoTableDesc tableDesc;
    VERIFY(tableDesc.CreateDispatch(TEXT("MapObjects2.TableDesc")));
    
    // set the field names, types, and lengths
    tableDesc.SetFieldCount(dlg.m_fieldCount);
    
    for (i=0; i<dlg.m_fieldCount; i++)
    {
      tableDesc.SetFieldName(i, dlg.GetFieldName(i));
      tableDesc.SetFieldType(i, dlg.GetFieldType(i));
      switch (dlg.GetFieldType(i))
      {
      case moLong:
        tableDesc.SetFieldPrecision(i, 15);
        tableDesc.SetFieldScale(i, 0);	// decimal places
        break;
        
      case moDouble:
        tableDesc.SetFieldPrecision(i, 15);
        tableDesc.SetFieldScale(i, 4);	// decimal places
        break;
        
      case moDate:
        tableDesc.SetFieldPrecision(i, 15);
        tableDesc.SetFieldScale(i, 0);	// decimal places
        break;
        
      case moString:
        tableDesc.SetFieldLength(i, 12);
        break;
        
      case moBoolean:
        tableDesc.SetFieldPrecision(i, 1);
        tableDesc.SetFieldScale(i, 0);	// decimal places
        break;
      }
    }
    
    pLayer = new CMapLayer( pDoc->GetMapPath() );
    // falls die Datei im Map - Verzeichnis erzeugt wird, einen relativen Pfad erzeugen
    CString newPath = dlg.m_file;
    if ( newPath.Find( pDoc->GetMapPath() ) == 0 )
      newPath = newPath.Mid( pDoc->GetMapPath().GetLength() );
    pLayer->SetGeoDatasetName( newPath, nType, tableDesc );
    pLayer->SetName(dlg.m_name);
    pLayer->SetType(CLayer::user_RW);
    
    CMoSymbol symbol(pLayer->GetSymbol());
    symbol.SetColor(moDarkGreen);
    switch (nType)
    {
    case moPoint:
      symbol.SetStyle(moCircleMarker);
      break;
      
    case moLine:
      symbol.SetStyle(moSolidLine);
      break;
      
    case moPolygon:
      symbol.SetStyle(moLightGrayFill);
      symbol.SetOutlineColor(moBlack);
      break;
    }
    
    pDoc->AddLayer( pLayer, TRUE );

    pLayer->BuildIndex( TRUE );
  }
}; // OnLayerNew

BOOL CMapView::OnLayerCommand( UINT nID )
{
  CMapDoc* pDoc = (CMapDoc*)GetDocument();
  CMainFrame* pMainFrame = (CMainFrame*)AfxGetApp()->GetMainWnd();
  if( !pDoc || !pMainFrame )
    return TRUE;

  CLayer* layer = pDoc->GetActiveLayer();

  // vor allen Commandos erstmal den MainFramen wieder schön machen
  pMainFrame->UpdateWindow();

  IMapDocListener::EventType fMapUpdate;
  switch ( nID )
  {
  case ID_LAYER_ZOOM:
    if ( layer )
      pDoc->SetExtent( layer->GetExtent() );
    break;

  case ID_LAYER_MOVETOTOP:
    m_map.GetLayers().MoveToTop( GetActiveLayerIndex() );
    fMapUpdate = THEME_ORDER_CHANGED;
    break;

  case ID_LAYER_MOVEFORWARD:
    m_map.GetLayers().MoveTo( GetActiveLayerIndex(), GetActiveLayerIndex() - 1 );
    fMapUpdate = THEME_ORDER_CHANGED;
    break;

  case ID_LAYER_MOVEBACK:
    m_map.GetLayers().MoveTo( GetActiveLayerIndex(), GetActiveLayerIndex() + 1 );
    fMapUpdate = THEME_ORDER_CHANGED;
    break;

  case ID_LAYER_MOVETOBACK:
    m_map.GetLayers().MoveToBottom( GetActiveLayerIndex() );
    fMapUpdate = THEME_ORDER_CHANGED;
    break;

  case ID_LAYER_OVERVIEW:
    pDoc->SetOverview( GetDocument()->GetActiveLayer()->GetDispatch() );
    break;

  case ID_LAYER_PROPERTIES:
    if ( layer )
      pDoc->EditLayerProperties( layer );
    break;

  case ID_LAYER_PROPERTIES_EXPORT:
    {
      // nach Datei fragen
      CString filter( MAKEINTRESOURCE( IDS_LAYER_FILEFILTER ) );
      CString fileName( BCE::MfcHelper::GetFileTitle( layer->GetGeoDatasetName() ) + ".wth" );
      CFileDialog fd( FALSE, TEXT( "wth" ), fileName, OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT, filter, this );
      if( fd.DoModal() == IDOK )
        layer->SaveProperties( fd.GetPathName() );
    }
    break;

  case ID_LAYER_PROPERTIES_IMPORT:
    {
      // nach Datei fragen
      CString filter( MAKEINTRESOURCE( IDS_LAYER_FILEFILTER ) );
      CFileDialog fd( TRUE, TEXT( "wth" ), NULL, OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT, filter, this );
      if( fd.DoModal() == IDOK )
      {
        layer->LoadProperties( fd.GetPathName(), *pDoc->GetMap() );
        GetDocument()->FireMapDocChanged( IMapDocListener::THEME_PROPS_CHANGED, layer );
      }
    }
    break;

  case ID_LAYER_MOVE_OUTWARDS:
    if ( layer && layer->GetLayerType() == moMapLayer )
      pDoc->MoveObjectsOutwards( (CMapLayer*)layer );
    break;

  case ID_LAYER_TINCUT:
    // überprüfen, ob alle Voraussetzungen stimmen, dann Verschneiden
    if( layer->GetLayerType() == moMapLayer && 
        ( layer->GetType() == CLayer::waterLevel || layer->GetType() == CLayer::waterLevelC ) )
    {
      CMapLayer* mapLayer = (CMapLayer*)layer;
      if( pDoc->TinCutLayer( mapLayer ) )
        fMapUpdate = THEME_ADDED;
    }; // if layertType = moMapLayer
    break;

  case ID_LAYER_GENERATE_PROFILES:
    if( layer->GetLayerType() ==  moMapLayer )
      pDoc->GenerateProfiles( (CMapLayer*)layer );
    break;

  case ID_LAYER_COPY:
    pDoc->CopyLayer( layer );
    break;

  case ID_EXTRAS_PROFILEDISTANCE:
	  if( layer->GetLayerType() == moMapLayer )
		  pDoc->RecalcProfileDistances( (CMapLayer*)layer );
	  break;

  default:  // Kommando wurde nicht bearbeitet, FALSE zurückgeben
    return FALSE;
  };

  if ( fMapUpdate != 0L )
  {
    pDoc->FireMapDocChanged( fMapUpdate, layer );
    pDoc->SetModifiedFlag( TRUE );
  };

  return TRUE;
}; // OnLayerCommand

void CMapView::OnUpdateLayer( CCmdUI* pCmdUI )
// Handler für CommandUI Nachrichten des Menüs 'Themen'
{
  CMapDoc* pDoc = (CMapDoc*)GetDocument();
  CLayer* layer = pDoc->GetActiveLayer();
  CLayerData* layerData = NULL;
  long layerType = 0;
  long shapeType = 0;

  if ( layer )
  {
    layerData = pDoc->GetLayerData( layer->GetType() );
    layerType = layer->GetLayerType();
    if ( layerType == moMapLayer )
      shapeType = ((CMapLayer*)layer)->GetShapeType();
  };

  BOOL fEnable = FALSE;

  switch( pCmdUI->m_nID )
  {
  case ID_LAYER_HMO_IMPORT:
      fEnable = TRUE;
    break;

  case ID_LAYER_ZOOM:
  case ID_LAYER_REMOVE:
  case ID_LAYER_PROPERTIES:
  case ID_LAYER_PROPERTIES_EXPORT:
  case ID_LAYER_PROPERTIES_IMPORT:
    fEnable = layer != NULL;
    break;

  case ID_LAYER_MOVETOTOP:
  case ID_LAYER_MOVEFORWARD:
    fEnable = GetActiveLayerIndex() > 0;
    break;

  case ID_LAYER_MOVEBACK:
  case ID_LAYER_MOVETOBACK:
    {
      CMoLayers layers(m_map.GetLayers());
      fEnable = ( GetActiveLayerIndex() != -1  && 
                  GetActiveLayerIndex() < layers.GetCount() - 1 );
    };
    break;

  case ID_LAYER_OVERVIEW:
    fEnable = layer != 0;
    break;

  case ID_LAYER_TINCUT:
    {
      CLayer* hmoLayer = pDoc->GetLayers()->FindFirstLayer( CLayer::hmo );
      if ( hmoLayer && layerData && layerData->bTinCut )
        fEnable = TRUE;
    };
    break;

  case ID_LAYER_NEW:
  case ID_LAYER_INSERT:
    fEnable = TRUE;
    break;

  case ID_LAYER_COPY:
    if ( ( layerData && layerData->bKopierbar ) || 
      ( layer &&  ( layer->GetType() == CLayer::waterLevel || layer->GetType() == CLayer::waterLevelC ) ) )
      fEnable = TRUE;
    break;

  case ID_LAYER_SHAPE_TO_PROFILE:
    fEnable = layerData && shapeType != moShapeTypePoint && layerData->bCutToProfileLines;
    break;

  case ID_LAYER_MOVE_OUTWARDS:
    fEnable = layerData && layerData->nProfilBezug != -1 && layerData->bObjectGeometryEditable && layerData->bTinCut;
    break;

  case ID_LAYER_GENERATE_PROFILES:
    fEnable =   layerType == moMapLayer && shapeType == moShapeTypeLine;
    break;

  case ID_EXTRAS_PROFILEDISTANCE:
	  fEnable = layerType == moMapLayer && shapeType == moShapeTypeLine;
	  break;

  default:
    ASSERT( FALSE );
  }; // switch nID

  pCmdUI->Enable( fEnable );
}; // OnUpdateLayer 


BOOL CMapView::OnToolsCommand( UINT nID )
// Handler für Kommandos aus dem Menue GeoObjekte
{
  m_toolController->Toggle( nID );
  m_mapController->Toggle( nID );

  return TRUE;
}; // OnToolsCommand

void CMapView::OnUpdateTools( CCmdUI* pCmdUI )
// UI-Update Handler für  Commandos aus dem Menü Geoobjekte und Tools
{
  BOOL bEnable = m_toolController->IsEnabled( pCmdUI->m_nID );
  BOOL bCheck = m_toolController->IsChecked( pCmdUI->m_nID );

  if( bCheck && !bEnable )
     m_toolController->Toggle( ID_TOOLS_NONE );
  
  pCmdUI->Enable( bEnable );
  pCmdUI->SetCheck( bCheck );
}; // OnUpdateTools

// sonstige einzelne Kommandos
BOOL CMapView::OnSingleCommand( UINT nID )
{
  CMapDoc* pDoc = GetDocument();
  CMainFrame* pMainFrame = (CMainFrame*)theApp.GetMainWnd();
  if( !pDoc || !pMainFrame )
    return TRUE;

  switch( nID )
  {
  case ID_TOOLS_MARK_PROFILE:
    pDoc->SetShowActiveProfile( !pDoc->IsShowActiveProfile() );
    pDoc->FireMapDocChanged( ACTIVE_PROFILE_CHANGED, NULL );
    break;

  case ID_TOOLS_FULL_EXTENT:
    pDoc->SetFullExtent();
    break;

  case ID_ESCAPE:
    m_mapController->Cancel();
    m_toolController->Cancel();
    break;

  default:
    m_mapController->Toggle( nID );
  }; // switch nID

  return TRUE; // Commando erfolgreich behandelt
}; // OnSingleCommand

void CMapView::OnUpdateSingle( CCmdUI* pCmdUI )
{
  BOOL bEnable = FALSE;
  BOOL bCheck = FALSE;

  CMapDoc* pDoc = (CMapDoc*)GetDocument();

  switch( pCmdUI->m_nID )
  {
  case ID_TOOLS_MARK_PROFILE:
    bEnable = TRUE;
    bCheck = pDoc->IsShowActiveProfile() && bEnable;
    break;

  case ID_FILE_PRINT:
  case ID_FILE_PRINT_DIRECT:
      bEnable = TRUE;
    break;

  default:
    bCheck = m_mapController->IsChecked( pCmdUI->m_nID );
    bEnable = m_mapController->IsEnabled( pCmdUI->m_nID );
  }; // switch m_nID

  pCmdUI->Enable( bEnable );
  pCmdUI->SetCheck( bCheck );
}; // OnUpdateSingle

void CMapView::OnUpdateMap( CCmdUI* pCmdUI )
{
  BOOL bEnable = FALSE;
  switch( pCmdUI->m_nID )
  {
  case ID_MAP_CLIPBOARD:
  case ID_MAP_SAVE_AS:
      bEnable = TRUE;
    break;
  }; // m_nID

  pCmdUI->Enable( bEnable );
}; // OnUpdateMap

/////////////////////////////////////////////////////////////////////////////
// Diagnose CMapView

#ifdef _DEBUG
void CMapView::AssertValid() const
{
	CFormView::AssertValid();
}

void CMapView::Dump(CDumpContext& dc) const
{
	CFormView::Dump(dc);
}
#endif //_DEBUG


void CMapView::OnInitialUpdate() 
{
	CMapDoc* pDoc = GetDocument();

	ASSERT_VALID( this );
  ASSERT_VALID( pDoc );

	if (!UpdateData(FALSE))
		TRACE0("UpdateData failed during formview initial update.\n");

  ASSERT( m_map.GetSafeHwnd() );

  CChildFrame* parentWnd = (CChildFrame*)GetParent();
  
  pDoc->SetProfilAuswahl( &(parentWnd->m_wndProfilAuswahl) );

	for( int i = 0; i < pDoc->GetLayerCount(); i++ )
  {
    CLayer* layer = pDoc->GetLayers()->GetAt( i );
    
    m_map.GetLayers().Add( layer->GetDispatch() );
    
    if( layer->GetLayerType() == moMapLayer )
      ((CMapLayer*)layer)->ApplyRenderer( m_map, TRUE );
  };

  // Verlinkung der Views
  m_profilEditor->SetMapDoc( pDoc );
  pDoc->AddMapDocListener( this );
  parentWnd->m_wndScaleBar.SetMapDoc( pDoc );
  m_legend->SetMapDoc( pDoc );
  

  // Symbole für TrackLayers initialisieren
  CMoTrackingLayer trackLayer(m_map.GetTrackingLayer());
  trackLayer.SetSymbolCount( MO2_TRACKSYMBOL_COUNT );
  // das Symbol MO2_TRACKSYMBOL_MOVEOBJECT wird jedesmal neu gesetzt, also nicht vorinitialisiert
  // MO2_TRACKSYMBOL_RAND: dito
  // gepunktete schwarze Linie
  CMoSymbol moveSymbol(trackLayer.GetSymbol( MO2_TRACKSYMBOL_MOVEPOINT ));
  moveSymbol.SetSymbolType( moLineSymbol );
  moveSymbol.SetStyle( moDotLine );
  moveSymbol.SetColor( moBlack );
  moveSymbol.SetSize( 1 );
  CMoSymbol genSymbol(  trackLayer.GetSymbol( MO2_TRACKSYMBOL_GENERATEPROFILE ) );
  genSymbol.SetSymbolType( moLineSymbol );
  CMoSymbol riverSymbol( trackLayer.GetSymbol( MO2_TRACKSYMBOL_RIVER ) );
  riverSymbol.SetSymbolType( moLineSymbol );
  riverSymbol.SetStyle( moDashDotLine );
  riverSymbol.SetColor( moBlue );
  riverSymbol.SetSize( 1 );

	//
	// This is needed for MDI apps.
	//
	CRect client;
	GetClientRect(&client);
	if (m_map.m_hWnd)
		m_map.SetWindowPos(0, 0, 0, client.Width() , client.Height() , SWP_NOZORDER);

  // Grösse etc. der Dockbars und des Frames nach den gespeicherten Einstellungen setzen
  CMapProperties* mapProps = pDoc->GetMapProperties();
  // es kann nur der Zustand ( maximiert, minimiert ) und die Grösse des Kartenfenster
  // wiederhergestellt werden, da das Umrechnen von ScreenToClient nicht richtig funktioniert
  CRect rect( mapProps->GetWindowRect() );
  parentWnd->SetWindowPos( NULL, rect.left, rect.top, rect.Width(), rect.Height(), 
                           SWP_NOZORDER | SWP_SHOWWINDOW );
  parentWnd->ShowWindow( mapProps->GetWindowState() );
  
  CDockState* dockState = mapProps->GetDockState();
  parentWnd->SetDockState( *dockState );

  // jetzt noch die Grössen der Fenster einzeln setzen
  for ( i = 0; i < dockState->m_arrBarInfo.GetSize(); i++ )
  {
    CControlBarInfo* pInfo = (CControlBarInfo*)dockState->m_arrBarInfo[i];

    CControlBar* bar = parentWnd->GetControlBar( pInfo->m_nBarID );
    if ( bar )
    {
      CRect barRect = mapProps->GetWindowRect( pInfo->m_nBarID );
      bar->SetWindowPos( NULL, 0, 0, barRect.Width(), barRect.Height(), SWP_NOMOVE | SWP_NOZORDER );
    }; // if bar
  }; // for i


  // ProfilEditor-Fenster: falls offen, soll es nicht leer sein sondern das erste Profil zeigen
  pDoc->SetActiveProfile( 1, FALSE ); // FeatureID 1 ist immer da
  
  // Tools initialisieren
  m_toolController->Toggle( ID_TOOLS_NONE ); // TOOLS_NON als default wählen
  m_mapController->Toggle( 0 ); // FOLLOW_PROFILE ist erst mal aus
  m_mapController->Toggle( 1 ); // FOLLOW_PROFILE ist erst mal aus

  // einzige Verwendung von m_extent: setzt den Kartenausschnitt aus der serialisierung
  m_map.SetExtent( pDoc->GetStartExtent() );

  pDoc->FireMapDocChanged( IMapDocListener::EXTENT_CHANGED | IMapDocListener::THEME_DATA_CHANGED |
                           IMapDocListener::ACTIVE_PROFILE_CHANGED, 0 );
};

BOOL CMapView::OnPreparePrinting(CPrintInfo* pInfo) 
// Überschreibung von CView::OnPreparePrinting, wird
// an das KartenLayout weitergeleitet
{
//Demo-Version ausgrenzen----------------------
	if (!WSPFeatures::Instance()->isEnabled("MAPPER","map_nodemo"))
	{
		std::string TheText = std::string(WSPFeatures::Instance()->GetDataStr("HEAD", "DEMO_INFO"));
		TheText.append("\n\n");
		TheText.append(WSPFeatures::Instance()->GetDataStr("MAPPER", "map_nodemo"));
		AfxMessageBox(TheText.c_str() ,MB_ICONINFORMATION,0);
		return false;
	}
	//Demo-Version ausgrenzen----------------------

  CMapDoc* pDoc = GetDocument();
  ASSERT(pDoc);
  CMapLayout* pMapLayout = pDoc->GetMapLayout();
  ASSERT(pMapLayout);

  return pMapLayout->OnPreparePrinting( pInfo, this, &m_map );
}

void CMapView::OnPrint(CDC* pDC, CPrintInfo* pInfo) 
{
  CMapDoc* pDoc = GetDocument();
  ASSERT(pDoc);
  CMapLayout* pMapLayout = pDoc->GetMapLayout();
  ASSERT(pMapLayout);

  pMapLayout->Paint( pDC );
}

void CMapView::OnSize(UINT nType, int cx, int cy) 
{
	CFormView::OnSize(nType, cx, cy);

	if (m_map.m_hWnd)
		m_map.SetWindowPos(0, 0, 0, cx, cy, SWP_NOZORDER);
}

void CMapView::OnFilePrintSetup()
{
	if (!WSPFeatures::Instance()->isEnabled("MAPPER","map_nodemo"))
	{
		std::string TheText = std::string(WSPFeatures::Instance()->GetDataStr("HEAD", "DEMO_INFO"));
		TheText.append("\n\n");
		TheText.append(WSPFeatures::Instance()->GetDataStr("MAPPER", "map_nodemo"));
		AfxMessageBox(TheText.c_str() ,MB_ICONINFORMATION,0);
		return;
	}

	CMapDoc* pDoc = GetDocument();
	ASSERT(pDoc);
	CMapLayout* pMapLayout = pDoc->GetMapLayout();
	ASSERT(pMapLayout);

	pMapLayout->DoPrinterSetup( CString(MAKEINTRESOURCE(IDS_PRINTER_SETUP)), this );
	
}

void CMapView::OnFilePrintPreview() 
// Grossenteils aus von CView::DoPrintPreview kopiert
// erzeugt ein CMapPreview statt eines CPrintPreview
{

	//Demo-Version ausgrenzen----------------------
	if (!WSPFeatures::Instance()->isEnabled("MAPPER","map_nodemo"))
	{
		std::string TheText = std::string(WSPFeatures::Instance()->GetDataStr("HEAD", "DEMO_INFO"));
		TheText.append("\n\n");
		TheText.append(WSPFeatures::Instance()->GetDataStr("MAPPER", "map_nodemo"));
		AfxMessageBox(TheText.c_str() ,MB_ICONINFORMATION,0);
		return;
	}
	//Demo-Version ausgrenzen----------------------



  // Problem: der Kartenauschnitt in der Preview und der in der View waren nicht derselbe
  // Grund: der Kartenauschnitt hängt von der Fenstergrösse ( der View ) ab, diese wird durch das deaktivieren
  // und verstekcen der View geändert.
  // 'Lösung': die View vor der Preview Iconifizieren, dann tritt dieser Effekt nicht mehr auf
  CChildFrame* parentWnd = (CChildFrame*)GetParent();

  // den aktuellen Zustand des Fensters merken, wird nach der Preview wiederhergestellt
  if( parentWnd->IsIconic() )
    m_restoreState = SW_MINIMIZE;
  else if( parentWnd->IsZoomed() )
    m_restoreState = SW_MAXIMIZE;
  else
    m_restoreState = SW_RESTORE;
  parentWnd->ShowWindow( SW_MINIMIZE );

  CMapDoc* pDoc = GetDocument();
  ASSERT( pDoc );
  CMapLayout* pMapLayout = pDoc->GetMapLayout();
  ASSERT( pMapLayout );

  // must not create this on the frame.  Must outlive this function
  CPrintPreviewState* pState = new CPrintPreviewState;

	CFrameWnd* pParent = STATIC_DOWNCAST(CFrameWnd, AfxGetMainWnd());
	ASSERT_VALID(pParent);

	CCreateContext context;
	context.m_pCurrentFrame = pParent;
  context.m_pCurrentDoc = GetDocument();
  context.m_pLastView = this;
  
  // Create the preview view object
  CMapPreview* pView = new CMapPreview();
  pView->m_pPreviewState = pState;        // save pointer
  
  pParent->OnSetPreviewMode(TRUE, pState);    // Take over Frame Window
  
  // Create the preview view as a child of the App Main Window.  This
  // is a sibling of this view if this is an SDI app.  This is NOT a sibling
  // if this is an MDI app.
  
  if (!pView->Create( pParent, NULL, NULL, AFX_WS_DEFAULT_VIEW,
    CRect(0,0,0,0), pParent, AFX_IDW_PANE_FIRST, &context))
  {
    TRACE0("Error: couldn't create preview view for frame.\n");
    pParent->OnSetPreviewMode(FALSE, pState);   // restore Frame Window
    pView->m_pPreviewState = NULL;  // do not delete state structure
    delete pView;
    delete pState;      // preview failed to initialize, delete State now
    
    return;
  }
  
  // Preview window shown now
  pState->pViewActiveOld = pParent->GetActiveView();
  CView* pActiveView = pParent->GetActiveFrame()->GetActiveView();
  
  // das MapLayout für die Preview reinitialisieren
  // und dann der Preview übergeben
  if( !pView->InitializePreview( pMapLayout, &m_map ) )
  {
    pView->OnPreviewClose();
    return;         // signal that OnEndPrintPreview was called
  }
  
  pParent->SetActiveView(pView);  // set active view - even for MDI
  
  // update toolbar and redraw everything
  pView->m_pToolBar->SendMessage(WM_IDLEUPDATECMDUI, (WPARAM)TRUE);

  pParent->RecalcLayout();            // position and size everything
  pParent->UpdateWindow();

  pView->SizeToFit(); // die Darstellung ins Fenster einpassen
} // OnFilePrintPreview

void CMapView::OnEndPrintPreview(CDC* pDC, CPrintInfo* pInfo, POINT point, CPreviewView* pView) 
{
	CFormView::OnEndPrintPreview(pDC, pInfo, point, pView);

  // den Zustand vor der Preview wiederherstellen
  RestoreAfterPreview();
}

void CMapView::RestoreAfterPreview()
{
  CMDIChildWnd* parentWnd = (CMDIChildWnd*)GetParent();
  switch( m_restoreState )
  {
  case SW_RESTORE:
    parentWnd->MDIRestore();
    break;

  case SW_MAXIMIZE:
    parentWnd->MDIMaximize();
    break;
  } // switch
} // RestoreAfterPreview


BEGIN_EVENTSINK_MAP(CMapView, CFormView)
    //{{AFX_EVENTSINK_MAP(CMapView)
	ON_EVENT(CMapView, IDC_MAP1, -605 /* MouseDown */, OnMapMouseDown, VTS_I2 VTS_I2 VTS_I4 VTS_I4)
	ON_EVENT(CMapView, IDC_MAP1, -606 /* MouseMove */, OnMapMouseMove, VTS_I2 VTS_I2 VTS_I4 VTS_I4)
	ON_EVENT(CMapView, IDC_MAP1, -601 /* DblClick */, OnMapDblClick, VTS_NONE)
	ON_EVENT(CMapView, IDC_MAP1, -607 /* MouseUp */, OnMapMouseUp, VTS_I2 VTS_I2 VTS_I4 VTS_I4)
	ON_EVENT(CMapView, IDC_MAP1, 4 /* AfterTrackingLayerDraw */, OnAfterTrackingLayerDrawMap1, VTS_I4)
	//}}AFX_EVENTSINK_MAP
END_EVENTSINK_MAP()

void CMapView::OnMapMouseDown( short Button, short Shift, long x, long y )
{
  CMoPoint cursor = m_map.ToMapPoint( (float)x, (float)y );

  m_mapController->OnMapMouseDown( cursor, Button, Shift );

  if( Button == 1 && Shift == 0 )
    m_toolController->OnMapMouseDown( cursor, Button, Shift );

  ReleaseCapture();
}

void CMapView::OnMapMouseMove( short Button, short Shift, long x, long y ) 
{
  CMoPoint cursor( m_map.ToMapPoint( (float)x, (float)y) );

  m_mapController->OnMapMouseMove( cursor, Button, Shift );
  m_toolController->OnMapMouseMove( cursor, Button, Shift );

  ReleaseCapture();
};

void CMapView::OnMapMouseUp( short Button, short Shift, long x, long y ) 
{
  CMoPoint cursor( m_map.ToMapPoint( (float)x, (float)y) );

  m_mapController->OnMapMouseUp( cursor, Button, Shift );

  if( Button == 1 && Shift == 0 )
    m_toolController->OnMapMouseUp( cursor, Button, Shift );

  ReleaseCapture();
}

void CMapView::OnMapDblClick() 
{
  m_mapController->OnMapDblClick();
  m_toolController->OnMapDblClick();

  ReleaseCapture();
}

void CMapView::OnUpdateIndicator( CCmdUI* pCmdUI )
{
	CMapDoc *pDoc = (CMapDoc*)GetDocument();

	for( int i = 1; i <= 3; i++ )
	{
    UINT nID, nStyle;
    CString str;
    int cxWidth;

		((CMainFrame*)theApp.m_pMainWnd)->GetStatusBar()->GetPaneInfo( i, nID, nStyle, cxWidth );
    switch( nID )
    {
    case ID_INDICATOR_LAYER:
      if( pDoc->GetActiveLayer() == NULL )
        str.LoadString(IDS_NOACTIVETHEME);
      else
      {
        CLayer *layer = pDoc->GetActiveLayer();
        str.FormatMessage(IDS_ACTIVETHEME, layer->GetName());
      }
      break;
      
    case ID_INDICATOR_XCOORD:
      str.Format( "X: %.3f", m_cursorPos.x );
      break;
      
    case ID_INDICATOR_YCOORD:
      str.Format( "Y: %.3f", m_cursorPos.y );
      break;
      
    default:
      return;
    }
		// resize pane
		CDC* pDC = ((CMainFrame*)theApp.m_pMainWnd)->GetStatusBar()->GetDC();
		CFont* pFont = ((CMainFrame*)theApp.m_pMainWnd)->GetStatusBar()->GetFont();
		CFont* pOldFont = pDC->SelectObject( pFont );
    CSize size = pDC->GetOutputTextExtent( str );
    pDC->SelectObject( pOldFont );
    
    ((CMainFrame*)theApp.m_pMainWnd)->GetStatusBar()->ReleaseDC( pDC );
    ((CMainFrame*)theApp.m_pMainWnd)->GetStatusBar()->SetPaneInfo( i, nID, nStyle, size.cx );
    ((CMainFrame*)theApp.m_pMainWnd)->GetStatusBar()->SetPaneText( i, str, TRUE );
  }
}

int CMapView::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
  if (CFormView::OnCreate(lpCreateStruct) == -1)
    return -1;

  CChildFrame* parentWnd = (CChildFrame*)GetParent();
  m_legend = &(parentWnd->m_legend);
  m_profilEditor = &(parentWnd->m_wndProfilEditor);

  return 0;
}

void CMapView::OnAfterTrackingLayerDrawMap1( long hDC )
{
};

void CMapView::OnPrepareDC(CDC* pDC, CPrintInfo* pInfo) 
// überschreibt CFormView::OnPrepareDC
// dies erledigt CMapLayout
{
  if( pInfo != NULL ) // d.h. Drucken oder Druckvorschau
  {
    // bei Druck oder Druckvorschau erledigt alles das Kartenlayout
    CMapDoc* pDoc = GetDocument();
    ASSERT(pDoc);
    CMapLayout* pMapLayout = pDoc->GetMapLayout();
    ASSERT(pMapLayout);
  
    pMapLayout->OnPrepareDC( pDC, pInfo, CSize( 0, 0 ) );
  }
  else // ansonsten machts die FormView
    CFormView::OnPrepareDC( pDC, pInfo );
}

void CMapView::OnUpdate( CView* pSender, LPARAM lHint, CObject* pHint /* = NULL */ )
{
  // sollte nicht aufgerufen werden
  TRACE( "CMapView::OnUpdate aufgerufen!\n" );

  // CFormView::OnUpdate nicht aufrufen, macht einen Invalidate( TRUE ) !, das wollen wir aber gar nicht!
};

int CMapView::GetActiveLayerIndex()
{
  return m_legend->GetActiveLayerIndex();
}

double CMapView::GetSearchDistance( )
{
  return m_map.ToMapDistance((float)(7));
}

void CMapView::RedrawLayerBorder()
// aktiviert einen TrackinLayer zum verändern der Wasseroberfläche für TinCut
{
  CMapDoc* pDoc = GetDocument();

  // den vorhandenen trackingLayer auf jeden Fall löschen
  CMoTrackingLayer trackingLayer( m_map.GetTrackingLayer() );

  if( !pDoc || !LPDISPATCH(trackingLayer) )
    return;

  // alle RAND TrackingShapes löschen
  while( TRUE )
  {
    CMoGeoEvent event( trackingLayer.FindEvent( MO2_TRACKTAG_RAND ) );
    if ( !LPDISPATCH(event) )
      break;
    trackingLayer.RemoveEvent( event.GetIndex() );
  }; // while true


  CLayer* layer = pDoc->GetActiveLayer();
  if( !layer || !layer->GetLayerType() == moMapLayer )
    return;

  CLayerData* layerData = pDoc->GetLayerData( layer->GetType() );
  if( !layerData || !layerData->bUmrandungZeichnen )
    return;

  CMapLayer* randLayer = (CMapLayer*)layer;

  // die Umrandungen abholen
  CMoPolyArray polyArray;
  randLayer->GetBorderPolygon( polyArray );

  CMoSymbol trackSymbol(trackingLayer.GetSymbol( MO2_TRACKSYMBOL_RAND ));
  CMoSymbol randSymbol( randLayer->GetSymbol() );
  if( !LPDISPATCH(trackSymbol) || !LPDISPATCH(randSymbol) )
    return;
        
  trackSymbol.SetOutlineColor( randSymbol.GetColor() );
  trackSymbol.SetOutline( TRUE );

  trackSymbol.SetSize( 1 ); 
  trackSymbol.SetSymbolType( moFillSymbol );
  trackSymbol.SetStyle( moTransparentFill );

  for( int p = 0; p < polyArray.GetSize(); p++ )
  {
    CMoPolygon poly = polyArray[p];
    CMoGeoEvent newEvent( trackingLayer.AddEvent( poly, MO2_TRACKSYMBOL_RAND ) );
    newEvent.SetTag( MO2_TRACKTAG_RAND );
  }; // for p

}; // ActivateWaterLevel

void CMapView::OnMapClipboard() 
// Kopiert die Karte in die Zwischenablage
{
	m_map.CopyMap( 1 );
}

void CMapView::OnMapSaveAs() 
// Speichert die Karte als
//  - Bitmap
{

		//Demo-Version ausgrenzen----------------------
	if (!WSPFeatures::Instance()->isEnabled("MAPPER","map_nodemo"))
	{
		std::string TheText = std::string(WSPFeatures::Instance()->GetDataStr("HEAD", "DEMO_INFO"));
		TheText.append("\n\n");
		TheText.append(WSPFeatures::Instance()->GetDataStr("MAPPER", "map_nodemo"));
		AfxMessageBox(TheText.c_str() ,MB_ICONINFORMATION,0);
		return;
	}
	//Demo-Version ausgrenzen----------------------


	// Datei speichern Dialog
  CString fileFilter = "Windows Bitmap (*.bmp)|*.bmp|Windows Enhanced Metafile (*.emf)|*.emf||";

  CFileDialog  dlg( FALSE, "bmp", (LPCTSTR)GetDocument()->GetTitle(), 
                    OFN_OVERWRITEPROMPT | OFN_HIDEREADONLY, fileFilter, this );
  if ( dlg.DoModal() == IDOK )
  {
    // Anhand der Dateiendung das Exportformat bestimmen und Karte entsprechend exportieren
    CString fileName = dlg.GetPathName();
    CString fileExt = dlg.GetFileExt();
    fileExt.MakeUpper();
    if ( fileExt == "BMP" )
      m_map.ExportMap2( moExportBMP, (LPCTSTR)fileName, 1, CComVariant( TRUE ) );
    else if( fileExt == "EMF" )
      m_map.ExportMap2( moExportEMF, (LPCTSTR)fileName, 1, CComVariant( TRUE ) );
    else
      AfxMessageBox( IDS_WRONG_EXPORT_FORMAT );
  };
}

void CMapView::SetActiveProfileTrackingLayer( long profilID, BOOL bShow )
// fügt einen TrackingLayer zur Karte hinzu, welcher die aktive Profillinie darstellt
// Parameter:
//        long profilID: Nummer der hervorzuhebenden Profillinie
//        BOOL bShow: TRUE: profilLinie wird gezeichnet, FALSE der trackingLayer wird gelöscht
{
  // in jedem Fall schon mal das alte Ereignis löschen
  CMoTrackingLayer trackLayer( m_map.GetTrackingLayer() );
  CMoGeoEvent event( trackLayer.FindEvent( MO2_TRACKTAG_ACTIVEPROFILE ) );
  if ( LPDISPATCH(event) )
    trackLayer.RemoveEvent( event.GetIndex() );

  // den ProfilLayer holen
  CMapLayer* profilLayer = GetDocument()->GetLayers()->FindFirstLayer( CLayer::profilLines );
  if ( !bShow || !profilLayer )
    return;

  // rausfinden, obs eine Mehrfeldbrücke ist
  int mfbID = -1;
  try
  {
    COleVariantEx( profilLayer->GetFieldValByID( profilID, MO2_FIELD_MFB ) );
  }
  catch( const COleException& )  { /* ignore */ };

  CString searchString; // jetzt werden alle Profillinien gesucht, die markiert werden sollen
  if( mfbID == -1 )
    searchString.Format( "%s = %d", MO2_FIELD_FEATUREID, profilID ); // falls keine Mehrfeldbrücke, nur das Profil mit dieser Nummer
  else
    searchString.Format( "%s = %d", MO2_FIELD_MFB, mfbID ); // falls MFB, alle die zu dieser MFB gehören

  CMoRecordset lineRecords( profilLayer->SearchExpression( searchString ) );
  if( !LPDISPATCH(lineRecords) )
    return;
  CMoFields lineFields( lineRecords.GetFields() );
  if( !LPDISPATCH(lineFields) )
    return;
  CMoField lineShapeField( lineFields.Item( COleVariant( MO2_FIELD_SHAPE  ) ) );

  // die TrackLinie erzeugen
  CMoLine profilLine;
  MO2CREATE( profilLine, "Line" ); // eine neue Linie erzeugen
  if( !LPDISPATCH(profilLine) )
    return;

  CMoParts profilParts( profilLine.GetParts() );
  if( !LPDISPATCH(profilParts) )
    return;

  while( !lineRecords.GetEof() )
  {
    VARIANT var = lineShapeField.GetValue();
    if( var.vt == VT_DISPATCH )
    {
      CMoLine line( var.pdispVal );
      if( LPDISPATCH(line) )
      {
        CMoParts parts( line.GetParts() );
        if( LPDISPATCH(parts) && parts.GetCount() == 1 )
        {
          CMoPoints points( parts.Item( CComVariant( 0 ) ) );
          if( LPDISPATCH(points) )
            profilParts.Add( points );
        }; // if parts
      }; // if line
    }; // if var == VT_DIsPATCH
    
    lineRecords.MoveNext();
  }; // while lineRecords

  // das Symbol des tracking layers initialisieren
  CMoSymbol profilSymbol( profilLayer->GetSymbol() );
  CMoSymbol trackSymbol( trackLayer.GetSymbol( MO2_TRACKSYMBOL_ACTIVEPROFILE ) );
  if( !LPDISPATCH(profilSymbol) || !LPDISPATCH(trackSymbol) )
    return;

  trackSymbol.SetSymbolType( profilSymbol.GetSymbolType() );
  trackSymbol.SetStyle( profilSymbol.GetStyle() );
  trackSymbol.SetColor( profilSymbol.GetColor() ^ 0xFFFFFF );
  trackSymbol.SetSize( profilSymbol.GetSize() * 2 );

  CMoGeoEvent newEvent( trackLayer.AddEvent( profilLine, MO2_TRACKSYMBOL_ACTIVEPROFILE ) );
  newEvent.SetTag( MO2_TRACKTAG_ACTIVEPROFILE );
}; // SetActieProfileTrackingLayer

void CMapView::Zoom( const double faktor, CMoPoint& cursor )
{
  CMapDoc* pDoc = GetDocument();
  CMoRectangle r( pDoc->GetMap()->GetExtent() );
  if( LPDISPATCH( r ) )
  {
    CMoPoint oldCenter( r.GetCenter() );

    r.ScaleRectangle( faktor );

    CMoPoint center( r.GetCenter() );
    
    double p_c_x = cursor.GetX() - center.GetX();
    double p_c_y = cursor.GetY() - center.GetY();
    
    double f = ( faktor + 1.0 ) / faktor;
    
    double x = p_c_x / f;
    double y = p_c_y / f;
    
    r.Offset( x, y );

    pDoc->SetExtent( r );
  }
}

/* virtual */
void CMapView::MapDocChanged( const long type, CLayer* layer )
{
  // erst den lHint checken und die Aktionen festlegen
  long redrawMap = type & ( THEME_GEOMETRY_CHANGED | THEME_ORDER_CHANGED | THEME_PROPS_CHANGED | THEME_DATA_CHANGED );
  long redrawLayerBorder = type & ( THEME_GEOMETRY_CHANGED | THEME_PROPS_CHANGED | THEME_REMOVED | ACTIVE_THEME_CHANGED );
  long adaptFullExtent = type & ( THEME_ADDED | THEME_REMOVED | THEME_GEOMETRY_CHANGED );
  long updateActiveProfile = type & ACTIVE_PROFILE_CHANGED;

  CMapDoc* pDoc = GetDocument();

  // if no extent is yet set, try to FullExtent
  if( redrawMap )
  {
    CMapDoc* pDoc = GetDocument();
    CMoRectangle rect( pDoc->GetExtent() );
    if( !LPDISPATCH(rect) || rect.GetCenter().GetX() < 1000.0 )
      pDoc->SetFullExtent();
  }

  if( redrawLayerBorder )
    RedrawLayerBorder();

  if( updateActiveProfile )
    SetActiveProfileTrackingLayer( pDoc->GetActiveProfile(), pDoc->IsShowActiveProfile() );

  if( adaptFullExtent )
    theApp.GetMapProject()->UpdateMapDoc( pDoc );

  if( redrawMap )
    m_map.Refresh();
}