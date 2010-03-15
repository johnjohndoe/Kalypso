// ovrvwbar.cpp: Implementierungsdatei
//
#pragma warning(disable:4786)
#pragma warning(disable:4503)

#include "stdafx.h"

#include "commonMFc/include/mfcHelper.h"

#include "wspmap.h"
#include "mapproj.h"
#include "maphelper.h"

#include "ovrvwbar.h"

extern CWSPMapApp theApp;

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// COverviewBar

IMPLEMENT_DYNAMIC(COverviewBar, CDialogBar)

COverviewBar::COverviewBar()
{
}

COverviewBar::~COverviewBar()
{
}

void COverviewBar::UpdateOverviewMap()
{
	CFileStatus rStatus;
  int i, maxLen = 0;

  if( !m_map.GetSafeHwnd() )
    return;
	if ( !m_map.m_hWnd )
		return;

  // Übersichtsfenster löschen
  CMoLayers layers( m_map.GetLayers() );
  layers.Clear();
  m_maps.DeleteAllItems();

  CMapProject* mapProject = theApp.GetMapProject();

  if ( !mapProject || mapProject->GetNumMapDocs() == 0)
		return;

  for ( i = 0; i < mapProject->GetNumMapDocs(); i++ )
		maxLen = max( maxLen, mapProject->GetMapDocName(i).GetLength() );

  // Shape Dateinamen erzeugen: stets wie Name des MapProjekts mit endung .shp
  CString path = BCE::MfcHelper::GetFileDirectory( theApp.GetMapProject()->GetPathName() );
  CString fileTitle = BCE::MfcHelper::GetFileTitle( theApp.GetMapProject()->GetPathName() );

  // neues shapefile erzeugen bzw. altes öffnen
  CMoDataConnection conn;
  MO2CREATE( conn, "DataConnection" );
  conn.SetDatabase( path );
  VERIFY( conn.Connect() );

  // die alten Shapes immer löschen, und dann neu erzeugen
  conn.DeleteGeoDataset( fileTitle );

  CMoTableDesc tableDesc;
  MO2CREATE( tableDesc, "TableDesc" );
  
  tableDesc.SetFieldCount(2);
  tableDesc.SetFieldName(0, MO2_FIELD_NUMBER);
  tableDesc.SetFieldType(0, moLong);
  tableDesc.SetFieldPrecision(0, 3);
  
  tableDesc.SetFieldName(1, MO2_FIELD_NAME);
  tableDesc.SetFieldType(1, moString);
  tableDesc.SetFieldLength(1, maxLen);
  
  CMoGeoDataset geoDataset( conn.AddGeoDataset( fileTitle, moPolygon, tableDesc, CComVariant(0), CComVariant(0) ) );
  ASSERT(LPDISPATCH(geoDataset));
    
  // layer mit GeoDataset verbinden
  
  // einen neuen Layer erzeugen und mit dem GeoDataset verbinden
  CMoMapLayer layer;
  MO2CREATE( layer, "MapLayer" );
  CMoSymbol symbol( layer.GetSymbol());
  symbol.SetStyle( moLightGrayFill );
  symbol.SetColor( moGreen );
  
  CMoLabelPlacer labelPlacer;
  MO2CREATE( labelPlacer, "LabelPlacer" );
  labelPlacer.SetField( MO2_FIELD_NAME );


  layer.SetGeoDataset( geoDataset );
  
  CMoRecordset recs( layer.GetRecords() );

  ASSERT( recs.GetUpdatable() );
  recs.MoveFirst();
  // alte Daten löschen
  for ( i = 0; i < recs.GetCount(); i++ )
  {
    recs.Delete();
    recs.MoveNext();
  }; // for i

  // neue Karte zeichnen
  CMoFields fields( recs.GetFields() );
    
  for ( i = 0; i < mapProject->GetNumMapDocs(); i++ )
  {
    Rect<double, FALSE> rect( mapProject->GetMapDocRect( i ) );

    CString path = mapProject->GetMapDocFile( i );
    CString name = mapProject->GetMapDocName( i );
    
    m_maps.InsertItem( LVIF_TEXT | LVIF_IMAGE | LVIF_STATE | LVIF_PARAM, i, LPCTSTR(name),
      0, 0, IMAGE_MAP, i );
    
    if( rect.IsRectEmpty() )
      continue;

		CMoPoints points;
    MO2CREATE( points, "Points" );

		CMoPoint point1;
    MO2CREATE( point1, "Point" );
		point1.SetX(rect.left);
		point1.SetY(rect.top);
		points.Add(point1);

		CMoPoint point2;
    MO2CREATE( point2, "Point" );
		point2.SetX(rect.right);
		point2.SetY(rect.top);
		points.Add(point2);

		CMoPoint point3;
    MO2CREATE( point3, "Point" );
		point3.SetX(rect.right);
		point3.SetY(rect.bottom);
		points.Add(point3);

		CMoPoint point4;
    MO2CREATE( point4, "Point" );
		point4.SetX(rect.left);
		point4.SetY(rect.bottom);
		points.Add(point4);

		CMoPolygon polygon;
    MO2CREATE( polygon, "Polygon" );
		
		CMoParts parts(polygon.GetParts());
		parts.Add(points);

		recs.AddNew();
		SetValue(fields, MO2_FIELD_SHAPE, LPDISPATCH(polygon));
		SetValue(fields, MO2_FIELD_NUMBER, i);
		SetValue(fields, MO2_FIELD_NAME, name);
		recs.Update();
	}

  layer.SetRenderer( labelPlacer );

  layers.Add( LPDISPATCH(layer) );

  CMoRectangle r(m_map.GetFullExtent());
	ASSERT(LPDISPATCH(r));
	m_map.SetExtent(r);

  m_map.Refresh();
}

void COverviewBar::DoPaint(CDC* pDC)
{
	ASSERT_VALID(this);
	ASSERT_VALID(pDC);

	CRect rect;
	GetClientRect(rect);
	rect.DeflateRect(8, 8);
	CRect map, text, list;
	GetDlgItem(IDC_STATIC1)->GetWindowRect(text);
	ScreenToClient(text);
	GetDlgItem(IDC_OVERVIEWLIST)->GetWindowRect(list);
	ScreenToClient(list);
	map = rect;
	map.bottom = map.top + min((rect.Height()/2 - text.Height()/2-4), rect.Width());
	text = CRect(rect.left, map.bottom+4, rect.right, map.bottom+4+text.Height());
	list = CRect(rect.left, text.bottom+4, rect.right, rect.bottom);
	GetDlgItem(IDC_OVERVIEWMAP)->MoveWindow(&map);
	GetDlgItem(IDC_STATIC1)->MoveWindow(&text);
	GetDlgItem(IDC_OVERVIEWLIST)->MoveWindow(&list);

	CDialogBar::DoPaint(pDC);
}

BEGIN_MESSAGE_MAP(COverviewBar, CDialogBar)
	//{{AFX_MSG_MAP(COverviewBar)
	ON_NOTIFY(NM_DBLCLK, IDC_OVERVIEWLIST, OnDblclkList1)
	ON_WM_CLOSE()
	ON_MESSAGE(WM_INITDIALOG, HandleInitDialog)
	ON_WM_DESTROY()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()


/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten COverviewBar 


void COverviewBar::DoDataExchange(CDataExchange* pDX) 
{
  CDialogBar::DoDataExchange(pDX);

  try
  {
    // dies ist die allererste Stelle im Programm, wo
    // ein MapObjects objekt initialisiert wird
    // wenn das nicht klappt ist MapObjects vermutlich nicht richtig installiert
    DDX_Control(pDX, IDC_OVERVIEWMAP, m_map);
  }
  catch( CNotSupportedException* se )
  {
    se->ReportError();
    AfxMessageBox( "MapObjects wurde bei der Installation nicht richtig initialisiert.\nBitte beachten Sie die Hinweise auf der Installations-CD.\nDie Anwendung wird geschlossen." );

    // unschön, wie schliesse ich die Anwendung sauber?
    ::exit( -1 );
  }

  DDX_Control(pDX, IDC_OVERVIEWLIST, m_maps);
}

LRESULT COverviewBar::HandleInitDialog(WPARAM wParam, LPARAM lParam)
{
	LRESULT lResult = CDialogBar::HandleInitDialog( wParam, lParam );
	UpdateData( FALSE );

  m_maps.SetImageList( CCommonImageList::GetList( FALSE ), LVSIL_SMALL);

  CRect rect;
  m_maps.GetClientRect(&rect);
  m_maps.InsertColumn( 0, "Map", LVCFMT_LEFT, rect.Width(), 0 );

	return lResult;
}

void COverviewBar::OnDblclkList1(NMHDR* /*pNMHDR*/, LRESULT* pResult) 
{
	int i;
	CString path, str;
	CFileStatus rStatus;

	for ( i = 0; i < m_maps.GetItemCount(); i++ )
	{
		if ( m_maps.GetItemState( i, LVIS_SELECTED ) == LVIS_SELECTED )
		{
      theApp.GetMapProject()->OpenMap( i );
			break;
		}
	}
	
	*pResult = 0;
}

BOOL COverviewBar::DestroyWindow() 
{
	// TODO: Speziellen Code hier einfügen und/oder Basisklasse aufrufen
	
	return CDialogBar::DestroyWindow();
}

void COverviewBar::OnClose() 
{
	// TODO: Code für die Behandlungsroutine für Nachrichten hier einfügen und/oder Standard aufrufen
	
	CDialogBar::OnClose();
}

void COverviewBar::OnDestroy() 
{
	CDialogBar::OnDestroy();
	
  m_map.DestroyWindow();
}
