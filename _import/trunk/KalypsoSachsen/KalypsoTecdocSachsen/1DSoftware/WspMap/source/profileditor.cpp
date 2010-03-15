#include "stdafx.h"

#include "resource.h"

#include "commonMFC/include/point.h"
#include "commonMFC/include/memdc.h"

#include "profilModel.h"
#include "mapdoc.h"

#include "profileditor.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define TOCLIENT( crect, rect, point )    \
   CPoint(                         \
      (long)(crect.left + ( ( point.x - rect.left ) / rect.Width() * crect.Width() ) ),      \
      (long)(crect.top + ( ( rect.top - point.y ) / rect.Height() * crect.Height() ) ) \
                );
/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CProfilEditor 


CProfilEditor::CProfilEditor() : CSizingControlBar()
{
	//{{AFX_DATA_INIT(CProfilEditor)
	//}}AFX_DATA_INIT
  m_pMapDoc = 0;
  m_profilModel = std::auto_ptr<CProfilModel>( 0 );
}

void CProfilEditor::DoDataExchange(CDataExchange* pDX)
{
	CSizingControlBar::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CProfilEditor)
	DDX_Control(pDX, IDC_DRAW_AREA, m_profile);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CProfilEditor, CSizingControlBar)
	//{{AFX_MSG_MAP(CProfilEditor)
	ON_WM_SIZE()
	ON_WM_PAINT()
	//}}AFX_MSG_MAP
  ON_WM_ERASEBKGND()
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CProfilEditor 

BOOL CProfilEditor::OnEraseBkgnd( CDC* pDC )
{
  // damits nicht flackert
  return FALSE;
}

BOOL CProfilEditor::Create(LPCTSTR lpszWindowName, 
                           CWnd* pParentWnd, 
                           CSize sizeDefault, 
                           BOOL bHasGripper,
                           UINT nID,
                           DWORD dwStyle /* = WS_CHILD | WS_VISIBLE | CBRS_TOP */ ) 
{
  if ( !CSizingControlBar::Create( lpszWindowName, pParentWnd, sizeDefault, bHasGripper, nID, dwStyle ) )
    return FALSE;

  if ( !m_profile.Create( NULL, WS_CHILD | 
                          SS_BLACKFRAME | SS_ETCHEDFRAME | SS_NOTIFY | SS_SUNKEN,
                          CRect( 100, 100, 200, 200 ), this, IDC_DRAW_AREA ) )
    return FALSE;
  
  return TRUE;
}


void CProfilEditor::OnSize(UINT nType, int cx, int cy) 
{
  CSizingControlBar::OnSize(nType, cx, cy);

  const int deflate = 0;
  const int axisRatio = 10;

  if (!m_profile.GetSafeHwnd())
    return;

  CRect cRect, rect;

  GetClientRect(cRect);
  cRect.DeflateRect(deflate, deflate);

  // Profil
  rect.SetRect(cRect.left, cRect.top, cRect.right, cRect.bottom);
  m_profile.MoveWindow(&rect, TRUE);

  Invalidate();
}; // OnSize()

void CProfilEditor::OnPaint() 
{
  if( !m_profile.GetSafeHwnd() )
    return;

	CPaintDC dc( this ); // device context for painting

  CMemDC memDC( &dc );

  CRect clientRect;
  PaintDC( memDC, clientRect );
}

void CProfilEditor::PaintDC( CDC& dc, CRect& clientRect )
{
  m_profile.GetWindowRect( &clientRect);
  ScreenToClient( &clientRect );

  // Stifte etc. initialisieren
  CPen blackPen1( PS_SOLID, 1, RGB( 0, 0, 0 ) );
  CPen grayPen1 ( PS_SOLID, 1, RGB( 128, 128, 128 ) );
  CPen violetPen1( PS_SOLID, 1, RGB(255, 0, 127) );
  CPen cursorPen( PS_DASHDOT, 1, RGB(255, 80, 50 ) );
  CBrush blackBrush( RGB( 0, 0, 0 ) );
  
  CPen* pOldPen = dc.SelectObject( &blackPen1 );

  CFont achsenFont;
  CString oldFontName;
  dc.GetTextFace(oldFontName);
  achsenFont.CreatePointFont( 80, oldFontName );
  CFont* pOldFont = dc.SelectObject( &achsenFont );
  dc.SetBkColor(0x00FFFFFF);   // hintergrundfarbe für Text ist Weiss

  dc.PatBlt( clientRect.left, clientRect.top, clientRect.Width(), clientRect.Height(), WHITENESS );
  dc.DrawEdge( clientRect, EDGE_SUNKEN, BF_RECT );
  clientRect.DeflateRect( 2, 2, 2, 2 );
  dc.IntersectClipRect( clientRect );

  if( m_pMapDoc && m_profilModel.get() )
  {
    // Ausgabedaten vorinitialisieren
    
    // profilExtent etwas vergössern, damit oben und unten Abstand von den Achsen bleibt
    CDoubleRect profilExtent( m_profilModel->GetExtent() );
    profilExtent.top += profilExtent.Height() / 10;
    profilExtent.bottom -= profilExtent.Height() / 10;

    CProfilModel::PointIterator pointIt = m_profilModel->GetPointBegin();
    if( pointIt == m_profilModel->GetPointEnd() )
      return;

    // Ausgabetexte formatieren und Abstände der Achsen ausrechnen
    CString stationStr;
    stationStr.Format("Station km %.3lf", m_profilModel->GetStation() );
    CString topStr;
    topStr.Format("%.2lf m", m_profilModel->GetExtent().top);
    CString bottomStr;  
    bottomStr.Format("%.2lf m", m_profilModel->GetExtent().bottom);
    CString leftStr;
    leftStr.Format("%.2lf m", m_profilModel->GetExtent().left);
    CString rightStr;
    rightStr.Format("%.2lf m", m_profilModel->GetExtent().right);
    
    int em = dc.GetTextExtent("m").cx; // breite eines 'm'
    int ex = dc.GetTextExtent("x").cy;  // höhe eines 'x'
    
    // eigentlicher Ausgabebereich für das Profil
    CRect profilRect(clientRect.left + dc.GetTextExtent(leftStr).cx + em,
      clientRect.top + dc.GetTextExtent(stationStr).cy + ex,
      clientRect.right - dc.GetTextExtent(rightStr).cx - em,
      clientRect.bottom - dc.GetTextExtent(leftStr).cy - ex);
    
    // Überschrift zeichnen (zentriert zum Profil)
    dc.TextOut(profilRect.left + profilRect.Width() / 2 - dc.GetTextExtent(stationStr).cx / 2,
      clientRect.top + ex / 2, stationStr);
    
    // Achsen Zeichnen
    // zAchse links
    dc.MoveTo( profilRect.left, profilRect.top);
    dc.LineTo( profilRect.left, profilRect.bottom + ex);
    // zAchse rechts
    dc.MoveTo( profilRect.right, profilRect.top);
    dc.LineTo( profilRect.right, profilRect.bottom + ex);
    // yAchse unten
    dc.MoveTo( profilRect.left - em / 2, profilRect.bottom );
    dc.LineTo( profilRect.right + em / 2, profilRect.bottom );
    
    // min-max höhen eintragen und Achsen beschriften
    CPoint point = TOCLIENT(profilRect, profilExtent,
      CDoublePoint( m_profilModel->GetExtent().left, m_profilModel->GetExtent().bottom ) );
    dc.MoveTo(point);
    dc.LineTo(point.x - em / 2, point.y);
    dc.TextOut(clientRect.left + em / 2, point.y, bottomStr);
    
    point = TOCLIENT(profilRect, profilExtent,
      CDoublePoint( m_profilModel->GetExtent().left, m_profilModel->GetExtent().top ) );
    dc.MoveTo(point);
    dc.LineTo(point.x - em / 2, point.y);
    dc.TextOut(clientRect.left + em / 2, point.y, topStr);
    
    dc.TextOut(profilRect.left + em / 2, profilRect.bottom + ex / 2, leftStr);
    dc.TextOut(profilRect.right + em / 2, profilRect.bottom + ex / 2, rightStr);

    CDoublePoint profilPoint( (*pointIt).x, (*pointIt).y );
    pointIt++;
    
    point = TOCLIENT( profilRect, profilExtent, profilPoint );
    dc.MoveTo( point );

    while( pointIt != m_profilModel->GetPointEnd() )
    {
      CDoublePoint profilPoint( (*pointIt).x, (*pointIt).y );

      // Gelände zeichnen
      dc.SelectObject( &violetPen1 );
      
      point = TOCLIENT(profilRect, profilExtent, profilPoint );
      dc.LineTo( point );
      
      // Schraffur zeichnen
      dc.SelectObject( &grayPen1 );
      dc.LineTo( point.x, profilRect.bottom );
      dc.MoveTo( point );

      pointIt++;
    };
    
    // weitere Profilbezogene Daten zeichnen
    CProfilModel::GrenzenIterator gIt = m_profilModel->GetGrenzenBegin();
    while( gIt != m_profilModel->GetGrenzenEnd() )
    {
      const CProfilModel::Grenze& g = *(gIt++);

      CPen typePen( PS_SOLID, 3, m_profilModel->GetTypeColor( g.typ ) );
        
      CPen* oldPen = (CPen*)dc.SelectObject( typePen );

      CDoublePoint gPoint = CDoublePoint( g.x, profilExtent.top );
      CDoublePoint point = TOCLIENT( profilRect, profilExtent, gPoint );
      dc.MoveTo( point.x, profilRect.top + ex / 2 );
      dc.LineTo( point.x, profilRect.bottom );
    }

    for( int i = 0; i < m_profilModel->GetWspCount(); i++ )
    {
      CProfilModel::WSP wsp = m_profilModel->GetWsp( i );

      CPen wspPen( PS_SOLID, 2, wsp.color );
      dc.SelectObject( wspPen );

      CDoublePoint point0 = TOCLIENT( profilRect, profilExtent, wsp.entry.first );
      dc.MoveTo( point0 );
      
      CDoublePoint point1 = TOCLIENT( profilRect, profilExtent, wsp.entry.second );
      dc.LineTo( point1 );
    }

    // Mehrfeldbrücken-Trennlinie zeichnen
    CPen mfbPen( PS_SOLID, 3, RGB( 0, 0, 0 ) );
    dc.SelectObject( mfbPen );
    for( i = 0; i < m_profilModel->GetMfbCount(); i++ )
    {
      CDoublePoint mfbPoint = CDoublePoint( m_profilModel->GetMfb( i ), profilExtent.top );
      point = TOCLIENT( profilRect, profilExtent, mfbPoint );
      dc.MoveTo( point.x, profilRect.top + ex / 2 );
      dc.LineTo( point.x, profilRect.bottom );
    }; // for i

    // Y-Cursor zeichnen
    // auch den x-Wert ausrechnen
    try
    {
      const double yCursor = m_pMapDoc->GetProfileCursor();
      
      CProfilModel::ProfilPoint pPoint = m_profilModel->GetInterpolPointAt( yCursor );
      double hoehe = pPoint.y;
      
      dc.SelectObject( cursorPen );
      
      CDoublePoint yCur( yCursor, profilExtent.bottom );
      point = TOCLIENT( profilRect, profilExtent, yCur );
      dc.MoveTo( point );
      
      CString yCurText;
      yCurText.Format("%.2lf", yCursor );
      point.y += ex / 4;
      point.x = max( profilRect.left, point.x - em );
      point.x = min( profilRect.right - dc.GetTextExtent(yCurText).cx, point.x );
      dc.TextOut( point.x, point.y, yCurText );
      
      yCur.y = hoehe;
      point = TOCLIENT( profilRect, profilExtent, yCur );
      dc.LineTo( point );
      
      dc.SelectObject( blackPen1 );
      CBrush* oldBrush = (CBrush*)dc.SelectObject( blackBrush );
      dc.Ellipse( point.x - 2, point.y - 2, point.x + 2, point.y + 2 );
      dc.SelectObject( cursorPen );
      
      yCur.x = profilExtent.left;
      point = TOCLIENT( profilRect, profilExtent, yCur );
      dc.LineTo( point );
      
      yCurText.Format( "%.2lf", hoehe );
      point.x -= dc.GetTextExtent(yCurText).cx + em / 4;
      dc.TextOut( point.x, point.y, yCurText );
    }
    catch( CProfilModel::InterpolException )
    {
    }
  }; // if profilID != 0

  dc.SelectObject( pOldPen );
  dc.SelectObject( pOldFont );
	// Kein Aufruf von CDialog::OnPaint() für Zeichnungsnachrichten
};


/////////////////////////////////////////////////////////////////////////////////
// Implementation  CProfilEditor

// forciert das Neuzeichnen des Profils und der Achsen
void CProfilEditor::Update()
{
  CRect rect;
  if ( m_profile.GetSafeHwnd() )
  {
    m_profile.GetWindowRect( &rect );
    ScreenToClient( &rect );
    InvalidateRect( rect );
  };

  UpdateWindow();
}; // Redraw()

/* virtual */ 
void CProfilEditor::MapDocChanged( const long type, CLayer* )
{
  if( !m_pMapDoc || ! GetSafeHwnd() )
    return;

  if( type & ( ACTIVE_PROFILE_CHANGED | THEME_GEOMETRY_CHANGED | THEME_PROPS_CHANGED ) )  
  {
    m_profilModel = std::auto_ptr<CProfilModel>( m_pMapDoc->GetActiveProfilModel() );

    CFrameWnd* parent = (CFrameWnd*)GetParent();

    if( m_profilModel.get() == 0 )
      parent->ShowControlBar( this, false, TRUE );
    else if( m_pMapDoc->IsShowActiveProfile() )
      parent->ShowControlBar( this, true, TRUE );
    
    Update();
  }
  else if( type & PROFILE_CURSOR_CHANGED )
    Update();
}

void CProfilEditor::SetMapDoc( CMapDoc* pDoc )
{
  if( m_pMapDoc )
    m_pMapDoc->RemoveMapDocListener( this );

  m_pMapDoc = pDoc; 

  if( m_pMapDoc )
    m_pMapDoc->AddMapDocListener( this );
}