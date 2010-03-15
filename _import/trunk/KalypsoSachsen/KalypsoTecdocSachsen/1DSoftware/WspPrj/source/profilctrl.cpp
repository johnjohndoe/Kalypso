// profilctrl.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "profil.h"
#include "triple.h"

#include "profilctrl.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif


ProfilCtrl::ProfilCtrl( const CString& title )
{
  m_title = title;

  m_profil = NULL;
  m_tripleArray = NULL;
  m_maxExtent = m_extent = CRect( 0, 0, 1, 1 );
  m_backColor = RGB( 255, 255, 255 ); // Weiss
  m_textColor = RGB( 0, 0, 0 ); // Schwarz
  m_headerSize = 10;
}

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten ProfilCtrl 

BEGIN_MESSAGE_MAP(ProfilCtrl, CWnd)
	//{{AFX_MSG_MAP(ProfilCtrl)
	ON_WM_LBUTTONDOWN()
	ON_WM_LBUTTONDBLCLK()
	ON_WM_LBUTTONUP()
	ON_WM_MOUSEMOVE()
	ON_WM_PAINT()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

void ProfilCtrl::OnLButtonDown(UINT nFlags, CPoint point) 
{
	NotifyParent( NM_CLICK );
	
	CWnd::OnLButtonDown(nFlags, point);
}

void ProfilCtrl::OnLButtonDblClk(UINT nFlags, CPoint point) 
{
  NotifyParent( NM_DBLCLK );

	CWnd::OnLButtonDblClk(nFlags, point);
}

void ProfilCtrl::OnLButtonUp(UINT nFlags, CPoint point) 
{
	// Code für die Behandlungsroutine für Nachrichten hier einfügen und/oder Standard aufrufen
	
	CWnd::OnLButtonUp(nFlags, point);
}

void ProfilCtrl::OnMouseMove(UINT nFlags, CPoint point) 
{
	// Code für die Behandlungsroutine für Nachrichten hier einfügen und/oder Standard aufrufen
	
	CWnd::OnMouseMove(nFlags, point);
}

void ProfilCtrl::OnPaint() 
{
  if ( GetUpdateRect( NULL, FALSE ) == 0 )
    return; // keine Update Region existiert -> nix zeichnen
  
	CPaintDC dc( this ); // device context for painting

  CRect clientRect;
  GetClientRect( &clientRect );

  CRgn clipRgn;
  clipRgn.CreateRectRgnIndirect( clientRect );

  dc.SelectClipRgn( &clipRgn );

  // Hintergrundfarbe und -modus setzen
  CBrush brush;
  if ( !brush.CreateSolidBrush( GetBackColor() ) )
    return;
	CBrush* pOldBrush = dc.SelectObject( &brush );

  dc.SetBkColor( GetBackColor() );
  dc.SetBkMode( OPAQUE );

  Profil* profil = GetProfil();
  if( !profil )
    return;

  // Textfarbe und Textfont setzen
  dc.SetTextColor( m_textColor );

  CFont systemFont;
  if ( !systemFont.CreateStockObject( DEFAULT_GUI_FONT ) )
    return;

  CSize headerSize = dc.GetOutputTextExtent( m_headerText );
  int headerHeight = 0;
  if ( headerSize.cy < clientRect.Height() / 3 )
    headerHeight = (int)(headerSize.cy * 1.5);

  if ( headerHeight > 0 ) // Textüberschrift zeichnen
  {
    CFont* oldFont = dc.SelectObject( &systemFont );

    CRect headerRect( clientRect ); // Platz für die Textüberschrift
    headerRect.bottom = headerRect.top + headerHeight;
    headerRect.left += headerRect.Width() * 5 / 100;
    headerRect.right -= headerRect.Width() * 5 / 100;

    UINT flag = DT_NOPREFIX | DT_VCENTER;
    if ( headerSize.cx < clientRect.Width() )
      flag |= DT_CENTER;
    else
      flag |= DT_RIGHT;

    dc.DrawText( m_headerText, headerRect, flag );

    // auf restlichen Zeichenbereich einschränken
    clientRect.top += headerHeight; 

    dc.SelectObject( oldFont );
  }
  else
    clientRect.top += clientRect.Height() * 5 / 100; // sonst 5% Rand nach oben

  clipRgn.Detach();
  clipRgn.CreateRectRgnIndirect( clientRect );
  dc.SelectClipRgn( &clipRgn );

  // Achsen zeichnen
  PaintAxes( &dc, &clientRect );

  if ( clientRect.Height() > 0 )
  {
    clipRgn.Detach();
    clipRgn.CreateRectRgnIndirect( clientRect );
    dc.SelectClipRgn( &clipRgn );

    dc.SetMapMode( MM_ANISOTROPIC );

    int tmpBreite = m_extent.Width();
    int tmpHoehe = m_extent.Height();
    if( tmpBreite == 0 )
      tmpBreite = 1;
    if( tmpHoehe == 0 )
      tmpHoehe = 1;

    dc.SetWindowExt( tmpBreite, tmpHoehe );
    dc.SetWindowOrg( m_extent.left, m_extent.bottom );
    
    tmpBreite = clientRect.Width();
    tmpHoehe = clientRect.Height();
    if( tmpBreite == 0 )
      tmpBreite = 1;
    if( tmpHoehe == 0 )
      tmpHoehe = 1;
    dc.SetViewportExt( tmpBreite, tmpHoehe );
    dc.SetViewportOrg( clientRect.left, clientRect.bottom );
    
    // das Profil selbst zeichnen
    profil->Paint( &dc, &m_extent );

    if( m_tripleArray )
      m_tripleArray->Paint( &dc, &m_extent );
  }; // if clientRect

  // Device Context zurücksetzen
  dc.SelectObject( pOldBrush );
}; // OnPaint


/////////////////////////////////////////////////////////////////////////////
// Überschreibungen von CWnd virtual Funtionen

BOOL ProfilCtrl::CreateEx( DWORD dwStyleEx, DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID ) 
{
  CString wndclass = ::AfxRegisterWndClass( CS_DBLCLKS,::LoadCursor(NULL, IDC_ARROW),
    ::CreateSolidBrush( RGB(255, 255, 255) ), 0 );


	return CWnd::CreateEx( dwStyleEx, wndclass, TEXT(""), dwStyle, rect, pParentWnd, nID );
}; // Create

void ProfilCtrl::WinHelp(DWORD dwData, UINT nCmd) 
{
	// Speziellen Code hier einfügen und/oder Basisklasse aufrufen
	
	CWnd::WinHelp(dwData, nCmd);
}; // WinHelp


///////////////
// Attribute //
///////////////

void ProfilCtrl::SetProfil( Profil* profil )
// setzt das darszustellende Profil
// Parameter:
//        Profil* profil: das darzustellende Profil
// Bemerkung:
//        falls profil == NULL, wird die Control gelöscht
{
  m_profil = profil;

  if ( m_profil )
    ReadProfil();

  Invalidate(); // alles neu zeichnen
}; // SetProfil

void ProfilCtrl::SetTriple( TripleArray* tripleArray )
{
  m_tripleArray = tripleArray;

  Invalidate();
}

void ProfilCtrl::SetHeaderSize( const UINT size )
// setzt die Grösse der Textüberschrift in Prozent
// Parameter:
//        const UINT size: Grösse der Überschrift in Prozent, nur werte zwischen 0 und 100 sind erlaubt
{
  if ( size <= 100 )
    m_headerSize = size;
}; // SetHeaderSize



/////////////////////////////////////////////////////////////////////////////
// Operationen / Implementation

void ProfilCtrl::ReadProfil()
// liest Informationen aus dem Profil
{
  Profil* profil = GetProfil();

  ASSERT( profil  ); // das Profil muss vorhanden sein
  
  m_extent = profil->GetExtent();
  m_extent.bottom += m_extent.Height() * 10 / 100; // +=, weil die Höhe negativ ist
  m_maxExtent = m_extent;

  if( m_title.GetLength() != 0 )
    m_headerText = m_title;
  else
    m_headerText = profil->GetPageDesc( 2 );
}; // ReadProfil

void ProfilCtrl::PaintAxes( CDC* dc, CRect* clientRect )
// zeichnet die Achsen in den Device Kontext
// die gesamte Clipping region darf ausgenutzt werden
// nach Rückkehr muss die Clipping region auf die Verbleibende Fläche gesetzt werden
// Parameter:
//        CDC* dc: der aktuelle device Kontext
//        CRect* clientRect: hier darf PaintAxes zeichnen ( device units )
//                           hier wird der restliche Zeichenbereich zurückübergeben
{
  // Abstände ausrechnen
  CSize xSize = dc->GetOutputTextExtent( TEXT("X"), 1 );

  clientRect->left = clientRect->left + xSize.cx - 1;
  clientRect->right = clientRect->right - xSize.cx + 1;
  clientRect->bottom = clientRect->bottom - xSize.cy + 1;


  CPen blackPen;
  if ( !blackPen.CreatePen( PS_SOLID | PS_ENDCAP_ROUND, 2, RGB( 0, 0, 0 ) ) )
    return;

  CPen* oldPen = dc->SelectObject( &blackPen );

  dc->MoveTo( clientRect->TopLeft() );
  dc->LineTo( clientRect->left, clientRect->bottom );
  dc->LineTo( clientRect->BottomRight() );
  dc->LineTo( clientRect->right, clientRect->top );

  dc->SelectObject( oldPen );

  clientRect->left++;
  clientRect->right--;
  clientRect->bottom--;
  clientRect->top++;
}; // PaintAxes

void ProfilCtrl::NotifyParent( UINT code )
// schickt ein WM_NOTIFY an das ParentWindow
// mit wParam wird stest die eigene ControlID
// mit lParam stets ein NMHDR Struktur geschickt, die den code, die eigene ID und das HWND enthält
// Parameter:
//        UINT code: der mit lParam geschickte code
{
  CWnd* pParent = GetParent();
  if ( pParent )
  {
    UINT nID = GetDlgCtrlID();
    NMHDR nmhdr;
    nmhdr.hwndFrom = GetSafeHwnd();
    nmhdr.idFrom = nID;
    nmhdr.code = code;
    
    pParent->SendMessage( WM_NOTIFY, (WPARAM)nID, (LPARAM)&nmhdr );
  }; // if pParent

}; // NotifyParent