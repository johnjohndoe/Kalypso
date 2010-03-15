// scalebar.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "resource.h"

#include "..\..\wspprj\wspprj.h"

#include "mapdoc.h"
#include "mapview.h"
#include "scalebar.h"


#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CScaleBar 


CScaleBar::CScaleBar()
	: CDialogBar()
{
	//{{AFX_DATA_INIT(CScaleBar)
		// HINWEIS: Der Klassen-Assistent fügt hier Elementinitialisierung ein
	//}}AFX_DATA_INIT
  m_pDoc = NULL;
}


void CScaleBar::DoDataExchange(CDataExchange* pDX)
{
	CDialogBar::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CScaleBar)
	DDX_Control(pDX, IDC_SBSCALEBAR1, m_scaleBar);
		// HINWEIS: Der Klassen-Assistent fügt hier DDX- und DDV-Aufrufe ein
	//}}AFX_DATA_MAP
}

void CScaleBar::RefreshScale()
{
  if( !m_pDoc )
    return;

  CMoMap* pMap = m_pDoc->GetMap();

  if( pMap->GetSafeHwnd() && m_scaleBar.GetSafeHwnd() )
  {
    CMoSbExtent mpExtent( m_scaleBar.GetMapExtent() );
    CMoSbExtent pgExtent( m_scaleBar.GetPageExtent() );
    CMoRectangle extent( pMap->GetExtent() );
    CRect rect;
    
    pMap->GetClientRect( &rect );
    
    if( rect.IsRectEmpty() )
      return;
    
    mpExtent.SetMinX( extent.GetLeft() );
    mpExtent.SetMinY( extent.GetBottom() );
    mpExtent.SetMaxX( extent.GetRight() );
    mpExtent.SetMaxY( extent.GetTop() );
    
    pgExtent.SetMinX( rect.left );
    pgExtent.SetMinY( rect.top );
    pgExtent.SetMaxX( rect.left + rect.Width() );
    pgExtent.SetMaxY( rect.top + rect.Height() );
    
    m_scaleBar.SetMapExtent( mpExtent );
    m_scaleBar.SetPageExtent( pgExtent );
    m_scaleBar.SetMapUnits( m_pDoc->GetMapUnit() );
    m_scaleBar.SetScaleBarUnits( m_pDoc->GetScaleUnit() );
    m_scaleBar.SetScreenUnits( m_pDoc->GetScreenUnit() );
    m_scaleBar.Refresh();
  }; // if GetSafeHwnd
}

BEGIN_MESSAGE_MAP(CScaleBar, CDialogBar)
	//{{AFX_MSG_MAP(CScaleBar)
	ON_WM_CONTEXTMENU()
	//}}AFX_MSG_MAP
	ON_MESSAGE(WM_INITDIALOG, HandleInitDialog)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CScaleBar 

LRESULT CScaleBar::HandleInitDialog(WPARAM wParam, LPARAM lParam)
{
	LRESULT lResult = CDialogBar::HandleInitDialog(wParam, lParam);
	UpdateData(FALSE);

	return lResult;
}

void CScaleBar::DoPaint(CDC* pDC)
{
	ASSERT_VALID(this);
	ASSERT_VALID(pDC);

	CRect rect;
	GetClientRect(rect);
	rect.DeflateRect(8, 8);
	GetDlgItem(IDC_SBSCALEBAR1)->MoveWindow(&rect);

	CDialogBar::DoPaint(pDC);
}

void CScaleBar::OnContextMenu(CWnd* pWnd, CPoint point) 
{
	ReleaseCapture();

	CMenu menu;
	VERIFY(menu.LoadMenu(IDR_SCALEBAR_POPUP));
	
	CMenu* pPopup = menu.GetSubMenu(0);
	ASSERT(pPopup != NULL);
	
	CWnd* pWndPopupOwner = this;
	while (pWndPopupOwner->GetStyle() & WS_CHILD)
		pWndPopupOwner = pWndPopupOwner->GetParent();
	
	pPopup->TrackPopupMenu(TPM_LEFTALIGN | TPM_RIGHTBUTTON, point.x, point.y,
		pWndPopupOwner);
}

void CScaleBar::SetMapDoc( CMapDoc* doc )
{
  if( m_pDoc )
    m_pDoc->RemoveMapDocListener( this );

  m_pDoc = doc;

  if( m_pDoc )
    m_pDoc->AddMapDocListener( this );
};

/* virtual */
void CScaleBar::MapDocChanged( const long type, CLayer* layer )
{
  if( type & ( SCALE_CHANGED | EXTENT_CHANGED ) )
    RefreshScale();
}
