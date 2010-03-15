// ChildFrm.cpp : Implementierung der Klasse CChildFrame
//
#pragma warning(disable:4786)
#pragma warning(disable:4503)

#include "stdafx.h"

#include "wspprj\wspprj.h"

#include "mapproj.h"
#include "mainfrm.h"
#include "wspmap.h"
#include "profilauswahl.h"
#include "childfrm.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CChildFrame

/////////////////////////////////////////////////////////////////////////////
// CChildFrame Konstruktion/Destruktion

CChildFrame::CChildFrame()
{
}

CChildFrame::~CChildFrame()
{
}

/////////////////////////////////////////////////////////////////////////////
// CChildFrame Diagnose

#ifdef _DEBUG
void CChildFrame::AssertValid() const
{
	CMDIChildWnd::AssertValid();
}

void CChildFrame::Dump(CDumpContext& dc) const
{
	CMDIChildWnd::Dump(dc);
}

#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CChildFrame Nachrichten-Handler

IMPLEMENT_DYNCREATE(CChildFrame, CMDIChildWnd)

BEGIN_MESSAGE_MAP(CChildFrame, CMDIChildWnd)
//{{AFX_MSG_MAP(CChildFrame)
  ON_WM_CREATE()
  ON_WM_CLOSE()
	//}}AFX_MSG_MAP
  ON_COMMAND_EX(ID_VIEW_LEGEND, OnCoolBarCheck)  
  ON_COMMAND_EX(ID_VIEW_PROFILEBAR, OnCoolBarCheck)
  ON_COMMAND_EX(ID_VIEW_PROFILAUSWAHL, OnBarCheck)
  ON_COMMAND_EX(ID_VIEW_SCALEBAR, OnBarCheck)

  ON_UPDATE_COMMAND_UI(ID_VIEW_LEGEND, OnUpdateControlBarMenu)
  ON_UPDATE_COMMAND_UI(ID_VIEW_PROFILEBAR, OnUpdateControlBarMenu)
  ON_UPDATE_COMMAND_UI(ID_VIEW_PROFILAUSWAHL, OnUpdateControlBarMenu)
  ON_UPDATE_COMMAND_UI(ID_VIEW_SCALEBAR, OnUpdateControlBarMenu)
END_MESSAGE_MAP()


int CChildFrame::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	if (CMDIChildWnd::OnCreate(lpCreateStruct) == -1)
		return -1;

  EnableDocking(CBRS_ALIGN_ANY);

  CString title;

  CRect clientRect;
  GetClientRect( &clientRect );

  // ProfilEditor
  CSize profilEditorSize( clientRect.Width(), clientRect.Height() * 2 / 5 );
  if ( !m_wndProfilEditor.Create( title, this, profilEditorSize, TRUE, 
                                  IDW_PROFIL_EDITOR, WS_CHILD | CBRS_TOP | CBRS_TOOLTIPS | 
                                  CBRS_FLYBY | CBRS_SIZE_DYNAMIC ) )
    return -1;
  m_wndProfilEditor.EnableDocking(CBRS_ALIGN_TOP | CBRS_ALIGN_BOTTOM);
  DockControlBar(&m_wndProfilEditor, AFX_IDW_DOCKBAR_TOP);

  // Legende
  if( !m_wndLegendBar.Create(this, &m_legend, title, IDD_LEGENDBAR, FALSE,
    WS_CHILD | WS_VISIBLE | CBRS_LEFT | CBRS_TOOLTIPS | CBRS_FLYBY | CBRS_SIZE_DYNAMIC ))
    return -1;
	m_wndLegendBar.EnableDocking(CBRS_ALIGN_LEFT | CBRS_ALIGN_RIGHT);
  DockControlBar(&m_wndLegendBar, AFX_IDW_DOCKBAR_LEFT);
  m_legend.SetBackStyle( 1 );
  
  // Maßstab
  if (!m_wndScaleBar.Create(this, IDD_SCALEBAR, CBRS_BOTTOM, IDW_SCALE_BAR))
    return -1;
  m_wndScaleBar.SetBarStyle( m_wndScaleBar.GetBarStyle() | CBRS_ALIGN_BOTTOM | 
                             CBRS_TOOLTIPS | CBRS_FLYBY | CBRS_SIZE_FIXED );
  m_wndScaleBar.ShowWindow( SW_HIDE );

  // ProfilAuswahl
  CSize profilAuswahlSize( clientRect.Width() / 2, clientRect.Height() / 2 );
  if ( !m_wndProfilAuswahl.Create( title, this, profilAuswahlSize, FALSE, 
    IDW_PROFIL_AUSWAHL, WS_CHILD | WS_VISIBLE | CBRS_TOOLTIPS | CBRS_FLYBY | CBRS_SIZE_DYNAMIC | 
    CBRS_ALIGN_BOTTOM ) )
    return -1;

  m_wndProfilAuswahl.EnableDocking( 0 );
  FloatControlBar( &m_wndProfilAuswahl, CPoint( 10,100 ) );
  
  return 0;
};


BOOL CChildFrame::OnCoolBarCheck( UINT nID )
// CCoolDialogBar's können nicht durch OnBarCheck behandelt werden, da dort keine
// eindeutige Fenster ID vergeben wird
{
  CControlBar* controlBar = NULL;
  
  switch ( nID )
  {
  case ID_VIEW_LEGEND:
    controlBar = &m_wndLegendBar;
    break;
    
  case ID_VIEW_PROFILEBAR:
    controlBar = &m_wndProfilEditor;
    break;
  }; // switch
  
  if ( controlBar && controlBar->GetSafeHwnd() )
  {
    ShowControlBar( controlBar, !controlBar->IsWindowVisible(), TRUE );
    return TRUE;
  };
  
  return FALSE;
};

void CChildFrame::OnUpdateControlBarMenu( CCmdUI* pCmdUI )
// siehe OnCoolBarCheck
{
  CControlBar* controlBar = NULL;

  switch( pCmdUI->m_nID )
  {
  case ID_VIEW_LEGEND:
    controlBar = &m_wndLegendBar;
    break;

  case ID_VIEW_PROFILEBAR:
      controlBar = &m_wndProfilEditor;
    break;

  case ID_VIEW_SCALEBAR:
    controlBar = &m_wndScaleBar;
    break;
    
  case ID_VIEW_PROFILAUSWAHL:
    controlBar = &m_wndProfilAuswahl;
    break;
  }; // switch

  if ( controlBar && controlBar->GetSafeHwnd() )
  {
    pCmdUI->Enable ( TRUE );
    pCmdUI->SetCheck(	controlBar->IsWindowVisible());
  }
  else
    pCmdUI->Enable( FALSE );
}; // OnUpdateControlBarMenu

void CChildFrame::OnClose() 
// Überschreibung von CFrameWnd::OnClose: einziger unterschied: falls das Dokument
// geschlossen werden soll übernimmt dies das CMapProject
// und: automatisches speichern hier implementiert
// Bemerkung:
//      es wird davon ausgegangen, dass nur eine einzige view existiert, d.h. sobald der benutzer
//      das Framewindow der View schliesst, wird das dokument abgeschossen
{
  if (m_lpfnCloseProc != NULL && !(*m_lpfnCloseProc)(this))
    return;
  
  // Note: only queries the active document
  CDocument* pDocument = GetActiveDocument();

  CMapProject* mapProject = theApp.GetMapProject();
  ASSERT( mapProject );
  
  mapProject->CloseMap( (CMapDoc*)pDocument );
}; // OnClose

void CChildFrame::OnUpdateFrameTitle( BOOL bAddToTitle )
{
  CMDIChildWnd::OnUpdateFrameTitle( bAddToTitle );

  // auch alle ControlBars mit neuen Titeln versorgen
  if ( bAddToTitle )
  {
    CString addTitle = TEXT(" - " ) + GetActiveDocument()->GetTitle();
    CString title;

    title.LoadString(IDS_LEGENDBAR_TITLE);
    AfxSetWindowText( m_wndLegendBar.GetSafeHwnd(), LPCTSTR(title + addTitle) );

    title.LoadString( IDS_PROFILAUSWAHL_TITLE );
    AfxSetWindowText( m_wndProfilAuswahl.GetSafeHwnd(), LPCTSTR(title +  addTitle) );

    title.LoadString(IDS_PROFILEDITOR_TITLE);
    AfxSetWindowText( m_wndProfilEditor.GetSafeHwnd(), LPCTSTR(title +  addTitle) );
  };
}; // OnUpdateFrameTitle
