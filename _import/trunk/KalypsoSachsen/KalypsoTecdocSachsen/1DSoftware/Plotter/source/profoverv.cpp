// Propfoverv.cpp : implementation file
//

#include "stdafx.h"

#include "..\..\wspprj\wspprj.h"

#include "plotfrm.h"
#include "profileovxy.h"
#include "plotter.h"
#include "plotview.h"

#include "profoverv.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif



/////////////////////////////////////////////////////////////////////////////
// CProfilOverview

IMPLEMENT_DYNCREATE( CProfilOverview, CMDIChildWnd )

CProfilOverview::CProfilOverview()
{
  m_state = NULL;
  m_width = 2;
  m_height = 3;

  m_activeCs = NULL;
  m_topLeftCs = NULL;
};

CProfilOverview::~CProfilOverview()
{
  DeleteContents();
};

void CProfilOverview::DeleteContents()
{
  for( int i = 0; i < m_profileWindows.GetSize(); i++ )
    delete m_profileWindows[i];
  m_profileWindows.RemoveAll();
}; // DeleteContents


BOOL CProfilOverview::Create( CMDIFrameWnd* pParent /* = NULL */ )
// erzeugt das Fenster und alle seine Childs
{
  return CMDIChildWnd::Create( NULL, CString(MAKEINTRESOURCE(IDS_PROFILE_OVERVIEW_TITLE)), 
                               WS_CHILD | WS_OVERLAPPEDWINDOW | WS_VISIBLE | WS_MAXIMIZE, 
                               rectDefault, pParent );
};

/////////////////////
// MessageHandling //
/////////////////////

BEGIN_MESSAGE_MAP( CProfilOverview, CMDIChildWnd )
  ON_WM_CLOSE()
  ON_WM_PAINT()
  ON_WM_SETFOCUS()
  ON_WM_MDIACTIVATE()

  ON_COMMAND_EX( ID_BROWSE_LEFT, OnBrowse )
  ON_COMMAND_EX( ID_BROWSE_RIGHT, OnBrowse )
  ON_COMMAND_EX(IDC_BROWSE_COMBO, OnBrowse )

  ON_NOTIFY_EX_RANGE( NM_DBLCLK, IDW_PROFIL_OVERVIEW + 1, -1, OnProfilCtrlDblClk )
  ON_COMMAND( ID_PROFIL_OV_XY, OnXY )
END_MESSAGE_MAP()

void CProfilOverview::OnClose()
{
  DeleteContents();
  
  CWnd::OnClose();
};

void CProfilOverview::OnPaint()
{
  CMDIChildWnd::OnPaint();

  if ( m_profileWindows.GetSize() < (int)(m_height * m_width) )
    return;

  CRect clientRect;
  CWnd::GetClientRect( clientRect );
  clientRect.DeflateRect( 3, 3 );
  
  UINT height = clientRect.Height() / m_height;
  UINT width = clientRect.Width() / m_width;

  for( UINT i = 0; i < m_height; i++ )
  {
    for( UINT j = 0; j < m_width; j++ )
    {
      CPoint topLeft( clientRect.left + j * width, clientRect.top + i * height );
      CRect childRect( topLeft, CSize( width, height ) );
      childRect.DeflateRect( 2, 2 );
      
      ProfilCtrl* profilCtrl = m_profileWindows.GetAt( i * m_width + j );
      CString str;
      profilCtrl->GetWindowText( str );
      profilCtrl->MoveWindow( childRect, TRUE );
    }; // for j
  }; // for i
};

void CProfilOverview::OnXY()
{
  CProfileOvXY xyDlg( m_width, m_height, 10, 20 );
  if ( xyDlg.DoModal() == IDOK )
    SetSizes( xyDlg.GetWidth(), xyDlg.GetHeight() );
}; // OnXy

BOOL CProfilOverview::OnProfilCtrlDblClk( UINT nID, NMHDR* pNMHDR, LRESULT* pResult )
{
  pResult = 0;
  
  if ( nID > IDW_PROFIL_OVERVIEW && nID < IDW_PROFIL_OVERVIEW + m_height * m_width + 1 )
  {
    ProfilCtrl* pCtrl = (ProfilCtrl*)GetDescendantWindow( nID );
    if ( pCtrl )
    {
      CrossSection* cs = (CrossSection*)pCtrl->GetData();
      if ( cs )
      {
        CPlotterFrame* pFrame = (CPlotterFrame*)GetParent();
        if ( pFrame )
        {
          GETPLOTTERAPP->m_pState = GetState();
          pFrame->OpenCrossSection( cs );
        }; // if pFrame
        return TRUE;
      }; // section
    } // if pCtrl
  }; // if nID

  return FALSE; // Notifikation kam von einer anderen Control: Message weiterbehandeln
}; // OnOpenProfile

void CProfilOverview::OnMDIActivate( BOOL bActivate, CWnd* pActivateWnd, CWnd* pDeactivateWnd )
{
  // zuerst deaktivieren
  if( pDeactivateWnd != NULL && pDeactivateWnd->IsKindOf( RUNTIME_CLASS(CMDIChildWnd) ) )
  {
    CPlotterFrame* pPlotFrame = (CPlotterFrame*)((CMDIChildWnd*)pDeactivateWnd)->GetMDIFrame();
    if( pPlotFrame != NULL )
      pPlotFrame->SetBrowseCombo( NULL, NULL );
  }

  if( pActivateWnd == this )
  {
    CPlotterFrame* pPlotFrame = (CPlotterFrame*)GetMDIFrame();
    if( pPlotFrame != NULL )
      pPlotFrame->SetBrowseCombo( m_state, (CrossSection*)m_topLeftCs );
  }

  CMDIChildWnd::OnMDIActivate( bActivate, pActivateWnd, pDeactivateWnd );
} // OnMDIActivate


///////////////
// Attribute //
///////////////

void CProfilOverview::SetState( State* state )
{
  DeleteContents();
  m_state = state;

  if ( m_state )
  {
    CreateProfileWindows();
    SetTopLeftSection( m_state->GetFirstCrossSection() );

    CString title( MAKEINTRESOURCE( IDS_PROFILE_OVERVIEW_TITLE ) );

    title += " - " + m_state->GetWaterName() + " - " + m_state->GetName();

    SetWindowText( title );
  };
}; // SetProject

void CProfilOverview::SetSizes( UINT width, UINT height )
{
  m_width = width;
  m_height = height;
  CreateProfileWindows();
  SetTopLeftSection( m_topLeftCs );
}; // SetSizes

void CProfilOverview::PostNcDestroy()
// nix tun, alles wird gut
{
};  // PostNcDestroy

/////////////////
// Operationen //
/////////////////

BOOL CProfilOverview::CreateProfileWindows()
// erzeugt die Profilfensterchen
{
  ASSERT( m_state );

  DeleteContents();

  for( UINT i = 0; i < m_height * m_width; i++ )
  {
    ProfilCtrl* pCtrl = new ProfilCtrl( "" );
    if ( !pCtrl->CreateEx( 0, WS_VISIBLE | WS_CHILD | WS_BORDER, CRect( 0, 0, 100, 100 ), 
                           this, IDW_PROFIL_OVERVIEW + i + 1 ) )
    {
      DeleteContents();
      return FALSE;
    };

    m_profileWindows.Add( pCtrl );
  }; // for i


  return TRUE;
}; // CreateProfileWindows

void CProfilOverview::SetTopLeftSection( Section* topleftCs )
// setzt, welche Profile angezeigt werden sollen
// Parameter:
//        CrossSection* topLeftCs: dieses Profil wird links oben dargesellt, die folgenden
//                                Fenster enthalten dann die Nachfolgenden Profile
//                            falls diese CrossSection nicht vorkommt, bleibts wies war
{
  ASSERT( m_state );

  CWaitCursor wait; // das laden der Profile kann ein wenig dauern

  Section* cs = m_state->GetFirstCrossSection();

  // topLeft finden
  BOOL bFound = FALSE;
  while ( cs )
  {
    if ( cs == topleftCs )
    {
      bFound = TRUE;
      break;
    };
 
    cs = m_state->GetNextCrossSection();
  }; // while cs

  if ( !bFound )
    return;

  // cs zeigt jetzt auf topLeftCs
  // also jetzt die Profile darstellen
  m_topLeftCs = topleftCs;
  UINT count = 0;
  while( cs && count < m_height * m_width )
  {
    if( !cs->LoadProfil() )
      continue;
    ProfilCtrl* pCtrl = m_profileWindows[count];
    pCtrl->SetData( (DWORD)cs );
    pCtrl->SetProfil( cs->GetProfil() );
    pCtrl->ShowWindow( TRUE );
    count++;
    cs = m_state->GetNextCrossSection();
  }; // while cs
  // restliche Fenster nullen
  for( ; count < m_height * m_width; count++ )
  {
    ProfilCtrl* pCtrl = m_profileWindows[count];
    pCtrl->SetData( (DWORD)NULL );
    pCtrl->SetProfil( NULL );
    pCtrl->ShowWindow( FALSE );
  };

  CPlotterFrame* plotFrame = (CPlotterFrame*)GetParentFrame();
  plotFrame->SetBrowseCombo( m_state, (CrossSection*)m_topLeftCs );
}; // SetTopLeftCs

BOOL CProfilOverview::OnBrowse( UINT nID )
{
  CPlotterFrame* plotFrame = (CPlotterFrame*)GETPLOTTERAPP->GetMainWnd();

  if ( plotFrame )
  {
    CComboBox* bCombo = plotFrame->GetBrowseCombo();
    int index = -1;

    if ( bCombo && bCombo->GetSafeHwnd() )
    {
      switch ( nID )
      {
      case ID_BROWSE_LEFT:
        index = max( 0, bCombo->GetCurSel() - 1);
        break;

      case ID_BROWSE_RIGHT:
        index = min( bCombo->GetCount() - 1, bCombo->GetCurSel() + 1 );
        break;

      case IDC_BROWSE_COMBO:
        index = bCombo->GetCurSel();
        break;

      default:
        return FALSE;
      }; // switch nID
      
      if ( index != -1 )
      {
        Section* newCs = (Section*)bCombo->GetItemDataPtr( index );
        SetTopLeftSection( newCs );
      }; // if index != -1
    }; // if bCombo
  }; // if plotFrame

  return TRUE;
}; // OnBrowse

