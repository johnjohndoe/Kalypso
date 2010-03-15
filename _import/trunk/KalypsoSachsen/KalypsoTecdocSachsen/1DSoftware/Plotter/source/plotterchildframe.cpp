// plotterchildframe.cpp: Implementierung der Klasse CPlotterChildFrame.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "splitFrm.h"
#include "plotview.h"
#include "plotfrm.h"

#include "plotterchildframe.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

IMPLEMENT_DYNCREATE( CPlotterChildFrame, CSplitFrame )

CPlotterChildFrame::CPlotterChildFrame() : CSplitFrame()
{
}

void CPlotterChildFrame::OnMDIActivate( BOOL bActivate, CWnd* pActivateWnd, CWnd* pDeactivateWnd )
{
  // zuerst deaktivieren
  if( pDeactivateWnd != NULL && pDeactivateWnd->IsKindOf( RUNTIME_CLASS(CMDIChildWnd) ) )
  {
    CPlotterFrame* pPlotFrame = (CPlotterFrame*)((CMDIChildWnd*)pDeactivateWnd)->GetMDIFrame();
    if( pPlotFrame != NULL )
      pPlotFrame->SetBrowseCombo( NULL, NULL );
  }

  if( pActivateWnd != NULL && pActivateWnd == this )
  {
    CPlotterView* actView = (CPlotterView*)GetActiveView();
    if( actView != NULL )
      actView->FillBrowseCombo( FALSE );
  }

  CMDIChildWnd::OnMDIActivate( bActivate, pActivateWnd, pDeactivateWnd );
} // OnMDIActivate


BEGIN_MESSAGE_MAP( CPlotterChildFrame, CSplitFrame )
  ON_WM_MDIACTIVATE()
END_MESSAGE_MAP()
