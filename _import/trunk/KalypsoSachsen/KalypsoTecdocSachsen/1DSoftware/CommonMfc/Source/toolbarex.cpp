// toolbarex.cpp: Implementierungsdatei
//
#include "stdafx.h"
#include "toolbarex.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CToolBarEx

BEGIN_MESSAGE_MAP(CToolBarEx, CToolBar)
	//{{AFX_MSG_MAP(CToolBarEx)
  ON_MESSAGE(WM_IDLEUPDATECMDUI, OnIdleUpdateCmdUI)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()


/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CToolBarEx 

LRESULT CToolBarEx::OnIdleUpdateCmdUI( WPARAM wParam, LPARAM lParam )
{
  if ( IsWindowVisible() )
  {
    CFrameWnd* pParent = (CFrameWnd*)GetParent();
    if (pParent)
      OnUpdateCmdUI( pParent, (BOOL)wParam );
  };
  return 0L;
};