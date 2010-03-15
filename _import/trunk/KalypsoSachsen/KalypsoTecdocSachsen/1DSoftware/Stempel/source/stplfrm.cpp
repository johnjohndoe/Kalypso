// StplFrm.cpp : implementation of the CStempelFrame class
//

#include "stdafx.h"

#include "newdlg.h"
#include "stempel.h"

#include "stplfrm.h"


#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CStempelFrame

IMPLEMENT_DYNAMIC(CStempelFrame, CMainFrame)

BEGIN_MESSAGE_MAP(CStempelFrame, CMainFrame)
	//{{AFX_MSG_MAP(CStempelFrame)
	ON_COMMAND(ID_FILE_NEW, OnFileNew)
	ON_WM_CREATE()
	ON_WM_CLOSE()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CStempelFrame construction/destruction

CStempelFrame::CStempelFrame() : CMainFrame()
{
}

CStempelFrame::~CStempelFrame()
{
}

/////////////////////////////////////////////////////////////////////////////
// CStempelFrame diagnostics

#ifdef _DEBUG
void CStempelFrame::AssertValid() const
{
	CMainFrame::AssertValid();
}

void CStempelFrame::Dump(CDumpContext& dc) const
{
	CMainFrame::Dump(dc);
}

#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CStempelFrame message handlers

void CStempelFrame::OnFileNew() 
{
	CNewDialog dlg;

	dlg.m_width = 70;
	dlg.m_height = 60;
	if (dlg.DoModal()==IDOK)
	{
    CStempelApp* stplApp = GETSTEMPELAPP;
		stplApp->m_strStempelName = dlg.m_name;
		stplApp->m_sizeStempel = CSize(dlg.m_width*MM_FACTOR, dlg.m_height*MM_FACTOR);
		stplApp->OnFileNew();
	}
}


int CStempelFrame::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	if (CMainFrame::OnCreate(lpCreateStruct) == -1)
		return -1;
	
  LoadBarState( _T( "Stempel-Settings-V1" ) );
  
	return 0;
}

void CStempelFrame::OnClose() 
{
	SaveBarState( _T( "Stempel-Settings-V1" ) );
	
	CMainFrame::OnClose();
}
