// optdlg.cpp : implementation file
//

#include "stdafx.h"

#include "styppage.h"

#include "optdlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// COptionDialog

IMPLEMENT_DYNAMIC(COptionDialog, CPropertySheet)

COptionDialog::COptionDialog(UINT nIDCaption, CWnd* pParentWnd, UINT iSelectPage)
	:CPropertySheet(nIDCaption, pParentWnd, iSelectPage)
{
	page1 = new CSelectTypePage;
	AddPage(page1);
}

COptionDialog::COptionDialog(LPCTSTR pszCaption, CWnd* pParentWnd, UINT iSelectPage)
	:CPropertySheet(pszCaption, pParentWnd, iSelectPage)
{
	page1 = new CSelectTypePage;
	AddPage(page1);
}

COptionDialog::~COptionDialog()
{
	delete page1;
}


BEGIN_MESSAGE_MAP(COptionDialog, CPropertySheet)
	//{{AFX_MSG_MAP(COptionDialog)
		// NOTE - the ClassWizard will add and remove mapping macros here.
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// COptionDialog message handlers
