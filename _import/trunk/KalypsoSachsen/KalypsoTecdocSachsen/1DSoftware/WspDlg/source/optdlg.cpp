// optdlg.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "resource.h"
#include "strdpage.h"
#include "respage.h"
#include "dirpage.h"
#include "optdlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// OptionsDialog

IMPLEMENT_DYNAMIC(OptionsDialog, CPropertySheet)

OptionsDialog::OptionsDialog( const BOOL bFeatureSort, const BOOL bBCE, UINT nIDCaption, CWnd* pParentWnd, UINT iSelectPage)
	:CPropertySheet(nIDCaption, pParentWnd, iSelectPage)
{
	page1 = new StandardPage( bFeatureSort, bBCE );
	page2 = new ResPage;
	page3 = new DirPage;
	AddPage(page1);
	AddPage(page2);
	AddPage(page3);
}

OptionsDialog::OptionsDialog( const BOOL bFeatureSort, const BOOL bBCE, LPCTSTR pszCaption, CWnd* pParentWnd, UINT iSelectPage)
	: CPropertySheet(pszCaption, pParentWnd, iSelectPage)
{
	page1 = new StandardPage( bFeatureSort, bBCE );
	page2 = new ResPage;
	page3 = new DirPage;
	AddPage(page1);
	AddPage(page2);
	AddPage(page3);
}

OptionsDialog::~OptionsDialog()
{
	delete page1;
	delete page2;
	delete page3;
}


BEGIN_MESSAGE_MAP(OptionsDialog, CPropertySheet)
	//{{AFX_MSG_MAP(OptionsDialog)
	ON_WM_NCCREATE()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten OptionsDialog 

BOOL OptionsDialog::OnNcCreate(LPCREATESTRUCT lpCreateStruct) 
{
	if (!CPropertySheet::OnNcCreate(lpCreateStruct))
		return FALSE;
	
	ModifyStyleEx(0, WS_EX_CONTEXTHELP);
	
	return TRUE;
}
