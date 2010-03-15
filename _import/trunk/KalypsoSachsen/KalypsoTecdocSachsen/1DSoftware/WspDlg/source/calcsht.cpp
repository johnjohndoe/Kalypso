// calcsht.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "resource.h"

#include "calcpg1.h"
#include "calcpg2.h"
#include "calcpg3.h"
#include "calcpg4.h"
#include "calcsht.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// LWACalcSheet

/* static */
const char LWACalcSheet::REPLACE_CHAR = ',';

IMPLEMENT_DYNAMIC(LWACalcSheet, CPropertySheet)

LWACalcSheet::LWACalcSheet(CalcData *pCD, UINT nIDCaption, CWnd* pParentWnd, UINT iSelectPage)
	:CPropertySheet(nIDCaption, pParentWnd, iSelectPage)
{
	m_pCD = pCD;
	EnableStackedTabs(FALSE);
	page1 = new LWACalcPage1(pCD);
	page2 = new LWACalcPage2(pCD);
	page3 = new LWACalcPage3(pCD);
	page4 = new LWACalcPage4(pCD);
	AddPage(page1);
	AddPage(page2);
	AddPage(page3);
	AddPage(page4);
}

LWACalcSheet::LWACalcSheet(CalcData *pCD, LPCTSTR pszCaption, CWnd* pParentWnd, UINT iSelectPage)
	:CPropertySheet(pszCaption, pParentWnd, iSelectPage)
{
	m_pCD = pCD;
	EnableStackedTabs(FALSE);
	page1 = new LWACalcPage1(pCD);
	page2 = new LWACalcPage2(pCD);
	page3 = new LWACalcPage3(pCD);
	page4 = new LWACalcPage4(pCD);
	AddPage(page1);
	AddPage(page2);
	AddPage(page3);
	AddPage(page4);
}

LWACalcSheet::~LWACalcSheet()
{
	delete page1;
	delete page2;
	delete page3;
	delete page4;
}


BEGIN_MESSAGE_MAP(LWACalcSheet, CPropertySheet)
	//{{AFX_MSG_MAP(LWACalcSheet)
	ON_WM_NCCREATE()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten LWACalcSheet 

BOOL LWACalcSheet::OnNcCreate(LPCREATESTRUCT lpCreateStruct) 
{
	if (!CPropertySheet::OnNcCreate(lpCreateStruct))
		return FALSE;
	
	ModifyStyleEx(0, WS_EX_CONTEXTHELP);
	
	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// BCECalcSheet

IMPLEMENT_DYNAMIC(BCECalcSheet, CPropertySheet)

BCECalcSheet::BCECalcSheet(CalcData* pCD, UINT nIDCaption, CWnd* pParentWnd, UINT iSelectPage, int nMode)
	:CPropertySheet(nIDCaption, pParentWnd, iSelectPage)
{
	m_pCD = pCD;
	EnableStackedTabs(FALSE);
	page1 = new BCECalcPage1(pCD, this);
	page2 = new BCECalcPage2(pCD);
	page3 = new BCECalcPage3(pCD);
	AddPage(page1);
	AddPage(page2);
	AddPage(page3);
}

BCECalcSheet::BCECalcSheet(CalcData* pCD, LPCTSTR pszCaption, CWnd* pParentWnd, UINT iSelectPage, int nMode)
	:CPropertySheet(pszCaption, pParentWnd, iSelectPage)
{
	m_pCD = pCD;
	EnableStackedTabs(FALSE);
	page1 = new BCECalcPage1(pCD, this);
	page2 = new BCECalcPage2(pCD);
	page3 = new BCECalcPage3(pCD);
	AddPage(page1);
	AddPage(page2);
	AddPage(page3);
}

BCECalcSheet::~BCECalcSheet()
{
	delete page1;
	delete page2;
	delete page3;
}

/* static */
const void LWACalcSheet::replaceChar( const CWnd* parentWnd, const int ctrlID )
{
  CWnd* pWnd = parentWnd->GetDlgItem( ctrlID );
  if( pWnd )
  {
    lconv* lc = ::localeconv();

    CString str;
    pWnd->GetWindowText( str );
    str.Replace( ',', *(lc->decimal_point) );
    pWnd->SetWindowText( str );
  }
}


BEGIN_MESSAGE_MAP(BCECalcSheet, CPropertySheet)
	//{{AFX_MSG_MAP(BCECalcSheet)
		// HINWEIS - Der Klassen-Assistent fügt hier Zuordnungsmakros ein und entfernt diese.
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten BCECalcSheet 

