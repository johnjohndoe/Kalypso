// PropDlg.cpp : implementation file
//

#include "stdafx.h"

#include "..\..\wspprj\wspprj.h"

#include "dataPage.h"
#include "plotview.h"
#include "plotdoc.h"
#include "genpage.h"
#include "margpage.h"
#include "formatpg.h"
#include "datapage.h"
#include "linepage.h"
#include "textpage.h"
#include "solpage.h"
#include "plotter.h"
#include "stempel.h"

#include "propdlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CPropertyDialog

IMPLEMENT_DYNAMIC(CPropertyDialog, CPropertySheet)

CPropertyDialog::CPropertyDialog(CPlotterDoc* pDoc, UINT nIDCaption, UINT iSelectPage, BOOL bTemplate /*=FALSE*/, CWnd* pParent )
	:CPropertySheet( nIDCaption, pParent, iSelectPage )
{
  Init( pDoc, iSelectPage, bTemplate );
}

CPropertyDialog::CPropertyDialog( CPlotterDoc* pDoc, LPCTSTR pszCaption, UINT iSelectPage, BOOL bTemplate /*=FALSE*/, CWnd* pParent )
	:CPropertySheet(pszCaption, pParent, iSelectPage)
{
  Init( pDoc, iSelectPage, bTemplate );
}

CPropertyDialog::Init( CPlotterDoc* pDoc, UINT iSelectPage, BOOL bTemplate )
{
	m_nFormat = 0;
	m_nActivePages = 0;
	m_bUpdatePagesNeeded = FALSE;
	m_bUndo = TRUE;
	m_pDoc = pDoc;
	page1 = new CGeneralPage( this, pDoc, bTemplate);
	page2 = new CMarginsPage( this, pDoc );
	page3 = new CFormatPage( this, pDoc );
	page4 = new CDataPage( this, pDoc );
	page5 = new CLinePage( this, pDoc );
	page6 = new CTextPage( this, pDoc, bTemplate );
	page7 = new CSolidPage( this, pDoc );
	AddPage(page1);
	AddPage(page2);
	AddPage(page3);
	AddPage(page4);
	AddPage(page5);
	AddPage(page6);
	AddPage(page7);
	GETPLOTTERAPP->LoadTemplates();
}; // Init


CPropertyDialog::~CPropertyDialog()
{
	delete page1;
  delete page2;
  delete page3;
	delete page4;
  delete page5;
  delete page6;
	delete page7;
	GETPLOTTERAPP->FlushTemplates();
}

void CPropertyDialog::UpdatePages()
{
	if( page2 != NULL )
		page2->Update();
	if (page3!=NULL)
		page3->Update();
	if (page4!=NULL)
		page4->UpdateTree1();
	if (page5!=NULL)
		page5->UpdateTree();
	if (page6!=NULL)
		page6->UpdateTree();
	if (page7!=NULL)
		page7->UpdateTree();
}

void CPropertyDialog::ApplyTemplate(CTemplate *pTemp)
{
	int index = GetActiveIndex();

	if (page1!=NULL)
	{
		if (page1->GetSafeHwnd()==NULL)
		{
			SetRedraw(FALSE);
			SetActivePage(page1);
		}
		page1->ApplyTemplate(pTemp);
	}
	if (page2!=NULL)
	{
		if (page2->GetSafeHwnd()==NULL)
		{
			SetRedraw(FALSE);
			SetActivePage(page2);
		}
		page2->ApplyTemplate(pTemp);
	}
	if (page3!=NULL)
	{
		if (page3->GetSafeHwnd()==NULL)
		{
			SetRedraw(FALSE);
			SetActivePage(page3);
		}
		page3->ApplyTemplate(pTemp);
	}
	if (page4!=NULL)
	{
		if (page4->GetSafeHwnd()==NULL)
		{
			SetRedraw(FALSE);
			SetActivePage(page4);
		}
		page4->ApplyTemplate(pTemp);
	}
	if (page5!=NULL)
	{
		if (page5->GetSafeHwnd()==NULL)
		{
			SetRedraw(FALSE);
			SetActivePage(page5);
		}
		page5->ApplyTemplate(pTemp);
	}
	if (page6!=NULL)
	{
		if (page6->GetSafeHwnd()==NULL)
		{
			SetRedraw(FALSE);
			SetActivePage(page6);
		}
		page6->ApplyTemplate(pTemp);
	}
	if (page7!=NULL)
	{
		if (page7->GetSafeHwnd()==NULL)
		{
			SetRedraw(FALSE);
			SetActivePage(page7);
		}
		page7->ApplyTemplate(pTemp);
	}
	SetActivePage(index);
	SetRedraw(TRUE);
}

void CPropertyDialog::ApplyPages(BOOL bUndoable)
{
	m_bUndo = bUndoable;
	if (page2!=NULL)
		page2->OnApply();
	if (page3!=NULL)
		page3->OnApply();
	if (page4!=NULL)
		page4->OnApply();
	if (page5!=NULL)
		page5->OnApply();
	if (page6!=NULL)
		page6->OnApply();
	if (page7!=NULL)
		page7->OnApply();
}

void CPropertyDialog::AttemptUpdateDrawing()
{
  CDrawView* pView = m_pDoc->GetView();
	if( pView != NULL && m_nFormat == 1 )
	{
		m_pDoc->UpdateDrawing();
		if( m_bUpdatePagesNeeded )
		{
			UpdatePages();
			m_bUpdatePagesNeeded = FALSE;
		}
		m_nFormat = 0;
		m_bUndo = TRUE;

    if( page1 != NULL )
    {
      // warum wird das hier gemacht!!!???
      CDoublePoint scale;
      m_pDoc->GetRealScale( scale );
      
      CString xScale;
      xScale.Format( "%g", scale.x );
      page1->m_staticXScale.FormatMessage( IDS_GPAGE_STATIC_X_SCALE, xScale );
      
      CString yScale;
      yScale.Format( "%g", scale.y );
      page1->m_staticYScale.FormatMessage( IDS_GPAGE_STATIC_Y_SCALE, yScale );
      
      page1->UpdateData( FALSE );
    }

	}
	else if( m_nFormat )
		m_nFormat--;
}

BEGIN_MESSAGE_MAP(CPropertyDialog, CPropertySheet)
	//{{AFX_MSG_MAP(CPropertyDialog)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CPropertyDialog message handlers

BOOL CPropertyDialog::OnCommand(WPARAM wParam, LPARAM lParam) 
{
	BOOL bResult;
	CString str;
	
	bResult = CPropertySheet::OnCommand(wParam, lParam);

	// crack message parameters
	UINT nID = LOWORD(wParam);
	int nCode = HIWORD(wParam);
	if (nCode==BN_CLICKED)
	{
		if (nID==IDOK || nID==ID_APPLY_NOW)
		{
			if (m_nFormat)
				m_nFormat = m_nActivePages;
			if( page1->m_bNewStempel )
			{
				if( m_pDoc->GetStempel()->GetObjectCount() > 0 )
				{
					str.LoadString(IDS_NEWSTEMPEL);
					if (AfxMessageBox(str, MB_ICONQUESTION | MB_YESNO)!=IDYES)
						bResult = TRUE;		// don't pass on this message any further!
				}
			}
		}
	}

	return bResult;
}
