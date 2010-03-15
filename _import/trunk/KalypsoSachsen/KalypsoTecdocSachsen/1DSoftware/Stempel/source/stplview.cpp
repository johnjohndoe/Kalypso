// drawvw.cpp : implementation of the CStempelView class
//

#include "stdafx.h"

#include "drawdoc.h"
#include "draw.h"

#include "griddlg.h"
#include "newdlg.h"
#include "stpldoc.h"
#include "stempel.h"

#include "stplview.h"


#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CStempelView

IMPLEMENT_DYNCREATE(CStempelView, CDrawView)

BEGIN_MESSAGE_MAP(CStempelView, CDrawView)
	//{{AFX_MSG_MAP(CStempelView)
	ON_COMMAND(ID_EXTRAS_GUIDE, OnExtrasGuide)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CStempelView construction/destruction

CStempelView::CStempelView() : CDrawView()
{
	m_bStempelText = TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// CStempelView implementation

void CStempelView::OnExtrasGuide() 
{
	CGridDialog dlg(this);
	
	dlg.DoModal();
}

void CStempelView::Props() 
{
	CNewDialog dlg(this);
	CString str;
	CSize sizeDrawing;
	CStempelDoc *pDoc = (CStempelDoc*)GetDocument();
	
	pDoc->GetName(str);
	sizeDrawing = pDoc->GetDrawingSize();
	dlg.m_name = str;
	dlg.m_width = (int)(sizeDrawing.cx/MM_FACTOR);
	dlg.m_height = (int)(sizeDrawing.cy/MM_FACTOR);
	if (dlg.DoModal()==IDOK)
	{
		pDoc->SetName(dlg.m_name);
		sizeDrawing = CSize(dlg.m_width*MM_FACTOR, dlg.m_height*MM_FACTOR);
		GETSTEMPELAPP->m_sizeStempel = sizeDrawing;
		pDoc->SetDrawingSize(sizeDrawing);
		Invalidate();
	}
}
