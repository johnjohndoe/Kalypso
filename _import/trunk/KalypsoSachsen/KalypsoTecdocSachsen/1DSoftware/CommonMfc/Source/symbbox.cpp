// SymbBox.cpp : implementation file
//
#include "stdafx.h"
#include "symbbox.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CSymbolComboBox

CSymbolComboBox::CSymbolComboBox() : CGraphicComboBox()
{
	m_nItems = 4;
}

CSymbolComboBox::~CSymbolComboBox()
{
}

BEGIN_MESSAGE_MAP(CSymbolComboBox, CGraphicComboBox)
	//{{AFX_MSG_MAP(CSymbolComboBox)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CSymbolComboBox message handlers

void CSymbolComboBox::DrawItem(LPDRAWITEMSTRUCT lpDrawItemStruct) 
{
	CDC dc;
	CRect rcSymbol;
	CPoint points[3];
	int nOldFillMode;
	CPen *pOldPen;
	CBrush *pOldBrush;

	PrepareDC(dc, lpDrawItemStruct);

	int nIndex = lpDrawItemStruct->itemID;
	rcItem = lpDrawItemStruct->rcItem;

	rcSymbol.SetRect(0, 0, rcItem.Width(), rcItem.Height());
	rcSymbol.InflateRect(-1, -1);
	rcSymbol.left = (int)((rcItem.right+rcItem.left-rcSymbol.Height())/2);
	rcSymbol.right = (int)((rcItem.right+rcItem.left+rcSymbol.Height())/2);

	CBrush brush(dc.GetNearestColor(dc.GetTextColor()));
	CPen pen(PS_SOLID, 0, dc.GetNearestColor(dc.GetTextColor()));
	pOldPen = dc.SelectObject(&pen);
	pOldBrush = dc.SelectObject(&brush);

	switch (nIndex)
	{
		case 0:		// circle
			dc.Ellipse(&rcSymbol);
			break;

		case 1:		// square
			dc.FillRect(&rcSymbol, &brush);
			break;

		case 2:		// triangle
			points[0] = CPoint((rcSymbol.left+rcSymbol.right)/2, rcSymbol.top);
			points[1] = CPoint(rcSymbol.right, rcSymbol.bottom);
			points[2] = CPoint(rcSymbol.left, rcSymbol.bottom);
			nOldFillMode = pDC->SetPolyFillMode(ALTERNATE);
			dc.Polygon(points, 3);
			dc.SetPolyFillMode(nOldFillMode);
			break;

		case 3:		// cross
			dc.MoveTo(rcSymbol.left, (rcSymbol.top+rcSymbol.bottom)/2);
			dc.LineTo(rcSymbol.right, (rcSymbol.top+rcSymbol.bottom)/2);
			dc.MoveTo((rcSymbol.left+rcSymbol.right)/2, rcSymbol.top);
			dc.LineTo((rcSymbol.left+rcSymbol.right)/2, rcSymbol.bottom);
			break;
	}

	dc.SelectObject(pOldPen);
	dc.SelectObject(pOldBrush);

	OutputDC(dc);

	// Clean up
	dc.DeleteDC();
}

