// LineBox.cpp : implementation file
//
#include "stdafx.h"
#include "linebox.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CLineComboBox

CLineComboBox::CLineComboBox() : CGraphicComboBox()
{
	m_nItems = NTYPES;
}

CLineComboBox::~CLineComboBox()
{
}

BEGIN_MESSAGE_MAP(CLineComboBox, CGraphicComboBox)
	//{{AFX_MSG_MAP(CLineComboBox)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CLineComboBox message handlers

void CLineComboBox::DrawItem(LPDRAWITEMSTRUCT lpDrawItemStruct) 
{
	CDC dc;
	CPen *pPen, *pOldPen;
	int x, y;
	int nIndex;

	PrepareDC(dc, lpDrawItemStruct);

	rcItem = lpDrawItemStruct->rcItem;

	// Then the item with the correct line type
	nIndex = (int)lpDrawItemStruct->itemID;
	if (nIndex<0 || nIndex>=NTYPES)
		nIndex = NTYPES;

	// Create a pen and draw the line
	pPen = new CPen(nIndex, 0, dc.GetNearestColor(dc.GetTextColor()));
	pOldPen = dc.SelectObject(pPen);
	y = (int)(rcItem.Height()/2);
	dc.MoveTo(0, y);
	x = rcItem.Width();
	dc.LineTo(x, y);

	OutputDC(dc);

	// Clean up
	dc.SelectObject(pOldPen);
	dc.DeleteDC();
	delete pPen;
}
