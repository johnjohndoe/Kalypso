// TextBox.cpp : implementation file
//
#include "stdafx.h"
#include "TextBox.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CTextComboBox

CTextComboBox::CTextComboBox() : CGraphicComboBox()
{
}

CTextComboBox::~CTextComboBox()
{
}

BEGIN_MESSAGE_MAP(CTextComboBox, CGraphicComboBox)
	//{{AFX_MSG_MAP(CTextComboBox)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CTextComboBox message handlers

void CTextComboBox::DrawItem(LPDRAWITEMSTRUCT lpDrawItemStruct) 
{
	CDC dc;
	CString str;
	CFont font, *pFont = GetFont();
	CRect rcDC;

	PrepareDC(dc, lpDrawItemStruct);

	rcItem = lpDrawItemStruct->rcItem;
	if (lpDrawItemStruct->itemID==-1)
		GetWindowText(str);
	else
		GetLBText(lpDrawItemStruct->itemID, str);

	dc.SelectObject(pFont);
	// Draw Text
	rcDC.SetRect(0, 0, rcItem.Width(), rcItem.Height());
	dc.ExtTextOut(0, 0, ETO_CLIPPED, rcDC, str, NULL);

	OutputDC(dc);

	// Clean up
	dc.DeleteDC();
}

