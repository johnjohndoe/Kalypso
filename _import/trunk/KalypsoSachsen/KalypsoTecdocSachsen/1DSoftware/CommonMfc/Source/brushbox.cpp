// BrushBox.cpp : implementation file
//
#include "stdafx.h"
#include "brushbox.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

BOOL operator==(const LOGBRUSH& lb1, const LOGBRUSH& lb2)
{
	if (lb1.lbStyle!=lb2.lbStyle)
		return FALSE; 
	if (lb1.lbHatch!=lb2.lbHatch)
		return FALSE;

	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// CBrushComboBox

CBrushComboBox::CBrushComboBox() : CGraphicComboBox()
{
}

CBrushComboBox::~CBrushComboBox()
{
}

void CBrushComboBox::AddLogBrush(LPLOGBRUSH lpLogBrush)
{
	m_logbrushes.Add(lpLogBrush);
	m_nItems++;
}

LPLOGBRUSH CBrushComboBox::GetLogBrush()
{
	return m_logbrushes[GetCurSel()];
}

void CBrushComboBox::SetLogBrush(LPLOGBRUSH lpLogBrush)
{
	int i;
	
	for (i=0; i<m_logbrushes.GetSize(); i++)
	{
		if (*m_logbrushes[i]==*lpLogBrush)
		{
			SetCurSel(i);
			break;
		}
	}
}

BEGIN_MESSAGE_MAP(CBrushComboBox, CGraphicComboBox)
	//{{AFX_MSG_MAP(CBrushComboBox)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CBrushComboBox message handlers

void CBrushComboBox::DrawItem(LPDRAWITEMSTRUCT lpDrawItemStruct) 
{
	CDC dc;
	CBrush brush, *pBrush;
	LPLOGBRUSH lpLogBrush;
	CRect rcFill;

	PrepareDC(dc, lpDrawItemStruct);

	int nIndex = lpDrawItemStruct->itemID;
	rcItem = lpDrawItemStruct->rcItem;

	dc.SetBkColor(RGB(255, 255, 255));
	dc.SetTextColor(RGB(0, 0, 0));

	// Then the item with the correct brush
	rcFill.SetRect(0, 0, rcItem.Width(), rcItem.Height());
	rcFill.InflateRect(-1, -1);
	if (nIndex>=0 && nIndex<m_logbrushes.GetSize())
	{
		lpLogBrush = m_logbrushes[nIndex];
		brush.CreateBrushIndirect(lpLogBrush);
		dc.FillRect(&rcFill, &brush);
	}
	pBrush = new CBrush(dc.GetTextColor());
	dc.FrameRect(&rcFill, pBrush);
	delete pBrush;

	OutputDC(dc);

	// Clean up
	dc.DeleteDC();
}
