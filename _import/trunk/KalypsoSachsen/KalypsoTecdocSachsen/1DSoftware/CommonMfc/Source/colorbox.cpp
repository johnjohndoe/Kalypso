// ColorBox.cpp : implementation file
//
#include "stdafx.h"
#include "commresource.h"
#include "colorbox.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CColorComboBox

CColorComboBox::CColorComboBox() : CGraphicComboBox()
{
	m_hPal = NULL;
}

CColorComboBox::~CColorComboBox()
{
}

COLORREF CColorComboBox::GetPaletteColor(int nIndex)
{
	PALETTEENTRY pe;
	CPalette *pPal;
	COLORREF color;

	pPal = CPalette::FromHandle(m_hPal);
	if (nIndex<0 || nIndex>pPal->GetEntryCount())
		nIndex = 0;
	GetPaletteEntries(m_hPal, nIndex, 1, &pe);
	color = RGB(pe.peRed, pe.peGreen, pe.peBlue);
	return color;
}

CString CColorComboBox::GetColorText(int nIndex)
{
	CString text;
	int stringID;

	switch (nIndex)
	{
		case BLACK:
			stringID = IDS_COLOR_BLACK;
			break;

		case DARKRED:
			stringID = IDS_COLOR_DARKRED;
			break;

		case DARKGREEN:
			stringID = IDS_COLOR_DARKGREEN;
			break;

		case LIGHTBROWN:
			stringID = IDS_COLOR_LIGHTBROWN;
			break;

		case DARKBLUE:
			stringID = IDS_COLOR_DARKBLUE;
			break;

		case DARKMAGENTA:
			stringID = IDS_COLOR_DARKMAGENTA;
			break;

		case DARKCYAN:
			stringID = IDS_COLOR_DARKCYAN;
			break;

		case GRAY:
			stringID = IDS_COLOR_GRAY;
			break;

		case LIGHTGRAY:
			stringID = IDS_COLOR_LIGHTGRAY;
			break;

		case RED:
			stringID = IDS_COLOR_RED;
			break;

		case GREEN:
			stringID = IDS_COLOR_GREEN;
			break;

		case YELLOW:
			stringID = IDS_COLOR_YELLOW;
			break;

		case BLUE:
			stringID = IDS_COLOR_BLUE;
			break;

		case MAGENTA:
			stringID = IDS_COLOR_MAGENTA;
			break;

		case CYAN:
			stringID = IDS_COLOR_CYAN;
			break;

		case WHITE:
			stringID = IDS_COLOR_WHITE;
			break;

		default:
			stringID = -1;
			break;
	}
	if (stringID!=-1)
		text.LoadString(stringID);
	return text;
}

COLORREF CColorComboBox::GetColor()
{
	PALETTEENTRY pe;
	COLORREF color;

	GetPaletteEntries(m_hPal, GetCurSel(), 1, &pe);
	color = RGB(pe.peRed, pe.peGreen, pe.peBlue);
	return color;
}

void CColorComboBox::SetColor(COLORREF color)
{
	CPalette *pPal;

	pPal = CPalette::FromHandle(m_hPal);
	SetCurSel(pPal->GetNearestPaletteIndex(color));
}

void CColorComboBox::SetPalette(HPALETTE hPal)
{
	CPalette* pPal;
	
	m_hPal = hPal;
	pPal = CPalette::FromHandle(m_hPal);
	VERIFY(pPal);
	m_nItems = pPal->GetEntryCount();
	Init();
}

BEGIN_MESSAGE_MAP(CColorComboBox, CGraphicComboBox)
	//{{AFX_MSG_MAP(CColorComboBox)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CColorComboBox message handlers

void CColorComboBox::DrawItem(LPDRAWITEMSTRUCT lpDrawItemStruct) 
{
	CDC dc;
	CRect rcColor;
	CBrush *pBrush;
	CPen *pOldPen;
	CFont *pOldFont;
	CPalette *pPal;
	COLORREF color;

	ASSERT(m_hPal!=NULL);
	// Get the palette for the item display context
	pPal = CPalette::FromHandle(m_hPal);
	VERIFY(pPal);

	PrepareDC(dc, lpDrawItemStruct);

	VERIFY(pDC->SelectPalette(pPal,0));
	pDC->RealizePalette();

	color = GetPaletteColor(lpDrawItemStruct->itemID);

	rcItem = lpDrawItemStruct->rcItem;

	int nIndex = lpDrawItemStruct->itemID;

	// Then the item in the correct color
	rcColor.SetRect(0, 0, 30, rcItem.Height());
	rcColor.InflateRect(-1, -1);
	pBrush = new CBrush(pDC->GetNearestColor(color));
	dc.FillRect(&rcColor, pBrush);
	delete pBrush;
	pBrush = new CBrush(pDC->GetNearestColor(COLOR_WINDOWTEXT));
	dc.FrameRect(&rcColor, pBrush);
	delete pBrush;
	rcColor.InflateRect(1, 1);
	pOldPen = dc.SelectObject(pDC->GetCurrentPen());
	pOldFont = dc.SelectObject(pDC->GetCurrentFont());
	dc.TextOut(rcColor.BottomRight().x+4, rcColor.TopLeft().y, GetColorText(lpDrawItemStruct->itemID));

	OutputDC(dc);

	// Clean up
	dc.SelectObject(pOldPen);
	dc.SelectObject(pOldFont);
	dc.DeleteDC();
}

