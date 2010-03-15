// colorbtn.cpp: Implementierungsdatei
//
#pragma warning(disable:4786)
#pragma warning(disable:4503)

#include "stdafx.h"

#include "wspmap.h"
#include "colorbtnex.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define RGB_BUTTON_BLACK    (GetSysColor(COLOR_WINDOWFRAME))
#define RGB_BUTTON_WHITE    (GetSysColor(COLOR_BTNHIGHLIGHT))
#define RGB_BUTTON_LIGHT    (GetSysColor(COLOR_BTNFACE))
#define RGB_BUTTON_DARK     (GetSysColor(COLOR_BTNSHADOW))

///////////////////////////////////////////////////////////////////////////////
// 3D Drawing helpers


void _AfxDraw3DFrameEx(CDC *pDC, CRect rcBox, COLORREF colBottomRight, COLORREF colTopLeft)
{
	CPen *pPen2, *pPen, *pOldPen;

	pPen = new CPen(PS_SOLID, 1, colBottomRight);
	pOldPen = pDC->SelectObject(pPen);
	pDC->MoveTo(rcBox.right-1, rcBox.top);
	pDC->LineTo(rcBox.right-1, rcBox.bottom-1);
	pDC->LineTo(rcBox.left-1, rcBox.bottom-1);

	pPen2 = new CPen(PS_SOLID, 1, colTopLeft);
	pDC->SelectObject(pPen2);
	delete pPen;

	pDC->MoveTo(rcBox.left, rcBox.bottom-2);
	pDC->LineTo(rcBox.left, rcBox.top);
	pDC->LineTo(rcBox.right-1, rcBox.top);

	pDC->SelectObject(pOldPen);
	delete pPen2;
}

void _AfxDraw3DButtonFrameEx(CDC *pDC, CRect rcButton, BOOL fFocus)
{
	CPen *pPen, *pOldPen;
	CBrush GrayBrush(RGB_BUTTON_LIGHT);
	CBrush BlackBrush(RGB_BUTTON_BLACK);

	pPen = new CPen(PS_SOLID, 1, RGB_BUTTON_BLACK);
	pOldPen = pDC->SelectObject(pPen);

	// Draw gray outside
	pDC->FrameRect(&rcButton, &GrayBrush);
	rcButton.InflateRect(-1, -1);

	if (fFocus)
	{
		// Draw inside of border
		pDC->FrameRect(&rcButton, &BlackBrush);
		// Draw curved border on outside;
		rcButton.InflateRect(1, 1);
	}
	else
	{
		// Prepare inside border
		pDC->FrameRect(&rcButton, &GrayBrush);
	}

	pDC->MoveTo(rcButton.left+1, rcButton.top);
	pDC->LineTo(rcButton.right-1, rcButton.top);
	pDC->MoveTo(rcButton.left+1, rcButton.bottom-1);
	pDC->LineTo(rcButton.right-1, rcButton.bottom-1);
	pDC->MoveTo(rcButton.left, rcButton.top+1);
	pDC->LineTo(rcButton.left, rcButton.bottom-1);
	pDC->MoveTo(rcButton.right-1, rcButton.top+1);
	pDC->LineTo(rcButton.right-1, rcButton.bottom-1);

	pDC->SelectObject(pOldPen);
	delete pPen;
}


/////////////////////////////////////////////////////////////////////////////
// CColorButtonEx class
// All buttons are initially unselected

CColorButtonEx::CColorButtonEx()
{
	m_hPal = _AfxInitPaletteEx();
	m_fSelected = FALSE;
}

// Set the face color of the button
void CColorButtonEx::SetFaceColor(COLORREF colFace)
{
	m_colFace = colFace;
}

// Return the palette reference for the button
COLORREF CColorButtonEx::colGetFaceColor()
{
	return m_colFace;
}

// Set a color button's selection state
void CColorButtonEx::SetState(BOOL fSelected)
{
	m_fSelected = fSelected;
}

// Static ID shows which color button generated the BN_CLICKED message
UINT CColorButtonEx::idClicked = 0;

// Redraw the color button, and record a change in selection status
void CColorButtonEx::DrawItem(LPDRAWITEMSTRUCT lpDrawItemStruct)
{
	CDC *pDC;
	CPalette *pPal;
	CRect rcButton;
	CBrush *pBrush;

	// Get the palette for the item display context
	pPal = CPalette::FromHandle(m_hPal);
	VERIFY(pPal);

	// Get the device context from the context
	pDC = CDC::FromHandle(lpDrawItemStruct->hDC);
	VERIFY(pDC);

	VERIFY(pDC->SelectPalette(pPal,0));
	pDC->RealizePalette();

	switch(lpDrawItemStruct->itemAction)
	{
		case ODA_SELECT:
			if (CButton::GetState() & 0x0004)
			{
				idClicked = lpDrawItemStruct->CtlID;
				// Redraw done via the BN_CLICKED notification
			}
			break;

		case ODA_DRAWENTIRE:
			rcButton = lpDrawItemStruct->rcItem;
			rcButton.InflateRect(-2, -2);
			if (m_fSelected)
			{
				_AfxDraw3DFrameEx(pDC, rcButton, RGB_BUTTON_DARK, RGB_BUTTON_WHITE);
				rcButton.InflateRect(-1, -1);
				_AfxDraw3DFrameEx(pDC, rcButton, RGB_BUTTON_DARK, RGB_BUTTON_WHITE);
				rcButton.InflateRect(-1, -1);
			}

			// Then the button face in the correct color
			pBrush = new CBrush(/*pDC->GetNearestColor(m_colFace)*/m_colFace);
			pDC->FillRect(&rcButton, pBrush);
			delete pBrush;

			// Drop through to draw Frame.
		case ODA_FOCUS:
			rcButton = lpDrawItemStruct->rcItem;
			_AfxDraw3DButtonFrameEx(pDC, rcButton, GetState() & 0x0008);
	}
}
