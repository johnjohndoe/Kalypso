// GrphBox.cpp : implementation file
//
#include "stdafx.h"
#include "Grphbox.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CGraphicComboBox

CGraphicComboBox::CGraphicComboBox() : CComboBox()
{
	pDC = NULL;
	pOldBmp = NULL;
	m_nItems = 0;
	m_nItemHeight = 18;
}

CGraphicComboBox::~CGraphicComboBox()
{
}

void CGraphicComboBox::Init( const BOOL bOneMore )
// Füllt die ComboBox
// Parameter:
//        const BOOL bOneMore: falls TRUE; wird noch ein Element mehr erzeugt
//                          ( bei der LineComboBox bewirkt dies, dass auch noch PS_NULL angezeigt wird )
{
  int nItems = m_nItems;
  if( bOneMore )
    nItems++;
  
  for( int i = 0; i < nItems; i++ )
		AddString(" ");
	SetItemHeight( -1, m_nItemHeight );
	SetCurSel( 0 );	// Override this if necessary!
}

void CGraphicComboBox::PrepareDC(CDC& dc, LPDRAWITEMSTRUCT lpDIS)
{
	CBrush *pBrush;
	COLORREF crBk, crText;
	CRect rcDC;

	// Get the device context from the context
	pDC = CDC::FromHandle(lpDIS->hDC);
	VERIFY(pDC);

	// Calculate the colors to use
	crBk = pDC->SetBkColor(GetSysColor(lpDIS->itemState & ODS_SELECTED ? COLOR_HIGHLIGHT : COLOR_WINDOW));
	crText = pDC->SetTextColor(GetSysColor(lpDIS->itemState & ODS_SELECTED ? COLOR_HIGHLIGHTTEXT : COLOR_WINDOWTEXT));

	rcItem = lpDIS->rcItem;

	// Create a compatible bitmap and DC
	VERIFY(bmp.CreateCompatibleBitmap(pDC, rcItem.Width(), rcItem.Height()));
	VERIFY(dc.CreateCompatibleDC(pDC));
	dc.SetBkColor(pDC->GetBkColor());
	dc.SetTextColor(pDC->GetTextColor());

	// Put the colors back as they were
	pDC->SetTextColor(crText);
	pDC->SetBkColor(crBk);

	// Select the bitmap
	pOldBmp = dc.SelectObject(&bmp);

	// Paint the bitmap with the background colour
	rcDC.SetRect(0, 0, rcItem.Width(), rcItem.Height());
	pBrush = new CBrush(dc.GetNearestColor(dc.GetBkColor()));
	dc.FillRect(&rcDC, pBrush);
	delete pBrush;
}

void CGraphicComboBox::OutputDC(CDC& dc)
{
	// Copy the bitmap to the output DC
	pDC->BitBlt(rcItem.TopLeft().x, rcItem.TopLeft().y, rcItem.Width(), rcItem.Height(), &dc, 0, 0, SRCCOPY);

	// Clean up
	dc.SelectObject(pOldBmp);
	bmp.DeleteObject();
}

BEGIN_MESSAGE_MAP(CGraphicComboBox, CComboBox)
	//{{AFX_MSG_MAP(CGraphicComboBox)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CGraphicComboBox message handlers

int CGraphicComboBox::CompareItem(LPCOMPAREITEMSTRUCT /*lpCompareItemStruct*/) 
{
	return 0;	// = item 1 and item 2 sort the same
}

void CGraphicComboBox::MeasureItem(LPMEASUREITEMSTRUCT lpMeasureItemStruct) 
{
	lpMeasureItemStruct->itemHeight = m_nItemHeight;
}

