// lctrlex.cpp: Implementierungsdatei
//
#include "stdafx.h"

#include "lctrlex.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

BOOL IsInteger( const CString& intStr )
// testes, ob die Zahl ein integer ist
{
  
  CString intStr2 = intStr;
  intStr2.TrimLeft();
  intStr2.TrimRight();

  CString newString = intStr2.SpanIncluding( TEXT("0123456789") );
  if( intStr2.IsEmpty() || newString.GetLength() < intStr2.GetLength() )
    return FALSE;
  else
    return TRUE;
}; // IsInteger

BOOL AFXAPI AfxSimpleFloatParse(LPCTSTR lpszText, double& d)
{
	ASSERT(lpszText != NULL);
	while (*lpszText == ' ' || *lpszText == '\t')
		lpszText++;

	TCHAR chFirst = lpszText[0];
	d = _tcstod(lpszText, (LPTSTR*)&lpszText);
	if (d == 0.0 && chFirst != '0')
		return FALSE;   // could not convert
	while (*lpszText == ' ' || *lpszText == '\t')
		lpszText++;

	if (*lpszText != '\0')
		return FALSE;   // not terminated properly

	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// CListCtrlEx

CListCtrlEx::CListCtrlEx()
{
	m_bFullRowSel = FALSE;
	m_bGridList = FALSE;
	m_bStateIcons = FALSE;
	m_bClientWidthSel = TRUE;

	m_nColumnSel = -1;
	m_nRowSel = -1;
	m_nCurrentCtrlId = 0;
	m_bParseOK = TRUE;

	m_cxClient = 0;
	m_cxStateImageOffset = 0;

	m_clrText = ::GetSysColor(COLOR_WINDOWTEXT);
	m_clrTextBk = ::GetSysColor(COLOR_WINDOW);
	m_clrBkgnd = ::GetSysColor(COLOR_WINDOW);
}

CListCtrlEx::~CListCtrlEx()
{
}


BEGIN_MESSAGE_MAP(CListCtrlEx, CListCtrl)
	//{{AFX_MSG_MAP(CListCtrlEx)
	ON_WM_SIZE()
	ON_WM_PAINT()
	ON_WM_SETFOCUS()
	ON_WM_KILLFOCUS()
	ON_WM_LBUTTONDOWN()
	ON_EN_KILLFOCUS(LCTRLEX_EDIT, OnKillFocusEdit)
	ON_CBN_KILLFOCUS(LCTRLEX_DROPDOWN, OnKillFocusDropDown)
	ON_CBN_KILLFOCUS(LCTRLEX_DROPDOWNLIST, OnKillFocusDropDownList)
	ON_WM_HSCROLL()
	ON_WM_VSCROLL()
	ON_WM_KEYDOWN()
	//}}AFX_MSG_MAP
	ON_MESSAGE(LVM_SETIMAGELIST, OnSetImageList)
	ON_MESSAGE(LVM_SETTEXTCOLOR, OnSetTextColor)
	ON_MESSAGE(LVM_SETTEXTBKCOLOR, OnSetTextBkColor)
	ON_MESSAGE(LVM_SETBKCOLOR, OnSetBkColor)
END_MESSAGE_MAP()

BOOL CListCtrlEx::SetFullRowSel(BOOL bFullRowSel)
{
	// no painting during change
	LockWindowUpdate();

	m_bFullRowSel = bFullRowSel;
	m_bGridList = FALSE;

	BOOL bRet;

	if (m_bFullRowSel)
		bRet = ModifyStyle(0L, LVS_OWNERDRAWFIXED);
	else
		bRet = ModifyStyle(LVS_OWNERDRAWFIXED, 0L);

	// repaint window if we are not changing view type
	if (bRet && (GetStyle() & LVS_TYPEMASK) == LVS_REPORT)
		Invalidate();

	// repaint changes
	UnlockWindowUpdate();

	return(bRet);
}

BOOL CListCtrlEx::GetFullRowSel()
{
	return(m_bFullRowSel);
}

BOOL CListCtrlEx::SetGridList(BOOL bGridList)
{
	// no painting during change
	LockWindowUpdate();

	m_bGridList = bGridList;
	m_bFullRowSel = FALSE;

	BOOL bRet;

	if (m_bGridList)
		bRet = ModifyStyle(0L, LVS_OWNERDRAWFIXED);
	else
		bRet = ModifyStyle(LVS_OWNERDRAWFIXED, 0L);

	// repaint window if we are not changing view type
	if (bRet && (GetStyle() & LVS_TYPEMASK) == LVS_REPORT)
		Invalidate();

	// repaint changes
	UnlockWindowUpdate();

	return(bRet);
}

BOOL CListCtrlEx::GetGridList()
{
	return(m_bGridList);
}

void CListCtrlEx::SetStateIcons(BOOL bStateIcons)
{
	m_bStateIcons = bStateIcons;
}

void CListCtrlEx::SetCheck(int nIndex, BOOL bChecked)
{
	SetItemState(nIndex, INDEXTOSTATEIMAGEMASK(bChecked ? 2 : 1), LVIS_STATEIMAGEMASK);
}

BOOL CListCtrlEx::GetCheck(int nIndex)
{
	return (BOOL)(GetItemState(nIndex, LVIS_STATEIMAGEMASK)==INDEXTOSTATEIMAGEMASK(2));
}

void CListCtrlEx::StartEdit(int nItem, int nSubItem)
{
	DeactivateControl();
	m_nRowSel = nItem;
	m_nColumnSel = nSubItem;
	ActivateControl();
}

BOOL CListCtrlEx::SetItemControl(int nItem, int nSubItem, int nControlType, LPCSTR lpszStrings /*=NULL*/, DWORD dwType /*=0*/)
{
	CString key, data;
	CRect rect(0, 1, 1, 0);
	LV_ITEM lvi;

	lvi.mask = LVIF_PARAM;
	lvi.iItem = nItem;
	lvi.iSubItem = nSubItem;
	if (!GetItem(&lvi))
		return FALSE;

	key.Format("%d,%d", nItem, nSubItem);
	// create an invisible control
	switch (nControlType)
	{
		case LCTRLEX_EDIT:
			if (m_edit.GetSafeHwnd()==NULL)
			{
				m_edit.Create(WS_CHILD | WS_VISIBLE | ES_AUTOHSCROLL | ES_LEFT, rect, this, LCTRLEX_EDIT);
				m_edit.ShowWindow(FALSE);
			}
			if (dwType!=0)
				m_datatypes.SetAt(key, dwType);
			break;

		case LCTRLEX_DROPDOWN:
			if (m_dropdown.GetSafeHwnd()==NULL)
			{
				m_dropdown.Create(WS_CHILD | WS_VISIBLE | WS_VSCROLL | CBS_OWNERDRAWVARIABLE | CBS_HASSTRINGS | CBS_AUTOHSCROLL | CBS_DROPDOWN, rect, this, LCTRLEX_DROPDOWN);
				m_dropdown.ShowWindow(FALSE);
			}
			if (lpszStrings!=NULL)
			{
				data = lpszStrings;
				m_strings.SetAt(key, data);
			}
			if (dwType!=0)
				m_datatypes.SetAt(key, dwType);
			break;

		case LCTRLEX_DROPDOWNLIST:
			if (m_dropdownlist.GetSafeHwnd()==NULL)
			{
				m_dropdownlist.Create(WS_CHILD | WS_VISIBLE | WS_VSCROLL | CBS_OWNERDRAWVARIABLE | CBS_HASSTRINGS | CBS_AUTOHSCROLL | CBS_DROPDOWNLIST, rect, this, LCTRLEX_DROPDOWNLIST);
				m_dropdownlist.ShowWindow(FALSE);
			}
			if (lpszStrings!=NULL)
			{
				data = lpszStrings;
				m_strings.SetAt(key, data);
			}
			break;

		default:
			return TRUE;
	}
	m_ctrls.SetAt(key, nControlType);

	return TRUE;
}

BOOL CListCtrlEx::SetItemColor(int nItem, int nSubItem, COLORREF clrText, COLORREF clrTextBk)
{
	CString key;
	LV_ITEM lvi;

	lvi.mask = LVIF_PARAM;
	lvi.iItem = nItem;
	lvi.iSubItem = nSubItem;
	if (!GetItem(&lvi))
		return FALSE;

	key.Format("%d,%d", nItem, nSubItem);
	m_textcolors.SetAt(key, clrText);
	m_bkcolors.SetAt(key, clrTextBk);
	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// CListCtrlEx drawing

// offsets for first and other columns
#define OFFSET_FIRST	2
#define OFFSET_OTHER	6

void CListCtrlEx::DrawItem(LPDRAWITEMSTRUCT lpDrawItemStruct)
{
	CDC* pDC = CDC::FromHandle(lpDrawItemStruct->hDC);
	CRect rcItem(lpDrawItemStruct->rcItem);
	UINT uiFlags = ILD_TRANSPARENT;
	CImageList* pImageList;
	int nItem = lpDrawItemStruct->itemID;
	BOOL bFocus = (GetFocus() == this);
	BOOL bEnabled = IsWindowEnabled();
	COLORREF clrTextSave, clrBkSave;
	COLORREF clrImage = m_clrBkgnd;
	COLORREF clrText, clrBk;
	static _TCHAR szBuff[MAX_PATH];
	LPCTSTR pszText;
	CString key;

// get item data

	LV_ITEM lvi;
	lvi.mask = LVIF_TEXT | LVIF_IMAGE | LVIF_STATE;
	lvi.iItem = nItem;
	lvi.iSubItem = 0;
	lvi.pszText = szBuff;
	lvi.cchTextMax = sizeof(szBuff);
	lvi.stateMask = 0xFFFF;		// get all state flags
	GetItem(&lvi);

	BOOL bSelected = (bFocus || (GetStyle() & LVS_SHOWSELALWAYS)) && lvi.state & LVIS_SELECTED;
	bSelected = bSelected || (lvi.state & LVIS_DROPHILITED);

// grid list has no selection displayed when control is active
	if (m_bGridList && nItem==m_nRowSel)
	{
		int ctrl;
		key.Format("%d,%d", nItem, m_nColumnSel);
		if (m_ctrls.Lookup(key, ctrl))
			bSelected = FALSE;
	}
	if (!bEnabled)
		bSelected = FALSE;
	if (GetEditControl()!=NULL)
		bSelected = FALSE;

// set colors if item is selected

	CRect rcAllLabels;
	GetItemRect(nItem, rcAllLabels, LVIR_BOUNDS);

	CRect rcLabel;
	GetItemRect(nItem, rcLabel, LVIR_LABEL);

	rcAllLabels.left = rcLabel.left;
	if (m_bClientWidthSel && rcAllLabels.right<m_cxClient)
		rcAllLabels.right = m_cxClient;

	if (bSelected)
	{
		clrTextSave = pDC->SetTextColor(::GetSysColor(COLOR_HIGHLIGHTTEXT));
		clrBkSave = pDC->SetBkColor(::GetSysColor(COLOR_HIGHLIGHT));
		
		pDC->FillRect(rcAllLabels, &CBrush(::GetSysColor(COLOR_HIGHLIGHT)));
	}
	else if (!bEnabled)
	{
		clrTextSave = pDC->SetTextColor(::GetSysColor(COLOR_GRAYTEXT));
		clrBkSave = pDC->SetBkColor(::GetSysColor(COLOR_BTNFACE));
		
		pDC->FillRect(rcAllLabels, &CBrush(::GetSysColor(COLOR_BTNFACE)));
	}
	else
		pDC->FillRect(rcAllLabels, &CBrush(m_clrTextBk));
	
	// set color and mask for the icon
	
	if (lvi.state & LVIS_CUT)
	{
		clrImage = m_clrBkgnd;
		uiFlags |= ILD_BLEND50;
	}
	else if (bSelected)
	{
		clrImage = ::GetSysColor(COLOR_HIGHLIGHT);
		uiFlags |= ILD_BLEND50;
	}
	
	// draw state icon
	
	UINT nStateImageMask = lvi.state & LVIS_STATEIMAGEMASK;
	if (nStateImageMask)
	{
		int nImage = (nStateImageMask>>12) - 1;
		pImageList = GetImageList(LVSIL_STATE);
		if (pImageList)
		{
			pImageList->Draw(pDC, nImage,
				CPoint(rcItem.left, rcItem.top), ILD_TRANSPARENT);
		}
	}
	
	// draw normal and overlay icon
	
	CRect rcIcon;
	GetItemRect(nItem, rcIcon, LVIR_ICON);
	
	pImageList = GetImageList(LVSIL_SMALL);
	if (pImageList)
	{
		UINT nOvlImageMask=lvi.state & LVIS_OVERLAYMASK;
		if (rcItem.left<rcItem.right-1)
		{
			ImageList_DrawEx(pImageList->m_hImageList, lvi.iImage,
				pDC->m_hDC,rcIcon.left,rcIcon.top, rcIcon.Width(), rcIcon.Height()-1,
				m_clrBkgnd, clrImage, uiFlags | nOvlImageMask);
		}
	}
	
	// draw item label
	
	GetItemRect(nItem, rcItem, LVIR_LABEL);
	rcItem.right -= m_cxStateImageOffset;
	
	pszText = MakeShortString(pDC, szBuff,
		rcItem.right-rcItem.left, 2*OFFSET_FIRST);

	rcItem.right += m_cxStateImageOffset;
	rcLabel = rcItem;
	rcLabel.left += OFFSET_FIRST;
	rcLabel.right -= OFFSET_FIRST;
	
	// set user defined colors for this cell when not selected

	key.Format("%d,%d", nItem, 0);
	if (!bSelected && bEnabled)
	{
		if (m_bkcolors.Lookup(key, clrBk))
			pDC->FillRect(rcLabel, &CBrush(clrBk));
		if (m_textcolors.Lookup(key, clrText))
			clrBk = pDC->SetTextColor(clrText);
	}

	pDC->DrawText(pszText,-1,rcLabel,DT_LEFT | DT_SINGLELINE | DT_NOPREFIX | DT_NOCLIP | DT_VCENTER);
	
	// set original colors if not selected
	
	if (!bSelected && bEnabled)
	{
		if (m_textcolors.Lookup(key, clrText))
			pDC->SetTextColor(clrBk);
	}

	if (m_bGridList)
		pDC->FrameRect(rcItem, &CBrush(m_clrText));
	
	// draw labels for extra columns
	
	LV_COLUMN lvc;
	lvc.mask = LVCF_FMT | LVCF_WIDTH;
	
	for(int nColumn = 1; GetColumn(nColumn, &lvc); nColumn++)
	{
		rcItem.left = rcItem.right;
		rcItem.right += lvc.cx;
		
		szBuff[0] = '\0';
		int nRetLen = GetItemText(nItem, nColumn,
			szBuff, sizeof(szBuff));
		
		pszText = MakeShortString(pDC, szBuff,
			rcItem.right - rcItem.left, 2*OFFSET_OTHER);
		
		UINT nJustify = DT_LEFT;
		
		if(pszText == szBuff)
		{
			switch(lvc.fmt & LVCFMT_JUSTIFYMASK)
			{
			case LVCFMT_RIGHT:
				nJustify = DT_RIGHT;
				break;
			case LVCFMT_CENTER:
				nJustify = DT_CENTER;
				break;
			default:
				break;
			}
		}
		
		rcLabel = rcItem;
		rcLabel.left += OFFSET_OTHER;
		rcLabel.right -= OFFSET_OTHER;
		
		// set user defined colors for this cell when not selected
		
		key.Format("%d,%d", nItem, nColumn);
		if (!bSelected && bEnabled)
		{
			if (m_bkcolors.Lookup(key, clrBk))
				pDC->FillRect(rcLabel, &CBrush(clrBk));
			if (m_textcolors.Lookup(key, clrText))
				clrBk = pDC->SetTextColor(clrText);
		}

		pDC->DrawText(pszText, -1, rcLabel,
			nJustify | DT_SINGLELINE | DT_NOPREFIX | DT_NOCLIP | DT_VCENTER);

		if (m_bGridList)
			pDC->FrameRect(rcItem, &CBrush(m_clrText));

		// set original colors if not selected
		
		if (!bSelected && bEnabled)
		{
			if (m_textcolors.Lookup(key, clrText))
				pDC->SetTextColor(clrBk);
		}
	}
	
	// draw focus rectangle if item has focus
	
	if (lvi.state & LVIS_FOCUSED && bFocus)
		pDC->DrawFocusRect(rcAllLabels);
	
	// set original colors if item was selected
	
	if (bSelected || !bEnabled)
	{
		pDC->SetTextColor(clrTextSave);
		pDC->SetBkColor(clrBkSave);
	}
}

BOOL CListCtrlEx::OnChildNotify(UINT message, WPARAM wParam, LPARAM lParam,
	LRESULT* pResult)
{
	if (message != WM_DRAWITEM)
		return CWnd::OnChildNotify(message, wParam, lParam, pResult);

	ASSERT(pResult == NULL);       // no return value expected
	UNUSED(pResult); // unused in release builds
	
	DrawItem((LPDRAWITEMSTRUCT)lParam);

	return TRUE;
}

LPCTSTR CListCtrlEx::MakeShortString(CDC* pDC, LPCTSTR lpszLong, int nColumnLen, int nOffset)
{
	static const _TCHAR szThreeDots[] = _T("...");

	int nStringLen = lstrlen(lpszLong);

	if(nStringLen == 0 ||
		(pDC->GetTextExtent(lpszLong, nStringLen).cx + nOffset) <= nColumnLen)
	{
		return(lpszLong);
	}

	static _TCHAR szShort[MAX_PATH];

	lstrcpy(szShort,lpszLong);
	int nAddLen = pDC->GetTextExtent(szThreeDots,sizeof(szThreeDots)).cx;

	for(int i = nStringLen-1; i > 0; i--)
	{
		szShort[i] = 0;
		if((pDC->GetTextExtent(szShort, i).cx + nOffset + nAddLen)
			<= nColumnLen)
		{
			break;
		}
	}

	lstrcat(szShort, szThreeDots);
	return(szShort);
}

void CListCtrlEx::RepaintSelectedItems()
{
	CRect rcItem, rcLabel;

// invalidate focused item so it can repaint properly

	int nItem = GetNextItem(-1, LVNI_FOCUSED);

	if(nItem != -1)
	{
		GetItemRect(nItem, rcItem, LVIR_BOUNDS);
		GetItemRect(nItem, rcLabel, LVIR_LABEL);
		rcItem.left = rcLabel.left;

		InvalidateRect(rcItem, FALSE);
	}

// if selected items should not be preserved, invalidate them

	if(!(GetStyle() & LVS_SHOWSELALWAYS))
	{
		for(nItem = GetNextItem(-1, LVNI_SELECTED);
			nItem != -1; nItem = GetNextItem(nItem, LVNI_SELECTED))
		{
			GetItemRect(nItem, rcItem, LVIR_BOUNDS);
			GetItemRect(nItem, rcLabel, LVIR_LABEL);
			rcItem.left = rcLabel.left;

			InvalidateRect(rcItem, FALSE);
		}
	}

// update changes 

	UpdateWindow();
}

/////////////////////////////////////////////////////////////////////////////
// CListViewEx diagnostics

#ifdef _DEBUG

void CListCtrlEx::Dump(CDumpContext& dc) const
{
	CListCtrl::Dump(dc);

	dc << "m_bFullRowSel = " << (UINT)m_bFullRowSel;
	dc << "\n";
	dc << "m_cxStateImageOffset = " << m_cxStateImageOffset;
	dc << "\n";
}

#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CListCtrlEx 

BOOL CListCtrlEx::PreCreateWindow(CREATESTRUCT& cs) 
{
	// default is report view and full row selection
	cs.style &= ~LVS_TYPEMASK;
	cs.style |= LVS_REPORT | LVS_OWNERDRAWFIXED;
	m_bFullRowSel = TRUE;

	return CListCtrl::PreCreateWindow(cs);
}

BOOL CListCtrlEx::PreTranslateMessage(MSG* pMsg) 
{
	if (pMsg->message==WM_KEYDOWN && pMsg->wParam==VK_TAB)
	{
		CWnd *pWnd = GetFocus();
		if (pWnd==this || m_nCurrentCtrlId!=0)
		{
			if (DoTabDown((BOOL)(::GetKeyState(VK_SHIFT) & 0x8000)))
				return TRUE;
		}
	}
	
	return CListCtrl::PreTranslateMessage(pMsg);
}

LRESULT CListCtrlEx::OnSetImageList(WPARAM wParam, LPARAM lParam)
{
	if(m_bStateIcons)
	{
		int cx, cy;

		if(::ImageList_GetIconSize((HIMAGELIST)lParam, &cx, &cy))
			m_cxStateImageOffset = cx;
		else
			m_cxStateImageOffset = 0;
	}

	return(Default());
}

LRESULT CListCtrlEx::OnSetTextColor(WPARAM wParam, LPARAM lParam)
{
	m_clrText = (COLORREF)lParam;
	return(Default());
}

LRESULT CListCtrlEx::OnSetTextBkColor(WPARAM wParam, LPARAM lParam)
{
	m_clrTextBk = (COLORREF)lParam;
	return(Default());
}

LRESULT CListCtrlEx::OnSetBkColor(WPARAM wParam, LPARAM lParam)
{
	m_clrBkgnd = (COLORREF)lParam;
	return(Default());
}

void CListCtrlEx::OnSize(UINT nType, int cx, int cy) 
{
	m_cxClient = cx;
	CListCtrl::OnSize(nType, cx, cy);
}

void CListCtrlEx::OnPaint() 
{
	// in full row select mode, we need to extend the clipping region
	// so we can paint a selection all the way to the right
	if (m_bClientWidthSel &&
		(GetStyle() & LVS_TYPEMASK) == LVS_REPORT &&
		(GetFullRowSel() || GetGridList()))
	{
		CRect rcAllLabels;
		GetItemRect(0, rcAllLabels, LVIR_BOUNDS);

		if(rcAllLabels.right < m_cxClient)
		{
			// need to call BeginPaint (in CPaintDC c-tor)
			// to get correct clipping rect
			CPaintDC dc(this);

			CRect rcClip;
			dc.GetClipBox(rcClip);

			rcClip.left = min(rcAllLabels.right-1, rcClip.left);
			rcClip.right = m_cxClient;

			InvalidateRect(rcClip, FALSE);
			// EndPaint will be called in CPaintDC d-tor
		}
	}

	CListCtrl::OnPaint();
}

void CListCtrlEx::OnSetFocus(CWnd* pOldWnd) 
{
	CListCtrl::OnSetFocus(pOldWnd);

	// check if we are getting focus from label edit box
	if(pOldWnd!=NULL && ::IsWindow(pOldWnd->GetSafeHwnd()) && pOldWnd->GetParent()==this)
		return;

	// repaint items that should change appearance
	if(m_bFullRowSel && (GetStyle() & LVS_TYPEMASK)==LVS_REPORT)
		RepaintSelectedItems();
}

void CListCtrlEx::OnKillFocus(CWnd* pNewWnd) 
{
	CListCtrl::OnKillFocus(pNewWnd);
	
	// check if we are losing focus to label edit box
	if(pNewWnd != NULL && pNewWnd->GetParent() == this)
		return;

	// repaint items that should change appearance
	if(m_bFullRowSel && (GetStyle() & LVS_TYPEMASK) == LVS_REPORT)
		RepaintSelectedItems();
}

void CListCtrlEx::OnLButtonDown(UINT nFlags, CPoint point)
{
	UINT uFlags = 0;
	int nHitItem = HitTest(point, &uFlags);

	// we need additional checking in owner-draw mode
	// because we only get LVHT_ONITEM
	BOOL bHit = FALSE;
	if (uFlags == LVHT_ONITEM && (GetStyle() & LVS_OWNERDRAWFIXED))
	{
		CRect rect;
		GetItemRect(nHitItem, rect, LVIR_ICON);

		// check if hit was on a state icon
		if (m_bStateIcons && point.x < rect.left)
			bHit = TRUE;
	}
	else if (uFlags & LVHT_ONITEMSTATEICON)
		bHit = TRUE;

	if (bHit)
		SetCheck(nHitItem, !GetCheck(nHitItem));
	else
	{
		if (m_bGridList)
		{
			if (!DeactivateControl())
				return;
			LockWindowUpdate();
		}
		CListCtrl::OnLButtonDown(nFlags, point);
		if (m_bGridList)
		{
			CRect rcItem;
			UINT flags;
			LV_COLUMN lvc;
			int iItem;
			
			UnlockWindowUpdate();
			lvc.mask = LVCF_FMT | LVCF_WIDTH;
			iItem = HitTest(point, &flags);
			m_nColumnSel = -1;
			m_nRowSel = iItem;
			if (flags & LVHT_ONITEM)
			{
				GetItemRect(iItem, rcItem, LVIR_LABEL);
				rcItem.right = rcItem.left + GetColumnWidth(0);
				
				if (rcItem.PtInRect(point))
					m_nColumnSel = 0;
				else
				{
					for(int nColumn = 1; GetColumn(nColumn, &lvc); nColumn++)
					{
						rcItem.left = rcItem.right;
						rcItem.right += lvc.cx;
						if (rcItem.PtInRect(point))
						{
							m_nColumnSel = nColumn;
							break;
						}
					}
				}
			}
			ActivateControl();
		}
	}
}

BOOL CListCtrlEx::DeactivateControl()
{
	BOOL bRet = TRUE;

	if (m_bGridList)
	{
		if (m_nColumnSel!=-1)
		{
			m_bParseOK = TRUE;
			if (m_edit.GetSafeHwnd()!=NULL)
				m_edit.SendMessage(WM_KILLFOCUS, (LPARAM)GetSafeHwnd());
			if (m_dropdown.GetSafeHwnd()!=NULL)
				m_dropdown.SendMessage(WM_KILLFOCUS, (LPARAM)GetSafeHwnd());
			if (m_dropdownlist.GetSafeHwnd()!=NULL)
				m_dropdownlist.SendMessage(WM_KILLFOCUS, (LPARAM)GetSafeHwnd());
			if (!m_bParseOK)
			{
				m_bParseOK = TRUE;
				bRet = FALSE;
			}
		}
	}

	return bRet;
}

void CListCtrlEx::ActivateControl()
{
	if (m_bGridList)
	{
		if (m_nColumnSel!=-1)
		{
			CFont *pFont = GetFont();
			CString key, data;
			int ctrl;
			int height;
			LV_COLUMN lvc;
			CRect rcItem, rcAllLabels;
			CString str;
			
			lvc.mask = LVCF_FMT | LVCF_WIDTH;
			GetItemRect(m_nRowSel, rcItem, LVIR_LABEL);
			rcItem.right = rcItem.left + GetColumnWidth(0);
			if (m_bStateIcons)
				rcItem.right -= m_cxStateImageOffset;
			
			if (m_nColumnSel!=0)
			{
				for(int nColumn = 1; GetColumn(nColumn, &lvc); nColumn++)
				{
					rcItem.left = rcItem.right;
					rcItem.right += lvc.cx;
					if (m_nColumnSel==nColumn)
						break;
				}
			}
			key.Format("%d,%d", m_nRowSel, m_nColumnSel);
			if (m_ctrls.Lookup(key, ctrl))
			{
				switch (ctrl)
				{
				case LCTRLEX_EDIT:
					rcItem.DeflateRect(2, 1);
					m_edit.MoveWindow(rcItem);
					m_edit.SetFont(pFont);
					str = GetItemText(m_nRowSel, m_nColumnSel);
					m_edit.SetWindowText(str);
					m_edit.SetFocus();
					m_edit.ShowWindow(TRUE);
					m_nCurrentCtrlId = ctrl;
					break;
					
				case LCTRLEX_DROPDOWN:
					rcItem.InflateRect(1, 1);
					rcItem -= CPoint(1, 1);
					height = rcItem.Height()-4;
					rcItem.bottom = rcItem.top + 8*height;
					m_dropdown.ResetContent();
					m_dropdown.MoveWindow(rcItem);
					m_dropdown.SetFont(pFont);
					m_dropdown.SetHeight(height);
					m_dropdown.Init();
					if (m_strings.Lookup(key, data))
					{
						int i;
						
						while ((i=data.Find("\n"))!=-1)
						{
							str = data.Left(i);
							data = data.Right(data.GetLength()-i-1);
							m_dropdown.AddString(str);
						}
					}
					str = GetItemText(m_nRowSel, m_nColumnSel);
					m_dropdown.SetWindowText(str);
					m_dropdown.ShowWindow(TRUE);
					m_dropdown.SetFocus();
					m_nCurrentCtrlId = ctrl;
					break;
					
				case LCTRLEX_DROPDOWNLIST:
					rcItem.InflateRect(1, 1);
					rcItem -= CPoint(1, 1);
					height = rcItem.Height()-4;
					rcItem.bottom = rcItem.top + 8*height;
					m_dropdownlist.ResetContent();
					m_dropdownlist.MoveWindow(rcItem);
					m_dropdownlist.SetFont(pFont);
					m_dropdownlist.SetHeight(height);
					m_dropdownlist.Init();
					if (m_strings.Lookup(key, data))
					{
						int i;
						
						while ((i=data.Find("\n"))!=-1)
						{
							str = data.Left(i);
							data = data.Right(data.GetLength()-i-1);
							m_dropdownlist.AddString(str);
						}
					}
					str = GetItemText(m_nRowSel, m_nColumnSel);
					m_dropdownlist.SelectString(-1, str);
					m_dropdownlist.ShowWindow(TRUE);
					m_dropdownlist.SetFocus();
					m_nCurrentCtrlId = ctrl;
					break;
					
				default:
					break;
				}
			}
			GetItemRect(m_nRowSel, rcAllLabels, LVIR_BOUNDS);
			InvalidateRect(&rcAllLabels);
		}
	}
}

BOOL CListCtrlEx::DoTabDown(BOOL bShift)
{
	int i, nRow, nCol, ctrl, nColCount;
	LV_COLUMN lvc;
	BOOL bCtrl = FALSE;
	CString key;
	
	if (!m_bGridList)
		return FALSE;

  // Tab löscht die aktuelle Auswahl
  POSITION pos = GetFirstSelectedItemPosition();
  int index = GetNextSelectedItem( pos );
  if( index != -1 )
    SetItemState( index, 0, LVIS_SELECTED );

	nRow = m_nRowSel;
	nCol = m_nColumnSel;
	lvc.mask = LVCF_FMT | LVCF_WIDTH;

	LockWindowUpdate();
	if (!DeactivateControl())
	{
		UnlockWindowUpdate();
		return TRUE;
	}

	for (i=0; ; i++)
	{
		if (!GetColumn(i, &lvc))
			break;
	}
	nColCount = i-1;

	if (nRow==-1)
	{
		if (bShift)
		{
			nRow = GetItemCount()-1;
			nCol = nColCount+1;
		}
		else
		{
			nRow = 0;
			nCol = -1;
		}
	}

	while (!bCtrl)
	{
		bShift ? nCol-- : nCol++;
		if (nCol<0)
		{
			nRow--;
			nCol = nColCount;
		}
		if (nCol>nColCount)
		{
			nRow++;
			nCol = 0;
		}
		if (nRow<0 || nRow>GetItemCount()-1)
		{
			UnlockWindowUpdate();
			return FALSE;
		}
		key.Format("%d,%d", nRow, nCol);
		if (m_ctrls.Lookup(key, ctrl))
			bCtrl = TRUE;
	}
	m_nRowSel = nRow;
	m_nColumnSel = nCol;
	EnsureVisible(nRow, FALSE);
	ActivateControl();
	UnlockWindowUpdate();

	return TRUE;
}

void CListCtrlEx::OnKillFocusEdit() 
{
	CString str, key;
	int type;
	
	if (m_edit.GetSafeHwnd()!=NULL && m_nCurrentCtrlId==LCTRLEX_EDIT)
	{
		m_edit.GetWindowText(str);
		key.Format("%d,%d", m_nRowSel, m_nColumnSel);
		if (m_datatypes.Lookup(key, type))
		{
			switch (type)
			{
				case STRING_FIELD:
					break;

				case EMPTYINT_FIELD:
					if (str.IsEmpty())
						break;
				case INT_FIELD:
					{
						if ( !IsInteger( str ) )
						{
							AfxMessageBox( AFX_IDP_PARSE_INT );
							m_edit.SetFocus();
							return;
						};
            str.Format( "%d", atoi( str ) );
					}
					break;

				case EMPTYDOUBLE_FIELD:
					if (str.IsEmpty())
						break;
				case DOUBLE_FIELD:
					{
            // das letzte Komma durch einen Punkt ersetzen, dann passts hoffentlich immer ( numerische locals wurden konstant auf englisch gesetzt )
            lconv* lc = ::localeconv();
            int i = str.ReverseFind( ',' );
            if( i != -1 )
              str = str.Left( i ) + lc->decimal_point + str.Mid( i + 1 );

            double d;

						if ( !AfxSimpleFloatParse( str, d ) )
						{
							AfxMessageBox(AFX_IDP_PARSE_REAL);
							m_edit.SetFocus();
							return;
						};
            str.Format( "%lf", d );
					}
					break;

				case DATE_FIELD:
					{
						COleDateTime value;

						m_bParseOK = value.ParseDateTime(str);

						if (!m_bParseOK)
						{
							AfxMessageBox(AFX_IDP_PARSE_DATETIME);
							m_edit.SetFocus();
							return;
						}
					}
					break;
			}
		}

    // Parent-Window Informieren: LVN_ENDLABELEDIT senden
    BOOL bReturn = TRUE;
    CWnd* pParent = GetParent();
    if( pParent )
    {
      NMLVDISPINFO dispinfo;
      dispinfo.item.mask = 0;
      dispinfo.item.iItem = m_nRowSel;
      dispinfo.item.iSubItem = m_nRowSel;
      dispinfo.item.state = 0;
      dispinfo.item.stateMask = 0;
      dispinfo.item.cchTextMax = 0;
      dispinfo.item.iImage = 0;
      dispinfo.item.lParam = 0;
      dispinfo.item.pszText = str.GetBuffer( str.GetLength() );
      dispinfo.hdr.hwndFrom = GetSafeHwnd();
      dispinfo.hdr.idFrom = GetDlgCtrlID();
      dispinfo.hdr.code = LVN_ENDLABELEDIT;

      str.ReleaseBuffer();

      bReturn = pParent->SendMessage( WM_NOTIFY, (WPARAM)GetDlgCtrlID(), (LPARAM)&(dispinfo.hdr) );
    };

		SetItemText(m_nRowSel, m_nColumnSel, str);
		m_edit.ShowWindow(FALSE);
		RedrawItems(m_nRowSel, m_nRowSel);
		m_nColumnSel = -1;
		m_nRowSel = -1;
		m_nCurrentCtrlId = 0;

	}
}

void CListCtrlEx::OnKillFocusDropDown() 
{
	CString str, key;
	int type;
	
	if (m_dropdown.GetSafeHwnd()!=NULL && m_nCurrentCtrlId==LCTRLEX_DROPDOWN)
	{
		m_dropdown.GetWindowText(str);
		key.Format("%d,%d", m_nRowSel, m_nColumnSel);
		if (m_datatypes.Lookup(key, type))
		{
			switch (type)
			{
				case STRING_FIELD:
					break;

				case EMPTYINT_FIELD:
					if (str.IsEmpty())
						break;
				case INT_FIELD:
					{
						UINT nIDPrompt = AFX_IDP_PARSE_INT;
						m_bParseOK = IsInteger( str );

						if (!m_bParseOK)
						{
							AfxMessageBox(nIDPrompt);
							m_dropdown.SetFocus();
							return;
						}
					}
					break;

				case EMPTYDOUBLE_FIELD:
					if (str.IsEmpty())
						break;
				case DOUBLE_FIELD:
					{
						lconv *lc;
						int i;
						double d;
						
						lc = ::localeconv();
						i = str.Find(',');
						if (i!=-1)
							str = str.Left(i) + lc->decimal_point + str.Right(str.GetLength()-i-1);
						m_bParseOK = AfxSimpleFloatParse(str, d);

						if (!m_bParseOK)
						{
							AfxMessageBox(AFX_IDP_PARSE_REAL);
							m_dropdown.SetFocus();
							return;
						}
					}
					break;

				case DATE_FIELD:
					{
						COleDateTime value;

						m_bParseOK = value.ParseDateTime(str);

						if (!m_bParseOK)
						{
							AfxMessageBox(AFX_IDP_PARSE_DATETIME);
							m_dropdown.SetFocus();
							return;
						}
					}
					break;
			}
		}
		SetItemText(m_nRowSel, m_nColumnSel, str);
		m_dropdown.ShowWindow(FALSE);
		RedrawItems(m_nRowSel, m_nRowSel);
		m_nColumnSel = -1;
		m_nRowSel = -1;
		m_nCurrentCtrlId = 0;
	}
}

void CListCtrlEx::OnKillFocusDropDownList() 
{
	CString str;
	
	if (m_dropdownlist.GetSafeHwnd()!=NULL && m_nCurrentCtrlId==LCTRLEX_DROPDOWNLIST)
	{
		m_dropdownlist.GetWindowText(str);
		SetItemText(m_nRowSel, m_nColumnSel, str);
		m_dropdownlist.ShowWindow(FALSE);
		RedrawItems(m_nRowSel, m_nRowSel);
		m_nColumnSel = -1;
		m_nRowSel = -1;
		m_nCurrentCtrlId = 0;
	}
}

void CListCtrlEx::OnHScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar) 
{
	if (m_edit.GetSafeHwnd()!=NULL)
		m_edit.SendMessage(WM_KILLFOCUS, (LPARAM)GetSafeHwnd());
	if (m_dropdown.GetSafeHwnd()!=NULL)
		m_dropdown.SendMessage(WM_KILLFOCUS, (LPARAM)GetSafeHwnd());
	if (m_dropdownlist.GetSafeHwnd()!=NULL)
		m_dropdownlist.SendMessage(WM_KILLFOCUS, (LPARAM)GetSafeHwnd());
	
	CListCtrl::OnHScroll(nSBCode, nPos, pScrollBar);
}

void CListCtrlEx::OnVScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar) 
{
	if (m_edit.GetSafeHwnd()!=NULL)
		m_edit.SendMessage(WM_KILLFOCUS, (LPARAM)GetSafeHwnd());
	if (m_dropdown.GetSafeHwnd()!=NULL)
		m_dropdown.SendMessage(WM_KILLFOCUS, (LPARAM)GetSafeHwnd());
	if (m_dropdownlist.GetSafeHwnd()!=NULL)
		m_dropdownlist.SendMessage(WM_KILLFOCUS, (LPARAM)GetSafeHwnd());
	
	CListCtrl::OnVScroll(nSBCode, nPos, pScrollBar);
}

void CListCtrlEx::OnKeyDown( UINT nChar, UINT nRepCnt, UINT nFlags ) 
{
  CWnd* pParent = GetParent();
  if( pParent )
    pParent->SendMessage( WM_KEYDOWN, (WPARAM)nChar, (LPARAM)nFlags );
	
	CListCtrl::OnKeyDown(nChar, nRepCnt, nFlags);
}; // OnKeyDown

BOOL CListCtrlEx::SetAllItemsState( UINT nState, UINT nMask )
// führt die Funktion SetItemState( i, nState, nMask ) für alle vorhandenen Items i aus
// Rückgabewert:
//            TRUE, falls alle SetItemsState TRUE zurückgaben, ansonsten FALSE
{
  BOOL bRet = TRUE;
  for( int i = 0; i < GetItemCount(); i++ )
    bRet &= SetItemState( i, nState, nMask );
  return bRet;
};


int CListCtrlEx::GetColumnCount()
// gibt die Anzahl der Spalten der ListCtrl zurück
{
  return this->GetHeaderCtrl()->GetItemCount();
}; // GetColumnCount()

void CListCtrlEx::AutoSizeColumns(int col /*=-1*/) 
// von: http://www.codeguru.com/listview/autosize_col.shtml, by Roger Onslow
{
  //SetRedraw(false);
  int mincol = col < 0 ? 0 : col;
  int maxcol = col < 0 ? GetColumnCount()-1 : col;
  for (col = mincol; col <= maxcol; col++) 
  {
    SetColumnWidth(col,LVSCW_AUTOSIZE);
    int wc1 = GetColumnWidth(col);
    SetColumnWidth(col,LVSCW_AUTOSIZE_USEHEADER);
    int wc2 = GetColumnWidth(col);
    int wc = max( MINCOLWIDTH, max( wc1,wc2 ) );
    SetColumnWidth(col,wc);
  }

  // RecalcHeaderTips(); *** uncomment this if you use my header tips method
  //   SetRedraw(true);
  Invalidate(); // *** uncomment this if you don't use my SetRedraw function
}; // AutoSizeColumns
