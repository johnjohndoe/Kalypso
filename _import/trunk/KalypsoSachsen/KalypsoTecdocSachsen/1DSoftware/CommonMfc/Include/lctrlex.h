#ifndef AFX_LCTRLEX_H__EC8AC4E1_2ADE_11D3_A4B8_0080ADAC5D6B__INCLUDED_
#define AFX_LCTRLEX_H__EC8AC4E1_2ADE_11D3_A4B8_0080ADAC5D6B__INCLUDED_

#include "textbox.h"

// lctrlex.h : Header-Datei
//

// control types for grid list
#define LCTRLEX_NONE			0
#define LCTRLEX_EDIT			1
#define LCTRLEX_DROPDOWN		2
#define LCTRLEX_DROPDOWNLIST	3

#define STRING_FIELD			1
#define EMPTYINT_FIELD			2
#define INT_FIELD				3
#define EMPTYDOUBLE_FIELD		4
#define DOUBLE_FIELD			5
#define DATE_FIELD				6

// minimal column Width for AutoSizeColumns()
#define MINCOLWIDTH   10

/////////////////////////////////////////////////////////////////////////////
// Fenster CListCtrlEx 

class CListCtrlEx : public CListCtrl
{
// Konstruktion
public:
	CListCtrlEx();

// Attribute
protected:
	BOOL m_bFullRowSel;
	BOOL m_bGridList;
	BOOL m_bStateIcons;
	CMap<CString, LPCTSTR, int, int> m_ctrls;
	CMap<CString, LPCTSTR, CString, LPCTSTR> m_strings;		// for combo boxes
	CMap<CString, LPCTSTR, int, int> m_datatypes;			// for edit fields
	CMap<CString, LPCTSTR, COLORREF, COLORREF> m_textcolors;
	CMap<CString, LPCTSTR, COLORREF, COLORREF> m_bkcolors;

public:
	BOOL SetFullRowSel(BOOL bFillRowSel);
	BOOL GetFullRowSel();
	BOOL SetGridList(BOOL bGridList);
	BOOL GetGridList();
	void SetStateIcons(BOOL bStateIcons);

	BOOL SetItemControl(int nItem, int nSubItem, int nControlType, LPCSTR lpszStrings = NULL, DWORD dwType = 0);
	BOOL SetItemColor(int nItem, int nSubItem, COLORREF clrText, COLORREF clrTextBk);
	void SetCheck(int nIndex, BOOL bChecked);
	BOOL GetCheck(int nIndex);

  BOOL SetAllItemsState( UINT nState, UINT nMask );

	void StartEdit(int nItem, int nSubItem);

	BOOL m_bClientWidthSel;

	BOOL DeactivateControl();

  int GetColumnCount();
  void AutoSizeColumns( int col = -1 );

  int GetSelectedColumn() { return m_nColumnSel; }


// Operationen
public:
	virtual void DrawItem(LPDRAWITEMSTRUCT lpDrawItemStruct);

// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CListCtrlEx)
	public:
	virtual BOOL PreTranslateMessage(MSG* pMsg);
	protected:
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	//}}AFX_VIRTUAL

// Implementierung
public:
	virtual ~CListCtrlEx();
#ifdef _DEBUG
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:
	virtual BOOL OnChildNotify(UINT, WPARAM, LPARAM, LRESULT*);
	static LPCTSTR MakeShortString(CDC* pDC, LPCTSTR lpszLong, int nColumnLen, int nOffset);
	void RepaintSelectedItems();
	void ActivateControl();
	BOOL DoTabDown(BOOL bShift);

// Implementation - grid list
	int m_nColumnSel, m_nRowSel;
	CEdit m_edit;
	CTextComboBox m_dropdown;
	CTextComboBox m_dropdownlist;
	int m_nCurrentCtrlId;
	BOOL m_bParseOK;

// Implementation - client area width
	int m_cxClient;

// Implementation - state icon width
	int m_cxStateImageOffset;
	afx_msg LRESULT OnSetImageList(WPARAM wParam, LPARAM lParam);

// Implementation - list view colors
	COLORREF m_clrText;
	COLORREF m_clrTextBk;
	COLORREF m_clrBkgnd;
	afx_msg LRESULT OnSetTextColor(WPARAM wParam, LPARAM lParam);
	afx_msg LRESULT OnSetTextBkColor(WPARAM wParam, LPARAM lParam);
	afx_msg LRESULT OnSetBkColor(WPARAM wParam, LPARAM lParam);

	// Generierte Nachrichtenzuordnungsfunktionen
protected:
	//{{AFX_MSG(CListCtrlEx)
	afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg void OnPaint();
	afx_msg void OnSetFocus(CWnd* pOldWnd);
	afx_msg void OnKillFocus(CWnd* pNewWnd);
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnKillFocusEdit();
	afx_msg void OnKillFocusDropDown();
	afx_msg void OnKillFocusDropDownList();
	afx_msg void OnHScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar);
	afx_msg void OnVScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar);
	afx_msg void OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // AFX_LCTRLEX_H__EC8AC4E1_2ADE_11D3_A4B8_0080ADAC5D6B__INCLUDED_
