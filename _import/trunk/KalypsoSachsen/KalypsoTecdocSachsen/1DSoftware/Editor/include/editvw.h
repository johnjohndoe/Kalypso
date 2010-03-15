// editvw.h : interface of the CEditorView class
//
//

class CEditorView : public CRichEditView
{
protected: // create from serialization only
	CEditorView();
	DECLARE_DYNCREATE(CEditorView)

// Attributes
public:
	UINT m_uTimerID;
	BOOL m_bDelayUpdateItems;
	BOOL m_bInPrint;
	CParaFormat m_defParaFormat;
	CCharFormat m_defCharFormat;
	CCharFormat m_defTextCharFormat;

	CEditorDoc* GetDocument();
	BOOL IsFormatText();

	virtual HMENU GetContextMenu(WORD seltype, LPOLEOBJECT lpoleobj, 
		CHARRANGE* lpchrg);
// presentation attributes
	CFont* GetPrinterFont() const;
	void SetPrinterFont(CFont* pFont);
// Operations
public:
	BOOL PasteNative(LPDATAOBJECT lpdataobj);
	void SetDefaultFont(BOOL bText);
	void SetUpdateTimer();
	void GetDefaultFont(CCharFormat& cf, UINT nFontNameID);
	void DrawMargins(CDC* pDC);
	BOOL SelectPalette();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CEditorView)
	public:
	virtual void OnPrepareDC(CDC* pDC, CPrintInfo* pInfo = NULL);
	protected:
	virtual void CalcWindowRect(LPRECT lpClientRect, UINT nAdjustType = adjustBorder);
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	virtual void OnPrint(CDC* pDC, CPrintInfo* pInfo);
	virtual void OnBeginPrinting(CDC* pDC, CPrintInfo* pInfo);
	virtual void OnEndPrinting(CDC* pDC, CPrintInfo* pInfo);
	//}}AFX_VIRTUAL
	BOOL OnPreparePrinting(CPrintInfo* pInfo);
	virtual HRESULT GetClipboardData(CHARRANGE* lpchrg, DWORD reco, 
		LPDATAOBJECT lpRichDataObj,	LPDATAOBJECT* lplpdataobj);
	virtual HRESULT QueryAcceptData(LPDATAOBJECT, CLIPFORMAT*, DWORD, 
		BOOL, HGLOBAL);
public:
	virtual void WrapChanged();

// Implementation
public:

#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:
	// OLE Container support

	virtual void DeleteContents();
	virtual void OnTextNotFound(LPCTSTR);

// Generated message map functions
protected:
	HFONT m_hMirrorFont;
	HFONT m_hPrinterFont;
    static LOGFONT NEAR m_lfDefPrintFont;
    CFont defPrinterFont;
	afx_msg void OnCancelEditSrvr();
	//{{AFX_MSG(CEditorView)
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnPageSetup();
	afx_msg void OnFormatParagraph();
	afx_msg void OnTimer(UINT nIDEvent);
	afx_msg void OnDestroy();
	afx_msg void OnMeasureItem(int nIDCtl, LPMEASUREITEMSTRUCT lpMeasureItemStruct);
	afx_msg void OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags);
	afx_msg void OnFilePrint();
	afx_msg void OnPaletteChanged(CWnd* pFocusWnd);
	afx_msg BOOL OnQueryNewPalette();
	afx_msg void OnWinIniChange(LPCTSTR lpszSection);
	afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg void OnFilePrintPreview();
	afx_msg void OnFormatPagebreak();
	afx_msg void OnSetfocus();
	afx_msg void OnKillfocus();
	afx_msg void OnFormatFont();
	//}}AFX_MSG
	afx_msg void OnEditChange();
	afx_msg int OnMouseActivate(CWnd* pWnd, UINT nHitTest, UINT message);
	afx_msg LONG OnPrinterChangedMsg(UINT, LONG);
	afx_msg void OnUpdateIndicatorLine(CCmdUI* pCmdUI);
	DECLARE_MESSAGE_MAP()
};

#ifndef _DEBUG  // debug version in wordpvw.cpp
inline CEditorDoc* CEditorView::GetDocument()
   { return (CEditorDoc*)m_pDocument; }
#endif

/////////////////////////////////////////////////////////////////////////////
