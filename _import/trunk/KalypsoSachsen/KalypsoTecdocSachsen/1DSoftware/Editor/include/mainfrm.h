// mainfrm.h : interface of the CMainFrame class
//

class CMainFrame : public CMDIFrameWnd
{
	DECLARE_DYNCREATE(CMainFrame)
public:
	CMainFrame();

// Attributes
public:
	HICON m_hIconDoc;
	HICON m_hIconText;
	HICON m_hIconWrite;
	HICON GetIcon(int nDocType);

// Operations
public:
	void CloseProject();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CMainFrame)
	public:
	virtual void ActivateFrame(int nCmdShow = -1);
	protected:
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CMainFrame();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif
	virtual void OnUpdateFrameTitle(BOOL bAddToTitle);

public:
	CToolBar	m_wndToolBar;
	CStatusBar  m_wndStatusBar;
protected:  // control bar embedded members
	BOOL CreateToolBar();
	BOOL CreateStatusBar();
// Generated message map functions
protected:
	//{{AFX_MSG(CMainFrame)
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg void OnMove(int x, int y);
	afx_msg void OnHelpFinder();
	afx_msg void OnDropFiles(HDROP hDropInfo);
	afx_msg BOOL OnQueryNewPalette();
	afx_msg void OnPaletteChanged(CWnd* pFocusWnd);
	afx_msg void OnDevModeChange(LPTSTR lpDeviceName);
	afx_msg void OnFileOpenprojekt();
	afx_msg void OnFileOpen();
	afx_msg void OnFileCloseprojekt();
	afx_msg void OnUpdateFileCloseprojekt(CCmdUI* pCmdUI);
	//}}AFX_MSG
	afx_msg LONG OnOpenMsg(UINT wParam, LONG lParam);
	DECLARE_MESSAGE_MAP()
};
