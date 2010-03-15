// ipframe.h : interface of the CInPlaceFrame class
//

class CInPlaceFrame : public COleIPFrameWnd
{
	DECLARE_DYNCREATE(CInPlaceFrame)
public:
	CInPlaceFrame();

// Implementation
public:
	virtual ~CInPlaceFrame();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif
	virtual BOOL OnCreateControlBars(CFrameWnd* pWndFrame, CFrameWnd* pWndDoc);

protected:
	// control bar embedded members
	CToolBar    m_wndMainToolBar;
	CToolBar    m_wndZeichnenToolBar;
	COleResizeBar   m_wndResizeBar;

	// for enabling drag/drop to our window
	COleDropTarget m_dropTarget;

// Generated message map functions
protected:
	//{{AFX_MSG(CInPlaceFrame)
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnUpdateContextHelp(CCmdUI* pCmdUI);
	//}}AFX_MSG
#if _MFC_VER<0x0421
	afx_msg LRESULT OnRecalcParent(WPARAM wParam, LPARAM lParam);
#endif
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////
