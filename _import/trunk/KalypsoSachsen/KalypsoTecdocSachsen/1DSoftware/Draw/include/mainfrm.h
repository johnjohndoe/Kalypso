// MainFrm.h : interface of the CMainFrame class
//
/////////////////////////////////////////////////////////////////////////////

#ifndef _MAINFRAME_H_INCLUDED_
#define _MAINFRAME_H_INCLUDED_

#include "mtoolbar.h"
#include "..\..\commonMfc\commonMfc.h"

class CMainFrame : public CMDIFrameWnd, public IDDEWnd
{
	DECLARE_DYNAMIC(CMainFrame)
public:
	CMainFrame();

// Attributes
public:
	BOOL m_bEditing;

// Operations
public:
	BOOL CreateStatusBarProgress(CString& str, int nLower, int nUpper);
	void IncStatusBarProgress();
	void DestroyStatusBarProgress();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CMainFrame)
	public:
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	virtual BOOL PreTranslateMessage(MSG* pMsg);
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CMainFrame();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

	CStatusBar  m_wndStatusBar;
protected:  // control bar embedded members
//	CToolBar    m_wndMainToolBar;
	CMainToolBar    m_wndMainToolBar;
	CToolBar    m_wndZeichnenToolBar;
	CProgressCtrl *m_pProgressCtrl;

// Generated message map functions
protected:
	//{{AFX_MSG(CMainFrame)
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg void OnWindowCloseAll();
	//}}AFX_MSG
  afx_msg LRESULT OnDDEAcknowledge( WPARAM wParam, LPARAM lParam );
  afx_msg LRESULT OnDDETerminate( WPARAM wParam, LPARAM lParam );
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

#endif _MAINFRAME_H_INCLUDED_