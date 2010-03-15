// MainFrm.h : Schnittstelle der Klasse CMainFrame
//
/////////////////////////////////////////////////////////////////////////////
#pragma warning(disable:4786)
#pragma warning(disable:4503)

#if !defined(AFX_MAINFRM_H__27A882C7_3E7F_11D3_A4B9_0080ADAC5D6B__INCLUDED_)
#define AFX_MAINFRM_H__27A882C7_3E7F_11D3_A4B9_0080ADAC5D6B__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#include "OvrvwBar.h"
#include "..\..\commonMfc\commonMfc.h"

class CMainFrame : public CMDIFrameWnd, public IDDEWnd
{
	DECLARE_DYNAMIC(CMainFrame)

public:
	CMainFrame();
	virtual ~CMainFrame();

#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif


// Attribute
public:
  COverviewBar* GetOverviewBar() { return &m_wndOverviewBar; };
  CStatusBar* GetStatusBar() { return &m_wndStatusBar; };
private:
  CStatusBar  m_wndStatusBar;
  COverviewBar m_wndOverviewBar;
	static int m_nUpdateProjectMsg;
	static int m_nUpdateProjectCompleteMsg;
	int m_nWSPWINCount;
  CProgressCtrl* m_statusProgressCtrl;

// Operationen
public:
	virtual void OnUpdateFrameTitle(BOOL bAddToTitle);

  CProgressCtrl* CreateStatusBarProgress( const CString& text );
  void DestroyStatusBarProgress();
  void SetStatusBarProgressText( const CString& text );

protected:
  void DockControlBarLeftOf( CToolBar* Bar, CToolBar* LeftOf );

// Überladungen
	// Vom Klassenassistenten generierte Überladungen virtueller Funktionen
 	//{{AFX_VIRTUAL(CMainFrame)
	public:
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	virtual BOOL OnCmdMsg(UINT nID, int nCode, void* pExtra, AFX_CMDHANDLERINFO* pHandlerInfo);
	//}}AFX_VIRTUAL

protected:  // Eingebundene Elemente der Steuerleiste
  CToolBar m_mapBar;
  CToolBar m_fileBar;
  CToolBar m_toolsBar;
  CToolBar m_themeBar;
  CToolBar m_objectBar;
  CToolBar m_helpBar;

// Generierte Message-Map-Funktionen
protected:
	//{{AFX_MSG(CMainFrame)
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnClose();
	afx_msg void OnDestroy();
	//}}AFX_MSG
  afx_msg void OnFileManager();
	afx_msg LONG OnUpdateProjectComplete(UINT wParam, LONG lParam);
  afx_msg BOOL OnExtras( UINT nID );

  afx_msg LRESULT OnDDEAcknowledge( WPARAM wParam, LPARAM lParam );
  afx_msg LRESULT OnDDETerminate( WPARAM wParam, LPARAM lParam );

  DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // !defined(AFX_MAINFRM_H__27A882C7_3E7F_11D3_A4B9_0080ADAC5D6B__INCLUDED_)
