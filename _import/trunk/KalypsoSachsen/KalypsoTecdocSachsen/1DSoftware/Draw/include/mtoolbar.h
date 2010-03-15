// mtoolbar.h : header file
//
#ifndef _MAINTOOLBAR_H_INCLUDED_
#define _MAINTOOLBAR_H_INCLUDED_
/////////////////////////////////////////////////////////////////////////////
// CMainToolBar window

#include "zoombx.h"

class CMainToolBar : public CToolBar
{
// Construction
public:
	CZoomBox m_ZoomBox;

	CMainToolBar();
	BOOL Init(CWnd* pParentWnd, BOOL bToolTips);
	BOOL SetHorizontal();
	BOOL SetVertical();

// Attributes
public:

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CMainToolBar)
	//}}AFX_VIRTUAL

// Implementation
public:
	BOOL m_bVertical;

	virtual ~CMainToolBar();
	virtual CSize CalcDynamicLayout(int nLength, DWORD dwMode);

	// Generated message map functions
protected:
	//{{AFX_MSG(CMainToolBar)
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////
#endif _MAINTOOLBAR_H_INCLUDED_