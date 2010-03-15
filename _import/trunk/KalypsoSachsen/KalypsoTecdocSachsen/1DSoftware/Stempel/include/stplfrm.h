// StplFrm.h : interface of the CStempelFrame class
//
/////////////////////////////////////////////////////////////////////////////

#include "mainfrm.h"

class CStempelFrame : public CMainFrame
{
	DECLARE_DYNAMIC(CStempelFrame)
public:
	CStempelFrame();

// Attributes
public:

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CStempelFrame)
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CStempelFrame();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

// Generated message map functions
protected:
	//{{AFX_MSG(CStempelFrame)
	afx_msg void OnFileNew();
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnClose();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////
