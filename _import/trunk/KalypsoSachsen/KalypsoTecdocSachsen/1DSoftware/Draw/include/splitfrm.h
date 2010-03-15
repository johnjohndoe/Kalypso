// splitfra.h : header file
//

#ifndef _SPLITFRM_H_INCLUDED_
#define _SPLITFRM_H_INCLUDED_

/////////////////////////////////////////////////////////////////////////////
// CSplitFrame frame with splitter

class CSplitFrame : public CMDIChildWnd
{
	DECLARE_DYNCREATE(CSplitFrame)
protected:
	CSplitFrame();          // protected constructor used by dynamic creation

// Attributes
protected:
	CSplitterWnd    m_wndSplitter;
public:

// Operations
public:

// Implementation
public:
	virtual BOOL OnCreateClient(LPCREATESTRUCT lpcs, CCreateContext* pContext);

	// Generated message map functions
	//{{AFX_MSG(CSplitFrame)
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

#endif // _SPLITFRM_H_INCLUDED_