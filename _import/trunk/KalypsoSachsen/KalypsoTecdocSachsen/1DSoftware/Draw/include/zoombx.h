// zoombx.h : header file
//

#ifndef _ZOOMBX_H_INCLUDED_
#define _ZOOMBX_H_INCLUDED_

/////////////////////////////////////////////////////////////////////////////
// CZoomBox window

class CZoomBox : public CComboBox
{
// Construction
public:
	CZoomBox();

// Attributes
public:

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CZoomBox)
	public:
	virtual BOOL PreTranslateMessage(MSG* pMsg);
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CZoomBox();

	// Generated message map functions
protected:
	//{{AFX_MSG(CZoomBox)
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnUpdateZoom();
	afx_msg void OnDestroy();
	//}}AFX_MSG

	afx_msg LONG OnZoomChanged(UINT, LONG); //handles registered message
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

#endif // _ZOOMBX_H_INCLUDED_