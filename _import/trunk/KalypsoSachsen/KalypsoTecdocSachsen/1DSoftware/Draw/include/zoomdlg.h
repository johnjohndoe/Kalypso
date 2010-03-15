// ZoomDlg.h : header file
//
#ifndef _ZOOMDLG_H_INCLUDED_
#define _ZOOMDLG_H_INCLUDED_
/////////////////////////////////////////////////////////////////////////////
// CZoomDialog dialog

class CZoomDialog : public CDialog
{
// Construction
public:
	CZoomDialog(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CZoomDialog)
	enum { IDD = IDD_ZOOM };
	CString	m_factor;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CZoomDialog)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	CDrawView *m_pView;
	// Generated message map functions
	//{{AFX_MSG(CZoomDialog)
	virtual BOOL OnInitDialog();
	virtual void OnOK();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


#endif // _ZOOMDLG_H_INCLUDED_