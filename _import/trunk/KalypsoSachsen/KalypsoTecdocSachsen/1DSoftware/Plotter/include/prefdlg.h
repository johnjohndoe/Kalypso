// PrefDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CPrefDialog dialog

#include "..\..\commonMfc\commonMfc.h"

class CPrefDialog : public CDialog
{
// Construction
public:
	CPrefDialog(CWnd* pParent = NULL);   // standard constructor
	~CPrefDialog();

// Dialog Data
	//{{AFX_DATA(CPrefDialog)
	enum { IDD = IDD_PREF };
	CListCtrlEx	m_list;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CPrefDialog)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	CTypedPtrArray<CPtrArray, CTemplate*> m_templates;
	CStringArray m_stempels;
	CArray<int, int> m_templatestempels;
	CImageList m_StateImageList;
	CString m_data;

	void SetCheck(int nIndex, BOOL bChecked);
	BOOL GetCheck(int nIndex);

	// Generated message map functions
	//{{AFX_MSG(CPrefDialog)
	virtual BOOL OnInitDialog();
	virtual void OnOK();
	afx_msg void OnEdit();
	afx_msg void OnAdd();
	afx_msg void OnDelete();
	afx_msg void OnEndlabeleditList1(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnDblclkList1(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnItemchangedList1(NMHDR* pNMHDR, LRESULT* pResult);
	virtual void OnCancel();
	afx_msg void OnClickList1(NMHDR* pNMHDR, LRESULT* pResult);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
