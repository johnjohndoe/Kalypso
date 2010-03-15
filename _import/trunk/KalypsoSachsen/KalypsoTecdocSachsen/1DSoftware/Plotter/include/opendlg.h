// OpenDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// COpenDialog dialog

#include "..\..\commonMfc\commonMfc.h"

class COpenDialog : public CDialog
{
// Construction
public:
	COpenDialog::COpenDialog( CWnd* pParent, BOOL bInsert, BOOL bDelete, BOOL bMultiPlot );

// Dialog Data
	//{{AFX_DATA(COpenDialog)
	CListCtrlEx	m_list;
	CTreeCtrl	m_tree;
	CString	m_project;
	BOOL	m_alignLowPoint;
	double	m_alignValue;
	BOOL	m_multiPlotCheck;
	//}}AFX_DATA

	CString m_filename;
	CTypedPtrArray<CObArray, Section*> m_Sections;
	State *m_pState;

	void SetCurrentSection(Section* pSec) { m_pCurrentSection = pSec; }

  BOOL GetMultiPlot() const { return m_multiPlotCheck; };

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(COpenDialog)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	BOOL m_bDelete;
	BOOL m_bInsert;
  BOOL m_bMultiPlot;
	Section* m_pCurrentSection;		// NULL for FileOpen, non-NULL for FileInsert
	CTypedPtrMap<CMapPtrToPtr, HANDLE, State*> m_lmap_tree;
	CTypedPtrMap<CMapPtrToPtr, HANDLE, State*> m_qmap_tree;
	CTypedPtrMap<CMapWordToPtr, WORD, Section*> m_secmap_list;

	// Generated message map functions
	//{{AFX_MSG(COpenDialog)
	virtual BOOL OnInitDialog();
	virtual void OnOK();
	afx_msg void OnDblclkList1(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnSelchangedTree1(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnColumnclickList1(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnCheckAusrichtung();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
