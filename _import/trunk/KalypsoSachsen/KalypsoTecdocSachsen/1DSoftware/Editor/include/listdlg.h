
class CListDlg : public CDialog
{
public:
	//{{AFX_DATA(CListDlg)
	enum { IDD = IDD_LISTDIALOG };
	//}}AFX_DATA
	CListDlg::CListDlg(UINT idStrDlgTitle, UINT idStrListTitle, 
		const CStringList& listItems, int nDefSel=0);
	CString m_strDlgTitle,m_strListTitle;
	const CStringList& m_listItems;
	int m_nSelection;

protected:
	BOOL OnInitDialog();
	//{{AFX_MSG(CListDlg)
	afx_msg void OnOK();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
