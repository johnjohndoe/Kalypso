// intrppg2.h : header file
//

#ifndef _INTRPPG2_H_INCLUDED_
#define _INTRPPG2_H_INCLUDED_

/////////////////////////////////////////////////////////////////////////////
// CInterpPage2 dialog

class CInterpSheet;

class CInterpPage2 : public CPropertyPage
{
	DECLARE_DYNCREATE(CInterpPage2)

// Construction
public:
	CInterpPage2(CInterpSheet *pOwner = NULL);
	~CInterpPage2();

// Dialog Data
	//{{AFX_DATA(CInterpPage2)
	enum { IDD = IDD_INTERP_PAGE2 };
	CString	m_watername;
	double	m_station;
	int		m_vzk;
	int		m_num;
	CString	m_state;
	CString	m_pk;
	//}}AFX_DATA

	CTypedPtrArray<CObArray, CrossSection*> m_profils;
	CMap<int, int, int, int> m_usedProfilNums;
	CStringList m_usedFileNames;

// Overrides
	// ClassWizard generate virtual function overrides
	//{{AFX_VIRTUAL(CInterpPage2)
	public:
	virtual BOOL OnSetActive();
	virtual LRESULT OnWizardBack();
	virtual LRESULT OnWizardNext();
	virtual BOOL OnWizardFinish();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

public:
	void SetProfilNum(int n);
	int GetProfilNum() { return m_profils.GetSize(); }

// Implementation
protected:
	CInterpSheet *m_pOwner;
	int m_nCount, m_nDone;

	BOOL UpdateProfilData();
	BOOL KeyExists(double station, int vzk, CString pk);

	// Generated message map functions
	//{{AFX_MSG(CInterpPage2)
	virtual BOOL OnInitDialog();
	afx_msg void OnKillfocusEdit2();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

#endif // _INTRPPG2_H_INCLUDED_