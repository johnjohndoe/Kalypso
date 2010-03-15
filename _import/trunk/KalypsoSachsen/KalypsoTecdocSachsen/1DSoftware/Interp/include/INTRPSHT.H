// intrpsht.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CInterpSheet

#include "batch.h"
class CInterpPage1;
#include "intrppg2.h"
class CInterpPage3;
class CInterpPage4;

class CInterpSheet : public CPropertySheet
{
	DECLARE_DYNAMIC(CInterpSheet)

// Construction
public:
	CInterpSheet(UINT nIDCaption, CWnd* pParentWnd = NULL, UINT iSelectPage = 0, BOOL bDemo = FALSE, BOOL bBatch = FALSE);
	CInterpSheet(LPCTSTR pszCaption, CWnd* pParentWnd = NULL, UINT iSelectPage = 0, BOOL bDemo = FALSE, BOOL bBatch = FALSE);

// Attributes
public:
	Batch m_batch;

// Operations
public:
	void SetProfilNum(int n) { page2->SetProfilNum(n); }
	int GetProfilNum() { return page2->GetProfilNum(); }
	void GetWaterName(CString& name);
	double GetMinStation();
	double GetMaxStation();
	void GetStateName(CString& name);

	void Interpolate();
// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CInterpSheet)
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CInterpSheet();

	// Generated message map functions
protected:
	BOOL m_bDemo;
	BOOL m_bBatch;
	CInterpPage1 *page1;
	CInterpPage2 *page2;
	CInterpPage3 *page3;
	CInterpPage4 *page4;
	//{{AFX_MSG(CInterpSheet)
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////
