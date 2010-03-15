// intrppg1.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CInterpPage1 dialog

#include "..\..\commonMfc\commonMfc.h"

class CInterpSheet;

class CInterpPage1 : public CPropertyPage
{
	DECLARE_DYNCREATE(CInterpPage1)

// Construction
public:
	CInterpPage1(CInterpSheet *pOwner = NULL);
	~CInterpPage1();

// Dialog Data
	//{{AFX_DATA(CInterpPage1)
	enum { IDD = IDD_INTERP_PAGE1 };
	CListCtrlEx	m_list;
	int		m_nProfilNum;
	//}}AFX_DATA

	CrossSection *sec[2];


// Overrides
	// ClassWizard generate virtual function overrides
	//{{AFX_VIRTUAL(CInterpPage1)
	public:
	virtual BOOL OnSetActive();
	virtual LRESULT OnWizardNext();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	CInterpSheet *m_pOwner;
	// Generated message map functions
	//{{AFX_MSG(CInterpPage1)
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};


template<class BASE_CLASS, class TYPE>
void OrderListByStation(CTypedPtrList<BASE_CLASS, TYPE>& list)
{
	POSITION pos1, pos2, pos3;
	CTypedPtrList<BASE_CLASS, TYPE> templist;
	TYPE l1, l2;
	double anf;
	BOOL bInserted;

	pos1 = list.GetHeadPosition();
	while(pos1!=NULL)
	{
		bInserted = FALSE;
		l1 = list.GetNext(pos1);
		anf = l1->GetStation();
		pos2 = templist.GetHeadPosition();
		while(pos2!=NULL)
		{
			pos3 = pos2;
			l2 = templist.GetNext(pos2);
			if (l2->GetStation()>=anf)
			{
				templist.InsertBefore(pos3, l1);
				pos2 = NULL;
				bInserted = TRUE;
			}
		}
		if (!bInserted)
			templist.AddTail(l1);
	}
	list.RemoveAll();
	pos1 = templist.GetHeadPosition();
	while (pos1!=NULL)
	{
		l1 = templist.GetNext(pos1);
		list.AddTail(l1);
	}
}
