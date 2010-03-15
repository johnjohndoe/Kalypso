// Stempel.h : main header file for the STEMPEL application
//

#include "draw.h"

/////////////////////////////////////////////////////////////////////////////
// CStempelApp:
// See Stempel.cpp for the implementation of this class
//

class CStempelApp : public CDrawApp
{
public:
	CStempelApp();

	void OnFileNew();
	//void OpenStempel(int n);

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CStempelApp)
	public:
	virtual BOOL InitInstance();
	//}}AFX_VIRTUAL

// Implementation

	//{{AFX_MSG(CStempelApp)
	afx_msg void OnAppAbout();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#define GETSTEMPELAPP ((CStempelApp*)AfxGetApp())

/////////////////////////////////////////////////////////////////////////////
