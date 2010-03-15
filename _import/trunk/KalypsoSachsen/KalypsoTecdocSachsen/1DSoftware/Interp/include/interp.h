// Interp.h : main header file for the INTERP application
//

/////////////////////////////////////////////////////////////////////////////
// CInterpApp:
// See Interp.cpp for the implementation of this class
//

class CInterpApp : public CWinApp
{
public:
	CInterpApp();
	Project *m_pProject;
	State *m_pState;
	CStringArray m_strFiles;
	CStringArray m_strErrors;

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CInterpApp)
	public:
	virtual BOOL InitInstance();
	virtual int ExitInstance();
	//}}AFX_VIRTUAL
	BOOL OnDDECommand(LPTSTR lpszCommand, CWnd* pParent);

// Implementation
protected:
	void LoadProject(CString& dir);

	//{{AFX_MSG(CInterpApp)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

extern CInterpApp theApp;

/////////////////////////////////////////////////////////////////////////////
