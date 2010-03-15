// editor.h : main header file for the EDITOR application
//

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

/////////////////////////////////////////////////////////////////////////////
// CEditorApp:
// See editor.cpp for the implementation of this class
//

class CEditorCommandLineInfo : public CCommandLineInfo
{
public:
	CEditorCommandLineInfo() {m_bForceTextMode = FALSE;}
	BOOL m_bForceTextMode;
	virtual void ParseParam(const char* pszParam,BOOL bFlag,BOOL bLast);
};

class CEditorApp : public CWinApp
{
public:
	CEditorApp();
	~CEditorApp();
	void LoadProject(CString& dir);
	Project* m_pProject;

//Attributes
	CEditorCommandLineInfo cmdInfo;
	CDC m_dcScreen;
	LOGFONT m_lf;
	int m_nDefFont;
	static int m_nOpenMsg;
	static int m_nPrinterChangedMsg;
	CRect m_rectPageMargin;
	CRect m_rectInitialFrame;
	BOOL m_bMaximized;
	BOOL m_bWin4;
#ifndef _UNICODE
	BOOL m_bWin31;
#endif
	BOOL m_bLargeIcons;
	BOOL m_bForceTextMode;
	BOOL m_bWordSel;
	BOOL m_bForceOEM;
	int m_nFilterIndex;
	int m_nNewDocType;
	CList<HWND, HWND> m_listPrinterNotify;

	BOOL IsDocOpen(LPCTSTR lpszFileName);
	int GetUnits() {return m_nUnits;}
	int GetTPU() { return GetTPU(m_nUnits);}
	int GetTPU(int n) { return m_units[n].m_nTPU;}
	LPCTSTR GetAbbrev() { return m_units[m_nUnits].m_strAbbrev;}
	LPCTSTR GetAbbrev(int n) { return m_units[n].m_strAbbrev;}
	const CUnit& GetUnit() {return m_units[m_nUnits];}

// Operations
	void RegisterFormats();
	static BOOL CALLBACK StaticEnumProc(HWND hWnd, LPARAM lParam);
	void NotifyPrinterChanged(BOOL bUpdatePrinterSelection = FALSE);
	BOOL PromptForFileName(CString& fileName, UINT nIDSTitle, DWORD dwFlags,
		BOOL bOpenFileDialog, int* pType = NULL);

	BOOL ParseMeasurement(TCHAR* buf, int& lVal);
	void PrintTwips(TCHAR* buf, int nValue, int nDecimal);
	void SaveOptions();
	void LoadOptions();
	HGLOBAL CreateDevNames();
	void SetPrinterDeviceSettings(short& po, short& ps, short& pw, short& pl);
	void GetPrinterDeviceSettings(short& po, short& ps, short& pw, short& pl);
	void SetStandardPrinterSettings(short& po, short& ps, short& pw, short& pl);
	void GetStandardPrinterSettings(short& po, short& ps, short& pw, short& pl);

// Overrides
	BOOL IsIdleMessage(MSG* pMsg);
	virtual CDocument* OpenDocumentFile(LPCTSTR lpszFileName); // open named file
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CEditorApp)
	public:
	virtual BOOL InitInstance();
	virtual int ExitInstance();
	virtual BOOL OnDDECommand(LPTSTR lpszCommand);
	virtual void WinHelp(DWORD dwData, UINT nCmd = HELP_CONTEXT);
	virtual BOOL PreTranslateMessage(MSG* pMsg);
	//}}AFX_VIRTUAL

	//{{AFX_MSG(CEditorApp)
	afx_msg void OnAppAbout();
	afx_msg void OnFileNew();
	afx_msg void OnFileOpen();
	afx_msg void OnFilePrintSetup();
	//}}AFX_MSG

protected:
	short m_dmOrientation;
	short m_dmPaperSize;
	short m_dmPaperWidth;
	short m_dmPaperLength;

	void UpdatePrintOrientation(BOOL bRefresh = TRUE);
	int DoPrintDialog(CPrintDialog* pPD);

	DECLARE_MESSAGE_MAP()
private:
	int m_nUnits;
	static const int m_nPrimaryNumUnits;
	static const int m_nNumUnits;
	static CUnit m_units[7];
};

/////////////////////////////////////////////////////////////////////////////

extern CEditorApp theApp;
