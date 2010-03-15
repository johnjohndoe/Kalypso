// editor.cpp : Defines the class behaviors for the application.
//

#include "stdafx.h"

#include "global.h"

extern BOOL AFXAPI AfxFullPath(LPTSTR lpszPathOut, LPCTSTR lpszFileIn);

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

CLIPFORMAT cfEmbeddedObject;
CLIPFORMAT cfRTF;
CLIPFORMAT cfRTO;

int CEditorApp::m_nOpenMsg = RegisterWindowMessage(_T("WordPadOpenMessage"));
int CEditorApp::m_nPrinterChangedMsg = RegisterWindowMessage(_T("WordPadPrinterChanged"));

const int CEditorApp::m_nPrimaryNumUnits = 4;
const int CEditorApp::m_nNumUnits = 7;

CUnit CEditorApp::m_units[7] = 
{
//	TPU, 	SmallDiv,	MedDiv,	LargeDiv,	MinMove,	szAbbrev,			bSpace
CUnit(1440,	180,		720,	1440,		90,			IDS_INCH1_ABBREV,	FALSE),//inches
CUnit(568,	142,		284,	568,		142,		IDS_CM_ABBREV,		TRUE),//centimeters
CUnit(20,	120,		720,	720,		100,		IDS_POINT_ABBREV,	TRUE),//points
CUnit(240,	240,		1440,	1440,		120,		IDS_PICA_ABBREV,	TRUE),//picas
CUnit(1440,	180,		720,	1440,		90,			IDS_INCH2_ABBREV,	FALSE),//in
CUnit(1440,	180,		720,	1440,		90,			IDS_INCH3_ABBREV,	FALSE),//inch
CUnit(1440,	180,		720,	1440,		90,			IDS_INCH4_ABBREV,	FALSE)//inches
};

/////////////////////////////////////////////////////////////////////////////
// CEditorApp

BEGIN_MESSAGE_MAP(CEditorApp, CWinApp)
	//{{AFX_MSG_MAP(CEditorApp)
	ON_COMMAND(ID_APP_ABOUT, OnAppAbout)
	ON_COMMAND(ID_FILE_NEW, OnFileNew)
	ON_COMMAND(ID_FILE_PRINT_SETUP, OnFilePrintSetup)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

void CEditorCommandLineInfo::ParseParam(const char* pszParam,BOOL bFlag,BOOL bLast)
{
	if (bFlag)
	{
		if (lstrcmpA(pszParam, "t") == 0)
		{
			m_bForceTextMode = TRUE;
			return;
		}
	}
	CCommandLineInfo::ParseParam(pszParam, bFlag, bLast);
}

/////////////////////////////////////////////////////////////////////////////
// CEditorApp construction

CEditorApp::CEditorApp()
{
	_tsetlocale(LC_ALL, _T(""));

	m_pProject = NULL;
	m_nFilterIndex = 1;
	DWORD dwVersion = ::GetVersion();
	m_bWin4 = (BYTE)dwVersion >= 4;
#ifndef _UNICODE
	m_bWin31 = (dwVersion > 0x80000000 && !m_bWin4);
#endif
	m_nDefFont = (m_bWin4) ? DEFAULT_GUI_FONT : ANSI_VAR_FONT;
	m_dcScreen.Attach(::GetDC(NULL));
	m_bLargeIcons = m_dcScreen.GetDeviceCaps(LOGPIXELSX) >= 120;
	m_bForceOEM = FALSE;
	m_nUnits = 1;
}

CEditorApp::~CEditorApp()
{
	if (m_dcScreen.m_hDC != NULL)
		::ReleaseDC(NULL, m_dcScreen.Detach());
}

CDocument* CEditorApp::OpenDocumentFile(LPCTSTR lpszFileName)
{
	CMDIFrameWnd *pMainFrame = (CMDIFrameWnd*)m_pMainWnd;
	CFrameWnd *pActiveWnd, *pWnd;
	BOOL bMaximized;
	CEditorDoc *pDoc;
	CString path = lpszFileName;

	pActiveWnd = pMainFrame->MDIGetActive(&bMaximized);
	if (pActiveWnd!=NULL)
	{
		pDoc = (CEditorDoc*)pActiveWnd->GetActiveDocument();
		if (pDoc!=NULL)
		{
			if (path.CompareNoCase(pDoc->GetPathName())==0)
			{
				pActiveWnd->SendMessage(WM_COMMAND, ID_FILE_CLOSE);
				return CWinApp::OpenDocumentFile(lpszFileName);
			}
		}
		pMainFrame->MDINext();
		pWnd=pMainFrame->MDIGetActive(&bMaximized);
		while (pWnd!=pActiveWnd)
		{
			if (pWnd!=NULL)
			{
				pDoc = (CEditorDoc*)pWnd->GetActiveDocument();
				if (pDoc!=NULL)
				{
					if (path.CompareNoCase(pDoc->GetPathName())==0)
					{
						pWnd->SendMessage(WM_COMMAND, ID_FILE_CLOSE);
						return CWinApp::OpenDocumentFile(lpszFileName);
					}
				}
				pWnd = pWnd->m_pNextFrameWnd;
			}
			pMainFrame->MDINext();
			pWnd=pMainFrame->MDIGetActive(&bMaximized);
		}
	}
	
	return CWinApp::OpenDocumentFile(lpszFileName);
}

void CEditorApp::LoadProject(CString& dir)
{
//	if (m_pProject!=NULL)
//		delete m_pProject;
	((CMainFrame*)m_pMainWnd)->CloseProject();
	m_pProject = new Project(dir);
	m_pProject->Load();
	// set the mainframe window title
	if (m_pMainWnd!=NULL)
		((CMainFrame*)m_pMainWnd)->OnUpdateFrameTitle(TRUE);
}

/////////////////////////////////////////////////////////////////////////////
// The one and only CEditorApp object

CEditorApp theApp;

/////////////////////////////////////////////////////////////////////////////
// CEditorApp initialization

BOOL CEditorApp::InitInstance()
{
	ParseCommandLine(cmdInfo);

	if (::FindWindow(szWSPEditorClass, NULL) && IsDocOpen(cmdInfo.m_strFileName))
		return FALSE;

	SetRegistryKey(szRegKey);
	LoadOptions();

	Enable3dControls();
	if (!cmdInfo.m_bRunEmbedded)
	{
		switch (m_nCmdShow)
		{
			case SW_HIDE:
			case SW_SHOWMINIMIZED:
			case SW_MINIMIZE:
			case SW_SHOWMINNOACTIVE:
				break;

			case SW_RESTORE:
			case SW_SHOW:
			case SW_SHOWDEFAULT:
			case SW_SHOWNA:
			case SW_SHOWNOACTIVATE:
			case SW_SHOWNORMAL:
			case SW_SHOWMAXIMIZED:
				if (m_bMaximized)
					m_nCmdShow = SW_SHOWMAXIMIZED;
				break;
		}
	}
	else
	{
 		//Excel 4 will start OLE servers minimized
 		m_nCmdShow = SW_SHOWNORMAL;
	}
	int nCmdShow = m_nCmdShow;

	m_hDevNames = CreateDevNames();
	NotifyPrinterChanged((m_hDevNames == NULL));

	// Initialize OLE libraries
	if (!AfxOleInit())
	{
		AfxMessageBox(IDP_OLE_INIT_FAILED);
		return FALSE;
	}
	RegisterFormats();

	// Initialize RichEdit control
	if (LoadLibrary(_T("RICHED32.DLL")) == NULL)
	{
		AfxMessageBox(IDS_RICHED_LOAD_FAIL, MB_OK|MB_ICONEXCLAMATION);
		return FALSE;
	}

	// Standard initialization
	// If you are not using these features and wish to reduce the size
	//  of your final executable, you should remove from the following
	//  the specific initialization routines you do not need.

	LoadStdProfileSettings();  // Load standard INI file options (including MRU)
	LoadOptions();

	// Register the application's document templates.  Document templates
	//  serve as the connection between documents, frame windows and views.
	CMultiDocTemplate* pDocTemplate;
	pDocTemplate = new CMultiDocTemplate(
		IDR_EDITORTYPE,
		RUNTIME_CLASS(CEditorDoc),
		RUNTIME_CLASS(CChildFrame),
		RUNTIME_CLASS(CEditorView));
	AddDocTemplate(pDocTemplate);


	pDocTemplate->SetContainerInfo(IDR_CNTR_INPLACE);

	// make sure the main window is showing 
	CMainFrame* pMainFrame = new CMainFrame;
	if (!pMainFrame->LoadFrame(IDR_MAINFRAME))
		return FALSE;
	m_pMainWnd = pMainFrame;

	EnableShellOpen();

	if (!cmdInfo.m_strFileName.IsEmpty())	// open an existing document
		m_nCmdShow = nCmdShow;

	if (cmdInfo.m_nShellCommand==CCommandLineInfo::FileNew)
		cmdInfo.m_nShellCommand = CCommandLineInfo::FileNothing;

	// Dispatch commands specified on the command line
	if (!ProcessShellCommand(cmdInfo))
	{
		return FALSE;
	}

	// Enable File Manager drag/drop open
	m_pMainWnd->DragAcceptFiles();

	// The main window has been initialized, so show and update it.
	pMainFrame->ShowWindow(m_nCmdShow);
	pMainFrame->UpdateWindow();

	// get path of executable (no extension) and update registry
	TCHAR szBuff[_MAX_PATH];
	if (::GetModuleFileName(m_hInstance, szBuff, _MAX_PATH))
	{
#ifndef _MAC
		LPTSTR lpszExt = _tcsrchr(szBuff, '.');
		ASSERT(lpszExt != NULL);
		ASSERT(*lpszExt == '.');
		*lpszExt = 0;       // no suffix
#endif

		WriteProfileString("Info", "FilePath", szBuff);
	}
	return TRUE;
}

BOOL CEditorApp::IsDocOpen(LPCTSTR lpszFileName)
{
	if (lpszFileName[0] == NULL)
		return FALSE;
	TCHAR szPath[_MAX_PATH];
	AfxFullPath(szPath, lpszFileName);
	ATOM atom = GlobalAddAtom(szPath);
	ASSERT(atom != NULL);
	if (atom == NULL)
		return FALSE;
	EnumWindows(StaticEnumProc, (LPARAM)&atom);
	if (atom == NULL)
		return TRUE;
	DeleteAtom(atom);
	return FALSE;
}

BOOL CALLBACK CEditorApp::StaticEnumProc(HWND hWnd, LPARAM lParam)
{
	TCHAR szClassName[30];
	GetClassName(hWnd, szClassName, 30);
	if (lstrcmp(szClassName, szWSPEditorClass) != 0)
		return TRUE;

	ATOM* pAtom = (ATOM*)lParam;
	ASSERT(pAtom != NULL);
	DWORD dw = NULL;
	::SendMessageTimeout(hWnd, m_nOpenMsg, NULL, (LPARAM)*pAtom,
		SMTO_ABORTIFHUNG, 500, &dw);
	if (dw)
	{
		::SetForegroundWindow(hWnd);
		DeleteAtom(*pAtom);
		*pAtom = NULL;
		return FALSE;
	}				 
	return TRUE;
}

void CEditorApp::RegisterFormats()
{
	cfEmbeddedObject = (CLIPFORMAT)::RegisterClipboardFormat(_T("Embedded Object"));
	cfRTF = (CLIPFORMAT)::RegisterClipboardFormat(_T(CF_RTF));
	cfRTO = (CLIPFORMAT)::RegisterClipboardFormat(_T(CF_RETEXTOBJ));
}

void CEditorApp::UpdatePrintOrientation(BOOL bRefresh /*=TRUE*/)
{
    // Get default printer settings.
    PRINTDLG   pd;
    pd.lStructSize = (DWORD) sizeof(PRINTDLG);
    if (GetPrinterDeviceDefaults(&pd))
	{
        // Lock memory handle.
        DEVMODE FAR* pDevMode =
            (DEVMODE FAR*)::GlobalLock(m_hDevMode);
        if (pDevMode)
		{
            if (bRefresh)
			{	// Change printer settings in here.
				pDevMode->dmOrientation = m_dmOrientation;
			}
			else
			{
				m_dmOrientation = pDevMode->dmOrientation;
			}
			
            // Unlock memory handle.
            ::GlobalUnlock(m_hDevMode);
		}
	}
}

void CEditorApp::SaveOptions()
{
	WriteProfileInt(szSection, szWordSel, m_bWordSel);
	WriteProfileInt(szSection, szMaximized, m_bMaximized);
	WriteProfileBinary(szSection, szFrameRect, (BYTE*)&m_rectInitialFrame, 
		sizeof(CRect));
	WriteProfileBinary(szSection, szPageMargin, (BYTE*)&m_rectPageMargin, 
		sizeof(CRect));
	UpdatePrintOrientation(FALSE);
	WriteProfileInt(szSection, "Paper Orientation", m_dmOrientation);
	WriteProfileInt(szSection, "Paper Size", m_dmPaperSize);
	WriteProfileInt(szSection, "Paper Width", m_dmPaperWidth);
	WriteProfileInt(szSection, "Paper Length", m_dmPaperLength);
}

void CEditorApp::LoadOptions()
{
	BYTE* pb = NULL;
	UINT nLen = 0;

	HFONT hFont = (HFONT)GetStockObject(DEFAULT_GUI_FONT);
	if (hFont == NULL)
		hFont = (HFONT)GetStockObject(ANSI_VAR_FONT);
	VERIFY(GetObject(hFont, sizeof(LOGFONT), &m_lf));

	m_bWordSel = GetProfileInt(szSection, szWordSel, TRUE);
	TCHAR buf[2];
	buf[0] = NULL;
	GetLocaleInfo(GetUserDefaultLCID(), LOCALE_IMEASURE, buf, 2);
	int nDefUnits = buf[0] == _T('1') ? 0 : 1;
	m_bMaximized = GetProfileInt(szSection, szMaximized, (int)FALSE);

	if (GetProfileBinary(szSection, szFrameRect, &pb, &nLen))
	{
		ASSERT(nLen == sizeof(CRect));
		memcpy(&m_rectInitialFrame, pb, sizeof(CRect));
		delete pb;
	}
	else
		m_rectInitialFrame.SetRect(0,0,0,0);


	CRect rectScreen(0, 0, GetSystemMetrics(SM_CXSCREEN), 
		GetSystemMetrics(SM_CYSCREEN));
	CRect rectInt;
	rectInt.IntersectRect(&rectScreen, &m_rectInitialFrame);
	if (rectInt.Width() < 10 || rectInt.Height() < 10)
		m_rectInitialFrame.SetRect(0, 0, 0, 0);

	if (GetProfileBinary(szSection, szPageMargin, &pb, &nLen))
	{
		ASSERT(nLen == sizeof(CRect));
		memcpy(&m_rectPageMargin, pb, sizeof(CRect));
		delete pb;
	}
	else
		m_rectPageMargin.SetRect(500, 500, 500, 500);	
	m_dmOrientation = GetProfileInt(szSection, "Paper Orientation", DMORIENT_LANDSCAPE);
	m_dmPaperSize = GetProfileInt(szSection, "Paper Size", DMPAPER_A4);
	m_dmPaperWidth = GetProfileInt(szSection, "Paper Width", 2101);
	m_dmPaperLength = GetProfileInt(szSection, "Paper Length", 2969);
	SetPrinterDeviceSettings(m_dmOrientation, m_dmPaperSize, m_dmPaperWidth, m_dmPaperLength);
	UpdatePrintOrientation();
}

BOOL CEditorApp::ParseMeasurement(LPTSTR buf, int& lVal)
{
	TCHAR* pch;
	if (buf[0] == NULL)
		return FALSE;
	float f = (float)_tcstod(buf,&pch);

	// eat white space, if any
	while (isspace(*pch))
		pch++;

	if (pch[0] == NULL) // default
	{
		lVal = (f < 0.f) ? (int)(f*GetTPU()-0.5f) : (int)(f*GetTPU()+0.5f);
		return TRUE;
	}
	for (int i=0;i<m_nNumUnits;i++)
	{
		if (lstrcmpi(pch, GetAbbrev(i)) == 0)
		{
			lVal = (f < 0.f) ? (int)(f*GetTPU(i)-0.5f) : (int)(f*GetTPU(i)+0.5f);
			return TRUE;
		}
	}
	return FALSE;
}

void CEditorApp::PrintTwips(TCHAR* buf, int nValue, int nDec)
{
	ASSERT(nDec == 2);
	int div = GetTPU();
	int lval = nValue;
	BOOL bNeg = FALSE;
	
	int* pVal = new int[nDec+1];

	if (lval < 0)
	{
		bNeg = TRUE;
		lval = -lval;
	}

	for (int i=0;i<=nDec;i++)
	{
		pVal[i] = lval/div; //integer number
		lval -= pVal[i]*div;
		lval *= 10;
	}
	i--;
	if (lval >= div/2)
		pVal[i]++;

	while ((pVal[i] == 10) && (i != 0))
	{
		pVal[i] = 0;
		pVal[--i]++;
	}

	while (nDec && pVal[nDec] == 0)
		nDec--;

	_stprintf(buf, _T("%.*f"), nDec, (float)nValue/(float)div);

	if (m_units[m_nUnits].m_bSpaceAbbrev)
		lstrcat(buf, _T(" "));
	lstrcat(buf, GetAbbrev());
	delete []pVal;
}

/////////////////////////////////////////////////////////////////////////////
// CAboutDlg dialog used for App About

class CAboutDlg : public CDialog
{
public:
	CAboutDlg();

// Dialog Data
	//{{AFX_DATA(CAboutDlg)
	enum { IDD = IDD_ABOUTBOX };
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CAboutDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	//{{AFX_MSG(CAboutDlg)
		// No message handlers
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

CAboutDlg::CAboutDlg() : CDialog(CAboutDlg::IDD)
{
	//{{AFX_DATA_INIT(CAboutDlg)
	//}}AFX_DATA_INIT
}

void CAboutDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CAboutDlg)
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CAboutDlg, CDialog)
	//{{AFX_MSG_MAP(CAboutDlg)
		// No message handlers
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CEditorApp commands

void CEditorApp::OnAppAbout()
{
//	CString strTitle;
//	VERIFY(strTitle.LoadString(AFX_IDS_APP_TITLE));
//	ShellAbout(m_pMainWnd->GetSafeHwnd(), strTitle, _T("Hello"), LoadIcon(IDR_MAINFRAME));
	CAboutDlg aboutDlg;
	aboutDlg.DoModal();
}

int CEditorApp::ExitInstance() 
{
	FreeLibrary(GetModuleHandle(_T("RICHED32.DLL")));
	SaveOptions();
	if (m_pProject!=NULL)
		delete m_pProject;

	return CWinApp::ExitInstance();
}

void CEditorApp::OnFileNew() 
{
	int nDocType = -1;
	if (cmdInfo.m_bForceTextMode)
		nDocType = RD_TEXT;
	else if (!cmdInfo.m_strFileName.IsEmpty())
	{
		CFileException fe;
		nDocType = GetDocTypeFromName(cmdInfo.m_strFileName, fe);
	}
	if (nDocType == -1)
		nDocType = RD_RICHTEXT;
	m_nNewDocType = nDocType;
	m_pDocManager->OnFileNew();
		// if returns NULL, the user has already been alerted
}

// prompt for file name - used for open and save as
// static function called from app
BOOL CEditorApp::PromptForFileName(CString& fileName, UINT nIDSTitle, 
	DWORD dwFlags, BOOL bOpenFileDialog, int* pType)
{
	ScanForConverters();
	CFileDialog dlgFile(bOpenFileDialog);
	CString title;

	VERIFY(title.LoadString(nIDSTitle));
	
	dlgFile.m_ofn.Flags |= dwFlags;
//	dlgFile.m_ofn.Flags &= ~OFN_SHOWHELP;

	int nIndex = m_nFilterIndex;
	if (!bOpenFileDialog)
	{
		int nDocType = (pType != NULL) ? *pType : RD_DEFAULT;
		nIndex = GetIndexFromType(nDocType, bOpenFileDialog);
		if (nIndex == -1)
			nIndex = GetIndexFromType(RD_DEFAULT, bOpenFileDialog);
		if (nIndex == -1)
			nIndex = GetIndexFromType(RD_NATIVE, bOpenFileDialog);
		ASSERT(nIndex != -1);
		nIndex++;
	}
	dlgFile.m_ofn.nFilterIndex = nIndex;
	// strDefExt is necessary to hold onto the memory from GetExtFromType
	CString strDefExt = GetExtFromType(GetTypeFromIndex(nIndex-1, bOpenFileDialog));
	dlgFile.m_ofn.lpstrDefExt = strDefExt;
		

	CString strFilter = GetFileTypes(bOpenFileDialog);
	dlgFile.m_ofn.lpstrFilter = strFilter;
	dlgFile.m_ofn.lpstrTitle = title;
	dlgFile.m_ofn.lpstrFile = fileName.GetBuffer(_MAX_PATH);

	BOOL bRet = (dlgFile.DoModal() == IDOK) ? TRUE : FALSE;
	fileName.ReleaseBuffer();
	if (bRet)
	{
		if (bOpenFileDialog)
			m_nFilterIndex = dlgFile.m_ofn.nFilterIndex;
		if (pType != NULL)
		{
			int nIndex = (int)dlgFile.m_ofn.nFilterIndex - 1;
			ASSERT(nIndex >= 0);
			*pType = GetTypeFromIndex(nIndex, bOpenFileDialog);
		}
	}
	return bRet;
}

void CEditorApp::OnFileOpen() 
{
	// prompt the user (with all document templates)
	CString newName;
	int nType = RD_DEFAULT;
	if (!PromptForFileName(newName, AFX_IDS_OPENFILE,
	  OFN_HIDEREADONLY | OFN_FILEMUSTEXIST, TRUE, &nType))
		return; // open cancelled

	if (nType == RD_OEMTEXT)
		m_bForceOEM = TRUE;
	OpenDocumentFile(newName);
	m_bForceOEM = FALSE;
	// if returns NULL, the user has already been alerted
}

BOOL CEditorApp::OnDDECommand(LPTSTR lpszCommand) 
{
   if (CWinApp::OnDDECommand(lpszCommand))
      return TRUE;

	CString strCommand = lpszCommand;
	CString strProject, strFileName, oldProject;

	// open project format is "[project("%s","%s",...)]" - no whitespace allowed, one per line
	CCommandLineInfo cmdInfo;
	strCommand.MakeLower();

	if (strCommand.Left(10) == _T("[project(\""))
	{
		strCommand = strCommand.Right(strCommand.GetLength() - 10);

		int i = strCommand.Find('"');
		if (i == -1)
			return FALSE; // illegally terminated

		strProject = strCommand.Left(i);
		strCommand = strCommand.Right(strCommand.GetLength() - i);

		// If we were started up for DDE retrieve the Show state
		if (m_pCmdInfo != NULL)
		{
			m_nCmdShow = (int)m_pCmdInfo;
			m_pCmdInfo = NULL;
		}

		// show the application window
		CWnd* pMainWnd = m_pMainWnd;
		int nCmdShow = m_nCmdShow;
		if (nCmdShow == -1 || nCmdShow == SW_SHOWNORMAL)
		{
			if (pMainWnd->IsIconic())
				nCmdShow = SW_RESTORE;
			else
				nCmdShow = SW_SHOW;
		}
		pMainWnd->ShowWindow(nCmdShow);
		if (nCmdShow != SW_MINIMIZE)
			pMainWnd->SetForegroundWindow();

		if (m_pProject!=NULL)
		{
			oldProject = m_pProject->GetDir();
			if (oldProject.CompareNoCase(strProject)!=0)
				oldProject.Empty();
		}
		if (oldProject.IsEmpty())
		{
			((CMainFrame*)pMainWnd)->CloseProject();
			// then open the project
			LoadProject(strProject);
		}

		if (m_pProject==NULL)
			return FALSE;

		// user is now "in control" of the application
		if (!AfxOleGetUserCtrl())
			AfxOleSetUserCtrl(TRUE);

		// next time, show the window as default
		m_nCmdShow = -1;

		while (strCommand.Left(3) == _T("\",\""))
		{
			strFileName = m_pProject->GetDir();
			strFileName += "\\";
			strCommand = strCommand.Right(strCommand.GetLength() - 3);
			i = strCommand.Find('"');
			if (i == -1)
				return FALSE; // illegally terminated
			else
			{
				strFileName += strCommand.Left(i);
				strCommand = strCommand.Right(strCommand.GetLength() - i);
			}
			cmdInfo.m_strFileName = strFileName;
			
			CFile file;
			CFileStatus rStatus;
			
			if (!file.GetStatus(cmdInfo.m_strFileName, rStatus))
			{
				CString str;
				
				str.FormatMessage(IDS_ERROR_READFILE, cmdInfo.m_strFileName);
				AfxMessageBox(str, MB_ICONEXCLAMATION | MB_OK);
			}
			else
			{
				if (m_pProject->FileIsOEM(cmdInfo.m_strFileName))
					m_bForceOEM = TRUE;
				// then open the profile
				OpenDocumentFile(cmdInfo.m_strFileName);
				m_bForceOEM = FALSE;
			}
		}

		return TRUE;
	}

   // Return FALSE for any unhandled DDE commands.
   return FALSE;
}

void CEditorApp::WinHelp(DWORD dwData, UINT nCmd) 
{
	if (nCmd == HELP_INDEX || nCmd == HELP_CONTENTS)
		nCmd = HELP_FINDER;
	CWinApp::WinHelp(dwData, nCmd);
}

BOOL CEditorApp::PreTranslateMessage(MSG* pMsg) 
{
	if (pMsg->message == WM_PAINT)
		return FALSE;
	// CWinApp::PreTranslateMessage does nothing but call base
	return CWinThread::PreTranslateMessage(pMsg);
}

void CEditorApp::NotifyPrinterChanged(BOOL bUpdatePrinterSelection)
{    
	if (bUpdatePrinterSelection)
		UpdatePrinterSelection(FALSE);
	POSITION pos = m_listPrinterNotify.GetHeadPosition();
	while (pos != NULL)
	{
		HWND hWnd = m_listPrinterNotify.GetNext(pos);
		::SendMessage(hWnd, m_nPrinterChangedMsg, 0, 0);
	}
}

BOOL CEditorApp::IsIdleMessage(MSG* pMsg)
{
	if (pMsg->message == WM_MOUSEMOVE || pMsg->message == WM_NCMOUSEMOVE)
		return FALSE;
	return CWinApp::IsIdleMessage(pMsg);
}

HGLOBAL CEditorApp::CreateDevNames()
{
	HGLOBAL hDev = NULL;
	if (!cmdInfo.m_strDriverName.IsEmpty() && !cmdInfo.m_strPrinterName.IsEmpty() &&
		!cmdInfo.m_strPortName.IsEmpty())
	{
		hDev = GlobalAlloc(GPTR, 4*sizeof(WORD)+
			cmdInfo.m_strDriverName.GetLength() + 1 +
			cmdInfo.m_strPrinterName.GetLength() + 1 +
			cmdInfo.m_strPortName.GetLength()+1);
		LPDEVNAMES lpDev = (LPDEVNAMES)GlobalLock(hDev);
		lpDev->wDriverOffset = sizeof(WORD)*4;
		lstrcpy((TCHAR*)lpDev + lpDev->wDriverOffset, cmdInfo.m_strDriverName);

		lpDev->wDeviceOffset = (WORD)(lpDev->wDriverOffset + cmdInfo.m_strDriverName.GetLength()+1);
		lstrcpy((TCHAR*)lpDev + lpDev->wDeviceOffset, cmdInfo.m_strPrinterName);

		lpDev->wOutputOffset = (WORD)(lpDev->wDeviceOffset + cmdInfo.m_strPrinterName.GetLength()+1);
		lstrcpy((TCHAR*)lpDev + lpDev->wOutputOffset, cmdInfo.m_strPortName);

		lpDev->wDefault = 0;
	}
	return hDev;
}

int CEditorApp::DoPrintDialog(CPrintDialog* pPD)
{
	UpdatePrinterSelection(FALSE);

	pPD->m_pd.hDevMode = m_hDevMode;
	pPD->m_pd.hDevNames = m_hDevNames;

	int nResponse = pPD->DoModal();

	if (nResponse==IDOK)
	{
		// refresh current CWinApp cache of printer device information
		m_hDevMode = pPD->m_pd.hDevMode;
		m_hDevNames = pPD->m_pd.hDevNames;
	}

	return nResponse;
}

void CEditorApp::OnFilePrintSetup() 
{	// only purpose is to change standard printer settings
	SetPrinterDeviceSettings(m_dmOrientation, m_dmPaperSize, m_dmPaperWidth, m_dmPaperLength);
	CPrintDialog pd(TRUE);
	DoPrintDialog(&pd);
	GetPrinterDeviceSettings(m_dmOrientation, m_dmPaperSize, m_dmPaperWidth, m_dmPaperLength);
}

void CEditorApp::SetPrinterDeviceSettings(short& po, short& ps, short& pw, short& pl)
{
    // Get default printer settings.
    PRINTDLG   pd;
    pd.lStructSize = (DWORD) sizeof(PRINTDLG);
    if (GetPrinterDeviceDefaults(&pd))
	{
        // Lock memory handle.
        DEVMODE FAR* pDevMode =
            (DEVMODE FAR*)::GlobalLock(m_hDevMode);
        if (pDevMode)
		{
			pDevMode->dmOrientation = po;
			pDevMode->dmPaperSize = ps;
			pDevMode->dmPaperWidth = pw;
			pDevMode->dmPaperLength = pl;
			
            // Unlock memory handle.
            ::GlobalUnlock(m_hDevMode);
		}
	}
}

void CEditorApp::GetPrinterDeviceSettings(short& po, short& ps, short& pw, short& pl)
{
    // Get default printer settings.
    PRINTDLG   pd;
    pd.lStructSize = (DWORD) sizeof(PRINTDLG);
    if (GetPrinterDeviceDefaults(&pd))
	{
        // Lock memory handle.
        DEVMODE FAR* pDevMode =
            (DEVMODE FAR*)::GlobalLock(m_hDevMode);
        if (pDevMode)
		{
			po = pDevMode->dmOrientation;
			ps = pDevMode->dmPaperSize;
			pw = pDevMode->dmPaperWidth;
			pl = pDevMode->dmPaperLength;
			
            // Unlock memory handle.
            ::GlobalUnlock(m_hDevMode);
		}
	}
}

void CEditorApp::SetStandardPrinterSettings(short& po, short& ps, short& pw, short& pl)
{
	m_dmOrientation = po;
	m_dmPaperSize = ps;
	m_dmPaperWidth = pw;
	m_dmPaperLength = pl;
}

void CEditorApp::GetStandardPrinterSettings(short& po, short& ps, short& pw, short& pl)
{
	po = m_dmOrientation;
	ps = m_dmPaperSize;
	pw = m_dmPaperWidth;
	pl = m_dmPaperLength;
}

