// Interp.cpp : Defines the class behaviors for the application.
//

#include "stdAfx.h"

#include "..\..\commonMfc\commonMfc.h"
#include "mainfrm.h"
#include "intrpsht.h"

#include "interp.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CInterpApp

BEGIN_MESSAGE_MAP(CInterpApp, CWinApp)
	//{{AFX_MSG_MAP(CInterpApp)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	//}}AFX_MSG
//	ON_COMMAND(ID_HELP, CWinApp::OnHelp)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CInterpApp construction

CInterpApp::CInterpApp()
{
	m_pProject = NULL;
	m_pState = NULL;
}

/////////////////////////////////////////////////////////////////////////////
// The one and only CInterpApp object

CInterpApp theApp;

/////////////////////////////////////////////////////////////////////////////
// CInterpApp initialization

BOOL CInterpApp::InitInstance()
{
	// Check for other instances currently active and shut them down
	TASK_LIST tlist[256];
	DWORD dwTasks, i;

	dwTasks = FillTaskList(tlist, 256);
	for( i = 0; i < dwTasks; i++ )
	{
		if (!_stricmp(tlist[i].ProcessName, "INTERP.EXE"))
		{
			if (tlist[i].dwProcessId!=GetCurrentProcessId())
			{
				HANDLE hProcess;

				hProcess = OpenProcess(PROCESS_TERMINATE, FALSE, tlist[i].dwProcessId);
				if ( hProcess )
					TerminateProcess(hProcess, 0);
			}
		}
	}; // for i
	
	char szDefaultLanguage[64];
	LANGID langidDefault;

	// Obtain the locale from the system and set the current locale to the
	// obtained locale.
	langidDefault = GetSystemDefaultLangID();
	GetLocaleInfo(langidDefault, LOCALE_SENGLANGUAGE, szDefaultLanguage, 64);
	setlocale(LC_ALL, szDefaultLanguage);

	// Standard initialization
	// If you are not using these features and wish to reduce the size
	//  of your final executable, you should remove from the following
	//  the specific initialization routines you do not need.

#ifdef _AFXDLL
	Enable3dControls();			// Call this when using MFC in a shared DLL
#else
	Enable3dControlsStatic();	// Call this when linking to MFC statically
#endif

	// create main MDI Frame window
	CMainFrame* pMainFrame = new CMainFrame;
	if (!pMainFrame->LoadFrame(IDR_MAINFRAME))
		return FALSE;
	m_pMainWnd = pMainFrame;
	// the frame window is invisible and is only used for DDE

	EnableShellOpen();

  // falls ein Kommando per Kommandozeile übergeben wurde, dieses ausführen 
  // und dann das Programm verlassen
  // ansonsten auf ein DDE - Kommando warten
  if( _tcslen( m_lpCmdLine ) > 0 )
  {
    OnDDECommand(m_lpCmdLine, m_pMainWnd);
    m_pMainWnd->SendMessage(WM_CLOSE);
    // Since the dialog has been closed, return FALSE so that we exit the
    //  application, rather than start the application's message pump.
    return FALSE;
  }
  else
    return TRUE;
}; // OnInitInstance

void CInterpApp::LoadProject(CString& dir)
{
	if (m_pProject!=NULL)
		delete m_pProject;
	m_pProject = new Project(dir);
	m_pProject->Load();
}

int CInterpApp::ExitInstance() 
{
	if (m_pProject!=NULL)
		delete m_pProject;
	return CWinApp::ExitInstance();
}

BOOL CInterpApp::OnDDECommand(LPTSTR lpszCommand, CWnd* pParent) 
{
   if (CWinApp::OnDDECommand(lpszCommand))
      return TRUE;
	CString strCommand = lpszCommand;
	CString strProject, strState, strType;
	State *st;
	BOOL bDemo = FALSE;
	BOOL bBatch = FALSE;

	// format is "[project("%s","%s","%s")]" - no whitespace allowed, one per line
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

		// then open the project
		LoadProject(strProject);

		if (m_pProject==NULL)
			return FALSE;

		if (strCommand.Left(3) != _T("\",\""))
			return TRUE; // no state given
		else
		{
			strCommand = strCommand.Right(strCommand.GetLength() - 3);
			i = strCommand.Find('"');
			if (i == -1)
				return FALSE; // illegally terminated
			else
			{
				strState = strCommand.Left(i);
				strCommand = strCommand.Right(strCommand.GetLength() - i);
			}
		}

		if (strCommand.Left(3) == _T("\",\""))
		{
			strCommand = strCommand.Right(strCommand.GetLength() - 3);
			i = strCommand.Find('"');
			if (i == -1)
				return FALSE; // illegally terminated
			else
			{
				strType = strCommand.Left(i);
				strCommand = strCommand.Right(strCommand.GetLength() - i);
				if (strType.CompareNoCase("DEMO")==0)
					bDemo = TRUE;
				else if (strType.CompareNoCase("BATCH")==0)
					bBatch = TRUE;
			}
		}

    st = m_pProject->GetFirstState();
		while (st!=NULL)
		{
			CString fileName = st->GetFileName();
			fileName.MakeLower();
			if (fileName == strState)
				break;
			st = m_pProject->GetNextState();
		}
		if (st==NULL)
			return FALSE;
		
		m_pState = st;

		CInterpSheet dlg(IDS_INTERP, pParent, 0, bDemo, bBatch);
		int nResponse = dlg.DoModal();
		if (nResponse == IDCANCEL)
			m_strFiles.RemoveAll();

		return TRUE;
	}

   // Return FALSE for any unhandled DDE commands.
   return FALSE;
}
