// mainfrm.cpp : implementation file
//

#include "stdAfx.h"

#include "interp.h"

#include "mainfrm.h"


#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CMainFrame

IMPLEMENT_DYNCREATE(CMainFrame, CFrameWnd)

CMainFrame::CMainFrame()
{
	m_bError = FALSE;
}

CMainFrame::~CMainFrame()
{
}


BEGIN_MESSAGE_MAP(CMainFrame, CFrameWnd)
	//{{AFX_MSG_MAP(CMainFrame)
		// NOTE - the ClassWizard will add and remove mapping macros here.
	//}}AFX_MSG_MAP
	ON_MESSAGE(WM_DDE_EXECUTE, OnDDEExecute)
	ON_MESSAGE(WM_DDE_REQUEST, OnDDERequest)
	ON_MESSAGE(WM_DDE_TERMINATE, OnDDETerminate)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CMainFrame message handlers

// always ACK the execute command - even if we do nothing
LRESULT CMainFrame::OnDDEExecute(WPARAM wParam, LPARAM lParam)
{
	// unpack the DDE message
	UINT unused;
	HGLOBAL hData;
	VERIFY(UnpackDDElParam(WM_DDE_EXECUTE, lParam, &unused, (UINT*)&hData));

	// get the command string
	TCHAR szCommand[_MAX_PATH * 2];
	LPCTSTR lpsz = (LPCTSTR)GlobalLock(hData);
	lstrcpyn(szCommand, lpsz, sizeof(szCommand)/sizeof(szCommand[0]));
	GlobalUnlock(hData);

	// don't execute the command when the window is disabled
	if (!IsWindowEnabled())
	{
		TRACE1("Warning: DDE command '%s' ignored because window is disabled.\n",
			szCommand);
	}
	else
	{
		CWnd parent;
		HWND hwndMain;

		hwndMain = ::GetParent((HWND)wParam);
		if (hwndMain==NULL)
			hwndMain = (HWND)wParam;
		if (parent.Attach(hwndMain))
		{
			// Execute the command. The dialogs parent is the client DDE window.
			if (!theApp.OnDDECommand(szCommand, &parent))
				TRACE1("Error: failed to execute DDE command '%s'.\n", szCommand);
			parent.Detach();
		}
	}

	// acknowledge now
	::PostMessage((HWND)wParam, WM_DDE_ACK, (WPARAM)m_hWnd,
		ReuseDDElParam(lParam, WM_DDE_EXECUTE, WM_DDE_ACK,
		(UINT)0x8000, (UINT)hData));

	return 0L;
}

LRESULT CMainFrame::OnDDERequest(WPARAM wParam, LPARAM lParam)
{
	HGLOBAL hData;
	CString strData, strAtom;
	TCHAR szData[_MAX_PATH * 2];
	char szAtom[_MAX_PATH];
	DDEDATA *lpData;
	int i;
	LPARAM lPar;

	// unpack the DDE message
	ATOM atom = HIWORD(lParam);
	if (LOWORD(lParam)==CF_TEXT)
	{
		if (HIWORD(lParam)!=0)
		{
			VERIFY(GlobalGetAtomName(HIWORD(lParam),
				szAtom, _MAX_PATH - 1) != 0);
			strAtom = szAtom;
			if (strAtom.CompareNoCase("files")==0)
			{
				if (theApp.m_strFiles.GetSize()>0 || m_bError)
				{
					strData = theApp.m_pProject->GetDataDir();
					strData = strData.Left(strData.GetLength()-1);
					for (i=0; i<theApp.m_strFiles.GetSize(); i++)
					{
						strData += " ";
						strData +=  theApp.m_strFiles[i];
					}
					strData += "\r\n";
					if (m_bError)
						strData = "Fehler\r\n";
					lstrcpyn((LPSTR) szData, (LPSTR) strData.GetBuffer(strData.GetLength()), strData.GetLength()); 
					strData.ReleaseBuffer();
					
					/* 
					* Allocate the size of the DDE data header, plus the data: a 
					* string,<CR><LF><NULL>. The byte for the string's terminating 
					* null character is counted by DDEDATA.Value[1]. 
					*/ 
					
					if ((hData = ::GlobalAlloc(GMEM_MOVEABLE | GMEM_DDESHARE, 
						(LONG) sizeof(DDEDATA) + lstrlen(szData) + 2)))
					{
						if (!(lpData = (DDEDATA FAR*) ::GlobalLock(hData)))
							GlobalFree(hData);
						else
						{
							lpData->cfFormat = CF_TEXT;
							lpData->fRelease = TRUE;
							lstrcpy((LPSTR) lpData->Value, (LPSTR) szData); 
							
							/* Each line of CF_TEXT data is terminated by CR/LF. */ 
							
							::GlobalUnlock(hData);
							if (atom!=0)
							{ 
								lPar = ::PackDDElParam(WM_DDE_DATA, (UINT) hData, atom); 
								if (!::PostMessage((HWND)wParam, 
									WM_DDE_DATA, 
									(WPARAM) m_hWnd, 
									lPar))
								{ 
									::GlobalFree(hData); 
									::FreeDDElParam(WM_DDE_DATA, lPar);
								}
								else
									return 0L;
							}
						}
					}
				}
			}
		}
	}
 
	/* negative acknowledgment */ 
 
	::PostMessage((HWND)wParam, 
		WM_DDE_ACK, 
		(WPARAM) m_hWnd, 
		PackDDElParam(WM_DDE_ACK, 0, atom));

	return 0L;	
}

LRESULT CMainFrame::OnDDETerminate(WPARAM wParam, LPARAM lParam)
{
	::PostMessage((HWND)wParam, WM_DDE_TERMINATE, (WPARAM)m_hWnd, lParam);
	return 0L;
}