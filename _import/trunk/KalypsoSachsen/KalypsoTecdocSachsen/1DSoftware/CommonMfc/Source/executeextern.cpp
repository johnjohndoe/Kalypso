// executeextern.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "commresource.h"

#include "executeextern.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CExecuteExtern 


CExecuteExtern::CExecuteExtern( const CString& text, const CString& cmdLine, CWnd* pParent /* = NULL */ )   // Standardkonstruktor
	: CDialog( CExecuteExtern::IDD, pParent ), m_cmdLine( cmdLine )
{
	//{{AFX_DATA_INIT(CExecuteExtern)
	m_exeName = _T("");
	//}}AFX_DATA_INIT
  m_exeName = text;
}


void CExecuteExtern::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CExecuteExtern)
	DDX_Text(pDX, IDC_EXECUTE_EXTERN_NAME, m_exeName);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CExecuteExtern, CDialog)
	//{{AFX_MSG_MAP(CExecuteExtern)
	ON_WM_TIMER()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CExecuteExtern 


BOOL CExecuteExtern::OnInitDialog() 
{
	CDialog::OnInitDialog();

	// den Prozess starten und einen Timer setzen, der schaut, ob der Prozess noch da ist
  ::GetStartupInfo( &m_sui );
  m_sui.lpReserved = NULL;
  m_sui.lpTitle = "";
  m_sui.dwFlags |= STARTF_USESHOWWINDOW;
  m_sui.wShowWindow = SW_SHOWNORMAL;

  CString line( m_cmdLine );
  BOOL bCreate = ::CreateProcess( NULL, line.GetBuffer( m_cmdLine.GetLength() ), NULL, NULL, TRUE, NORMAL_PRIORITY_CLASS, NULL, NULL, &m_sui, &m_pi );
  DWORD lastError = GetLastError();
  line.ReleaseBuffer();

  // we dont need the thread handle, so close it
  CloseHandle( m_pi.hThread );

  if( bCreate )
  {
	  // einen Timer einrichten, der den Thread überwacht
	  m_timerID = SetTimer( 1, 500, NULL );
	  if( m_timerID == 0 )
	  {
		CloseHandle( m_pi.hProcess );
		EndDialog( IDOK ); // der Timer konnte nicht gestartet werden!
	  }
  }
  else
  {
	LPVOID lpMsgBuf;
	::FormatMessage( FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
		NULL, lastError, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPTSTR) &lpMsgBuf,0, NULL );

	CString systemMsg( (LPCTSTR)lpMsgBuf );

	::LocalFree( lpMsgBuf );

	AfxMessageBox( m_cmdLine + "\n\n" + systemMsg, MB_OK | MB_ICONEXCLAMATION );
	
	EndDialog( IDCANCEL );
  }
  
  return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

void CExecuteExtern::OnTimer( UINT nIDEvent ) 
{
  if( m_timerID == nIDEvent )
  {
    DWORD m_result;
    if( GetExitCodeProcess( m_pi.hProcess, &m_result ) && m_result != STILL_ACTIVE )
    {
       KillTimer( m_timerID );
	   CloseHandle( m_pi.hProcess );
       CDialog::OnOK();
    }
  }

	
	CDialog::OnTimer( nIDEvent );
}
