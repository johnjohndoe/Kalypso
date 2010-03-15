// observerdialog.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "resource.h"

#include "bce/include/observable.h"
#include "bce/include/tincut.h"

#include "observerdialog.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CObserverDialog 

#define OBS_DIALOG_TIMER 1            // ID des Timers
#define OBS_DIALOG_LOOKUP_TIME 1000   // alle Sekunde nach dem Thread schauen

CObserverDialog::CObserverDialog( BCE::Observable* observedObj, LPVOID observedParam, const CString& titel, 
                                 CWnd* pParent /* = NULL */ )
	: CDialog( CObserverDialog::IDD, pParent )
{
	//{{AFX_DATA_INIT(CObserverDialog)
	m_statusText = _T("");
	//}}AFX_DATA_INIT

  m_observedObj = observedObj;
  m_observedParam = observedParam;
  m_obsThread = NULL;
  m_titel = titel;

  m_returnCode = OBS_STATE_UNKNOWN;
}; // Standardkonstruktor

CObserverDialog::~CObserverDialog()
{
};

void CObserverDialog::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CObserverDialog)
	DDX_Control(pDX, IDC_OBSERVER_PROGRESS, m_progress);
	DDX_Text(pDX, IDC_OBSERVER_TEXT, m_statusText);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CObserverDialog, CDialog)
	//{{AFX_MSG_MAP(CObserverDialog)
	ON_WM_CLOSE()
	ON_WM_TIMER()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CObserverDialog 

BOOL CObserverDialog::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
  // Titel des Dialogs setzen
  SetWindowText( m_titel );

  // den Statustext setzen
  m_statusText = m_observedObj->getObsText();

  // die ProgressControl initialiseren
  m_progress.SetRange32( 0, m_observedObj->getObsMaxProcess() );

  // den Thread ( angehalten ) starten
  m_obsThread = AfxBeginThread( BCE::TinCut::run, m_observedParam, 0, CREATE_SUSPENDED );
  if( m_obsThread == NULL )
    EndDialog( IDOK ); // falls der Thread nicht gestartet werden kann, gleich wieder abbrechen

  // einen Timer einrichten, der den Thread überwacht
  m_timerID = SetTimer( OBS_DIALOG_TIMER, OBS_DIALOG_LOOKUP_TIME, NULL );
  if( m_timerID == 0 )
    EndDialog( IDOK ); // der Timer konnte nicht gestartet werden!

  // den Thread jetzt tatsächlich starten
  m_obsThread->ResumeThread();
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

void CObserverDialog::OnCancel() 
{
	stopThread();
	
	CDialog::OnCancel();
}

void CObserverDialog::OnClose() 
{
	stopThread();
	
	CDialog::OnClose();
}

void CObserverDialog::stopThread()
{
  // Stopt den Thread
  // da zur Zeit noch keine Möglichkeit vorhanden ist
  // dem Thread selbst mitzuteilen, dass er stoppen soll
  // wird er einfach angehalten. Es bleiben Speicherlöscher etc.

  // zuerst den Timer Killen
  KillTimer( OBS_DIALOG_TIMER );

  m_obsThread->SuspendThread();
  //TerminateThread( m_obsThread->m_hThread, 0 );
}; // StopThread

void CObserverDialog::OnTimer( UINT nIDEvent ) 
{
  if( nIDEvent == OBS_DIALOG_TIMER )
  {
    // Nachschauen, ob der Thread noch läuft
    DWORD exitCode;
    if( !GetExitCodeThread( m_obsThread->m_hThread, &exitCode ) || exitCode != STILL_ACTIVE )
    {
      KillTimer( OBS_DIALOG_TIMER );

      m_returnCode = exitCode;

      OnOK();
    };

    // ansonsten den Daloginhalt auffrischen
    m_progress.SetPos( m_observedObj->getObsProcess() );
    m_statusText = m_observedObj->getObsText();
    UpdateData( FALSE );
  }
  else
    CDialog::OnTimer( nIDEvent );
}; // OnTimer
