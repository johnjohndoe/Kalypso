// statedialog.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "..\..\wspprj\wspprj.h"

#include "statedialog.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CStateDialog 


CStateDialog::CStateDialog( Project* project, CWnd* pParent /*=NULL*/ )
	: CDialog(CStateDialog::IDD, pParent)
{
  m_project = project;
  m_state = NULL;
  
	//{{AFX_DATA_INIT(CStateDialog)
		// HINWEIS: Der Klassen-Assistent fügt hier Elementinitialisierung ein
	//}}AFX_DATA_INIT
}


void CStateDialog::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CStateDialog)
	DDX_Control(pDX, IDOK, m_okButton);
	DDX_Control(pDX, IDCANCEL, m_cancelButton);
	DDX_Control(pDX, IDC_STATE_COMBO, m_stateCombo);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CStateDialog, CDialog)
	//{{AFX_MSG_MAP(CStateDialog)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CStateDialog 

BOOL CStateDialog::OnInitDialog() 
{
	CDialog::OnInitDialog();

  ASSERT( m_project );

  // Texte setzen ( für Mehrsprachige Version )
  SetWindowText( CString(MAKEINTRESOURCE( IDS_STATE_DLG_TITLE ) ) );
  m_okButton.SetWindowText( CString( MAKEINTRESOURCE( IDS_OK ) ) );
  m_cancelButton.SetWindowText( CString( MAKEINTRESOURCE( IDS_CANCEL ) ) );

  // ComboBox füllen

  State* state = m_project->GetFirstState();
  while( state )
  {
    int index = m_stateCombo.AddString( state->GetWaterName() + "-" + state->GetName() );
    m_stateCombo.SetItemDataPtr( index, state );

    state = m_project->GetNextState();
  }; // while state

  m_stateCombo.SetCurSel( 0 );
  m_stateCombo.SetFocus();
	
	return FALSE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
};


State* CStateDialog::GetCurrentState()
// gibt den ausgewälten Zustand zurück
// NULL, falls nix ausgewählt ist
{
  return m_state;
}; // GetcurrentState

void CStateDialog::OnOK() 
{
	m_state = (State*)m_stateCombo.GetItemDataPtr( m_stateCombo.GetCurSel() );
	
	CDialog::OnOK();
}; // OnOk
