// csregion.cpp: Implementierungsdatei
//

#include "stdafx.h"
#include "..\..\commonMfc\include\commResource.h"

#include "cSection.h"
#include "state.h"

#include "csregion.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CCsRegion 


CCsRegion::CCsRegion( State* state, CWnd* pParent /*=NULL*/ )	: CDialog(CCsRegion::IDD, pParent)
{
  this->state = state;
  startIndex = -1;
  endIndex = -1;
	//{{AFX_DATA_INIT(CCsRegion)
		// HINWEIS: Der Klassen-Assistent fügt hier Elementinitialisierung ein
	//}}AFX_DATA_INIT
}


void CCsRegion::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CCsRegion)
	DDX_Control(pDX, IDOK, m_okbutton);
	DDX_Control(pDX, IDCANCEL, m_cancelButton);
	DDX_Control(pDX, IDC_END, m_end);
	DDX_Control(pDX, IDC_BEGIN, m_begin);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CCsRegion, CDialog)
	//{{AFX_MSG_MAP(CCsRegion)
	ON_CBN_SELCHANGE(IDC_BEGIN, OnSelchangeBegin)
	ON_CBN_SELCHANGE(IDC_END, OnSelchangeEnd)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CCsRegion 

BOOL CCsRegion::OnInitDialog() 
{
  ASSERT(state);

	CDialog::OnInitDialog();


  CString helpString;

  // Fenstertitel setzen
  helpString.LoadString( IDS_CS_REGION_TITLE );
  SetWindowText( helpString );

  // die beiden ComboBoxes mit Daten füllen
	CrossSection* cs = state->GetFirstCrossSection();
  while ( cs )
  {
    helpString.Format( "%.4lf km", cs->GetStation() );
    m_begin.AddString( helpString );
    m_end.AddString( helpString );

    cs = state->GetNextCrossSection();
  }; // while cs

  m_begin.SetCurSel( 0 );
  m_end.SetCurSel( m_end.GetCount() - 1 );

  m_begin.SetFocus();
	
	return FALSE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}


void CCsRegion::OnSelchangeBegin() 
{
	int index = m_begin.GetCurSel();
  if ( m_end.GetCurSel() < index )
    m_end.SetCurSel( index );
}

void CCsRegion::OnSelchangeEnd() 
{
  int index = m_end.GetCurSel();
  if ( m_begin.GetCurSel() > index )
    m_begin.SetCurSel( index );
}


void CCsRegion::OnOK() 
{
	startIndex = m_begin.GetCurSel();
  endIndex = m_end.GetCurSel();
	
	CDialog::OnOK();
};
