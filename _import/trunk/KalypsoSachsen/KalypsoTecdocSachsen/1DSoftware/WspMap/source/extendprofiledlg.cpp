// extendprofiledlg.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "resource.h"

#include "..\..\wspprj\wspprj.h"

#include "extendprofiledlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif


/* static */ 
double CExtendProfileDlg::s_ausduennLimit = 0.05;

CExtendProfileDlg::CExtendProfileDlg( const CString& titel, const CString& okText, 
                                      Profil* profil, TripleArray* tripleArray /* = NULL */, CWnd* pParent /* = NULL */ )
	: CDialog(CExtendProfileDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CExtendProfileDlg)
	//}}AFX_DATA_INIT

  m_titel = titel;
  m_okText = okText;

  m_profil = profil;

  m_oldTripleArray = tripleArray;

  m_editColor = RGB( 0, 0, 0 );

  m_profilCtrl = new ProfilCtrl( CString( MAKEINTRESOURCE( IDS_EXTEND_PROFILE_TITLE ) ) );
}

CExtendProfileDlg::~CExtendProfileDlg()
{
  delete m_profilCtrl;
}; // Destruktor


void CExtendProfileDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CExtendProfileDlg)
	DDX_Control(pDX, IDC_PROFILE_DUMMY, m_profileDummyCtrl);
	DDX_Text(pDX, IDC_EDIT_AUSDUENN, s_ausduennLimit);
	DDV_MinMaxDouble(pDX, s_ausduennLimit, 0.01, 10.0);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CExtendProfileDlg, CDialog)
	//{{AFX_MSG_MAP(CExtendProfileDlg)
	ON_WM_PAINT()
	ON_WM_CREATE()
	ON_EN_UPDATE(IDC_EDIT_AUSDUENN, OnUpdateEditAusduenn)
	ON_WM_CTLCOLOR()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CExtendProfileDlg 

BOOL CExtendProfileDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
  // alle Texte setzen
  SetWindowText( m_titel );
  GetDlgItem( IDOK )->SetWindowText( m_okText );
  GetDlgItem( IDCANCEL )->SetWindowText( CString(MAKEINTRESOURCE(IDS_CANCEL)) );
  GetDlgItem( IDC_STATIC_AUSDUENN )->SetWindowText( CString( MAKEINTRESOURCE(IDS_AUSUENN_TEXT) ) );

  m_profilCtrl->SetProfil( m_profil );

  // wenn keine Trippledaten mitgeliefert wurden, alles verstecken
  if( m_oldTripleArray == NULL )
  {
    GetDlgItem( IDC_EDIT_AUSDUENN )->ShowWindow( SW_HIDE );
    GetDlgItem( IDC_STATIC_COUNT )->ShowWindow( SW_HIDE );
  }

  OnUpdateEditAusduenn();

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

void CExtendProfileDlg::OnPaint() 
{
	CPaintDC dc(this); // device context for painting
	
  // das Profilfenster auf die DummyCtrl plazieren
  if( m_profilCtrl->GetSafeHwnd() && m_profileDummyCtrl.GetSafeHwnd() )
  {
    CRect rect;
    m_profileDummyCtrl.GetWindowRect( rect );
    ScreenToClient( rect );
    m_profilCtrl->MoveWindow( rect, TRUE );
  }; // if m_profileCtr && m_profileDummyCtrl

	// Kein Aufruf von CDialog::OnPaint() für Zeichnungsnachrichten
}; // OnPaint

int CExtendProfileDlg::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	if (CDialog::OnCreate(lpCreateStruct) == -1)
		return -1;
	
  if( m_profilCtrl->CreateEx( WS_EX_CLIENTEDGE, WS_VISIBLE | WS_CHILD | WS_BORDER, 
                              CRect( 0, 0, 100, 100 ), this, IDC_PROFILE_DUMMY + 1 ) == -1 )
    return -1;

	return 0;
}; // OnCreate

void CExtendProfileDlg::OnOK() 
{
	UpdateData( TRUE );

	// jetzt nochmal mit den richtigen Daten ausdünnen, diesmal die Punkte wirklich löschen
	if( m_oldTripleArray )
		m_oldTripleArray->Smooth( s_ausduennLimit, true );
  
	CDialog::OnOK();
}

void CExtendProfileDlg::OnUpdateEditAusduenn() 
{
	// TODO: Wenn es sich hierbei um ein RICHEDIT-Steuerelement handelt, sendet es
	// sendet diese Benachrichtigung nur, wenn die Funktion CDialog::OnInitDialog()
	// überschrieben wird, um die EM_SETEVENTMASK-Nachricht an das Steuerelement
	// mit dem ENM_UPDATE-Attribut Ored in die Maske lParam zu senden.
	
  if( !m_oldTripleArray )
    return;

	CWnd* editCtrl = GetDlgItem( IDC_EDIT_AUSDUENN );
	CString valStr;
	editCtrl->GetWindowText( valStr );
	double val;
	bool valid = sscanf( LPCTSTR( valStr ), "%lf", &val ) == 1;
	if( !valid )
	{
		m_editColor = RGB( 255,0, 0 );
		return;
	}

	m_editColor = RGB( 0, 0, 0 );
	s_ausduennLimit = val;

  // immer wieder mit den Originalpunkten anfangen
  int oldCount = m_oldTripleArray->size();

  m_tripleArray.clear();
  for( TripleArray::const_iterator tIt = m_oldTripleArray->begin(); tIt != m_oldTripleArray->end(); tIt++ )
      m_tripleArray.push_back( *tIt );

  int count = m_tripleArray.Smooth( s_ausduennLimit, false );

  m_profilCtrl->SetTriple( &m_tripleArray );

  CString countStr;
  countStr.Format( CString( MAKEINTRESOURCE( IDS_AUSUENN_MSG ) ), count, oldCount );
  GetDlgItem( IDC_STATIC_COUNT )->SetWindowText( countStr );
}

HBRUSH CExtendProfileDlg::OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor) 
{
	HBRUSH hbr = CDialog::OnCtlColor(pDC, pWnd, nCtlColor);
	
	// TODO: Attribute des Gerätekontexts hier ändern
	
	// TODO: Anderen Pinsel zurückgeben, falls Standard nicht verwendet werden soll
	if( pWnd->GetDlgCtrlID() == IDC_EDIT_AUSDUENN )
		pDC->SetTextColor( m_editColor );

	return hbr;
}
