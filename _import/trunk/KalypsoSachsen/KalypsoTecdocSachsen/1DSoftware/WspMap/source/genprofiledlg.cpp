// genprofiledlg.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "resource.h"

#include "..\..\wspprj\wspprj.h"

#include "genprofiledlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif



/* static */
double CGenProfileDlg::s_ausduennValue = 0.01;

CGenProfileDlg::CGenProfileDlg( const CString& titel, const CString& description,
                                Profil* profil, const CStringArray& zustaende,
                                const CString& predFile, const CString& follFile, const CString& newFile,
                                const double predStation, const double follStation, const double newStation,
                                const int predVzk, const int follVzk, const CUIntArray& newVzks,
                                const CString& predPK, const CString& follPk, const CString& newPK,
                                const double predDist, const double follDist,
                                const BOOL bPred, const BOOL bFollow,
                                TripleArray* tripleArray,
                                CWnd* pParent /* = NULL */ )
	: CDialog( CGenProfileDlg::IDD, pParent )
{
	//{{AFX_DATA_INIT(CGenProfileDlg)
	m_description = _T("");
	m_editFileFollow = _T("");
	m_editFilePred = _T("");
	m_editPkFollow = _T("");
	m_editPkNew = _T("");
	m_editPkPred = _T("");
	m_editStationFollow = _T("");
	m_editStationNew = _T("");
	m_editStationPred = _T("");
	m_editVorLFollow = _T("");
	m_editFlussFollow = _T("");
	m_editFlussPred = _T("");
	m_editVorLPred = _T("");
	m_editVorRFollow = _T("");
	m_editVorRPred = _T("");
	m_editVzkFollow = _T("");
	m_editVzkPred = _T("");
	m_fileText = _T("");
	m_followerText = _T("");
	m_newText = _T("");
	m_pkText = _T("");
	m_predText = _T("");
	m_stateText = _T("");
	m_stationText = _T("");
	m_strangDistText = _T("");
	m_vorLText = _T("");
	m_vorRText = _T("");
	m_vzkText = _T("");
	m_flussText = _T("");
	m_reduceCount = _T("");
	//}}AFX_DATA_INIT

  // Eingangsdaten übernehmen
  m_profil = profil;
  m_oldTripleArray = tripleArray;
  m_profilCtrl = new ProfilCtrl( CString( MAKEINTRESOURCE( IDS_GENPROFILE_TITLE ) ) );
  m_titel = titel;
  m_description = description;

  m_zustaende.Copy( zustaende );


  CString helpStr;

  // Vorgängerprofile:
  if( bPred )
  {
    m_editFilePred = predFile;
    
    helpStr.Format( "%.4lf", predStation );
    m_editStationPred = helpStr;

    helpStr.Format( "%d", predVzk );
    m_editVzkPred = helpStr;

    m_editPkPred = predPK;

    helpStr.Format( "%.4lf", predDist );
    m_editVorLPred = helpStr;
    m_editFlussPred = helpStr;
    m_editVorRPred = helpStr;
  }; // if bPred

  // Nachfolgerprofil

  if( bFollow )
  {
    m_editFileFollow = follFile;

    helpStr.Format( "%.4lf", follStation );
    m_editStationFollow = helpStr;

    helpStr.Format( "%d", follVzk );
    m_editVzkFollow = helpStr;
  
    m_editPkFollow = follPk;

    helpStr.Format( "%.4lf", follDist );
    m_editVorLFollow = helpStr;
    m_editFlussFollow = helpStr;
    m_editVorRFollow = helpStr;
  }; // if bFollow


  // neues Profil
  helpStr.Format( "%.4lf", newStation );
  m_editStationNew = helpStr;

  m_vzks.Copy( newVzks );

  m_editPkNew = newPK;
}; // Konstruktor

CGenProfileDlg::~CGenProfileDlg()
{
  delete m_profilCtrl;
  m_profilCtrl = NULL;
}; // Destruktor


void CGenProfileDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CGenProfileDlg)
	DDX_Control(pDX, IDC_STATE_COMBO, m_stateCombo);
	DDX_Control(pDX, IDC_COMBO_VZK, m_vzkCombo);
	DDX_Control(pDX, IDC_PROFILE_DUMMY, m_profileDummyCtrl);
	DDX_Text(pDX, IDC_DESCRIPTION, m_description);
	DDX_Text(pDX, IDC_EDIT_FILE_FOLLOW, m_editFileFollow);
	DDX_Text(pDX, IDC_EDIT_FILE_PRED, m_editFilePred);
	DDX_Text(pDX, IDC_EDIT_PK_FOLLOW, m_editPkFollow);
	DDX_Text(pDX, IDC_EDIT_PK_NEW, m_editPkNew);
	DDX_Text(pDX, IDC_EDIT_PK_PRED, m_editPkPred);
	DDX_Text(pDX, IDC_EDIT_STATION_FOLLOW, m_editStationFollow);
	DDX_Text(pDX, IDC_EDIT_STATION_NEW, m_editStationNew);
	DDX_Text(pDX, IDC_EDIT_STATION_PRED, m_editStationPred);
	DDX_Text(pDX, IDC_EDIT_VOR_L_FOLLOW, m_editVorLFollow);
	DDX_Text(pDX, IDC_EDIT_FLUSS_FOLLOW, m_editFlussFollow);
	DDX_Text(pDX, IDC_EDIT_FLUSS_PRED, m_editFlussPred);
	DDX_Text(pDX, IDC_EDIT_VOR_L_PRED, m_editVorLPred);
	DDX_Text(pDX, IDC_EDIT_VOR_R_FOLLOW, m_editVorRFollow);
	DDX_Text(pDX, IDC_EDIT_VOR_R_PRED, m_editVorRPred);
	DDX_Text(pDX, IDC_EDIT_VZK_FOLLOW, m_editVzkFollow);
	DDX_Text(pDX, IDC_EDIT_VZK_PRED, m_editVzkPred);
	DDX_Text(pDX, IDC_FILE_TEXT, m_fileText);
	DDX_Text(pDX, IDC_FOLLOWER_TEXT, m_followerText);
	DDX_Text(pDX, IDC_NEW_TEXT, m_newText);
	DDX_Text(pDX, IDC_PK_TEXT, m_pkText);
	DDX_Text(pDX, IDC_PRED_TEXT, m_predText);
	DDX_Text(pDX, IDC_STATE_TEXT, m_stateText);
	DDX_Text(pDX, IDC_STATION_TEXT, m_stationText);
	DDX_Text(pDX, IDC_STRANG_DIST_TEXT, m_strangDistText);
	DDX_Text(pDX, IDC_VOR_L_TEXT, m_vorLText);
	DDX_Text(pDX, IDC_VOR_R_TEXT, m_vorRText);
	DDX_Text(pDX, IDC_VZK_TEXT, m_vzkText);
	DDX_Text(pDX, IDC_FLUSS_TEXT, m_flussText);
	DDX_Text(pDX, IDC_EDIT_AUSDUENN, s_ausduennValue);
	DDV_MinMaxDouble(pDX, s_ausduennValue, 1.e-002, 10.);
	DDX_Text(pDX, IDC_STATIC_COUNT, m_reduceCount);
	//}}AFX_DATA_MAP
};


BEGIN_MESSAGE_MAP(CGenProfileDlg, CDialog)
	//{{AFX_MSG_MAP(CGenProfileDlg)
	ON_WM_PAINT()
	ON_WM_CREATE()
	ON_EN_KILLFOCUS(IDC_EDIT_AUSDUENN, OnKillfocusEditAusduenn)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CGenProfileDlg 

BOOL CGenProfileDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();

  // alle Texte setzen
  SetWindowText( m_titel );
  GetDlgItem( IDOK )->SetWindowText( CString(MAKEINTRESOURCE(IDS_GENERATEPROFILE_OKTEXT) ) );
  GetDlgItem( IDCANCEL )->SetWindowText( CString(MAKEINTRESOURCE(IDS_CANCEL)) );

  m_stateText = CString( MAKEINTRESOURCE(IDS_STATE) );
  m_fileText = CString( MAKEINTRESOURCE(IDS_PROFILE_FILE) );
  m_predText = CString( MAKEINTRESOURCE(IDS_PRED_PROFILE) );
  m_followerText = CString( MAKEINTRESOURCE(IDS_FOLLOW_PROFILE) );
  m_newText = CString( MAKEINTRESOURCE(IDS_NEW_PROFILE) );
  m_stationText = CString( MAKEINTRESOURCE(IDS_STATION) );
  m_vzkText = CString( MAKEINTRESOURCE(IDS_VZK) );
  m_pkText = CString( MAKEINTRESOURCE(IDS_PK) );
  m_vorLText = CString( MAKEINTRESOURCE(IDS_LEFT) );
  m_vorRText = CString( MAKEINTRESOURCE(IDS_RIGHT) );
  m_flussText = CString( MAKEINTRESOURCE(IDS_RIVER) );
  m_strangDistText = CString( MAKEINTRESOURCE(IDS_STRANG_DISTANCE) );

  // das Profil anzeigen

  m_profilCtrl->SetProfil( m_profil );

  // die Comboboxen füllen
  for( int i = 0; i < m_zustaende.GetSize(); i++ )
    m_stateCombo.AddString( m_zustaende[i] );
  m_stateCombo.SetCurSel( 0 );

  for( i = 0; i < m_vzks.GetSize(); i++ )
  {
    CString helpStr;
    helpStr.Format( "%d", m_vzks[i] );
    m_vzkCombo.AddString( helpStr );
  }; // for i
  m_vzkCombo.SetCurSel( 0 );

  GetDlgItem( IDC_STATIC_AUSDUENN )->SetWindowText( CString( MAKEINTRESOURCE(IDS_AUSUENN_TEXT) ) );
  // wenn keine Trippledaten mitgeliefert wurden, alles verstecken
  if( m_oldTripleArray == NULL )
  {
    GetDlgItem( IDC_EDIT_AUSDUENN )->ShowWindow( SW_HIDE );
    GetDlgItem( IDC_STATIC_COUNT )->ShowWindow( SW_HIDE );
  }

  OnKillfocusEditAusduenn();

  UpdateData( FALSE );
 
	return TRUE;  // return TRUE unless you set the focus to a control
}; // OnInitDialog

void CGenProfileDlg::OnPaint() 
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

int CGenProfileDlg::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
  if (CDialog::OnCreate(lpCreateStruct) == -1)
		return -1;

  if( m_profilCtrl->CreateEx( WS_EX_CLIENTEDGE, WS_VISIBLE | WS_CHILD | WS_BORDER, 
                              CRect( 0, 0, 100, 100 ), this, IDC_PROFILE_DUMMY + 1 ) == -1 )
    return -1;

	return 0;
}; // OnCreate

void CGenProfileDlg::OnOK() 
{
  UpdateData( TRUE );

	// die Ausgangsvariblen setzen
  int index = m_stateCombo.GetCurSel();
  if( index != CB_ERR )
    m_stateCombo.GetLBText( index, newState );

  sscanf( m_editStationNew, "%lf", &newStation );

  index = m_vzkCombo.GetCurSel();
  if( index != CB_ERR )
  {
    CString helpStr;
    m_vzkCombo.GetLBText( index, helpStr );
    sscanf( helpStr, "%d", &newVZK );
  }; // if index

  newPK = m_editPkNew;

  // jetzt nochmal mit den richtigen Daten ausdünnen, diesmal die Punkte wirklich löschen
  if( m_oldTripleArray )
    m_oldTripleArray->Smooth( s_ausduennValue, true );
	
	CDialog::OnOK();
}; // OnOK

void CGenProfileDlg::OnKillfocusEditAusduenn() 
{
  if( !m_oldTripleArray )
    return;

  UpdateData( TRUE );

  // immer wieder mit den Originalpunkten anfangen
  int oldCount = m_oldTripleArray->size();

  m_tripleArray.clear();
  for( TripleArray::const_iterator tIt = m_oldTripleArray->begin(); tIt != m_oldTripleArray->end(); tIt++ )
      m_tripleArray.push_back( *tIt );

  int count = m_tripleArray.Smooth( s_ausduennValue, false );

  m_profilCtrl->SetTriple( &m_tripleArray );

  m_reduceCount.Format( CString( MAKEINTRESOURCE( IDS_AUSUENN_MSG ) ), count, oldCount );

  UpdateData( FALSE );
}
