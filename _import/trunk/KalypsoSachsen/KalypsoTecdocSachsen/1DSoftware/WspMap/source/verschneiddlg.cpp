// verschneiddlg.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "resource.h"

#include "maplayer.h"
#include "zuordnung.h"

#include "verschneiddlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CVerschneidDlg 


CVerschneidDlg::CVerschneidDlg( CString nutzDirectory, CMapLayer* mapLayer, CWnd* pParent /*=NULL*/ )
	: CDialog(CVerschneidDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CVerschneidDlg)
	m_action = -1;
	m_staticArt = _T("");
	m_reli = -1;
	m_staticReli = _T("");
	//}}AFX_DATA_INIT

  m_mapLayer = mapLayer;
  m_zList = new CZList(nutzDirectory);
  m_zTable = NULL;
  m_bDeleteTheme = FALSE;
};

CVerschneidDlg::~CVerschneidDlg()
{
  delete m_zList;
};


void CVerschneidDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CVerschneidDlg)
	DDX_Control(pDX, IDC_CHECK_DELETE_THEME, m_themeCheck);
	DDX_Control(pDX, IDC_FIELD_CHECK, m_fieldCheck);
	DDX_Control(pDX, IDC_VERSCHNEID_EDIT_CONSTRAINT, m_editConstraint);
	DDX_Control(pDX, IDC_VERSCHNEID_COMBO_TYPE, m_typeCombo);
	DDX_Control(pDX, IDC_VERSCHNEID_COMBO_FIELDS, m_fieldCombo);
	DDX_Radio(pDX, IDC_VERSCHNEID_RADIO_NUTZ, m_action);
	DDX_Text(pDX, IDC_VERSCHNEID_STATIC_ART, m_staticArt);
	DDX_Radio(pDX, IDC_VERSCHNEID_RADIO_LEFT, m_reli);
	DDX_Text(pDX, IDC_VERSCHNEID_STATIC_RELI, m_staticReli);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CVerschneidDlg, CDialog)
	//{{AFX_MSG_MAP(CVerschneidDlg)
	ON_BN_CLICKED(IDC_VERSCHNEID_RADIO_NUTZ, OnVerschneidRadio)
	ON_BN_CLICKED(IDC_FIELD_CHECK, OnFieldCheck)
	ON_BN_CLICKED(IDC_VERSCHNEID_RADIO_PROFIL, OnVerschneidRadio)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CVerschneidDlg 

BOOL CVerschneidDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();

  // Texte setzen
  SetWindowText( CString( MAKEINTRESOURCE( IDS_VERSCHNEID_TITLE ) ) );
  SetDlgItemText( IDC_VERSCHNEID_RADIO_NUTZ, CString(MAKEINTRESOURCE(IDS_VERSCHNEID_NUTZ)) );
  SetDlgItemText( IDC_VERSCHNEID_RADIO_PROFIL, CString(MAKEINTRESOURCE(IDS_VERSCHNEID_PROFIL)) );
  m_staticReli.LoadString( IDS_VERSCHNEID_STATIC_UFER );
  SetDlgItemText( IDC_VERSCHNEID_RADIO_LEFT, CString(MAKEINTRESOURCE(IDS_VERSCHNEID_LEFT)) );
  SetDlgItemText( IDC_VERSCHNEID_RADIO_RIGHT, CString(MAKEINTRESOURCE(IDS_VERSCHNEID_RIGHT)) );
  m_themeCheck.SetWindowText( CString(MAKEINTRESOURCE(IDS_VERSCHNEID_DELETE_THEME) ) );

  // Daten initialisieren

  m_zList->Load();

  // FieldCombo füllen
  CMoRecordset records( m_mapLayer->GetRecords() );
  if( LPDISPATCH(records) )
  {
    CMoTableDesc table( records.GetTableDesc() );
    if( LPDISPATCH(table) )
    {
      for( short i = 0; i < table.GetFieldCount(); i++ )
      {
        CString* name = new CString( table.GetFieldName( i ) );
        int index = m_fieldCombo.AddString( *name );
        m_fieldCombo.SetItemDataPtr( index, name );
      }; // for i
    }; // if table
  }; // if records
  
  m_fieldCombo.SetCurSel(0);

  m_fieldCheck.SetCheck( FALSE );
  m_themeCheck.SetCheck( FALSE );

  if( m_mapLayer->GetShapeType() != moShapeTypePolygon || m_zList->GetZTableCount() == 0 )
  {
    GetDlgItem( IDC_VERSCHNEID_RADIO_NUTZ )->EnableWindow( FALSE );
    m_action = 1;
  }
  else
    m_action = 0;

  m_reli = 0;
  
  UpdateData( FALSE );

  OnVerschneidRadio();

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

void CVerschneidDlg::OnOK() 
{
  // Daten aus Dialog auslesen
	UpdateData( TRUE );

  if( m_fieldCheck.GetCheck() || !m_action )
    m_feature = *((CString*)m_fieldCombo.GetItemDataPtr( m_fieldCombo.GetCurSel() ));
  else
    m_feature.Empty();
  m_editConstraint.GetWindowText( m_constraint );

  int typeIndex = m_typeCombo.GetCurSel();
  if( m_action )
    m_type = (CLayer::LayerType)m_typeCombo.GetItemData( typeIndex );
  else
    m_zTable = (CZTable*)m_typeCombo.GetItemDataPtr( typeIndex );

  if( m_reli )
    m_reliStr = TEXT("RE");
  else
    m_reliStr = TEXT("LI");

  m_bDeleteTheme = m_themeCheck.GetCheck();
	
	CDialog::OnOK();
}; // OnOK

void CVerschneidDlg::OnVerschneidRadio() 
{
  UpdateData( TRUE );

  m_typeCombo.ResetContent();

  GetDlgItem( IDC_VERSCHNEID_RADIO_LEFT )->EnableWindow( m_action );
  GetDlgItem( IDC_VERSCHNEID_RADIO_RIGHT )->EnableWindow( m_action );
  //m_fieldCheck.EnableWindow( m_action );
  m_editConstraint.EnableWindow( m_action && m_fieldCheck.GetCheck() );
  
  m_fieldCombo.EnableWindow( !m_action || m_fieldCheck.GetCheck() );

  m_themeCheck.EnableWindow( m_action );

  if( m_action )
  { // 'Profilbezogene Daten' ausgewählt
    m_staticArt.LoadString( IDS_VERSCHNEID_ART_PROFIL );
    m_fieldCheck.SetWindowText( CString( MAKEINTRESOURCE( IDS_VERSCHNEID_FIELDS_PROFIL ) ) );
    m_fieldCheck.SetCheck( FALSE );
    m_fieldCheck.SetButtonStyle( BS_AUTOCHECKBOX );

    // artCombo füllen
    for( int i = 0; i < m_strings.GetSize(); i++ )
    {
      int index = m_typeCombo.AddString( m_strings[i] );
      m_typeCombo.SetItemData( index, m_types[i] );
    }; // for i
    m_typeCombo.SetCurSel( 0 );
  }
  else
  { // 'Nutzungsklassen ausgewählt
    m_staticArt.LoadString( IDS_VERSCHNEID_ART_NUTZ );
    m_fieldCheck.SetWindowText( CString( MAKEINTRESOURCE( IDS_VERSCHNEID_FIELDS_NUTZ ) ) );
    m_fieldCheck.SetCheck( TRUE );
    m_fieldCheck.SetButtonStyle( BS_CHECKBOX );

    // typeCombo füllen
    for( int i = 0; i < m_zList->GetZTableCount(); i++ )
    {
      int index = m_typeCombo.AddString( m_zList->GetZTable( i )->GetName() );
      m_typeCombo.SetItemDataPtr( index, m_zList->GetZTable( i ) );
    }; // for i
    m_typeCombo.SetCurSel( 0 );
  }; // if m_action
	
  UpdateData( FALSE );
}; // OnVerschneidRadio

BOOL CVerschneidDlg::DestroyWindow() 
{
  // die FieldsCombo löschen ( die Strings darin )
	for( int i = 0; i < m_fieldCombo.GetCount(); i++ )
  {
    CString* str = (CString*)m_fieldCombo.GetItemDataPtr( i );
    delete str;
  }; // for i
  m_fieldCombo.ResetContent();
	
	return CDialog::DestroyWindow();
}; // Destroy Window

void CVerschneidDlg::AddEntry( CString text, CLayer::LayerType type )
{
  m_strings.Add( text );
  m_types.Add( type );
};

void CVerschneidDlg::OnFieldCheck() 
{
  m_fieldCombo.EnableWindow( m_fieldCheck.GetCheck() );
  m_editConstraint.EnableWindow( m_fieldCheck.GetCheck() );
}; // OnFieldCheck
