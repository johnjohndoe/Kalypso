// updatedatablocksdlg.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "resource.h"

#include "updatedatablocksdlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CUpdateDatablocksDlg 


CUpdateDatablocksDlg::CUpdateDatablocksDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CUpdateDatablocksDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CUpdateDatablocksDlg)
	m_heading = _T("");
	//}}AFX_DATA_INIT
}

CUpdateDatablocksDlg::~CUpdateDatablocksDlg()
{
  // Daten l�schen
  for( int i = 0; i < m_data.GetSize(); i++ )
    delete m_data[i];
  m_data.RemoveAll();
};

void CUpdateDatablocksDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CUpdateDatablocksDlg)
	DDX_Control(pDX, IDC_DATABLOCKS_LIST, m_listCtrl);
	DDX_Text(pDX, IDC_DATABLOCKS_STATIC, m_heading);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CUpdateDatablocksDlg, CDialog)
	//{{AFX_MSG_MAP(CUpdateDatablocksDlg)
	ON_NOTIFY(LVN_ITEMCHANGED, IDC_DATABLOCKS_LIST, OnItemchangedDatablocksList)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen f�r Nachrichten CUpdateDatablocksDlg 

BOOL CUpdateDatablocksDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();

  SetWindowText( CString(MAKEINTRESOURCE(IDS_DATABLOCKS_TITLE)) );
  m_heading.LoadString(IDS_DATABLOCKS_HEADING);

  // ListCtrl f�llen
  CRect rect;
  m_listCtrl.GetClientRect( rect );

	m_StateImageList.Create(IDB_STATEICONS, 16, 1, RGB(255, 0, 0));

	m_listCtrl.SetImageList( &m_StateImageList, LVSIL_STATE );
  m_listCtrl.SetStateIcons( TRUE );
	
  m_listCtrl.InsertColumn( 0, "", LVCFMT_LEFT, rect.Width() ); // es gibt nur eine Spalte, keine �berschrift

	for( int i = 0; i < m_data.GetSize(); i++ )
  {
    Data* data = m_data[i];
    if( data )
    {
      int index = m_listCtrl.InsertItem( LVIF_TEXT | LVIF_PARAM, i, data->name, 0, 0, 0, DWORD(data) );
      m_listCtrl.SetCheck( index, data->checked ); // muss vor SetItemData stehen, da 
    }; // if data
  }; // for i

  UpdateData( FALSE );
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zur�ckgeben
}

void CUpdateDatablocksDlg::OnItemchangedDatablocksList(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_LISTVIEW* pNMListView = (NM_LISTVIEW*)pNMHDR;

  // rausfinden, ob das Item gerade gechecked wurde
  Data* data = (Data*)pNMListView->lParam;
  int index = pNMListView->iItem;

  // falls gecheckt wird die Ausgeschlossenen ent-checken
  if( m_listCtrl.GetCheck( index ) == TRUE && data->checked == FALSE )
  {
    for( int i = 0; i < data->exclusions.GetSize(); i++ )
      m_listCtrl.SetCheck( data->exclusions[i], FALSE );
  }; // wenn das item gechecked wurde
  
  // jetzt den checkstate der Daten richtig setzen
  data->checked = m_listCtrl.GetCheck( index );

  // die Abh�ngigen gleich selektieren wie dieses
  for( int i = 0; i < data->inclusions.GetSize(); i++ )
    m_listCtrl.SetCheck( data->inclusions[i], data->checked );
	
	*pResult = 0;
};

void CUpdateDatablocksDlg::GetCheckedTypes( CArray<int, int>& typeArray )
// f�llt eine Liste mit den angekreuzten Typen
// Parameter:
//        CArray<int, int> typeArray: hier werden die angekreuzten Typen hinzugef�gt
// Bemerkung: evtl. vorhandene Daten in typeArray werden auf jeden Fall gel�scht
{
  typeArray.RemoveAll();
  for( int i = 0; i < m_data.GetSize(); i++ )
  {
    Data* data = m_data[i];
    if( data->checked )
      typeArray.Add( data->type );
  }; // for i
}; // GetCheckedTypes

void CUpdateDatablocksDlg::AddExclusion( const int index, const int exclusion )
// f�gt einen neuen Aussschluss hinzu
// Parameter:
//        const int index: dieses Feld bekommt den Ausschluss
//        const int exclusion: dieses Feld wird ausgeschlossen
{
  if( 0 <= index && index < m_data.GetSize() && 0 <= exclusion && exclusion < m_data.GetSize() && index != exclusion )
    m_data[index]->exclusions.Add( exclusion );
};

void CUpdateDatablocksDlg::AddInclusion( const int index, const int inclusion )
// f�gt einen neue Abh�ngigkeit hinzu
// Parameter:
//        const int index: dieses Feld bekommt die Abh�ngigkeit
//        const int inclusion: dieses Feld wird angeh�ngt
{
  if( 0 <= index && index < m_data.GetSize() && 0 <= inclusion && inclusion < m_data.GetSize() && index != inclusion )
    m_data[index]->inclusions.Add( inclusion );
}; // SetInclusion
