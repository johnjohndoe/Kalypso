// openmdlg.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "resource.h"

#include "openmdlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld COpenMapDialog 


COpenMapDialog::COpenMapDialog( CStringArray* names, CStringArray* files, CWnd* pParent /*=NULL*/, 
                               BOOL bDelete /*=FALSE*/)
	: CDialog(COpenMapDialog::IDD, pParent)
{
	//{{AFX_DATA_INIT(COpenMapDialog)
		// HINWEIS: Der Klassen-Assistent fügt hier Elementinitialisierung ein
	//}}AFX_DATA_INIT
	m_bDelete = bDelete;
  m_names = names;
  m_files = files;
}


void COpenMapDialog::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(COpenMapDialog)
	DDX_Control(pDX, IDC_LIST1, m_list);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(COpenMapDialog, CDialog)
	//{{AFX_MSG_MAP(COpenMapDialog)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten COpenMapDialog 

BOOL COpenMapDialog::OnInitDialog() 
{
	int i, width;
	CRect rect;
	CFile file;
	CFileStatus rStatus;
	CString str;
	
	CDialog::OnInitDialog();
	
	if (m_bDelete)
	{
		str.LoadString(IDS_DELETE);
		SetWindowText(str);
	}

	m_list.SetFullRowSel(TRUE);
  m_list.SetImageList( CCommonImageList::GetList( FALSE ), LVSIL_SMALL);
	m_list.GetClientRect(&rect);
	width = rect.Width() / 2;
	str.LoadString(IDS_NAME);
  m_list.InsertColumn( 0, LPCTSTR(str), LVCFMT_LEFT, width, 0 );

	str.LoadString(IDS_FILE);
  m_list.InsertColumn( 1, LPCSTR(str), LVCFMT_LEFT, width, 1 );

	for ( i = 0; i < m_names->GetSize(); i++ )
	{
    m_list.InsertItem( LVIF_TEXT | LVIF_IMAGE | LVIF_PARAM | LVIF_STATE, i, 
                        m_names->GetAt( i ), 0, 0, IMAGE_PROJECT, i );
    m_list.SetItemText( i, 1, m_files->GetAt( i ) );
	}

	m_list.SetItemState(0, LVIS_SELECTED, LVIS_SELECTED);
  m_list.SetFocus();
	
	return FALSE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void COpenMapDialog::OnOK() 
{
  if ( UpdateData( TRUE ) )
  {
    for ( int i = 0; i < m_list.GetItemCount(); i++)
    {
      if ( m_list.GetItemState( i, LVIS_SELECTED) == LVIS_SELECTED )
        m_numbers.Add( m_list.GetItemData( i ) );
    }; // for i

    CDialog::OnOK();
  }; // if UpdateData()
}
