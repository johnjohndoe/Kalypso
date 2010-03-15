// datablocktypechooserdialog.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "dtypes.h"
#include "datablck.h"

#include "datablocktypechooserdialog.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CDataBlockTypeChooserDialog 


CDataBlockTypeChooserDialog::CDataBlockTypeChooserDialog(CWnd* pParent /*=NULL*/)
	: CDialog( IDD_DATABLOCK_TYPE_CHOOSER_DLG, pParent )
{
	//{{AFX_DATA_INIT(CDataBlockTypeChooserDialog)
		// HINWEIS: Der Klassen-Assistent fügt hier Elementinitialisierung ein
	//}}AFX_DATA_INIT
}


void CDataBlockTypeChooserDialog::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CDataBlockTypeChooserDialog)
	DDX_Control(pDX, IDC_DATABLOCK_TYPE_CHOOSER_DLG_LIST, m_dbList);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CDataBlockTypeChooserDialog, CDialog)
	//{{AFX_MSG_MAP(CDataBlockTypeChooserDialog)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CDataBlockTypeChooserDialog 

BOOL CDataBlockTypeChooserDialog::OnInitDialog() 
{
	CDialog::OnInitDialog();

  CImageList* pList = CCommonImageList::GetList( FALSE );
  m_dbList.SetImageList( pList, LVSIL_SMALL );
  
  CRect clientRect;
  m_dbList.GetClientRect( clientRect );
  m_dbList.InsertColumn( 0, "Datablock", LVCFMT_LEFT, clientRect.Width() - 20 ); // - 20 wegen Platz für Scrollbar
	
  // die  Listcontrol mit allen möglichen Datenblocktypen füllen
  for( int i = N_DSTYPES_MIN; i < N_DSTYPES; i++ )
  {
    DataBlock db( NULL, i );

    CString name = db.GetDesc( 0 );
    
    // falls die Bezeichnung zu kurz ist, ist es vermutlich kein richtiger Datenblock
    if( name.GetLength() > 2 )
      m_dbList.InsertItem( LVIF_TEXT | LVIF_PARAM | LVIF_IMAGE, i, name, 0, 0, db.GetImageType(), i );
  }; // for i
  

  UpdateData( FALSE );
	
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

void CDataBlockTypeChooserDialog::OnOK() 
{
  // liest aus, welche Datenblocktypen selektiert sind
  // und fügt diese als Array zusammen
  POSITION pos = m_dbList.GetFirstSelectedItemPosition();
  while( pos != NULL )
  {
    int nItem = m_dbList.GetNextSelectedItem( pos );
    m_dbs.Add( m_dbList.GetItemData( nItem ) );
  } // while pos

	CDialog::OnOK();
}



/*!
 * Gibt zurück, welche Datenblocktypen gewählt wurden
 * Macht erst Sinn, nach Ausführung des Dialogs
 *
 * @param CArray<int, int>& dbArray: dieses Array wird mit den Typen gefüllt. Vorhandene Daten werden gelöscht.
 */
void CDataBlockTypeChooserDialog::GetDatablockTypes( CArray<int, int>& dbArray ) const
{
  dbArray.RemoveAll();
  dbArray.Append( m_dbs );
} // GetDatablockTypes