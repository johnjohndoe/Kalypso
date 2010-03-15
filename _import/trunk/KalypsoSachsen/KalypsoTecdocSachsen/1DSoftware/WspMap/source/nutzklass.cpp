// nutzklass.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "resource.h"

#include "..\..\commonMfc\commonMfc.h"
#include "..\..\wspprj\wspprj.h"

#include "zuordnung.h"

#include "nutzklass.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CNutzKlass 


CNutzKlass::CNutzKlass( const CString& directory, CWnd* pParent /*=NULL*/ )
	: CDialog(CNutzKlass::IDD, pParent)
// Parameter:
//        const CString& directory: Verzeichnis der Zuordnungstabellen
{
	//{{AFX_DATA_INIT(CNutzKlass)
	m_staticZoTable = _T("");
	//}}AFX_DATA_INIT

  m_zList = NULL;
  m_zTable = NULL;
  m_directory = directory;
}

CNutzKlass::~CNutzKlass()
{
  delete m_zList;
};

void CNutzKlass::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CNutzKlass)
	DDX_Control(pDX, IDC_NUTZKLASS_DELETE_ROW, m_delRowButton);
	DDX_Control(pDX, IDC_NUTZKLASS_ADD_COL, m_addColButton);
	DDX_Control(pDX, IDC_NUTZKLASS_COL_COMBO, m_colCombo);
	DDX_Control(pDX, IDC_NUTZKLASS_DELETE_COL, m_deleteColButton);
	DDX_Control(pDX, IDC_NUTZKLASS_EDIT, m_editCtrl);
	DDX_Control(pDX, IDC_NUTZKLASS_NAME_COMBO, m_nameCombo);
	DDX_Control(pDX, IDC_NUTZKLASS_NEW_BUTTON, m_newButton);
	DDX_Control(pDX, IDC_NUTZKLASS_BUTTON_CANCEL, m_cancelButton);
	DDX_Control(pDX, IDC_NUTZKLASS_BUTTON_OK, m_okButton);
	DDX_Text(pDX, IDC_NUTZKLASS_STATIC_ZOTABLE, m_staticZoTable);
	//}}AFX_DATA_MAP
  DDX_GridControl( pDX, IDC_NUTZKLASS_GRID, m_gridCtrl );
}


BEGIN_MESSAGE_MAP(CNutzKlass, CDialog)
	//{{AFX_MSG_MAP(CNutzKlass)
	ON_CBN_SELCHANGE(IDC_NUTZKLASS_NAME_COMBO, OnSelchangeNutzklassNameCombo)
	ON_BN_CLICKED(IDC_NUTZKLASS_NEW_BUTTON, OnNutzklassNewButton)
	ON_EN_KILLFOCUS(IDC_NUTZKLASS_EDIT, OnKillfocusNutzklassEdit)
	ON_BN_CLICKED(IDC_NUTZKLASS_DELETE_COL, OnNutzklassDeleteCol)
	ON_BN_CLICKED(IDC_NUTZKLASS_DELETE_ROW, OnNutzklassDeleteRow)
	ON_BN_CLICKED(IDC_NUTZKLASS_ADD_COL, OnNutzklassAddCol)
	//}}AFX_MSG_MAP
  ON_BN_CLICKED( IDC_NUTZKLASS_BUTTON_OK, OnNutzklassOKButton )
  ON_BN_CLICKED( IDC_NUTZKLASS_BUTTON_CANCEL, OnNutzklassCancelButton )
  ON_NOTIFY( GVN_ENDLABELEDIT, IDC_NUTZKLASS_GRID, OnEndlabeleditGridCtrl )
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CNutzKlass 

BOOL CNutzKlass::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
  // Texte setzen: für verschiedene Sprachen
  SetWindowText( CString(MAKEINTRESOURCE(IDS_NUTZKLASS_TITLE)) );
  m_staticZoTable.LoadString( IDS_NUTKLASS_ZOTABLE );
  m_newButton.SetWindowText( CString(MAKEINTRESOURCE(IDS_NUTZKLASS_BUTTON_NEW)) );
  m_okButton.SetWindowText( CString(MAKEINTRESOURCE(IDS_NUTZKLASS_BUTTON_OK)) );
  m_addColButton.SetWindowText( CString(MAKEINTRESOURCE(IDS_NUTZKLASS_ADD_COL)) );
  m_deleteColButton.SetWindowText( CString(MAKEINTRESOURCE(IDS_NUTZKLASS_DEL_COL)) );
  m_delRowButton.SetWindowText( CString(MAKEINTRESOURCE(IDS_NUTZKLASS_DEL_ROW)) );

  // EditControl für neue Tabellen verstecken
  m_editCtrl.ShowWindow( SW_HIDE );

  // Daten Initialisieren und ComboBox füllen
  m_zList = new CZList( m_directory );
  m_zList->Load();
  for( int i = 0; i < m_zList->GetZTableCount(); i++ )
  {
    CZTable* zTable = m_zList->GetZTable( i );
    if( zTable->GetColumnCount() <= 5 ) // nur Tabellen mit maximal 5 Einträgen werden zu Zeit benutzt
    {
      int index = m_nameCombo.AddString( zTable->GetName() );
      m_nameCombo.SetItemDataPtr( index, (void*)(zTable) ); // der Eintrag zeigt direkt auf die Tabelle
    }; // if zTable Count = 5
  }; // for i

  // die GridControl initialisieren
  m_gridCtrl.SetGridLines( GVL_BOTH );
  m_gridCtrl.SetEditable( TRUE );
    

  // oberste Tabelle auswählen
  m_nameCombo.SetCurSel( 0 );
  OnSelchangeNutzklassNameCombo();
  m_nameCombo.SetFocus();

  UpdateData( FALSE );

	return FALSE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

void CNutzKlass::OnSelchangeNutzklassNameCombo() 
// nachdem die ComboBox ein neues Element anzeigt, soll die Entsprechende Tabelle angezeigt werden
{
  ReadCurZTable();
  
  int index = m_nameCombo.GetCurSel();
  if( index > -1 )
    m_zTable = (CZTable*)m_nameCombo.GetItemDataPtr( index );
  else
    m_zTable = NULL;

  ShowCurZTable();
}; // OnSelchangeNutzklassNameCombo

void CNutzKlass::OnNutzklassNewButton() 
{
  m_editCtrl.ShowWindow( SW_SHOW );
  m_editCtrl.SetFocus();
}; // OnNutzklassNewButton

void CNutzKlass::OnOK() 
// fängt 'Enter' ab. Falls gerade ein Name eingegeben wird wird er jetzt übernommen
{  
  if( m_editCtrl.IsWindowVisible() )
    m_gridCtrl.SetFocus();
}; // OnOk

void CNutzKlass::OnCancel()
// fängt 'Escape' ab. Falls gerade ein Name editiert wird, wird abgebrochen
{
  if( m_editCtrl.IsWindowVisible() )
  {
    m_editCtrl.SetWindowText( TEXT("") );
    m_gridCtrl.SetFocus();
  }; // if m_editCtrl
}; // OnCancel

void CNutzKlass::OnKillfocusNutzklassEdit() 
// wenn die EditCtrl den Focus verliert, neue Tabelle anlegen und die EditCtrl verstecken
{
  CString newName;
  m_editCtrl.GetWindowText( newName );
  newName.TrimLeft();
  newName.TrimRight();
  if( newName.FindOneOf( "\\/:*\"\'" ) != -1 )
  {
    AfxMessageBox( IDS_NUTZKLASS_ERROR_NAME, MB_ICONEXCLAMATION );
    m_editCtrl.SetFocus();
  }
  else if( m_nameCombo.FindString( -1, newName ) != CB_ERR )
  {
    AfxMessageBox( IDS_NUTZKLASS_ERROR_DOUBLE_NAME, MB_ICONEXCLAMATION );
    m_editCtrl.SetFocus();
  }
  else
  {
    int index = m_nameCombo.GetCurSel();
    
    if( !newName.IsEmpty() )
    {
      // neue Tabelle anlegen und selektieren
      CZTable* zTable = m_zList->GetZTable( m_zList->AddZTable( newName ) );
      index = m_nameCombo.AddString( newName );
      m_nameCombo.SetItemDataPtr( index, zTable );

      // neue Tabelle initialisieren
      zTable->AddColumn( MO2_FIELD_RAUHEIT );
      zTable->AddColumn( MO2_FIELD_RAUHEITKST );
      zTable->AddColumn( MO2_FIELD_AXM );
      zTable->AddColumn( MO2_FIELD_AYM );
      zTable->AddColumn( MO2_FIELD_DPM );
    };
    m_editCtrl.SetWindowText( "" );
    m_editCtrl.ShowWindow( FALSE );
    
    m_nameCombo.SetCurSel( index );
    OnSelchangeNutzklassNameCombo();
    m_gridCtrl.SetFocus();
  };
}; // OnKillfocusNutzklassEdit

void CNutzKlass::OnNutzklassOKButton()
{ // extra Funktion für den Ok-Button, um Enter abzufangen
  // aktuelle Tabelle übernehmen und alle Änderungen speichern
  ReadCurZTable();
  
  m_zList->Save();
  
  CDialog::OnOK();
}; // OnNutzklassOkButton

void CNutzKlass::OnNutzklassCancelButton()
{ // extra Funtkion für denCacnel-Button, um Escape abzufangen
  CDialog::OnCancel();
};

void CNutzKlass::ShowCurZTable()
// zeigt die aktuelle Tabelle in der ListCtrl
{
  // zuerst die bisherige Tabelle löschen
  m_gridCtrl.DeleteAllItems();
  
  // und neu füllen
  if( m_zTable )
  {
    // jetzt neu füllen
    m_gridCtrl.SetFixedRowCount( 1 );

    // Spalten einfügen: 2 fest
    m_gridCtrl.InsertColumn( CString(MAKEINTRESOURCE(IDS_NUTZKLASS_VON)) );
    m_gridCtrl.InsertColumn( TEXT("Text" ) );

    // und der Rest dynamisch
    for( int i = 0; i < m_zTable->GetColumnCount(); i++ )
      m_gridCtrl.InsertColumn( m_zTable->GetColumnHeader( i ) );

    // jetzt die Datenzeilen
    for( i = 0; i < m_zTable->GetRowCount(); i++ )
    {
      m_gridCtrl.InsertRow( "x" );

      CString dataStr;
      int von = m_zTable->GetVon( i );
      dataStr.Format( "%d", von );
      
      m_gridCtrl.SetItemText( i + 1, 0, dataStr );
      m_gridCtrl.SetItemFormat( i + 1, 0, DT_RIGHT );

      // den Kommentar
      m_gridCtrl.SetItemText( i + 1, 1, m_zTable->GetComment( i ) );
      m_gridCtrl.SetItemFormat( i + 1, 1, DT_LEFT );
      
      // jetzt die Daten
      for( int j = 0; j < m_zTable->GetColumnCount(); j++ )
      {
        double data = m_zTable->GetValue( j, i );
        dataStr.Format( "%.4lf", data );

        m_gridCtrl.SetItemText( i + 1, j + 2, dataStr );
        m_gridCtrl.SetItemFormat( i + 1, j + 2, DT_RIGHT );
      }; // for j
    }; // for i
    
    // und jetzt noch eine leere Zeile
    m_gridCtrl.InsertRow( " " ); // ein Leerzeichen, sonst wird die Zeile mein Size zu klein
    m_gridCtrl.AutoSizeRows();
    m_gridCtrl.ExpandColumnsToFit();
    m_gridCtrl.SetFocus();

    FillColCombo();
  }; // if zTable
}; // CNutzKlass

void CNutzKlass::OnEndlabeleditGridCtrl(NMHDR* pNMHDR, LRESULT* pResult)
// fügt eine neue Zeile ein, wenn es noch keine Leere gibt
{
  // testen, ob die unterste Zeile komplett leer ist ( außer Leerzeichen )
  BOOL bLeer = TRUE;

  // die Eingabe überprüfen
  NM_GRIDVIEW* pGridView = (NM_GRIDVIEW*)pNMHDR;
  CString cellText = m_gridCtrl.GetItemText( pGridView->iRow, pGridView->iColumn );
  CString scanString; // mit diesem String wird die Eingabe via sscanf geparst
  int err = -1;
  TCHAR buffer[1024]; // Hoffentlich nie mehr als 1000 Zeichen in der Zelle
  buffer[0] = '\0';
  LPCTSTR cellBuffer = cellText.GetBuffer( cellText.GetLength() );
  switch( pGridView->iColumn )
  {
  case 0: // Typ wird als Integer geparst
    {
      int value;
      err = _stscanf( cellBuffer, "%d", &value );
      if( err != 1 )
        value = 0;
      _stprintf( buffer, "%30d", value );
    }
    break;

  case 1: // Kommentar ist Text
    err = 0; // hier kann kein Fehler auftreten
    _tcsncpy( buffer, cellBuffer, cellText.GetLength() + 1 );
    break;

  default: // alles andere ist Double mit 4 Nachkommastellen
    {
      double value;
      err = _stscanf( cellBuffer, "%lf", &value );
      if( err != 1 )
        value = 0.0;
      _stprintf( buffer, "%20.4lf", value );
      break;
    }
  } // switch iRow

  CString text = buffer;
  text.TrimLeft();
  text.TrimRight();
  m_gridCtrl.SetItemText( pGridView->iRow, pGridView->iColumn, text );

  // jetzt prüfen, ob eine weitere Zeile angefügt werden soll
  int rowIndex = m_gridCtrl.GetRowCount() - 1;
  if( rowIndex < 0 )
    return;

  for( int colIndex = 0; colIndex < m_gridCtrl.GetColumnCount(); colIndex++ )
  {
    CString text = m_gridCtrl.GetItemText( rowIndex, colIndex );
    text.TrimLeft();
    text.TrimRight();

    if( !text.IsEmpty() )
    {
      bLeer = FALSE;
      break;
    }; 
  }; // for colIndex
 
  if( !bLeer )
  {
    m_gridCtrl.InsertRow( " " ); // ein Leerzeichen, sonst wird die Zeile mein Size zu klein
    m_gridCtrl.SetFocusCell( pGridView->iRow, pGridView->iColumn );
  }

  m_gridCtrl.AutoSizeRows();
  m_gridCtrl.ExpandColumnsToFit();
}; // OnEndlabelEditGridCtrl

void CNutzKlass::ReadCurZTable()
// füllt die aktuelle ZTable neu mit den Daten aus der ListCtrl
{
  if( m_zTable )
  {
    // die aktuelle ZTabelle komplett leeren
    m_zTable->RemoveAllRows();
    m_zTable->RemoveAllColumns();
    
    // die Spalten erzeugen
    LVCOLUMN hColumn;
    TCHAR text[MAX_PATH];
    text[0] = '\0';
    hColumn.cchTextMax = MAX_PATH;
    hColumn.pszText = (char*)text;
    hColumn.mask = LVCF_TEXT;
    hColumn.iSubItem = 0;
    for( int i = 2; i < m_gridCtrl.GetColumnCount(); i++ ) // alles ab der dritten Spalte
      m_zTable->AddColumn( m_gridCtrl.GetItemText( 0, i ) );
    
    // und jetzt neu füllen
    for( i = 1; i < m_gridCtrl.GetRowCount() - 1; i++ ) // die letzte Zeile ist stets leer, die erste die Überschrift
    {
      // neue Zeile anlegen
      int von = atoi( m_gridCtrl.GetItemText( i, 0 ) );
      int rowIndex = m_zTable->AddRow( von, von );
      
      // Kommentar lesen
      m_zTable->SetComment( rowIndex, m_gridCtrl.GetItemText( i, 1 ) );
      
      // Zeile füllen
      for( int j = 0; j < m_zTable->GetColumnCount(); j++ )
      {
        double value = atof( m_gridCtrl.GetItemText( i, j + 2 ) );
        m_zTable->SetValue( rowIndex, j, value );
      }; // for j
    }; // for i
  }; // if m_zTable
  
}; // ReadCurZTable


void CNutzKlass::OnNutzklassDeleteCol() 
{
  CCellID focusCell = m_gridCtrl.GetFocusCell();
  if( focusCell.col > 1 && focusCell.col < m_gridCtrl.GetColumnCount() )
  {
    m_gridCtrl.DeleteColumn( focusCell.col );
    m_gridCtrl.AutoSizeRows();
    m_gridCtrl.ExpandColumnsToFit();
    FillColCombo();
  }
} // OnNutzklassDeleteCol

void CNutzKlass::OnNutzklassDeleteRow() 
{
  CCellID focusCell = m_gridCtrl.GetFocusCell();
  if( focusCell.row > 0 && focusCell.row < m_gridCtrl.GetRowCount() - 1 )
  {
    m_gridCtrl.DeleteRow( focusCell.row );
    m_gridCtrl.AutoSizeRows();
    m_gridCtrl.ExpandColumnsToFit();
  }
}

void CNutzKlass::OnNutzklassAddCol() 
// wenn der Benutzer auf "Spalte einfügen" drückt, wird die aktuell
// selektierte Spalte hinzugefügt
{
	int comboIndex = m_colCombo.GetCurSel();
  if( comboIndex != CB_ERR )
  {
    CString newColName;
    m_colCombo.GetLBText( comboIndex, newColName );

    newColName.TrimLeft();
    newColName.TrimRight();

    if( !newColName.IsEmpty() )
    {
      m_gridCtrl.InsertColumn( newColName );
      m_gridCtrl.AutoSizeRows();
      m_gridCtrl.ExpandColumnsToFit();
      FillColCombo();
    };
  }; // if comboIndex
} // OnNutzklassAddCol

void CNutzKlass::FillColCombo()
// liest die zur Zeit vorhandenen Spalten aus und füllt
// die ColCombo mit denjenigen, welche nicht in der Tabelle sind
{
  CStringArray colNames;

  colNames.Add( MO2_FIELD_RAUHEIT );
  colNames.Add( MO2_FIELD_RAUHEITKST );
  colNames.Add( MO2_FIELD_AXM );
  colNames.Add( MO2_FIELD_AYM );
  colNames.Add( MO2_FIELD_DPM );

  // jetzt diejenigen wieder rausschmeissen, die schon da sind
  for( int i = 2; i < m_gridCtrl.GetColumnCount(); i++ )
  {
    CString colName = m_gridCtrl.GetItemText( 0, i );

    // suchen ob in der Liste
    for( int j = 0; j < colNames.GetSize(); j++ )
    {
      if( colName.CompareNoCase( colNames[j] ) == 0 )
      {
        colNames.RemoveAt( j );
        break;
      }
    } // for j
  } // for i

  // und jetzt die Combo neu füllen
  m_colCombo.ResetContent();
  for( i = 0; i < colNames.GetSize(); i++ )
    m_colCombo.AddString( colNames[i] );

  if( m_colCombo.GetCount() > 0 )
  {
    m_colCombo.SetCurSel( 0 );
    m_colCombo.ShowWindow( TRUE );
    m_addColButton.EnableWindow( TRUE );
  }
  else
  {
    m_colCombo.ShowWindow( FALSE );
    m_addColButton.EnableWindow( FALSE );
  } // if m_colCombo.GetCount() > 0
} // FillColCombo
