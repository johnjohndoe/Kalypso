// strangdistancedlg.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "resource.h"

//#include "wspmap.h"
#include "strangdistancedlg.h"
#include "wspprj/include/connect.h"
#include "wspprj/include/state.h"



#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CStrangDistanceDlg 

COLORREF COLOR_WHITE = RGB( 255, 255, 255 );
COLORREF COLOR_GREEN = RGB( 0, 255, 128 );

CStrangDistanceDlg::CStrangDistanceDlg( State* state, const CProfileDistancer::ConnMap& connMap, CString logFile, CWnd* pParent /*=NULL*/ )
	: CDialog(CStrangDistanceDlg::IDD, pParent), m_state( state ), m_connMap( connMap ), m_logFile( logFile )
{
	//{{AFX_DATA_INIT(CStrangDistanceDlg)
		// HINWEIS: Der Klassen-Assistent fügt hier Elementinitialisierung ein
	//}}AFX_DATA_INIT
}


void CStrangDistanceDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CStrangDistanceDlg)
		// HINWEIS: Der Klassen-Assistent fügt hier DDX- und DDV-Aufrufe ein
	//}}AFX_DATA_MAP

	DDX_GridControl( pDX, IDC_GRID, m_gridCtrl );
}


BEGIN_MESSAGE_MAP(CStrangDistanceDlg, CDialog)
	//{{AFX_MSG_MAP(CStrangDistanceDlg)
	ON_BN_CLICKED(ID_BUTTON_LOG, OnButtonLog)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CStrangDistanceDlg 

BOOL CStrangDistanceDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
	// die GridControl initialisieren
	m_gridCtrl.SetGridLines( GVL_BOTH );
	m_gridCtrl.SetEditable( FALSE );

    // jetzt neu füllen
    m_gridCtrl.SetFixedRowCount( 1 );
	m_gridCtrl.EnableTitleTips();

    // Spalten einfügen: 2 fest
    m_gridCtrl.InsertColumn( TEXT( "Von KM" ) );
	m_gridCtrl.InsertColumn( TEXT( "Bis KM" ) );
    m_gridCtrl.InsertColumn( TEXT( "Vorland links" ) );
	m_gridCtrl.InsertColumn( TEXT( "Hauptöffnung" ) );
	m_gridCtrl.InsertColumn( TEXT( "Vorland rechts" ) );

	int count = 0;
	Connection* conn = m_state->GetFirstConnection();
	while( conn )
	{
		count++;

		m_gridCtrl.InsertRow( "x" );
		
		SetItem( count, 0, DT_RIGHT, "%10.3lf", conn->GetAnfStation(), COLOR_WHITE, -999.9 );
		SetItem( count, 1, DT_RIGHT, "%10.3lf", conn->GetEndStation(), COLOR_WHITE, -999.9 );
		SetDistance( count, 2, conn->GetVorlandLinks(), conn, CProfilModel::Zone::VL );
		SetDistance( count, 3, conn->GetFluss(), conn, CProfilModel::Zone::HF );
		SetDistance( count, 4, conn->GetVorlandRechts(), conn, CProfilModel::Zone::VR );

		m_gridCtrl.SetItemData( count, 0, (LPARAM)conn );

		conn = m_state->GetNextConnection();
    };
    
    m_gridCtrl.AutoSizeRows();
    m_gridCtrl.ExpandColumnsToFit();

	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

void CStrangDistanceDlg::SetItem( int nRow, int nCol, UINT nFormat, const CString& formatStr, double value, COLORREF color, double oldValue )
{
	CString string;
	string.Format( formatStr, value );
    
	m_gridCtrl.SetItemText( nRow, nCol, string );
	m_gridCtrl.SetItemFormat( nRow, nCol, nFormat );
	m_gridCtrl.SetItemBkColour( nRow, nCol, color );
	if( oldValue > 0.0 )
	{
		CString format2 = "Bisheriger Wert: " + formatStr;
		string.Format( format2, oldValue );
		m_gridCtrl.SetItemTitleTip( nRow, nCol, string );
	}
}

void CStrangDistanceDlg::SetDistance( int nRow, int nCol, double istValue, Connection* conn, CProfilModel::Zone zone )
{
	CProfileDistancer::ConnMap::const_iterator connIt = m_connMap.find( conn );
	if( connIt != m_connMap.end() )
	{
		CProfileDistancer::DistMap* distMap = connIt->second;
		CProfileDistancer::DistMap::iterator distIt = distMap->find( zone );
		if( distIt !=  distMap->end() )
		{
			// neuer wert in km
			double newValue = distIt->second / 1000;

			SetItem( nRow, nCol, DT_RIGHT, "%10.3lf", newValue, COLOR_GREEN, istValue );
			return;
		}
	}


	SetItem( nRow, nCol, DT_RIGHT, "%10.3lf", istValue, COLOR_WHITE, -999.9 );
}

void CStrangDistanceDlg::ApplyChanges()
{
	int rowCount = m_gridCtrl.GetRowCount();
	for( int row = m_gridCtrl.GetFixedRowCount(); row < rowCount; row++ )
	{
		Connection* conn = (Connection*)m_gridCtrl.GetItemData( row, 0 );

		conn->SetVorlandLinks( GetValue( row, 2 ) );
		conn->SetFluss( GetValue( row, 3 ) );
		conn->SetVorlandRechts( GetValue( row, 4 ) );
	}

	m_state->SetModified();
}

double CStrangDistanceDlg::GetValue( int row, int col )
{
	CString text = m_gridCtrl.GetItemText( row, col );
	double value;
	sscanf( LPCTSTR(text), "%lf", &value );
	return value;
}

/** Opens the log-file with 'start' */
void CStrangDistanceDlg::OnButtonLog() 
{
	PROCESS_INFORMATION pi;
	STARTUPINFO sui;

	::GetStartupInfo( &sui );
	sui.lpReserved = NULL;
	sui.lpTitle = "";
	sui.dwFlags |= STARTF_USESHOWWINDOW;
	sui.wShowWindow = SW_HIDE;

	CString cmdLine = "cmd.exe /c \"start " + m_logFile + "\"";
	BOOL bCreate = ::CreateProcess( NULL, cmdLine.GetBuffer( cmdLine.GetLength() ), NULL, NULL, TRUE, NORMAL_PRIORITY_CLASS, NULL, NULL, &sui, &pi );
	DWORD lastError = GetLastError();
	cmdLine.ReleaseBuffer();

	// we dont need the handles, so close it now
	CloseHandle( pi.hThread );
	CloseHandle( pi.hProcess );
}
