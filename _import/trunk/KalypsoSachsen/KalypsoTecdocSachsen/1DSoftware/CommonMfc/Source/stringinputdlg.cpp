// stringinputdlg.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "commresource.h"

#include "stringinputdlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CStringInputDlg 



/*!
 * Der Konstruktor.
 *
 * @param title : der Titel des Dialog
 * @param text : der Text des Dialogs
 * @param input : der vorgegebene Text für das Eingabefeld
 * @param pParent : das Parent-Window (= NULL )
 *
 */
CStringInputDlg::CStringInputDlg( const CString& title, const CString& text, const CString& input, CWnd* pParent /*=NULL*/ )
	: CDialog( CStringInputDlg::IDD, pParent )
{
	//{{AFX_DATA_INIT(CStringInputDlg)
	m_text = _T("");
	m_input = _T("");
	//}}AFX_DATA_INIT

  m_title = title;
  m_text = text;
  m_input = input;
}


void CStringInputDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CStringInputDlg)
	DDX_Text(pDX, IDC_STRING_INPUT_TEXT, m_text);
	DDX_Text(pDX, IDC_STRING_INPUT_EDIT, m_input);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CStringInputDlg, CDialog)
	//{{AFX_MSG_MAP(CStringInputDlg)
		// HINWEIS: Der Klassen-Assistent fügt hier Zuordnungsmakros für Nachrichten ein
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CStringInputDlg 
