// nmapdlg.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "..\..\wspprj\wspprj.h"

#include "mapproj.h"
#include "nmapdlg.h"


#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CNewMapDlg 


CNewMapDlg::CNewMapDlg(CWnd* pParent /*=NULL*/, CMapProject* pMapProject /*=NULL*/)
	: CDialog(CNewMapDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CNewMapDlg)
		// HINWEIS: Der Klassen-Assistent fügt hier Elementinitialisierung ein
	//}}AFX_DATA_INIT
	m_pMapProject = pMapProject;
}


void CNewMapDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CNewMapDlg)
	DDX_Control(pDX, IDC_COMBO1, m_names);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CNewMapDlg, CDialog)
	//{{AFX_MSG_MAP(CNewMapDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CNewMapDlg 

BOOL CNewMapDlg::OnInitDialog() 
{
	CString str;
	int i;
	Project *proj;

	CDialog::OnInitDialog();
	
	ASSERT(m_pMapProject);
	proj = m_pMapProject->GetProject();
	ASSERT(proj);

	for (i = 0; i < proj->GetWaterCount(); i++)
		m_names.AddString( proj->GetWaterName(i) );
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

void CNewMapDlg::OnOK() 
{
	CString str;
	int i;
	
	GetDlgItem( IDC_COMBO1 )->GetWindowText( m_name );
	if (m_name.IsEmpty() )
	{
		AfxMessageBox( IDS_NONAME, MB_OK | MB_ICONEXCLAMATION );
		GetDlgItem( IDC_COMBO1 )->SetFocus();
		return;
	}

	for ( i = 0; i < m_pMapProject->GetNumMapDocs(); i++ )
	{
		str = m_pMapProject->GetMapDocName( i );
		if ( m_name.CompareNoCase( str ) == 0 )
		{
			AfxMessageBox( IDS_NAMEUSED, MB_OK | MB_ICONEXCLAMATION );
			GetDlgItem( IDC_COMBO1 )->SetFocus();
			return;
		}
	}
	
/*
	for ( i = 0; i < m_pMapProject->m_oldMapDocNames.GetSize(); i++)
	{
		if (m_name.CompareNoCase(m_pMapProject->m_oldMapDocNames[i])==0)
		{
			AfxMessageBox(IDS_NAMEUSED, MB_OK | MB_ICONEXCLAMATION);
			GetDlgItem(IDC_COMBO1)->SetFocus();
			return;
		}
	}
*/
	
	CDialog::OnOK();
}
