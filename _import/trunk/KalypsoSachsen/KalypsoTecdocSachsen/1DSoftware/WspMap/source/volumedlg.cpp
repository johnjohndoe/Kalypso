// volumedlg.cpp: Implementierungsdatei
//

#include "stdafx.h"
#include "..\include\wspmap.h"
#include "..\include\VolumeDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CVolumeDlg 


CVolumeDlg::CVolumeDlg( const CString& volume, CWnd* pParent /*=NULL*/)
	: CDialog(CVolumeDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CVolumeDlg)
	m_volumeText = _T("");
	//}}AFX_DATA_INIT
	m_volumeText = volume;
}


void CVolumeDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CVolumeDlg)
	DDX_Text(pDX, IDC_VOLUME_EDIT, m_volumeText);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CVolumeDlg, CDialog)
	//{{AFX_MSG_MAP(CVolumeDlg)
		// HINWEIS: Der Klassen-Assistent fügt hier Zuordnungsmakros für Nachrichten ein
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CVolumeDlg 
