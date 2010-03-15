// PrintScaleDefDlg.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "..\..\wspprj\wspprj.h"

#include "mapdoc.h"
#include "printscaledefdlg.h"


#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CPrintScaleDefDlg 


CPrintScaleDefDlg::CPrintScaleDefDlg(CWnd* pParent /*=NULL*/,CMapDoc *pDoc)
	: CDialog(CPrintScaleDefDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CPrintScaleDefDlg)
	m_MapUnit = -1;
	m_scale = _T("");
	//}}AFX_DATA_INIT
    m_pDoc=pDoc;
}


void CPrintScaleDefDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CPrintScaleDefDlg)
	DDX_CBIndex(pDX, IDC_COMBO1, m_MapUnit);
	DDX_CBString(pDX, IDC_COMBO2, m_scale);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CPrintScaleDefDlg, CDialog)
	//{{AFX_MSG_MAP(CPrintScaleDefDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CPrintScaleDefDlg 

BOOL CPrintScaleDefDlg::OnInitDialog() 
{
  ASSERT(m_pDoc);
  CDialog::OnInitDialog();
  
  m_MapUnit=m_pDoc->GetMapUnit();
  m_scale.Format("%d",m_pDoc->GetScale());
  UpdateData(FALSE);
  
  return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

void CPrintScaleDefDlg::OnOK() 
{
    
  if (!UpdateData())
    return;
  m_pDoc->SetMapUnit(m_MapUnit);
  m_pDoc->SetScale(atoi(m_scale));
  CDialog::OnOK();
}
