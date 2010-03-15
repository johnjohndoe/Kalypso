// KopfTxtDlg.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "resource.h"

#include "KopfTxtDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld KopfTxtDlg 


KopfTxtDlg::KopfTxtDlg( BOOL bChangeKunde, CWnd* pParent /*=NULL*/,CString start_path )
	: CDialog(KopfTxtDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(KopfTxtDlg)
	m_Strasse = _T("");
	m_PLZStadt = _T("");
	m_Telefon = _T("");
	//}}AFX_DATA_INIT
  m_pParent=pParent;
  m_bChangeKunde = bChangeKunde;
  pFileNameKopf=start_path+"kopf.txt";
  pFileNameHydra=start_path+"hydra\\hydra.kdd";
}


void KopfTxtDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(KopfTxtDlg)
	DDX_Control(pDX, IDC_EDIT1, m_kundenControl);
	DDX_Text(pDX, IDC_EDIT2, m_Strasse);
	DDX_Text(pDX, IDC_EDIT3, m_PLZStadt);
	DDX_Text(pDX, IDC_EDIT4, m_Telefon);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(KopfTxtDlg, CDialog)
	//{{AFX_MSG_MAP(KopfTxtDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten KopfTxtDlg 

BOOL KopfTxtDlg::OnInitDialog() 
{
  BOOL ok;
  CString str,dummy_str;
  int pos=0,i=0;
  
  CDialog::OnInitDialog();
  
  KopfTxt dat(pFileNameKopf);
  
  ok=dat.Load();
  if(ok)
  {
    if(dat.GetSize()>2)
    {
      dat.GetStr( 0, str );
      m_kundenControl.SetWindowText( str );
      dat.GetStr(1,str);
      i=str.Find('*');
      if(i>=0)
      {
        m_Strasse=str.Left(i);
        m_Strasse.TrimLeft();
        m_Strasse.TrimRight();
        str=str.Right(str.GetLength() - i-1);
        if(!str.IsEmpty())
        {
          str.TrimLeft();
          i=str.Find('*');
          if(i>=0)
          {
            m_PLZStadt=str.Left(i);
            m_PLZStadt.TrimLeft();
            m_PLZStadt.TrimRight();                   
            str=str.Right(str.GetLength() - i-1);
            if(!str.IsEmpty())
            {
              m_Telefon=str;
              m_Telefon.TrimLeft();
              m_Telefon.TrimRight();
            }
          } // if i>= 0
          else
          {
            m_PLZStadt = str;
          };
        } // !str.IsEmpty
      }
      else
      {
        m_Strasse=str;
      }
      UpdateData(FALSE);
    }
  }
  m_kundenControl.SetReadOnly( !m_bChangeKunde );
  
  return TRUE;  // return TRUE unless you set the focus to a control
  // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

void KopfTxtDlg::OnOK() 
{
  CString str;
  if (!UpdateData(TRUE))
    return;
  KopfTxt dat(pFileNameKopf);
  BOOL ok;
  ok = dat.Load();
  if(ok)
  {
    if(dat.GetSize()>2)
    {
      m_kundenControl.GetWindowText( str );
      dat.SetStr( 0, str );
      m_Strasse.TrimLeft();
      m_Strasse.TrimRight();
      m_PLZStadt.TrimLeft();
      m_PLZStadt.TrimRight();
      m_Telefon.TrimLeft();
      m_Telefon.TrimRight();
      str = m_Strasse + " * " + m_PLZStadt + " * " + m_Telefon;
      dat.SetStr(1,str);
      dat.Save();
    }
  }
  CDialog::OnOK();
}
