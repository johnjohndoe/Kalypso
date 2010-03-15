// printimagepage.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "resource.h"

#include "printRectImage.h"

#include "printimagepage.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Eigenschaftenseite CPrintImagePage 

CPrintImagePage::CPrintImagePage( UINT captionID, CPrintRectImage* imageRect ) : CPropertyPage( CPrintImagePage::IDD, captionID )
{
	//{{AFX_DATA_INIT(CPrintImagePage)
	m_fileName = _T("");
	m_fixSize = FALSE;
	//}}AFX_DATA_INIT

  m_imageRect = imageRect;

  m_fileName = imageRect->GetFileName();
  m_fixSize = imageRect->GetFixSize();
}

CPrintImagePage::~CPrintImagePage()
{
}

void CPrintImagePage::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CPrintImagePage)
	DDX_Control(pDX, IDC_PRINT_IMAGE_SIZE_CHECK, m_sizeCheckbox);
	DDX_Text(pDX, IDC_PRINT_IMAGE_NAME, m_fileName);
	DDX_Check(pDX, IDC_PRINT_IMAGE_SIZE_CHECK, m_fixSize );
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CPrintImagePage, CPropertyPage)
	//{{AFX_MSG_MAP(CPrintImagePage)
	ON_BN_CLICKED(IDC_PRINT_IMAGE_SEARCH, OnPrintImageSearch)
	ON_EN_CHANGE(IDC_PRINT_IMAGE_NAME, OnChange)
	ON_BN_CLICKED(IDC_PRINT_IMAGE_SIZE_CHECK, OnChange)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CPrintImagePage 

void CPrintImagePage::OnOK() 
{
  UpdateData( TRUE );

  if( m_fileName.IsEmpty() )
  {
    EndDialog( IDCANCEL );
    return;
  };

  m_imageRect->SetFixSize( m_fixSize );

	if( !m_imageRect->LoadBitmap( m_fileName ) )
    return;

	CPropertyPage::OnOK();
}

BOOL CPrintImagePage::OnInitDialog() 
{
	CPropertyPage::OnInitDialog();
	
	
  GetDlgItem( IDC_PRINT_IMAGE_TEXT )->SetWindowText( CString(MAKEINTRESOURCE(IDC_PRINT_IMAGE_TEXT) ) );
  GetDlgItem( IDC_PRINT_IMAGE_SEARCH )->SetWindowText( CString(MAKEINTRESOURCE(IDC_PRINT_IMAGE_SEARCH ) ) );
  GetDlgItem( IDC_PRINT_IMAGE_SIZE_CHECK )->SetWindowText( CString(MAKEINTRESOURCE(IDC_PRINT_IMAGE_SIZE_CHECK ) ) );
  
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

void CPrintImagePage::OnPrintImageSearch() 
{
  // einen FileDialog anzeigen

  CString bitmapFilter = TEXT( "Bitmaps (*.bmp; *.dib)|*.bmp;*.dib" );

  CFileDialog dlg( TRUE, TEXT("BMP"), m_fileName, OFN_FILEMUSTEXIST | OFN_HIDEREADONLY, bitmapFilter, this );
  if( dlg.DoModal() == IDOK )
  {
    m_fileName = dlg.GetPathName();
    SetModified( TRUE );
    UpdateData( FALSE );
  }
}

void CPrintImagePage::OnChange() 
{
  SetModified( TRUE );
}
