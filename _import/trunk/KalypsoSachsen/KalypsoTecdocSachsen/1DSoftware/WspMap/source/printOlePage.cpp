// printolepage.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "resource.h"

#include "printRectOle.h"

#include "printolepage.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Eigenschaftenseite CPrintOlePage 

CPrintOlePage::CPrintOlePage( UINT captionID, CPrintRectOle* printRect ) : CPropertyPage(CPrintOlePage::IDD, captionID)
{
	//{{AFX_DATA_INIT(CPrintOlePage)
		// HINWEIS: Der Klassen-Assistent fügt hier Elementinitialisierung ein
	//}}AFX_DATA_INIT

  m_printRect = printRect;
}

CPrintOlePage::~CPrintOlePage()
{
}

void CPrintOlePage::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CPrintOlePage)
		// HINWEIS: Der Klassen-Assistent fügt hier DDX- und DDV-Aufrufe ein
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CPrintOlePage, CPropertyPage)
	//{{AFX_MSG_MAP(CPrintOlePage)
	ON_BN_CLICKED(IDC_PRINT_OLE_BUTTON, OnPrintOleButton)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CPrintOlePage 

void CPrintOlePage::OnPrintOleButton() 
{
}
