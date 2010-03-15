// graphdlg.cpp : implementation file
//

#include "stdafx.h"

#include "drawobj.h"
#include "linppage.h"
#include "solppage.h"
#include "txtppage.h"

#include "graphdlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CGraphicDialog

IMPLEMENT_DYNAMIC(CGraphicDialog, CPropertySheet)

CGraphicDialog::CGraphicDialog( UINT nIDCaption, CDrawObjList* pSelection, CDrawView* pView, UINT iSelectPage /* = 0 */ )
	: CPropertySheet( nIDCaption, (CWnd*)pView, iSelectPage )
{
  CreatePages( pSelection, pView );
}

CGraphicDialog::CGraphicDialog( LPCTSTR pszCaption, CDrawObjList* pSelection, CDrawView* pView, UINT iSelectPage /* = 0 */ )
	: CPropertySheet( pszCaption, (CWnd*)pView, iSelectPage )
{
  CreatePages( pSelection, pView );
}

void CGraphicDialog::CreatePages( CDrawObjList* pObs, CDrawView* pView )
// erzeugt die benötigten Seiten
{
  if( pObs == NULL )
    return;

  BOOL bHasFont = FALSE;
  BOOL bHasPen = FALSE;
  BOOL bHasBrush = FALSE;

	POSITION pos = pObs->GetHeadPosition();
	while( pos != NULL )
	{
		CDrawObj* pDrawObj = pObs->GetNextObject( pos );
		if( pDrawObj->HasFont() )
      bHasFont = TRUE;
		if( pDrawObj->HasPen() )
      bHasPen = TRUE;
		if( pDrawObj->HasBrush() )
		  bHasBrush = TRUE;
  } // while pos

  if( bHasFont )
    m_pages.Add( new CTextPropPage( pObs, pView ) );
  if( bHasPen )
    m_pages.Add( new CLinePropPage( pObs, pView ) );
  if( bHasBrush )
    m_pages.Add( new CSolidPropPage( pObs, pView ) );

  // jetzt die Seiten wirklich hinzufügen
  for( int i = 0; i < m_pages.GetSize(); i++ )
    AddPage( m_pages[i] );
} // CreatePages

CGraphicDialog::~CGraphicDialog()
{
  for( int i = 0; i < m_pages.GetSize(); i++ )
    delete m_pages[i];
} // Destruktor


BEGIN_MESSAGE_MAP(CGraphicDialog, CPropertySheet)
	//{{AFX_MSG_MAP(CGraphicDialog)
		// NOTE - the ClassWizard will add and remove mapping macros here.
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CGraphicDialog message handlers
