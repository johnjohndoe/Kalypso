// solppage.cpp : implementation file
//

#include "stdafx.h"

#include "drawobj.h"
#include "drawvw.h"
#include "drawdoc.h"
#include "draw.h"

#include "solppage.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CSolidPropPage property page

CSolidPropPage::CSolidPropPage( CDrawObjList* pSelection, CDrawView* pView ) : CPropertyPage( CSolidPropPage::IDD )
{
	//{{AFX_DATA_INIT(CSolidPropPage)
	m_filled = FALSE;
	//}}AFX_DATA_INIT
	m_pSelection = pSelection;
	m_pView = pView;
}

void CSolidPropPage::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CSolidPropPage)
	DDX_Control(pDX, IDC_COMBO2, m_pattern);
	DDX_Control(pDX, IDC_COMBO1, m_color);
	DDX_Check(pDX, IDC_CHECK1, m_filled);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CSolidPropPage, CPropertyPage)
	//{{AFX_MSG_MAP(CSolidPropPage)
	ON_BN_CLICKED(IDC_CHECK1, OnChange)
	ON_CBN_SELCHANGE(IDC_COMBO1, OnChange)
	ON_CBN_SELCHANGE(IDC_COMBO2, OnChange)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CSolidPropPage message handlers

BOOL CSolidPropPage::OnInitDialog() 
{
  CDrawApp* drawApp = GETDRAWAPP;
	HPALETTE hPal = drawApp->m_hPal;

	CPropertyPage::OnInitDialog();
	
	ASSERT( m_pSelection != NULL );
	
	
	
  m_filled = FALSE;
  m_color.SetPalette( hPal );
	
  for( int i = 0; i < drawApp->m_logbrushes.GetSize(); i++ )
	{
			LPLOGBRUSH lpLogBrush = drawApp->m_logbrushes[i];
		  m_pattern.AddLogBrush( lpLogBrush );
	}
	m_pattern.Init();

	LPLOGBRUSH lpLogBrush = NULL;
  COLORREF color = 0;
	POSITION pos = m_pSelection->GetHeadPosition();
	while( pos )
	{
		CDrawObj* pDrawObj = m_pSelection->GetNextObject( pos );
		if( pDrawObj->HasBrush() )
		{
			if( pDrawObj->IsFilled() )
				m_filled = TRUE;
			if( color == 0 )
				color = pDrawObj->m_logbrush.lbColor;
			if( lpLogBrush == NULL )
				lpLogBrush = &( pDrawObj->m_logbrush );
		}
	}
	m_color.SetColor( color );
	m_pattern.SetLogBrush( lpLogBrush );
	UpdateData( FALSE );
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void CSolidPropPage::OnOK() 
{
	if( !UpdateData( TRUE ) )
		return;
	
  COLORREF color = m_color.GetColor();
  LPLOGBRUSH lpLogBrush = m_pattern.GetLogBrush();

	// add changed objects to undo buffer
	CDrawObjList objList;

	POSITION undoPos = m_pSelection->GetHeadPosition();
	while( undoPos )
	{
		CDrawObj* pDrawObj = m_pSelection->GetNextObject( undoPos );
		if( pDrawObj->HasBrush() )
			objList.AddTailObject( pDrawObj );
	}

	if( objList.GetObjectCount() > 0 )
		m_pView->AddToUndoBuffer( &objList, HINT_OBJ_EDIT );

	POSITION selPos = m_pSelection->GetHeadPosition();
	while( selPos )
	{
		CDrawObj* pDrawObj = m_pSelection->GetNextObject( selPos );
		if( pDrawObj->HasBrush() )
		{
			m_filled ? pDrawObj->SetFlags( CDrawObj::filled, TRUE ) : pDrawObj->UnsetFlags( CDrawObj::filled, TRUE );
			pDrawObj->m_logbrush = *lpLogBrush;
			pDrawObj->m_logbrush.lbColor = color;
			pDrawObj->DeletePattern();
			pDrawObj->Invalidate();
			pDrawObj->m_pDocument->SetModifiedFlag();
		}
	}
	
	CPropertyPage::OnOK();
}

void CSolidPropPage::OnChange() 
{
	SetModified(TRUE);
}
