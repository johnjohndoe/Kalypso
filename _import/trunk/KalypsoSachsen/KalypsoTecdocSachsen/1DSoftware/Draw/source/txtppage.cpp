// txtppage.cpp : implementation file
//

#include "stdafx.h"

#include "drawobj.h"
#include "drawvw.h"
#include "drawdoc.h"

#include "txtppage.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CTextPropPage property page

CTextPropPage::CTextPropPage( CDrawObjList* pSelection, CDrawView* pView ) 
: CPropertyPage( CTextPropPage::IDD )
{
	//{{AFX_DATA_INIT(CTextPropPage)
	m_orientation = 0;
	m_horz = -1;
	m_vert = -1;
	m_precision = 0;
	//}}AFX_DATA_INIT
	m_pSelection = pSelection;
	m_pView = pView;
	bTextHasChanged = FALSE;
}

void CTextPropPage::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CTextPropPage)
	DDX_Control(pDX, IDC_EDIT_TEXT, m_text);
	DDX_Control(pDX, IDC_SPIN2, m_spin2);
	DDX_Control(pDX, IDC_COMBO3, m_type);
	DDX_Control(pDX, IDC_SPIN1, m_spin);
	DDX_Text(pDX, IDC_EDIT1, m_orientation);
	DDV_MinMaxInt(pDX, m_orientation, 0, 359);
	DDX_CBIndex(pDX, IDC_COMBO1, m_horz);
	DDX_CBIndex(pDX, IDC_COMBO2, m_vert);
	DDX_Text(pDX, IDC_EDIT2, m_precision);
	DDV_MinMaxInt(pDX, m_precision, 0, 10);
	//}}AFX_DATA_MAP
}

BOOL CTextPropPage::FontIsTrueType(LPLOGFONT lpLogFont)
{
	CClientDC dc(NULL);
	CFont font;
	
	font.CreateFontIndirect(lpLogFont);
	dc.SelectObject(&font);
	if(dc.GetOutlineTextMetrics(0, NULL))
		return TRUE;
	else
		return FALSE;
}

BEGIN_MESSAGE_MAP(CTextPropPage, CPropertyPage)
//{{AFX_MSG_MAP(CTextPropPage)
ON_BN_CLICKED(IDC_BUTTON1, OnSchrift)
ON_CBN_SELCHANGE(IDC_COMBO1, OnChange)
ON_NOTIFY(UDN_DELTAPOS, IDC_SPIN1, OnDeltaposSpin1)
ON_NOTIFY(UDN_DELTAPOS, IDC_SPIN2, OnDeltaposSpin2)
ON_EN_CHANGE(IDC_EDIT_TEXT, OnChangeEditText)
ON_CBN_SELCHANGE(IDC_COMBO2, OnChange)
ON_EN_CHANGE(IDC_EDIT1, OnChange)
ON_EN_CHANGE(IDC_EDIT2, OnChange)
ON_CBN_SELCHANGE(IDC_COMBO3, OnSelchangeCombo3)
//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CTextPropPage message handlers

void CTextPropPage::OnSchrift() 
{
	CFontDialog dlg(&m_logfont, CF_EFFECTS | CF_BOTH, NULL, this);
	dlg.m_cf.rgbColors = m_color;
	
	if (dlg.DoModal() != IDOK)
		return;
	
	m_logfont = dlg.m_lf;
	// set CLIP_LH_ANGLES to ensure the coordinate system for all devices is the same
	m_logfont.lfClipPrecision = (BYTE)(m_logfont.lfClipPrecision | CLIP_LH_ANGLES);
	m_color = dlg.m_cf.rgbColors;
	if (FontIsTrueType(&dlg.m_lf))
		GetDlgItem(IDC_EDIT1)->EnableWindow(TRUE);
	else
		GetDlgItem(IDC_EDIT1)->EnableWindow(FALSE);
	SetModified(TRUE);
}

BOOL CTextPropPage::OnInitDialog() 
{
	ASSERT( m_pView != NULL );
	
	CPropertyPage::OnInitDialog();
	
	CString drawText;
	BOOL bHasNormalText = FALSE;
	BOOL bHasPrecision = FALSE;
	CDrawObj* pDrawObj = NULL;
	
	POSITION pos = m_pSelection->GetHeadPosition();
	while( pos != NULL )
	{
		CDrawObj* pObj = m_pSelection->GetNextObject( pos );
		if( pObj->IsText() )
		{
			if( pDrawObj == NULL )
				pDrawObj = pObj;
			
			if( ((CDrawRect*)pObj)->GetTextType() != CDrawRect::normal )
			{
				bHasPrecision = TRUE;
				if( drawText.IsEmpty() )
					((CDrawDoc*)m_pView->GetDocument())->GetDrawText( (CDrawRect*)pObj, drawText );
			}
			else
			{
				bHasNormalText = TRUE;
				if( drawText.IsEmpty() )
					((CDrawDoc*)m_pView->GetDocument())->GetDrawText( (CDrawRect*)pObj, drawText );
				else
					drawText = TEXT(" "); // nur wenn genau ein Textobjekt da ist seinen Text darstellen, sonst leer
			};
		}
	}; // while
	
	if ( drawText == TEXT(" ") )
		drawText.Empty();
	
	ASSERT( pDrawObj );
	
	m_horz = ((CDrawRect*)pDrawObj)->m_nHorzJust;
	m_vert = ((CDrawRect*)pDrawObj)->m_nVertJust;
	int orient = pDrawObj->m_logfont.lfOrientation;
	m_spin.SetRange(0, 359);
	m_spin.SetPos(orient);
	m_logfont = pDrawObj->m_logfont;
	m_color = pDrawObj->m_colorText;
	
	m_text.SetWindowText( drawText );
/* STEFAN
	if( !bHasNormalText )
		m_text.EnableWindow( FALSE );
*/

	if (FontIsTrueType(&m_logfont))
		GetDlgItem(IDC_EDIT1)->EnableWindow(TRUE);
	else
		GetDlgItem(IDC_EDIT1)->EnableWindow(FALSE);
	m_orientation = (int)(orient/10);
	m_spin2.SetRange(0, 10);
	if (!bHasPrecision)
	{
		GetDlgItem(IDC_EDIT2)->EnableWindow(FALSE);
		GetDlgItem(IDC_SPIN2)->EnableWindow(FALSE);
	}
	else
	{
		m_precision = ((CDrawRect*)pDrawObj)->GetPrecision();
		m_spin2.SetPos(m_precision);
	}
	if (m_pView->m_bStempelText)
	{
		for( int i = STPL_TEXT_NONE; i < N_STPLTEXTS; i++ )
		{
			CString str;
			if( i== STPL_TEXT_NONE )
				str.LoadString(IDS_NONE);
			else
				m_pView->GetDocument()->GetDefaultStempelText(i, str);
			int j = m_type.AddString( str );
			m_type.SetItemData(j, i);
		}
		i = ((CDrawRect*)pDrawObj)->GetStempelTextType();
		if( i == STPL_TEXT_NONE )
			m_type.SetCurSel( 0 );
		else
		{
			CString str;
			m_pView->GetDocument()->GetDefaultStempelText( i, str );
			m_type.SelectString( 0, str );
		}
		GetDlgItem(IDC_STATIC2)->ShowWindow(FALSE);
		GetDlgItem(IDC_EDIT2)->ShowWindow(FALSE);
		GetDlgItem(IDC_SPIN2)->ShowWindow(FALSE);
		OnSelchangeCombo3();
	}
	else
	{
		GetDlgItem(IDC_STATIC1)->ShowWindow(FALSE);
		GetDlgItem(IDC_COMBO3)->ShowWindow(FALSE);
	}
	UpdateData(FALSE);
	
	return TRUE;  // return TRUE unless you set the focus to a control
	// EXCEPTION: OCX Property Pages should return FALSE
}

void CTextPropPage::OnOK() 
{
	BOOL bUpdateDrawing = FALSE;
	BOOL bNeedsDrawingUpdate = FALSE;
	
	if( !UpdateData( TRUE ) )
		return;
	int orient = m_orientation * 10;
	if( FontIsTrueType( &m_logfont ) )
	{
		m_logfont.lfEscapement = orient;
		m_logfont.lfOrientation = orient;
	}
	else
	{
		m_logfont.lfEscapement = 0;
		m_logfont.lfOrientation = 0;
	}
	
	CString drawText;
	m_text.GetWindowText( drawText );
	
	// add changed objects to undo buffer
	CDrawObjList undoList;		// for undo
	POSITION selPos = m_pSelection->GetHeadPosition();
	while( selPos )
	{
		CDrawObj* pDrawObj = m_pSelection->GetNextObject( selPos );
		if( pDrawObj->IsText() )
			undoList.AddTailObject( pDrawObj );
	}
	if( undoList.GetObjectCount() > 0 )
		m_pView->AddToUndoBuffer( &undoList, HINT_OBJ_EDIT );
	
	POSITION pos = m_pSelection->GetHeadPosition();
	while( pos != NULL )
	{
		CDrawObj* pDrawObj = m_pSelection->GetNextObject( pos );
		if( pDrawObj->IsText() )
		{
			if( pDrawObj->m_logfont != m_logfont )
				bNeedsDrawingUpdate = TRUE;
			pDrawObj->m_logfont = m_logfont;
			pDrawObj->m_colorText = m_color;
			((CDrawRect*)pDrawObj)->SetHorzJust( m_horz );
			((CDrawRect*)pDrawObj)->SetVertJust( m_vert );
			
			if( !pDrawObj->IsUser() )
				bUpdateDrawing = TRUE;
			if( ((CDrawRect*)pDrawObj)->GetTextType()!=CDrawRect::normal )
			{
				if( ((CDrawRect*)pDrawObj)->GetPrecision() != m_precision )
				{
					((CDrawRect*)pDrawObj)->SetPrecision( m_precision );
					bNeedsDrawingUpdate = TRUE;
				}
			}

			if( bTextHasChanged )
			{
				((CDrawDoc*)m_pView->GetDocument())->SetDrawText( (CDrawRect*)pDrawObj, drawText );
				
				bNeedsDrawingUpdate = TRUE;
				
				pDrawObj->Invalidate();
				pDrawObj->m_pDocument->SetModifiedFlag();
			};
		}; // if IsText
	}; // while pos
	
	CDrawObj* pDrawObj = m_pSelection->GetHeadObject();
	int i = ((CDrawRect*)pDrawObj)->GetStempelTextType();
	if (m_pView->m_bStempelText)
	{
		m_pView->m_texts.RemoveKey(i);
		int j = m_type.GetItemData(m_type.GetCurSel());
		if( j != STPL_TEXT_NONE )
		{
			CString str;
			m_pView->GetDocument()->GetDefaultStempelText(j, str);
			((CDrawRect*)pDrawObj)->SetText(str);
			pDrawObj->UnsetFlags(CDrawObj::editable);
		}
		else
			pDrawObj->SetFlags(CDrawObj::editable);
		((CDrawRect*)pDrawObj)->SetStempelTextType(j);
		m_pView->m_texts.SetAt(j, 1);
		pDrawObj->Invalidate();
	}
	
	if (bUpdateDrawing && bNeedsDrawingUpdate)
		pDrawObj->m_pDocument->UpdateDrawing();
	
	CPropertyPage::OnOK();
} // OnOK

void CTextPropPage::OnChange() 
{
	SetModified(TRUE);
}

void CTextPropPage::OnDeltaposSpin1(NMHDR* pNMHDR, LRESULT* pResult) 
{
	int newOrient;
	
	NM_UPDOWN* pNMUpDown = (NM_UPDOWN*)pNMHDR;
	newOrient = pNMUpDown->iDelta;
	newOrient += m_orientation;
	if (newOrient>=0 && newOrient<=359)
		m_orientation = newOrient;
	SetModified(TRUE);
	UpdateData(FALSE);
	
	*pResult = 0;
}

void CTextPropPage::OnDeltaposSpin2(NMHDR* pNMHDR, LRESULT* pResult) 
{
	int newPrecision;
	
	NM_UPDOWN* pNMUpDown = (NM_UPDOWN*)pNMHDR;
	newPrecision = pNMUpDown->iDelta;
	newPrecision += m_precision;
	if (newPrecision>=0 && newPrecision<=10)
		m_precision = newPrecision;
	SetModified(TRUE);
	UpdateData(FALSE);
	
	*pResult = 0;
}

void CTextPropPage::OnChangeEditText() 
{
	bTextHasChanged = TRUE;
	SetModified( TRUE );
}; // OnChangeEditText

void CTextPropPage::OnSelchangeCombo3() 
// Behandlungsroutine, falls der Texttyp ( nur Stempeleditor ) geändert wird
{
	if ( m_type.GetItemData( m_type.GetCurSel() ) == STPL_TEXT_NONE )
		m_text.EnableWindow( TRUE );
	else
		m_text.EnableWindow( FALSE );
	
	SetModified( TRUE );
}; // OnSelchalngecombo3
