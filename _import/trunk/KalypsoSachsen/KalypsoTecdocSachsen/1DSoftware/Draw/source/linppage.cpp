// linppage.cpp : implementation file
//

#include "stdafx.h"

#include "drawobj.h"
#include "drawvw.h"
#include "drawdoc.h"
#include "draw.h"

#include "linppage.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CLinePropPage property page

CLinePropPage::CLinePropPage(CDrawObjList* pSelection, CDrawView* pView ) : 
	CPropertyPage( CLinePropPage::IDD )
{
	//{{AFX_DATA_INIT(CLinePropPage)
	m_breite = 0.0;
	//}}AFX_DATA_INIT
	m_pSelection = pSelection;
	m_pView = pView;
}

void CLinePropPage::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CLinePropPage)
	DDX_Control(pDX, IDC_SPIN1, m_spin);
	DDX_Control(pDX, IDC_COMBO3, m_color);
	DDX_Control(pDX, IDC_COMBO1, m_type);
	DDX_Control(pDX, IDC_COMBO9, m_rightArrow);
	DDX_Control(pDX, IDC_COMBO8, m_leftArrow);
	DDX_Text(pDX, IDC_EDIT1, m_breite);
	DDV_MinMaxDouble(pDX, m_breite, 0, 10.0);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CLinePropPage, CPropertyPage)
	//{{AFX_MSG_MAP(CLinePropPage)
	ON_NOTIFY(UDN_DELTAPOS, IDC_SPIN1, OnDeltaposSpin1)
	ON_EN_KILLFOCUS(IDC_EDIT1, OnKillfocusEdit1)
	ON_EN_CHANGE(IDC_EDIT1, OnChange)
	ON_CBN_SELCHANGE(IDC_COMBO1, OnSelchangeCombo1)
	ON_CBN_SELCHANGE(IDC_COMBO3, OnChange)
	ON_CBN_SELCHANGE(IDC_COMBO8, OnChange)
	ON_CBN_SELCHANGE(IDC_COMBO9, OnChange)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CLinePropPage message handlers

BOOL CLinePropPage::OnInitDialog() 
{
	int width, style, leftArrow, rightArrow;
	COLORREF color;
	POSITION pos;
	CDrawObj *pDrawObj;
	
	HPALETTE hPal = GETDRAWAPP->m_hPal;

	CPropertyPage::OnInitDialog();
	
	ASSERT(m_pSelection!=NULL);
	
	m_type.Init();
	m_color.SetPalette(hPal);
	width = style = 0;
	color = 0;
	leftArrow = rightArrow = -1;
	GetDlgItem(IDC_STATIC1)->ShowWindow(FALSE);
	m_leftArrow.ShowWindow(FALSE);
	GetDlgItem(IDC_STATIC2)->ShowWindow(FALSE);
	m_rightArrow.ShowWindow(FALSE);
	pos = m_pSelection->GetHeadPosition();
	while (pos!=NULL)
	{
		pDrawObj = m_pSelection->GetNextObject( pos );
		if (pDrawObj->HasPen())
		{
			if (width==0)
				width = pDrawObj->m_logpen.lopnWidth.x;
			if (style==0)
				style = pDrawObj->m_logpen.lopnStyle;
			if (color==0)
				color = pDrawObj->m_logpen.lopnColor;
		}
		if( pDrawObj->HasArrow() && pDrawObj->IsUser() )
		{
			if (leftArrow==-1 || rightArrow==-1)
			{
				GetDlgItem(IDC_STATIC1)->ShowWindow(TRUE);
				m_leftArrow.ShowWindow(TRUE);
				GetDlgItem(IDC_STATIC2)->ShowWindow(TRUE);
				m_rightArrow.ShowWindow(TRUE);
				leftArrow = pDrawObj->m_nLeftArrow;
				rightArrow = pDrawObj->m_nRightArrow;
			}
		}
	}
	m_spin.SetRange(0, 10*MM_FACTOR);
	m_spin.SetPos(width);
	m_breite = width;
	m_breite /= MM_FACTOR;
	m_type.SetCurSel(style);
	m_color.SetColor(color);
	m_leftArrow.SetCurSel(leftArrow);
	m_rightArrow.SetCurSel(rightArrow);

  int type=m_type.GetCurSel();
  if(type!=0)
  {
    m_spin.EnableWindow(FALSE);
    GetDlgItem(IDC_EDIT1)->EnableWindow(FALSE);
    m_breite=0;
  }
  else
  {
    m_spin.EnableWindow(TRUE);
    GetDlgItem(IDC_EDIT1)->EnableWindow(TRUE);
  };

	UpdateData(FALSE);
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void CLinePropPage::OnOK() 
{
	int width, style, leftArrow, rightArrow;
	COLORREF color;
	POSITION pos;
	CDrawObj *pDrawObj;
	CDrawObjList *pObjList;		// for undo
	BOOL bChanged;

	if (!UpdateData(TRUE))
		return;
	width = (int)(m_breite*MM_FACTOR);
	style = m_type.GetCurSel();
	color = m_color.GetColor();
	leftArrow = m_leftArrow.GetCurSel();
	rightArrow = m_rightArrow.GetCurSel();
	// add changed objects to undo buffer
	pObjList = new CDrawObjList;
	pos = m_pSelection->GetHeadPosition();
	while (pos!=NULL)
	{
		pDrawObj = m_pSelection->GetNextObject( pos );
		bChanged = FALSE;
		if (pDrawObj->HasPen())
			bChanged = TRUE;
		if (pDrawObj->HasArrow())
			bChanged = TRUE;
		if (bChanged)
			pObjList->AddTailObject( pDrawObj );
	}
	if( pObjList->GetObjectCount() > 0 )
		m_pView->AddToUndoBuffer(pObjList, HINT_OBJ_EDIT);
	delete pObjList;

	pos = m_pSelection->GetHeadPosition();
	while (pos!=NULL)
	{
		pDrawObj = m_pSelection->GetNextObject( pos );
		bChanged = FALSE;
		if (pDrawObj->HasPen())
		{
			pDrawObj->m_logpen.lopnStyle = style;
			pDrawObj->m_logpen.lopnWidth.x = width;
			pDrawObj->m_logpen.lopnColor = color;
			bChanged = TRUE;
		}
		if (pDrawObj->HasArrow())
		{
			pDrawObj->m_nLeftArrow = (CDrawObj::ArrowHead)leftArrow;
			pDrawObj->m_nRightArrow = (CDrawObj::ArrowHead)rightArrow;
			bChanged = TRUE;
		}
		if (bChanged)
		{
			pDrawObj->Invalidate();
			pDrawObj->m_pDocument->SetModifiedFlag();
		}
	}

	CPropertyPage::OnOK();
}

void CLinePropPage::OnDeltaposSpin1(NMHDR* pNMHDR, LRESULT* pResult) 
{
	int newWidth;

	NM_UPDOWN* pNMUpDown = (NM_UPDOWN*)pNMHDR;
	newWidth = pNMUpDown->iDelta;
	newWidth += (int)(m_breite * MM_FACTOR);
	if (newWidth>=0 && newWidth <= 10 *MM_FACTOR )
		m_breite = (double)newWidth / MM_FACTOR;
	SetModified(TRUE);
	UpdateData(FALSE);
	
	*pResult = 0;
}

void CLinePropPage::OnKillfocusEdit1() 
{
	UpdateData();
	m_spin.SetPos((int)m_breite*MM_FACTOR);
}

void CLinePropPage::OnChange() 
{
	SetModified(TRUE);
}

void CLinePropPage::OnSelchangeCombo1() 
{
  int type=m_type.GetCurSel();
  if(type!=0)
  {
    m_breite=0;
    m_spin.EnableWindow(FALSE);
    GetDlgItem(IDC_EDIT1)->EnableWindow(FALSE);
    UpdateData(FALSE);
  }
  else
  {
    m_spin.EnableWindow(TRUE);
    GetDlgItem(IDC_EDIT1)->EnableWindow(TRUE);
  };

	SetModified(TRUE);	
}

