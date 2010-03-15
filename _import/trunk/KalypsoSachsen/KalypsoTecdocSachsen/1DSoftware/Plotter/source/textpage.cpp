// TextPage.cpp : implementation file
//

#include "stdafx.h"

#include "..\..\wspprj\wspprj.h"

#include "drawobj.h"
#include "plotdoc.h"
#include "plotdocdata.h"
#include "plotview.h"
#include "template.h"
#include "propdlg.h"
#include "plotter.h"

#include "textpage.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CTextPage property page

CTextPage::CTextPage(CPropertyDialog *pParent, CPlotterDoc* pDoc /*=NULL*/, BOOL bTemplate /*=FALSE*/) : CPropertyPage(CTextPage::IDD)
{
	//{{AFX_DATA_INIT(CTextPage)
	m_orientation = 0;
	m_horz = -1;
	m_vert = -1;
	m_nonegative = FALSE;
	m_precision = 0;
	//}}AFX_DATA_INIT
	m_pParent = pParent;
	m_pDoc = pDoc;
  if( pDoc != NULL )
    m_pView = (CPlotterView*)pDoc->GetView();
  else
    m_pView = NULL;
	m_pTemp = NULL;
	m_bTemplate = bTemplate;
}

CTextPage::~CTextPage()
{
	POSITION pos = m_logfonts.GetStartPosition();
	while( pos )
	{
    LPLOGFONT lpLogFont = NULL;
    HANDLE hTI = NULL;
		m_logfonts.GetNextAssoc(pos, hTI, lpLogFont);
		delete lpLogFont;
	}
	m_logfonts.RemoveAll();

  pos = m_data.GetStartPosition();
	while( pos)
	{
    DATA* lpData = NULL;
    HANDLE hTI = NULL;
		m_data.GetNextAssoc( pos, hTI, lpData );
		delete lpData;
	}
	m_data.RemoveAll();
}

void CTextPage::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CTextPage)
	DDX_Control(pDX, IDC_SPIN2, m_spin2);
	DDX_Control(pDX, IDC_SPIN1, m_spin);
	DDX_Control(pDX, IDC_TREE1, m_tree);
	DDX_Text(pDX, IDC_EDIT1, m_orientation);
	DDV_MinMaxInt(pDX, m_orientation, 0, 359);
	DDX_CBIndex(pDX, IDC_COMBO1, m_horz);
	DDX_CBIndex(pDX, IDC_COMBO2, m_vert);
	DDX_Check(pDX, IDC_CHECK1, m_nonegative);
	DDX_Text(pDX, IDC_EDIT2, m_precision);
	DDV_MinMaxInt(pDX, m_precision, 0, 10);
	//}}AFX_DATA_MAP
}

void CTextPage::ApplyTemplate(CTemplate *pTemp)
{
	m_pTemp = pTemp;
	UpdateTree();
	m_pTemp = NULL;
}

BEGIN_MESSAGE_MAP(CTextPage, CPropertyPage)
	//{{AFX_MSG_MAP(CTextPage)
	ON_NOTIFY(TVN_BEGINLABELEDIT, IDC_TREE1, OnBeginlabeleditTree1)
	ON_NOTIFY(TVN_ENDLABELEDIT, IDC_TREE1, OnEndlabeleditTree1)
	ON_BN_CLICKED(IDC_BUTTON1, OnShrift)
	ON_NOTIFY(UDN_DELTAPOS, IDC_SPIN1, OnDeltaposSpin1)
	ON_NOTIFY(TVN_SELCHANGED, IDC_TREE1, OnSelchangedTree1)
	ON_CBN_SELCHANGE(IDC_COMBO1, OnSelchangeHoriz)
	ON_CBN_SELCHANGE(IDC_COMBO2, OnSelchangeVert)
	ON_EN_CHANGE(IDC_EDIT1, OnChangeAngle)
	ON_EN_KILLFOCUS(IDC_EDIT1, OnKillfocusAngle)
	ON_BN_CLICKED(IDC_CHECK1, OnNoNegative)
	ON_EN_CHANGE(IDC_EDIT2, OnChangePrecision)
	ON_EN_KILLFOCUS(IDC_EDIT2, OnKillfocusPrecision)
	ON_NOTIFY(UDN_DELTAPOS, IDC_SPIN2, OnDeltaposSpin2)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CTextPage message handlers

BOOL CTextPage::OnInitDialog() 
{
	ASSERT(m_pDoc!=NULL);

	CPropertyPage::OnInitDialog();
	
	m_pParent->m_nActivePages++;
	// initialize controls
	m_spin.SetRange(0, 359);
	m_spin2.SetRange(0, 10);
	// initialize tree image list
	m_tree.SetImageList( CCommonImageList::GetList( FALSE ), TVSIL_NORMAL);
	UpdateTree();

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

#define ITEM_TITLE		0
#define ITEM_HEIGHT		1
#define ITEM_COMMENT  2
#define ITEM_TABLE		3
#define ITEM_STEMPEL	4
#define ITEM_STATION	5

#define N_ITEMS			6

void CTextPage::UpdateTree()
{
	HTREEITEM hTI, hTI1, hTI2;
	HANDLE hTreeItem, hTS;
	DataBlock temp(NULL);
	CDrawObj *pObj, *pTempObj;
	LPLOGFONT lpLogFont;
	DATA *lpData;
	POSITION pos;
	State *st;
	Section *sec;
	CString str, fmt;
	int i, j;
	CTypedPtrMap<CMapWordToPtr, int, HANDLE> sections;
	CString empty;
	CDrawObjListArray *tableKey1 = m_pDoc->GetTableKey1();
	CDrawObjListArray *tableKey2 = m_pDoc->GetTableKey2();
	CDrawObjListArray *table = m_pDoc->GetTable();
	CDrawObjList *stempel = m_pDoc->GetStempel();
	CDrawObjList *rahmen = m_pDoc->GetRahmen();
	CDrawObjList *profil = m_pDoc->GetProfil();
	CDrawRect* titel = m_pDoc->GetTitle()->GetTitle();
	CDrawRect* height = m_pDoc->GetHeight();
  CDrawRect* comment = m_pDoc->GetComment();
	CDrawRect *ctitel, *ltitel;
	CMap<int, int, int, int> dbindexes;

	if (m_bTemplate)
	{
		ctitel = (CDrawRect*)((CTemplate*)m_pDoc)->GetTemplateObj(TMPL_CTITEL, DST_UNKNOWN);
		ltitel = (CDrawRect*)((CTemplate*)m_pDoc)->GetTemplateObj(TMPL_LTITEL, DST_UNKNOWN);
	}

	if (GetSafeHwnd()==NULL)
		return;
	// initialize maps and lists
	pos = m_logfonts.GetStartPosition();
	while (pos!=NULL)
	{
		m_logfonts.GetNextAssoc(pos, hTreeItem, lpLogFont);
		delete lpLogFont;
	}
	m_logfonts.RemoveAll();
	pos = m_data.GetStartPosition();
	while (pos!=NULL)
	{
		m_data.GetNextAssoc(pos, hTreeItem, lpData);
		delete lpData;
	}
	m_data.RemoveAll();
	m_objects.RemoveAll();
	m_updated.RemoveAll();
	m_items.RemoveAll();
	m_edits.RemoveAll();
	empty.LoadString(IDS_EMPTY);
	// initialize tree
	m_tree.DeleteAllItems();

	// add Titel
	if (titel!=NULL && !titel->IsInvisible())
	{
		if (titel->IsText())
		{
			str.LoadString(IDS_TITLE);
			hTI = m_tree.InsertItem(str, IMAGE_TITEL, IMAGE_TITEL);
			m_items.AddTail(hTI);
			if (m_pTemp!=NULL)
				pTempObj = m_pTemp->GetTemplateObj(TMPL_TITEL, DST_UNKNOWN);
			lpLogFont = new LOGFONT;
			if (m_pTemp!=NULL)
				*lpLogFont = pTempObj->m_logfont;
			else
				*lpLogFont = titel->m_logfont;
			m_logfonts.SetAt(hTI, lpLogFont);
			lpData = new DATA;
			if (m_pTemp!=NULL)
			{
				lpData->color = pTempObj->m_colorText;
				lpData->horz = ((CDrawRect*)pTempObj)->m_nHorzJust;
				lpData->vert = ((CDrawRect*)pTempObj)->m_nVertJust;
				lpData->type = ((CDrawRect*)pTempObj)->GetTextType();
				lpData->bShowNeg = ((CDrawRect*)pTempObj)->ShowNegative();
				lpData->precision = ((CDrawRect*)pTempObj)->GetPrecision();
			}
			else
			{
				lpData->color = titel->m_colorText;
				lpData->horz = ((CDrawRect*)titel)->m_nHorzJust;
				lpData->vert = ((CDrawRect*)titel)->m_nVertJust;
				lpData->type = ((CDrawRect*)titel)->GetTextType();
				lpData->bShowNeg = !((CDrawRect*)titel)->ShowNegative();
				lpData->precision = ((CDrawRect*)titel)->GetPrecision();
			}
			m_data.SetAt(hTI, lpData);
			if (m_pTemp!=NULL)
				m_updated.SetAt(hTI, TRUE);
			else
				m_updated.SetAt(hTI, FALSE);

			if (m_bTemplate)
			{
				str.LoadString(IDS_CROSSSECTION);
				hTI1 = m_tree.InsertItem(str, IMAGE_TITEL, IMAGE_TITEL, hTI);
				ctitel->GetText(str);
				if (str.IsEmpty())
					str = empty;
				hTI2 = m_tree.InsertItem(str, IMAGE_TEXT, IMAGE_TEXT, hTI1);
				m_items.AddTail(hTI2);
				lpLogFont = new LOGFONT;
				*lpLogFont = ctitel->m_logfont;
				m_logfonts.SetAt(hTI2, lpLogFont);
				lpData = new DATA;
				lpData->color = ctitel->m_colorText;
				lpData->horz = ((CDrawRect*)ctitel)->m_nHorzJust;
				lpData->vert = ((CDrawRect*)ctitel)->m_nVertJust;
				lpData->type = ((CDrawRect*)ctitel)->GetTextType();
				lpData->bShowNeg = ((CDrawRect*)ctitel)->ShowNegative();
				lpData->precision = ((CDrawRect*)ctitel)->GetPrecision();
				m_data.SetAt(hTI2, lpData);
				m_objects.SetAt(hTI2, ctitel);
				m_updated.SetAt(hTI2, FALSE);
				m_edits.SetAt(hTI2, ctitel);
				str.LoadString(IDS_LENGTHSECTION);
				hTI1 = m_tree.InsertItem(str, IMAGE_TITEL, IMAGE_TITEL, hTI);
				ltitel->GetText(str);
				if (str.IsEmpty())
					str = empty;
				hTI2 = m_tree.InsertItem(str, IMAGE_TEXT, IMAGE_TEXT, hTI1);
				m_items.AddTail(hTI2);
				lpLogFont = new LOGFONT;
				*lpLogFont = ltitel->m_logfont;
				m_logfonts.SetAt(hTI2, lpLogFont);
				lpData = new DATA;
				lpData->color = ltitel->m_colorText;
				lpData->horz = ((CDrawRect*)ltitel)->m_nHorzJust;
				lpData->vert = ((CDrawRect*)ltitel)->m_nVertJust;
				lpData->type = ((CDrawRect*)ltitel)->GetTextType();
				lpData->bShowNeg = ((CDrawRect*)ltitel)->ShowNegative();
				lpData->precision = ((CDrawRect*)ltitel)->GetPrecision();
				m_data.SetAt(hTI2, lpData);
				m_objects.SetAt(hTI2, ltitel);
				m_updated.SetAt(hTI2, FALSE);
				m_edits.SetAt(hTI2, ltitel);
			}
			else
			{
				if (m_pTemp!=NULL)
				{
					if (m_pDoc->GetMSection( 0 )!=NULL)
					{
						if (m_pDoc->GetMSection( 0 )->GetClassType()==CLASS_TYPE_CSECTION)
							((CDrawRect*)m_pTemp->GetTemplateObj(TMPL_CTITEL, DST_UNKNOWN))->GetText(str);
						else
							((CDrawRect*)m_pTemp->GetTemplateObj(TMPL_CTITEL, DST_UNKNOWN))->GetText(str);
					}
					else
						((CDrawRect*)pTempObj)->GetText(str);
				}
				else
					str = m_pDoc->GetTitle()->GetFormatText();
				if (str.IsEmpty())
					str = empty;
				hTI1 = m_tree.InsertItem(str, IMAGE_TEXT, IMAGE_TEXT, hTI);
				m_items.AddTail(hTI1);
				lpLogFont = new LOGFONT;
				if (m_pTemp!=NULL)
					*lpLogFont = pTempObj->m_logfont;
				else
					*lpLogFont = titel->m_logfont;
				m_logfonts.SetAt(hTI1, lpLogFont);
				lpData = new DATA;
				if (m_pTemp!=NULL)
				{
					lpData->color = pTempObj->m_colorText;
					lpData->horz = ((CDrawRect*)pTempObj)->m_nHorzJust;
					lpData->vert = ((CDrawRect*)pTempObj)->m_nVertJust;
					lpData->type = ((CDrawRect*)pTempObj)->GetTextType();
					lpData->bShowNeg = ((CDrawRect*)pTempObj)->ShowNegative();
					lpData->precision = ((CDrawRect*)pTempObj)->GetPrecision();
				}
				else
				{
					lpData->color = titel->m_colorText;
					lpData->horz = ((CDrawRect*)titel)->m_nHorzJust;
					lpData->vert = ((CDrawRect*)titel)->m_nVertJust;
					lpData->type = ((CDrawRect*)titel)->GetTextType();
					lpData->bShowNeg = ((CDrawRect*)titel)->ShowNegative();
					lpData->precision = ((CDrawRect*)titel)->GetPrecision();
				}
				m_data.SetAt(hTI1, lpData);
				m_objects.SetAt(hTI1, titel);
				if (m_pTemp!=NULL)
					m_updated.SetAt(hTI1, TRUE);
				else
					m_updated.SetAt(hTI1, FALSE);
				m_edits.SetAt(hTI1, titel);
			}
		}
	}
	// add Height
	if( height && !height->IsInvisible() && height->IsText() )
	{
    str.LoadString(IDS_HEIGHT);
    hTI = m_tree.InsertItem(str, IMAGE_HEIGHT, IMAGE_HEIGHT);
    m_items.AddTail(hTI);
    if (m_pTemp!=NULL)
      pTempObj = m_pTemp->GetTemplateObj(TMPL_HEIGHT, DST_UNKNOWN);
    lpLogFont = new LOGFONT;
    if (m_pTemp!=NULL)
      *lpLogFont = pTempObj->m_logfont;
    else
      *lpLogFont = height->m_logfont;
    m_logfonts.SetAt(hTI, lpLogFont);
    lpData = new DATA;
    if (m_pTemp!=NULL)
    {
      lpData->color = pTempObj->m_colorText;
      lpData->horz = ((CDrawRect*)pTempObj)->m_nHorzJust;
      lpData->vert = ((CDrawRect*)pTempObj)->m_nVertJust;
      lpData->type = ((CDrawRect*)pTempObj)->GetTextType();
      lpData->bShowNeg = ((CDrawRect*)pTempObj)->ShowNegative();
      lpData->precision = ((CDrawRect*)pTempObj)->GetPrecision();
    }
    else
    {
      lpData->color = height->m_colorText;
      lpData->horz = ((CDrawRect*)height)->m_nHorzJust;
      lpData->vert = ((CDrawRect*)height)->m_nVertJust;
      lpData->type = ((CDrawRect*)height)->GetTextType();
      lpData->bShowNeg = ((CDrawRect*)height)->ShowNegative();
      lpData->precision = ((CDrawRect*)height)->GetPrecision();
    }
    m_data.SetAt(hTI, lpData);
    if (m_pTemp!=NULL)
      m_updated.SetAt(hTI, TRUE);
    else
      m_updated.SetAt(hTI, FALSE);
    if (m_pTemp!=NULL)
      str = m_pTemp->m_pPData->m_heightFormatText;
    else
      str = m_pDoc->m_pPData->m_heightFormatText;
    if (str.IsEmpty())
      str = empty;
    hTI1 = m_tree.InsertItem(str, IMAGE_TEXT, IMAGE_TEXT, hTI);
    m_items.AddTail(hTI1);
    lpLogFont = new LOGFONT;
    if (m_pTemp!=NULL)
      *lpLogFont = pTempObj->m_logfont;
    else
      *lpLogFont = height->m_logfont;
    m_logfonts.SetAt(hTI1, lpLogFont);
    lpData = new DATA;
    if (m_pTemp!=NULL)
    {
      lpData->color = pTempObj->m_colorText;
      lpData->horz = ((CDrawRect*)pTempObj)->m_nHorzJust;
      lpData->vert = ((CDrawRect*)pTempObj)->m_nVertJust;
      lpData->type = ((CDrawRect*)pTempObj)->GetTextType();
      lpData->bShowNeg = ((CDrawRect*)pTempObj)->ShowNegative();
      lpData->precision = ((CDrawRect*)pTempObj)->GetPrecision();
    }
    else
    {
      lpData->color = height->m_colorText;
      lpData->horz = ((CDrawRect*)height)->m_nHorzJust;
      lpData->vert = ((CDrawRect*)height)->m_nVertJust;
      lpData->type = ((CDrawRect*)height)->GetTextType();
      lpData->bShowNeg = ((CDrawRect*)height)->ShowNegative();
      lpData->precision = ((CDrawRect*)height)->GetPrecision();
    }
    m_data.SetAt(hTI1, lpData);
    m_objects.SetAt(hTI1, height);
    if (m_pTemp!=NULL)
      m_updated.SetAt(hTI1, TRUE);
    else
      m_updated.SetAt(hTI1, FALSE);
    m_edits.SetAt(hTI1, height);
	}

	// add Comment
	if( comment && comment->IsText() )
	{
    CDrawRect* pCommentRect;
    if( m_pTemp )
      pCommentRect = (CDrawRect*)m_pTemp->GetTemplateObj( TMPL_COMMENT, DST_UNKNOWN );
    else
      pCommentRect = (CDrawRect*)comment;

    if( !pCommentRect->IsInvisible() )
    {
      str.LoadString( IDS_TITLECOMMENT );
      hTI = m_tree.InsertItem( str, IMAGE_TITEL, IMAGE_TITEL ); // Commentar hat gleiches Icon wie Titel
      m_items.AddTail( hTI );

      lpLogFont = new LOGFONT;
      *lpLogFont = pCommentRect->m_logfont;
      m_logfonts.SetAt( hTI, lpLogFont );
      
      lpData = new DATA;
      lpData->color = pCommentRect->m_colorText;
      lpData->horz = pCommentRect->m_nHorzJust;
      lpData->vert = pCommentRect->m_nVertJust;
      lpData->type = pCommentRect->GetTextType();
      lpData->bShowNeg = pCommentRect->ShowNegative();
      lpData->precision = pCommentRect->GetPrecision();
      
      m_data.SetAt( hTI, lpData );
      if( m_pTemp )
        m_updated.SetAt( hTI, TRUE );
      else
        m_updated.SetAt( hTI, FALSE );
      
      if( m_pTemp )
        str = m_pTemp->m_pPData->m_commentFormatText;
      else
        str = m_pDoc->m_pPData->m_commentFormatText;
      
      if( str.IsEmpty() )
        str = empty;
      hTI1 = m_tree.InsertItem( str, IMAGE_TEXT, IMAGE_TEXT, hTI );
      m_items.AddTail( hTI1 );
      
      lpLogFont = new LOGFONT;
      *lpLogFont = pCommentRect->m_logfont;
      m_logfonts.SetAt( hTI1, lpLogFont );
      
      lpData = new DATA;
      lpData->color = pCommentRect->m_colorText;
      lpData->horz = pCommentRect->m_nHorzJust;
      lpData->vert = pCommentRect->m_nVertJust;
      lpData->type = pCommentRect->GetTextType();
      lpData->bShowNeg = pCommentRect->ShowNegative();
      lpData->precision = pCommentRect->GetPrecision();
      
      m_data.SetAt( hTI1, lpData );
      m_objects.SetAt( hTI1, comment );
      
      if( m_pTemp )
        m_updated.SetAt(hTI1, TRUE);
      else
        m_updated.SetAt(hTI1, FALSE);
      
      m_edits.SetAt(hTI1, comment);
    }; // if !pCommentRect.IsInvisible
  }; // AddComment


	// add Tabelle
	if (table->GetSize()>0)
	{
		str.LoadString(IDS_TABLE);
		hTI = m_tree.InsertItem(str, IMAGE_TABELLE, IMAGE_TABELLE);
		m_hTITable = hTI;
		m_items.AddTail(hTI);
		for (i=0; i<table->GetSize(); i++)
		{
			pos = table->GetAt(i)->GetHeadPosition();
			while (pos!=NULL)
			{
				pObj = table->GetAt(i)->GetNextObject( pos );
				if (!pObj->IsInvisible() && pObj->IsText())// && ((CDrawRect*)pObj)->GetTextType()==CDrawRect::normal)
				{
					break;
				}
			}
		}
		if (m_pTemp!=NULL)
		{
			for (i=0; i<m_pTemp->GetTable()->GetSize(); i++)
			{
				pos = m_pTemp->GetTable()->GetAt(i)->GetHeadPosition();
				while (pos!=NULL)
				{
					pTempObj = m_pTemp->GetTable()->GetAt(i)->GetNextObject(pos);
					if (!pTempObj->IsInvisible() && pTempObj->IsText())// && ((CDrawRect*)pTempObj)->GetTextType()==CDrawRect::normal)
					{
						break;
					}
				}
			}
		}
		lpLogFont = new LOGFONT;
		if (m_pTemp!=NULL)
			*lpLogFont = pTempObj->m_logfont;
		else
			*lpLogFont = pObj->m_logfont;
		m_logfonts.SetAt(hTI, lpLogFont);
		lpData = new DATA;
		if (m_pTemp!=NULL)
		{
			lpData->color = pTempObj->m_colorText;
			lpData->horz = ((CDrawRect*)pTempObj)->m_nHorzJust;
			lpData->vert = ((CDrawRect*)pTempObj)->m_nVertJust;
			lpData->type = ((CDrawRect*)pTempObj)->GetTextType();
			lpData->bShowNeg = ((CDrawRect*)pTempObj)->ShowNegative();
			lpData->precision = ((CDrawRect*)pTempObj)->GetPrecision();
		}
		else
		{
			lpData->color = pObj->m_colorText;
			lpData->horz = ((CDrawRect*)pObj)->m_nHorzJust;
			lpData->vert = ((CDrawRect*)pObj)->m_nVertJust;
			lpData->type = ((CDrawRect*)pObj)->GetTextType();
			lpData->bShowNeg = ((CDrawRect*)pObj)->ShowNegative();
			lpData->precision = ((CDrawRect*)pObj)->GetPrecision();
		}
		m_data.SetAt(hTI, lpData);
		m_objects.SetAt(hTI, pObj);
		if (m_pTemp!=NULL)
			m_updated.SetAt(hTI, TRUE);
		else
			m_updated.SetAt(hTI, FALSE);
	}
	// add Stempel
	if (stempel->GetObjectCount()>0)
	{
		str.LoadString(IDS_STAMP);
		hTI = m_tree.InsertItem(str, IMAGE_STEMPEL, IMAGE_STEMPEL);
    m_hTIStamp = hTI;
		m_items.AddTail(hTI);
		pos = stempel->GetHeadPosition();
		while (pos!=NULL)
		{
			pObj = stempel->GetNextObject(pos);
			if (!pObj->IsInvisible() && pObj->IsText())
			{
				int i = ((CDrawRect*)pObj)->GetStempelTextType();
				if (i!=STPL_TEXT_NONE)
				{
					m_pDoc->GetDefaultStempelText(i, str);
					hTI1 = m_tree.InsertItem(str, IMAGE_TEXT, IMAGE_TEXT, hTI);
					m_items.AddTail(hTI1);
					if (m_pTemp!=NULL)
						pTempObj = m_pTemp->GetTemplateObj(i, DST_UNKNOWN);
					lpLogFont = new LOGFONT;
					if (m_pTemp!=NULL)
						*lpLogFont = pTempObj->m_logfont;
					else
						*lpLogFont = pObj->m_logfont;
					m_logfonts.SetAt(hTI1, lpLogFont);
					lpData = new DATA;
					if (m_pTemp!=NULL)
					{
						lpData->color = pTempObj->m_colorText;
						lpData->horz = ((CDrawRect*)pTempObj)->m_nHorzJust;
						lpData->vert = ((CDrawRect*)pTempObj)->m_nVertJust;
						lpData->type = ((CDrawRect*)pTempObj)->GetTextType();
						lpData->bShowNeg = ((CDrawRect*)pTempObj)->ShowNegative();
						lpData->precision = ((CDrawRect*)pTempObj)->GetPrecision();
					}
					else
					{
						lpData->color = pObj->m_colorText;
						lpData->horz = ((CDrawRect*)pObj)->m_nHorzJust;
						lpData->vert = ((CDrawRect*)pObj)->m_nVertJust;
						lpData->type = ((CDrawRect*)pObj)->GetTextType();
						lpData->bShowNeg = ((CDrawRect*)pObj)->ShowNegative();
						lpData->precision = ((CDrawRect*)pObj)->GetPrecision();
					}
					m_data.SetAt(hTI1, lpData);
					if (m_pTemp!=NULL)
						m_updated.SetAt(hTI1, TRUE);
					else
						m_updated.SetAt(hTI1, FALSE);
					if (m_pTemp!=NULL)
					{
						((CDrawRect*)pTempObj)->GetText(str);
						if (str.IsEmpty())
							((CDrawRect*)pObj)->GetText(str);
					}
					else
						((CDrawRect*)pObj)->GetText(str);
					if (str.IsEmpty())
						str = empty;
					hTI2 = m_tree.InsertItem(str, IMAGE_TEXT, IMAGE_TEXT, hTI1);
					m_items.AddTail(hTI2);
					lpLogFont = new LOGFONT;
					if (m_pTemp!=NULL)
						*lpLogFont = pTempObj->m_logfont;
					else
						*lpLogFont = pObj->m_logfont;
					m_logfonts.SetAt(hTI2, lpLogFont);
					lpData = new DATA;
					if (m_pTemp!=NULL)
					{
						lpData->color = pTempObj->m_colorText;
						lpData->horz = ((CDrawRect*)pTempObj)->m_nHorzJust;
						lpData->vert = ((CDrawRect*)pTempObj)->m_nVertJust;
						lpData->type = ((CDrawRect*)pTempObj)->GetTextType();
						lpData->bShowNeg = ((CDrawRect*)pTempObj)->ShowNegative();
						lpData->precision = ((CDrawRect*)pTempObj)->GetPrecision();
					}
					else
					{
						lpData->color = pObj->m_colorText;
						lpData->horz = ((CDrawRect*)pObj)->m_nHorzJust;
						lpData->vert = ((CDrawRect*)pObj)->m_nVertJust;
						lpData->type = ((CDrawRect*)pObj)->GetTextType();
						lpData->bShowNeg = ((CDrawRect*)pObj)->ShowNegative();
						lpData->precision = ((CDrawRect*)pObj)->GetPrecision();
					}
					m_data.SetAt(hTI2, lpData);
					m_objects.SetAt(hTI2, pObj);
					if (m_pTemp!=NULL)
						m_updated.SetAt(hTI2, TRUE);
					else
						m_updated.SetAt(hTI2, FALSE);
					m_edits.SetAt(hTI2, pObj);
				}
			}
		}
		lpLogFont = new LOGFONT;
		if (m_pTemp!=NULL)
			*lpLogFont = pTempObj->m_logfont;
		else
			*lpLogFont = pObj->m_logfont;
		m_logfonts.SetAt(hTI, lpLogFont);
		lpData = new DATA;
		if (m_pTemp!=NULL)
		{
			lpData->color = pTempObj->m_colorText;
			lpData->horz = ((CDrawRect*)pTempObj)->m_nHorzJust;
			lpData->vert = ((CDrawRect*)pTempObj)->m_nVertJust;
			lpData->type = ((CDrawRect*)pTempObj)->GetTextType();
			lpData->bShowNeg = ((CDrawRect*)pTempObj)->ShowNegative();
			lpData->precision = ((CDrawRect*)pTempObj)->GetPrecision();
		}
		else
		{
			lpData->color = pObj->m_colorText;
			lpData->horz = ((CDrawRect*)pObj)->m_nHorzJust;
			lpData->vert = ((CDrawRect*)pObj)->m_nVertJust;
			lpData->type = ((CDrawRect*)pObj)->GetTextType();
			lpData->bShowNeg = ((CDrawRect*)pObj)->ShowNegative();
			lpData->precision = ((CDrawRect*)pObj)->GetPrecision();
		}
		m_data.SetAt(hTI, lpData);
		m_objects.SetAt(hTI, pObj);
		if (m_pTemp!=NULL)
			m_updated.SetAt(hTI, TRUE);
		else
			m_updated.SetAt(hTI, FALSE);
	}
	// Add Station
	if (profil->GetObjectCount()>0)
	{
		str.LoadString(IDS_DATA);
		for (j=0; j<m_pDoc->GetSections()->GetSize(); j++)
		{
			st = m_pDoc->GetMState( j );
			sec = m_pDoc->GetMSection( j );
			if (sec!=NULL)
			{
				if (sec->GetClassType()==CLASS_TYPE_CSECTION)
				{
					if (st!=NULL)
					{
						CString wname;
						
						wname = st->GetWaterName();
						str = st->GetName();
						fmt.Format("%.4f", ((CrossSection*)sec)->GetStation());
						str.FormatMessage(IDS_STATION_DESC1, wname, str, fmt);
					}
				}
				else
				{
					str = ((LengthSection*)sec)->GetName();
					if (str.IsEmpty())
						str.LoadString(IDS_DATA);
				}
			}
			hTI = m_tree.InsertItem(str, IMAGE_CSECTION, IMAGE_CSECTION);
			m_items.AddTail(hTI);
			sections.SetAt(j, hTI);
		}
		// Add Profile
		for( i = 0; i < table->GetSize(); i++ )
		{
			BOOL bXCoord, bYCoord, bNorm;
			
			bXCoord = bYCoord = bNorm = FALSE;
			pos = tableKey1->GetAt(i)->GetHeadPosition();

      BOOL bFirst = TRUE; // markiert für den Key1, ob es das erste gefundene objekt war oder ein nächstes ( siehe unten )
			while (pos!=NULL)
			{
				pObj = tableKey1->GetAt(i)->GetNextObject(pos);
				if (!pObj->IsInvisible() && pObj->IsText())
				{
					int type, image;

					// name of DatenSatz
					type = pObj->GetType();
					if (!dbindexes.Lookup(m_bTemplate ? pObj->GetType() : pObj->GetDataBlockIndex(), image))
					{
						if (m_bTemplate)
							dbindexes.SetAt(pObj->GetType(), 1);
						else
							dbindexes.SetAt(pObj->GetDataBlockIndex(), 1);
						temp.SetType(type);
						str = temp.GetDesc(0);
						image = GETPLOTTERAPP->GetImageType(type); 
						sections.Lookup(pObj->GetSectionIndex(), hTS);
						hTI1 = m_tree.InsertItem(str, image, image, (HTREEITEM)hTS);
						m_items.AddTail(hTI1);
						if (m_pTemp!=NULL)
							pTempObj = m_pTemp->GetTemplateObj(TMPL_TABELLE_KEY1TEXT, type);
						lpLogFont = new LOGFONT;
						if (m_pTemp!=NULL)
							*lpLogFont = pTempObj->m_logfont;
						else
							*lpLogFont = pObj->m_logfont;
						m_logfonts.SetAt(hTI1, lpLogFont);
						lpData = new DATA;
						if (m_pTemp!=NULL)
						{
							lpData->color = pTempObj->m_colorText;
							lpData->horz = ((CDrawRect*)pTempObj)->m_nHorzJust;
							lpData->vert = ((CDrawRect*)pTempObj)->m_nVertJust;
							lpData->type = ((CDrawRect*)pTempObj)->GetTextType();
							lpData->bShowNeg = ((CDrawRect*)pTempObj)->ShowNegative();
							lpData->precision = ((CDrawRect*)pTempObj)->GetPrecision();
						}
						else
						{
							lpData->color = pObj->m_colorText;
							lpData->horz = ((CDrawRect*)pObj)->m_nHorzJust;
							lpData->vert = ((CDrawRect*)pObj)->m_nVertJust;
							lpData->type = ((CDrawRect*)pObj)->GetTextType();
							lpData->bShowNeg = ((CDrawRect*)pObj)->ShowNegative();
							lpData->precision = ((CDrawRect*)pObj)->GetPrecision();
						}
						m_data.SetAt(hTI1, lpData);
						if (m_pTemp!=NULL)
							m_updated.SetAt(hTI1, TRUE);
						else
							m_updated.SetAt(hTI1, FALSE);
					}
					// key1 text

          // nur falls es ein Template gibt und falls es das erste Objekt im Key1 ist, den Text
          // aus dem Template übernehmen: bFirst war nötig, damit bei den Wsp-höhen nicht der Text 'HQ10' o.ä.
          // überschrieben wurde
					if( m_pTemp != NULL && bFirst )
						((CDrawRect*)pTempObj)->GetText( str );
					else
						((CDrawRect*)pObj)->GetText( str );
					if (str.IsEmpty())
						str = empty;
					hTI2 = m_tree.InsertItem(str, IMAGE_TEXT, IMAGE_TEXT, hTI1);
					m_items.AddTail(hTI2);
					lpLogFont = new LOGFONT;
					if (m_pTemp!=NULL)
						*lpLogFont = pTempObj->m_logfont;
					else
						*lpLogFont = pObj->m_logfont;
					m_logfonts.SetAt(hTI2, lpLogFont);
					lpData = new DATA;
					if (m_pTemp!=NULL)
					{
						lpData->color = pTempObj->m_colorText;
						lpData->horz = ((CDrawRect*)pTempObj)->m_nHorzJust;
						lpData->vert = ((CDrawRect*)pTempObj)->m_nVertJust;
						lpData->type = ((CDrawRect*)pTempObj)->GetTextType();
						lpData->bShowNeg = ((CDrawRect*)pTempObj)->ShowNegative();
						lpData->precision = ((CDrawRect*)pTempObj)->GetPrecision();
					}
					else
					{
						lpData->color = pObj->m_colorText;
						lpData->horz = ((CDrawRect*)pObj)->m_nHorzJust;
						lpData->vert = ((CDrawRect*)pObj)->m_nVertJust;
						lpData->type = ((CDrawRect*)pObj)->GetTextType();
						lpData->bShowNeg = ((CDrawRect*)pObj)->ShowNegative();
						lpData->precision = ((CDrawRect*)pObj)->GetPrecision();
					}
					m_data.SetAt(hTI2, lpData);
					m_objects.SetAt(hTI2, pObj);
					if (m_pTemp!=NULL)
						m_updated.SetAt(hTI2, TRUE);
					else
						m_updated.SetAt(hTI2, FALSE);
					m_edits.SetAt(hTI2, pObj);
                  
          bFirst = FALSE; // jetzt kanns nicht mehr das erste sein
        } // if IsText && !IsInvisible
      } // while pos
			pos = tableKey2->GetAt(i)->GetHeadPosition();
			while (pos!=NULL)
			{
				pObj = tableKey2->GetAt(i)->GetNextObject(pos);
				if (!pObj->IsInvisible() && pObj->IsText())
				{
					int type;

					// key2 text
					type = pObj->GetType();
					if (m_pTemp!=NULL)
						pTempObj = m_pTemp->GetTemplateObj(TMPL_TABELLE_KEY2TEXT, type);
					if (m_pTemp!=NULL)
						((CDrawRect*)pTempObj)->GetText(str);
					else
						((CDrawRect*)pObj)->GetText(str);
					if (str.IsEmpty())
						str = empty;
					hTI2 = m_tree.InsertItem(str, IMAGE_TEXT, IMAGE_TEXT, hTI1);
					m_items.AddTail(hTI2);
					lpLogFont = new LOGFONT;
					if (m_pTemp!=NULL)
						*lpLogFont = pTempObj->m_logfont;
					else
						*lpLogFont = pObj->m_logfont;
					m_logfonts.SetAt(hTI2, lpLogFont);
					lpData = new DATA;
					if (m_pTemp!=NULL)
					{
						lpData->color = pTempObj->m_colorText;
						lpData->horz = ((CDrawRect*)pTempObj)->m_nHorzJust;
						lpData->vert = ((CDrawRect*)pTempObj)->m_nVertJust;
						lpData->type = ((CDrawRect*)pTempObj)->GetTextType();
						lpData->bShowNeg = ((CDrawRect*)pTempObj)->ShowNegative();
						lpData->precision = ((CDrawRect*)pTempObj)->GetPrecision();
					}
					else
					{
						lpData->color = pObj->m_colorText;
						lpData->horz = ((CDrawRect*)pObj)->m_nHorzJust;
						lpData->vert = ((CDrawRect*)pObj)->m_nVertJust;
						lpData->type = ((CDrawRect*)pObj)->GetTextType();
						lpData->bShowNeg = ((CDrawRect*)pObj)->ShowNegative();
						lpData->precision = ((CDrawRect*)pObj)->GetPrecision();
					}
					m_data.SetAt(hTI2, lpData);
					m_objects.SetAt(hTI2, pObj);
					if (m_pTemp!=NULL)
						m_updated.SetAt(hTI2, TRUE);
					else
						m_updated.SetAt(hTI2, FALSE);
					m_edits.SetAt(hTI2, pObj);
				}
			}
			pos = table->GetAt(i)->GetHeadPosition();
			while (pos!=NULL)
			{
				pObj = table->GetAt(i)->GetNextObject(pos);
				if (!pObj->IsInvisible() && pObj->IsText())
				{
					if (((CDrawRect*)pObj)->GetTextType()==CDrawRect::xcoord && !bXCoord)
					{
						int type;
						
						// xvalue text
						type = pObj->GetType();
						str.LoadString(IDS_XVALUE);
						hTI2 = m_tree.InsertItem(str, IMAGE_TEXT, IMAGE_TEXT, hTI1);
						m_items.AddTail(hTI2);
						if (m_pTemp!=NULL)
							pTempObj = m_pTemp->GetTemplateObj(TMPL_TABELLE_XCOORD, type);
						lpLogFont = new LOGFONT;
						if (m_pTemp!=NULL)
							*lpLogFont = pTempObj->m_logfont;
						else
							*lpLogFont = pObj->m_logfont;
						m_logfonts.SetAt(hTI2, lpLogFont);
						lpData = new DATA;
						if (m_pTemp!=NULL)
						{
							lpData->color = pTempObj->m_colorText;
							lpData->horz = ((CDrawRect*)pTempObj)->m_nHorzJust;
							lpData->vert = ((CDrawRect*)pTempObj)->m_nVertJust;
							lpData->type = ((CDrawRect*)pTempObj)->GetTextType();
							lpData->bShowNeg = ((CDrawRect*)pTempObj)->ShowNegative();
							lpData->precision = ((CDrawRect*)pTempObj)->GetPrecision();
						}
						else
						{
							lpData->color = pObj->m_colorText;
							lpData->horz = ((CDrawRect*)pObj)->m_nHorzJust;
							lpData->vert = ((CDrawRect*)pObj)->m_nVertJust;
							lpData->type = ((CDrawRect*)pObj)->GetTextType();
							lpData->bShowNeg = ((CDrawRect*)pObj)->ShowNegative();
							lpData->precision = ((CDrawRect*)pObj)->GetPrecision();
						}
						m_data.SetAt(hTI2, lpData);
						m_objects.SetAt(hTI2, pObj);
						if (m_pTemp!=NULL)
							m_updated.SetAt(hTI2, TRUE);
						else
							m_updated.SetAt(hTI2, FALSE);
						m_edits.SetAt(hTI2, pObj);
						bXCoord = TRUE;
					}
					else if (((CDrawRect*)pObj)->GetTextType()==CDrawRect::ycoord && !bYCoord)
					{
						// yvalue text
						int type = pObj->GetType();
						str.LoadString(IDS_YVALUE);
						hTI2 = m_tree.InsertItem(str, IMAGE_TEXT, IMAGE_TEXT, hTI1);
						m_items.AddTail(hTI2);
						if (m_pTemp!=NULL)
							pTempObj = m_pTemp->GetTemplateObj(TMPL_TABELLE_YCOORD, type);
						lpLogFont = new LOGFONT;
						if (m_pTemp!=NULL)
							*lpLogFont = pTempObj->m_logfont;
						else
							*lpLogFont = pObj->m_logfont;
						m_logfonts.SetAt(hTI2, lpLogFont);
						lpData = new DATA;
						if (m_pTemp!=NULL)
						{
							lpData->color = pTempObj->m_colorText;
							lpData->horz = ((CDrawRect*)pTempObj)->m_nHorzJust;
							lpData->vert = ((CDrawRect*)pTempObj)->m_nVertJust;
							lpData->type = ((CDrawRect*)pTempObj)->GetTextType();
							lpData->bShowNeg = ((CDrawRect*)pTempObj)->ShowNegative();
							lpData->precision = ((CDrawRect*)pTempObj)->GetPrecision();
						}
						else
						{
							lpData->color = pObj->m_colorText;
							lpData->horz = ((CDrawRect*)pObj)->m_nHorzJust;
							lpData->vert = ((CDrawRect*)pObj)->m_nVertJust;
							lpData->type = ((CDrawRect*)pObj)->GetTextType();
							lpData->bShowNeg = ((CDrawRect*)pObj)->ShowNegative();
							lpData->precision = ((CDrawRect*)pObj)->GetPrecision();
						}
						m_data.SetAt(hTI2, lpData);
						m_objects.SetAt(hTI2, pObj);
						if (m_pTemp!=NULL)
							m_updated.SetAt(hTI2, TRUE);
						else
							m_updated.SetAt(hTI2, FALSE);
						m_edits.SetAt(hTI2, pObj);
						bYCoord = TRUE;
					}
					else if (((CDrawRect*)pObj)->GetTextType()==CDrawRect::normal && !bNorm)
					{
						int type;
						
						// yvalue text
						type = pObj->GetType();
						str.LoadString(IDS_EXTRATEXT);
						hTI2 = m_tree.InsertItem(str, IMAGE_TEXT, IMAGE_TEXT, hTI1);
						m_items.AddTail(hTI2);
						if (m_pTemp!=NULL)
							pTempObj = m_pTemp->GetTemplateObj(TMPL_TABELLE_NORMTEXT, type);
						lpLogFont = new LOGFONT;
						if (m_pTemp!=NULL)
							*lpLogFont = pTempObj->m_logfont;
						else
							*lpLogFont = pObj->m_logfont;
						m_logfonts.SetAt(hTI2, lpLogFont);
						lpData = new DATA;
						if (m_pTemp!=NULL)
						{
							lpData->color = pTempObj->m_colorText;
							lpData->horz = ((CDrawRect*)pTempObj)->m_nHorzJust;
							lpData->vert = ((CDrawRect*)pTempObj)->m_nVertJust;
							lpData->type = ((CDrawRect*)pTempObj)->GetTextType();
							lpData->bShowNeg = ((CDrawRect*)pTempObj)->ShowNegative();
							lpData->precision = ((CDrawRect*)pTempObj)->GetPrecision();
						}
						else
						{
							lpData->color = pObj->m_colorText;
							lpData->horz = ((CDrawRect*)pObj)->m_nHorzJust;
							lpData->vert = ((CDrawRect*)pObj)->m_nVertJust;
							lpData->type = ((CDrawRect*)pObj)->GetTextType();
							lpData->bShowNeg = ((CDrawRect*)pObj)->ShowNegative();
							lpData->precision = ((CDrawRect*)pObj)->GetPrecision();
						}
						m_data.SetAt(hTI2, lpData);
						m_objects.SetAt(hTI2, pObj);
						if (m_pTemp!=NULL)
							m_updated.SetAt(hTI2, TRUE);
						else
							m_updated.SetAt(hTI2, FALSE);
						m_edits.SetAt(hTI2, pObj);
						bNorm = TRUE;
					}
				}
				if (bXCoord && bYCoord && bNorm)
					break;
			}
		}
		LPLOGFONT lpLogFont2 = new LOGFONT;
		*lpLogFont2 = *lpLogFont;
		m_logfonts.SetAt(hTS, lpLogFont2);
		DATA *lpData2 = new DATA;
		*lpData2 = *lpData;
		m_data.SetAt(hTS, lpData2);
		m_updated.SetAt(hTS, FALSE);
	}
	// select the first item and update controls accordingly
	hTI = m_tree.GetFirstVisibleItem();
	if (m_data.Lookup(hTI, lpData))
	{
		if (m_logfonts.Lookup(hTI, lpLogFont))
		{
			m_tree.SelectItem(hTI);
			m_horz = lpData->horz;
			m_vert = lpData->vert;
			m_orientation = (int)(lpLogFont->lfEscapement/10);
			m_spin.SetPos(m_orientation);
			if (FontIsTrueType(lpLogFont))
				GetDlgItem(IDC_EDIT1)->EnableWindow(TRUE);
			else
				GetDlgItem(IDC_EDIT1)->EnableWindow(FALSE);
		}
		if (lpData->type==CDrawRect::normal)
		{
			GetDlgItem(IDC_EDIT2)->EnableWindow(FALSE);
			GetDlgItem(IDC_SPIN2)->EnableWindow(FALSE);
		}
		else
		{
			GetDlgItem(IDC_EDIT2)->EnableWindow(TRUE);
			GetDlgItem(IDC_SPIN2)->EnableWindow(TRUE);
			m_precision = lpData->precision;
			m_spin2.SetPos(m_precision);
		}
	}
	UpdateData(FALSE);
}

void CTextPage::OnBeginlabeleditTree1(NMHDR* pNMHDR, LRESULT* pResult) 
{
	TV_DISPINFO* pTVDispInfo = (TV_DISPINFO*)pNMHDR;
	HTREEITEM hItem;
	CDrawObj *pObj;
	BOOL bUpdated;
	
	hItem = pTVDispInfo->item.hItem;
	*pResult = 1;
	if (hItem!=NULL)
	{
		if (m_edits.Lookup(hItem, pObj))
		{
			if (pObj->IsEditable() || pObj==m_pDoc->GetHeight())
			{
				CString str;
				
				if (m_updated.Lookup(hItem, bUpdated))
					m_updated.SetAt(hItem, TRUE);
				str.LoadString(IDS_EMPTY);
				if (m_tree.GetItemText(hItem)==str)
				{
					str.Empty();
					m_tree.GetEditControl()->SetWindowText(str);
				}
				m_pParent->m_nFormat = 1;
				SetModified(TRUE);
				*pResult = 0;
			}
		}
	}
}

void CTextPage::OnEndlabeleditTree1(NMHDR* pNMHDR, LRESULT* pResult) 
{
	TV_DISPINFO* pTVDispInfo = (TV_DISPINFO*)pNMHDR;
	CString str;
	HTREEITEM hItem;

	*pResult = 1;
	hItem = pTVDispInfo->item.hItem;
	m_tree.GetEditControl()->GetWindowText(str);
	if (str.IsEmpty())
	{
		str.LoadString(IDS_EMPTY);
		m_tree.SetItemText(hItem, str);
		*pResult = 0;
	}
}

void CTextPage::OnShrift() 
{
	HTREEITEM hTI;
	LPLOGFONT lpLogFont;
	DATA *lpData;
	BOOL bUpdated;
	CDrawObjListArray *tableKey1 = m_pDoc->GetTableKey1();
	CDrawObjListArray *tableKey2 = m_pDoc->GetTableKey2();
	CDrawObjListArray *table = m_pDoc->GetTable();
	LONG lfE, lfO;
	
	hTI = m_tree.GetSelectedItem();
	if (m_logfonts.Lookup(hTI, lpLogFont))
	{
		if (m_data.Lookup(hTI, lpData))
		{
			UpdateData();
			CFontDialog dlg(lpLogFont, CF_EFFECTS | CF_BOTH, NULL, this);
			dlg.m_cf.rgbColors = lpData->color;
			lfE = lpLogFont->lfEscapement;
			lfO = lpLogFont->lfOrientation;
	
			if (dlg.DoModal() != IDOK)
				return;

			*lpLogFont = dlg.m_lf;
			lpLogFont->lfEscapement = lfE;
			lpLogFont->lfOrientation = lfO;
			// set CLIP_LH_ANGLES to ensure the coordinate system for all devices is the same
			lpLogFont->lfClipPrecision = (BYTE)(lpLogFont->lfClipPrecision | CLIP_LH_ANGLES);
			if (FontIsTrueType(lpLogFont) && hTI!=m_hTITable)
				GetDlgItem(IDC_EDIT1)->EnableWindow(TRUE);
			else
				GetDlgItem(IDC_EDIT1)->EnableWindow(FALSE);
			if (m_updated.Lookup(hTI, bUpdated))
				m_updated.SetAt(hTI, TRUE);
			UpdateItemAndChildren(hTI, schrift, lpLogFont, dlg.m_cf.rgbColors);
			if (hTI==m_hTITable)
			{
				POSITION pos;
				CDrawObj *pObj;
				HANDLE hTI1;

				m_pParent->m_nFormat = 1;	// can't tell if we need to reformat or not!
				pos = m_items.GetHeadPosition();
				while (pos!=NULL)
				{
					hTI1 = m_items.GetNext(pos);
					if (m_objects.Lookup(hTI1, pObj))
					{
						if (pObj->IsText())
						{
							if (table->FindObjectIndex( pObj )!=-1 
								|| tableKey1->FindObjectIndex( pObj )!=-1
								|| tableKey2->FindObjectIndex( pObj )!=-1)
							{
								UpdateItemAndParent((HTREEITEM)hTI1, schrift, lpLogFont, dlg.m_cf.rgbColors);
							}
						}
					}
				}
			}
			SetModified(TRUE);
		}
	}
	UpdateData(FALSE);
}

void CTextPage::OnOK() 
{
	CDrawObj *pObj, *pDrawObj;
	POSITION pos1, pos2;
	int i;
	HTREEITEM hTI;
	LPLOGFONT lpLogFont;
	DATA *lpData;
	BOOL bUpdated;
	CString str;
	CDrawObjList *pObjList;		// for undo
	CDrawObjListArray *tableKey1 = m_pDoc->GetTableKey1();
	CDrawObjListArray *tableKey2 = m_pDoc->GetTableKey2();
	CDrawObjListArray *table = m_pDoc->GetTable();
	CDrawObjList *stempel = m_pDoc->GetStempel();
	CDrawObjList *rahmen = m_pDoc->GetRahmen();
	CDrawRect *titel = m_pDoc->GetTitle()->GetTitle();
	CDrawRect *height = m_pDoc->GetHeight();
  CDrawRect* comment = m_pDoc->GetComment();
	CDrawRect *ctitel, *ltitel;
	if (m_bTemplate)
	{
		ctitel = (CDrawRect*)((CTemplate*)m_pDoc)->GetTemplateObj(TMPL_CTITEL, DST_UNKNOWN);
		ltitel = (CDrawRect*)((CTemplate*)m_pDoc)->GetTemplateObj(TMPL_LTITEL, DST_UNKNOWN);
	}

	if (!UpdateData(TRUE))
		return;

	i = m_items.GetCount();
	if (m_pView!=NULL && m_pParent->m_bUndo)
	{
		i *= 2;
		str.LoadString(IDS_RENEWING_DRAWING);
	}
	else
		str.LoadString(IDS_RENEWING_TEMPLATE);
	GETPLOTTERAPP->CreateStatusBarProgress(str, 0, i);
	if (m_pView!=NULL && m_pParent->m_bUndo)
	{
		// add updated objects to undo buffer
		pObjList = new CDrawObjList;
		pos1 = m_items.GetHeadPosition();
		while (pos1!=NULL)
		{
			hTI = (HTREEITEM)m_items.GetNext(pos1);
			GETPLOTTERAPP->IncStatusBarProgress();
			if (m_objects.Lookup(hTI, pObj))
			{
				bUpdated = FALSE;
				m_updated.Lookup(hTI, bUpdated);
				if (bUpdated)
				{
					if (m_data.Lookup(hTI, lpData))
					{
						if (table->FindObjectIndex(pObj)!=-1)
						{
							int secIndex = pObj->GetSectionIndex();
							int dbIndex = pObj->GetDataBlockIndex();
							int type = pObj->GetType();
							
							for (i=0; i<table->GetSize(); i++)
							{
								pos2 = table->GetAt(i)->GetHeadPosition();
								while (pos2!=NULL)
								{
									pDrawObj = table->GetAt(i)->GetNextObject(pos2);
									if (pDrawObj->IsText() && ((CDrawRect*)pDrawObj)->GetTextType()==lpData->type
										&& pDrawObj->GetType()==type && pDrawObj->GetSectionIndex()==secIndex &&
										pDrawObj->GetDataBlockIndex()==dbIndex)
									{
										pObjList->AddTailObject(pDrawObj);
									}
								}
							}
						}
						else if (tableKey1->FindObjectIndex(pObj)!=-1)
						{
							int secIndex = pObj->GetSectionIndex();
							int dbIndex = pObj->GetDataBlockIndex();
							int type = pObj->GetType();
							
							for (i=0; i<tableKey1->GetSize(); i++)
							{
								pos2 = tableKey1->GetAt(i)->GetHeadPosition();
								while (pos2!=NULL)
								{
									pDrawObj = tableKey1->GetAt(i)->GetNextObject(pos2);
									if (pDrawObj->IsText() && ((CDrawRect*)pDrawObj)->GetTextType()==lpData->type
										&& pDrawObj->GetType()==type && pDrawObj->GetSectionIndex()==secIndex &&
										pDrawObj->GetDataBlockIndex()==dbIndex)
									{
										pObjList->AddTailObject(pDrawObj);
									}
								}
							}
						}
						else if (tableKey2->FindObjectIndex(pObj)!=-1)
						{
							int secIndex = pObj->GetSectionIndex();
							int dbIndex = pObj->GetDataBlockIndex();
							int type = pObj->GetType();
							
							for (i=0; i<tableKey2->GetSize(); i++)
							{
								pos2 = tableKey2->GetAt(i)->GetHeadPosition();
								while (pos2!=NULL)
								{
									pDrawObj = tableKey2->GetAt(i)->GetNextObject(pos2);
									if (pDrawObj->IsText() && ((CDrawRect*)pDrawObj)->GetTextType()==lpData->type
										&& pDrawObj->GetType()==type && pDrawObj->GetSectionIndex()==secIndex &&
										pDrawObj->GetDataBlockIndex()==dbIndex)
									{
										pObjList->AddTailObject(pDrawObj);
									}
								}
							}
						}
						else if( rahmen->FindObject( pObj )!=NULL)
						{
							pos2 = rahmen->GetHeadPosition();
							while (pos2!=NULL)
							{
								pDrawObj = rahmen->GetNextObject( pos2 );
								if (pDrawObj->IsText())
								{
									pObjList->AddTailObject(pDrawObj);
								}
							}
						}
						else if (pObj->IsText())
							pObjList->AddTailObject(pObj);
					}
				}
			}
		}
		if (pObjList->GetObjectCount()>0)
			m_pView->AddToUndoBuffer(pObjList, HINT_OBJ_EDIT);
		delete pObjList;
	}

	CString empty;
	empty.LoadString(IDS_EMPTY);

	pos1 = m_items.GetHeadPosition();
	while (pos1!=NULL)
	{
		hTI = (HTREEITEM)m_items.GetNext(pos1);
		GETPLOTTERAPP->IncStatusBarProgress();
		if (m_objects.Lookup(hTI, pObj))
		{
			bUpdated = FALSE;
			m_updated.Lookup(hTI, bUpdated);
			if (bUpdated)
			{
				m_updated.SetAt(hTI, FALSE);
				if( pObj->IsEditable() || pObj == height || pObj == comment )
				{
					str = m_tree.GetItemText(hTI);
					if( str == empty )
						str.Empty();
					if( pObj->IsText() )
					{
						if( pObj == height )
							m_pDoc->m_pPData->m_heightFormatText = str;
						else if( pObj == titel )
              m_pDoc->GetTitle()->SetFormatText( str );
            else if( pObj == comment )
              m_pDoc->m_pPData->m_commentFormatText = str;
            else if( ((CDrawRect*)pObj)->GetTextType() == CDrawRect::normal )
							((CDrawRect*)pObj)->SetText( str );
					}
				}
				if (m_logfonts.Lookup(hTI, lpLogFont))
				{
					if (m_data.Lookup(hTI, lpData))
					{
						if (m_bTemplate)
						{
							if (ctitel==pObj)
							{
								if (pObj->IsText())
								{
									pObj->m_logfont = *lpLogFont;
									pObj->m_colorText = lpData->color;
									((CDrawRect*)pObj)->SetHorzJust(lpData->horz);
									((CDrawRect*)pObj)->SetVertJust(lpData->vert);
									((CDrawRect*)pObj)->SetShowNegative(lpData->bShowNeg);
									((CDrawRect*)pObj)->SetPrecision(lpData->precision);
									if (m_pView!=NULL)
										pObj->Invalidate();
									m_pDoc->SetModifiedFlag();
								}
							}
							if (ltitel==pObj)
							{
								if (pObj->IsText())
								{
									pObj->m_logfont = *lpLogFont;
									pObj->m_colorText = lpData->color;
									((CDrawRect*)pObj)->SetHorzJust(lpData->horz);
									((CDrawRect*)pObj)->SetVertJust(lpData->vert);
									((CDrawRect*)pObj)->SetShowNegative(lpData->bShowNeg);
									((CDrawRect*)pObj)->SetPrecision(lpData->precision);
									if (m_pView!=NULL)
										pObj->Invalidate();
									m_pDoc->SetModifiedFlag();
								}
							}
						}
						else
						{
							if (titel==pObj)
							{
								if (pObj->IsText())
								{
									pObj->m_logfont = *lpLogFont;
									pObj->m_colorText = lpData->color;
									((CDrawRect*)pObj)->SetHorzJust(lpData->horz);
									((CDrawRect*)pObj)->SetVertJust(lpData->vert);
									((CDrawRect*)pObj)->SetShowNegative(lpData->bShowNeg);
									((CDrawRect*)pObj)->SetPrecision(lpData->precision);
									if (m_pView!=NULL)
										pObj->Invalidate();
									m_pDoc->SetModifiedFlag();
								}
							}
						}
						if (height==pObj)
						{
							if (pObj->IsText())
							{
								pObj->m_logfont = *lpLogFont;
								pObj->m_colorText = lpData->color;
								((CDrawRect*)pObj)->SetHorzJust(lpData->horz);
								((CDrawRect*)pObj)->SetVertJust(lpData->vert);
								((CDrawRect*)pObj)->SetShowNegative(lpData->bShowNeg);
								((CDrawRect*)pObj)->SetPrecision(lpData->precision);
								if (m_pView!=NULL)
									pObj->Invalidate();
								m_pDoc->SetModifiedFlag();
							}
						}
            else if( pObj == comment )
            {
              if( pObj->IsText() )
              {
								pObj->m_logfont = *lpLogFont;
								pObj->m_colorText = lpData->color;
								((CDrawRect*)pObj)->SetHorzJust(lpData->horz);
								((CDrawRect*)pObj)->SetVertJust(lpData->vert);
								((CDrawRect*)pObj)->SetShowNegative(lpData->bShowNeg);
								((CDrawRect*)pObj)->SetPrecision(lpData->precision);
								if (m_pView!=NULL)
									pObj->Invalidate();
								m_pDoc->SetModifiedFlag();
              }; // if pObj.IsText
            }
						else if (table->FindObjectIndex(pObj)!=-1)
						{
							int secIndex = pObj->GetSectionIndex();
							int dbIndex = pObj->GetDataBlockIndex();
							int type = pObj->GetType();

							for( i = 0; i < table->GetSize(); i++ )
							{
								pos2 = table->GetAt( i )->GetHeadPosition();
								while( pos2 != NULL )
								{
									pDrawObj = table->GetAt( i )->GetNextObject( pos2 );
									if( pDrawObj->IsText() && ((CDrawRect*)pDrawObj)->GetTextType()== lpData->type
										&& pDrawObj->GetType()==type && pDrawObj->GetSectionIndex()==secIndex &&
										pDrawObj->GetDataBlockIndex() == dbIndex )
									{
										pDrawObj->m_logfont = *lpLogFont;
										pDrawObj->m_colorText = lpData->color;

                    ((CDrawRect*)pDrawObj)->SetHorzJust(lpData->horz);
                    ((CDrawRect*)pDrawObj)->SetVertJust(lpData->vert);
										
										((CDrawRect*)pDrawObj)->SetPrecision(lpData->precision);
                    ((CDrawRect*)pDrawObj)->SetShowNegative(lpData->bShowNeg);

										if (m_pView!=NULL)
											pDrawObj->Invalidate();
										m_pDoc->SetModifiedFlag();
									}
								}
							}
						}
						else if (tableKey1->FindObjectIndex(pObj)!=-1)
						{
							int secIndex = pObj->GetSectionIndex();
							int dbIndex = pObj->GetDataBlockIndex();
							int type = pObj->GetType();

							for (i=0; i<tableKey1->GetSize(); i++)
							{
								pos2 = tableKey1->GetAt(i)->GetHeadPosition();
								while (pos2!=NULL)
								{
									pDrawObj = tableKey1->GetAt(i)->GetNextObject(pos2);
									if (pDrawObj->IsText() && ((CDrawRect*)pDrawObj)->GetTextType()==lpData->type
										&& pDrawObj->GetType()==type && pDrawObj->GetSectionIndex()==secIndex &&
										pDrawObj->GetDataBlockIndex()==dbIndex)
									{
										pDrawObj->m_logfont = *lpLogFont;
										pDrawObj->m_colorText = lpData->color;
										((CDrawRect*)pDrawObj)->SetHorzJust(lpData->horz);
										((CDrawRect*)pDrawObj)->SetVertJust(lpData->vert);
										((CDrawRect*)pDrawObj)->SetShowNegative(lpData->bShowNeg);
										((CDrawRect*)pDrawObj)->SetPrecision(lpData->precision);
										if (m_pView!=NULL)
											pDrawObj->Invalidate();
										m_pDoc->SetModifiedFlag();
									}
								}
							}
						}
						else if (tableKey2->FindObjectIndex(pObj)!=-1)
						{
							int secIndex = pObj->GetSectionIndex();
							int dbIndex = pObj->GetDataBlockIndex();
							int type = pObj->GetType();

							for (i=0; i<tableKey2->GetSize(); i++)
							{
								pos2 = tableKey2->GetAt(i)->GetHeadPosition();
								while (pos2!=NULL)
								{
									pDrawObj = tableKey2->GetAt(i)->GetNextObject(pos2);
									if (pDrawObj->IsText() && ((CDrawRect*)pDrawObj)->GetTextType()==lpData->type
										&& pDrawObj->GetType()==type && pDrawObj->GetSectionIndex()==secIndex &&
										pDrawObj->GetDataBlockIndex()==dbIndex)
									{
										pDrawObj->m_logfont = *lpLogFont;
										pDrawObj->m_colorText = lpData->color;
										((CDrawRect*)pDrawObj)->SetHorzJust(lpData->horz);
										((CDrawRect*)pDrawObj)->SetVertJust(lpData->vert);
										((CDrawRect*)pDrawObj)->SetShowNegative(lpData->bShowNeg);
										((CDrawRect*)pDrawObj)->SetPrecision(lpData->precision);
										if (m_pView!=NULL)
											pDrawObj->Invalidate();
										m_pDoc->SetModifiedFlag();
									}
								}
							}
						}
						else if( stempel->FindObject( pObj ) != NULL )
						{
							if (pObj->IsText())
							{
								pObj->m_logfont = *lpLogFont;
								pObj->m_colorText = lpData->color;
								((CDrawRect*)pObj)->SetHorzJust(lpData->horz);
								((CDrawRect*)pObj)->SetVertJust(lpData->vert);
								((CDrawRect*)pObj)->SetShowNegative(lpData->bShowNeg);
								((CDrawRect*)pObj)->SetPrecision(lpData->precision);
								if (m_pView!=NULL)
									pObj->Invalidate();
								m_pDoc->SetModifiedFlag();
							}
						}
						else if( rahmen->FindObject( pObj ) != NULL )
						{
							pos2 = rahmen->GetHeadPosition();
							while (pos2!=NULL)
							{
								pDrawObj = rahmen->GetNextObject(pos2);
								if (pDrawObj->IsText())
								{
									pDrawObj->m_logfont = *lpLogFont;
									pDrawObj->m_colorText = lpData->color;
									((CDrawRect*)pDrawObj)->SetHorzJust(lpData->horz);
									((CDrawRect*)pDrawObj)->SetVertJust(lpData->vert);
									((CDrawRect*)pDrawObj)->SetShowNegative(lpData->bShowNeg);
									((CDrawRect*)pDrawObj)->SetPrecision(lpData->precision);
									if (m_pView!=NULL)
										pDrawObj->Invalidate();
									m_pDoc->SetModifiedFlag();
								}
							}
						}
					}
				}
			}
		}
	}
	GETPLOTTERAPP->DestroyStatusBarProgress();

	CPropertyPage::OnOK();
}

BOOL CTextPage::OnApply() 
{
	CPropertyPage::OnApply();
	m_pParent->AttemptUpdateDrawing();
	return TRUE;
}

void CTextPage::UpdateItemAndChildren( HTREEITEM hTI, int format, LPLOGFONT lpLF /*=NULL*/, COLORREF color /*=0*/ ) 
{
  LPLOGFONT lpLogFont = NULL;
	if( m_logfonts.Lookup( hTI, lpLogFont ) )
	{
    DATA* lpData = NULL;
		if( m_data.Lookup( hTI, lpData ) )
		{
			if( format & schrift && lpLF != NULL )
			{
				LONG lfE = lpLogFont->lfEscapement;
				LONG lfO = lpLogFont->lfOrientation;
				*lpLogFont = *lpLF;
				lpLogFont->lfEscapement = lfE;
				lpLogFont->lfOrientation = lfO;
				lpData->color = color;
			}
			if( format & angle )
			{
				lpLogFont->lfEscapement = m_orientation * 10;
				lpLogFont->lfOrientation = m_orientation * 10;
			}

      CDrawObj* pObj = NULL;
			if( m_objects.Lookup( hTI, pObj ) )
			{
				if( m_pParent->m_nFormat != 1 && pObj->m_logfont != *lpLogFont )
					m_pParent->m_nFormat = 1;
			}
			if( format & horz )
				lpData->horz = m_horz;
			if( format & vert )
				lpData->vert = m_vert;
			if( format & noneg )
			{
        CDrawObj* pObj = NULL;
				if( m_objects.Lookup( hTI, pObj ) )
				{
					if( pObj->IsText() && ((CDrawRect*)pObj)->GetTextType()==CDrawRect::xcoord )
						lpData->bShowNeg = !m_nonegative;
				}
			}

			if( format & precision )
				lpData->precision = m_precision;

      BOOL bUpdated = FALSE;
			if( m_updated.Lookup( hTI, bUpdated ) )
				m_updated.SetAt( hTI, TRUE );

			if( m_tree.ItemHasChildren( hTI ) )
			{
        HTREEITEM hTI1 = m_tree.GetChildItem(hTI);
				while( hTI1 != NULL )
				{
					UpdateItemAndChildren( hTI1, format, lpLF, color );
          HTREEITEM hTI2 = m_tree.GetNextSiblingItem( hTI1 );
          hTI1 = hTI2;
				}
			}
		}
	}
}

void CTextPage::UpdateItemAndParent(HTREEITEM hTI, int format, LPLOGFONT lpLF/*=NULL*/, COLORREF color/*=0*/) 
{
	LPLOGFONT lpLogFont;
	DATA *lpData;
	HTREEITEM hTI1;
	BOOL bUpdated;
	LONG lfE, lfO;
	CDrawObj *pObj;

	if (m_logfonts.Lookup(hTI, lpLogFont))
	{
		if (m_data.Lookup(hTI, lpData))
		{
			if (format & schrift && lpLF!=NULL)
			{
				lfE = lpLogFont->lfEscapement;
				lfO = lpLogFont->lfOrientation;
				*lpLogFont = *lpLF;
				lpLogFont->lfEscapement = lfE;
				lpLogFont->lfOrientation = lfO;
				lpData->color = color;
			}
			if (format & angle)
			{
				lpLogFont->lfEscapement = m_orientation*10;
				lpLogFont->lfOrientation = m_orientation*10;
			}
			if (m_objects.Lookup(hTI, pObj))
			{
				if (m_pParent->m_nFormat!=1 && pObj->m_logfont!=*lpLogFont)
					m_pParent->m_nFormat =1;
			}
			if (format & horz)
				lpData->horz = m_horz;
			if (format & vert)
				lpData->vert = m_vert;
			if (format & noneg)
			{
				if (m_objects.Lookup(hTI, pObj))
				{
					if (pObj->IsText()/* && pObj->GetType()==DST_STATION*/ &&
						((CDrawRect*)pObj)->GetTextType()==CDrawRect::xcoord)
						lpData->bShowNeg = !m_nonegative;
				}
			}
			if (format & precision)
				lpData->precision = m_precision;
			if (m_updated.Lookup(hTI, bUpdated))
				m_updated.SetAt(hTI, TRUE);
			hTI1 = m_tree.GetParentItem(hTI);
			if (hTI1!=NULL)
				UpdateItemAndParent(hTI1, format, lpLF, color);
		}
	}
}

BOOL CTextPage::FontIsTrueType(LPLOGFONT lpLogFont)
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

void CTextPage::OnDeltaposSpin1(NMHDR* pNMHDR, LRESULT* pResult) 
{
	int newOrient;

	NM_UPDOWN* pNMUpDown = (NM_UPDOWN*)pNMHDR;
	newOrient = pNMUpDown->iDelta;
	newOrient += m_orientation;
	if (newOrient>=0 && newOrient<=359)
		m_orientation = newOrient;
	UpdateData(FALSE);
	OnChangeAngle();
	
	*pResult = 0;
}

void CTextPage::OnSelchangedTree1(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_TREEVIEW* pNMTreeView = (NM_TREEVIEW*)pNMHDR;
	HTREEITEM hTI = m_tree.GetSelectedItem();

  BOOL bShowEDIT1 = TRUE;
  BOOL bShowEDIT2 = TRUE;
  BOOL bShowSPIN1 = TRUE;
  BOOL bShowSPIN2 = TRUE;
  BOOL bShowCOMBO1 = TRUE;
  BOOL bShowCOMBO2 = TRUE;
  BOOL bShowCHECK1 = TRUE;
  BOOL bShowBUTTON1 = TRUE;

  LPLOGFONT lpLogFont = NULL;
	if( m_logfonts.Lookup( hTI, lpLogFont ) )
	{
    DATA *lpData = NULL;
		if( m_data.Lookup( hTI, lpData ) )
		{
			m_horz = lpData->horz;
			m_vert = lpData->vert;
			m_orientation = (int)(lpLogFont->lfEscapement/10);
			m_spin.SetPos(m_orientation);
			m_nonegative = !(lpData->bShowNeg);
      bShowEDIT1 = TRUE;
			bShowSPIN1 = TRUE;
			bShowCOMBO1 = TRUE;
			bShowCOMBO2 = TRUE;
			bShowCHECK1 = FALSE;
			bShowEDIT2 = FALSE;
			bShowSPIN2 = FALSE;
			if( !FontIsTrueType( lpLogFont ) )
			{
				bShowEDIT1 = FALSE;
				bShowSPIN1 = FALSE;
			}

      CDrawObj* pObj = NULL;
			if( hTI == m_hTITable )
			{
				bShowEDIT1 = FALSE;
				bShowSPIN1 = FALSE;
				bShowCOMBO1 = FALSE;
				bShowCOMBO2 = FALSE;
			}
			else if( m_objects.Lookup(hTI, pObj ) )
			{
				if( pObj->IsText() && ((CDrawRect*)pObj)->GetTextType() == CDrawRect::xcoord )
          bShowCHECK1 = TRUE;
            //GetDlgItem( IDC_CHECK1 )->ShowWindow( TRUE );
			}
			if (hTI==m_hTITable || lpData->type!=CDrawRect::normal)
			{
				bShowEDIT2 = TRUE;
				bShowSPIN2 = TRUE;
				m_precision = lpData->precision;
				m_spin2.SetPos(m_precision);
			}

      // kein Element des Stempels kann die Schirft ändern (wird im Stempeleditor festgelegt)
      // TODO: nur Template
      if( m_bTemplate == TRUE && 
        ( hTI == m_hTIStamp || m_tree.GetParentItem( hTI ) == m_hTIStamp || m_tree.GetParentItem( m_tree.GetParentItem( hTI ) ) == m_hTIStamp ) )
      {
        bShowEDIT1 = FALSE;
        bShowSPIN1 = FALSE;
        bShowCOMBO1 = FALSE;
        bShowCOMBO2 = FALSE;
        bShowBUTTON1 = FALSE;
        bShowCHECK1 = FALSE;
      }

		}
	}

  GetDlgItem( IDC_EDIT1 )->EnableWindow( bShowEDIT1 );
  GetDlgItem( IDC_EDIT2 )->EnableWindow( bShowEDIT2 );
  GetDlgItem( IDC_SPIN1 )->EnableWindow( bShowSPIN1 );
  GetDlgItem( IDC_SPIN2 )->EnableWindow( bShowSPIN2 );
  GetDlgItem( IDC_COMBO1 )->EnableWindow( bShowCOMBO1 );
  GetDlgItem( IDC_COMBO2 )->EnableWindow( bShowCOMBO2 );
  GetDlgItem( IDC_CHECK1 )->EnableWindow( bShowCHECK1 );
  GetDlgItem( IDC_BUTTON1 )->EnableWindow( bShowBUTTON1 );

	UpdateData( FALSE );
	
	*pResult = 0;
}

void CTextPage::OnSelchangeHoriz() 
{
	HTREEITEM hTI;
	
	hTI = m_tree.GetSelectedItem();
	UpdateData();
	UpdateItemAndChildren(hTI, horz);
	SetModified(TRUE);
}

void CTextPage::OnSelchangeVert() 
{
	HTREEITEM hTI;
	
	hTI = m_tree.GetSelectedItem();
	UpdateData();
	UpdateItemAndChildren(hTI, vert);
	SetModified(TRUE);
}

void CTextPage::OnChangeAngle() 
{
	HTREEITEM hTI;
	
	hTI = m_tree.GetSelectedItem();
	UpdateData();
	UpdateItemAndChildren(hTI, angle);
//	m_pParent->m_nFormat = 1;
	SetModified(TRUE);
}

void CTextPage::OnNoNegative() 
{
	HTREEITEM hTI;
	
	hTI = m_tree.GetSelectedItem();
	UpdateData();
	UpdateItemAndChildren(hTI, noneg);
	m_pParent->m_nFormat = 1;
	SetModified(TRUE);
}

void CTextPage::OnKillfocusAngle() 
{
	UpdateData();
	m_spin.SetPos(m_orientation);
}

void CTextPage::OnChangePrecision() 
{
	HTREEITEM hTI = m_tree.GetSelectedItem();
	UpdateData();
	UpdateItemAndChildren( hTI, precision );
	m_pParent->m_nFormat = 1;
	if( hTI == m_hTITable )
	{
		CDrawObjListArray *tableKey1 = m_pDoc->GetTableKey1();
		CDrawObjListArray *tableKey2 = m_pDoc->GetTableKey2();
		CDrawObjListArray *table = m_pDoc->GetTable();
		
    POSITION pos = m_items.GetHeadPosition();
		while( pos )
		{
      HANDLE hTI1 = m_items.GetNext(pos);

      CDrawObj* pObj = NULL;
			if( m_objects.Lookup( hTI1, pObj ) )
			{
				if (pObj->IsText())
				{
					if (table->FindObjectIndex(pObj)!=-1 
						|| tableKey1->FindObjectIndex(pObj)!=-1
						|| tableKey2->FindObjectIndex(pObj)!=-1)
					{
						UpdateItemAndParent((HTREEITEM)hTI1, precision);
					}
				}
			}
		}
	}
	SetModified(TRUE);
}

void CTextPage::OnKillfocusPrecision() 
{
	UpdateData();
	m_spin2.SetPos(m_precision);
}

void CTextPage::OnDeltaposSpin2(NMHDR* pNMHDR, LRESULT* pResult) 
{
	int newPrecision;

	NM_UPDOWN* pNMUpDown = (NM_UPDOWN*)pNMHDR;
	newPrecision = pNMUpDown->iDelta;
	newPrecision += m_precision;
	if (newPrecision>=0 && newPrecision<=10)
		m_precision = newPrecision;
	UpdateData(FALSE);
	OnChangePrecision();
	
	*pResult = 0;
}
