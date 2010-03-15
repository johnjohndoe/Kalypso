// editvw.cpp : implementation of the CEditorView class
//

#include "stdafx.h"

#include "global.h"
#include <tchar.h>

LOGFONT NEAR CEditorView::m_lfDefPrintFont; //test


extern CLIPFORMAT cfEmbeddedObject;
extern CLIPFORMAT cfRTO;

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

BOOL CCharFormat::operator==(CCharFormat& cf)
{
	return 
	dwMask == cf.dwMask
	&& dwEffects == cf.dwEffects
	&& yHeight == cf.yHeight
	&& yOffset == cf.yOffset
	&& crTextColor == cf.crTextColor
	&& bPitchAndFamily == cf.bPitchAndFamily
	&& (lstrcmpA(szFaceName, cf.szFaceName) == 0);
}

BOOL CParaFormat::operator==(PARAFORMAT& pf)
{
	if(
		dwMask != pf.dwMask
		|| wNumbering != pf.wNumbering
		|| wReserved != pf.wReserved
		|| dxStartIndent != pf.dxStartIndent
		|| dxRightIndent != pf.dxRightIndent
		|| dxOffset != pf.dxOffset
		|| cTabCount != pf.cTabCount
		)
	{
		return FALSE;
	}
	for (int i=0;i<pf.cTabCount;i++)
	{
		if (rgxTabs[i] != pf.rgxTabs[i])
			return FALSE;
	}
	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// CEditorView

IMPLEMENT_DYNCREATE(CEditorView, CRichEditView)

//WM_WININICHANGE -- default printer might have changed
//WM_FONTCHANGE -- pool of fonts changed
//WM_DEVMODECHANGE -- printer settings changes

BEGIN_MESSAGE_MAP(CEditorView, CRichEditView)
	ON_COMMAND(ID_OLE_INSERT_NEW, OnInsertObject)
	ON_COMMAND(ID_CANCEL_EDIT_CNTR, OnCancelEditCntr)
	//{{AFX_MSG_MAP(CEditorView)
	ON_WM_CREATE()
	ON_COMMAND(ID_PAGE_SETUP, OnPageSetup)
	ON_COMMAND(ID_FORMAT_PARAGRAPH, OnFormatParagraph)
	ON_WM_TIMER()
	ON_WM_DESTROY()
	ON_WM_MEASUREITEM()
	ON_WM_KEYDOWN()
	ON_COMMAND(ID_FILE_PRINT, OnFilePrint)
	ON_WM_PALETTECHANGED()
	ON_WM_QUERYNEWPALETTE()
	ON_WM_SIZE()
	ON_COMMAND(ID_FILE_PRINT_PREVIEW, OnFilePrintPreview)
	ON_COMMAND(ID_FORMAT_PAGEBREAK, OnFormatPagebreak)
	ON_CONTROL_REFLECT(EN_SETFOCUS, OnSetfocus)
	ON_CONTROL_REFLECT(EN_KILLFOCUS, OnKillfocus)
	ON_COMMAND(ID_FILE_PRINT_DIRECT, OnFilePrint)
	ON_WM_DROPFILES()
	ON_COMMAND(ID_FORMAT_FONT, OnFormatFont)
	//}}AFX_MSG_MAP
	// Standard printing commands
//	ON_COMMAND(ID_FILE_PRINT_PREVIEW, CRichEditView::OnFilePrintPreview)
	ON_WM_WININICHANGE()
	ON_EN_CHANGE(AFX_IDW_PANE_FIRST, OnEditChange)
	ON_WM_MOUSEACTIVATE()
	ON_REGISTERED_MESSAGE(CEditorApp::m_nPrinterChangedMsg, OnPrinterChangedMsg)
	ON_UPDATE_COMMAND_UI(ID_INDICATOR_LINE, OnUpdateIndicatorLine)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CEditorView construction/destruction

CEditorView::CEditorView()
{
	m_bSyncCharFormat = m_bSyncParaFormat = TRUE;
	m_uTimerID = 0;
	m_bDelayUpdateItems = FALSE;
	m_bInPrint = FALSE;
	m_nPasteType = 0;
	m_rectMargin = theApp.m_rectPageMargin;
//	m_nWordWrap = WrapToTargetDevice;
	m_nWordWrap = WrapNone;
    m_hPrinterFont = NULL;
    m_hMirrorFont = NULL;
	
}

BOOL CEditorView::PreCreateWindow(CREATESTRUCT& cs) 
{
	BOOL bRes = CRichEditView::PreCreateWindow(cs);
	cs.style |= ES_SELECTIONBAR;
	return bRes;
}			

/////////////////////////////////////////////////////////////////////////////
// CEditorView attributes

BOOL CEditorView::IsFormatText()
{
	// this function checks to see if any formatting is not default text
	BOOL bRes = FALSE;
	CHARRANGE cr;
	CCharFormat cf;
	CParaFormat pf;
	GetRichEditCtrl().GetSel(cr);
	GetRichEditCtrl().HideSelection(TRUE, FALSE);
	GetRichEditCtrl().SetSel(0,-1);

	if (!(GetRichEditCtrl().GetSelectionType() & (SEL_OBJECT|SEL_MULTIOBJECT)))
	{
		GetRichEditCtrl().GetSelectionCharFormat(cf);
		if (cf == m_defTextCharFormat)
		{
			GetRichEditCtrl().GetParaFormat(pf);
			if (pf == m_defParaFormat) //compared using CParaFormat::operator==
				bRes = TRUE;
		}
	}

	GetRichEditCtrl().SetSel(cr);
	GetRichEditCtrl().HideSelection(FALSE, FALSE);
	return bRes;
}

HMENU CEditorView::GetContextMenu(WORD, LPOLEOBJECT, CHARRANGE* )
{
	CRichEditCntrItem* pItem = GetSelectedItem();
	if (pItem == NULL || !pItem->IsInPlaceActive())
	{
		CMenu menuText;
		menuText.LoadMenu(IDR_TEXT_POPUP);
		CMenu* pMenuPopup = menuText.GetSubMenu(0);
		menuText.RemoveMenu(0, MF_BYPOSITION);
		return pMenuPopup->Detach();
	}
	return NULL;
}

/////////////////////////////////////////////////////////////////////////////
// CEditorView operations

void CEditorView::WrapChanged()
{
	CWaitCursor wait;
	CFrameWnd* pFrameWnd = GetParentFrame();
	ASSERT(pFrameWnd != NULL);
	pFrameWnd->SetMessageText(IDS_FORMATTING);
	CWnd* pBarWnd = pFrameWnd->GetMessageBar();
	if (pBarWnd != NULL)
		pBarWnd->UpdateWindow();

	CRichEditView::WrapChanged();

	pFrameWnd->SetMessageText(AFX_IDS_IDLEMESSAGE);
	if (pBarWnd != NULL)
		pBarWnd->UpdateWindow();
}

void CEditorView::SetUpdateTimer()
{
	if (m_uTimerID != 0) // if outstanding timer kill it
		KillTimer(m_uTimerID);
	m_uTimerID = SetTimer(1, 1000, NULL); //set a timer for 1000 milliseconds
	if (m_uTimerID == 0) // no timer available so force update now
		GetDocument()->UpdateAllItems(NULL);
	else
		m_bDelayUpdateItems = TRUE;
}

void CEditorView::DeleteContents()
{
	ASSERT_VALID(this);
	ASSERT(m_hWnd != NULL);
	CRichEditView::DeleteContents();
	SetDefaultFont(IsTextType(GetDocument()->m_nNewDocType));
}

void CEditorView::SetDefaultFont(BOOL bText)
{
	ASSERT_VALID(this);
	ASSERT(m_hWnd != NULL);
	m_bSyncCharFormat = m_bSyncParaFormat = TRUE;
	CHARFORMAT* pCharFormat = bText ? &m_defTextCharFormat : &m_defCharFormat;
	// set the default character format -- the FALSE makes it the default
    /*test //LOGFONT m_lfDefPrintFont;
    m_lfDefPrintFont.lfHeight=(pCharFormat->dwMask&CFM_SIZE) ? pCharFormat->yHeight-20 : 0;
    m_lfDefPrintFont.lfWeight=(pCharFormat->dwMask&CFM_BOLD && pCharFormat->dwEffects&CFE_BOLD) ?FW_BOLD : 0;
    m_lfDefPrintFont.lfItalic=(pCharFormat->dwMask&CFM_ITALIC && pCharFormat->dwEffects&CFE_ITALIC);
    m_lfDefPrintFont.lfUnderline=(pCharFormat->dwMask&CFM_UNDERLINE && pCharFormat->dwEffects&CFE_UNDERLINE);
    m_lfDefPrintFont.lfPitchAndFamily=pCharFormat->bPitchAndFamily;
    m_lfDefPrintFont.lfCharSet=pCharFormat->bCharSet;
    strcpy(m_lfDefPrintFont.lfFaceName,pCharFormat->szFaceName);
    
    //CFont defPrinterFont;
    defPrinterFont.CreateFontIndirect(&m_lfDefPrintFont);
    SetPrinterFont(&defPrinterFont);
     */
	GetRichEditCtrl().SetSel(0,-1);
	GetRichEditCtrl().SetDefaultCharFormat(*pCharFormat);
	GetRichEditCtrl().SetSelectionCharFormat(*pCharFormat);

	GetRichEditCtrl().SetParaFormat(m_defParaFormat);

	GetRichEditCtrl().SetSel(0,0);
	GetRichEditCtrl().EmptyUndoBuffer();
	GetRichEditCtrl().SetModify(FALSE);
	ASSERT_VALID(this);
}

/////////////////////////////////////////////////////////////////////////////
// CEditorView drawing

/////////////////////////////////////////////////////////////////////////////
// CEditorView printing

void CEditorView::OnPrint(CDC* pDC, CPrintInfo* pInfo) 
{
	ASSERT_VALID(this);
	ASSERT_VALID(pDC);
	ASSERT(pInfo != NULL);
	ASSERT(pInfo->m_bContinuePrinting);

	UINT nPage = pInfo->m_nCurPage;
	ASSERT(nPage <= (UINT)m_aPageStart.GetSize());
	long nIndex = (long) m_aPageStart[nPage-1];
	LONG nEnd = 0xFFFFFFFF;
	CString line;
	long nLineIndex, nCharIndex;
    
    //CFont* pOldFont = NULL;
	

	nLineIndex = GetRichEditCtrl().LineFromChar(nIndex);
	GetRichEditCtrl().GetLine(nLineIndex, line.GetBuffer(MAX_PATH), MAX_PATH);
	line.ReleaseBuffer();
	nCharIndex = GetRichEditCtrl().LineIndex(nLineIndex);
	if (line.GetAt(nIndex-nCharIndex)==0x0c)
		nIndex++;
	for (int i=nLineIndex; i<GetRichEditCtrl().GetLineCount(); i++)
	{
		int n = GetRichEditCtrl().LineIndex(i);
		if (n>nIndex)
		{
			GetRichEditCtrl().GetLine(i, line.GetBuffer(MAX_PATH), MAX_PATH);
			line.ReleaseBuffer();
			nCharIndex = line.Find(0x0c);
			if (nCharIndex!=-1)
			{
				nEnd = n + nCharIndex;
				break;
			}
		}
	}
/*	for (int i=0; i<m_pageBreaks.GetSize(); i++)
	{
		if (m_pageBreaks[i]>nIndex)
		{
			nEnd = m_pageBreaks[i];
			break;
		}
	}*/

	// print as much as possible in the current page.
	nIndex = PrintPage(pDC, nIndex, nEnd);

	if (nIndex >= GetTextLength())
	{
		TRACE0("End of Document\n");
		pInfo->SetMaxPage(nPage);
	}

	// update pagination information for page just printed
	if (nPage == (UINT)m_aPageStart.GetSize())
	{
		if (nIndex < GetTextLength())
			m_aPageStart.Add(nIndex);
	}
	else
	{
		ASSERT(nPage+1 <= (UINT)m_aPageStart.GetSize());
		ASSERT(nIndex == (long)m_aPageStart[nPage+1-1]);
	}
	//if (pInfo != NULL && pInfo->m_bPreview)
	//	DrawMargins(pDC);
    //Test
    //if (m_hPrinterFont != NULL)
	//	pOldFont = pDC->SelectObject(CFont::FromHandle(m_hPrinterFont));	
    //TEXTMETRIC txtmet;
    //pDC->GetTextMetrics(&txtmet);
    //End Test
    
}

void CEditorView::DrawMargins(CDC* pDC)
{
	if (pDC->m_hAttribDC != NULL)
	{
		CRect rect;
		rect.left = m_rectMargin.left;
		rect.right = m_sizePaper.cx - m_rectMargin.right;
		rect.top = m_rectMargin.top;
		rect.bottom = m_sizePaper.cy - m_rectMargin.bottom;
		//rect in twips
		int logx = ::GetDeviceCaps(pDC->m_hDC, LOGPIXELSX);
		int logy = ::GetDeviceCaps(pDC->m_hDC, LOGPIXELSY);
		rect.left = MulDiv(rect.left, logx, 1440);
		rect.right = MulDiv(rect.right, logx, 1440);
		rect.top = MulDiv(rect.top, logy, 1440);
		rect.bottom = MulDiv(rect.bottom, logy, 1440);
		CPen pen(PS_DOT, 0, pDC->GetTextColor());
		CPen* ppen = pDC->SelectObject(&pen);
		pDC->MoveTo(0, rect.top);
		pDC->LineTo(10000, rect.top);
		pDC->MoveTo(rect.left, 0);
		pDC->LineTo(rect.left, 10000);
		pDC->MoveTo(0, rect.bottom);
		pDC->LineTo(10000, rect.bottom);
		pDC->MoveTo(rect.right, 0);
		pDC->LineTo(rect.right, 10000);
		pDC->SelectObject(ppen);
	}
}

BOOL CEditorView::OnPreparePrinting(CPrintInfo* pInfo)
{
	return DoPreparePrinting(pInfo);
}

/////////////////////////////////////////////////////////////////////////////
// OLE Client support and commands

inline int roundleast(int n)
{
	int mod = n%10;
	n -= mod;
	if (mod >= 5)
		n += 10;
	else if (mod <= -5)
		n -= 10;
	return n;
}

static void RoundRect(LPRECT r1)
{
	r1->left = roundleast(r1->left);
	r1->right = roundleast(r1->right);
	r1->top = roundleast(r1->top);
	r1->bottom = roundleast(r1->bottom);
}

static void MulDivRect(LPRECT r1, LPRECT r2, int num, int div)
{
	r1->left = MulDiv(r2->left, num, div);
	r1->top = MulDiv(r2->top, num, div);
	r1->right = MulDiv(r2->right, num, div);
	r1->bottom = MulDiv(r2->bottom, num, div);
}

void CEditorView::OnPageSetup()
{
	CEditorDoc *pDoc = GetDocument();
	CPageSetupDialog dlg;
	PAGESETUPDLG& psd = dlg.m_psd;
	BOOL bMetric = theApp.GetUnits() == 1; //centimeters
	psd.Flags |= PSD_MARGINS | (bMetric ? PSD_INHUNDREDTHSOFMILLIMETERS : 
		    PSD_INTHOUSANDTHSOFINCHES);
	int nUnitsPerInch = bMetric ? 2540 : 1000;
	MulDivRect(&psd.rtMargin, m_rectMargin, nUnitsPerInch, 1440);
	RoundRect(&psd.rtMargin);
	// get the current device from the app
	theApp.SetPrinterDeviceSettings(pDoc->m_dmOrientation, pDoc->m_dmPaperSize,
		pDoc->m_dmPaperWidth, pDoc->m_dmPaperLength);
	PRINTDLG pd;
	pd.hDevNames = NULL;
	pd.hDevMode = NULL;
	theApp.GetPrinterDeviceDefaults(&pd);
	psd.hDevNames = pd.hDevNames;
	psd.hDevMode = pd.hDevMode;
	if (dlg.DoModal() == IDOK)
	{
		RoundRect(&psd.rtMargin);
		MulDivRect(m_rectMargin, &psd.rtMargin, 1440, nUnitsPerInch);
		theApp.m_rectPageMargin = m_rectMargin;
		theApp.SelectPrinter(psd.hDevNames, psd.hDevMode);
		theApp.NotifyPrinterChanged(TRUE);
        theApp.GetPrinterDeviceSettings(pDoc->m_dmOrientation, pDoc->m_dmPaperSize,//Dick 21.01.00 sonst pDoc ist nicht aktuell
            pDoc->m_dmPaperWidth, pDoc->m_dmPaperLength);        
	}
	// PageSetupDlg failed
	if (CommDlgExtendedError() != 0)
	{
		CPageSetupDlg dlg;
		dlg.m_nBottomMargin = m_rectMargin.bottom;
		dlg.m_nLeftMargin = m_rectMargin.left;
		dlg.m_nRightMargin = m_rectMargin.right;
		dlg.m_nTopMargin = m_rectMargin.top;
		if (dlg.DoModal() == IDOK)
		{
			m_rectMargin.SetRect(dlg.m_nLeftMargin, dlg.m_nTopMargin, 
				dlg.m_nRightMargin, dlg.m_nBottomMargin);
			// m_page will be changed at this point
			theApp.m_rectPageMargin = m_rectMargin;
			theApp.SelectPrinter(psd.hDevNames, psd.hDevMode);
			theApp.GetPrinterDeviceSettings(pDoc->m_dmOrientation, pDoc->m_dmPaperSize,
				pDoc->m_dmPaperWidth, pDoc->m_dmPaperLength);
			theApp.NotifyPrinterChanged();
		}
	}
}

/////////////////////////////////////////////////////////////////////////////
// CEditorView diagnostics

#ifdef _DEBUG
void CEditorView::AssertValid() const
{
	CRichEditView::AssertValid();
}

void CEditorView::Dump(CDumpContext& dc) const
{
	CRichEditView::Dump(dc);
}

CEditorDoc* CEditorView::GetDocument() // non-debug version is inline
{
	return (CEditorDoc*)m_pDocument;
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CEditorView message helpers

/////////////////////////////////////////////////////////////////////////////
// CEditorView message handlers

int CEditorView::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	if (CRichEditView::OnCreate(lpCreateStruct) == -1)
		return -1;
	theApp.m_listPrinterNotify.AddTail(m_hWnd);
	CDC dc;
	AfxGetApp()->CreatePrinterDC(dc);
	OnPrinterChanged(dc);

	if (theApp.m_bWordSel)
		GetRichEditCtrl().SetOptions(ECOOP_OR, ECO_AUTOWORDSELECTION);
	else
		GetRichEditCtrl().SetOptions(ECOOP_AND, ~(DWORD)ECO_AUTOWORDSELECTION);
//	GetRichEditCtrl().SetOptions(ECOOP_OR, ECO_SELECTIONBAR);

	GetDefaultFont(m_defTextCharFormat, IDS_DEFAULTTEXTFONT);
	GetDefaultFont(m_defCharFormat, IDS_DEFAULTFONT);

    
		
	GetRichEditCtrl().GetParaFormat(m_defParaFormat);
	m_defParaFormat.cTabCount = 0;

	return 0;
}

void CEditorView::GetDefaultFont(CCharFormat& cf, UINT nFontNameID)
{
	USES_CONVERSION;
	CString strDefFont;
	VERIFY(strDefFont.LoadString(nFontNameID));
	ASSERT(cf.cbSize == sizeof(CHARFORMAT));
	cf.dwMask = CFM_BOLD|CFM_ITALIC|CFM_UNDERLINE|CFM_STRIKEOUT|CFM_SIZE|
		CFM_COLOR|CFM_OFFSET|CFM_PROTECTED;
	cf.dwEffects = CFE_AUTOCOLOR;
	cf.yHeight = 160; // 8pt (6pt = 120)
	cf.yOffset = 0;
	cf.crTextColor = RGB(0, 0, 0);
	cf.bCharSet = 0;
	cf.bPitchAndFamily = DEFAULT_PITCH | FF_DONTCARE;
	ASSERT(strDefFont.GetLength() < LF_FACESIZE);
	lstrcpynA(cf.szFaceName, T2A(strDefFont.GetBuffer(strDefFont.GetLength())), LF_FACESIZE);
	strDefFont.ReleaseBuffer();
	cf.dwMask |= CFM_FACE;
}

void CEditorView::OnFormatParagraph() 
{
	CFormatParaDlg dlg(GetParaFormatSelection());
	dlg.m_nWordWrap = m_nWordWrap;
	if (dlg.DoModal() == IDOK)
		SetParaFormat(dlg.m_pf);
}

void CEditorView::OnTextNotFound(LPCTSTR lpStr)
{
	ASSERT_VALID(this);
	MessageBeep(0);
	AfxMessageBox(IDS_FINISHED_SEARCH,MB_OK|MB_ICONINFORMATION);
	CRichEditView::OnTextNotFound(lpStr);
}

void CEditorView::OnTimer(UINT nIDEvent) 
{
	if (m_uTimerID != nIDEvent) // not our timer
		CRichEditView::OnTimer(nIDEvent);
	else
	{
		KillTimer(m_uTimerID); // kill one-shot timer
		m_uTimerID = 0;
		if (m_bDelayUpdateItems)
			GetDocument()->UpdateAllItems(NULL);
		m_bDelayUpdateItems = FALSE;
	}	
}

void CEditorView::OnEditChange()
{
	SetUpdateTimer();
}

void CEditorView::OnDestroy() 
{
	POSITION pos = theApp.m_listPrinterNotify.Find(m_hWnd);
	ASSERT(pos != NULL);
	theApp.m_listPrinterNotify.RemoveAt(pos);

	CRichEditView::OnDestroy();
	
	if (m_uTimerID != 0) // if outstanding timer kill it
		OnTimer(m_uTimerID);
	ASSERT(m_uTimerID == 0);
}

void CEditorView::CalcWindowRect(LPRECT lpClientRect, UINT nAdjustType) 
{
	int nOldWidth = lpClientRect->right - lpClientRect->left;
	CRichEditView::CalcWindowRect(lpClientRect, nAdjustType);

	if (theApp.m_bWin4 && nAdjustType != 0 && (GetStyle() & WS_VSCROLL))
		lpClientRect->right--;
}

void CEditorView::OnMeasureItem(int nIDCtl, LPMEASUREITEMSTRUCT lpMIS) 
{
	lpMIS->itemID = (UINT)(WORD)lpMIS->itemID;
	CRichEditView::OnMeasureItem(nIDCtl, lpMIS);
}

void CEditorView::OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags) 
{
	if (nChar == VK_F10 && GetKeyState(VK_SHIFT) < 0)
	{
		long nStart, nEnd;
		GetRichEditCtrl().GetSel(nStart, nEnd);
		CPoint pt = GetRichEditCtrl().GetCharPos(nEnd);
		SendMessage(WM_CONTEXTMENU, (WPARAM)m_hWnd, MAKELPARAM(pt.x, pt.y));
	}
		
	CRichEditView::OnKeyDown(nChar, nRepCnt, nFlags);
}

HRESULT CEditorView::GetClipboardData(CHARRANGE* lpchrg, DWORD /*reco*/, 
	LPDATAOBJECT lpRichDataObj,	LPDATAOBJECT* lplpdataobj)
{
	CHARRANGE& cr = *lpchrg;

	if ((cr.cpMax - cr.cpMin == 1) && 
		GetRichEditCtrl().GetSelectionType() == SEL_OBJECT)
	{
		return E_NOTIMPL;
	}

	BeginWaitCursor();
	//create the data source
	COleDataSource* pDataSource = new COleDataSource;

	// put the formats into the data source
	LPENUMFORMATETC lpEnumFormatEtc;
	lpRichDataObj->EnumFormatEtc(DATADIR_SET, &lpEnumFormatEtc);
	if (lpEnumFormatEtc != NULL)
	{
		FORMATETC etc;
		while (lpEnumFormatEtc->Next(1, &etc, NULL) == S_OK) 
		{
			STGMEDIUM stgMedium;
			lpRichDataObj->GetData(&etc, &stgMedium);
			pDataSource->CacheData(etc.cfFormat, &stgMedium, &etc);
		}
		lpEnumFormatEtc->Release();
	}

	// get the IDataObject from the data source
	*lplpdataobj = 	(LPDATAOBJECT)pDataSource->GetInterface(&IID_IDataObject);

	EndWaitCursor();
	return S_OK;
}

HRESULT CEditorView::QueryAcceptData(LPDATAOBJECT lpdataobj,
	CLIPFORMAT* lpcfFormat, DWORD reco, BOOL bReally, 
	HGLOBAL hMetaPict)
{
	if (bReally && *lpcfFormat == 0 && (m_nPasteType == 0))
	{
		COleDataObject dataobj;
		dataobj.Attach(lpdataobj, FALSE);
		if (!dataobj.IsDataAvailable(cfRTO)) // native avail, let richedit do as it wants
		{
			if (dataobj.IsDataAvailable(cfEmbeddedObject))
			{
				if (PasteNative(lpdataobj))
					return S_FALSE;
			}
		}
	}
	return CRichEditView::QueryAcceptData(lpdataobj, lpcfFormat, reco, bReally,
		hMetaPict);
}

BOOL CEditorView::PasteNative(LPDATAOBJECT lpdataobj)
{
	// check data object for wordpad object
	// if true, suck out RTF directly
	FORMATETC etc = {NULL, NULL, DVASPECT_CONTENT, -1, TYMED_ISTORAGE};
	etc.cfFormat = (CLIPFORMAT)cfEmbeddedObject;
	STGMEDIUM stgMedium = {TYMED_ISTORAGE, 0, NULL};

	// create an IStorage to transfer the data in
	LPLOCKBYTES lpLockBytes;
	if (FAILED(::CreateILockBytesOnHGlobal(NULL, TRUE, &lpLockBytes)))
		return FALSE;
	ASSERT(lpLockBytes != NULL);

	HRESULT hr = ::StgCreateDocfileOnILockBytes(lpLockBytes,
		STGM_SHARE_EXCLUSIVE|STGM_CREATE|STGM_READWRITE, 0, &stgMedium.pstg);
	lpLockBytes->Release(); //storage addref'd
	if (FAILED(hr))
		return FALSE;

	ASSERT(stgMedium.pstg != NULL);
	CLSID clsid;
	BOOL bRes = FALSE; //let richedit do what it wants
	if (SUCCEEDED(lpdataobj->GetDataHere(&etc, &stgMedium)) &&
		SUCCEEDED(ReadClassStg(stgMedium.pstg, &clsid)) &&
		clsid == GetDocument()->GetClassID())
	{
		//suck out RTF now
		// open Contents stream
		COleStreamFile file;
		CFileException fe;
		if (file.OpenStream(stgMedium.pstg, szContents,
			CFile::modeReadWrite|CFile::shareExclusive, &fe))
		{

			// load it with CArchive (loads from Contents stream)
			CArchive loadArchive(&file, CArchive::load | 
				CArchive::bNoFlushOnDelete);
			Stream(loadArchive, TRUE); //stream in selection
			hr = TRUE; // don't let richedit do anything
		}
	}
	::ReleaseStgMedium(&stgMedium);
	return bRes;
}

// things to fix
// if format==0 we are doing a straight EM_PASTE
// 	look for native formats
//		richedit specific -- allow richedit to handle (these will be first)
// 		look for RTF, CF_TEXT.  If there paste special as these
// 	Do standard OLE scenario

// if pasting a particular format (format != 0)
//	if richedit specific, allow through
//	if RTF, CF_TEXT. paste special
//	if OLE format, do standard OLE scenario


void CEditorView::OnFilePrint() 
{
	CEditorDoc *pDoc = GetDocument();
	
	// don't allow winini changes to occur while printing
	m_bInPrint = TRUE;
	theApp.SetPrinterDeviceSettings(pDoc->m_dmOrientation, pDoc->m_dmPaperSize,
		pDoc->m_dmPaperWidth, pDoc->m_dmPaperLength);
	CRichEditView::OnFilePrint();
	// printer may have changed
	theApp.NotifyPrinterChanged(); // this will cause a GetDocument()->PrinterChanged();
	m_bInPrint = FALSE;
}

int CEditorView::OnMouseActivate(CWnd* pWnd, UINT nHitTest, UINT message)
{
	return CRichEditView::OnMouseActivate(pWnd, nHitTest, message);
}

LONG CEditorView::OnPrinterChangedMsg(UINT, LONG)
{
	CDC dc;
	AfxGetApp()->CreatePrinterDC(dc);
	OnPrinterChanged(dc);
	return 0;
}

static void ForwardPaletteChanged(HWND hWndParent, HWND hWndFocus)
{
	// this is a quick and dirty hack to send the WM_QUERYNEWPALETTE to a window that is interested
	HWND hWnd = NULL;
	for (hWnd = ::GetWindow(hWndParent, GW_CHILD); hWnd != NULL; hWnd = ::GetWindow(hWnd, GW_HWNDNEXT))
	{
		if (hWnd != hWndFocus)
		{
			::SendMessage(hWnd, WM_PALETTECHANGED, (WPARAM)hWndFocus, 0L);
			ForwardPaletteChanged(hWnd, hWndFocus);
		}
	}
}

void CEditorView::OnPaletteChanged(CWnd* pFocusWnd) 
{
	ForwardPaletteChanged(m_hWnd, pFocusWnd->GetSafeHwnd());
	// allow the richedit control to realize its palette
	// remove this if if richedit fixes their code so that
	// they don't realize their palette into foreground
	if (::GetWindow(m_hWnd, GW_CHILD) == NULL)
		CRichEditView::OnPaletteChanged(pFocusWnd);
}

static BOOL FindQueryPalette(HWND hWndParent)
{
	// this is a quick and dirty hack to send the WM_QUERYNEWPALETTE to a window that is interested
	HWND hWnd = NULL;
	for (hWnd = ::GetWindow(hWndParent, GW_CHILD); hWnd != NULL; hWnd = ::GetWindow(hWnd, GW_HWNDNEXT))
	{
		if (::SendMessage(hWnd, WM_QUERYNEWPALETTE, 0, 0L))
			return TRUE;
		else if (FindQueryPalette(hWnd))
			return TRUE;
	}
	return FALSE;
}

BOOL CEditorView::OnQueryNewPalette() 
{
	if(FindQueryPalette(m_hWnd))
		return TRUE;
	return CRichEditView::OnQueryNewPalette();
}

void CEditorView::OnWinIniChange(LPCTSTR lpszSection) 
{
	CRichEditView::OnWinIniChange(lpszSection);
	//printer might have changed
	if (!m_bInPrint)
	{
		if (lstrcmpi(lpszSection, _T("windows")) == 0)
			theApp.NotifyPrinterChanged(TRUE); // force update to defaults
	}
}

void CEditorView::OnSize(UINT nType, int cx, int cy)
{
	CRichEditView::OnSize(nType, cx, cy);
	CRect rect(HORZ_TEXTOFFSET, VERT_TEXTOFFSET, cx, cy);
	GetRichEditCtrl().SetRect(rect);
}

void CEditorView::OnFilePrintPreview() 
{
	// make sure printer settings are updated
	CDC dc;
	CEditorDoc *pDoc = GetDocument();
	theApp.SetPrinterDeviceSettings(pDoc->m_dmOrientation, pDoc->m_dmPaperSize,
		pDoc->m_dmPaperWidth, pDoc->m_dmPaperLength);
	AfxGetApp()->CreatePrinterDC(dc);
	OnPrinterChanged(dc);
	CRichEditView::OnFilePrintPreview();
}

void CEditorView::OnFormatPagebreak() 
{
	CHARRANGE cr;
	char fn[] = "\f\n";
	char nfn[] = "\n\f\n";
	long nLineIndex, nCharIndex;

	GetRichEditCtrl().GetSel(cr);
	if (cr.cpMin==cr.cpMax)
	{
		nLineIndex = GetRichEditCtrl().LineFromChar(cr.cpMin);
		nCharIndex = GetRichEditCtrl().LineIndex(nLineIndex);
		GetRichEditCtrl().SetSel(nCharIndex, nCharIndex);
		GetRichEditCtrl().ReplaceSel(fn);		// insert form feed and new line
		GetRichEditCtrl().SetSel(cr);
	}
	else
	{
		nLineIndex = GetRichEditCtrl().LineFromChar(cr.cpMin);
		nCharIndex = GetRichEditCtrl().LineIndex(nLineIndex);
		if (nCharIndex==cr.cpMin)
			GetRichEditCtrl().ReplaceSel(fn);		// insert form feed and new line
		else
			GetRichEditCtrl().ReplaceSel(nfn);		// insert new line, form feed and new line
	}
}

void CEditorView::OnUpdateIndicatorLine(CCmdUI* pCmdUI)
{
	CString str;
	CPoint pt = GetCaretPos();
	long nStart, nEnd, nChar;
	long nLine, nCol;
	UINT nID, nStyle;
	int cxWidth;
	CSize size;
	CDC *pDC;
	CFont *pFont, *pOldFont;

	pCmdUI->Enable(TRUE);
	// create string from line num and column num
	GetRichEditCtrl().GetSel(nStart, nEnd);
	if (GetRichEditCtrl().GetCharPos(nStart)==pt)
		nChar = nStart;
	else
		nChar = nEnd;
	nLine = GetRichEditCtrl().LineFromChar(nChar);
	nCol = nChar-GetRichEditCtrl().LineIndex(nLine);
	str.Format("Z.: %d, Sp.: %d ", nLine, nCol);
	// resize pane
	((CMainFrame*)theApp.m_pMainWnd)->m_wndStatusBar.GetPaneInfo(1, nID, nStyle, cxWidth);
	pDC = ((CMainFrame*)theApp.m_pMainWnd)->m_wndStatusBar.GetDC();
	pFont = ((CMainFrame*)theApp.m_pMainWnd)->m_wndStatusBar.GetFont();
	pOldFont = pDC->SelectObject(pFont);
	size = pDC->GetOutputTextExtent(str);
	pDC->SelectObject(pOldFont);
	cxWidth = size.cx;
	((CMainFrame*)theApp.m_pMainWnd)->m_wndStatusBar.ReleaseDC(pDC);
	((CMainFrame*)theApp.m_pMainWnd)->m_wndStatusBar.SetPaneInfo(1, nID, nStyle, cxWidth);
	// set text
	pCmdUI->SetText(str);
}

static UINT BASED_CODE indicatorsOn[] =
{
	ID_SEPARATOR,           // status line indicator
	ID_INDICATOR_LINE,
	ID_INDICATOR_CAPS,
	ID_INDICATOR_NUM,
	ID_INDICATOR_SCRL,
};

static UINT BASED_CODE indicatorsOff[] =
{
	ID_SEPARATOR,           // status line indicator
	ID_INDICATOR_CAPS,
	ID_INDICATOR_NUM,
	ID_INDICATOR_SCRL,
};

void CEditorView::OnSetfocus() 
{
	((CMainFrame*)theApp.m_pMainWnd)->m_wndStatusBar.SetIndicators(indicatorsOn,
		  sizeof(indicatorsOn)/sizeof(UINT));
}

void CEditorView::OnKillfocus() 
{
	((CMainFrame*)theApp.m_pMainWnd)->m_wndStatusBar.SetIndicators(indicatorsOff,
		  sizeof(indicatorsOff)/sizeof(UINT));
}


void CEditorView::OnPrepareDC(CDC* pDC, CPrintInfo* pInfo) 
{
	//LOGFONT lf_font; 
    //(pDC->GetCurrentFont())->GetLogFont(&lf_font);
    //pDC->SetMapperFlags(ASPECT_FILTERING);
	CRichEditView::OnPrepareDC(pDC, pInfo);
}

void CEditorView::OnBeginPrinting(CDC* pDC, CPrintInfo* pInfo) 
{

    CHARRANGE cr;
    CCharFormat cf;	
	BOOL save_flag=GetRichEditCtrl().GetModify();

    GetRichEditCtrl().GetSel(cr);
	GetRichEditCtrl().HideSelection(TRUE, FALSE);
	GetRichEditCtrl().SetSel(0,-1);
    GetRichEditCtrl().GetSelectionCharFormat(cf);
	
    if(cf.yHeight>20)
        cf.yHeight-=20;
    GetRichEditCtrl().SetSelectionCharFormat(cf);	
    GetRichEditCtrl().SetSel(cr);
	GetRichEditCtrl().HideSelection(FALSE, FALSE);
    GetRichEditCtrl().SetModify(save_flag);

    CRichEditView::OnBeginPrinting(pDC, pInfo);
/*	if (m_hPrinterFont == NULL)
	{
		// get current screen font object metrics
		CFont* pFont = pDC->GetCurrentFont();
		LOGFONT lf;
		LOGFONT lfSys;
		if (pFont == NULL)
			return;
		VERIFY(pFont->GetObject(sizeof(LOGFONT), &lf));
		VERIFY(::GetObject(::GetStockObject(SYSTEM_FONT), sizeof(LOGFONT),
			&lfSys));
		if (lstrcmpi((LPCTSTR)lf.lfFaceName, (LPCTSTR)lfSys.lfFaceName) == 0)
			return;

		// map to printer font metrics
		HDC hDCFrom = ::GetDC(NULL);
		lf.lfHeight = ::MulDiv(lf.lfHeight, pDC->GetDeviceCaps(LOGPIXELSY),
			::GetDeviceCaps(hDCFrom, LOGPIXELSY));
		lf.lfWidth = ::MulDiv(lf.lfWidth, pDC->GetDeviceCaps(LOGPIXELSX),
			::GetDeviceCaps(hDCFrom, LOGPIXELSX));
		::ReleaseDC(NULL, hDCFrom);

		// create it, if it fails we just use the printer's default.
		m_hMirrorFont = ::CreateFontIndirect(&lf);
		m_hPrinterFont = m_hMirrorFont;
	} */
	
}

void CEditorView::OnEndPrinting(CDC* pDC, CPrintInfo* pInfo) 
{
	/*if (m_hMirrorFont != NULL && m_hPrinterFont == m_hMirrorFont)
	{
		//AfxDeleteObject((HGDIOBJ*)&m_hMirrorFont);
    HGDIOBJ* pObject = (HGDIOBJ*)&m_hMirrorFont;
    ASSERT(pObject != NULL);
	if (*pObject != NULL)
	{
		DeleteObject(*pObject);
		*pObject = NULL;
	}
		m_hPrinterFont = NULL;
	}*/


    CHARRANGE cr;
    CCharFormat cf;	
	BOOL save_flag=GetRichEditCtrl().GetModify();

    GetRichEditCtrl().GetSel(cr);
	GetRichEditCtrl().HideSelection(TRUE, FALSE);
	GetRichEditCtrl().SetSel(0,-1);
    GetRichEditCtrl().GetSelectionCharFormat(cf);
	
    
    cf.yHeight+=20;
    GetRichEditCtrl().SetSelectionCharFormat(cf);	
    GetRichEditCtrl().SetSel(cr);
	GetRichEditCtrl().HideSelection(FALSE, FALSE);
    GetRichEditCtrl().SetModify(save_flag);

	CRichEditView::OnEndPrinting(pDC, pInfo);
}

void CEditorView::SetPrinterFont(CFont* pFont)
{
	ASSERT_VALID(this);
	m_hPrinterFont = (HFONT)pFont->GetSafeHandle();
	ASSERT_VALID(this);
}

CFont* CEditorView::GetPrinterFont() const
{
	ASSERT_VALID(this);
	return CFont::FromHandle(m_hPrinterFont);
}

static void ScaleLogFont(LPLOGFONT plf, const CDC& dcFrom, const CDC& dcTo)
	// Hilfsfunktion zur Um-Skalierung von logfont Elementen in verschiedenen DCs.
{
	plf->lfHeight = MulDiv(plf->lfHeight,
		dcTo.GetDeviceCaps(LOGPIXELSY), dcFrom.GetDeviceCaps(LOGPIXELSY));
	plf->lfWidth = MulDiv(plf->lfWidth,
		dcTo.GetDeviceCaps(LOGPIXELSX), dcFrom.GetDeviceCaps(LOGPIXELSX));
}

void CEditorView::OnFormatFont() 
{
    CWaitCursor wait;
    CPrintDialog dlgPrint(FALSE);
	if (!AfxGetApp()->GetPrinterDeviceDefaults(&dlgPrint.m_pd))
	{
		AfxMessageBox("Cannot get printer device context.");
		return;
	}
    wait.Restore();
	HDC hdcPrint = dlgPrint.CreatePrinterDC();
	if (hdcPrint == NULL)
	{
		AfxMessageBox("Cannot get printer device defaults.");
		return;
	}

	CDC dcScreen;
	dcScreen.Attach(::GetDC(NULL));
	CDC dcPrint;
	dcPrint.Attach(hdcPrint);

	
	GetCharFormatSelection();
	CFontDialog dlg(m_charformat, CF_BOTH|CF_NOOEMFONTS,&dcPrint);
	if (dlg.DoModal() == IDOK)
	{
		dlg.GetCharFormat(m_charformat);
		SetCharFormat(m_charformat);
	}
}
