// editdoc.cpp : implementation of the CEditorDoc class
//

#include "stdafx.h"

#include "global.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

extern BOOL AFXAPI AfxFullPath(LPTSTR lpszPathOut, LPCTSTR lpszFileIn);
extern UINT AFXAPI AfxGetFileTitle(LPCTSTR lpszPathName, LPTSTR lpszTitle, UINT nMax);

#ifndef OFN_EXPLORER
#define OFN_EXPLORER 0x00080000L
#endif
/////////////////////////////////////////////////////////////////////////////
// CEditorDoc
IMPLEMENT_DYNCREATE(CEditorDoc, CRichEditDoc)

BEGIN_MESSAGE_MAP(CEditorDoc, CRichEditDoc)
	//{{AFX_MSG_MAP(CEditorDoc)
	ON_UPDATE_COMMAND_UI(ID_OLE_VERB_POPUP, OnUpdateOleVerbPopup)
	ON_COMMAND(ID_FILE_SEND_MAIL, OnFileSendMail)
	ON_COMMAND(ID_FORMAT_CONVERT, OnFormatConvert)
	ON_UPDATE_COMMAND_UI(ID_FORMAT_CONVERT, OnUpdateFormatConvert)
	//}}AFX_MSG_MAP
	ON_UPDATE_COMMAND_UI(ID_FILE_SEND_MAIL, OnUpdateFileSendMail)
	ON_COMMAND(ID_OLE_EDIT_LINKS, CRichEditDoc::OnEditLinks)
	ON_UPDATE_COMMAND_UI(ID_OLE_VERB_FIRST, CRichEditDoc::OnUpdateObjectVerbMenu)
	ON_UPDATE_COMMAND_UI(ID_OLE_EDIT_CONVERT, CRichEditDoc::OnUpdateObjectVerbMenu)
	ON_UPDATE_COMMAND_UI(ID_OLE_EDIT_LINKS, CRichEditDoc::OnUpdateEditLinksMenu)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CEditorDoc construction/destruction

CEditorDoc::CEditorDoc()
{
	m_nDocType = -1;
	m_nNewDocType = -1;
	theApp.GetStandardPrinterSettings(m_dmOrientation, m_dmPaperSize,
		m_dmPaperWidth, m_dmPaperLength);
	theApp.SetPrinterDeviceSettings(m_dmOrientation, m_dmPaperSize,
		m_dmPaperWidth, m_dmPaperLength);
}

BOOL CEditorDoc::OnNewDocument()
{
	if (!CRichEditDoc::OnNewDocument())
		return FALSE;

 	//correct type already set in theApp.m_nNewDocType;
 	int nDocType = (IsEmbedded()) ? RD_EMBEDDED : theApp.m_nNewDocType;

	GetView()->SetDefaultFont(IsTextType(nDocType));
	SetDocType(nDocType);

	return TRUE;
}

void CEditorDoc::ReportSaveLoadException(LPCTSTR lpszPathName,
	CException* e, BOOL bSaving, UINT nIDP)
{
	if (!m_bDeferErrors && e != NULL)
	{
		ASSERT_VALID(e);
		if (e->IsKindOf(RUNTIME_CLASS(CFileException)))
		{
			switch (((CFileException*)e)->m_cause)
			{
			case CFileException::fileNotFound:
			case CFileException::badPath:
				nIDP = AFX_IDP_FAILED_INVALID_PATH;
				break;
			case CFileException::diskFull:
				nIDP = AFX_IDP_FAILED_DISK_FULL;
				break;
			case CFileException::accessDenied:
				nIDP = bSaving ? AFX_IDP_FAILED_ACCESS_WRITE :
						AFX_IDP_FAILED_ACCESS_READ;
				if (((CFileException*)e)->m_lOsError == ERROR_WRITE_PROTECT)
					nIDP = IDS_WRITEPROTECT;
				break;
			case CFileException::tooManyOpenFiles:
				nIDP = IDS_TOOMANYFILES;
				break;
			case CFileException::directoryFull:
				nIDP = IDS_DIRFULL;
				break;
			case CFileException::sharingViolation:
				nIDP = IDS_SHAREVIOLATION;
				break;
			case CFileException::lockViolation:
			case CFileException::badSeek:
			case CFileException::generic:
			case CFileException::invalidFile:
			case CFileException::hardIO:
				nIDP = bSaving ? AFX_IDP_FAILED_IO_ERROR_WRITE :
						AFX_IDP_FAILED_IO_ERROR_READ;
				break;
			default:
				break;
			}
			CString prompt;
			AfxFormatString1(prompt, nIDP, lpszPathName);
			AfxMessageBox(prompt, MB_ICONEXCLAMATION, nIDP);
			return;
		}
	}
	CRichEditDoc::ReportSaveLoadException(lpszPathName, e, bSaving, nIDP);
	return;
}

BOOL CEditorDoc::OnOpenDocument(LPCTSTR lpszPathName) 
{
	if (m_lpRootStg != NULL) // we are embedded
	{
		// we really want to use the converter on this storage
		m_nNewDocType = RD_EMBEDDED;
	}
	else
	{
		if (theApp.cmdInfo.m_bForceTextMode)
			m_nNewDocType = RD_TEXT;
		else
		{
			CFileException fe;
			m_nNewDocType = GetDocTypeFromName(lpszPathName, fe);
			if (m_nNewDocType == -1)
			{
				ReportSaveLoadException(lpszPathName, &fe, FALSE, 
					AFX_IDP_FAILED_TO_OPEN_DOC);
				return FALSE;
			}
			if (m_nNewDocType == RD_TEXT && theApp.m_bForceOEM)
				m_nNewDocType = RD_OEMTEXT;
		}
		ScanForConverters();
		if (!doctypes[m_nNewDocType].bRead)
		{
			CString str;
			CString strName = doctypes[m_nNewDocType].GetString(DOCTYPE_DOCTYPE);
			AfxFormatString1(str, IDS_CANT_LOAD, strName);
			AfxMessageBox(str, MB_OK|MB_ICONINFORMATION);
			return FALSE;
		}
	}

//	SetDocType(nNewDocType);
	if (!CRichEditDoc::OnOpenDocument(lpszPathName))
		return FALSE;
	return TRUE;
}

BOOL CEditorDoc::OnSaveDocument(LPCTSTR lpszPathName) 
{
	State *st;
	Calculation *calc;
	Project *proj = theApp.m_pProject;
	int i;
	CString fileName, file, str, path;

	if (proj!=NULL)
	{
		file.Format("%s", lpszPathName);
		path = proj->GetCalcDir();
		path.MakeLower();
		file.MakeLower();
		st = proj->GetFirstState();
		while (st!=NULL)
		{
			calc = st->GetFirstCalculation();
			while (calc!=NULL)
			{
				for (i=0; i<N_RESULT_TYPES; i++)
				{
					if (calc->GetResultFileName(fileName, i))
					{
						fileName = path + fileName;
						if (file==fileName)
						{
							str.LoadString(IDS_FILE_ERROR_PROJECTFILE);
							AfxMessageBox(str, MB_ICONEXCLAMATION | MB_OK);
							return FALSE;
						}
					}
				}
				calc = st->GetNextCalculation();
			}
			st = proj->GetNextState();
		}
	}

	return CRichEditDoc::OnSaveDocument(lpszPathName);
}

void CEditorDoc::Serialize(CArchive& ar)
{
	COleMessageFilter* pFilter = AfxOleGetMessageFilter();
	ASSERT(pFilter != NULL);
	pFilter->EnableBusyDialog(FALSE);
	if (ar.IsLoading())
		SetDocType(m_nNewDocType);
	CRichEditDoc::Serialize(ar);
	pFilter->EnableBusyDialog(TRUE);
}

BOOL CEditorDoc::DoSave(LPCTSTR pszPathName, BOOL bReplace /*=TRUE*/)
// Save the document data to a file
// pszPathName = path name where to save document file
// if pszPathName is NULL then the user will be prompted (SaveAs)
// note: pszPathName can be different than 'm_strPathName'
// if 'bReplace' is TRUE will change file name if successful (SaveAs)
// if 'bReplace' is FALSE will not change path name (SaveCopyAs)
{
	CString newName = pszPathName;
	int nOrigDocType = m_nDocType;  //saved in case of SaveCopyAs or failure

	//	newName		bWrite	type	result
	//	empty		TRUE	-		SaveAs dialog
	//	empty		FALSE	-		SaveAs dialog
	//	notempty	TRUE	-		nothing
	//	notempty	FALSE	W6		warn (change to wordpad, save as, cancel)
	//	notempty	FALSE	other	warn (save as, cancel)

	BOOL bModified = IsModified();

	ScanForConverters();

	BOOL bSaveAs = FALSE;
	if (newName.IsEmpty())
		bSaveAs = TRUE;
	else if (!doctypes[m_nDocType].bWrite)
	{
		if (m_nDocType == RD_WINWORD6)
			bSaveAs = TRUE;
		else // 
		{
			if (AfxMessageBox(IDS_SAVE_UNSUPPORTED, 
				MB_YESNO | MB_ICONQUESTION) != IDYES)
			{
				return FALSE;
			}
			else
				bSaveAs = TRUE;
		}
	}

	if (m_lpRootStg == NULL && IsTextType(m_nDocType) && 
		!GetView()->IsFormatText())
	{
		// formatting changed in plain old text file
		DWORD nHelpIDs[] = 
		{
			0, IDH_WORDPAD_WORD6FILE,
			0, IDH_WORDPAD_FORMATTED,
			0, IDH_WORDPAD_TEXTFILE,
			0, 0
		};
		CString str = GetTitle();
		ConvertDialog dlg(str);

		if (dlg.DoModal()!=IDOK)
			return FALSE;
		int nDocType = (dlg.m_nType==0) ? RD_RICHTEXT :	//RTF
						RD_TEXT;					//text
		if (IsTextType(m_nDocType) && nDocType != RD_TEXT)
			SetDocType(nDocType, TRUE);
		if (nDocType != RD_TEXT)
			bSaveAs = TRUE;
	}

	GetView()->GetParentFrame()->RecalcLayout();
	if (bSaveAs)
	{
		newName = m_strPathName;
		if (bReplace && newName.IsEmpty())
		{
			newName = m_strTitle;
			int iBad = newName.FindOneOf(_T(" #%;/\\"));    // dubious filename
			if (iBad != -1)
				newName.ReleaseBuffer(iBad);

			// append the default suffix if there is one
			newName += GetExtFromType(m_nDocType);
		}

		int nDocType = m_nDocType;
		if (!theApp.PromptForFileName(newName, 
			bReplace ? AFX_IDS_SAVEFILE : AFX_IDS_SAVEFILECOPY,
			OFN_HIDEREADONLY | OFN_PATHMUSTEXIST, FALSE, &nDocType))
		{
			SetDocType(nOrigDocType, TRUE);
			return FALSE;       // don't even try to save
		}
		SetDocType(nDocType, TRUE);
	}

	BeginWaitCursor();
	if (!OnSaveDocument(newName))
	{
		if (pszPathName == NULL)
		{
			// be sure to delete the file
			TRY 
			{
				CFile::Remove(newName);
			}
			CATCH_ALL(e)
			{
				TRACE0("Warning: failed to delete file after failed SaveAs\n");
			}
			END_CATCH_ALL
		}
		// restore orginal document type
		SetDocType(nOrigDocType, TRUE);
		EndWaitCursor();
		return FALSE;
	}

	EndWaitCursor();
	if (bReplace)
	{
		int nType = m_nDocType;
		SetDocType(nOrigDocType, TRUE);
		SetDocType(nType);
		// Reset the title and change the document name
		SetPathName(newName, TRUE);
		ASSERT(m_strPathName == newName);       // must be set
	}
	else // SaveCopyAs
	{
		SetDocType(nOrigDocType, TRUE);
		SetModifiedFlag(bModified);
	}
	return TRUE;        // success
}

class COIPF : public COleIPFrameWnd
{
public:
	CFrameWnd* GetMainFrame() { return (CFrameWnd*)m_pMainFrame;}
	CFrameWnd* GetDocFrame() { return (CFrameWnd*)m_pDocFrame;}
};

void CEditorDoc::OnDeactivateUI(BOOL bUndoable)
{
	if (GetView()->m_bDelayUpdateItems)
		UpdateAllItems(NULL);
//	SaveState(m_nDocType);
	CRichEditDoc::OnDeactivateUI(bUndoable);
	COIPF* pFrame = (COIPF*)m_pInPlaceFrame;
	if (pFrame != NULL)
	{
		if (pFrame->GetMainFrame() != NULL)
			ForceDelayed(pFrame->GetMainFrame());
		if (pFrame->GetDocFrame() != NULL)
			ForceDelayed(pFrame->GetDocFrame());
	}
}

void CEditorDoc::ForceDelayed(CFrameWnd* pFrameWnd)
{
	ASSERT_VALID(this);
	ASSERT_VALID(pFrameWnd);

	POSITION pos = pFrameWnd->m_listControlBars.GetHeadPosition();
	while (pos != NULL)
	{
		// show/hide the next control bar
		CControlBar* pBar =
			(CControlBar*)pFrameWnd->m_listControlBars.GetNext(pos);

		BOOL bVis = pBar->GetStyle() & WS_VISIBLE;
		UINT swpFlags = 0;
		if ((pBar->m_nStateFlags & CControlBar::delayHide) && bVis)
			swpFlags = SWP_HIDEWINDOW;
		else if ((pBar->m_nStateFlags & CControlBar::delayShow) && !bVis)
			swpFlags = SWP_SHOWWINDOW;
		pBar->m_nStateFlags &= ~(CControlBar::delayShow|CControlBar::delayHide);
		if (swpFlags != 0)
		{
			pBar->SetWindowPos(NULL, 0, 0, 0, 0, swpFlags|
				SWP_NOMOVE|SWP_NOSIZE|SWP_NOZORDER|SWP_NOACTIVATE);
		}
	}
}

/////////////////////////////////////////////////////////////////////////////
// CEditorDoc Attributes
CLSID CEditorDoc::GetClassID()
{
	return (m_pFactory == NULL) ? CLSID_NULL : m_pFactory->GetClassID();
}

void CEditorDoc::SetDocType(int nNewDocType, BOOL bNoOptionChange)
{
	ASSERT(nNewDocType != -1);
	if (nNewDocType == m_nDocType)
		return;

	POSITION pos;
	pos = GetFirstViewPosition();
	while (pos!=NULL)
	{
		CEditorView *pView = (CEditorView*)GetNextView(pos);
		CMainFrame *pMainFrame = (CMainFrame*)pView->GetTopLevelFrame();
		HICON hIcon = pMainFrame->GetIcon(nNewDocType);
		pView->GetParent()->SendMessage(WM_SETICON, TRUE, (LPARAM)hIcon);
	}
	
	m_bRTF = !IsTextType(nNewDocType);
	m_nDocType = nNewDocType;
}

CEditorView* CEditorDoc::GetView()
{
	POSITION pos = GetFirstViewPosition();
	return (CEditorView* )GetNextView( pos );
}

/////////////////////////////////////////////////////////////////////////////
// CEditorDoc Operations

CFile* CEditorDoc::GetFile(LPCTSTR pszPathName, UINT nOpenFlags, CFileException* pException)
{
	CTrackFile* pFile = NULL;
	CFrameWnd* pWnd = GetView()->GetParentFrame();
#ifdef CONVERTERS
	ScanForConverters();

	// if writing use current doc type otherwise use new doc type
	int nType = (nOpenFlags & CFile::modeReadWrite) ? m_nDocType : m_nNewDocType;
	// m_nNewDocType will be same as m_nDocType except when opening a new file
	if (doctypes[nType].pszConverterName != NULL)
		pFile = new CConverter(doctypes[nType].pszConverterName, pWnd);
	else
#endif
	if (nType == RD_OEMTEXT)
		pFile = new COEMFile(pWnd);
	else
		pFile = new CTrackFile(pWnd);
 	if (!pFile->Open(pszPathName, nOpenFlags, pException))
	{
 		delete pFile;
		return NULL;
	}
	if (nOpenFlags & (CFile::modeWrite | CFile::modeReadWrite))
		pFile->m_dwLength = 0; // can't estimate this
	else
		pFile->m_dwLength = pFile->GetLength();
	return pFile;
}

CRichEditCntrItem* CEditorDoc::CreateClientItem(REOBJECT* preo) const
{
	// cast away constness of this
	return new CEditorCntrItem(preo, (CEditorDoc*)this);
}

/////////////////////////////////////////////////////////////////////////////
// CEditorDoc serialization

/////////////////////////////////////////////////////////////////////////////
// CEditorDoc diagnostics

#ifdef _DEBUG
void CEditorDoc::AssertValid() const
{
	CRichEditDoc::AssertValid();
}

void CEditorDoc::Dump(CDumpContext& dc) const
{
	CRichEditDoc::Dump(dc);
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CEditorDoc commands

int CEditorDoc::MapType(int nType)
{
	if (nType == RD_OEMTEXT)
		nType = RD_TEXT;
	else if (!IsInPlaceActive() && nType == RD_EMBEDDED)
		nType = RD_RICHTEXT;
	return nType;
}

void CEditorDoc::OnUpdateOleVerbPopup(CCmdUI* pCmdUI) 
{
	pCmdUI->m_pParentMenu = pCmdUI->m_pMenu;
	CRichEditDoc::OnUpdateObjectVerbMenu(pCmdUI);
}

BOOL CEditorDoc::OnCmdMsg(UINT nID, int nCode, void* pExtra, AFX_CMDHANDLERINFO* pHandlerInfo) 
{
	if (nCode == CN_COMMAND && nID == ID_OLE_VERB_POPUP)
		nID = ID_OLE_VERB_FIRST;	
	return CRichEditDoc::OnCmdMsg(nID, nCode, pExtra, pHandlerInfo);
}

void CEditorDoc::OnFileSendMail() 
{
	if (m_strTitle.Find('.') == -1)
	{
		// add the extension because the default extension will be wrong
		CString strOldTitle = m_strTitle;
		m_strTitle += GetExtFromType(m_nDocType);
		CRichEditDoc::OnFileSendMail();
		m_strTitle = strOldTitle;
	}
	else
		CRichEditDoc::OnFileSendMail();
}

void CEditorDoc::OEMConvert(BOOL bOEMToANSI) 
{
	CFrameWnd* pWnd = GetView()->GetParentFrame();
	CMemOEMFile file(pWnd);
	CFileException fe;
	
	if (bOEMToANSI)
		file.m_bOEMToANSI = TRUE;
	else
		file.m_bOEMToANSI = FALSE;
	CArchive saveArchive(&file, CArchive::store | CArchive::bNoFlushOnDelete);
	TRY
	{
		CWaitCursor wait;
		Serialize(saveArchive);
		saveArchive.Close();
	}
	CATCH_ALL(e)
	{
		file.Abort();
		do
		{
			e->Delete();
		}
		while (0);
		return;
	}
	END_CATCH_ALL
	file.SeekToBegin();
	DeleteContents();
	CArchive loadArchive(&file, CArchive::load | CArchive::bNoFlushOnDelete);
	TRY
	{
		CWaitCursor wait;
		Serialize(loadArchive);
		loadArchive.Close();
	}
	CATCH_ALL(e)
	{
		file.Abort();
		do
		{
			e->Delete();
		}
		while (0);
		return;
	}
	END_CATCH_ALL
}

void CEditorDoc::OnUpdateFormatConvert(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(m_nDocType==RD_OEMTEXT || m_nDocType==RD_TEXT);
}

void CEditorDoc::OnFormatConvert() 
{
	OEMConvert(m_nDocType!=RD_OEMTEXT);
	if (m_nDocType==RD_TEXT)
		m_nNewDocType = m_nDocType = RD_OEMTEXT;
	else
		m_nNewDocType = m_nDocType = RD_TEXT;
}
