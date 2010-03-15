// drawdoc.cpp : implementation of the CStempelDoc class
//

#include "stdafx.h"

#include "..\..\stempel\include\stempel.h"

#include "stpldoc.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CStempelDocData - all serializable data from CStempelDoc

IMPLEMENT_SERIAL(CStempelDocData, CObject, VERSIONABLE_SCHEMA|1)

CStempelDocData::CStempelDocData()
{
}

CStempelDocData::~CStempelDocData()
{
}

void CStempelDocData::Serialize(CArchive& ar)
{
	CObject::Serialize(ar);
	if (ar.IsStoring())
	{
		ar << m_name;
	}
	else
	{
        int nVersion = ar.GetObjectSchema();
		ar.SetObjectSchema(nVersion);
        switch (nVersion)
        {
            case 0:
			case 1:
				ar >> m_name;
                break;

            default:
				AfxThrowArchiveException(CArchiveException::badSchema);
                break;
        }
	}
}

#ifdef _DEBUG
void CStempelDocData::AssertValid()
{
}
#endif

/////////////////////////////////////////////////////////////////////////////
// CStempelDoc

IMPLEMENT_DYNCREATE(CStempelDoc, CDrawDoc)

BEGIN_MESSAGE_MAP(CStempelDoc, CDrawDoc)
	//{{AFX_MSG_MAP(CStempelDoc)
	//}}AFX_MSG_MAP
	// Enable default OLE container implementation
	ON_UPDATE_COMMAND_UI(ID_EDIT_PASTE, COleDocument::OnUpdatePasteMenu)
	ON_UPDATE_COMMAND_UI(ID_EDIT_PASTE_LINK, COleDocument::OnUpdatePasteLinkMenu)
	ON_UPDATE_COMMAND_UI(ID_OLE_EDIT_LINKS, COleDocument::OnUpdateEditLinksMenu)
	ON_COMMAND(ID_OLE_EDIT_LINKS, COleDocument::OnEditLinks)
	ON_UPDATE_COMMAND_UI(ID_OLE_VERB_FIRST, COleDocument::OnUpdateObjectVerbMenu)
#if !defined(_MAC)
        // MAPI support
	ON_COMMAND(ID_FILE_SEND_MAIL, OnFileSendMail)
	ON_UPDATE_COMMAND_UI(ID_FILE_SEND_MAIL, OnUpdateFileSendMail)

	ON_COMMAND(ID_FILE_DXF_EXPORT, OnFileDxfExport)
#endif
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CStempelDoc construction/destruction

CStempelDoc::CStempelDoc() : CDrawDoc()
{
	m_pSData = new CStempelDocData;
}

CStempelDoc::CStempelDoc(CString& name) : CDrawDoc()
{
	m_pSData = new CStempelDocData;
	m_pSData->m_name = name;
}

CStempelDoc::~CStempelDoc()
{
	if (m_pSData!=NULL)
		delete m_pSData;
}

void CStempelDoc::DeleteContents()
{
	CDrawDoc::DeleteContents();
}

/////////////////////////////////////////////////////////////////////////////
// CStempelDoc serialization

void CStempelDoc::Serialize(CArchive& ar)
{
	CDrawDoc::Serialize(ar);
	if (ar.IsStoring())
		ar << m_pSData;
	else
	{
		delete m_pSData;
		m_pSData = NULL;	// prepare for exception
		ar >> m_pSData;
	}
}

/////////////////////////////////////////////////////////////////////////////
// CStempelDoc implementation

void CStempelDoc::UpdateDrawing(CDrawView* /*pView*/)
{
	POSITION pos;
	CDrawObj *pObj;
	int i;
	CString str;
	
	pos = GetObjects()->GetHeadPosition();
	while (pos!=NULL)
	{
		pObj = GetObjects()->GetNextObject( pos );
		if (pObj->IsText())
		{
			i = ((CDrawRect*)pObj)->GetStempelTextType();
			if (i!=STPL_TEXT_NONE)
			{
				GetDefaultStempelText(i, str);
				((CDrawRect*)pObj)->SetText(str);
			}
		}
	}
}

BOOL CStempelDoc::OnNewDocument()
{
	BOOL bReturn = CDrawDoc::OnNewDocument();

	if( bReturn )
	{
		m_pSData->m_name = GETSTEMPELAPP->m_strStempelName;
		SetDrawingSize( GETSTEMPELAPP->m_sizeStempel );
	}

	return bReturn;
}

BOOL CStempelDoc::OnOpenDocument(LPCTSTR lpszPathName)
{
	return CDrawDoc::OnOpenDocument(lpszPathName);
}

BOOL CStempelDoc::OnSaveDocument(LPCTSTR lpszPathName)
{
	CString path;
	int i;
	BOOL bFound = FALSE;
	
	GETSTEMPELAPP->LoadStempelFiles();
	path.Format("%s", lpszPathName);
	i = path.ReverseFind('\\');
	if (i!=-1)
	{
		CString test = path.Left( i + 1 );
		CString dir = GETSTEMPELAPP->GetExecDirectory();
		
    if( test.CompareNoCase( dir ) == 0 )
			path = path.Right( path.GetLength() - ( i + 1 ) );
	}
	for (i=0; i<GETSTEMPELAPP->m_strStempelFiles.GetSize(); i++)
	{
		if (GETSTEMPELAPP->m_strStempelFiles[i].CompareNoCase(path)==0)
		{
			bFound = TRUE;
			break;
		}
	}
	if (!bFound)
		GETSTEMPELAPP->m_strStempelFiles.SetAtGrow(GETSTEMPELAPP->m_strStempelFiles.GetSize(), path);
	if (!GETSTEMPELAPP->SaveStempelFiles())
		return FALSE;
	GETSTEMPELAPP->FlushStempelFiles();
	return CDrawDoc::OnSaveDocument(lpszPathName);
}

/*static */
CStempelDoc* CStempelDoc::LoadStempel( const CString& path )
{
	CStempelDoc* pDoc = new CStempelDoc;

  CString stplPath = path;
	if( path.Find( '\\' ) == -1 )
		stplPath = GETSTEMPELAPP->GetExecDirectory() + path;
	
  // we must load the stempel through COleDocument and not
	// COleServerDoc since our stempel doc is not registered!
	if( pDoc->COleDocument::OnOpenDocument( stplPath ) )
		return pDoc;
	else
	{
		CString msg;

		msg.FormatMessage( IDS_SHAREVIOLATION_STEMPEL, stplPath );
		AfxMessageBox( msg, MB_ICONSTOP | MB_OK );
		pDoc->DeleteContents();
		delete pDoc;
		return NULL;
	}
} // LoadStempel
