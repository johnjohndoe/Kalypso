// profdoc.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "..\..\wspprj\wspprj.h"

#include "summinfo.h"
#include "plotter.h"

#include "profdoc.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CProfDatDoc

IMPLEMENT_DYNCREATE(CProfDatDoc, CPlotterDoc)

CProfDatDoc::CProfDatDoc() : CPlotterDoc()
{
}

CProfDatDoc::~CProfDatDoc()
{
}


void CProfDatDoc::DeleteContents()
{
	if (GetSections()->GetSize()>0 && GetMSection( 0 ) != NULL )
		delete GetMSection( 0 );
	CPlotterDoc::DeleteContents();
}

BEGIN_MESSAGE_MAP(CProfDatDoc, CPlotterDoc)
	//{{AFX_MSG_MAP(CProfDatDoc)
		// HINWEIS - Der Klassen-Assistent fügt hier Zuordnungsmakros ein und entfernt diese.
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Diagnose CProfDatDoc

#ifdef _DEBUG
void CProfDatDoc::AssertValid() const
{
	CPlotterDoc::AssertValid();
}

void CProfDatDoc::Dump(CDumpContext& dc) const
{
	CPlotterDoc::Dump(dc);
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// Serialisierung CProfDatDoc 

void CProfDatDoc::Serialize(CArchive& ar)
{
	CPlotterDoc::Serialize(ar);
}

/////////////////////////////////////////////////////////////////////////////
// Befehle CProfDatDoc 


BOOL CProfDatDoc::OnOpenDocument(LPCTSTR lpszPathName) 
{
	CFile file;
	CFileStatus rStatus;
	CString fileName = lpszPathName;
	if (!file.GetStatus( fileName, rStatus ) )
		return FALSE;
	else
	{
		if (!COleDocument::OnNewDocument())
			return FALSE;

#if !defined(_MAC)
#if _MFC_VER>=0x0421
		delete m_pData->m_pSummInfo;
		m_pData->m_pSummInfo = new CSummInfo;
		// Title, Subject, Author, Keywords default to empty string
		// Comments, Template, SavedBy default to empty string
		// LastSave, LastPrint, EditTime, RevNum default to 0
		m_pData->m_pSummInfo->StartEditTimeCount();
		m_pData->m_pSummInfo->RecordCreateDate();
		m_pData->m_pSummInfo->SetNumPages(1);
		// NumWords, NumChars default to 0
		m_pData->m_pSummInfo->SetAppname( _T("WSPWIN Plotter") );
		// Security defaults to 0 
#endif
#endif
  }

	Section* newSection = TryLoadSection( fileName );
  if( newSection )
  {
    newSection->LoadProfil();
    InsertData( NULL, newSection, FALSE, FALSE );

    m_originalFile = lpszPathName;
    
    return TRUE;
  }
  else
    return FALSE;
}; // OnOpenDocument

/* static */
Section* CProfDatDoc::TryLoadSection( CString& fileName )
{
	// first create a test section to determine the class type...
	Section pSec( CLASS_TYPE_LSECTION, NULL );
	pSec.SetFileName( fileName );
	pSec.LoadProfil();

	Profil* pProf = pSec.GetProfil();
	DataBlock* pDB = pProf->GetDataBlock( DST_SOHLHOEHE );

	pSec.FlushProfil();

	if( pDB )
	{
		LengthSection* pLSec = new LengthSection( NULL );
		pLSec->SetFileName( fileName );

		// Den Namen der LengthSection setzen, er wird im Bearbeiten-Eigenschaften Dialog
		// unter Daten gezeigt
		pLSec->SetName( fileName );

		return pLSec;
	}
	else
	{
		Section* pCSec = new CrossSection( NULL );
		pCSec->SetFileName( fileName );
		return pCSec;
	};
}

void CProfDatDoc::SetTitle(LPCTSTR lpszTitle) 
{
	CString strDocName = lpszTitle;
	BOOL bUntitled = FALSE;

	if (GetMSection( 0 )==NULL)
		bUntitled = TRUE;
	else
	{
		CString fileName;
		fileName = GetMSection( 0 )->GetFileName();
		if (fileName.CompareNoCase(m_strPathName)==0)
			bUntitled = TRUE;
	}

	if( bUntitled )
	{
		// use generic 'untitled' - ignore untitled count
		VERIFY(strDocName.LoadString(AFX_IDS_UNTITLED));
		m_strPathName.Empty();
		
    // sehr sehr dirty, hier wird einfach mal das Document Template ausgetauscht
    m_pDocTemplate->RemoveDocument( this );
		GETPLOTTERAPP->GetPlotterDocTemplate()->AddDocument(this);
	}
	
	CDocument::SetTitle(strDocName);
}

