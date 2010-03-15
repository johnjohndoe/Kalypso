// mapproj.cpp: Implementierungsdatei
//
#pragma warning(disable:4786)
#pragma warning(disable:4503)

#include "stdafx.h"

#include "resource.h"

#include "commonMfc/include/mfchelper.h"

#include "nmapdlg.h"
#include "openmdlg.h"
#include "mainfrm.h"
#include "wspmap.h"
#include "mapview.h" // für MAPUPDATE...
#include "mappreview.h"

#include "mapproj.h"

extern CWSPMapApp theApp;

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

IMPLEMENT_DYNAMIC(CProjectDocTemplate, CMultiDocTemplate)

CProjectDocTemplate::CProjectDocTemplate(UINT nIDResource, CRuntimeClass* pDocClass,
	CRuntimeClass* pFrameClass, CRuntimeClass* pViewClass) :
	CMultiDocTemplate(nIDResource, pDocClass, pFrameClass, pViewClass)
{
}

CDocument* CProjectDocTemplate::OpenDocumentFile(LPCTSTR lpszPathName,
	BOOL bMakeVisible)
// diese Überschreibung der klasse OpenDocumentFile ist exact gleich wie das Original,
// nur alles, was das erzeugen eines CFrameWnd betrifft ist weggelassen
{
	CDocument* pDocument = CreateNewDocument();
	if (pDocument == NULL)
	{
		TRACE0("CDocTemplate::CreateNewDocument returned NULL.\n");
		AfxMessageBox(AFX_IDP_FAILED_TO_CREATE_DOC);
		return NULL;
	}
	ASSERT_VALID(pDocument);

	BOOL bAutoDelete = pDocument->m_bAutoDelete;
	pDocument->m_bAutoDelete = FALSE;   // don't destroy if something goes wrong
	if (lpszPathName == NULL)
	{
		// create a new document - with default document name
		SetDefaultTitle(pDocument);

		// avoid creating temporary compound file when starting up invisible
		if (!bMakeVisible)
			pDocument->m_bEmbedded = TRUE;

		if (!pDocument->OnNewDocument())
		{
			// user has be alerted to what failed in OnNewDocument
			TRACE0("CDocument::OnNewDocument returned FALSE.\n");
			return NULL;
		}

		// it worked, now bump untitled count
		m_nUntitledCount++;
	}
	else
	{
		// open an existing document
		CWaitCursor wait;
		if (!pDocument->OnOpenDocument(lpszPathName))
		{
			// user has be alerted to what failed in OnOpenDocument
			TRACE0("CDocument::OnOpenDocument returned FALSE.\n");
			return NULL;
		}
#ifdef _MAC
		// if the document is dirty, we must have opened a stationery pad
		//  - don't change the pathname because we want to treat the document
		//  as untitled
		if (!pDocument->IsModified())
#endif
			pDocument->SetPathName(lpszPathName);
	}

	return pDocument;
}

/////////////////////////////////////////////////////////////////////////////
// CMapProject

IMPLEMENT_SERIAL(CMapProject, CDocument, VERSIONABLE_SCHEMA|2)

CMapProject::CMapProject()
{
	m_pProject = NULL;
}

CMapProject::~CMapProject()
{
	if (m_pProject!=NULL)
		delete m_pProject;
}

void CMapProject::DeleteContents()
{
  if ( m_pProject )
  {
    delete m_pProject;
    m_pProject = NULL;
  };
  m_strMapDocFiles.RemoveAll();
  m_strMapDocNames.RemoveAll();
  m_MapDocRects.RemoveAll();
	CDocument::DeleteContents();
}

///////////////////////////////////////////////////////////////////
// Attribute

CString CMapProject::GetMapDocFile(int i)
{
  ASSERT( m_pProject );
	if (i >= 0 && i < m_strMapDocFiles.GetSize())
  {
    if ( m_strMapDocFiles[i].Find( TCHAR(':') ) == -1 )
      return m_pProject->GetMapDir() + m_strMapDocFiles[i];
    else
      return m_strMapDocFiles[i];
  }
	else
		return "";
};

CString CMapProject::GetMapDocName(int i)
{
	if (i >= 0 && i < m_strMapDocNames.GetSize())
		return m_strMapDocNames[i];
	else
		return "";
}

CDoubleRect CMapProject::GetMapDocRect(int i)
{
	if (i >= 0 && i < m_MapDocRects.GetSize())
		return m_MapDocRects[i];
	else
		return CDoubleRect(0, 0, 0, 0);
}

CString CMapProject::GetNewMapDocFile()
{
  ASSERT( m_pProject );
  
	int i = 1;
	CFileStatus rStatus;
  CString file;

  CString path = m_pProject->GetMapDir();
	file.Format("map%05d.wpm", i++);
	while (CFile::GetStatus( path + file, rStatus))
		file.Format("map%05d.wpm", i++);
  return file;
}

////////////////////////////////////////////////////////////////////
// Befehlsbehandlungsroutinen

BEGIN_MESSAGE_MAP(CMapProject, CDocument)
	//{{AFX_MSG_MAP(CMapProject)
	ON_COMMAND(ID_MAP_NEW, OnMapNew)
	ON_COMMAND(ID_MAP_OPEN, OnMapOpen)
	ON_UPDATE_COMMAND_UI(ID_MAP_OPEN, OnUpdateMapExists)
  ON_UPDATE_COMMAND_UI(ID_MAP_DELETE, OnUpdateMapExists)
	ON_COMMAND(ID_MAP_DELETE, OnMapDelete)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()


void CMapProject::OnMapOpen()
{
  OpenMap();
}; // OnMapOpen

void CMapProject::OnMapNew() 
{
  CreateMap();
}; // OnMapNew

void CMapProject::OnMapDelete() 
{
  DeleteMap();
};

void CMapProject::OnUpdateMapExists(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable( (GetNumMapDocs() > 0) );
}

BOOL CMapProject::OnOpenDocument( LPCTSTR lpszPathName )
{
  CFileStatus fileStatus;
  // existiert das Map-Verzeichnis bereits?, falls nein: Verzeichnis erstellen
  CString fileName( lpszPathName );
  CString path = BCE::MfcHelper::GetFileDirectory( fileName );
  if ( !CFile::GetStatus( path + "\\*.*", fileStatus ) )
    CreateDirectory( path, NULL );

  if ( CFile::GetStatus( fileName, fileStatus ) )
    return CDocument::OnOpenDocument( LPCTSTR(fileName) ); // bestehendes Dokument öffnen
  else
  { // neues Dokument erzeugen
    if ( !CDocument::OnNewDocument() )
      return FALSE;
		SetModifiedFlag( TRUE );
  };

  return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// Diagnose CMapProject

#ifdef _DEBUG
void CMapProject::AssertValid() const
{
	CDocument::AssertValid();
}

void CMapProject::Dump(CDumpContext& dc) const
{
	CDocument::Dump(dc);
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// Serialisierung CMapProject 

void CMapProject::Serialize(CArchive& ar)
{
  unsigned int i;

	if (ar.IsStoring())
  {
    ar << DWORD(2);
    ar << CString( "..." ); // aus kompabilitätsgründen hier noch was abspeichern: früher: Pfad aufs Projekt
		ar.WriteCount(m_strMapDocFiles.GetSize());
		for (i = 0; i < (DWORD)m_strMapDocFiles.GetSize(); i++)
			ar << m_strMapDocFiles[i];
		ar.WriteCount(m_strMapDocNames.GetSize());
		for (i = 0; i < (DWORD)m_strMapDocNames.GetSize(); i++)
			ar << m_strMapDocNames[i];
		ar.WriteCount(m_MapDocRects.GetSize());
		for (i = 0; i < (DWORD)m_MapDocRects.GetSize(); i++)
			ar << m_MapDocRects[i];
  }
	else
	{
    DWORD nVersion;
    ar >> nVersion;
    switch (nVersion)
    {
    case 2:
      {
        DWORD dwOldSize;
        CString str;
        ar >> str;  // früher wurde hier der Pfad gespeichert

        dwOldSize = ar.ReadCount();
        m_strMapDocFiles.SetSize(dwOldSize);
        for (i = 0; i < dwOldSize; i++)
          ar >> m_strMapDocFiles[i];
        dwOldSize = ar.ReadCount();
        m_strMapDocNames.SetSize(dwOldSize);
        for (i = 0; i < dwOldSize; i++)
          ar >> m_strMapDocNames[i];
        dwOldSize = ar.ReadCount();
        m_MapDocRects.SetSize(dwOldSize);
        for (i = 0; i < dwOldSize; i++)
          ar >> m_MapDocRects[i];
      }
      break;
      
    default:
      AfxThrowArchiveException(CArchiveException::badSchema);
      break;
    }; // switch nVersion
	}; // isStoring
}; // serialize

/////////////////////////////////////////////////////////////////////////////
// Befehle CMapProject 

void CMapProject::DeleteMapDocFiles( CString& file ) 
{
  std::vector<CString> shapeFiles;

  shapeFiles.push_back( CString( MAKEINTRESOURCE( IDS_PROFILES ) ) );
	shapeFiles.push_back( CString( MAKEINTRESOURCE( IDS_POINTS ) ) );
	shapeFiles.push_back( CString( MAKEINTRESOURCE( IDS_SEP ) ) );
	shapeFiles.push_back( CString( MAKEINTRESOURCE( IDS_FLOW ) ) );
  shapeFiles.push_back( CString( MAKEINTRESOURCE( IDS_BUHNEN ) ) );
	shapeFiles.push_back( CString( MAKEINTRESOURCE( IDS_WLEVEL ) ) );
  shapeFiles.push_back( CString( MAKEINTRESOURCE( IDS_PIVOT_SHP ) ) );
  shapeFiles.push_back( CString( MAKEINTRESOURCE( IDS_WLINES ) ) );
  shapeFiles.push_back( CString( MAKEINTRESOURCE( IDS_BANK_SHP ) ) );
  
	CMoDataConnection conn;
  if( !conn.CreateDispatch(TEXT("MapObjects2.DataConnection") ) )
    return;
	conn.SetDatabase( m_pProject->GetMapDir() );
  if( !conn.Connect() )
    return;

  CString name;
  BOOL bFound = FALSE;
	for( int i = 0; i < m_strMapDocFiles.GetSize(); i++)
	{
		if( file.CompareNoCase( m_strMapDocFiles[i] ) == 0 )
		{
			name = GetMapDocName( i );
			bFound = TRUE;
			break;
		}
	}

	if( bFound )
	{
    // alle Geodatasets finden, die weg müssen
    std::vector<CString> removeGeoDatasets;
    CMoGeoDatasets dataSets( conn.GetGeoDatasets() );
    for( int i = 0; i < dataSets.GetCount(); i++ )
    {
      CMoGeoDataset dataSet( dataSets.Item( CComVariant( i ) ) );
      CString gName = dataSet.GetName();
      
      for( std::vector<CString>::const_iterator sIt = shapeFiles.begin(); sIt != shapeFiles.end(); sIt++ )
      {
        if( gName.Find( name + *sIt ) == 0 ) // = gName.StartsWith
        {
          removeGeoDatasets.push_back( gName );
          break;
        }
      }
    }

    // jetzt löschen
    for( std::vector<CString>::const_iterator sIt = removeGeoDatasets.begin(); sIt < removeGeoDatasets.end(); sIt++ )
      conn.DeleteGeoDataset( *sIt );
	}
	conn.Disconnect();
}

BOOL CMapProject::OpenMap()
// zeigt den Kartenauswahldialog an und öffnet die selektierten Karten
// Rückgabewert:
//          TRUE, falls mindestens eine Karte geöffnet wurde     
{
  BOOL bReturn = FALSE;

  if ( m_strMapDocNames.GetSize() > 0 )
  {
    COpenMapDialog dlg( &m_strMapDocNames, &m_strMapDocFiles, AfxGetMainWnd(), FALSE );
    
    if ( dlg.DoModal() == IDOK )
    {
      for ( int i = 0; i < dlg.m_numbers.GetSize(); i++ )
      {
        if ( OpenMap( dlg.m_numbers[i] ) )
          bReturn = TRUE;
      };
    }; // if dlg.DoModal == IDOK
  } // if m_strMapDocNames.GetSize() > 0
  else
    AfxMessageBox( IDS_NO_MAPS_AVAILABLE );

  return bReturn;
}; // OpenMap

BOOL CMapProject::OpenMap( int i )
// öffnet die i. Karte
// Rückgabewert:
//          TRUE, falls Karte geöffnet werden konnte
{
  BOOL bReturn = FALSE;

  CFileStatus fileStatus;
  CString path = GetMapDocFile( i );
  if ( !CFile::GetStatus( path, fileStatus ) )
  {
    CString str;
    str.FormatMessage(IDS_ERROR_READFILE, path);
    AfxMessageBox(str, MB_OK | MB_ICONEXCLAMATION);
  }
  else
  {
    CMapDoc* mapDoc = (CMapDoc*)theApp.OpenDocumentFile( path );

    if ( mapDoc )
    {
      bReturn = TRUE;
      mapDoc->SetTitle( m_strMapDocNames[i] );
      mapDoc->SetProject( m_pProject );

      UpdateMapDoc( mapDoc ); // die Kartenübersicht aktualisieren
    }; // if mapDoc
  };

  return bReturn;
}; // OpenMap( i )

BOOL CMapProject::CloseMap( CMapDoc* pDoc )
// schliesst das Dokument und aktualisiert die Projektübersicht
{
  // falls die Druckvorschau offen ist, diese zuerst schliessen
  // da sonst mit OnClosedocument der MainFrame ( = der Parent der Vorschau )
  // zerstört wird

  // zuerst den MainFrame und die derzeit aktive View holen
  CFrameWnd* pParent = STATIC_DOWNCAST(CFrameWnd, AfxGetMainWnd());
  CView* pView = pParent->GetActiveView();

  // ist die aktive View eine Preview, diese jetzt schliessen
  if( pView && pView->IsKindOf( RUNTIME_CLASS(CMapPreview) ) )
    ((CMapPreview*)pView)->ClosePreview();

  // die restlichen Views ( es gibt nur eine ) wiederherstellen, weil sie vor der Preview
  // minimiert wurden
  POSITION pos = pDoc->GetFirstViewPosition();
  while( pos )
  {
    CMapView* pView = (CMapView*)pDoc->GetNextView( pos );
    pView->RestoreAfterPreview();
  }; // while pos


  if ( !pDoc->SaveModified() )
    return FALSE;

  // Projektübersicht aktualisieren bevor das Document zerstört wird
  UpdateOverview();

  // den Dateinamen merken
  CString fileName = pDoc->GetPathName();

  // das Dokument schliessen, ggfls. speichern, die Übersicht aktualisieren
  pDoc->OnCloseDocument();
  
  // testen, ob das Dokument gespeichert wurde, falls nein: aus der Übersicht löschen
  CFileStatus fileStatus;
  if ( !CFile::GetStatus( fileName, fileStatus ) )
  { // dokument aus übersicht löschen
    for ( int i = 0; i < m_strMapDocFiles.GetSize(); i++ )
    {
      if ( m_strMapDocFiles[i] == fileName )
      {
        m_strMapDocFiles.RemoveAt( i );
        m_strMapDocNames.RemoveAt( i );
        m_MapDocRects.RemoveAt( i );
      };
    }; // for i
  } // if CFile

  return TRUE;
}; // CloseMap

BOOL CMapProject::CloseMaps()
// schliesst alle offenen Karten
// ist bSave == TRUE, so wird vorher gefragt ob gespeichert werden soll
// falls eine der Karten nicht geschlossen werden konnte ( 'Abbrechen' )
// gibts FALSE zurück
{
  CMultiDocTemplate* pMultiDocTemplate = theApp.GetMapDocTemplate();
  POSITION pos = pMultiDocTemplate->GetFirstDocPosition();
  BOOL bErfolg = TRUE;

  while ( pos && bErfolg )
    bErfolg = CloseMap( (CMapDoc*)pMultiDocTemplate->GetNextDoc( pos ) );

  return bErfolg;
}; // CloseMaps()

void CMapProject::UpdateMapDoc( CMapDoc* mapDoc )
// aktualisiert die Daten des durch mapDoc gegebenen Dokuments
{
	int i;

	SetModifiedFlag(TRUE);
	
  for (i = 0; i < m_strMapDocFiles.GetSize(); i++)
	{
    CString docFileName = BCE::MfcHelper::GetFileName( mapDoc->GetPathName() );
    if ( m_strMapDocFiles[i].CompareNoCase( docFileName ) == 0 )
		{
      CMoRectangle extent( mapDoc->GetFullExtent() );
      m_MapDocRects[i] = CDoubleRect( extent.GetLeft(), extent.GetTop(), 
        extent.GetRight(), extent.GetBottom() );
      UpdateOverview();
			return;
		}
	}
};

void CMapProject::UpdateOverview()
{
  m_overviewBar->UpdateOverviewMap();
};

BOOL CMapProject::ReloadProject()
{
  if ( m_pProject )
  {
    if (m_pProject->Load() )
      return TRUE;
    else
    {
      delete m_pProject;
      m_pProject = NULL;
    };
  };
  return FALSE;
};

/**
 * ruft den Auswahldialog zum erstellen einer Neuen Karte auf
 * @return Zeiger auf das CMapDoc, falls die Karte erfolgreich erstellt werden konnte
 */
CMapDoc* CMapProject::CreateMap()
{
  CMapDoc* mapDoc = NULL;

  CNewMapDlg dlg( AfxGetApp()->m_pMainWnd , this );
  if( dlg.DoModal() == IDOK )
  {
	  CString file = GetNewMapDocFile();
	  CString mapFile = m_pProject->GetMapDir() + file;

	  mapDoc = (CMapDoc*)theApp.GetMapDocTemplate()->OpenDocumentFile( NULL, TRUE );
	  
	  mapDoc->SetPathName( mapFile, FALSE );
	  mapDoc->SetTitle( dlg.m_name );
	  mapDoc->SetModifiedFlag( TRUE );
	  mapDoc->OnSaveDocument( mapFile );
    
	  m_strMapDocNames.Add( dlg.m_name );
	  m_strMapDocFiles.Add( file );
	  int index = m_MapDocRects.Add( CDoubleRect() );

	  if ( !OpenMap( index ) )
	  {
		  delete mapDoc;
		  mapDoc = NULL;
	  };
	  
	  SetModifiedFlag( TRUE );
	  UpdateOverview();

	  // nach jeder neuen Karte Abspeichern ohne zu fragen ( Änderungen können durch nichtspeichern
	  DoFileSave(); 
  };

  return mapDoc;
}; // CreateMap

void CMapProject::DeleteMap()
{
	COpenMapDialog dlg( &m_strMapDocNames, &m_strMapDocFiles, AfxGetApp()->GetMainWnd(), TRUE );

	if ( dlg.DoModal() == IDOK )
	{
		for (int i = 0; i < dlg.m_numbers.GetSize(); i++)
		{
      //CString file = m_strMapDocFiles[ dlg.m_numbers[i] ];
      CString docPath = GetMapDocFile( dlg.m_numbers[i] );
      // überprüfen, ob dieses Dokument zur Zeit geöffnet ist und es ggfls. schliessen
      CMultiDocTemplate* pMapDocTemplate = theApp.GetMapDocTemplate();

      POSITION pos = pMapDocTemplate->GetFirstDocPosition();
      while ( pos )
      {
        CMapDoc* pDoc = (CMapDoc*)pMapDocTemplate->GetNextDoc( pos );

        if ( docPath.CompareNoCase( pDoc->GetPathName() ) == 0 )
        {
          pDoc->OnCloseDocument(); // schliessen ohne zu fragen, ob es gespeichert werden soll
          break; // ein Dokument reicht 
        }
      }; // while pos


      DeleteMapDocFiles( BCE::MfcHelper::GetFileName( docPath ) );
      m_strMapDocFiles[dlg.m_numbers[i]] = TEXT(""); // zum löschen markieren
      
      remove( docPath );
		}; // for i

    // jetzt die Einträge wirklich löschen
    for ( i = 0; i < m_strMapDocFiles.GetSize(); i++ )
    {
      if ( m_strMapDocFiles[i].IsEmpty() )
      {
        m_strMapDocFiles.RemoveAt( i );
        m_strMapDocNames.RemoveAt( i );
        m_MapDocRects.RemoveAt( i );
        i--;
      }; // if .IsEmpty
    }; // for i

    OnSaveDocument( GetPathName() ); // MapProjekt speichern, sonst kann der Nutzer beim beenden nicht speichern, und die Karten sind beim wiederladen angeblich wieder da, obwohl die dateien bereits gelöscht sind
    UpdateOverview();

    // nach jeder neuen Karte Abspeichern ohne zu fragen ( Änderungen können durch nichtspeichern
    DoFileSave(); 
  }; // if dlg.DoModal
}; // DeleteMap


/**
 * führt die durch Command gegebenen Befehle aus, wie z.B. Karte öffnen oder erstellen
 * @return TRUE, falls das Commando erfolgreich ausgeführt werden konnte
 */
BOOL CMapProject::ParseCommand( NMPROJECTMNG* command )
{
  BOOL bErfolg = FALSE;

  switch ( command->type )
  {
  case NMPROJECTMNG::openMap:
    bErfolg = OpenMap();
    break;

  case NMPROJECTMNG::createMap:
    {
      CMapDoc* pDoc = CreateMap();
      if ( pDoc )
        bErfolg = pDoc->ParseCommand( command );
    }; // case createMap
    break;

  case NMPROJECTMNG::deleteMap:
    DeleteMap();
    break;
  }; // else

  return bErfolg;
}; // ParseCommand