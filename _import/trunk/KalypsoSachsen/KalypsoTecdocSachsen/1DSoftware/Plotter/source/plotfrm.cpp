// PlotFrm.cpp : implementation of the CPlotterFrame class
//

#include "stdafx.h"

#include "resource.h"

#include "..\..\wspprj\wspprj.h"

#include "drawobj.h"
#include "profoverv.h"
#include "plotdoc.h"
#include "plotter.h"
#include "opendlg.h"
#include "prefdlg.h"
#include "openpdlg.h"
#include "multidoc.h"

#include "plotfrm.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CPlotterFrame

IMPLEMENT_DYNAMIC(CPlotterFrame, CMainFrame)

BEGIN_MESSAGE_MAP(CPlotterFrame, CMainFrame)
	//{{AFX_MSG_MAP(CPlotterFrame)
	ON_COMMAND(ID_FILE_OPEN, OnFileOpen)
	ON_COMMAND(ID_EXTRAS_PREF, OnExtrasPref)
	ON_COMMAND(ID_FILE_NEW, OnFileNew)
	ON_COMMAND(ID_FILE_OPENPROJEKT, OnFileOpenprojekt)
	ON_COMMAND(ID_FILE_PRINT_ALL, OnFilePrintAll)
	ON_COMMAND(ID_EXTRAS_STEMPEL, OnExtrasStempel)
	ON_COMMAND(ID_FILE_CLOSEPROJEKT, OnFileCloseprojekt)
	ON_UPDATE_COMMAND_UI(ID_FILE_CLOSEPROJEKT, OnUpdateFileCloseprojekt)
	ON_COMMAND(ID_FILE_DELETE, OnFileDelete)
	ON_UPDATE_COMMAND_UI(ID_FILE_DELETE, OnUpdateFileDelete)
	ON_WM_CREATE()
	ON_WM_CLOSE()
	//}}AFX_MSG_MAP
  ON_UPDATE_COMMAND_UI(ID_BROWSE_LEFT, OnUpdateEditBrowse)
  ON_UPDATE_COMMAND_UI(ID_BROWSE_RIGHT, OnUpdateEditBrowse)
  ON_UPDATE_COMMAND_UI(IDC_BROWSE_COMBO, OnUpdateEditBrowse)

  ON_COMMAND_EX(ID_BROWSE_LEFT, OnEditBrowse)
  ON_COMMAND_EX(ID_BROWSE_RIGHT, OnEditBrowse)
  ON_COMMAND_EX(IDC_BROWSE_COMBO, OnEditBrowse)

  ON_COMMAND_EX(ID_VIEW_PLOTBAR, OnBarCheck)
  ON_UPDATE_COMMAND_UI(ID_VIEW_PLOTBAR, OnUpdateControlBarMenu)

  ON_COMMAND( ID_FILE_OVERVIEW, OnShowOverview )
  ON_UPDATE_COMMAND_UI( ID_FILE_OVERVIEW, OnUpdateShowOverview )

END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CPlotterFrame diagnostics

#ifdef _DEBUG
void CPlotterFrame::AssertValid() const
{
	CMainFrame::AssertValid();
}

void CPlotterFrame::Dump(CDumpContext& dc) const
{
	CMainFrame::Dump(dc);
}

#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CPlotterFrame message handlers

void CPlotterFrame::OnUpdateFrameTitle(BOOL bAddToTitle)
{
	if ((GetStyle() & FWS_ADDTOTITLE) == 0)
		return;     // leave it alone!

#ifndef _AFX_NO_OLE_SUPPORT
	// allow hook to set the title (used for OLE support)
	if (m_pNotifyHook != NULL)
	{
		CMDIFrameWnd::OnUpdateFrameTitle(bAddToTitle);
		return;
	}
#endif

	CString str;

	if( GETPLOTTERAPP->GetProject() )
	{
		str = GETPLOTTERAPP->GetProject()->GetDir();
		str.Format("[%s]", str);
	}
	else
		str.Empty();

	CMDIChildWnd* pActiveChild;
	CDocument* pDocument = GetActiveDocument();
	if (bAddToTitle &&
	  (pActiveChild = MDIGetActive()) != NULL &&
	  (pActiveChild->GetStyle() & WS_MAXIMIZE) == 0 &&
	  (pDocument != NULL ||
	   (pDocument = pActiveChild->GetActiveDocument()) != NULL))
	{
		str += " - ";
		str += pDocument->GetTitle();
	}
	if (!str.IsEmpty())
		UpdateFrameTitleForDocument(str);
	else
		UpdateFrameTitleForDocument(NULL);
}

void CPlotterFrame::OnFileOpen() 
{
  if( GETPLOTTERAPP->GetProject() == NULL )
    GETPLOTTERAPP->OnFileOpen();
  else
    OpenFiles( TRUE );
}; // OnFileOpen

void CPlotterFrame::OpenFiles( BOOL bAutoFileName )
{
  if( !GETPLOTTERAPP->GetProject() )
    return;

  COpenDialog dlg( this, FALSE, FALSE, TRUE );
  if( dlg.DoModal() == IDOK )
  {
    CWaitCursor wait;

    // welche Sections wurden ausgewählt??
    SectionArray secArray;
    for( int i = 0; i < dlg.m_Sections.GetSize(); i++ )
      secArray.Add( dlg.m_Sections[i] );

    if( dlg.GetMultiPlot() == FALSE )
      CreatePlot( dlg.m_pState, secArray, bAutoFileName );
    else
      GETPLOTTERAPP->CreateMultiPlot( secArray, bAutoFileName );
  }; // if dlg.DoModal
}; // OpenFiles

void CPlotterFrame::CreatePlot( State* pState, const SectionArray& secArray, BOOL bAutoFileName )
{
  GETPLOTTERAPP->m_pState = pState;
  GETPLOTTERAPP->m_Sections.RemoveAll();
  for( int i = 0; i < secArray.GetSize(); i++ )
    GETPLOTTERAPP->m_Sections.SetAtGrow( i, secArray[i] );
    
  for( i = 0; i < GETPLOTTERAPP->m_Sections.GetSize(); i++ )
  {
    CString path;
    if( bAutoFileName )
      GETPLOTTERAPP->GetDrawingFileName( path, GETPLOTTERAPP->m_Sections[i] );
    
    GETPLOTTERAPP->m_nCount = i;
    GETPLOTTERAPP->OpenDocumentFile( path );
  } // for i
  
  GETPLOTTERAPP->m_Sections.RemoveAll();
  GETPLOTTERAPP->m_nCount = -1;
} // CreatePlot

void CPlotterFrame::OnExtrasPref() 
{
	CPrefDialog dlg(this);

	dlg.DoModal();
}

void CPlotterFrame::OnFileOpenprojekt() 
{
	COpenProjectDialog dlg(this);

	if (dlg.DoModal()==IDOK)
	{
		CWaitCursor wait;
		CloseProject();
		GETPLOTTERAPP->LoadProject(dlg.m_dir);
    if( GETPLOTTERAPP->GetProject()->IsLoaded() )
  		OnFileOpen();
	}
}

void CPlotterFrame::OnFileNew() 
{
	if( GETPLOTTERAPP->GetProject() == NULL )
    GETPLOTTERAPP->OnFileNew();
  else
    OpenFiles( FALSE );
} // OnFileNew

void CPlotterFrame::OnFilePrintAll() 
{
	CFrameWnd *pActiveWnd, *pWnd;
	BOOL bMaximized;
	CPlotterDoc *pDoc;

	pActiveWnd = MDIGetActive(&bMaximized);
	if (pActiveWnd!=NULL)
	{
		pDoc = (CPlotterDoc*)pActiveWnd->GetActiveDocument();
		if (pDoc!=NULL)
			pActiveWnd->SendMessage(WM_COMMAND, ID_FILE_PRINT_DIRECT);
		MDINext();
		pWnd=MDIGetActive(&bMaximized);
		while (pWnd!=pActiveWnd)
		{
			if (pWnd!=NULL)
			{
				pDoc = (CPlotterDoc*)pWnd->GetActiveDocument();
				if (pDoc!=NULL)
					pWnd->SendMessage(WM_COMMAND, ID_FILE_PRINT_DIRECT);
				pWnd = pWnd->m_pNextFrameWnd;
			}
			MDINext();
			pWnd=MDIGetActive(&bMaximized);
		}
	}
}

void CPlotterFrame::OnExtrasStempel() 
{
	STARTUPINFO sui;
	PROCESS_INFORMATION pi;
	
	::GetStartupInfo(&sui);
	sui.lpReserved = NULL;
	sui.lpTitle = "STEMPEL.EXE";
	sui.dwFlags |= STARTF_USESHOWWINDOW;
	sui.wShowWindow = SW_SHOWNORMAL;
	BOOL bSuccess = ::CreateProcess(NULL, "stempel.exe", NULL, NULL, TRUE, NORMAL_PRIORITY_CLASS, NULL, NULL, &sui, &pi);
#ifdef _DEBUG
	if (!bSuccess)
	{
		TRACE1("Warning: CreateProcess failed: GetLastError returns 0x%8.8X\n",
			GetLastError());
	}
#endif
}

void CPlotterFrame::CloseProject()
{
  // jetzt alle Fenster mit Profilen aus diesem Projekt schliessen
  if( GETPLOTTERAPP->GetProject() )
	{
    BOOL bMaximized;
		
    CFrameWnd* pWnd = MDIGetActive( &bMaximized );
		while( pWnd )
		{
			CDocument* pDoc = pWnd->GetActiveDocument();
      if( pDoc )
      {
        if ( !pDoc->SaveModified() )
          return;
        pDoc->OnCloseDocument();
      }
      else
        pWnd->SendMessage( WM_CLOSE, 0, 0 );
      
      CFrameWnd* pNextWnd = MDIGetActive( &bMaximized );
      if( pWnd == pNextWnd )
        break;

      pWnd = pNextWnd;
    }; // while pWnd
  }; // m_pProject

  // die Profilübersichten löschen
  for( int i = 0; i < m_profilOverviews.GetSize(); i++ )
  {
    CProfilOverview* pO = m_profilOverviews[i];
    if ( pO->GetSafeHwnd() )
      pO->SendMessage( WM_CLOSE );
    delete pO;
  };
  m_profilOverviews.RemoveAll();

  // auch die ComboBox in der Toolbar löschen
  if ( m_browseToolbar.GetSafeHwnd() )
  {
    CComboBox* comboBox = m_browseToolbar.GetComboBox();
    if ( comboBox )
    {
      comboBox->ResetContent();
      comboBox->EnableWindow( FALSE );
    };
  };


  GETPLOTTERAPP->DeleteProject();

  OnUpdateFrameTitle( TRUE );
}; // CloseProject

void CPlotterFrame::OnFileCloseprojekt() 
{
	CloseProject();
}

void CPlotterFrame::OnUpdateFileCloseprojekt(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable( GETPLOTTERAPP->GetProject() != NULL );
}


void CPlotterFrame::OnFileDelete() 
{
	COpenDialog dlg( this, FALSE, TRUE, FALSE );
	CString path;
	Section *sec;
	int i;
	CFile file;
	CFileStatus rStatus;
	
	if( GETPLOTTERAPP->GetProject() == NULL )
		return;
	if (dlg.DoModal()==IDOK)
	{
		SHFILEOPSTRUCT shfos;
		char *files, *lpStr;
		char null = '\0';
		int nInc = 0;
		
		files = (char*)malloc((dlg.m_Sections.GetSize()*MAX_PATH+1)*sizeof(char));
		files[0] = '\0';
		shfos.hwnd = GetSafeHwnd();
		shfos.wFunc = FO_DELETE;
		shfos.pTo = "";
		shfos.fFlags = FOF_ALLOWUNDO;
		shfos.hNameMappings = NULL;
		shfos.lpszProgressTitle = "";
		for (i=0; i<dlg.m_Sections.GetSize(); i++)
		{
			sec = dlg.m_Sections[i];
			GETPLOTTERAPP->GetDrawingFileName(path, sec);
			if (file.GetStatus(path, rStatus))
			{
				lpStr = &files[nInc];
				lstrcpy(lpStr, path.GetBuffer(path.GetLength()));
				path.ReleaseBuffer();
				nInc += strlen(lpStr)+1;
				files[nInc] = '\0';
			}
		}
		files[nInc+1] = '\0';
		shfos.pFrom = files;
		SHFileOperation(&shfos);
		free(files);
	}
}

void CPlotterFrame::OnUpdateFileDelete(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable( GETPLOTTERAPP->GetProject() != NULL );
}

int CPlotterFrame::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	if (CMainFrame::OnCreate(lpCreateStruct) == -1)
		return -1;
	
  if ( !m_browseToolbar.Init( this, ID_EDIT_BROWSE, IDC_BROWSE_COMBO, ID_VIEW_PLOTBAR,
        WS_CHILD | WS_VISIBLE | CBRS_TOP | CBRS_TOOLTIPS | CBRS_FLYBY | CBRS_SIZE_DYNAMIC, 
        IDR_PLOTTERTYPE ) )
    return FALSE;

  m_browseToolbar.EnableDocking( CBRS_ALIGN_TOP | CBRS_ALIGN_BOTTOM );

  DockControlBar( &m_browseToolbar, AFX_IDW_DOCKBAR_TOP );

  LoadBarState( _T( "Plotter-Settings-V2" ) );

	return 0;
}; // OnCreate

void CPlotterFrame::OnClose()
{
  SaveBarState( _T( "Plotter-Settings-V2" ) );
  
	CMainFrame::OnClose();
}; // OnClose

BOOL CPlotterFrame::OnEditBrowse( UINT nID )
{
  CMDIChildWnd* pChildWnd = MDIGetActive();
  if (pChildWnd)
  {
    CPlotterDoc* pDoc = (CPlotterDoc*)pChildWnd->GetActiveDocument();

    Section* section = pDoc->GetMSection( 0 );
    if ( section && GETPLOTTERAPP->GetProject() && GETPLOTTERAPP->m_pState )
    {
      if (section->GetClassType() == CLASS_TYPE_CSECTION)
      {
        CrossSection* cs = (CrossSection*)section;
  
        CComboBox* bCombo = GetBrowseCombo();
        int index = -1;

        if ( bCombo && bCombo->GetSafeHwnd() )
        {
          switch ( nID )
          {
          case IDC_BROWSE_COMBO:
            index = bCombo->GetCurSel();
            break;
            
          case ID_BROWSE_LEFT:
            index = max( 0, bCombo->GetCurSel() - 1 ); 
            break;
            
          case ID_BROWSE_RIGHT:
            index = min( bCombo->GetCount() - 1, bCombo->GetCurSel() + 1 );
            break;
            
          default:
            return FALSE;  // diese ID muss an einer anderen Stelle behandelt werden
          }; // switch
        }; // if bcombo
  
        if ( index != - 1 )
        {
          // muss vor OnClosedocument stehen, da dieses den
          // Inhalt der Combo ändert
          CrossSection* newCs = (CrossSection*)bCombo->GetItemDataPtr( index );

          // altes Dokument schliessen 
          if ( pDoc->SaveModified() )
            pDoc->OnCloseDocument();

          // neues Document öffnen
          OpenCrossSection( newCs );

          bCombo->SetCurSel( index );
        };
      }; // if section == CLASS_TYPE_CSECTION
    }; // if section && GETPLOTTERAPP->m_pProject && GETPLOTTERAPP->m_pState
  }; // if pChildWnd

  if ( nID == ID_EDIT_BROWSE || nID == ID_BROWSE_LEFT || nID == ID_BROWSE_RIGHT)
    return TRUE;
  else
    return FALSE;
}; // OnEditBrowse()

void CPlotterFrame::OnUpdateEditBrowse( CCmdUI* pCmdUI ) 
{
	BOOL bEnable = FALSE;

  CComboBox* bCombo = GetBrowseCombo();

  if( bCombo != NULL && bCombo->GetCount() > 0 ) // sonst niemals aktivieren
  {
    switch( pCmdUI->m_nID )
    {
    case IDC_BROWSE_COMBO:
      bEnable = TRUE;
      break;

    case ID_BROWSE_LEFT:
      bEnable = bCombo->GetCurSel() != 0;
        break;
        
    case ID_BROWSE_RIGHT:
      bEnable = bCombo->GetCurSel() < bCombo->GetCount() - 1;
      break;
    }; // switch
  }; // if count > 0

  pCmdUI->Enable( bEnable );
};

void CPlotterFrame::OpenCrossSection( CrossSection* cs )
{
  ASSERT( cs );
  
  CString drawName;
          
  int index = GETPLOTTERAPP->m_Sections.Add( cs );
  int oldCount = GETPLOTTERAPP->m_nCount;
  GETPLOTTERAPP->m_nCount = index;
  
  GETPLOTTERAPP->GetDrawingFileName( drawName, cs );
  GETPLOTTERAPP->OpenDocumentFile( drawName );
  
  GETPLOTTERAPP->m_Sections.RemoveAt( index );
  GETPLOTTERAPP->m_nCount = oldCount;
}; // OpenCrossSection()

void CPlotterFrame::OnUpdateShowOverview( CCmdUI* pCmdUI )
{
  BOOL bEnable = FALSE;
  if ( GETPLOTTERAPP->GetProject() )
    bEnable = TRUE;
  pCmdUI->Enable( bEnable );
}; // OnUpdateShowOverview

void CPlotterFrame::OnShowOverview()
{
  // zuerst rausfinden welcher Zustand gezeigt werden soll
  Project* project = GETPLOTTERAPP->GetProject();

  ASSERT(project);

  StateChooser stDlg( project, this );
  if ( stDlg.DoModal() == IDOK )
  {
    State* state = stDlg.GetCurrentState();
    if ( state )
    {
      CProfilOverview* pOver = new CProfilOverview;
      pOver->Create( this );
      pOver->SetState( state );
      pOver->SetTopLeftSection( state->GetFirstCrossSection() );
      m_profilOverviews.Add( pOver );
    }; // state
  }; // DoModal

}; // OnShowOverview

void CPlotterFrame::SetBrowseCombo( State* state, CrossSection* activeCs )
// füllt die ComboBox der BrowseToolBar
// Parameter:
//        State* state: die Stationen dieses Zustandes werden angezeigt
//        CrossSection* cs: dieses Querprofil soll ausgewählt sein
{
  CComboBox* comboBox = GetBrowseCombo();
  if ( !comboBox || !comboBox->GetSafeHwnd() )
    return;

  // erstmal löschen
  comboBox->ResetContent();

  if ( state )
  {
    CrossSection* cs = state->GetFirstCrossSection();
    while ( cs )
    {
      CString stationStr;
      stationStr.Format( "%.4lf", cs->GetStation() );
      int index = comboBox->AddString( stationStr );
      comboBox->SetItemDataPtr( index, cs );
      if ( cs == activeCs )
        comboBox->SetCurSel( index );

      cs = state->GetNextCrossSection();
    }; // while cs
  }; // state

}; // SetBrowseCombo