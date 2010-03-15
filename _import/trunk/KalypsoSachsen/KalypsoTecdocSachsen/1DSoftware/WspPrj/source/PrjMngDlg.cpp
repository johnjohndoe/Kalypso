/*! Time-stamp: <@(#)PrjMngDlg.cpp   29.08.02 - 09:50:06   Belger>
*********************************************************************
*  @file   : PrjMngDlg.cpp
*
*  Project : WSPWIN
*
*  Package : ProjektManager
*
*  Company : BCE
*
*  Author  : Belger                              Date: 29.08.02
*
*  Purpose : Implementation of methods for class ProjectManagerDialog
*
*********************************************************************
*/
// PrjMngDlg.cpp: Implementierungsdatei
//
#pragma warning(disable:4786)
#pragma warning(disable:4503)


#include "stdafx.h"

#include "..\..\commonMfc\commonMfc.h"
#include "bce\include\WSPFeatures.h"

#include "statedlg.h"
#include "csregion.h"
#include "projectlist.h"
#include "nmprojmng.h"
#include "project.h"
#include "dirdlg.h"
#include "state.h"
#include "profil.h"
#include "lsection.h"
#include "csection.h"
#include "calc.h"
#include "outflow.h"
#include "datablocktypechooserdialog.h"


#include "prjmngdlg.h"


#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define TYPE_DOUBLE		0
#define TYPE_STRING		1

static CListCtrl *listCtrl;
static int col_types[4];
static DWORD listcount = 0; // um den Zeilen der Listboxen eindeutige Werte zuzuordnen (für CompareFunction)

int CALLBACK ListCompareFuncMng(LPARAM lParam1, LPARAM lParam2, LPARAM lParamSort)
// Vergleichsfunktion für die ListCtrl's
// Parameter:
//        LPARAM lParam1, lParam2: die beiden zu vergleichenden Objekte
//        LPARAM lParamSort: der über CListCtrl->SortItems() übergebene Parameter:
//                      LOWORD(lParamSort): zu sortierende Spalte
//                      HIWORD(lParamSort): falls 0 wird vorwärts, sonst rückwärts sortiert
{
	LV_FINDINFO lvFindInfo;
	int iItem1, iItem2;
	int nResult = 0;
	CString str1, str2;
	WORD index = LOWORD(lParamSort);
	WORD direction = HIWORD(lParamSort);
	
	lvFindInfo.flags = LVFI_PARAM;
	lvFindInfo.psz = NULL;
	lvFindInfo.lParam = lParam1;
	iItem1 = listCtrl->FindItem(&lvFindInfo);
	lvFindInfo.lParam = lParam2;
	iItem2 = listCtrl->FindItem(&lvFindInfo);
	
	str1 = listCtrl->GetItemText(iItem1, index);
	str2 = listCtrl->GetItemText(iItem2, index);
	if (col_types[index]==TYPE_DOUBLE)
	{
		double a, b;
		
		a = atof(str1);
		b = atof(str2);
		if (a<b)
			nResult = -1;
		else if (a>b)
			nResult = 1;
		else
			nResult = 0;
	}
	else
		nResult = str1.CompareNoCase(str2);
	
	if ( direction != 0 )
		nResult *= -1;
	
	return nResult;
}
/////////////////////////////////////////////////////////////////////////////
// Dialogfeld ProjectManagerDialog 


ProjectManagerDialog::ProjectManagerDialog(CWnd* pParent /*=NULL*/)
: CDialog(ProjectManagerDialog::IDD, pParent)
{
	//{{AFX_DATA_INIT(ProjectManagerDialog)
	//}}AFX_DATA_INIT
	m_lastSortDirection = 0;
	m_wspProjects = NULL;
	m_command = NULL;
}

ProjectManagerDialog::~ProjectManagerDialog()
{
	DeleteContents();
}

ProjectManagerDialog::DeleteContents()
{
	if ( m_wspProjects )
	{
		delete m_wspProjects;
		m_wspProjects = NULL;
	};
	
	DeleteStateTree();
}; // DeleteContents


void ProjectManagerDialog::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(ProjectManagerDialog)
	DDX_Control(pDX, IDC_STATIC_PROJEKTE, m_staticProjekte);
	DDX_Control(pDX, IDC_STATIC_PROFILE, m_staticProfile);
	DDX_Control(pDX, IDC_PROJECT_LIST, m_projectList);
	DDX_Control(pDX, IDC_STATE_TREE, m_stateTree);
	DDX_Control(pDX, IDC_DATA_LIST, m_dataList);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(ProjectManagerDialog, CDialog)
//{{AFX_MSG_MAP(ProjectManagerDialog)
ON_NOTIFY(NM_CLICK, IDC_PROJECT_LIST, OnClickProjectList)
ON_NOTIFY(LVN_COLUMNCLICK, IDC_PROJECT_LIST, OnColumnclickProjectList)
ON_NOTIFY(LVN_COLUMNCLICK, IDC_DATA_LIST, OnColumnclickDataList)
ON_NOTIFY(TVN_SELCHANGED, IDC_STATE_TREE, OnSelchangedStateTree)
ON_NOTIFY(LVN_KEYDOWN, IDC_PROJECT_LIST, OnKeydownProjectList)
ON_WM_KEYDOWN()
ON_WM_SIZE()
ON_NOTIFY(LVN_ENDLABELEDIT, IDC_PROJECT_LIST, OnEndlabeleditProjectList)
ON_NOTIFY(LVN_ITEMCHANGED, IDC_PROJECT_LIST, OnItemchangedProjectList)
//}}AFX_MSG_MAP

ON_MESSAGE_VOID( WM_KICKIDLE, OnKickIdle ) // für update_command_ui handling

ON_COMMAND_EX(ID_MNG_DATENIMPORT_DA66, OnDatenimport)
ON_COMMAND_EX(ID_MNG_DATENIMPORT_DA50, OnDatenimport)
ON_COMMAND_EX(ID_MNG_DATENIMPORT_WSV, OnDatenimport)
ON_COMMAND_EX(ID_MNG_DATENIMPORT_TRIPPLE, OnDatenimport)
ON_COMMAND_EX(ID_MNG_DATENIMPORT_HYK, OnDatenimport)
ON_COMMAND_EX(ID_MNG_DATENIMPORT_WST, OnDatenimport)
ON_COMMAND_EX(ID_MNG_DATENIMPORT_RELI, OnDatenimport)
ON_COMMAND_EX(ID_MNG_DATENEXPORT_DA66, OnDatenexport)
ON_COMMAND_EX(ID_MNG_DATENEXPORT_TRIPPLE, OnDatenexport)
ON_COMMAND_EX(ID_MNG_DATENEXPORT_CSV, OnDatenexport)
ON_COMMAND_EX(ID_MNG_DATENEXPORT_HYK, OnDatenexport)
ON_COMMAND_EX(ID_MNG_DATENEXPORT_RELI, OnDatenexport)

ON_UPDATE_COMMAND_UI( ID_MNG_DATENIMPORT_DA66, OnUpdateDatenimport )
ON_UPDATE_COMMAND_UI( ID_MNG_DATENIMPORT_DA50, OnUpdateDatenimport )
ON_UPDATE_COMMAND_UI( ID_MNG_DATENIMPORT_WSV, OnUpdateDatenimport )
ON_UPDATE_COMMAND_UI( ID_MNG_DATENIMPORT_TRIPPLE, OnUpdateDatenimport )
ON_UPDATE_COMMAND_UI( ID_MNG_DATENIMPORT_HYK, OnUpdateDatenimport )
ON_UPDATE_COMMAND_UI( ID_MNG_DATENIMPORT_WST, OnUpdateDatenimport )
ON_UPDATE_COMMAND_UI( ID_MNG_DATENIMPORT_RELI, OnUpdateDatenimport )

ON_UPDATE_COMMAND_UI( ID_MNG_DATENEXPORT_DA66, OnUpdateDatenexport )
ON_UPDATE_COMMAND_UI( ID_MNG_DATENEXPORT_TRIPPLE, OnUpdateDatenexport )
ON_UPDATE_COMMAND_UI( ID_MNG_DATENEXPORT_CSV, OnUpdateDatenexport )
ON_UPDATE_COMMAND_UI( ID_MNG_DATENEXPORT_HYK, OnUpdateDatenexport )
ON_UPDATE_COMMAND_UI( ID_MNG_DATENEXPORT_RELI, OnUpdateDatenexport )

ON_COMMAND( ID_MNG_OPEN_PROJECT, OnManagerOpenProject)
ON_COMMAND_EX(ID_MNG_ADD_PROJECT, OnManagerAddProject)
ON_COMMAND_EX(ID_MNG_ADD_PROJECT_ENTRY, OnManagerAddProject)
ON_COMMAND_EX(ID_MNG_DELETE_PROJECT, OnManagerDeleteProject)
ON_COMMAND_EX(ID_MNG_DELETE_PROJECT_ENTRY, OnManagerDeleteProject)
ON_COMMAND( ID_MNG_PRINT, OnManagerPrintProject )

ON_UPDATE_COMMAND_UI( ID_MNG_OPEN_PROJECT, OnUpdateProject)
ON_UPDATE_COMMAND_UI( ID_MNG_ADD_PROJECT, OnUpdateProject )
ON_UPDATE_COMMAND_UI( ID_MNG_ADD_PROJECT_ENTRY, OnUpdateProject )
ON_UPDATE_COMMAND_UI( ID_MNG_DELETE_PROJECT, OnUpdateProject )
ON_UPDATE_COMMAND_UI( ID_MNG_DELETE_PROJECT_ENTRY, OnUpdateProject )
ON_UPDATE_COMMAND_UI( ID_MNG_PRINT, OnUpdateProject )

ON_COMMAND_EX(ID_MNG_ADD_STATE, OnManagerState)
ON_COMMAND_EX(ID_MNG_DELETE_STATE, OnManagerState)
ON_COMMAND_EX(ID_MNG_COPY_STATE, OnManagerState)

ON_UPDATE_COMMAND_UI( ID_MNG_ADD_STATE, OnUpdateState )
ON_UPDATE_COMMAND_UI( ID_MNG_DELETE_STATE, OnUpdateState )
ON_UPDATE_COMMAND_UI( ID_MNG_COPY_STATE, OnUpdateState )

ON_COMMAND_EX( ID_MNG_INSERT_WSP, OnManagerWsp )
ON_COMMAND_EX( ID_MNG_DELETE_WSP, OnManagerWsp )
ON_COMMAND_EX( ID_MNG_LS_EXPORT_TABLE, OnManagerWsp )

ON_UPDATE_COMMAND_UI( ID_MNG_INSERT_WSP, OnUpdateWsp )
ON_UPDATE_COMMAND_UI( ID_MNG_DELETE_WSP, OnUpdateWsp )
ON_UPDATE_COMMAND_UI( ID_MNG_LS_EXPORT_TABLE, OnUpdateWsp )

ON_COMMAND_EX( ID_MNG_CREATE_MAP, OnManagerMap)
ON_COMMAND_EX( ID_MNG_OPEN_MAP, OnManagerMap)
ON_COMMAND_EX( ID_MNG_DELETE_MAP, OnManagerMap)

ON_UPDATE_COMMAND_UI( ID_MNG_CREATE_MAP, OnUpdateMap)
ON_UPDATE_COMMAND_UI( ID_MNG_OPEN_MAP, OnUpdateMap)
ON_UPDATE_COMMAND_UI( ID_MNG_DELETE_MAP, OnUpdateMap)

ON_COMMAND_EX( ID_MNG_MENU_CS_DELETE_DB, OnManagerCS )
ON_UPDATE_COMMAND_UI( ID_MNG_MENU_CS_DELETE_DB, OnManagerCS )
ON_COMMAND_EX( ID_MNG_MENU_CS_REFLECT, OnManagerCS )
ON_UPDATE_COMMAND_UI( ID_MNG_MENU_CS_REFLECT, OnUpdateCS )
ON_UPDATE_COMMAND_UI( ID_MNG_CREATE_LSECTION, OnUpdateCS )
ON_COMMAND( ID_MNG_CREATE_LSECTION, OnCreateLSection )


ON_COMMAND(ID_MNG_CLOSE, OnClose)
ON_WM_CLOSE()

END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten ProjectManagerDialog 
/////////////////////////////////////////////////////////////////////////////

// Windows Nachrichten

BOOL ProjectManagerDialog::OnInitDialog() 
{
	CString str;
	int width;
	CRect rect;
	
	CDialog::OnInitDialog();
	
	// Icon in Titelbar
	HICON hicon = AfxGetApp()->LoadIcon(IDI_MNG_ICON);
	ASSERT( hicon );
	SetIcon( hicon, TRUE );
	
	
	// Menubar erstellen
	menuBar.LoadMenu( IDR_PROJECT_MANAGER );
	
	SetMenu( &menuBar );
	
	// wsp.prj laden
	m_wspProjects = new ProjectList;
	if ( !m_wspProjects->LoadIsValid() )
		AfxMessageBox( IDS_OPEN_WSPPRJ_ERROR, MB_WARN );
	
	CImageList* pImageList = CCommonImageList::GetList( FALSE );
	
	// Projektliste initialisieren und füllen
	m_projectList.SetExtendedStyle( m_projectList.GetExtendedStyle() | LVS_EX_FULLROWSELECT | LVS_EX_GRIDLINES );
	m_projectList.SetImageList( pImageList, LVSIL_SMALL );
	m_projectList.GetClientRect(&rect);
	width = rect.Width()/2;
	str.LoadString(IDS_NAME);
	m_projectList.InsertColumn(0, (LPCTSTR)str, LVCFMT_LEFT, width, 0);
	str.LoadString(IDS_DIRECTORY);
	m_projectList.InsertColumn(1, (LPCTSTR)str, LVCFMT_LEFT, width, 1);
	
	m_projectList.SetFocus();
	
	UpdateProjectList( m_wspProjects );
	
	// Zustandsbaum initialisieren
	m_stateTree.SetImageList( pImageList, TVSIL_NORMAL);
	
	// Datenliste initialisieren
	m_dataList.SetExtendedStyle( m_dataList.GetExtendedStyle() | LVS_EX_FULLROWSELECT );
	
	m_dataList.SetImageList( pImageList, LVSIL_SMALL);
	m_dataList.GetClientRect(&rect);
	width = rect.Width()/4;
	
	m_dataList.InsertColumn(0, "", LVCFMT_LEFT, width, 0);
	m_dataList.InsertColumn(1, "", LVCFMT_LEFT, width, 1);
	m_dataList.InsertColumn(2, "", LVCFMT_LEFT, width, 2);
	m_dataList.InsertColumn(3, "", LVCFMT_LEFT, width, 3);
	
	UpdateTitle();
	
	return FALSE;  // return TRUE unless you set the focus to a control
	// EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}; // OnInitDialog


void ProjectManagerDialog::OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags) 
{
	// wird nie erreicht! Warum??
	
	CDialog::OnKeyDown(nChar, nRepCnt, nFlags);
}

void ProjectManagerDialog::OnClose()
{
	NMPROJECTMNG* command = new NMPROJECTMNG( NMPROJECTMNG::closeProgram );
	CloseManager( command );
};


void ProjectManagerDialog::OnOK()
{
	// überschreibt den Default: Enter führt nicht mehr zum Verlassen des PM
}; // OnOk

void ProjectManagerDialog::OnCancel()
{
	// Escape führt nicht mehr zum schliessen des PM
};

void ProjectManagerDialog::OnSize(UINT nType, int cx, int cy) 
{
	CDialog::OnSize(nType, cx, cy);
	
	ArrangeControls();
}; // OnInitDialog


////////////////////////////
// Menü/Toolbar Kommandos //
////////////////////////////

void ProjectManagerDialog::OnCreateLSection()
// Handler des Menus 'Längsschnitt->Erzeugen'
{
	CrossSectionArray* csArray = GetSelectedCrossSections();
	if( !csArray )
		return;
	
	CFileDialog dlg( FALSE, "lng", NULL, OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT, "Längsschnitt (*.lng)|*.lng||", this );
	if ( dlg.DoModal() != IDOK )
		return;
	
	LengthSection* ls = csArray->CreateLSection();
	delete csArray;
	csArray = NULL;
	
	CString path = dlg.GetPathName();
	ls->SetFileName( path, false );
	CString ext = ls->GetFileExt();
	
	Profil* profil = ls->GetProfil();
	if( !profil || !profil->Save( path ) )
		AfxMessageBox( "Längsschnitt wurde nicht gespeichert !", MB_ERROR );
	else
		AfxMessageBox( "Längsschnitt erfolgreich erzeugt", MB_ICONINFORMATION );
	
	profil = NULL; // vergessen
	delete ls;
	ls = NULL;
}; // OnCreateLSection  


void ProjectManagerDialog::OnKickIdle()
{
	UpdateDialogControls( this, FALSE );
	
	if ( menuBar.GetSafeHmenu() )
	{
		CmdRouteMenu( this, &menuBar );
		DrawMenuBar(); // sonst werden die Menuitems der obersten Ebene nicht richtig dargestellt ( für das 'simple Menu' )
	};
}; // OnKickIdle

void ProjectManagerDialog::OnManagerOpenProject()
// öffnet das aktuelle Project
{
	Project* project = GetSelectedProject();
	if ( project )
	{
		NMPROJECTMNG* command = new NMPROJECTMNG( NMPROJECTMNG::openProject, project->GetDir() );
		CloseManager( command );
	}; // if project
};

void ProjectManagerDialog::OnManagerPrintProject()
{
	Project* project = GetSelectedProject();
	
	CFileDialog fd( FALSE, NULL, NULL, OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT, NULL, this );
	if( fd.DoModal() == IDOK )
	{
		CWaitCursor;
		CStringArray strings;
		project->CreatePrintSummary( strings );
		
		try
		{
			CStdioFile output( fd.GetPathName(), CFile::modeWrite | CFile::modeCreate );
			
			for( int i = 0; i < strings.GetSize(); i++ )
				output.WriteString( strings[i]  + "\n" );
			
			output.Close();
		}
		catch( CFileException& fe )
		{
			TCHAR szCause[255];
			
			fe.GetErrorMessage( szCause, 255 );
			
			AfxMessageBox( szCause );
		}
	}
}

BOOL ProjectManagerDialog::OnManagerAddProject( UINT nID ) 
// neues Projekt erzeugen und zur Liste hinzufügen oder neuen Eintrag hinzufügen
{
	UINT title, errorMsg;
	BOOL exists;
	
	switch ( nID )
	{
	case ID_MNG_ADD_PROJECT:
		title = IDS_MNG_ADD_PROJECT_TITLE;
		errorMsg = IDS_PROJEKT_CREATE_FAILED;
		exists = FALSE;
		break;
		
	case ID_MNG_ADD_PROJECT_ENTRY:
		title = IDS_MNG_ADD_ENTRY_TITLE;
		errorMsg = IDS_PROJECT_LOAD_ERROR;
		exists = TRUE;
		break;
		
	default:
		return FALSE;
	}; // switch
	
	DirectoryDlg dlg( CString( MAKEINTRESOURCE( title ) ), NULL, this );
	if ( dlg.DoModal() == IDOK )
	{
		Project* project = m_wspProjects->AddProject( dlg.m_strDir, exists );
		if ( project )
			UpdateProjectList( m_wspProjects, project );
		else
			AfxMessageBox( errorMsg );
	}; // IDOK
	
	return TRUE;
}; // OnManagerAddProject

BOOL ProjectManagerDialog::OnManagerDeleteProject( UINT nID )
// löscht Projekt oder nur Projekteintrag
{
	BOOL remove = FALSE;
	UINT errorMsg;
	
	Project* project = GetSelectedProject();
	
	if ( project && AfxMessageBox( IDS_DELETE_PROJECT_ENTRY, MB_YESNO ) == IDYES )
	{
		switch( nID )
		{
		case ID_MNG_DELETE_PROJECT:
			{
				CString message;
				message.FormatMessage(IDS_DELETE_PROJECT_QUERY, project->GetDir() );
				if ( AfxMessageBox( message, MB_YESNO ) == IDYES )
				{
					remove = TRUE;
					errorMsg = IDS_DELETE_PROJECT_ERROR;
				}
				else
				{
					remove = FALSE;
					errorMsg = IDS_DELETE_ENTRY_ERROR;
					project->Save(); // alle Änderungen vorher noch speichern
				};
			};
			break;
			
		case ID_MNG_DELETE_PROJECT_ENTRY:
			remove = FALSE;
			errorMsg = IDS_DELETE_ENTRY_ERROR;
			break;
			
		default:
			return FALSE;
		}; // switch
		
		CWaitCursor cur;
		
		if ( !m_wspProjects->RemoveProject( project, remove ) )
			AfxMessageBox( errorMsg, MB_WARN );
		
		UpdateProjectList( m_wspProjects );
	};
	
	return TRUE;
};

void ProjectManagerDialog::OnUpdateProject( CCmdUI* pCmdUI )
{
	BOOL bEnable = FALSE; 
	
	switch( pCmdUI->m_nID )
	{
	case ID_MNG_ADD_PROJECT:
	case ID_MNG_ADD_PROJECT_ENTRY:
		bEnable = TRUE;
		break;
		
	case ID_MNG_OPEN_PROJECT:
	case ID_MNG_DELETE_PROJECT:
	case ID_MNG_DELETE_PROJECT_ENTRY:
	case ID_MNG_PRINT:
		bEnable = ( GetSelectedProject() != 0 );
		break;
		
	default:
		ASSERT( FALSE );
	}; // swtich nID
	
	pCmdUI->Enable( bEnable );
	
}; // OnUpdateProject

BOOL ProjectManagerDialog::OnManagerState( UINT nID )
// alle Commandos aus dem Menü 'Zustand' kommen hier an
{
	Project* projekt = GetSelectedProject();
	if ( projekt )
	{
		CWaitCursor wait;
		projekt->Load(); // falls das Projekt bisher nicht geladen war, jetzt laden
		
		switch ( nID )
		{
		case ID_MNG_ADD_STATE:
			{
				StateDialog dlg( STATE_DLG_MASK_ALL, this );
				dlg.m_oleDate = COleDateTime::GetCurrentTime();
				if ( dlg.DoModal() == IDOK )
				{
					State* zustand = new State( projekt );
					CWaitCursor cur;
					zustand->SetWaterName( dlg.m_water );
					zustand->SetDate( dlg.m_oleDate );
					zustand->SetName( dlg.m_name );
					zustand->CreateFileName();
					projekt->AddState( zustand );
					projekt->Save();
					UpdateStateTree( projekt, zustand, STATE_TYPE_STATE );
				};
			}; // case ID_MNG_ADD_STATE
			break;
			
		case ID_MNG_COPY_STATE:
			{
				State* zustand = GetSelectedState();
				if ( zustand )
				{
					StateDialog dlg( STATE_DLG_MASK_NAME | STATE_DLG_MASK_DATE, this );
					dlg.m_water = zustand->GetWaterName();
					dlg.m_oleDate = COleDateTime::GetCurrentTime();
					if ( dlg.DoModal() == IDOK )
					{
						if ( dlg.m_name == zustand->GetName() )
							AfxMessageBox( IDS_SAME_STATE_NAME );
						else
						{
							CWaitCursor wait;
							State* newState = zustand->Clone( projekt, dlg.m_name, TRUE );
							newState->SetDate( dlg.m_oleDate );
							projekt->AddState( newState );
							projekt->Save();
							UpdateStateTree( projekt, newState, STATE_TYPE_STATE );
						};
					};
				} // if zustand
				else
					AfxMessageBox( IDS_NOSTATECHANGE );
			}; // case id_MNG_COPY_STATE
			break;
			
		case ID_MNG_DELETE_STATE:
			{
				State* zustand = GetSelectedState();
				if ( zustand )
				{
					CString message;
					message.FormatMessage( IDS_DELETE_STATE_QUERY, zustand->GetName() );
					if( AfxMessageBox( message, MB_YESNO ) == IDYES )
					{
						// erstmal alle Profile löschen ( ohne Rücksicht auf Verluste )
						CrossSection* cs = zustand->GetFirstCrossSection();
						while( cs )
						{
							BOOL bLast = zustand->RemoveCrossSection( cs );
							if( bLast )
								delete cs;
							cs = zustand->GetFirstCrossSection();
						}; // while cs
						
						projekt->RemoveState( zustand );
						projekt->Save();
						UpdateStateTree( projekt );
					}; // IDYES
				} // if zustand
				else
					AfxMessageBox( IDS_NOSTATECHANGE );
			}; // case ID_MNG_DELETE_STATE
			break;
			
		default:
			return FALSE; // Kommando konnte nicht bearbeitet werden
		}; // switch
	}
	else
		AfxMessageBox( IDS_NO_SELECTED_PROJECT );
	
	return TRUE;
}; // OnManagerState

void ProjectManagerDialog::OnUpdateState( CCmdUI* pCmdUI )
{
	BOOL bEnable = FALSE;
	
	switch ( pCmdUI->m_nID )
	{
	case ID_MNG_ADD_STATE:
		bEnable = ( GetSelectedProject() != NULL );
		break;
		
	case ID_MNG_COPY_STATE:
	case ID_MNG_DELETE_STATE:
		bEnable = ( GetSelectedState() != NULL );
		break;
		
	default:
		ASSERT( FALSE );
	}; // switch
	
	pCmdUI->Enable( bEnable );
}; // onUpdateState

BOOL ProjectManagerDialog::OnManagerWsp( UINT nID )
{
	BOOL bErfolg = TRUE; // nur FALSE, falls diese nID nicht bearbeitet wurde
	int nReturn = -1;
	
	Project* project = GetSelectedProject();
	LengthSectionArray* lsArray = GetSelectedLengthSections();
	
	State* state = GetSelectedState();
	
	switch ( nID )
	{
	case ID_MNG_INSERT_WSP:
		if ( state )
		{
			CWSPInsertDialog dlg( this );
			if( dlg.DoModal() == IDOK )
			{
				BOOL bDurchst = dlg.GetDurchst();
				CString strAbflussFormat = dlg.GetAbflussFormat();
				
				CWaitCursor wait;
				nReturn = state->InsertWsp( lsArray, bDurchst, strAbflussFormat );
				
				wait.Restore();
				if ( nReturn == 0 )
				{
					project->Save();
					AfxMessageBox( IDS_INSERT_WSP_SUCCESS, MB_ICONINFORMATION );
				};
				
			} // switch AfxMessageBox
		};
		break;
		
	case ID_MNG_DELETE_WSP:
		if ( state )
		{
			CWaitCursor wait;
			nReturn = state->RemoveWsp( lsArray );
			wait.Restore();
			if ( nReturn == 0 )
			{
				project->Save();
				AfxMessageBox( IDS_DELETE_WSP_SUCCES, MB_ICONINFORMATION );
			};
		};
		break;
		
	case ID_MNG_LS_EXPORT_TABLE:
		{
			if( lsArray && lsArray->GetSize() > 0 )
				ExportLengthSectionToCSV( state, lsArray );
			else
				AfxMessageBox( IDS_LS_EXPORT_SELECTION, MB_ICONINFORMATION );
		}
		break;
		
	default:
		bErfolg = FALSE;
	}; // switch
	
	delete lsArray;
	
	return bErfolg; // das Kommando wurde bearbeitet oder nicht
}; // OnManagerWsp

void ProjectManagerDialog::OnUpdateWsp( CCmdUI* pCmdUI )
{
	BOOL bEnable = TRUE;
	
	LengthSectionArray* lsArray = GetSelectedLengthSections();
	State* state = GetSelectedState();
	
	switch( pCmdUI->m_nID )
	{
	case ID_MNG_INSERT_WSP:
	case ID_MNG_DELETE_WSP:
		bEnable = lsArray && state;
		break;
		
		//  case ID_MNG_LS_EXPORT_TABLE:
		//	  bEnable = state && lsArray && lsArray->GetSize() == 1;
		//	  break;
		
	default:
		break; 
	}; // switch m_nID
	
	delete lsArray;
	
	pCmdUI->Enable( bEnable );
}; // OnUpdateWsp


BOOL  ProjectManagerDialog::OnDatenimport( UINT nID )
// alle Datenimport MenuBefehle werden hierin geleitet
{
	State* zustand = GetSelectedState();
	if ( zustand )
	{
		CString fileFilter;
		Project::ImportType importType;
		
		DWORD data = 0;
		
		switch ( nID )
		{
		case ID_MNG_DATENIMPORT_DA66:
			fileFilter = "DA66 Dateien (.d66;.prf)|*.d66;*.prf|";
			importType = Project::ImportDa66;
			break;
		case ID_MNG_DATENIMPORT_DA50:
			{
				if( AfxMessageBox( IDS_MNG_DATENIMPORT_DA50_REFPOINT, MB_YESNO | MB_ICONQUESTION ) == IDYES )
					data = State::IMPORT_DA50_REFFIRST;
				else
					data = State::IMPORT_DA50_REFZERO;
				fileFilter = "DA50 Dateien (.d50;.prf)|*.d50;*.prf|";
				importType = Project::ImportDa50;
			}
			break;
			
		case ID_MNG_DATENIMPORT_WSV:
			fileFilter = "WSV Dateien (.wsv)|*.wsv|";
			importType = Project::ImportWsv;
			break;
			
		case ID_MNG_DATENIMPORT_TRIPPLE:
			fileFilter = "Tripple Dateien (.txt)|*.txt|";
			importType = Project::ImportTripple;
			break;
			
		case ID_MNG_DATENIMPORT_HYK:
			fileFilter = "HYK Dateien (.hyk)|*.hyk|";
			importType = Project::ImportHyk;
			break;
			
		case ID_MNG_DATENIMPORT_WST:
			fileFilter = "WST Dateien (.wst)|*.wst|";
			importType = Project::ImportWst;
			break;
			
		case ID_MNG_DATENIMPORT_RELI:
			fileFilter = "Buhnen Dateien (.re;.li)|*.re; *.li|";
			importType = Project::ImportReli;
			break;
			
		default:
			return FALSE; // die entsprechende ID war nicht dabei, weiter bearbeiten
		}; // switch nID
		
		CFileDialog dlg( TRUE, NULL, NULL, OFN_HIDEREADONLY, fileFilter + "Alle Dateien (*.*)|*.*||", this );
		if( dlg.DoModal()==IDOK )
		{
			CWaitCursor wait;
			BOOL erfolg;
			CString importDatei = dlg.GetPathName();
			
			UpdateWindow();
			
			Project* projekt = GetSelectedProject();
			ASSERT(projekt);
			projekt->Load();
			
			if ( importType != Project::ImportReli )
				erfolg = projekt->ImportData( (int)importType, importDatei, zustand, data );
			else
			{
				CString dateiName;
				dateiName = importDatei.Left( importDatei.Find('.') );
				erfolg = projekt->ImportData( (int)importType, dateiName + ".re", zustand, data );
				if ( erfolg )
					erfolg = projekt->ImportData( (int)importType, dateiName + ".li", zustand, data );
			};
			
			if ( erfolg  )
			{
				projekt->Save();
				UpdateStateTree( projekt, zustand, STATE_TYPE_STATE );  // neu laden aber Zustand wieder anzeigen
				AfxMessageBox( IDS_IMPORT_SUCCESFUL );
			}
			else
				AfxMessageBox( IDS_IMPORT_ERROR );
		}; // if IDOK
	}
	else
	{
		AfxMessageBox( IDS_NOSTATECHANGE );
		return FALSE;
	}; // if zustand
	
	return TRUE; // Commando erfolgreich bearbeitet
}; // OnDatenImport

BOOL ProjectManagerDialog::OnManagerMap( UINT nID )
{
	Project* project = GetSelectedProject();
	State* state = GetSelectedState();
	CrossSectionArray* csArray = GetSelectedCrossSections();
	
	switch( nID )
	{
	case ID_MNG_OPEN_MAP:
		if ( project )
		{
			delete csArray;
			csArray = NULL;
			
			NMPROJECTMNG* command = new NMPROJECTMNG( NMPROJECTMNG::openMap, project->GetDir() );
			CloseManager( command );
		}; // if project
		break;
		
	case ID_MNG_DELETE_MAP:
		if ( project )
		{
			delete csArray;
			csArray = NULL;
			
			NMPROJECTMNG* command = new NMPROJECTMNG( NMPROJECTMNG::deleteMap, project->GetDir() );
			CloseManager( command );
		};
		break;
		
	case ID_MNG_CREATE_MAP:
		if ( project && state )
		{
			// falls keine Profile ausgewählt, soll eine Stationierung abgefragt werden
			if ( !csArray )
			{
				csArray = new CrossSectionArray;
				CCsRegion dlg( state, this );
				if ( dlg.DoModal() == IDOK )
				{
					int count = 0;
					CrossSection* cs = state->GetFirstCrossSection();
					while ( cs )
					{
						if ( count >= dlg.startIndex && count <= dlg.endIndex )
							csArray->Add( cs );
						count++;
						cs = state->GetNextCrossSection();
					}; // while cs
				} // if dlg.doModal
				else
				{
					delete csArray;
					return TRUE;
				};
			}; // if !csArray
			
			// die einzufügenden Querprofile ermitteln
			CStringArray* csStrings = new CStringArray();
			for ( int i = 0; i < csArray->GetSize(); i++ )
			{
				CrossSection* cs = csArray->GetAt( i );
				if ( cs )
					csStrings->Add( cs->GetFileName() );
			}; // for i
			delete csArray;
			csArray = NULL;
			
			NMPROJECTMNG* command = new NMPROJECTMNG( NMPROJECTMNG::createMap, project->GetDir(), 
				state->GetFileName(), csStrings );
			CloseManager( command );
		}; // if project && state
		break;
		
	default:
		delete csArray;
		return FALSE;
	}; // switch
	
	return TRUE;
}; // OnManagerMap

void ProjectManagerDialog::OnUpdateMap( CCmdUI* pCmdUI )
{
	Project* project = GetSelectedProject();
	State* state = GetSelectedState();
	
	BOOL bEnable = FALSE;
	
	switch( pCmdUI->m_nID )
	{
	case ID_MNG_CREATE_MAP:
		if ( project && state )
			bEnable = TRUE;
		break;
		
	case ID_MNG_OPEN_MAP:
	case ID_MNG_DELETE_MAP:
		if ( project )
			bEnable = TRUE;
		break;
		
	default:
		ASSERT( FALSE );
	}; // switch
	
	pCmdUI->Enable( bEnable );
}; // OnUpdateMap

void ProjectManagerDialog::OnUpdateDatenimport( CCmdUI* pCmdUI )
{
	pCmdUI->Enable( ( GetSelectedState() != NULL ) );
}; // OnUpdateDatenimport

BOOL ProjectManagerDialog::OnDatenexport ( UINT nID )
// alle Datenexport Menubefehle werden hierin geleitet
{
	CrossSectionArray* csArray = GetSelectedCrossSections();
	if ( csArray != NULL )
	{
		CString fileFilter, fileExt;
		Project::ExportType exportType;
		
		switch ( nID )
		{
		case ID_MNG_DATENEXPORT_DA66:
			fileExt = "d66";
			fileFilter = "DA66 Dateien (*.d66)|*.d66|";
			exportType = Project::ExportDa66;
			break;
			
		case ID_MNG_DATENEXPORT_TRIPPLE:
			fileExt = "txt";
			fileFilter = "Tripple Dateien (*.txt)|*.txt|";
			exportType = Project::ExportTripple;
			break;
			
		case ID_MNG_DATENEXPORT_CSV:
			fileExt = "CSV";
			fileFilter = "Comma Separated Values (*.csv)|*.csv|";
			exportType = Project::ExportCSV;
			break;
			
		case ID_MNG_DATENEXPORT_HYK:
			fileExt = "hyk";
			fileFilter = "HYK Dateien (*.hyk)|*.hyk|";
			exportType = Project::ExportHyk;
			break;
			
		case ID_MNG_DATENEXPORT_RELI:
			fileExt = "";
			fileFilter = "Buhnen Dateien (*.re; *.li)|*.re; *.li|";
			exportType = Project::ExportReliL;
			break;
			
		default:
			return FALSE; // Commando wurde hier nicht bearbeitet, weiterschicken
		}; // switch nID
		
		State* zustand = GetSelectedState();
		ASSERT(zustand);
		CString stateName = zustand->GetName();
		stateName.Replace( '\\', '_' ); // sonst gibt CFileDialog IDCANCEL zurück
		stateName.Replace( '/', '_' );
		CFileDialog dlg( FALSE, fileExt, stateName,
			OFN_OVERWRITEPROMPT | OFN_HIDEREADONLY, fileFilter, this );
		if ( dlg.DoModal()==IDOK )
		{
			CWaitCursor wait;
			Project* projekt = GetSelectedProject();
			ASSERT(projekt);
			
			CString exportDatei = dlg.GetPathName();
			
			UpdateWindow();  // Fenster schon mal neu zeichnen ( sonst bleibt ein hässliches Loch während des Exports )
			
			int error;
			
			if ( exportType == Project::ExportReliL )
			{
				int pointPos = exportDatei.Find( '.' );
				if ( pointPos != -1 )
					exportDatei = exportDatei.Left( pointPos + 1 );
				error = projekt->ExportData( exportType, csArray, exportDatei + "li" );
				if ( error == 0 ) 
					error = projekt->ExportData(Project::ExportReliR, csArray, exportDatei + "re" );
			}
			else
				error = projekt->ExportData( exportType, csArray, exportDatei );
			
			if ( error != 0 )
				AfxMessageBox( IDS_EXPORT_ERROR );
			else
				AfxMessageBox( IDS_EXPORT_SUCCESFUL );
		}; // if IDOK
		delete csArray;
		
	} // if profilListe
	else
	{
		AfxMessageBox( IDS_NO_PROFILES_SELECTED );
		return FALSE;
	};
	
	return TRUE;
}; // OnDatenExport

void ProjectManagerDialog::OnUpdateDatenexport( CCmdUI* pCmdUI )
{
	BOOL bEnable = FALSE;
	
	HTREEITEM treeItem = m_stateTree.GetSelectedItem();
	if ( treeItem )
	{
		StateTreeData* treeData = (StateTreeData*)m_stateTree.GetItemData( treeItem );
		if ( treeData && treeData->type == STATE_TYPE_QS )
			bEnable = TRUE;
	}; // if treeItem
	
	pCmdUI->Enable( bEnable );
}; // OnUpdateDatenexport



   /*!
   * Die Funktion behandelt alle Kommandos der Gruppe 'Querprofile'
   *
   * @param nID : die Id des Kommandos, welches ausgelöst wurde
   *
   * @return BOOL  : TRUE, falls dieses Kommando hier behandelt wurde, sonst FALSE
*/
BOOL ProjectManagerDialog::OnManagerCS( UINT nID )
{
	Project* pProjekt = GetSelectedProject();
	CrossSectionArray* csArray = GetSelectedCrossSections();
	if( !pProjekt || !csArray )
		return FALSE;
	
	BOOL bErfolg = TRUE;
	
	switch( nID )
	{
	case ID_MNG_MENU_CS_DELETE_DB:
		{
			// welche Datenblöcke sollen gelöscht werden?
			CDataBlockTypeChooserDialog dlg( this );
			if( dlg.DoModal() == IDOK )
			{
				CArray<int, int> dbArray;
				dlg.GetDatablockTypes( dbArray );
				
				// erst nochmal Warnen
				if( AfxMessageBox( IDS_MNG_CS_DELETE_DB_WARNING, MB_YESNO | MB_ICONEXCLAMATION ) == IDYES )
				{
					CWaitCursor wait;
					UINT count = csArray->DeleteDatablocks( dbArray );
					if( count > 0 )
					{
						CString message;
						message.Format( "%d", count );
						message.FormatMessage( IDS_MNG_CS_DELETE_DB_OK, message );
						AfxMessageBox( message, MB_OK | MB_ICONINFORMATION );
						pProjekt->Save();
					}
					else
						AfxMessageBox( IDS_MNG_CS_DELETE_DB_NODBS );
				} // if AfxMEssageBox == IDYES
			} // if dlg->DoModal() == IDOK
		}; // ID_MNG_MENU_CS_DELETE_DB
		break;
		
	case ID_MNG_MENU_CS_REFLECT:
		{
			csArray->FlipHorizontal( TRUE );
			AfxMessageBox( IDS_MNG_CS_FLIP );
		}
		break;
		
	default:
		bErfolg = FALSE; // nur in diesem Fall FALSE, das kommando konnte nicht bearbeitet werden
		break;
	}; // switch nID
	
	delete csArray;
	csArray = NULL;
	
	return bErfolg;
} // OnManagerCS


  /*!
  * GUI-Handler für die Kommandos aus der Gruppe 'Querprofile'
  *
  * @param pCmdUI : Zeigt auf die Abgefragte UI, wird entsprechend aktiviert7deaktiviert etc.
*/
void ProjectManagerDialog::OnUpdateCS( CCmdUI* pCmdUI )
{
	CrossSectionArray* csArray = GetSelectedCrossSections();
	
	switch( pCmdUI->m_nID )
	{
	case ID_MNG_MENU_CS_DELETE_DB:
	case ID_MNG_MENU_CS_REFLECT:
		pCmdUI->Enable( csArray != NULL );
		break;
		
	case ID_MNG_CREATE_LSECTION: 
		{
			State* state = GetSelectedState();
			pCmdUI->Enable( csArray != NULL && state != NULL );
		}
		break;
		
	default:
		ASSERT( FALSE ); // darf nicht sein
	} // switch m_nID
	
	delete csArray;
	csArray = NULL;
} // OnUpdateCS


///////////////////
// ControlEvents //
///////////////////

void ProjectManagerDialog::OnEndlabeleditProjectList(NMHDR* pNMHDR, LRESULT* pResult) 
// wenn der Name editiert wurde:
// - den Projektnamen entsprechend setzen
// - die ListCtrlUpdaten
{
	LV_DISPINFO* pDispInfo = (LV_DISPINFO*)pNMHDR;
	*pResult = 0;
	
	int index = pDispInfo->item.iItem;
	CString name = pDispInfo->item.pszText;
	
	Project* project = (Project*)m_projectList.GetItemData( index );
	if ( project && ( pDispInfo->item.mask & LVIF_TEXT == LVIF_TEXT ) )
		project->SetName( name );
	
	
	if ( m_wspProjects )
		UpdateProjectList( m_wspProjects, project );
}; // OnEndlabeleditProjectList

void ProjectManagerDialog::OnItemchangedProjectList(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_LISTVIEW* pNMListView = (NM_LISTVIEW*)pNMHDR;
	
	if ( pNMListView->uNewState & LVIS_SELECTED == LVIS_SELECTED )
		UpdateStateTree( (Project*)pNMListView->lParam );
	else
		UpdateStateTree( NULL );
	
	*pResult = 0;
}

void ProjectManagerDialog::OnKeydownProjectList(NMHDR* pNMHDR, LRESULT* pResult) 
{
	LV_KEYDOWN* pLVKeyDow = (LV_KEYDOWN*)pNMHDR;
	
	Project* project = GetSelectedProject();
	
	switch(pLVKeyDow->wVKey)
	{
	case VK_SPACE: 
		if ( project )
		{
			project->Load();
			UpdateStateTree( project );  // den Zustandsbaum neu laden
		};
		break;
	}
	
	*pResult = 0;
}; // OnKeydownProjectList

void ProjectManagerDialog::OnClickProjectList(NMHDR* pNMHDR, LRESULT* pResult) 
{
	Project* project = GetSelectedProject();
	
	if ( project )
	{
		project->Load();
		UpdateStateTree( project );
	};
	
	*pResult = 0;
}; // OnClockProjectList

void ProjectManagerDialog::OnColumnclickProjectList(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_LISTVIEW* pNMListView = (NM_LISTVIEW*)pNMHDR;
	col_types[0] = TYPE_STRING;
	col_types[1] = TYPE_STRING;
	listCtrl = &m_projectList;
	m_projectList.SortItems( ListCompareFuncMng, pNMListView->iSubItem );
	
	*pResult = 0;
}

void ProjectManagerDialog::OnSelchangedStateTree(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_TREEVIEW* pNMTreeView = (NM_TREEVIEW*)pNMHDR;
	TV_ITEM item = pNMTreeView->itemNew;
	if ( item.hItem && ( ( item.state & LVIS_SELECTED ) == LVIS_SELECTED ) )
		UpdateDataList( (StateTreeData*)item.lParam );
	*pResult = 0;
}; // OnSelchangedStateTree


void ProjectManagerDialog::OnColumnclickDataList(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_LISTVIEW* pNMListView = (NM_LISTVIEW*)pNMHDR;
	
	col_types[0] = TYPE_STRING;
	col_types[1] = TYPE_STRING;
	col_types[2] = TYPE_STRING;
	col_types[3] = TYPE_STRING;
	
	HTREEITEM hTreeItem = NULL;
	if ( m_stateTree.GetSafeHwnd() )
		hTreeItem = m_stateTree.GetSelectedItem();
	if ( hTreeItem )
	{
		StateTreeData* treeData = (StateTreeData*)m_stateTree.GetItemData( hTreeItem );
		if ( treeData )
		{
			switch ( treeData->type )
			{
			case STATE_TYPE_QS:
				col_types[0] = TYPE_DOUBLE;
				break;
				
			case STATE_TYPE_LS:
			case STATE_TYPE_OUTFLOW:
				col_types[1] = TYPE_DOUBLE;
				col_types[2] = TYPE_DOUBLE;
				break;
				
			}; // switch
		}; // if treeData
		
		listCtrl = &m_dataList;
		m_lastSortDirection ^= 1;
		LPARAM lParam = DWORD(m_lastSortDirection << 16 ) | WORD(pNMListView->iSubItem);
		m_dataList.SortItems( ListCompareFuncMng, lParam );
	}; // if hTreeItem
	
	*pResult = 0;
}


///////////////
// Attribute //
///////////////

Project* ProjectManagerDialog::GetSelectedProject()
// gibt das zur Zeit selektierte Projekt zurück
{
	if ( m_projectList.GetSafeHwnd() )
	{
		for ( int i = 0; i < m_projectList.GetItemCount(); i++ )
		{
			if ( m_projectList.GetItemState( i, LVIS_SELECTED ) == LVIS_SELECTED )
				return (Project*)(m_projectList.GetItemData( i ));
		}; // for i
	};
	
	return NULL;
} // GetSelectedProject

int ProjectManagerDialog::GetSelectedProjectID()
// gibt Index des zur Zeit selektierten Projektes zurück
{
	if ( m_projectList.GetSafeHwnd() )
	{
		for ( int i = 0; i < m_projectList.GetItemCount(); i++ )
		{
			if ( m_projectList.GetItemState( i, LVIS_SELECTED ) == LVIS_SELECTED )
				return i;
		}; // for
	}; // if m_projectList
	return -1;
}; // GetSelectedProjectID

State * ProjectManagerDialog::GetSelectedState()
// gibt den Momentan selektierten Zustand zurück
// Bemerkung:
//      der Zustand ist selektiert, falls Im Baum
//        - der Zustandsname oder eines seiner Blätter ( Querprofile etc. ausgweählt ist )
//      nicht, falls das Gewässer ausgewählt ist
{
	HTREEITEM treeItem = NULL;
	if ( m_stateTree.GetSafeHwnd() )
		treeItem = m_stateTree.GetSelectedItem();
	if ( treeItem )
	{
		StateTreeData* data = (StateTreeData*)m_stateTree.GetItemData( treeItem );
		if ( data )
			return (State*)data->data;
	}; // if treeItem
	
	return NULL;
}; // GetSelectedState

CrossSectionArray* ProjectManagerDialog::GetSelectedCrossSections()
// gibt Liste der selektierten Profile zurück
// Rückgabewert:
//          Zeiger auf CrossSectionArray; NULL, falls kein Querprofil selektiert ist
// Bemerkung:
//        die Liste ist in der Reihenfolge sortiert, wie sie in der ListView angezeigt wird
//        ist ein Zustand im StetTree ausgewählt, so werden alle seine Querprofile ausgewählt
{
	CrossSectionArray* csArray = NULL;
	StateTreeData* treeData = NULL;
	HTREEITEM treeItem = NULL; 
	
	if ( m_stateTree.GetSafeHwnd() )
		treeItem = m_stateTree.GetSelectedItem();
	if ( treeItem )
		treeData = (StateTreeData*)m_stateTree.GetItemData( treeItem );
	
	if ( treeData && treeData->type == STATE_TYPE_QS  && ( m_dataList.GetSelectedCount() == 0 || 
		m_dataList.GetSelectedCount() > 0 ) )
	{
		csArray = new CrossSectionArray;
		for ( int i = 0; i < m_dataList.GetItemCount(); i++ )
		{
			if (
				m_dataList.GetSelectedCount() == 0 || 
				m_dataList.GetItemState( i, LVIS_SELECTED ) == LVIS_SELECTED 
				)
				csArray->Add( (CrossSection*)m_dataList.GetItemData( i ) );
		}; // for i
	}; // if treeData->type
	
	return csArray;
}; // GetSelectedCrossSections

LengthSectionArray* ProjectManagerDialog::GetSelectedLengthSections()
// gibt Liste der selektierten zurück
// Rückgabewert:
//          Zeiger auf LengthSectionArray; NULL, falls kein Längsschnitt selektiert ist
{
	LengthSectionArray* lsArray = NULL;
	StateTreeData* treeData = NULL;
	HTREEITEM treeItem = NULL;
	
	if ( m_stateTree.GetSafeHwnd() )
		treeItem = m_stateTree.GetSelectedItem();
	if ( treeItem )
		treeData = (StateTreeData*)m_stateTree.GetItemData( treeItem );
	
	if ( treeData && treeData->type == STATE_TYPE_LS )
	{
		lsArray = new LengthSectionArray;
		for ( int i = 0; i < m_dataList.GetItemCount(); i++ )
		{
			if ( m_dataList.GetItemState( i, LVIS_SELECTED ) == LVIS_SELECTED )
				lsArray->Add( (LengthSection*)m_dataList.GetItemData( i ) );
		}; // for i
		if ( lsArray->GetSize() == 0 )
		{
			delete lsArray;
			lsArray = NULL;
		};
	}; // if treeData->type
	
	return lsArray;
}; // GetSelectedCalculations
////////////////////
// Implementation //
////////////////////

void ProjectManagerDialog::UpdateStateTree( Project* projekt, State* alterZustand /* = NULL */, int type /* = 0 */ )
// füllt den Zustandsbaum, falls das Projekt geladen ist
// danach wird versucht den Zweig mit 'alterZustand', Blatt Nummer 'type' anzuzeigen
// Parameter:
//          Project* project: Projekt, dessen Daten angezeigt werden sollen
//          State* alterZustand: ist dieser Zustand vorhanden, wird er selektiert
//          int type: diese Information des selektierten Zustandes wird ausgewählt
// Bemerkung:
//          es werden nur Daten angezeigt, falls das Projekt geladen ist
{
	CWaitCursor wait;
	
	// Zustandsbaum und Datenliste löschen
	DeleteStateTree();
	m_dataList.DeleteAllItems();
	
	if ( !projekt || !projekt->IsLoaded() )
		return;
	
	HTREEITEM selectItem = NULL; // dieses Blatt soll selektiert werden
	
	// falls kein Zustand selektiert werden soll, den ersten des Projektes selektiern, 
	// aber nicht den focus auf den Baum setzen
	BOOL focusTree = TRUE;
	if ( alterZustand == NULL )
	{
		alterZustand = projekt->GetFirstState();
		focusTree = FALSE;
	}; // alterZustand == NULL
	
	// Zustandsbaum neu füllen
	WORD zustandsCount = 0;
	for ( int i = 0; i < projekt->GetWaterCount(); i++ )
	{
		CString wasserName = projekt->GetWaterName(i);
		HTREEITEM wasserItem = m_stateTree.InsertItem( wasserName, IMAGE_WATER, IMAGE_WATER );
		m_stateTree.SetItemData( wasserItem, (LPARAM)new StateTreeData( STATE_TYPE_WATER, NULL ) );
		
		State* zustand = projekt->GetFirstState();
		while ( zustand )
		{
			if ( zustand->GetWaterName() == wasserName )
			{
				zustandsCount++;
				HTREEITEM zustandItem = m_stateTree.InsertItem
					(LPCTSTR(zustand->GetName()), IMAGE_STATE, IMAGE_STATE, wasserItem );
				if ( zustand == alterZustand )
					selectItem = zustandItem;
				m_stateTree.SetItemData( zustandItem, (LPARAM)new StateTreeData( STATE_TYPE_STATE, zustand ) );
				
				// Querprofile zufügen, falls vorhanden
				if (zustand->GetNumCrossSections() > 0 )
				{
					
					CString str;
					str.LoadString(IDS_CROSSSECTIONS);
					HTREEITEM csectionTI = m_stateTree.InsertItem(str, IMAGE_CSECTION, IMAGE_CSECTION, zustandItem );
					if ( zustand == alterZustand && type == STATE_TYPE_QS )
						selectItem = csectionTI;
					
					m_stateTree.SetItemData( csectionTI, (LPARAM)new StateTreeData( STATE_TYPE_QS, zustand ) );
				}
				// Längsschnitte zufügen, falls vorhanden
				Calculation *calc = zustand->GetFirstCalculation();
				while (calc)
				{
					if (calc->GetLengthSection() )
					{
						CString str;
						str.LoadString(IDS_LENGTHSECTIONS);
						HTREEITEM lsectionTI = m_stateTree.InsertItem(str, IMAGE_LSECTION, IMAGE_LSECTION, zustandItem);
						if ( zustand == alterZustand && type == STATE_TYPE_LS )
							selectItem = lsectionTI;
						m_stateTree.SetItemData(lsectionTI, (LPARAM)new StateTreeData( STATE_TYPE_LS, zustand ) );
						break;
					}
					calc = zustand->GetNextCalculation();
				}
				
				// Berechnungsvarianten zufügen, falls vorhanden
				if ( zustand->GetNumCalculations() > 0  )
				{
					CString str;
					str.LoadString( IDS_CALCULATIONS );
					HTREEITEM calcTI = m_stateTree.InsertItem( str, IMAGE_CALC, IMAGE_CALC, zustandItem );
					if ( zustand == alterZustand && type == STATE_TYPE_CALC )
						selectItem = calcTI;
					m_stateTree.SetItemData( calcTI, (LPARAM)new StateTreeData( STATE_TYPE_CALC, zustand ) );
				}
				
				// Abflüsse hinzufügen, fall vorhanden
				if ( zustand->GetNumOutFlows() > 0 )
				{
					CString str;
					str.LoadString( IDS_OUTFLOWS );
					HTREEITEM abflussTI = m_stateTree.InsertItem( str, IMAGE_ABFLUSS, IMAGE_ABFLUSS, zustandItem );
					if ( zustand == alterZustand && type == STATE_TYPE_OUTFLOW )
						selectItem = abflussTI;
					
					m_stateTree.SetItemData( abflussTI, (LPARAM)new StateTreeData( STATE_TYPE_OUTFLOW, zustand ) );
				}
			}
			zustand = projekt->GetNextState();
		}; // while st
	}; // for i
	
	if ( selectItem )
	{
		m_stateTree.SelectItem( selectItem );
		if ( focusTree )
			m_stateTree.SetFocus();
	};
	UpdateTitle();
} // UpdateStateTree


void ProjectManagerDialog::DeleteStateTree()
// löscht den StatusBaum, insbesondere alle StateTreeData aus dem ItemDatas
{
	if ( m_stateTree.GetSafeHwnd() )
	{
		HTREEITEM treeItem = m_stateTree.GetRootItem();
		while ( treeItem )
		{
			DeleteStateTreeData( treeItem );
			treeItem = m_stateTree.GetNextItem( treeItem, TVGN_NEXT );
		}; // while treeItem
		
		m_stateTree.DeleteAllItems();
	};
}; // DeleteStateTree

void ProjectManagerDialog::DeleteStateTreeData( HTREEITEM treeItem )
// löscht ( rekursiv ) alle Daten des jeweiligen TreeItems
{
	HTREEITEM childItem = m_stateTree.GetNextItem( treeItem, TVGN_CHILD );
	while ( childItem )
	{
		DeleteStateTreeData( childItem );
		childItem = m_stateTree.GetNextItem( childItem, TVGN_NEXT );
	}; // while chdilItem
	
	delete (StateTreeData*)m_stateTree.GetItemData( treeItem );
	m_stateTree.SetItemData( treeItem, NULL );
}; // DeleteTreeData


void ProjectManagerDialog::UpdateDataList( ProjectManagerDialog::StateTreeData* treeData )
// füllt die Datenliste
{
	m_dataList.DeleteAllItems();
	
	State* zustand = NULL;
	if ( treeData )
		zustand = (State*)treeData->data;
	
	if ( zustand )
	{
		CString header[4];
		
		switch ( treeData->type )
		{
		case STATE_TYPE_WATER:
		case STATE_TYPE_STATE:
			break;
			
		case STATE_TYPE_QS:
			{
				header[0].LoadString(IDS_STATION);
				header[1].LoadString(IDS_PK);
				header[2].LoadString(IDS_VZK);
				header[3].LoadString(IDS_FILE);
				CrossSection* cs = zustand->GetFirstCrossSection();
				while ( cs )
				{
					CString str;
					str.Format("%.4f", cs->GetStation());
					
					int count = m_dataList.GetItemCount();
					int index = m_dataList.InsertItem( LVIF_TEXT | LVIF_IMAGE | LVIF_PARAM | LVIF_STATE, 
						count, LPCTSTR(str), 0, 0, IMAGE_CSECTION, LPARAM(cs) );
					
					m_dataList.SetItemText( index, 1, cs->GetPK() );
					
					str.Format( "%d", cs->GetVZK() );
					m_dataList.SetItemText( index, 2, str );
					
					m_dataList.SetItemText( index, 3, LPCTSTR( "prof\\" + cs->GetFileName() ) );
					
					cs = zustand->GetNextCrossSection();
				}; // while cs
			}; // case STATE_TYPE_QS    
			break;
			
		case STATE_TYPE_LS:
			{
				header[0].LoadString( IDS_NAME );
				header[1].LoadString( IDS_START );
				header[2].LoadString( IDS_END );
				header[3].LoadString( IDS_FILE );
				
				Calculation* calc = zustand->GetFirstCalculation();
				while ( calc )
				{
					LengthSection* ls = calc->GetLengthSection();
					if ( ls )
					{
						CString str;
						int count = m_dataList.GetItemCount();
						int index = m_dataList.InsertItem( LVIF_TEXT | LVIF_IMAGE | LVIF_PARAM | LVIF_STATE, count, 
							ls->GetName(), 0, 0, IMAGE_LSECTION, LPARAM(ls) );
						
						str.Format( "%.4f", ls->GetStartStation() );
						m_dataList.SetItemText( index, 1, str );
						str.Format( "%.4f", ls->GetEndStation() );
						m_dataList.SetItemText( index, 2, str );
						str = "dath\\" + ls->GetFileName();  // fileTitle + '.' + fileExt;
						m_dataList.SetItemText( index, 3, str );
					}; // if ls
					calc = zustand->GetNextCalculation();
				}; // while cs
			}; // case STATE_TYPE_LS
			break;
			
		case STATE_TYPE_CALC:
			{
				header[0].LoadString(IDS_NAME);
				header[1].LoadString(IDS_START);
				header[2].LoadString(IDS_END);
				header[3].LoadString(IDS_FILE);
				Calculation* calc = zustand->GetFirstCalculation();
				while ( calc )
				{
					CString str;
					int count = m_dataList.GetItemCount();
					int index = m_dataList.InsertItem( LVIF_TEXT | LVIF_IMAGE | LVIF_PARAM | LVIF_STATE, count, 
						calc->GetName(), 0, 0, IMAGE_CALC, LPARAM(calc) );
					
					str.Format( "%.4f", calc->GetStartStation() );
					m_dataList.SetItemText( index, 1, str );
					str.Format( "%.4f", calc->GetEndStation() );
					m_dataList.SetItemText( index, 2, str );
					str = "prof\\" + calc->GetFileName(); // fileTitle + '.' + fileExt;
					m_dataList.SetItemText( index, 3, str );
					calc = zustand->GetNextCalculation();
				}; // while calc
			}; // case STATE_TYPE_CALC
			break;
			
		case STATE_TYPE_OUTFLOW:
			{
				header[0].LoadString( IDS_NAME );
				header[1].LoadString( IDS_WSPFIX );
				header[2].Empty();
				header[3].LoadString( IDS_FILE );
				OutFlow* of = zustand->GetFirstOutFlow();
				while ( of )
				{
					CString str;
					int count = m_dataList.GetItemCount();
					int index = m_dataList.InsertItem( LVIF_TEXT | LVIF_IMAGE | LVIF_PARAM | LVIF_STATE, count, 
						of->GetName(), 0, 0, IMAGE_ABFLUSS, LPARAM(of) );
					
					if ( of->WSPFIsDefined() )
						str.LoadString( IDS_DEFINED );
					else
						str.LoadString( IDS_NOTDEFINED );
					m_dataList.SetItemText( index, 1, str );
					str = "prof\\" + zustand->GetFileTitle() + ".qwt";
					m_dataList.SetItemText( index, 3, str );
					of = zustand->GetNextOutFlow();
				}; // while of
			}; // case STATE_TYPE_OUTFLOW
			break;
			
		default:
			ASSERT(FALSE);
    }; // switch
    
    for ( int i = 0; i < 4; i++ )
    {
		LV_COLUMN lvColumn;
		lvColumn.mask = LVCF_FMT | LVCF_SUBITEM | LVCF_TEXT | LVCF_WIDTH;
		lvColumn.fmt = LVCFMT_LEFT;
		lvColumn.pszText = header[i].GetBuffer( header[i].GetLength() );
		header[i].ReleaseBuffer();
		lvColumn.iSubItem = i;
		lvColumn.cx = m_dataList.GetColumnWidth( i );
		m_dataList.SetColumn( i, &lvColumn );
    }; // for i
  }; // if zustand
  
  m_dataList.AutoSizeColumns();
  UpdateTitle();
}; // UpdateDataList

void ProjectManagerDialog::UpdateProjectList( const ProjectList* wspProjects, 
											 const Project* selProject /* = NULL */ )
											 // füllt die ProjektListe und selektiert eine Zeile
											 // Parameter:
											 //      const ProjectList* wspProject: die ProjectListcontrol mit diesen Daten füllen
											 //      const Project* selProject: dieses Projekt wird selektiertm, bei NULL wird das erste selektiert
											 // Bemerkung:
											 //        das ItemData jeder Zeile ist ein Zeiger auf das entsprechende Projekt ( Project* )
											 //        nach dem Füllen wird ein AutoResizecolumns durchgeführt
{
	if ( m_projectList.GetSafeHwnd() )
		m_projectList.DeleteAllItems();
	
	if ( !m_wspProjects )
		return;
	
	int selSpalte = 0; // im Zweifelsfall immer die 0. Spalte selektieren
	for ( int i = 0; i < wspProjects->GetCount(); i++ )
	{
		Project* project = m_wspProjects->GetProject( i );
		
		int index = m_projectList.InsertItem( LVIF_TEXT | LVIF_IMAGE | LVIF_PARAM, i, project->GetName(), 0, 0, 
			IMAGE_PROJECT, (LPARAM)project );
		
		m_projectList.SetItemText( index, 1, project->GetDir() );
		m_projectList.GetItemText( index, 1 );
		
		if ( selProject == project )
			selSpalte = index;
	}; // for i
	
	switch ( m_projectList.GetItemCount() )
	{
	case 0:
		m_projectList.ShowWindow( SW_HIDE );
		m_staticProjekte.ShowWindow( SW_HIDE );
		UpdateStateTree( NULL ); // den StateTree löschen
		m_stateTree.SetFocus();
		break;
		
	case 1:  // falls nur ein Projekt vorhanden, dieses auswählen, und laden
		{
			Project* project = (Project*)m_projectList.GetItemData( 0 );
			if ( project )
				project->Load();
			
			m_projectList.SetItemState( 0, LVIS_SELECTED | LVIS_FOCUSED, LVIS_SELECTED | LVIS_FOCUSED );
			m_projectList.ShowWindow( SW_HIDE );
			m_staticProjekte.ShowWindow( SW_HIDE );
			m_stateTree.SetFocus();
		};
		break;
		
	default: // sonst einfach nur das gewünschte Projekt auswählen
		m_projectList.ShowWindow( SW_SHOW );
		m_staticProjekte.ShowWindow( SW_SHOW );
		m_projectList.SetItemState( selSpalte, LVIS_SELECTED | LVIS_FOCUSED, LVIS_SELECTED | LVIS_FOCUSED );
	};
	
	ArrangeControls();
	m_projectList.AutoSizeColumns();
	UpdateTitle();
}; // FillProjectList

void ProjectManagerDialog::ArrangeControls()
// Ordnet den Inhalt des Dialogs an
{
	const int ratioVert = 40; // Vertteilung linke und rechte Seite in Prozent
	const int ratioHorz = 60; // Verteilung linke Seite in oben und unten
	const int border = 4;     // Breite des Randes
	
	if ( !m_staticProjekte.GetSafeHwnd() || !m_staticProfile.GetSafeHwnd() || 
		!m_projectList.GetSafeHwnd() || !m_stateTree.GetSafeHwnd() || !m_dataList.GetSafeHwnd() )
		return;
	// Fenstergrösse holen
	CRect clientRect, moveRect;
	GetClientRect( clientRect );
	
	// Toolbar abziehen und positionieren
	RepositionBars( AFX_IDW_CONTROLBAR_FIRST, AFX_IDW_CONTROLBAR_LAST, 0 );
	
	
	CRect toolBarRect( 0, 0, 0, 0 );
	if ( toolBar.GetSafeHwnd() )
	{
		toolBar.GetClientRect( toolBarRect );
		
		moveRect = clientRect;
		moveRect.bottom = moveRect.top + 2 * toolBarRect.Height();
		toolBar.MoveWindow( moveRect, TRUE );
	};
	
	clientRect.DeflateRect( border, border );
	clientRect.top += toolBarRect.Height(); // toolBarRect.Height();
	
	int leftWidth;
	int rightWidth;
	
	if ( m_dataList.IsWindowVisible() )
	{
		leftWidth =  clientRect.Width() * ratioVert / 100 - border / 2;
		rightWidth = clientRect.Width() * ( 100 - ratioVert ) / 100 - border / 2;
	}
	else
	{
		leftWidth = clientRect.Width() - border;
		rightWidth = 0;
	};
	
	// linke Hälfte positionieren
	CRect leftRect = clientRect;
	leftRect.right = leftRect.left + leftWidth;
	
	// Überschrift
	m_staticProjekte.GetClientRect( moveRect );
	int staticHeight = moveRect.Height();
	moveRect.top = leftRect.top;
	moveRect.bottom = moveRect.top + staticHeight;
	moveRect.left = leftRect.left;
	moveRect.right = leftRect.right;
	m_staticProjekte.MoveWindow( moveRect, TRUE );
	
	leftRect.top += staticHeight;
	
	int upperHeight = leftRect.Height() * ratioHorz / 100 - border / 2;
	int lowerHeight = leftRect.Height() * ( 100 - ratioHorz ) / 100 - border / 2;
	
	moveRect = leftRect;
	moveRect.bottom = moveRect.top + upperHeight;
	m_projectList.MoveWindow( moveRect, TRUE );
	
	moveRect = leftRect;
	if ( m_projectList.IsWindowVisible() ) // falls ProjektListe unsichtbar, ganze linke Seite ausfüllen
		moveRect.top = moveRect.bottom - lowerHeight;
	m_stateTree.MoveWindow( moveRect, TRUE );
	
	// rechte Hälfte Positionieren
	CRect rightRect = clientRect;
	rightRect.left = rightRect.right - rightWidth;
	
	m_staticProfile.GetClientRect( moveRect );
	moveRect.top = rightRect.top;
	moveRect.bottom = moveRect.top + staticHeight;
	moveRect.right = rightRect.right;
	moveRect.left = rightRect.left;
	m_staticProfile.MoveWindow( moveRect, TRUE );
	
	rightRect.top += staticHeight;
	m_dataList.MoveWindow( rightRect );
	
	m_projectList.AutoSizeColumns();
	m_dataList.AutoSizeColumns();
}; // ArrangeControls

void ProjectManagerDialog::UpdateTitle()
// setzt den Fenstertitel in der Form. 'Projekmanager - Project - Gewässer - Zustand - x '
{
	CString titel = CString( MAKEINTRESOURCE( IDS_MNG_TITEL ) );
	
	if ( m_wspProjects ) // falls Null Dialog vermutlich noch nicht offen oder schon zu
	{
		Project* project = GetSelectedProject();
		if ( project )
		{
			titel += " - " + project->GetDir();
			State* state = GetSelectedState();
			if ( state )
			{
				titel += " - " + state->GetWaterName() + " : " + state->GetName();
			}; // if state
		}; // if project
		
		SetWindowText( titel );
	}; // if wspProjects
}; // UpdateTitle


void ProjectManagerDialog::CmdRouteMenu( CWnd* pWnd,CMenu* pMenu )
// von http://www.codeproject.com/menu/cmdroutemenu.asp; By Noel Dillabough
// Trick um Update_command_ui für das Menu zu erzwingen
{
	CCmdUI state;
	state.m_pMenu = pMenu;
	state.m_pParentMenu = pMenu;
	state.m_nIndexMax = pMenu->GetMenuItemCount();
	
	for (state.m_nIndex = 0; 
	state.m_nIndex < state.m_nIndexMax; 
	state.m_nIndex++) 
	{
		state.m_nID = pMenu->GetMenuItemID(state.m_nIndex);
		
		// menu separator or invalid cmd - ignore it
		if (state.m_nID == 0) continue; 
		
		if (state.m_nID == (UINT)-1)
		{
			// possibly a popup menu, route to child menu if so
			CMenu* pSub=pMenu->GetSubMenu(state.m_nIndex);
			if(pSub) CmdRouteMenu(pWnd,pSub);
		}
		else 
		{
			// normal menu item, Auto disable if command is 
			// _not_ a system command.
			state.m_pSubMenu = NULL;
			state.DoUpdate(pWnd, FALSE);
		}
	}
}

void ProjectManagerDialog::CloseManager( NMPROJECTMNG* command )
// schliesst den Manager und übergibt den CommandoString an die aufrufende Anwendung
{
	m_wspProjects->Save();
	
	DeleteContents();
	
	m_command = command;
	
	CDialog::OnOK();
}; // CloseManager


void CStringWriter( std::ostream os, CString string )
{
	os << LPCTSTR( string );
}

void ProjectManagerDialog::ExportLengthSectionToCSV( const State* state, const LengthSectionArray* lsArray )
{
	CFileDialog dlg( FALSE, "csv", state->GetName(), OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT, "Längsschnitt tabellarisch (*.csv)|*.csv||", this );
	if ( dlg.DoModal() != IDOK )
		return;

	const CString path = dlg.GetPathName();

	// create csv
	BCE::CSV<double, CString, double> table;

	for( int i = 0; i < lsArray->GetSize(); i++ )
	{
		LengthSection* ls = lsArray->GetAt( i );
		ls->LoadProfil();

		CString s;
		s.Format( "-%d", i );

		Profil* profil = ls->GetProfil();
		if( profil )
			profil->DumpCSV( table, s );
	}

	// write to file
	std::ofstream out( path, ios::out | ios::trunc );
	if( out.is_open() )
		table.WriteToStream( out, "STATION", ';', &CStringWriter, -9999.9 );
}