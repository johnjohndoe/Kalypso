#pragma warning(disable:4786)
#pragma warning(disable:4503)

#include "stdafx.h"

#include "resource.h"

#include "commonMfc\include\processHelper.h"
#include "wspprj\wspprj.h"

#include "mapview.h"
#include "maplayer.h"
#include "mapdoc.h"
#include "profilauswahl.h"

#include "MapStateProfiles.h"
#include "updatedatablocksdlg.h"
#include "mainfrm.h"
#include "genprofiledlg.h"


#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif


/////////////////////////////////////////////////////////////////////////////
// CProfilAuswahl

CProfilAuswahl::CProfilAuswahl() : CSizingControlBar()
{
  m_activeState = NULL;
  m_mapDoc = NULL;
  //{{AFX_DATA_INIT(CProfilAuswahl)
		// HINWEIS: Der Klassen-Assistent fügt hier Elementinitialisierung ein
  //}}AFX_DATA_INIT

}

CProfilAuswahl::~CProfilAuswahl()
{
  DeleteContents();
};

void CProfilAuswahl::DoDataExchange(CDataExchange* pDX)
{
  CSizingControlBar::DoDataExchange(pDX);
  DDX_Control(pDX, ID_PROFILE_LIST, m_profileList);
  //{{AFX_DATA_MAP(CProfilAuswahl)
  //}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CProfilAuswahl, CSizingControlBar)
	//{{AFX_MSG_MAP(CProfilAuswahl)
	//}}AFX_MSG_MAP
  ON_WM_SIZE()
	ON_WM_CREATE()
  ON_MESSAGE_VOID(WM_KICKIDLE, OnKickIdle)
  ON_NOTIFY(LVN_ITEMCHANGED, ID_PROFILE_LIST, OnItemchangedProfileList)
  ON_NOTIFY(TVN_SELCHANGED, ID_PROFILAUSWAHL_TREE_CTRL, OnSelchangedTreeCtrl)

  // Commandos aus der Toolbar
  ON_COMMAND_EX(ID_PROFIL_AUSWAHL_ADD, OnProfilAuswahlCommand)
  ON_COMMAND_EX(ID_PROFIL_AUSWAHL_REMOVE, OnProfilAuswahlCommand)
  ON_COMMAND_EX(ID_PROFILAUSWAHL_SAVE, OnProfilAuswahlCommand)
  ON_COMMAND_EX(ID_PROFILAUSWAHL_SELECTALL, OnProfilAuswahlCommand)
  ON_UPDATE_COMMAND_UI(ID_PROFIL_AUSWAHL_ADD, OnUpdateProfilAuswahlToolbar)
  ON_UPDATE_COMMAND_UI(ID_PROFIL_AUSWAHL_REMOVE, OnUpdateProfilAuswahlToolbar)
  ON_UPDATE_COMMAND_UI(ID_PROFILAUSWAHL_SAVE, OnUpdateProfilAuswahlToolbar)

END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CProfilAuswahl 

void CProfilAuswahl::OnKickIdle()
// benötigt für's Updaten der Toolbar
{
  SendMessageToDescendants( WM_IDLEUPDATECMDUI );
};

int CProfilAuswahl::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
  if ( CSizingControlBar::OnCreate(lpCreateStruct) == -1 ||
       !m_treeCtrl.Create( WS_VISIBLE | WS_TABSTOP | WS_BORDER | TVS_SHOWSELALWAYS |
                           TVS_HASLINES | TVS_HASBUTTONS | TVS_LINESATROOT | TVS_DISABLEDRAGDROP,
                           CRect( 0, 0, 100, 100 ), this, ID_PROFILAUSWAHL_TREE_CTRL ) ||
       !m_profileList.Create( WS_VISIBLE | WS_TABSTOP | WS_BORDER |
                              LVS_REPORT | LVS_NOLABELWRAP | LVS_SHAREIMAGELISTS | LVS_SHOWSELALWAYS, 
                              CRect( 0, 0, 100, 100 ), this, ID_PROFILE_LIST ) ||
       !m_toolbar.Create( this,  WS_CHILD | WS_VISIBLE | CBRS_TOP | 
                          CBRS_TOOLTIPS | CBRS_FLYBY, IDW_PROFILAUSWAHL_TOOLBAR ) ||
       !m_toolbar.LoadToolBar( IDR_PROFILAUSWAHL )
      )
    return -1;

  // Extendet Styles setzen
  m_treeCtrl.ModifyStyleEx( 0, WS_EX_CLIENTEDGE );
  m_profileList.ModifyStyleEx( 0, WS_EX_CLIENTEDGE );

  // TreeCtrl initialisieren
  m_treeCtrl.SetImageList( CCommonImageList::GetList( FALSE ), TVSIL_NORMAL );
  
  // Profilliste initialisieren
  CString headline;

  m_imageList.Create(IDB_STATEICONS, 16, 1, RGB(255, 0, 0));
  
  m_profileList.SetImageList( &m_imageList, LVSIL_STATE );
  m_profileList.SetStateIcons(TRUE);
  headline.LoadString( IDS_GEWAESSER );
  m_profileList.InsertColumn( 0, headline, LVCFMT_LEFT, -1, 0 );
  headline.LoadString( IDS_ZUSTAND );
  m_profileList.InsertColumn( 1, headline, LVCFMT_LEFT, -1, 1 );
  headline.LoadString( IDS_STATION );
  m_profileList.InsertColumn( 2, headline, LVCFMT_LEFT, -1, 2 );
  headline.LoadString( IDS_FILE );
  m_profileList.InsertColumn( 3, headline, LVCFMT_LEFT, -1, 3 );

  return 0;
}

void CProfilAuswahl::OnSize(UINT nType, int cx, int cy) 
{
	CSizingControlBar::OnSize(nType, cx, cy);

  if ( m_treeCtrl.GetSafeHwnd() && m_profileList.GetSafeHwnd() && m_toolbar.GetSafeHwnd() )
  {
    CRect clientRect;

    // Toolbar an die richtige Stelle docken
    RepositionBars( IDW_PROFILAUSWAHL_TOOLBAR, IDW_PROFILAUSWAHL_TOOLBAR, 
      ID_PROFILE_LIST, reposQuery, clientRect );

    RepositionBars( IDW_PROFILAUSWAHL_TOOLBAR, IDW_PROFILAUSWAHL_TOOLBAR,
      ID_PROFILE_LIST );

    CRect treeRect( clientRect );
    treeRect.DeflateRect( 0, 0, treeRect.Width() / 3 * 2, 0 );
    treeRect.DeflateRect( 2, 2 );
    m_treeCtrl.MoveWindow( treeRect, TRUE );

    CRect listRect( clientRect );
    listRect.DeflateRect( listRect.Width() / 3, 0, 0, 0 );
    listRect.DeflateRect( 2, 2 );
    m_profileList.MoveWindow( listRect, TRUE );

    m_profileList.AutoSizeColumns();
  }; // m_profileList.GetSafeHwnd
}

void CProfilAuswahl::OnItemchangedProfileList(NMHDR* pNMHDR, LRESULT* pResult)
// falls sich etwas am Zustand der Liste ändert:
// - fügt Profile zur Karte hinzu oder entfernt sie, falls das Kontrollkästchen geändert wird
//
{
 NM_LISTVIEW* pListView = (NM_LISTVIEW*)pNMHDR;

 // Un-Checked
 if( pListView->uNewState & INDEXTOSTATEIMAGEMASK(1) )
 {
   ProfilInfo* pInfo = (ProfilInfo*)m_profileList.GetItemData( pListView->iItem );
   if( pInfo->bInMap )
   {
     ProfilInfoArray infoList;
     infoList.Add( pInfo );
     RemoveProfilesFromMap( infoList );
   };
 };

 // Checked
 if ( pListView->uNewState & INDEXTOSTATEIMAGEMASK(2) )
 {
   ProfilInfo* pInfo = (ProfilInfo*)m_profileList.GetItemData( pListView->iItem );
   if ( !pInfo->bInMap )
   {
     ProfilInfoArray infoList;
     infoList.Add( pInfo );
     AddProfilesToMap( infoList, m_activeState );

     // die state neu setzen, es kann sein, dass es geändert wurde!
     if( !pInfo->bInMap )
       m_profileList.SetCheck( pListView->iItem, FALSE );
   };
 };
}; // OnItemchangedProfilList

BOOL CProfilAuswahl::OnProfilAuswahlCommand( UINT nID ) 
{
  ProfilInfoArray infoList;

  bool bSomeAreInMap = false; // für add
  for ( int i = 0; i < m_profileList.GetItemCount(); i++)
  {
    if ( m_profileList.GetItemState( i, LVIS_SELECTED ) == LVIS_SELECTED )
    {
      ProfilInfo* pi = (ProfilInfo*)m_profileList.GetItemData( i );
      
      infoList.Add( pi );

      if( pi->bInMap )
        bSomeAreInMap = true;
    }
  };

  switch ( nID )
  {
  case ID_PROFIL_AUSWAHL_ADD:
    {
      if( !bSomeAreInMap || AfxMessageBox( IDS_PROFILE_OVERVIEW_REMOVE_PRF, MB_OKCANCEL | MB_ICONWARNING ) == IDOK )
        AddProfilesToMap( infoList, m_activeState );
    }
    break;

  case ID_PROFIL_AUSWAHL_REMOVE:
    {
      if( AfxMessageBox( IDS_PROFILE_OVERVIEW_REMOVE_PRF, MB_OKCANCEL | MB_ICONWARNING ) == IDOK )
        RemoveProfilesFromMap( infoList );
    }
    break;

  case ID_PROFILAUSWAHL_SAVE:
    {
      if( AfxMessageBox( IDS_PROFILE_OVERVIEW_SAVE_PRF, MB_OKCANCEL | MB_ICONWARNING ) == IDOK )
      {
        if( SaveChangesInMap( infoList ) )
          AfxMessageBox( IDS_PROFILE_SAVE_SUCCES, MB_ICONINFORMATION );
      }
    }
    break;

  case ID_PROFILAUSWAHL_SELECTALL:
    m_profileList.SetAllItemsState( LVIS_SELECTED, LVIS_SELECTED );
    break;
  
  default:
    return FALSE; // dieses Commando wird nicht hier behandelt
  };

  UpdateListStatus();
  
  return TRUE;
}

void CProfilAuswahl::OnUpdateProfilAuswahlToolbar(CCmdUI* pCmdUI) 
{
  pCmdUI->Enable( m_profileList.GetSelectedCount() > 0 );
}

void CProfilAuswahl::OnSelchangedTreeCtrl( NMHDR* pNMHDR, LRESULT* pResult )
{
  NM_TREEVIEW* pNMTreeView = (NM_TREEVIEW*)pNMHDR;

  State* state = (State*)pNMTreeView->itemNew.lParam;

  if ( state )
  {
    m_activeState = state;
    FillProfileList( state );
    m_profileList.SetAllItemsState( LVIS_SELECTED, LVIS_SELECTED );
    m_profileList.AutoSizeColumns();
  };
}; // OnSelchangeTreeCtrl


///////////////////////////////////////////////////////////////////////////
// Attribute

void CProfilAuswahl::SetDocument( CMapDoc* mapDoc, Project* project )
// setzt die Attribute m_doc und m_project, löscht alle Daten und liest die TreeCtrl ein
{
  m_mapDoc = mapDoc;
  m_project = project;
  
  DeleteContents();

  if ( !m_project || !m_mapDoc )
    return;
  
  // TreeCtrl füllen
  for ( int i = 0; i < m_project->GetWaterCount(); i++ )
  {
    CString wasserName = m_project->GetWaterName(i);
    HTREEITEM wasserItem = m_treeCtrl.InsertItem( wasserName, IMAGE_WATER, IMAGE_WATER );
    m_treeCtrl.SetItemData( wasserItem, DWORD(0) );
    State *zustand = m_project->GetFirstState();
    while (zustand)
    {
      if ( zustand->GetWaterName() == wasserName )
      {
        HTREEITEM zustandItem = m_treeCtrl.InsertItem( LPCTSTR(zustand->GetName()), IMAGE_STATE, IMAGE_STATE, wasserItem );
        m_treeCtrl.SetItemData( zustandItem, DWORD(zustand) );
      }; // if zustand->GetWaterName
      zustand = m_project->GetNextState();
    }; // while zustand
  }; // for i

  // m_profileData füllen
  CFileStatus fileStatus;

  CString profilePath = m_project->GetDataDir();

  CrossSection* cs = m_project->GetFirstCrossSection();
  while ( cs )
  {
    CString csPath = profilePath + cs->GetFileName();
    if ( !CFile::GetStatus( csPath, fileStatus ) )
    {
      cs = m_project->GetNextCrossSection();
      continue; // das Profil ist korrupt -> überspringen
    };

    // neues ProfilInfo erzeugen und mit Daten füllen
    ProfilInfo* pInfo = new ProfilInfo;
    pInfo->cs = cs;
    pInfo->state = NULL;
    pInfo->fileTime = fileStatus.m_mtime;
    pInfo->bInMap = FALSE;
    pInfo->bGeoreferenced = FALSE;
    pInfo->bModInMap = FALSE;
    pInfo->bModExtern = FALSE;

    m_profileData.Add( pInfo );
    
    cs = m_project->GetNextCrossSection();
  };

  // hier erfolgt der Abgleich  Daten im Strang <-> Daten in der Karte
  CStringArray profilNames;
  CStringArray stateNames;
  
  m_mapDoc->GetProfileNames( profilNames, stateNames );

  CStringArray missingProfiles;
  for( int n = 0; n < profilNames.GetSize(); n++ )
  {
    // id = 0 existiert nicht
    if( n == 0 )
      continue;

    CString fileName = profilNames[n];
    CString stateName = stateNames[n];

    bool bFound = false;
    for ( int i = 0; i < m_profileData.GetSize(); i++ )
    {
      if ( fileName == m_profileData[i]->cs->GetFileName() )
      {
        m_profileData[i]->state = m_project->GetState( stateName );
        m_profileData[i]->bInMap = TRUE;
        bFound = true;
        break;
      };
    }; // for i

    if( !bFound )
      missingProfiles.Add( fileName );
  }; // for n

  // erst die Vernetzung neu ausrechnen, da sie sonst nicht richtig in der Karte aktualisiert wird
  UpdateVernetzung();

  // jetzt die fehlenden Profile aus der Karte schmeissen
  if( missingProfiles.GetSize() > 0 )
  {
    AfxMessageBox( IDS_PROFIL_AUSWAHL_MISSING_PRFS, MB_OK | MB_ICONINFORMATION );
    m_mapDoc->RemoveProfiles( missingProfiles );
  }
}; // SetDocument


///////////////////////////////////////////////////////////////////////////
// Implementierung

void CProfilAuswahl::DeleteContents()
// löscht m_profilData
{
  for ( int i = 0; i < m_profileData.GetSize(); i++ )
  {
    if ( m_profileData[i] )
      delete m_profileData[i];
  };
  m_profileData.RemoveAll();

  if ( m_treeCtrl.GetSafeHwnd() )
    m_treeCtrl.DeleteAllItems();
  if ( m_profileList.GetSafeHwnd() )
    m_profileList.DeleteAllItems();

  DeleteVernetzung();
}; // DeleteContents

void CProfilAuswahl::DeleteVernetzung()
{
  POSITION pos = m_netz.GetStartPosition();
  while( pos )
  {
    CStringArray* strings;
    CString fileName;
    m_netz.GetNextAssoc( pos, fileName, strings );
    delete strings;
  }; // while pos
  m_netz.RemoveAll();
}; // DeleteVernetzung


/*
 * fügt Profile in die Karte bzgl. eines Zustandes ein
 * falls Profile bereits in der Karte vorhanden sind werden sie vorher gelöscht
 * falls der Benutzer dem zustimmt
 *
 * ändert bInMap
 * lädt Profile ( cs->LoadProfile )
 */
void CProfilAuswahl::AddProfilesToMap( ProfilInfoArray& profilInfos, State* state )
{
  int i;
  
  CString projectPath = m_project->GetDataDir() + "\\";
  ProfilInfoArray addedProfiles;
  CrossSectionArray cSections;
  StatesArray states;
  CStringArray removedCSections; // Dateinamen der zu löschenden Profile

  if ( !UpdateExternChanges() ) // falls Profile extern gelöscht wurden nix tun
  {                             // Benutzer muss evtl. Projekt neu laden
    AfxMessageBox( "Einige der Profile wurden extern gelöscht, Abbruch" );
    return;
  }

  CWaitCursor wait;
  
  // Liste der wirklich zu ladenden Profile erstellen, sicherheitsabfragen tätigen
  // CrossSections falls nötig initialisieren ( d.h. laden )
  for ( i = 0; i < profilInfos.GetSize(); i++ )
  {
    ProfilInfo* pInfo = profilInfos[i];
    if( pInfo->bInMap )
    { 
        // falls Profil bereits in der Karte vorhanden ist, prüfen ob es modifiziert wurde
        // TODO: bModInMap bislang unbenutzt!
        if( pInfo->bModInMap )
        {
          CString message;
          message.Format( "Profil %s wurde in der Karte verändert.\n Änderungen verwerfen?", pInfo->cs );
          switch ( AfxMessageBox( message, MB_YESNOCANCEL ) )
          {
          case IDYES:
            removedCSections.Add( pInfo->cs->GetFileName() );
            addedProfiles.Add( pInfo );
            break;

          case IDNO:
            break;

          case IDCANCEL:
            return;
          }; // switch
        } // if pInfo->bModInMap
        else // TODO: zur Zeit wird immer überschrieben ( d.h. vorher gelöscht, weil bModInMap noch nicht gesetzt wird )
        {
          removedCSections.Add( pInfo->cs->GetFileName() );
          addedProfiles.Add( pInfo );
        };
    } // if pInfo->bInMap
    else
      addedProfiles.Add( pInfo );
  }; // for i
  
  // die zu erneuernden Profile erst aus der Karte löschen
  if ( removedCSections.GetSize() > 0 )
    GetDocument()->RemoveProfiles( removedCSections );

  wait.Restore();  // möglicherweise durch RemoveProfiles geändert

  for ( i = 0; i < addedProfiles.GetSize(); i++ )
  {
    ProfilInfo* pInfo = addedProfiles[i];
    pInfo->bInMap = TRUE;
    pInfo->state = state;
    CrossSection* cs = pInfo->cs;
    if ( pInfo->bModExtern ) // extern geänderte Profile neu laden
      cs->FlushProfil();
    if ( !cs->GetProfil() )
      cs->LoadProfil(); // Profil laden ( falls nicht bereits geschehen )
    cSections.Add( cs );
    states.Add( state );
  };

  UpdateVernetzung();
  
  if ( cSections.GetSize() > 0 )
  {
    std::vector<bool> loadedProfiles = GetDocument()->AddProfiles( &cSections, &states );

    ASSERT( loadedProfiles.size() == addedProfiles.GetSize() );

    // die Profile, die nicht geladen werden konnten, als ungeladen markieren
    for( int i = 0; i < addedProfiles.GetSize(); i++ )
      addedProfiles[i]->bInMap = loadedProfiles[i];
  }
};

void CProfilAuswahl::RemoveProfilesFromMap( ProfilInfoArray& profilInfos )
{
  CStringArray cSections;
  CrossSectionArray saveSections;
  ProfilInfoArray removedProfiles;

  CWaitCursor wait;

  for ( int i = 0; i < profilInfos.GetSize(); i++ )
  {
    ProfilInfo* pInfo = profilInfos[i];
    if ( pInfo->bModInMap )
    {
      switch ( AfxMessageBox( "Profil wurde editiert: Änderungen speichern?", 
                               MB_YESNOCANCEL ) )
      {
      case IDYES:
        saveSections.Add( pInfo->cs );
        break;

      case IDNO:
        break;

      case IDCANCEL:
        return;
      };
    }; // if pInfo->bModInMap
    removedProfiles.Add ( pInfo );
  }; // for i

  for( i = 0; i < removedProfiles.GetSize(); i++ )
  {
    ProfilInfo* pInfo = removedProfiles[i];
    pInfo->bInMap = FALSE;
    pInfo->state = NULL;
    cSections.Add( pInfo->cs->GetFileName() );
  };

  /* TODO: bModInMap zur Zeit noch nicht implementiert
  if ( saveSections.GetSize() > 0 )
    GetDocument()->SaveProfileModifications( &saveSections );
  */

  wait.Restore();

  UpdateVernetzung();

  if ( cSections.GetSize() > 0 )
    GetDocument()->RemoveProfiles( cSections );
}; // RemoveProfileFromMap()

BOOL CProfilAuswahl::SaveChangesInMap( ProfilInfoArray& profilInfos, BOOL bAsk /* = TRUE */ )
// zur Zeit: alle in der Liste und in der Karte befindlichen Profile werden gespeichert
// TODO: nur die mit bModInMap sollen gespeichert werden
// Parameter:
//        ProfilInfoArray& profilInfos: diese Profile sollen gespeichert werden
//        BOOL bAsk: soll gefragt werden, welche Daten gespeichert werden sollen?
{
  CMapDoc* pDoc = GetDocument();
  if( !pDoc )
    return FALSE;

  CWaitCursor wait;

  // Liste der zu Speichernen Profile erstellen
  CrossSectionArray cSections;
  for ( int i = 0; i < profilInfos.GetSize(); i++ )
  {
    ProfilInfo* pInfo = profilInfos[i];
    if ( pInfo->bInMap )
    {
      CrossSection* cs = pInfo->cs;
      if ( !cs->GetProfil() )
        cs->LoadProfil();
      cSections.Add( cs );
    }; // if bInMap
  }; // for i
    
  // Änderungen in die Profile eintragen lassen
  if ( cSections.GetSize() > 0 )
  {
    if( pDoc->SaveProfileModifications( &cSections ) == TRUE )
    {
      // falls alles ok die Profile jetzt speichern
      CMainFrame* mainFrame = (CMainFrame*)AfxGetMainWnd();
      CProgressCtrl* progress = NULL;
      if( mainFrame )
        progress = mainFrame->CreateStatusBarProgress( CString(MAKEINTRESOURCE(IDS_SAVE_PROFILES)) );
      if( progress )
        progress->SetRange( 0, cSections.GetSize() );
      
      for ( i = 0; i < cSections.GetSize(); i++ )
      {
        cSections[i]->SaveProfil();
        if( progress )
          progress->StepIt();
      }; // for i
      
      if( mainFrame )
        mainFrame->DestroyStatusBarProgress();

      return TRUE;
    }
    else
    {
      // sonst die Änderungen in den Profilen verwerfen
      for( int i = 0; i < cSections.GetSize(); i++ )
        cSections[i]->FlushProfil();

      return FALSE;
    } // if pDoc->SaveProfileModifications
  } // if cSections.GetSize() > 0
  
  return FALSE;
}; // SaveChangesInMap

BOOL CProfilAuswahl::UpdateExternChanges()
// setzt bModExtern für m_profilData
// Rückgabewert:
//            gibt FALSE zurück, falls Profile verschwunden sind
{
  CString projectPath = m_project->GetDataDir() + "\\";
  CFileStatus fileStatus;

  for ( int i = 0; i < m_profileData.GetSize(); i++ )
  {
    ProfilInfo* pInfo = m_profileData[i];
    pInfo->bModExtern = FALSE;

    if ( !CFile::GetStatus( projectPath + pInfo->cs->GetFileName(), 
      fileStatus ) )
    {
      pInfo->bModExtern = TRUE;
      return FALSE;
    }
    else
    {
      if ( fileStatus.m_mtime > pInfo->fileTime )
        pInfo->bModExtern = TRUE;
    };
  };

  return TRUE;
};

void CProfilAuswahl::UpdateListStatus()
// setzt die versch. Statusanzeigen der Liste nach den Daten in m_profileData
{
  for ( int i = 0; i < m_profileList.GetItemCount(); i++ )
  {
    ProfilInfo* pInfo = (ProfilInfo*)m_profileList.GetItemData( i );
    m_profileList.SetCheck( i, pInfo->bInMap );
  };
};

void CProfilAuswahl::FillProfileList( State* state, ProfilInfo* showInfo /* = NULL */ )
// zeigt in der ProfilListe die Profile des aktuell ausgewählten Zustandes an
// Parameter:
//        State* state: alle Profile dieses Zustandes zeigen
//        ProfilInfo* pInfo: falls ungleich NULL wird dafür gesorgt, dass dieses Element angezeigt wird
// Bemerkung. es werden alle Profile angezeigt, deren Zustand gleich state ist, oder, 
//            deren Zustand NULL ist, aber welche zum Zustand state gehören.
//            d.h. Profile, welche unter einem anderen Zustand geladen wurden werden nicht angezeigt
{
  m_profileList.DeleteAllItems();
  int count = 0;
  int showIndex = -1; // merkt sich, welches Element angezeigt werden soll

  for ( int i = 0; i < m_profileData.GetSize(); i++ )
  {
    CString hlpStr;
    ProfilInfo* pInfo = m_profileData[i];
    CrossSection* cs = pInfo->cs;

    if ( pInfo->state == state || ( !pInfo->state && state->CrossSectionExists( cs ) ) )
    {
      int index = m_profileList.InsertItem( LVIF_TEXT | LVIF_STATE | LVIF_PARAM, ++count,
        cs->GetWaterName(), INDEXTOSTATEIMAGEMASK(1), LVIS_STATEIMAGEMASK, 0,LPARAM( pInfo ) );
      m_profileList.SetItem( index, 1, LVIF_TEXT, state->GetName(), 0, 0, 0, 0 );
      hlpStr.Format( "%.4lf", cs->GetStation() );
      m_profileList.SetItem( index, 2, LVIF_TEXT, hlpStr, 0, 0, 0, 0 );
      m_profileList.SetItem( index, 3, LVIF_TEXT, cs->GetFileName(), 0, 0, 0, 0 );
      
      m_profileList.SetCheck( index, pInfo->bInMap );

      if( showInfo == pInfo )
        showIndex = index;
    }; // if pInfo->state == state
  }; // for i

  if( showIndex > -1 )
    m_profileList.EnsureVisible( showIndex, FALSE );
}; // fillProfileList

BOOL CProfilAuswahl::ParseCommand( NMPROJECTMNG* command )
// Bearbeitet das Kommando aus dem Projektmanager
{
  ASSERT( m_project );
  ASSERT( GetSafeHwnd() );

  BOOL bErfolg = FALSE;
  if ( command && command->type == NMPROJECTMNG::createMap && command->stateFilename &&
       command->csArray )
  {
    // erstmal rausfinden, welcher zustand es ist.
    State* state = m_project->GetState( CString(command->stateFilename) );
    ProfilInfoArray pInfos;
    for ( int i = 0; i < command->csArray->GetSize(); i++ )
    {
      for ( int j = 0; j < m_profileData.GetSize(); j++ )
      {
        ProfilInfo* pInfo = m_profileData[j];
        if ( command->csArray->GetAt( i ) == pInfo->cs->GetFileName() )
          pInfos.Add( pInfo );
      }; // for j
    }; // for i
    
    if ( pInfos.GetSize() > 0 )
      AddProfilesToMap( pInfos, state );

  }; // if command ...

  return bErfolg;
}; // Parsecommand

void CProfilAuswahl::UpdateVernetzung()
// holt sich vom Projekt die aktuelle Vernetzung der geladenen Profile und teilt diese der Karte mit
{
  // zuerst die alte Vernetzung löschen
  DeleteVernetzung();

  CMapDoc* pDoc = GetDocument();
  if( !pDoc )
    return;

  // benutzt die entsprechende Funktion von Projekt::, muss aber den Input/Output von String nach CrossSection umbauen

  // eine Liste der geladenen Profile zusammenstellen
  CrossSectionArray csArray;
  StatesArray stateArray;
  for( int i = 0; i < m_profileData.GetSize(); i++ )
  {
    ProfilInfo* pInfo = m_profileData[i];
    if( pInfo->bInMap )
    {
      csArray.Add( pInfo->cs );
      stateArray.Add( pInfo->state );
    }; // if pInfo->bInMap
  }; // for i

  Project::Vernetzung* projektNetz = m_project->GetVernetzung( &csArray, &stateArray );
  
  // jetzt den Output erstellen
  if( projektNetz )
  {
    POSITION pos = projektNetz->GetStartPosition();
    while( pos )
    {
      CrossSection* cs;
      CrossSectionArray* csArray;
      
      projektNetz->GetNextAssoc( pos, cs, csArray );
      if( cs && csArray )
      {
        if( csArray->GetSize() != 4 ) // es müssen immer genau 4 sein!
          continue;

        CStringArray* fileNames = new CStringArray;
        for( int i = 0; i < csArray->GetSize(); i++ )
        {
          CrossSection* bildCs = csArray->GetAt( i );
          if( bildCs )
            fileNames->Add( bildCs->GetFileName() );
          else
            fileNames->Add( CString() ); // sonst leeren String -> siehe Definition von Vernetzung
        }; // for i
        if( fileNames->GetSize() > 0 )
          m_netz.SetAt( cs->GetFileName(), fileNames );
        else
        {
          delete fileNames;
          fileNames = NULL;
        }; // for i
      }; // if cs && csArray

      // zuletzt auch noch die Daten löschen, werden nicht mehr gebraucht
      delete csArray;
      csArray = NULL;
    }; // while pos
  }; // if projektNetz

  // und den Rest des Netzes löschen
  delete projektNetz;
  projektNetz = NULL;

  pDoc->SetProfileNet( &m_netz );
}; // GetVernetzung

void CProfilAuswahl::EditProfile( const CString& csFile, const CString& stFile )
// startet den Graphikeditor von WspWin mit dem angegebenen Profil
{
  CMainFrame* mFrame = (CMainFrame*)AfxGetMainWnd();
  Project* proj = m_project;
  if( !mFrame || !m_project )
    return;

  // zuerst WSPWIN starten, falls nicht schon geschehen
  CProcessHelper::StartExternProcess( TEXT("WSPWIN.EXE") );
  
  // dann den GraphikEditor anstossen
  CString m_DDECommand;
  m_DDECommand.Format("[greditor(\"%s\",\"%s\",\"%s\")]", proj->GetDir(), stFile, csFile );
  mFrame->ExecuteDDECommand( TEXT("WSPWIN"), TEXT("SYSTEM"), m_DDECommand );
}; // EditProfile

bool CProfilAuswahl::AddNewProfile( TripleArray* triples, const CString profilFiles[2], const CString stateFiles[2],
                                    const double profilAbstaende[2], const CString& comment, const bool bLoad /* = true */ )
// fügt ein neues Profil dem Projekt hinzu und lädt es ggfls. in die Karte
// Parameter:
//        Profil* profil: das hinzuzufügende Profil
//        const CString& profilFiles[2]: die Dateinamen des vorhergehenden und nachfolgenden Profils, es wird versucht das neue Profil dazwischen einzufügen
//        const CString& profilAbstaende[2]: die dazugehörenden Abstände in metern, die Stationierung und die Strangabstände werden hiervon abgeleitet
//        BOOL bLoad: falls TRUE, wird das Profil auch gleich in die Karte geladen
//        const double* stateName: falls nicht 0, wird versucht das Profil in diesen Zustand zu laden
//        const double* station: falls nicht 0, wird diese Station vorgeschlagen
//        const int* vzk: falls nicht 0, wird diese Verzweigungskennung benutzt
// Rückgabewert:
//        FALSE, falls ein Fehler auftrat
{
  if( !triples )
    return FALSE;

  // Überprüfen, ob WSPWIN offen ist. Falls ja, eine Warnung ausgeben
  if( IsTaskActive( "WSPWIN.EXE" ) &&
      AfxMessageBox( IDS_GEN_PROFILE_WSPWIN_OPEN, MB_OKCANCEL ) == IDCANCEL )
    return FALSE;

  // erstmal alle Eingangsdaten überprüfen und zusätzliche Infos aus den Strängen und den Profilen holen
  // für die zusätzlichen Infos brauchen wir ein paar Variable:
  StatesArray newStates; // Liste der States unter welchen das neue Profil eingefügt werden kann
  double newStation = 0.0; // die neue Station des Querprofils
  CUIntArray newVzks;  // die möglichen VZKs des neuen Profils

  // zuerst die beiden CrossSections finden:
  CrossSection* csNachbarn[2] = { NULL, NULL }; // die beiden benachbarten CrossSections
  State* stateNachbarn[2] = { NULL, NULL };      // die Zustände unter welchen sie geladen sind
  double abstNachbarn[2];
  abstNachbarn[0] = profilAbstaende[0];   // werden möglicherweise vertauscht
  abstNachbarn[1] = profilAbstaende[1];

  for( int i = 0; i < m_profileData.GetSize(); i++ )
  {
    ProfilInfo* pInfo = m_profileData[i];
    if( !pInfo )
      continue;

    CrossSection* cs = pInfo->cs;
    State* state = pInfo->state;
    if( !cs || !state )
      continue;

    CString csFile = cs->GetFileName();
    CString stateFile = state->GetFileName();

    for( int j = 0; j < 2; j++ )
    {
      CString nachbarFile = profilFiles[j];
      CString nachbarState = stateFiles[j];
      if( nachbarFile.CompareNoCase( csFile ) == 0 && nachbarState.CompareNoCase( stateFile ) == 0 )
      {
        csNachbarn[j] = cs;
        stateNachbarn[j] = state;
      }; // for j
    }; // for j
  }; // for i
  

  // Überprüfung der Voraussetzungen
  int error = 0; // falls ungleich 0 wird eine entsprechende Nachricht angezeigt und abgebrochen
  BOOL bVertausche = FALSE; // möglicherweise müssen die Rollen von Vorgänger und Nachfolger vertauscht werden
  if( csNachbarn[0] && csNachbarn[1] )
  {
    // müssen gleichen Zustand haben
    if( stateNachbarn[0] != stateNachbarn[1] || stateNachbarn[0] == NULL )
      error = 1; // "nicht im gleichen Zustand
    else
    {
      State* zustand = stateNachbarn[0];
      newStates.Add( zustand );
      

      // jetzt testen, ob und wie die beiden Profile benachbart sind
      error = 2; // im Zweifelsfall sind sie nicht benachbart

      // alle vier möglichen Nachfolge Zustände durchgehen
      // Bemerkung: es reicht wenn das folgenden Profil gleiche Station und gleiche 
      // VZK hat ( operator== auf CSection ), wegen Mehrfeldbrücken
      for( int bRichtung = 0; bRichtung < 2; bRichtung++ )
      {
        for( int bSeite = 0; bSeite < 2; bSeite++ )
        {
          CrossSection* csFollow = csNachbarn[0];

          // falls dieses Profile( 0 ) eine Mehrfeldbrücke ist,
          // solange suchen bis das echte nächste kommt
          while( csFollow != NULL && *csFollow == *csNachbarn[0] )
            csFollow = zustand->GetFollowingCs( csFollow, CrossSectionArray(), bRichtung, bSeite );

          if( csFollow != NULL && *csFollow == *(csNachbarn[1]) )
          {
            // falls das andere als Nachfolger(Vorgänger) gefunden wurde, reicht es nun aus,
            // dass es einer der beiden Vorgänger(Nachfolger) war
            for( int c = 0; c < 2; c++ )
            {
              CrossSection* csPred = zustand->GetFollowingCs( csFollow, CrossSectionArray(), !bRichtung, c );
              if( csPred != NULL && *csPred == *(csNachbarn[0]) )
              {
                error = 0;
                bVertausche = bRichtung;
                break;
              };
            }; // for c
          }; // if ... = csNachbarn[1]
        }; // for bSeite
      }; // for BRichtung

      if( error == 0 )
      {
        // alles ok, die echten Abstände und die Stationierung ausrechnen sowie alle Daten für den 
        // Dialog initialisieren ausrechen

        // zuerst die Station:
        double stationPred = csNachbarn[0]->GetStation();
        double stationPost = csNachbarn[1]->GetStation();
        newStation = ( ( abstNachbarn[1] * stationPred ) + ( abstNachbarn[0] * stationPost ) ) /
                      ( abstNachbarn[0] + abstNachbarn[1] );

        newVzks.Add( csNachbarn[0]->GetVZK() );
        if( csNachbarn[0]->GetVZK() != csNachbarn[1]->GetVZK() )
          newVzks.Add( csNachbarn[1]->GetVZK() );
      }; // if error == 0
    }; // state0 == state1 ?
  }
  else if( csNachbarn[0] || csNachbarn[1] )
  {
    int nachbar = csNachbarn[0] ? 0 : 1; // welcher Nachbar ists denn?

    // prüfen, ob das wirklich der letzte in der Strangtabelle ist
    State* zustand = stateNachbarn[nachbar];
    if( zustand )
    {
      // zuerst den Zustand, in welchem das Profil geladen ist
      newStates.Add( zustand );
      // dann alle anderen Zustände
      State* otherState = csNachbarn[nachbar]->GetFirstState();
      while( otherState )
      {
        if( otherState != zustand )
          newStates.Add( otherState );
        otherState = csNachbarn[nachbar]->GetNextState();
      }; // while other State


      // auch noch die VZk
      newVzks.Add( csNachbarn[nachbar]->GetVZK() );

      if( zustand->GetFollowingCs( csNachbarn[nachbar], CrossSectionArray(), TRUE, TRUE ) == NULL )
      {
        // profil hat keinen Nachfolger
        // also neues profil als Nachfolger anhängen
        // falls index = 0 alles ok, sonst nachbarn vertauschen
        if( nachbar == 1 )
          bVertausche = TRUE;
        newStation = csNachbarn[nachbar]->GetStation() - abstNachbarn[nachbar] / 1000.0;
      }
      else if( zustand->GetFollowingCs( csNachbarn[nachbar], CrossSectionArray(), FALSE, TRUE ) == NULL )
      {
        // profil hat keinen Vorgänger
        // also neues profil als Vorgänger anhängen
        // falls index = 1 alles ok, sonst nachbarn vertauschen
        if( nachbar == 0 )
          bVertausche = TRUE;

        newStation = csNachbarn[nachbar]->GetStation() + abstNachbarn[nachbar] / 1000.0;
      }
      else
        error = 11; // ist nicht am Ende des Strangs
    }
    else
      error = 10;
  }
  else
  {
    State* zustand = m_project->GetFirstState();
    while( zustand )
    {
      newStates.Add( zustand );
      zustand = m_project->GetNextState();
    }; // while pos

    newVzks.Add( 0 );

    if( newStates.GetSize() == 0 )
      error = 20; // keine leeren Zustände da -> Fehler und Abbruch
  }; // Prüfen der Voraussetzungen

  // falls ein Fehler passiert ist, jetzt Meldung geben und abbrechen
  CString errorMessage;
  switch( error )
  {
  case 0: // kein Fehler
    break;
    // 1 - 9: es gibt zwei benachbarte Profile
  case 1:
    errorMessage.LoadString( IDS_PROFGEN_ERROR_VERSCH_ZUSTAENDE );
    break;

  case 2:
    errorMessage.LoadString( IDS_PROFGEN_ERROR_NICHT_BENACHBART );
    break;

    // 10 - 19: es gibt ein benachbartes Profil
  case 10:
    errorMessage.LoadString( IDS_PROFGEN_ERROR_PROJECT );
    break;

  case 11:
    errorMessage.LoadString( IDS_PROFGEN_ERROR_NOTLAST );
    break;

    // > 20: es gibt kein benachbartes Profil
  case 20:
    errorMessage.LoadString( IDS_PROFGEN_NO_EMPTYSTATES );
    break;

  default:
    ASSERT( FALSE ); // da hab ich wohl was vergessen
  }; // switch error 

  // falls ein Fehlermeldung da ist, diese anzeigen und abbrechen
  if( !errorMessage.IsEmpty() )
  {
    AfxMessageBox( errorMessage, MB_ICONERROR );
    return FALSE;
  }; // if errorMessage != ""


  // falls vertauscht werden soll, dies jetzt tun
  if( bVertausche )
  {
    CrossSection* helpCs = csNachbarn[0];
    csNachbarn[0] = csNachbarn[1];
    csNachbarn[1] = helpCs;
    
    double helpAbst = abstNachbarn[0];
    abstNachbarn[0] = abstNachbarn[1];
    abstNachbarn[1] = helpAbst;
  }; // if bVertausche


  // noch ein paar Variable für den Dialog
  CStringArray stateNames;
  for( i = 0; i < newStates.GetSize(); i++ )
  {
    State* state = newStates[i];
    if( state )
      stateNames.Add( state->GetName() );
  }; // for i

  CString predFile, followFile, newFile;
  double predStation = 0, followStation = 0;
  int predVzk = 0, followVzk = 0;
  CString predPK, followPK, newPK;
  BOOL bPred = FALSE, bFollow = FALSE; // sind sie denn vorhanden

  if( csNachbarn[0] )
  {
    CrossSection* cs = csNachbarn[0];
    predFile = cs->GetFileName();
    predStation = cs->GetStation();
    predVzk = cs->GetVZK();
    predPK = cs->GetPK();
    bPred = TRUE;
  }; // if csNachbarn[0]

  if( csNachbarn[1] )
  {
    CrossSection* cs = csNachbarn[1];
    followFile = cs->GetFileName();
    followStation = cs->GetStation();
    followVzk = cs->GetVZK();
    followPK = cs->GetPK();
    bFollow = TRUE;
  }; // if csNachbarn[0]

  newPK = "0";

  // ansonsten die Vorschau anzeigen
  CString titel( MAKEINTRESOURCE(IDS_GENERATEPROFILE_TITEL) );
  CString description( CString(MAKEINTRESOURCE( IDS_GENPROFILE_DESCRIPTION ) ) );


  // temporäres Profil erzeugen
  std::auto_ptr<Profil> tmpProfil( Profil::CreateFromTriple( *triples ) );

  CGenProfileDlg genDlg( titel, description, &*tmpProfil, stateNames, predFile, followFile,
                         newFile, predStation, followStation, newStation,
                         predVzk, followVzk, newVzks,
                         predPK, followPK, newPK,
                         abstNachbarn[0] / 1000.0, abstNachbarn[1] / 1000.0,
                         bPred, bFollow,
                         triples,
                         NULL );

  if( genDlg.DoModal() == IDOK )
  {
    // die Parameter des Profils wurden alle ermittelt -> das Profil jetzt dem Zustand übergeben
    CString newStateName = genDlg.GetNewState();
    State* newState = m_project->FindStateByName( newStateName );
    double newStation = genDlg.GetNewStation();
    CString newPk = genDlg.GetNewPk();
    int newVzk = genDlg.GetNewVzk();


    // jetzt das Profil tatsächlich hinzufügen
    AddNewProfile( triples, bLoad, newState, newStation, newVzk, newPK, comment );
  }; // if IDOK

  return TRUE;
}; // AddNewProfile


/*!
 * Fügt ein neues Profil in das Projekt und ggfls. in die Karte ein
 *
 * @param profil : das neue Profil
 * @param bLoad : falls TRUE, wird das Profil sofort in die Karte übernommen
 * @param stateName : zu diesem Zustand wird das Profil hinzugefügt
 * @param station : an dieser Station
 * @param vzk : mit dieser VZK
 * @param pk : mit dieser PK
 *
 * @return BOOL  : 
 */
void CProfilAuswahl::AddNewProfile( TripleArray* triples, bool bLoad, State* state, const double station, const int vzk, const CString& pk, const CString& comment )
{
  // Voraussetzungen prüfen
  if( !triples || !state )
    return;

  // ein Profil aus den Triples-Daten erzeugen
  Profil* profil = Profil::CreateFromTriple( *triples );
  if( comment.GetLength() != 0 )
    profil->AddComment( comment );

  // das Profil dem Projekt als CrossSection übergeben
  CrossSection* newCs = m_project->AddCrossSection( profil, state->GetWaterName(), state->GetName(), station, pk, vzk );

    // und das Profil in den Strang einsortieren
  state->AddCrossSection( newCs );

  // dann das Projekt speichern
  m_project->Save();

  // und zuletzt das Profil zur ProfilÜbersicht hinzufügen und ggfls. gleich laden
  ProfilInfo* pInfo = new ProfilInfo;
  pInfo->cs = newCs;
  pInfo->state = state;
  pInfo->bInMap = FALSE;
  pInfo->bGeoreferenced = TRUE;
  pInfo->bModInMap = FALSE;
  pInfo->bModExtern = FALSE;
  
  m_profileData.Add( pInfo );
  
  if( bLoad == true )
  {
    ProfilInfoArray infoArray;
    infoArray.Add( pInfo );
    AddProfilesToMap( infoArray, state ); // Profil hinzufügen aber Ausschnitt nicht ändern
  }; // if bLoad
  
  // auf jeden Fall die Profilübersicht Updaten
  ShowPInfo( pInfo );
}; // AddNewProfile


void CProfilAuswahl::DeleteProfiles( const CStringArray& profilNames, const BOOL bRemoveFromStrand )
// löscht die Profile aus der Karte und aus jweils dem Zustand unter dem sie eingetragen waren
// Parameter:
//        const CStringArray& profilNames: die Dateinamen der zu löschenden Profile
//        const BOOL bDeleteFiles: falls, true werden auch die Dateien gelöscht, falls es der letzte
//                          Zustand war, aus dem das Profil gelöscht wird
{
  // aus den Namen/States eine ProfilListe machen
  ProfilInfoArray profilList;
  
  // die states in einer eigenen Liste merken, weil sie durch RemoveProfileFromMap 
  // auf NULL gesetzt werden
  StatesArray stateList; 

  for( int i = 0; i < m_profileData.GetSize(); i++ )
  {
    ProfilInfo* pInfo = m_profileData[i];
    if( pInfo != NULL )
    {
      CrossSection* cs = pInfo->cs;
      if( cs )
      {
        CString fileName = cs->GetFileName();

        for( int j = 0; j < profilNames.GetSize(); j++ )
        {
          CString csName = profilNames[j];
          if( fileName.CompareNoCase( csName ) == 0 )
          {
            profilList.Add( pInfo );
            stateList.Add( pInfo->state ); 
            break; // Verlässt die j Schleife
          }
        }; // for j
      }; // if cs
    }; // if pInfo
  } // for i

  // als erstes die Profile aus der Karte nehmen
  RemoveProfilesFromMap( profilList );

  if( !bRemoveFromStrand )
  {
    UpdateListStatus();
    return;
  };

  // die Profile aus dem Projekt entfernen
  // und die Profile tatsächlich löschen, falls bDelete = TRUE
  BOOL bAnswer = IDYES; // irgendwas ungleich IDNEVER, IDALWAYS oder IDCANCEL
  for( i = 0; i < profilList.GetSize(); i++ )
  {
    ProfilInfo* pInfo = profilList[i];
    State* state = stateList[i];
    if( pInfo && state )
    {
      if( state->RemoveCrossSection( pInfo->cs ) )
      {
        CrossSection* cs = pInfo->cs;

        // falls es in keinem anderen Zustand mehr ist, fragen, ob die Datei gelöscht werden soll
        if( bAnswer != IDALWAYS && bAnswer != IDNEVER )
        {
          CString text;
          text.Format( IDS_PROFIL_AUSWAHL_DELETE_TEXT, cs->GetStation(), cs->GetVZK(), cs->GetFileName() );
          
          CMessageBox5 dlg( CString( MAKEINTRESOURCE( IDS_PROFIL_AUSWAHL_DELETE_TITLE ) ), text, this );
          bAnswer = dlg.DoModal();
        }; // if bAnswer

        switch( bAnswer )
        {
        case IDYES:
        case IDALWAYS:
          // jetzt die Profildatei tatsächlich löschen
          pInfo->cs->RemoveFile();
          break;

        case IDNO:
        case IDNEVER:
          break;

        case IDCANCEL:
          break;
        }; // switch

        // auf jeden Fall die CrossSection jetzt zerstören ( wurde aus dem Project genommen )
        delete cs;
        pInfo->cs = NULL;
      }; // if RemoveCrossSection
    }; // if pInfo

    if( bAnswer == IDCANCEL )
      break; // Verlässt for i

  }; // for i

  // das Projekt speichern
  m_project->Save();

  // und die Daten aus der ProfilListe nehmen
  for( i = 0; i < m_profileData.GetSize(); i++ )
  {
    ProfilInfo* pInfo = m_profileData[i];
    if( pInfo == NULL || pInfo->cs == NULL )
    {
      m_profileData.RemoveAt( i );
      delete pInfo;
      i--;
    };
  }; // for i

  if( m_activeState != NULL )
    FillProfileList( m_activeState );

  UpdateVernetzung();
}; // DeleteProfiles


void CProfilAuswahl::ShowPInfo( ProfilInfo* pInfo )
// sorgt dafür, dass ein Profil in der Liste sichtbar ist
// Parameter:
//        ProfilInfo* pInfo: das anzuzeigende Profil
{
  if( !pInfo )
    return;

  // zuerst mal den Zustand im Baum anzeigen und die ProfilListe erzeugen
  SelectState( pInfo->state );
  FillProfileList( pInfo->state, pInfo );
}; // ShowPInfo

void CProfilAuswahl::SelectState( State* zustand )
// sorgt dafür, das ein Zustand im Baum selektiert wird
// Parameter:
//        State* zustand: der zu selektierende Zustand
{
  // den Baum durchsuchen
  HTREEITEM hGewaesserItem = m_treeCtrl.GetRootItem();
  while( hGewaesserItem )
  {
    HTREEITEM hStateItem = m_treeCtrl.GetChildItem( hGewaesserItem );
    while( hStateItem )
    {
      State* otherState = (State*)m_treeCtrl.GetItemData( hStateItem );
      if( otherState = zustand )
      {
        m_treeCtrl.Select( hStateItem, TVGN_CARET );
        return;
      };

      hStateItem = m_treeCtrl.GetNextSiblingItem( hStateItem );
    }; // while hStateItem

    hGewaesserItem = m_treeCtrl.GetNextSiblingItem( hGewaesserItem );
  }; // while hGewaesserItem

}; // ShowState