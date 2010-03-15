////////////////////////
// ProjectList.cpp    //
////////////////////////
// Klasse ProjectList //
////////////////////////

#include "stdafx.h"

#include "project.h"

#include "projectlist.h"


// Konstructoren / Destructoren

ProjectList::ProjectList()
{
  validLoad = Load();
};

ProjectList::~ProjectList()
{
  DeleteContents();
};

void ProjectList::DeleteContents()
{
  for ( int i = 0; i < projectList.GetSize(); i++ )
  {
    Project* project = projectList[i];
    if ( project )
      delete project;
  };
  projectList.RemoveAll();
};

/////////////////
// Operationen //
/////////////////

CString ProjectList::GetWspPath() const
// findet Pfad auf die Wsp.Prj
// Rückgabwert:
//          vollständiger ( mit Dateiname ) und absoluter Pfad auf die Wsp.Prj
//                    "", falls Operation ein Misserfolg
// Bemerkung:
// die Position der wsp.prj befindet sich in der wspwin.ini
// in der Zeile PRJPATH=...
{
  TCHAR windowsDir[MAX_PATH];
  UINT returnValue = GetWindowsDirectory( windowsDir, MAX_PATH );
  if ( returnValue > MAX_PATH || returnValue == 0 )
    return CString( "" );
  
  TCHAR wspDir[MAX_PATH];
  GetPrivateProfileString( TEXT("WSPWIN"), TEXT("PRJPATH"), windowsDir, wspDir, MAX_PATH, 
    TEXT("WSPWIN.INI" ) );

  return CString( wspDir ) + "\\wsp.prj";
};

BOOL ProjectList::Load()
// lädt die projectliste aus der wsp.prj
// und die Projektbezeichnungen aus den projbez.txt
// Rückgabewert:
//          TRUE bei Erfolg, FALSE bei Misserfolg der Operation
// Bemerkung:
//    evtl. Listeneinträge werden jetzt gelöscht
{
  BOOL bReturn = TRUE;

  CString wspDir = GetWspPath();
  if ( wspDir == "" )
    return FALSE;

  try
  {
    CStdioFile wspFile( wspDir, CFile::modeRead );
    
    // ProjectListe löschen, falls nicht leer
    DeleteContents();

    CString projectString;
    while ( wspFile.ReadString( projectString ) )
    {
      projectString.TrimLeft();
      projectString.TrimRight();
      if ( projectString.GetLength() > 0 )
      {
        Project* project = new Project( projectString );
        project->LoadProjectName();
        projectList.Add( project );
      };
    };

    wspFile.Close();
  }
  catch( CFileException* e )
  {
	  // die Datei konnte nicht geöffnet werden
	  // eine neue erzeugen?
	  switch( e->m_cause )
	  {
	  case CFileException::fileNotFound:
		  {
			  try
			  {
				  CStdioFile wspFile( wspDir, CFile::modeCreate );
				  wspFile.Close();
			  }
			  catch( CFileException* e2 )
			  {
				  bReturn = FALSE;
				  e2->Delete();
			  }
		  }
		  break;

	  default:
		  bReturn = FALSE;
		  break;
	  }
	  
    e->Delete();
  };

  return bReturn;
}; // Load

BOOL ProjectList::Save() const
// speichert die ProfilListe in die wsp.prj
// und die Projektnamen in die jeweilige projbez.txt
{
  BOOL bReturn = TRUE;

  CString wspDir = GetWspPath();
  if ( wspDir == "" )
    return FALSE;

  try
  {
    CStdioFile wspFile( wspDir, CFile::modeCreate | CFile::modeWrite );

    for ( int i = 0; i < projectList.GetSize(); i++ )
    {
      Project* project = projectList[i];
      wspFile.WriteString( project->GetDir() + "\n" );
      project->SaveProjectName();
    };

    wspFile.Close();
  } // try
  catch( CFileException* e )
  {
    e->Delete();
    bReturn = FALSE;
    throw;
  }; // catch

  return bReturn;
}; // Save

Project* ProjectList::AddProject( const CString& projectPath, const BOOL exists )
// fügt einen neuen Projekteintrag ein. Gegebenenfalls wird ein neues Projekt erzeugt
// Parameter:
//        const CString& projectPath:  Pfad auf das Projektverzeichnis, darf nicht bereits vorhanden sein
//        const BOOL exists: falls TRUE wird versucht das Projekt zu laden und falls 
//                           dies erfolgreich ist, wird der Projekteintrag hinzugefügt
//                           falls FALSE, wird ein neues Projekt erzeugt
// Rückgabewert:
//          Project*: Zeiger auf das neue Projekt, NULL bei Misserfolg
// Bemerkung:
//        falls ein neues Projekt erzeugt werden soll, wird ohne Rücksicht in den gegebenen 
//        Pfad reingeschrieben
//        das erzeugte Project ist nach der Operation stets geladen
//        falls ein Projekt mit diesem Pfad bereits vorhanden ist, wird NULL zurückgegeben
{
  ASSERT( projectPath != "" );

  // zuerst suchen, ob diese Projekt bereits vorhanden ist
  if ( GetProject( projectPath ) )
    return NULL;

  // jetzt prüfen, ob es bereits ein Project mit diesem Pfad existiert -> dann nur 
  // Projekt in die Liste eintragen
  Project* project = new Project( projectPath );

  if ( exists )
  { // nur Projekteintrag erstellen -> testen, ob Projekt in Ordnung ist
    if ( !project->Load() )
    {
      delete project;
      project = NULL;
    };
  }
  else
  { // neues Projekt erzeugen
    if( !project->Create() || !project->Load() )
    {
      delete project;
      project = NULL;
    };
  }; // if exists

  // falls das Project ok ist, es jetzt an den Anfagn der Projektliste setzen
  if( project != NULL )
    projectList.InsertAt( 0, project );

  return project;
}; // AddProject

BOOL ProjectList::RemoveProject( Project* project, BOOL deleteFiles )
// löscht ein Projekt aus der ProjektListe
// Parameter:
//        const Project* projekt: das zu löschende Projekt
//        BOOL deleteFiles: falls, TRUE, werden die Projektdateien vollständig gelöscht
// Rückgabewert:
//          TRUE / FALSE, je nach Erfolg der Operation
// Bemerkung:
//        die Datenstruktur des Projektes wird ebenfalls vollstädnig gelöscht, d.h. der Zeiger projekt
//        ist nach dieser Operation nicht mehr gültig und wird deshalb auf NULL gesetzt
{
  BOOL erfolg = FALSE;

  int index = GetProjectID( project );
  if ( index != -1 )
  {
    projectList.RemoveAt( index );
    if ( deleteFiles )
      erfolg = project->Remove();
    else
      erfolg = TRUE;
  };

  // jetzt das Projekt selbst löschen
  if ( project )
  {
    delete project;
    project = NULL;
  };

  return erfolg;
}; // RemoveProject

///////////////
// Attribute //
///////////////

int ProjectList::GetCount() const
{
  return projectList.GetSize();
}; // GetCount

Project* ProjectList::GetProject( const int index ) const
// gibt ein Projekt aus der Liste zurück
// Parameter:
//        const int index: Nummer des gesuchten Projektes
// Rückgabewert:
//        Zeiger auf das Project oder NULL, falls index ausserhalb der Grenzen liegt
{
  if ( index >= 0 && index < projectList.GetSize() )
    return projectList[index];
  else
    return NULL;
}; // GetProject

Project* ProjectList::GetProject( const CString& projectPath ) const
// sucht Project nach seinem Projectpfad
// Parameter:
//        const CString& projectPath: Pfad auf das Project
// Rückgabewert:
//          Zeiger auf das gefundene Project oder NULL, falls es nicht existiert
{
  CString projectPathCopy = projectPath;
  projectPathCopy.TrimLeft();
  projectPathCopy.TrimRight();
  for ( int i = 0; i < projectList.GetSize(); i++ )
  {
    Project* project = projectList[i];
    if ( projectPathCopy.CompareNoCase( project->GetDir() ) == 0 )
      return project;
  };

  return NULL;
};

int ProjectList::GetProjectID( const Project* project ) const
// sucht den Index eines Projektes in der Liste
// Parameter:
//        const Project* project: das gesuchte Project
// Rückgabewert:
//        int:  index des Projectes oder -1, falls es nicht in der Liste ist
{
  for ( int i = 0; i < projectList.GetSize(); i++ )
  {
    if ( projectList[i] == project )
      return i;
  };

  return -1;
}; // GetProjectID