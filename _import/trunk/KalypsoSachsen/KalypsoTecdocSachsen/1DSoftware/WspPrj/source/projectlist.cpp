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
// R�ckgabwert:
//          vollst�ndiger ( mit Dateiname ) und absoluter Pfad auf die Wsp.Prj
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
// l�dt die projectliste aus der wsp.prj
// und die Projektbezeichnungen aus den projbez.txt
// R�ckgabewert:
//          TRUE bei Erfolg, FALSE bei Misserfolg der Operation
// Bemerkung:
//    evtl. Listeneintr�ge werden jetzt gel�scht
{
  BOOL bReturn = TRUE;

  CString wspDir = GetWspPath();
  if ( wspDir == "" )
    return FALSE;

  try
  {
    CStdioFile wspFile( wspDir, CFile::modeRead );
    
    // ProjectListe l�schen, falls nicht leer
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
	  // die Datei konnte nicht ge�ffnet werden
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
// f�gt einen neuen Projekteintrag ein. Gegebenenfalls wird ein neues Projekt erzeugt
// Parameter:
//        const CString& projectPath:  Pfad auf das Projektverzeichnis, darf nicht bereits vorhanden sein
//        const BOOL exists: falls TRUE wird versucht das Projekt zu laden und falls 
//                           dies erfolgreich ist, wird der Projekteintrag hinzugef�gt
//                           falls FALSE, wird ein neues Projekt erzeugt
// R�ckgabewert:
//          Project*: Zeiger auf das neue Projekt, NULL bei Misserfolg
// Bemerkung:
//        falls ein neues Projekt erzeugt werden soll, wird ohne R�cksicht in den gegebenen 
//        Pfad reingeschrieben
//        das erzeugte Project ist nach der Operation stets geladen
//        falls ein Projekt mit diesem Pfad bereits vorhanden ist, wird NULL zur�ckgegeben
{
  ASSERT( projectPath != "" );

  // zuerst suchen, ob diese Projekt bereits vorhanden ist
  if ( GetProject( projectPath ) )
    return NULL;

  // jetzt pr�fen, ob es bereits ein Project mit diesem Pfad existiert -> dann nur 
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
// l�scht ein Projekt aus der ProjektListe
// Parameter:
//        const Project* projekt: das zu l�schende Projekt
//        BOOL deleteFiles: falls, TRUE, werden die Projektdateien vollst�ndig gel�scht
// R�ckgabewert:
//          TRUE / FALSE, je nach Erfolg der Operation
// Bemerkung:
//        die Datenstruktur des Projektes wird ebenfalls vollst�dnig gel�scht, d.h. der Zeiger projekt
//        ist nach dieser Operation nicht mehr g�ltig und wird deshalb auf NULL gesetzt
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

  // jetzt das Projekt selbst l�schen
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
// gibt ein Projekt aus der Liste zur�ck
// Parameter:
//        const int index: Nummer des gesuchten Projektes
// R�ckgabewert:
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
// R�ckgabewert:
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
// R�ckgabewert:
//        int:  index des Projectes oder -1, falls es nicht in der Liste ist
{
  for ( int i = 0; i < projectList.GetSize(); i++ )
  {
    if ( projectList[i] == project )
      return i;
  };

  return -1;
}; // GetProjectID