// ProjMngExport.h: Definitionsdatei für struct NMPROJECTMNG

#ifndef _PROJMNG_EXPORT_H_INCLUDED_
#define _PROJMNG_EXPORT_H_INCLUDED_

class CStringArray;

struct NMPROJECTMNG
{
  typedef enum CommandType 
  { 
    closeProgram,
    openProject,
    openMap, 
    createMap,
    deleteMap
  };

  NMPROJECTMNG() // standardkonstruktor
  {
    type = closeProgram;
    projectPath = NULL;
    stateFilename = NULL;
    csArray = NULL;
  };
  NMPROJECTMNG( CommandType type, LPCTSTR projectPath = NULL, LPCTSTR stateFilename = NULL, 
                CStringArray* csArray = NULL )
  {
    this->type = type;

    if ( projectPath )
    {
      this->projectPath = (LPTSTR)malloc( ( lstrlen( projectPath ) + 1 ) * sizeof( TCHAR ) );
      lstrcpy( this->projectPath, projectPath );
    }
    else
      this->projectPath = NULL;

    if ( stateFilename )
    {
      this->stateFilename = (LPTSTR)malloc( ( lstrlen( stateFilename ) + 1 ) * sizeof( TCHAR ) );
      lstrcpy( this->stateFilename, stateFilename );
    }
    else
      this->stateFilename = NULL;

    this->csArray = csArray;
  };
  ~NMPROJECTMNG() // standarddestruktor
  {
    free( projectPath );
    free( stateFilename );
    delete csArray;
  };

  CommandType type;

  LPTSTR projectPath;
  LPTSTR stateFilename;
  CStringArray* csArray;
};


#endif _PROJMNG_EXPORT_H_INCLUDED_