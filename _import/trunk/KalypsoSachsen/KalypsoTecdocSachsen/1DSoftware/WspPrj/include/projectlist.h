#ifndef _PROJECTLIST_H_INCLUDED_
#define _PROJECTLIST_H_INCLUDED_

////////////////////////////////////////
// Klasse ProjectList                 //
////////////////////////////////////////
// Wrapperklasse für die wsp.prj      //
////////////////////////////////////////
// Erlaubt das Verwalten von Projekten//
////////////////////////////////////////

class Project;

class ProjectList
{
  // Konstructoren / Destructoren
public:
   ProjectList();
   ~ProjectList();
  
  // Operationen
public:
   Project* AddProject( const CString& projectPath, const BOOL exists );
   BOOL RemoveProject( Project* project, BOOL deleteFiles );
   BOOL Save() const;

protected:
  void DeleteContents();
  BOOL Load(); // wird nur aus dem Konstruktor aufgerufen -> deswegen protected
  
  // Attribute
public:
   int GetCount() const;
   Project* GetProject( const int index ) const;
   int GetProjectID( const Project* project ) const;
   BOOL LoadIsValid() const { return validLoad; };

protected:
  Project* GetProject( const CString& projectPath ) const;

protected:
  CString GetWspPath() const;
  
protected:
  CTypedPtrArray<CPtrArray, Project*> projectList;
  BOOL validLoad;
  
}; // class ProjectList


#endif _PROJECTLIST_H_INCLUDED_