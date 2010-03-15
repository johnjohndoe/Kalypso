// Project.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef PROJECT_H
#define PROJECT_H

class CrossSection;
class CrossSectionArray;
class State;
class Profil;
class StatesArray;

class Project : public CObject
{
  // Typdefinitionen
public:
  // Typ Vernetzung: jeder CrossSection ist ein Array von 4 anderen CrossSections folgendermassen zugeordnet:
  // die erstenbeiden sind die maximal zwei Vorgänger, der 3 und 4 die beiden Nachfolger. Die Einträge sind
  // jeweils 0, falls es keinen solchen gibt ( z.B. nicht verzweigt: nur jeweils der erste != NULL )
  typedef CTypedPtrMap<CMapPtrToPtr, CrossSection*, CrossSectionArray*> Vernetzung;

  enum ImportType
  {
    ImportDa66, ImportDa50, ImportWsv, ImportTripple, ImportHyk, ImportWst,
      ImportCaddy, ImportWaspila, ImportJabron, ImportReli 
  };
  enum ExportType
  {
    ExportDa66, ExportTripple, ExportHyk, ExportReliL, ExportReliR, ExportCSV
  };

  enum ProjectType
  {
    undefined, LWA, BCE
  };

   Project::Project( const CString& dir, const ProjectType& type = undefined );
	 ~Project();

	 BOOL Load();	// Loads PROJBEZ.TXT and WSP.CFG Files
   BOOL LoadProjectName(); // Load only Probez.txt
	 BOOL Save();	// Saves PROJBEZ.TXT and WSP.CFG Files
   BOOL SaveProjectName() const;  // Save Probez.txt
	 BOOL Create();
	 BOOL Remove();

   BOOL IsLoaded() const { return m_bIsLoaded; };
	
	 CString GetName() const;
	 CString GetDir() const;
	 CString GetCalcDir() const;
	 CString GetDataDir() const;
   CString GetMapDir() const;
   CString GetPlotQPDir() const;
   CString GetPlotLSDir() const;

   const ProjectType& GetProjectType() { return m_projectType; }
   void SetProjectType( const ProjectType& type ) { m_projectType = type; SetModified(); };

	 void AddState(State* st);
	 State* GetFirstState();
	 State* GetNextState();
	 State* GetState( const CString& file );

	 int GetWaterCount();
	 CString GetWaterName(int n);

	 void SetName( const CString& name );

	 void SetModified();

	 void AddCrossSection(CrossSection* cs);
   CrossSection* AddCrossSection( Profil* profil, const CString& waterName, const CString& stateName,
                                             const double station, const CString& pk, const int VZK );

	 BOOL RemoveCrossSection(CrossSection* cs);
	 CrossSection* GetFirstCrossSection();
	 CrossSection* GetNextCrossSection();
	 CrossSection* FindCrossSection(CString& file);
	 int GetCrossSectionCount();

   State* FindStateByName( const CString& stateName );

	 int GetResultFileType(CString& filename);
	 BOOL FileIsOEM(CString& filename);
	 BOOL KeyExists(double station, int vzk, CString& pk);

	 BOOL ImportData( const int type, const CString& inputFileName, State* zustand, DWORD data );
   int ExportData( ExportType type, CrossSectionArray* cs, CString ausgabeDatei );

   int ImportDataAccess( const CString& stationPath, const CString& profilePath );

   BOOL RemoveState(State *zustand);

   Vernetzung* GetVernetzung( CrossSectionArray* csArray = NULL, StatesArray* stateArray = NULL );

   void CreatePrintSummary( CStringArray& strings );

protected:
	friend istream& operator>>(istream& is, Project &proj);
	friend ostream& operator<<(ostream& os, Project &proj);

	void LoadProfProj();
	void SaveProfProj();

public:
	int m_nNextState;

protected:
	int ExportDataDa66( CStdioFile& ausgabeDatei, CrossSectionArray* csArray );
  int ExportDataTripple( CStdioFile& ausgabeDatei, CrossSectionArray* csArray );
  int ExportDataHyk( CStdioFile& ausgabeDatei, CrossSectionArray* csArray );
  int ExportDataReli( CStdioFile& ausagabeDatei, CrossSectionArray* csArray, BOOL bLeft );
  int ExportDataCSV( CStdioFile& ausgabeDatei, CrossSectionArray* csArray );

	BOOL ImportDataWst( CStdioFile *eingabedatei, State *zustand );
	BOOL ImportDataReli( CStdioFile *eingabedatei, State *zustand );
	BOOL ImportDataHyk( CStdioFile *eingabedatei, State *zustand );
	BOOL ImportDataWsv( CStdioFile* eingabedatei, State* zustand );

  BOOL m_bIsLoaded;
	CString m_name;
	CString m_dir;
	CTypedPtrList<CObList, CrossSection*> m_CrossSections;
	CStringArray m_waters;		// list of all Gewaessernamen
	POSITION m_CSectionPos;
	CTypedPtrList<CObList, State*> m_States;
	POSITION m_StatePos;
	BOOL m_bModified;

private:
  static ProjectType AskCalcModel();

  ProjectType m_projectType;
};

#endif // PROJECT_H