// State.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef STATE_H
#define STATE_H

class Connection;
class OutFlow;
class Loss;
class BranchTable;
class LengthSectionArray;
class Project;
class Calculation;
class CrossSection;
class CrossSectionArray;
//Die Klasse LenghtSection
class LengthSection;
class istream;
class ostream;

class State : public CObject
{
  ////////////////
  // Konstanten //
  ////////////////
public:
  enum Consts
  {
    // Konstanten für DA50 Import
    IMPORT_DA50_REFFIRST = 0,
    IMPORT_DA50_REFZERO = 1

  };


public:
   State::State(Project* pProject);
	~State();

   State* Clone( Project* pProject, const CString& name, BOOL createNewFiles = TRUE );

	 BOOL Load();						// Loads STR File
	 BOOL Save();						// Saves STR File
	
	 Project* GetProject();
	 CString GetWaterName();
	 const CString& GetName() const;
	 CString GetDateString();
	 double GetStartStation();
	 double GetEndStation();
	 CString GetFileName();
	 CString GetFileTitle();
	 CString GetFileExt();

	void SetModified();
	void SetBERModified();
	void SetOutFlowModified();
	void SetLossModified();
	void SetBranchTableModified();
	
	void SetNumber(int num);
	 void SetWaterName( const CString& name );
	 void SetName( const CString& name );
	 void SetDate( const CString& date );
   void SetDate( const COleDateTime& date );
	 void SetStartStation( const double value );
	 void SetEndStation( const double value );
	 void SetFileName( const CString& file );

	 void CreateFileName();

	 void AddCrossSection(CrossSection* cs);
	 CrossSection* GetFirstCrossSection();
	 CrossSection* GetNextCrossSection();
	 BOOL CrossSectionExists(CrossSection* cs);
	 BOOL RemoveCrossSection(CrossSection* cs);
	 CrossSection* FindCrossSection(CString& file);
	 CrossSection* FindCrossSection(double station);
	 int GetNumCrossSections();

   BOOL ImportDataDa50( CStdioFile* eingabedatei, DWORD data );
   BOOL ImportDataDa66( CStdioFile* eingabedatei );
   BOOL ImportDataTripple( CStdioFile* eingabedatei );
  
	 Connection* GetFirstConnection();
	 Connection* GetNextConnection();
	 Connection* GetLastConnection();
	 Connection* GetPrevConnection();
	 int GetNumConnections();

	 void AddCalculation(Calculation* ca);
	 void RemoveCalculation(Calculation* ca);
	 Calculation* GetFirstCalculation();
	 Calculation* GetNextCalculation();
	 Calculation* GetLastCalculation();
	 Calculation* GetPrevCalculation();
	 Calculation* FindCalculation(CString& file);
	 BOOL CalculationExists(Calculation* ca);
	 int GetNumCalculations();

	 void AddOutFlow(OutFlow* of);
	 void RemoveOutFlow(OutFlow* of);
	 OutFlow* GetFirstOutFlow();
	 OutFlow* GetNextOutFlow();
	 OutFlow* FindOutFlow(CString& name);
	 int GetNumOutFlows();

	 void AddLoss(Loss* loss);
	 void RemoveLoss(Loss* loss);
	 Loss* GetFirstLoss();
	 Loss* GetNextLoss();
	 Loss* FindLoss(double station);
	 int GetNumLosses();

	 BranchTable* GetBranchTable();
	 void SetBranchTable( BranchTable* bt );

   int InsertWsp( LengthSectionArray* lsArray, BOOL bDurchst, const CString& strAbflussFormat );
   int RemoveWsp( LengthSectionArray* lsArray );

   BOOL IsPredOf( const CrossSection& cs1, const CrossSection& cs2 );

   CrossSection* GetFollowingCs( CrossSection* cs, const CrossSectionArray& csArray, 
                                            const BOOL bVorwaerts, const BOOL bLeft );


   void CreatePrintSummary( CStringArray& strings );

protected:
  BOOL InsertConnection( CrossSection* newCs, CrossSection* predCs, CrossSection* postCs );
  void RemoveConnection( Connection* conn );

  void RemoveCSFromConnections( CrossSection* cs );

  void CalcAnfEndStation();

	BOOL LoadCalculations();// Loads BER File
	BOOL LoadOutFlows();	// Loads QWT File
	BOOL LoadLosses();		// Loads PSI File
	BOOL SaveCalculations();// Saves BER File
	BOOL SaveOutFlows();	// Saves QWT File
	BOOL SaveLosses();		// Saves PSI File
	BOOL LoadBranchTable();	// Loads VZK File
	BOOL SaveBranchTable();	// Saves VZK File
	friend istream& operator>>(istream& is, State &zs);
	friend ostream& operator<<(ostream& os, State &zs);

protected:

	Project* m_pProject;
	int m_nNumber;
	CString m_WaterName;
	CString m_name;
	COleDateTime m_date;
	double m_dStartStation;
	double m_dEndStation;
	CString m_fileTitle, m_fileExt;
	CTypedPtrList<CObList, CrossSection*> m_CrossSections;
	POSITION m_CSectionPos;
	CTypedPtrList<CObList, Connection*> m_Connections;
	POSITION m_ConnectionPos;
	CTypedPtrList<CObList, Calculation*> m_Calculations;
	POSITION m_CalculationPos;
	CTypedPtrList<CObList, OutFlow*> m_OutFlows;
	POSITION m_OutFlowPos;
	CTypedPtrList<CObList, Loss*> m_Losses;
	POSITION m_LossPos;
	BranchTable *m_pBranchTable;
	BOOL m_bModified;
	BOOL m_bBERModified;
	BOOL m_bOutFlowModified;
	BOOL m_bLossModified;
	BOOL m_bBranchTableModified;
};

class StatesArray : public CTypedPtrArray<CPtrArray, State*> {};

#endif // STATE_H