// Calc.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef CALC_H
#define CALC_H

#define RESULT_TYPE_NONE	-1
#define RESULT_TYPE_ER		0
#define RESULT_TYPE_TB		1
#define RESULT_TYPE_WK		2
#define RESULT_TYPE_UE		3
#define RESULT_TYPE_MA		4
#define RESULT_TYPE_EX		5
#define RESULT_TYPE_PR		6
#define RESULT_TYPE_VG		7

#define N_RESULT_TYPES		8

static const char szResultTypes[N_RESULT_TYPES][3] =
{"er", "tb", "wk", "ue", "ma", "ex", "pr", "vg"};

class Project;
class State;
class CalcData;
class LengthSection;

class Calculation : public CObject
{
public:
   Calculation::Calculation(Project *pProject, State* pState);
	 ~Calculation();

   Calculation* Clone( Project* pProject, State* pState, BOOL copyFile );

	 CString GetFileTitle();
	 CString GetFileExt();
	 CString GetFileName();
	 CalcData* GetCalcData();
	 CString GetName();
	 double GetStartStation();
	 double GetEndStation();

	 void CreateFileName();

	 void SetFileName(CString& file);
	 void SetName(CString& name);
	 void SetStartStation(double value);
	 void SetEndStation(double value);

	 BOOL LoadCalcData(BOOL bLWA);
	 BOOL SaveCalcData();
	 void SetCalcData(CalcData* cd);
	 void FlushCalcData();

	 void SetLengthSection(LengthSection* ls);
	 LengthSection* GetLengthSection();

	 BOOL GetResultFileName(CString& file, int nType);

protected:
	State *m_pState;
	Project *m_pProject;
	CalcData *m_pCalcData;
	LengthSection *m_pLengthSection;
	CString m_fileTitle, m_fileExt;
	CString m_name;
	double m_dStartStation;
	double m_dEndStation;
};

typedef CTypedPtrArray<CPtrArray, Calculation*> CalculationArray;

#endif // CALC_H