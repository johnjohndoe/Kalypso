// OutFlow.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef OUTFLOW_H
#define OUTFLOW_H

class Project;
class State;
class C3DCoord;

class OutFlow : public CObject
{
public:
	OutFlow( Project *pProject, State* pState );
	~OutFlow();

   OutFlow* Clone( Project* pProject, State* pState );

	 CString GetName();
	 int GetNumCoords();

	 void SetName(CString& name);

	 void AddCoord(C3DCoord* crd);
	 void RemoveCoord(C3DCoord* crd);
	 void InsertCoordAt(int n, C3DCoord* crd);
	 C3DCoord* GetCoordAt(int n);
	 C3DCoord* GetFirstCoord();
	 C3DCoord* GetNextCoord();
	 C3DCoord* GetLastCoord();
	 C3DCoord* GetPrevCoord();
	 BOOL WSPFIsDefined();

protected:
	State *m_pState;
	Project *m_pProject;
	CString m_name;
	CTypedPtrArray<CObArray, C3DCoord*> m_C3DCoords;
	int m_nC3DCoordIndex;
};

#endif // OUTFLOW_H