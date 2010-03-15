#if !defined(AFX_MOGEOCODER_H__CC25FE69_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOGEOCODER_H__CC25FE69_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
class CMoStandardizer;
class CMoGeoDataset;
class CMoStrings;
class CMoAddressLocation;

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoGeocoder 

class CMoGeocoder : public COleDispatchDriver
{
public:
	CMoGeocoder() {}		// Ruft den Standardkonstruktor für COleDispatchDriver auf
	CMoGeocoder(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoGeocoder(const CMoGeocoder& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	BOOL GetValid();
	void SetValid(BOOL);
	short GetCandidateCount();
	void SetCandidateCount(short);
	double GetSqueezeFactor();
	void SetSqueezeFactor(double);
	double GetOffset();
	void SetOffset(double);
	CString GetMatchRules();
	void SetMatchRules(LPCTSTR);
	short GetMatchVariableCount();
	void SetMatchVariableCount(short);
	CMoStandardizer GetStandardizer();
	void SetStandardizer(LPDISPATCH);
	CMoGeoDataset GetStreetTable();
	void SetStreetTable(LPDISPATCH);
	long GetLastError();
	void SetLastError(long);
	CString GetIntersectionMatchRules();
	void SetIntersectionMatchRules(LPCTSTR);
	short GetIntersectionMatchVariableCount();
	void SetIntersectionMatchVariableCount(short);
	short GetMinimumMatchScore();
	void SetMinimumMatchScore(short);
	float GetSpellingSensitivity();
	void SetSpellingSensitivity(float);
	BOOL GetMatchWhenAmbiguous();
	void SetMatchWhenAmbiguous(BOOL);
	CMoStrings GetSearchQueries();
	void SetSearchQueries(LPDISPATCH);

// Operationen
public:
	CString GetCandidate(short index);
	CString GetMatchVariable(short index);
	CString GetMatchVariableField(LPCTSTR variable);
	void SetMatchVariableField(LPCTSTR variable, LPCTSTR lpszNewValue);
	CMoAddressLocation LocateCandidate(short index);
	CString GetIntersectionMatchVariable(short index);
	CString GetMatchVariableIntersectionLink(LPCTSTR variable, long linkGroup);
	void SetMatchVariableIntersectionLink(LPCTSTR variable, long linkGroup, LPCTSTR lpszNewValue);
	CString GetBatchMatchVariableField(LPCTSTR variable);
	void SetBatchMatchVariableField(LPCTSTR variable, LPCTSTR lpszNewValue);
	CMoStrings ListIndices();
	long IndexStatus();
	BOOL EraseIndices();
	BOOL AddIndex(LPCTSTR FieldName, LPCTSTR secondaryFieldName, long indexType);
	BOOL BuildIndices(BOOL force);
	long GenerateCandidates();
	long BatchMatch(LPDISPATCH addressTable, LPCTSTR addressField, LPDISPATCH DataConnection, LPCTSTR outputTableName, LPDISPATCH outputFields);
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_MOGEOCODER_H__CC25FE69_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
