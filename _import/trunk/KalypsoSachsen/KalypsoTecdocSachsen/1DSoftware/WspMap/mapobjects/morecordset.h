#if !defined(AFX_MORECORDSET_H__CC25FE4F_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MORECORDSET_H__CC25FE4F_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht �ndern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre �nderungen �berschrieben.


// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
class CMoFields;
class CMoTableDesc;
class CMoStatistics;
class CMoGeoDataset;

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoRecordset 

class CMoRecordset : public COleDispatchDriver
{
public:
	CMoRecordset() {}		// Ruft den Standardkonstruktor f�r COleDispatchDriver auf
	CMoRecordset(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoRecordset(const CMoRecordset& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	CMoFields GetFields();
	void SetFields(LPDISPATCH);
	BOOL GetEof();
	void SetEof(BOOL);
	long GetCount();
	void SetCount(long);
	long GetEditMode();
	void SetEditMode(long);
	BOOL GetUpdatable();
	void SetUpdatable(BOOL);
	CMoTableDesc GetTableDesc();
	void SetTableDesc(LPDISPATCH);
	BOOL GetAutoFlush();
	void SetAutoFlush(BOOL);
	BOOL GetSupportsTransactions();
	void SetSupportsTransactions(BOOL);

// Operationen
public:
	void MoveFirst();
	void MoveNext();
	void MovePrevious();
	CMoStatistics CalculateStatistics(LPCTSTR FieldName);
	void Delete();
	void Edit();
	void AddNew();
	void Update();
	void CancelUpdate();
	void StopEditing();
	CMoGeoDataset Export(LPCTSTR OutName, const VARIANT& OutCoordSys);
	void StartTransaction();
	void RollbackTransaction();
	void CommitTransaction();
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ f�gt unmittelbar vor der vorhergehenden Zeile zus�tzliche Deklarationen ein.

#endif // AFX_MORECORDSET_H__CC25FE4F_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
