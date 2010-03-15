#if !defined(AFX_MOTABLE_H__CC25FE46_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOTABLE_H__CC25FE46_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
class CMoRecordset;

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoTable 

class CMoTable : public COleDispatchDriver
{
public:
	CMoTable() {}		// Ruft den Standardkonstruktor für COleDispatchDriver auf
	CMoTable(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoTable(const CMoTable& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	CString GetUser();
	void SetUser(LPCTSTR);
	CString GetPassword();
	void SetPassword(LPCTSTR);
	CString GetDatabase();
	void SetDatabase(LPCTSTR);
	CString GetName();
	void SetName(LPCTSTR);
	CMoRecordset GetRecords();
	void SetRecords(LPDISPATCH);
	CString GetServer();
	void SetServer(LPCTSTR);

// Operationen
public:
	CMoRecordset SearchExpression(LPCTSTR expression);
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_MOTABLE_H__CC25FE46_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
