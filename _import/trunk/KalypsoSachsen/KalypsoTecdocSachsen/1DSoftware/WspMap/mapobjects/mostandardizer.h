#if !defined(AFX_MOSTANDARDIZER_H__CC25FE4C_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOSTANDARDIZER_H__CC25FE4C_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoStandardizer 

class CMoStandardizer : public COleDispatchDriver
{
public:
	CMoStandardizer() {}		// Ruft den Standardkonstruktor für COleDispatchDriver auf
	CMoStandardizer(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoStandardizer(const CMoStandardizer& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	BOOL GetValid();
	void SetValid(BOOL);
	short GetFieldCount();
	void SetFieldCount(short);
	CString GetStandardizingRules();
	void SetStandardizingRules(LPCTSTR);
	long GetLastError();
	void SetLastError(long);
	CString GetIntersectionStandardizingRules();
	void SetIntersectionStandardizingRules(LPCTSTR);

// Operationen
public:
	BOOL StandardizeAddress(LPCTSTR address);
	CString GetFieldName(short index);
	CString GetFieldValue(LPCTSTR FieldName);
	void SetFieldValue(LPCTSTR FieldName, LPCTSTR lpszNewValue);
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_MOSTANDARDIZER_H__CC25FE4C_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
