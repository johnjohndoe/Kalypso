#if !defined(AFX_MODATUM_H__CC25FE4E_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MODATUM_H__CC25FE4E_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
class CMoSpheroid;

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoDatum 

class CMoDatum : public COleDispatchDriver
{
public:
	CMoDatum() {}		// Ruft den Standardkonstruktor für COleDispatchDriver auf
	CMoDatum(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoDatum(const CMoDatum& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	long GetType();
	void SetType(long);
	CString GetName();
	void SetName(LPCTSTR);
	CMoSpheroid GetSpheroid();
	void SetSpheroid(LPDISPATCH);

// Operationen
public:
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_MODATUM_H__CC25FE4E_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
