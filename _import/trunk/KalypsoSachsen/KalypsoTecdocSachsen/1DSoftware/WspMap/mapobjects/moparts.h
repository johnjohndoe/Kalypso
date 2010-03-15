#if !defined(AFX_MOPARTS_H__CC25FE41_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOPARTS_H__CC25FE41_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoParts 

class CMoParts : public COleDispatchDriver
{
public:
	CMoParts() {}		// Ruft den Standardkonstruktor für COleDispatchDriver auf
	CMoParts(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoParts(const CMoParts& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	short GetCount();
	void SetCount(short);

// Operationen
public:
	void Add(LPDISPATCH Points);
	void Set(short index, LPDISPATCH Points);
	void Remove(short index);
	void Insert(short index, LPDISPATCH Points);
	LPDISPATCH Item(const VARIANT& Item);
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_MOPARTS_H__CC25FE41_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
