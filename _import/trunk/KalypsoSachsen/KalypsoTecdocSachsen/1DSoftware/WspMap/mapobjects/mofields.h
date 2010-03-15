#if !defined(AFX_MOFIELDS_H__CC25FE6E_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOFIELDS_H__CC25FE6E_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
class CMoField;

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoFields 

class CMoFields : public COleDispatchDriver
{
public:
	CMoFields() {}		// Ruft den Standardkonstruktor für COleDispatchDriver auf
	CMoFields(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoFields(const CMoFields& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	short GetCount();
	void SetCount(short);

// Operationen
public:
	CMoField Item(const VARIANT& Item);
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_MOFIELDS_H__CC25FE6E_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
