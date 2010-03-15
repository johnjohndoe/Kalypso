#if !defined(AFX_MOADDRESSLOCATION_H__CC25FE63_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOADDRESSLOCATION_H__CC25FE63_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht �ndern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre �nderungen �berschrieben.


// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
class CMoPoint;

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoAddressLocation 

class CMoAddressLocation : public COleDispatchDriver
{
public:
	CMoAddressLocation() {}		// Ruft den Standardkonstruktor f�r COleDispatchDriver auf
	CMoAddressLocation(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoAddressLocation(const CMoAddressLocation& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	CMoPoint GetLocation();
	void SetLocation(LPDISPATCH);
	short GetMatchScore();
	void SetMatchScore(short);
	long GetStreetSide();
	void SetStreetSide(long);

// Operationen
public:
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ f�gt unmittelbar vor der vorhergehenden Zeile zus�tzliche Deklarationen ein.

#endif // AFX_MOADDRESSLOCATION_H__CC25FE63_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
