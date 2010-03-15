#if !defined(AFX_MOGEODATASETS_H__CC25FE48_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOGEODATASETS_H__CC25FE48_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoGeoDatasets 

class CMoGeoDatasets : public COleDispatchDriver
{
public:
	CMoGeoDatasets() {}		// Ruft den Standardkonstruktor für COleDispatchDriver auf
	CMoGeoDatasets(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoGeoDatasets(const CMoGeoDatasets& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	short GetCount();
	void SetCount(short);

// Operationen
public:
	LPDISPATCH Item(const VARIANT& Item);
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_MOGEODATASETS_H__CC25FE48_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
