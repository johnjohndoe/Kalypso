#if !defined(AFX_MOSTRINGS_H__CC25FE68_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOSTRINGS_H__CC25FE68_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht �ndern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre �nderungen �berschrieben.

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoStrings 

class CMoStrings : public COleDispatchDriver
{
public:
	CMoStrings() {}		// Ruft den Standardkonstruktor f�r COleDispatchDriver auf
	CMoStrings(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoStrings(const CMoStrings& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	short GetCount();
	void SetCount(short);
	BOOL GetUnique();
	void SetUnique(BOOL);

// Operationen
public:
	CString Item(const VARIANT& Item);
	BOOL Add(LPCTSTR string);
	void Clear();
	long Find(LPCTSTR itemName, const VARIANT& startPos);
	void PopulateWithUnits();
	void PopulateWithSpheroids();
	void PopulateWithDatums();
	void PopulateWithMeridians();
	void PopulateWithProjections();
	void PopulateWithProjectedCoordSys();
	void PopulateWithGeographicCoordSys();
	void PopulateWithGeoTransformations();
	void PopulateWithParameters(long Projection);
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ f�gt unmittelbar vor der vorhergehenden Zeile zus�tzliche Deklarationen ein.

#endif // AFX_MOSTRINGS_H__CC25FE68_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
