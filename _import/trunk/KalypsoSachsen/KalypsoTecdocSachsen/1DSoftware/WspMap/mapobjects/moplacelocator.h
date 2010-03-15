#if !defined(AFX_MOPLACELOCATOR_H__CC25FE54_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOPLACELOCATOR_H__CC25FE54_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
class CMoGeoDataset;
class CMoPoints;
class CMoStrings;

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoPlaceLocator 

class CMoPlaceLocator : public COleDispatchDriver
{
public:
	CMoPlaceLocator() {}		// Ruft den Standardkonstruktor für COleDispatchDriver auf
	CMoPlaceLocator(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoPlaceLocator(const CMoPlaceLocator& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	BOOL GetIndexed();
	void SetIndexed(BOOL);
	CMoGeoDataset GetPlaceNameTable();
	void SetPlaceNameTable(LPDISPATCH);

// Operationen
public:
	BOOL BuildIndex(LPCTSTR Field, BOOL force);
	CMoPoints Locate(LPCTSTR placeName);
	CMoStrings FindApproximateMatches(LPCTSTR placeName);
	CMoStrings FindAllPlaceNames(LPCTSTR prefix);
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_MOPLACELOCATOR_H__CC25FE54_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
