#if !defined(AFX_MOGEOCOORDSYS_H__CC25FE44_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOGEOCOORDSYS_H__CC25FE44_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
class CMoDatum;
class CMoUnit;
class CMoPrimeMeridian;

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoGeoCoordSys 

class CMoGeoCoordSys : public COleDispatchDriver
{
public:
	CMoGeoCoordSys() {}		// Ruft den Standardkonstruktor für COleDispatchDriver auf
	CMoGeoCoordSys(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoGeoCoordSys(const CMoGeoCoordSys& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	long GetType();
	void SetType(long);
	CString GetName();
	void SetName(LPCTSTR);
	CMoDatum GetDatum();
	void SetDatum(LPDISPATCH);
	CMoUnit GetUnit();
	void SetUnit(LPDISPATCH);
	CMoPrimeMeridian GetPrimeMeridian();
	void SetPrimeMeridian(LPDISPATCH);
	BOOL GetIsProjected();
	void SetIsProjected(BOOL);

// Operationen
public:
	LPDISPATCH Transform(LPDISPATCH FromCoordSys, LPDISPATCH FromShape, const VARIANT& DensificationTolerance, const VARIANT& GeoTransformation);
	void Export(LPCTSTR OutName);
	CString ReturnDescription();
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_MOGEOCOORDSYS_H__CC25FE44_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
