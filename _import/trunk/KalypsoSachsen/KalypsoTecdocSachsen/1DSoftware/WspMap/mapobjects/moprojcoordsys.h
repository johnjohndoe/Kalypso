#if !defined(AFX_MOPROJCOORDSYS_H__CC25FE61_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOPROJCOORDSYS_H__CC25FE61_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
class CMoGeoCoordSys;
class CMoUnit;
class CMoProjection;

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoProjCoordSys 

class CMoProjCoordSys : public COleDispatchDriver
{
public:
	CMoProjCoordSys() {}		// Ruft den Standardkonstruktor für COleDispatchDriver auf
	CMoProjCoordSys(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoProjCoordSys(const CMoProjCoordSys& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	long GetType();
	void SetType(long);
	CString GetName();
	void SetName(LPCTSTR);
	CMoGeoCoordSys GetGeoCoordSys();
	void SetGeoCoordSys(LPDISPATCH);
	CMoUnit GetUnit();
	void SetUnit(LPDISPATCH);
	CMoProjection GetProjection();
	void SetProjection(LPDISPATCH);
	BOOL GetIsProjected();
	void SetIsProjected(BOOL);

// Operationen
public:
	void SetParameter(long ParameterType, double ParameterValue);
	double GetParameter(long ParameterType);
	LPDISPATCH Transform(LPDISPATCH FromCoordSys, LPDISPATCH FromShape, const VARIANT& DensificationTolerance, const VARIANT& GeoTransformation);
	void Export(LPCTSTR OutName);
	CString ReturnDescription();
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_MOPROJCOORDSYS_H__CC25FE61_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
