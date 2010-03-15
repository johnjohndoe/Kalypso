#if !defined(AFX_MOGEOTRANSFORMATION_H__CC25FE4D_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOGEOTRANSFORMATION_H__CC25FE4D_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht �ndern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre �nderungen �berschrieben.


// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
class CMoGeoCoordSys;

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoGeoTransformation 

class CMoGeoTransformation : public COleDispatchDriver
{
public:
	CMoGeoTransformation() {}		// Ruft den Standardkonstruktor f�r COleDispatchDriver auf
	CMoGeoTransformation(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoGeoTransformation(const CMoGeoTransformation& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	long GetType();
	void SetType(long);
	CString GetName();
	void SetName(LPCTSTR);
	CMoGeoCoordSys GetFromGeoCoordSys();
	void SetFromGeoCoordSys(LPDISPATCH);
	CMoGeoCoordSys GetToGeoCoordSys();
	void SetToGeoCoordSys(LPDISPATCH);
	long GetMethod();
	void SetMethod(long);
	long GetDirection();
	void SetDirection(long);
	long GetSecondType();
	void SetSecondType(long);
	CString GetSecondName();
	void SetSecondName(LPCTSTR);
	long GetSecondDirection();
	void SetSecondDirection(long);

// Operationen
public:
	void SetParameter(long ParameterType, double ParameterValue);
	double GetParameter(long ParameterType);
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ f�gt unmittelbar vor der vorhergehenden Zeile zus�tzliche Deklarationen ein.

#endif // AFX_MOGEOTRANSFORMATION_H__CC25FE4D_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
