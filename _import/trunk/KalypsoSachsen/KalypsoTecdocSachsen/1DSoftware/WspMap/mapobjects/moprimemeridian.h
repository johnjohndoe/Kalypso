#if !defined(AFX_MOPRIMEMERIDIAN_H__CC25FE6C_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOPRIMEMERIDIAN_H__CC25FE6C_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht �ndern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre �nderungen �berschrieben.

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoPrimeMeridian 

class CMoPrimeMeridian : public COleDispatchDriver
{
public:
	CMoPrimeMeridian() {}		// Ruft den Standardkonstruktor f�r COleDispatchDriver auf
	CMoPrimeMeridian(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoPrimeMeridian(const CMoPrimeMeridian& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	long GetType();
	void SetType(long);
	CString GetName();
	void SetName(LPCTSTR);
	double GetLongitude();
	void SetLongitude(double);

// Operationen
public:
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ f�gt unmittelbar vor der vorhergehenden Zeile zus�tzliche Deklarationen ein.

#endif // AFX_MOPRIMEMERIDIAN_H__CC25FE6C_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
