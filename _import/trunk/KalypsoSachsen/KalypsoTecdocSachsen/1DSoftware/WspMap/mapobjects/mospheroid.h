#if !defined(AFX_MOSPHEROID_H__CC25FE5D_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOSPHEROID_H__CC25FE5D_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht �ndern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre �nderungen �berschrieben.

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoSpheroid 

class CMoSpheroid : public COleDispatchDriver
{
public:
	CMoSpheroid() {}		// Ruft den Standardkonstruktor f�r COleDispatchDriver auf
	CMoSpheroid(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoSpheroid(const CMoSpheroid& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	long GetType();
	void SetType(long);
	double GetAxis();
	void SetAxis(double);
	double GetFlattening();
	void SetFlattening(double);
	CString GetName();
	void SetName(LPCTSTR);

// Operationen
public:
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ f�gt unmittelbar vor der vorhergehenden Zeile zus�tzliche Deklarationen ein.

#endif // AFX_MOSPHEROID_H__CC25FE5D_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
