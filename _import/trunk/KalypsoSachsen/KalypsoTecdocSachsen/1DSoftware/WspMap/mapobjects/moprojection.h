#if !defined(AFX_MOPROJECTION_H__CC25FE6A_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOPROJECTION_H__CC25FE6A_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht �ndern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre �nderungen �berschrieben.

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoProjection 

class CMoProjection : public COleDispatchDriver
{
public:
	CMoProjection() {}		// Ruft den Standardkonstruktor f�r COleDispatchDriver auf
	CMoProjection(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoProjection(const CMoProjection& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	long GetType();
	void SetType(long);
	CString GetName();
	void SetName(LPCTSTR);
	LPUNKNOWN GetCustom();
	void SetCustom(LPUNKNOWN);
	BOOL GetIsCustom();
	void SetIsCustom(BOOL);

// Operationen
public:
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ f�gt unmittelbar vor der vorhergehenden Zeile zus�tzliche Deklarationen ein.

#endif // AFX_MOPROJECTION_H__CC25FE6A_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
