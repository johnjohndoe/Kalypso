#if !defined(AFX_MOFIELD_H__CC25FE58_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOFIELD_H__CC25FE58_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht �ndern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre �nderungen �berschrieben.

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoField 

class CMoField : public COleDispatchDriver
{
public:
	CMoField() {}		// Ruft den Standardkonstruktor f�r COleDispatchDriver auf
	CMoField(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoField(const CMoField& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	CString GetName();
	void SetName(LPCTSTR);
	long GetType();
	void SetType(long);
	CString GetValueAsString();
	void SetValueAsString(LPCTSTR);
	VARIANT GetValue();
	void SetValue(const VARIANT&);
	VARIANT Get_Value();
	void Set_Value(const VARIANT&);

// Operationen
public:
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ f�gt unmittelbar vor der vorhergehenden Zeile zus�tzliche Deklarationen ein.

#endif // AFX_MOFIELD_H__CC25FE58_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
