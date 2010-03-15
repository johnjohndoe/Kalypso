#if !defined(AFX_MOPICTURE_H__CC25FE74_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOPICTURE_H__CC25FE74_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht �ndern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre �nderungen �berschrieben.

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoPicture 

class CMoPicture : public COleDispatchDriver
{
public:
	CMoPicture() {}		// Ruft den Standardkonstruktor f�r COleDispatchDriver auf
	CMoPicture(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoPicture(const CMoPicture& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	long GetHandle();
	long GetHPal();
	void SetHPal(long);
	short GetType();
	long GetWidth();
	long GetHeight();

// Operationen
public:
	// Methode 'Render' wird wegen eines ung�ltigen R�ckgabetyps oder Parametertyps nicht verwendet
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ f�gt unmittelbar vor der vorhergehenden Zeile zus�tzliche Deklarationen ein.

#endif // AFX_MOPICTURE_H__CC25FE74_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
