#if !defined(AFX_MOTEXTSYMBOL_H__CC25FE64_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOTEXTSYMBOL_H__CC25FE64_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
class COleFont;

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoTextSymbol 

class CMoTextSymbol : public COleDispatchDriver
{
public:
	CMoTextSymbol() {}		// Ruft den Standardkonstruktor für COleDispatchDriver auf
	CMoTextSymbol(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoTextSymbol(const CMoTextSymbol& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	long GetHorizontalAlignment();
	void SetHorizontalAlignment(long);
	long GetVerticalAlignment();
	void SetVerticalAlignment(long);
	unsigned long GetColor();
	void SetColor(unsigned long);
	COleFont GetFont();
	void SetFont(LPDISPATCH);
	double GetRotation();
	void SetRotation(double);
	double GetHeight();
	void SetHeight(double);
	BOOL GetFitted();
	void SetFitted(BOOL);

// Operationen
public:
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_MOTEXTSYMBOL_H__CC25FE64_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
