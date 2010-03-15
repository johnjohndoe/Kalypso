#if !defined(AFX_MOSYMBOL_H__CC25FE66_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOSYMBOL_H__CC25FE66_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
class COleFont;

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoSymbol 

class CMoSymbol : public COleDispatchDriver
{
public:
	CMoSymbol() {}		// Ruft den Standardkonstruktor für COleDispatchDriver auf
	CMoSymbol(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoSymbol(const CMoSymbol& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	short GetSize();
	void SetSize(short);
	short GetStyle();
	void SetStyle(short);
	unsigned long GetColor();
	void SetColor(unsigned long);
	short GetCharacterIndex();
	void SetCharacterIndex(short);
	COleFont GetFont();
	void SetFont(LPDISPATCH);
	unsigned long GetOutlineColor();
	void SetOutlineColor(unsigned long);
	long GetSymbolType();
	void SetSymbolType(long);
	BOOL GetOutline();
	void SetOutline(BOOL);
	BOOL GetCenterOnAscent();
	void SetCenterOnAscent(BOOL);
	LPUNKNOWN GetCustom();
	void SetCustom(LPUNKNOWN);
	double GetRotation();
	void SetRotation(double);

// Operationen
public:
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_MOSYMBOL_H__CC25FE66_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
