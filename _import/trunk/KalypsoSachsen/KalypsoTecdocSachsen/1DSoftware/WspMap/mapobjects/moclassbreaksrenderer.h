#if !defined(AFX_MOCLASSBREAKSRENDERER_H__CC25FE6D_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOCLASSBREAKSRENDERER_H__CC25FE6D_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
class CMoSymbol;

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoClassBreaksRenderer 

class CMoClassBreaksRenderer : public COleDispatchDriver
{
public:
	CMoClassBreaksRenderer() {}		// Ruft den Standardkonstruktor für COleDispatchDriver auf
	CMoClassBreaksRenderer(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoClassBreaksRenderer(const CMoClassBreaksRenderer& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	CString GetField();
	void SetField(LPCTSTR);
	short GetBreakCount();
	void SetBreakCount(short);
	CString GetTag();
	void SetTag(LPCTSTR);
	long GetSymbolType();
	void SetSymbolType(long);

// Operationen
public:
	double GetBreak(short index);
	void SetBreak(short index, double newValue);
	CMoSymbol GetSymbol(short index);
	void SetRefSymbol(short index, LPDISPATCH newValue);
	void RampColors(unsigned long startColor, unsigned long endColor);
	void SizeSymbols(short startSize, short endSize);
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_MOCLASSBREAKSRENDERER_H__CC25FE6D_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
