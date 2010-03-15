#if !defined(AFX_MOZRENDERER_H__CC25FE59_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOZRENDERER_H__CC25FE59_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoZRenderer 

class CMoZRenderer : public COleDispatchDriver
{
public:
	CMoZRenderer() {}		// Ruft den Standardkonstruktor für COleDispatchDriver auf
	CMoZRenderer(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoZRenderer(const CMoZRenderer& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	CString GetTag();
	void SetTag(LPCTSTR);
	long GetSymbolType();
	void SetSymbolType(long);
	short GetBreakCount();
	void SetBreakCount(short);
	long GetValueCalculation();
	void SetValueCalculation(long);

// Operationen
public:
	double GetBreak(short index);
	void SetBreak(short index, double newValue);
	LPDISPATCH GetSymbol(short index);
	void RampColors(unsigned long startColor, unsigned long endColor);
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_MOZRENDERER_H__CC25FE59_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
