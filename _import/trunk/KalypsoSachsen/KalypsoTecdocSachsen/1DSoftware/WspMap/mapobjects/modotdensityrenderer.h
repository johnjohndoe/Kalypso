#if !defined(AFX_MODOTDENSITYRENDERER_H__CC25FE57_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MODOTDENSITYRENDERER_H__CC25FE57_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoDotDensityRenderer 

class CMoDotDensityRenderer : public COleDispatchDriver
{
public:
	CMoDotDensityRenderer() {}		// Ruft den Standardkonstruktor für COleDispatchDriver auf
	CMoDotDensityRenderer(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoDotDensityRenderer(const CMoDotDensityRenderer& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	CString GetField();
	void SetField(LPCTSTR);
	double GetDotValue();
	void SetDotValue(double);
	short GetDotSize();
	void SetDotSize(short);
	unsigned long GetDotColor();
	void SetDotColor(unsigned long);
	BOOL GetDrawBackground();
	void SetDrawBackground(BOOL);
	CString GetTag();
	void SetTag(LPCTSTR);

// Operationen
public:
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_MODOTDENSITYRENDERER_H__CC25FE57_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
