#if !defined(AFX_MOGROUPRENDERER_H__CC25FE52_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOGROUPRENDERER_H__CC25FE52_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoGroupRenderer 

class CMoGroupRenderer : public COleDispatchDriver
{
public:
	CMoGroupRenderer() {}		// Ruft den Standardkonstruktor für COleDispatchDriver auf
	CMoGroupRenderer(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoGroupRenderer(const CMoGroupRenderer& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	short GetCount();
	void SetCount(short);
	BOOL GetDrawBackground();
	void SetDrawBackground(BOOL);

// Operationen
public:
	LPDISPATCH GetRenderer(short index);
	void SetRefRenderer(short index, LPDISPATCH newValue);
	short Add(LPDISPATCH Renderer);
	void Remove(short index);
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_MOGROUPRENDERER_H__CC25FE52_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
