#if !defined(AFX_MOTRACKINGLAYER_H__CC25FE43_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOTRACKINGLAYER_H__CC25FE43_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
class CMoSymbol;
class CMoGeoEvent;

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoTrackingLayer 

class CMoTrackingLayer : public COleDispatchDriver
{
public:
	CMoTrackingLayer() {}		// Ruft den Standardkonstruktor für COleDispatchDriver auf
	CMoTrackingLayer(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoTrackingLayer(const CMoTrackingLayer& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	long GetEventCount();
	void SetEventCount(long);
	long GetSymbolCount();
	void SetSymbolCount(long);
	BOOL GetVisible();
	void SetVisible(BOOL);

// Operationen
public:
	CMoSymbol GetSymbol(long index);
	CMoGeoEvent GetEvent(long index);
	void RemoveEvent(long index);
	CMoGeoEvent AddEvent(LPDISPATCH shape, long SymbolIndex);
	void ClearEvents();
	void Refresh(BOOL erase, const VARIANT& rect);
	CMoGeoEvent FindEvent(LPCTSTR Tag);
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_MOTRACKINGLAYER_H__CC25FE43_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
