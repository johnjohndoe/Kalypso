#if !defined(AFX_MOEVENTRENDERER_H__CC25FE4A_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOEVENTRENDERER_H__CC25FE4A_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
class CMoSymbol;
class CMoTable;
class CMoRectangle;

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoEventRenderer 

class CMoEventRenderer : public COleDispatchDriver
{
public:
	CMoEventRenderer() {}		// Ruft den Standardkonstruktor für COleDispatchDriver auf
	CMoEventRenderer(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoEventRenderer(const CMoEventRenderer& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	CString GetTag();
	void SetTag(LPCTSTR);
	BOOL GetUseDefault();
	void SetUseDefault(BOOL);
	CMoSymbol GetDefaultSymbol();
	void SetDefaultSymbol(LPDISPATCH);
	long GetSymbolType();
	void SetSymbolType(long);
	short GetValueCount();
	void SetValueCount(short);
	CString GetFeatureRouteIDField();
	void SetFeatureRouteIDField(LPCTSTR);
	CString GetEventRouteIDField();
	void SetEventRouteIDField(LPCTSTR);
	CString GetStartMeasureField();
	void SetStartMeasureField(LPCTSTR);
	CString GetEndMeasureField();
	void SetEndMeasureField(LPCTSTR);
	CString GetSymbolField();
	void SetSymbolField(LPCTSTR);
	BOOL GetDrawBackground();
	void SetDrawBackground(BOOL);
	CMoTable GetEventTable();
	void SetEventTable(LPDISPATCH);
	BOOL GetIndexEvents();
	void SetIndexEvents(BOOL);
	CMoRectangle GetIndexExtent();
	void SetIndexExtent(LPDISPATCH);

// Operationen
public:
	CMoSymbol GetSymbol(short index);
	CString GetValue(short index);
	void SetValue(short index, LPCTSTR lpszNewValue);
	BOOL InvalidateIndex(LPCTSTR key);
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_MOEVENTRENDERER_H__CC25FE4A_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
