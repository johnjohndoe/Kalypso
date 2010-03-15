#if !defined(AFX_MOLABELPLACER_H__CC25FE56_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOLABELPLACER_H__CC25FE56_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
class CMoTextSymbol;

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoLabelPlacer 

class CMoLabelPlacer : public COleDispatchDriver
{
public:
	CMoLabelPlacer() {}		// Ruft den Standardkonstruktor für COleDispatchDriver auf
	CMoLabelPlacer(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoLabelPlacer(const CMoLabelPlacer& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	CString GetField();
	void SetField(LPCTSTR);
	BOOL GetDrawBackground();
	void SetDrawBackground(BOOL);
	BOOL GetAllowDuplicates();
	void SetAllowDuplicates(BOOL);
	BOOL GetPlaceAbove();
	void SetPlaceAbove(BOOL);
	BOOL GetPlaceBelow();
	void SetPlaceBelow(BOOL);
	BOOL GetPlaceOn();
	void SetPlaceOn(BOOL);
	CMoTextSymbol GetDefaultSymbol();
	void SetDefaultSymbol(LPDISPATCH);
	BOOL GetUseDefault();
	void SetUseDefault(BOOL);
	short GetValueCount();
	void SetValueCount(short);
	CString GetValueField();
	void SetValueField(LPCTSTR);
	LPDISPATCH GetBackgroundRenderer();
	void SetBackgroundRenderer(LPDISPATCH);
	BOOL GetMaskLabels();
	void SetMaskLabels(BOOL);
	unsigned long GetMaskColor();
	void SetMaskColor(unsigned long);
	short GetSymbolWidth();
	void SetSymbolWidth(short);
	short GetSymbolHeight();
	void SetSymbolHeight(short);

// Operationen
public:
	CMoTextSymbol GetSymbol(short index);
	CString GetValue(short index);
	void SetValue(short index, LPCTSTR lpszNewValue);
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_MOLABELPLACER_H__CC25FE56_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
