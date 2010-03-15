#if !defined(AFX_MOCHARTRENDERER_H__CC25FE6B_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOCHARTRENDERER_H__CC25FE6B_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoChartRenderer 

class CMoChartRenderer : public COleDispatchDriver
{
public:
	CMoChartRenderer() {}		// Ruft den Standardkonstruktor für COleDispatchDriver auf
	CMoChartRenderer(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoChartRenderer(const CMoChartRenderer& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	CString GetSizeField();
	void SetSizeField(LPCTSTR);
	short GetFieldCount();
	void SetFieldCount(short);
	double GetNullValue();
	void SetNullValue(double);
	long GetChartType();
	void SetChartType(long);
	short GetBarWidth();
	void SetBarWidth(short);
	short GetBarHeight();
	void SetBarHeight(short);
	CString GetNormalizationField();
	void SetNormalizationField(LPCTSTR);
	short GetMinPieSize();
	void SetMinPieSize(short);
	short GetMaxPieSize();
	void SetMaxPieSize(short);
	BOOL GetShowOutline();
	void SetShowOutline(BOOL);

// Operationen
public:
	CString GetField(short index);
	void SetField(short index, LPCTSTR lpszNewValue);
	unsigned long GetColor(short index);
	void SetColor(short index, unsigned long newValue);
	void NoNullValue();
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_MOCHARTRENDERER_H__CC25FE6B_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
