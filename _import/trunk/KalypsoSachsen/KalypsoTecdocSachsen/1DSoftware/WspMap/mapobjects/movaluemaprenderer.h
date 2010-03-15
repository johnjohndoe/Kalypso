#if !defined(AFX_MOVALUEMAPRENDERER_H__CC25FE4B_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOVALUEMAPRENDERER_H__CC25FE4B_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
class CMoSymbol;

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoValueMapRenderer 

class CMoValueMapRenderer : public COleDispatchDriver
{
public:
	CMoValueMapRenderer() {}		// Ruft den Standardkonstruktor für COleDispatchDriver auf
	CMoValueMapRenderer(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoValueMapRenderer(const CMoValueMapRenderer& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	CString GetField();
	void SetField(LPCTSTR);
	CMoSymbol GetDefaultSymbol();
	void SetDefaultSymbol(LPDISPATCH);
	BOOL GetUseDefault();
	void SetUseDefault(BOOL);
	short GetValueCount();
	void SetValueCount(short);
	CString GetTag();
	void SetTag(LPCTSTR);
	long GetSymbolType();
	void SetSymbolType(long);
	CString GetRotationField();
	void SetRotationField(LPCTSTR);
	CString GetScalingField();
	void SetScalingField(LPCTSTR);

// Operationen
public:
	CString GetValue(short index);
	void SetValue(short index, LPCTSTR lpszNewValue);
	CMoSymbol GetSymbol(short index);
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_MOVALUEMAPRENDERER_H__CC25FE4B_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
