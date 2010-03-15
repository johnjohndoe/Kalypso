#if !defined(AFX_MOIMAGELAYER_H__CC25FE5A_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOIMAGELAYER_H__CC25FE5A_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht �ndern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre �nderungen �berschrieben.


// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
class CMoRectangle;

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoImageLayer 

class CMoImageLayer : public COleDispatchDriver
{
public:
	CMoImageLayer() {}		// Ruft den Standardkonstruktor f�r COleDispatchDriver auf
	CMoImageLayer(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoImageLayer(const CMoImageLayer& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	BOOL GetVisible();
	void SetVisible(BOOL);
	CString GetName();
	void SetName(LPCTSTR);
	CMoRectangle GetExtent();
	void SetExtent(LPDISPATCH);
	CString GetFile();
	void SetFile(LPCTSTR);
	long GetLayerType();
	void SetLayerType(long);
	CString GetTag();
	void SetTag(LPCTSTR);
	BOOL GetValid();
	void SetValid(BOOL);
	BOOL GetUpdateWhileDrawing();
	void SetUpdateWhileDrawing(BOOL);
	BOOL GetTransparent();
	void SetTransparent(BOOL);
	unsigned long GetTransparentColor();
	void SetTransparentColor(unsigned long);

// Operationen
public:
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ f�gt unmittelbar vor der vorhergehenden Zeile zus�tzliche Deklarationen ein.

#endif // AFX_MOIMAGELAYER_H__CC25FE5A_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
