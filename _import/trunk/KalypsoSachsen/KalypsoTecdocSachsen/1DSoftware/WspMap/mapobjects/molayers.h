#if !defined(AFX_MOLAYERS_H__CC25FE67_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOLAYERS_H__CC25FE67_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoLayers 

class CMoLayers : public COleDispatchDriver
{
public:
	CMoLayers() {}		// Ruft den Standardkonstruktor für COleDispatchDriver auf
	CMoLayers(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoLayers(const CMoLayers& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	short GetCount();
	void SetCount(short);

// Operationen
public:
	LPDISPATCH Item(const VARIANT& index);
	BOOL Add(LPDISPATCH layer);
	void Remove(short index);
	void Clear();
	void MoveTo(short fromIndex, short toIndex);
	void MoveToTop(short index);
	void MoveToBottom(short index);
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_MOLAYERS_H__CC25FE67_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
