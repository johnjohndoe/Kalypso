#if !defined(AFX_MOTABLEDESC_H__CC25FE5F_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOTABLEDESC_H__CC25FE5F_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoTableDesc 

class CMoTableDesc : public COleDispatchDriver
{
public:
	CMoTableDesc() {}		// Ruft den Standardkonstruktor für COleDispatchDriver auf
	CMoTableDesc(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoTableDesc(const CMoTableDesc& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	short GetFieldCount();
	void SetFieldCount(short);
	long GetCodePage();
	void SetCodePage(long);

// Operationen
public:
	CString GetFieldName(short index);
	void SetFieldName(short index, LPCTSTR lpszNewValue);
	long GetFieldType(short index);
	void SetFieldType(short index, long nNewValue);
	short GetFieldPrecision(short index);
	void SetFieldPrecision(short index, short nNewValue);
	short GetFieldLength(short index);
	void SetFieldLength(short index, short nNewValue);
	short GetFieldScale(short index);
	void SetFieldScale(short index, short nNewValue);
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_MOTABLEDESC_H__CC25FE5F_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
