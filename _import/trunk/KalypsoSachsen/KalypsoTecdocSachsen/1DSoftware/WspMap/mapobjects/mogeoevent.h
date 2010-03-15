#if !defined(AFX_MOGEOEVENT_H__CC25FE5C_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOGEOEVENT_H__CC25FE5C_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoGeoEvent 

class CMoGeoEvent : public COleDispatchDriver
{
public:
	CMoGeoEvent() {}		// Ruft den Standardkonstruktor für COleDispatchDriver auf
	CMoGeoEvent(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoGeoEvent(const CMoGeoEvent& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	CString GetTag();
	void SetTag(LPCTSTR);
	long GetSymbolIndex();
	void SetSymbolIndex(long);
	double GetX();
	void SetX(double);
	double GetY();
	void SetY(double);
	long GetIndex();
	void SetIndex(long);
	LPDISPATCH GetShape();
	void SetShape(LPDISPATCH);

// Operationen
public:
	void MoveTo(double X, double Y);
	void Move(double X, double Y);
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_MOGEOEVENT_H__CC25FE5C_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
