#if !defined(AFX_MOSBEXTENT_H__CC25FE72_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOSBEXTENT_H__CC25FE72_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht �ndern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre �nderungen �berschrieben.

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoSbExtent 

class CMoSbExtent : public COleDispatchDriver
{
public:
	CMoSbExtent() {}		// Ruft den Standardkonstruktor f�r COleDispatchDriver auf
	CMoSbExtent(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoSbExtent(const CMoSbExtent& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:

// Operationen
public:
	double GetMinX();
	void SetMinX(double newValue);
	double GetMinY();
	void SetMinY(double newValue);
	double GetMaxX();
	void SetMaxX(double newValue);
	double GetMaxY();
	void SetMaxY(double newValue);
	double GetWidth();
	double GetHeight();
	double GetCenterX();
	VARIANT GetCenterY();
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ f�gt unmittelbar vor der vorhergehenden Zeile zus�tzliche Deklarationen ein.

#endif // AFX_MOSBEXTENT_H__CC25FE72_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
