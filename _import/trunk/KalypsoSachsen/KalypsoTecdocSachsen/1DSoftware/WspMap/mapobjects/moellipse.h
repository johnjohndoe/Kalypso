#if !defined(AFX_MOELLIPSE_H__CC25FE42_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOELLIPSE_H__CC25FE42_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
class CMoRectangle;
class CMoPoint;

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoEllipse 

class CMoEllipse : public COleDispatchDriver
{
public:
	CMoEllipse() {}		// Ruft den Standardkonstruktor für COleDispatchDriver auf
	CMoEllipse(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoEllipse(const CMoEllipse& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	CMoRectangle GetExtent();
	void SetExtent(LPDISPATCH);
	double GetLeft();
	void SetLeft(double);
	double GetRight();
	void SetRight(double);
	double GetTop();
	void SetTop(double);
	double GetBottom();
	void SetBottom(double);
	CMoPoint GetCenter();
	void SetCenter(LPDISPATCH);
	double GetWidth();
	void SetWidth(double);
	double GetHeight();
	void SetHeight(double);
	long GetShapeType();
	void SetShapeType(long);

// Operationen
public:
	BOOL IsPointIn(LPDISPATCH Point);
	void Offset(double deltaX, double deltaY);
	void Inset(double deltaX, double deltaY);
	LPDISPATCH Union(LPDISPATCH anotherShape, const VARIANT& Extent);
	LPDISPATCH Xor(LPDISPATCH anotherShape, const VARIANT& Extent);
	LPDISPATCH Difference(LPDISPATCH anotherShape, const VARIANT& Extent);
	LPDISPATCH Intersect(LPDISPATCH anotherShape, const VARIANT& Extent);
	LPDISPATCH Buffer(double distance, const VARIANT& Extent);
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_MOELLIPSE_H__CC25FE42_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
