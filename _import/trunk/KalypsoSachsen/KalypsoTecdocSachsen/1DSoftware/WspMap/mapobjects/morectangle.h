#if !defined(AFX_MORECTANGLE_H__CC25FE62_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MORECTANGLE_H__CC25FE62_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
class CMoPoint;
class CMoPoints;

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoRectangle 

class CMoRectangle : public COleDispatchDriver
{
public:
	CMoRectangle() {}		// Ruft den Standardkonstruktor für COleDispatchDriver auf
	CMoRectangle(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoRectangle(const CMoRectangle& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	double GetLeft();
	void SetLeft(double);
	double GetRight();
	void SetRight(double);
	double GetTop();
	void SetTop(double);
	double GetBottom();
	void SetBottom(double);
	double GetWidth();
	void SetWidth(double);
	double GetHeight();
	void SetHeight(double);
	CMoPoint GetCenter();
	void SetCenter(LPDISPATCH);
	double GetFloor();
	void SetFloor(double);
	double GetCeiling();
	void SetCeiling(double);
	double GetDepth();
	void SetDepth(double);
	long GetShapeType();
	void SetShapeType(long);

// Operationen
public:
	void ScaleRectangle(double scaleFactor);
	void Offset(double deltaX, double deltaY);
	void Inset(double deltaX, double deltaY);
	BOOL IsPointIn(LPDISPATCH Point);
	BOOL Intersects(LPDISPATCH Rectangle);
	double DistanceTo(LPDISPATCH shape);
	CMoPoints GetCrossings(LPDISPATCH shape);
	LPDISPATCH Difference(LPDISPATCH anotherShape, const VARIANT& Extent);
	LPDISPATCH Intersect(LPDISPATCH anotherShape, const VARIANT& Extent);
	LPDISPATCH Xor(LPDISPATCH anotherShape, const VARIANT& Extent);
	LPDISPATCH Union(LPDISPATCH anotherShape, const VARIANT& Extent);
	LPDISPATCH Buffer(double distance, const VARIANT& Extent);
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_MORECTANGLE_H__CC25FE62_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
