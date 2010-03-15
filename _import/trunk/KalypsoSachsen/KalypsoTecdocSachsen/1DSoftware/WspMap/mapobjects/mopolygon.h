#if !defined(AFX_MOPOLYGON_H__CC25FE40_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOPOLYGON_H__CC25FE40_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht �ndern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre �nderungen �berschrieben.


// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
class CMoRectangle;
class CMoParts;
class CMoPoint;
class CMoPoints;

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoPolygon 

class CMoPolygon : public COleDispatchDriver
{
public:
	CMoPolygon() {}		// Ruft den Standardkonstruktor f�r COleDispatchDriver auf
	CMoPolygon(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoPolygon(const CMoPolygon& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	CMoRectangle GetExtent();
	void SetExtent(LPDISPATCH);
	CMoParts GetParts();
	void SetParts(LPDISPATCH);
	CMoPoint GetCentroid();
	void SetCentroid(LPDISPATCH);
	double GetArea();
	void SetArea(double);
	double GetPerimeter();
	void SetPerimeter(double);
	long GetShapeType();
	void SetShapeType(long);

// Operationen
public:
	BOOL IsPointIn(LPDISPATCH Point);
	void Offset(double deltaX, double deltaY);
	double DistanceTo(LPDISPATCH shape);
	CMoPoints GetCrossings(LPDISPATCH shape);
	LPDISPATCH Union(LPDISPATCH anotherShape, const VARIANT& Extent);
	LPDISPATCH Xor(LPDISPATCH anotherShape, const VARIANT& Extent);
	LPDISPATCH Difference(LPDISPATCH anotherShape, const VARIANT& Extent);
	LPDISPATCH Intersect(LPDISPATCH anotherShape, const VARIANT& Extent);
	LPDISPATCH Buffer(double distance, const VARIANT& Extent);
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ f�gt unmittelbar vor der vorhergehenden Zeile zus�tzliche Deklarationen ein.

#endif // AFX_MOPOLYGON_H__CC25FE40_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
