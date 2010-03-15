#if !defined(AFX_MOPOINT_H__CC25FE55_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOPOINT_H__CC25FE55_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
class CMoPoints;

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoPoint 

class CMoPoint : public COleDispatchDriver
{
public:
	CMoPoint() {}		// Ruft den Standardkonstruktor für COleDispatchDriver auf
	CMoPoint(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoPoint(const CMoPoint& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	double GetY();
	void SetY(double);
	double GetX();
	void SetX(double);
	double GetZ();
	void SetZ(double);
	double GetMeasure();
	void SetMeasure(double);
	long GetShapeType();
	void SetShapeType(long);

// Operationen
public:
	double DistanceTo(LPDISPATCH shape);
	double DistanceToSegment(LPDISPATCH point1, LPDISPATCH point2);
	CMoPoints GetCrossings(LPDISPATCH shape);
	LPDISPATCH Union(LPDISPATCH anotherShape, const VARIANT& Extent);
	LPDISPATCH Xor(LPDISPATCH anotherShape, const VARIANT& Extent);
	LPDISPATCH Difference(LPDISPATCH anotherShape, const VARIANT& Extent);
	LPDISPATCH Intersect(LPDISPATCH anotherShape, const VARIANT& Extent);
	LPDISPATCH Buffer(double distance, const VARIANT& Extent);
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_MOPOINT_H__CC25FE55_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
