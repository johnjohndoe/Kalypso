#if !defined(AFX_MOPOINTS_H__CC25FE49_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOPOINTS_H__CC25FE49_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht �ndern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre �nderungen �berschrieben.


// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
class CMoRectangle;
class CMoPoint;

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoPoints 

class CMoPoints : public COleDispatchDriver
{
public:
	CMoPoints() {}		// Ruft den Standardkonstruktor f�r COleDispatchDriver auf
	CMoPoints(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoPoints(const CMoPoints& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	long GetCount();
	void SetCount(long);
	long GetShapeType();
	void SetShapeType(long);
	CMoRectangle GetExtent();
	void SetExtent(LPDISPATCH);

// Operationen
public:
	CMoPoint Item(const VARIANT& Item);
	void Add(LPDISPATCH Point);
	void Set(long index, LPDISPATCH Point);
	void Remove(long index);
	void Insert(long index, LPDISPATCH Point);
	void Reverse();
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

#endif // AFX_MOPOINTS_H__CC25FE49_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
