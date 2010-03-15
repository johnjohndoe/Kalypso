#if !defined(AFX_MOLINE_H__CC25FE5B_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOLINE_H__CC25FE5B_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
class CMoRectangle;
class CMoParts;
class CMoPoints;

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoLine 

class CMoLine : public COleDispatchDriver
{
public:
	CMoLine() {}		// Ruft den Standardkonstruktor für COleDispatchDriver auf
	CMoLine(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoLine(const CMoLine& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	CMoRectangle GetExtent();
	void SetExtent(LPDISPATCH);
	CMoParts GetParts();
	void SetParts(LPDISPATCH);
	double GetLength();
	void SetLength(double);
	long GetShapeType();
	void SetShapeType(long);
	BOOL GetIsFullyMeasured();
	void SetIsFullyMeasured(BOOL);

// Operationen
public:
	void Offset(double deltaX, double deltaY);
	double DistanceTo(LPDISPATCH shape);
	CMoPoints GetCrossings(LPDISPATCH shape);
	CMoPoints ReturnPointEvents(double Measure);
	CMoLine ReturnLineEvent(double start, double end);
	double ReturnMeasure(LPDISPATCH location);
	void UpdateMeasures();
	void SetMeasures(double start, double end);
	void SetMeasuresAsLength();
	void OffsetMeasures(double Offset);
	void MultiplyMeasures(double factor);
	LPDISPATCH Union(LPDISPATCH anotherLine, const VARIANT& Extent);
	LPDISPATCH Xor(LPDISPATCH anotherShape, const VARIANT& Extent);
	LPDISPATCH Difference(LPDISPATCH anotherShape, const VARIANT& Extent);
	LPDISPATCH Intersect(LPDISPATCH anotherShape, const VARIANT& Extent);
	LPDISPATCH Buffer(double distance, const VARIANT& Extent);
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_MOLINE_H__CC25FE5B_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
