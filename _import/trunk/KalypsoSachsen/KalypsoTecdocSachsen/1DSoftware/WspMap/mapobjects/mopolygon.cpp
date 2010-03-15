// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "mopolygon.h"

// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
#include "MoRectangle.h"
#include "MoParts.h"
#include "MoPoint.h"
#include "MoPoints.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoPolygon 

CMoRectangle CMoPolygon::GetExtent()
{
	LPDISPATCH pDispatch;
	GetProperty(0x1, VT_DISPATCH, (void*)&pDispatch);
	return CMoRectangle(pDispatch);
}

void CMoPolygon::SetExtent(LPDISPATCH propVal)
{
	SetProperty(0x1, VT_DISPATCH, propVal);
}

CMoParts CMoPolygon::GetParts()
{
	LPDISPATCH pDispatch;
	GetProperty(0x2, VT_DISPATCH, (void*)&pDispatch);
	return CMoParts(pDispatch);
}

void CMoPolygon::SetParts(LPDISPATCH propVal)
{
	SetProperty(0x2, VT_DISPATCH, propVal);
}

CMoPoint CMoPolygon::GetCentroid()
{
	LPDISPATCH pDispatch;
	GetProperty(0x3, VT_DISPATCH, (void*)&pDispatch);
	return CMoPoint(pDispatch);
}

void CMoPolygon::SetCentroid(LPDISPATCH propVal)
{
	SetProperty(0x3, VT_DISPATCH, propVal);
}

double CMoPolygon::GetArea()
{
	double result;
	GetProperty(0x4, VT_R8, (void*)&result);
	return result;
}

void CMoPolygon::SetArea(double propVal)
{
	SetProperty(0x4, VT_R8, propVal);
}

double CMoPolygon::GetPerimeter()
{
	double result;
	GetProperty(0x5, VT_R8, (void*)&result);
	return result;
}

void CMoPolygon::SetPerimeter(double propVal)
{
	SetProperty(0x5, VT_R8, propVal);
}

long CMoPolygon::GetShapeType()
{
	long result;
	GetProperty(0x6, VT_I4, (void*)&result);
	return result;
}

void CMoPolygon::SetShapeType(long propVal)
{
	SetProperty(0x6, VT_I4, propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoPolygon 

BOOL CMoPolygon::IsPointIn(LPDISPATCH Point)
{
	BOOL result;
	static BYTE parms[] =
		VTS_DISPATCH;
	InvokeHelper(0x7, DISPATCH_METHOD, VT_BOOL, (void*)&result, parms,
		Point);
	return result;
}

void CMoPolygon::Offset(double deltaX, double deltaY)
{
	static BYTE parms[] =
		VTS_R8 VTS_R8;
	InvokeHelper(0x8, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 deltaX, deltaY);
}

double CMoPolygon::DistanceTo(LPDISPATCH shape)
{
	double result;
	static BYTE parms[] =
		VTS_DISPATCH;
	InvokeHelper(0x9, DISPATCH_METHOD, VT_R8, (void*)&result, parms,
		shape);
	return result;
}

CMoPoints CMoPolygon::GetCrossings(LPDISPATCH shape)
{
	LPDISPATCH pDispatch;
	static BYTE parms[] =
		VTS_DISPATCH;
	InvokeHelper(0xa, DISPATCH_METHOD, VT_DISPATCH, (void*)&pDispatch, parms,
		shape);
	return CMoPoints(pDispatch);
}

LPDISPATCH CMoPolygon::Union(LPDISPATCH anotherShape, const VARIANT& Extent)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_DISPATCH VTS_VARIANT;
	InvokeHelper(0xb, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		anotherShape, &Extent);
	return result;
}

LPDISPATCH CMoPolygon::Xor(LPDISPATCH anotherShape, const VARIANT& Extent)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_DISPATCH VTS_VARIANT;
	InvokeHelper(0xc, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		anotherShape, &Extent);
	return result;
}

LPDISPATCH CMoPolygon::Difference(LPDISPATCH anotherShape, const VARIANT& Extent)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_DISPATCH VTS_VARIANT;
	InvokeHelper(0xd, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		anotherShape, &Extent);
	return result;
}

LPDISPATCH CMoPolygon::Intersect(LPDISPATCH anotherShape, const VARIANT& Extent)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_DISPATCH VTS_VARIANT;
	InvokeHelper(0xe, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		anotherShape, &Extent);
	return result;
}

LPDISPATCH CMoPolygon::Buffer(double distance, const VARIANT& Extent)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_R8 VTS_VARIANT;
	InvokeHelper(0xf, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		distance, &Extent);
	return result;
}
