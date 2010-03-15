// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "mopoint.h"

// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
#include "mopoints.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoPoint 

double CMoPoint::GetY()
{
	double result;
	GetProperty(0x1, VT_R8, (void*)&result);
	return result;
}

void CMoPoint::SetY(double propVal)
{
	SetProperty(0x1, VT_R8, propVal);
}

double CMoPoint::GetX()
{
	double result;
	GetProperty(0x2, VT_R8, (void*)&result);
	return result;
}

void CMoPoint::SetX(double propVal)
{
	SetProperty(0x2, VT_R8, propVal);
}

double CMoPoint::GetZ()
{
	double result;
	GetProperty(0x3, VT_R8, (void*)&result);
	return result;
}

void CMoPoint::SetZ(double propVal)
{
	SetProperty(0x3, VT_R8, propVal);
}

double CMoPoint::GetMeasure()
{
	double result;
	GetProperty(0x4, VT_R8, (void*)&result);
	return result;
}

void CMoPoint::SetMeasure(double propVal)
{
	SetProperty(0x4, VT_R8, propVal);
}

long CMoPoint::GetShapeType()
{
	long result;
	GetProperty(0x5, VT_I4, (void*)&result);
	return result;
}

void CMoPoint::SetShapeType(long propVal)
{
	SetProperty(0x5, VT_I4, propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoPoint 

double CMoPoint::DistanceTo(LPDISPATCH shape)
{
	double result;
	static BYTE parms[] =
		VTS_DISPATCH;
	InvokeHelper(0x6, DISPATCH_METHOD, VT_R8, (void*)&result, parms,
		shape);
	return result;
}

double CMoPoint::DistanceToSegment(LPDISPATCH point1, LPDISPATCH point2)
{
	double result;
	static BYTE parms[] =
		VTS_DISPATCH VTS_DISPATCH;
	InvokeHelper(0x7, DISPATCH_METHOD, VT_R8, (void*)&result, parms,
		point1, point2);
	return result;
}

CMoPoints CMoPoint::GetCrossings(LPDISPATCH shape)
{
	LPDISPATCH pDispatch;
	static BYTE parms[] =
		VTS_DISPATCH;
	InvokeHelper(0x8, DISPATCH_METHOD, VT_DISPATCH, (void*)&pDispatch, parms,
		shape);
	return CMoPoints(pDispatch);
}

LPDISPATCH CMoPoint::Union(LPDISPATCH anotherShape, const VARIANT& Extent)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_DISPATCH VTS_VARIANT;
	InvokeHelper(0x9, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		anotherShape, &Extent);
	return result;
}

LPDISPATCH CMoPoint::Xor(LPDISPATCH anotherShape, const VARIANT& Extent)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_DISPATCH VTS_VARIANT;
	InvokeHelper(0xa, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		anotherShape, &Extent);
	return result;
}

LPDISPATCH CMoPoint::Difference(LPDISPATCH anotherShape, const VARIANT& Extent)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_DISPATCH VTS_VARIANT;
	InvokeHelper(0xb, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		anotherShape, &Extent);
	return result;
}

LPDISPATCH CMoPoint::Intersect(LPDISPATCH anotherShape, const VARIANT& Extent)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_DISPATCH VTS_VARIANT;
	InvokeHelper(0xc, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		anotherShape, &Extent);
	return result;
}

LPDISPATCH CMoPoint::Buffer(double distance, const VARIANT& Extent)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_R8 VTS_VARIANT;
	InvokeHelper(0xd, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		distance, &Extent);
	return result;
}
