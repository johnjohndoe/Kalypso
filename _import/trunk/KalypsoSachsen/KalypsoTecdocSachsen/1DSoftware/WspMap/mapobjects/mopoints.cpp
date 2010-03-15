// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "mopoints.h"

// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
#include "MoRectangle.h"
#include "MoPoint.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoPoints 

long CMoPoints::GetCount()
{
	long result;
	GetProperty(0x1, VT_I4, (void*)&result);
	return result;
}

void CMoPoints::SetCount(long propVal)
{
	SetProperty(0x1, VT_I4, propVal);
}

long CMoPoints::GetShapeType()
{
	long result;
	GetProperty(0x2, VT_I4, (void*)&result);
	return result;
}

void CMoPoints::SetShapeType(long propVal)
{
	SetProperty(0x2, VT_I4, propVal);
}

CMoRectangle CMoPoints::GetExtent()
{
	LPDISPATCH pDispatch;
	GetProperty(0x3, VT_DISPATCH, (void*)&pDispatch);
	return CMoRectangle(pDispatch);
}

void CMoPoints::SetExtent(LPDISPATCH propVal)
{
	SetProperty(0x3, VT_DISPATCH, propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoPoints 

CMoPoint CMoPoints::Item(const VARIANT& Item)
{
	LPDISPATCH pDispatch;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x4, DISPATCH_METHOD, VT_DISPATCH, (void*)&pDispatch, parms,
		&Item);
	return CMoPoint(pDispatch);
}

void CMoPoints::Add(LPDISPATCH Point)
{
	static BYTE parms[] =
		VTS_DISPATCH;
	InvokeHelper(0x5, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 Point);
}

void CMoPoints::Set(long index, LPDISPATCH Point)
{
	static BYTE parms[] =
		VTS_I4 VTS_DISPATCH;
	InvokeHelper(0x6, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 index, Point);
}

void CMoPoints::Remove(long index)
{
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x7, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 index);
}

void CMoPoints::Insert(long index, LPDISPATCH Point)
{
	static BYTE parms[] =
		VTS_I4 VTS_DISPATCH;
	InvokeHelper(0x8, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 index, Point);
}

void CMoPoints::Reverse()
{
	InvokeHelper(0x9, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void CMoPoints::Offset(double deltaX, double deltaY)
{
	static BYTE parms[] =
		VTS_R8 VTS_R8;
	InvokeHelper(0xa, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 deltaX, deltaY);
}

double CMoPoints::DistanceTo(LPDISPATCH shape)
{
	double result;
	static BYTE parms[] =
		VTS_DISPATCH;
	InvokeHelper(0xb, DISPATCH_METHOD, VT_R8, (void*)&result, parms,
		shape);
	return result;
}

CMoPoints CMoPoints::GetCrossings(LPDISPATCH shape)
{
	LPDISPATCH pDispatch;
	static BYTE parms[] =
		VTS_DISPATCH;
	InvokeHelper(0xc, DISPATCH_METHOD, VT_DISPATCH, (void*)&pDispatch, parms,
		shape);
	return CMoPoints(pDispatch);
}

LPDISPATCH CMoPoints::Union(LPDISPATCH anotherShape, const VARIANT& Extent)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_DISPATCH VTS_VARIANT;
	InvokeHelper(0xd, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		anotherShape, &Extent);
	return result;
}

LPDISPATCH CMoPoints::Xor(LPDISPATCH anotherShape, const VARIANT& Extent)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_DISPATCH VTS_VARIANT;
	InvokeHelper(0xe, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		anotherShape, &Extent);
	return result;
}

LPDISPATCH CMoPoints::Difference(LPDISPATCH anotherShape, const VARIANT& Extent)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_DISPATCH VTS_VARIANT;
	InvokeHelper(0xf, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		anotherShape, &Extent);
	return result;
}

LPDISPATCH CMoPoints::Intersect(LPDISPATCH anotherShape, const VARIANT& Extent)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_DISPATCH VTS_VARIANT;
	InvokeHelper(0x10, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		anotherShape, &Extent);
	return result;
}

LPDISPATCH CMoPoints::Buffer(double distance, const VARIANT& Extent)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_R8 VTS_VARIANT;
	InvokeHelper(0x11, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		distance, &Extent);
	return result;
}
