// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "moline.h"

// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
#include "MoRectangle.h"
#include "moparts.h"
#include "mopoints.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoLine 

CMoRectangle CMoLine::GetExtent()
{
	LPDISPATCH pDispatch;
	GetProperty(0x1, VT_DISPATCH, (void*)&pDispatch);
	return CMoRectangle(pDispatch);
}

void CMoLine::SetExtent(LPDISPATCH propVal)
{
	SetProperty(0x1, VT_DISPATCH, propVal);
}

CMoParts CMoLine::GetParts()
{
	LPDISPATCH pDispatch;
	GetProperty(0x2, VT_DISPATCH, (void*)&pDispatch);
	return CMoParts(pDispatch);
}

void CMoLine::SetParts(LPDISPATCH propVal)
{
	SetProperty(0x2, VT_DISPATCH, propVal);
}

double CMoLine::GetLength()
{
	double result;
	GetProperty(0x3, VT_R8, (void*)&result);
	return result;
}

void CMoLine::SetLength(double propVal)
{
	SetProperty(0x3, VT_R8, propVal);
}

long CMoLine::GetShapeType()
{
	long result;
	GetProperty(0x4, VT_I4, (void*)&result);
	return result;
}

void CMoLine::SetShapeType(long propVal)
{
	SetProperty(0x4, VT_I4, propVal);
}

BOOL CMoLine::GetIsFullyMeasured()
{
	BOOL result;
	GetProperty(0x5, VT_BOOL, (void*)&result);
	return result;
}

void CMoLine::SetIsFullyMeasured(BOOL propVal)
{
	SetProperty(0x5, VT_BOOL, propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoLine 

void CMoLine::Offset(double deltaX, double deltaY)
{
	static BYTE parms[] =
		VTS_R8 VTS_R8;
	InvokeHelper(0x6, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 deltaX, deltaY);
}

double CMoLine::DistanceTo(LPDISPATCH shape)
{
	double result;
	static BYTE parms[] =
		VTS_DISPATCH;
	InvokeHelper(0x7, DISPATCH_METHOD, VT_R8, (void*)&result, parms,
		shape);
	return result;
}

CMoPoints CMoLine::GetCrossings(LPDISPATCH shape)
{
	LPDISPATCH pDispatch;
	static BYTE parms[] =
		VTS_DISPATCH;
	InvokeHelper(0x8, DISPATCH_METHOD, VT_DISPATCH, (void*)&pDispatch, parms,
		shape);
	return CMoPoints(pDispatch);
}

CMoPoints CMoLine::ReturnPointEvents(double Measure)
{
	LPDISPATCH pDispatch;
	static BYTE parms[] =
		VTS_R8;
	InvokeHelper(0x9, DISPATCH_METHOD, VT_DISPATCH, (void*)&pDispatch, parms,
		Measure);
	return CMoPoints(pDispatch);
}

CMoLine CMoLine::ReturnLineEvent(double start, double end)
{
	LPDISPATCH pDispatch;
	static BYTE parms[] =
		VTS_R8 VTS_R8;
	InvokeHelper(0xa, DISPATCH_METHOD, VT_DISPATCH, (void*)&pDispatch, parms,
		start, end);
	return CMoLine(pDispatch);
}

double CMoLine::ReturnMeasure(LPDISPATCH location)
{
	double result;
	static BYTE parms[] =
		VTS_DISPATCH;
	InvokeHelper(0xb, DISPATCH_METHOD, VT_R8, (void*)&result, parms,
		location);
	return result;
}

void CMoLine::UpdateMeasures()
{
	InvokeHelper(0xc, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void CMoLine::SetMeasures(double start, double end)
{
	static BYTE parms[] =
		VTS_R8 VTS_R8;
	InvokeHelper(0xd, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 start, end);
}

void CMoLine::SetMeasuresAsLength()
{
	InvokeHelper(0xe, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void CMoLine::OffsetMeasures(double Offset)
{
	static BYTE parms[] =
		VTS_R8;
	InvokeHelper(0xf, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 Offset);
}

void CMoLine::MultiplyMeasures(double factor)
{
	static BYTE parms[] =
		VTS_R8;
	InvokeHelper(0x10, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 factor);
}

LPDISPATCH CMoLine::Union(LPDISPATCH anotherLine, const VARIANT& Extent)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_DISPATCH VTS_VARIANT;
	InvokeHelper(0x11, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		anotherLine, &Extent);
	return result;
}

LPDISPATCH CMoLine::Xor(LPDISPATCH anotherShape, const VARIANT& Extent)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_DISPATCH VTS_VARIANT;
	InvokeHelper(0x12, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		anotherShape, &Extent);
	return result;
}

LPDISPATCH CMoLine::Difference(LPDISPATCH anotherShape, const VARIANT& Extent)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_DISPATCH VTS_VARIANT;
	InvokeHelper(0x13, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		anotherShape, &Extent);
	return result;
}

LPDISPATCH CMoLine::Intersect(LPDISPATCH anotherShape, const VARIANT& Extent)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_DISPATCH VTS_VARIANT;
	InvokeHelper(0x14, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		anotherShape, &Extent);
	return result;
}

LPDISPATCH CMoLine::Buffer(double distance, const VARIANT& Extent)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_R8 VTS_VARIANT;
	InvokeHelper(0x15, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		distance, &Extent);
	return result;
}
