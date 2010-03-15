// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "morectangle.h"

// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
#include "mopoint.h"
#include "mopoints.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoRectangle 

double CMoRectangle::GetLeft()
{
	double result;
	GetProperty(0x1, VT_R8, (void*)&result);
	return result;
}

void CMoRectangle::SetLeft(double propVal)
{
	SetProperty(0x1, VT_R8, propVal);
}

double CMoRectangle::GetRight()
{
	double result;
	GetProperty(0x2, VT_R8, (void*)&result);
	return result;
}

void CMoRectangle::SetRight(double propVal)
{
	SetProperty(0x2, VT_R8, propVal);
}

double CMoRectangle::GetTop()
{
	double result;
	GetProperty(0x3, VT_R8, (void*)&result);
	return result;
}

void CMoRectangle::SetTop(double propVal)
{
	SetProperty(0x3, VT_R8, propVal);
}

double CMoRectangle::GetBottom()
{
	double result;
	GetProperty(0x4, VT_R8, (void*)&result);
	return result;
}

void CMoRectangle::SetBottom(double propVal)
{
	SetProperty(0x4, VT_R8, propVal);
}

double CMoRectangle::GetWidth()
{
	double result;
	GetProperty(0x5, VT_R8, (void*)&result);
	return result;
}

void CMoRectangle::SetWidth(double propVal)
{
	SetProperty(0x5, VT_R8, propVal);
}

double CMoRectangle::GetHeight()
{
	double result;
	GetProperty(0x6, VT_R8, (void*)&result);
	return result;
}

void CMoRectangle::SetHeight(double propVal)
{
	SetProperty(0x6, VT_R8, propVal);
}

CMoPoint CMoRectangle::GetCenter()
{
	LPDISPATCH pDispatch;
	GetProperty(0x7, VT_DISPATCH, (void*)&pDispatch);
	return CMoPoint(pDispatch);
}

void CMoRectangle::SetCenter(LPDISPATCH propVal)
{
	SetProperty(0x7, VT_DISPATCH, propVal);
}

double CMoRectangle::GetFloor()
{
	double result;
	GetProperty(0x8, VT_R8, (void*)&result);
	return result;
}

void CMoRectangle::SetFloor(double propVal)
{
	SetProperty(0x8, VT_R8, propVal);
}

double CMoRectangle::GetCeiling()
{
	double result;
	GetProperty(0x9, VT_R8, (void*)&result);
	return result;
}

void CMoRectangle::SetCeiling(double propVal)
{
	SetProperty(0x9, VT_R8, propVal);
}

double CMoRectangle::GetDepth()
{
	double result;
	GetProperty(0xa, VT_R8, (void*)&result);
	return result;
}

void CMoRectangle::SetDepth(double propVal)
{
	SetProperty(0xa, VT_R8, propVal);
}

long CMoRectangle::GetShapeType()
{
	long result;
	GetProperty(0xb, VT_I4, (void*)&result);
	return result;
}

void CMoRectangle::SetShapeType(long propVal)
{
	SetProperty(0xb, VT_I4, propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoRectangle 

void CMoRectangle::ScaleRectangle(double scaleFactor)
{
	static BYTE parms[] =
		VTS_R8;
	InvokeHelper(0xc, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 scaleFactor);
}

void CMoRectangle::Offset(double deltaX, double deltaY)
{
	static BYTE parms[] =
		VTS_R8 VTS_R8;
	InvokeHelper(0xd, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 deltaX, deltaY);
}

void CMoRectangle::Inset(double deltaX, double deltaY)
{
	static BYTE parms[] =
		VTS_R8 VTS_R8;
	InvokeHelper(0xe, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 deltaX, deltaY);
}

BOOL CMoRectangle::IsPointIn(LPDISPATCH Point)
{
	BOOL result;
	static BYTE parms[] =
		VTS_DISPATCH;
	InvokeHelper(0xf, DISPATCH_METHOD, VT_BOOL, (void*)&result, parms,
		Point);
	return result;
}

BOOL CMoRectangle::Intersects(LPDISPATCH Rectangle)
{
	BOOL result;
	static BYTE parms[] =
		VTS_DISPATCH;
	InvokeHelper(0x10, DISPATCH_METHOD, VT_BOOL, (void*)&result, parms,
		Rectangle);
	return result;
}

double CMoRectangle::DistanceTo(LPDISPATCH shape)
{
	double result;
	static BYTE parms[] =
		VTS_DISPATCH;
	InvokeHelper(0x11, DISPATCH_METHOD, VT_R8, (void*)&result, parms,
		shape);
	return result;
}

CMoPoints CMoRectangle::GetCrossings(LPDISPATCH shape)
{
	LPDISPATCH pDispatch;
	static BYTE parms[] =
		VTS_DISPATCH;
	InvokeHelper(0x12, DISPATCH_METHOD, VT_DISPATCH, (void*)&pDispatch, parms,
		shape);
	return CMoPoints(pDispatch);
}

LPDISPATCH CMoRectangle::Difference(LPDISPATCH anotherShape, const VARIANT& Extent)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_DISPATCH VTS_VARIANT;
	InvokeHelper(0x13, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		anotherShape, &Extent);
	return result;
}

LPDISPATCH CMoRectangle::Intersect(LPDISPATCH anotherShape, const VARIANT& Extent)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_DISPATCH VTS_VARIANT;
	InvokeHelper(0x14, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		anotherShape, &Extent);
	return result;
}

LPDISPATCH CMoRectangle::Xor(LPDISPATCH anotherShape, const VARIANT& Extent)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_DISPATCH VTS_VARIANT;
	InvokeHelper(0x15, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		anotherShape, &Extent);
	return result;
}

LPDISPATCH CMoRectangle::Union(LPDISPATCH anotherShape, const VARIANT& Extent)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_DISPATCH VTS_VARIANT;
	InvokeHelper(0x16, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		anotherShape, &Extent);
	return result;
}

LPDISPATCH CMoRectangle::Buffer(double distance, const VARIANT& Extent)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_R8 VTS_VARIANT;
	InvokeHelper(0x17, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		distance, &Extent);
	return result;
}
