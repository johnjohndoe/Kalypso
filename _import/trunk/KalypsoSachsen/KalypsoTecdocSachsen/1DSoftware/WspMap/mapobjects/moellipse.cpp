// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "moellipse.h"

// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
#include "MoRectangle.h"
#include "MoPoint.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoEllipse 

CMoRectangle CMoEllipse::GetExtent()
{
	LPDISPATCH pDispatch;
	GetProperty(0x1, VT_DISPATCH, (void*)&pDispatch);
	return CMoRectangle(pDispatch);
}

void CMoEllipse::SetExtent(LPDISPATCH propVal)
{
	SetProperty(0x1, VT_DISPATCH, propVal);
}

double CMoEllipse::GetLeft()
{
	double result;
	GetProperty(0x2, VT_R8, (void*)&result);
	return result;
}

void CMoEllipse::SetLeft(double propVal)
{
	SetProperty(0x2, VT_R8, propVal);
}

double CMoEllipse::GetRight()
{
	double result;
	GetProperty(0x3, VT_R8, (void*)&result);
	return result;
}

void CMoEllipse::SetRight(double propVal)
{
	SetProperty(0x3, VT_R8, propVal);
}

double CMoEllipse::GetTop()
{
	double result;
	GetProperty(0x4, VT_R8, (void*)&result);
	return result;
}

void CMoEllipse::SetTop(double propVal)
{
	SetProperty(0x4, VT_R8, propVal);
}

double CMoEllipse::GetBottom()
{
	double result;
	GetProperty(0x5, VT_R8, (void*)&result);
	return result;
}

void CMoEllipse::SetBottom(double propVal)
{
	SetProperty(0x5, VT_R8, propVal);
}

CMoPoint CMoEllipse::GetCenter()
{
	LPDISPATCH pDispatch;
	GetProperty(0x6, VT_DISPATCH, (void*)&pDispatch);
	return CMoPoint(pDispatch);
}

void CMoEllipse::SetCenter(LPDISPATCH propVal)
{
	SetProperty(0x6, VT_DISPATCH, propVal);
}

double CMoEllipse::GetWidth()
{
	double result;
	GetProperty(0x7, VT_R8, (void*)&result);
	return result;
}

void CMoEllipse::SetWidth(double propVal)
{
	SetProperty(0x7, VT_R8, propVal);
}

double CMoEllipse::GetHeight()
{
	double result;
	GetProperty(0x8, VT_R8, (void*)&result);
	return result;
}

void CMoEllipse::SetHeight(double propVal)
{
	SetProperty(0x8, VT_R8, propVal);
}

long CMoEllipse::GetShapeType()
{
	long result;
	GetProperty(0x9, VT_I4, (void*)&result);
	return result;
}

void CMoEllipse::SetShapeType(long propVal)
{
	SetProperty(0x9, VT_I4, propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoEllipse 

BOOL CMoEllipse::IsPointIn(LPDISPATCH Point)
{
	BOOL result;
	static BYTE parms[] =
		VTS_DISPATCH;
	InvokeHelper(0xa, DISPATCH_METHOD, VT_BOOL, (void*)&result, parms,
		Point);
	return result;
}

void CMoEllipse::Offset(double deltaX, double deltaY)
{
	static BYTE parms[] =
		VTS_R8 VTS_R8;
	InvokeHelper(0xb, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 deltaX, deltaY);
}

void CMoEllipse::Inset(double deltaX, double deltaY)
{
	static BYTE parms[] =
		VTS_R8 VTS_R8;
	InvokeHelper(0xc, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 deltaX, deltaY);
}

LPDISPATCH CMoEllipse::Union(LPDISPATCH anotherShape, const VARIANT& Extent)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_DISPATCH VTS_VARIANT;
	InvokeHelper(0xd, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		anotherShape, &Extent);
	return result;
}

LPDISPATCH CMoEllipse::Xor(LPDISPATCH anotherShape, const VARIANT& Extent)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_DISPATCH VTS_VARIANT;
	InvokeHelper(0xe, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		anotherShape, &Extent);
	return result;
}

LPDISPATCH CMoEllipse::Difference(LPDISPATCH anotherShape, const VARIANT& Extent)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_DISPATCH VTS_VARIANT;
	InvokeHelper(0xf, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		anotherShape, &Extent);
	return result;
}

LPDISPATCH CMoEllipse::Intersect(LPDISPATCH anotherShape, const VARIANT& Extent)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_DISPATCH VTS_VARIANT;
	InvokeHelper(0x10, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		anotherShape, &Extent);
	return result;
}

LPDISPATCH CMoEllipse::Buffer(double distance, const VARIANT& Extent)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_R8 VTS_VARIANT;
	InvokeHelper(0x11, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		distance, &Extent);
	return result;
}
