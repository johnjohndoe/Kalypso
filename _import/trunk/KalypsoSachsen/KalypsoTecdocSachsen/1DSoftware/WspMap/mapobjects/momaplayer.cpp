// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "momaplayer.h"

// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
#include "MoRectangle.h"
#include "MoRecordset.h"
#include "MoSymbol.h"
#include "MoGeoDataset.h"
#include "MoStrings.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoMapLayer 

CString CMoMapLayer::GetName()
{
	CString result;
	GetProperty(0x1, VT_BSTR, (void*)&result);
	return result;
}

void CMoMapLayer::SetName(LPCTSTR propVal)
{
	SetProperty(0x1, VT_BSTR, propVal);
}

CMoRectangle CMoMapLayer::GetExtent()
{
	LPDISPATCH pDispatch;
	GetProperty(0x4, VT_DISPATCH, (void*)&pDispatch);
	return CMoRectangle(pDispatch);
}

void CMoMapLayer::SetExtent(LPDISPATCH propVal)
{
	SetProperty(0x4, VT_DISPATCH, propVal);
}

BOOL CMoMapLayer::GetVisible()
{
	BOOL result;
	GetProperty(0x3, VT_BOOL, (void*)&result);
	return result;
}

void CMoMapLayer::SetVisible(BOOL propVal)
{
	SetProperty(0x3, VT_BOOL, propVal);
}

CMoRecordset CMoMapLayer::GetRecords()
{
	LPDISPATCH pDispatch;
	GetProperty(0x5, VT_DISPATCH, (void*)&pDispatch);
	return CMoRecordset(pDispatch);
}

void CMoMapLayer::SetRecords(LPDISPATCH propVal)
{
	SetProperty(0x5, VT_DISPATCH, propVal);
}

CMoSymbol CMoMapLayer::GetSymbol()
{
	LPDISPATCH pDispatch;
	GetProperty(0x6, VT_DISPATCH, (void*)&pDispatch);
	return CMoSymbol(pDispatch);
}

void CMoMapLayer::SetSymbol(LPDISPATCH propVal)
{
	SetProperty(0x6, VT_DISPATCH, propVal);
}

CMoGeoDataset CMoMapLayer::GetGeoDataset()
{
	LPDISPATCH pDispatch;
	GetProperty(0x7, VT_DISPATCH, (void*)&pDispatch);
	return CMoGeoDataset(pDispatch);
}

void CMoMapLayer::SetGeoDataset(LPDISPATCH propVal)
{
	SetProperty(0x7, VT_DISPATCH, propVal);
}

long CMoMapLayer::GetLayerType()
{
	long result;
	GetProperty(0x8, VT_I4, (void*)&result);
	return result;
}

void CMoMapLayer::SetLayerType(long propVal)
{
	SetProperty(0x8, VT_I4, propVal);
}

CMoRectangle CMoMapLayer::GetAreaOfInterest()
{
	LPDISPATCH pDispatch;
	GetProperty(0x9, VT_DISPATCH, (void*)&pDispatch);
	return CMoRectangle(pDispatch);
}

void CMoMapLayer::SetAreaOfInterest(LPDISPATCH propVal)
{
	SetProperty(0x9, VT_DISPATCH, propVal);
}

LPDISPATCH CMoMapLayer::GetRenderer()
{
	LPDISPATCH result;
	GetProperty(0xa, VT_DISPATCH, (void*)&result);
	return result;
}

void CMoMapLayer::SetRenderer(LPDISPATCH propVal)
{
	SetProperty(0xa, VT_DISPATCH, propVal);
}

CString CMoMapLayer::GetTag()
{
	CString result;
	GetProperty(0x2, VT_BSTR, (void*)&result);
	return result;
}

void CMoMapLayer::SetTag(LPCTSTR propVal)
{
	SetProperty(0x2, VT_BSTR, propVal);
}

long CMoMapLayer::GetShapeType()
{
	long result;
	GetProperty(0xb, VT_I4, (void*)&result);
	return result;
}

void CMoMapLayer::SetShapeType(long propVal)
{
	SetProperty(0xb, VT_I4, propVal);
}

BOOL CMoMapLayer::GetValid()
{
	BOOL result;
	GetProperty(0xc, VT_BOOL, (void*)&result);
	return result;
}

void CMoMapLayer::SetValid(BOOL propVal)
{
	SetProperty(0xc, VT_BOOL, propVal);
}

BOOL CMoMapLayer::GetIndexed()
{
	BOOL result;
	GetProperty(0xd, VT_BOOL, (void*)&result);
	return result;
}

void CMoMapLayer::SetIndexed(BOOL propVal)
{
	SetProperty(0xd, VT_BOOL, propVal);
}

VARIANT CMoMapLayer::GetCoordinateSystem()
{
	VARIANT result;
	GetProperty(0xe, VT_VARIANT, (void*)&result);
	return result;
}

void CMoMapLayer::SetCoordinateSystem(const VARIANT& propVal)
{
	SetProperty(0xe, VT_VARIANT, &propVal);
}

VARIANT CMoMapLayer::GetGeographicTransformation()
{
	VARIANT result;
	GetProperty(0xf, VT_VARIANT, (void*)&result);
	return result;
}

void CMoMapLayer::SetGeographicTransformation(const VARIANT& propVal)
{
	SetProperty(0xf, VT_VARIANT, &propVal);
}

double CMoMapLayer::GetDensificationTolerance()
{
	double result;
	GetProperty(0x10, VT_R8, (void*)&result);
	return result;
}

void CMoMapLayer::SetDensificationTolerance(double propVal)
{
	SetProperty(0x10, VT_R8, propVal);
}

VARIANT CMoMapLayer::GetFilterShape()
{
	VARIANT result;
	GetProperty(0x11, VT_VARIANT, (void*)&result);
	return result;
}

void CMoMapLayer::SetFilterShape(const VARIANT& propVal)
{
	SetProperty(0x11, VT_VARIANT, &propVal);
}

long CMoMapLayer::GetFilterOperator()
{
	long result;
	GetProperty(0x12, VT_I4, (void*)&result);
	return result;
}

void CMoMapLayer::SetFilterOperator(long propVal)
{
	SetProperty(0x12, VT_I4, propVal);
}

CString CMoMapLayer::GetFilterExpression()
{
	CString result;
	GetProperty(0x13, VT_BSTR, (void*)&result);
	return result;
}

void CMoMapLayer::SetFilterExpression(LPCTSTR propVal)
{
	SetProperty(0x13, VT_BSTR, propVal);
}

CMoStrings CMoMapLayer::GetFilterFields()
{
	LPDISPATCH pDispatch;
	GetProperty(0x14, VT_DISPATCH, (void*)&pDispatch);
	return CMoStrings(pDispatch);
}

void CMoMapLayer::SetFilterFields(LPDISPATCH propVal)
{
	SetProperty(0x14, VT_DISPATCH, propVal);
}

long CMoMapLayer::GetFilterOrder()
{
	long result;
	GetProperty(0x15, VT_I4, (void*)&result);
	return result;
}

void CMoMapLayer::SetFilterOrder(long propVal)
{
	SetProperty(0x15, VT_I4, propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoMapLayer 

CMoRecordset CMoMapLayer::SearchExpression(LPCTSTR expression)
{
	LPDISPATCH pDispatch;
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x16, DISPATCH_METHOD, VT_DISPATCH, (void*)&pDispatch, parms,
		expression);
	return CMoRecordset(pDispatch);
}

BOOL CMoMapLayer::AddRelate(LPCTSTR toField, LPDISPATCH Table, LPCTSTR fromField, const VARIANT& CheckFields)
{
	BOOL result;
	static BYTE parms[] =
		VTS_BSTR VTS_DISPATCH VTS_BSTR VTS_VARIANT;
	InvokeHelper(0x17, DISPATCH_METHOD, VT_BOOL, (void*)&result, parms,
		toField, Table, fromField, &CheckFields);
	return result;
}

void CMoMapLayer::RemoveRelates()
{
	InvokeHelper(0x18, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

CMoRecordset CMoMapLayer::SearchByDistance(LPDISPATCH shape, double distance, LPCTSTR expression)
{
	LPDISPATCH pDispatch;
	static BYTE parms[] =
		VTS_DISPATCH VTS_R8 VTS_BSTR;
	InvokeHelper(0x19, DISPATCH_METHOD, VT_DISPATCH, (void*)&pDispatch, parms,
		shape, distance, expression);
	return CMoRecordset(pDispatch);
}

CMoRecordset CMoMapLayer::SearchShape(LPDISPATCH shape, long searchMethod, LPCTSTR expression)
{
	LPDISPATCH pDispatch;
	static BYTE parms[] =
		VTS_DISPATCH VTS_I4 VTS_BSTR;
	InvokeHelper(0x1a, DISPATCH_METHOD, VT_DISPATCH, (void*)&pDispatch, parms,
		shape, searchMethod, expression);
	return CMoRecordset(pDispatch);
}

BOOL CMoMapLayer::BuildIndex(BOOL force)
{
	BOOL result;
	static BYTE parms[] =
		VTS_BOOL;
	InvokeHelper(0x1b, DISPATCH_METHOD, VT_BOOL, (void*)&result, parms,
		force);
	return result;
}
