// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "moeventrenderer.h"

// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
#include "MoSymbol.h"
#include "motable.h"
#include "MoRectangle.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoEventRenderer 

CString CMoEventRenderer::GetTag()
{
	CString result;
	GetProperty(0x1, VT_BSTR, (void*)&result);
	return result;
}

void CMoEventRenderer::SetTag(LPCTSTR propVal)
{
	SetProperty(0x1, VT_BSTR, propVal);
}

BOOL CMoEventRenderer::GetUseDefault()
{
	BOOL result;
	GetProperty(0x2, VT_BOOL, (void*)&result);
	return result;
}

void CMoEventRenderer::SetUseDefault(BOOL propVal)
{
	SetProperty(0x2, VT_BOOL, propVal);
}

CMoSymbol CMoEventRenderer::GetDefaultSymbol()
{
	LPDISPATCH pDispatch;
	GetProperty(0x9, VT_DISPATCH, (void*)&pDispatch);
	return CMoSymbol(pDispatch);
}

void CMoEventRenderer::SetDefaultSymbol(LPDISPATCH propVal)
{
	SetProperty(0x9, VT_DISPATCH, propVal);
}

long CMoEventRenderer::GetSymbolType()
{
	long result;
	GetProperty(0xa, VT_I4, (void*)&result);
	return result;
}

void CMoEventRenderer::SetSymbolType(long propVal)
{
	SetProperty(0xa, VT_I4, propVal);
}

short CMoEventRenderer::GetValueCount()
{
	short result;
	GetProperty(0xb, VT_I2, (void*)&result);
	return result;
}

void CMoEventRenderer::SetValueCount(short propVal)
{
	SetProperty(0xb, VT_I2, propVal);
}

CString CMoEventRenderer::GetFeatureRouteIDField()
{
	CString result;
	GetProperty(0x3, VT_BSTR, (void*)&result);
	return result;
}

void CMoEventRenderer::SetFeatureRouteIDField(LPCTSTR propVal)
{
	SetProperty(0x3, VT_BSTR, propVal);
}

CString CMoEventRenderer::GetEventRouteIDField()
{
	CString result;
	GetProperty(0x4, VT_BSTR, (void*)&result);
	return result;
}

void CMoEventRenderer::SetEventRouteIDField(LPCTSTR propVal)
{
	SetProperty(0x4, VT_BSTR, propVal);
}

CString CMoEventRenderer::GetStartMeasureField()
{
	CString result;
	GetProperty(0x5, VT_BSTR, (void*)&result);
	return result;
}

void CMoEventRenderer::SetStartMeasureField(LPCTSTR propVal)
{
	SetProperty(0x5, VT_BSTR, propVal);
}

CString CMoEventRenderer::GetEndMeasureField()
{
	CString result;
	GetProperty(0x6, VT_BSTR, (void*)&result);
	return result;
}

void CMoEventRenderer::SetEndMeasureField(LPCTSTR propVal)
{
	SetProperty(0x6, VT_BSTR, propVal);
}

CString CMoEventRenderer::GetSymbolField()
{
	CString result;
	GetProperty(0x7, VT_BSTR, (void*)&result);
	return result;
}

void CMoEventRenderer::SetSymbolField(LPCTSTR propVal)
{
	SetProperty(0x7, VT_BSTR, propVal);
}

BOOL CMoEventRenderer::GetDrawBackground()
{
	BOOL result;
	GetProperty(0x8, VT_BOOL, (void*)&result);
	return result;
}

void CMoEventRenderer::SetDrawBackground(BOOL propVal)
{
	SetProperty(0x8, VT_BOOL, propVal);
}

CMoTable CMoEventRenderer::GetEventTable()
{
	LPDISPATCH pDispatch;
	GetProperty(0xc, VT_DISPATCH, (void*)&pDispatch);
	return CMoTable(pDispatch);
}

void CMoEventRenderer::SetEventTable(LPDISPATCH propVal)
{
	SetProperty(0xc, VT_DISPATCH, propVal);
}

BOOL CMoEventRenderer::GetIndexEvents()
{
	BOOL result;
	GetProperty(0xd, VT_BOOL, (void*)&result);
	return result;
}

void CMoEventRenderer::SetIndexEvents(BOOL propVal)
{
	SetProperty(0xd, VT_BOOL, propVal);
}

CMoRectangle CMoEventRenderer::GetIndexExtent()
{
	LPDISPATCH pDispatch;
	GetProperty(0xe, VT_DISPATCH, (void*)&pDispatch);
	return CMoRectangle(pDispatch);
}

void CMoEventRenderer::SetIndexExtent(LPDISPATCH propVal)
{
	SetProperty(0xe, VT_DISPATCH, propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoEventRenderer 

CMoSymbol CMoEventRenderer::GetSymbol(short index)
{
	LPDISPATCH pDispatch;
	static BYTE parms[] =
		VTS_I2;
	InvokeHelper(0x10, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&pDispatch, parms,
		index);
	return CMoSymbol(pDispatch);
}

CString CMoEventRenderer::GetValue(short index)
{
	CString result;
	static BYTE parms[] =
		VTS_I2;
	InvokeHelper(0x11, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, parms,
		index);
	return result;
}

void CMoEventRenderer::SetValue(short index, LPCTSTR lpszNewValue)
{
	static BYTE parms[] =
		VTS_I2 VTS_BSTR;
	InvokeHelper(0x11, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 index, lpszNewValue);
}

BOOL CMoEventRenderer::InvalidateIndex(LPCTSTR key)
{
	BOOL result;
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0xf, DISPATCH_METHOD, VT_BOOL, (void*)&result, parms,
		key);
	return result;
}
