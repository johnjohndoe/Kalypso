// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "movaluemaprenderer.h"

// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
#include "MoSymbol.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoValueMapRenderer 

CString CMoValueMapRenderer::GetField()
{
	CString result;
	GetProperty(0x1, VT_BSTR, (void*)&result);
	return result;
}

void CMoValueMapRenderer::SetField(LPCTSTR propVal)
{
	SetProperty(0x1, VT_BSTR, propVal);
}

CMoSymbol CMoValueMapRenderer::GetDefaultSymbol()
{
	LPDISPATCH pDispatch;
	GetProperty(0x6, VT_DISPATCH, (void*)&pDispatch);
	return CMoSymbol(pDispatch);
}

void CMoValueMapRenderer::SetDefaultSymbol(LPDISPATCH propVal)
{
	SetProperty(0x6, VT_DISPATCH, propVal);
}

BOOL CMoValueMapRenderer::GetUseDefault()
{
	BOOL result;
	GetProperty(0x2, VT_BOOL, (void*)&result);
	return result;
}

void CMoValueMapRenderer::SetUseDefault(BOOL propVal)
{
	SetProperty(0x2, VT_BOOL, propVal);
}

short CMoValueMapRenderer::GetValueCount()
{
	short result;
	GetProperty(0x7, VT_I2, (void*)&result);
	return result;
}

void CMoValueMapRenderer::SetValueCount(short propVal)
{
	SetProperty(0x7, VT_I2, propVal);
}

CString CMoValueMapRenderer::GetTag()
{
	CString result;
	GetProperty(0x3, VT_BSTR, (void*)&result);
	return result;
}

void CMoValueMapRenderer::SetTag(LPCTSTR propVal)
{
	SetProperty(0x3, VT_BSTR, propVal);
}

long CMoValueMapRenderer::GetSymbolType()
{
	long result;
	GetProperty(0x8, VT_I4, (void*)&result);
	return result;
}

void CMoValueMapRenderer::SetSymbolType(long propVal)
{
	SetProperty(0x8, VT_I4, propVal);
}

CString CMoValueMapRenderer::GetRotationField()
{
	CString result;
	GetProperty(0x4, VT_BSTR, (void*)&result);
	return result;
}

void CMoValueMapRenderer::SetRotationField(LPCTSTR propVal)
{
	SetProperty(0x4, VT_BSTR, propVal);
}

CString CMoValueMapRenderer::GetScalingField()
{
	CString result;
	GetProperty(0x5, VT_BSTR, (void*)&result);
	return result;
}

void CMoValueMapRenderer::SetScalingField(LPCTSTR propVal)
{
	SetProperty(0x5, VT_BSTR, propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoValueMapRenderer 

CString CMoValueMapRenderer::GetValue(short index)
{
	CString result;
	static BYTE parms[] =
		VTS_I2;
	InvokeHelper(0x9, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, parms,
		index);
	return result;
}

void CMoValueMapRenderer::SetValue(short index, LPCTSTR lpszNewValue)
{
	static BYTE parms[] =
		VTS_I2 VTS_BSTR;
	InvokeHelper(0x9, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 index, lpszNewValue);
}

CMoSymbol CMoValueMapRenderer::GetSymbol(short index)
{
	LPDISPATCH pDispatch;
	static BYTE parms[] =
		VTS_I2;
	InvokeHelper(0xa, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&pDispatch, parms,
		index);
	return CMoSymbol(pDispatch);
}
