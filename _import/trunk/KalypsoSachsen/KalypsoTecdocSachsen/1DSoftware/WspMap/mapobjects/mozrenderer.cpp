// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "mozrenderer.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoZRenderer 

CString CMoZRenderer::GetTag()
{
	CString result;
	GetProperty(0x1, VT_BSTR, (void*)&result);
	return result;
}

void CMoZRenderer::SetTag(LPCTSTR propVal)
{
	SetProperty(0x1, VT_BSTR, propVal);
}

long CMoZRenderer::GetSymbolType()
{
	long result;
	GetProperty(0x2, VT_I4, (void*)&result);
	return result;
}

void CMoZRenderer::SetSymbolType(long propVal)
{
	SetProperty(0x2, VT_I4, propVal);
}

short CMoZRenderer::GetBreakCount()
{
	short result;
	GetProperty(0x3, VT_I2, (void*)&result);
	return result;
}

void CMoZRenderer::SetBreakCount(short propVal)
{
	SetProperty(0x3, VT_I2, propVal);
}

long CMoZRenderer::GetValueCalculation()
{
	long result;
	GetProperty(0x4, VT_I4, (void*)&result);
	return result;
}

void CMoZRenderer::SetValueCalculation(long propVal)
{
	SetProperty(0x4, VT_I4, propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoZRenderer 

double CMoZRenderer::GetBreak(short index)
{
	double result;
	static BYTE parms[] =
		VTS_I2;
	InvokeHelper(0x6, DISPATCH_PROPERTYGET, VT_R8, (void*)&result, parms,
		index);
	return result;
}

void CMoZRenderer::SetBreak(short index, double newValue)
{
	static BYTE parms[] =
		VTS_I2 VTS_R8;
	InvokeHelper(0x6, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 index, newValue);
}

LPDISPATCH CMoZRenderer::GetSymbol(short index)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_I2;
	InvokeHelper(0x7, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&result, parms,
		index);
	return result;
}

void CMoZRenderer::RampColors(unsigned long startColor, unsigned long endColor)
{
	static BYTE parms[] =
		VTS_I4 VTS_I4;
	InvokeHelper(0x5, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 startColor, endColor);
}
