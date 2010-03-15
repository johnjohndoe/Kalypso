// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "moclassbreaksrenderer.h"

// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
#include "mosymbol.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoClassBreaksRenderer 

CString CMoClassBreaksRenderer::GetField()
{
	CString result;
	GetProperty(0x1, VT_BSTR, (void*)&result);
	return result;
}

void CMoClassBreaksRenderer::SetField(LPCTSTR propVal)
{
	SetProperty(0x1, VT_BSTR, propVal);
}

short CMoClassBreaksRenderer::GetBreakCount()
{
	short result;
	GetProperty(0x3, VT_I2, (void*)&result);
	return result;
}

void CMoClassBreaksRenderer::SetBreakCount(short propVal)
{
	SetProperty(0x3, VT_I2, propVal);
}

CString CMoClassBreaksRenderer::GetTag()
{
	CString result;
	GetProperty(0x2, VT_BSTR, (void*)&result);
	return result;
}

void CMoClassBreaksRenderer::SetTag(LPCTSTR propVal)
{
	SetProperty(0x2, VT_BSTR, propVal);
}

long CMoClassBreaksRenderer::GetSymbolType()
{
	long result;
	GetProperty(0x4, VT_I4, (void*)&result);
	return result;
}

void CMoClassBreaksRenderer::SetSymbolType(long propVal)
{
	SetProperty(0x4, VT_I4, propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoClassBreaksRenderer 

double CMoClassBreaksRenderer::GetBreak(short index)
{
	double result;
	static BYTE parms[] =
		VTS_I2;
	InvokeHelper(0x7, DISPATCH_PROPERTYGET, VT_R8, (void*)&result, parms,
		index);
	return result;
}

void CMoClassBreaksRenderer::SetBreak(short index, double newValue)
{
	static BYTE parms[] =
		VTS_I2 VTS_R8;
	InvokeHelper(0x7, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 index, newValue);
}

CMoSymbol CMoClassBreaksRenderer::GetSymbol(short index)
{
	LPDISPATCH pDispatch;
	static BYTE parms[] =
		VTS_I2;
	InvokeHelper(0x8, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&pDispatch, parms,
		index);
	return CMoSymbol(pDispatch);
}

void CMoClassBreaksRenderer::SetRefSymbol(short index, LPDISPATCH newValue)
{
	static BYTE parms[] =
		VTS_I2 VTS_DISPATCH;
	InvokeHelper(0x8, DISPATCH_PROPERTYPUTREF, VT_EMPTY, NULL, parms,
		 index, newValue);
}

void CMoClassBreaksRenderer::RampColors(unsigned long startColor, unsigned long endColor)
{
	static BYTE parms[] =
		VTS_I4 VTS_I4;
	InvokeHelper(0x5, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 startColor, endColor);
}

void CMoClassBreaksRenderer::SizeSymbols(short startSize, short endSize)
{
	static BYTE parms[] =
		VTS_I2 VTS_I2;
	InvokeHelper(0x6, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 startSize, endSize);
}
