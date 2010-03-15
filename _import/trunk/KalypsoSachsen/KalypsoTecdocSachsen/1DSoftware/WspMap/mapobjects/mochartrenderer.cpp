// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "mochartrenderer.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoChartRenderer 

CString CMoChartRenderer::GetSizeField()
{
	CString result;
	GetProperty(0x1, VT_BSTR, (void*)&result);
	return result;
}

void CMoChartRenderer::SetSizeField(LPCTSTR propVal)
{
	SetProperty(0x1, VT_BSTR, propVal);
}

short CMoChartRenderer::GetFieldCount()
{
	short result;
	GetProperty(0x6, VT_I2, (void*)&result);
	return result;
}

void CMoChartRenderer::SetFieldCount(short propVal)
{
	SetProperty(0x6, VT_I2, propVal);
}

double CMoChartRenderer::GetNullValue()
{
	double result;
	GetProperty(0x7, VT_R8, (void*)&result);
	return result;
}

void CMoChartRenderer::SetNullValue(double propVal)
{
	SetProperty(0x7, VT_R8, propVal);
}

long CMoChartRenderer::GetChartType()
{
	long result;
	GetProperty(0x8, VT_I4, (void*)&result);
	return result;
}

void CMoChartRenderer::SetChartType(long propVal)
{
	SetProperty(0x8, VT_I4, propVal);
}

short CMoChartRenderer::GetBarWidth()
{
	short result;
	GetProperty(0x9, VT_I2, (void*)&result);
	return result;
}

void CMoChartRenderer::SetBarWidth(short propVal)
{
	SetProperty(0x9, VT_I2, propVal);
}

short CMoChartRenderer::GetBarHeight()
{
	short result;
	GetProperty(0xa, VT_I2, (void*)&result);
	return result;
}

void CMoChartRenderer::SetBarHeight(short propVal)
{
	SetProperty(0xa, VT_I2, propVal);
}

CString CMoChartRenderer::GetNormalizationField()
{
	CString result;
	GetProperty(0x2, VT_BSTR, (void*)&result);
	return result;
}

void CMoChartRenderer::SetNormalizationField(LPCTSTR propVal)
{
	SetProperty(0x2, VT_BSTR, propVal);
}

short CMoChartRenderer::GetMinPieSize()
{
	short result;
	GetProperty(0x3, VT_I2, (void*)&result);
	return result;
}

void CMoChartRenderer::SetMinPieSize(short propVal)
{
	SetProperty(0x3, VT_I2, propVal);
}

short CMoChartRenderer::GetMaxPieSize()
{
	short result;
	GetProperty(0x4, VT_I2, (void*)&result);
	return result;
}

void CMoChartRenderer::SetMaxPieSize(short propVal)
{
	SetProperty(0x4, VT_I2, propVal);
}

BOOL CMoChartRenderer::GetShowOutline()
{
	BOOL result;
	GetProperty(0x5, VT_BOOL, (void*)&result);
	return result;
}

void CMoChartRenderer::SetShowOutline(BOOL propVal)
{
	SetProperty(0x5, VT_BOOL, propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoChartRenderer 

CString CMoChartRenderer::GetField(short index)
{
	CString result;
	static BYTE parms[] =
		VTS_I2;
	InvokeHelper(0xc, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, parms,
		index);
	return result;
}

void CMoChartRenderer::SetField(short index, LPCTSTR lpszNewValue)
{
	static BYTE parms[] =
		VTS_I2 VTS_BSTR;
	InvokeHelper(0xc, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 index, lpszNewValue);
}

unsigned long CMoChartRenderer::GetColor(short index)
{
	unsigned long result;
	static BYTE parms[] =
		VTS_I2;
	InvokeHelper(0xd, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, parms,
		index);
	return result;
}

void CMoChartRenderer::SetColor(short index, unsigned long newValue)
{
	static BYTE parms[] =
		VTS_I2 VTS_I4;
	InvokeHelper(0xd, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 index, newValue);
}

void CMoChartRenderer::NoNullValue()
{
	InvokeHelper(0xb, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}
