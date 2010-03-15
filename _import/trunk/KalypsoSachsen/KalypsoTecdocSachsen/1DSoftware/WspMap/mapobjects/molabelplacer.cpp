// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "molabelplacer.h"

// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
#include "MoTextSymbol.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoLabelPlacer 

CString CMoLabelPlacer::GetField()
{
	CString result;
	GetProperty(0x1, VT_BSTR, (void*)&result);
	return result;
}

void CMoLabelPlacer::SetField(LPCTSTR propVal)
{
	SetProperty(0x1, VT_BSTR, propVal);
}

BOOL CMoLabelPlacer::GetDrawBackground()
{
	BOOL result;
	GetProperty(0x2, VT_BOOL, (void*)&result);
	return result;
}

void CMoLabelPlacer::SetDrawBackground(BOOL propVal)
{
	SetProperty(0x2, VT_BOOL, propVal);
}

BOOL CMoLabelPlacer::GetAllowDuplicates()
{
	BOOL result;
	GetProperty(0x3, VT_BOOL, (void*)&result);
	return result;
}

void CMoLabelPlacer::SetAllowDuplicates(BOOL propVal)
{
	SetProperty(0x3, VT_BOOL, propVal);
}

BOOL CMoLabelPlacer::GetPlaceAbove()
{
	BOOL result;
	GetProperty(0xa, VT_BOOL, (void*)&result);
	return result;
}

void CMoLabelPlacer::SetPlaceAbove(BOOL propVal)
{
	SetProperty(0xa, VT_BOOL, propVal);
}

BOOL CMoLabelPlacer::GetPlaceBelow()
{
	BOOL result;
	GetProperty(0xb, VT_BOOL, (void*)&result);
	return result;
}

void CMoLabelPlacer::SetPlaceBelow(BOOL propVal)
{
	SetProperty(0xb, VT_BOOL, propVal);
}

BOOL CMoLabelPlacer::GetPlaceOn()
{
	BOOL result;
	GetProperty(0xc, VT_BOOL, (void*)&result);
	return result;
}

void CMoLabelPlacer::SetPlaceOn(BOOL propVal)
{
	SetProperty(0xc, VT_BOOL, propVal);
}

CMoTextSymbol CMoLabelPlacer::GetDefaultSymbol()
{
	LPDISPATCH pDispatch;
	GetProperty(0xd, VT_DISPATCH, (void*)&pDispatch);
	return CMoTextSymbol(pDispatch);
}

void CMoLabelPlacer::SetDefaultSymbol(LPDISPATCH propVal)
{
	SetProperty(0xd, VT_DISPATCH, propVal);
}

BOOL CMoLabelPlacer::GetUseDefault()
{
	BOOL result;
	GetProperty(0x4, VT_BOOL, (void*)&result);
	return result;
}

void CMoLabelPlacer::SetUseDefault(BOOL propVal)
{
	SetProperty(0x4, VT_BOOL, propVal);
}

short CMoLabelPlacer::GetValueCount()
{
	short result;
	GetProperty(0xe, VT_I2, (void*)&result);
	return result;
}

void CMoLabelPlacer::SetValueCount(short propVal)
{
	SetProperty(0xe, VT_I2, propVal);
}

CString CMoLabelPlacer::GetValueField()
{
	CString result;
	GetProperty(0x5, VT_BSTR, (void*)&result);
	return result;
}

void CMoLabelPlacer::SetValueField(LPCTSTR propVal)
{
	SetProperty(0x5, VT_BSTR, propVal);
}

LPDISPATCH CMoLabelPlacer::GetBackgroundRenderer()
{
	LPDISPATCH result;
	GetProperty(0xf, VT_DISPATCH, (void*)&result);
	return result;
}

void CMoLabelPlacer::SetBackgroundRenderer(LPDISPATCH propVal)
{
	SetProperty(0xf, VT_DISPATCH, propVal);
}

BOOL CMoLabelPlacer::GetMaskLabels()
{
	BOOL result;
	GetProperty(0x6, VT_BOOL, (void*)&result);
	return result;
}

void CMoLabelPlacer::SetMaskLabels(BOOL propVal)
{
	SetProperty(0x6, VT_BOOL, propVal);
}

unsigned long CMoLabelPlacer::GetMaskColor()
{
	unsigned long result;
	GetProperty(0x7, VT_I4, (void*)&result);
	return result;
}

void CMoLabelPlacer::SetMaskColor(unsigned long propVal)
{
	SetProperty(0x7, VT_I4, propVal);
}

short CMoLabelPlacer::GetSymbolWidth()
{
	short result;
	GetProperty(0x8, VT_I2, (void*)&result);
	return result;
}

void CMoLabelPlacer::SetSymbolWidth(short propVal)
{
	SetProperty(0x8, VT_I2, propVal);
}

short CMoLabelPlacer::GetSymbolHeight()
{
	short result;
	GetProperty(0x9, VT_I2, (void*)&result);
	return result;
}

void CMoLabelPlacer::SetSymbolHeight(short propVal)
{
	SetProperty(0x9, VT_I2, propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoLabelPlacer 

CMoTextSymbol CMoLabelPlacer::GetSymbol(short index)
{
	LPDISPATCH pDispatch;
	static BYTE parms[] =
		VTS_I2;
	InvokeHelper(0x10, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&pDispatch, parms,
		index);
	return CMoTextSymbol(pDispatch);
}

CString CMoLabelPlacer::GetValue(short index)
{
	CString result;
	static BYTE parms[] =
		VTS_I2;
	InvokeHelper(0x11, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, parms,
		index);
	return result;
}

void CMoLabelPlacer::SetValue(short index, LPCTSTR lpszNewValue)
{
	static BYTE parms[] =
		VTS_I2 VTS_BSTR;
	InvokeHelper(0x11, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 index, lpszNewValue);
}
