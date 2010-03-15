// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "molegend.h"

// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
#include "OleFont.h"

/////////////////////////////////////////////////////////////////////////////
// CMoLegend

IMPLEMENT_DYNCREATE(CMoLegend, CWnd)

/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoLegend

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoLegend

CString CMoLegend::GetEntryName(short* Index)
{
	CString result;
	static BYTE parms[] =
		VTS_PI2;
	InvokeHelper(0x6003000a, DISPATCH_METHOD, VT_BSTR, (void*)&result, parms,
		Index);
	return result;
}

unsigned long CMoLegend::GetBackColor()
{
	unsigned long result;
	InvokeHelper(0x68030009, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}

void CMoLegend::SetBackColor(unsigned long newValue)
{
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x68030009, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 newValue);
}

unsigned long CMoLegend::GetForeColor()
{
	unsigned long result;
	InvokeHelper(0x68030008, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}

void CMoLegend::SetForeColor(unsigned long newValue)
{
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x68030008, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 newValue);
}

BOOL CMoLegend::GetEnabled()
{
	BOOL result;
	InvokeHelper(0x68030007, DISPATCH_PROPERTYGET, VT_BOOL, (void*)&result, NULL);
	return result;
}

void CMoLegend::SetEnabled(BOOL bNewValue)
{
	static BYTE parms[] =
		VTS_BOOL;
	InvokeHelper(0x68030007, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 bNewValue);
}

BOOL CMoLegend::GetEnableDragDrop()
{
	BOOL result;
	InvokeHelper(0x68030006, DISPATCH_PROPERTYGET, VT_BOOL, (void*)&result, NULL);
	return result;
}

void CMoLegend::SetEnableDragDrop(BOOL bNewValue)
{
	static BYTE parms[] =
		VTS_BOOL;
	InvokeHelper(0x68030006, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 bNewValue);
}

COleFont CMoLegend::GetFont()
{
	LPDISPATCH pDispatch;
	InvokeHelper(DISPID_FONT, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&pDispatch, NULL);
	return COleFont(pDispatch);
}

void CMoLegend::SetRefFont(LPDISPATCH newValue)
{
	static BYTE parms[] =
		VTS_DISPATCH;
	InvokeHelper(DISPID_FONT, DISPATCH_PROPERTYPUTREF, VT_EMPTY, NULL, parms,
		 newValue);
}

short CMoLegend::GetBackStyle()
{
	short result;
	InvokeHelper(0x68030005, DISPATCH_PROPERTYGET, VT_I2, (void*)&result, NULL);
	return result;
}

void CMoLegend::SetBackStyle(short nNewValue)
{
	static BYTE parms[] =
		VTS_I2;
	InvokeHelper(0x68030005, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 nNewValue);
}

short CMoLegend::GetBorderStyle()
{
	short result;
	InvokeHelper(0x68030004, DISPATCH_PROPERTYGET, VT_I2, (void*)&result, NULL);
	return result;
}

void CMoLegend::SetBorderStyle(short nNewValue)
{
	static BYTE parms[] =
		VTS_I2;
	InvokeHelper(0x68030004, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 nNewValue);
}

void CMoLegend::LoadLegend(BOOL* ShowCheck)
{
	static BYTE parms[] =
		VTS_PBOOL;
	InvokeHelper(0x6003001d, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 ShowCheck);
}

void CMoLegend::RemoveAll()
{
	InvokeHelper(0x60030021, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

BOOL CMoLegend::GetActive(short* Index)
{
	BOOL result;
	static BYTE parms[] =
		VTS_PI2;
	InvokeHelper(0x68030002, DISPATCH_PROPERTYGET, VT_BOOL, (void*)&result, parms,
		Index);
	return result;
}

void CMoLegend::SetActive(short* Index, BOOL bNewValue)
{
	static BYTE parms[] =
		VTS_PI2 VTS_BOOL;
	InvokeHelper(0x68030002, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 Index, bNewValue);
}

short CMoLegend::getActiveLayer()
{
	short result;
	InvokeHelper(0x6003002f, DISPATCH_METHOD, VT_I2, (void*)&result, NULL);
	return result;
}

void CMoLegend::setMapSource(LPDISPATCH* map)
{
	static BYTE parms[] =
		VTS_PDISPATCH;
	InvokeHelper(0x60030030, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 map);
}

BOOL CMoLegend::GetLayerVisible(short* Index)
{
	BOOL result;
	static BYTE parms[] =
		VTS_PI2;
	InvokeHelper(0x68030001, DISPATCH_PROPERTYGET, VT_BOOL, (void*)&result, parms,
		Index);
	return result;
}

void CMoLegend::SetLayerVisible(short* Index, BOOL bNewValue)
{
	static BYTE parms[] =
		VTS_PI2 VTS_BOOL;
	InvokeHelper(0x68030001, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 Index, bNewValue);
}

BOOL CMoLegend::GetShowLegend(short* lyrIndex)
{
	BOOL result;
	static BYTE parms[] =
		VTS_PI2;
	InvokeHelper(0x68030000, DISPATCH_PROPERTYGET, VT_BOOL, (void*)&result, parms,
		lyrIndex);
	return result;
}

void CMoLegend::SetShowLegend(short* lyrIndex, BOOL bNewValue)
{
	static BYTE parms[] =
		VTS_PI2 VTS_BOOL;
	InvokeHelper(0x68030000, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 lyrIndex, bNewValue);
}

void CMoLegend::ShowAllLegend()
{
	InvokeHelper(0x60030032, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void CMoLegend::HideAllLegend()
{
	InvokeHelper(0x60030033, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

BOOL CMoLegend::ExportToBmp(BSTR* FileName, short* LayerIndex)
{
	BOOL result;
	static BYTE parms[] =
		VTS_PBSTR VTS_PI2;
	InvokeHelper(0x6003003c, DISPATCH_METHOD, VT_BOOL, (void*)&result, parms,
		FileName, LayerIndex);
	return result;
}
