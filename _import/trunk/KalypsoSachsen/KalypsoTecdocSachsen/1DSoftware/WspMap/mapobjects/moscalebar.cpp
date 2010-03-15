// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "moscalebar.h"

// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
#include "moPicture.h"
#include "mosbextent.h"

/////////////////////////////////////////////////////////////////////////////
// CMoScaleBar

IMPLEMENT_DYNCREATE(CMoScaleBar, CWnd)

/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoScaleBar 

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoScaleBar 

void CMoScaleBar::SetRefFont(LPDISPATCH newValue)
{
	static BYTE parms[] =
		VTS_DISPATCH;
	InvokeHelper(0x68030016, DISPATCH_PROPERTYPUTREF, VT_EMPTY, NULL, parms,
		 newValue);
}

void CMoScaleBar::SetRefPicture(LPDISPATCH newValue)
{
	static BYTE parms[] =
		VTS_DISPATCH;
	InvokeHelper(0x68030015, DISPATCH_PROPERTYPUTREF, VT_EMPTY, NULL, parms,
		 newValue);
}

CMoPicture CMoScaleBar::GetPicture()
{
	LPDISPATCH pDispatch;
	InvokeHelper(0x68030015, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&pDispatch, NULL);
	return CMoPicture(pDispatch);
}

void CMoScaleBar::SetScaleBarUnits(long nNewValue)
{
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x68030014, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 nNewValue);
}

void CMoScaleBar::SetScreenUnits(long nNewValue)
{
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x68030013, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 nNewValue);
}

long CMoScaleBar::GetScreenUnits()
{
	long result;
	InvokeHelper(0x68030013, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}

void CMoScaleBar::Refresh()
{
	InvokeHelper(0x6003001c, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

unsigned long CMoScaleBar::GetBarColor1()
{
	unsigned long result;
	InvokeHelper(0x68030012, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}

void CMoScaleBar::SetBarColor1(unsigned long newValue)
{
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x68030012, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 newValue);
}

unsigned long CMoScaleBar::GetBarColor2()
{
	unsigned long result;
	InvokeHelper(0x68030011, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}

void CMoScaleBar::SetBarColor2(unsigned long newValue)
{
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x68030011, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 newValue);
}

short CMoScaleBar::GetBarWidth()
{
	short result;
	InvokeHelper(0x68030010, DISPATCH_PROPERTYGET, VT_I2, (void*)&result, NULL);
	return result;
}

void CMoScaleBar::SetBarWidth(short nNewValue)
{
	static BYTE parms[] =
		VTS_I2;
	InvokeHelper(0x68030010, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 nNewValue);
}

long CMoScaleBar::GetMapUnits()
{
	long result;
	InvokeHelper(0x6803000f, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}

void CMoScaleBar::SetMapUnits(long nNewValue)
{
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x6803000f, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 nNewValue);
}

long CMoScaleBar::GetScaleBarUnits()
{
	long result;
	InvokeHelper(0x68030014, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}

long CMoScaleBar::GetScaleText()
{
	long result;
	InvokeHelper(0x6803000e, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}

void CMoScaleBar::SetScaleText(long nNewValue)
{
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x6803000e, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 nNewValue);
}

unsigned long CMoScaleBar::GetTextColor()
{
	unsigned long result;
	InvokeHelper(0x6803000d, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}

void CMoScaleBar::SetTextColor(unsigned long newValue)
{
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x6803000d, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 newValue);
}

unsigned long CMoScaleBar::GetBackColor()
{
	unsigned long result;
	InvokeHelper(0x6803000c, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}

void CMoScaleBar::SetBackColor(unsigned long newValue)
{
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x6803000c, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 newValue);
}

CMoSbExtent CMoScaleBar::GetMapExtent()
{
	LPDISPATCH pDispatch;
	InvokeHelper(0x6803000b, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&pDispatch, NULL);
	return CMoSbExtent(pDispatch);
}

void CMoScaleBar::SetMapExtent(LPDISPATCH newValue)
{
	static BYTE parms[] =
		VTS_DISPATCH;
	InvokeHelper(0x6803000b, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 newValue);
}

CMoSbExtent CMoScaleBar::GetPageExtent()
{
	LPDISPATCH pDispatch;
	InvokeHelper(0x6803000a, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&pDispatch, NULL);
	return CMoSbExtent(pDispatch);
}

void CMoScaleBar::SetPageExtent(LPDISPATCH newValue)
{
	static BYTE parms[] =
		VTS_DISPATCH;
	InvokeHelper(0x6803000a, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 newValue);
}

double CMoScaleBar::GetRFScale()
{
	double result;
	InvokeHelper(0x68030009, DISPATCH_PROPERTYGET, VT_R8, (void*)&result, NULL);
	return result;
}

LPDISPATCH CMoScaleBar::GetFont()
{
	LPDISPATCH result;
	InvokeHelper(0x68030016, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&result, NULL);
	return result;
}

long CMoScaleBar::GetHWnd()
{
	long result;
	InvokeHelper(0x68030008, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}

long CMoScaleBar::GetHDC()
{
	long result;
	InvokeHelper(0x68030007, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}

short CMoScaleBar::GetAppearance()
{
	short result;
	InvokeHelper(0x68030006, DISPATCH_PROPERTYGET, VT_I2, (void*)&result, NULL);
	return result;
}

long CMoScaleBar::GetBorderStyle()
{
	long result;
	InvokeHelper(0x68030005, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}

void CMoScaleBar::SetBorderStyle(long nNewValue)
{
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x68030005, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 nNewValue);
}

CMoPicture CMoScaleBar::GetMouseIcon()
{
	LPDISPATCH pDispatch;
	InvokeHelper(0x68030004, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&pDispatch, NULL);
	return CMoPicture(pDispatch);
}

void CMoScaleBar::SetRefMouseIcon(LPDISPATCH newValue)
{
	static BYTE parms[] =
		VTS_DISPATCH;
	InvokeHelper(0x68030004, DISPATCH_PROPERTYPUTREF, VT_EMPTY, NULL, parms,
		 newValue);
}

long CMoScaleBar::GetMousePointer()
{
	long result;
	InvokeHelper(0x68030003, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}

void CMoScaleBar::SetMousePointer(long nNewValue)
{
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x68030003, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 nNewValue);
}

void CMoScaleBar::PaintPicture(LPDISPATCH* Picture, float* X1, float* Y1, VARIANT* Width1, VARIANT* Height1, VARIANT* X2, VARIANT* Y2, VARIANT* Width2, VARIANT* Height2, VARIANT* Opcode)
{
	static BYTE parms[] =
		VTS_PDISPATCH VTS_PR4 VTS_PR4 VTS_PVARIANT VTS_PVARIANT VTS_PVARIANT VTS_PVARIANT VTS_PVARIANT VTS_PVARIANT VTS_PVARIANT;
	InvokeHelper(0x60030025, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 Picture, X1, Y1, Width1, Height1, X2, Y2, Width2, Height2, Opcode);
}

CString CMoScaleBar::GetToolTipText()
{
	CString result;
	InvokeHelper(0x68030002, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, NULL);
	return result;
}

void CMoScaleBar::SetToolTipText(LPCTSTR lpszNewValue)
{
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x68030002, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 lpszNewValue);
}

float CMoScaleBar::GetMinTicSpace()
{
	float result;
	InvokeHelper(0x68030001, DISPATCH_PROPERTYGET, VT_R4, (void*)&result, NULL);
	return result;
}

void CMoScaleBar::SetMinTicSpace(float newValue)
{
	static BYTE parms[] =
		VTS_R4;
	InvokeHelper(0x68030001, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 newValue);
}

BOOL CMoScaleBar::GetAdjustForLatitude()
{
	BOOL result;
	InvokeHelper(0x68030000, DISPATCH_PROPERTYGET, VT_BOOL, (void*)&result, NULL);
	return result;
}

void CMoScaleBar::SetAdjustForLatitude(BOOL bNewValue)
{
	static BYTE parms[] =
		VTS_BOOL;
	InvokeHelper(0x68030000, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 bNewValue);
}
