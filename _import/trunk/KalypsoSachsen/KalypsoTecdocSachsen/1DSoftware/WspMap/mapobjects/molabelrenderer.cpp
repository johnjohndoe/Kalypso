// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "molabelrenderer.h"

// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
#include "MoTextSymbol.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoLabelRenderer 

CString CMoLabelRenderer::GetField()
{
	CString result;
	GetProperty(0x1, VT_BSTR, (void*)&result);
	return result;
}

void CMoLabelRenderer::SetField(LPCTSTR propVal)
{
	SetProperty(0x1, VT_BSTR, propVal);
}

BOOL CMoLabelRenderer::GetDrawBackground()
{
	BOOL result;
	GetProperty(0x2, VT_BOOL, (void*)&result);
	return result;
}

void CMoLabelRenderer::SetDrawBackground(BOOL propVal)
{
	SetProperty(0x2, VT_BOOL, propVal);
}

short CMoLabelRenderer::GetSymbolCount()
{
	short result;
	GetProperty(0x10, VT_I2, (void*)&result);
	return result;
}

void CMoLabelRenderer::SetSymbolCount(short propVal)
{
	SetProperty(0x10, VT_I2, propVal);
}

CString CMoLabelRenderer::GetSymbolField()
{
	CString result;
	GetProperty(0x3, VT_BSTR, (void*)&result);
	return result;
}

void CMoLabelRenderer::SetSymbolField(LPCTSTR propVal)
{
	SetProperty(0x3, VT_BSTR, propVal);
}

CString CMoLabelRenderer::GetLevelField()
{
	CString result;
	GetProperty(0x4, VT_BSTR, (void*)&result);
	return result;
}

void CMoLabelRenderer::SetLevelField(LPCTSTR propVal)
{
	SetProperty(0x4, VT_BSTR, propVal);
}

short CMoLabelRenderer::GetMinLevel()
{
	short result;
	GetProperty(0x5, VT_I2, (void*)&result);
	return result;
}

void CMoLabelRenderer::SetMinLevel(short propVal)
{
	SetProperty(0x5, VT_I2, propVal);
}

short CMoLabelRenderer::GetMaxLevel()
{
	short result;
	GetProperty(0x6, VT_I2, (void*)&result);
	return result;
}

void CMoLabelRenderer::SetMaxLevel(short propVal)
{
	SetProperty(0x6, VT_I2, propVal);
}

CString CMoLabelRenderer::GetRotationField()
{
	CString result;
	GetProperty(0x7, VT_BSTR, (void*)&result);
	return result;
}

void CMoLabelRenderer::SetRotationField(LPCTSTR propVal)
{
	SetProperty(0x7, VT_BSTR, propVal);
}

CString CMoLabelRenderer::GetHeightField()
{
	CString result;
	GetProperty(0x8, VT_BSTR, (void*)&result);
	return result;
}

void CMoLabelRenderer::SetHeightField(LPCTSTR propVal)
{
	SetProperty(0x8, VT_BSTR, propVal);
}

BOOL CMoLabelRenderer::GetSplinedText()
{
	BOOL result;
	GetProperty(0x9, VT_BOOL, (void*)&result);
	return result;
}

void CMoLabelRenderer::SetSplinedText(BOOL propVal)
{
	SetProperty(0x9, VT_BOOL, propVal);
}

BOOL CMoLabelRenderer::GetAllowDuplicates()
{
	BOOL result;
	GetProperty(0xa, VT_BOOL, (void*)&result);
	return result;
}

void CMoLabelRenderer::SetAllowDuplicates(BOOL propVal)
{
	SetProperty(0xa, VT_BOOL, propVal);
}

CString CMoLabelRenderer::GetTag()
{
	CString result;
	GetProperty(0xb, VT_BSTR, (void*)&result);
	return result;
}

void CMoLabelRenderer::SetTag(LPCTSTR propVal)
{
	SetProperty(0xb, VT_BSTR, propVal);
}

BOOL CMoLabelRenderer::GetFlip()
{
	BOOL result;
	GetProperty(0xc, VT_BOOL, (void*)&result);
	return result;
}

void CMoLabelRenderer::SetFlip(BOOL propVal)
{
	SetProperty(0xc, VT_BOOL, propVal);
}

CString CMoLabelRenderer::GetXOffsetField()
{
	CString result;
	GetProperty(0xd, VT_BSTR, (void*)&result);
	return result;
}

void CMoLabelRenderer::SetXOffsetField(LPCTSTR propVal)
{
	SetProperty(0xd, VT_BSTR, propVal);
}

CString CMoLabelRenderer::GetYOffsetField()
{
	CString result;
	GetProperty(0xe, VT_BSTR, (void*)&result);
	return result;
}

void CMoLabelRenderer::SetYOffsetField(LPCTSTR propVal)
{
	SetProperty(0xe, VT_BSTR, propVal);
}

CString CMoLabelRenderer::GetFittedField()
{
	CString result;
	GetProperty(0xf, VT_BSTR, (void*)&result);
	return result;
}

void CMoLabelRenderer::SetFittedField(LPCTSTR propVal)
{
	SetProperty(0xf, VT_BSTR, propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoLabelRenderer 

CMoTextSymbol CMoLabelRenderer::GetSymbol(short index)
{
	LPDISPATCH pDispatch;
	static BYTE parms[] =
		VTS_I2;
	InvokeHelper(0x11, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&pDispatch, parms,
		index);
	return CMoTextSymbol(pDispatch);
}
