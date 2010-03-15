// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "moimagelayer.h"

// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
#include "MoRectangle.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoImageLayer 

BOOL CMoImageLayer::GetVisible()
{
	BOOL result;
	GetProperty(0x1, VT_BOOL, (void*)&result);
	return result;
}

void CMoImageLayer::SetVisible(BOOL propVal)
{
	SetProperty(0x1, VT_BOOL, propVal);
}

CString CMoImageLayer::GetName()
{
	CString result;
	GetProperty(0x2, VT_BSTR, (void*)&result);
	return result;
}

void CMoImageLayer::SetName(LPCTSTR propVal)
{
	SetProperty(0x2, VT_BSTR, propVal);
}

CMoRectangle CMoImageLayer::GetExtent()
{
	LPDISPATCH pDispatch;
	GetProperty(0x5, VT_DISPATCH, (void*)&pDispatch);
	return CMoRectangle(pDispatch);
}

void CMoImageLayer::SetExtent(LPDISPATCH propVal)
{
	SetProperty(0x5, VT_DISPATCH, propVal);
}

CString CMoImageLayer::GetFile()
{
	CString result;
	GetProperty(0x6, VT_BSTR, (void*)&result);
	return result;
}

void CMoImageLayer::SetFile(LPCTSTR propVal)
{
	SetProperty(0x6, VT_BSTR, propVal);
}

long CMoImageLayer::GetLayerType()
{
	long result;
	GetProperty(0x7, VT_I4, (void*)&result);
	return result;
}

void CMoImageLayer::SetLayerType(long propVal)
{
	SetProperty(0x7, VT_I4, propVal);
}

CString CMoImageLayer::GetTag()
{
	CString result;
	GetProperty(0x3, VT_BSTR, (void*)&result);
	return result;
}

void CMoImageLayer::SetTag(LPCTSTR propVal)
{
	SetProperty(0x3, VT_BSTR, propVal);
}

BOOL CMoImageLayer::GetValid()
{
	BOOL result;
	GetProperty(0x8, VT_BOOL, (void*)&result);
	return result;
}

void CMoImageLayer::SetValid(BOOL propVal)
{
	SetProperty(0x8, VT_BOOL, propVal);
}

BOOL CMoImageLayer::GetUpdateWhileDrawing()
{
	BOOL result;
	GetProperty(0x4, VT_BOOL, (void*)&result);
	return result;
}

void CMoImageLayer::SetUpdateWhileDrawing(BOOL propVal)
{
	SetProperty(0x4, VT_BOOL, propVal);
}

BOOL CMoImageLayer::GetTransparent()
{
	BOOL result;
	GetProperty(0x9, VT_BOOL, (void*)&result);
	return result;
}

void CMoImageLayer::SetTransparent(BOOL propVal)
{
	SetProperty(0x9, VT_BOOL, propVal);
}

unsigned long CMoImageLayer::GetTransparentColor()
{
	unsigned long result;
	GetProperty(0xa, VT_I4, (void*)&result);
	return result;
}

void CMoImageLayer::SetTransparentColor(unsigned long propVal)
{
	SetProperty(0xa, VT_I4, propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoImageLayer 
