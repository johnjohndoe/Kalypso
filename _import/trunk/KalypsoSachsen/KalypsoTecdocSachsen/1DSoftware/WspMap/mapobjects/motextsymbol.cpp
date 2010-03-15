// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "motextsymbol.h"

// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
#include "olefont.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoTextSymbol 

long CMoTextSymbol::GetHorizontalAlignment()
{
	long result;
	GetProperty(0x5, VT_I4, (void*)&result);
	return result;
}

void CMoTextSymbol::SetHorizontalAlignment(long propVal)
{
	SetProperty(0x5, VT_I4, propVal);
}

long CMoTextSymbol::GetVerticalAlignment()
{
	long result;
	GetProperty(0x6, VT_I4, (void*)&result);
	return result;
}

void CMoTextSymbol::SetVerticalAlignment(long propVal)
{
	SetProperty(0x6, VT_I4, propVal);
}

unsigned long CMoTextSymbol::GetColor()
{
	unsigned long result;
	GetProperty(0x1, VT_I4, (void*)&result);
	return result;
}

void CMoTextSymbol::SetColor(unsigned long propVal)
{
	SetProperty(0x1, VT_I4, propVal);
}

COleFont CMoTextSymbol::GetFont()
{
	LPDISPATCH pDispatch;
	GetProperty(0x7, VT_DISPATCH, (void*)&pDispatch);
	return COleFont(pDispatch);
}

void CMoTextSymbol::SetFont(LPDISPATCH propVal)
{
	SetProperty(0x7, VT_DISPATCH, propVal);
}

double CMoTextSymbol::GetRotation()
{
	double result;
	GetProperty(0x2, VT_R8, (void*)&result);
	return result;
}

void CMoTextSymbol::SetRotation(double propVal)
{
	SetProperty(0x2, VT_R8, propVal);
}

double CMoTextSymbol::GetHeight()
{
	double result;
	GetProperty(0x3, VT_R8, (void*)&result);
	return result;
}

void CMoTextSymbol::SetHeight(double propVal)
{
	SetProperty(0x3, VT_R8, propVal);
}

BOOL CMoTextSymbol::GetFitted()
{
	BOOL result;
	GetProperty(0x4, VT_BOOL, (void*)&result);
	return result;
}

void CMoTextSymbol::SetFitted(BOOL propVal)
{
	SetProperty(0x4, VT_BOOL, propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoTextSymbol 
