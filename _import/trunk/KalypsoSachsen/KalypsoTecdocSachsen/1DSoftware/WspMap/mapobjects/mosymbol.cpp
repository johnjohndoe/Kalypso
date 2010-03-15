// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "mosymbol.h"

// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
#include "olefont.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoSymbol 

short CMoSymbol::GetSize()
{
	short result;
	GetProperty(0x6, VT_I2, (void*)&result);
	return result;
}

void CMoSymbol::SetSize(short propVal)
{
	SetProperty(0x6, VT_I2, propVal);
}

short CMoSymbol::GetStyle()
{
	short result;
	GetProperty(0x7, VT_I2, (void*)&result);
	return result;
}

void CMoSymbol::SetStyle(short propVal)
{
	SetProperty(0x7, VT_I2, propVal);
}

unsigned long CMoSymbol::GetColor()
{
	unsigned long result;
	GetProperty(0x1, VT_I4, (void*)&result);
	return result;
}

void CMoSymbol::SetColor(unsigned long propVal)
{
	SetProperty(0x1, VT_I4, propVal);
}

short CMoSymbol::GetCharacterIndex()
{
	short result;
	GetProperty(0x2, VT_I2, (void*)&result);
	return result;
}

void CMoSymbol::SetCharacterIndex(short propVal)
{
	SetProperty(0x2, VT_I2, propVal);
}

COleFont CMoSymbol::GetFont()
{
	LPDISPATCH pDispatch;
	GetProperty(0x8, VT_DISPATCH, (void*)&pDispatch);
	return COleFont(pDispatch);
}

void CMoSymbol::SetFont(LPDISPATCH propVal)
{
	SetProperty(0x8, VT_DISPATCH, propVal);
}

unsigned long CMoSymbol::GetOutlineColor()
{
	unsigned long result;
	GetProperty(0x3, VT_I4, (void*)&result);
	return result;
}

void CMoSymbol::SetOutlineColor(unsigned long propVal)
{
	SetProperty(0x3, VT_I4, propVal);
}

long CMoSymbol::GetSymbolType()
{
	long result;
	GetProperty(0x9, VT_I4, (void*)&result);
	return result;
}

void CMoSymbol::SetSymbolType(long propVal)
{
	SetProperty(0x9, VT_I4, propVal);
}

BOOL CMoSymbol::GetOutline()
{
	BOOL result;
	GetProperty(0x4, VT_BOOL, (void*)&result);
	return result;
}

void CMoSymbol::SetOutline(BOOL propVal)
{
	SetProperty(0x4, VT_BOOL, propVal);
}

BOOL CMoSymbol::GetCenterOnAscent()
{
	BOOL result;
	GetProperty(0x5, VT_BOOL, (void*)&result);
	return result;
}

void CMoSymbol::SetCenterOnAscent(BOOL propVal)
{
	SetProperty(0x5, VT_BOOL, propVal);
}

LPUNKNOWN CMoSymbol::GetCustom()
{
	LPUNKNOWN result;
	GetProperty(0xa, VT_UNKNOWN, (void*)&result);
	return result;
}

void CMoSymbol::SetCustom(LPUNKNOWN propVal)
{
	SetProperty(0xa, VT_UNKNOWN, propVal);
}

double CMoSymbol::GetRotation()
{
	double result;
	GetProperty(0xb, VT_R8, (void*)&result);
	return result;
}

void CMoSymbol::SetRotation(double propVal)
{
	SetProperty(0xb, VT_R8, propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoSymbol 
