// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "mofield.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoField 

CString CMoField::GetName()
{
	CString result;
	GetProperty(0x1, VT_BSTR, (void*)&result);
	return result;
}

void CMoField::SetName(LPCTSTR propVal)
{
	SetProperty(0x1, VT_BSTR, propVal);
}

long CMoField::GetType()
{
	long result;
	GetProperty(0x2, VT_I4, (void*)&result);
	return result;
}

void CMoField::SetType(long propVal)
{
	SetProperty(0x2, VT_I4, propVal);
}

CString CMoField::GetValueAsString()
{
	CString result;
	GetProperty(0x3, VT_BSTR, (void*)&result);
	return result;
}

void CMoField::SetValueAsString(LPCTSTR propVal)
{
	SetProperty(0x3, VT_BSTR, propVal);
}

VARIANT CMoField::GetValue()
{
	VARIANT result;
	GetProperty(0x4, VT_VARIANT, (void*)&result);
	return result;
}

void CMoField::SetValue(const VARIANT& propVal)
{
	SetProperty(0x4, VT_VARIANT, &propVal);
}

VARIANT CMoField::Get_Value()
{
	VARIANT result;
	GetProperty(0x0, VT_VARIANT, (void*)&result);
	return result;
}

void CMoField::Set_Value(const VARIANT& propVal)
{
	SetProperty(0x0, VT_VARIANT, &propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoField 
