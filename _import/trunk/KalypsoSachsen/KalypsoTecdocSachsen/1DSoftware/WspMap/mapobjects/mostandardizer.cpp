// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "mostandardizer.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoStandardizer 

BOOL CMoStandardizer::GetValid()
{
	BOOL result;
	GetProperty(0x1, VT_BOOL, (void*)&result);
	return result;
}

void CMoStandardizer::SetValid(BOOL propVal)
{
	SetProperty(0x1, VT_BOOL, propVal);
}

short CMoStandardizer::GetFieldCount()
{
	short result;
	GetProperty(0x2, VT_I2, (void*)&result);
	return result;
}

void CMoStandardizer::SetFieldCount(short propVal)
{
	SetProperty(0x2, VT_I2, propVal);
}

CString CMoStandardizer::GetStandardizingRules()
{
	CString result;
	GetProperty(0x3, VT_BSTR, (void*)&result);
	return result;
}

void CMoStandardizer::SetStandardizingRules(LPCTSTR propVal)
{
	SetProperty(0x3, VT_BSTR, propVal);
}

long CMoStandardizer::GetLastError()
{
	long result;
	GetProperty(0x4, VT_I4, (void*)&result);
	return result;
}

void CMoStandardizer::SetLastError(long propVal)
{
	SetProperty(0x4, VT_I4, propVal);
}

CString CMoStandardizer::GetIntersectionStandardizingRules()
{
	CString result;
	GetProperty(0x5, VT_BSTR, (void*)&result);
	return result;
}

void CMoStandardizer::SetIntersectionStandardizingRules(LPCTSTR propVal)
{
	SetProperty(0x5, VT_BSTR, propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoStandardizer 

BOOL CMoStandardizer::StandardizeAddress(LPCTSTR address)
{
	BOOL result;
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x6, DISPATCH_METHOD, VT_BOOL, (void*)&result, parms,
		address);
	return result;
}

CString CMoStandardizer::GetFieldName(short index)
{
	CString result;
	static BYTE parms[] =
		VTS_I2;
	InvokeHelper(0x7, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, parms,
		index);
	return result;
}

CString CMoStandardizer::GetFieldValue(LPCTSTR FieldName)
{
	CString result;
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x8, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, parms,
		FieldName);
	return result;
}

void CMoStandardizer::SetFieldValue(LPCTSTR FieldName, LPCTSTR lpszNewValue)
{
	static BYTE parms[] =
		VTS_BSTR VTS_BSTR;
	InvokeHelper(0x8, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 FieldName, lpszNewValue);
}
