// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "mogeotransformation.h"

// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
#include "mogeocoordsys.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoGeoTransformation 

long CMoGeoTransformation::GetType()
{
	long result;
	GetProperty(0x1, VT_I4, (void*)&result);
	return result;
}

void CMoGeoTransformation::SetType(long propVal)
{
	SetProperty(0x1, VT_I4, propVal);
}

CString CMoGeoTransformation::GetName()
{
	CString result;
	GetProperty(0x2, VT_BSTR, (void*)&result);
	return result;
}

void CMoGeoTransformation::SetName(LPCTSTR propVal)
{
	SetProperty(0x2, VT_BSTR, propVal);
}

CMoGeoCoordSys CMoGeoTransformation::GetFromGeoCoordSys()
{
	LPDISPATCH pDispatch;
	GetProperty(0x3, VT_DISPATCH, (void*)&pDispatch);
	return CMoGeoCoordSys(pDispatch);
}

void CMoGeoTransformation::SetFromGeoCoordSys(LPDISPATCH propVal)
{
	SetProperty(0x3, VT_DISPATCH, propVal);
}

CMoGeoCoordSys CMoGeoTransformation::GetToGeoCoordSys()
{
	LPDISPATCH pDispatch;
	GetProperty(0x4, VT_DISPATCH, (void*)&pDispatch);
	return CMoGeoCoordSys(pDispatch);
}

void CMoGeoTransformation::SetToGeoCoordSys(LPDISPATCH propVal)
{
	SetProperty(0x4, VT_DISPATCH, propVal);
}

long CMoGeoTransformation::GetMethod()
{
	long result;
	GetProperty(0x5, VT_I4, (void*)&result);
	return result;
}

void CMoGeoTransformation::SetMethod(long propVal)
{
	SetProperty(0x5, VT_I4, propVal);
}

long CMoGeoTransformation::GetDirection()
{
	long result;
	GetProperty(0x6, VT_I4, (void*)&result);
	return result;
}

void CMoGeoTransformation::SetDirection(long propVal)
{
	SetProperty(0x6, VT_I4, propVal);
}

long CMoGeoTransformation::GetSecondType()
{
	long result;
	GetProperty(0x7, VT_I4, (void*)&result);
	return result;
}

void CMoGeoTransformation::SetSecondType(long propVal)
{
	SetProperty(0x7, VT_I4, propVal);
}

CString CMoGeoTransformation::GetSecondName()
{
	CString result;
	GetProperty(0x8, VT_BSTR, (void*)&result);
	return result;
}

void CMoGeoTransformation::SetSecondName(LPCTSTR propVal)
{
	SetProperty(0x8, VT_BSTR, propVal);
}

long CMoGeoTransformation::GetSecondDirection()
{
	long result;
	GetProperty(0x9, VT_I4, (void*)&result);
	return result;
}

void CMoGeoTransformation::SetSecondDirection(long propVal)
{
	SetProperty(0x9, VT_I4, propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoGeoTransformation 

void CMoGeoTransformation::SetParameter(long ParameterType, double ParameterValue)
{
	static BYTE parms[] =
		VTS_I4 VTS_R8;
	InvokeHelper(0xa, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 ParameterType, ParameterValue);
}

double CMoGeoTransformation::GetParameter(long ParameterType)
{
	double result;
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0xb, DISPATCH_METHOD, VT_R8, (void*)&result, parms,
		ParameterType);
	return result;
}
