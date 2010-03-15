// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "mogeocoordsys.h"

// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
#include "MoDatum.h"
#include "MoUnit.h"
#include "MoPrimeMeridian.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoGeoCoordSys 

long CMoGeoCoordSys::GetType()
{
	long result;
	GetProperty(0x1, VT_I4, (void*)&result);
	return result;
}

void CMoGeoCoordSys::SetType(long propVal)
{
	SetProperty(0x1, VT_I4, propVal);
}

CString CMoGeoCoordSys::GetName()
{
	CString result;
	GetProperty(0x2, VT_BSTR, (void*)&result);
	return result;
}

void CMoGeoCoordSys::SetName(LPCTSTR propVal)
{
	SetProperty(0x2, VT_BSTR, propVal);
}

CMoDatum CMoGeoCoordSys::GetDatum()
{
	LPDISPATCH pDispatch;
	GetProperty(0x3, VT_DISPATCH, (void*)&pDispatch);
	return CMoDatum(pDispatch);
}

void CMoGeoCoordSys::SetDatum(LPDISPATCH propVal)
{
	SetProperty(0x3, VT_DISPATCH, propVal);
}

CMoUnit CMoGeoCoordSys::GetUnit()
{
	LPDISPATCH pDispatch;
	GetProperty(0x4, VT_DISPATCH, (void*)&pDispatch);
	return CMoUnit(pDispatch);
}

void CMoGeoCoordSys::SetUnit(LPDISPATCH propVal)
{
	SetProperty(0x4, VT_DISPATCH, propVal);
}

CMoPrimeMeridian CMoGeoCoordSys::GetPrimeMeridian()
{
	LPDISPATCH pDispatch;
	GetProperty(0x5, VT_DISPATCH, (void*)&pDispatch);
	return CMoPrimeMeridian(pDispatch);
}

void CMoGeoCoordSys::SetPrimeMeridian(LPDISPATCH propVal)
{
	SetProperty(0x5, VT_DISPATCH, propVal);
}

BOOL CMoGeoCoordSys::GetIsProjected()
{
	BOOL result;
	GetProperty(0x6, VT_BOOL, (void*)&result);
	return result;
}

void CMoGeoCoordSys::SetIsProjected(BOOL propVal)
{
	SetProperty(0x6, VT_BOOL, propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoGeoCoordSys 

LPDISPATCH CMoGeoCoordSys::Transform(LPDISPATCH FromCoordSys, LPDISPATCH FromShape, const VARIANT& DensificationTolerance, const VARIANT& GeoTransformation)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_DISPATCH VTS_DISPATCH VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x7, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		FromCoordSys, FromShape, &DensificationTolerance, &GeoTransformation);
	return result;
}

void CMoGeoCoordSys::Export(LPCTSTR OutName)
{
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x8, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 OutName);
}

CString CMoGeoCoordSys::ReturnDescription()
{
	CString result;
	InvokeHelper(0x9, DISPATCH_METHOD, VT_BSTR, (void*)&result, NULL);
	return result;
}
