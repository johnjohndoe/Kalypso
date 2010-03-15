// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "modataconnection.h"

// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
#include "mogeodataset.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoDataConnection 

BOOL CMoDataConnection::GetConnected()
{
	BOOL result;
	GetProperty(0x1, VT_BOOL, (void*)&result);
	return result;
}

void CMoDataConnection::SetConnected(BOOL propVal)
{
	SetProperty(0x1, VT_BOOL, propVal);
}

CString CMoDataConnection::GetDatabase()
{
	CString result;
	GetProperty(0x2, VT_BSTR, (void*)&result);
	return result;
}

void CMoDataConnection::SetDatabase(LPCTSTR propVal)
{
	SetProperty(0x2, VT_BSTR, propVal);
}

CString CMoDataConnection::GetServer()
{
	CString result;
	GetProperty(0x3, VT_BSTR, (void*)&result);
	return result;
}

void CMoDataConnection::SetServer(LPCTSTR propVal)
{
	SetProperty(0x3, VT_BSTR, propVal);
}

CString CMoDataConnection::GetUser()
{
	CString result;
	GetProperty(0x4, VT_BSTR, (void*)&result);
	return result;
}

void CMoDataConnection::SetUser(LPCTSTR propVal)
{
	SetProperty(0x4, VT_BSTR, propVal);
}

CString CMoDataConnection::GetPassword()
{
	CString result;
	GetProperty(0x5, VT_BSTR, (void*)&result);
	return result;
}

void CMoDataConnection::SetPassword(LPCTSTR propVal)
{
	SetProperty(0x5, VT_BSTR, propVal);
}

LPDISPATCH CMoDataConnection::GetGeoDatasets()
{
	LPDISPATCH result;
	GetProperty(0x6, VT_DISPATCH, (void*)&result);
	return result;
}

void CMoDataConnection::SetGeoDatasets(LPDISPATCH propVal)
{
	SetProperty(0x6, VT_DISPATCH, propVal);
}

long CMoDataConnection::GetConnection()
{
	long result;
	GetProperty(0x7, VT_I4, (void*)&result);
	return result;
}

void CMoDataConnection::SetConnection(long propVal)
{
	SetProperty(0x7, VT_I4, propVal);
}

long CMoDataConnection::GetConnectError()
{
	long result;
	GetProperty(0x8, VT_I4, (void*)&result);
	return result;
}

void CMoDataConnection::SetConnectError(long propVal)
{
	SetProperty(0x8, VT_I4, propVal);
}

long CMoDataConnection::GetExtendedError()
{
	long result;
	GetProperty(0x9, VT_I4, (void*)&result);
	return result;
}

void CMoDataConnection::SetExtendedError(long propVal)
{
	SetProperty(0x9, VT_I4, propVal);
}

CString CMoDataConnection::GetExtendedErrorString()
{
	CString result;
	GetProperty(0xa, VT_BSTR, (void*)&result);
	return result;
}

void CMoDataConnection::SetExtendedErrorString(LPCTSTR propVal)
{
	SetProperty(0xa, VT_BSTR, propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoDataConnection 

BOOL CMoDataConnection::Connect()
{
	BOOL result;
	InvokeHelper(0xb, DISPATCH_METHOD, VT_BOOL, (void*)&result, NULL);
	return result;
}

void CMoDataConnection::Disconnect()
{
	InvokeHelper(0xc, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

CMoGeoDataset CMoDataConnection::FindGeoDataset(LPCTSTR Name)
{
	LPDISPATCH pDispatch;
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0xd, DISPATCH_METHOD, VT_DISPATCH, (void*)&pDispatch, parms,
		Name);
	return CMoGeoDataset(pDispatch);
}

CMoGeoDataset CMoDataConnection::AddGeoDataset(LPCTSTR Name, long shapeType, LPDISPATCH TableDesc, const VARIANT& HasZ, const VARIANT& HasMeasure)
{
	LPDISPATCH pDispatch;
	static BYTE parms[] =
		VTS_BSTR VTS_I4 VTS_DISPATCH VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0xe, DISPATCH_METHOD, VT_DISPATCH, (void*)&pDispatch, parms,
		Name, shapeType, TableDesc, &HasZ, &HasMeasure);
	return CMoGeoDataset(pDispatch);
}

BOOL CMoDataConnection::DeleteGeoDataset(LPCTSTR Name)
{
	BOOL result;
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0xf, DISPATCH_METHOD, VT_BOOL, (void*)&result, parms,
		Name);
	return result;
}

LPDISPATCH CMoDataConnection::FindCoordinateSystem(LPCTSTR Name)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x10, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		Name);
	return result;
}

void CMoDataConnection::ClearConnectError()
{
	InvokeHelper(0x11, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

LPDISPATCH CMoDataConnection::FindArcInfoCoordinateSystem(LPCTSTR Name)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x12, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		Name);
	return result;
}
