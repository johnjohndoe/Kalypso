// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "motable.h"

// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
#include "MoRecordset.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoTable 

CString CMoTable::GetUser()
{
	CString result;
	GetProperty(0x1, VT_BSTR, (void*)&result);
	return result;
}

void CMoTable::SetUser(LPCTSTR propVal)
{
	SetProperty(0x1, VT_BSTR, propVal);
}

CString CMoTable::GetPassword()
{
	CString result;
	GetProperty(0x2, VT_BSTR, (void*)&result);
	return result;
}

void CMoTable::SetPassword(LPCTSTR propVal)
{
	SetProperty(0x2, VT_BSTR, propVal);
}

CString CMoTable::GetDatabase()
{
	CString result;
	GetProperty(0x3, VT_BSTR, (void*)&result);
	return result;
}

void CMoTable::SetDatabase(LPCTSTR propVal)
{
	SetProperty(0x3, VT_BSTR, propVal);
}

CString CMoTable::GetName()
{
	CString result;
	GetProperty(0x4, VT_BSTR, (void*)&result);
	return result;
}

void CMoTable::SetName(LPCTSTR propVal)
{
	SetProperty(0x4, VT_BSTR, propVal);
}

CMoRecordset CMoTable::GetRecords()
{
	LPDISPATCH pDispatch;
	GetProperty(0x6, VT_DISPATCH, (void*)&pDispatch);
	return CMoRecordset(pDispatch);
}

void CMoTable::SetRecords(LPDISPATCH propVal)
{
	SetProperty(0x6, VT_DISPATCH, propVal);
}

CString CMoTable::GetServer()
{
	CString result;
	GetProperty(0x5, VT_BSTR, (void*)&result);
	return result;
}

void CMoTable::SetServer(LPCTSTR propVal)
{
	SetProperty(0x5, VT_BSTR, propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoTable 

CMoRecordset CMoTable::SearchExpression(LPCTSTR expression)
{
	LPDISPATCH pDispatch;
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x7, DISPATCH_METHOD, VT_DISPATCH, (void*)&pDispatch, parms,
		expression);
	return CMoRecordset(pDispatch);
}
