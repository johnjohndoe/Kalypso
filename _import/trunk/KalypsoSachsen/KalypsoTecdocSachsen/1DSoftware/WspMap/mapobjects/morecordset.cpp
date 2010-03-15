// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "morecordset.h"

// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
#include "MoFields.h"
#include "MoTableDesc.h"
#include "MoStatistics.h"
#include "mogeodataset.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoRecordset 

CMoFields CMoRecordset::GetFields()
{
	LPDISPATCH pDispatch;
	GetProperty(0x1, VT_DISPATCH, (void*)&pDispatch);
	return CMoFields(pDispatch);
}

void CMoRecordset::SetFields(LPDISPATCH propVal)
{
	SetProperty(0x1, VT_DISPATCH, propVal);
}

BOOL CMoRecordset::GetEof()
{
	BOOL result;
	GetProperty(0x2, VT_BOOL, (void*)&result);
	return result;
}

void CMoRecordset::SetEof(BOOL propVal)
{
	SetProperty(0x2, VT_BOOL, propVal);
}

long CMoRecordset::GetCount()
{
	long result;
	GetProperty(0x3, VT_I4, (void*)&result);
	return result;
}

void CMoRecordset::SetCount(long propVal)
{
	SetProperty(0x3, VT_I4, propVal);
}

long CMoRecordset::GetEditMode()
{
	long result;
	GetProperty(0x4, VT_I4, (void*)&result);
	return result;
}

void CMoRecordset::SetEditMode(long propVal)
{
	SetProperty(0x4, VT_I4, propVal);
}

BOOL CMoRecordset::GetUpdatable()
{
	BOOL result;
	GetProperty(0x5, VT_BOOL, (void*)&result);
	return result;
}

void CMoRecordset::SetUpdatable(BOOL propVal)
{
	SetProperty(0x5, VT_BOOL, propVal);
}

CMoTableDesc CMoRecordset::GetTableDesc()
{
	LPDISPATCH pDispatch;
	GetProperty(0x6, VT_DISPATCH, (void*)&pDispatch);
	return CMoTableDesc(pDispatch);
}

void CMoRecordset::SetTableDesc(LPDISPATCH propVal)
{
	SetProperty(0x6, VT_DISPATCH, propVal);
}

BOOL CMoRecordset::GetAutoFlush()
{
	BOOL result;
	GetProperty(0x7, VT_BOOL, (void*)&result);
	return result;
}

void CMoRecordset::SetAutoFlush(BOOL propVal)
{
	SetProperty(0x7, VT_BOOL, propVal);
}

BOOL CMoRecordset::GetSupportsTransactions()
{
	BOOL result;
	GetProperty(0x8, VT_BOOL, (void*)&result);
	return result;
}

void CMoRecordset::SetSupportsTransactions(BOOL propVal)
{
	SetProperty(0x8, VT_BOOL, propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoRecordset 

void CMoRecordset::MoveFirst()
{
	InvokeHelper(0x9, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void CMoRecordset::MoveNext()
{
	InvokeHelper(0xa, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void CMoRecordset::MovePrevious()
{
	InvokeHelper(0xb, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

CMoStatistics CMoRecordset::CalculateStatistics(LPCTSTR FieldName)
{
	LPDISPATCH pDispatch;
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0xc, DISPATCH_METHOD, VT_DISPATCH, (void*)&pDispatch, parms,
		FieldName);
	return CMoStatistics(pDispatch);
}

void CMoRecordset::Delete()
{
	InvokeHelper(0xd, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void CMoRecordset::Edit()
{
	InvokeHelper(0xe, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void CMoRecordset::AddNew()
{
	InvokeHelper(0xf, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void CMoRecordset::Update()
{
	InvokeHelper(0x10, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void CMoRecordset::CancelUpdate()
{
	InvokeHelper(0x11, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void CMoRecordset::StopEditing()
{
	InvokeHelper(0x12, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

CMoGeoDataset CMoRecordset::Export(LPCTSTR OutName, const VARIANT& OutCoordSys)
{
	LPDISPATCH pDispatch;
	static BYTE parms[] =
		VTS_BSTR VTS_VARIANT;
	InvokeHelper(0x13, DISPATCH_METHOD, VT_DISPATCH, (void*)&pDispatch, parms,
		OutName, &OutCoordSys);
	return CMoGeoDataset(pDispatch);
}

void CMoRecordset::StartTransaction()
{
	InvokeHelper(0x14, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void CMoRecordset::RollbackTransaction()
{
	InvokeHelper(0x15, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void CMoRecordset::CommitTransaction()
{
	InvokeHelper(0x16, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}
