// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "mogeocoder.h"

// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
#include "mostandardizer.h"
#include "mogeodataset.h"
#include "mostrings.h"
#include "moaddresslocation.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoGeocoder 

BOOL CMoGeocoder::GetValid()
{
	BOOL result;
	GetProperty(0x1, VT_BOOL, (void*)&result);
	return result;
}

void CMoGeocoder::SetValid(BOOL propVal)
{
	SetProperty(0x1, VT_BOOL, propVal);
}

short CMoGeocoder::GetCandidateCount()
{
	short result;
	GetProperty(0x2, VT_I2, (void*)&result);
	return result;
}

void CMoGeocoder::SetCandidateCount(short propVal)
{
	SetProperty(0x2, VT_I2, propVal);
}

double CMoGeocoder::GetSqueezeFactor()
{
	double result;
	GetProperty(0x3, VT_R8, (void*)&result);
	return result;
}

void CMoGeocoder::SetSqueezeFactor(double propVal)
{
	SetProperty(0x3, VT_R8, propVal);
}

double CMoGeocoder::GetOffset()
{
	double result;
	GetProperty(0x4, VT_R8, (void*)&result);
	return result;
}

void CMoGeocoder::SetOffset(double propVal)
{
	SetProperty(0x4, VT_R8, propVal);
}

CString CMoGeocoder::GetMatchRules()
{
	CString result;
	GetProperty(0x5, VT_BSTR, (void*)&result);
	return result;
}

void CMoGeocoder::SetMatchRules(LPCTSTR propVal)
{
	SetProperty(0x5, VT_BSTR, propVal);
}

short CMoGeocoder::GetMatchVariableCount()
{
	short result;
	GetProperty(0x6, VT_I2, (void*)&result);
	return result;
}

void CMoGeocoder::SetMatchVariableCount(short propVal)
{
	SetProperty(0x6, VT_I2, propVal);
}

CMoStandardizer CMoGeocoder::GetStandardizer()
{
	LPDISPATCH pDispatch;
	GetProperty(0x7, VT_DISPATCH, (void*)&pDispatch);
	return CMoStandardizer(pDispatch);
}

void CMoGeocoder::SetStandardizer(LPDISPATCH propVal)
{
	SetProperty(0x7, VT_DISPATCH, propVal);
}

CMoGeoDataset CMoGeocoder::GetStreetTable()
{
	LPDISPATCH pDispatch;
	GetProperty(0x8, VT_DISPATCH, (void*)&pDispatch);
	return CMoGeoDataset(pDispatch);
}

void CMoGeocoder::SetStreetTable(LPDISPATCH propVal)
{
	SetProperty(0x8, VT_DISPATCH, propVal);
}

long CMoGeocoder::GetLastError()
{
	long result;
	GetProperty(0x9, VT_I4, (void*)&result);
	return result;
}

void CMoGeocoder::SetLastError(long propVal)
{
	SetProperty(0x9, VT_I4, propVal);
}

CString CMoGeocoder::GetIntersectionMatchRules()
{
	CString result;
	GetProperty(0xa, VT_BSTR, (void*)&result);
	return result;
}

void CMoGeocoder::SetIntersectionMatchRules(LPCTSTR propVal)
{
	SetProperty(0xa, VT_BSTR, propVal);
}

short CMoGeocoder::GetIntersectionMatchVariableCount()
{
	short result;
	GetProperty(0xb, VT_I2, (void*)&result);
	return result;
}

void CMoGeocoder::SetIntersectionMatchVariableCount(short propVal)
{
	SetProperty(0xb, VT_I2, propVal);
}

short CMoGeocoder::GetMinimumMatchScore()
{
	short result;
	GetProperty(0xc, VT_I2, (void*)&result);
	return result;
}

void CMoGeocoder::SetMinimumMatchScore(short propVal)
{
	SetProperty(0xc, VT_I2, propVal);
}

float CMoGeocoder::GetSpellingSensitivity()
{
	float result;
	GetProperty(0xd, VT_R4, (void*)&result);
	return result;
}

void CMoGeocoder::SetSpellingSensitivity(float propVal)
{
	SetProperty(0xd, VT_R4, propVal);
}

BOOL CMoGeocoder::GetMatchWhenAmbiguous()
{
	BOOL result;
	GetProperty(0xe, VT_BOOL, (void*)&result);
	return result;
}

void CMoGeocoder::SetMatchWhenAmbiguous(BOOL propVal)
{
	SetProperty(0xe, VT_BOOL, propVal);
}

CMoStrings CMoGeocoder::GetSearchQueries()
{
	LPDISPATCH pDispatch;
	GetProperty(0xf, VT_DISPATCH, (void*)&pDispatch);
	return CMoStrings(pDispatch);
}

void CMoGeocoder::SetSearchQueries(LPDISPATCH propVal)
{
	SetProperty(0xf, VT_DISPATCH, propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoGeocoder 

CString CMoGeocoder::GetCandidate(short index)
{
	CString result;
	static BYTE parms[] =
		VTS_I2;
	InvokeHelper(0x18, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, parms,
		index);
	return result;
}

CString CMoGeocoder::GetMatchVariable(short index)
{
	CString result;
	static BYTE parms[] =
		VTS_I2;
	InvokeHelper(0x19, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, parms,
		index);
	return result;
}

CString CMoGeocoder::GetMatchVariableField(LPCTSTR variable)
{
	CString result;
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x1a, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, parms,
		variable);
	return result;
}

void CMoGeocoder::SetMatchVariableField(LPCTSTR variable, LPCTSTR lpszNewValue)
{
	static BYTE parms[] =
		VTS_BSTR VTS_BSTR;
	InvokeHelper(0x1a, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 variable, lpszNewValue);
}

CMoAddressLocation CMoGeocoder::LocateCandidate(short index)
{
	LPDISPATCH pDispatch;
	static BYTE parms[] =
		VTS_I2;
	InvokeHelper(0x10, DISPATCH_METHOD, VT_DISPATCH, (void*)&pDispatch, parms,
		index);
	return CMoAddressLocation(pDispatch);
}

CString CMoGeocoder::GetIntersectionMatchVariable(short index)
{
	CString result;
	static BYTE parms[] =
		VTS_I2;
	InvokeHelper(0x1b, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, parms,
		index);
	return result;
}

CString CMoGeocoder::GetMatchVariableIntersectionLink(LPCTSTR variable, long linkGroup)
{
	CString result;
	static BYTE parms[] =
		VTS_BSTR VTS_I4;
	InvokeHelper(0x1c, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, parms,
		variable, linkGroup);
	return result;
}

void CMoGeocoder::SetMatchVariableIntersectionLink(LPCTSTR variable, long linkGroup, LPCTSTR lpszNewValue)
{
	static BYTE parms[] =
		VTS_BSTR VTS_I4 VTS_BSTR;
	InvokeHelper(0x1c, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 variable, linkGroup, lpszNewValue);
}

CString CMoGeocoder::GetBatchMatchVariableField(LPCTSTR variable)
{
	CString result;
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x1d, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, parms,
		variable);
	return result;
}

void CMoGeocoder::SetBatchMatchVariableField(LPCTSTR variable, LPCTSTR lpszNewValue)
{
	static BYTE parms[] =
		VTS_BSTR VTS_BSTR;
	InvokeHelper(0x1d, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 variable, lpszNewValue);
}

CMoStrings CMoGeocoder::ListIndices()
{
	LPDISPATCH pDispatch;
	InvokeHelper(0x11, DISPATCH_METHOD, VT_DISPATCH, (void*)&pDispatch, NULL);
	return CMoStrings(pDispatch);
}

long CMoGeocoder::IndexStatus()
{
	long result;
	InvokeHelper(0x12, DISPATCH_METHOD, VT_I4, (void*)&result, NULL);
	return result;
}

BOOL CMoGeocoder::EraseIndices()
{
	BOOL result;
	InvokeHelper(0x13, DISPATCH_METHOD, VT_BOOL, (void*)&result, NULL);
	return result;
}

BOOL CMoGeocoder::AddIndex(LPCTSTR FieldName, LPCTSTR secondaryFieldName, long indexType)
{
	BOOL result;
	static BYTE parms[] =
		VTS_BSTR VTS_BSTR VTS_I4;
	InvokeHelper(0x14, DISPATCH_METHOD, VT_BOOL, (void*)&result, parms,
		FieldName, secondaryFieldName, indexType);
	return result;
}

BOOL CMoGeocoder::BuildIndices(BOOL force)
{
	BOOL result;
	static BYTE parms[] =
		VTS_BOOL;
	InvokeHelper(0x15, DISPATCH_METHOD, VT_BOOL, (void*)&result, parms,
		force);
	return result;
}

long CMoGeocoder::GenerateCandidates()
{
	long result;
	InvokeHelper(0x16, DISPATCH_METHOD, VT_I4, (void*)&result, NULL);
	return result;
}

long CMoGeocoder::BatchMatch(LPDISPATCH addressTable, LPCTSTR addressField, LPDISPATCH DataConnection, LPCTSTR outputTableName, LPDISPATCH outputFields)
{
	long result;
	static BYTE parms[] =
		VTS_DISPATCH VTS_BSTR VTS_DISPATCH VTS_BSTR VTS_DISPATCH;
	InvokeHelper(0x17, DISPATCH_METHOD, VT_I4, (void*)&result, parms,
		addressTable, addressField, DataConnection, outputTableName, outputFields);
	return result;
}
