// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "moplacelocator.h"

// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
#include "mogeodataset.h"
#include "mopoints.h"
#include "MoStrings.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoPlaceLocator 

BOOL CMoPlaceLocator::GetIndexed()
{
	BOOL result;
	GetProperty(0x1, VT_BOOL, (void*)&result);
	return result;
}

void CMoPlaceLocator::SetIndexed(BOOL propVal)
{
	SetProperty(0x1, VT_BOOL, propVal);
}

CMoGeoDataset CMoPlaceLocator::GetPlaceNameTable()
{
	LPDISPATCH pDispatch;
	GetProperty(0x2, VT_DISPATCH, (void*)&pDispatch);
	return CMoGeoDataset(pDispatch);
}

void CMoPlaceLocator::SetPlaceNameTable(LPDISPATCH propVal)
{
	SetProperty(0x2, VT_DISPATCH, propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoPlaceLocator 

BOOL CMoPlaceLocator::BuildIndex(LPCTSTR Field, BOOL force)
{
	BOOL result;
	static BYTE parms[] =
		VTS_BSTR VTS_BOOL;
	InvokeHelper(0x3, DISPATCH_METHOD, VT_BOOL, (void*)&result, parms,
		Field, force);
	return result;
}

CMoPoints CMoPlaceLocator::Locate(LPCTSTR placeName)
{
	LPDISPATCH pDispatch;
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x4, DISPATCH_METHOD, VT_DISPATCH, (void*)&pDispatch, parms,
		placeName);
	return CMoPoints(pDispatch);
}

CMoStrings CMoPlaceLocator::FindApproximateMatches(LPCTSTR placeName)
{
	LPDISPATCH pDispatch;
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x5, DISPATCH_METHOD, VT_DISPATCH, (void*)&pDispatch, parms,
		placeName);
	return CMoStrings(pDispatch);
}

CMoStrings CMoPlaceLocator::FindAllPlaceNames(LPCTSTR prefix)
{
	LPDISPATCH pDispatch;
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x6, DISPATCH_METHOD, VT_DISPATCH, (void*)&pDispatch, parms,
		prefix);
	return CMoStrings(pDispatch);
}
