// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "moparts.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoParts 

short CMoParts::GetCount()
{
	short result;
	GetProperty(0x1, VT_I2, (void*)&result);
	return result;
}

void CMoParts::SetCount(short propVal)
{
	SetProperty(0x1, VT_I2, propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoParts 

void CMoParts::Add(LPDISPATCH Points)
{
	static BYTE parms[] =
		VTS_DISPATCH;
	InvokeHelper(0x2, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 Points);
}

void CMoParts::Set(short index, LPDISPATCH Points)
{
	static BYTE parms[] =
		VTS_I2 VTS_DISPATCH;
	InvokeHelper(0x3, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 index, Points);
}

void CMoParts::Remove(short index)
{
	static BYTE parms[] =
		VTS_I2;
	InvokeHelper(0x4, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 index);
}

void CMoParts::Insert(short index, LPDISPATCH Points)
{
	static BYTE parms[] =
		VTS_I2 VTS_DISPATCH;
	InvokeHelper(0x5, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 index, Points);
}

LPDISPATCH CMoParts::Item(const VARIANT& Item)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x6, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		&Item);
	return result;
}
