// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "mofields.h"

// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
#include "mofield.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoFields 

short CMoFields::GetCount()
{
	short result;
	GetProperty(0x1, VT_I2, (void*)&result);
	return result;
}

void CMoFields::SetCount(short propVal)
{
	SetProperty(0x1, VT_I2, propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoFields 

CMoField CMoFields::Item(const VARIANT& Item)
{
	LPDISPATCH pDispatch;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x2, DISPATCH_METHOD, VT_DISPATCH, (void*)&pDispatch, parms,
		&Item);
	return CMoField(pDispatch);
}
