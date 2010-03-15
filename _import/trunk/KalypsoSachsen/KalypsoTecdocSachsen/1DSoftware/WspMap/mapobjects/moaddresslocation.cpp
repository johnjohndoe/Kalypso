// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "moaddresslocation.h"

// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
#include "mopoint.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoAddressLocation 

CMoPoint CMoAddressLocation::GetLocation()
{
	LPDISPATCH pDispatch;
	GetProperty(0x1, VT_DISPATCH, (void*)&pDispatch);
	return CMoPoint(pDispatch);
}

void CMoAddressLocation::SetLocation(LPDISPATCH propVal)
{
	SetProperty(0x1, VT_DISPATCH, propVal);
}

short CMoAddressLocation::GetMatchScore()
{
	short result;
	GetProperty(0x2, VT_I2, (void*)&result);
	return result;
}

void CMoAddressLocation::SetMatchScore(short propVal)
{
	SetProperty(0x2, VT_I2, propVal);
}

long CMoAddressLocation::GetStreetSide()
{
	long result;
	GetProperty(0x3, VT_I4, (void*)&result);
	return result;
}

void CMoAddressLocation::SetStreetSide(long propVal)
{
	SetProperty(0x3, VT_I4, propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoAddressLocation 
