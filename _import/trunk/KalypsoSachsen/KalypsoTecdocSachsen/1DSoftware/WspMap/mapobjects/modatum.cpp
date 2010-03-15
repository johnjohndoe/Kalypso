// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "modatum.h"

// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
#include "MoSpheroid.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoDatum 

long CMoDatum::GetType()
{
	long result;
	GetProperty(0x1, VT_I4, (void*)&result);
	return result;
}

void CMoDatum::SetType(long propVal)
{
	SetProperty(0x1, VT_I4, propVal);
}

CString CMoDatum::GetName()
{
	CString result;
	GetProperty(0x2, VT_BSTR, (void*)&result);
	return result;
}

void CMoDatum::SetName(LPCTSTR propVal)
{
	SetProperty(0x2, VT_BSTR, propVal);
}

CMoSpheroid CMoDatum::GetSpheroid()
{
	LPDISPATCH pDispatch;
	GetProperty(0x3, VT_DISPATCH, (void*)&pDispatch);
	return CMoSpheroid(pDispatch);
}

void CMoDatum::SetSpheroid(LPDISPATCH propVal)
{
	SetProperty(0x3, VT_DISPATCH, propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoDatum 
