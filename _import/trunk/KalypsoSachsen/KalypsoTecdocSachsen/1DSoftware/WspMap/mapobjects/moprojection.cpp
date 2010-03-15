// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "moprojection.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoProjection 

long CMoProjection::GetType()
{
	long result;
	GetProperty(0x1, VT_I4, (void*)&result);
	return result;
}

void CMoProjection::SetType(long propVal)
{
	SetProperty(0x1, VT_I4, propVal);
}

CString CMoProjection::GetName()
{
	CString result;
	GetProperty(0x2, VT_BSTR, (void*)&result);
	return result;
}

void CMoProjection::SetName(LPCTSTR propVal)
{
	SetProperty(0x2, VT_BSTR, propVal);
}

LPUNKNOWN CMoProjection::GetCustom()
{
	LPUNKNOWN result;
	GetProperty(0x3, VT_UNKNOWN, (void*)&result);
	return result;
}

void CMoProjection::SetCustom(LPUNKNOWN propVal)
{
	SetProperty(0x3, VT_UNKNOWN, propVal);
}

BOOL CMoProjection::GetIsCustom()
{
	BOOL result;
	GetProperty(0x4, VT_BOOL, (void*)&result);
	return result;
}

void CMoProjection::SetIsCustom(BOOL propVal)
{
	SetProperty(0x4, VT_BOOL, propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoProjection 
