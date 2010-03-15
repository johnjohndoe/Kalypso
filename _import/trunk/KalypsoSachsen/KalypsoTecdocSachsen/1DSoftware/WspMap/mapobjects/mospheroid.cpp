// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "mospheroid.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoSpheroid 

long CMoSpheroid::GetType()
{
	long result;
	GetProperty(0x1, VT_I4, (void*)&result);
	return result;
}

void CMoSpheroid::SetType(long propVal)
{
	SetProperty(0x1, VT_I4, propVal);
}

double CMoSpheroid::GetAxis()
{
	double result;
	GetProperty(0x2, VT_R8, (void*)&result);
	return result;
}

void CMoSpheroid::SetAxis(double propVal)
{
	SetProperty(0x2, VT_R8, propVal);
}

double CMoSpheroid::GetFlattening()
{
	double result;
	GetProperty(0x3, VT_R8, (void*)&result);
	return result;
}

void CMoSpheroid::SetFlattening(double propVal)
{
	SetProperty(0x3, VT_R8, propVal);
}

CString CMoSpheroid::GetName()
{
	CString result;
	GetProperty(0x4, VT_BSTR, (void*)&result);
	return result;
}

void CMoSpheroid::SetName(LPCTSTR propVal)
{
	SetProperty(0x4, VT_BSTR, propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoSpheroid 
