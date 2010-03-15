// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "moprimemeridian.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoPrimeMeridian 

long CMoPrimeMeridian::GetType()
{
	long result;
	GetProperty(0x1, VT_I4, (void*)&result);
	return result;
}

void CMoPrimeMeridian::SetType(long propVal)
{
	SetProperty(0x1, VT_I4, propVal);
}

CString CMoPrimeMeridian::GetName()
{
	CString result;
	GetProperty(0x2, VT_BSTR, (void*)&result);
	return result;
}

void CMoPrimeMeridian::SetName(LPCTSTR propVal)
{
	SetProperty(0x2, VT_BSTR, propVal);
}

double CMoPrimeMeridian::GetLongitude()
{
	double result;
	GetProperty(0x3, VT_R8, (void*)&result);
	return result;
}

void CMoPrimeMeridian::SetLongitude(double propVal)
{
	SetProperty(0x3, VT_R8, propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoPrimeMeridian 
