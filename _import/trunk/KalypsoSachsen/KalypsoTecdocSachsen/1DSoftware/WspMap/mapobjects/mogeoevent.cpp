// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "mogeoevent.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoGeoEvent 

CString CMoGeoEvent::GetTag()
{
	CString result;
	GetProperty(0x1, VT_BSTR, (void*)&result);
	return result;
}

void CMoGeoEvent::SetTag(LPCTSTR propVal)
{
	SetProperty(0x1, VT_BSTR, propVal);
}

long CMoGeoEvent::GetSymbolIndex()
{
	long result;
	GetProperty(0x2, VT_I4, (void*)&result);
	return result;
}

void CMoGeoEvent::SetSymbolIndex(long propVal)
{
	SetProperty(0x2, VT_I4, propVal);
}

double CMoGeoEvent::GetX()
{
	double result;
	GetProperty(0x3, VT_R8, (void*)&result);
	return result;
}

void CMoGeoEvent::SetX(double propVal)
{
	SetProperty(0x3, VT_R8, propVal);
}

double CMoGeoEvent::GetY()
{
	double result;
	GetProperty(0x4, VT_R8, (void*)&result);
	return result;
}

void CMoGeoEvent::SetY(double propVal)
{
	SetProperty(0x4, VT_R8, propVal);
}

long CMoGeoEvent::GetIndex()
{
	long result;
	GetProperty(0x5, VT_I4, (void*)&result);
	return result;
}

void CMoGeoEvent::SetIndex(long propVal)
{
	SetProperty(0x5, VT_I4, propVal);
}

LPDISPATCH CMoGeoEvent::GetShape()
{
	LPDISPATCH result;
	GetProperty(0x6, VT_DISPATCH, (void*)&result);
	return result;
}

void CMoGeoEvent::SetShape(LPDISPATCH propVal)
{
	SetProperty(0x6, VT_DISPATCH, propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoGeoEvent 

void CMoGeoEvent::MoveTo(double X, double Y)
{
	static BYTE parms[] =
		VTS_R8 VTS_R8;
	InvokeHelper(0x7, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 X, Y);
}

void CMoGeoEvent::Move(double X, double Y)
{
	static BYTE parms[] =
		VTS_R8 VTS_R8;
	InvokeHelper(0x8, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 X, Y);
}
