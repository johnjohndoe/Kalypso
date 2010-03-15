// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "mosbextent.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoSbExtent 

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoSbExtent 

double CMoSbExtent::GetMinX()
{
	double result;
	InvokeHelper(0x68030007, DISPATCH_PROPERTYGET, VT_R8, (void*)&result, NULL);
	return result;
}

void CMoSbExtent::SetMinX(double newValue)
{
	static BYTE parms[] =
		VTS_R8;
	InvokeHelper(0x68030007, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 newValue);
}

double CMoSbExtent::GetMinY()
{
	double result;
	InvokeHelper(0x68030006, DISPATCH_PROPERTYGET, VT_R8, (void*)&result, NULL);
	return result;
}

void CMoSbExtent::SetMinY(double newValue)
{
	static BYTE parms[] =
		VTS_R8;
	InvokeHelper(0x68030006, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 newValue);
}

double CMoSbExtent::GetMaxX()
{
	double result;
	InvokeHelper(0x68030005, DISPATCH_PROPERTYGET, VT_R8, (void*)&result, NULL);
	return result;
}

void CMoSbExtent::SetMaxX(double newValue)
{
	static BYTE parms[] =
		VTS_R8;
	InvokeHelper(0x68030005, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 newValue);
}

double CMoSbExtent::GetMaxY()
{
	double result;
	InvokeHelper(0x68030004, DISPATCH_PROPERTYGET, VT_R8, (void*)&result, NULL);
	return result;
}

void CMoSbExtent::SetMaxY(double newValue)
{
	static BYTE parms[] =
		VTS_R8;
	InvokeHelper(0x68030004, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 newValue);
}

double CMoSbExtent::GetWidth()
{
	double result;
	InvokeHelper(0x68030003, DISPATCH_PROPERTYGET, VT_R8, (void*)&result, NULL);
	return result;
}

double CMoSbExtent::GetHeight()
{
	double result;
	InvokeHelper(0x68030002, DISPATCH_PROPERTYGET, VT_R8, (void*)&result, NULL);
	return result;
}

double CMoSbExtent::GetCenterX()
{
	double result;
	InvokeHelper(0x68030001, DISPATCH_PROPERTYGET, VT_R8, (void*)&result, NULL);
	return result;
}

VARIANT CMoSbExtent::GetCenterY()
{
	VARIANT result;
	InvokeHelper(0x68030000, DISPATCH_PROPERTYGET, VT_VARIANT, (void*)&result, NULL);
	return result;
}
