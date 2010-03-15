// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "modotdensityrenderer.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoDotDensityRenderer 

CString CMoDotDensityRenderer::GetField()
{
	CString result;
	GetProperty(0x1, VT_BSTR, (void*)&result);
	return result;
}

void CMoDotDensityRenderer::SetField(LPCTSTR propVal)
{
	SetProperty(0x1, VT_BSTR, propVal);
}

double CMoDotDensityRenderer::GetDotValue()
{
	double result;
	GetProperty(0x2, VT_R8, (void*)&result);
	return result;
}

void CMoDotDensityRenderer::SetDotValue(double propVal)
{
	SetProperty(0x2, VT_R8, propVal);
}

short CMoDotDensityRenderer::GetDotSize()
{
	short result;
	GetProperty(0x6, VT_I2, (void*)&result);
	return result;
}

void CMoDotDensityRenderer::SetDotSize(short propVal)
{
	SetProperty(0x6, VT_I2, propVal);
}

unsigned long CMoDotDensityRenderer::GetDotColor()
{
	unsigned long result;
	GetProperty(0x3, VT_I4, (void*)&result);
	return result;
}

void CMoDotDensityRenderer::SetDotColor(unsigned long propVal)
{
	SetProperty(0x3, VT_I4, propVal);
}

BOOL CMoDotDensityRenderer::GetDrawBackground()
{
	BOOL result;
	GetProperty(0x4, VT_BOOL, (void*)&result);
	return result;
}

void CMoDotDensityRenderer::SetDrawBackground(BOOL propVal)
{
	SetProperty(0x4, VT_BOOL, propVal);
}

CString CMoDotDensityRenderer::GetTag()
{
	CString result;
	GetProperty(0x5, VT_BSTR, (void*)&result);
	return result;
}

void CMoDotDensityRenderer::SetTag(LPCTSTR propVal)
{
	SetProperty(0x5, VT_BSTR, propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoDotDensityRenderer 
