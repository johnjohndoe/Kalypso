// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "mogrouprenderer.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoGroupRenderer 

short CMoGroupRenderer::GetCount()
{
	short result;
	GetProperty(0x2, VT_I2, (void*)&result);
	return result;
}

void CMoGroupRenderer::SetCount(short propVal)
{
	SetProperty(0x2, VT_I2, propVal);
}

BOOL CMoGroupRenderer::GetDrawBackground()
{
	BOOL result;
	GetProperty(0x1, VT_BOOL, (void*)&result);
	return result;
}

void CMoGroupRenderer::SetDrawBackground(BOOL propVal)
{
	SetProperty(0x1, VT_BOOL, propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoGroupRenderer 

LPDISPATCH CMoGroupRenderer::GetRenderer(short index)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_I2;
	InvokeHelper(0x5, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&result, parms,
		index);
	return result;
}

void CMoGroupRenderer::SetRefRenderer(short index, LPDISPATCH newValue)
{
	static BYTE parms[] =
		VTS_I2 VTS_DISPATCH;
	InvokeHelper(0x5, DISPATCH_PROPERTYPUTREF, VT_EMPTY, NULL, parms,
		 index, newValue);
}

short CMoGroupRenderer::Add(LPDISPATCH Renderer)
{
	short result;
	static BYTE parms[] =
		VTS_DISPATCH;
	InvokeHelper(0x3, DISPATCH_METHOD, VT_I2, (void*)&result, parms,
		Renderer);
	return result;
}

void CMoGroupRenderer::Remove(short index)
{
	static BYTE parms[] =
		VTS_I2;
	InvokeHelper(0x4, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 index);
}
