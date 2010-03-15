// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "momap.h"

// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
#include "MoLayers.h"
#include "MoRectangle.h"
#include "motrackinglayer.h"
#include "MoPoint.h"
#include "moellipse.h"
#include "MoLine.h"
#include "mopolygon.h"
#include "mogeodatasets.h"
#include "MoDataConnection.h"
#include "mogeodataset.h"
#include "momaplayer.h"
#include "morecordset.h"
#include "MoFields.h"
#include "MoField.h"
#include "mopoints.h"
#include "MoSymbol.h"
#include "movaluemaprenderer.h"
#include "MoStrings.h"
#include "MoStatistics.h"
#include "MoClassBreaksRenderer.h"
#include "MoDotDensityRenderer.h"
#include "MoLabelRenderer.h"
#include "MoGeoEvent.h"
#include "MoImageLayer.h"
#include "motable.h"
#include "MoTextSymbol.h"
#include "MoTableDesc.h"
#include "MoAddressLocation.h"
#include "MoPlaceLocator.h"
#include "moparts.h"
#include "MoProjection.h"
#include "MoProjCoordSys.h"
#include "mogeocoordsys.h"
#include "MoUnit.h"
#include "modatum.h"
#include "MoSpheroid.h"
#include "MoPrimeMeridian.h"
#include "mogeotransformation.h"
#include "mostandardizer.h"
#include "MoGeocoder.h"
#include "MoZRenderer.h"
#include "MoGroupRenderer.h"
#include "MoChartRenderer.h"
#include "MoLabelPlacer.h"
#include "moeventrenderer.h"

/////////////////////////////////////////////////////////////////////////////
// CMoMap

IMPLEMENT_DYNCREATE(CMoMap, CWnd)

/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoMap

CMoLayers CMoMap::GetLayers()
{
	LPDISPATCH pDispatch;
	GetProperty(0x4, VT_DISPATCH, (void*)&pDispatch);
	return CMoLayers(pDispatch);
}

void CMoMap::SetLayers(LPDISPATCH propVal)
{
	SetProperty(0x4, VT_DISPATCH, propVal);
}

CMoRectangle CMoMap::GetExtent()
{
	LPDISPATCH pDispatch;
	GetProperty(0x5, VT_DISPATCH, (void*)&pDispatch);
	return CMoRectangle(pDispatch);
}

void CMoMap::SetExtent(LPDISPATCH propVal)
{
	SetProperty(0x5, VT_DISPATCH, propVal);
}

CMoRectangle CMoMap::GetFullExtent()
{
	LPDISPATCH pDispatch;
	GetProperty(0x6, VT_DISPATCH, (void*)&pDispatch);
	return CMoRectangle(pDispatch);
}

void CMoMap::SetFullExtent(LPDISPATCH propVal)
{
	SetProperty(0x6, VT_DISPATCH, propVal);
}

OLE_COLOR CMoMap::GetBackColor()
{
	OLE_COLOR result;
	GetProperty(DISPID_BACKCOLOR, VT_I4, (void*)&result);
	return result;
}

void CMoMap::SetBackColor(OLE_COLOR propVal)
{
	SetProperty(DISPID_BACKCOLOR, VT_I4, propVal);
}

short CMoMap::GetBorderStyle()
{
	short result;
	GetProperty(DISPID_BORDERSTYLE, VT_I2, (void*)&result);
	return result;
}

void CMoMap::SetBorderStyle(short propVal)
{
	SetProperty(DISPID_BORDERSTYLE, VT_I2, propVal);
}

BOOL CMoMap::GetScrollBars()
{
	BOOL result;
	GetProperty(0x2, VT_BOOL, (void*)&result);
	return result;
}

void CMoMap::SetScrollBars(BOOL propVal)
{
	SetProperty(0x2, VT_BOOL, propVal);
}

long CMoMap::GetCancelAction()
{
	long result;
	GetProperty(0x8, VT_I4, (void*)&result);
	return result;
}

void CMoMap::SetCancelAction(long propVal)
{
	SetProperty(0x8, VT_I4, propVal);
}

CMoTrackingLayer CMoMap::GetTrackingLayer()
{
	LPDISPATCH pDispatch;
	GetProperty(0x9, VT_DISPATCH, (void*)&pDispatch);
	return CMoTrackingLayer(pDispatch);
}

void CMoMap::SetTrackingLayer(LPDISPATCH propVal)
{
	SetProperty(0x9, VT_DISPATCH, propVal);
}

long CMoMap::GetRefreshCount()
{
	long result;
	GetProperty(0xa, VT_I4, (void*)&result);
	return result;
}

void CMoMap::SetRefreshCount(long propVal)
{
	SetProperty(0xa, VT_I4, propVal);
}

BOOL CMoMap::GetEnabled()
{
	BOOL result;
	GetProperty(DISPID_ENABLED, VT_BOOL, (void*)&result);
	return result;
}

void CMoMap::SetEnabled(BOOL propVal)
{
	SetProperty(DISPID_ENABLED, VT_BOOL, propVal);
}

OLE_HANDLE CMoMap::GetHWnd()
{
	OLE_HANDLE result;
	GetProperty(DISPID_HWND, VT_I4, (void*)&result);
	return result;
}

void CMoMap::SetHWnd(OLE_HANDLE propVal)
{
	SetProperty(DISPID_HWND, VT_I4, propVal);
}

short CMoMap::GetAppearance()
{
	short result;
	GetProperty(DISPID_APPEARANCE, VT_I4, (void*)&result);
	return result;
}

void CMoMap::SetAppearance(short propVal)
{
	SetProperty(DISPID_APPEARANCE, VT_I4, propVal);
}

double CMoMap::GetMinWidth()
{
	double result;
	GetProperty(0x1, VT_R8, (void*)&result);
	return result;
}

void CMoMap::SetMinWidth(double propVal)
{
	SetProperty(0x1, VT_R8, propVal);
}

long CMoMap::GetMousePointer()
{
	long result;
	GetProperty(0xb, VT_I4, (void*)&result);
	return result;
}

void CMoMap::SetMousePointer(long propVal)
{
	SetProperty(0xb, VT_I4, propVal);
}

BOOL CMoMap::GetFullRedrawOnPan()
{
	BOOL result;
	GetProperty(0x3, VT_BOOL, (void*)&result);
	return result;
}

void CMoMap::SetFullRedrawOnPan(BOOL propVal)
{
	SetProperty(0x3, VT_BOOL, propVal);
}

VARIANT CMoMap::GetCoordinateSystem()
{
	VARIANT result;
	GetProperty(0xc, VT_VARIANT, (void*)&result);
	return result;
}

void CMoMap::SetCoordinateSystem(const VARIANT& propVal)
{
	SetProperty(0xc, VT_VARIANT, &propVal);
}

long CMoMap::GetMaxFileBuffer()
{
	long result;
	GetProperty(0xd, VT_I4, (void*)&result);
	return result;
}

void CMoMap::SetMaxFileBuffer(long propVal)
{
	SetProperty(0xd, VT_I4, propVal);
}

long CMoMap::GetWindowMode()
{
	long result;
	GetProperty(0xe, VT_I4, (void*)&result);
	return result;
}

void CMoMap::SetWindowMode(long propVal)
{
	SetProperty(0xe, VT_I4, propVal);
}

double CMoMap::GetRotationAngle()
{
	double result;
	GetProperty(0xf, VT_R8, (void*)&result);
	return result;
}

void CMoMap::SetRotationAngle(double propVal)
{
	SetProperty(0xf, VT_R8, propVal);
}

LPDISPATCH CMoMap::GetVisibleRegion()
{
	LPDISPATCH result;
	GetProperty(0x10, VT_DISPATCH, (void*)&result);
	return result;
}

void CMoMap::SetVisibleRegion(LPDISPATCH propVal)
{
	SetProperty(0x10, VT_DISPATCH, propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoMap

void CMoMap::Pan()
{
	InvokeHelper(0x11, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

CMoPoint CMoMap::ToMapPoint(float X, float Y)
{
	LPDISPATCH pDispatch;
	static BYTE parms[] =
		VTS_R4 VTS_R4;
	InvokeHelper(0x12, DISPATCH_METHOD, VT_DISPATCH, (void*)&pDispatch, parms,
		X, Y);
	return CMoPoint(pDispatch);
}

void CMoMap::FlashShape(LPDISPATCH shape, short nTimes)
{
	static BYTE parms[] =
		VTS_DISPATCH VTS_I2;
	InvokeHelper(0x13, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 shape, nTimes);
}

void CMoMap::Refresh()
{
	InvokeHelper(DISPID_REFRESH, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void CMoMap::DrawShape(LPDISPATCH shape, LPDISPATCH Symbol)
{
	static BYTE parms[] =
		VTS_DISPATCH VTS_DISPATCH;
	InvokeHelper(0x14, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 shape, Symbol);
}

void CMoMap::DrawText(LPCTSTR text, LPDISPATCH shape, LPDISPATCH Symbol)
{
	static BYTE parms[] =
		VTS_BSTR VTS_DISPATCH VTS_DISPATCH;
	InvokeHelper(0x15, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 text, shape, Symbol);
}

CMoEllipse CMoMap::TrackCircle()
{
	LPDISPATCH pDispatch;
	InvokeHelper(0x16, DISPATCH_METHOD, VT_DISPATCH, (void*)&pDispatch, NULL);
	return CMoEllipse(pDispatch);
}

CMoLine CMoMap::TrackLine()
{
	LPDISPATCH pDispatch;
	InvokeHelper(0x17, DISPATCH_METHOD, VT_DISPATCH, (void*)&pDispatch, NULL);
	return CMoLine(pDispatch);
}

CMoRectangle CMoMap::TrackRectangle()
{
	LPDISPATCH pDispatch;
	InvokeHelper(0x18, DISPATCH_METHOD, VT_DISPATCH, (void*)&pDispatch, NULL);
	return CMoRectangle(pDispatch);
}

void CMoMap::FromMapPoint(LPDISPATCH Point, float* X, float* Y)
{
	static BYTE parms[] =
		VTS_DISPATCH VTS_PR4 VTS_PR4;
	InvokeHelper(0x19, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 Point, X, Y);
}

double CMoMap::ToMapDistance(float distance)
{
	double result;
	static BYTE parms[] =
		VTS_R4;
	InvokeHelper(0x1a, DISPATCH_METHOD, VT_R8, (void*)&result, parms,
		distance);
	return result;
}

float CMoMap::FromMapDistance(double distance)
{
	float result;
	static BYTE parms[] =
		VTS_R8;
	InvokeHelper(0x1b, DISPATCH_METHOD, VT_R4, (void*)&result, parms,
		distance);
	return result;
}

CMoPolygon CMoMap::TrackPolygon()
{
	LPDISPATCH pDispatch;
	InvokeHelper(0x1c, DISPATCH_METHOD, VT_DISPATCH, (void*)&pDispatch, NULL);
	return CMoPolygon(pDispatch);
}

void CMoMap::OutputMap(long hDC)
{
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x1d, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 hDC);
}

void CMoMap::PrintMap(LPCTSTR docName, LPCTSTR outputFile, BOOL landscapeOrientation)
{
	static BYTE parms[] =
		VTS_BSTR VTS_BSTR VTS_BOOL;
	InvokeHelper(0x1e, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 docName, outputFile, landscapeOrientation);
}

void CMoMap::ExportMap(long exportType, LPCTSTR outputFile, double scaleFactor)
{
	static BYTE parms[] =
		VTS_I4 VTS_BSTR VTS_R8;
	InvokeHelper(0x1f, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 exportType, outputFile, scaleFactor);
}

void CMoMap::CopyMap(double scaleFactor)
{
	static BYTE parms[] =
		VTS_R8;
	InvokeHelper(0x20, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 scaleFactor);
}

void CMoMap::CenterAt(double X, double Y)
{
	static BYTE parms[] =
		VTS_R8 VTS_R8;
	InvokeHelper(0x21, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 X, Y);
}

void CMoMap::RefreshLayer(short index, const VARIANT& rect)
{
	static BYTE parms[] =
		VTS_I2 VTS_VARIANT;
	InvokeHelper(0x22, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 index, &rect);
}

void CMoMap::ExportMap2(long exportType, LPCTSTR outputFile, double scaleFactor, const VARIANT& useSourceDepth)
{
	static BYTE parms[] =
		VTS_I4 VTS_BSTR VTS_R8 VTS_VARIANT;
	InvokeHelper(0x23, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 exportType, outputFile, scaleFactor, &useSourceDepth);
}

void CMoMap::OutputMap2(long hDC, long X, long Y, long Width, long Height, const VARIANT& DrawFlags)
{
	static BYTE parms[] =
		VTS_I4 VTS_I4 VTS_I4 VTS_I4 VTS_I4 VTS_VARIANT;
	InvokeHelper(0x24, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 hDC, X, Y, Width, Height, &DrawFlags);
}

void CMoMap::RefreshRect(LPDISPATCH rect)
{
	static BYTE parms[] =
		VTS_DISPATCH;
	InvokeHelper(0x25, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 rect);
}

void CMoMap::EnableTIFFLZW(LPCTSTR licenseCode)
{
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x26, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 licenseCode);
}

void CMoMap::EnableGIF(LPCTSTR licenseCode)
{
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x27, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 licenseCode);
}

void CMoMap::AboutBox()
{
	InvokeHelper(0xfffffdd8, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}
