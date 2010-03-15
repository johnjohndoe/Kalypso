#if !defined(AFX_MAP_H__CC25FE50_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MAP_H__CC25FE50_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
class CMoLayers;
class CMoRectangle;
class CMoTrackingLayer;
class CMoPoint;
class CMoEllipse;
class CMoLine;
class CMoPolygon;
class CMoGeoDatasets;
class CMoDataConnection;
class CMoGeoDataset;
class CMoMapLayer;
class CMoRecordset;
class CMoFields;
class CMoField;
class CMoPoints;
class CMoSymbol;
class CMoValueMapRenderer;
class CMoStrings;
class CMoStatistics;
class CMoClassBreaksRenderer;
class CMoDotDensityRenderer;
class CMoLabelRenderer;
class CMoGeoEvent;
class CMoImageLayer;
class CMoTable;
class CMoTextSymbol;
class CMoTableDesc;
class CMoAddressLocation;
class CMoPlaceLocator;
class CMoParts;
class CMoProjection;
class CMoProjCoordSys;
class CMoGeoCoordSys;
class CMoUnit;
class CMoDatum;
class CMoSpheroid;
class CMoPrimeMeridian;
class CMoGeoTransformation;
class CMoStandardizer;
class CMoGeocoder;
class CMoZRenderer;
class CMoGroupRenderer;
class CMoChartRenderer;
class CMoLabelPlacer;
class CMoEventRenderer;

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoMap 

class CMoMap : public CWnd
{
protected:
	DECLARE_DYNCREATE(CMoMap)
public:
	CLSID const& GetClsid()
	{
		static CLSID const clsid
			= { 0x9bd6a64b, 0xce75, 0x11d1, { 0xaf, 0x4, 0x20, 0x4c, 0x4f, 0x4f, 0x50, 0x20 } };
		return clsid;
	}
	virtual BOOL Create(LPCTSTR lpszClassName,
		LPCTSTR lpszWindowName, DWORD dwStyle,
		const RECT& rect,
		CWnd* pParentWnd, UINT nID,
		CCreateContext* pContext = NULL)
	{ return CreateControl(GetClsid(), lpszWindowName, dwStyle, rect, pParentWnd, nID); }

    BOOL Create(LPCTSTR lpszWindowName, DWORD dwStyle,
		const RECT& rect, CWnd* pParentWnd, UINT nID,
		CFile* pPersist = NULL, BOOL bStorage = FALSE,
		BSTR bstrLicKey = NULL)
	{ return CreateControl(GetClsid(), lpszWindowName, dwStyle, rect, pParentWnd, nID,
		pPersist, bStorage, bstrLicKey); }

// Attribute
public:
	CMoLayers GetLayers();
	void SetLayers(LPDISPATCH);
	CMoRectangle GetExtent();
	void SetExtent(LPDISPATCH);
	CMoRectangle GetFullExtent();
	void SetFullExtent(LPDISPATCH);
	OLE_COLOR GetBackColor();
	void SetBackColor(OLE_COLOR);
	short GetBorderStyle();
	void SetBorderStyle(short);
	BOOL GetScrollBars();
	void SetScrollBars(BOOL);
	long GetCancelAction();
	void SetCancelAction(long);
	CMoTrackingLayer GetTrackingLayer();
	void SetTrackingLayer(LPDISPATCH);
	long GetRefreshCount();
	void SetRefreshCount(long);
	BOOL GetEnabled();
	void SetEnabled(BOOL);
	OLE_HANDLE GetHWnd();
	void SetHWnd(OLE_HANDLE);
	short GetAppearance();
	void SetAppearance(short);
	double GetMinWidth();
	void SetMinWidth(double);
	long GetMousePointer();
	void SetMousePointer(long);
	BOOL GetFullRedrawOnPan();
	void SetFullRedrawOnPan(BOOL);
	VARIANT GetCoordinateSystem();
	void SetCoordinateSystem(const VARIANT&);
	long GetMaxFileBuffer();
	void SetMaxFileBuffer(long);
	long GetWindowMode();
	void SetWindowMode(long);
	double GetRotationAngle();
	void SetRotationAngle(double);
	LPDISPATCH GetVisibleRegion();
	void SetVisibleRegion(LPDISPATCH);

// Operationen
public:
	void Pan();
	CMoPoint ToMapPoint(float X, float Y);
	void FlashShape(LPDISPATCH shape, short nTimes);
	void Refresh();
	void DrawShape(LPDISPATCH shape, LPDISPATCH Symbol);
	void DrawText(LPCTSTR text, LPDISPATCH shape, LPDISPATCH Symbol);
	CMoEllipse TrackCircle();
	CMoLine TrackLine();
	CMoRectangle TrackRectangle();
	void FromMapPoint(LPDISPATCH Point, float* X, float* Y);
	double ToMapDistance(float distance);
	float FromMapDistance(double distance);
	CMoPolygon TrackPolygon();
	void OutputMap(long hDC);
	void PrintMap(LPCTSTR docName, LPCTSTR outputFile, BOOL landscapeOrientation);
	void ExportMap(long exportType, LPCTSTR outputFile, double scaleFactor);
	void CopyMap(double scaleFactor);
	void CenterAt(double X, double Y);
	void RefreshLayer(short index, const VARIANT& rect);
	void ExportMap2(long exportType, LPCTSTR outputFile, double scaleFactor, const VARIANT& useSourceDepth);
	void OutputMap2(long hDC, long X, long Y, long Width, long Height, const VARIANT& DrawFlags);
	void RefreshRect(LPDISPATCH rect);
	void EnableTIFFLZW(LPCTSTR licenseCode);
	void EnableGIF(LPCTSTR licenseCode);
	void AboutBox();
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_MAP_H__CC25FE50_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
