#if !defined(AFX_MOMAPLAYER_H__CC25FE45_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOMAPLAYER_H__CC25FE45_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
class CMoRectangle;
class CMoRecordset;
class CMoSymbol;
class CMoGeoDataset;
class CMoStrings;

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoMapLayer 

class CMoMapLayer : public COleDispatchDriver
{
public:
	CMoMapLayer() {}		// Ruft den Standardkonstruktor für COleDispatchDriver auf
	CMoMapLayer(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoMapLayer(const CMoMapLayer& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	CString GetName();
	void SetName(LPCTSTR);
	CMoRectangle GetExtent();
	void SetExtent(LPDISPATCH);
	BOOL GetVisible();
	void SetVisible(BOOL);
	CMoRecordset GetRecords();
	void SetRecords(LPDISPATCH);
	CMoSymbol GetSymbol();
	void SetSymbol(LPDISPATCH);
	CMoGeoDataset GetGeoDataset();
	void SetGeoDataset(LPDISPATCH);
	long GetLayerType();
	void SetLayerType(long);
	CMoRectangle GetAreaOfInterest();
	void SetAreaOfInterest(LPDISPATCH);
	LPDISPATCH GetRenderer();
	void SetRenderer(LPDISPATCH);
	CString GetTag();
	void SetTag(LPCTSTR);
	long GetShapeType();
	void SetShapeType(long);
	BOOL GetValid();
	void SetValid(BOOL);
	BOOL GetIndexed();
	void SetIndexed(BOOL);
	VARIANT GetCoordinateSystem();
	void SetCoordinateSystem(const VARIANT&);
	VARIANT GetGeographicTransformation();
	void SetGeographicTransformation(const VARIANT&);
	double GetDensificationTolerance();
	void SetDensificationTolerance(double);
	VARIANT GetFilterShape();
	void SetFilterShape(const VARIANT&);
	long GetFilterOperator();
	void SetFilterOperator(long);
	CString GetFilterExpression();
	void SetFilterExpression(LPCTSTR);
	CMoStrings GetFilterFields();
	void SetFilterFields(LPDISPATCH);
	long GetFilterOrder();
	void SetFilterOrder(long);

// Operationen
public:
	CMoRecordset SearchExpression(LPCTSTR expression);
	BOOL AddRelate(LPCTSTR toField, LPDISPATCH Table, LPCTSTR fromField, const VARIANT& CheckFields);
	void RemoveRelates();
	CMoRecordset SearchByDistance(LPDISPATCH shape, double distance, LPCTSTR expression);
	CMoRecordset SearchShape(LPDISPATCH shape, long searchMethod, LPCTSTR expression);
	BOOL BuildIndex(BOOL force);
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_MOMAPLAYER_H__CC25FE45_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
