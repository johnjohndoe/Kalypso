// MapLayer.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef MAPLAYER_H
#define MAPLAYER_H

#include "..\..\wspprj\wspprj.h"

#include "commonMfc/include/variant_helper.h"
#include "maprend.h"
#include "layer.h"
#include "mapObject.h"

struct Triple;

typedef CMap<CString, LPCTSTR, CComVariant, CComVariant&> CMapStringToVariant;

class CMapLayer : public CLayer
{
	DECLARE_SERIAL(CMapLayer);

public:
  class InvalidLayerTypeException{};

protected:
  	CMapLayer(); // soll nur beim dynamischen Kreieren benutzt werden

public:
  CMapLayer( const CString& strBaseDirectory );
	CMapLayer( const CString& strBaseDirectory, const LPDISPATCH pDispatch );
	CMapLayer( const CString& strBaseDirectory, const CMapLayer& dispatchSrc );
	~CMapLayer();

  CMapLayer* Copy( const CString& newFileName );
  
  BOOL CreateDispatch(LPCTSTR lpszProgID, COleException* pError = NULL);

  virtual BOOL ShowPropertyDialog( CMoMap& pMap, CWnd* pWnd = NULL );
  virtual void SerializeProperties( CArchive& ar, BOOL bOnlyProps );

	enum LabelType
	{
		standard,
		withoutOverlap
	};

protected:
	CMoMapLayer m_mapLayer;

// CMoMapLayer Attributes
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
	void SetGeoDataset( LPDISPATCH );
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

  void RenderRamp( const CString& fieldName );

  void AddProfilIDField( const CString& fieldName );

// CMoMapLayer Operations
public:
	CMoRecordset SearchExpression(LPCTSTR expression);
	BOOL AddRelate(LPCTSTR toField, LPDISPATCH Table, LPCTSTR fromField, const VARIANT& CheckFields);
	void RemoveRelates();
	CMoRecordset SearchByDistance(LPDISPATCH shape, double distance, LPCTSTR expression);
	CMoRecordset SearchByDistance(CMoPoint& point, double distance, LPCTSTR expression);
	CMoRecordset SearchShape(LPDISPATCH shape, long searchMethod, LPCTSTR expression);
	BOOL BuildIndex(BOOL force);
  long SearchNextPointByDistance( CMoPoint& point, const double distance, DWORD &data, LPCTSTR expression = TEXT("") );

// Operations
  TripleArray* CutWithLine( CMoLine& line, const CStringArray& attribute, LPDISPATCH extraObject = NULL, 
                             CProgressCtrl* progressCtrl = NULL );
  void GetSchnittpunkte( CMapObjectArray&, CMapLayer* otherLayer, const CString& sqlStr, CProgressCtrl* pCtrl );

  BOOL GetBorderInfo( CList<long, long>& pointList, CArray<CMoPoint, CMoPoint&>& moPointList, CArray<double, double>& pointYList, CUIntArray& nextPointID, CTypedPtrArray<CPtrArray, CUIntArray*>& pointsByProfil, BOOL bDeleteData = FALSE );
  BOOL GetBorderPolygon( CMoPolyArray& polygone );
	void ApplyRenderer(CMoMap& map, BOOL bForce);

  void GetObjects( CMapObjectArray& presentObjects, CProgressCtrl* pCtrl );

  void GetProfilData( CMapObjectArray& newSchnittObjects, CProgressCtrl* pCtrl );

  void AddObjects( CMapObjectArray& newObjects, CProgressCtrl* pCtrl );
  void RemoveObjects( CMapObjectArray& deleteObjects, CProgressCtrl* pCtrl );

  BOOL GetDatablocks( CTypedPtrMap<CMapWordToPtr, WORD, DataBlockArray*>& dataBlocks, CProgressCtrl* progress );

  bool TestLayerType( const LayerType& type );
  CMapObject SearchFirstObject( LPDISPATCH shape, long searchMethod, LPCTSTR expression );

// Attributes
public:
	BOOL ClearRecordset();
	int SetFieldValByID(long lFeatureID, LPCTSTR fieldName, VARIANT value);
	int DeleteObject(long lFeatureID);
	long SearchNextObjectByDistance(CMoPoint point, double distance, LPCTSTR expression = TEXT(""));
	COleVariantEx GetFieldValByID( const long lFeatureID, LPCTSTR fieldName );
	int MovePoint( CMoPoint& ptTargetPoint, const long lFeatureID, const DWORD dwPointData = 0 );
  int InsertPoint( CMoPoint& ptTargetPoint, const long lFeatureID, const DWORD dwPointData );
	BOOL SetGeoDatasetName( const CString& path );
	void SetGeoDatasetName( const CString& path, const short shapeType, CMoTableDesc& tableDesc );
	void SetColor(COLORREF color);
	COLORREF GetColor();
	void SetOutlineColor(COLORREF color);
	COLORREF GetOutlineColor();
  void SetOutline( BOOL bOutline );
  BOOL GetOutline();
	void SetStyle(int style);
	int GetStyle();
	void SetSize(int size);
	int GetSize();
	void SetMapRenderer( CMapRenderer* pRenderer );
	CMapRenderer* GetMapRenderer() { return m_pMapRenderer; }

  std::map<long, CString> GetIDToFileMap();
  std::map<CString,long> GetFileToIDMap();

  BOOL GetAttributes( const long featureID, CMapStringToVariant& attributes );
  BOOL SetAttributes( const long featureID, const CMapStringToVariant& attributes );

  void GetTableNames( CStringArray& fieldNames );
  void GetFieldValues( const CString& fieldName, CStringArray& fieldValues );

  BOOL FindNextPoint( CMoPoint& projPoint, const long profilID, CMapStringToVariant& attribute );

protected:
	CMapRenderer *m_pMapRenderer;
}; // class CMapLayer

#endif // MAPLAYER_H