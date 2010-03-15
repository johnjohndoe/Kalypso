#if !defined _MAP_HELPER_H
#define _MAP_HELPER_H
//////////////////////////////////////////////////////////////////////////
// MapHelper.h - Utility routines for working with the MapObjects control
// 

///////////////////////////////////////////////////////////////////////////
// Interesting Macros
#include "commonMfc\include\oleDispDriverEx.h"
#include "mapObject.h"

struct ObjectQuery;
class CMoFields;
class CMoTableDesc;
typedef CMap<CString, LPCTSTR, CComVariant, CComVariant&> CMapStringToVariant;

class CMapHelper
{
public:
	static CMoLine CreateSegment( CMoPoint& p1, CMoPoint& p2 );
	static void WriteObjectQueryToFields( const ObjectQuery& oq, CMoFields& fields );
	static void WriteTableDescToObjectQuery( CMoTableDesc& tdesc, ObjectQuery& pOQ, const CMapStringToVariant& attributes );
	static void WriteFieldsToObjectQuery( CMoFields& fields, ObjectQuery& pOQ );
	static void CMapHelper::ReadObjects( CMapObjectArray& list, CMoRecordset& records, CProgressCtrl* pCtrl );
};

//
// Perform precise integer division.  Cast the result to the type of
// integer that you need.
//
#define DIV(a, b)       (((a)*(b) < 0) ? (((double)(a) / (double)(b)) - 0.5)  \
                                       : (((double)(a) / (double)(b)) + 0.5))
#define MULT(a, b)      (((a)*(b) < 0) ? (((double)(a) * (double)(b)) - 0.5)  \
                                       : (((double)(a) * (double)(b)) + 0.5))
#define MULDIV(a, b, c) (((a)*(b)*(c) < 0) ?                                  \
                           (((double)(a) * (double)(b) / (double)(c)) - 0.5)  \
                         : (((double)(a) * (double)(b) / (double)(c)) + 0.5))

//
// Convert from radians to degrees and back
//
#define RAD2DEG(r) ((r)*57.2957795131)
#define DEG2RAD(d) ((d)/57.2957795131)


LPDISPATCH GetIDispatchFromCWnd(CWnd* pWnd);

//
// Iterate through collections.  
//
//	ObjType	The type of a single object, i.e., "CMoMapLayer"
//	obj		The iterator variable.  It's attached to each object in the
//			collection in turn.
//	objs	A reference to the collection, i.e., "const CMoLayers layers".
//
// The following code demonstrates the use of the macros:
//
//	CMoLayers layers(m_map.GetLayers());
//	FOR_EACH_IN(CMoMapLayer, layer, layers)
//	  TRACE("%s\n", layer.GetName());
//	END_FOR
//
#define FOR_EACH_IN(ObjType, obj, objs)									\
	LPUNKNOWN pUnk;                                 \
	objs.GetProperty(0xfffffffc, VT_UNKNOWN, (void*)&pUnk);    \
	ASSERT(pUnk);														\
	IEnumVARIANT* pEnumVariant = 0;										\
	pUnk->QueryInterface(IID_IEnumVARIANT, (void**)&pEnumVariant);		\
	pUnk->Release();													\
	ULONG c;															\
	VARIANT v;															\
	ObjType obj;														\
	while (pEnumVariant->Next(1, &v, &c) == 0 && c == 1)				\
	{																	\
		obj.AttachDispatch(v.pdispVal);									

#define END_FOR															\
	}																	\
	pEnumVariant->Release();


///////////////////////////////////////////////////////////////////////////
// Utilities
//
void DeleteShapeFiles( const CStringArray& fileNames );

///////////////////////////////////////////////////////////////////////////
// Conversion
//
CMoPoint YKoordToGeokoords( CArray<double, double>& rWerte,
                            CArray<double, double>& hWerte, 
                            CArray<double, double>& yWerte, 
                            CMap<double, double, int, int>& yIndex, double yKoord );


///////////////////////////////////////////////////////////////////////////
// Miscellaneous
//
//
//
// Coordinate Transformation - Use these if MapObjects doesn't return
// accurate results in the container you're using.
//
// FullExtent - gibt den Umriss zweier Rechtecke zurück
#define MO2CREATE(object, text) VERIFY(object.CreateDispatch(LPCSTR(CString("MapObjects2.") + text )));
CMoRectangle FullExtent(CMoRectangle rect1, CMoRectangle rect2);
CMoPoints GetPointsFromObject(COleDispatchDriverEx object);

///////////////////////////////////////////////////////////////////////////
// GeoDatasets
//
void SetValue(CMoFields& fields, LPCTSTR name, const LONG value);
void SetValue(CMoFields& fields, LPCTSTR name, const double value);
void SetValue(CMoFields& fields, LPCTSTR name, const COleDateTime& value);
void SetValue(CMoFields& fields, LPCTSTR name, const LPCTSTR value);
void SetValue(CMoFields& fields, LPCTSTR name, const BOOL value);
void SetValue(CMoFields& fields, LPCTSTR name, const LPDISPATCH value);

///////////////////////////////////////////////////////////////////////////
// Printing and Export
//
//
void FrameMapToScale(CMoMap& map, CDC* pDC, const CRect& dstRect, long UserScale,const short MapUnits);


#endif

/////////////////////////////////////////////////////
/// Debug
/////////////////////////////////////////////////////

void AfxFormattedMessageBox(CString text, ...);
