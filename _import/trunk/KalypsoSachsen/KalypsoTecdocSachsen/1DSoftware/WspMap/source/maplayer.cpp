#pragma warning(disable:4786)
#pragma warning(disable:4503)

#include "stdafx.h"

#include "bce\include\WSPFeatures.h"

#include "wspprj\wspprj.h"
#include "wspprj\include\triple.h"
#include "commonMfc/include/mfcHelper.h"
#include "commonMfc/include/variant_helper.h"


#include "resource.h"

#include "mofont.h"
#include "maphelper.h"
#include "mlpdlg.h"  // CMapLayerPropertyDlg

#include "maplayer.h"


#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

////////////////////////////
//  Klasse  CMapLayer
///////////////////////////

IMPLEMENT_SERIAL(CMapLayer, CLayer, VERSIONABLE_SCHEMA|2)

CMapLayer::CMapLayer() : CLayer()
{
  m_pMapRenderer = new CMapRenderer;
  m_nType = user_RO;
}

CMapLayer::CMapLayer( const CString& strBaseDirectory ) : CLayer( strBaseDirectory )
{
  m_pMapRenderer = new CMapRenderer;
  m_nType = user_RO;
}


CMapLayer::CMapLayer( const CString& strBaseDirectory, const LPDISPATCH pDispatch ) : 
CLayer( strBaseDirectory, pDispatch, FALSE )
{
  m_pMapRenderer = new CMapRenderer;
  m_mapLayer.AttachDispatch(pDispatch);
  m_nType = user_RO;
}

CMapLayer::CMapLayer( const CString& strBaseDirectory, const CMapLayer& dispatchSrc ) : 
CLayer( strBaseDirectory, dispatchSrc, FALSE )
{
  m_pMapRenderer = new CMapRenderer(*dispatchSrc.m_pMapRenderer);
  m_mapLayer.AttachDispatch(dispatchSrc.m_mapLayer.m_lpDispatch);
  
  m_nType = dispatchSrc.m_nType;
}

CMapLayer::~CMapLayer()
{
  delete m_pMapRenderer;
}

CMapLayer* CMapLayer::Copy( const CString& newFileName )
// Kopiert den Layer in eine neue Datei und legt einen neuen Layer an
//
// Parameter:
//        const CString&: newFileName: absoluter oder relativer Pfad der neuen Datei ( ohne .shp )
//        const LayerType& type: der neue Layer bekommt diesen Typ, falls die Attribute nicht passen,
 //                     gibts 'ne Meldung und der Layer wird nicht kopiert
// Rückgabewert:
//        der neue ( kopierte ) Layer, NULL bei Misserfolg
{
  if ( LoadIsValid() )
  {
    CString oldBase = BCE::MfcHelper::GetFileWOExt( GetFullGeoDatasetName() );
    CString newBase;
    if ( newFileName.Find(':') == -1 )
      newBase = BCE::MfcHelper::GetFileWOExt( m_strBaseDirectory + newFileName );
    else
      newBase = BCE::MfcHelper::GetFileWOExt( newFileName );
    
    BOOL bSuccess = CopyFile( oldBase + ".shp", newBase + ".shp", TRUE );
    if ( bSuccess )
      bSuccess = bSuccess && CopyFile( oldBase + ".shx", newBase + ".shx", TRUE );
    if ( bSuccess )
      bSuccess = bSuccess && CopyFile( oldBase + ".dbf", newBase + ".dbf", TRUE );
    
    if ( bSuccess )
    {
      CMapLayer* newLayer = new CMapLayer( m_strBaseDirectory );
      newLayer->SetGeoDatasetName( newFileName );
      if ( newLayer->LoadIsValid() )
        return newLayer;
    };
  };
  
  return NULL;
}; // Copy


BOOL CMapLayer::CreateDispatch(LPCTSTR lpszProgID, COleException* pError)
{
  BOOL bRet;
  bRet = m_mapLayer.CreateDispatch(lpszProgID, pError);
  if (bRet)
    m_dispatchDriver.AttachDispatch(LPDISPATCH(m_mapLayer), FALSE);
  return bRet;
}

void CMapLayer::SerializeProperties( CArchive& ar, BOOL bOnlyProps  )
{
  if( ar.IsStoring() )
  {
    if( bOnlyProps )
    {
      CRuntimeClass* rClass = GetRuntimeClass();
      ar << ( VERSIONABLE_SCHEMA ^ rClass->m_wSchema );
    }

    ar << (DWORD)GetColor();
    ar << (WORD)GetStyle();
    ar << (DWORD)GetOutlineColor();
    ar << GetOutline();
    ar << (WORD)GetSize();
    ar << m_pMapRenderer;
  }
 	else
  {
    int nVersion;

    if( bOnlyProps )
      ar >> nVersion;
    else
      nVersion = ar.GetObjectSchema();

    ar.SetObjectSchema( nVersion );
    switch( nVersion )
    {
    case 0:
    case 1:
    case 2:
      {
        DWORD color, outlineColor;
        WORD style, size;
        BOOL outline = TRUE;
        
        ar >> color;
        ar >> style; 
        ar >> outlineColor;
        if ( nVersion >= 2 )
          ar >> outline;
        ar >> size; 
        if( LoadIsValid() )
        {
          SetColor( (COLORREF)color );
          SetStyle( (int)style );
          SetOutlineColor( (COLORREF)outlineColor );
          SetOutline( outline );
          SetSize( (int)size );
        };

        delete m_pMapRenderer;
        m_pMapRenderer = NULL;	// prepare for exception
        ar >> m_pMapRenderer;
      }
      break;
      
    default:
      AfxThrowArchiveException(CArchiveException::badSchema);
      break;
    }
  }
}

void CMapLayer::SetMapRenderer( CMapRenderer* pRenderer )
{
  delete m_pMapRenderer;

  m_pMapRenderer = pRenderer;
}

void CMapLayer::ApplyRenderer( CMoMap& map, BOOL bForce )
{
  if( bForce || ( m_pMapRenderer->GetRendererType() == CMapRenderer::advancedLabel && !m_pMapRenderer->GetBool( 3 ) ) )
  {
    CMoRecordset recs( m_mapLayer.GetRecords() );
    CMoFields fields( recs.GetFields() );
    CStringArray fieldNames;
    
    FOR_EACH_IN( CMoField, field, fields )
      fieldNames.Add( field.GetName() );
    END_FOR
      
    switch( m_pMapRenderer->GetRendererType() )
    {
        case CMapRenderer::singleSymbol:
          m_mapLayer.SetRenderer( NULL );
          break;
          
        case CMapRenderer::valueMap:
          {
            CMoValueMapRenderer renderer;
            VERIFY( renderer.CreateDispatch(TEXT("MapObjects2.ValueMapRenderer") ) );
            m_mapLayer.SetRenderer( renderer );
            
            renderer.SetField( m_pMapRenderer->GetField( 0 ) );
            renderer.SetUseDefault( TRUE );
            
            CMoSymbol symbol( GetSymbol() );
            renderer.SetSymbolType( symbol.GetSymbolType() );
            CMoSymbol defSymb( renderer.GetDefaultSymbol() );
            short symbStyle = GetStyle();
            short symbSize = GetSize();
            COLORREF outlineColor = GetOutlineColor();
            
            defSymb.SetColor( GetColor() );
            defSymb.SetStyle( symbStyle );

            BOOL useOutline = m_pMapRenderer->GetBool( 0 ) == FALSE;


            if( symbol.GetSymbolType() != moLineSymbol )
            {
              defSymb.SetOutlineColor( outlineColor );
              defSymb.SetOutline(  useOutline );
            }
            defSymb.SetSize( symbSize );
            
            CMoField field( fields.Item( COleVariant( m_pMapRenderer->GetField( 0 ) ) ) );
            if( LPDISPATCH( field ) )
            {
              CMap<CString, LPCSTR, int, int> stringMap;
              
              long fieldType = field.GetType();
              while( !recs.GetEof() )
              {
                CString str = field.GetValueAsString();
                stringMap.SetAt( str, 0 );
                recs.MoveNext();
              }
              short count1 = 0;
              POSITION pos1 = stringMap.GetStartPosition();
              while( pos1 != NULL )
              {
                short count2 = 0;
                CString str;
                int i;
                stringMap.GetNextAssoc( pos1, str, i );
                POSITION pos2 = stringMap.GetStartPosition();
                while( pos2 != NULL )
                {
                  CString comp;
                  stringMap.GetNextAssoc( pos2, comp, i );
                  if( pos2 != pos1 )
                  {
                    switch( fieldType )
                    {
                    default:
                      count2 = count1;
                      pos2 = NULL;	// don't need to search
                      break;
                      
                    case moLong:
                    case moPoint:
                    case moLine:
                    case moPolygon:
                      {
                        long d1 = atoi( str );
                        long d2 = atoi( comp );
                        if( d1 > d2 )
                          count2++;
                      }
                      break;
                      
                    case moDouble:
                      {
                        double d1 = atof( str );
                        double d2 = atof( comp );
                        if( d1 > d2 )
                          count2++;
                      }
                      break;
                      
                    case moDate:
                      {
                        COleDateTime date1, date2;
                        COleDateTimeSpan diff;
                        
                        date1.ParseDateTime( str );
                        date2.ParseDateTime( comp );
                        diff = date1 - date2;
                        if( diff.GetTotalSeconds() > 0 )
                          count2++;
                      }
                      break;
                      
                    case moString:
                      {
                        if( str.CompareNoCase( comp ) > 0 )
                          count2++;
                      }
                      break;
                      
                    case moBoolean:
                      {
                        if( str == "true" )
                          count2 = 0;
                        else
                          count2 = 1;
                        pos2 = NULL;	// don't need to search
                      }
                      break;
                    }
                  }
                }
                stringMap.SetAt( str, count2 );
                count1++;
              }
              
              renderer.SetValueCount( stringMap.GetCount() );
              pos1 = stringMap.GetStartPosition();
              while( pos1 != NULL )
              {
                CString str;
                int i;
                stringMap.GetNextAssoc( pos1, str, i );
                renderer.SetValue( i, str );
                CMoSymbol valSymb( renderer.GetSymbol( i ) );
                valSymb.SetStyle( symbStyle );
                valSymb.SetSize( symbSize );
                if( symbol.GetSymbolType() != moLineSymbol )
                {
                  valSymb.SetOutline(  useOutline );
                  valSymb.SetOutlineColor( outlineColor );
                }
                if( i < m_pMapRenderer->GetNumValueColors() )
                  valSymb.SetColor( m_pMapRenderer->GetValueColor( i ) );
              };
            }; // if field
          }
          break;
          
        case CMapRenderer::classBreaks:
          {
            CMoClassBreaksRenderer renderer;
            VERIFY(renderer.CreateDispatch(TEXT("MapObjects2.ClassBreaksRenderer")));
            m_mapLayer.SetRenderer(renderer);
            
            renderer.SetField(m_pMapRenderer->GetField(0));
            
            if( m_pMapRenderer->GetField(0).IsEmpty() )
              break;
            
            CMoSymbol symbol( GetSymbol() );
            renderer.SetSymbolType( symbol.GetSymbolType() );
            short symbStyle = GetStyle();
            short symbSize = GetSize();
            COLORREF outlineColor = symbol.GetOutlineColor();
            BOOL useOutline  = m_pMapRenderer->GetBool( 0 ) == FALSE;
            
            CMoField field( fields.Item(COleVariant(m_pMapRenderer->GetField(0))) );
            CArray<double, double> valArray;
            long fieldType = field.GetType();
            ASSERT(fieldType==moLong || fieldType==moDouble);
            while (!recs.GetEof())
            {
              double dVal;
              BOOL bInserted = FALSE;
              
              CString str = field.GetValueAsString();
              dVal = atof( str );
              for( int i = 0; i < valArray.GetSize() && !bInserted; i++ )
              {
                if (dVal==valArray[i])
                  bInserted = TRUE;
                if (dVal<valArray[i])
                {
                  valArray.InsertAt(i, dVal);
                  bInserted = TRUE;
                }
              }
              if (!bInserted)
                valArray.Add(dVal);
              recs.MoveNext();
            }

            renderer.SetBreakCount(m_pMapRenderer->GetConst(0));
            for( int i = 0; i < m_pMapRenderer->GetConst( 0 ) + 1; i++ )
            {
              CMoSymbol brkSymb(renderer.GetSymbol(i));
              brkSymb.SetStyle(symbStyle);
              brkSymb.SetSize(symbSize);
              if( symbol.GetSymbolType()!=moLineSymbol )
              {
                brkSymb.SetOutline( useOutline );
                brkSymb.SetOutlineColor(outlineColor);
              }
              if (i<m_pMapRenderer->GetConst(0)-1)
              {
                short count1 = MulDiv(valArray.GetSize(), i+1, m_pMapRenderer->GetConst(0));
                renderer.SetBreak( i, valArray[count1] );
              }
            }
            renderer.RampColors(m_pMapRenderer->GetColor(0), m_pMapRenderer->GetColor(1));
            renderer.SizeSymbols(m_pMapRenderer->GetConst(1), m_pMapRenderer->GetConst(2));
          }
          break;
          
        case CMapRenderer::standardLabel:
          {
            CMoLabelRenderer renderer;
            VERIFY(renderer.CreateDispatch(TEXT("MapObjects2.LabelRenderer")));
            m_mapLayer.SetRenderer(renderer);
            
            renderer.SetField(m_pMapRenderer->GetField(0));
            renderer.SetXOffsetField(m_pMapRenderer->GetField(1));
            renderer.SetYOffsetField(m_pMapRenderer->GetField(2));
            renderer.SetFittedField(m_pMapRenderer->GetField(3));
            renderer.SetDrawBackground(m_pMapRenderer->GetBool(0));
            renderer.SetAllowDuplicates(m_pMapRenderer->GetBool(1));
            renderer.SetSplinedText(m_pMapRenderer->GetBool(2));
            renderer.SetFlip(m_pMapRenderer->GetBool(3));
            renderer.SetSymbolCount(1);
            
            CMoTextSymbol tsymbol(renderer.GetSymbol(0));
            tsymbol.SetColor(m_pMapRenderer->GetColor(0));
            tsymbol.SetHorizontalAlignment(m_pMapRenderer->GetConst(0));
            tsymbol.SetVerticalAlignment(m_pMapRenderer->GetConst(1));
            tsymbol.SetRotation(m_pMapRenderer->GetConst(2));
            CMoFont font;
            LOGFONT logfont = m_pMapRenderer->GetFont();
            font.SetName( logfont.lfFaceName );
            font.SetSize( abs( logfont.lfHeight ) );
            font.SetWeight((short)logfont.lfWeight);
            font.SetCharSet(logfont.lfCharSet);
            font.SetItalic(logfont.lfItalic);
            font.SetUnderline(logfont.lfUnderline);
            font.SetStrikeThrough(logfont.lfStrikeOut);
            tsymbol.SetFont(font);
          }
          break;
          
        case CMapRenderer::advancedLabel:
          {
            CMoLabelPlacer labelPlacer;
            labelPlacer.CreateDispatch("MapObjects2.LabelPlacer");
            labelPlacer.SetField(m_pMapRenderer->GetField(0));
            labelPlacer.SetDrawBackground(m_pMapRenderer->GetBool(0));
            labelPlacer.SetAllowDuplicates(m_pMapRenderer->GetBool(1));
            labelPlacer.SetUseDefault(TRUE);
            labelPlacer.SetMaskLabels(m_pMapRenderer->GetBool(2));
            labelPlacer.SetMaskColor(m_pMapRenderer->GetColor(1));
            labelPlacer.SetPlaceOn(m_pMapRenderer->GetBool(4));
            labelPlacer.SetPlaceAbove(m_pMapRenderer->GetBool(5));
            labelPlacer.SetPlaceBelow(m_pMapRenderer->GetBool(6));
            
            labelPlacer.SetSymbolHeight(m_pMapRenderer->GetConst(0));
            labelPlacer.SetSymbolWidth(m_pMapRenderer->GetConst(0));
            
            CMoTextSymbol textSym(labelPlacer.GetDefaultSymbol());
            LOGFONT logfont = m_pMapRenderer->GetFont();
            CMoRectangle layerExtent(m_mapLayer.GetExtent());
            double size = fabs(logfont.lfHeight/10)*layerExtent.GetHeight()/1000;
            if( !m_pMapRenderer->GetBool(3) && map.GetSafeHwnd() )
            {
              CMoRectangle mapExtent(map.GetExtent());
              CMoRectangle fullExtent(map.GetFullExtent());
              
              size *= m_pMapRenderer->GetConst(1);
              size *= mapExtent.GetHeight();
              size /= fullExtent.GetHeight();
            }
            else
              size *= m_pMapRenderer->GetConst( 2 );

            textSym.SetHeight(size);
            textSym.SetColor(m_pMapRenderer->GetColor(0));
            
            CMoFont font;
            font.SetName(logfont.lfFaceName);
            font.SetSize(logfont.lfHeight);
            font.SetWeight((short)logfont.lfWeight);
            font.SetCharSet(logfont.lfCharSet);
            font.SetItalic(logfont.lfItalic);
            font.SetUnderline(logfont.lfUnderline);
            font.SetStrikeThrough(logfont.lfStrikeOut);
            textSym.SetFont(font);
            
            m_mapLayer.SetRenderer(labelPlacer);
          }
          break;
    }
  }
}

BOOL CMapLayer::SetGeoDatasetName( const CString& path )
// lädt einen CMoMapLayer aus den durch path spezifizierten Vector - Daten
//
// Rückgabe:
//        FALSE, falls die Datei nicht geladen werden konnte
// Parameter:
//        const CString &path: absoluter oder relativer Pfad auf die Vektordaten 
//                            (mit Datentyp Kennung nach ESRI)
{
  ASSERT( m_strBaseDirectory != "" );
  
  // Pfad immer relativ zum Kartenverzeichnis machen, wenn möglich
  CString relPath = BCE::MfcHelper::GetPathAsRelative( m_strBaseDirectory, path );
  
  CString absolutePath;
  if ( relPath.Find( TCHAR(':') ) == -1 )
    absolutePath = m_strBaseDirectory + relPath;
  else
    absolutePath = relPath;
  
  CString directory = BCE::MfcHelper::GetFileDirectory( absolutePath );
  CString filename = BCE::MfcHelper::GetFileName( absolutePath );
  
  m_strGeoDatasetName = "";
  
  // Establish connection to data
  CMoDataConnection conn;
  VERIFY(conn.CreateDispatch(TEXT("MapObjects2.DataConnection")));
  
  conn.SetDatabase(directory);
  if (!conn.Connect())
    return FALSE;
  
  // Find the geodataset
  CMoGeoDataset geoDataset( conn.FindGeoDataset( filename ) );
  conn.Disconnect(); // nicht mehr länger benötigt
  if( !LPDISPATCH( geoDataset ) )
    return FALSE;
  
 	VERIFY( CreateDispatch( TEXT("MapObjects2.MapLayer") ) );
  SetGeoDataset( geoDataset );
  
  if( !GetValid() )
    return FALSE;
  
  m_strGeoDatasetName = relPath;
  return TRUE;
}

void CMapLayer::SetGeoDatasetName( const CString& path, const short shapeType, CMoTableDesc& tableDesc )
// erzeugt neue Shape files
// Parameter:
//      const CString& path: absoluter oder relativer ( bzgl. strBasedirectory ) Pfad auf das shapeFile
{
  m_strGeoDatasetName = "";
  
  // Pfad immer relativ zum Kartenverzeichnis machen, wenn möglich
  CString relPath = path;
  
  if ( relPath.Find( m_strBaseDirectory ) == 0 )
    relPath = relPath.Mid( m_strBaseDirectory.GetLength() );
  
  CString absolutePath; // absoluter Pfad auf das shapefile
  if ( relPath.Find( TCHAR(':') ) == -1 )
    absolutePath = m_strBaseDirectory + relPath;
  else
    absolutePath = relPath;
  
  // Establish connection to data
  CMoDataConnection conn;
  VERIFY(conn.CreateDispatch(TEXT("MapObjects2.DataConnection")));
  conn.SetDatabase( BCE::MfcHelper::GetFileDirectory( absolutePath ) );
  VERIFY(conn.Connect());
  
  // Delete the GeoDataset if already exists
  LPDISPATCH lpDispatch = LPDISPATCH(conn.FindGeoDataset( BCE::MfcHelper::GetFileTitle( absolutePath ) ) );
  if (lpDispatch!=NULL)
  {
    VERIFY( conn.DeleteGeoDataset( BCE::MfcHelper::GetFileTitle( absolutePath ) ) );
    conn.Disconnect();
    VERIFY(conn.Connect());
  }
  // Add the geodataset
  CMoGeoDataset geoDataset( conn.AddGeoDataset( BCE::MfcHelper::GetFileTitle( absolutePath ), shapeType, tableDesc, 
    CComVariant(1), CComVariant((short)1) ) );
  ASSERT(LPDISPATCH(geoDataset));
 	VERIFY(CreateDispatch(TEXT("MapObjects2.MapLayer")));
  SetGeoDataset(geoDataset);
  conn.Disconnect();
  
  m_strGeoDatasetName = relPath;
}

CString CMapLayer::GetName()
{
  return m_mapLayer.GetName();
}

void CMapLayer::SetName(LPCTSTR propVal)
{
  m_mapLayer.SetName(propVal);
}

CMoRectangle CMapLayer::GetExtent()
{
  return m_mapLayer.GetExtent();
}

void CMapLayer::SetExtent(LPDISPATCH propVal)
{
  m_mapLayer.SetExtent(propVal);
}

BOOL CMapLayer::GetVisible()
{
  return m_mapLayer.GetVisible();
}

void CMapLayer::SetVisible(BOOL propVal)
{
  m_mapLayer.SetVisible(propVal);
}

CMoRecordset CMapLayer::GetRecords()
{
  return m_mapLayer.GetRecords();
}

void CMapLayer::SetRecords(LPDISPATCH propVal)
{
  m_mapLayer.SetRecords(propVal);
}

CMoSymbol CMapLayer::GetSymbol()
{
  return m_mapLayer.GetSymbol();
}

void CMapLayer::SetSymbol(LPDISPATCH propVal)
// setzt das Symbol des Layers
// Bemerkung:
//    diese Funktion wird von CMoMapLayer gar nicht unterstützt, dennoch 
//    taucht sie in der Mantelklasse auf. Hier wird sie nur simuliert
{
  // m_mapLayer.SetSymbol(propVal); // Original -> führt zu einer MO2 - Exception
  CMoSymbol symbol( propVal );
  symbol.m_bAutoRelease = FALSE;
  CMoSymbol mySymbol( GetSymbol() );
  
  ASSERT( mySymbol.GetSymbolType() == symbol.GetSymbolType() );
  //  mySymbol.SetCenterOnAscent( symbol.GetCenterOnAscent() );
  //  mySymbol.SetCharacterIndex( symbol.GetCharacterIndex() );
  mySymbol.SetColor( symbol.GetColor() );
  mySymbol.SetCustom( symbol.GetCustom() );
  //  mySymbol.SetFont( symbol.GetFont() );
  if ( symbol.GetSymbolType() != moLineSymbol )
  {
    mySymbol.SetOutline( symbol.GetOutline() );
    mySymbol.SetOutlineColor( symbol.GetOutlineColor() );
  };
  //  mySymbol.SetRotation( symbol.GetRotation() );
  mySymbol.SetSize( symbol.GetSize() );
  mySymbol.SetStyle( symbol.GetStyle() );
}

CMoGeoDataset CMapLayer::GetGeoDataset()
{
  return m_mapLayer.GetGeoDataset();
}

void CMapLayer::SetGeoDataset(LPDISPATCH propVal)
{
  m_mapLayer.SetGeoDataset(propVal);
}

long CMapLayer::GetLayerType()
{
  return m_mapLayer.GetLayerType();
}

void CMapLayer::SetLayerType(long propVal)
{
  m_mapLayer.SetLayerType(propVal);
}

CMoRectangle CMapLayer::GetAreaOfInterest()
{
  return m_mapLayer.GetAreaOfInterest();
}

void CMapLayer::SetAreaOfInterest(LPDISPATCH propVal)
{
  m_mapLayer.SetAreaOfInterest(propVal);
}

LPDISPATCH CMapLayer::GetRenderer()
{
  return m_mapLayer.GetRenderer();
}

void CMapLayer::SetRenderer(LPDISPATCH propVal)
{
  m_mapLayer.SetRenderer(propVal);
}

CString CMapLayer::GetTag()
{
  return m_mapLayer.GetTag();
}

void CMapLayer::SetTag(LPCTSTR propVal)
{
  m_mapLayer.SetTag(propVal);
}

long CMapLayer::GetShapeType()
{
  return m_mapLayer.GetShapeType();
}

void CMapLayer::SetShapeType(long propVal)
{
  m_mapLayer.SetShapeType(propVal);
}

BOOL CMapLayer::GetValid()
{
  return m_mapLayer.GetValid();
}

void CMapLayer::SetValid(BOOL propVal)
{
  m_mapLayer.SetValid(propVal);
}

BOOL CMapLayer::GetIndexed()
{
  return m_mapLayer.GetIndexed();
}

void CMapLayer::SetIndexed(BOOL propVal)
{
  m_mapLayer.SetIndexed(propVal);
}

VARIANT CMapLayer::GetCoordinateSystem()
{
  return m_mapLayer.GetCoordinateSystem();
}

void CMapLayer::SetCoordinateSystem(const VARIANT& propVal)
{
  m_mapLayer.SetCoordinateSystem(propVal);
}

VARIANT CMapLayer::GetGeographicTransformation()
{
  return m_mapLayer.GetGeographicTransformation();
}

void CMapLayer::SetGeographicTransformation(const VARIANT& propVal)
{
  m_mapLayer.SetGeographicTransformation(propVal);
}

double CMapLayer::GetDensificationTolerance()
{
  return m_mapLayer.GetDensificationTolerance();
}

void CMapLayer::SetDensificationTolerance(double propVal)
{
  m_mapLayer.SetDensificationTolerance(propVal);
}

VARIANT CMapLayer::GetFilterShape()
{
  return m_mapLayer.GetFilterShape();
}

void CMapLayer::SetFilterShape(const VARIANT& propVal)
{
  m_mapLayer.SetFilterShape(propVal);
}

long CMapLayer::GetFilterOperator()
{
  return m_mapLayer.GetFilterOperator();
}

void CMapLayer::SetFilterOperator(long propVal)
{
  m_mapLayer.SetFilterOperator(propVal);
}

CString CMapLayer::GetFilterExpression()
{
  return m_mapLayer.GetFilterExpression();
}

void CMapLayer::SetFilterExpression(LPCTSTR propVal)
{
  m_mapLayer.SetFilterExpression(propVal);
}

CMoStrings CMapLayer::GetFilterFields()
{
  return m_mapLayer.GetFilterFields();
}

void CMapLayer::SetFilterFields(LPDISPATCH propVal)
{
  m_mapLayer.SetFilterFields(propVal);
}

long CMapLayer::GetFilterOrder()
{
  return m_mapLayer.GetFilterOrder();
}

void CMapLayer::SetFilterOrder(long propVal)
{
  m_mapLayer.SetFilterOrder(propVal);
}

CMoRecordset CMapLayer::SearchExpression(LPCTSTR expression)
{
  return m_mapLayer.SearchExpression(expression);
}

BOOL CMapLayer::AddRelate(LPCTSTR toField, LPDISPATCH Table, LPCTSTR fromField, const VARIANT& CheckFields)
{
  return m_mapLayer.AddRelate(toField, Table, fromField, CheckFields);
}

void CMapLayer::RemoveRelates()
{
  m_mapLayer.RemoveRelates();
}

CMoRecordset CMapLayer::SearchByDistance(CMoPoint& point, double distance, LPCTSTR expression)
{
  CMoRectangle rect;
  VERIFY(rect.CreateDispatch(TEXT("MapObjects2.Rectangle")));
  rect.SetTop(point.GetY()+distance);
  rect.SetBottom(point.GetY()-distance);
  rect.SetLeft(point.GetX()-distance);
  rect.SetRight(point.GetX()+distance);
  
  return m_mapLayer.SearchShape(rect, moAreaIntersect, expression);
}

CMoRecordset CMapLayer::SearchByDistance(LPDISPATCH shape, double distance, LPCTSTR expression)
{
  return m_mapLayer.SearchByDistance(shape, distance, expression);
}

CMoRecordset CMapLayer::SearchShape(LPDISPATCH shape, long searchMethod, LPCTSTR expression)
{
  return m_mapLayer.SearchShape(shape, searchMethod, expression);
}

BOOL CMapLayer::BuildIndex(BOOL force)
{
  return m_mapLayer.BuildIndex(force);
}

void CMapLayer::SetColor(COLORREF color)
{
  CMoSymbol layerSymbol(GetSymbol());
  layerSymbol.SetColor(color);
}

COLORREF CMapLayer::GetColor()
{
  CMoSymbol layerSymbol(GetSymbol());
  return layerSymbol.GetColor();
}

void CMapLayer::SetOutlineColor(COLORREF color)
{
  CMoSymbol layerSymbol( GetSymbol() );
  if( layerSymbol.GetSymbolType() != moLineSymbol )
    layerSymbol.SetOutlineColor( color );
}

COLORREF CMapLayer::GetOutlineColor()
{
  CMoSymbol layerSymbol(GetSymbol());
  return layerSymbol.GetOutlineColor();
}

void CMapLayer::SetOutline( BOOL bOutline )
{
  CMoSymbol layerSymbol( GetSymbol() );
  if ( layerSymbol.GetSymbolType() != moLineSymbol )
    layerSymbol.SetOutline( bOutline );
}; // SetOutline

BOOL CMapLayer::GetOutline()
{
  CMoSymbol layerSymbol( GetSymbol() );
  return layerSymbol.GetOutline();
}; // GetOutline


void CMapLayer::SetStyle(int style)
{
  CMoSymbol layerSymbol(GetSymbol());
  layerSymbol.SetStyle(style);
}

int CMapLayer::GetStyle()
{
  CMoSymbol layerSymbol(GetSymbol());
  return layerSymbol.GetStyle();
}

void CMapLayer::SetSize(int size)
{
  CMoSymbol layerSymbol(GetSymbol());
  layerSymbol.SetSize(size);
}

int CMapLayer::GetSize()
{
  CMoSymbol layerSymbol(GetSymbol());
  return layerSymbol.GetSize();
}

long CMapLayer::SearchNextPointByDistance( CMoPoint &point, const double distance, DWORD &data, LPCTSTR expression /* = TEXT("") */)
//
// finde einen nächstgelegenen Punkt im Layer
//
// Argumente:
//            point:    - enthält Koordinaten des Punktes in desssen Nähe der zu findende Punkt liegen soll
//            distance: - Radius der Suche (in Karteneinheiten)
//            data:     - falls der ShapeType dieses Layers Punkt ist wird 0 zurückgegeben
//                      - falls der ShapeType dieses Layers moLine oder moPolygon ist enthält das LOWORD die 
//                        Nummer des gefundenen Punktes in der Pointskollektion mit Nummer 'HIWORD' in der
//                        Partskollection der Linie in welcher er liegt
// Rückgabewert:
//            FeatureID des gefundenen GeoObjectes (im Falle eines Punkte - Layers der Punkt, sonst die Linie,
//             das Polygon in welcher der gefundene Punkt liegt. In data steht dann, wie man den Punkt findet).
//            -1 falls kein Punkt gefunden wurde
//
{
  LONG featureID = -1;
  data = 0;
  
  CMoRecordset selectedRecords( m_mapLayer.SearchByDistance( point, distance, expression ) );
  
  double abstand = distance;
  
  CMoFields fields(selectedRecords.GetFields());
  
  while (!selectedRecords.GetEof())
  {
    CMoField  shapeField(fields.Item(COleVariant(MO2_FIELD_SHAPE)));
    CMoField  featureField(fields.Item(COleVariant(MO2_FIELD_FEATUREID)));
    
    long ShapeType = GetShapeType();
    switch (ShapeType)
    {
    case moShapeTypePoint:
      {
        CMoPoint tmp_punkt( shapeField.GetValue().pdispVal );
        double tmp_abst = point.DistanceTo( tmp_punkt );
        if( tmp_abst < abstand)
        {
          featureID = long( COleVariantEx( featureField.GetValue() ) );
          abstand = tmp_abst;
        };
      };
      break;
      
    case moShapeTypeLine:
    case moShapeTypePolygon:
      {
        CMoParts parts;
        LPDISPATCH dispatch = shapeField.GetValue().pdispVal;
        if( ShapeType = moShapeTypeLine )
        {
          CMoLine line(dispatch);
          parts = line.GetParts();
        }
        else
        {
          CMoPolygon poly(dispatch);
          parts = poly.GetParts();
        };
        for( int i = 0; i < parts.GetCount(); i++ )
        {
          CMoPoints points(parts.Item(COleVariant((short)i)));
          for (int j = 0; j < points.GetCount(); j++)
          {
            CMoPoint tmp_punkt( points.Item(COleVariant((short)j)) );
            double tmp_abst = point.DistanceTo( tmp_punkt );
            if( tmp_abst < abstand )
            {
              featureID = long( COleVariantEx( featureField.GetValue() ) );
              data = ((DWORD)(i << 16)) | (j & 0xffff);
              abstand = tmp_abst;
            }; 
          }; // for j
        }; // for i
      };
      break;
      
    default:
      ASSERT(FALSE);  // darf nicht sein
    }; // switch
    
    selectedRecords.MoveNext();
  }; // while
  
  return featureID;
}

int CMapLayer::MovePoint( CMoPoint& ptTargetPoint, const long lFeatureID, const DWORD dwPointData /* = 0 */ )
// verschiebt ein (Punkt-)Objekt innerhalb des MapLayers
//
//  Parameter:
//          CMoPoint ptTagetPoint:  Zielkoordinaten des zu verschiebenden Punktes
//          long lFeatureID: ID des zu verädnernden Objektes
//          DWORD dwPointData: falls der Layer kein Punkte Layer ist, Daten zur Identifikation des Punktes innerhalb der Line 7 des Polygons:
//                              HIWORD(dwPointData): Index innerhabl der Parts
//                              LOWORD(dwPointData): Index innerhalb der Points
//  Rückgabewert:
//          Fehlercode:   0 kein Fehler
//                        1 recordset ist nicht updatable
//                        2 Eintrag mit Nummer lFeatureID nicht gefunden
//                        3 Feld ist nicht editierbar (EditMode != 1)
//                       10 Layer hat falschen ShapeType
//
// Bemerkungen:
//          Je nach Typ des Layers wird entweder ein Object (ein Punkt) verschoben und
//          z.B. Profilbezogene Daten entsprechend verändert oder ein Punkt innerhalb einer
//          Linie / eines Polygons verschoben
//
{
  LPDISPATCH sourceShape = GetFieldValByID(lFeatureID, MO2_FIELD_SHAPE).pdispVal; // dispatch des zu verändernden Shape-Objekts
  COleDispatchDriver targetShape;  // verändertes Shape-Objekt
  
  switch (GetShapeType())
  {
  case moShapeTypePoint:
    targetShape = ptTargetPoint;
    break;
    
  case moShapeTypeLine:
    {
      CMoLine line(sourceShape);
      CMoParts parts(line.GetParts());
      CMoPoints points(parts.Item(CComVariant(HIWORD(dwPointData))));
      points.Set(LOWORD(dwPointData), ptTargetPoint);
      targetShape = line;
    };
    break;
    
  case moShapeTypePolygon:
    {
      CMoPolygon polygon(sourceShape);
      CMoParts parts(polygon.GetParts());
      CMoPoints points(parts.Item(CComVariant(HIWORD(dwPointData))));
      points.Set(LOWORD(dwPointData), ptTargetPoint);
      targetShape = polygon;
    };
    break;
    
  default:
    return 10; // falscher ShapeType
  }; // switch ShapeType
  
  return SetFieldValByID(lFeatureID, MO2_FIELD_SHAPE, CComVariant(targetShape));
}

int CMapLayer::InsertPoint( CMoPoint& ptTargetPoint, const long lFeatureID, const DWORD dwPointData )
// fügt einen Punkt in eine Linie/Poly eines Objekts des MapLayers ein
//
//  Parameter:
//          CMoPoint& ptTagetPoint:  Zielkoordinaten des zu verschiebenden Punktes
//          long lFeatureID: ID des zu verädnernden Objektes
//          DWORD dwPointData: falls der Layer kein Punkte Layer ist, Daten zur Identifikation des Punktes innerhalb der Line des Polygons:
//                              HIWORD(dwPointData): Index innerhabl der Parts
//                              LOWORD(dwPointData): Index innerhalb der Points
//  Rückgabewert:
//          Fehlercode:   0 kein Fehler
//                        1 recordset ist nicht updatable
//                        2 Eintrag mit Nummer lFeatureID nicht gefunden
//                        3 Feld ist nicht editierbar (EditMode != 1)
//                       10 Layer hat falschen ShapeType
//
{
  LPDISPATCH sourceShape = GetFieldValByID( lFeatureID, MO2_FIELD_SHAPE).pdispVal; // dispatch des zu verändernden Shape-Objekts
  COleDispatchDriver targetShape;  // verändertes Shape-Objekt
  
  CMoPoint newPoint;
  MO2CREATE( newPoint, "Point" );
  newPoint.SetX( ptTargetPoint.GetX() );
  newPoint.SetY( ptTargetPoint.GetY() );

  switch( GetShapeType() )
  {
  case moShapeTypeLine:
    {
      CMoLine line( sourceShape );
      CMoParts parts( line.GetParts() );
      CMoPoints points( parts.Item( CComVariant( HIWORD(dwPointData) ) ) );
      points.Insert( LOWORD( dwPointData ), LPDISPATCH( newPoint ) );
      targetShape = line;
    };
    break;
    
  case moShapeTypePolygon:
    {
      CMoPolygon polygon( sourceShape );
      CMoParts parts( polygon.GetParts() );
      CMoPoints points( parts.Item(CComVariant(HIWORD(dwPointData) ) ) );
      points.Insert( LOWORD( dwPointData ), LPDISPATCH( newPoint ) );
      targetShape = polygon;
    };
    break;
    
  default:
    return 10; // falscher ShapeType
  }; // switch ShapeType
  
  return SetFieldValByID( lFeatureID, MO2_FIELD_SHAPE, CComVariant( targetShape ) );
}



COleVariantEx CMapLayer::GetFieldValByID( const long lFeatureID, LPCTSTR fieldName )
//
// gibt Feldinhalt eines Objektes zurück
//
// Parameter:
//          long lFeatureID:    ID des gesuchten Objektes
//          LPCTSTR fieldName:  Feldname des gesuchten Feldes
// Rückgabewert:
//          Feldinhalt als Variant, bei Fehler den Typ VT_ERROR
//          
{
  COleVariantEx var;
  var.vt = VT_ERROR;
  
  CMoRecordset records( GetRecords() );
  if( !LPDISPATCH(records) )
    return var;
  
  CMoFields fields( records.GetFields() );
  if( !LPDISPATCH(fields) )
    return var;
  
  CMoField field( fields.Item( COleVariant(fieldName) ) );
  CMoField idField( fields.Item(COleVariant(MO2_FIELD_FEATUREID)) );
  if( !LPDISPATCH(field) || !LPDISPATCH(idField) )
    return var;
  
  while( !records.GetEof() )
  {
    const long id = long( COleVariantEx( idField.GetValue() ) );
    if( id == lFeatureID )
      return COleVariantEx( field.GetValue() );
    
    records.MoveNext();
  }; // while records
  
  // wenn man hier ankommt, ists ein Fehler
  return var;
}

long CMapLayer::SearchNextObjectByDistance( CMoPoint point, double distance, LPCTSTR expression )
//
// finde das  nächstgelegenen Objekt im Layer
//
// Argumente:
//            CMOPoint point:   - enthält Koordinaten des Punktes in desssen Nähe der zu findende Punkt liegen soll
//            double distance:  - Radius der Suche (in Karteneinheiten)
//            LPCTSTR expression: - SQL Ausdruck zur Einschränkung der Objekte
// Rückgabewert:
//            FeatureID des gefundenen GeoObjectes
//            -1 falls kein Punkt gefunden wurde
//
{
  long featureID = -1;

  if( !LPDISPATCH(point) )
    return featureID;

  CMoRecordset selectedRecords( m_mapLayer.SearchByDistance( (LPDISPATCH)point, distance, expression ) );
  if( !LPDISPATCH(selectedRecords) )
    return featureID;
  
  CMoFields fields( selectedRecords.GetFields() );
  if( !LPDISPATCH(fields) )
    return featureID;

  CMoField shapeField( fields.Item(COleVariant(MO2_FIELD_SHAPE) ) );
  CMoField featureField( fields.Item(COleVariant(MO2_FIELD_FEATUREID) ) );
  if( !LPDISPATCH(shapeField) || !LPDISPATCH(featureField) )
    return featureID;
  
  double abstand = distance;
  while( !selectedRecords.GetEof() )
  {
    LPDISPATCH disp = shapeField.GetValue().pdispVal;

    if( disp )
    {
      CMoPoint p( disp );
      double tmp_abst = point.DistanceTo(disp);
    
      if ( tmp_abst < abstand)
      {
        featureID = COleVariantEx( featureField.GetValue() );
        abstand = tmp_abst;
      };
    }

    selectedRecords.MoveNext();
  }; // while
  
  return featureID;
}; // SearchNextObjectByDistance

int CMapLayer::DeleteObject(long lFeatureID)
//
// löscht ein Objekt aus dem Layer
//
//  Parameter:
//          long lFeatureId:  Index des zu löschenden Objektes
//  Rückgabewert:
//            Fehlerkode: 0 keine Fehler
//                        1 Recordset ist nicht Updatable
//                        2 Objekt nicht gefunden
{
  CMoRecordset records(GetRecords());
  if (!records.GetUpdatable())
    return 1;
  
  CMoFields fields( records.GetFields() );
  CMoField idField( fields.Item(COleVariant( MO2_FIELD_FEATUREID ) ) );
  while( !records.GetEof() )
  {
    if( long( COleVariantEx( idField.GetValue() ) ) == lFeatureID )
    {
      records.Edit();
      records.Delete();
      records.MoveNext();  // wirklich löschen -> records.Update() wirft eine EXEPTION
      return 0;
    }
    records.MoveNext();
  };
  return 2;  // Objekt nicht gefunden
}; // CMapLayer::DeleteObject()

int CMapLayer::SetFieldValByID(long lFeatureID, LPCTSTR fieldName, VARIANT value )
//
// setzt Feldinhalt eines Objektes
//
// Parameter:
//          long lFeatureID:    ID des zu verändernden Objektes
//          LPCTSTR fieldName:  Feldname des zu verändernden Feldes
//          VARIANT val:        neuer Wert
// Rückgabewert:
//          Fehlerkode: 0 keine Fehler
//                      1 recordset ist nicht updatable
//                      2 Eintrag mit Nummer lFeatureID nicht gefunden
//                      3 Feld ist nicht editierbar (EditMode != 1)
//                      
//          
{
  CMoRecordset recs( GetRecords() );
  if( !recs.GetUpdatable() )
    return 1;
  
  CMoFields fields( recs.GetFields() );
  CMoField field( fields.Item( COleVariant( fieldName ) ) );
  CMoField idField( fields.Item( COleVariant( MO2_FIELD_FEATUREID ) ) );
  while( !recs.GetEof() )
  {
    if( long( COleVariantEx( idField.GetValue() ) ) == lFeatureID )
    {
      recs.Edit();
      if( recs.GetEditMode() == 1 )
      {
        field.SetValue( value );
        recs.Update();
        return 0;
      }
      else
        return 3;
    };
    recs.MoveNext();
  };
  
  // Feld nicht gefunden
  return 2;
}; // SetFieldValByID

BOOL CMapLayer::ClearRecordset()
// löscht alle Datensätze dieses Layers
// Rückgabewert:
//            TRUE/FALSE je nach Ausgang der Operation
{
  CMoRecordset records(GetRecords());
  if (LPDISPATCH(records) && records.GetUpdatable())
  {
    records.SetAutoFlush(FALSE);
    while (!records.GetEof())
    {
      records.Delete();
      records.MoveNext();
    };
    records.SetAutoFlush(TRUE);
  }
  else
    return FALSE;
  
  return TRUE;
}; // ClearRecordset


TripleArray* CMapLayer::CutWithLine( CMoLine& line, const CStringArray& attribute, LPDISPATCH extraObject /* = NULL */,  CProgressCtrl* progressCtrl /* = NULL */ )
// verschneidet eine Linie mit diesem Thema
// Parameter:
//        CMoLine& line: die zu verschneidende Linie
//        const CStringArray& attribute: falls nicht leer, wird im TrippleArray bei jedem Punkt der Wert aller
//                                       Attributes dieser Namen gemerkt
//        LPDISPATCH extraObject: falls ungleich NULL, wird dieses Objekt wie ein zusätzliches Objekt dieses Themas
//                                behandelt ( also ebenfalls Schnittpunkte mit diesem gesucht ). Es muss den 
//                                gleichen ShapeType haben wie deses Thema
//        CProgressCtrl* progressCtrl: falls ungleich Null, wird die Fortschrittanzeige entsprechend bewegt
// Rückgabewert:
//          CTrippleArray*: NULL bei Fehler, sonst ein Liste von Tripple-Punkten
//                        das Object muss von der aufrufenden Funktion zerstört werden
// Bemerkung:
//  - die Linie wird folgendermassen verschnitten:
//        - falls das Thema ein Polygonthema ist gibts für jeden Punkt der Linie einen Tripple-Punkt
//        - für jeden Schnittpunkt eines Liniensegemnts mit einem Object des Themas gibts einen Punkt
//        - die Punkte werden im Durchlaufsinn der Linie geordnet und die Abstände gemessen
//        - doppelte Punkte werden vermieden
{
  // ein paar voraussetzungen prüfen
  if( !GetDispatch() || !GetValid() || !LPDISPATCH(line) )
    return NULL;
  
  CMoParts lineParts( line.GetParts() );
  if( !LPDISPATCH(lineParts) )
    return NULL;
  
  long shapeType = GetShapeType(); // der ShapteType dieses Themas
  
  // das zusätzätzliche Objekt überprüfen
  // falls eines angegeben wurde, muss es den gleichen Typ haben
  COleDispatchDriverEx eObjekt( extraObject );
  if( extraObject )
  {
    extraObject->AddRef(); // weil der COleDispatchDriverEx destruktor ein Release macht ( und weils halt so gehört )

    VARIANT var = eObjekt.GetProperty( "ShapeType" );
    if( var.vt == VT_ERROR || long( COleVariantEx( var ) ) != shapeType )
      return NULL;
  }; // if extraObject
  
  // für die ProgressCtrl die LinienPunkte zählen
  int pointCount = 0;
  for( int i = 0; i < lineParts.GetCount(); i++ )
  {
    CMoPoints linePoints( lineParts.Item( CComVariant( i ) ) );
    if( LPDISPATCH(linePoints) )
      pointCount += linePoints.GetCount();
  }; // for i
  
  if( progressCtrl )
    progressCtrl->SetRange( 0, pointCount * 2 ); // die erste Hälfte ist für SearchShape im HMO
  
  // jetzt die Linie mit dem Thema verschneiden
  
  // als erstes nur die interessanten Objekte auswählen, d.h die, welche sich mit der Linie schneiden

  CMoRecordset records;
  switch( shapeType )
  {
  case moShapeTypePolygon:
    records = SearchShape( line, moLineCross, TEXT("") ); // schöner wäre ein moIntersect, gibts aber leider nicht
    break;
    
  case moShapeTypeLine:
    records = GetRecords(); // moLineCross scheint für LinienThemen nicht zu funktionieren
    break;
    
  case moShapeTypePoint:
    records = GetRecords();
    break;
  }; // switch shapeType
  
  if( !LPDISPATCH( records ) || records.GetCount() <= 0 )
    return NULL;
  
  // die ProgressCtrl auf die Hälfte setzen
  if( progressCtrl )
    progressCtrl->SetPos( pointCount );
  
  // einige Felder, die wir brauchen
  CMoFields fields( records.GetFields() );
  if( !LPDISPATCH(fields) )
    return NULL;
  
  CMoField shapeField( fields.Item( COleVariant( MO2_FIELD_SHAPE ) ) );
  if( !LPDISPATCH(shapeField) ) // das ShapeField muss natürlich existieren
    return NULL;
  
  // die gewünschten AttributFelder initialisieren
  CArray<CMoField, CMoField&> attributeFields;
  for( i = 0; i < attribute.GetSize(); i++ )
    attributeFields.Add( CMoField( fields.Item( COleVariant( attribute[i] ) ) ) );
  
  // jetzt die Schnittpunkte der Linie mit den Objekten finden
  // CMoPolygon::Intersect findet schnittpunkte und im inneren Liegende Endpunkte von
  // Strecken -> genau die brauchen wir
  TripleArray* neueCrds = new TripleArray;
  
  double yAbstand = 0.0; // der bis jetzt kumulierte y-Abstand
  
  for( i = 0; i < lineParts.GetCount(); i++ )
  {
    CMoPoints linePoints( lineParts.Item( CComVariant( i ) ) );
    if( !LPDISPATCH(linePoints) )
      continue;
    
    if( progressCtrl ) // sieht hübscher aus, ausserdem fehlt pro Part ein vorher gezählter Punkt
      progressCtrl->StepIt();
    
    for( int j = 0; j < linePoints.GetCount() - 1; j++ )
    {
      // das aktuelle Streckenstück herstellen
      CMoLine strecke;
      MO2CREATE( strecke, "Line" );
      if( !LPDISPATCH(strecke) )
        continue;
      
      CMoParts streckenParts( strecke.GetParts() );
      if( !LPDISPATCH(streckenParts) )
        continue;
      
      CMoPoints streckenPoints;
      MO2CREATE( streckenPoints, "Points" );
      if( !LPDISPATCH(streckenPoints) )
        continue;
      
      CMoPoint linePoint1( linePoints.Item( CComVariant( j ) ) );
      CMoPoint linePoint2( linePoints.Item( CComVariant( j + 1 ) ) );
      
      if( !LPDISPATCH(linePoint1) || !LPDISPATCH(linePoint2) )
        continue;

      CMoPoint point1, point2;
      MO2CREATE( point1, "Point" );
      MO2CREATE( point2, "Point" );

      point1.SetX( linePoint1.GetX() );
      point1.SetY( linePoint1.GetY() );
      point2.SetX( linePoint2.GetX() );
      point2.SetY( linePoint2.GetY() );

      streckenPoints.Add( point1 );
      streckenPoints.Add( point2 );
      
      streckenParts.Add( streckenPoints );
      
      // die Koordinaten des Anfangspunkt brauchen wir später für die Abstandsmessung
      double p1x = point1.GetX();
      double p1y = point1.GetY();
      
      // jetzt diese Strecke mit allen Objekten schneiden
      CPtrArray tmpCoords; // hier erstmal alle Schnittpunkte sammeln
      
      records.MoveFirst();
      BOOL bExtraObject = ( LPDISPATCH(eObjekt) != NULL );
      while( bExtraObject || !records.GetEof() )
      {
        // das aktuelle Object holen
        // falls ein extra Objekt da ist zuerst dieses behandeln, sonst normal die records abarbeiten
        LPDISPATCH objectDisp = NULL;
        if( bExtraObject )
        {
          objectDisp = LPDISPATCH(eObjekt);
          objectDisp->AddRef(); // den ReferenzCount auf dieses Objekt erhöhen, weil er durch den COle... Destruktor wieder runtergesetzt wird
        }
        else
        {
          VARIANT var = shapeField.GetValue();
          if( var.vt != VT_DISPATCH || !var.pdispVal )
            break; // darf nicht sein
          objectDisp = var.pdispVal; // den ReferenzCount nicht erhöhen, das wurde schon durch GetValue erledigt
        }; // if bExtraObject
        
        COleDispatchDriver object( objectDisp );
        
        // Abhängig vom ShapeType gibts verschiedene Möglichkeiten die Höhe auszurechnen
        double scalar = 0.0; // ein Scalar zum ausrechnen der Höhe
        double nx = 0.0, ny = 0.0, nz = 1.0; // der Normalenvektor für das Polygon ( falls es eins ist )
        
        switch( shapeType )
        {
        case moShapeTypePolygon:
          {
            // falls das Thema ein PolygonZ-Thema ist und das aktuelle Object genau drei Ecken hat
            // kann man die Höhe des Punktes leicht ausrechnen
            CMoPolygon dreieck( object );
            LPDISPATCH(object)->AddRef();
            
            // den NormalenVektor der durch das Dreieck gegebenen Ebene ausrechnen
            CMoParts dreieckParts( dreieck.GetParts() );
            if( LPDISPATCH(dreieckParts) && dreieckParts.GetCount() == 1 )
            {
              CMoPoints dreieckPoints( dreieckParts.Item( CComVariant( 0 ) ) );
              if( LPDISPATCH(dreieckPoints) && dreieckPoints.GetCount() == 3 )
              {
                CMoPoint dP1( dreieckPoints.Item( CComVariant( 0 ) ) );
                CMoPoint dP2( dreieckPoints.Item( CComVariant( 1 ) ) );
                CMoPoint dP3( dreieckPoints.Item( CComVariant( 2 ) ) );
                if( LPDISPATCH(dP1) && LPDISPATCH(dP2) && LPDISPATCH(dP3) )
                {
                  double x1 = dP1.GetX();
                  double x2 = dP2.GetX();
                  double x3 = dP3.GetX();
                  double y1 = dP1.GetY();
                  double y2 = dP2.GetY();
                  double y3 = dP3.GetY();
                  double z1 = dP1.GetZ();
                  double z2 = dP2.GetZ();
                  double z3 = dP3.GetZ();
                  
                  nx = ( y2 - y1 ) * ( z3 - z2 ) - ( y3 - y2 ) * ( z2 - z1 );
                  ny = ( x3 - x2 ) * ( z2 - z1 ) - ( x2 - x1 ) * ( z3 - z2 );
                  nz = ( x2 - x1 ) * ( y3 - y2 ) - ( x3 - x2 ) * ( y2 - y1 );
                  scalar = nx * x1 + ny * y1 + nz * z1; // Skalarprodukt aus n und v1
                }; // if dP1 && dP2 && dP3
              }; // if dreieckPoints.Count == 3
            }; // if dreieckParts
          }; // case moPolygon
          break;
          
        case moShapeTypeLine:
          {
            // noch nicht implementiert
          };
          break;
        }; // switch shapeType
        
        // jetzt die eigentlichen Schnittpunkte finden
        CMoPoints schnittPunkte( strecke.GetCrossings( object ) );
        
        // bei Polygonen auch noch die Endpunkte mit aufnehmen
        if( shapeType == moShapeTypePolygon )
        {
          // Punkte kreieren, falls wir welche hinzufügen müssen
          if( !LPDISPATCH(schnittPunkte) )
            MO2CREATE( schnittPunkte, "Points" );
          
          CMoPolygon poly( object );
          LPDISPATCH(object)->AddRef();
          
          if( poly.IsPointIn( point1 ) )
            schnittPunkte.Add( point1 );
          if( poly.IsPointIn( point2 ) )
            schnittPunkte.Add( point2 );
        }; // if shapeType == moPolygon
        
        if( LPDISPATCH(schnittPunkte ) )
        {
          // es gibt schnittPunkte, also den Attributwert dieses Objektes auslesen
          CStringArray geleseneAttribute;
          
          if( bExtraObject )
          {
            for( int a = 0; a < attribute.GetSize(); a++ )
              geleseneAttribute.Add( MO2_EXTRA_OBJECT );
          }
          else
          {
            for( int a = 0; a < attributeFields.GetSize(); a++ )
            {
              CMoField aField = attributeFields[a];
              if( LPDISPATCH(aField) )
              {
                // den Wert des Feldes holen und gleich nach String konvertieren
                COleVariant var( aField.GetValue() );
                var.ChangeType( VT_BSTR );
                if( var.vt != VT_ERROR )
                  geleseneAttribute.Add( CString( var.bstrVal ) );
              }; // if attributField
            }; // for a
          }; // if bExtraObject
          
          for( int k = 0; k < schnittPunkte.GetCount(); k++ )
          {
            CMoPoint p( schnittPunkte.Item( CComVariant( k ) ) );
            if( !LPDISPATCH(p) )
              continue;
            
            double x = p.GetX();
            double y = p.GetY();
            
            // den Abstand zum Anfangspunkt der Strecke ausrechnen
            double distance = sqrt( ( x - p1x ) * ( x - p1x ) + ( y - p1y ) * ( y - p1y ) );
            
            // die Höhe ausrechnen
            double hoehe = 0.0;
            switch( shapeType )
            {
            case moShapeTypePolygon:
              if( nz != 0.0 )
                hoehe = ( scalar - nx * x - ny * y ) / nz;
              break;
              
            case moShapeTypeLine:
              // nicht implementiert;
              break;
              
            case moShapeTypePoint:
              hoehe = p.GetZ();
              break;
            }; // switch shapeType
            
            tmpCoords.Add( new Triple( x, y, hoehe, distance + yAbstand, geleseneAttribute ) );
          }; // for k
        }; // if schnittPunkte
        
        if( bExtraObject )
          bExtraObject = FALSE; // das ExtraObjekt wird immer als erstes behandelt, deswegen ists ok das Flag jetzt zu löschen
        else
          records.MoveNext();
      }; // while hmoRecords
      
      // jetzt die aus den temporären Koordinaten die doppelten rausschmeissen und der Reihe nach ordnen
      while( tmpCoords.GetSize() > 0 )
      {
        // den aktuellen kleinsten Abstand suchen
        double minAbst = 1e36;
        for( int m = 0; m < tmpCoords.GetSize(); m++ )
        {
          Triple* trip = (Triple*)tmpCoords.GetAt( m );
          minAbst = min( minAbst, trip->breite );
        }; // for m
        
        BOOL bFirst = FALSE; // der erste Punkt mit diesem Abstand wurde bereits gefunden
        for( m = 0; m < tmpCoords.GetSize(); m++ )
        {
          Triple* trip = (Triple*)tmpCoords.GetAt( m );
          if( fabs( trip->breite - minAbst ) < 0.0000001 )
          {
            if( bFirst )
              delete trip;
            else
            {
              neueCrds->push_back( trip );
              bFirst = TRUE;
            };
            tmpCoords.RemoveAt( m );
            m--;
          }; // if trip.abstand = minAbstand
        }; // for m
      }; // while tmpCoords.GetSize() > 0
      
      // zuletzt noch den yAbstand neu ausrechnen
      double p2x = point2.GetX();
      double p2y = point2.GetY();
      double deltaStrecke = sqrt( ( p2x - p1x ) * ( p2x - p1x ) + ( p2y - p1y ) * ( p2y - p1y ) );
      yAbstand += deltaStrecke;
      
      if( progressCtrl )
        progressCtrl->StepIt();
    }; // for j
  }; // for i
  
  // erst noch schnell die doppelten Punkte, welchen an den jeweiligen Streckenenden auftauchen, rausfiltern
  for( i = 0; i < neueCrds->size() - 1; i++ )
  {
    Triple* p1 = neueCrds->at( i );
    Triple* p2 = neueCrds->at( i + 1 );
    double abst = sqrt( ( p1->rw - p2->rw ) * ( p1->rw - p2->rw ) + ( p1->hw - p2->hw ) * ( p1->hw - p2->hw ) );
    if( abst < 0.0001 )
    {
      delete p2;
      neueCrds->RemoveAt( i + 1, false );
      i--;
    }; // if abst < 0.0001
  }; // for i
  
  return neueCrds;
}; // CutWithLine

/**
 * gibt eine Zuordnung von FeatureId zum Feld MO2_FIELD_FILE zurück
 * 
 * @return eine UINT |-> CString Abbildung; muss von deraufrufenden Funktion mit delete gelöscht weren
 *
 * @throws InvalidLayerTypeException
 */
std::map<long, CString> CMapLayer::GetIDToFileMap()
{
  if( !LPDISPATCH( m_mapLayer ) || !GetValid() )
    throw InvalidLayerTypeException();
  
  CMoRecordset records( GetRecords() );
  if( !LPDISPATCH(records) )
    throw InvalidLayerTypeException();
  
  CMoFields fields( records.GetFields() );
  if( !LPDISPATCH(fields) )
    throw InvalidLayerTypeException();
  
  CMoField idField( fields.Item( CComVariant( MO2_FIELD_FEATUREID ) ) );
  CMoField fileField( fields.Item( CComVariant( MO2_FIELD_FILE ) ) );
  if( !LPDISPATCH( idField ) || !LPDISPATCH( fileField ) )
    throw InvalidLayerTypeException();
  
  // jetzt die Records auslesen und die Map generieren
  std::map<long, CString> idMap;
  while( !records.GetEof() )
  {
    idMap[COleVariantEx( idField.GetValue() )] = COleVariantEx( fileField.GetValue() );
    
    records.MoveNext();
  }; // while records
  
  return idMap;
}; // GetIDToFileMap

/**
 * gibt eine Zuordnung vom Feld MO2_FIELD_FILE zur FeatureId zurück
 *
 * @return eine CString |-> UINT Abbildung; muss von deraufrufenden Funktion mit delete gelöscht werden,
 *
 * @throws InvalidLayerTypeException
 */
std::map<CString, long> CMapLayer::GetFileToIDMap()
{
  if( !LPDISPATCH( m_mapLayer ) || !GetValid() )
    throw InvalidLayerTypeException();
  
  CMoRecordset records( GetRecords() );
  if( !LPDISPATCH(records) )
    throw InvalidLayerTypeException();
  
  CMoFields fields( records.GetFields() );
  if( !LPDISPATCH(fields) )
    throw InvalidLayerTypeException();
  
  CMoField idField( fields.Item( CComVariant( MO2_FIELD_FEATUREID ) ) );
  CMoField fileField( fields.Item( CComVariant( MO2_FIELD_FILE ) ) );
  if( !LPDISPATCH( idField ) || !LPDISPATCH( fileField ) )
    throw InvalidLayerTypeException();
  
  // jetzt die Records auslesen und die Map generieren
  std::map<CString, long> fileMap;
  while( !records.GetEof() )
  {
    const long id = COleVariantEx( idField.GetValue() );
    const CString fileName = COleVariantEx( fileField.GetValue() );
    fileMap[fileName] = id;
    
    records.MoveNext();
  }; // while records
  
  return fileMap;
}; // GetFileToIDMap


// Bereitet die Umrandungsinformation eines ( Wsp-)Layers auf
// Rückgabewert:
//          FALSE, falls die Informationen nicht ausgelesen werden konnten ( LayerTyp stimmt nicht o.ä. )
// Parameter:
//        CList<long, long>& pointList:  Liste aller Punkte ( FeatureID )
//        CArray<CMoPoint, CMoPoint&>& moPointList: Array aller Punkte: Zuordnung featureID -> CMoPoint
//        CUIntArray& nextPointID:  Zuordnung featureID -> featureID des nächsten Punktes
//        CTypedPtrArray<CPtrArray, CUIntArray*>& pointsByProfil: Die Profilpunkte nach Profilen grupiert
//                                            zur Zeit nur die beiden äussersten
//        BOOL bDeleteData = /* FALSE */: falls TRUE, wird nicht gemacht ausser alle Daten zu löschen
// Bemerkung:
//    Funktioniert nur bei Punkte-Layern, die ein Attrubut MO2_FIELD_NEXTID besitzen
//    Die Funktion löscht alle übergebenen Daten
//    Die z-Werte der CMoPoint's enthalten die WSP-Lage an diesem Punkt
BOOL CMapLayer::GetBorderInfo( CList<long, long>& pointList, CArray<CMoPoint, CMoPoint&>& moPointList, CArray<double, double>& pointYList, CUIntArray& nextPointID,
                              CTypedPtrArray<CPtrArray, CUIntArray*>& pointsByProfil, BOOL bDeleteData /* = FALSE */ )
{
  // erstmal alle übergebenen Container löschen
  pointList.RemoveAll();
  moPointList.RemoveAll();
  nextPointID.RemoveAll();
  
  // auch die pointByProfil richtig löschen
  for( int i = 0; i < pointsByProfil.GetSize(); i++ )
    delete pointsByProfil[i];
  pointsByProfil.RemoveAll();
  
  // falls nur die Daten gelöscht werden sollten, gleich zurück
  if( bDeleteData )
    return TRUE;
  
  // erstmal Eingangsdaten überprüfen und vorbereiten
  if( !GetValid() || GetShapeType() != moShapeTypePoint )
    return FALSE;
  
  CMoRecordset records( GetRecords() );
  if( !LPDISPATCH(records) )
    return FALSE;
  
  CMoFields fields( records.GetFields() );
  if( !LPDISPATCH(fields) )
    return FALSE;
  
  CMoField shapeField( fields.Item( COleVariant(MO2_FIELD_SHAPE) ) );
  CMoField featureField( fields.Item( COleVariant(MO2_FIELD_FEATUREID) ) );
  CMoField nextField( fields.Item( COleVariant(MO2_FIELD_NEXTID ) ) );
  CMoField profilIDField( fields.Item( COleVariant( MO2_FIELD_PROFILID ) ) );
  CMoField hoeheField( fields.Item( COleVariant( MO2_FIELD_HEIGHT ) ) );
  CMoField yField( fields.Item( COleVariant( MO2_FIELD_YKRD ) ) );

  if( !LPDISPATCH(shapeField) || !LPDISPATCH(featureField) || !LPDISPATCH(nextField) || 
    !LPDISPATCH(profilIDField) || !LPDISPATCH(hoeheField) || !LPDISPATCH(yField) )
    return FALSE;
  
  // Informationen aus Layer auslesen
  while ( !records.GetEof() )
  {
    VARIANT shapeVar = shapeField.GetValue();

    const long featureID = COleVariantEx( featureField.GetValue() );
    const long nextID = COleVariantEx( nextField.GetValue() );
    const long profilID = COleVariantEx( profilIDField.GetValue() );
    const double hoehe = COleVariantEx( hoeheField.GetValue() );
    const double yKrd = COleVariantEx( yField.GetValue() );
        
    if( shapeVar.vt == VT_DISPATCH )
    {
      if( nextID != 0 )
      {
        LPDISPATCH disp = shapeVar.pdispVal;
        disp->AddRef();
        
        pointList.AddHead( featureID );
        CMoPoint point( disp );
        point.SetZ( hoehe ); // den Z-Wert des Punkts setzen
        moPointList.SetAtGrow( featureID, point );
        pointYList.SetAtGrow( featureID, yKrd );
        nextPointID.SetAtGrow( featureID, nextID );
        
        if( profilID >= pointsByProfil.GetSize() || pointsByProfil[profilID] == NULL )
          pointsByProfil.SetAtGrow( profilID, new CUIntArray() );
        pointsByProfil[profilID]->Add( featureID );
      }; // if nextID != 0
      
      records.MoveNext();
    }; // if vt. == ...
  }; // while records
  
  return TRUE;
};


BOOL CMapLayer::GetBorderPolygon( CMoPolyArray& polygone )
// erstellt, falls dieser Layer ein Punkte Layer ist mit Attribut 'NextPtID', eine Polygonumrandung
// Parameter:
//        CMoPolyArray& polygone: hier werden die gefundenen Polygone abgelegt, das Array wird auf jeden Fall zuerst geleert
{
  // erstmal alle Polygone löschen
  polygone.RemoveAll(); // erstmal leeren
  
  // Umrandungsinformation auslesen
  CList<long, long> pointsLeft;  // Liste der noch nicht bearbeitete Punkte
  CArray<CMoPoint, CMoPoint&> punkteListe;
  CArray<double, double> pointYList;
  CUIntArray nextIDs;  // featureID -> featureID
  CTypedPtrArray<CPtrArray, CUIntArray*> pointsByProfil; // nur Dummy, benötigt für GetBorderInfo
  
  if( !GetBorderInfo( pointsLeft, punkteListe, pointYList, nextIDs, pointsByProfil ) )
    return FALSE;
  
  // jetzt aus den Ausgelesenen Informationen die Polygone erstellen
  long nextPoint = 0;
  
  CMoPoints polyPoints; // die jeweils aktuellen Punkte
  CMoParts polyParts;
  
  while ( !pointsLeft.IsEmpty() )
  {
    POSITION pos = pointsLeft.Find( nextPoint ); // naechsten Punkt finden
    if ( !pos ) // falls naechster Punkt nicht vorhanden: neues Polygon erstellen
    { 
      // die letzten Punkte zu den letzten Parts hinzufügen
      if( LPDISPATCH(polyParts) && LPDISPATCH(polyPoints) && polyPoints.GetCount() > 0 )
        polyParts.Add( polyPoints );
      
      // ein neues Polygon erzeugen
      CMoPolygon poly;
      MO2CREATE( poly, "Polygon" );
      if( !LPDISPATCH(poly) )
        continue;
      
      polygone.Add( poly );
      
      polyParts.ReleaseDispatch();
      polyParts = poly.GetParts();
      if( !LPDISPATCH( polyParts ) )
        continue;
      
      polyPoints.ReleaseDispatch(); // alte Punkte vergessen
      MO2CREATE( polyPoints, "Points" );
      
      pos = pointsLeft.GetHeadPosition();
      nextPoint = pointsLeft.GetAt( pos );
    };
    pointsLeft.RemoveAt( pos );
    
    polyPoints.Add( punkteListe[nextPoint] );
    nextPoint = nextIDs[nextPoint];
  }; // while !pointsLeft.IsEmpty();
  
  // möglicherweise die letzten Punkte noch den letzten Parts hinzufügen
  if( LPDISPATCH(polyParts) && LPDISPATCH(polyPoints) && polyPoints.GetCount() > 0 )
    polyParts.Add(polyPoints);
  
  // und die von GetBorderInfo geholten Daten löschen
  GetBorderInfo( pointsLeft, punkteListe, pointYList, nextIDs, pointsByProfil, TRUE );
  
  return TRUE;
}; // GetBorderPolygon

void CMapLayer::GetTableNames( CStringArray& fieldNames )
// gibt die Namen der Atribute des Layers zurück
// Parameter:
//        CStringArray& fieldNames: an diese Liste werden die Feldnamen angehängt
{
  if( !GetValid() || !&fieldNames )
    return;
  
  CMoRecordset records( GetRecords() );
  if( !LPDISPATCH(records) )
    return;
  
  CMoTableDesc tableDesc( records.GetTableDesc() );
  if( !LPDISPATCH(tableDesc) )
    return;
  
  for( short i = 0; i < tableDesc.GetFieldCount(); i++ )
    fieldNames.Add( tableDesc.GetFieldName( i ) );
}; // GetTableNames

void CMapLayer::GetFieldValues( const CString& fieldName, CStringArray& fieldValues )
// gibt die Werte eins bestimmten Feldes aller Objekte des Layers als String zurück
// die Indices des Arrays entsprechen dabei den IDs der Objekte
// Fehlende ID werden mit NULL-Strings aufgefüllt
// Parameter:
//        const CString& fieldName: der Feldname
//        CStringArray& fieldNames: hier werden die gefundenen Daten angehängt
{
  if( !GetValid() || fieldName.IsEmpty() )
    return;
  
  CMoRecordset records( GetRecords() );
  if( !LPDISPATCH(records) )
    return;
  CMoFields fields( records.GetFields() );
  if( !LPDISPATCH(fields) )
    return;
  CMoField field( fields.Item( CComVariant( fieldName ) ) );
  CMoField idField( fields.Item( CComVariant( MO2_FIELD_FEATUREID ) ) );
  if( !LPDISPATCH(field) || !LPDISPATCH(idField) )
    return;
  
  // jetzt die Felder auslesen
  while( !records.GetEof() )
  {
    const long id = COleVariantEx( idField.GetValue() );
    CComVariant var( field.GetValue() );
    var.ChangeType( VT_BSTR ); // nach String wandeln
    if( var.vt != VT_ERROR )
    {
      CString feldString( var.bstrVal );
      fieldValues.SetAtGrow( id, feldString );
    }; // if var.vt
    
    records.MoveNext();
  }; // while records
  
}; // GetFieldValues

BOOL CMapLayer::GetAttributes( const long featureID, CMapStringToVariant& attributes )
// Gibt die Attribute eines Objektes zurück
// Parameter:
//        long featureID: Attribute des Objekts diesen Typ werden zurückgegeben
//        CMap<CString, CString&, CComVariant, CComVariant>& attributes: hier werden die Attribute übergeben
// Rückgabewert:
//        BOOL: FALSE, falls die Attribute nicht ermittelt werden konnten
// Bemerkung:
//        die Map wird auf jeden Fall erstmal gelöscht
// Seiteneffekte: greift auf das Recordset des Layers zu
{
  // gleich die Map löschen
  attributes.RemoveAll();
  
  // bin ich überhaupt geladen und initialisiert?
  if( !GetValid() )
    return FALSE;
  
  // den entsprechenden Record holen
  CString searchExpr;
  searchExpr.Format( "%s = %d", MO2_FIELD_FEATUREID, featureID );
  CMoRecordset records( SearchExpression( searchExpr ) );  
  if( !LPDISPATCH(records) )
    return FALSE;
  
  CMoFields fields( records.GetFields() );
  if( !LPDISPATCH(fields) )
    return FALSE;
  
  FOR_EACH_IN( CMoField, field, fields )
  {
    if( LPDISPATCH(field) )
      attributes.SetAt( field.GetName(), CComVariant( field.GetValue() ) );
  }
  END_FOR;
  
  return TRUE;
}; // GetAttributes

BOOL CMapLayer::SetAttributes( const long featureID, const CMapStringToVariant& attributes )
// Setzt die Attribute eines bestimmten Objektes neu
// Parameter:
//        const long featureID: die ID des zu verändernden Objektes
//        const CMapStringToVariant& attributes: die neuen Attribute: nur die in der Liste werden verändert, wenn sie in diesem Objekt überhaupt vorkommen
// Rückgabewert:
//        BOOL: FALSE; bei Schwierigkeiten: Thema nicht initialisiert etc.
// Bemerkung:
//        die Typen der Attribute müssen zu denen dieses Themas passen
// Seiteneffekte:
//        die Records werden angeschaut und verändert
{
  // bin ich überhaupt geladen und initialisiert?
  if( !GetValid() )
    return FALSE;
  
  // den entsprechenden Record holen
  CString searchExpr;
  searchExpr.Format( "%s = %d", MO2_FIELD_FEATUREID, featureID );
  CMoRecordset records( SearchExpression( searchExpr ) );  
  if( !LPDISPATCH(records) || records.GetEof() || !records.GetUpdatable() )
    return FALSE;
  
  CMoFields fields( records.GetFields() );
  if( !LPDISPATCH(fields) )
    return FALSE;
  
  records.Edit();
  if( records.GetEditMode() == moEditInProgress )
  {
    POSITION pos = attributes.GetStartPosition();
    while( pos )
    {
      CString fieldName;
      CComVariant var;
      attributes.GetNextAssoc( pos, fieldName, var );
      
      // versuchen die Daten dieses Feldes zu setzen
      CMoField field( fields.Item( CComVariant( fieldName ) ) );
      if( LPDISPATCH(field) )
        field.SetValue( var );
    }; // while pos
    records.Update();
  }; // if editMode == moEditInProgress
  
  return TRUE;
}; // SetAttributes

/**
 Projiziert einen Punkt zum naechstgelegenen Punkt innerhalb einer Profillinie
 Parameter:
        CMoPoint& projPoint: der zu projizierende Punkt, nach Rückkehr der Funktion
                             der projizierte Punkt
        long profilID: featureID der ProfilLinie
        CMapStringToVariant& attribute: hier werde alle Attribute des gefundenen Punktes abgelegt
 Rückgabewert:
        BOOL: FALSE, falls die Aktion fehlschlug ( falscher Layertyp o.ä. )
 Nebeneffekte:
        Das Recordset des Profilpunkt Layers wird benutzt
 Bemerkung:
        Dies Thema muss ein Punktethema sein, welches das Attribut MO2_FIELD_PROFILID enthält
*/
BOOL CMapLayer::FindNextPoint( CMoPoint& projPoint, const long profilID, CMapStringToVariant& attribute )
{
  // Voraussetzungen prüfen
  if( !GetValid() || GetShapeType() != moShapeTypePoint )
    return FALSE;
  
  // alle Punkte im Profil suchen
  CString searchExpression;
  searchExpression.Format( "%s = %d", MO2_FIELD_PROFILID, profilID );
  CMoRecordset records( SearchExpression( searchExpression ) );
  if( !LPDISPATCH(records ) )
    return FALSE;
  
  CMoFields fields(records.GetFields());
  if( !LPDISPATCH(fields) )
    return FALSE;
  
  CMoField shapeField( fields.Item( COleVariant( MO2_FIELD_SHAPE ) ) );
  
  CMoPoint point;
  double distance = HUGE_VAL;
  double x, y;
  long pointID = FALSE;
  
  while( !records.GetEof() )
  {
    VARIANT shapeVar = shapeField.GetValue();
    if ( shapeVar.vt == VT_DISPATCH )
    {
      point.AttachDispatch( shapeVar.pdispVal, FALSE );
      double tmpDistance = point.DistanceTo( projPoint );
      if ( tmpDistance < distance )
      {
        distance = tmpDistance;
        x = point.GetX();
        y = point.GetY();
        
        // und die Attribute in die Map schreiben
        attribute.RemoveAll();
        FOR_EACH_IN( CMoField, field, fields )
        {
          attribute[field.GetName()] = field.GetValue();
        }
        END_FOR;
      };
      point.ReleaseDispatch();
    }; // var.vt == VT_DISPATCH
    
    records.MoveNext();        
  }; // while !pointRecords.GetEof
  
  if( distance < HUGE_VAL )
  {
    projPoint.SetX( x );
    projPoint.SetY( y );
    return TRUE;
  }
  else
    return FALSE;
}; // FindNextPoint

void CMapLayer::GetSchnittpunkte( CMapObjectArray& schnittObjekte, CMapLayer* otherLayer, const CString& sqlStr, CProgressCtrl* pCtrl )
// verschneidet diesen Layer mit dem übergebenen und generiert daraus Schnittpunkte
// Wenn der Typ dieses Layer 
//          - Punkt ist: TODO
//          - Linie ist: alle Schittpunkte, falls der andere Polygon ist auch die Enden der Linien, die im Polygon liegen
//          - Polygon: TODO
// Parameter:
//          CMapObjectArray: dieses Array wird erst gelöscht und dann mit den Schnittpunkten gefüllt ( Feld: MO2_FIELD_SHAPE )
//                            weiter werden auch die Felder MO2_FIELD_PROFILID, MO2_FIELD_YKRD und MO2_FIELD_HOEHE gesetzt falls möglich
//          CMapLayer* otherLayer: der zu verschneidende Layer
//          const CString&: sqlString für Einschränkung der zu verschneidenden Objekte
//          CProgressCtrl: eine Progresscontrol für eine Fortschittsanzeige. Die Range
//                      wird gesetzt und es wird gestept
{
  schnittObjekte.RemoveAll();
  
  if( GetShapeType() != moShapeTypeLine )
    return;
  
  if( otherLayer == NULL )
    return;
  
  CMoRecordset otherRecords( otherLayer->SearchExpression( sqlStr ) ); // nur Records, die den Einschränkungen entsprechen
  if( !LPDISPATCH(otherRecords) )
    return;
  
  CMoFields otherFields( otherRecords.GetFields() );
  if( !LPDISPATCH(otherFields) )
    return;
  
  CMoField otherShapeField( otherFields.Item( COleVariant( MO2_FIELD_SHAPE ) ) );
  
  if( pCtrl )
  {
    pCtrl->SetRange32( 0, otherRecords.GetCount() );
    pCtrl->SetPos( 0 );
  } // if pCtrl
  
  // die fremden Records der Reihe nach durchgehen
  while( !otherRecords.GetEof() )
  {
    LPDISPATCH otherDisp = otherShapeField.GetValue().pdispVal;
    if( otherDisp )
    {       
      COleDispatchDriver otherShape( otherDisp );
      
      CMoRecordset records( GetRecords() );
      if( LPDISPATCH(records) )
      {
        CMoFields fields( records.GetFields() );
        CMoField iDField( fields.Item( COleVariant( MO2_FIELD_FEATUREID  ) ) );
        CMoField shapeField( fields.Item( COleVariant( MO2_FIELD_SHAPE ) ) );
        
        while( !records.GetEof() )
        {
          const long profilID = COleVariantEx( iDField.GetValue() );
          CMoLine line( shapeField.GetValue().pdispVal );
          
          CMoPoints points( line.GetCrossings( otherShape ) );
          if( LPDISPATCH(points) )
          {
            for( int i = 0; i < points.GetCount(); i++ )
            {
              CMapObject* mapObject = new CMapObject();
              CMoPoint point = CMoPoint( points.Item( CComVariant( i ) ) );
              LPDISPATCH pointDisp = LPDISPATCH(point);
              pointDisp->AddRef();
              mapObject->SetAt( MO2_FIELD_SHAPE, CComVariant( pointDisp ) );
              mapObject->SetAt( MO2_FIELD_PROFILID, CComVariant( profilID ) );
              
              schnittObjekte.Add( mapObject );
            };
          }; // if schnittpunkte
          
          if( otherLayer->GetShapeType() == moPolygon )
          { // auch testen, ob die Endpunkte der Linie im Polygon liegen, wenn ja, hinzufügen
            CMoParts parts( line.GetParts() );
            if ( parts.GetCount() > 0 )
            {
              CMoPoints points( parts.Item( CComVariant( 0 ) ) );
              if ( points.GetCount() > 1 )
              {
                CMoPolygon polygon( otherShape ); // geht nur, weil shapeType == moPolygon
                polygon.m_bAutoRelease = FALSE;
                
                
                CMoPoint point( points.Item( CComVariant( 0 ) ) );
                if ( polygon.IsPointIn( point ) )
                {
                  CMapObject* mapObject = new CMapObject();
                  LPDISPATCH pointDisp = LPDISPATCH(point);
                  pointDisp->AddRef();
                  mapObject->SetAt( MO2_FIELD_SHAPE, CComVariant( pointDisp ) );
                  mapObject->SetAt( MO2_FIELD_PROFILID, CComVariant( profilID ) );
                  
                  schnittObjekte.Add( mapObject );
                }
                
                point = CMoPoint( points.Item( CComVariant( points.GetCount() - 1 ) ) );
                if ( polygon.IsPointIn( point ) )
                {
                  CMapObject* mapObject = new CMapObject();
                  LPDISPATCH pointDisp = LPDISPATCH(point);
                  pointDisp->AddRef();
                  mapObject->SetAt( MO2_FIELD_SHAPE, CComVariant( pointDisp ) );
                  mapObject->SetAt( MO2_FIELD_PROFILID, CComVariant( profilID ) );
                  
                  schnittObjekte.Add( mapObject );
                }
              }; // points.Getcount() > 0
            }; // parts.Getcount() > 0
          };
          
          records.MoveNext();
        }; // while records
        
      }; // if profilRecords
      
      if( pCtrl )
        pCtrl->StepIt();
      
    }
    otherRecords.MoveNext();
  }; // while !otherRecords.GetEof
  
}; // GetSchnittpunkte

/**
 * liest alle Objekte dieses Layers aus und legt sie als CMapObjectArray ab
 * @param objectArray wird mit den ( Repräsentanten von ) Objekten dieses Layers gefüllt
 * @param pCtrl eine Fortschrittsanzeige
 */
void CMapLayer::GetObjects( CMapObjectArray& objectArray, CProgressCtrl* pCtrl )
{
  CMoRecordset records( GetRecords() );
  if( !LPDISPATCH(records) )
    return;

  CMapHelper::ReadObjects( objectArray, records, pCtrl );
}; // GetObjects

void CMapLayer::GetProfilData( CMapObjectArray& pointArray, CProgressCtrl* pCtrl )
// Ermittelt für eine Anzahl von Punkten Kenndaten aus den Profilpunkten
//   - ist dieser Layer der ProfilPunktLayer( ShapeType = point, hat ProfilID, yKrd, Hoehe ), so werden anhand dieser
//      Felder die Höhe und yKrd jedes Objects ausgerechnet
//   - ist dieser Layer der ProfilLinienLayer( shapeType = Line und Felder ProfilID )
//      so werden jedem Punkt alle Attribute der entsprechenden ProfilLinie hinzugefügt
//
// Voraussetzungen:
//      - die Objekte müssen ein Feld MO2_FIELD_PROFILID haben
// Parameter:
//      CMapObjectArray& pointArray: die Punkte: zu diesen Objekten werden neu Felder hinzugefügt bzw. geändert
//      CProgressCtrl* pCtr: eine Fortschrittsanzeige
{
  if( pCtrl )
  {
    pCtrl->SetRange32( 0, pointArray.GetSize() );
    pCtrl->SetPos( 0 );
  };
  
  // jetzt die einzelnen Objekte durchgehen
  for( int i = 0; i < pointArray.GetSize(); i++ )
  {
    CMapObject* obj = pointArray[i];
    CComVariant profilIDVar;
    if( !obj->Lookup( MO2_FIELD_PROFILID, profilIDVar ) )
      continue;
    const long profilID = COleVariantEx( profilIDVar );
    
    CComVariant shapeVar;
    if( !obj->Lookup( MO2_FIELD_SHAPE, shapeVar ) )
      continue;
    CMoPoint point( shapeVar.pdispVal );
    shapeVar.pdispVal->AddRef();
    
    // die Objekte holen, die die gleiche ProfilId haben
    CString expression;
    CString feldName = GetShapeType() == moShapeTypePoint ? MO2_FIELD_PROFILID : MO2_FIELD_FEATUREID;
    expression.Format( "%s = %d", feldName, profilID );
    CMoRecordset records( SearchExpression( expression ) );
    if( !LPDISPATCH( records ) || records.GetEof() )
      continue;
    
    CMoFields fields( records.GetFields() );
    if( !LPDISPATCH(fields) )
      continue;
    
    switch( GetShapeType() )
    {
    case moShapeTypePoint:
      // bei Punkten versuchen Höhe und yKrd auszurechnen
      {
        CMoField shapeField( fields.Item( CComVariant( MO2_FIELD_SHAPE ) ) );
        CMoField yField( fields.Item( CComVariant( MO2_FIELD_YKRD ) ) );
        CMoField heightField( fields.Item( CComVariant( MO2_FIELD_HEIGHT ) ) );
        if( !LPDISPATCH(shapeField) || !LPDISPATCH(yField) || !LPDISPATCH(heightField) )
          continue;
        
        // jetzt suchen, welchem Punktepaar der aktuelle Punkt am nächsten liegt
        
        // den ersten Punkt holen
        CMoPoint startPoint( shapeField.GetValue().pdispVal ); // stets der Startpunkt eines Segments
        double startY = COleVariantEx( yField.GetValue() ); // stets die y-Crd des Startsegments
        double startH = COleVariantEx( heightField.GetValue() ); // stets die Höhe des Startsegments
        records.MoveNext();
        if( !LPDISPATCH(startPoint) )
          continue;
        
        double distance = HUGE_VAL;
        double yKrd = 0; // die gesuchte yKrd
        double hoehe = 0; // die gesuchte Hohe
        
        while( !records.GetEof() )
        {
          CMoPoint endPoint( shapeField.GetValue().pdispVal ); // stets der Endpunkt eines Segments
          const double endY = COleVariantEx( yField.GetValue() ); // stets die y-Crd des Startsegments
          const double endH = COleVariantEx( heightField.GetValue() ); // stets die Höhe des Startsegments
          double segLength = startPoint.DistanceTo( endPoint );
          if( LPDISPATCH(endPoint) && segLength > 0 ) // 0 - Segmente ausschliessen, sonst gibts unten Problem
          {
            double tmpDistance = point.DistanceToSegment( startPoint, endPoint );
            if( tmpDistance < distance )
            {
              distance = tmpDistance;
              
              // jedesmal die Höhe und die YKrd neu interpolieren
              // dauert auch nicht länger als einen Haufen Werte merken
              // ( z.B: die beiden Punkte )
              
              // die yKrd werden relativ zu den Tatsächlichen abständen aus den vorh. y-Krd interpoliert
              double pointLength = point.DistanceTo( startPoint );
              yKrd = startY + (  pointLength / segLength ) * ( endY - startY );
              
              // die Höhen genauso
              hoehe = startH + ( pointLength / segLength ) * ( endH - startH );
            }; // if tmpDistance < distance
          }; // if endPoint
          
          startPoint = endPoint;
          startY = endY;
          startH = endH;
          
          records.MoveNext();
        }; // while records
        
        obj->SetAt( MO2_FIELD_YKRD, CComVariant( yKrd ) );
        obj->SetAt( MO2_FIELD_HEIGHT, CComVariant( hoehe ) );
      };
      break;
      
    case moShapeTypeLine:
    case moShapeTypePolygon:
      // bei Linien und Polygonen einfach alle Attribute
      // des ersten Objekts übernehmen ( bei Profillinien sollte es genau eines sein )
      {
        CMapObject newObj( fields );
        obj->AddAttributes( newObj, false );
      }
      break;
    }
    
    if( pCtrl )
      pCtrl->StepIt();
    
  }; // for i
  
} // GetProfilData


void CMapLayer::AddObjects( CMapObjectArray& newObjects, CProgressCtrl* pCtrl )
// fügt dem Layer neue Objecte hinzu
// alle Attribute aus den Objekten werden übernommen, falls das Thema diese Attribute besitzt
// Parameter:
//        CMapObjectArray& newObjects: die neuen Objekte
//        CProgressCtrl* pCtr: eine Fortscrhittsanzeige
//
{
  // als erstes checken, ob die Objekte durchnummeriert sind ( ProfilPunkte )
  bool bNummer = false;
  
  { // damit der Dispatch der Records wieder befreit wird 
    CMoRecordset records( GetRecords() );
    if( !LPDISPATCH(records) || !records.GetUpdatable() )
      return;
    
    CMoFields fields( records.GetFields() );
    if( !LPDISPATCH(fields) )
      return;
    
    CMoField nummerField( fields.Item( COleVariant( MO2_FIELD_NUMBER ) ) );
    CMoField yKrdField( fields.Item( COleVariant( MO2_FIELD_YKRD ) ) );
    if( LPDISPATCH(nummerField) && LPDISPATCH(yKrdField) )
      bNummer = true;
  }; // die Records wieder loslassen
  
  if( pCtrl )
  {
    pCtrl->SetRange32( 0, newObjects.GetSize() );
    pCtrl->SetPos( 0 );
  };
  
  // und die Punkte der Reihe nach hinzufügen
  for( int i = 0; i < newObjects.GetSize(); i++ )
  {
    CMapObject* obj = newObjects[i];
    CComVariant yVar;
    CComVariant idVar;
    
    // falls es ein Nummerfeld gibt, die Nummern schonmal neu setzen und 
    // rausfinden, welche Nummer das neue Element bekommt
    int nummer = 0;
    if( bNummer && obj->Lookup( MO2_FIELD_YKRD, yVar ) && 
      obj->Lookup( MO2_FIELD_PROFILID, idVar ) )
    {
      const long profilID = COleVariantEx( idVar );
      const double yKrd = yVar.dblVal;
      
      CString sqlStr;
      sqlStr.Format( "%s = %d", MO2_FIELD_PROFILID, profilID );
      CMoRecordset records( SearchExpression( sqlStr ) );
      if( LPDISPATCH(records) && records.GetUpdatable() )
      {
        records.SetAutoFlush( FALSE );
        CMoFields fields( records.GetFields() );
        if( LPDISPATCH(fields) )
        {
          CMoField yField( fields.Item( CComVariant( MO2_FIELD_YKRD ) ) );
          CMoField nrField( fields.Item( CComVariant( MO2_FIELD_NUMBER ) ) );
          if( LPDISPATCH(yField) && LPDISPATCH(nrField) )
          {
            // es wird wie folgt umnumeriert:
            // ist die yKrd kleiner als die des neuen Objektes, bleibts wie es ist
            // ist sie grösser, wird die Nummer um eins erhöht
            while( !records.GetEof() )
            {
              const double y = COleVariantEx( yField.GetValue() );
              const long nr = COleVariantEx( nrField.GetValue() );
              
              // am Schluss wird nummer die Nummer des Punktes mit der grössten yKrd kleiner als die neue yKrd sein, plus 1
              // auch nur grössere Nummern als bisher zulassen, da die neuen Punkte
              // erstmal mit einer kleinen nr ans Ende des records gehängt werden
              if( y < yKrd ) // && nummer < nr  ) // nur die Nummer merken
                nummer = max( nummer, nr + 1 ); 
              else // die Nummer um eins erhöhen
              {
                records.Edit();
                if( records.GetEditMode() == moEditInProgress )
                {
                  nrField.SetValue( CComVariant( nr + 1 ) );
                  records.Update();
                }
              } // if y < yKrd
              
              records.MoveNext();
            } // while records
          } // if yField && nrField
        } // if fields
      } // if records
    } // if bNummer
    
    // jetzt das Element hinzufügen
    CMoRecordset records( GetRecords() );
    if( !LPDISPATCH(records) || !records.GetUpdatable() )
      continue;
    records.SetAutoFlush( FALSE );
    
    
    CMoFields fields( records.GetFields() );
    if( !LPDISPATCH(fields) )
      continue;
    
    records.AddNew();
    if( records.GetEditMode() != moEditAdd )
      continue;
    
    // erstmal einfach alle Attribute des Objekt übertragen
    POSITION pos = obj->GetStartPosition();
    while( pos )
    {
      CComVariant value;
      CString name;
      obj->GetNextAssoc( pos, name, value );
      CMoField field( fields.Item( CComVariant( name ) ) );
      if( LPDISPATCH(field) )
        field.SetValue( value );
    }
    
    // falls es eine Nummer gibt, auch diese noch setzen
    if( bNummer )
    {
      CMoField field( fields.Item( CComVariant( MO2_FIELD_NUMBER ) ) );
      field.SetValue( CComVariant( nummer ) );
    };
    
    records.Update();
    
    if( pCtrl )
      pCtrl->StepIt();
    
  }; // for i
  
  CMoRecordset records( GetRecords() );
  if( LPDISPATCH(records) )
    records.SetAutoFlush( TRUE );
} // AddObjects

void CMapLayer::RemoveObjects( CMapObjectArray& deleteObjects, CProgressCtrl* pCtrl )
// löscht die angegebenen Objekte aus dem Layer
// Parameter:
//        CMapObjectArray& deleteObjects: die zu löschenden Objekte: Objekte werden anhand der FeatureID identifiziert
//        CProgressCtrl* pCtrl: eine Fortschrittsanzeige
{
  CMoRecordset records( GetRecords() );
  if( !LPDISPATCH(records) || !records.GetUpdatable() )
    return;
  
  CMoFields fields( records.GetFields() );
  if( !LPDISPATCH(fields) )
    return;
  
  CMoField idField( fields.Item( CComVariant( MO2_FIELD_FEATUREID ) ) );
  if( !LPDISPATCH(idField) )
    return;
  
  // für Performance ein neues Array anlegen, dessen ArrayIndex
  // mit der FeatureId übereinstimmt
  // ist der Inhalt = 0 ( default, wird nicht gelöscht ), sonst schon
  CUIntArray removeObjects;
  removeObjects.SetSize( records.GetCount() + 1 );
  for( int i = 0; i < deleteObjects.GetSize(); i++ )
  {
    CMapObject* obj = deleteObjects[i];
    CComVariant idVar;
    if( obj->Lookup( MO2_FIELD_FEATUREID, idVar ) )
    {
      const long id = COleVariantEx( idVar );
      removeObjects.SetAtGrow( id, 1 );
    } // if Lookup
  } // for i
  
  if( pCtrl )
  {
    pCtrl->SetRange32( 0, records.GetCount() );
    pCtrl->SetPos( 0 );
  };
  
  // jetzt die Punkte löschen
  records.SetAutoFlush( FALSE );
  while( !records.GetEof() )
  {
    const long id = COleVariantEx( idField.GetValue() );
    if( removeObjects.GetSize() > id && removeObjects[id] == 1 )
      records.Delete();
    
    records.MoveNext();
    if( pCtrl )
      pCtrl->StepIt();
  }; // while records
  
  records.SetAutoFlush( TRUE );
} // RemoveObjects


BOOL CMapLayer::GetDatablocks( CTypedPtrMap<CMapWordToPtr, WORD, DataBlockArray*>& dataBlocks, CProgressCtrl* progress )
// erstellt aus den Daten des Layers WspPrj-Datenblöcke
// Parameter:
//        CTypedPtrMap<CMapWordToPtr, CString, DataBlockArray*>& dataBlocks: ein DataBlockArray für jede erzeugte Datenblock Art;
//                                              jedes DataBlockArray enthält für jede ProfilID einen DatenBlock oder NULL
//        CProgressCtrl: falls ungleich NULL, wird für jeden Record einmal gestept
// Rückgabewert:
//        FALSE, bei einem schweren Fehler ( z.B. kein Recordset vorhanden o.ä. )      
// Bemerkung:
//        Das DatenBlockArray wird auf jeden Fall gelöscht
{
  // alle Records durchgehen
  CMoRecordset mapRecords( GetRecords() );
  if( !LPDISPATCH(mapRecords) )
    return FALSE;
  CMoFields mapFields( mapRecords.GetFields() );
  if( !LPDISPATCH(mapFields) )
    return FALSE;
  
  // am Ende soll die Progress Bar um genau diese Anzahl weitergestellt sein
  int newProgressPos = progress->GetPos() + mapRecords.GetCount();
  
  // jetzt einen ganzen Haufen Felder ( alles was potentiel an Daten da sein kann )
  CMoField shapeField( mapFields.Item( CComVariant( MO2_FIELD_SHAPE ) ) );
  CMoField idField( mapFields.Item( CComVariant( MO2_FIELD_FEATUREID ) ) );
  CMoField profilIDField( mapFields.Item( CComVariant( MO2_FIELD_PROFILID ) ) );
  CMoField yField( mapFields.Item( CComVariant( MO2_FIELD_YKRD ) ) );
  CMoField hoeheField( mapFields.Item( CComVariant( MO2_FIELD_HEIGHT ) ) );
  CMoField numberField( mapFields.Item( CComVariant( MO2_FIELD_NUMBER ) ) );
  CMoField lageField( mapFields.Item( CComVariant( MO2_FIELD_TRENNSPEZIAL ) ) );
  CMoField variantField( mapFields.Item( CComVariant( MO2_FIELD_VARIANT ) ) );
  
  CMoField rauheitField( mapFields.Item( CComVariant( MO2_FIELD_RAUHEIT ) ) );
  CMoField rauheitKSTField( mapFields.Item( CComVariant( MO2_FIELD_RAUHEITKST ) ) );
  CMoField axmField( mapFields.Item( CComVariant( MO2_FIELD_AXM ) ) );
  CMoField aymField( mapFields.Item( CComVariant( MO2_FIELD_AYM ) ) );
  CMoField dpmField( mapFields.Item( CComVariant( MO2_FIELD_DPM ) ) );
  
  // es müssen mindesten Shape, id, ProfilID und yKoordinate vorhanden sein
  if( !LPDISPATCH(shapeField) || !LPDISPATCH(idField) || !LPDISPATCH(profilIDField) || !LPDISPATCH(yField) )
  {
    progress->SetPos( newProgressPos );
    return FALSE;
  }; // if !shapeField || ...
  
  // für jedes Profil gibts jetzt Potentiel soviele DatenBlöcke, wie in dbNewArray angegeben
  // d.h eine Zurodnung: Index der CrossSection -> Liste von DatenBlöcken
  
  // die Records durchgehen und der Reihe nach in Koordinaten von Datenblöcken verwandeln
  int recordCount = 0; // zählt die Records
  while( !mapRecords.GetEof() )
  {
    progress->StepIt();
    
    // die ProfilId auslesen und gegebenenfalls entsprechende DatenBlöcke erzeugen
    VARIANT shapeVar = shapeField.GetValue();
    
    if( shapeVar.vt != VT_DISPATCH )
      break;
    
    const long profilID = COleVariantEx( profilIDField.GetValue() ); 
    
    // TODO: möglicherweise die zu lesenden ProfilIDs einschränken
    //    if( profilID < 0 || profilID >= idToCsArray.GetSize() )
    //    {
    //      mapRecords.MoveNext();
    //      break;
    //    }; // if profilID
    
    const double yKoord = COleVariantEx( yField.GetValue() );
    long sortValue = (long)( yKoord * 10000.0 ); // mithilfe dieses Werts werden später die Koordinaten sortiert, normalerweise nach der y-koordinate
    
    // falls die Punkte durchnummeriert sind, die Nummer zum sortieren benutzen
    if( LPDISPATCH(numberField) )
        sortValue = COleVariantEx( numberField.GetValue() );
    
    double hoehe = 0.0;
    if( LPDISPATCH(hoeheField) )
      hoehe = COleVariantEx( hoeheField.GetValue() );
    
    CMoPoint point( shapeVar.pdispVal );
    shapeVar.pdispVal->AddRef();
    if( !LPDISPATCH(point) )
      break;
    
    // je nach LayerTyp müssen jetzt neue Datenblöcke erzeugt werden
    // welche DatenblockTypen kann ich erzeugen
    CUIntArray dbTypes;
    
    switch( GetType() )
    {
    case CLayer::profilPoints:
      dbTypes.Add( DST_GELAENDEHOEHE );
      dbTypes.Add( DST_RECHTSWERT );
      dbTypes.Add( DST_HOCHWERT );
      dbTypes.Add( DST_AXM );
      dbTypes.Add( DST_AYM );
      dbTypes.Add( DST_DPM );
      dbTypes.Add( DST_RAUHIGKEIT );
      dbTypes.Add( DST_RAUHIGKEIT_KST );
      break;
      
    case CLayer::trennflaechen:
      dbTypes.Add( DST_TRENNFLAECHEN );
      break;
      
    case CLayer::durchst_bereiche:
      dbTypes.Add( DST_DURCHST_BEREICH );
      break;

    case CLayer::bordvoll:
      dbTypes.Add( DST_BORDVOLL );
      break;

    case CLayer::modellgrenzen:
      dbTypes.Add( DST_MODELLGRENZEN );
      break;

    case CLayer::waterLevel:
      {
        if( WSPFeatures::Instance()->isEnabled("MAPPER","map_writewsp"))
          dbTypes.Add( DST_WSP_HOEHE );
      }
      break;
    } // switch GetType
    
    // jetzt für jeden Typ die Koordianten erzeugen
    for( int d = 0; d < dbTypes.GetSize(); d++ )
    {
      int dbType = dbTypes[d];
      
      // schauen, ob für diesen Typ schon eine DatenBlockArray existiert, sonst erzeugen
      DataBlockArray* dbArray = NULL;
      if( !dataBlocks.Lookup( dbType, dbArray ) || dbArray == NULL )
      {
        dbArray = new DataBlockArray;
        dataBlocks.SetAt( dbType, dbArray );
      } // if !Lookup
      
      // jetzt schauen, ob es für diese ProfilID schon einen DatenBlock gibt, sonst erzeugen
      DataBlock* db = NULL;
      if( profilID < dbArray->GetSize() )
        db = dbArray->GetAt( profilID );
      if( db == NULL )
      {
        db = new DataBlock( NULL, dbType );

        // Inits für die speziellen Typen
        switch( dbType )
        {
        case DST_WSP_HOEHE:
          db->SetWSPName( GetTag() ); // alternative: die Variante im Tag vom Layer speichern
          break;

        default:
          break;
        };

        dbArray->SetAtGrow( profilID, db );
      } // if db == NULL
      
      // jetzt diesem DatenBlock tatsächlich die Koordinate hinzufügen
      BOOL bAdd = TRUE;
      CMoField specialField;
      
      double zKoord = 0.0;
      
      switch( dbType )
      {
      case DST_TRENNFLAECHEN:
        specialField = lageField;
        break;

      case DST_BORDVOLL:
      case DST_MODELLGRENZEN:
        {
          if( db && db->GetNumCoords() > 0 )
            zKoord = 2.0;
          else
            zKoord = 1.0;
        }
        break;
        
      case DST_GELAENDEHOEHE:
      case DST_DURCHST_BEREICH:
        zKoord = hoehe;
        break;

      case DST_WSP_HOEHE:
        zKoord = hoehe;
        break;
        
      case DST_RAUHIGKEIT:
        specialField = rauheitField;
        break;
        
      case DST_RAUHIGKEIT_KST:
        specialField = rauheitKSTField;
        break;
        
      case DST_AXM:
        specialField = axmField;
        break;
        
      case DST_AYM:
        specialField = aymField;
        break;
        
      case DST_DPM:
        specialField = dpmField;
        break;
        
      case DST_RECHTSWERT:
        zKoord = point.GetX();
        break;
        
      case DST_HOCHWERT:
        zKoord = point.GetY();
        break;

        // bei unbekanntem Typ nichts schreiben
      default:
        bAdd = FALSE;
      }; // switch dbType
      
      if( LPDISPATCH( specialField ) )
      {
        try{ zKoord = COleVariantEx( specialField.GetValue() ); }
        catch( const COleException* )  {}
      }
      
      // die hinzuzufügende Coordinate
      if( bAdd )
        db->AddCoord( new Coord( yKoord, zKoord, sortValue, 0 ) );
    }; // for d
    
    mapRecords.MoveNext();
    recordCount++;
  }; // while( !mapRecords.GetEof() )
  
  // jetzt müssen noch alle DatenBlöcke sortiert werden
  POSITION pos = dataBlocks.GetStartPosition();
  while( pos )
  {
    DataBlockArray* dbArray = NULL;
    WORD dbType;
    dataBlocks.GetNextAssoc( pos, dbType, dbArray );
    if( dbArray != NULL )
    {
      for( int i = 0; i < dbArray->GetSize(); i++ )
      {
        DataBlock* db = dbArray->GetAt( i );
        if( db != NULL )
        {
          db->SortCoordsByXs();

          // auch noch einen Check, ob nicht alle Koordinaten gleich null sind
          if( ( dbType == DST_AXM || dbType == DST_AYM || dbType == DST_DPM || 
            dbType == DST_RAUHIGKEIT || dbType == DST_RAUHIGKEIT_KST ) 
            && db->CheckAllYZero() == TRUE )
          {
            delete db;
            dbArray->SetAt( i, NULL );
          } // if CheckAllXZero
        } // if db
      } // for i
    } // if dbArray != NULL
  } // while pos
  
  return TRUE; // keine Fehler
}; // GetDataBlocks



/*!
 * Testet, ob dieser Layer die Spezifikation eines speziellen Typs erfüllt.
 *
 * @param type : der zu testende Typ
 *
 * @return bool  : true, falls dieser Layer die Spezifikation des angegebenen Typs erfüllt.
 */
bool CMapLayer::TestLayerType( const LayerType& type )
{
  CMoRecordset records( GetRecords() );
  if( !LPDISPATCH(records) )
    return false;
  CMoTableDesc tableDesc( records.GetTableDesc() );
  if( !LPDISPATCH(tableDesc) )
    return false;

  switch( type )
  {
  case user_RW:
    return true;

  case flussachse:
    {
      // muss mindestens ein Attribute VZK haben
      for( int i = 0; i < tableDesc.GetFieldCount(); i++ )
      {
        CString name = tableDesc.GetFieldName( i );
        long fieldType = tableDesc.GetFieldType( i );
        short precision = tableDesc.GetFieldPrecision( i );

        if( name.CompareNoCase( MO2_FIELD_VZK ) == 0 && fieldType == moLong && precision > 2 )
          return true;
      }; // for i
    }
    break;

  default:
    return false; // im Zweifelsfall nicht kopieren
  }; // switch type

  return false;
}; // TestLayerType




/*!
 * Sucht ein Objekt nach einem bestimmten Schema und gibt das erste zurück, welches es erfüllt
 * Parameter wie SearchShape
 *
 * @param shape : dieses Objekt soll geschnitten werden
 *
 * @return CMapObject : alle Daten des gefundenen Objekts
 */
CMapObject CMapLayer::SearchFirstObject( LPDISPATCH shape, long searchMethod, LPCTSTR expression )
{
  CMoRecordset records( SearchShape( shape, searchMethod, expression ) );
  if( LPDISPATCH(records) && !records.GetEof() )
  {
    CMoFields fields( records.GetFields() );
    if( LPDISPATCH(fields) )
      return CMapObject( fields );
  }; // if records

  // wenn wir hier ankommen, haben wir nix gefunden
  return CMapObject();
}; // GetObject



/*!
 * Erzeugt für diesen Layer einen neuen Renderer, der einen schönen Farbverlauf darstellt
 *
 * @param fieldName : Feld, über welches eingefärbt wird
 * @param 
 *
 * @return void  : 
 */
void CMapLayer::RenderRamp( const CString& fieldName )
{
  CMapRenderer* pMapRenderer = GetMapRenderer();

  pMapRenderer->SetRendererType( CMapRenderer::valueMap );
  pMapRenderer->SetField( 0, fieldName );
  pMapRenderer->SetField( 1, "" );
  pMapRenderer->SetField( 2, "" );

  pMapRenderer->SetBool( 0, TRUE );

  pMapRenderer->ClearValueColors();

  CArray<COLORREF, COLORREF> valCols;

  CMoRecordset records( GetRecords() );
  CMoFields fields( records.GetFields() );
  CMoField attribField( fields.Item( CComVariant( fieldName ) ) );

  if( LPDISPATCH( attribField ) )
  {
    CMap<CString, LPCTSTR, int, int> valueCounter;

    while( !records.GetEof() )
    {
      CString val = attribField.GetValueAsString();

      int count = 0;
      if( valueCounter.Lookup( val, count ) )
        valueCounter.SetAt( val, count + 1 );
      else
         valueCounter.SetAt( val, 1 );

      records.MoveNext();
    }

    POSITION pos = valueCounter.GetStartPosition();

    double step = 256 / valueCounter.GetCount();
    int counter = 0;
    while( pos )
    {
      CString val;
      int count;
      valueCounter.GetNextAssoc( pos, val, count );

      valCols.Add( RGB( 0, 255 - counter * step, 255 ) );
      counter++;
    }
  }

  pMapRenderer->SetValueColors( valCols );
}

/* virtual */
BOOL CMapLayer::ShowPropertyDialog( CMoMap& pMap, CWnd* pWnd )
{
  CMoSymbol symbol( GetSymbol() );
    
  CMapLayerPropertyDlg dlg( this, symbol.GetSymbolType(), pWnd, GetMapRenderer()->GetRendererType() );
 
  if( dlg.DoModal() != IDCANCEL )
  {
      ApplyRenderer( pMap, TRUE );
      return TRUE;
  }
  else
    return FALSE;
}
