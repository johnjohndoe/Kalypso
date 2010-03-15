//////////////////////////////////////////////////////////////////////////
// MapHelper.cpp - Utility routines for working with the MapObjects control
// 

#include "stdafx.h"

#include "commonMfc/include/variant_helper.h"
#include "maphelper.h"

#include "obqrydlg.h"

enum MapUnitConstants
{
  muDecimalDegrees = 0,
    muFeet = 1,
    muMeters = 2,
};

/**
 * Fills a CMapObjectArray with objects from a recordset
 */
void CMapHelper::ReadObjects( CMapObjectArray& list, CMoRecordset& records, CProgressCtrl* pCtrl )
{
  if( pCtrl )
  {
    pCtrl->SetRange32( 0, records.GetCount() );
    pCtrl->SetPos( 0 );
  } // if pCtrl
  
  CMoFields fields( records.GetFields() );
  if( !LPDISPATCH(fields) )
    return;
  
  while( !records.GetEof() )
  {
    CMapObject* object = new CMapObject( fields );
    list.Add( object );
    
    records.MoveNext();

	if( pCtrl )
		pCtrl->StepIt();
  } // while records
}

/** static */
CMoLine CMapHelper::CreateSegment( CMoPoint& p1, CMoPoint& p2 )
{
	CMoPoint pc1;
	pc1.CreateDispatch( TEXT( "MapObjects2.Point" ) );
	pc1.SetX( p1.GetX() );
	pc1.SetY( p1.GetY() );
	pc1.SetZ( p1.GetZ() );

	CMoPoint pc2;
	pc2.CreateDispatch( TEXT( "MapObjects2.Point" ) );
	pc2.SetX( p2.GetX() );
	pc2.SetY( p2.GetY() );
	pc2.SetZ( p2.GetZ() );

	CMoPoints points;
	points.CreateDispatch( TEXT( "MapObjects2.Points" ) );

	points.Add( pc1 );
	points.Add( pc2 );

	CMoLine segment;
	segment.CreateDispatch( TEXT( "MapObjects2.Line" ) );
	
	CMoParts segmentParts( segment.GetParts() );
	segmentParts.Add( points );

	return segment;
}


/* static */
void CMapHelper::WriteObjectQueryToFields( const ObjectQuery& oq, CMoFields& fields )
{
  for( int i = 0; i < oq.m_fieldNames.GetSize(); i++ )
  {
    switch( oq.m_fieldTypes[i] )
    {
    case moLong:
      SetValue( fields, TEXT(oq.m_fieldNames[i] ), oq.m_longValues[i] );
      break;
      
    case moDouble:
      SetValue( fields, TEXT(oq.m_fieldNames[i]), oq.m_doubleValues[i]);
      break;
      
    case moString:
      SetValue( fields, TEXT(oq.m_fieldNames[i]), oq.m_stringValues[i]);
      break;
      
    case moDate:
      SetValue( fields, TEXT(oq.m_fieldNames[i]), oq.m_dateValues[i].Format("%Y-%m-%d") );
      break;
      
    case moBoolean:
      SetValue( fields, TEXT(oq.m_fieldNames[i]), oq.m_boolValues[i] ? TEXT("true") : TEXT("false"));
      break;
    }; // switch
  }; // for
}

/* static */ 
void CMapHelper::WriteFieldsToObjectQuery( CMoFields& fields, ObjectQuery& pOQ )
{
  int count = 0;
  FOR_EACH_IN( CMoField, field, fields )
  {
    if( field.GetName() != MO2_FIELD_SHAPE )
    {
      pOQ.m_fieldNames.Add( field.GetName() );
      pOQ.m_fieldTypes.Add( field.GetType() );
      switch( field.GetType() )
      {
      case moLong:
        pOQ.m_longValues.SetAtGrow( count, COleVariantEx( field.GetValue() ) );
        break;
        
      case moDouble:
        pOQ.m_doubleValues.SetAtGrow( count, COleVariantEx( field.GetValue() ) );
        break;
        
      case moDate:
        {
          COleDateTime date;
          
          date.ParseDateTime(field.GetValueAsString());
          pOQ.m_dateValues.SetAtGrow(count, date);
        }
        break;
        
      case moString:
        pOQ.m_stringValues.SetAtGrow(count, field.GetValueAsString());
        break;
        
      case moBoolean:
        {
          CString str;
          
          str = field.GetValueAsString();
          pOQ.m_boolValues.SetAtGrow(count, str=="true" ? TRUE : FALSE);
        }
        break;
      }
      count++;
    }
  }
  END_FOR
}

/* static */
void CMapHelper::WriteTableDescToObjectQuery( CMoTableDesc& tDesc, ObjectQuery& pOQ, const CMapStringToVariant& attributes )
{
  for( short i = 0; i < tDesc.GetFieldCount(); i++ )
  {
    CString fieldName = tDesc.GetFieldName( i );
    long fieldType = tDesc.GetFieldType( i );
    
    // feststellen, ob dieses Attribut schon vorbesetzt ist
    CComVariant fieldValue;
    BOOL bVar = attributes.Lookup( fieldName, fieldValue );
    
    pOQ.m_fieldNames.Add( fieldName );
    pOQ.m_fieldTypes.Add( fieldType );
    switch( fieldType )
    {
    case moLong:
      pOQ.m_longValues.SetAtGrow( i, bVar ? COleVariantEx( fieldValue ) : 0 );
      break;
      
    case moDouble:
      pOQ.m_doubleValues.SetAtGrow( i, bVar ? fieldValue.dblVal : 0.0 );
      break;
      
    case moDate:
      {
        COleDateTime date = COleDateTime::GetCurrentTime();
        pOQ.m_dateValues.SetAtGrow( i, bVar ? fieldValue.date : date );
      }
      break;
      
    case moString:
      pOQ.m_stringValues.SetAtGrow( i, bVar ? CString( fieldValue.bstrVal ) : TEXT("") );
      break;
      
    case moBoolean:
      pOQ.m_boolValues.SetAtGrow( i, bVar ? fieldValue.boolVal : FALSE );
      break;
    }; // switch
  }; // for i
}

void DeleteShapeFiles( const CStringArray& fileNames )
// löscht alle schape files, welche in der Stringliste aufgeführt sind
// die endung muss jeweils .shp sein
{
  for( int i = 0; i < fileNames.GetSize(); i++ )
  {
    CString file = fileNames[i];
    file.MakeUpper();
    int index = file.Find( ".SHP" );
    if ( index == -1 )
      continue;
    
    file = file.Left( index );
    
    try
    {
      CFile::Remove( file + ".shp" );
      CFile::Remove( file + ".shx" );
      CFile::Remove( file + ".dbf" );
      CFile::Remove( file + ".sbn" );
      CFile::Remove( file + ".sbx" );
    } // try
    catch( CFileException* e )
    { // Fehlermeldung ausgeben
      TCHAR szCause[MAX_PATH];
      
      e->GetErrorMessage( szCause, MAX_PATH );
      e->Delete();
    }; // catch
  } // for i
};


///////////////////////////////////////////////////////////////////////////
// Conversion
//

CMoPoint YKoordToGeokoords( CArray<double, double>& rWerte, 
                           CArray<double, double>& hWerte, 
                           CArray<double, double>& yWerte, 
                           CMap<double, double, int, int>& yIndex, double yKoord )
                           // findet einen Punkt in Geokoordinaten anhand seines yWertes innerhalb einer Profillinie
                           // Parameter:
                           //        CArray<double, double>& rWerte, hWerte, yWerte: Daten einer Profillinie
                           //        CMap<double, double, int, int>& yIndex: Zurodnung aller vorhandenen yWerte zu den Array Indices
                           //        double yKoord: yPosition innerhalb der Profillinie
                           // Rückgabwert:
                           //        CMoPoint: der gefundenen Punkt, nullDispatch bei Fehler
                           // Bemerkung:
                           //        ist die yKoordinate vorhanden ( d.h in yIndex ), so wird der exakte Punkt zurückgegeben
{
  CMoPoint ergebnis;
  
  int yCount = yWerte.GetSize();
  int rCount = rWerte.GetSize();
  int hCount = rWerte.GetSize();
  int icount = yIndex.GetCount();
  
  if ( yCount == rCount && rCount == hCount && hCount > 1 )
  {
    int index;
    if ( yIndex.Lookup( yKoord, index ) )
    {
      MO2CREATE( ergebnis, "Point" );
      ergebnis.SetX( rWerte[index] );
      ergebnis.SetY( hWerte[index] );
    }
    else
    {
      int index = -1;
      double yMin = HUGE_VAL;
      double yMax = -HUGE_VAL;
      for ( int i = 0; i < yCount - 1; i++ )
      {
        double y1 = yWerte[i];
        double y2 = yWerte[i + 1];
        yMin = min( yMin, min( y1, y2 ) );
        yMax = max( yMax, max( y1, y2 ) );
        
        if ( y1 <= yKoord && yKoord <= y2 )
        {
          index = i;
          break;
        }; // if yKoord \in [y1, y2]
      }; // for i
      
      if ( index == -1 )
      {
        if ( yKoord < yMin )
          index = 0;
        else if ( yKoord > yMax )
          index = yCount - 2;
      };
      
      if ( index != -1 )
      {
        double y1 = yWerte[index];
        double rw1 = rWerte[index];
        double hw1 = hWerte[index];
        double rw2 = rWerte[index + 1];
        double hw2 = hWerte[index + 1];
        
        double vx = rw2 - rw1;
        double vy = hw2 - hw1;
        double vlaenge = sqrt((vx *vx) + (vy * vy));
        if ( vlaenge != 0.0 ) // ansonsten ist vx == vx == 0, die beiden Punkte sind glich mit yKoordinate yKoord
        {
          vx /= vlaenge;
          vy /= vlaenge;
        };
        
        MO2CREATE( ergebnis, "Point" );
        ergebnis.SetX( rw1 + ( yKoord - y1 ) * vx );
        ergebnis.SetY( hw1 + ( yKoord - y1 ) * vy );
      }; // if index != -1
    }; // if yCount ...
  }; // if Lookup
  
  return ergebnis;
}; // YKoordToGeokoords

///////////////////////////////////////////////////////////////////////////
// Miscellaneous
//
LPDISPATCH GetIDispatchFromCWnd(CWnd* pWnd)
{
  ASSERT_VALID(pWnd);
  
  LPUNKNOWN lpUnk = pWnd->GetControlUnknown();
  
  ASSERT(lpUnk);
  
  LPOLELINK lpOleLink = NULL;
  if (lpUnk->QueryInterface(IID_IOleLink, 
    (LPVOID FAR*)&lpOleLink) == NOERROR)
  {
    ASSERT(lpOleLink != NULL);
    lpUnk = NULL;
    if (lpOleLink->GetBoundSource(&lpUnk) != NOERROR)
    {
      TRACE0("Warning: Link is not connected!\n");
      lpOleLink->Release();
      return NULL;
    }
    ASSERT(lpUnk != NULL);
  }
  
  LPDISPATCH lpDispatch = NULL;
  if (lpUnk->QueryInterface(IID_IDispatch, (void**)&lpDispatch) 
    != NOERROR)
  {
    TRACE0("Warning: does not support IDispatch!\n");
    return NULL;
  }
  
  ASSERT(lpDispatch != NULL);
  
  lpDispatch->Release();
  
  return lpDispatch;
}

CMoRectangle FullExtent(CMoRectangle rect1, CMoRectangle rect2)
{
  CMoRectangle rect;
  VERIFY(rect.CreateDispatch(TEXT("MapObjects2.Rectangle")));
  rect.SetLeft(min(rect1.GetLeft(), rect2.GetLeft()));
  rect.SetRight(max(rect1.GetRight(), rect2.GetRight()));
  rect.SetTop(max(rect1.GetTop(), rect2.GetTop()));
  rect.SetBottom(min(rect1.GetBottom(), rect2.GetBottom()));
  return rect;
};

CMoPoints GetPointsFromObject(COleDispatchDriverEx object)
// sammelt alle punkte eines (MO-Shape)Objectes in einer Punktkollektion
// Funktion ist kein Member von COleDispatchDriverEx, da sonst CommonMFC.lib
// MapObject-Code benutzen würde
{
  CMoPoints outPoints;
  if( LPDISPATCH( object ) )
  {
    const int shapeType = COleVariantEx( object.GetProperty( "ShapeType" ) ); //GetShapeType(object);
    VERIFY(outPoints.CreateDispatch(TEXT("MapObjects2.Points")));
    
    CMoParts parts; // für Linien und Polygone
    switch (shapeType)
    {
    case moShapeTypePoint:
      {
        CMoPoint point(object);
        outPoints.Add(point);
      };
      break;
      
    case moShapeTypeMultipoint:
      {
        CMoPoints points(object);
        outPoints = points;
      };
      break;
      
    case moShapeTypeLine:
      {
        CMoLine line(object);
        CMoParts p(line.GetParts());
        parts = p;
      };
      break;
      
    case moShapeTypePolygon:
      {
        CMoPolygon poly(object);
        CMoParts p(poly.GetParts());
        parts = p;
      };
      break;
      
    default:
      ASSERT(FALSE);
    }; // switch ShapeType
    
    if (LPDISPATCH(parts))
    {
      for (int i = 0; i < parts.GetCount(); i++)
      {
        CMoPoints points(parts.Item(CComVariant(i)));
        for (int j = 0; j < points.GetCount(); j++)
        {
          CMoPoint point(points.Item(CComVariant(j)));
          outPoints.Add(point);
        };
      };
    };
  }; // if object
  
  return outPoints;
}; // GetPointsFromObject(COleDispatchDriver object)

///////////////////////////////////////////////////////////////////////////
// Geodatasets
//
void SetValue( CMoFields& fields, LPCTSTR name, VARIANT& value )
{
  // Find the field
  CMoField field( fields.Item( COleVariant(name) ) );
  if( !LPDISPATCH(field) )
    return;
  
  // Set the field value
  field.SetValue( value );
}

void SetValue(CMoFields& fields, LPCTSTR name, const LONG value)
{
  VARIANT v;
  v.vt = VT_I4;
  v.lVal = value;
  SetValue(fields, name, v);
}

void SetValue(CMoFields& fields, LPCTSTR name, const double value)
{
  VARIANT v;
  v.vt = VT_R8;
  v.dblVal = value;
  SetValue(fields, name, v);
}

void SetValue(CMoFields& fields, LPCTSTR name, const COleDateTime& value)
{
  VARIANT v;
  v.vt = VT_DATE;
  v.date = value.m_dt;
  SetValue(fields, name, v);
}

void SetValue(CMoFields& fields, LPCTSTR name, const LPCTSTR value)
{
  SetValue(fields, name, COleVariant(value));
}

void SetValue(CMoFields& fields, LPCTSTR name, const BOOL value)
{
  VARIANT v;
  v.vt = VT_BOOL;
  v.bVal = value;
  SetValue(fields, name, v);
}

void SetValue(CMoFields& fields, LPCTSTR name, const LPDISPATCH value)
{
  VARIANT v;
  v.vt = VT_DISPATCH;
  v.pdispVal = value;
  SetValue(fields, name, v);
}


///////////////////////////////////////////////////////////////////////////
// Printing and Export
//
void FrameMapToScale(CMoMap& map, CDC* pDC, const CRect& dstRect, long UserScale,const short MapUnits)
{
  VARIANT nullVariant = { moClipToExtent };
  
  double ConversionFactor;
  switch(MapUnits)
  {
  case muDecimalDegrees:
    ConversionFactor=4322893.46;
    break;
  case muFeet:
    ConversionFactor=12.;
    break;
  case muMeters:
    ConversionFactor=39.37;
    break;
  }
  double TwipsPerPixX =1440.0/(double)::GetDeviceCaps(pDC->m_hAttribDC, LOGPIXELSX);
  double TwipsPerPixY =1440.0/(double)::GetDeviceCaps(pDC->m_hAttribDC, LOGPIXELSY);
  CMoRectangle r(map.GetExtent());
  double MapWidth=r.GetWidth() * ConversionFactor;
  double MapHeight=r.GetHeight() * ConversionFactor;
  double OutputRectWidth=MapWidth/UserScale;
  double OutputRectHeight=MapHeight/UserScale;
  double OneInchToUnits=UserScale/ConversionFactor;
  
  OutputRectWidth=(OutputRectWidth *1440.0)/TwipsPerPixX;
  OutputRectHeight=(OutputRectHeight *1440.0)/TwipsPerPixX;
  
  double PageOriginX=(dstRect.Width()-OutputRectWidth)/2;
  double PageOriginY=(dstRect.Height()-OutputRectHeight)/2;
  
  
  int saveID = pDC->SaveDC();
  
  pDC->SetMapMode(MM_ISOTROPIC);//   MM_TWIPS    MM_ISOTROPIC
  pDC->SetWindowOrg(dstRect.left,dstRect.top); 
  pDC->SetWindowExt(dstRect.Width(),dstRect.Height());
  pDC->SetViewportOrg(dstRect.left, dstRect.top);
  pDC->SetViewportExt(dstRect.Width(), dstRect.Height());
  
  if(dstRect.Width()>=OutputRectWidth && dstRect.Height() >= OutputRectHeight)
    map.OutputMap2((OLE_HANDLE)pDC->GetSafeHdc(),(int)(PageOriginX+0.5),(int)(PageOriginY+0.5),(int)(OutputRectWidth+0.5),(int)(OutputRectHeight+0.5),nullVariant);
  else 
    AfxMessageBox("Masstab ist zu groß!");
  pDC->RestoreDC(saveID);
  
#if 0
  // Rectangles for testing
  HBRUSH hBrush = (HBRUSH)::GetStockObject(NULL_BRUSH);
  HBRUSH hOldBrush = (HBRUSH)::SelectObject(pDC->GetSafeHdc(), hBrush);
  pDC->Rectangle(dstRect);
  
  HPEN   hPen = (HPEN)::CreatePen(PS_SOLID, 0, RGB(255, 0, 0));
  HPEN   hOldPen = (HPEN)::SelectObject(pDC->GetSafeHdc(), hPen);
  pDC->Rectangle(dstRect);
  ::SelectObject(pDC->GetSafeHdc(), hOldPen);
  
  ::SelectObject(pDC->GetSafeHdc(), hOldBrush);
#endif
  
}

////////////////
// Debug ///////
////////////////


void AfxFormattedMessageBox(CString text, ...)
{
  char *str = new char[strlen(text)+80];
  va_list pData;
  va_start(pData, text);
  
  char buffer[250];
  vsprintf(buffer ,(LPCTSTR)text,pData);
  AfxMessageBox(buffer);
  
  va_end(pData);
  delete []str;
  return;
}

