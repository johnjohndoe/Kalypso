#pragma warning(disable:4786)
#pragma warning(disable:4503)

#include "stdafx.h"

#include "maphelper.h"

#include "commonMfc/include/variant_helper.h"

#include "maplayer.h"
#include "mapdocdata.h"
#include "wspmap.h"


#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

////////////////////////////////////////////////////////////////////////////
// CLayerArray

CLayerArray::CLayerArray()
{
}

void CLayerArray::Serialize( CArchive& ar )
{
	CTypedPtrArray<CObArray, CLayer*>::Serialize(ar);
  
  // nicht geladenen Layer wieder rausschmeissen
  for (int i = 0; i < GetSize(); i++)
  {
    CLayer *layer = GetAt(i);
    if (!layer->LoadIsValid())
    {
      delete layer;
      RemoveAt(i--);
    };
  }; // for i
}; // serialize

void CLayerArray::MatchLayerOrder( CMoMap& map )
{
	if (!map.m_hWnd)
		return;
	CMoLayers layers(map.GetLayers());
	CLayerArray tempArray;
	CLayerArray invalidLayers;
	int i, j;

	for (i = 0; i < layers.GetCount(); i++)
	{
    LPDISPATCH dispatch = layers.Item(COleVariant((short)i));
		for (j=0; j<GetSize(); j++)
		{
			if (GetAt(j)->GetDispatch() == dispatch)
				tempArray.Add(GetAt(j));
		}; // for j
    dispatch->Release();
	}

	// check for invalid layers (layers not in map!)
	for (i=0; i < GetSize(); i++)
	{
		BOOL bFound = FALSE;

		for (j=0; j < tempArray.GetSize(); j++)
		{
			if (tempArray[j]==GetAt(i))
			{
				bFound = TRUE;
				break;
			}
		}
		if (!bFound)
    {
			invalidLayers.Add(GetAt(i));
    };
	}

	RemoveAll();
	for (i = tempArray.GetSize()-1; i>=0; i--)
    Add(tempArray[i]);

  // Layer welche nicht in der Karte sind werden am Schluss wieder angehängt
	for (i = 0; i < invalidLayers.GetSize(); i++)
		Add(invalidLayers[i]);
}; // MatchLayerOrder

CMapLayer* CLayerArray::FindFirstLayer( const int type, POSITION* pos /* = NULL */ )
{
	for( int i = 0; i < GetSize(); i++ )
	{
		CLayer* pLayer = GetAt( i );
		if( pLayer->GetType() == type && pLayer->GetLayerType() == moMapLayer )
    {
      if( pos )
        *pos = (POSITION)i;
      return (CMapLayer*)pLayer;
    };
	}; // for i
	return NULL;
}; // FindFirstLayer

// naechsten Layer mit Typ type suchen
CMapLayer* CLayerArray::FindNextLayer( const int type, POSITION* pos )
{
  if( !pos )
    return NULL;
	for( int i = (int)*pos + 1; i < GetSize(); i++)
	{
		CLayer* pLayer = GetAt( i );
		if( pLayer->GetType() == type && pLayer->GetLayerType() == moMapLayer )
    {
      *pos = (POSITION)i;
      return (CMapLayer*)pLayer;
    };
	}
  return NULL;
}; // FindNextLayer

CMoRectangle CLayerArray::GetExtent()
{
  CMoRectangle maxRect;
  if (this->GetSize() > 0)
  {
    maxRect = GetAt(0)->GetExtent();
    for (int i = 1; i < this->GetSize(); i++)
    {
      CMoRectangle rect(GetAt(i)->GetExtent());
      maxRect = FullExtent(maxRect, rect);
    };
  };
  return maxRect;
};

CLayer* CLayerArray::FindLayer( LPDISPATCH dispatch, POSITION* pos /* = NULL */ )
// Findet ersten Layer mit dem gleichen Dispatch
// 
// Output: Zeiger auf den gefundenen Layer oder NULL, falls nix gefunden
{
  for( int i = 0; i < this->GetSize(); i++ )
  {
    CLayer*layer = (*this)[i];
    if( layer->GetDispatch() == dispatch )
    {
      if( pos )
        *pos = (POSITION)i;
      return layer;
    }; // if layer == dispatch
  };
  return NULL;
}

int CLayerArray::GetLayerIndex( CLayer* layer )
// findet Nummer des Layers layer
// gibt -1 zurück falls Suche erfolglos war
{
  for( int i = 0; i < this->GetSize(); i++ )
  {
    if ((*this)[i] == layer)
      return i;
  };
  return -1;
}

CLayer * CLayerArray::FindFirstLayerByTag( const CString& tag, POSITION* pos /* = NULL */ )
// gibt Zeiger auf ersten Layer mit Tag tag zurück
// NULL falls Suche erfolglos
{
	for( int i = 0; i < GetSize(); i++ )
	{
    CLayer* pLayer = GetAt( i );
    if( pLayer->GetTag() == tag )
    {
      if( pos )
        *pos = (POSITION)i;
      return pLayer;
    };
	}
	return NULL;
}; // FindFirstLayerByTag

CLayer* CLayerArray::FindFirstLayerByName( const CString& name, POSITION* pos /* NULL */ )
// gibt Zeiger auf ersten Layer mit Tag tag zurück
// NULL falls Suche erfolglos
{
	for( int i = 0; i < GetSize(); i++ )
	{
    CLayer* pLayer = GetAt( i );
    if( name.CompareNoCase(pLayer->GetName()) == 0 )
    {
      if( pos )
        *pos = (POSITION)i;
      return pLayer;
    };
	}
	return NULL;
}

CMapLayer* CLayerArray::FindFirstLayerByTagAndType( const CString& tag, CLayer::LayerType type, 
                                                    POSITION* pos /* NULL */ )
// gibt Zeiger auf ersten Layer mit Tag tag zurück
// NULL falls Suche erfolglos
{
	for( int i = 0; i < GetSize(); i++ )
	{
    CLayer* pLayer = GetAt( i );
    if( pLayer->GetTag() == tag && pLayer->GetType() == type && pLayer->GetLayerType() == moMapLayer )
    {
      if( pos )
        *pos = (POSITION)i;
      return (CMapLayer*)pLayer;
    };
	}
	return NULL;
}

double CLayerArray::ProjectToProfilLine( CMoPoint& point, const long profilID )
//  projiziert einen Punkt auf die angegebene ProfilLinie
//
//  Parameter:
//            CMoPoint& point: zu projizierender Punkt, nach Rückkehr der Funktion 
//                              der projizierte Punkt
//            long profilID: FeatureID der ProfilLinie
// Rückgabewert:
//            yKoordinate des Punktes ( allerdings, nur anhand der Abstände der profilPunkte
//            ermittelt, nicht durch interpolation der yKrd der benachbarten Profilpunkte
{
  double ergebnis = -9999.99;
  CMapLayer* profilLayer = FindFirstLayer( CLayer::profilLines );
  ASSERT( profilLayer );

  CMoRecordset profilRecords( profilLayer->GetRecords() );
  CMoFields profilFields( profilRecords.GetFields() );
  CMoField idField( profilFields.Item( COleVariant( MO2_FIELD_FEATUREID ) ) );
  CMoField dyField( profilFields.Item( COleVariant( MO2_FIELD_DELTAY ) ) );
  CMoField shapeField( profilFields.Item( COleVariant( MO2_FIELD_SHAPE ) ) );
  while ( !profilRecords.GetEof() )
  {
    if( long( COleVariantEx( idField.GetValue() ) ) == profilID )
    {
      CMoLine line( shapeField.GetValue().pdispVal );
      double deltay = COleVariantEx( dyField.GetValue() );
      
      CMoParts parts(line.GetParts());
      CMoPoints points(parts.Item(CComVariant(0)));
      ASSERT(points.GetCount() >= 2); // mindestens zwei Punkte müssen vorhanden sein
      double distance = HUGE_VAL;
      CArray<double, double> yKrds; // die yKoordinaten der Segmente
      int index = -1;
      for ( int i = 0; i < points.GetCount() - 1; i++ )
      {
        CMoPoint point1( points.Item( CComVariant(i) ));
        CMoPoint point2( points.Item( CComVariant(i + 1) ));

        double segmentLaenge = point1.DistanceTo( point2 );

        yKrds.SetAtGrow( i, deltay );
        deltay += segmentLaenge; // TODO was passiert bei Rückläufigen Profilen

        if( segmentLaenge > 0 ) // 0 - Segmente ausschliessen, sonst gibts unten Probleme
        {
          double tmpDistance = point.DistanceToSegment( point1, point2 );
          if ( tmpDistance < distance )
          {
            distance = tmpDistance;
            index = i;
          };
        }; // if segmentLaenge > 0
      };

      if ( index > -1 )
      {
        CMoPoint anfPunkt(points.Item( CComVariant(index) ));
        CMoPoint endPunkt(points.Item( CComVariant(index + 1) ));
        
        double vx = endPunkt.GetX() - anfPunkt.GetX();
        double vy = endPunkt.GetY() - anfPunkt.GetY();
        double vlaenge = sqrt(vx * vx + vy * vy);
        
        double wx = point.GetX() - anfPunkt.GetX();
        double wy = point.GetY() - anfPunkt.GetY();
        
        double skalarProdukt  = (vx * wx + vy * wy);
        
        double pX, pY; // die neuen Koordinaten
        
        if ( skalarProdukt / vlaenge / vlaenge <= 0 && index != 0 )
        {
          pX = anfPunkt.GetX();
          pY = anfPunkt.GetY();
          ergebnis = yKrds[index];
        }
        else if ( skalarProdukt / vlaenge / vlaenge > 1 && index != points.GetCount() - 2 )
        {
          pX = endPunkt.GetX();
          pY = endPunkt.GetY();
          ergebnis = yKrds[index] + anfPunkt.DistanceTo( endPunkt );
        }
        else
        {
          pX = anfPunkt.GetX() + skalarProdukt / vlaenge * vx / vlaenge;
          pY = anfPunkt.GetY() + skalarProdukt / vlaenge * vy / vlaenge;
          ergebnis = yKrds[index] + ( skalarProdukt / vlaenge );
        };
        
        point.SetX( pX );
        point.SetY( pY );
      }; // if index > -1
      break;
    }; // if idField.GetValue...
    profilRecords.MoveNext();
  }; // while records.GetEof
  return ergebnis;
}; // ProjectToProfilLine

long CLayerArray::GetProfil( CMoPoint& point )
//
// gibt die ID der zum Punkt nächstgelegenen Profillinie zurück
//
// Parameter::
//          CMoPoint point: Punkt auf einer Profillinie
// Rückgabewert:
//            FeatureID der gefundenen Linie, -1 bei Fehler
{
  CMapLayer *profilLayer = FindFirstLayer(CLayer::profilLines);
  if (!profilLayer)
    return -1;
  CMoRecordset records(profilLayer->SearchByDistance(point, 1.0, TEXT("")));
  if (!LPDISPATCH(records))
    return -1;
  if (records.GetCount() != 1)
    return -1;
  CMoFields fields(records.GetFields());
  CMoField idField(fields.Item(COleVariant(MO2_FIELD_FEATUREID)));
  return COleVariantEx( idField.GetValue() );
}

double CLayerArray::GetProfilStation( const long featureID )
// gibt Station des Profils mit Nummer feature ID zurück, -1 bei Fehler
{
  CMapLayer *profilLayer = FindFirstLayer(CLayer::profilLines);
  if( !profilLayer )
    return -1;

  return profilLayer->GetFieldValByID( featureID, MO2_FIELD_STATION );
}

CString CLayerArray::GetProfilFile( const long featureID )
// gibt Urdateinamen des Profils mit Nummer feature ID zurück, "" bei Fehler
{
  CMapLayer *profilLayer = FindFirstLayer(CLayer::profilLines);
  if (!profilLayer)
    return "";

  return profilLayer->GetFieldValByID( featureID, MO2_FIELD_FILE );
}

CString CLayerArray::GetProfilState( const long featureID )
// gibt Zudstandsdateinamen des Profils mit Nummer feature ID zurück, "" bei Fehler
{
  CMapLayer *profilLayer = FindFirstLayer(CLayer::profilLines);
  if (!profilLayer)
    return "";

  return profilLayer->GetFieldValByID( featureID, MO2_FIELD_STATE );
}

LPDISPATCH CLayerArray::GetProfilShape( const long featureID )
// gibt Shape des Profils mit Nummer feature ID zurück, NULL bei Fehler
{
  CMapLayer* profilLayer = FindFirstLayer( CLayer::profilLines );
  if( !profilLayer )
    return NULL;

  VARIANT var = profilLayer->GetFieldValByID( featureID, MO2_FIELD_SHAPE );
  if( var.vt != VT_DISPATCH )
    return NULL;
  return var.pdispVal;
}

long CLayerArray::ProjectToProfilLineByDistance( CMoPoint &point, const double distance )
// Projiziert einen Punkt auf eine in der Nähe liegende ProfilLinie
//
// Parameter:
//        CMoPoint point: Koordinaten des zu projizierenden Punktes (wird verändert)
//        double distance: Maximaler Abstand einer ProfilLinie
// Rückgabewert:
//         Index der gefundenen ProfilLinie, -1 bei Misserfolg (keine Linie in der Nähe)
//          CMoPoint &point: der projizierte Punkt
{
  long profilLineID = -1;
  CMapLayer *profilLayer = FindFirstLayer(CLayer::profilLines);
  if (profilLayer)
  {
    profilLineID = profilLayer->SearchNextObjectByDistance(point, distance);
    if (profilLineID != -1)
      ProjectToProfilLine( point, profilLineID );
  }; // if profilLayer
  return profilLineID;
}; // ProjectToProfileLineByDistance

long CLayerArray::ProjectToProfilPointByDistance( CMoPoint& point, const double distance )
// Projiziert einen Punkt auf den naechsten Profilpunkt innerhalb von distance
//
// Parameter:
//        CMoPoint& point: Koordinaten des zu projizierenden Punktes (wird verändert),
//                          nach erfolgreicher Rückkehr der Funktion der projizierte Punkt, sonst unverändert
//        double distance: Maximaler Abstand des gesuchten Punktes
// Rückgabewert:
//         Index des gefundenen ProfilPunktes, -1 bei Misserfolg ( kein Profilpunkt in der Nähe )
{
  long pointID = -1;
  CMapLayer* pointLayer = FindFirstLayer( CLayer::profilPoints );
  if( LPDISPATCH(pointLayer) )
  {
    CMoRecordset records( pointLayer->SearchByDistance( point, distance, "" ) );
    if ( LPDISPATCH(records) )
    {
      CMoFields fields( records.GetFields() );
      CMoField shapeField( fields.Item( COleVariant( MO2_FIELD_SHAPE ) ) );
      CMoField idField( fields.Item( COleVariant( MO2_FIELD_FEATUREID ) ) );
      
      double distance = HUGE_VAL;
      double x, y; // Rechts- und Hochwert des gefundenen Punktes
      while( !records.GetEof() )
      {
        VARIANT var = shapeField.GetValue();
        if( var.vt == VT_DISPATCH )
        {
          CMoPoint tmpPoint( var.pdispVal );
          double tmpDistance = tmpPoint.DistanceTo( point );
          if ( tmpDistance < distance )
          {
            distance = tmpDistance;
            x = tmpPoint.GetX();
            y = tmpPoint.GetY();

            pointID = COleVariantEx( idField.GetValue() );
          }; // tmpDistance < distance
        }; // var.vt = VT_DISPATCH
        records.MoveNext();
      }; // while ( !records.GetEof() )
      
      if( pointID != -1 )
      {
        point.SetX( x );
        point.SetY( y );
      };
    }; // if records
  }; // if pointLayer

  return pointID;
}; // ProjectToProfilPointByDistance
