// InsertPointController.cpp: Implementierung der Klasse CInsertPointController.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "iMapChanger.h"
#include "layerData.h"
#include "linePointMover.h"

#include "InsertPointController.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

CInsertPointController::CInsertPointController( CMapView* view ) : IMapController( view )
{
  m_mapMover = 0; // wg. delete in reset

  reset();
}

CInsertPointController::~CInsertPointController()
{
  reset();
}

void CInsertPointController::Cancel()
{
  reset();
}


bool CInsertPointController::IsEnabled()
{
  CLayer* layer = m_view->GetDocument()->GetActiveLayer();
  if( !layer || layer->GetLayerType() != moMapLayer )
    return false;

  CLayerData* layerData = m_view->GetDocument()->GetLayerData( layer->GetType() );
  CMapLayer* mapLayer = (CMapLayer*)layer;

  return layerData && layerData->bObjectGeometryEditable && mapLayer->GetShapeType() != moShapeTypePoint;
}


void CInsertPointController::OnMapMouseDown( CMoPoint& cursor, short Button, short Shift )
{
  CLayer* layer = m_view->GetDocument()->GetActiveLayer();
  if( !layer || layer->GetLayerType() != moMapLayer )
  {
    reset();
    return;
  }

  m_mapLayer = (CMapLayer*)layer;

  m_lFeatureID = m_mapLayer->SearchNextObjectByDistance( cursor, m_view->GetSearchDistance() );

  if( m_lFeatureID == -1 )
    ::MessageBeep( MB_ICONQUESTION );		// not found
  else
  { // nächste Linie suchen
    COleDispatchDriver targetShape;
    CMoParts targetParts;

    int shapeType = m_mapLayer->GetShapeType();
    switch( shapeType )
    {
    case moShapeTypeLine:
      {
        CMoLine line( m_mapLayer->GetFieldValByID( m_lFeatureID, MO2_FIELD_SHAPE ).pdispVal );
        CMoParts parts( line.GetParts() );
        targetShape = line;
        targetParts = parts;
      };
      break;

    case moShapeTypePolygon:
      {
        CMoPolygon poly( m_mapLayer->GetFieldValByID(m_lFeatureID, MO2_FIELD_SHAPE).pdispVal );
        CMoParts parts(poly.GetParts());
        targetShape = poly;
        targetParts = parts;
      };
      break;

    default:
      TRACE( "InsertPoint in Layer vom Typ ungleich Poly oder Linie" );
      ASSERT(FALSE);  // darf nicht passieren
    }; // switch ShapeType
        
    double abstand = m_view->GetSearchDistance();
    for( int i = 0; i < targetParts.GetCount(); i++ )
    {
      CMoPoints points( targetParts.Item(CComVariant(i)) );
      long last = points.GetCount() - 1;
      for( int j = 0; j < ( shapeType == moShapeTypeLine ? last : last + 1 ); j++ ) // für Polygone auch Verbindung letzter mit erster Punkt testen
      {
        CMoPoint anfang( points.Item(CComVariant(j) ) );  // erster Punkt der Testlinie
        CMoPoint ende( points.Item(CComVariant( j == last ? 0 : j + 1) ) );  // zweiter Punkt = nächster bzw. erster Punkt dieser Points
        CMoPoints testPoints;
        VERIFY( testPoints.CreateDispatch(TEXT("MapObjects2.Points") ) );
        testPoints.Add( anfang );
        testPoints.Add( ende );
        double testAbst = testPoints.DistanceTo( LPDISPATCH( cursor ) );
        if( testAbst < abstand )
        {
          abstand = testAbst;
          m_dwPointData = (i << 16) | ((j == last ? 0 : j + 1) & 0xffff );
        };
      }; // for j
    }; // for i

    CMoPoints points( targetParts.Item( CComVariant( HIWORD( m_dwPointData ) ) ) );
    points.Insert( LOWORD( m_dwPointData ), cursor );

    m_mapMover = new CLinePointMover( m_view->GetDocument()->GetMap()->GetTrackingLayer(), targetShape, m_mapLayer->GetShapeType(), m_dwPointData, cursor );
  }; // if m_FeatureID
}


void CInsertPointController::OnMapMouseMove( CMoPoint& cursor, short Button, short Shift )
{
  if( m_mapMover )
    m_mapMover->MoveTo( cursor );
}

void CInsertPointController::OnMapMouseUp( CMoPoint& cursor, short Button, short Shift )
{
  if( m_mapLayer && m_mapMover )
  {
    m_mapLayer->InsertPoint( cursor, m_lFeatureID, m_dwPointData );

    m_view->GetDocument()->FireMapDocChanged( CMapView::THEME_GEOMETRY_CHANGED, m_mapLayer );

    reset();
  }
}