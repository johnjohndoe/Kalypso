// MoveObjectController.cpp: Implementierung der Klasse CMoveObjectController.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "commonMfc/include/variant_helper.h"

#include "mapView.h"
#include "mapDoc.h"
#include "layerData.h"
#include "shapeMover.h"

#include "MoveObjectController.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

CMoveObjectController::CMoveObjectController( CMapView* view ) : IMapController( view )
{ 
  m_shapeMover = 0 ;
};

CMoveObjectController::~CMoveObjectController() 
{
  delete m_shapeMover;
};

void CMoveObjectController::Cancel()
{
  delete m_shapeMover;
  m_shapeMover = 0;
  m_lFeatureID = -1;
};

bool CMoveObjectController::IsEnabled()
{
  CLayer* layer = m_view->GetDocument()->GetActiveLayer();
  if( layer )
  {
    CLayerData* layerData = m_view->GetDocument()->GetLayerData( layer->GetType() );
    return layerData && layerData->bObjectGeometryEditable;
  }
  else
     return FALSE;
}

void CMoveObjectController::OnMapMouseDown( CMoPoint& cursor, short Button, short Shift )
{
  // Umriss des Objekts als Tracking Layer
  CLayer* layer = m_view->GetDocument()->GetActiveLayer();
  if( !layer || layer->GetLayerType() != moMapLayer )
    return;

  CLayerData* layerData = m_view->GetDocument()->GetLayerData( layer->GetType() );
  CMapLayer* mapLayer = (CMapLayer*)layer;
  if( !mapLayer || !layerData )
    return;

  m_lFeatureID = mapLayer->SearchNextObjectByDistance( cursor, m_view->GetSearchDistance() );
  

  if( m_lFeatureID == -1 )
    ::MessageBeep( MB_ICONQUESTION );		// not found
  else
  {
    COleDispatchDriver shape( mapLayer->GetFieldValByID( m_lFeatureID, MO2_FIELD_SHAPE ).pdispVal );
    CMoTrackingLayer trackLayer = m_view->GetDocument()->GetMap()->GetTrackingLayer();

    long profilID = -1;
    CMapLayer* pointLayer = 0;

    if ( layerData->nProfilBezug > -1 ) // ggfls. cursor auf Profillinie projizieren
    {
      profilID = COleVariantEx( mapLayer->GetFieldValByID( m_lFeatureID, MO2_FIELD_PROFILID ) );
      pointLayer = m_view->GetDocument()->GetLayers()->FindFirstLayer( CLayer::profilPoints );
    }

    m_shapeMover = new CShapeMover( trackLayer, mapLayer->GetSymbol(), shape, profilID, pointLayer, cursor );
  }; // if m_lFeatureID
}


void CMoveObjectController::OnMapMouseMove( CMoPoint& cursor, short Button, short Shift )
{
  if( m_shapeMover )
    m_shapeMover->MoveTo( cursor );
}

void CMoveObjectController::OnMapMouseUp( CMoPoint& cursor, short Button, short Shift )
{
  CLayer* layer = m_view->GetDocument()->GetActiveLayer();
  if( !layer || layer->GetLayerType() != moMapLayer )
    return;

  CMapLayer* mapLayer = (CMapLayer*)layer;
  CLayerData* layerData = m_view->GetDocument()->GetLayerData( layer->GetType() );
  CMoTrackingLayer trackingLayer = m_view->GetDocument()->GetMap()->GetTrackingLayer();
  CMoGeoEvent event( trackingLayer.FindEvent( MO2_TRACKTAG_MOVEOBJECT ) );
  if ( LPDISPATCH( event ) && layerData )
  {
    LPDISPATCH eventDisp = event.GetShape();
    COleDispatchDriver shape;
    int shapeType = mapLayer->GetShapeType();
    switch( shapeType )
    {
    case moShapeTypePoint:
      {
        CMoPoint point(eventDisp);
        shape = point;
        if ( layerData->nProfilBezug != -1 ) // für Profilbezogene Daten Extrawurst
        {
          const long profilID = COleVariantEx( mapLayer->GetFieldValByID( m_lFeatureID, MO2_FIELD_PROFILID ) );
          
          CMapStringToVariant attr;
          CMapLayer* pointLayer = m_view->GetDocument()->GetLayers()->FindFirstLayer( CLayer::profilPoints );
          if( pointLayer && pointLayer->FindNextPoint( point, profilID, attr ) )
          {
            // diejenigen Attriute löschen, die auf keinen Fall uebertragen werden sollen
            attr.RemoveKey( MO2_FIELD_SHAPE );
            attr.RemoveKey( MO2_FIELD_FEATUREID );
            attr.RemoveKey( MO2_FIELD_NUMBER );
            if( layerData->bAdaptHeight == FALSE )
              attr.RemoveKey( MO2_FIELD_HEIGHT ); // die Höhe nur bei bestimmten Layern übertragen
            mapLayer->SetAttributes( m_lFeatureID, attr );
          };
        };
      }
      break;
      
    case moShapeTypeLine:
      {// das eventShape direkt zu übertragen geht hier leider nicht Warum??
        CMoLine eventLine(eventDisp);
        CMoParts eventParts(eventLine.GetParts());
        CMoLine shapeLine;
        MO2CREATE( shapeLine, "Line" );
        CMoParts shapeParts(shapeLine.GetParts());
        for (int i = 0; i < eventParts.GetCount(); i++)
        {
          CMoPoints points(eventParts.Item(CComVariant(i)));
          shapeParts.Add(points);
        };
        shape = shapeLine;
      };
      break;
      
    case moShapeTypePolygon:
      { // das eventShape direkt zu übertragen geht hier leider nicht Warum??
        CMoPolygon eventPoly(eventDisp);
        CMoParts eventParts(eventPoly.GetParts());
        CMoPolygon shapePoly;
        MO2CREATE( shapePoly, "Polygon" );
        CMoParts shapeParts(shapePoly.GetParts());
        for (int i = 0; i < eventParts.GetCount(); i++)
        {
          CMoPoints points(eventParts.Item(CComVariant(i)));
          shapeParts.Add(points);
        };
        shape = shapePoly;
      };
      break;
      
    default:
      ASSERT(FALSE);
    }; // switch ShapeType

    int error = mapLayer->SetFieldValByID( m_lFeatureID, MO2_FIELD_SHAPE, CComVariant( shape ) );
    if( error )
      TRACE("SetFieldValByID error %d", error);

    delete m_shapeMover;
    m_shapeMover = 0;
    
    m_view->GetDocument()->FireMapDocChanged( IMapDocListener::THEME_GEOMETRY_CHANGED, mapLayer );
  }; // if tracking.Layer
}