// ClipObjectController.cpp: Implementierung der Klasse CClipObjectController.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "commonMfc/include/variant_helper.h"

#include "shapeMover.h"
#include "layerData.h"
#include "maphelper.h"
#include "obqrydlg.h"
#include "markMapObject.h"

#include "ClipObjectController.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

CClipObjectController::CClipObjectController( CMapView* view )  : IMapController( view ) 
{
  m_shapeMover = 0;
};

CClipObjectController::~CClipObjectController() 
{
  delete m_shapeMover;
};


bool CClipObjectController::IsEnabled()
{
  CLayer* layer = m_view->GetDocument()->GetActiveLayer();
  if( layer && layer->GetLayerType() == moMapLayer )
  {
    CMapLayer* mapLayer = (CMapLayer*)layer;

    CLayerData* layerData = m_view->GetDocument()->GetLayerData( layer->GetType() );
    return layerData && layerData->bObjectGeometryEditable && mapLayer->GetShapeType() != moShapeTypePoint;
  }
  else
     return FALSE;
}

void CClipObjectController::Cancel()
{
  delete m_shapeMover;
  m_shapeMover = 0;
};

void CClipObjectController::OnMapMouseDown( CMoPoint& cursor, short Button, short Shift )
{
  // falls gerade ein Objekt bewegt wird nichts tun
  if( m_shapeMover )
    return;

  CLayer* layer = m_view->GetDocument()->GetActiveLayer();
  if( !layer || layer->GetLayerType() != moMapLayer )
    return;

  CMapLayer* mapLayer = (CMapLayer*)layer;
  if( !mapLayer )
     return;

  CMoMap* map = m_view->GetDocument()->GetMap();
  CMoPolygon poly( map->TrackPolygon() );
  if ( LPDISPATCH( poly ) )
  {
    // die Cursorposition hat sich während des digitalisierens verändert
    // deshalb kann nicht einfach 'cursor' an den ShapeMover übergeben werden
    // sondern es hängt ab vom digitalisierten Poly
    CMoParts parts( poly.GetParts() );
    if( parts.GetCount() != 1 )
    {
      TRACE( "Polyogon mit 0 oder mehr als einem Teil digitalisiert" );
      return;
    }

    CMoPoints points( parts.Item( CComVariant( 0 ) ) );
    if( points.GetCount() < 2 )
    {
      TRACE( "Leeres Polygon digitalisiert" );
      return;
    }

    // die Cursorposition für den Shapemover auf diese Position setzen
    // warum es gerade der 2. Punkt ist??? klappt aber!
    CMoPoint lastCursor( points.Item( CComVariant( 1 ) ) );

    // erstmal alle Objekte holen, die überhaupt in Frage kommen
    CMoRecordset records( mapLayer->SearchShape( poly, moEdgeTouchOrAreaIntersect, "") );
    
    // diese Records nach einem geeigneten Schnittobjekt durchsuchen ( das erste wird genommen )
    CMoRectangle fullExtent( map->GetFullExtent() ); // hilfsrechteck für 'Intersect'
    CMoFields fields( records.GetFields() );
    
    CMoField shapeField( fields.Item( COleVariant( MO2_FIELD_SHAPE ) ) );
    for( ; !records.GetEof(); records.MoveNext() )
    {
      LPDISPATCH disp = shapeField.GetValue().pdispVal;
      COleDispatchDriver object( disp, TRUE ); // release the dispatch after use
      
      COleDispatchDriverEx schnitt;
      
      switch( mapLayer->GetShapeType() )
      {
      case moShapeTypeLine:
        {
          CMoLine line( object );   // hier wird die Version CMoLine ( LPDISPATCH disp ) aufgerufen !
          line.m_bAutoRelease = FALSE; // do not release the dispatch
          schnitt = COleDispatchDriver( line.Intersect( poly, CComVariant( fullExtent ) ) );
        }
        break;
            
      case moShapeTypePolygon:
        {
          CMoPolygon polygon( object );   
          polygon.m_bAutoRelease = FALSE; // do not release the dispatch
          schnitt = COleDispatchDriver( polygon.Intersect( poly, CComVariant ( fullExtent ) ) );
        }
        break;
        
      default:
        ASSERT( FALSE ); // darf nicht sein
      };
          
      if( LPDISPATCH( schnitt ) && 
        long( COleVariantEx( schnitt.GetProperty( "ShapeType" ) ) ) == mapLayer->GetShapeType() )
      { 
        m_shapeMover = new CShapeMover( map->GetTrackingLayer(), mapLayer->GetSymbol(), schnitt, -1, 0, lastCursor );
        return;
      };
    }; 
  }; // if poly

  ::MessageBeep( MB_ICONQUESTION );		// not found
}


void CClipObjectController::OnMapMouseMove( CMoPoint& cursor, short Button, short Shift )
{
  if( m_shapeMover )
    m_shapeMover->MoveTo( cursor );
}

void CClipObjectController::OnMapMouseUp( CMoPoint& cursor, short Button, short Shift )
{
  CLayer* layer = m_view->GetDocument()->GetActiveLayer();
  if( !layer || layer->GetLayerType() != moMapLayer )
    return;

  CLayerData* layerData = m_view->GetDocument()->GetLayerData( layer->GetType() );
  CMapLayer* mapLayer = (CMapLayer*)layer;
  if( !mapLayer || !layerData )
     return;

  // falls gerade kein Objekt bewegt wird nichts tun
  if( !m_shapeMover )
    return;

  // dem Layer das neue Shape hinzufügen
  CMoRecordset layerRecs( mapLayer->GetRecords() );
  CMoFields layerFields( layerRecs.GetFields() );
  CMoField layerShapeField( layerFields.Item( COleVariant( MO2_FIELD_SHAPE ) ) );

  LPDISPATCH eventDisp = CMoGeoEvent( m_shapeMover->GetShape() ).GetShape();

  COleDispatchDriver shape;
  switch( mapLayer->GetShapeType() )
  {
  case moShapeTypeLine:
    {
      // das eventShape direkt zu übertragen geht hier leider nicht Warum??
      CMoLine eventLine( eventDisp );
      CMoParts eventParts( eventLine.GetParts() );
      CMoLine shapeLine;
      MO2CREATE( shapeLine, "Line" );
      CMoParts shapeParts( shapeLine.GetParts() );
      for (int i = 0; i < eventParts.GetCount(); i++)
      {
        CMoPoints points( eventParts.Item( CComVariant( i ) ) );
        shapeParts.Add( points );
      };
      shape = shapeLine;
    };
    break;
    
  case moShapeTypePolygon:
    { 
      // das eventShape direkt zu übertragen geht hier leider nicht Warum??
      CMoPolygon eventPoly( eventDisp );
      CMoParts eventParts( eventPoly.GetParts() );
      CMoPolygon shapePoly;
      MO2CREATE( shapePoly, "Polygon" );
      CMoParts shapeParts( shapePoly.GetParts() );
      for (int i = 0; i < eventParts.GetCount(); i++)
      {
        CMoPoints points( eventParts.Item( CComVariant(i) ) );
        shapeParts.Add( points );
      };
      shape = shapePoly;
    };
    break;

  default:
    ASSERT( FALSE );
  }

  delete m_shapeMover;
  m_shapeMover = 0;

  // jetz die Attribute abfragen
  CObjectQueryDialog dlg( m_view, TRUE );
  dlg.m_position.Format( "X: %f, Y: %f", cursor.GetX(), cursor.GetY() );
  dlg.m_numfound.LoadString( IDS_1OBJECTFOUND );
  dlg.m_layername = mapLayer->GetName();
  dlg.m_nShapeType = mapLayer->GetShapeType();
  
  CMoTableDesc tDesc( layerRecs.GetTableDesc() );

  ObjectQuery pOQ;
  CMapHelper::WriteTableDescToObjectQuery( tDesc, pOQ, CMapStringToVariant() );
  dlg.m_queries.Add( &pOQ );
    
  CMoMap* pMap = m_view->GetDocument()->GetMap();
  CMarkMapObject mmo( shape, mapLayer, pMap->GetExtent(), pMap->GetTrackingLayer(), MO2_TRACKSYMBOL_MARK_OBJECT );

  if( dlg.DoModal() == IDOK )
  {
    layerRecs.AddNew();
  
    layerShapeField.SetValue( CComVariant( shape ) );

    CMapHelper::WriteObjectQueryToFields( pOQ, layerFields );
      
    layerRecs.Update();
    m_view->GetDocument()->FireMapDocChanged( CMapView::THEME_GEOMETRY_CHANGED | CMapView::THEME_DATA_CHANGED, NULL );
  }; // if dlg.DoModal
}