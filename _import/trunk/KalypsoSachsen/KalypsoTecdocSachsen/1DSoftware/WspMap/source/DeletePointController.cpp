// DeletePointController.cpp: Implementierung der Klasse CDeletePointController.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "layerData.h"

#include "DeletePointController.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

CDeletePointController::CDeletePointController( CMapView* view ) : IMapController( view )
{
  m_lFeatureID = -1;
  m_dwPointData = -1;
}

CDeletePointController::~CDeletePointController() {}


bool CDeletePointController::IsEnabled()
{
  CLayer* layer = m_view->GetDocument()->GetActiveLayer();
  if( !layer || layer->GetLayerType() != moMapLayer )
    return false;

  CLayerData* layerData = m_view->GetDocument()->GetLayerData( layer->GetType() );
  CMapLayer* mapLayer = (CMapLayer*)layer;

  return layerData && layerData->bObjectGeometryEditable && mapLayer->GetShapeType() != moShapeTypePoint;
}

void CDeletePointController::OnMapMouseDown( CMoPoint& cursor, short Button, short Shift )
{
  CLayer* layer = m_view->GetDocument()->GetActiveLayer();
  if( !layer || layer->GetLayerType() != moMapLayer )
    return;

  CMapLayer* mapLayer = (CMapLayer*)layer;

  m_lFeatureID = mapLayer->SearchNextPointByDistance( cursor, m_view->GetSearchDistance(), m_dwPointData );
}

void CDeletePointController::OnMapMouseUp( CMoPoint& cursor, short Button, short Shift )
{
  CLayer* layer = m_view->GetDocument()->GetActiveLayer();
  if( !layer || layer->GetLayerType() != moMapLayer )
    return;
  
  CMapLayer* mapLayer = (CMapLayer*)layer;

  DWORD pointData = -1;
  long lFeatureID = mapLayer->SearchNextPointByDistance( cursor, m_view->GetSearchDistance(), pointData );


  BOOL bError = TRUE;
  COleDispatchDriver targetShape;
  
  if( lFeatureID != -1 && lFeatureID == m_lFeatureID && m_dwPointData == pointData )
  {
    switch( mapLayer->GetShapeType() )
    {
    case moShapeTypeLine:
      {
        CMoLine line( mapLayer->GetFieldValByID( m_lFeatureID, MO2_FIELD_SHAPE ).pdispVal );
        CMoParts parts( line.GetParts() );
        CMoPoints points( parts.Item( CComVariant( HIWORD( m_dwPointData ) ) ) );
        if( points.GetCount() > 2 )
        {
          points.Remove( LOWORD( m_dwPointData ) );
          targetShape = line;
          bError = FALSE;
        }
      };
      break;

    case moShapeTypePolygon:
      {
        CMoPolygon poly( mapLayer->GetFieldValByID( m_lFeatureID, MO2_FIELD_SHAPE ).pdispVal );
        CMoParts parts( poly.GetParts() );
        CMoPoints points( parts.Item( CComVariant( HIWORD( m_dwPointData) ) ) );
        if( points.GetCount() > 3 )
        {
          points.Remove( LOWORD( m_dwPointData ) );
          targetShape = poly;
          bError = FALSE;
        }
      };
      break;

    default:
      ASSERT( FALSE );
    }; // switch shapeType
  }; // if m_lFeatureID
      
  if( bError )
    ::MessageBeep( MB_ICONQUESTION );
  else
  {
    mapLayer->SetFieldValByID( m_lFeatureID, MO2_FIELD_SHAPE, CComVariant( targetShape ) );
    m_view->GetDocument()->FireMapDocChanged( CMapView::THEME_GEOMETRY_CHANGED, NULL );
  };
  
  m_lFeatureID = -1;
  m_dwPointData = -1;
}