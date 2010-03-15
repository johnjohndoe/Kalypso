// MovePointController.cpp: Implementierung der Klasse CMovePointController.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "layerData.h"
#include "LinePointMover.h"

#include "MovePointController.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

CMovePointController::CMovePointController( CMapView* view ) : IMapController( view ) 
{
  m_mapMover = 0; // wegen delete in reset

  reset();
}

CMovePointController::~CMovePointController() 
{
  reset();
}

void CMovePointController::Cancel()
{
  reset();
}

bool CMovePointController::IsEnabled()
{
  CLayer* layer = m_view->GetDocument()->GetActiveLayer();
  if( !layer || layer->GetLayerType() != moMapLayer )
    return false;

  CLayerData* layerData = m_view->GetDocument()->GetLayerData( layer->GetType() );
  CMapLayer* mapLayer = (CMapLayer*)layer;

  return layerData && layerData->bObjectGeometryEditable && mapLayer->GetShapeType() != moShapeTypePoint;
}

void CMovePointController::OnMapMouseDown( CMoPoint& cursor, short Button, short Shift )
{
  CLayer* layer = m_view->GetDocument()->GetActiveLayer();
  if( !layer || layer->GetLayerType() != moMapLayer )
  {
    reset();
    return;
  }

  m_mapLayer = (CMapLayer*)layer;

  m_lFeatureID = m_mapLayer->SearchNextPointByDistance( cursor, m_view->GetSearchDistance(), m_dwPointData );
  LPDISPATCH shapeDisp = m_mapLayer->GetFieldValByID( m_lFeatureID, MO2_FIELD_SHAPE ).pdispVal;

  if( m_lFeatureID == -1 )
    ::MessageBeep( MB_ICONQUESTION );		// not found
  else
    m_mapMover = new CLinePointMover( m_view->GetDocument()->GetMap()->GetTrackingLayer(), shapeDisp, m_mapLayer->GetShapeType(), m_dwPointData, cursor );
}

void CMovePointController::OnMapMouseMove( CMoPoint& cursor, short Button, short Shift )
{
  if( m_mapMover )
    m_mapMover->MoveTo( cursor );
}


void CMovePointController::OnMapMouseUp( CMoPoint& cursor, short Button, short Shift )
{
  if( m_mapMover && m_mapLayer )
  {
    m_mapLayer->MovePoint( cursor, m_lFeatureID, m_dwPointData );

    reset();

    m_view->GetDocument()->FireMapDocChanged( IMapDocListener::THEME_GEOMETRY_CHANGED, NULL );
  }

}