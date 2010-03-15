// NewObjectController.cpp: Implementierung der Klasse CNewObjectController.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "commonMfc/include/variant_helper.h"

#include "mapView.h"
#include "mapDoc.h"
#include "layerData.h"
#include "obqrydlg.h"
#include "maphelper.h"
#include "markMapObject.h"

#include "NewObjectController.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif


bool CNewObjectController::IsEnabled()
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

void CNewObjectController::OnMapMouseDown( CMoPoint& cursor, short Button, short Shift )
{
  CLayer* layer = m_view->GetDocument()->GetActiveLayer();
  if( !layer || layer->GetLayerType() != moMapLayer )
    return;

  CLayerData* layerData = m_view->GetDocument()->GetLayerData( layer->GetType() );

  long pointID = -1;
      
  if( layerData->nProfilBezug > -1 )
  { 
    // cursor auf die nächste Profillinie/oder Profilpunkt projizieren 
    // (falls eine/r in der Nähe ist)
    pointID = m_view->GetDocument()->GetLayers()->ProjectToProfilPointByDistance( cursor, m_view->GetSearchDistance() );
    
    if ( pointID == -1 )
      return;
      //cursor.ReleaseDispatch();
  }; // if layerData

  CMapLayer* mapLayer = (CMapLayer*)layer;
  CMoMap* map = m_view->GetDocument()->GetMap();
  long shapeType = mapLayer->GetShapeType();

  LPDISPATCH shapeDisp = NULL; // das hinzuzufügende Objekt
  switch( shapeType )
  {
  case moLine:
    {
      CMoLine line( map->TrackLine() );
      if( LPDISPATCH(line) )
      {
        CMoParts parts( line.GetParts() );
        CMoPoints points( parts.Item( CComVariant(0) ) );
        if( points.GetCount() > 1 )
        {
          shapeDisp = LPDISPATCH(line);
          shapeDisp->AddRef();
        };
      };
    };
    break;

  case moPolygon:
    {
      CMoPolygon poly( map->TrackPolygon() );
      if( LPDISPATCH(poly) )
      {
        CMoParts parts( poly.GetParts() );
        CMoPoints points( parts.Item( CComVariant(0) ) );
        if( points.GetCount() > 2 )
        {
          shapeDisp = LPDISPATCH(poly);
          shapeDisp->AddRef();
        };
      };
    };
    break;

  case moPoint:
    {
      if( LPDISPATCH(cursor) )
      {
        shapeDisp = LPDISPATCH(cursor);
        shapeDisp->AddRef();
      };
    } // case moPoint
  }; // switch shapeType
      

  if( shapeDisp != NULL )
  {
    CMoRecordset recs( mapLayer->GetRecords() );
    CMoTableDesc tDesc( recs.GetTableDesc() );
    CMoFields fields( recs.GetFields() );

    // für Punkte, die auf Profilpunkten liegen gelten Sonderregeln
    CMapStringToVariant pointAttributes;
    if( pointID != -1 )
    {
      // die Daten des Profilpunkts auslesen: mit diesen wird der neue Punkt vorinitialisiert
      CMapLayer* pointLayer = m_view->GetDocument()->GetLayers()->FindFirstLayer( CLayer::profilPoints );
      if( pointLayer )
        pointLayer->GetAttributes( pointID, pointAttributes );
      
      // falls es eine beschränkung in der Punktezahl gibt, dies jetzt auch überprüfen
      if( layerData->nProfilBezug > 0 )
      {
        // die ProfilID feststellen
        CComVariant idVar;
        if( pointAttributes.Lookup( MO2_FIELD_PROFILID, idVar ) || idVar.vt != VT_ERROR )
        {
          const long profilID = COleVariantEx( idVar );
          CString searchExpr;
          searchExpr.Format( "%s = %d", MO2_FIELD_PROFILID, profilID );
          CMoRecordset mapRecords( mapLayer->SearchExpression( searchExpr ) );
          
          // die Anzahl steht nach dieser Suche vermutlich nicht fest:
          CMoStatistics mapStats( mapRecords.CalculateStatistics( MO2_FIELD_PROFILID ) );
          if( LPDISPATCH( mapStats ) )
          {
            if( mapStats.GetCount() >= layerData->nProfilBezug )
            {
              CString message;
              message.Format( IDS_COUNT_TOO_BIG, layerData->nProfilBezug );
              AfxMessageBox( message, MB_ERROR );
              return;
            }; // if Count > nProfilBezug
          }; // if mapStats
        }; // if lookup
      }; // if( layerData->nProfilBezug > 0 )
    }; // if pointID != -1
    
    CObjectQueryDialog dlg( m_view, TRUE );
    dlg.m_position.Format( "X: %f, Y: %f", cursor.GetX(), cursor.GetY() );
    dlg.m_numfound.LoadString( IDS_1OBJECTFOUND );
    dlg.m_layername = mapLayer->GetName();
    dlg.m_nShapeType = shapeType;
    
    ObjectQuery pOQ;
    CMapHelper::WriteTableDescToObjectQuery( tDesc, pOQ, pointAttributes );
    dlg.m_queries.Add( &pOQ );
    
    CMarkMapObject mmo( shapeDisp, mapLayer, map->GetExtent(), map->GetTrackingLayer(), MO2_TRACKSYMBOL_MARK_OBJECT );

    if( dlg.DoModal() == IDOK )
    {
      recs.AddNew();
      
      SetValue( fields, MO2_FIELD_SHAPE, shapeDisp );

      CMapHelper::WriteObjectQueryToFields( pOQ, fields );
      
      recs.Update();
      m_view->GetDocument()->FireMapDocChanged( IMapDocListener::THEME_DATA_CHANGED | IMapDocListener::THEME_GEOMETRY_CHANGED, mapLayer );
    }; // if dlg.DoModal
  }  
  else
  {
    ::MessageBeep(MB_ICONQUESTION);
  }; // if 'Shape richtig getracked'
}