// DeleteObjectController.cpp: Implementierung der Klasse CDeleteObjectController.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "maphelper.h"
#include "mapView.h"
#include "mapDoc.h"
#include "layerData.h"

#include "DeleteObjectController.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif


CDeleteObjectController::CDeleteObjectController( CMapView* view  ) : IMapController( view )
{
  m_featureID = -1; 
  MO2CREATE( m_selectPoint, "Point" );
};

bool CDeleteObjectController::IsEnabled()
{
  CLayer* layer = m_view->GetDocument()->GetActiveLayer();

  if( layer )
  {
    CLayer::LayerType type = layer->GetType();
    CLayerData* layerData = m_view->GetDocument()->GetLayerData( type );

    return layerData && ( layerData->bObjectGeometryEditable  || type == CLayer::profilLines );
  }
  else
    return false;
}

void CDeleteObjectController::OnMapMouseDown( CMoPoint& cursor, short Button, short Shift )
{
  // nur das zu löschende Objekt auswählen
  CLayer *layer = m_view->GetDocument()->GetActiveLayer();
  if( layer && layer->GetType() != CLayer::image )
  {
    CMapLayer* mapLayer = (CMapLayer*)layer;
    m_featureID = mapLayer->SearchNextObjectByDistance( cursor, m_view->GetSearchDistance() );
    m_selectPoint.SetX( cursor.GetX() );
    m_selectPoint.SetY( cursor.GetY() );
  }
}

void CDeleteObjectController::OnMapMouseMove( CMoPoint& cursor, short Button, short Shift )
{
  // falls sich die Maus zu weit bewegt, das selektierte Objekt vergessen
  if( m_featureID != -1 && m_selectPoint.DistanceTo( cursor ) > m_view->GetSearchDistance() )
     m_featureID = -1;
}

void CDeleteObjectController::OnMapMouseUp( CMoPoint& cursor, short Button, short Shift )
{
  if( m_featureID == -1 )
    ::MessageBeep( MB_ICONQUESTION );		// not found
  else
  {
    CLayer *layer = m_view->GetDocument()->GetActiveLayer();
    if( layer && layer->GetType() != CLayer::image )
    {
      CMapLayer* mapLayer = (CMapLayer*)layer;
      if( mapLayer->GetType() == CLayer::profilLines )
      {
        // die Profile auch aus dem Strang rausnehmen?
        int bRet = AfxMessageBox( CString( MAKEINTRESOURCE( IDS_DELETE_PROFILE_FROM_MAP ) ), MB_YESNOCANCEL | MB_ICONQUESTION );
        if( bRet != IDCANCEL )
        {
          CMapStringToVariant attribs;
          mapLayer->GetAttributes( m_featureID, attribs );
          
          CComVariant csNameVar;
          if( !attribs.Lookup( MO2_FIELD_FILE, csNameVar ) )
            return;
          
          CStringArray csNames;
          csNames.Add( CString( csNameVar.bstrVal ) );
          
          m_view->GetDocument()->GetProfilAuswahl()->DeleteProfiles( csNames, bRet == IDYES );
        }; // if bRet
      }
      else
      {
        int error = mapLayer->DeleteObject( m_featureID );
        if( error )
          AfxFormattedMessageBox("Error %d while deleting object", error );
      };
      
      m_view->GetDocument()->FireMapDocChanged( CMapView::THEME_GEOMETRY_CHANGED | CMapView::THEME_DATA_CHANGED, layer );
    };
  };

  m_featureID = -1;
}
