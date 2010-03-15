// QueryObjectController.cpp: Implementierung der Klasse CQueryObjectController.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "mapView.h"
#include "mapDoc.h"
#include "obqrydlg.h"
#include "maphelper.h"

#include "QueryObjectController.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

bool CQueryObjectController::IsEnabled()
{
  CLayer* layer = m_view->GetDocument()->GetActiveLayer();
  return layer && layer->GetLayerType() == moMapLayer;
}

void CQueryObjectController::OnMapMouseDown( CMoPoint& cursor, short Button, short Shift )
{
  CLayer* layer = m_view->GetDocument()->GetActiveLayer();
  if( !layer || layer->GetLayerType() != moMapLayer )
      return;

  CMapLayer* mapLayer = (CMapLayer*)layer;
  CLayer::LayerType layerType = mapLayer->GetType();
  CLayerData* layerData = m_view->GetDocument()->GetLayerData( layerType );
  
  BOOL bEdit = ( layerType == CLayer::user_RW ||
    layerType == CLayer::buhnen || 
    layerType == CLayer::waterLevel || 
    layerType == CLayer::trennflaechen );
  
  CMoRecordset selectedRecords(mapLayer->SearchByDistance(LPDISPATCH(cursor), m_view->GetSearchDistance(), TEXT("")));
  if( selectedRecords.GetCount() == 0 )
    ::MessageBeep( MB_ICONQUESTION );		// not found
  else
  {
    CObjectQueryDialog dlg( m_view, bEdit );
    dlg.m_position.Format("X: %f, Y: %f", cursor.GetX(), cursor.GetY());
    dlg.m_numfound.FormatMessage(IDS_NOBJECTSFOUND, selectedRecords.GetCount());
    dlg.m_layername = mapLayer->GetName();
    dlg.m_nShapeType = mapLayer->GetShapeType();
    
    CMoFields fields( selectedRecords.GetFields() );

    std::vector< std::auto_ptr<ObjectQuery> > objectQueries;

    while( !selectedRecords.GetEof() )
    {
      ObjectQuery* pOQ = new ObjectQuery;
      std::auto_ptr<ObjectQuery> autoOQ( pOQ );

      objectQueries.push_back( autoOQ );

      CMapHelper::WriteFieldsToObjectQuery( fields, *pOQ );

      dlg.m_queries.Add( pOQ );
      selectedRecords.MoveNext();
    };  // while selecteRecords
    
    if( dlg.DoModal() == IDOK && bEdit )
    {
      selectedRecords.MoveFirst();
      int count = 0;
      while( !selectedRecords.GetEof() )
      {
        std::auto_ptr<ObjectQuery> pOQ = objectQueries[count];

        selectedRecords.Edit();

        CMapHelper::WriteObjectQueryToFields( *pOQ, fields );

        selectedRecords.Update();
        selectedRecords.MoveNext();
        count++;
      }; // while selectedRecords
      m_view->GetDocument()->FireMapDocChanged( IMapDocListener::THEME_DATA_CHANGED, NULL );
    }; // if dl.DOModal
  }; // else
}