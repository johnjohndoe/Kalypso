// NilController.cpp: Implementierung der Klasse CNilController.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "NilController.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

CNilController::CNilController( CMapView* view ) : IMapController( view ) {};
CNilController::~CNilController() {};

void CNilController::OnMapMouseDown( CMoPoint& cursor, short Button, short Shift )
{
  for( short i = 0; i < m_view->GetDocument()->GetLayerCount(); i++ )
  {
    CLayer *layer = m_view->GetDocument()->GetLayer(i);
    if( ( !layer->GetVisible() ) || ( layer->GetLayerType() != moMapLayer ) )
      continue;
          
    CMapLayer *mapLayer = (CMapLayer*)layer;

    if( mapLayer->SearchNextObjectByDistance( cursor, m_view->GetSearchDistance()) != -1 )
    {
      m_view->GetDocument()->SetOverview( mapLayer->GetDispatch() );
      return;
    };
  }; // for i

  ::MessageBeep( MB_ICONQUESTION );
}