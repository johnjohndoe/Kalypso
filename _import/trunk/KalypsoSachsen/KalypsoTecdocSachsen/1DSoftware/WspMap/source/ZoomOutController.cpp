// ZoomOutController.cpp: Implementierung der Klasse CZoomOutController.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "maphelper.h"

#include "ZoomOutController.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

CZoomOutController::CZoomOutController( CMapView* view ) : IMapController( view )
{
  m_bHot = false;
  MO2CREATE( m_hotSpot, "Point" );
}

CZoomOutController::~CZoomOutController() {}

void CZoomOutController::OnMapDblClick()
{
  // nur falls 'hot', damit nur bei rechter Maustaste gezoomt wird
  if( m_bHot )
  {
    m_view->GetDocument()->SetFullExtent();
    m_bHot = false;
  }
}

void CZoomOutController::OnMapMouseDown( CMoPoint& cursor, short Button, short Shift )
{
  if( Button == 2 && Shift == 0 )
  {
    m_bHot = true;
    m_hotSpot.SetX( cursor.GetX() );
    m_hotSpot.SetY( cursor.GetY() );
  }
}

void CZoomOutController::OnMapMouseMove( CMoPoint& cursor, short Button, short Shift )
{
    // falls beim klicken geruckelt wird, nichts tun
    if( m_bHot && cursor.DistanceTo( m_hotSpot ) > m_view->GetSearchDistance() )
      m_bHot = false;
}


void CZoomOutController::OnMapMouseUp( CMoPoint& cursor, short Button, short Shift )
{
  if( Button == 2 && Shift == 0 )
  {
    if( m_bHot )
      m_view->Zoom( 1.5, cursor );
    
    m_bHot = false;
  }
}