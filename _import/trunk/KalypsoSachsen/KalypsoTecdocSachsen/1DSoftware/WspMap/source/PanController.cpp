// PanController.cpp: Implementierung der Klasse CPanController.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "mapview.h"
#include "mapDoc.h"
#include "mapHelper.h"

#include "PanController.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

CPanController::CPanController( CMapView* view, const short Button, const short Shift ) :IMapController( view, true ), m_button( Button ), m_shift( Shift )
{
  m_bHot = false;
  MO2CREATE( m_hotSpot, "Point" );
}

void CPanController::OnMapMouseDown( CMoPoint& cursor, short Button, short Shift )
{
  if( Button == m_button && Shift == m_shift )
  {
    m_bHot = true;
    m_hotSpot.SetX( cursor.GetX() );
    m_hotSpot.SetY( cursor.GetY() );
  }
}

void CPanController::OnMapMouseUp( CMoPoint& cursor, short Button, short Shift )
{
  m_bHot = false;
}

void CPanController::OnMapMouseMove( CMoPoint& cursor, short Button, short Shift )
{
  if( m_bHot && m_hotSpot.DistanceTo( cursor ) > m_view->GetSearchDistance() )
  {
    CMoMap* pMap = m_view->GetDocument()->GetMap();
    
    const long mousePointer = pMap->GetMousePointer();
    pMap->SetMousePointer( this->MousePointer() );
    
    pMap->Pan();
    
    pMap->SetMousePointer( mousePointer );
    
    m_view->GetDocument()->FireMapDocChanged( IMapDocListener::EXTENT_CHANGED, NULL );

    m_bHot = false;
  }
}