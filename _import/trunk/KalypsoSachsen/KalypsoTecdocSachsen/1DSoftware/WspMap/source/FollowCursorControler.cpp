// FollowCursorControler.cpp: Implementierung der Klasse CFollowCursorControler.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "mapHelper.h"

#include "FollowCursorControler.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

CFollowCursorControler::CFollowCursorControler( CMapView* pView ) : IMapController( pView ) 
{
}

CFollowCursorControler::~CFollowCursorControler()
{}

void CFollowCursorControler::OnMapMouseMove( CMoPoint& cursor, short Button, short Shift )
{
  CMapLayer* profilLayer = m_view->GetDocument()->GetLayers()->FindFirstLayer( CLayer::profilLines );
  if( profilLayer )
  {
    const long profilID = m_view->GetDocument()->GetActiveProfile();

    if( profilID > 0 )
    {
      CMoPoint point;
      MO2CREATE( point, "Point" );
      point.SetX( cursor.GetX() );
      point.SetY( cursor.GetY() );

      const double yKrd = m_view->GetDocument()->GetLayers()->ProjectToProfilLine( point, profilID );

      m_view->GetDocument()->SetProfilCursor( yKrd );
    }
  };
}