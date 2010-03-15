// FollowProfileControler.cpp: Implementierung der Klasse CFollowProfileControler.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "mapHelper.h"

#include "FollowProfileControler.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

CFollowProfileControler::CFollowProfileControler( CMapView* view ) : IMapController( view ) {};
CFollowProfileControler::~CFollowProfileControler() {};

void CFollowProfileControler::OnMapMouseMove( CMoPoint& cursor, short Button, short Shift )
{
  CMapLayer* profilLayer = m_view->GetDocument()->GetLayers()->FindFirstLayer( CLayer::profilLines );
  if( profilLayer )
  {
    long profilID = profilLayer->SearchNextObjectByDistance( cursor, m_view->GetSearchDistance() );
    if( profilID != -1 )
      m_view->GetDocument()->SetActiveProfile( profilID, TRUE );
  };
}
