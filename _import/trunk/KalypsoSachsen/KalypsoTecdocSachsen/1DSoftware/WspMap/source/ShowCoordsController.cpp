// ShowCoordsController.cpp: Implementierung der Klasse CShowCoordsController.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "ShowCoordsController.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

CShowCoordsController::CShowCoordsController( CMapView* view ) : IMapController( view ) {}

CShowCoordsController::~CShowCoordsController() {}

void CShowCoordsController::OnMapMouseMove( CMoPoint& cursor, short Button, short Shift )
{
  m_view->SetCursorPos( CDoublePoint( cursor.GetX(), cursor.GetY() ) );
}
