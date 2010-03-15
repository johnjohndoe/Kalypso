// GroupController.cpp: Implementierung der Klasse CGroupController.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "maphelper.h"

#include "GroupController.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

CGroupController::CGroupController( CMapView* view ) : IMultiMapController( view ) 
{
  m_activeID = -1; 
  m_activeController = 0;
};

CGroupController::~CGroupController() {};

void CGroupController::Toggle( const long id )
{
  if( m_activeController )
    m_activeController->Toggle();

  m_activeID = -1;
  m_activeController = 0;
  m_view->GetDocument()->GetMap()->SetMousePointer( moArrow );
  
  ControllerMap::iterator cIt = Find( id );
  if( cIt != GetControllerEnd() )
  {
    m_activeID = id;
    m_activeController = (*cIt).second.get();

    if( m_activeController )
      m_activeController->Toggle();

    m_view->GetDocument()->GetMap()->SetMousePointer( m_activeController->MousePointer() );
  }
};

void CGroupController::OnMapDblClick()
{
  if( m_activeController )
    m_activeController->OnMapDblClick();
};

void CGroupController::OnMapMouseDown( CMoPoint& cursor, short Button, short Shift )
{
  if( m_activeController )
    m_activeController->OnMapMouseDown( cursor, Button, Shift );
};

void CGroupController::OnMapMouseMove( CMoPoint& cursor, short Button, short Shift )
{
  if( m_activeController )
    m_activeController->OnMapMouseMove( cursor, Button, Shift );
};

void CGroupController::OnMapMouseUp( CMoPoint& cursor, short Button, short Shift )
{
  if( m_activeController )
    m_activeController->OnMapMouseUp( cursor, Button, Shift );
};

void CGroupController::Cancel()
{
  if( m_activeController )
    m_activeController->Cancel();
};
