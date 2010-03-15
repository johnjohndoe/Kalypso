// MultiMapController.cpp: Implementierung der Klasse CMultiMapController.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "stdafx.h"
#include "MultiMapController.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif


void CMultiMapController::Toggle( const long id )
{
  ControllerMap::iterator cIt = Find( id );
  if( cIt != GetControllerEnd() )
    (*cIt).second->Toggle();
}

void CMultiMapController::OnMapDblClick()
{
  for( ControllerMap::const_iterator cIt = GetControllerBegin(); cIt != GetControllerEnd(); cIt++ )
  {
    if( (*cIt).second->IsChecked() )
      (*cIt).second->OnMapDblClick();
  }
};

void CMultiMapController::OnMapMouseDown( CMoPoint& cursor, short Button, short Shift )
{
  for( ControllerMap::const_iterator cIt = GetControllerBegin(); cIt != GetControllerEnd(); cIt++ )
  {
    if( (*cIt).second->IsChecked() )
      (*cIt).second->OnMapMouseDown( cursor, Button, Shift );
  }
};

void CMultiMapController::OnMapMouseMove( CMoPoint& cursor, short Button, short Shift )
{
  for( ControllerMap::const_iterator cIt = GetControllerBegin(); cIt != GetControllerEnd(); cIt++ )
  {
    if( (*cIt).second->IsChecked() )
      (*cIt).second->OnMapMouseMove( cursor, Button, Shift );
  }
};

void CMultiMapController::OnMapMouseUp( CMoPoint& cursor, short Button, short Shift )
{
  for( ControllerMap::const_iterator cIt = GetControllerBegin(); cIt != GetControllerEnd(); cIt++ )
  {
    if( (*cIt).second->IsChecked() )
      (*cIt).second->OnMapMouseUp( cursor, Button, Shift );
  }
};

void CMultiMapController::Cancel()
{
  for( ControllerMap::const_iterator cIt = GetControllerBegin(); cIt != GetControllerEnd(); cIt++ )
  {
    if( (*cIt).second->IsChecked() )
      (*cIt).second->Cancel();
  }
};
