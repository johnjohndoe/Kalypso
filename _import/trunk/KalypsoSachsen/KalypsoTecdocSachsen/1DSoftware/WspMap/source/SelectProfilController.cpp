// SelectProfilController.cpp: Implementierung der Klasse CSelectProfilController.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "maplayer.h"
#include "mapview.h"
#include "mapdoc.h"

#include "SelectProfilController.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

bool CSelectProfilController::IsEnabled()
{
  CMapLayer* profilLayer = m_view->GetDocument()->GetLayers()->FindFirstLayer( CLayer::profilLines );
  return profilLayer != 0;
}

void CSelectProfilController::OnMapMouseDown( CMoPoint& cursor, short Button, short Shift )
{
  CMapLayer* profilLayer = m_view->GetDocument()->GetLayers()->FindFirstLayer( CLayer::profilLines );
  if( profilLayer )
  {
    m_profilID = profilLayer->SearchNextObjectByDistance( cursor, m_view->GetSearchDistance() );
    if( m_profilID == -1 )
      ::MessageBeep( MB_ICONQUESTION );		// not found
    else
      m_view->GetDocument()->SetActiveProfile( m_profilID, TRUE );
  }
}


void CSelectProfilController::OnMapDblClick()
{
  if( m_profilID != -1 )
    m_view->GetDocument()->EditProfile( m_profilID );
};
