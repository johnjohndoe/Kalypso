// ShapeMover.cpp: Implementierung der Klasse CShapeMover.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "maphelper.h"
#include "mapLayer.h"

#include "ShapeMover.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

CShapeMover::CShapeMover( CMoTrackingLayer trackingLayer, CMoSymbol& symbol, LPDISPATCH shape, const long profilID, CMapLayer* pointLayer, CMoPoint& cursor ) : IMapChanger( trackingLayer, cursor )
{
  m_profilID = profilID;
  m_pointLayer = pointLayer;

  // Symbol - kopieren
  CMoSymbol trackSymbol( trackingLayer.GetSymbol( MO2_TRACKSYMBOL_MOVEOBJECT ) );
  trackSymbol.SetSymbolType( symbol.GetSymbolType() );
  trackSymbol.SetColor( symbol.GetColor() );
  trackSymbol.SetStyle( symbol.GetStyle() );
  trackSymbol.SetSize( symbol.GetSize() );
  
  CMoGeoEvent event( trackingLayer.AddEvent( shape, MO2_TRACKSYMBOL_MOVEOBJECT ) );
  event.SetTag( MO2_TRACKTAG_MOVEOBJECT );
}

CShapeMover::~CShapeMover()
{
  CMoGeoEvent event( GetTrackingLayer().FindEvent( MO2_TRACKTAG_MOVEOBJECT ) );
  if( LPDISPATCH( event ) )
    GetTrackingLayer().RemoveEvent( event.GetIndex() );
  else
    TRACE( "CShapeMover: failure during delete: no event" );
}

void CShapeMover::MoveToInternal( CMoPoint& cursor )
{
  CMoGeoEvent event( GetTrackingLayer().FindEvent( MO2_TRACKTAG_MOVEOBJECT ) );
  if( LPDISPATCH( event ) )
  {
    if( m_pointLayer ) // ggfls. cursor auf Profillinie projizieren
    {
      CMapStringToVariant attr;
      m_pointLayer->FindNextPoint( cursor, m_profilID, attr );
      event.MoveTo( cursor.GetX(), cursor.GetY() );
    }
    else
    {
      event.Move( cursor.GetX() - GetLastCursor().GetX(), cursor.GetY() - GetLastCursor().GetY() );
      //event.Move( cursor.GetX(), cursor.GetY() );
    }
  }
}

CMoGeoEvent CShapeMover::GetShape()
{
  CMoGeoEvent event( GetTrackingLayer().FindEvent( MO2_TRACKTAG_MOVEOBJECT ) );
  return event;
}
