// LinePointMover.cpp: Implementierung der Klasse CLinePointMover.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "../mapObjects/moTrackingLayer.h"
#include "IMapChanger.h"
#include "mapLayer.h"

#include "LinePointMover.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif


CLinePointMover::CLinePointMover( CMoTrackingLayer trackingLayer, LPDISPATCH shapeDisp, const long shapeType, const DWORD dwPointData, CMoPoint& cursor ) : IMapChanger( trackingLayer, cursor )
{
  m_dwPointData = dwPointData;

  CMoLine trackLine;
  MO2CREATE( trackLine, "Line" );
  CMoParts trackParts( trackLine.GetParts() );
  CMoPoints trackPoints;
  MO2CREATE( trackPoints, "Points" );
  CMoPoint trackPointA, trackPointE; // Anfangs und Endpunkt der tracklinie

  int index = LOWORD( dwPointData );

  switch( shapeType )
  {
  case moShapeTypeLine:
    {
      shapeDisp->AddRef();
      CMoLine line( shapeDisp );
      CMoParts parts( line.GetParts() );
      CMoPoints points( parts.Item( CComVariant( ( HIWORD( dwPointData ) ) ) ) );
      int last = points.GetCount() - 1;
      if( index > 0 )
        trackPointA = points.Item( CComVariant( index - 1) );
      if( index < last )
        trackPointE = points.Item( CComVariant( index + 1 ) );
    };
    break;

  case moShapeTypePolygon:
    {
      shapeDisp->AddRef();
      CMoPolygon poly( shapeDisp );
      CMoParts parts( poly.GetParts() );
      CMoPoints points( parts.Item( CComVariant( HIWORD( m_dwPointData ) ) ) );
      int last = points.GetCount() - 1;
      trackPointA = points.Item( CComVariant( index == 0 ? last : index - 1 ) );
      trackPointE = points.Item( CComVariant( index == last ? 0 : index + 1 ) );
    };
    break;
    
  default:
    ASSERT( FALSE );
  };
        
  if( LPDISPATCH( trackPointA ) )
    trackPoints.Add( trackPointA );
  
  trackPoints.Add( cursor );
  
  if( LPDISPATCH( trackPointE ) )
    trackPoints.Add( trackPointE );
    
  trackParts.Add( trackPoints );
  CMoGeoEvent event( GetTrackingLayer().AddEvent( trackLine, MO2_TRACKSYMBOL_MOVEPOINT ) );
  event.SetTag( MO2_TRACKTAG_MOVEPOINT );
}

CLinePointMover::~CLinePointMover()
{
  CMoGeoEvent event( GetTrackingLayer().FindEvent( MO2_TRACKTAG_MOVEPOINT ) );
  GetTrackingLayer().RemoveEvent( event.GetIndex() );
}


void CLinePointMover::MoveToInternal( CMoPoint& cursor )
{
  CMoGeoEvent event( GetTrackingLayer().FindEvent( MO2_TRACKTAG_MOVEPOINT ) );

  if( LPDISPATCH( event ) )
  {
    CMoLine line( event.GetShape() );
    CMoParts parts( line.GetParts() );
    CMoPoints points( parts.Item( COleVariant( (short)0 ) ) );
        
    if( LOWORD( m_dwPointData ) == 0 && points.GetCount() != 3 )
      points.Set( 0, cursor );
    else
      points.Set( 1, cursor );
        
    event.SetShape( line );
  }; // if trackingLayer
}
