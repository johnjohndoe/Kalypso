// ExtendProfileController.cpp: Implementierung der Klasse CExtendProfileController.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "mapView.h"
#include "mapDoc.h"
#include "maphelper.h"

#include "ExtendProfileController.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

bool CExtendProfileController::IsEnabled()
{
  // es muss ein Geländemodell sowie Profillinien existieren
  return m_view->GetDocument()->GetLayers()->FindFirstLayer( CLayer::hmo ) && 
         m_view->GetDocument()->GetLayers()->FindFirstLayer( CLayer::profilLines ); 
}

void CExtendProfileController::OnMapMouseDown( CMoPoint& cursor, short Button, short Shift )
{
  m_bHot = TRUE;
  m_hotSpot.SetX( cursor.GetX() );
  m_hotSpot.SetY( cursor.GetY() );
}

void CExtendProfileController::OnMapMouseUp( CMoPoint& cursor, short Button, short Shift )
{
  // nur falls jüngst in der Nähe ein MouseDown da war (ohne Doppelklick), wird was getan
  if( !m_bHot )
     return;

  if( m_hotSpot.DistanceTo( cursor ) < m_view->GetSearchDistance() )
  {
    CMapDoc* pDoc = m_view->GetDocument();
    CMoTrackingLayer trackLayer( pDoc->GetMap()->GetTrackingLayer() );
    if( LPDISPATCH( trackLayer ) )
    {
      CMoGeoEvent event( trackLayer.FindEvent( MO2_TRACKTAG_GENERATEPROFILE ) );
      
      if( LPDISPATCH( event ) )
        InsertPoint( trackLayer, cursor, event );
      else
        StartTracking( trackLayer, cursor );
    }
  }
  else
    ::MessageBeep( MB_ICONQUESTION ); // kein Endpunkt einer Profillinie wurde gefunden

  m_bHot = FALSE;
}

void CExtendProfileController::StartTracking( CMoTrackingLayer& trackLayer, CMoPoint& cursor )
{
  // Benutzer ist noch nicht am Linie ziehen -> mit aktion beginnen
  // Hat der Benutzer den Endpunkt einer Profillinie erwischt?
  CMapLayer* profilLayer = m_view->GetDocument()->GetLayers()->FindFirstLayer( CLayer::profilLines );
  if ( profilLayer )
  {
    m_profilID = profilLayer->SearchNextObjectByDistance( cursor, m_view->GetSearchDistance() );
    if( m_profilID != -1 )
    {
      // die Linie suchen und schauen, ob man einen der Endpunkte erwischt hat
      LPDISPATCH disp = profilLayer->GetFieldValByID( m_profilID, MO2_FIELD_SHAPE ).pdispVal;
      if( disp )
      {
        CMoLine line( disp );

        disp->AddRef(); // OK??
        
        // die beiden Endpunkte der Linie holen
        CMoParts lineParts( line.GetParts() );
        if( LPDISPATCH(lineParts) && lineParts.GetCount() > 0 )
        {
          CMoPoints linePoints( lineParts.Item( CComVariant( 0 ) ) );
          if( LPDISPATCH(linePoints) && linePoints.GetCount() > 1 )
          {
            CMoPoint anfPoint( linePoints.Item( CComVariant( 0 ) ) );
            CMoPoint endPoint( linePoints.Item( CComVariant( linePoints.GetCount() - 1 ) ) );
            if( LPDISPATCH(anfPoint) && LPDISPATCH(endPoint) )
            {
              // testen, ob einer dieser Punkte in der Nähe ist
              CMoPoint point, point2; // wird für geradliniges Fortsetzen benötigt

              MO2CREATE( point, "Point" );
              MO2CREATE( point2, "Point" );

              if( anfPoint.DistanceTo( cursor ) < m_view->GetSearchDistance() )
              {
                point.SetX( anfPoint.GetX() );
                point.SetY( anfPoint.GetY() );

                CMoPoint linePoint( linePoints.Item( CComVariant( 1 ) ) );
                point2.SetX( linePoint.GetX() );
                point2.SetY( linePoint.GetY() );
              }
              else if( endPoint.DistanceTo( cursor ) < m_view->GetSearchDistance() )
              {
                point.SetX( endPoint.GetX() );
                point.SetY( endPoint.GetY() );

                CMoPoint linePoint( linePoints.Item( CComVariant( linePoints.GetCount() - 2 ) ) );
                point2.SetX( linePoint.GetX() );
                point2.SetY( linePoint.GetY() );
              }
              else
                ::MessageBeep( MB_ICONQUESTION ); // kein Endpunkt einer Profillinie wurde gefunden
              
              CMoLine newLine;
              MO2CREATE( newLine, "Line" );
              CMoParts newParts( newLine.GetParts() );
              CMoPoints newPoints;
              MO2CREATE( newPoints, "Points" );
              newPoints.Add( point2 );
              newPoints.Add( point );
              newPoints.Add( point ); // die einpunktige Strecke
              newParts.Add( newPoints );
              
              CMoSymbol lineSymbol( profilLayer->GetSymbol() );
              CMoSymbol trackSymbol( trackLayer.GetSymbol( MO2_TRACKSYMBOL_GENERATEPROFILE ) );
              trackSymbol.SetSize( lineSymbol.GetSize() * 1 + 0 ); // erstmal genauso dick
              trackSymbol.SetColor( lineSymbol.GetColor() );
              // solange an den ersten beiden Punkten ausgerichtet wird eine moDotLine, dann eine moDashLine
              trackSymbol.SetStyle( MO2_TRACKFLAG_GERADE ); 
              CMoGeoEvent event(trackLayer.AddEvent( newLine, MO2_TRACKSYMBOL_GENERATEPROFILE ) );
              event.SetTag( MO2_TRACKTAG_GENERATEPROFILE );
              return;
            }; // if anfpoint && endpoint
          }; // if linePoints
        }; // if lineParts
      }; // if line
    }; // if m_lFeatureID != -1
  }; // if profilLayer;

}

void CExtendProfileController::InsertPoint( CMoTrackingLayer& trackLayer, CMoPoint& cursor, CMoGeoEvent& event )
{
  // falls bereits begonnen wurde die Linie zu ziehen, einen neuen Punkt einfügen
  CMoLine line( event.GetShape() );
  if( LPDISPATCH(line) )
  {
    CMoParts lineParts( line.GetParts() );
    if( LPDISPATCH(lineParts) && lineParts.GetCount() > 0 )
    {
      CMoPoints linePoints( lineParts.Item( CComVariant( 0 ) ) );
      if( LPDISPATCH(linePoints) )
      {
        // falls geradlinig Fortgesetzt wird noch schnell auf Linie projizieren
        CMoSymbol symbol( trackLayer.GetSymbol( event.GetSymbolIndex() ) );
        if( LPDISPATCH(symbol) )
          symbol.SetStyle( MO2_TRACKFLAG_KRUMM ); // das bedeutet: ab jetzt nicht mehr geradlinig fortsetzen
    
        CMoPoint insPoint;
        MO2CREATE( insPoint, "Point" );
        insPoint.SetX( cursor.GetX() );
        insPoint.SetY( cursor.GetY() );

        linePoints.Add( insPoint );
        event.SetShape( line );
      }; // if linePoints
    }; // if lineParts
  }; // if line
}

void CExtendProfileController::OnMapMouseMove( CMoPoint& cursor, short Button, short Shift )
{
  m_x = cursor.GetX();
  m_y = cursor.GetY();

  // ein Tracklinine zeichnen, vom letzten Klick aus
  CMoTrackingLayer trackLayer( m_view->GetDocument()->GetMap()->GetTrackingLayer() );
  if( !LPDISPATCH( trackLayer ) )
    return;

  CMoGeoEvent event( trackLayer.FindEvent( MO2_TRACKTAG_GENERATEPROFILE ) );
  if( !LPDISPATCH(event) )
    return;

  CMoLine line( event.GetShape() );
  if( !LPDISPATCH( line ) )
    return;

  CMoParts lineParts( line.GetParts() );
  if( !LPDISPATCH(lineParts) || lineParts.GetCount() == 0 )
    return;

  CMoPoints linePoints( lineParts.Item( CComVariant( 0 ) ) );
      
  if( !LPDISPATCH(linePoints) || linePoints.GetCount() == 0 )
    return;

  CMoSymbol symbol( trackLayer.GetSymbol( event.GetSymbolIndex() ) );
  if( !LPDISPATCH(symbol) )
    return;

  CMoPoint newCursor;
  MO2CREATE( newCursor, "Point" );
  newCursor.SetX( cursor.GetX() );
  newCursor.SetY( cursor.GetY() );
    
  if( symbol.GetStyle() == MO2_TRACKFLAG_GERADE )
  {
    long lineCount = linePoints.GetCount();
    // falls geradlinie Fortgesetzt werden soll, 
    // den jetzt Cursor auf die geradlinige Fortsetzung projizieren
    if( lineCount > 2 )
    {
      // zuerst die beiden letzten Punkte holen: sie bestimmen die geradlinige Fortsetung
      CMoPoint pointA( linePoints.Item( CComVariant( lineCount - 3 ) ) );
      CMoPoint pointE( linePoints.Item( CComVariant( lineCount - 2 ) ) );
      if( LPDISPATCH(pointA) && LPDISPATCH(pointE) )
      {
        // ( vx/vy ) ist der Vektor auf den projiziert wird
        double vx = pointE.GetX() - pointA.GetX();
        double vy = pointE.GetY() - pointA.GetY();
        // v normalisieren
        double vNorm = sqrt( vx * vx + vy * vy );
        vx /= vNorm;
        vy /= vNorm;
        
        // ( wx, wy ) ist der Vektor von PunktE zum cursor
        double wx = cursor.GetX() - pointE.GetX();
        double wy = cursor.GetY() - pointE.GetY();
        
        double scalar = vx * wx + vy * wy;
        
        // falls der Punkt jetzt in der Profillinnie liegen müsste, ihn auf den Endpunkt setzen
        // ansonsten auf die Linie projizieren
        if( scalar < 0 )
        {
          newCursor.SetX( pointE.GetX() );
          newCursor.SetY( pointE.GetY() );
        }
        else
        {
          newCursor.SetX( pointE.GetX() + scalar * vx );
          newCursor.SetY( pointE.GetY() + scalar * vy );
        }; // if scalar < 0
      }; // if pointA && pointE
    }; // if lineCount > 2
  }

  linePoints.Set( linePoints.GetCount() - 1, newCursor );
  event.SetShape( line );
}

void CExtendProfileController::OnMapDblClick()
{
  // ziehen der Linie wird beendet -> gezogene Linie wird ggfls mit Geländemodel verschnitten
  // und dem Profil hinzugefügt
  CMoMap* map = m_view->GetDocument()->GetMap();
  CMoTrackingLayer trackLayer( map->GetTrackingLayer() );
  if( LPDISPATCH(trackLayer) )
  {
    CMoGeoEvent event( trackLayer.FindEvent( MO2_TRACKTAG_GENERATEPROFILE ) );
    if( LPDISPATCH(event) )
    {
      CMoLine line( event.GetShape() );
      if( LPDISPATCH( line ) )
      {
        LPDISPATCH(line)->AddRef();

        // den letzten Punkt der linie löschen ( wurde durch den Doppelklick hínzugefügt )
        CMoParts lineParts( line.GetParts() );
        if( LPDISPATCH(lineParts) && lineParts.GetCount() > 0 )
        {
          CMoPoints linePoints( lineParts.Item( CComVariant( 0 ) ) );
          if( LPDISPATCH(linePoints) && linePoints.GetCount() > 1 )
          {
            linePoints.Remove( linePoints.GetCount() - 1 ); // den letzten Punkt löschen: durch den Doppelklick entstanden
            linePoints.Remove( 0 ); // den ersten Punkt löschen: Hilfspunkt für gerades Fortsetzen
            
            event.SetShape( line );
            event.SetTag( TEXT("") ); // den Tag löschen, damit er nicht mehr von MouseMove etc. erkannt wird
            trackLayer.Refresh( TRUE, CComVariant( map->GetExtent() ) );
            
            // die Linie mit einem DGM verschneiden und dem Profil hinzufügen
            if( m_profilID != -1 )
              m_view->GetDocument()->ExtendProfile( line, m_profilID );
          }; // if linePoints
        }; // if lineParts
      }; // if line
      
      // dann auf jeden Fall den TrackLayer löschen
      trackLayer.RemoveEvent( event.GetIndex() );
    }; // if event
  };// if trackLayer
  
  // nach doppelklick auf jeden Fall die gemerkte Profillinie löschen
  m_profilID = -1;
  m_bHot = FALSE;
}

void CExtendProfileController::Cancel()
{
  CMoTrackingLayer trackLayer( m_view->GetDocument()->GetMap()->GetTrackingLayer() );
  if( !LPDISPATCH(trackLayer) )
    return;

  // falls gerade eine Profilverlängerung gezogen wird und zwar noch geradlinig
  // auf nicht geradlinig umstellen
  CMoGeoEvent extendEvent( trackLayer.FindEvent( MO2_TRACKTAG_GENERATEPROFILE ) );
  if( !LPDISPATCH(extendEvent) )
    return;

  CMoSymbol symbol( trackLayer.GetSymbol( extendEvent.GetSymbolIndex() ) );
  if( symbol.GetStyle() == MO2_TRACKFLAG_GERADE )
  {
    // nicht nur das Symbol ändern, sondern auch die Position auf die aktuelle Stelle setzen
    CMoLine line( extendEvent.GetShape() );
    if( LPDISPATCH( line ) )
    {
      CMoParts lineParts( line.GetParts() );
      if( LPDISPATCH(lineParts) && lineParts.GetCount() > 0 )
      {
        CMoPoints linePoints( lineParts.Item( CComVariant( 0 ) ) );
        if( LPDISPATCH(linePoints) && linePoints.GetCount() > 0 )
        {
          long lineCount = linePoints.GetCount() - 1;
          
          // Flag für die beliebige Fortsetzung setzen
          symbol.SetStyle( MO2_TRACKFLAG_KRUMM );
          
          CMoPoint point( linePoints.Item( CComVariant( lineCount ) ) );

          CMoPoint newPoint;
          MO2CREATE( newPoint, "Point" );
          newPoint.SetX( m_x );
          newPoint.SetY( m_y );
          
          linePoints.Set( lineCount, newPoint );

          extendEvent.SetShape( line );
        }; // if linePoints
      }; // if lineParts
    }; // if line
  } // if symbol.GetStyle
  else // ansonsten dieses event löschen und damit die Aktion beenden
  {
    trackLayer.RemoveEvent( extendEvent.GetIndex() );
    m_profilID = -1;
  }
}  
  
