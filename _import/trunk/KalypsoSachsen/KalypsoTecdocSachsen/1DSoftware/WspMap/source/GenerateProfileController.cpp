// GenerateProfileController.cpp: Implementierung der Klasse CGenerateProfileController.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "mapView.h"
#include "mapDoc.h"
#include "mainfrm.h"
#include "tincutdlg.h"

#include "GenerateProfileController.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

void CGenerateProfileController::Toggle()
{
  IMapController::Toggle();

  // falls keine Flusslinie existiert eine Meldung ausgeben
  if( IsChecked() && m_view->GetDocument()->GetLayers()->FindFirstLayer( CLayer::flussachse ) == 0 )
  {
    // vielleicht gibts ja eine digitalisierte Linie?
    CMoTrackingLayer trackLayer( m_view->GetDocument()->GetMap()->GetTrackingLayer() );
    if( LPDISPATCH(trackLayer) )
    {
      CMoGeoEvent event(trackLayer.FindEvent(MO2_TRACKTAG_RIVER));
      if( !LPDISPATCH(event) )
        AfxMessageBox( IDS_GENPROFILE_DIGITRIVER, MB_ICONINFORMATION );
    }; // if trackLAyer
  }; // if pDoc->GetLayers
}

   
bool CGenerateProfileController::IsEnabled()
{
  // es muss ein Geländemodell existieren
  return m_view->GetDocument()->GetLayers()->FindFirstLayer( CLayer::hmo ) != 0;
}



void CGenerateProfileController::OnMapMouseDown( CMoPoint& cursor, short Button, short Shift )
{
  switch( m_view->GetDocument()->CreateRiverLine() )
  {
  case 0: // Flusslinie da, also weitermachen und Profil digitalisieren
    break; 
    
  case 1: // Linie wurde gerade digitalisiert, also zurück
  case 2: // Linie konnte nicht erzeugt werden, auch abbrchen
    return;
    
  default:
    ASSERT(  FALSE );
    return;
  };
  
  // eine Flusslinie existiert, wir können also jetzt eine ProfilLinie ziehen lassen
  
  // die Linie erzeugen
  CMainFrame* pMainFrame = (CMainFrame*)AfxGetApp()->GetMainWnd();
  pMainFrame->SetStatusBarProgressText( CString(MAKEINTRESOURCE(IDS_TRACK_PROFILE)) );
  CMoLine line( m_view->GetDocument()->GetMap()->TrackLine() );
  pMainFrame->SetStatusBarProgressText( CString(MAKEINTRESOURCE(AFX_IDS_IDLEMESSAGE)) );
  
  // erfragen, welches Geländemodel gewählt werden soll
  CDgmDlg dlg( m_view->GetDocument()->GetLayers(), pMainFrame );
  if( dlg.DoModal() != IDOK )
    return;
  CMapLayer* hmoLayer = dlg.GetLayer();
  if( !hmoLayer )
    return;
  
  if( LPDISPATCH(line) )
  {
    int lastCutAnswer = IDYES;
    try{ m_view->GetDocument()->GenerateProfile( line, hmoLayer, true, &lastCutAnswer, 0, 0, "" ); }
    catch( CMapDoc::cancel_action ) {}; // Abbruch ist uns egal: es passiert sonst eh weiter nix
  }
}

void CGenerateProfileController::Cancel()
{
  // falls eine Flusslinie vorhanden ist, aber keine ProfilLinie, diese löschen
  // aber nur, falls das Profilgenerierentool ausgewählt ist
  CMoTrackingLayer trackLayer = m_view->GetDocument()->GetMap()->GetTrackingLayer();
  CMoGeoEvent riverEvent( trackLayer.FindEvent( MO2_TRACKTAG_RIVER ) );
  CMoGeoEvent generateEvent( trackLayer.FindEvent( MO2_TRACKTAG_GENERATEPROFILE ) );
  if( LPDISPATCH(riverEvent) && !LPDISPATCH(generateEvent) )
  {
    trackLayer.RemoveEvent( riverEvent.GetIndex() );
    CMainFrame* pMainFrame = (CMainFrame*)AfxGetApp()->GetMainWnd();
    pMainFrame->SetStatusBarProgressText( CString(MAKEINTRESOURCE(AFX_IDS_IDLEMESSAGE)) );
  }; // if riverEvent && !generateEvent
}


