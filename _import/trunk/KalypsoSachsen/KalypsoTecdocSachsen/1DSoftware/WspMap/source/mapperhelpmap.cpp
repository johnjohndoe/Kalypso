// MapperHelpMap.cpp: Implementierung der Klasse CMapperHelpMap.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "resource.h"

#include "mapperhelpmap.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

CMapperHelpMap::IdMap CMapperHelpMap::m_idMap[count] =
{
  // das Default-Sprungziel:
  IdMap( 0, TEXT( "toc" ) ),

  //////////////////
  // Menu - DATEI //
  //////////////////
  IdMap( ID_FILE_MANAGER, TEXT( "cmd:datei-projektmanager" ) ),	
  IdMap( ID_FILE_PRINT, TEXT( "cmd:datei-drucken" ) ),
  IdMap( ID_FILE_PRINT_PREVIEW, TEXT( "cmd:datei-seitenansicht" ) ),
  IdMap( ID_FILE_PRINT_SETUP, TEXT( "cmd:datei-druckereinrichtung" ) ),
  IdMap( ID_APP_EXIT, TEXT( "cmd:beenden" ) ),

  ////////////////////
  // Menu - ANSICHT //
  ////////////////////
	IdMap( ID_VIEW_STATUS_BAR, TEXT( "cmd:ansicht-statusleiste" ) ),
	IdMap( ID_VIEW_OVERVIEW_MAP, TEXT( "cmd:ansicht-uebersichtskarte" ) ),
	IdMap( ID_VIEW_PROFILEBAR, TEXT( "cmd:ansicht-profileditor" ) ),
	IdMap( ID_VIEW_PROFILAUSWAHL, TEXT( "cmd:ansicht-profiluebersicht" ) ),
	IdMap( ID_VIEW_LEGEND, TEXT( "cmd:ansicht-legende" ) ),
	IdMap( ID_VIEW_OVERVIEW, TEXT( "cmd:ansicht-uebersichtsfenster" ) ),
	IdMap( ID_VIEW_SCALEBAR, TEXT( "cmd:ansicht-massstabsanzeige" ) ),
	
  
  //////////////////
  // Menu - KARTE //
  //////////////////
	IdMap( ID_MAP_NEW, TEXT( "cmd:karte-neu" ) ),
	IdMap( ID_MAP_OPEN, TEXT( "cmd:karte-oeffnen" ) ),
	IdMap( ID_MAP_DELETE, TEXT( "cmd:karte-loeschen" ) ),
	IdMap( ID_MAP_SAVE_AS, TEXT( "cmd:karte-exportierenals" ) ),
	IdMap( ID_MAP_CLIPBOARD, TEXT( "cmd:karte-inzwischenablage" ) ),
	IdMap( ID_SAFE_DEFAULT_PROPERTIES, TEXT( "cmd:karte-vorlagespeichern" ) ),

  

  //////////////////
  // Menu - THEMEN //
  //////////////////
	IdMap( ID_LAYER_NEW, TEXT( "cmd:themen-neu" ) ),
	IdMap( ID_LAYER_INSERT, TEXT( "cmd:themen-hinzufuegen" ) ),

	IdMap( ID_LAYER_MOVETOTOP, TEXT( "cmd:themen-verschieben-nachoben" ) ),
	IdMap( ID_LAYER_MOVEFORWARD, TEXT( "cmd:themen-verschieben-einshoch" ) ),
	IdMap( ID_LAYER_MOVEBACK, TEXT( "cmd:themen-verschieben-einsrunter" ) ),
	IdMap( ID_LAYER_MOVETOBACK, TEXT( "cmd:themen-verschieben-nachunten" ) ),

	IdMap( ID_LAYER_COPY, TEXT( "cmd:themen-kopieren") ),
	IdMap( ID_LAYER_REMOVE, TEXT( "cmd:themen-entfernen") ),
	
	IdMap( ID_LAYER_SHAPE_TO_PROFILE, TEXT( "cmd:themen-mitprofilenschneiden" ) ),
	IdMap( ID_LAYER_HMO_IMPORT, TEXT( "cmd:themen-Hoehenmodellimportieren" ) ),
	IdMap( ID_LAYER_TINCUT, TEXT( "cmd:themen-dgmmitwasserspiegel" ) ),
	IdMap( ID_LAYER_MOVE_OUTWARDS, TEXT( "cmd:themen-wasserspiegelnachaussen" ) ),
	IdMap( ID_LAYER_ZOOM, TEXT( "cmd:themen-zoomzumthema" ) ),

	IdMap( ID_LAYER_OVERVIEW, TEXT( "cmd:themen-uebersichtsfenster" ) ),
	IdMap( ID_LAYER_PROPERTIES, TEXT( "cmd:themen-eigenschaften" ) ),

  

  ////////////////////////
  // Menu - GEO-Objekte //
  ////////////////////////
	IdMap( ID_OBJECT_NEW, TEXT( "cmd:objekte-erzeugen" ) ),
	IdMap( ID_OBJECT_QUERY, TEXT( "cmd:objekte-abfragen" ) ),
	IdMap( ID_OBJECT_MOVE, TEXT( "cmd:objekte-verschieben" ) ),
	IdMap( ID_OBJECT_DELETE, TEXT( "cmd:objekte-loeschen" ) ),
	IdMap( ID_OBJECT_CLIP, TEXT( "cmd:objekte-ausschneiden" ) ),
	IdMap( ID_POINT_INSERT, TEXT( "cmd:objekte-punkt-einfuegen" ) ),
	IdMap( ID_POINT_DELETE, TEXT( "cmd:objekte-punkt-loeschen" ) ),
	IdMap( ID_POINT_MOVE, TEXT( "cmd:objekte-punkt-verschieben" ) ),


  /////////////////////
  // Menu - TOOLS	 //
  /////////////////////
	IdMap( ID_TOOLS_FULL_EXTENT, TEXT( "cmd:tools-maximaledarstellung" ) ),
	IdMap( ID_TOOLS_ZOOMIN, TEXT( "cmd:tools-ausschnittvergoessern" ) ),
	IdMap( ID_TOOLS_ZOOMOUT, TEXT( "cmd:tools-ausschnitverkleinern" ) ),
	IdMap( ID_TOOLS_PAN, TEXT( "cmd:tools-ausschnittverschieben" ) ),
	IdMap( ID_TOOLS_EDIT_PROFIL, TEXT( "cmd:tools-profileditieren" ) ),
	IdMap( ID_TOOLS_FOLLOW_PROFILE, TEXT( "cmd:tools-profileditorautomatik" ) ),
	IdMap( ID_TOOLS_MARK_PROFILE, TEXT( "cmd:tools-aktivesprofilmarkieren" ) ),
	IdMap( ID_TOOLS_GENERATE_PROFILE, TEXT( "cmd:tools-profilgenerieren" ) ),
  IdMap( ID_LAYER_GENERATE_PROFILES, TEXT( "cmd:tools-profilgenerieren" ) ),
	IdMap( ID_TOOLS_EXTEND_PROFILE, TEXT( "cmd:tools-profilverlaengern" ) ),
 

  ///////////////////
  // Menu - EXTRAS //
  ///////////////////
  IdMap( ID_EXTRAS_EDIT_NUTZUNG, TEXT( "cmd:extras-nutzungsklassen" ) ),

  /////////////
  // DIALOGE //
  /////////////
  IdMap( IDD_GENERATE_PROFILE, TEXT( "cmd:tools-profilgenerieren" ) ),
};



/* static */ CMapperHelpMap::FillHelpMap( CMapUIntToString& idMap )
// Füllt eine CMapUIntToString mit Werten
{
  for( int i = 0; i < 100; i++ )
    idMap.SetAt( m_idMap[i].id, m_idMap[i].target );
} // FillHelpMap