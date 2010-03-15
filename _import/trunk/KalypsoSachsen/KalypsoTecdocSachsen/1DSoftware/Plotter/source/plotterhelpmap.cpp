// plotterhelpmap.cpp: Implementierung der Klasse CPlotterHelpMap.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "resource.h"

#include "plotterhelpmap.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

CPlotterHelpMap::IdMap CPlotterHelpMap::m_idMap[count] =
{
  // das Default-Sprungziel:
  IdMap( 0, TEXT( "toc" ) ),

  ////////////
  // Frames //
  ////////////
  IdMap( IDR_MULTIPLOTTYPE, TEXT( "cmd:multiplot" ) ),

  //////////////////
  // Menu - DATEI //
  //////////////////

  IdMap( ID_FILE_NEW, TEXT( "cmd:datei-neu" ) ),
  IdMap( ID_FILE_OPEN, TEXT( "cmd:datei-oeffen" ) ),
  IdMap( ID_FILE_CLOSE, TEXT( "cmd:datei-schliessen" ) ),
  IdMap( ID_FILE_SAVE, TEXT( "cmd:datei-speichern" ) ),
  IdMap( ID_FILE_SAVE_AS, TEXT( "cmd:datei-speichern-unter" ) ),
  IdMap( ID_FILE_OPENPROJEKT, TEXT( "cmd:projekt-oeffnen" ) ),
  IdMap( ID_FILE_DELETE, TEXT( "cmd:datei-löschen" ) ),
  IdMap( ID_FILE_CLOSEPROJEKT, TEXT( "cmd:projekt-schliessen" ) ),
  IdMap( ID_FILE_OVERVIEW, TEXT( "cmd:datei-profiluebersicht" ) ),
  IdMap( ID_FILE_INSERT, TEXT( "cmd:datei-einfügen" ) ),
  IdMap( ID_FILE_DXF_EXPORT, TEXT( "cmd:datei-dxf-export" ) ),
  IdMap( ID_FILE_PAGE_SETUP, TEXT( "cmd:seite-einrichten" ) ),
  IdMap( ID_FILE_PRINT_PREVIEW, TEXT( "cmd:druck-vorschau" ) ),
  IdMap( ID_FILE_PRINT, TEXT( "cmd:drucken" ) ),
  IdMap( ID_FILE_PRINT_ALL, TEXT( "cmd:drucken-alles" ) ),
  IdMap( ID_FILE_PRINT_SETUP, TEXT( "cmd:drucker-einrichten" ) ),
  IdMap( ID_FILE_SEND_MAIL, TEXT( "cmd:senden" ) ),
  IdMap( ID_FILE_SUMMARYINFO, TEXT( "cmd:summary" ) ),
  IdMap( ID_APP_EXIT, TEXT( "cmd:beenden" ) ),

  ///////////////////////
  // Menu - BEARBEITEN //
  ///////////////////////

  IdMap( ID_EDIT_UNDO, TEXT( "cmd:bearbeiten-rueckgaengig" ) ),
  IdMap( ID_EDIT_CUT, TEXT( "cmd:bearbeiten-ausschneiden" ) ),
  IdMap( ID_EDIT_COPY, TEXT( "cmd:bearbeiten-kopieren" ) ),
  IdMap( ID_EDIT_PASTE, TEXT( "cmd:bearbeiten-einfuegen" ) ),
  IdMap( ID_EDIT_PASTE_SPECIAL, TEXT( "cmd:bearbeiten-inhalte-einfuegen" ) ),
  IdMap( ID_EDIT_CLEAR, TEXT( "cmd:bearbeiten-loeschen" ) ),
  IdMap( ID_EDIT_PROPS, TEXT("cmd:bearbeiten-eigenschaften") ),
  IdMap( ID_EDIT_ZOOM, TEXT( "cmd:zoom" ) ),
  IdMap( ID_OLE_INSERT_NEW, TEXT("cmd:objekt-einfuegen") ),
  IdMap( ID_OLE_EDIT_LINKS, TEXT( "cmd:bearbeiten-link" ) ),

  ////////////////////
  // Menu - ANSICHT //
  /////////////////////

  IdMap( ID_VIEW_SHOWOBJECTS, TEXT( "cmd:ansicht-zeigen" ) ),
  IdMap( ID_VIEW_ZOOMIN, TEXT( "cmd:ansicht-zoomin" ) ),
  IdMap( ID_VIEW_ZOOMOUT, TEXT( "cmd:ansicht-zoomout" ) ),
  IdMap( ID_VIEW_TOOLBAR, TEXT( "cmd:ansicht-haupttoolbar" ) ),
  IdMap( ID_VIEW_ZEICHNEN_BAR, TEXT( "cmd:ansicht-zeichnentoolbar" ) ),
  IdMap( ID_VIEW_STATUS_BAR, TEXT( "cmd:ansicht-statusbar" ) ),
  IdMap( ID_VIEW_PLOTBAR, TEXT( "cmd:ansicht-brwosetoolbar" ) ),

  /////////////////////
  // Menu - ZEICHNEN //
  /////////////////////

  IdMap( ID_DRAW_TEXT, TEXT("cmd:zeichnen-text") ),
  IdMap( ID_DRAW_LINE, TEXT("cmd:zeichnen-linie") ),
  IdMap( ID_DRAW_RECT, TEXT("cmd:zeichnen-rechteck") ),
  IdMap( ID_DRAW_ELLIPSE, TEXT("cmd:zeichnen-ellipse") ),
  IdMap( ID_DRAW_POLYLINE, TEXT("cmd:zeichnen-linienzug") ),
  IdMap( ID_DRAW_POLYGON, TEXT("cmd:zeichnen-vieleck") ),
  IdMap( ID_DRAW_MEASURE, TEXT("cmd:zeichnen-messen") ),

  ///////////////////
  // Menu - Objekt //
  ///////////////////

  IdMap( ID_OBJECT_PROPERTIES, TEXT( "cmd:objekt-attribute" ) ),
  IdMap( ID_OBJECT_MOVETOFRONT, TEXT( "cmd:objekt-z-order" ) ),
  IdMap( ID_OBJECT_MOVETOBACK, TEXT( "cmd:objekt-z-order" ) ),
  IdMap( ID_OBJECT_MOVEFORWARD, TEXT( "cmd:objekt-z-order" ) ),
  IdMap( ID_OBJECT_MOVEBACK, TEXT( "cmd:objekt-z-order" ) ),
  IdMap( ID_OBJECT_ISPROFIL, TEXT( "cmd:objekt-fixieren" ) ),

  ///////////////////
  // Menu - Extras //
  ///////////////////

  IdMap( ID_EXTRAS_PREF, TEXT( "cmd:extras-standard" ) ),
  IdMap( ID_EXTRAS_STEMPEL, TEXT( "cmd:extras-stempel" ) ),
  IdMap( ID_EXTRAS_OPTIONS, TEXT( "cmd:extras-optionen" ) ),

  ////////////////////
  // Menu - Fenster //
  ////////////////////

  IdMap( ID_WINDOW_CASCADE, TEXT( "cmd:fenster-cascade" ) ),
  IdMap( ID_WINDOW_TILE_HORZ, TEXT( "cmd:fenster-hoizontal" ) ),
  IdMap( ID_WINDOW_ARRANGE, TEXT( "cmd:fenster-anordnen" ) ),
  IdMap( ID_WINDOW_CLOSEALL, TEXT( "cmd:fenster-alleschliessen" ) ),
  
  ////////////
  // Menu ? //
  ////////////

  IdMap( ID_HELP_FINDER, TEXT( "cmd:hilfe-index" ) ),
  IdMap( ID_APP_ABOUT, TEXT( "cmd:hilfe-about" ) ),

  /////////////////////////
  // Toolbar - Mainframe //
  /////////////////////////

  IdMap( ID_CONTEXT_HELP, TEXT( "cmd:hilfe-contex" ) ),

  ///////////////////////////
  // Toolbar - Plottertype //
  ///////////////////////////

  IdMap( ID_BROWSE_LEFT, TEXT( "cmd:browse-links" ) ),
  IdMap( ID_EDIT_BROWSE, TEXT( "cmd:browse" ) ),
  IdMap( ID_BROWSE_RIGHT, TEXT( "cmd:browse-rechts" ) ),
  IdMap( ID_PROFIL_OV_XY, TEXT("cmd:profiluebersicht-eigenschaften") ),

  ////////////////////////
  // Toolbar - Zeichnen //
  ////////////////////////

  IdMap( IDR_ZEICHNEN, TEXT("toolbar:draw") ),
  IdMap( ID_DRAW_SELECT, TEXT("cmd:zeichnen-select") ),

  /////////////
  // Dialoge //
  /////////////

  IdMap( IDD_ABOUTBOX, TEXT( "dlg:about" ) ),
  IdMap( IDD_BROWSE_PROFILES, TEXT( "dlg:browse-eigenschaften" ) ),
  IdMap( IDD_MEASURE, TEXT( "dlg:messen" ) ),
  IdMap( IDD_OFFNEN, TEXT( "dlg:profil-oeffnen" ) ),
  IdMap( IDD_OPENPROJEKT, TEXT( "dlg:projekt-oeffnen" ) ),
  IdMap( IDD_OPTION_SELTYPE, TEXT( "dlg:???" ) ),
  IdMap( IDD_PREF, TEXT( "dlg:einstellungen" ) ),
  IdMap( IDD_PROFILE_OV_XY, TEXT( "dlg:???" ) ),
  IdMap( IDD_PROP_LINE, TEXT( "dlg:eigenschaften-linie" ) ),
  IdMap( IDD_PROP_SOLID, TEXT( "dlg:eigenschaften-flaeche" ) ),
  IdMap( IDD_PROP_TEXT, TEXT( "dlg:eigenschaften-text" ) ),
  IdMap( IDD_PROP_TYPE, TEXT( "dlg:eigenschaften-typ" ) ),
  IdMap( IDD_SPLASH, TEXT( "dlg:splash" ) ),
  IdMap( IDD_STAT_PAGE, TEXT( "dlg:statistik" ) ),
  IdMap( IDD_SUMM_PAGE, TEXT( "dlg:uebersicht" ) ),
  IdMap( IDD_ZOOM, TEXT( "dlg:zoom" ) ),

  IdMap( IDD_GENERAL, TEXT("dlg:bearbeiten-eigenschaften-allgemein") ),
  IdMap( IDD_MARGINS, TEXT("dlg:bearbeiten-eigenschaften-ausrichtung") ),
  IdMap( IDD_FORMAT, TEXT("dlg:bearbeiten-eigenschaften-format") ),
  IdMap( IDD_DATEN, TEXT("dlg:bearbeiten-eigenschaften-daten") ),
  IdMap( IDD_LINE, TEXT("dlg:bearbeiten-eigenschaften-linien") ),
  IdMap( IDD_SOLID, TEXT("dlg:bearbeiten-eigenschaften-fuellungen") ),
  IdMap( IDD_TEXT, TEXT("dlg:bearbeiten-eigenschaften-text") ),
  
  IdMap( IDD_MULTI_PROPS, TEXT("cmd:multiplot") )
};

/* static */ CPlotterHelpMap::FillHelpMap( CMapUIntToString& idMap )
// Füllt eine CMapUIntToString mit Werten
{
  for( int i = 0; i < 100; i++ )
    idMap.SetAt( m_idMap[i].id, m_idMap[i].target );
} // FillHelpMap