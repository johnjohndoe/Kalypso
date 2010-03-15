// mappreviewbar.cpp: Implementierung der Klasse CMapPreviewBar.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "resource.h"

#include "..\..\commonMfc\commonMfc.h"

#include "mappreviewbar.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

BOOL CMapPreviewBar::InitDialogBar()
// Aktiviert die ToolTips und die Inhalte der Knöpfe
// sollte ( und muss ) nach Create aufgerufen werden
// Rückgabewert:
//        TRUE, wenn alles ok ist
{
  // die ToolTipConntrol erzeugen
  if( !m_toolTip.Create( this ) )
  {
    TRACE0("Error: Unable to Create ToolTip Control\n");
    return FALSE;
  };
  
  // Die Toolbar initialisieren
  CImageList* pImageList = CCommonImageList::GetList( TRUE ); // für die Buttons brauchen wir die ImageList
  
  // die Buttons der Reihe nach durchgehen und initialisieren
  CButton* button = (CButton*)GetDlgItem( AFX_ID_PREVIEW_PRINT );
  button->SetIcon( pImageList->ExtractIcon( ICON_PRINT ) );
  m_toolTip.AddTool( button, IDS_PREVIEW_PRINT );

  button = (CButton*)GetDlgItem( ID_PREVIEW_PRINTER_SETUP );
  button->SetIcon( pImageList->ExtractIcon( ICON_PRINT_PROPS ) );
  m_toolTip.AddTool( button, IDS_PREVIEW_PRINTER_SETUP );

  button = (CButton*)GetDlgItem( AFX_ID_PREVIEW_ZOOMIN );
  button->SetIcon( pImageList->ExtractIcon( ICON_ZOOMIN ) );
  m_toolTip.AddTool( button, IDS_PREVIEW_ZOOMIN );

  button = (CButton*)GetDlgItem( AFX_ID_PREVIEW_ZOOMOUT );
  button->SetIcon( pImageList->ExtractIcon( ICON_ZOOMOUT ) );
  m_toolTip.AddTool( button, IDS_PREVIEW_ZOOMOUT );

  button = (CButton*)GetDlgItem( ID_PREVIEW_DELETE );
  button->SetIcon( pImageList->ExtractIcon( ICON_DELETE ) );
  m_toolTip.AddTool( button, IDS_PREVIEW_DELETE );

  button = (CButton*)GetDlgItem( ID_PREVIEW_PROPS );
  button->SetIcon( pImageList->ExtractIcon( ICON_OBJECT_PROPS ) );
  m_toolTip.AddTool( button, IDS_PREVIEW_PROPS );

  button = (CButton*)GetDlgItem( ID_PREVIEW_ZFRONT );
  button->SetIcon( pImageList->ExtractIcon( ICON_ZORDER_FRONT ) );
  m_toolTip.AddTool( button, IDS_PREVIEW_ZFRONT );

  button = (CButton*)GetDlgItem( ID_PREVIEW_ZBACK );
  button->SetIcon( pImageList->ExtractIcon( ICON_ZORDER_BACK ) );
  m_toolTip.AddTool( button, IDS_PREVIEW_ZBACK );

  button = (CButton*)GetDlgItem( AFX_ID_PREVIEW_CLOSE );
  button->SetIcon( pImageList->ExtractIcon( ICON_CLOSE_DOC ) );
  m_toolTip.AddTool( button, IDS_PREVIEW_CLOSE );
  
  // die Radio-Buttons ( = Tools )
  button = (CButton*)GetDlgItem( ID_PREVIEW_SELECT );
  button->SetIcon( pImageList->ExtractIcon( ICON_SELECT ) );
  button->SetCheck( TRUE ); // dieser Knopf ist vorselektiert
  m_toolTip.AddTool( button, IDS_PREVIEW_SELECT );
  
  button = (CButton*)GetDlgItem( ID_PREVIEW_NEW_TEXT );
  button->SetIcon( pImageList->ExtractIcon( ICON_TEXT2 ) );
  m_toolTip.AddTool( button, IDS_PREVIEW_NEW_TEXT );

  button = (CButton*)GetDlgItem( ID_PREVIEW_NEW_LEGEND2 );
  button->SetIcon( pImageList->ExtractIcon( ICON_LEGEND ) );
  m_toolTip.AddTool( button, IDS_PREVIEW_NEW_LEGEND );

  button = (CButton*)GetDlgItem( ID_PREVIEW_NEW_MAP );
  button->SetIcon( pImageList->ExtractIcon( ICON_KARTE ) );
  m_toolTip.AddTool( button, IDS_PREVIEW_NEW_MAP );

  button = (CButton*)GetDlgItem( ID_PREVIEW_NEW_LOGO );
  button->SetIcon( pImageList->ExtractIcon( ICON_LOGO ) );
  m_toolTip.AddTool( button, IDS_PREVIEW_NEW_LOGO );

  // und die Tooltips auch aktivieren
  m_toolTip.Activate( TRUE );

  return TRUE;
} // InitDialogBar

BOOL CMapPreviewBar::PreTranslateMessage( MSG* pMsg )
// überschrieben, um die ToolTips zu aktivieren
{
  m_toolTip.RelayEvent( pMsg );
  return CDialogBar::PreTranslateMessage( pMsg );
}; // PreTranslateMessage

void CMapPreviewBar::ClickButton( const int id )
// drückt den durch die ID gegebenen Button einmal
{
  CButton* button = (CButton*)GetDlgItem( id );

  if( button != NULL )
  {
    button->SendMessage( WM_LBUTTONDOWN, 0, LPARAM( 0 ) );
    button->SendMessage( WM_LBUTTONUP, 0, LPARAM( 0 ) );
  }
} // ClickButton