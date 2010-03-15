// lgndbar.cpp: Implementierungsdatei
//
#pragma warning(disable:4786)
#pragma warning(disable:4503)

#include "stdafx.h"

#include "wspprj\wspprj.h"

#include "maphelper.h"
#include "wspmap.h"
#include "mapview.h"
#include "mapdoc.h"
#include "mapproj.h"

#include "lgndbar.h"


extern VARIANT nullVariant;

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif


/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CLegend 


CLegend::CLegend() : CDialog()
{
	//{{AFX_DATA_INIT(CLegend)
		// HINWEIS: Der Klassen-Assistent fügt hier Elementinitialisierung ein
	//}}AFX_DATA_INIT

	m_pDoc = NULL;
  mdownX_OV=0;
  mdownY_OV=0;

  for(int i=0; i < 3; i++)
  {
    CMoLine linie;
    CMoSymbol symbol;
    VERIFY(linie.CreateDispatch(TEXT("MapObjects2.Line")));
    VERIFY(symbol.CreateDispatch(TEXT("MapObjects2.Symbol")));
    m_pProfilLinien.Add(linie);
    m_pProfilSymbole.Add(symbol);
  };
}

void CLegend::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CLegend)
  DDX_Control(pDX, IDC_LEGENDE, m_legend);
  DDX_Control(pDX, IDC_MAP1, m_ovMap);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CLegend, CDialog)
	//{{AFX_MSG_MAP(CLegend)
	ON_WM_SIZE()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CLegend

void CLegend::OnSize(UINT nType, int cx, int cy) 
{
  CDialog::OnSize( nType, cx, cy );

  if (!m_ovMap.m_hWnd || !m_legend.m_hWnd)
    return;

  BOOL showOverview;
  
  if( m_pDoc )
    showOverview = m_pDoc->GetShowOverview();
  else
    showOverview = TRUE;
    
  m_ovMap.ShowWindow( showOverview );
  if( showOverview )
  {
    CRect rect;
    GetClientRect(rect);
    rect.DeflateRect(5, 5);
    rect.SetRect(rect.left,rect.top,rect.right,rect.bottom- MulDiv(rect.Height(),1,3)-8);
    m_legend.MoveWindow(&rect, TRUE);
    
    GetClientRect(rect);
    rect.DeflateRect(5, 5);
    rect.SetRect(rect.left,rect.top+MulDiv(rect.Height(),2,3)+8,rect.right,rect.bottom);
    m_ovMap.MoveWindow(&rect, TRUE);
  }
  else
  {
    CRect rect;
    GetClientRect(rect);
    rect.DeflateRect(5, 5);
    m_legend.MoveWindow(&rect, TRUE);
  }; // if showOverview
};


BEGIN_EVENTSINK_MAP(CLegend, CDialog)
    //{{AFX_EVENTSINK_MAP(CLegend)
  ON_EVENT(CLegend, IDC_LEGENDE, 8 /* AfterReorder */, OnAfterReorderLegende, VTS_NONE)
 	ON_EVENT(CLegend, IDC_LEGENDE, 7 /* BeforeSetLayerVisible */, OnBeforeSetLayerVisibleLegende, VTS_PI2 VTS_PBOOL)
  ON_EVENT(CLegend, IDC_LEGENDE, 6 /* AfterSetLayerVisible */, OnAfterSetLayerVisibleLegend, VTS_PI2 VTS_PBOOL)
	ON_EVENT(CLegend, IDC_LEGENDE, 1 /* LayerDblClick */, OnLayerDblClickLegend, VTS_PI2)
	ON_EVENT(CLegend, IDC_LEGENDE, 2 /* RenderClick */, OnRenderClickLegend, VTS_PI2 VTS_PI2 VTS_PVARIANT VTS_PVARIANT)
	ON_EVENT(CLegend, IDC_LEGENDE, 3 /* MouseDown */, OnMouseDownLegend, VTS_PI2 VTS_PI2 VTS_PI2 VTS_PR4 VTS_PR4)
  ON_EVENT(CLegend, IDC_MAP1, -605 /* MouseDown */, OnMapMouseDownOverView, VTS_I2 VTS_I2 VTS_I4 VTS_I4)
	ON_EVENT(CLegend, IDC_MAP1, -606 /* MouseMove */, OnMapMouseMoveOverView, VTS_I2 VTS_I2 VTS_I4 VTS_I4)
	ON_EVENT(CLegend, IDC_MAP1, -607 /* MouseUp */, OnMapMouseUpOverView, VTS_I2 VTS_I2 VTS_I4 VTS_I4)
	ON_EVENT(CLegend, IDC_MAP1, 4 /* AfterTrackingLayerDraw */, OnAfterTrackingLayerDrawMap1OverView, VTS_I4)	
	//}}AFX_EVENTSINK_MAP
END_EVENTSINK_MAP()

void CLegend::OnBeforeSetLayerVisibleLegende(short FAR* Index, BOOL FAR* cancel) 
{ // wird nie aufgerufen??
	return;
}

void CLegend::OnAfterSetLayerVisibleLegend(short FAR* Index, BOOL FAR* isVisible) 
{
  if( m_pDoc )
  {
    CMoMap* pMap = m_pDoc->GetMap();
    CMoLayers layers( pMap->GetLayers() );
    
    CLayer layer( m_pDoc->GetMapPath(), layers.Item(CComVariant(*Index)) );
    layer.SetVisible( m_legend.GetLayerVisible(Index) );

    m_pDoc->SetModifiedFlag( TRUE );
    pMap->Refresh();
    m_ovMap.Refresh();
  }
}; // OnAfterSetLayerVisibleLegend

void CLegend::OnLayerDblClickLegend(short FAR* Index) 
{
  m_pDoc->EditLayerProperties(m_pDoc->GetActiveLayer());
}; // OnLayerDblClickLegend

void CLegend::OnRenderClickLegend(short FAR* LayerIndex, short FAR* BreakIndex, VARIANT FAR* val1, VARIANT FAR* val2) 
{

}

void CLegend::OnMouseDownLegend(short FAR* Index, short FAR* Button, short FAR* Shift, float FAR* X, float FAR* Y) 
{
  if( m_pDoc )
  {
    CMoLayers layers( m_pDoc->GetMap()->GetLayers() );
    if ( *Index == -1 )
      return;
    LPDISPATCH newDisp = layers.Item( CComVariant(*Index) );
    CLayer* layer = m_pDoc->GetActiveLayer();
    m_pDoc->SetActiveLayer( newDisp );
    newDisp->Release();
    
    // der aktive Layer ist gesetzt, dies auch darstellen
    UpdateActiveLayer();
    
    if ( *Button == 2 )
    {	// right mouse button
      CPoint point;
      
      ::GetCursorPos(&point);
      ReleaseCapture();
      
      CMenu menu;
      VERIFY( menu.LoadMenu( IDR_THEME ) );
      
      CMenu* pPopup = menu.GetSubMenu(0);
      ASSERT(pPopup != NULL);
      
      // unschön, brauche aber den CChildFrame
      // sonst wird das KontextMenu nicht richtig aktualisiert
	  CWnd* parent = GetDocParent();
      pPopup->TrackPopupMenu( TPM_LEFTALIGN | TPM_RIGHTBUTTON, point.x, point.y, parent );
    }; // if rechter Knopf und Menu 'Thema' erlaubt
    
    m_pDoc->FireMapDocChanged( CMapView::ACTIVE_THEME_CHANGED, NULL );
  }
};

void CLegend::OnAfterTrackingLayerDrawMap1OverView(long hDC) 
{
};

void CLegend::OnMapMouseDownOverView(short Button, short Shift, long x, long y) 
{
  if( Button == 1 )
  {
    CMoPoint point( m_ovMap.ToMapPoint( (float)x, (float)y) );
    if( m_pDoc->GetOverviewLayer() )
    {
      CMoTrackingLayer trackLayer(m_ovMap .GetTrackingLayer() );
      if( trackLayer.GetEventCount() > 0 )
      {
        CMoGeoEvent trackLayerEvent( trackLayer.GetEvent( 0 ) );
        CMoRectangle mapRectLayer(trackLayerEvent.GetShape() );
        if( mapRectLayer.IsPointIn( point ) )
        {
          mdownX_OV = point.GetX();
          mdownY_OV = point.GetY();
          return;
        }                     
      }
    }   
    mdownX_OV = 0;
    mdownY_OV = 0;
  }
}

void CLegend::OnMapMouseMoveOverView(short Button, short Shift, long x, long y) 
{
  if(Button==1)
  {
    if(mdownX_OV!=0 && mdownY_OV!=0)
    {
      CMoPoint point(m_ovMap.ToMapPoint((float)x, (float)y));
      CMoTrackingLayer trackLayer(m_ovMap.GetTrackingLayer());
      if(trackLayer.GetEventCount()>0)
      {
        CMoGeoEvent trackLayerEvent(trackLayer.GetEvent(0));
        trackLayerEvent.Move((double)(point.GetX()-mdownX_OV),(double)(point.GetY()-mdownY_OV));
        mdownX_OV=point.GetX();
        mdownY_OV=point.GetY();
      }
    }
  }
}

void CLegend::OnMapMouseUpOverView(short Button, short Shift, long x, long y) 
{
  if(Button==1)
  {
    CMoTrackingLayer trackLayer(m_ovMap.GetTrackingLayer());
    if(trackLayer.GetEventCount()>0)
    {
      CMoGeoEvent trackLayerEvent(trackLayer.GetEvent(0));
      CMoRectangle mapRectLayer(trackLayerEvent.GetShape());
      m_pDoc->SetExtent( mapRectLayer );
      UpdateOverView( FALSE ); 
    }         
  }
  mdownX_OV=0;
  mdownY_OV=0;
}

//
// Implementierung
//

short CLegend::GetActiveLayerIndex()
{
  return m_legend.getActiveLayer();
}

void CLegend::UpdateActiveLayer()
{
  if( !m_pDoc )
    return;

  CLayer* layer = m_pDoc->GetActiveLayer();
  LPDISPATCH disp = NULL;
  if( layer )
    disp = layer->GetDispatch();

  CMoLayers layers( m_pDoc->GetMap()->GetLayers() );
  for( short i = 0; i < layers.GetCount(); i++ )
  {
    LPDISPATCH layerDisp = layers.Item( CComVariant(i) );
    if( disp == layerDisp )
      m_legend.SetActive(&i, TRUE);
    else
      m_legend.SetActive(&i, FALSE);
    layerDisp->Release();
  }; // for i
}; // UpdateActiveLayer()

void CLegend::SetMapDoc( CMapDoc* doc )
{
  if( m_pDoc )
    m_pDoc->RemoveMapDocListener( this );

  m_pDoc = doc;

  if( m_pDoc )
  {
    m_pDoc->AddMapDocListener( this );
    
    if ( GetSafeHwnd() )
    {
      LPDISPATCH disp = GetIDispatchFromCWnd( m_pDoc->GetMap() );
      if( disp )
        m_legend.setMapSource( &disp );
    };
  }
};

void CLegend::UpdateLegend()
{
  if ( m_legend.GetSafeHwnd() )
  {
    BOOL bShow = TRUE;
  
    m_legend.RemoveAll();
    m_legend.LoadLegend(&bShow);

    UpdateOverView( TRUE );
    UpdateActiveLayer();
  };
}

void CLegend::UpdateOverView( BOOL bRedraw )
// nur wenn Layer redraw == TRUE wird echt neu gezeichnet (sonst nur Ausschnittsfenster ändern etc.)
{
  if ( !m_pDoc || !m_ovMap.GetSafeHwnd() )
    return;

  CRect rect;
  GetClientRect( &rect );
  rect.NormalizeRect();
  OnSize( SIZE_RESTORED, rect.Width(), rect.Height() );

  if( m_pDoc->GetOverviewLayer() )
  {
    if( bRedraw )
      DrawOverviewLayer();

    CMoLayers ovLayers( m_ovMap.GetLayers() );
    if( ovLayers.GetCount() > 0 )
    {
      CMoRectangle mapRectLayer( m_pDoc->GetMap()->GetExtent() );
      CMoTrackingLayer trackLayer( m_ovMap.GetTrackingLayer() );
      if( trackLayer.GetEventCount() > 0 )
      {
        CMoGeoEvent trackLayerEvent( trackLayer.GetEvent( 0 ) );
        trackLayerEvent.SetShape( mapRectLayer );
      };
    };
  }
  else
    ClearOverview();
};

void CLegend::DrawOverviewLayer()
{
  CLayer* layer = m_pDoc->GetOverviewLayer();

  CMoTrackingLayer trackLayer(m_ovMap.GetTrackingLayer());
  trackLayer.ClearEvents();
 
	//Dick, Rechteck im Übersichtsfenster einfügen
	CMoRectangle mapRectLayer( m_pDoc->GetMap()->GetExtent() );
  CMoRectangle mapRectovLayer;
		
	VERIFY(mapRectovLayer.CreateDispatch(TEXT("MapObjects2.Rectangle")));
	mapRectovLayer.SetLeft(mapRectLayer.GetLeft());
	mapRectovLayer.SetRight(mapRectLayer.GetRight());
	mapRectovLayer.SetTop(mapRectLayer.GetTop());
	mapRectovLayer.SetBottom(mapRectLayer.GetBottom());
		
	trackLayer.SetSymbolCount(1);
	CMoSymbol  trackSymbol(trackLayer.GetSymbol(0));
		
	trackSymbol.SetSymbolType(moFillSymbol);
	trackSymbol.SetOutlineColor(moRed);
	trackSymbol.SetStyle(moTransparentFill);
	
	trackLayer.AddEvent(mapRectovLayer,0);
	trackLayer.Refresh(TRUE, nullVariant);

  // Übersichtskarte neu zeichnen
  CMoLayers ovLayers( m_ovMap.GetLayers() );

  if( ovLayers.GetCount() > 0 )
  {
    LPDISPATCH ovDisp = ovLayers.Item( COleVariant((short)0) );
    if( ovDisp == layer->GetDispatch() )
      m_ovMap.Refresh();
    else
    {
      ovLayers.Clear();
      ovLayers.Add(layer->GetDispatch());
    };
    ovDisp->Release();
  }
  else
    ovLayers.Add(layer->GetDispatch());
};

void CLegend::ClearOverview()
{
    CMoLayers ovLayers(m_ovMap.GetLayers());

    ovLayers.Clear();
    CMoTrackingLayer trackLayer(m_ovMap.GetTrackingLayer());
    trackLayer.ClearEvents();
    m_ovMap.Refresh();
};

BOOL CLegend::PreTranslateMessage(MSG* pMsg) 
{
  if (AfxGetMainWnd()->PreTranslateMessage(pMsg))
    return TRUE;
  return CDialog::PreTranslateMessage(pMsg);
}

void CLegend::OnAfterReorderLegende() 
{
	// TODO: Code für die Behandlungsroutine der Steuerelement-Benachrichtigung hier einfügen
	
}

/* virtual */
void CLegend::MapDocChanged( const long type, CLayer* layer )
{
  if( type & ( THEME_ADDED | THEME_REMOVED | THEME_PROPS_CHANGED | THEME_DATA_CHANGED | THEME_ORDER_CHANGED ) )
    UpdateLegend();
  else if( type & ( THEME_GEOMETRY_CHANGED | OVERVIEW_CHANGED ) )
    UpdateOverView( TRUE );
  else if( type & EXTENT_CHANGED )
    UpdateOverView( FALSE );
}


/** 
 * Gibt den Parent für das Kontextmenu zurück.
 * Muss der Parent der CMapView sein, da sonst die Menüeinträge nicht
 * korrekt aktiviert werden.
 */
CWnd* CLegend::GetDocParent()
{
	if( !m_pDoc )
		return 0;
	
	POSITION viewPos = m_pDoc->GetFirstViewPosition();
	while( viewPos )
	{
		CView* m_view = m_pDoc->GetNextView( viewPos );
		return m_view->GetParent();
	}
	
	return 0;
}