// mappreview.cpp: Implementierung der Klasse CMapPreview.
//
//////////////////////////////////////////////////////////////////////
#pragma warning(disable:4786)
#pragma warning(disable:4503)

#include "stdafx.h"

#include "Bce\include\WSPFeatures.h"

#include "mapDoc.h"
#include "mapLayout.h"
#include "mapPreviewBar.h"

#include "printrect.h"

#include "mappreview.h"


#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

IMPLEMENT_DYNCREATE( CMapPreview, CPreviewView )

CMapPreview::CMapPreview() : CPreviewView()
{
  m_mapLayout = NULL;
  m_curTool = ID_PREVIEW_SELECT;
  m_state = normal;
  
  m_selectedRect = NULL;
  
  // allocate preview info: wird von ~CPreviewView wieder zersört
  m_pPreviewInfo = new CPrintInfo;
  m_cursorID = IDC_ARROW;
}

CMapPreview::~CMapPreview()
{
  // meldet sich beim MapLayout als Listener ab
  m_mapLayout->RemoveListener( this );
}


BEGIN_MESSAGE_MAP(CMapPreview, CPreviewView)
ON_WM_SIZE()        // overriding CScrollView
ON_WM_CREATE()
ON_WM_VSCROLL()
ON_WM_HSCROLL()
ON_WM_LBUTTONDOWN()
ON_WM_LBUTTONUP()
ON_WM_LBUTTONDBLCLK()
ON_WM_MOUSEMOVE()
ON_WM_SETCURSOR()

ON_COMMAND_EX(AFX_ID_PREVIEW_ZOOMIN, OnCommand)
ON_COMMAND_EX(AFX_ID_PREVIEW_ZOOMOUT, OnCommand)
ON_COMMAND_EX(ID_PREVIEW_PRINTER_SETUP, OnCommand)
ON_COMMAND_EX(ID_PREVIEW_SELECT, OnCommand)
ON_COMMAND_EX(ID_PREVIEW_NEW_TEXT, OnCommand)
ON_COMMAND_EX(ID_PREVIEW_NEW_MAP, OnCommand)
ON_COMMAND_EX(ID_PREVIEW_NEW_LEGEND2, OnCommand)
ON_COMMAND_EX(ID_PREVIEW_NEW_LOGO, OnCommand)
ON_COMMAND_EX(ID_PREVIEW_DELETE, OnCommand)
ON_COMMAND_EX(ID_PREVIEW_ZFRONT, OnCommand)
ON_COMMAND_EX(ID_PREVIEW_ZBACK, OnCommand)
ON_COMMAND_EX(ID_PREVIEW_PROPS, OnCommand)

ON_UPDATE_COMMAND_UI(AFX_ID_PREVIEW_PRINT, OnUpdateCommand)
ON_UPDATE_COMMAND_UI(AFX_ID_PREVIEW_ZOOMIN, OnUpdateCommand)
ON_UPDATE_COMMAND_UI(AFX_ID_PREVIEW_ZOOMOUT, OnUpdateCommand)
ON_UPDATE_COMMAND_UI(ID_PREVIEW_NEW_TEXT, OnUpdateCommand)
ON_UPDATE_COMMAND_UI(ID_PREVIEW_NEW_MAP, OnUpdateCommand)
ON_UPDATE_COMMAND_UI(ID_PREVIEW_NEW_LEGEND2, OnUpdateCommand)
ON_UPDATE_COMMAND_UI(ID_PREVIEW_NEW_LOGO, OnUpdateCommand)
ON_UPDATE_COMMAND_UI(ID_PREVIEW_DELETE, OnUpdateCommand)
ON_UPDATE_COMMAND_UI(ID_PREVIEW_ZFRONT, OnUpdateCommand)
ON_UPDATE_COMMAND_UI(ID_PREVIEW_ZBACK, OnUpdateCommand)
ON_UPDATE_COMMAND_UI(ID_PREVIEW_PROPS, OnUpdateCommand)

END_MESSAGE_MAP()

BOOL CMapPreview::Create( CWnd* pToolbarParent, LPCTSTR lpszClassName, LPCTSTR lpszWindowName, DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID, CCreateContext* pContext )
// überschrieben, um hier die Toolbar zu erzeugen;
// wurde vorher in CView::DoPrintPreview gemacht, hier ist aber schöner
// wegen Kapselung
{
  // Create the toolbar from the dialog resource
  CMapPreviewBar* pMapPreviewBar = new CMapPreviewBar;
  
  if (!pMapPreviewBar->Create( pToolbarParent, MAKEINTRESOURCE(IDD_PREVIEW_BAR),
    CBRS_TOP, AFX_IDW_PREVIEW_BAR))
  {
    TRACE0("Error: Preview could not create toolbar dialog.\n");
    delete pMapPreviewBar;       // not autodestruct yet
    
    return FALSE;
  }
  
  pMapPreviewBar->m_bAutoDelete = TRUE;    // automatic cleanup
  pMapPreviewBar->InitDialogBar(); // und initialisieren
  m_pToolBar = pMapPreviewBar; // jetzt richtig setzen
  
  // anschliessend einfach Create der Basisklasse aufrufen
  return CPreviewView::Create( lpszClassName, lpszWindowName, dwStyle, rect, pParentWnd, nID, pContext );
} // Create


//////////////////////
// Überschreibungen //
//////////////////////

void CMapPreview::OnSize(UINT nType, int cx, int cy)
// Überschreibt CPreviewView::OnSize mit dem Standardverhalten
{
  CScrollView::OnSize( nType, cx, cy );
}


void CMapPreview::OnPrepareDC(CDC* pDC, CPrintInfo* pInfo)
{
  CRect clientRect;
  GetClientRect( clientRect );
  m_mapLayout->OnPrepareDC( pDC, pInfo, clientRect.Size() );
}

void CMapPreview::OnDraw(CDC* pDC )
{
  // wegen Scrollens muss auch noch ein Offset angegeben werden
  
  // GetScrollPosition benutzt nicht den richtigen DC zur Umrechnung
  CPoint ptOffset = GetDeviceScrollPosition();
  pDC->DPtoLP( &ptOffset );
  
  pDC->OffsetWindowOrg( ptOffset.x, ptOffset.y );
  m_mapLayout->Paint( pDC );
} // OnDraw

void CMapPreview::OnHScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar)
// Überschreibt CPreviewView::OnHScroll mit dem Standardverhalten
{
  CScrollView::OnHScroll(nSBCode, nPos, pScrollBar);
}

void CMapPreview::OnVScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar)
// Überschreibt CPreviewView::OnVScroll mit dem Standardverhalten
{
  CScrollView::OnVScroll( nSBCode, nPos, pScrollBar );
}

void CMapPreview::OnLButtonUp( UINT nFlags, CPoint point )
{
  CClientDC* pDC = GetDC();
  pDC->DPtoLP( &point );
  
  switch(m_curTool)
  {
  case ID_PREVIEW_NEW_TEXT:
  case ID_PREVIEW_NEW_MAP:
  case ID_PREVIEW_NEW_LEGEND2:
  case ID_PREVIEW_NEW_LOGO:
    switch( m_state )
    {
    case selected:
      m_state = normal;
      
      // zuletzt gezeichnetes Rechteck löschen
      CRect delRect( m_trackPoint, m_lastPoint );
      delRect.NormalizeRect();
      pDC->DrawFocusRect( delRect );
      
      // ein neues Druckrechteck erzeugen und initialisieren
      CRect printRect( m_trackPoint, point );
      printRect.NormalizeRect();
      
      // ein neues Druckrechteck erzeugen
      // Die ID des Tool-Buttons wird als Typ des Druckrechtecks benutzt
      m_mapLayout->CreatePrintRect( printRect, m_curTool, pDC );
      
      // nach dem new soll gleich das select tool selectiert sein
      ((CMapPreviewBar*)m_pToolBar)->ClickButton( ID_PREVIEW_SELECT );
      break;
    }
    break;
    
    case ID_PREVIEW_SELECT:
      if( m_selectedRect != NULL )
      {
        pDC->DrawFocusRect( m_trackRect );
        m_selectedRect->SetBounds( m_trackRect, pDC );
      }
      m_state = normal;
      break;
  }
  
  ReleaseCapture();
  
  delete pDC;
} // OnLButtonUp

void CMapPreview::OnLButtonDown( UINT nFlags, CPoint point)
// Rechnet die Position in Blattkoordinaten um und schickts dann weiter
// an das Kartenlayout
{
  ASSERT(m_mapLayout);
  
  CClientDC* pDC = GetDC();
  pDC->DPtoLP( &point );
  
  if( !m_mapLayout->IsPointInPage( point ) )
  {
    delete pDC;
    return;
  }
  
  SetCapture();
  
  switch( m_curTool )
  {
  case ID_PREVIEW_NEW_TEXT:
  case ID_PREVIEW_NEW_LEGEND2:
  case ID_PREVIEW_NEW_MAP:
  case ID_PREVIEW_NEW_LOGO:
    {
      m_trackPoint = point;
      m_state = selected;
      
      // neues Tracking Rectangle zeichnen, damit es bei MousMove gleich wieder gelöscht werden kann
      CRect trackRect( m_trackPoint, point );
      trackRect.NormalizeRect();
      pDC->DrawFocusRect( trackRect );
      
      m_lastPoint = point;
    }
    break;
    
  case ID_PREVIEW_SELECT:
    // auf jeden Fall das zur Zeit selektierte Rechteck neu zeichnen lassen ( ziemlich wahrscheinlich, 
    // dass es jetzt nicht mehr selektiert ist )
    m_trackPoint = point;
    m_lastPoint = m_trackPoint;

    //Routine zur Überprüfung der Handles im Rechteck, diese führt dann 
    // resizen des Rechtecks
    if( m_selectedRect != NULL )
    { 
      for( int i = 1; i <= 8; i += 1 ) 
      {
        CRect handleRect =  m_selectedRect->GetHandleRect( i, pDC );
        if(handleRect.PtInRect(point))
        { 
          switch(i)
          {
          case 1 : m_state = NW;
            break;
          case 2 : m_state = N;
            break;
          case 3 : m_state = NE;
            break;
          case 4 : m_state = E;
            break;
          case 5 : m_state = SE;
            break;
          case 6 : m_state = S;
            break;
          case 7 : m_state = SW;
            break;
          case 8 : m_state = W;
            break;
          };
          delete pDC;

          m_trackRect = m_selectedRect->GetBounds();
          // neues Tracking Rectangle zeichnen, damit es bei MousMove gleich wieder gelöscht werden kann
          pDC->DrawFocusRect( m_trackRect );

          return;  // aus der Methode
        }
      }
    }
    
    CPrintRect* c = m_mapLayout->IsPointInRect( point );

    if( c != m_selectedRect )
    {
      if( m_selectedRect != NULL )
      {
        m_selectedRect->SetState( CPrintRect::normal );
        InvalidateLogRect( m_selectedRect->GetBounds() );
      }

      if( c != NULL )
        InvalidateLogRect( c->GetBounds());

      m_selectedRect = c;
    }

    if( c != NULL )
    {
      c->SetState( CPrintRect::selected );
      
      m_trackRect = c->GetBounds();
      // neues Tracking Rectangle zeichnen, damit es bei MousMove gleich wieder gelöscht werden kann
      pDC->DrawFocusRect( m_trackRect );

      m_state = selected;
    }
    else
      m_state = normal;

    break;
  } // switch m_curTool
  
  delete pDC;
  
} // OnLButtonDown

void CMapPreview::OnLButtonDblClk( UINT nFlags, CPoint point )
// wenn der Benutzer ein Druckrechteck doppelcklickt, soll der
// Eigenschaftsdialog aufgerufen werden
{
  ASSERT(m_mapLayout);
  
  // zuerst Punkt in logische Koordinaten umrechnen
  CClientDC* pDC = GetDC();
  pDC->DPtoLP( &point );
  delete pDC;
  pDC = NULL;
  
  // und dann das Druckrechteck holen, in welches geklickt wurde
  CPrintRect* printRect = m_mapLayout->IsPointInRect( point );
  if( printRect != NULL )
    printRect->DoPropertyDialog( this );
} // OnLButtonDblClk

void CMapPreview::OnRButtonDown( UINT nFlags, CPoint point )
// Rechnet die Position in Blattkoordinaten um und schickts dann weiter
// an das Kartenlayout
{
  ASSERT(m_mapLayout);
  
  ToLogCrds( point );
}

BOOL CMapPreview::OnSetCursor(CWnd* pWnd, UINT nHitTest, UINT message)
// ändert den Cursor je nach Position und selektiertem Tool
{
  ASSERT(m_mapLayout);
  
  // wenns nicht im Clientbereich ist solls die ScrollView machen
  if (nHitTest != HTCLIENT)
    return CScrollView::OnSetCursor(pWnd, nHitTest, message);
  
  // den Punkt ausrechnen
  CPoint point;
  ::GetCursorPos(&point);
  ScreenToClient(&point);     // client coordinates of mouse position
  CDC* pDC = GetDC();
  pDC->DPtoLP( &point );
  
  if(m_selectedRect!=NULL){
    //erstens gucke, ob in einem der Handles
    
    BOOL bHandle = FALSE;
    for (int i=1;i<=8;i++)
    {
      CRect handle = m_selectedRect->GetHandleRect( i, pDC );
      if(handle.PtInRect(point))
      {
        switch(i)
        {
        case 1 : if(m_cursorID != IDC_SIZENWSE)
                 {
                   m_cursorID = IDC_SIZENWSE;
                   ::SetCursor(::LoadCursor(NULL,m_cursorID));
                 }
          break;
        case 2 : if(m_cursorID !=IDC_SIZENS)
                 {
                   m_cursorID = IDC_SIZENS;
                   ::SetCursor(::LoadCursor(NULL,m_cursorID));
                   
                 }
          break;
        case 3 : if(m_cursorID !=IDC_SIZENESW)
                 {
                   m_cursorID = IDC_SIZENESW;
                   ::SetCursor(::LoadCursor(NULL,m_cursorID));
                 }
          break;
        case 4 : if(m_cursorID != IDC_SIZEWE)
                 {
                   m_cursorID = IDC_SIZEWE;
                   ::SetCursor(::LoadCursor(NULL,m_cursorID));
                 }
          break;
        case 5 : if(m_cursorID != IDC_SIZENWSE)
                 {
                   m_cursorID = IDC_SIZENWSE;
                   ::SetCursor(::LoadCursor(NULL,m_cursorID));
                 }
          break;
        case 6 : if(m_cursorID != IDC_SIZENS)
                 {
                   m_cursorID = IDC_SIZENS;
                   ::SetCursor(::LoadCursor(NULL,m_cursorID));
                 }
          break;
        case 7 : if(m_cursorID !=IDC_SIZENESW)
                 {
                   m_cursorID = IDC_SIZENESW;
                   ::SetCursor(::LoadCursor(NULL,m_cursorID));
                 }
          break;
        case 8 : if(m_cursorID != IDC_SIZEWE)
                 {
                   m_cursorID = IDC_SIZEWE;
                   ::SetCursor(::LoadCursor(NULL,m_cursorID));
                 }
          break;
        } //switch end
        bHandle = TRUE;
        break; //aus der Schleife
      } // if handle.PtInRect
      
    } // for i
    
    if(!bHandle && m_state==normal){
      CRect bounds = m_selectedRect->GetBounds();
      if(bounds.PtInRect(point)){
        if(m_cursorID!= IDC_SIZEALL) {
          m_cursorID = IDC_SIZEALL;
          ::SetCursor(::LoadCursor(NULL,m_cursorID));
        }
      }
      else{
        if(m_cursorID!=IDC_ARROW){
          m_cursorID = IDC_ARROW;
          ::SetCursor(::LoadCursor(NULL,m_cursorID));
        }
      }
      
      
    }
    
  }
  
  delete pDC;
  
  return 0;
}

void CMapPreview::OnMouseMove( UINT nFlags, CPoint point )
{
  ASSERT(m_mapLayout);
  
  CClientDC* pDC = GetDC();
  // den Punkt umrechnen
  pDC->DPtoLP( &point );
  
  
  switch( m_curTool )
  {
  case ID_PREVIEW_NEW_TEXT:
  case ID_PREVIEW_NEW_MAP:
  case ID_PREVIEW_NEW_LEGEND2:
  case ID_PREVIEW_NEW_LOGO:
    switch( m_state )
    {
    case selected:
      {
        // zuletzt gezeichnetes Rechteck löschen
        CRect delRect( m_trackPoint, m_lastPoint );
        delRect.NormalizeRect();
        pDC->DrawFocusRect( delRect );
        
        // neues Tracking Rectangle zeichnen
        CRect trackRect( m_trackPoint, point );
        trackRect.NormalizeRect();
        pDC->DrawFocusRect( trackRect );
        m_lastPoint = point;
      }
      break;
      
    case normal:
      break;
      
    }
    break;
    
    case ID_PREVIEW_SELECT : 
      
      //verhindere das Ändern der Größe außerhalb der Seite
      if(point.x<0)
        point.x = 0;
      if(point.y<0)
        point.y = 0;

      // das alte TrackRect löschen
      if( m_state != normal )
        pDC->DrawFocusRect( m_trackRect );

      switch(m_state)
      {
      case selected:  
        if( m_selectedRect != NULL ) 
        {
          m_trackPoint = point;
          //das alte Löschen
          
          CPoint offsetPoint = m_trackPoint - m_lastPoint;
          m_trackRect.OffsetRect(offsetPoint);
          
          // verhindern, dass der Benutzer nach links/oben ausserhalb des Bildschirms zieht
          offsetPoint = CPoint( 0, 0 );
          if( m_trackRect.left < 0 )
            offsetPoint.x = -m_trackRect.left;
          if( m_trackRect.top < 0 )
            offsetPoint.y = -m_trackRect.top;
          m_trackRect.OffsetRect( offsetPoint );

          //das neue zeichnen
          
          m_lastPoint = m_trackPoint;
        }
        break;
      case SE:
        {
          //Resizen nach Südosten
          CPoint& brPoint = m_trackRect.BottomRight();
          brPoint.x = point.x;
          brPoint.y = point.y;
        }
        break; 
      case NE:
        //Resizen nach Nordosten
        {
          CPoint tPoint = m_trackRect.TopLeft();
          CPoint bPoint = m_trackRect.BottomRight();
          m_trackRect.SetRect(tPoint.x,point.y,point.x,bPoint.y);
        }
        break;
      case E:
        {
          CPoint& brPoint = m_trackRect.BottomRight();
          brPoint.x = point.x;
        }
        break;
      case S:
        {
          CPoint& brPoint = m_trackRect.BottomRight();
          brPoint.y = point.y;
        }
        break;
      case SW:    
        {
          m_trackRect.TopLeft().x = point.x;
          m_trackRect.BottomRight().y = point.y;
        }
        break;
      case W:
        {
          m_trackRect.TopLeft().x = point.x;
        }
        break;
      case NW:
        {
          m_trackRect.TopLeft().x = point.x;
          m_trackRect.TopLeft().y = point.y;
        }
        break;
      case N:
        {
          m_trackRect.TopLeft().y = point.y;
        }
        break;
      }

      if( m_selectedRect != NULL )
        m_selectedRect->AdjustBounds( pDC, m_trackRect );

      if( m_state != normal )
        pDC->DrawFocusRect( m_trackRect );

      break;
      
  } // OnMouseMove
  
  delete pDC;
}

///////////////
// Attribute //
///////////////

BOOL CMapPreview::InitializePreview( CMapLayout* pMapLayout, CMoMap* pMoMap )
// ersetzt CPreviewView::SetPrintView
// Bereitet alles für die Darstellung des MapLayouts vor
{
  ASSERT( pMapLayout && pMoMap ); // darf nicht NULL sein
  m_mapLayout = pMapLayout;
  
  // mich als Listener anmelden
  m_mapLayout->AddListener( this );
  
  ASSERT(m_pPreviewInfo);
  
  // m_pPreviewDC setzen ( bewirkt sonst nichts ) damit
  // CPrintPreview::OnClosePreview() keine ASSERTION durch den Aufruf
  // von CView::OnEndPrinting( m_pPreviewDC, this ) erzeugt
  m_pPreviewDC = new CPreviewDC;
  
  // PreparePrinting aufrufen, damit Einstellungen in pMapLayout neu initialisiert werden
  m_pPreviewInfo->m_bPreview = TRUE; // damit der Druckdialog nicht angezeigt wird
  return pMapLayout->OnPreparePrinting( m_pPreviewInfo, this, pMoMap );
} // SetMapLayout


/////////////////
// Operationen //
/////////////////

void CMapPreview::InvalidateLogRect( const CRect& rect )
// Invalidiert eine durch logische Koordinaten gegebene Region auf dem Bildschirm
{
  // zuerst die Koordinaten in device Pixels umrechnen
  CClientDC* pDC = GetDC();
  
  CRect dpRect = rect;
  pDC->LPtoDP(dpRect);
  dpRect.InflateRect( 10,10 ); // immer 5 Pixel mehr, damit auch sicher der ganze Rand neu gezeichnet wird
  
  // das wars schon
  InvalidateRect( dpRect );
  
  delete pDC;
}; // InvalidateLogRect

void CMapPreview::SizeToFit()
// die Darstellung optimal ins Fesnter einpassen
{
  ASSERT(m_mapLayout);
  
  m_mapLayout->SetZoomFaktor( 100 ); // 100% bedeutet 'Seitenansicht'
  ResyncScrollSizes();
}

void CMapPreview::ResyncScrollSizes()
// Passt die Scrollgrössen an
{
  if( m_mapLayout == NULL )
    return;
  
  CClientDC dc( this );
  OnPrepareDC( &dc, NULL );
  
  CSize sizeDoc = m_mapLayout->GetSize();
  UINT zoomFaktor = m_mapLayout->GetZoomFaktor();
  
  CSize sizePage;
  sizePage.cx = MulDiv(sizeDoc.cx, 100, zoomFaktor * 10 );
  sizePage.cy = MulDiv(sizeDoc.cy, 100, zoomFaktor * 10 );
  CSize sizeLine;
  sizeLine.cx = MulDiv(sizePage.cx, 100, zoomFaktor * 10 );
  sizeLine.cy = MulDiv(sizePage.cy, 100, zoomFaktor * 10 );
  dc.LPtoDP(&sizeDoc);
  dc.LPtoDP(&sizePage);
  dc.LPtoDP(&sizeLine);
  SetScrollSizes( MM_TEXT, sizeDoc, sizePage, sizeLine );
  
  // und alles neu Zeichnen
  Invalidate();
} // ResyncScrollSizes

void CMapPreview::ToLogCrds( CPoint& point )
// rechnet einen Punkt in Bildschirmkoordinaten in Logische Koordinaten um
{
  CClientDC* pDC = GetDC();
  
  // Umrechnung beruht darauf, dass der Viewport genau dem ClientBereich entspricht
  pDC->DPtoLP( &point );
  
  delete pDC;
}

CClientDC* CMapPreview::GetDC()
{
  // den Punkt noch um die Scrollposition verschieben
  CPoint ptOffset = GetDeviceScrollPosition();
  
  
  CClientDC* pDC = new CClientDC( this );
  OnPrepareDC( pDC, NULL );
  pDC->OffsetViewportOrg( -ptOffset.x, -ptOffset.y );
  
  return pDC;
}

///////////////////////////////
// Kommandos von den Knöpfen //
///////////////////////////////

BOOL CMapPreview::OnCommand( UINT nID )
// alle Kommandos der Knöpfe kommen hier an
{
  switch( nID )
  {
  case AFX_ID_PREVIEW_ZOOMIN:
    {
      UINT zF = min( m_mapLayout->GetZoomFaktor() * 2, 1000 ); // maximal 1000%
      m_mapLayout->SetZoomFaktor( zF );
      ResyncScrollSizes();
    }
    break;
    
  case AFX_ID_PREVIEW_ZOOMOUT:
    {
      UINT zF = max( m_mapLayout->GetZoomFaktor() / 2, 10 ); // mindestens 10%
      m_mapLayout->SetZoomFaktor( zF );
      ResyncScrollSizes(); 
    }
    break;
    
  case ID_PREVIEW_PRINTER_SETUP:
	m_mapLayout->DoPrinterSetup( CString(MAKEINTRESOURCE(IDS_PRINTER_SETUP)), this );
	ResyncScrollSizes(); 
   break;
    
  case ID_PREVIEW_SELECT:
  case ID_PREVIEW_NEW_TEXT:
  case ID_PREVIEW_NEW_LEGEND2:
  case ID_PREVIEW_NEW_MAP:
  case ID_PREVIEW_NEW_LOGO:
    m_curTool = nID;
    break;
    
  case ID_PREVIEW_DELETE:
    InvalidateLogRect( m_selectedRect->GetBounds() );
    m_mapLayout->DeleteRect( m_selectedRect );
    m_selectedRect = NULL;
    break;
    
  case ID_PREVIEW_ZBACK:
    m_mapLayout->ToBack( m_selectedRect );
    break;
    
  case ID_PREVIEW_ZFRONT:
    m_mapLayout->ToFront( m_selectedRect );
    break;
    
  case ID_PREVIEW_PROPS:
    if( m_selectedRect != NULL ) 
      m_selectedRect->DoPropertyDialog( this );
    break;
    
  default:
    return FALSE; // falls die ID nicht bekannt ist soll sich ein anderer drum kümmern
  }
  
  return TRUE;
}

void CMapPreview::OnUpdateCommand( CCmdUI* pCmdUI )
// Alle UpdateUIs der Knöpfe kommen hier an
{
  ASSERT( m_mapLayout );
  
  BOOL bEnable = FALSE;
  UINT id = pCmdUI->m_nID;
  switch( id )
  {
  case AFX_ID_PREVIEW_PRINT:
      bEnable = TRUE;
    break;

  case AFX_ID_PREVIEW_ZOOMOUT:
    bEnable = m_mapLayout->GetZoomFaktor() > 10;
    break;
    
  case AFX_ID_PREVIEW_ZOOMIN:
    bEnable = m_mapLayout->GetZoomFaktor() < 1000;
    break;
    
  case ID_PREVIEW_SELECT:
  case ID_PREVIEW_NEW_TEXT:
  case ID_PREVIEW_NEW_MAP:
  case ID_PREVIEW_NEW_LOGO:
  case ID_PREVIEW_NEW_LEGEND2:
    bEnable = TRUE; // die Tools sind immer erlaubt
    break;
    
  case ID_PREVIEW_DELETE:
  case ID_PREVIEW_ZFRONT:
  case ID_PREVIEW_ZBACK:
  case ID_PREVIEW_PROPS:
    bEnable = m_selectedRect != NULL;
    break;
  }; // switch ID
  
  // und die UI erlauben oder halt nicht
  pCmdUI->Enable( bEnable );
}

void CMapPreview::Update( const CRect& rect, const BOOL bPages )
// Implementation des CPrintRectListener Interfaces
{
  if( bPages )
    ResyncScrollSizes();
  else
    InvalidateLogRect( rect );
} // Update