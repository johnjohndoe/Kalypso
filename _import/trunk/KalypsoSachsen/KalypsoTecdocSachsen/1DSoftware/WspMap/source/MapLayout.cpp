// MapLayout.cpp: Implementierung der Klasse CMapLayout.
//
//////////////////////////////////////////////////////////////////////


#include "stdafx.h"

#include "resource.h"

#include "commonMfc\include\printerSettings.h"

#include "mapHelper.h"
#include "printRect.h"
#include "printRectText.h"
#include "printRectLegend.h"
#include "printRectLegend2.h"
#include "printRectMap.h"
#include "printRectOle.h"
#include "printRectImage.h"

#include "MapLayout.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

IMPLEMENT_SERIAL( CMapLayout, CObject, VERSIONABLE_SCHEMA | 0 )

//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

CMapLayout::CMapLayout()
{
  m_pageCountX = 0;
  m_pageCountY = 0;
  CSize m_printSize = CSize( 0, 0 );
  m_nZoomFaktor = 100; // der Zoomfaktor für die Druckvorschau

  m_printerSettings = NULL;
  m_pMoMap = NULL;
}

CMapLayout::~CMapLayout()
{
  DeleteContents();
}

void CMapLayout::DeleteContents()
// zerstört alle assoziierten Objekte
{
  delete m_printerSettings;
  m_printerSettings = NULL;

  for( int i = 0; i < m_printRects.GetSize(); i++ )
	  delete m_printRects[i];
  m_printRects.RemoveAll();
} // DeleteContents


void CMapLayout::Initialize()
// lädt standardeinstellungen für die Karte, falls nicht schon geschehen
{
  // falls noch keine PrinterSettings vorhanden sind, diese jetzt erzeugen
  if( m_printerSettings == NULL )
  {
    m_printerSettings = new CPrinterSettings();
    m_printerSettings->CopyDefaultMfcPrinter();
  };
}

//////////////////////////////////////////////////////////////////////
// Attribute
//////////////////////////////////////////////////////////////////////

CSize CMapLayout::GetExtent() const
// Gibt die Grösse des Anzeigebereichs in Millimetern zurück
{
  int width = 0;
  int height = 0;

  for( int i = 0; i < m_printRects.GetSize(); i++ )
  {
    CRect rect = m_printRects[i]->GetBounds();
    width = max( width, rect.right );
    height = max( height, rect.bottom );
  } // for i

  return CSize( width, height ); // erstmal standard 30 x 40 cm
} // GetExtent

//////////////////////////////////////////////////////////////////////
// Operationen
//////////////////////////////////////////////////////////////////////

void CMapLayout::AddPrintRect( CPrintRect* newRect )
// nimmt ein neues PrintRect in die Liste auf und meldet sich selbst als Listener an
{
  ASSERT( newRect != NULL );

  m_printRects.Add( newRect );
  newRect->AddListener( this );
} // AddPrintRect

void CMapLayout::Serialize( CArchive& ar )
// Serialisierung a la CObject
{
  if ( ar.IsStoring() )  // Speichern
  {
    ar.WriteObject( m_printerSettings );
    
    // die Druckrechtecke speichern
    ar << m_printRects.GetSize();
    for( int i = 0; i < m_printRects.GetSize(); i++ )
      ar.WriteObject( m_printRects[i] );
  } // IsStoring
  else                  // Laden
  {
    // evtl. vorhandene Inhalte löschen
    DeleteContents();

    UINT nVersion = ar.GetObjectSchema();

    m_printerSettings = (CPrinterSettings*)ar.ReadObject( RUNTIME_CLASS(CPrinterSettings) );

    // die Druckrechtecke lesen
    int count;
    ar >> count;
    for( int i = 0; i < count; i++ )
    {
      CPrintRect* newRect = (CPrintRect*)ar.ReadObject( RUNTIME_CLASS(CPrintRect) );
      if( newRect != NULL  )
      {
        // alte CPrintRectLegend's rausschmeissen
        if( newRect->GetRuntimeClass() == RUNTIME_CLASS( CPrintRectLegend ) )
        {
          delete newRect;
          newRect = 0;
        }
        else
          AddPrintRect( newRect );
      }
    } // for i

    // nach dem Laden Darstellungsgrössen neu Ausrechnen
    CalcPageSizes();
  }    
} // Serialize

void CMapLayout::DoPrinterSetup( LPCTSTR strTitle /* = NULL */, CWnd* pWnd /* = NULL */ )
// zeigt den Dialog zum Ändern der Druckeinstellungen an
{
  if( m_printerSettings != NULL )
    m_printerSettings->PrinterSetup( strTitle, pWnd );

  // nach Änderung der Druckereinstellungen die Seitengrössen neu ausrechnen
  CalcPageSizes();
} // DoPrinterSetup

BOOL CMapLayout::CalcPageSizes()
// Berechnet die Aufteilung der Seiten für den Druck und die gesamtgrösse
// des Ausdrucks
// Sollte immer nach Änderungen der Druckeinstellungen aufgerufen werden
// sowie nach Änderung der zu druckenden Daten
// Rückgabewert:
//          TRUE, falls die Seitenaufteilung geändert wurde, sonst FALSE
{
  if( !m_printerSettings )
    return FALSE;

  // wie gross muss es den mindestens sein
  CSize extent = GetExtent();

  CSize pageSize = m_printerSettings->GetPaperSize();

  UINT pageCountX = 0;
  UINT pageCountY = 0;
  
  m_printSize = CSize( 0, 0 );

  // testen, da sonst endlosschleife
  if( pageSize.cx == 0 || pageSize.cy == 0 )
  {
    m_pageCountX = 0;
    m_pageCountY = 0;

    return TRUE;
  }

  // X-Richtung
  while( extent.cx >= 0 )
  {
    extent.cx -= pageSize.cx;
    m_printSize.cx += pageSize.cx;
    pageCountX++;
  }  // while extent.cx

  // y-Richtung
  while( extent.cy >= 0 )
  {
    extent.cy -= pageSize.cy;
    m_printSize.cy += pageSize.cy;
    pageCountY++;
  } // while extent.cy

  // falls sich was geändert hat true zurück
  if( m_pageCountX != pageCountX || m_pageCountY != pageCountY )
  {
    m_pageCountX = pageCountX;
    m_pageCountY = pageCountY;
    return TRUE;
  }
  else
    return FALSE;
} // CalcPageSizes

BOOL CMapLayout::OnPreparePrinting( CPrintInfo* pInfo, CView* pView, CMoMap* pMoMap )
// die eigentliche Überschreibung von OnPrepearePrinting von CMapView
// Die Seitenanzahlen werden entsprechend gesetzt
// Parameter:
//        CPrintInfo* pInfo: wie an CView::OnPreparePrinting übergeben
//        CView* pView: Zeigt auf eine View ( um deren DoPreparePrinting aufzurufen ), darf nicht NULL sein
// Rückgabewert: der Rückgabewert von CView::DoPreparePrinting
{
  ASSERT(m_printerSettings);
  ASSERT(pView);
  
  BOOL bRet = FALSE;
  
  // die richtigen Einstellungen einstellen
  m_printerSettings->SetThisPrinter();
  
  
  // die Referenz auf die Karte prüfen und übernehmen
  ASSERT( pMoMap );
  m_pMoMap = pMoMap;
  
  // die PrinterSettings mit dem StandardDrucker initialisieren
  if( m_printerSettings == NULL )
  {
    m_printerSettings = new CPrinterSettings();
    m_printerSettings->CopyDefaultMfcPrinter();
  }
    
  // falls bereits Druckrechtecke da sind, diese mit der Karte initialisieren
  for( int i = 0; i < m_printRects.GetSize(); i++ )
    m_printRects[i]->SetMap( pMoMap );
    
  // auf jeden Fall die Darstellungsgrössen neu ausrechnen
  CalcPageSizes();

  // und die Anzahl der Seiten setzen
  pInfo->SetMaxPage( GetPageCount() );
    
  bRet = pView->DoPreparePrinting(pInfo);

  // falls die Einstellungen ( potentiel ) geändert wurden
  // diese jetzt übernehmen
  if( bRet )
  {
    m_printerSettings->CopyDefaultMfcPrinter();
    CalcPageSizes(); // die könnten sich geändert haben

    // die Seitenanzahlen könnten sich geändert haben, dies jetzt anpassen
    pInfo->SetMaxPage( GetPageCount() );
  }

  // noch ein paar Einstellungen reinitialisieren
  m_nZoomFaktor = 100;

  return bRet;
} // PreparePrinting

void CMapLayout::OnBeginPrinting( CDC* pDC, CPrintInfo* pInfo )
// Die eigentliche Überschreibung von OnBeginPrinting von CMapView
// Bei der Druckvorschau gibts nur eine Seite!
{
  if( pInfo->m_bPreview )
  {
    // in der Vorschau wird immer genau eine Seite angezeigt
    // eine Übersicht über alles
    pInfo->SetMaxPage( 1 );
    pInfo->SetMinPage( 1 );
  }
  
} // BeginPrinting

void CMapLayout::OnPrepareDC(CDC* pDC, CPrintInfo* pInfo, const CSize& clientRect )
// Die eigentliche Überschreibung von CMapView::OnPrepareDC für
// Druck und Druckvorschau
// Der Mapping Mode wird so eingestellt, dass:
//        - der Ursprung links oben ist
//        - pos x Achse nach Rechts
//        - pos y Achse nach unten
//        - logische Einheiten in Millimetern
// Für den Ausdruck wird der Ursprung so verschoben, dass die
// richtige Seite gedruckt wird
{
  // mapping mode is MM_ANISOTROPIC
  // these extents setup a mode similar to MM_LOMETRIC
  // MM_LOMETRIC is in .1 physical millimetres
  // these extents provide .1 logical millimetres
  pDC->SetMapMode(MM_ANISOTROPIC);
    
  double hppmm, vppmm;

  int hRes, vRes; // Anzahl Pixel des Zeichenbereiches
  if( pInfo == NULL )
  {
    hRes = clientRect.cx;
    vRes = clientRect.cy;
  }
  else
  {
    hRes = pDC->GetDeviceCaps(HORZRES);
    vRes = pDC->GetDeviceCaps(VERTRES);
  };

  int hSize = pDC->GetDeviceCaps(HORZSIZE);
  int vSize = pDC->GetDeviceCaps(VERTSIZE);

  hppmm = (double)hRes / (double)hSize;
  vppmm = (double)vRes / (double)vSize;
  
  CSize sizeDoc = m_printSize;
  sizeDoc.cy = -sizeDoc.cy;
  pDC->SetWindowExt( sizeDoc );
  
  long xExt = (long)(sizeDoc.cx * hppmm );
  long yExt = (long)(sizeDoc.cy * vppmm );
  pDC->SetViewportExt((int)xExt, (int)yExt);

  // Ursprung und Zoom hängen davon ab, ob gedruckt wird oder nicht
  CPoint ptOrg;
  if( pInfo == NULL )
  {	
    // wenn nicht gedruckt wird, entsprechend des ZoomFaktor skalieren
    // is der zoomfaktor noch nicht gesetzt ( == 0 ), einen Standardwert nehmen
    if( m_nZoomFaktor == 0 )
      m_nZoomFaktor = 100;

    // für die Druckvorschau so zurückskalieren, dass 100% Zoom 'Seitenansicht' bedeuten
    int denomX = hRes == 0 ? 100 : xExt * 100 / hRes;
    int denomY = vRes == 0 ? 100 : -yExt * 100 / vRes;

    int denom = max( denomX, denomY );
    denom += 1; // ein bisschen kleiner, damits dann wirklich passt

    //pDC->ScaleViewportExt(m_nZoomFaktor, denom, m_nZoomFaktor, denom );
    // manchmal gibt ScaleViewPortExtEx ohne ersichtlichen Grund 0 zurück, was in
    // pDC->ScaleViewportExt zu einer Assertion führt ( GetLastError gibt aber 0 zurück )
    // deswegen wirds direkt aufgerufen
    ::ScaleViewportExtEx( pDC->m_hDC, m_nZoomFaktor, denom, m_nZoomFaktor, denom, NULL );

    // when not printing the page is the whole drawing
    ptOrg.x = 0;
    ptOrg.y = 0;
  }
  else
  {	// when printing we must set the origin to the top left of the current page
    // ausrechnen welche Seite ( x / y ) gedruck wird
    int page = pInfo->m_nCurPage - 1;
    int pageX = page % m_pageCountX;
    int pageY = ( page - pageX ) / m_pageCountX; // sollte immer aufgehen

    CSize pageSize = m_printerSettings->GetPaperSize();
    ptOrg.x = pageX * pageSize.cx;
    ptOrg.y = pageY * pageSize.cy;
  }
  
  // set the Window origin
  pDC->SetWindowOrg( ptOrg );
}  // PrepeareDC

void CMapLayout::Paint( CDC* pDC )
// Hier wird endlich tatsächlich etwas gezeichnet
// Der CDC muss so vorbereitet sein, dass alles einfach in 10tel Millimetern
// d.h. Papierkoordinaten rausgeschrieben wird
// Parameter:
//      CDC* pDC: hierhinein wird gezeichnet. Der Mapping mode muss so eingestellt sein,
//                dass alles in Millimetern rausgeschrieben werden kann
//      const CPoint& ptOffset: alles wird noch um dieses Offset verschoben
{
  ASSERT(m_printerSettings);

  // zuerst den Hintergrund neu zeichnen
  CBrush brush;
  if( !brush.CreateSolidBrush( RGB( 255, 255, 255 ) ) )
    return;
  
  brush.UnrealizeObject();
  CRect backGrnd = CRect( 0, 0, m_printSize.cx, m_printSize.cy );
  pDC->FillRect( backGrnd, &brush );
  
  // die alten GDI Objekte merken
  CPen* oldPen = NULL;

  CSize paperSize = m_printerSettings->GetPaperSize();
  
  // falls es die Seitenvorschau ist, eine Seitenunterteilung zeichnen
  if( !pDC->IsPrinting() )
  {
    CPen pagePen;
    if( pagePen.CreatePen( PS_DOT, 1, RGB( 0, 0, 255 ) ) )
      oldPen = (CPen*)pDC->SelectObject( &pagePen );
  
    for( UINT i = 1; i < m_pageCountX; i++ )
    {
      CPoint pt( paperSize.cx * i, 0 );
      pDC->MoveTo( pt );

      pt = CPoint( paperSize.cx * i, m_printSize.cy );
      pDC->LineTo( pt );
    } // for i
  
    for( i = 1; i < m_pageCountY; i++ )
    {
      CPoint pt( 0, paperSize.cy * i );
      pDC->MoveTo( pt );

      pt = CPoint( m_printSize.cx, paperSize.cy * i );
      pDC->LineTo( pt );
    } // for j
  } // if !pDC->IsPrinting()

  CPen blackPen;
  if( blackPen.CreatePen( PS_SOLID, 1, RGB( 0, 0, 0 ) ) )
  {
    CPen* otherOldPen = pDC->SelectObject( &blackPen );
    if( oldPen == NULL )
      oldPen = otherOldPen;
  }

  // die einzelnen Druckrechtecke zeichnen
  for( int i = 0; i < m_printRects.GetSize(); i++ )
	m_printRects[i]->Paint( pDC );
  
  // die alten GDI Objekte wiederherstellen
  if( oldPen )
    pDC->SelectObject( oldPen );
} // Print

BOOL CMapLayout::IsPointInPage( const CPoint& point ) const
// Testet, ob der angegebene Punkt im Druckbereich liegt
// Parameter:
//			const CPoint& point: der zu testende Punkt
// Rückgabewert:
//		TRUE, falls ja 
{
	if( 0 <= point.x && 0 <= point.y && point.x <= m_printSize.cx && point.y <= m_printSize.cy )
		return TRUE;
	else
		return FALSE;
} // IsPointIn

void CMapLayout::CreatePrintRect( const CRect& rect, const UINT type, CDC* pDC )
// Erzeugt ein neues Druckrechteck
// Parameter:
//        const Rect& rect: die Ausmasse des neuen Rechtecks
//        const UINT type: der Typ des zu erzeugenden Rechtecks
//        CDC* pDC: braucht CPrintRect::SetBounds
{
	CPrintRect* printRect = NULL;
	BOOL bShowProperties = FALSE;
	
	switch( type )
	{
	case ID_PREVIEW_NEW_TEXT:
		printRect = new CPrintRectText();
		bShowProperties = TRUE;
		break;
		
  case ID_PREVIEW_NEW_LEGEND2:
    printRect = new CPrintRectLegend2();
    bShowProperties = FALSE;
    break;
    
	case ID_PREVIEW_NEW_MAP:
		printRect = new CPrintRectMap();
		bShowProperties = TRUE;
		break;

  case ID_PREVIEW_NEW_LOGO:
/*
    printRect = new CPrintRectOle();
    if( !((CPrintRectOle*)printRect)->CreateOleObject() )
    {
      delete printRect;
      printRect = NULL;
    };
*/
    printRect = new CPrintRectImage();
    bShowProperties = TRUE;
    break;
	}; // switch type

  if( printRect != NULL )
  {
    AddPrintRect( printRect );

    printRect->SetMap( m_pMoMap ); // dem PrintRect die Referenz auf die Karte geben
    printRect->SetBounds( rect, pDC );
	
    // falls der Eigenschaftendialog gleich angezeigt werden soll, dies jetzt tun
    if( bShowProperties && printRect->DoPropertyDialog() != IDOK )
    {
      // falls der Dialog mit CANCEL verlassen wird, das Rechteck gleich wieder löschen
      DeleteRect( printRect );
      printRect = NULL;
    }
  } // if printRect
} // CreatePrintRect


CPrintRect* CMapLayout::IsPointInRect(const CPoint& point)
// Gibt das Rechteck zurück, in dem der angeklickte Punkt liegt
{
  // umgekehrt suchen, damit die oberen Rechtecke zuerst gefunden werden
  for( int i = m_printRects.GetSize(); i > 0; i-- )
  {
    if( m_printRects[i - 1]->GetBounds().PtInRect( point ) )
      return m_printRects[i - 1];
  } // for i
  return NULL;
}

void CMapLayout::ToFront(CPrintRect* rect)
// bringt das angegebene Rechteck in der Z-Order ganz nach vorne
{
  //finde Position
	for(int i =0;i<m_printRects.GetSize();i++)
	{
		if(m_printRects[i]==rect)
		{  
			//Die Stelle ist gefunden, entfernen
			m_printRects.RemoveAt(i);
			//Am Ende wieder einfügen
			m_printRects.Add(rect);

      Update( rect->GetBounds() );
		}
	}
}

void CMapLayout::DeleteRect(CPrintRect* rect)
// löscht das angegebene Rechteck
{
  ASSERT( rect );

  for(int i=0;i<m_printRects.GetSize();i++)
  {
    if( m_printRects[i] == rect )
    {
      CRect bounds = rect->GetBounds();

      delete m_printRects[i];
      m_printRects.RemoveAt( i );

      // erst nach dem löschen updaten, da sonst die Seitengrössen falsch berechnet werden
      Update( bounds );
   }
  } // for i
} // DeleteRect

void CMapLayout::ToBack(CPrintRect* rect)
// bringt das angegebene Rechteck in der Z-Order ganz nach hinten
{
  //finde Position
	for(int i =0;i<m_printRects.GetSize();i++)
	{
		if(m_printRects[i]==rect)
		{  
			//Die Stelle ist gefunden, entfernen
			m_printRects.RemoveAt(i);
			//Am Ende wieder einfügen
			
			m_printRects.InsertAt(0,rect);

      Update( rect->GetBounds() );
		}
	}
}

void CMapLayout::AddListener( CMapLayoutListener* pListener )
// fügt einen neuen Listener in die Liste ein
{
  ASSERT( pListener != NULL );

  m_listeners.Add( pListener );
} // AddListener

void CMapLayout::RemoveListener( CMapLayoutListener* pListener )
// löäscht einen Listener aus der Liste
{
  for( int i = 0; i < m_listeners.GetSize(); i++ )
  {
    if( m_listeners[i] == pListener )
    {
      m_listeners.RemoveAt( i );
      return;
    }; // if ...
  }; // for i
}; // RemoveListener

void CMapLayout::NotifyListeners( const CRect& rect, const BOOL bPages )
// benachrichtigt alle Listener in der Liste
{
  for( int i = 0; i < m_listeners.GetSize(); i++ )
    m_listeners[i]->Update( rect, bPages );
} // NotifyListeners
  


void CMapLayout::Update( const CRect& rect )
// Implementation des Interfaces CPrintRectListener
{
  // einfach den Event weiterschicken an den CMapLayoutListener
  // vorher schnell die Seitengrössen neu ausrechnen
  NotifyListeners( rect, CalcPageSizes() );
}; // Update
