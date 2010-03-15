// PrintRectMap.cpp: Implementierung der Klasse CPrintRectMap.
//
//////////////////////////////////////////////////////////////////////
#pragma warning(disable:4786)
#pragma warning(disable:4503)

#include "stdafx.h"

#include "mapHelper.h"
#include "wspmap.h"
#include "printMapPage.h"

#include "PrintRectMap.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

IMPLEMENT_SERIAL( CPrintRectMap, CPrintRect, VERSIONABLE_SCHEMA | 2 )

//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

CPrintRectMap::CPrintRectMap() : CPrintRect()
{
  m_pMoMap = NULL;
  m_scale = 50000; // Standard: 1 : 50000, wird nie benutzt

  SetFontAlign( -1 ); // nichts zeichnen
  SetFontPtSize( 240 ); // 24pt
}

CPrintRectMap::~CPrintRectMap()
{
}

void CPrintRectMap::SetMap( CMoMap* pMoMap )
// wird vor jedem Anzeigen der Preview und dem Drucken aufgerufen
// es kann sein, dass sich der Kartenausschnitt ge‰ndert hat
// deswegen dei Bounds neu setzen, um das Rechteck anzupassen
{
  CPrintRect::SetMap( pMoMap );

  CalcScale( GetBounds() );
  SetBounds( GetBounds(), NULL );
}

/* virtual */
void CPrintRectMap::SetBounds( const CRect& rect, CDC* pDC )
{
  // die neue Skale ausrechnen!
  //calcScale( rect );

  CPrintRect::SetBounds( rect, pDC );
}

void CPrintRectMap::AdjustBounds( CDC* pDC, CRect& bounds )
// Setzt die Grˆsse des Druckrechtecks, wobei die Seitenverh‰ltnisse
// an die des Kartenausschnitts angepasst werden
{
  // erstmal das aktuelle verh‰ltnis der Karte bestimmen
  double mapHeight = m_pMoMap->GetExtent().GetHeight() * 1000.0;
  double mapWidth = m_pMoMap->GetExtent().GetWidth() * 1000.0;
  double mapRatio = mapHeight / mapWidth;

  // je nachdem die Hˆhe an die BReit oder die Breite an die Hˆhe anpassen
  double boundsWidth = bounds.Width();
  double boundsHeight = boundsWidth * mapRatio;
  
  bounds.bottom = bounds.top + boundsHeight;
  CalcScale( bounds );  
} // SetBounds

void CPrintRectMap::Paint( CDC* pDC )
// Zeichnet die Karte im richtigen Maﬂstab
{
  // Im Gegensatz zur ESRI-Dok, m¸ssen bei CMoMap::OutputMap2 die logischen
  // Koordinaten des Ausgaberechtecks angegeben werden
  // das Funktioniert einfach so schon ganz prima, nur die Linienst‰rken
  // und Punktgrˆssen h‰ngen vom aktuellen MappingMode ab
  // -> deswegen m¸ssen wir auf MM_TEXT umschalten, dann wird wieder in Pixeln
  // gerechnet

  // da wir im neuen Mapping mode die logischen Koordinaten brauchen, m¸ssen
  // wir unsere Bounds erstmal in Device Koordinaten umrechnen
  CRect outRect = GetBounds();
  pDC->LPtoDP( outRect );


  // die Hˆhe des Fonts transformieren
  LOGFONT logFont = GetFontLogfont();
  // jetzt Punkt in Millimeter umrechnen; 1pt = 0.376 mm -> 1/10pt = 0.0376mm
  logFont.lfHeight = -(long)( GetFontPtSize() * 0.0376 ); // gibt blˆde Rundungsfehler -> vielleicht doch lieber in MM_HIMETRIC arbeiten??
  CFont font;
  if( !font.CreateFontIndirect( &logFont ) )
    return;

  // den Context merken
  int saveDC = pDC->SaveDC();

  // jetzt den Mapping Mode ‰ndern
  pDC->SetMapMode( MM_TEXT );

  // die Device Koordinaten wieder zur¸ckrechnen
  pDC->DPtoLP( outRect );

  // jetzt endlich kˆnnen wir die Karte schˆn ausgeben
  m_pMoMap->OutputMap2( (OLE_HANDLE)pDC->GetSafeHdc(), outRect.left, outRect.top, 
    outRect.Width(), outRect.Height(), CComVariant( moNoBackground | moClipToExtent ) );

  // TODO: die Karte wird nicht hundertprozent genau in dieses outRect gezeichnet ( meistens ein bisschen
  // zu schmal und ein bisschen zu lang ). Das outRect stimmt aber ( ein gezeichnetes Rechteck stimmt genau mit
  // dem Rand ¸berein -> entweder wird der Rand nicht genau gezeichnet oder es liegt am MM_TEXT mapping
  // Mode ( im alten Mapmode wars besser ( Breite ok, L‰nge zu lang ), allerdings sind dann die Linien zu dick.


  // jetzt noch den aktuellen Maszstab ausgeben

  // Font und Farbe setzen
  pDC->SelectObject( &font ); // den neuen Font setzen
  pDC->SetTextColor( GetFontColor() ); // und die Farbe
  pDC->SetBkMode( TRANSPARENT ); // den Hintergrund nicht zeichnen


  // Position des Textes ermitteln
  CRect textRect( outRect );
  textRect.DeflateRect( 5, 5 ); // damit der Text nicht ganz am Rand klebt

  CString scaleStr;
  scaleStr.Format( TEXT("1 : %d"), m_scale );

  if( GetFontAlign() != -1 )
    pDC->DrawText( scaleStr, textRect, GetFontAlign() );

  // und den Context wiederherstellen
  pDC->RestoreDC( saveDC );

  // und den Rand zeichnen lassen
  CPrintRect::Paint( pDC );
} // Paint

CPropertyPage* CPrintRectMap::CreatePropPage( UINT captionID )
{
  return new CPrintMapPage( IDS_PRINT_PROP_CAPTION, this );
}

void CPrintRectMap::SetScale( const UINT scale, const BOOL bAdjustRect, const BOOL bDouble /* = false */ )
// Setzt den Maﬂstab neu und ‰ndert entweder Kartenauschnitt oder Druckrechteck
// Parameter:
//        const UINT scale: der neue Masstab
//        const BOOL bAdjustRect: falls TRUE wird das Druckrechteck entsprechend ge‰ndert,
//                                falls FALSE wird der Kartenausschnitt neu gesetzt
{
  ASSERT( scale > 0 );

  TRACE( "\n\nSetze scale auf: %d\n", scale );

  if( bAdjustRect )
  {
    // die Grˆsse der Druckbereichs anhand von Extent und Scale setzen
    
    // in Millimeter umrechnen
    CMoRectangle mapExtent = m_pMoMap->GetExtent();
    double width = mapExtent.GetWidth() * 1000;
    double height = mapExtent.GetHeight() * 1000; 

    // jetzt die neuen Bounds ausrechnen: TODO: das ist zu ungenau->dadurch gibts zu grosse rundungsfehler
    // die Bounds m¸ssten in 10tel oder 100tel millimeter sein (noch ein Tip: kˆnnen wir nicht gleich in MM_TEXT zeichnen?)
    double printWidth = width / (double)scale;
    double printHeight = height / (double)scale;

    CRect newBounds = GetBounds();
    newBounds.right = newBounds.left + (int)printWidth;
    newBounds.bottom = newBounds.top + (int)printHeight;
    SetBounds( newBounds, NULL );
  }
  else
  {
    // den Kartenausschnitt anhand der Grˆsse des Druckbereichs und der Scale setzen
    // den gleichen Mittelpunkt beibehalten
    
    // pr¸fen, ob wird am maximum sind, dann muss die Scale zweimal gesetzt werden
    // ziemlich ¸bler hack, liegt aber an MapObjects, wenn der Extent grˆsser als der FullExtent
    // war, wird der neue Extent nicht richtig gesetzt (eigene Anpassung von MO, beim zweiten mal 
    // klappts aber immer 
    CMoRectangle fullExtent = m_pMoMap->GetFullExtent();
    CMoRectangle mapExtent = m_pMoMap->GetExtent();
    if( !bDouble && ( mapExtent.GetWidth() >= fullExtent.GetWidth() || mapExtent.GetHeight() >= fullExtent.GetHeight() ) )
      SetScale( scale, bAdjustRect, true );

    // das Kantenverh‰ltnis der Karte
    double mapHeight = m_pMoMap->GetExtent().GetHeight() * 1000.0;
    double mapWidth = m_pMoMap->GetExtent().GetWidth() * 1000.0;
    double mapRatio = mapHeight / mapWidth;

    // Ausmasse der Karte in Millimeter
    CRect bounds = GetBounds();
    double width = bounds.Width() * double(scale) / 1000.0;
    double height = width * mapRatio; // damit das Verh‰ltnis gewahrt bleibt

    TRACE( "New width: %lf\n", width );
    TRACE( "New height: %lf\n", height );
    
    CMoRectangle oldExtent = m_pMoMap->GetExtent();
    TRACE( "Old Extent ist left: %lf\n", oldExtent.GetLeft() );
    TRACE( "Old Extent ist right: %lf\n", oldExtent.GetRight() );
    TRACE( "Old Extent ist top: %lf\n", oldExtent.GetTop() );
    TRACE( "Old Extent ist bottom: %lf\n", oldExtent.GetBottom() );

    CMoPoint centerPoint = oldExtent.GetCenter();

    CMoRectangle newExtent;
    MO2CREATE( newExtent, "Rectangle" );

    double newLeft = centerPoint.GetX() - width / 2.0;
    double newRight = newLeft + width;
    double newBottom = centerPoint.GetY() - height / 2.0;
    double newTop = newBottom + height;

    TRACE( "newleft: %lf\n", newLeft );
    TRACE( "newright: %lf\n", newRight );
    TRACE( "newtop: %lf\n", newTop );
    TRACE( "newbottom: %lf\n", newBottom );

    // das komische ausrechnen ist nˆtig, da CMoMap.centerAt erst wirksam wird, wenn die Karte wieder
    // neu in der CMapView angezeigt wird, setExtent aber immer geht
    newExtent.SetLeft( newLeft );
    newExtent.SetRight( newRight );
    newExtent.SetTop( newTop );
    newExtent.SetBottom( newBottom );

    m_pMoMap->SetExtent( newExtent );

    CMoRectangle newTExtent( m_pMoMap->GetExtent() );

    TRACE( "New Extend ist left: %lf\n", newTExtent.GetLeft() );
    TRACE( "New Extend ist right: %lf\n", newTExtent.GetRight() );
    TRACE( "New Extend ist top: %lf\n", newTExtent.GetTop() );
    TRACE( "New Extend ist bottom: %lf\n", newTExtent.GetBottom() );

    double newTWidth = newTExtent.GetWidth();
    double newTHeight = newTExtent.GetHeight();

    SetBounds( GetBounds(), NULL );
       
    TRACE( "Scale nach setScale: %d\n\n", m_scale );
    NotifyListeners( bounds );
  };
} // SetScale


/*!
 * Rechnet den aktuellen Maﬂstab der Karte aus
 *
 */
void CPrintRectMap::CalcScale( const CRect& rect )
{
  CMoRectangle mapExtent = m_pMoMap->GetExtent();
  
  // die L‰ngen der fixierten Seite zum ausrechnen benutzen
  double mapWidth = mapExtent.GetWidth();

  double printWidth = rect.Width();
  
  // in Millimeter umrechnen ( TODO: h‰ngt ab von Karteneinheit )
  mapWidth *= 1000;
  
  // den Maﬂstab ausrechnen: vorl‰ufiger Maﬂstab
  int scale = int( mapWidth / printWidth );

  // jetzt den Maﬂstab solange runden, bis der Fehler grˆsser als ein Millimeter ist
  int digit = 1;
  double newScale = scale;
  double lastScale = scale;
  /*
  while( fabs( ( mapWidth / newScale ) - printWidth ) < 1 )
  {
    double faktor = pow( 10, digit++ );
    lastScale = newScale;

    double toRound = newScale / faktor;
    //dirty hack
    char buffer[256];
    buffer[0] = '\0';

    sprintf( buffer, "%lf.0", toRound );
    newScale = atoi( buffer ) * faktor;
  }
  */

  m_scale = lastScale;
}

void CPrintRectMap::Serialize( CArchive& ar )
{
  // zuerst die allgmeinen Daten laden/schreiben
  CPrintRect::Serialize( ar );
  
  if( ar.IsStoring() )
  {
    ar << m_scale;

    WriteFont( ar );
  }
  else
  {
    UINT nVersion = ar.GetObjectSchema();

    ar >> m_scale;

    if( nVersion > 0 )
      ReadFont( ar );
  }; 
  
}; // Serialize