/*! Time-stamp: <@(#)PrintRectLegend2.cpp   04.12.02 - 10:20:03   Belger>
 *********************************************************************
 *  @file   : PrintRectLegend2.cpp
 *
 *  Author  : Belger                              Date: 04.12.02
 *
 *  Purpose : Implementation of methods for class CPrintRectLegend2
 *
 *********************************************************************
 */

#include "stdafx.h"

#include "resource.h"

#include "commonMfc/include/variant_helper.h"

#include "mapHelper.h"
#include "PrintLegend2Page.h"

#include "PrintRectLegend2.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

IMPLEMENT_SERIAL( CPrintRectLegend2, CPrintRect, VERSIONABLE_SCHEMA | 2 )

//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////


/*!
 * Destruktor. Ruft nur DeleteContents() auf.
 *
 */
/* virtual */
CPrintRectLegend2::~CPrintRectLegend2()
{
  DeleteContents();
}


/*!
 * Löscht alle Inhalte.
 */
void CPrintRectLegend2::DeleteContents()
{
  m_entries.RemoveAll();
}


/*!
 * Überschreibung von CPrintRect::AdjustBounds.
 * Mithilfe von Draw(...) wird die benötigte Grösse ermittelt.
 *
 * @param pDC : der DeviceContext
 * @param bounds : die gewünschte Grösse, wird entsprechend verändert
 *
 */
void CPrintRectLegend2::AdjustBounds( CDC* pDC, CRect& bounds )
{
  // wenn die Karte noch nicht gesetzt ist, nix tun
  if( !GetMap() )
    return;

  // so tun, als wollten wir zeichnen, dabei wird die Grösse bestimmt
  Draw( pDC, &bounds, FALSE );

  return;
};


/*!
 * Überschreibung von CPrintRect::Paint
 * Ruft die Draw Routine auf, die die eigentliche Zeichnung macht.
 *
 * @param pDC : In diesen Context wird gemalt.
 *
 */
void CPrintRectLegend2::Paint( CDC* pDC )
{
  CRect bounds = GetBounds();
  Draw( pDC, &bounds, TRUE );
  if( GetBounds() != bounds )
    SetBounds( bounds, pDC );
} // Paint



/*!
 * Erzeugt die eigenschaftenseite.
 * In diesem Fall eine CPrintRectLegend2Page
 *
 * @param captionID : Für die Überschrift des Reiters.
 *
 * @return CPropertyPage*  : die neue Seite, muss extern zerstört werden
 */
CPropertyPage* CPrintRectLegend2::CreatePropPage( UINT captionID )
{
  return new CPrintLegend2Page( captionID, this );
}


/*!
 * Die Serialisierung.
 * Es wird sich nur die gewählte Schirft gemerkt.
 *
 * @param ar : in dieses Archiv wird serialisiert
 *
 */
void CPrintRectLegend2::Serialize( CArchive& ar )
{
  // zuerst die allgmeinen Daten laden/schreiben
  CPrintRect::Serialize( ar );

  if( ar.IsStoring() )
  {
    WriteFont( ar );
  }
  else
  {
    ReadFont( ar );
  }; 
}; // Serialize

/*!
 * Setzt Karte und Legende (die ActiveX Controls).
 * Es werden sogleich die relevatnen Daten ausgelesen: zur Zeit
 * Text und Symbol jeden Layers
 *
 * @param pMoMap : Referenz auf die Karte
 * @param pMoLegend : Referenz auf die Legende, wird nicht benutzt
 *
 */
/* virtual */
void CPrintRectLegend2::SetMap( CMoMap* pMoMap )
{
  CPrintRect::SetMap( pMoMap );

  // erstmal den alten Inhalt löschen
  DeleteContents();

  // jetzt die Daten auslesen
  CMoLayers moLayers( GetMap()->GetLayers() );
  for( short layerID = 0; layerID < moLayers.GetCount(); layerID++ )
  {
    LPDISPATCH objDisp = moLayers.Item( CComVariant( layerID ) );
    if( objDisp )
    {
      COleDispatchDriverEx object( objDisp, FALSE );
      VARIANT visibleVar = object.GetProperty( "Visible" );
      if( visibleVar.vt != VT_ERROR && ( visibleVar.boolVal != 0 ) == true )
      {
        VARIANT nameVar = object.GetProperty( "Name" );
        if( nameVar.vt != VT_BSTR )
          continue;

        CString name( nameVar.bstrVal );
        CMoSymbol symbol;

        switch( long( COleVariantEx( object.GetProperty( "LayerType" ) ) ) )
        {
        case moMapLayer:
          {
            CMoMapLayer mapLayer( objDisp );
            symbol = mapLayer.GetSymbol();
          } // case moMapLayer
          break;

        case moImageLayer:
          break;
        }; // switch

        // wenn wir hier ankommen, haben wir alle infos für einen schönen Legendeneintrag
        LegendEntry entry( name, symbol );
        m_entries.Add( entry );
      }; // if layer.Visible == true
    }; // if objDisp
  }; // for layerID

  // zuletzt nochmal die Bounds neu setzen, um ein ausrechnen der Seitenverhätlnisse zu erzwingen
  SetBounds( GetBounds(), NULL );
}; // Rechteck mit der Karte neu initialisieren



/*!
 * Die eigentliche Zeichenroutine. Wir aber auch von AdjustBounds aufgerufen, um die
 * Grösse der Zeichnung zu bestimmen
 *
 * @param pDC : der DeviceContext, in welchen gezeichnet wird.
 * @param pBounds : Die anfängliche Grösse des Zeichenbereichs. Die Breite wird ggfls. angepast.
 * @param bDraw : falls TRUE, wird tatsächlch gezeichnet, sonst nicht
 *
 */
void CPrintRectLegend2::Draw( CDC* pDC, CRect* pBounds, const BOOL bDraw )
{
  DrawLegend( pDC, pBounds, bDraw );

  // und den Rand zeichnen lassen
  if( bDraw )
    CPrintRect::Paint( pDC );
}; // Draw


/*!
 * Hier wird tatsächlich die Legende gezeichnet.
 *
 * @param pDC : der context in den gemalt wird
 * @param pBounds : die Grösse des Zeichenbereichs, die Breite wird evtl. geändert
 * @param bDraw : falls TRUE, wird echt gezeichnet, asonst nur die Grösse angepasst
 *
 */
void CPrintRectLegend2::DrawLegend( CDC* pDC, CRect* pBounds, const BOOL bDraw )
{
  // die Voraussetzungen prüfen
  if( !pDC || !pBounds || m_entries.GetSize() == 0 )
    return;

  // ein paar konstanten
  static double TEXTGAP = 0.10; // Platz zwischen Text und Kästchenrand in % der Gesamthöhe
  
  ////////////////////////////////////
  // des Device Kontext vorbereiten //
  ////////////////////////////////////

  // Im Gegensatz zur ESRI-Dok, müssen bei CMoMap::OutputMap2 die logischen
  // Koordinaten des Ausgaberechtecks angegeben werden
  // das Funktioniert einfach so schon ganz prima, nur die Linienstärken
  // und Punktgrössen hängen vom aktuellen MappingMode ab
  // -> deswegen müssen wir auf MM_TEXT umschalten, dann wird wieder in Pixeln
  // gerechnet

  // da wir im neuen Mapping mode die logischen Koordinaten brauchen, müssen
  // wir unsere Bounds erstmal in Device Koordinaten umrechnen
  CRect outRect = *pBounds;
  pDC->LPtoDP( outRect );

  // den Context merken
  int saveDC = pDC->SaveDC();

  // jetzt den Mapping Mode ändern
  pDC->SetMapMode( MM_TEXT );

  // die Device Koordinaten wieder zurückrechnen
  pDC->DPtoLP( outRect );
  outRect.right = 0;

  /////////////////////
  // den Font setzen //
  /////////////////////

  // dazu erstmal die Höhe eines Kästchens bestimmen
  int sizeY = outRect.Height() / m_entries.GetSize();
  int textGap = int( sizeY * TEXTGAP );


  // jetzt kann die Punktgrösse des Fonts bestimmt werden
  long lfHeight = sizeY - 2 * textGap;

  // die Höhe des Fonts transformieren
  LOGFONT logFont = GetFontLogfont();
  // jetzt Punkt in Millimeter umrechnen; 1pt = 0.376 mm -> 1/10pt = 0.0376mm
  logFont.lfHeight = lfHeight;
  CFont font;
  if( !font.CreateFontIndirect( &logFont ) )
    return;

  // Font und Farbe setzen
  pDC->SelectObject( &font ); // den neuen Font setzen
  pDC->SetTextColor( GetFontColor() ); // und die Farbe
  pDC->SetBkMode( TRANSPARENT ); // den Hintergrund nicht zeichnen

  // jetzt die Legende zeichnen
  CPoint basePoint( outRect.left + sizeY, outRect.top );

  // der Reihe nach die Layer durchgehen und zeichnen!
  for( int entryID = 0; entryID < m_entries.GetSize(); entryID++ )
  {
    LegendEntry& e = m_entries[entryID];

    CString text = e.text;

    // die benötigte Grösse des Textes bestimmen
    CSize textExtent = pDC->GetTextExtent( text );
    outRect.right = max( outRect.right, basePoint.x + textExtent.cx + 2 * textGap );

    // Position des Textes ermitteln
    CRect textRect( basePoint.x, basePoint.y, outRect.right, basePoint.y + sizeY );
    textRect.DeflateRect( textGap, textGap ); // damit der Text nicht ganz am Rand klebt
        
    if( bDraw )
    {
      pDC->DrawText( text, textRect, DT_SINGLELINE | DT_LEFT );
      
      // jetzt die Markierung zeichnen
      CRect markRect( outRect.left, basePoint.y, basePoint.x, basePoint.y + sizeY );
      markRect.DeflateRect( textGap, textGap );

      CMoSymbol symbol( e.symbol );
      if( LPDISPATCH(symbol) )
      {
        // den Brush erzeugen
        COLORREF brushColor = RGB( 0, 0, 0 );
        COLORREF lineColor = RGB( 255, 255, 255 );
        int lineWidth = 2;
        
        switch( symbol.GetSymbolType() )
        {
        case moPointSymbol:
          lineColor = RGB( 0, 0, 0 );
          brushColor = moColor2Colorref( symbol.GetColor() );
          lineWidth = 1;
          break;
          
        case moLineSymbol:
          lineColor = moColor2Colorref( symbol.GetColor() );
          lineWidth = 3;
          break;
          
        case moFillSymbol:
          lineColor = moColor2Colorref( symbol.GetOutlineColor() );
          brushColor = moColor2Colorref( symbol.GetColor() );
          break;
        }; // switch
        
        CBrush markBrush( brushColor );
        CPen markPen( PS_SOLID, lineWidth, lineColor );
        
        CBrush* oldBrush = pDC->SelectObject( &markBrush );
        CPen* oldPen = pDC->SelectObject( &markPen );
        
        switch( symbol.GetSymbolType() )
        {
        case moPointSymbol:
          markRect.DeflateRect( markRect.Width() / 3, markRect.Height() / 3 );
          pDC->Ellipse( markRect );
          break;
          
        case moLineSymbol:
          {
            markRect.DeflateRect( markRect.Width() / 4, markRect.Height() / 4 );
            int width3 = markRect.Width() / 3;
            int height3 = markRect.Width() / 3;
            pDC->MoveTo( markRect.left, markRect.bottom );
            pDC->LineTo( markRect.left + width3, markRect.top + height3 );
            pDC->LineTo( markRect.left + 2 * width3, markRect.top + 2 * height3 );
            pDC->LineTo( markRect.right, markRect.top );
          }
          break;
          
        case moFillSymbol:
          markRect.DeflateRect( markRect.Width() / 4, markRect.Height() / 4 );
          pDC->FillSolidRect( markRect, brushColor );
          pDC->MoveTo( markRect.left, markRect.top );
          pDC->LineTo( markRect.left, markRect.bottom );
          pDC->LineTo( markRect.right, markRect.bottom );
          pDC->LineTo( markRect.right, markRect.top );
          pDC->LineTo( markRect.left, markRect.top );
          break;
        };
        
        pDC->SelectObject( oldBrush );
        pDC->SelectObject( oldPen );
      }; // if LPDISPATCH(symbol)
    } // if bDraw
    
    basePoint.y += sizeY;
  }; // for boxID

  // jetzt die Grösse des Ausgaberechteckes wieder zurückrechnen
  pDC->LPtoDP( outRect );

  // und den Context wiederherstellen
  pDC->RestoreDC( saveDC );

  pDC->DPtoLP( outRect );
  pBounds->SetRect( outRect.left, outRect.top, outRect.right, outRect.bottom );
} // DrawLegend


/*!
 * Statische Hilfsfunktion, um MapObjects2 Farben in RGB umzuwandeln.
 *
 * @param mo2Color : Farbe, nach MO2 codiert
 *
 * @return COLORREF  : Farbe, nach RGB codiert
 */
/* static */
COLORREF CPrintRectLegend2::moColor2Colorref( long mo2Color )
{
  int red = mo2Color & 0xff;
  int green = ( mo2Color & 0xff00 ) >> 8;
  int blue = ( mo2Color & 0xff0000 ) >> 16;
  return RGB( red, green, blue );
}
