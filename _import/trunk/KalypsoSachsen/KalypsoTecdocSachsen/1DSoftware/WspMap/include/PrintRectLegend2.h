/*! Time-stamp: <@(#)PrintRectLegend2.h   04.12.02 - 10:33:26   Belger>
 *********************************************************************
 *  @file   : PrintRectLegend2.h
 *
 *  Author  : Belger                              Date: 04.12.02
 *
 *  Purpose : Declaration of class CPrintRectLegend2
 *
 *********************************************************************
 */
// PrintRectLegend.h: Schnittstelle für die Klasse CPrintRectLegend.
//
//////////////////////////////////////////////////////////////////////

#ifndef _PRINT_RECT_LEGEND2_INCLUDED_
#define _PRINT_RECT_LEGEND2_INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "PrintRect.h"
#include "IPrintRectFont.h"

/*!
 * @class : CPrintRectLegend2
 *          Ein Druckrechteck zum Anzeigen der Legende.
 *          Dieses Druckrechteck kopiert nicht die CPrintRectLegend einfach
 *          die Legende als Bitmap, sondern erzeugt die Legende selbst. Dazu
 *          liest es die Layer der Reihe nach aus und interpretiert sie (d.h.
 *          totale Kontrolle). Zur Zeit wird allerdings ausschliesslich
 *          'Single Symbol' unterstüzt, d.h. pro Thema wird ein Symbol (Punkt, Linie,
 *          Polygon) und Farbe angezeigt.
 *
 * @usage : Wie alle Druckrechtecke. Siehe CPrintRect
*/
class CPrintRectLegend2 : public CPrintRect, public IPrintRectFont
{
  /*! Ein Legendeneintrag.
   * Hier werden die im ersten Schritt (SetMap) ausgelesenen Daten
   * abgelegt.
   */
  struct LegendEntry
  {
    /// Der angezeigte Text
    CString text;
    /// Das aus dem MapLayer gelesene Symbo. ImageLayer haben Dispatch 0.
    CMoSymbol symbol;

    /// Standardkonstruktor
    LegendEntry( const CString& text = "", const CMoSymbol& symbol = CMoSymbol() ) : text( text ), symbol( symbol ) {};
    /// Copy-Konstruktor
    LegendEntry( const LegendEntry& e ) : text( e.text ), symbol( e.symbol ) {};
  }; // struct Entry

  /// Der Container der Legendeneinträge.
  typedef CArray<LegendEntry, LegendEntry&> Entries;

public:
  /// Der Standardkonstruktor
  CPrintRectLegend2() {};

  virtual ~CPrintRectLegend2();

  /// diese Klasse ist Serialisierbar
  DECLARE_SERIAL( CPrintRectLegend2 );

protected:
  /// Löscht alle Legendeneinträge.
  void DeleteContents();
  
public:
  /// Überschreibung von CPrintRect::Paint
  virtual void Paint( CDC* );

protected:
  /// Überschreibung von CPrintRect::Serialize
  virtual void Serialize( CArchive& ar );

  /// Überschreibung von CPrintRect::CreatePropPage
  virtual CPropertyPage* CreatePropPage( UINT captionID );

  /// Überschreibung von CPrintRect::AdjustBounds
  virtual void AdjustBounds( CDC* pDC, CRect& bounds );

  /// Überschreibung von CPrintRect::SetMap
  virtual void SetMap( CMoMap* pMoMap );

private:
  /// Die eigentliche, interen, Zeichenroutine.
  void Draw( CDC* pDC, CRect* pBounds, BOOL bDraw );

  /// Die Zeichenrotuine für die eigentliche Legende.
  void DrawLegend( CDC* pDC, CRect* pBounds, const BOOL bDraw );

  ///////////////
  // Attribute //
  ///////////////
private:
  /// Die ausgelesenen Legendeneinträge.
  Entries m_entries;

  ////////////
  // Helper //
  ////////////
private:
  /// Hilfsfunktion, um MapObjects2 Farben in RGBs umzuwandeln.
  static COLORREF moColor2Colorref( long mo2Color );
};

#endif // _PRINT_RECT_LEGEND2_INCLUDED_
