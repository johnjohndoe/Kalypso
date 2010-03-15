/*! Time-stamp: <@(#)BoxPager.h   02.12.02 - 16:21:00   Belger>
 *********************************************************************
 *  @file   : BoxPager.h
 *
 *  Author  : Belger                              Date: 02.12.02
 *
 *  Purpose : Diese Klasse dient dazu, eine Reihe von Boxen (=Rechtecken)
 *            auf einer oder mehreren Seiten anzuordnen.
 *            Der Nutzer hat (potentiell) die Auswahl unter verschiedenen
 *            Anordnungsalgorythmen.
 *
 *********************************************************************
 */

#if !defined(AFX_BOXPAGER_H__71F79120_03AC_11D7_B35E_00104BB3E525__INCLUDED_)
#define AFX_BOXPAGER_H__71F79120_03AC_11D7_B35E_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

/*!
 * @class : CBoxPager
*/
class CBoxPager  
{
public:
  /// Diese Boxen werden auf den Seiten angeordnet
  struct Box
  {
    CSize size;
    CPoint point;
    UINT page;

    Box( const CSize& size = CSize( 0, 0 ), const CPoint& point = CPoint( 0, 0 ), const UINT page = 0 ) : size( size ), point( point ), page( page ) {};
  };

  /// Hier werden alle Boxen zwischengespeichert
  typedef CArray<Box, Box&> BoxArray;

  /*!
   *  Die verschiedenen Typen, nach denen die Boxen auf
   *  den Seiten angeordnet werden.
  */
  enum ArrangeType
  {
    single,
      horizontal
  };

  /*!
  * Der Standardkonstruktor
  *
  * @param type : Die Anordnungsmethode
  * @param pageSize : Die Grösse einer Seite
  * @param gaps : Die einzuhaltenden Abstände zwischen den Boxen in x und y Richtung.
  *
  */
  CBoxPager( const ArrangeType& type, const CSize& pageSize, const CSize& gaps ) : 
    m_arrangeType( type ), m_pageSize( pageSize ), m_gaps( gaps ) {};

  /// Ändert die Seitengrösse nachträglich
  void SetPageSize( const CSize& size );

  /// Gibt die Anzahl der bisher benötigten Seiten zurück
  UINT GetPageCount() const;

  /// Fügt eine Box zu den Seiten hinzu, diese wird automatisch angeordnet
  UINT AddBox( const CSize& size );

  /*!
  * Gibt die Anzahl der Boxen zurück.
  *
  * @return UINT  : Die Anzahl der Boxen.
  */
  UINT GetBoxCount() const { return m_boxes.GetSize(); };

  /*!
  * Gibt die Grösse einer bestimmten Box zurück, ist immer gleich der 
  * Grösse der Übergebenen Box.
  *
  * @param index : Die Nummer der abzufragenden Box.
  *
  * @return CSize&  : Die Grösse der Box mit Nummer index.
  */
  CSize GetBoxSize( const UINT index ) const { return m_boxes[index].size; };

  /*!
  * Gibt den Basispunkt (rechts/unten) der angeordneten Box zurück.
  * D.h. hierin hat der Algorythmus die Box gestellt.
  *
  * @param index : Die Nummer der abzufragenden Box.
  *
  * @return CPoint&  : Die Position der Box mit Nummer index.
  */
  CPoint GetBoxPoint( const UINT index ) const { return m_boxes[index].point; };

  /*!
  * Gibt die Seitennummer der Box zurück, wohin sie angeordnet wurde.
  *
  * @param index : Die Nummer der abzufragenden Box.
  *
  * @return int : Die Seitennummer der Box mit Nummer index. -1, falls die Box nicht 
  *                angeordnet werden konnte
  */
  int GetBoxPage( const UINT index ) const { return m_boxes[index].page; };

  /// Testet, ob ein bestimmtes Rechteck auf eine angegebene Seite passt
  bool FitsRect( const CRect& rect ) const;

private:
  /// Mit diesem Typ wird angeordnet.
  ArrangeType m_arrangeType;

  /// Die Grösse einer Seite
  CSize m_pageSize; // Grösse einer Seite

  /// Die Abstände zwischen den Boxen in x respektive y Richtung.
  CSize m_gaps;     // Abstände zwischen den Boxen

  /// Alle meine Boxen.
  BoxArray m_boxes; // die Boxen

  /// Ordnet alle Boxen neu an.
  void Arrange();       // arrangiert die Boxen auf den Seiten

  /// Ordnet die Boxen nach System 'single' an.
  void ArrangeSingle(); // jede Box auf eine Seite

  /// Ordnet die Boxen nach System 'horizontal' an.
  void ArrangeHorizontal(); // Horizontal ausrichten
};

#endif // !defined(AFX_BOXPAGER_H__71F79120_03AC_11D7_B35E_00104BB3E525__INCLUDED_)
