/*! Time-stamp: <@(#)BoxPager.cpp   02.12.02 - 17:53:43   Belger>
 *********************************************************************
 *  @file   : BoxPager.cpp
 *
 *  Author  : Belger                              Date: 02.12.02
 *
 *  Purpose : Implementation of methods for class CBoxPager.
 *
 *********************************************************************
 */

#include "stdafx.h"

#include "BoxPager.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif


/*!
 * Setzt die Seitengrösse nachträglich.
 * Alle Boxen werden neu angeordnet.
 *
 * @param size : Die neue Seitengrösse, kein Check auf 0.
 *
 */
void CBoxPager::SetPageSize( const CSize& size )
{
  // die Seitengrösse neu setzen
  m_pageSize = size;

  // alles neu Arrangieren
  Arrange();
}; // SetPagesize


/*!
 * Gibt die Anzahl der bisher benötigten Seiten zurück, d.h. die Grösste Seitennummer + 1.
 *
 * @return UINT  : Die Anzahl der benötigten Seiten.
 */
UINT CBoxPager::GetPageCount() const
{
  UINT pageCount = 0;
  for( int boxID = 0; boxID < (int)m_boxes.GetSize(); boxID++ )
    pageCount = max( pageCount, m_boxes[boxID].page + 1 );

  return pageCount;
}; // GetPageCount

/*!
 * Fügt eine neue Box zu den Seiten hinzu.
 * Danach werden alle Boxen neu angeordent.
 *
 * @param size : Die Grösse der neuen Box. Kein Check auf 0.
 *
 * @return UINT  : Der interne Index der Neuen Box, wird für zugriffe auf
 *                  die Boxen benötigt. Liegt immer zwischen 0 und GetBoxCount()
 */
UINT CBoxPager::AddBox( const CSize& size )
{
  // die Box auf der letzten Seite hinzufügen
  UINT id = m_boxes.Add( Box( size ) );

  // alles neu Arrangieren
  Arrange();

  return id;
}; // AddBox

void CBoxPager::Arrange()
{
  switch( m_arrangeType )
  {
  case single:
    ArrangeSingle();
    break;

  case horizontal:
    ArrangeHorizontal();
    break;

  default:
    break;
  }; // switch m_arrangeType
} // Arrange


void CBoxPager::ArrangeSingle()
{
  for( int boxID = 0; boxID < m_boxes.GetSize(); boxID++ )
  {
    m_boxes[boxID].point = CPoint( 0, 0 );

    if( m_boxes[boxID].size.cx > m_pageSize.cx || m_boxes[boxID].size.cy > m_pageSize.cy )
      m_boxes[boxID].page = -1; // box passt nicht ganz auf eine Seite
    else
      m_boxes[boxID].page = boxID;

  };
}; // ArrangeSingle

void CBoxPager::ArrangeHorizontal()
{
  int pageCount = 0; // Anzahl der bisher benutzten Seiten
  int pageHeight = m_pageSize.cy - m_gaps.cy; // verbleibende Höhe der Seite
  int rowWidth = m_pageSize.cx - m_gaps.cx;   // verbleibende Breite der aktuellen Zeile
  int rowHeight = 0; // bisher maximale Zeilenhöhe
  CArray<int, int> boxRow; // die Boxen der aktuellen Zeile

  UINT boxCount = GetBoxCount();
  for( UINT boxID = 0; boxID < boxCount + 1; boxID++ )
  {
    // die Box der aktuellen Zeile hinzufügen
    int boxX = boxID < boxCount ? m_boxes[boxID].size.cx + m_gaps.cx : rowWidth * 2;  // Breite einer Box
    int boxY = boxID < boxCount ? m_boxes[boxID].size.cy + m_gaps.cy : 0;  // Höhe einer Box

    // falls die box nicht mehr in die Zeile passt, eine neue Zeile anlegen
    if( boxX > rowWidth || boxY > pageHeight )
    {
      // die Boxen justieren
      CPoint basePoint = CPoint( m_gaps.cx, pageHeight - rowHeight + m_gaps.cy );
      for( int i = 0; i < boxRow.GetSize(); i++ )
      {
        m_boxes[boxRow[i]].page = pageCount;
        m_boxes[boxRow[i]].point = basePoint;

        basePoint.x += m_boxes[boxRow[i]].size.cx + m_gaps.cx;
      };
      boxRow.RemoveAll(); // die aktuelle Zeile löschen

      // die Parameter aktualisieren
      pageHeight -= rowHeight;
      rowHeight = 0;
      rowWidth = m_pageSize.cx - m_gaps.cx;
    }

    if( boxID == boxCount )
      continue;

    // falls die Box nicht mehr auf die Seite passt, eine neue Seite anlegen
    if( boxY > pageHeight )
    {
      pageCount++;
      pageHeight = m_pageSize.cy - m_gaps.cy;
    };

    // jetzt sollte die Box aber passen
    if( boxX < rowWidth && boxY < pageHeight )
    {
      boxRow.Add( boxID ); // falls es noch past, einfach hinzufügen
      rowHeight = max( rowHeight, boxY );
      rowWidth -= boxX;
    }
    else
      m_boxes[boxID].page = -1; // sonst als zu gross markieren
  }; // for boxID
}; // ReArrange


/*!
 * Gibt zrück, ob das angegebene Rechteck auf ein bestimmtes Blatt passt
 *
 * @param page : Die Seitennummer
 * @param rect : Dieses Rechteck soll passen
 *
 * @return bool  : true, wenns passt, sonst false
 */
bool CBoxPager::FitsRect( const CRect& rect ) const
{
  for( UINT boxID = 0; boxID < GetBoxCount(); boxID++ )
  {
    int boxPage = GetBoxPage( boxID );
    if(  boxPage != -1 )
    {
      CPoint boxPoint = GetBoxPoint( boxID );
      CSize boxSize = GetBoxSize( boxID );
      
      CRect boxRect( boxPoint.x + m_pageSize.cx * boxPage, boxPoint.y + boxSize.cy, boxPoint.x + m_pageSize.cx * boxPage + boxSize.cx, boxPoint.y );
      boxRect.NormalizeRect();
      CRect iRect( rect );
      iRect.NormalizeRect();

      if( iRect.IntersectRect( boxRect, iRect ) )
        return false;
    } // if page
  }; // for boxID
  
  return true;
}; // CBoxPager