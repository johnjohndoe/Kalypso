/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 * 
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 * 
 *  ---------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.map;

import java.util.LinkedList;

import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * The history of the extents of the map panel. This is simply a stack of bounding boxes, with the additional feature of
 * remembering the currently shown bbox (for navigating in the history).
 * 
 * @author Gernot Belger
 */
public class ExtentHistory
{
  private final LinkedList<GM_Envelope> m_bboxHistory = new LinkedList<GM_Envelope>();

  private final int m_capacity;

  private int m_position = -1;

  /**
   * @param capacity
   *          Number of elements supported by this history.
   */
  public ExtentHistory( final int capacity )
  {
    m_capacity = capacity;
  }

  /**
   * Add a new extent to the history. <br>
   * If the maximum number of extents is reached, the oldest entry will be lost.<br>
   * Going forward in the history after adding a new entry is not possible any more.<b> Default visible, as only the map
   * panel shall add new extents here.
   */
  /* default */void push( final GM_Envelope extent )
  {
    if( m_bboxHistory.size() >= m_capacity )
      m_bboxHistory.remove();

    m_bboxHistory.add( extent );
    m_position = m_bboxHistory.size() - 1;
  }

  /**
   * @return <code>true</code>, if the history contains a previous extent, <code>false</code> otherwise.
   * @see #getPrevious()
   */
  public boolean hasPrevious( )
  {
    return m_position > 0;
  }

  /**
   * @return <code>true</code>, if the history contains a next extent, <code>false</code> otherwise.
   * @see #getNext()
   */
  public boolean hasNext( )
  {
    return m_position < m_bboxHistory.size() - 1;
  }

  /**
   * Gets the previous extents from the current position within the history.
   * 
   * @return <code>null</code>, if the beginning of the stack is reached.
   * @see #hasPrevious()
   */
  public GM_Envelope getPrevious( )
  {
    if( m_position > 0 )
      return m_bboxHistory.get( --m_position );

    return null;
  }

  /**
   * Gets the next extents from the current position within the history.
   * 
   * @return <code>null</code>, if the beginning of the stack is reached.
   * @see #hasNext()
   */
  public GM_Envelope getNext( )
  {
    if( m_position < m_bboxHistory.size() - 1 )
      return m_bboxHistory.get( ++m_position );

    return null;
  }

}
