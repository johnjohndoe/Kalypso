/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree_impl.model.sort;

import java.util.ListIterator;

/**
 * Ein einfacher ListIterator auf einem Array.
 * 
 * Alle Methoden, die die Liste verändern führen zu einer {@link java.lang.UnsupportedOperationException}.
 * 
 * @author belger
 */
public class ArrayIterator<T> implements ListIterator<T>
{
  private int m_index;

  private final T[] m_objects;

  public ArrayIterator( final int index, final T[] objects )
  {
    m_index = index;
    m_objects = objects;
  }

  /**
   * @see java.util.ListIterator#nextIndex()
   */
  public int nextIndex()
  {
    return m_index;
  }

  /**
   * @see java.util.ListIterator#previousIndex()
   */
  public int previousIndex()
  {
    return m_index - 1;
  }

  /**
   * @see java.util.Iterator#remove()
   */
  public void remove()
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.Iterator#hasNext()
   */
  public boolean hasNext()
  {
    return m_index < m_objects.length;
  }

  /**
   * @see java.util.ListIterator#hasPrevious()
   */
  public boolean hasPrevious()
  {
    return m_index > 0;
  }

  /**
   * @see java.util.Iterator#next()
   */
  public T next()
  {
    return m_objects[m_index++];
  }

  /**
   * @see java.util.ListIterator#previous()
   */
  public T previous()
  {
    m_index--;
    return m_objects[m_index];
  }

  /**
   * @see java.util.ListIterator#add(java.lang.Object)
   */
  public void add( T o )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.ListIterator#set(java.lang.Object)
   */
  public void set( T o )
  {
    throw new UnsupportedOperationException();
  }
}
