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
package org.kalypso.model.wspm.sobek.result.processing.model.implementation;

import java.awt.Graphics;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

import org.eclipse.core.runtime.Assert;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;

/**
 * @author kuch
 */
public abstract class AbstractListWrapper implements FeatureList
{

  private final FeatureList m_list;

  public AbstractListWrapper( final FeatureList list )
  {
    Assert.isNotNull( list );
    m_list = list;
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureList#accept(org.kalypsodeegree.model.feature.FeatureVisitor)
   */
  public void accept( final FeatureVisitor visitor )
  {
    m_list.accept( visitor );
  }

  /**
   * @see java.util.List#add(int, java.lang.Object)
   */
  public void add( final int index, final Object element )
  {
    m_list.add( index, element );
  }

  /**
   * @see java.util.List#add(java.lang.Object)
   */
  public boolean add( final Object o )
  {
    return m_list.add( o );
  }

  /**
   * @see java.util.List#addAll(java.util.Collection)
   */
  public boolean addAll( final Collection c )
  {
    return m_list.addAll( c );
  }

  /**
   * @see java.util.List#addAll(int, java.util.Collection)
   */
  public boolean addAll( final int index, final Collection c )
  {
    return m_list.addAll( index, c );
  }

  /**
   * @see java.util.List#clear()
   */
  public void clear( )
  {
    m_list.clear();
  }

  /**
   * @see java.util.List#contains(java.lang.Object)
   */
  public boolean contains( final Object o )
  {
    return m_list.contains( o );
  }

  /**
   * @see java.util.List#containsAll(java.util.Collection)
   */
  public boolean containsAll( final Collection c )
  {
    return m_list.containsAll( c );
  }

  // $ANALYSIS-IGNORE
  /**
   * @see org.kalypsodeegree.model.feature.FeatureList#first()
   */
  public Object first( )
  {
    return m_list.first();
  }

  // $ANALYSIS-IGNORE
  /**
   * @see java.util.List#get(int)
   */
  public Object get( final int index )
  {
    return m_list.get( index );
  }

  /**
   * @see org.kalypsodeegree.model.sort.JMSpatialIndex#getBoundingBox()
   */
  public GM_Envelope getBoundingBox( )
  {
    return m_list.getBoundingBox();
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureList#getParentFeature()
   */
  public Feature getParentFeature( )
  {
    return m_list.getParentFeature();
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureList#getParentFeatureTypeProperty()
   */
  public IRelationType getParentFeatureTypeProperty( )
  {
    return m_list.getParentFeatureTypeProperty();
  }

  /**
   * @see java.util.List#indexOf(java.lang.Object)
   */
  public int indexOf( final Object o )
  {
    return m_list.indexOf( o );
  }

  /**
   * @see org.kalypsodeegree.model.sort.JMSpatialIndex#invalidate()
   */
  public void invalidate( )
  {
    m_list.invalidate();
  }

  /**
   * @see org.kalypsodeegree.model.sort.JMSpatialIndex#invalidate(java.lang.Object)
   */
  public void invalidate( final Object o )
  {
    m_list.invalidate( o );
  }

  /**
   * @see java.util.List#isEmpty()
   */
  public boolean isEmpty( )
  {
    return m_list.isEmpty();
  }

  /**
   * @see java.util.List#iterator()
   */
  public Iterator iterator( )
  {
    return m_list.iterator();
  }

  /**
   * @see java.util.List#lastIndexOf(java.lang.Object)
   */
  public int lastIndexOf( final Object o )
  {
    return m_list.lastIndexOf( o );
  }

  /**
   * @see java.util.List#listIterator()
   */
  public ListIterator listIterator( )
  {
    return m_list.listIterator();
  }

  /**
   * @see java.util.List#listIterator(int)
   */
  public ListIterator listIterator( final int index )
  {
    return m_list.listIterator( index );
  }

  /**
   * @see org.kalypsodeegree.model.sort.JMSpatialIndex#paint(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform)
   */
  public void paint( final Graphics g, final GeoTransform geoTransform )
  {
    m_list.paint( g, geoTransform );
  }

  /**
   * @see org.kalypsodeegree.model.sort.JMSpatialIndex#query(org.kalypsodeegree.model.geometry.GM_Envelope,
   *      java.util.List)
   */
  public List query( final GM_Envelope env, final List result )
  {
    return m_list.query( env, result );
  }

  /**
   * @see org.kalypsodeegree.model.sort.JMSpatialIndex#query(org.kalypsodeegree.model.geometry.GM_Position,
   *      java.util.List)
   */
  public List query( final GM_Position env, final List result )
  {
    return m_list.query( env, result );
  }

  // $ANALYSIS-IGNORE
  /**
   * @see java.util.List#remove(int)
   */
  public Object remove( final int index )
  {
    return m_list.remove( index );
  }

  /**
   * @see java.util.List#remove(java.lang.Object)
   */
  public boolean remove( final Object o )
  {
    return m_list.remove( o );
  }

  /**
   * @see java.util.List#removeAll(java.util.Collection)
   */
  public boolean removeAll( final Collection c )
  {
    return m_list.removeAll( c );
  }

  /**
   * @see java.util.List#retainAll(java.util.Collection)
   */
  public boolean retainAll( final Collection c )
  {
    return m_list.retainAll( c );
  }

  // $ANALYSIS-IGNORE
  /**
   * @see java.util.List#set(int, java.lang.Object)
   */
  public Object set( final int index, final Object element )
  {
    return m_list.set( index, element );
  }

  /**
   * @see java.util.List#size()
   */
  public int size( )
  {
    return m_list.size();
  }

  /**
   * @see java.util.List#subList(int, int)
   */
  public List subList( final int fromIndex, final int toIndex )
  {
    return m_list.subList( fromIndex, toIndex );
  }

  /**
   * @see java.util.List#toArray()
   */
  public Object[] toArray( )
  {
    return m_list.toArray();
  }

  /**
   * @see java.util.List#toArray(T[])
   */
  public Object[] toArray( final Object[] a )
  {
    return m_list.toArray( a );
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureList#toFeatures()
   */
  public Feature[] toFeatures( )
  {
    return m_list.toFeatures();
  }

}
