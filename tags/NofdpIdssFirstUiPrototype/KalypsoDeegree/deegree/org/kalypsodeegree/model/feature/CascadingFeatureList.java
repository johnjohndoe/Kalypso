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
package org.kalypsodeegree.model.feature;

import java.awt.Graphics;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;

/**
 * this featurelist cascades serveral lists, so it is possible to merge other lists without resorting or copying
 * listcontents <br>
 * this featurelist is <b>readonly </b>
 * 
 * @author doemming
 */
public class CascadingFeatureList implements FeatureList
{
  private final FeatureList[] m_lists;

  public CascadingFeatureList( final FeatureList[] lists )
  {
    m_lists = lists;
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureList#toFeatures()
   */
  public Feature[] toFeatures( )
  {
    List result = new ArrayList();
    for( int i = 0; i < m_lists.length; i++ )
    {
      result.addAll( Arrays.asList( m_lists[i].toFeatures() ) );
    }
    return (Feature[]) result.toArray( new Feature[result.size()] );
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureList#accept(org.kalypsodeegree.model.feature.FeatureVisitor)
   */
  public void accept( FeatureVisitor visitor )
  {
    // TODO
    // for( int i = 0; i < m_lists.length; i++ )
    // {
    // m_lists[i];
    // }
  }

  /**
   * @see java.util.Collection#size()
   */
  public int size( )
  {
    int result = 0;
    for( int i = 0; i < m_lists.length; i++ )
      result += m_lists[i].size();
    return result;
  }

  /**
   * @see java.util.Collection#clear()
   */
  public void clear( )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.Collection#isEmpty()
   */
  public boolean isEmpty( )
  {
    return size() == 0;
  }

  /**
   * @see java.util.Collection#toArray()
   */
  public Object[] toArray( )
  {
    return toFeatures();
  }

  /**
   * @see java.util.List#get(int)
   */
  public Object get( int index )
  {
    int c = 0;
    for( int i = 0; i < m_lists.length; i++ )
    {
      int size = m_lists[i].size();
      if( c + size < index )
        c += size;
      else
        return m_lists[i].get( index - c );
    }
    return null;
  }

  /**
   * @see java.util.List#remove(int)
   */
  public Object remove( int index )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.List#add(int, java.lang.Object)
   */
  public void add( int index, Object element )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.List#indexOf(java.lang.Object)
   */
  public int indexOf( Object o )
  {
    int c = 0;
    for( int i = 0; i < m_lists.length; i++ )
    {
      int index = m_lists[i].indexOf( o );
      if( index >= 0 )
        return index + c;
      c += m_lists[i].size();
    }
    return -1;
  }

  /**
   * @see java.util.List#lastIndexOf(java.lang.Object)
   */
  public int lastIndexOf( Object o )
  {
    int result = -1;
    int c = 0;
    for( int i = 0; i < m_lists.length; i++ )
    {
      int index = m_lists[i].indexOf( o );
      if( index >= 0 )
        result = index + c;
      c += m_lists[i].size();
    }
    return result;
  }

  /**
   * @see java.util.Collection#add(java.lang.Object)
   */
  public boolean add( Object o )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.Collection#contains(java.lang.Object)
   */
  public boolean contains( Object o )
  {
    for( int i = 0; i < m_lists.length; i++ )
    {
      if( m_lists[i].contains( o ) )
        return true;
    }
    return false;
  }

  /**
   * @see java.util.Collection#remove(java.lang.Object)
   */
  public boolean remove( Object o )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.List#addAll(int, java.util.Collection)
   */
  public boolean addAll( int index, Collection c )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.Collection#addAll(java.util.Collection)
   */
  public boolean addAll( Collection c )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.Collection#containsAll(java.util.Collection)
   */
  public boolean containsAll( final Collection c )
  {
    Collection left = c;// new ArrayList();
    for( int i = 0; i < m_lists.length; i++ )
    {
      final Collection stillLeft = new ArrayList();
      for( Iterator iter = left.iterator(); iter.hasNext(); )
      {
        final Object object = iter.next();
        if( !m_lists[i].contains( object ) )
          stillLeft.add( object );
      }
      if( stillLeft.isEmpty() )
        return true;
      left = stillLeft;
    }
    return false;
  }

  /**
   * @see java.util.Collection#removeAll(java.util.Collection)
   */
  public boolean removeAll( Collection c )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.Collection#retainAll(java.util.Collection)
   */
  public boolean retainAll( Collection c )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.Collection#iterator()
   */
  public Iterator iterator( )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.List#subList(int, int)
   */
  public List subList( int fromIndex, int toIndex )
  {
    // could be implemented
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.List#listIterator()
   */
  public ListIterator listIterator( )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.List#listIterator(int)
   */
  public ListIterator listIterator( int index )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.List#set(int, java.lang.Object)
   */
  public Object set( int index, Object element )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.Collection#toArray(java.lang.Object[])
   */
  public Object[] toArray( Object[] a )
  {
    try
    {
      Object[] objects = toArray();
      if( objects.length != a.length )
      {
        throw new ArrayStoreException( "wrong length" );
      }
      for( int i = 0; i < objects.length; i++ )
        a[i] = objects[i];
    }
    catch( Exception e )
    {
      throw new ArrayStoreException( e.getMessage() );
    }
    return a;
  }

  /**
   * @see org.kalypsodeegree.model.sort.JMSpatialIndex#query(org.kalypsodeegree.model.geometry.GM_Envelope,
   *      java.util.List)
   */
  public List query( GM_Envelope env, List result )
  {
    if( result == null )
      result = new ArrayList();
    for( int i = 0; i < m_lists.length; i++ )
      result = m_lists[i].query( env, result );
    return result;
  }

  /**
   * @see org.kalypsodeegree.model.sort.JMSpatialIndex#query(org.kalypsodeegree.model.geometry.GM_Position,
   *      java.util.List)
   */
  public List query( GM_Position env, List result )
  {
    if( result == null )
      result = new ArrayList();
    if( result == null )
      result = new ArrayList();
    for( int i = 0; i < m_lists.length; i++ )
    {
      result = m_lists[i].query( env, result );
    }
    return result;
  }

  /**
   * @see org.kalypsodeegree.model.sort.JMSpatialIndex#queryAll(java.util.List)
   */
  public List queryAll( List result )
  {
    if( result == null )
      result = new ArrayList();
    for( int i = 0; i < m_lists.length; i++ )
      result = m_lists[i].queryAll( result );
    return result;
  }

  /**
   * @see org.kalypsodeegree.model.sort.JMSpatialIndex#paint(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform)
   */
  public void paint( Graphics g, GeoTransform geoTransform )
  {
    for( int i = 0; i < m_lists.length; i++ )
    {
      m_lists[i].paint( g, geoTransform );
    }
  }

  /**
   * @see org.kalypsodeegree.model.sort.JMSpatialIndex#rsize()
   */
  public int rsize( )
  {
    int result = 0;
    for( int i = 0; i < m_lists.length; i++ )
    {
      int rsize = m_lists[i].rsize();
      if( rsize > result )
        result = rsize;
    }
    return result;
  }

  /**
   * @see org.kalypsodeegree.model.sort.JMSpatialIndex#getBoundingBox()
   */
  public GM_Envelope getBoundingBox( )
  {
    GM_Envelope result = null;
    for( int i = 0; i < m_lists.length; i++ )
    {
      GM_Envelope boundingBox = m_lists[i].getBoundingBox();
      if( result == null )
        result = boundingBox;
      else
        result = result.getMerged( boundingBox );
    }
    return result;
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureList#getParentFeature()
   * @return null, as this are mixed lists
   */
  public Feature getParentFeature( )
  {
    return null;
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureList#getParentFeatureTypeProperty()
   * @return null, as this are mixed lists
   */
  public IRelationType getParentFeatureTypeProperty( )
  {
    return null;
  }

  /**
   * @see org.kalypsodeegree.model.sort.JMSpatialIndex#invalidate()
   */
  public void invalidate( )
  {
    for( final FeatureList list : m_lists )
      list.invalidate();
  }

  /**
   * @see org.kalypsodeegree.model.sort.JMSpatialIndex#invalidate(java.lang.Object)
   */
  public void invalidate( final Object o )
  {
    for( final FeatureList list : m_lists )
      list.invalidate( o );
  }
}