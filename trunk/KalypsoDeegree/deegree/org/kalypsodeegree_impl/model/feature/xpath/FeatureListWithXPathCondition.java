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
package org.kalypsodeegree_impl.model.feature.xpath;

import java.awt.Graphics;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;

/**
 * @author doemming
 */
public class FeatureListWithXPathCondition implements FeatureList
{

  private final FeatureList m_feList;

  private final IXElement m_conditionXElement;

  private final GMLWorkspace m_contextWS;

  public FeatureListWithXPathCondition( final GMLWorkspace contextWS, final FeatureList feList, IXElement conditionXElement )
  {
    m_contextWS = contextWS;
    m_feList = feList;
    m_conditionXElement = conditionXElement;
  }

  boolean fitXPathCondition( Object feature )
  {
    if( !(feature instanceof Feature) )
      return false;
    try
    {
      return (Boolean) m_conditionXElement.evaluate( m_contextWS, (Feature) feature );
    }
    catch( FeaturePathException e )
    {
      return false;
    }
  }

  public void accept( FeatureVisitor visitor )
  {
    final FeatureVisitor wrapperVisitor = new XPathConditionFeatureVisitor( m_contextWS, m_conditionXElement, visitor );
    m_feList.accept( wrapperVisitor );
  }

  public Iterator iterator( )
  {
    final Iterator orgIter = m_feList.iterator();
    return new Iterator()
    {
      private Object m_next = null;

      {
        // initial call
        setNext();
      }

      public void remove( )
      {
        throw new UnsupportedOperationException();
      }

      public Object next( )
      {
        final Object result = m_next;
        setNext();
        return result;
      }

      public boolean hasNext( )
      {
        return m_next != null;
      }

      private void setNext( )
      {
        while( orgIter.hasNext() )
        {
          final Object next = orgIter.next();
          if( fitXPathCondition( next ) )
          {
            m_next = next;
            return;
          }
        }
        m_next = null;
      }
    };
  }

  public void add( int index, Object element )
  {
    m_feList.add( index, element );
  }

  public boolean add( Object o )
  {
    return m_feList.add( o );
  }

  public boolean addAll( Collection c )
  {
    return m_feList.addAll( c );
  }

  public boolean addAll( int index, Collection c )
  {
    return m_feList.addAll( index, c );
  }

  public void clear( )
  {
    m_feList.clear();
  }

  public boolean contains( Object o )
  {
    boolean contains = m_feList.contains( o );
    if( !contains )
      return false;
    return fitXPathCondition( o );
  }

  public boolean containsAll( Collection c )
  {
    // check contains
    boolean containsAll = m_feList.containsAll( c );
    if( !containsAll )
      return false;
    // check fits
    final Iterator iterator = c.iterator();
    while( iterator.hasNext() )
    {
      boolean b = fitXPathCondition( iterator.next() );
      if( !b )
        return false;
    }
    return true;
  }

  public Object get( int index )
  {
    int c = 0;
    final Iterator iterator = iterator();
    while( iterator.hasNext() )
    {
      final Object object = iterator.next();
      if( index == c )
        return object;
      c++;
    }
    return null;
  }

  public int indexOf( Object o )
  {
    int index = 0;
    final Iterator iterator = iterator();
    while( iterator.hasNext() )
    {
      final Object object = iterator.next();
      if( o == object )
        return index;
      index++;
    }
    return -1;
  }

  public GM_Envelope getBoundingBox( )
  {
    return m_feList.getBoundingBox();
  }

  public Feature getParentFeature( )
  {
    return m_feList.getParentFeature();
  }

  public IRelationType getParentFeatureTypeProperty( )
  {
    return m_feList.getParentFeatureTypeProperty();
  }

  public boolean isEmpty( )
  {
    final Iterator iterator = iterator();
    return !iterator.hasNext();
  }

  public int lastIndexOf( Object o )
  {
    final Iterator iterator = iterator();
    int result = -1;
    int c = 0;
    while( iterator.hasNext() )
    {
      Object object = iterator.next();
      if( object == o )
        result = c;
      c++;
    }
    return result;
  }

  public ListIterator listIterator( )
  {
    throw new UnsupportedOperationException();
  }

  public ListIterator listIterator( int index )
  {
    throw new UnsupportedOperationException();
  }

  public void paint( Graphics g, GeoTransform geoTransform )
  {
    // this paints only the boxes of the sorting for debug, not wrapped
    m_feList.paint( g, geoTransform );
  }

  public List query( GM_Envelope env, List result )
  {
    if( result == null )
      result = new ArrayList();
    final List list = m_feList.query( env, null );
    final Iterator iterator = list.iterator();
    while( iterator.hasNext() )
    {
      final Object object = iterator.next();
      if( fitXPathCondition( object ) )
        result.add( object );
    }
    return result;
  }

  public List query( GM_Position env, List result )
  {
    if( result == null )
      result = new ArrayList();
    final List list = m_feList.query( env, null );
    final Iterator iterator = list.iterator();
    while( iterator.hasNext() )
    {
      final Object object = iterator.next();
      if( fitXPathCondition( object ) )
        result.add( object );
    }
    return result;
  }

  public List queryAll( List result )
  {
    if( result == null )
      result = new ArrayList();
    final Iterator iterator = iterator();
    while( iterator.hasNext() )
      result.add( iterator.next() );
    return result;
  }

  public Object remove( int index )
  {
    final Object object = get( index );
    return m_feList.remove( object );
  }

  public boolean remove( Object o )
  {
    return m_feList.remove( o );
  }

  public boolean removeAll( Collection c )
  {
    return m_feList.removeAll( c );
  }

  public void resort( )
  {
    m_feList.resort();
  }

  public boolean retainAll( Collection c )
  {
    return m_feList.retainAll( c );
  }

  public int rsize( )
  {
    return m_feList.rsize();
  }

  public Object set( int index, Object element )
  {
    final Object object = get( index );
    final int orgIndex = m_feList.indexOf( object );
    return m_feList.set( orgIndex, element );
  }

  public int size( )
  {
    final Iterator iterator = iterator();
    int result = 0;
    while( iterator.hasNext() )
    {
      result++;
      iterator.next();
    }
    return result;
  }

  public List subList( int fromIndex, int toIndex )
  {
    throw new UnsupportedOperationException();
  }

  public Object[] toArray( )
  {
    final List result = queryAll( null );
    return result.toArray();
  }

  public Object[] toArray( Object[] a )
  {
    final List result = queryAll( null );
    return result.toArray( a );
  }

  public Feature[] toFeatures( )
  {
    final List result = queryAll( null );
    return (Feature[]) result.toArray( new Feature[result.size()] );
  }
}
