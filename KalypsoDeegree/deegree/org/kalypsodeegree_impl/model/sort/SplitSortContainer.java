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

import java.awt.Graphics;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.kalypsodeegree.graphics.transformation.GeoTransform;

import com.vividsolutions.jts.geom.Envelope;

public class SplitSortContainer
{
  public static interface Visitor
  {
    void visit( final Object item, final Envelope env );
  }

  /**
   * A visitor implementation that calculates the combined bounding box of all elements of this container.
   */
  public static class BboxVisitor implements Visitor
  {
    @SuppressWarnings("hiding")
    private Envelope m_envelope = null;

    public Envelope getEnvelope( )
    {
      return m_envelope;
    }

    /**
     * @see org.kalypsodeegree_impl.model.sort.SplitSortContainer.Visitor#visit(java.lang.Object,
     *      com.vividsolutions.jts.geom.Envelope)
     */
    public void visit( final Object item, final Envelope env )
    {
      if( env != null )
      {
        if( m_envelope == null )
          m_envelope = new Envelope( env );
        else
          m_envelope.expandToInclude( env );
      }
    }
  }

  private static final int MAX_OBJECTS = 200;

  private static final int LEFT_BOTTOM = 0;

  private static final int RIGHT_BOTTOM = 1;

  private static final int RIGHT_TOP = 2;

  private static final int LEFT_TOP = 3;

  private final SplitSortContainer[] m_subContainer = new SplitSortContainer[4];

  private Envelope m_envelope;

  private Map<Object, Envelope> m_objects = new LinkedHashMap<Object, Envelope>();

  private SplitSortContainer m_parent;

  public SplitSortContainer( final SplitSortContainer parent, final Envelope env )
  {
    m_envelope = env;
    m_parent = parent;
  }

  public boolean containsEnvelope( final Envelope e )
  {
    if( m_envelope == null )
      return false;

    return m_envelope.contains( e );
  }

  public void setParent( final SplitSortContainer container )
  {
    m_parent = container;
  }

  public int size( )
  {
    return m_objects.size();
  }

  private void resort( )
  {
    final Map<Object, Envelope> oldObjects = m_objects;
    m_objects = new LinkedHashMap<Object, Envelope>( oldObjects.size() );

    for( final Entry<Object, Envelope> entry : oldObjects.entrySet() )
    {
      final Object object = entry.getKey();
      final Envelope env = entry.getValue();
      add( object, env );
    }
  }

  public void add( final Object object, final Envelope env )
  {
    if( env == null )
    {
      m_objects.put( object, env );
      return;
    }
    else if( m_envelope == null )
    {
      // Prohibits, that an root container ever has an null-envelope while containing elements with envelope
      m_envelope = env;
    }

    // Add to sub container, if it belongs there
    if( hasSubContainers() )
    {
      for( final SplitSortContainer subContainer : m_subContainer )
      {
        if( subContainer.containsEnvelope( env ) )
        {
          subContainer.add( object, env );
          return;
        }
      }

      // If it does not fit into the sub-containers, always add to me: ??? this object could get very full indeed ???
      // Maybe re-balance this container if it gets fuller than MAX_OBJECTS * 2 ?
      m_objects.put( object, env );
      return;
    }

    // No sub containers yet present

    if( m_objects.size() < MAX_OBJECTS )
    {
      // as long this container is not too full, add into own list
      m_objects.put( object, env );
      return;
    }

    createSubContainers();
    resort();
    add( object, env );
  }

  public void createSubContainers( final SplitSortContainer container )
  {
    double midX = 0d;
    double midY = 0d;
    boolean midXset = false;
    boolean midYset = false;

    final double maxX = m_envelope.getMaxX();
    final double maxY = m_envelope.getMaxY();
    final double minX = m_envelope.getMinX();
    final double minY = m_envelope.getMinY();

    // REMARK: sub containers never have a null envelope, only root containers do
    final Envelope subEnv = container.getEnvelope();
    final double maxXsub = subEnv.getMaxX();
    final double maxYsub = subEnv.getMaxY();
    final double minXsub = subEnv.getMinX();
    final double minYsub = subEnv.getMinY();

    // !!! == for double comparison !!!
    if( maxX == maxXsub )
    {
      midX = minXsub;
      midXset = true;
    }
    if( minX == minXsub )
    {
      midX = maxXsub;
      midXset = true;
    }
    if( maxY == maxYsub )
    {
      midY = minYsub;
      midYset = true;
    }
    if( minY == minYsub )
    {
      midY = maxYsub;
      midYset = true;
    }
    if( midXset && midYset )
    {
      createSubContainers( midX, midY );
      for( int i = 0; i < 4; i++ )
      {
        if( m_subContainer[i].m_envelope.equals( container.m_envelope ) )
          m_subContainer[i] = container;
      }
    }
    else
    {
      midX = (minX + maxX) / 2d;
      midY = (minY + maxY) / 2d;
      switch( bestPoint( midX, midY, minXsub, minYsub, maxXsub, maxYsub ) )
      {
        case LEFT_BOTTOM:
          createSubContainers( minXsub, minYsub );
          break;
        case RIGHT_BOTTOM:
          createSubContainers( maxXsub, minYsub );
          break;
        case RIGHT_TOP:
          createSubContainers( maxXsub, maxYsub );
          break;
        case LEFT_TOP:
          createSubContainers( minXsub, maxYsub );
          break;
      }
      for( int i = 0; i < 4; i++ )
      {
        if( m_subContainer[i].containsEnvelope( container.m_envelope ) )
          m_subContainer[i].createSubContainers( container );
      }
    }
  }

  private int bestPoint( final double midX, final double midY, final double minX, final double minY, final double maxX, final double maxY )
  {
    final double dist[] = new double[4];
    dist[LEFT_BOTTOM] = Math.pow( minX - midX, 2d ) + Math.pow( minY - midY, 2d );
    dist[RIGHT_BOTTOM] = Math.pow( maxX - midX, 2d ) + Math.pow( minY - midY, 2d );
    dist[RIGHT_TOP] = Math.pow( maxX - midX, 2d ) + Math.pow( maxY - midY, 2d );
    dist[LEFT_TOP] = Math.pow( minX - midX, 2d ) + Math.pow( maxY - midY, 2d );
    int result = 0;
    for( int i = 0; i < 4; i++ )
    {
      if( dist[i] < dist[result] )
        result = i;
    }
    return result;
  }

  private void createSubContainers( )
  {
    final double maxX = m_envelope.getMaxX();
    final double maxY = m_envelope.getMaxY();

    final double minX = m_envelope.getMinX();
    final double minY = m_envelope.getMinY();

    final double midX = (minX + maxX) / 2d;
    final double midY = (minY + maxY) / 2d;
    createSubContainers( midX, midY );
  }

  private void createSubContainers( final double midX, final double midY )
  {
    final double maxX = m_envelope.getMaxX();
    final double maxY = m_envelope.getMaxY();
    final double minX = m_envelope.getMinX();
    final double minY = m_envelope.getMinY();

    m_subContainer[0] = new SplitSortContainer( this, new Envelope( minX, midX, minY, midY ) );
    m_subContainer[1] = new SplitSortContainer( this, new Envelope( midX, maxX, minY, midY ) );
    m_subContainer[2] = new SplitSortContainer( this, new Envelope( midX, maxX, midY, maxY ) );
    m_subContainer[3] = new SplitSortContainer( this, new Envelope( minX, midX, midY, maxY ) );
  }

  private boolean hasSubContainers( )
  {
    for( int i = 0; i < 4; i++ )
    {
      if( m_subContainer[i] == null )
        return false;
    }

    return true;
  }

  public List<Object> query( final Envelope env, List<Object> result )
  {
    if( result == null )
      result = new ArrayList<Object>();

    if( env == null )
      return result;

    for( final Entry<Object, Envelope> entry : m_objects.entrySet() )
    {
      final Object object = entry.getKey();
      final Envelope envelope = entry.getValue();

      // objects with null envelope are always found...
      if( envelope == null || env.intersects( envelope ) )
        result.add( object );
    }

    if( hasSubContainers() )
    {
      for( int i = 0; i < 4; i++ )
      {
        if( m_subContainer[i].containsEnvelope( env ) )
        {
          result = m_subContainer[i].query( env, result );
          return result;
        }
      }
      for( int i = 0; i < 4; i++ )
      {
        if( env.intersects( m_subContainer[i].m_envelope ) )
          m_subContainer[i].query( env, result );
      }
    }
    return result;
  }

  /**
   * Removes the item from this container.
   * 
   * @param env
   *            If <code>null</code>, no optimization is made via the position of the item.
   */
  public boolean remove( final Envelope env, final Object item )
  {
    /* First, check if it is inside this envelope and remove it in this case */
    if( m_objects.containsKey( item ) )
    {
      m_objects.remove( item );

      if( m_parent != null )
        m_parent.optimize();

      return true;
    }

    if( hasSubContainers() )
    {
      for( int i = 0; i < 4; i++ )
      {
        /* Only search in containers, that may contain this object */
        if( env == null || m_subContainer[i].containsEnvelope( env ) )
        {
          final boolean subRemoved = m_subContainer[i].remove( env, item );
          if( subRemoved )
            return true;
        }
      }
    }

    return false;
  }

  private void optimize( )
  {
    if( hasSubContainers() )
    {
      for( int i = 0; i < 4; i++ )
      {
        if( m_subContainer[i].hasSubContainers() )
          return;
        if( m_subContainer[i].size() > 0 )
          return;
      }

      for( int i = 0; i < m_subContainer.length; i++ )
        m_subContainer[i] = null;
    }
    if( hasSubContainers() )
      return;
    if( size() > 0 )
      return;
    if( m_parent != null )
      m_parent.optimize();
  }

  public void paint( final Graphics g, final GeoTransform geoTransform )
  {
    if( m_envelope != null )
    {
      final double g1x = geoTransform.getDestX( m_envelope.getMinX() );
      final double g1y = geoTransform.getDestY( m_envelope.getMinY() );
      final double g2x = geoTransform.getDestX( m_envelope.getMaxX() );
      final double g2y = geoTransform.getDestY( m_envelope.getMaxY() );

      g.drawRect( (int) (g1x < g2x ? g1x : g2x), (int) (g1y < g2y ? g1y : g2y), (int) Math.abs( (g2x - g1x) ), (int) Math.abs( (g2y - g1y) ) );
    }

    if( hasSubContainers() )
    {
      for( int i = 0; i < 4; i++ )
        m_subContainer[i].paint( g, geoTransform );
    }
  }

  /**
   * Checks, if this container contains the item.
   * 
   * @param env
   *            If non-<code>null</code>, this envelope is used to improve the search.
   */
  public boolean contains( final Envelope env, final Object item )
  {
    if( env != null )
    {
      final List<Object> query = query( env, null );
      if( query.contains( item ) )
        return true;
    }

    // Else: brute force search
    if( m_objects.containsKey( item ) )
      return true;

    for( final SplitSortContainer subContainer : m_subContainer )
    {
      // Search with null-envelope so it is not again searched via query
      if( subContainer != null && subContainer.contains( null, item ) )
        return true;
    }

    return false;
  }

  public void accept( final Visitor visitor )
  {
    for( final Entry<Object, Envelope> entry : m_objects.entrySet() )
      visitor.visit( entry.getKey(), entry.getValue() );

    for( final SplitSortContainer subContainer : m_subContainer )
    {
      if( subContainer != null )
        subContainer.accept( visitor );
    }
  }

  /* default visibility */boolean hasNullEnvelope( )
  {
    return m_envelope == null;
  }

  /* default visibility */Envelope getEnvelope( )
  {
    return m_envelope;
  }
}