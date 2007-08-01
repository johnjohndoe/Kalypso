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
package org.kalypsodeegree_impl.model.sort;

import java.awt.Graphics;
import java.util.ArrayList;
import java.util.List;

import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.index.ItemVisitor;

/**
 * A Spatial-Index implemented by the good old kalypso SplitSort.
 * <p>
 * Made this in order to easyly exchange SplitSort with a JTS implementation.
 * 
 * @author Gernot Belger
 */
public class SplitSortSpatialIndex implements SpatialIndexExt
{
  private final IEnvelopeProvider m_envelopeProvider;

  private final List<Object> m_items = new ArrayList<Object>();

  private SplitSortContainer m_rootContainer = null;

  /**
   * A flag indicating if the spacial index is upd-to-date (if false). If not, the next call to 'query' will first
   * recalculate the index.
   */
  private boolean m_invalid = true;

  public SplitSortSpatialIndex( final IEnvelopeProvider envelopeProvider, final GM_Envelope env )
  {
    m_envelopeProvider = envelopeProvider;

    if( env != null )
      m_rootContainer = new SplitSortContainer( null, env, m_envelopeProvider );
  }

  /**
   * @see com.vividsolutions.jts.index.SpatialIndex#insert(com.vividsolutions.jts.geom.Envelope, java.lang.Object)
   */
  public void insert( final Envelope itemEnv, final Object item )
  {
    m_items.add( item );

    /* Only update index if we are valid. Else we do not need to because we get a resort at the next query. */
    if( itemEnv.isNull() || m_invalid )
      return;

    final GM_Envelope env = JTSAdapter.wrap( itemEnv );
    spatialAdd( item, env );
  }

  private void spatialAdd( final Object item, final GM_Envelope env )
  {
    if( m_rootContainer == null )
      m_rootContainer = new SplitSortContainer( null, env, m_envelopeProvider );

    if( m_rootContainer.getEnvelope().contains( env ) )
      m_rootContainer.add( env, item );
    else
    {
      final double maxX = env.getMax().getX();
      final double maxY = env.getMax().getY();
      final double minX = env.getMin().getX();
      final double minY = env.getMin().getY();

      final GM_Envelope envRoot = m_rootContainer.getEnvelope();

      final double maxXroot = envRoot.getMax().getX();
      final double maxYroot = envRoot.getMax().getY();
      final double minXroot = envRoot.getMin().getX();
      final double minYroot = envRoot.getMin().getY();
      final GM_Envelope newEnv = GeometryFactory.createGM_Envelope( minX < minXroot ? minX : minXroot, minY < minYroot ? minY : minYroot, maxX > maxXroot ? maxX : maxXroot, maxY > maxYroot ? maxY
          : maxYroot );

      final SplitSortContainer newRootContainer = new SplitSortContainer( null, newEnv, m_envelopeProvider );
      m_rootContainer.setParent( newRootContainer );
      newRootContainer.createSubContainers( m_rootContainer );
      m_rootContainer = newRootContainer;
      m_rootContainer.add( env, item );
    }
  }

  /**
   * @see com.vividsolutions.jts.index.SpatialIndex#query(com.vividsolutions.jts.geom.Envelope)
   */
  public List query( final Envelope searchEnv )
  {
    final List result = new ArrayList();

    resort();

    final GM_Envelope queryEnv = JTSAdapter.wrap( searchEnv );
    if( m_rootContainer != null )
      m_rootContainer.query( queryEnv, result );
    return result;
  }

  /**
   * @see com.vividsolutions.jts.index.SpatialIndex#query(com.vividsolutions.jts.geom.Envelope,
   *      com.vividsolutions.jts.index.ItemVisitor)
   */
  public void query( final Envelope searchEnv, final ItemVisitor visitor )
  {
    final List list = query( searchEnv );
    for( final Object object : list )
      visitor.visitItem( object );
  }

  /**
   * @see com.vividsolutions.jts.index.SpatialIndex#remove(com.vividsolutions.jts.geom.Envelope, java.lang.Object)
   */
  public boolean remove( final Envelope itemEnv, final Object item )
  {
    invalidate();

    return m_items.remove( item );
  }

  private void resort( )
  {
    if( m_invalid == false )
      return;

    m_rootContainer = null;

    GM_Envelope bbox = null;

    // REMARK: the access to m_items is not synchronized, so we
    // retrieve the objects as array to reduce probability of ConcurrentModificationExceptions's
    final Object[] items = m_items.toArray( new Object[m_items.size()] );
    for( final Object f : items )
    {
      final GM_Envelope envelope = getEnvelope( f );
      if( bbox == null )
        bbox = envelope;
      else
        bbox = bbox.getMerged( envelope );
    }

    if( bbox != null )
    {
      m_rootContainer = new SplitSortContainer( null, bbox, m_envelopeProvider );
      for( final Object next : items )
      {
        final GM_Envelope envelope = getEnvelope( next );
        if( envelope == null )
        {
          // because it throws exception
        }
        else
        {
          spatialAdd( next, envelope );
        }
      }
    }
    else
      m_rootContainer = null;

    m_invalid = false;
  }

  public GM_Envelope getEnvelope( final Object object )
  {
    return m_envelopeProvider.getEnvelope( object );
  }

  public void invalidate( )
  {
    m_invalid = true;
  }

  public void paint( final Graphics g, final GeoTransform geoTransform )
  {
    if( m_rootContainer != null )
      m_rootContainer.paint( g, geoTransform );
  }

  public Envelope getBoundingBox( )
  {
    if( m_invalid )
      resort();

    if( m_rootContainer != null )
      return JTSAdapter.export( m_rootContainer.getEnvelope() );

    return new Envelope();
  }

  /**
   * @see org.kalypsodeegree_impl.model.sort.SpatialIndexExt#clear()
   */
  public void clear( )
  {
    m_items.clear();

    m_rootContainer = null;
    invalidate();
  }

  /**
   * @see org.kalypsodeegree_impl.model.sort.SpatialIndexExt#invalidate(java.lang.Object)
   */
  public void invalidate( final Object o )
  {
    invalidate();
  }

  /**
   * @see org.kalypsodeegree_impl.model.sort.SpatialIndexExt#size()
   */
  public int size( )
  {
    return m_items.size();
  }
}
