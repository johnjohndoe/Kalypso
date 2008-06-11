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
import java.util.List;

import org.kalypsodeegree.graphics.transformation.GeoTransform;

import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.index.ItemVisitor;

/**
 * A Spatial-Index implemented by the good old kalypso SplitSort.
 * <p>
 * Made this in order to easily exchange SplitSort with a JTS implementation.
 * 
 * @author Gernot Belger
 */
public class SplitSortSpatialIndex implements SpatialIndexExt
{
  private SplitSortContainer m_rootContainer = null;

  public SplitSortSpatialIndex( final Envelope env )
  {
    m_rootContainer = new SplitSortContainer( null, env );
  }

  /**
   * @see com.vividsolutions.jts.index.SpatialIndex#insert(com.vividsolutions.jts.geom.Envelope, java.lang.Object)
   */
  public void insert( final Envelope env, final Object item )
  {
    if( env == null || m_rootContainer.hasNullEnvelope() || m_rootContainer.containsEnvelope( env ) )
      m_rootContainer.add( item, env );
    else
    {
      final Envelope newRootEnv = new Envelope( env );
      newRootEnv.expandToInclude( m_rootContainer.getEnvelope() );

      final SplitSortContainer newRootContainer = new SplitSortContainer( null, newRootEnv );
      m_rootContainer.setParent( newRootContainer );
      newRootContainer.createSubContainers( m_rootContainer );
      m_rootContainer = newRootContainer;
      m_rootContainer.add( item, env );
    }
  }

  /**
   * @see com.vividsolutions.jts.index.SpatialIndex#query(com.vividsolutions.jts.geom.Envelope)
   */
  public List<Object> query( final Envelope searchEnv )
  {
    final List<Object> result = new ArrayList<Object>();

    m_rootContainer.query( searchEnv, result );

    return result;
  }

  /**
   * @see com.vividsolutions.jts.index.SpatialIndex#query(com.vividsolutions.jts.geom.Envelope,
   *      com.vividsolutions.jts.index.ItemVisitor)
   */
  public void query( final Envelope searchEnv, final ItemVisitor visitor )
  {
    final List<Object> list = query( searchEnv );
    for( final Object object : list )
      visitor.visitItem( object );
  }

  public void clear( )
  {
    m_rootContainer = new SplitSortContainer( null, null );
  }

  /**
   * @see com.vividsolutions.jts.index.SpatialIndex#remove(com.vividsolutions.jts.geom.Envelope, java.lang.Object)
   */
  public boolean remove( final Envelope itemEnv, final Object item )
  {
    return m_rootContainer.remove( itemEnv, item );
  }

  public void paint( final Graphics g, final GeoTransform geoTransform )
  {
    m_rootContainer.paint( g, geoTransform );
  }

  public Envelope getBoundingBox( )
  {
    return m_rootContainer.getEnvelope();
  }

  /**
   * @see org.kalypsodeegree_impl.model.sort.SpatialIndexExt#contains(com.vividsolutions.jts.geom.Envelope,
   *      java.lang.Object)
   */
  public boolean contains( final Envelope itemEnv, final Object item )
  {
    return m_rootContainer.contains( itemEnv, item );
  }

}
