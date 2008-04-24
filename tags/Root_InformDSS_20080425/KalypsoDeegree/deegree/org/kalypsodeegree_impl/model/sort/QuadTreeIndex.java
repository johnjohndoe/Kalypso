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
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.index.ItemVisitor;
import com.vividsolutions.jts.index.SpatialIndex;
import com.vividsolutions.jts.index.quadtree.Quadtree;

/**
 * // TODO: refaktor QuadTreeIndex in order to not use the envelope-provider. This is necessary, because everything is
 * synchronized now.... (don't call foreign code within synchronized code)
 * 
 * @author Gernot Belger
 */
public class QuadTreeIndex implements SpatialIndexExt
{
  private SpatialIndex m_index;

  private final List<Object> m_items = new ArrayList<Object>();

  private final IEnvelopeProvider m_envelopeProvider;

  private Envelope m_boundingBox = null;

  public QuadTreeIndex( final IEnvelopeProvider envelopeProvider )
  {
    m_envelopeProvider = envelopeProvider;
    m_index = null;
  }

  /**
   * @see org.kalypsodeegree_impl.model.sort.SpatialIndexExt#clear()
   */
  public void clear( )
  {
    m_items.clear();
    m_index = null;
    m_boundingBox = null;
  }

  private Envelope getEnvelope( final Object object )
  {
    final GM_Envelope envelope = m_envelopeProvider.getEnvelope( object );
    return JTSAdapter.export( envelope );
  }

  /**
   * @see org.kalypsodeegree_impl.model.sort.SpatialIndexExt#getBoundingBox()
   */
  public Envelope getBoundingBox( )
  {
    if( m_boundingBox == null )
      m_boundingBox = recalcEnvelope();

    return m_boundingBox;
  }

  private Envelope recalcEnvelope( )
  {
    if( m_items.isEmpty() )
      return new Envelope();

    Envelope bbox = null;
    for( final Object item : m_items )
    {
      final Envelope envelope = getEnvelope( item );
      if( envelope.isNull() )
        continue;

      if( bbox == null )
        bbox = envelope;
      else
        bbox.expandToInclude( envelope );
    }

    if( bbox == null )
      return new Envelope();

    return bbox;
  }

  /**
   * @see org.kalypsodeegree_impl.model.sort.SpatialIndexExt#invalidate()
   */
  public void invalidate( )
  {
    m_boundingBox = null;
    m_index = null;
  }

  /**
   * @see org.kalypsodeegree_impl.model.sort.SpatialIndexExt#invalidate(java.lang.Object)
   */
  public void invalidate( final Object item )
  {
    revalidate();

    final Envelope itemEnv = getEnvelope( item );
    // TODO: this is not good, because the env this object was added may differ from the new envelope
    // however, this may lead to the case, that the object will not be removed
    m_index.remove( itemEnv, item );
    if( !itemEnv.isNull() )
      m_index.insert( itemEnv, item );

    /* Bounding box must be calculated anew */
    m_boundingBox = null;
  }

  private void revalidate( )
  {
    if( m_index != null )
      return;

    m_index = createIndex();

    for( final Object item : m_items )
    {
      final Envelope itemEnv = getEnvelope( item );
      if( itemEnv.isNull() )
      {
// System.out.println( "Null envelope for: " + item );
      }
      else
        m_index.insert( itemEnv, item );
    }
  }

  private SpatialIndex createIndex( )
  {
    return new Quadtree();
// return new STRtree();
  }

  /**
   * @see org.kalypsodeegree_impl.model.sort.SpatialIndexExt#paint(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform)
   */
  public void paint( final Graphics g, final GeoTransform geoTransform )
  {
    // sorry, don't know how to paint a quadtree
  }

  /**
   * @see org.kalypsodeegree_impl.model.sort.SpatialIndexExt#size()
   */
  public int size( )
  {
    return m_items.size();
  }

  /**
   * @see com.vividsolutions.jts.index.SpatialIndex#insert(com.vividsolutions.jts.geom.Envelope, java.lang.Object)
   */
  public void insert( final Envelope itemEnv, final Object item )
  {
    m_items.add( item );

    if( m_boundingBox != null && !itemEnv.isNull() )
      m_boundingBox.expandToInclude( itemEnv );

    if( m_index == null )
      revalidate();
    else if( !itemEnv.isNull() )
      m_index.insert( itemEnv, item );

    // TODO:check depth of quadtree, if too big, maybe we have to revalidate?
  }

  /**
   * @see com.vividsolutions.jts.index.SpatialIndex#query(com.vividsolutions.jts.geom.Envelope)
   */
  public List query( final Envelope searchEnv )
  {
    revalidate();

    return m_index.query( searchEnv );
  }

  /**
   * @see com.vividsolutions.jts.index.SpatialIndex#query(com.vividsolutions.jts.geom.Envelope,
   *      com.vividsolutions.jts.index.ItemVisitor)
   */
  public void query( final Envelope searchEnv, final ItemVisitor visitor )
  {
    revalidate();

    m_index.query( searchEnv, visitor );
  }

  /**
   * @see com.vividsolutions.jts.index.SpatialIndex#remove(com.vividsolutions.jts.geom.Envelope, java.lang.Object)
   */
  public boolean remove( final Envelope itemEnv, final Object item )
  {
    m_index.remove( itemEnv, item );
    m_boundingBox = null;

    return m_items.remove( item );
  }

  /**
   * NOT IMPLEMENTED
   * 
   * @see org.kalypsodeegree_impl.model.sort.SpatialIndexExt#contains(com.vividsolutions.jts.geom.Envelope,
   *      java.lang.Object)
   */
  public boolean contains( final Envelope itemEnv, final Object item )
  {
    throw new UnsupportedOperationException();
  }

}
