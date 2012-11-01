/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.kalypso1d2d.internal.importNet;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypsodeegree_impl.model.sort.SpatialIndexExt;

import com.bce.gis.io.zweidm.IPolygonWithName;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.index.ItemVisitor;

/**
 * Checks if a collection of {@link IPolygonWithName}'s forms a ''CW-Complex' (note: check the exact definition,
 * probably our notion is somewhat softer).
 * 
 * @author Gernot Belger
 */
public class SurfacePlaneChecker
{
  /**
   * name -> item
   */
  private final Map<String, IPolygonWithName> m_badElements = new HashMap<>();

  private final IStatusCollector m_stati = new StatusCollector( Kalypso1d2dProjectPlugin.PLUGIN_ID );

  private final SpatialIndexExt m_index;

  /**
   * @param surfaceIndex
   *          The surface elements to check. All elements of the given geo index must be {@link IPolygonWithName}'s
   */
  public SurfacePlaneChecker( final SpatialIndexExt index )
  {
    m_index = index;
  }

  /**
   * returns an (unmodifiable) collection of all elements that violate the check.
   */
  public Collection<IPolygonWithName> getBadElements( )
  {
    return Collections.unmodifiableCollection( m_badElements.values() );
  }

  public IStatus execute( )
  {
    final ItemVisitor checkVisitor = new ItemVisitor()
    {
      @Override
      public void visitItem( final Object item )
      {
        checkItem( (IPolygonWithName)item );
      }
    };

    final Envelope fullExtent = m_index.getBoundingBox();
    m_index.query( fullExtent, checkVisitor );

    return m_stati.asMultiStatusOrOK( Messages.getString("SurfacePlaneChecker_0"), Messages.getString("SurfacePlaneChecker_1") ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  protected void checkItem( final IPolygonWithName item )
  {
    final String itemWarning = doCheckItem( item );
    if( itemWarning != null )
    {
      final String name = item.getName();
      m_badElements.put( name, item );

      m_stati.add( IStatus.WARNING, Messages.getString("SurfacePlaneChecker_2"), null, name, itemWarning ); //$NON-NLS-1$
      return;
    }

    /* Check relation to other elements */
    final ItemVisitor intersectionVisitor = new ItemVisitor()
    {
      @Override
      public void visitItem( final Object intersected )
      {
        checkItemIntersection( item, (IPolygonWithName)intersected );
      }
    };

    final Envelope envelope = item.getEnvelope();
    m_index.query( envelope, intersectionVisitor );
  }

  protected void checkItemIntersection( final IPolygonWithName item, final IPolygonWithName intersected )
  {
    final String name1 = item.getName();
    final String name2 = intersected.getName();

    if( name1.equals( name2 ) )
      return;

    if( m_badElements.containsKey( name2 ) )
      return;

    final ModelTopologyValidator intersectionValidator = new ModelTopologyValidator();
    final String warning = intersectionValidator.validate( item, intersected );
    if( warning == null )
      return;

    m_badElements.put( name1, item );

    m_stati.add( IStatus.WARNING, Messages.getString("SurfacePlaneChecker_3"), null, name1, name2, warning ); //$NON-NLS-1$
  }

  private String doCheckItem( final IPolygonWithName item )
  {
    final Polygon polygon = item.getPolygon();

    final int numPoints = polygon.getNumPoints();
    if( numPoints > 5 )
      // TODO: would be nice if we could handle this -> preprocess elements and triangulate them
      return Messages.getString("SurfacePlaneChecker_4"); //$NON-NLS-1$

    if( !polygon.isValid() )
      return Messages.getString("SurfacePlaneChecker_5"); //$NON-NLS-1$
    if( polygon.getNumInteriorRing() != 0 )
      return Messages.getString("SurfacePlaneChecker_6"); //$NON-NLS-1$

    return null;
  }
}