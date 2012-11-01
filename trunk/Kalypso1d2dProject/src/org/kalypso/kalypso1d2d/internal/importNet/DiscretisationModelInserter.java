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
package org.kalypso.kalypso1d2d.internal.importNet;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.ElementGeometryHelper;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.kalypsodeegree_impl.model.sort.SpatialIndexExt;
import org.kalypsodeegree_impl.model.sort.SplitSortSpatialIndex;

import com.bce.gis.io.zweidm.IPolygonWithName;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.index.ItemVisitor;

/**
 * TODO: check: merge with insertion code of BandGen etc.<br/>
 * Wrapper around a {@link org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d} that allows to
 * insert new element into the model.
 * TODO: <br/>
 * <ul>
 * <li>monitor progress</li>
 * <li>check incoming elements for consistency, OK: TODO: testing!</li>
 * <li>check if incoming intersect existing</li>
 * <li>- skip intersecting</li>
 * <li>- or, remove intersected from existing</li>
 * <li>- or, refine on intersection</li>
 * </ul>
 * 
 * @author Gernot Belger
 */
public class DiscretisationModelInserter
{
  private Collection<IPolygonWithName> m_incoming = new LinkedList<>();

  private final CommandableWorkspace m_workspace;

  private final IFEDiscretisationModel1d2d m_model;

  private SpatialIndexExt m_incomingIndex;

  private SurfacePlaneChecker m_incomingValidator;

  private SurfaceModelChecker m_againstModelValidator;

  // TODO: get rid of the workspace
  public DiscretisationModelInserter( final CommandableWorkspace workspace, final IFEDiscretisationModel1d2d model )
  {
    m_workspace = workspace;
    m_model = model;
  }

  public IFEDiscretisationModel1d2d getModel( )
  {
    return m_model;
  }

  public void addElement( final IPolygonWithName surface )
  {
    m_incoming.add( surface );
  }

  public void buildIncomingIndex( )
  {
    Assert.isTrue( m_incomingIndex == null );

    /* Calculate fullExtent */
    final Envelope incomingExtent = calculateIncomingExtent();

    final SpatialIndexExt incomingIndex = new SplitSortSpatialIndex( incomingExtent );

    for( final Iterator<IPolygonWithName> iterator = m_incoming.iterator(); iterator.hasNext(); )
    {
      final IPolygonWithName surface = iterator.next();
      final Envelope envelope = surface.getEnvelope();
      incomingIndex.insert( envelope, surface );

      // Clear incoming at the same time in order to avoid having too enormous collections in memory at the same time
      iterator.remove();
    }

    m_incomingIndex = incomingIndex;
    m_incoming = null;
  }

  public IStatus validateIncoming( )
  {
    Assert.isNotNull( m_incomingIndex );
    Assert.isTrue( m_incomingValidator == null );

    m_incomingValidator = new SurfacePlaneChecker( m_incomingIndex );
    return m_incomingValidator.execute();
  }

  public Collection<IPolygonWithName> getBadIncoming( )
  {
    Assert.isNotNull( m_incomingValidator );
    return m_incomingValidator.getBadElements();
  }

  public IStatus validateAgainstModel( )
  {
    Assert.isNotNull( m_incomingIndex );
    Assert.isTrue( m_againstModelValidator == null );

    m_againstModelValidator = new SurfaceModelChecker( m_incomingIndex, m_model );
    return m_againstModelValidator.execute();
  }

  public Collection<IPolygonWithName> getBadAgainstModel( )
  {
    Assert.isNotNull( m_againstModelValidator );
    return m_againstModelValidator.getBadElements();
  }

  public void removeElements( final Collection<IPolygonWithName> elements )
  {
    Assert.isNotNull( m_incomingIndex );

    for( final IPolygonWithName item : elements )
    {
      final Envelope envelope = item.getEnvelope();
      m_incomingIndex.remove( envelope, item );
    }
  }

  public IStatus commitChanges( )
  {
    Assert.isNotNull( m_incomingIndex );

    final IStatusCollector stati = new StatusCollector( Kalypso1d2dProjectPlugin.PLUGIN_ID );

    final Envelope boundingBox = m_incomingIndex.getBoundingBox();

    final ItemVisitor insertVisitor = new ItemVisitor()
    {
      @Override
      public void visitItem( final Object item )
      {
        final IPolygonWithName surface = (IPolygonWithName)item;
        final IStatus status = insertElementIntoModel( surface );
        if( !status.isOK() )
          stati.add( status );
      }
    };
    m_incomingIndex.query( boundingBox, insertVisitor );

    return stati.asMultiStatusOrOK( "Problem(s) while inserting elements into model" );
  }

  private Envelope calculateIncomingExtent( )
  {
    Envelope fullExtent = null;
    for( final IPolygonWithName surface : m_incoming )
    {
      final Envelope envelope = surface.getEnvelope();
      if( fullExtent == null )
        fullExtent = envelope;
      else
        fullExtent.expandToInclude( envelope );
    }
    return fullExtent;
  }

  IStatus insertElementIntoModel( final IPolygonWithName item )
  {
    try
    {
      final Polygon polygon = item.getPolygon();
      final GM_Polygon surface = (GM_Polygon)JTSAdapter.wrapWithSrid( polygon );

      final Feature parentFeature = m_workspace.getRootFeature();
      final IFEDiscretisationModel1d2d discModel = (IFEDiscretisationModel1d2d)parentFeature.getAdapter( IFEDiscretisationModel1d2d.class );
      ElementGeometryHelper.createFE1D2DfromSurface( m_workspace, discModel, surface );
      return Status.OK_STATUS;
    }
    catch( final Exception e )
    {
      final String message = String.format( "Failed to insert element '%s'", item.getName() );
      return new Status( IStatus.ERROR, Kalypso1d2dProjectPlugin.PLUGIN_ID, message, e );
    }
  }
}