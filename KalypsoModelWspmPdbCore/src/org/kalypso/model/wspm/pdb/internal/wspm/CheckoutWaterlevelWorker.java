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
package org.kalypso.model.wspm.pdb.internal.wspm;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.wspm.core.gml.WspmFixation;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.db.mapping.WaterlevelFixation;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.wspm.CheckoutDataMapping;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.observation.result.TupleResultUtilities;
import org.kalypso.transformation.transformer.JTSTransformer;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.opengis.geometry.MismatchedDimensionException;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Point;

/**
 * @author Gernot Belger
 */
public class CheckoutWaterlevelWorker
{
  private final CheckoutDataMapping m_mapping;

  private final int m_dbSRID;

  public CheckoutWaterlevelWorker( final CheckoutDataMapping mapping, final int dbSRID )
  {
    m_mapping = mapping;
    m_dbSRID = dbSRID;
  }

  public void execute( final IProgressMonitor monitor ) throws CoreException
  {
    final Event[] events = m_mapping.getEvents();

    monitor.beginTask( Messages.getString( "CheckoutWaterlevelWorker.0" ), events.length ); //$NON-NLS-1$

    try
    {
      for( final Event event : events )
      {
        monitor.subTask( String.format( Messages.getString( "CheckoutWaterlevelWorker.1" ), event.getName() ) ); //$NON-NLS-1$

        final Object wspmObject = m_mapping.getWaterlevel( event );
        final Feature newWspmObject = createOrReplaceEvent( event, wspmObject );
        m_mapping.addAddedFeatures( newWspmObject );

        ProgressUtilities.worked( monitor, 1 );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, "Failed to import waterlevels", e ); //$NON-NLS-1$
      throw new CoreException( status );
    }
    finally
    {
      ProgressUtilities.done( monitor );
    }
  }

  private Feature createOrReplaceEvent( final Event event, final Object wspmObject ) throws Exception
  {
    final WaterBody waterBody = event.getWaterBody();
    final WspmWaterBody wspmWater = m_mapping.getWspmWaterBody( waterBody );

    final Feature removed = removeEvent( wspmObject, wspmWater );
    if( removed != null )
      m_mapping.addRemovedFeatures( removed );

    return insertEvent( event, wspmWater );
  }

  private Feature removeEvent( final Object wspmObject, final WspmWaterBody wspmWater )
  {
    if( wspmObject instanceof WspmFixation )
      return removeFixation( (WspmFixation)wspmObject, wspmWater );

    if( wspmObject instanceof TuhhCalculation )
      // FIXME
      throw new UnsupportedOperationException();

    return null;
  }

  private Feature removeFixation( final WspmFixation fixation, final WspmWaterBody wspmWater )
  {
    final IFeatureBindingCollection<WspmFixation> fixations = wspmWater.getWspFixations();
    fixations.remove( fixation );
    return fixation;
  }

  private Feature insertEvent( final Event event, final WspmWaterBody wspmWater ) throws Exception
  {
    switch( event.getType() )
    {
      case Measurement:
      case Simulation:
        return insertFixation( event, wspmWater );
        // FIXME: simulation should create simulation results
        // return insertCalculation( event, wspmWater );
    }

    throw new IllegalStateException();
  }

  private Feature insertFixation( final Event event, final WspmWaterBody wspmWater ) throws FactoryException, MismatchedDimensionException, TransformException
  {
    final IFeatureBindingCollection<WspmFixation> wspFixations = wspmWater.getWspFixations();

    /* create fixation and set name */
    final WspmFixation wspFixation = wspFixations.addNew( WspmFixation.QNAME_FEATURE_WSPM_FIXATION );

    final IObservation<TupleResult> obs = wspFixation.toObservation();
    obs.setName( event.getName() );
    obs.setDescription( event.getDescription() );

    /* fetch tuples and component indices */
    final TupleResult result = obs.getResult();

    final int stationComp = result.indexOfComponent( WspmFixation.COMPONENT_STATION );
    final int waterlevelComp = result.indexOfComponent( WspmFixation.COMPONENT_WSP );
    final int commentComp = result.indexOfComponent( WspmFixation.COMPONENT_COMMENT );
    final int runoffComp = TupleResultUtilities.getOrCreateComponent( result, WspmFixation.COMPONENT_RUNOFF );
    final int eastingComp = TupleResultUtilities.getOrCreateComponent( result, WspmFixation.COMPONENT_EASTING );
    final int northingComp = TupleResultUtilities.getOrCreateComponent( result, WspmFixation.COMPONENT_NORTHING );

    /* sort in direction of calculation */
    final Set<WaterlevelFixation> waterlevelFixations = event.getWaterlevelFixations();
    final WaterlevelFixation[] sortedFixations = waterlevelFixations.toArray( new WaterlevelFixation[waterlevelFixations.size()] );
    Arrays.sort( sortedFixations, new FixationStationComparator( wspmWater.isDirectionUpstreams() ) );

    /* initialize geo transformation */
    // TODO: it is not save using Kalypso srs here; we should rather save the srs in the waterlevel as well.
    final int kalypsoSRID = JTSAdapter.toSrid( KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );
    final JTSTransformer transformer = new JTSTransformer( m_dbSRID, kalypsoSRID );

    /* convert waterlevels */
    for( final WaterlevelFixation fixation : sortedFixations )
    {
      final BigDecimal station = fixation.getStation().movePointLeft( 3 );
      final BigDecimal waterlevel = fixation.getWaterlevel();
      final BigDecimal discharge = fixation.getDischarge();
      final String description = fixation.getDescription();
      final Point location = fixation.getLocation();

      if( waterlevel != null )
      {
        final IRecord record = result.createRecord();

        record.setValue( stationComp, station );
        record.setValue( waterlevelComp, waterlevel );
        record.setValue( commentComp, description );
        record.setValue( runoffComp, discharge );

        if( location != null )
        {
          final Coordinate transformed = transformer.transform( location.getCoordinate() );
          record.setValue( eastingComp, new BigDecimal( transformed.x ) );
          record.setValue( northingComp, new BigDecimal( transformed.y ) );
        }

        result.add( record );
      }
    }

    wspFixation.saveObservation( obs );
    return wspFixation;
  }
}