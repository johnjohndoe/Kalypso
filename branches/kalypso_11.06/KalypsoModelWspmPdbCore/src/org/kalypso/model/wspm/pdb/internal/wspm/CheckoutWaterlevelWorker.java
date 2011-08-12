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

import org.apache.commons.lang.NotImplementedException;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.WspmFixation;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.db.mapping.WaterlevelFixation;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Gernot Belger
 */
public class CheckoutWaterlevelWorker
{
  private final CheckoutDataMapping m_mapping;

  public CheckoutWaterlevelWorker( final CheckoutDataMapping mapping )
  {
    m_mapping = mapping;
  }

  public void execute( final IProgressMonitor monitor ) throws CoreException
  {
    final Event[] events = m_mapping.getEvents();

    monitor.beginTask( "Reading water levels from database", events.length );

    try
    {
      for( final Event event : events )
      {
        monitor.subTask( String.format( "Converting %s", event.getName() ) );

        final Object wspmObject = m_mapping.getWaterlevel( event );
        final Feature newWspmObject = createOrReplaceEvent( event, wspmObject );
        m_mapping.addChangedFeatures( newWspmObject );

        ProgressUtilities.worked( monitor, 1 );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, "Should never happen", e ); //$NON-NLS-1$
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
      return removeFixation( (WspmFixation) wspmObject, wspmWater );

    if( wspmObject instanceof TuhhCalculation )
      // FIXME
      throw new NotImplementedException();

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
        return insertFixation( event, wspmWater );

      case Simulation:
        return insertCalculation( event, wspmWater );
    }

    throw new IllegalStateException();
  }

  private Feature insertFixation( final Event event, final WspmWaterBody wspmWater )
  {
    // TODO: create waterlevels and create runoffs

    final IFeatureBindingCollection<WspmFixation> wspFixations = wspmWater.getWspFixations();

    final WspmFixation wspFixation = wspFixations.addNew( WspmFixation.QNAME_FEATURE_WSPM_FIXATION );

    final IObservation<TupleResult> obs = wspFixation.toObservation();
    obs.setName( event.getName() );
    obs.setDescription( event.getDescription() );

    final TupleResult result = obs.getResult();

    final int stationComp = result.indexOfComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_STATION );
    final int valueComp = result.indexOfComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_WATERLEVEL );

    final Set<WaterlevelFixation> waterlevelFixations = event.getWaterlevelFixations();
    final WaterlevelFixation[] sortedFixations = waterlevelFixations.toArray( new WaterlevelFixation[waterlevelFixations.size()] );
    Arrays.sort( sortedFixations, new FixationStationComparator( wspmWater.isDirectionUpstreams() ) );

    for( final WaterlevelFixation fixation : sortedFixations )
    {
      final BigDecimal station = fixation.getStation().movePointLeft( 3 );

      final BigDecimal waterlevel = fixation.getWaterlevel();
      if( waterlevel != null )
      {
        final IRecord record = result.createRecord();
        result.add( record );
        record.setValue( stationComp, station );
        record.setValue( valueComp, waterlevel );
      }
    }

    wspFixation.saveObservation( obs );
    return wspFixation;
  }

  private Feature insertCalculation( final Event event, final WspmWaterBody wspmWater )
  {
    // FIXME
    throw new NotImplementedException();
  }
}