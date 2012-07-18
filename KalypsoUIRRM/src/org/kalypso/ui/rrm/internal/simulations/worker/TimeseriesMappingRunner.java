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
package org.kalypso.ui.rrm.internal.simulations.worker;

import java.io.UnsupportedEncodingException;
import java.util.Calendar;
import java.util.Date;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.joda.time.Period;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.binding.timeseriesMappings.IMappingElement;
import org.kalypso.model.hydrology.binding.timeseriesMappings.ITimeseriesMapping;
import org.kalypso.model.hydrology.binding.timeseriesMappings.TimeseriesMappingType;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.filter.IObservationFilter;
import org.kalypso.ogc.sensor.filter.filters.interval.IntervalDefinition;
import org.kalypso.ogc.sensor.filter.filters.interval.IntervalFilter;
import org.kalypso.ogc.sensor.metadata.MetadataHelper;
import org.kalypso.ogc.sensor.request.ObservationRequest;
import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypso.ogc.sensor.timeseries.interpolation.InterpolationFilter;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.calccase.CatchmentModelHelper;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.IXLinkedFeature;

/**
 * Executes the timeseries mapping for a simulation.
 * 
 * @author Gernot Belger
 */
public class TimeseriesMappingRunner implements ICoreRunnableWithProgress
{
  private final RrmSimulation m_rrmSimulation;

  private final TimeseriesMappingType m_mappingType;

  private final NAControl m_simulation;

  public TimeseriesMappingRunner( final RrmSimulation rrmSimulation, final NAControl simulation, final TimeseriesMappingType mappingType )
  {
    m_rrmSimulation = rrmSimulation;
    m_simulation = simulation;
    m_mappingType = mappingType;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    final IStatusCollector log = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    final String monitorTitle = String.format( Messages.getString( "TimeseriesMappingRunner_0" ), m_mappingType.getLabel() ); //$NON-NLS-1$

    final ITimeseriesMapping mapping = getMapping();
    if( mapping == null )
      log.add( IStatus.OK, Messages.getString( "TimeseriesMappingRunner_1" ) ); //$NON-NLS-1$
    else
    {
      monitor.beginTask( monitorTitle, mapping.getMappings().size() );
      executeMapping( mapping, log, monitor );
    }

    monitor.done();

    return log.asMultiStatus( monitorTitle );
  }

  private ITimeseriesMapping getMapping( )
  {
    switch( m_mappingType )
    {
      case gaugeMeasurement:
        return m_simulation.getMappingGauge();

      case nodeInflow:
        return m_simulation.getMappingNodeInflow();

      case storageEvaporation:
        return m_simulation.getMappingStorageEvaporation();
    }

    return null;
  }

  private void executeMapping( final ITimeseriesMapping mapping, final IStatusCollector log, final IProgressMonitor monitor )
  {
    final IFeatureBindingCollection<IMappingElement> mappings = mapping.getMappings();
    for( final IMappingElement element : mappings )
    {
      final IXLinkedFeature modelElement = element.getLinkedModelElement();

      final ZmlLink linkedTimeseries = element.getLinkedTimeseries();

      if( linkedTimeseries.isLinkSet() )
      {
        final IStatus status = executeMappingElement( linkedTimeseries, modelElement );
        log.add( status );
      }

      monitor.worked( 1 );
    }
  }

  private IStatus executeMappingElement( final ZmlLink linkedTimeseries, final IXLinkedFeature modelElement )
  {
    /* Check if link is valid */
    if( !linkedTimeseries.isLinkExisting() )
    {
      final String message = String.format( Messages.getString( "TimeseriesMappingRunner_2" ), modelElement.getName(), linkedTimeseries.getHref() ); //$NON-NLS-1$
      return new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), message );
    }

    try
    {
      /* Copy and filter the timeseries */
      final IObservation timeseries = linkedTimeseries.loadObservation();

      /* Get the unadjusted simulation range. */
      final Date simulationStart = m_simulation.getSimulationStart();
      final Date simulationEnd = m_simulation.getSimulationEnd();
      final DateRange unadjustedSimulationRange = new DateRange( simulationStart, simulationEnd );

      /* Check, if the range of the timeseries covers the unadjusted simulation range. */
      final DateRange timeseriesRange = MetadataHelper.getDateRange( timeseries.getMetadataList() );
      if( !timeseriesRange.containsInclusive( unadjustedSimulationRange ) )
        throw new SensorException( String.format( Messages.getString("TimeseriesMappingRunner.0"), timeseries.getName(), timeseriesRange.toString(), unadjustedSimulationRange.toString() ) ); //$NON-NLS-1$

      /* Apply filter. */
      final IObservationFilter filteredTimeseries = createTimeseriesFilter();
      filteredTimeseries.initFilter( null, timeseries, null );

      /* Change model link */
      final QName modelLinkProperty = m_mappingType.getModelLinkProperty();
      final TimeseriesLinkType newLinkType = new TimeseriesLinkType();
      newLinkType.setHref( buildTargetHref( modelElement ) );
      modelElement.setProperty( modelLinkProperty, newLinkType );

      /* Build request */
      final Integer timestepMinutes = m_simulation.getMinutesOfTimestep();
      final Period timestep = Period.minutes( timestepMinutes ).normalizedStandard();
      // TODO: Check if we need timestamp for sea evaporation
      final DateRange simulationRange = CatchmentModelHelper.getRange( m_simulation, timestep, null );
      final ObservationRequest request = new ObservationRequest( simulationRange );

      /* Filter and save to new location */
      final ZmlLink zmlLink = new ZmlLink( modelElement, modelLinkProperty );
      zmlLink.saveObservation( filteredTimeseries, request );

      return Status.OK_STATUS;
    }
    catch( final SensorException | UnsupportedEncodingException | CoreException e )
    {
      return new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), Messages.getString( "TimeseriesMappingRunner_3" ), e ); //$NON-NLS-1$
    }
  }

  private IObservationFilter createTimeseriesFilter( )
  {
    // TODO: check if always good like this
    final Integer minutesOfTimestep = m_simulation.getMinutesOfTimestep();

    switch( m_mappingType )
    {
      case gaugeMeasurement:
      case nodeInflow:
        return new InterpolationFilter( Calendar.MINUTE, minutesOfTimestep, true, "0.0", KalypsoStati.BIT_CHECK ); //$NON-NLS-1$

      case storageEvaporation:
        final IntervalDefinition definition = new IntervalDefinition( Calendar.MINUTE, minutesOfTimestep, 0.0, KalypsoStati.BIT_CHECK );
        return new IntervalFilter( definition );
    }

    throw new IllegalArgumentException();
  }

  private String buildTargetHref( final IXLinkedFeature modelElement ) throws UnsupportedEncodingException
  {
    final String parameterType = m_mappingType.getLinkParameterType();
    final String folderName = m_mappingType.getTargetFolderName( m_rrmSimulation );

    return CatchmentModelHelper.buildLink( m_rrmSimulation, null, folderName, parameterType, modelElement );
  }
}