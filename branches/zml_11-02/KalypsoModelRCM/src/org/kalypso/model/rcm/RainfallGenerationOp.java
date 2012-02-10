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
package org.kalypso.model.rcm;

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.joda.time.Duration;
import org.joda.time.Period;
import org.kalypso.commons.tokenreplace.IStringResolver;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.rcm.binding.IMetadata;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.model.rcm.internal.KalypsoModelRcmActivator;
import org.kalypso.model.rcm.util.RainfallGeneratorUtilities;
import org.kalypso.observation.util.ObservationHelper;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTupleModel;
import org.kalypso.ogc.sensor.metadata.MetadataHelper;
import org.kalypso.ogc.sensor.metadata.MetadataList;
import org.kalypso.ogc.sensor.request.IRequest;
import org.kalypso.ogc.sensor.request.ObservationRequest;
import org.kalypso.ogc.sensor.timeseries.forecast.ForecastFilter;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * This class does the real generation stuff.
 * 
 * @author Gernot Belger
 */
public class RainfallGenerationOp
{
  private final IStatusCollector m_generatorStati = new StatusCollector( KalypsoModelRcmActivator.PLUGIN_ID );

  private final ILog m_log;

  private final IStringResolver m_variables;

  private IObservation[] m_result;

  private final Feature[] m_catchments;

  private final List<IObservation>[] m_results;

  private final IMetadata[] m_metadata;

  private final IRainfallGenerator[] m_generators;

  /**
   * @param gmlContext
   *          If set to non-<code>null</code>, this location will be set to the rcm-workspace as context.
   * @param log
   *          If provided, the generators will write messages to this log.
   */
  public RainfallGenerationOp( final Feature[] catchments, final IRainfallGenerator[] generators, final IMetadata[] metadata, final IStringResolver variables, final ILog log )
  {
    m_catchments = catchments;
    m_generators = generators;
    m_metadata = metadata;
    m_variables = variables;
    m_log = log;

    m_results = initResults();
  }

  private List<IObservation>[] initResults( )
  {
    @SuppressWarnings("unchecked")
    final List<IObservation>[] results = new List[m_catchments.length];
    for( int i = 0; i < results.length; i++ )
      results[i] = new ArrayList<IObservation>();
    return results;
  }

  public IObservation[] getResult( )
  {
    return m_result;
  }

  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    /* Monitor. */
    final SubMonitor progress = SubMonitor.convert( monitor, "", m_generators.length * 10 + 2 * 3 + 10 );

    /* Generate the rainfall. */
    for( final IRainfallGenerator generator : m_generators )
    {
      try
      {
        /* Generate the rainfall for this generator. */
        doGeneration( generator, generator.getName(), progress.newChild( 10, SubMonitor.SUPPRESS_NONE ) );
      }
      catch( final Exception e )
      {
        final String msg = String.format( "Niederschlagserzeugung für Generator '%s' fehlgeschlagen", generator.getName() );
        final IStatus status = m_generatorStati.add( IStatus.WARNING, msg, e );
        m_log.log( status );
      }
    }

    /* Monitor. */
    progress.subTask( "Schreibe Zeitreihen" );

    /* Combine observations and write into target file while applying the targetFilter. */
    final IObservation[] combinedObservations = combineObservations();

    /* Add additional metadata, if wanted. */
    addAdditionalMetadata( combinedObservations );

    /* Monitor. */
    ProgressUtilities.worked( progress, 10 );

    /* Store the combined observations as result. */
    m_result = combinedObservations;

    return Status.OK_STATUS;
  }

  protected void doGeneration( final IRainfallGenerator generator, final String generatorLabel, final IProgressMonitor progress ) throws CoreException, SensorException
  {
    final DateRange range = generator.getPeriod( m_variables );
    final IObservation[] obses = generate( generator, range, progress );
    if( obses == null )
    {
      final String msg = String.format( "Niederschlagserzeugung für Generator '%s' liefert keine Ergebnisse und wird ingoriert", generatorLabel );
      m_generatorStati.add( IStatus.WARNING, msg );
    }
    else if( obses.length != m_results.length )
    {
      final String msg = String.format( "Niederschlagserzeugung für Generator '%s': Anzahl Ergebnisszeitreihen ist falsch und wird ingoriert", generatorLabel );
      m_generatorStati.add( IStatus.WARNING, msg );
    }
    else
    {
      for( int i = 0; i < obses.length; i++ )
      {
        final IObservation e = obses[i];
        if( e != null )
        {
          final IObservation resolvedObs = resolveObservation( e, range );
          m_results[i].add( resolvedObs );
        }
      }
    }
  }

  /**
   * Resolves all filters etc. and creates an new SimplObservationin memory, enforcing a given date range.<br>
   * TODO: move into ObservationUtilities
   */
  private IObservation resolveObservation( final IObservation o, final DateRange range ) throws SensorException
  {
    final IRequest request = new ObservationRequest( range );

    final String href = o.getHref();
    final String name = o.getName();
    final MetadataList metadataList = new MetadataList();
    metadataList.putAll( o.getMetadataList() );
    final ITupleModel values = o.getValues( request );
    final SimpleTupleModel clonedValues = new SimpleTupleModel( values, range );

    return new SimpleObservation( href, name, metadataList, clonedValues );
  }

  private IObservation[] combineObservations( ) throws CoreException
  {
    final IObservation[] result = new IObservation[m_results.length];
    for( int i = 0; i < result.length; i++ )
      result[i] = combineObservations( m_results[i] );

    return result;
  }

  private void addAdditionalMetadata( final IObservation[] combinedObservations )
  {
    if( combinedObservations == null )
      return;

    for( int i = 0; i < combinedObservations.length; i++ )
    {
      final IObservation observation = combinedObservations[i];
      if( observation == null )
        continue;

      /* All arrays must be in the same order and must have the same length. */
      final Feature feature = m_catchments[i];

      final MetadataList metadataList = observation.getMetadataList();

      for( final IMetadata metadata : m_metadata )
      {
        final String metadataName = metadata.getName();
        final String metadataValue = getMetadataValue( metadata, feature );
        if( metadataValue != null )
          metadataList.setProperty( metadataName, metadataValue );
      }
    }
  }

  private String getMetadataValue( final IMetadata metadata, final Feature feature )
  {
    final QName catchmentProperty = metadata.getCatchmentFroperty();
    if( catchmentProperty != null )
    {
      if( feature == null )
        return null;

      /* Get the metadata property. */
      final Object property = FeatureHelper.getPropertyLax( feature, catchmentProperty );
      if( property == null )
        return "-";

      return property.toString();
    }

    final String value = metadata.getValue();
    if( value == null )
      return null;

    return m_variables.resolve( value.toString() );
  }

  /**
   * Combines a list of observations into a single one.
   */
  public static IObservation combineObservations( final List<IObservation> observations ) throws CoreException
  {
    try
    {
      if( observations.isEmpty() )
        return null;

      if( observations.size() == 1 )
        return observations.get( 0 );

      final IObservation[] combine = observations.toArray( new IObservation[] {} );
      checkCombinedTimestep( combine );

      final ForecastFilter fc = new ForecastFilter();
      fc.initFilter( combine, combine[0], null );

      /* Clone and set the timestep. */
      final IObservation clonedObservation = ObservationHelper.clone( fc, null );
      final MetadataList metadataList = clonedObservation.getMetadataList();
      final String timestep = metadataList.getProperty( MetadataHelper.MD_TIMESTEP );
      if( timestep == null )
      {
        final String bestGuess = RainfallGeneratorUtilities.findTimeStep( combine );
        if( bestGuess != null )
          metadataList.setProperty( MetadataHelper.MD_TIMESTEP, bestGuess );
      }

      return clonedObservation;
    }
    catch( final SensorException e )
    {
      KalypsoModelRcmActivator.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }

    return null;
  }

  /**
   * Check if all timeseries that get combined here have the same timestep<br/>
   * Necessary, because the timestep of the combined timeseries is the timestep of the first timseries.
   */
  private static void checkCombinedTimestep( final IObservation[] observations ) throws CoreException
  {
    Period combinedTimestep = null;
    for( final IObservation observation : observations )
    {
      final Period currentTimestep = MetadataHelper.getTimestep( observation.getMetadataList() );

      boolean hasValues = hasValues( observation );
      if( currentTimestep == null && hasValues )
      {
        final IStatus status = new Status( IStatus.ERROR, KalypsoModelRcmActivator.PLUGIN_ID, "All timeseries involved in rainfall generation must have a timestep." );
        throw new CoreException( status );
      }

      if( combinedTimestep != null )
      {
        final Duration currentDuration = currentTimestep.toStandardDuration();
        final Duration combinedDuration = combinedTimestep.toStandardDuration();
        if( !currentDuration.equals( combinedDuration ) )
        {
          final String message = String.format( "All timeseries involed in rainfall generation must have the same timestep. Found: %s and %s", combinedTimestep, currentTimestep );
          final IStatus status = new Status( IStatus.ERROR, KalypsoModelRcmActivator.PLUGIN_ID, message );
          throw new CoreException( status );
        }
      }

      combinedTimestep = currentTimestep;
    }
  }

  private static boolean hasValues( IObservation observation )
  {
    try
    {
      return observation.getValues( null ).size() > 0;
    }
    catch( SensorException e )
    {
      e.printStackTrace();
      return false;
    }
  }

  /**
   * @param generatorDesc
   * @param rcmWorkspace
   * @param catchmentFeatures
   * @param from
   * @param to
   * @param logger
   *          To this log, the function will write its messages.
   * @param log
   *          If provided, the generators will write messages to this log.
   * @param monitor
   */
  private IObservation[] generate( final IRainfallGenerator rainGen, final DateRange range, final IProgressMonitor monitor ) throws CoreException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, 100 );

    final String generatorName = rainGen.getName();
    progress.subTask( generatorName );

    try
    {
      return rainGen.createRainfall( m_catchments, range, m_log, progress.newChild( 100, SubMonitor.SUPPRESS_NONE ) );
    }
    finally
    {
      ProgressUtilities.done( progress );
    }
  }
}
