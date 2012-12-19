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
package org.kalypso.ui.rrm.internal.conversion.to12_02;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.util.Arrays;
import java.util.Date;
import java.util.Iterator;

import javax.xml.namespace.QName;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.joda.time.Interval;
import org.joda.time.LocalTime;
import org.joda.time.Period;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.commons.time.PeriodUtils;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.java.util.CalendarUtilities.FIELD;
import org.kalypso.model.hydrology.binding.timeseries.IStation;
import org.kalypso.model.hydrology.binding.timeseries.IStationCollection;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.model.hydrology.binding.timeseries.StationUtils;
import org.kalypso.model.hydrology.project.RrmProject;
import org.kalypso.model.hydrology.timeseries.HydrologyTimeseriesImportWorker;
import org.kalypso.model.hydrology.timeseries.StationClassesCatalog;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.metadata.MetadataHelper;
import org.kalypso.ogc.sensor.timeseries.AxisUtils;
import org.kalypso.ogc.sensor.timeseries.TimeseriesUtils;
import org.kalypso.ogc.sensor.util.Observations;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.timeseries.operations.StoreTimeseriesStatusOperation;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

import com.google.common.base.Charsets;

/**
 * Helper that imports the timeseries from the old 'Zeitreihen' folder into the new timeseries management.
 * 
 * @author Gernot Belger
 */
public class TimeseriesImporter
{
  private final TimeseriesIndex m_timeseriesIndex = new TimeseriesIndex();

  private final File m_sourceDir;

  private IStationCollection m_stations;

  private final IStatusCollector m_log;

  private final IParameterTypeIndex m_parameterIndex;

  private final File m_stationsFile;

  public TimeseriesImporter( final File sourceDir, final File targetDir, final IStatusCollector log, final IParameterTypeIndex parameterIndex )
  {
    m_log = log;
    m_parameterIndex = parameterIndex;
    m_sourceDir = new File( sourceDir, INaProjectConstants.FOLDER_ZEITREIHEN );

    final IPath stationsGmlPath = RrmProject.getStationsGmlPath();
    m_stationsFile = new File( targetDir, stationsGmlPath.toOSString() );
  }

  /** Read timeseries management */
  public void readStations( ) throws CoreException
  {
    try
    {
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( m_stationsFile, null );
      m_stations = (IStationCollection)workspace.getRootFeature();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), Messages.getString( "TimeseriesImporter_0" ), e ); //$NON-NLS-1$
      throw new CoreException( status );
    }
  }

  public void saveStations( ) throws CoreException
  {
    try
    {
      GmlSerializer.serializeWorkspace( m_stationsFile, m_stations.getWorkspace(), Charsets.UTF_8.name() );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), Messages.getString( "TimeseriesImporter_1" ), e ); //$NON-NLS-1$
      throw new CoreException( status );
    }
  }

  public void copyTimeseries( final IProgressMonitor monitor )
  {
    final String name = Messages.getString( "TimeseriesImporter_2", m_sourceDir.getName() ); //$NON-NLS-1$
    monitor.beginTask( name, IProgressMonitor.UNKNOWN );

    final File sourceTimeseriesDir = m_sourceDir;

    /* Return, if directory does not exist */
    if( !sourceTimeseriesDir.isDirectory() )
      return;

    final IStatusCollector stati = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    /* Find all .zml */
    final Iterator<File> zmlIterator = FileUtils.iterateFiles( sourceTimeseriesDir, new String[] { "zml" }, true ); //$NON-NLS-1$
    for( final Iterator<File> iterator = zmlIterator; iterator.hasNext(); )
    {
      final File zmlFile = iterator.next();
      monitor.subTask( zmlFile.getName() );

      try
      {
        stati.add( importZml( sourceTimeseriesDir, zmlFile ) );
      }
      catch( final CoreException e )
      {
        stati.add( e.getStatus() );
      }
      catch( final Exception e )
      {
        final String relativePath = FileUtilities.getRelativePathTo( sourceTimeseriesDir, zmlFile );

        final String message = String.format( Messages.getString( "TimeseriesImporter_4" ), relativePath ); //$NON-NLS-1$
        final IStatus status = new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), message, e );
        stati.add( status );
      }

      monitor.worked( 1 );
    }

    /* Log it */
    final String message = Messages.getString( "TimeseriesImporter_5", m_sourceDir.getName() ); //$NON-NLS-1$
    final IStatus status = stati.asMultiStatusOrOK( message, message );
    m_log.add( status );

    monitor.done();
  }

  private IStatus importZml( final File baseDir, final File zmlFile ) throws SensorException, CoreException, IOException
  {
    final IStatusCollector stati = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    final String baseName = FilenameUtils.removeExtension( zmlFile.getName() );
    final String relativePath = FileUtilities.getRelativePathTo( baseDir, zmlFile );

    /* Read and check observation. */
    IObservation observation = readObservation( zmlFile, relativePath );

    if( observation == null )
    {
      stati.add( IStatus.INFO, Messages.getString( "TimeseriesImporter.1" ) ); //$NON-NLS-1$

      return stati.asMultiStatus( String.format( Messages.getString( "TimeseriesImporter.0" ), baseName ) ); //$NON-NLS-1$
    }

    final IAxis[] axes = observation.getAxes();

    final IAxis dateAxis = AxisUtils.findDateAxis( axes );
    if( dateAxis == null )
    {
      final String message = String.format( Messages.getString( "TimeseriesImporter_6" ), relativePath ); //$NON-NLS-1$
      final IStatus status = new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), message );
      throw new CoreException( status );
    }

    final IAxis[] valueAxes = AxisUtils.findValueAxes( axes, true );
    if( valueAxes.length == 0 )
    {
      final String message = String.format( Messages.getString( "TimeseriesImporter_7" ), relativePath ); //$NON-NLS-1$
      final IStatus status = new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), message );
      throw new CoreException( status );
    }
    else if( valueAxes.length > 1 )
    {
      final String message = String.format( Messages.getString( "TimeseriesImporter_8" ), relativePath ); //$NON-NLS-1$
      final IStatus status = new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), message );
      throw new CoreException( status );
    }

    final IAxis valueAxis = valueAxes[0];
    final String parameterType = valueAxis.getType();

    final ITupleModel values = observation.getValues( null );
    final DateRange dateRange = Observations.findDateRange( values );

    /* Guess the timestep. */
    final Period timestep = TimeseriesUtils.guessTimestep( values );
    if( timestep != null )
    {
      final int amount = PeriodUtils.findCalendarAmount( timestep );
      final FIELD field = PeriodUtils.findCalendarField( timestep );
      MetadataHelper.setTimestep( observation.getMetadataList(), field.getField(), amount );
    }

    /* The timestamp is only relevant for day values. */
    final LocalTime timestamp = TimeseriesUtils.guessTimestamp( values, timestep );

    final RepairTimeseriesOperation repair = new RepairTimeseriesOperation( observation, timestep, timestamp, zmlFile.getName() );
    final MultiStatus repairStatus = repair.execute( new NullProgressMonitor() );
    /* add all children to avoid too deep status hierarchy */
    stati.addAll( Arrays.asList( repairStatus.getChildren() ) );

    observation = repair.getObservation();

    /* Assign station and timeseries parameters. */
    final String stationDescription = baseName;
    final String timeseriesDescription = baseName;
    final String groupName = findGroupName( relativePath );

    /* Add to timeseries management. */
    final IStation station = findOrCreateStation( stationDescription, groupName, parameterType, relativePath );

    /* Always create a new timeseries. */
    final ITimeseries newTimeseries = createTimeseries( station, timeseriesDescription, parameterType, timestep, dateRange );

    /* Copy observation file. */
    final ZmlLink dataLink = newTimeseries.getDataLink();

    /* We write the file from the read observation (instead of copy) */
    /* in order to compress the data and add status and source axes (now required). */
    final HydrologyTimeseriesImportWorker cleanupWorker = new HydrologyTimeseriesImportWorker( observation, dateRange );
    final IObservation observationWithSource = cleanupWorker.convert( newTimeseries.getTimestep(), timestamp );

    /* Save the observation. */
    dataLink.saveObservation( observationWithSource );

    /* Add as new entry into timeseries index (used later for catchment guessing). */
    final IPath sourceDirPath = new Path( m_sourceDir.getParent() );
    final IPath sourcePath = new Path( zmlFile.getAbsolutePath() );
    final IPath relativeSourcePath = sourcePath.makeRelativeTo( sourceDirPath );

    final Interval interval = toInterval( dateRange );

    final TimeseriesIndexEntry newEntry = new TimeseriesIndexEntry( relativeSourcePath, dataLink.getHref(), parameterType, timestep, timestamp, interval );
    m_timeseriesIndex.addEntry( newEntry );

    final MultiStatus status = stati.asMultiStatus( String.format( Messages.getString( "TimeseriesImporter.0" ), baseName ) ); //$NON-NLS-1$
    final StoreTimeseriesStatusOperation storeStatusOperation = new StoreTimeseriesStatusOperation( newTimeseries, status );
    stati.add( storeStatusOperation.execute( new NullProgressMonitor() ) );

    return status;
  }

  private Interval toInterval( final DateRange dateRange )
  {
    if( dateRange == null )
      return null;

    final Date to = dateRange.getTo();
    final Date from = dateRange.getFrom();

    if( Objects.isNull( from, to ) )
      return null;

    return new Interval( from.getTime(), to.getTime() );
  }

  private IObservation readObservation( final File zmlFile, final String relativePath ) throws SensorException, MalformedURLException
  {
    final IObservation observation = ZmlFactory.parseXML( zmlFile.toURI().toURL() );
    if( observation.isEmpty() )
      return null;

    final String forcedParmaterType = getForcedParameterType( observation, relativePath );
    if( forcedParmaterType == null )
      return observation;

    return ObservationUtilities.forceParameterType( observation, forcedParmaterType );
  }

  private String getForcedParameterType( final IObservation observation, final String relativePath )
  {
    final String parmeterType = m_parameterIndex.getParameterType( relativePath );
    if( StringUtils.equals( ITimeseriesConstants.TYPE_TEMPERATURE, parmeterType ) )
      return ITimeseriesConstants.TYPE_MEAN_TEMPERATURE;

    if( StringUtils.equals( ITimeseriesConstants.TYPE_EVAPORATION, parmeterType ) )
      return ITimeseriesConstants.TYPE_EVAPORATION_LAND_BASED;

    if( parmeterType == null )
    {
      final IAxis valueAxis = AxisUtils.findValueAxis( observation.getAxes(), true );
      if( valueAxis != null && StringUtils.equals( ITimeseriesConstants.TYPE_TEMPERATURE, valueAxis.getType() ) )
        return ITimeseriesConstants.TYPE_MEAN_TEMPERATURE;

      if( valueAxis != null && StringUtils.equals( ITimeseriesConstants.TYPE_EVAPORATION, valueAxis.getType() ) )
        return ITimeseriesConstants.TYPE_EVAPORATION_LAND_BASED;
    }

    return parmeterType;
  }

  private String findGroupName( final String relativePath )
  {
    final Path path = new Path( relativePath );
    if( path.segmentCount() < 2 )
      return null;

    final IPath relativeFolders = path.removeLastSegments( 1 );
    final String portableRelativeFolders = relativeFolders.toPortableString();
    return portableRelativeFolders.replace( Path.SEPARATOR, '_' );
  }

  private IStation findOrCreateStation( final String description, final String group, final String parameterType, final String relativePath ) throws CoreException
  {
    final QName stationType = StationClassesCatalog.getTypeFor( parameterType );
    if( stationType == null )
    {
      final String message = String.format( Messages.getString( "TimeseriesImporter_9" ), parameterType, relativePath ); //$NON-NLS-1$
      final IStatus status = new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), message );
      throw new CoreException( status );
    }

    // REMARK: always create a new station, because we can never have two stations with the same station name
    int count = 1;
    while( true )
    {
      final String stationName = buildNewStationName( description, count++ );

      /* Search for a station that does not exist */
      final IStation existingStation = findStation( stationName );
      if( existingStation == null )
        return createNewStation( stationName, group, stationType );
    }
  }

  private String buildNewStationName( final String description, final int count )
  {
    String stationName;
    if( count == 1 )
      stationName = description;
    else
      stationName = String.format( "%s (%s)", description, count ); //$NON-NLS-1$
    return stationName;
  }

  private IStation findStation( final String description )
  {
    final String foldername = StationUtils.getTimeseriesFoldername( description );

    // REMARK: linear search here, we assume we do not have too many stations...

    final IFeatureBindingCollection<IStation> stations = m_stations.getStations();
    for( final IStation station : stations )
    {
      if( foldername.equals( station.getTimeseriesFoldername() ) )
        return station;
    }

    return null;
  }

  private IStation createNewStation( final String description, final String group, final QName stationType )
  {
    final IFeatureBindingCollection<IStation> stations = m_stations.getStations();

    final IStation newStation = stations.addNew( stationType );
    newStation.setDescription( description );
    newStation.setComment( Messages.getString( "TimeseriesImporter_10" ) ); //$NON-NLS-1$
    newStation.setGroup( group );

    return newStation;
  }

  private ITimeseries createTimeseries( final IStation station, final String timeseriesDescription, final String parameterType, final Period timestep, final DateRange daterange )
  {
    final ITimeseries newTimeseries = station.getTimeseries().addNew( ITimeseries.FEATURE_TIMESERIES );
    newTimeseries.setDescription( timeseriesDescription );

    final String quality = findUniqueQuality( station, parameterType, timestep );

    newTimeseries.setParameterType( parameterType );
    newTimeseries.setQuality( quality );
    newTimeseries.setTimestep( timestep );
    newTimeseries.setMeasurementStart( daterange.getFrom() );
    newTimeseries.setMeasurementEnd( daterange.getTo() );

    return newTimeseries;
  }

  /**
   * @return unique quality string - sometimes a time series with the same parameter type, time step, ... exists multi
   *         times in a project
   */
  private String findUniqueQuality( final IStation station, final String parameterType, final Period timestep )
  {
    final FindUniqueQualityVisitor visitor = new FindUniqueQualityVisitor( parameterType, timestep );

    final IFeatureBindingCollection<ITimeseries> timeseries = station.getTimeseries();
    timeseries.accept( visitor );

    final String base = Messages.getString( "TimeseriesImporter_11" ); //$NON-NLS-1$
    int count = 1;
    String quality = base;
    while( visitor.hasQuality( quality ) )
    {
      quality = String.format( "%s (%d)", base, count ); //$NON-NLS-1$
      count++;
    }

    return quality;
  }

  public TimeseriesIndex getIndex( )
  {
    return m_timeseriesIndex;
  }
}