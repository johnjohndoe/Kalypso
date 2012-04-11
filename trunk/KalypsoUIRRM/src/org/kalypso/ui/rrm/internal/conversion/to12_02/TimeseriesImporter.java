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
import java.util.Iterator;

import javax.xml.namespace.QName;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.joda.time.LocalTime;
import org.joda.time.Period;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.hydrology.project.INaProjectConstants;
import org.kalypso.model.hydrology.timeseries.StationClassesCatalog;
import org.kalypso.model.hydrology.timeseries.TimeseriesImportWorker;
import org.kalypso.model.hydrology.timeseries.binding.IStation;
import org.kalypso.model.hydrology.timeseries.binding.IStationCollection;
import org.kalypso.model.hydrology.timeseries.binding.ITimeseries;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTupleModel;
import org.kalypso.ogc.sensor.metadata.MetadataHelper;
import org.kalypso.ogc.sensor.metadata.MetadataList;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.timeseries.AxisUtils;
import org.kalypso.ogc.sensor.timeseries.TimeseriesUtils;
import org.kalypso.ogc.sensor.timeseries.datasource.DataSourceHelper;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
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

  private final File m_timeseriesDir;

  private IStationCollection m_stations;

  private final IStatusCollector m_log;

  private final IParameterTypeIndex m_parameterIndex;

  public TimeseriesImporter( final File sourceDir, final File targetDir, final IStatusCollector log, final IParameterTypeIndex parameterIndex )
  {
    m_log = log;
    m_parameterIndex = parameterIndex;
    m_sourceDir = new File( sourceDir, INaProjectConstants.FOLDER_ZEITREIHEN );
    m_timeseriesDir = new File( targetDir, INaProjectConstants.PATH_TIMESERIES );
  }

  /** Read timeseries management */
  public void readStations( ) throws CoreException
  {
    try
    {
      final File stationsFile = new File( m_timeseriesDir, INaProjectConstants.GML_STATIONS );
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( stationsFile, null );
      m_stations = (IStationCollection) workspace.getRootFeature();
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
      final File stationsFile = new File( m_timeseriesDir, INaProjectConstants.GML_STATIONS );
      GmlSerializer.serializeWorkspace( stationsFile, m_stations.getWorkspace(), Charsets.UTF_8.name() );
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
        importZml( sourceTimeseriesDir, zmlFile );
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

  private void importZml( final File baseDir, final File zmlFile ) throws SensorException, CoreException, IOException
  {
    final String baseName = FilenameUtils.removeExtension( zmlFile.getName() );
    final String relativePath = FileUtilities.getRelativePathTo( baseDir, zmlFile );

    /* Read and check observation. */
    final IObservation observation = readObservation( zmlFile, relativePath );
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

    if( valueAxes.length > 1 )
    {
      final String message = String.format( Messages.getString( "TimeseriesImporter_8" ), relativePath ); //$NON-NLS-1$
      final IStatus status = new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), message );
      throw new CoreException( status );
    }

    final IAxis valueAxis = valueAxes[0];
    final String parameterType = valueAxis.getType();

    final ITupleModel values = observation.getValues( null );

    /* Guess the timestep. */
    final Period timestep = TimeseriesUtils.guessTimestep( values );

    /* The timestamp is only relevant for day values. */
    final LocalTime timestamp = TimeseriesUtils.guessTimestamp( values, timestep );

    /* Assign station and timeseries parameters. */
    final String stationDescription = baseName;
    final String timeseriesDescription = baseName;
    final String groupName = findGroupName( relativePath );

    /* Add to timeseries management. */
    final IStation station = findOrCreateStation( stationDescription, groupName, parameterType, relativePath );

    /* Always create a new timeseries. */
    final ITimeseries newTimeseries = createTimeseries( station, timeseriesDescription, parameterType, timestep );

    /* Copy observation file. */
    final ZmlLink dataLink = newTimeseries.getDataLink();

    /* We write the file from the read observation (instead of copy) */
    /* in order to compress the data and add status and source axes (now required). */
    final TimeseriesImportWorker cleanupWorker = new TimeseriesImportWorker( observation );
    final IObservation observationWithSource = cleanupWorker.convert( newTimeseries.getTimestep(), timestamp );

    /* Save the observation. */
    dataLink.saveObservation( observationWithSource );

    /* Add as new entry into timeseries index (used later for catchment guessing). */
    final IPath sourceDirPath = new Path( m_sourceDir.getParent() );
    final IPath sourcePath = new Path( zmlFile.getAbsolutePath() );
    final IPath relativeSourcePath = sourcePath.makeRelativeTo( sourceDirPath );

    final TimeseriesIndexEntry newEntry = new TimeseriesIndexEntry( relativeSourcePath, dataLink.getHref(), parameterType, timestep, timestamp );
    m_timeseriesIndex.addEntry( newEntry );
  }

  private IObservation readObservation( final File zmlFile, final String relativePath ) throws SensorException, MalformedURLException
  {
    final IObservation observation = ZmlFactory.parseXML( zmlFile.toURI().toURL() );

    final String forcedParmaterType = m_parameterIndex.getParmaterType( relativePath );
    if( forcedParmaterType == null )
      return observation;

    /* Force the parameter type for evaporation and temperature */
    final ITupleModel tupleModel = observation.getValues( null );
    final Object[][] rawData = getRawData( tupleModel );

    /* Exchange old value type with forced parameter type */
    final IAxis[] axes = tupleModel.getAxes();
    final IAxis[] forcedAxes = new IAxis[axes.length];
    for( int i = 0; i < forcedAxes.length; i++ )
      forcedAxes[i] = getForcedParameterAxes( axes[i], forcedParmaterType );

    final SimpleTupleModel newModel = new SimpleTupleModel( forcedAxes, rawData );

    /* Create and return new obs */
    final String name = observation.getName();
    final String href = observation.getHref();
    final MetadataList metadata = MetadataHelper.clone( observation.getMetadataList() );
    return new SimpleObservation( href, name, metadata, newModel );
  }

  // REMARK: heavy, but necessary as we cannot assume that we always have a simple tuple model
  // For older models, we often have ZmlTupleModels instead.
  private Object[][] getRawData( final ITupleModel tupleModel ) throws SensorException
  {
    final int size = tupleModel.size();
    final IAxis[] axes = tupleModel.getAxes();

    final Object[][] rawData = new Object[size][];

    for( int i = 0; i < rawData.length; i++ )
    {
      rawData[i] = new Object[axes.length];

      for( int a = 0; a < axes.length; a++ )
        rawData[i][a] = tupleModel.get( i, axes[a] );
    }

    return rawData;
  }

  private IAxis getForcedParameterAxes( final IAxis axis, final String forcedParmaterType )
  {
    if( AxisUtils.isValueAxis( axis ) )
      return TimeseriesUtils.createDefaultAxis( forcedParmaterType );

    if( AxisUtils.isStatusAxis( axis ) )
    {
      final IAxis tempAxis = TimeseriesUtils.createDefaultAxis( forcedParmaterType );
      return KalypsoStatusUtils.createStatusAxisFor( tempAxis, true );
    }

    if( AxisUtils.isDataSrcAxis( axis ) )
    {
      final IAxis tempAxis = TimeseriesUtils.createDefaultAxis( forcedParmaterType );
      return DataSourceHelper.createSourceAxis( tempAxis );
    }

    /* Default: do nothing */
    return axis;
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

    final IStation existingStation = findStation( description, stationType );
    if( existingStation != null )
      return existingStation;

    return createNewStation( description, group, stationType );
  }

  private IStation findStation( final String description, final QName stationType )
  {
    // REMARK: linear search here, we assume we do not have too many stations...

    final IFeatureBindingCollection<IStation> stations = m_stations.getStations();
    for( final IStation station : stations )
    {
      if( station.getFeatureType().getQName().equals( stationType ) )
      {
        if( description.equalsIgnoreCase( station.getDescription() ) )
          return station;
      }
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

  private ITimeseries createTimeseries( final IStation station, final String timeseriesDescription, final String parameterType, final Period timestep )
  {
    final ITimeseries newTimeseries = station.getTimeseries().addNew( ITimeseries.FEATURE_TIMESERIES );
    newTimeseries.setDescription( timeseriesDescription );

    final String quality = Messages.getString( "TimeseriesImporter_11" ); //$NON-NLS-1$

    newTimeseries.setParameterType( parameterType );
    newTimeseries.setQuality( quality );
    newTimeseries.setTimestep( timestep );

    return newTimeseries;
  }

  public TimeseriesIndex getIndex( )
  {
    return m_timeseriesIndex;
  }
}