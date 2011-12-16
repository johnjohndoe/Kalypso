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
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.joda.time.Period;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.hydrology.project.INaProjectConstants;
import org.kalypso.model.hydrology.timeseries.StationClassesCatalog;
import org.kalypso.model.hydrology.timeseries.binding.IStation;
import org.kalypso.model.hydrology.timeseries.binding.IStationCollection;
import org.kalypso.model.hydrology.timeseries.binding.ITimeseries;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.timeseries.AxisUtils;
import org.kalypso.ogc.sensor.timeseries.TimeseriesUtils;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.timeseries.view.TimeseriesBean;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * Helper that imports the timeserties from the old 'Zeitreihen' folder into the new timeseries management.
 *
 * @author Gernot Belger
 */
public class TimeseriesImporter
{
  private final File m_sourceDir;

  private final File m_timeseriesDir;

  private IStationCollection m_stations;

  private final IStatusCollector m_log;

  public TimeseriesImporter( final File sourceDir, final File targetDir, final IStatusCollector log )
  {
    m_log = log;
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
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "Failed to read stations.gml", e );
      throw new CoreException( status );
    }
  }

  public void copyTimeseries( final String folder )
  {
    final File sourceTimeseriesDir = new File( m_sourceDir, folder );

    final IStatusCollector stati = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    /* Find all .zml */
    final Iterator<File> zmlIterator = FileUtils.iterateFiles( sourceTimeseriesDir, new String[] { "zml" }, true );
    for( final Iterator<File> iterator = zmlIterator; iterator.hasNext(); )
    {
      try
      {
        final File zmlFile = iterator.next();
        importZml( sourceTimeseriesDir, zmlFile );
      }
      catch( final MalformedURLException e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
      catch( final SensorException e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
      catch( final CoreException e )
      {
        stati.add( e.getStatus() );
      }
      catch( final IOException e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
    }

    /* Log it */
    final String message = String.format( "Importing timeseries from '%s'", folder );
    final IStatus status = stati.asMultiStatusOrOK( message, message );
    m_log.add( status );
  }

  private void importZml( final File baseDir, final File zmlFile ) throws SensorException, CoreException, IOException
  {
    /* Guess station and other parameters */
    final String baseName = FilenameUtils.removeExtension( zmlFile.getName() );
    final String relativePath = FileUtilities.getRelativePathTo( baseDir, zmlFile );

    /* Read and check observation */
    final IObservation observation = ZmlFactory.parseXML( zmlFile.toURI().toURL() );
    final IAxis[] axes = observation.getAxes();

    final IAxis dateAxis = AxisUtils.findDateAxis( axes );
    if( dateAxis == null )
    {
      final String message = String.format( "Skipped timeseries '%s': no date axis", relativePath );
      final IStatus status = new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), message );
      throw new CoreException( status );
    }

    final IAxis[] valueAxes = AxisUtils.findValueAxes( axes, false );

    if( valueAxes.length == 0 )
    {
      final String message = String.format( "Skipped timeseries '%s': no value axis", relativePath );
      final IStatus status = new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), message );
      throw new CoreException( status );
    }

    if( valueAxes.length > 0 )
    {
      final String message = String.format( "Failed to import timeseries '%s': more than one value axis.", relativePath );
      final IStatus status = new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), message );
      throw new CoreException( status );
    }

    final IAxis valueAxis = valueAxes[0];
    final String parameterType = valueAxis.getType();

    final Period timestep = TimeseriesUtils.guessTimestep( observation.getValues( null ) );

    /* Assign station and timeseries parameters */
    final String stationDescription = baseName;
    final String timeseriesDescription = baseName;
    final String groupName = relativePath;

    /* Copy zml file */

    /* Add to timeseries management */
    final IStation station = findOrCreateStation( stationDescription, groupName, parameterType );

    /* Always create a new timeseries */
    final ITimeseries newTimeseries = createTimeseries( station, timeseriesDescription, parameterType, timestep );

    /* copy observation file */
    final ZmlLink dataLink = newTimeseries.getDataLink();
    final File timeseriesFile = dataLink.getJavaFile();
    FileUtils.copyFile( zmlFile, timeseriesFile );
  }

  private IStation findOrCreateStation( final String description, final String group, final String parameterType ) throws CoreException
  {
    final QName stationType = StationClassesCatalog.getTypeFor( parameterType );
    if( stationType == null )
    {
      final String message = String.format( "Unsupported parameter type '%s' for timeseries '%s'", parameterType );
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
    newStation.setComment( "Created by project conversion" );
    newStation.setGroup( group );

    return newStation;
  }

  private ITimeseries createTimeseries( final IStation station, final String timeseriesDescription, final String parameterType, final Period timestep )
  {
    final ITimeseries newTimeseries = station.getTimeseries().addNew( ITimeseries.FEATURE_TIMESERIES );
    newTimeseries.setDescription( timeseriesDescription );

    final String quality = "Project Conversion";

    newTimeseries.setParameterType( parameterType );
    newTimeseries.setQuality( quality );
    newTimeseries.setTimestep( timestep );

    final String stationFoldername = station.getTimeseriesFoldername();
    final String timeseriesFilename = TimeseriesBean.formatTimeseriesFilename( parameterType, quality, timestep );

    final String timeseriesPath = stationFoldername + IPath.SEPARATOR + timeseriesFilename;

    newTimeseries.setDataLink( timeseriesPath );

    return newTimeseries;
  }
}