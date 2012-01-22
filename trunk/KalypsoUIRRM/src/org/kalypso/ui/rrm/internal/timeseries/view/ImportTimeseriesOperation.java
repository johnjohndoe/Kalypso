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
package org.kalypso.ui.rrm.internal.timeseries.view;

import java.io.File;
import java.util.HashMap;
import java.util.Map;
import java.util.TimeZone;

import javax.xml.namespace.QName;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.joda.time.Period;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.commons.time.PeriodUtils;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.java.net.UrlResolver;
import org.kalypso.contribs.java.util.CalendarUtilities.FIELD;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.project.INaProjectConstants;
import org.kalypso.model.hydrology.timeseries.TimeseriesImportWorker;
import org.kalypso.model.hydrology.timeseries.binding.IStation;
import org.kalypso.model.hydrology.timeseries.binding.ITimeseries;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.adapter.INativeObservationAdapter;
import org.kalypso.ogc.sensor.metadata.MetadataHelper;
import org.kalypso.ogc.sensor.metadata.MetadataList;
import org.kalypso.ogc.sensor.timeseries.TimeseriesUtils;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ui.editor.gmleditor.command.AddFeatureCommand;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypso.zml.ui.KalypsoZmlUI;
import org.kalypso.zml.ui.imports.ImportObservationData;

/**
 * @author Gernot Belger
 */
public class ImportTimeseriesOperation implements ICoreRunnableWithProgress
{
  private final ImportObservationData m_data;

  private final IStation m_station;

  private final TimeseriesBean m_bean;

  private final CommandableWorkspace m_workspace;

  private ITimeseries m_timeseries;

  public ImportTimeseriesOperation( final CommandableWorkspace workspace, final IStation station, final ImportObservationData data, final TimeseriesBean bean )
  {
    m_workspace = workspace;
    m_station = station;
    m_data = data;
    m_bean = bean;
  }

  /**
   * Updates some of the bean data before the execute method is called in another thread.<br/>
   * This is needed, because the bean stuff is not allowed to be called outside the swt thread.
   */
  void updateDataAfterFinish( )
  {
    m_bean.setProperty( ITimeseries.PROPERTY_PARAMETER_TYPE, m_data.getParameterType() );
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    final File fileSource = m_data.getSourceFileData().getFile();

    final IObservation observation = createObservation( fileSource );

    final Period timestep = findTimestep( observation );

    final IFile targetFile = createDataFile( m_bean, timestep );

    m_timeseries = createTimeseries( timestep, targetFile );

    updateMetadata( observation, m_timeseries );

    final TimeseriesImportWorker cleanupWorker = new TimeseriesImportWorker( observation, fileSource.getAbsolutePath() );
    final IObservation observationWithSource = cleanupWorker.convert();

    writeResult( targetFile, observationWithSource );

    return Status.OK_STATUS;
  }

  private Period findTimestep( final IObservation observation ) throws CoreException
  {
    final String message = "Failed to determine timestep";

    try
    {
      final Period timestep = TimeseriesUtils.guessTimestep( observation.getValues( null ) );
      if( timestep == null )
      {
        final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), message );
        throw new CoreException( status );
      }

      return timestep;
    }
    catch( final SensorException e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), message, e );
      throw new CoreException( status );
    }
  }

  private void updateMetadata( final IObservation observation, final ITimeseries timeseries )
  {
    /* Timestep */
    final MetadataList metadataList = observation.getMetadataList();
    MetadataHelper.setTimestep( metadataList, timeseries.getTimestep() );
  }

  private ITimeseries createTimeseries( final Period timestep, final IFile targetFile ) throws CoreException
  {
    try
    {
      final String projectPath = buildTargetPath( targetFile );

      /* Create timeseries feature */
      final IRelationType parentRelation = (IRelationType) m_station.getFeatureType().getProperty( IStation.MEMBER_TIMESERIES );

      final int timestepAmount = PeriodUtils.findCalendarAmount( timestep );
      final FIELD timestepField = PeriodUtils.findCalendarField( timestep );

      if( timestepAmount == Integer.MAX_VALUE || timestepField == null )
      {
        final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "Failed to determine timestep of timeseries" );
        throw new CoreException( status );
      }

      final TimeseriesLinkType dataLink = new TimeseriesLinkType();
      dataLink.setHref( projectPath );

      final Map<QName, Object> properties = new HashMap<>( m_bean.getProperties() );
      properties.put( ITimeseries.PROPERTY_TIMESTEP_AMOUNT, timestepAmount );
      properties.put( ITimeseries.PROPERTY_TIMESTEP_FIELD, timestepField.name() );
      properties.put( ITimeseries.PROPERTY_DATA, dataLink );

      final AddFeatureCommand command = new AddFeatureCommand( m_workspace, ITimeseries.FEATURE_TIMESERIES, m_station, parentRelation, -1, properties, null, -1 );

      m_workspace.postCommand( command );

      return (ITimeseries) command.getNewFeature();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "Failed to create new timeseries definition", e );
      throw new CoreException( status );
    }
  }

  private String buildTargetPath( final IFile targetFile )
  {
    final IPath projectRelativePath = targetFile.getProjectRelativePath();
    final String projectPath = UrlResolver.PROJECT_PROTOCOLL + "//" + projectRelativePath.toPortableString();

    return projectPath;
  }

  private void writeResult( final IFile targetFile, final IObservation newObservation ) throws CoreException
  {
    try
    {
      targetFile.getLocation().toFile().getParentFile().mkdirs();
      ZmlFactory.writeToFile( newObservation, targetFile );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, KalypsoZmlUI.PLUGIN_ID, "Failed to write result file", e );
      throw new CoreException( status );
    }
  }

  private IObservation createObservation( final File fileSource ) throws CoreException
  {
    try
    {
      final TimeZone timezone = m_data.getTimezoneParsed();
      final INativeObservationAdapter nativaAdapter = m_data.getAdapter();
      final String parameterType = m_data.getParameterType();

      return nativaAdapter.importTimeseries( fileSource, timezone, parameterType, false );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final String message = String.format( "Failed to import timeseries" );
      final IStatus status = new Status( IStatus.ERROR, KalypsoZmlUI.PLUGIN_ID, message, e );
      throw new CoreException( status );
    }
  }

  private IFile createDataFile( final TimeseriesBean timeseries, final Period timestep ) throws CoreException
  {
    final String parameterType = (String) timeseries.getProperty( ITimeseries.PROPERTY_PARAMETER_TYPE );
    final String quality = (String) timeseries.getProperty( ITimeseries.PROPERTY_QUALITY );

    final String stationFoldername = m_station.getTimeseriesFoldername();
    final String timeseriesFilename = TimeseriesBean.formatTimeseriesFilename( parameterType, quality, timestep );

    final SzenarioDataProvider scenarioDataProvider = ScenarioHelper.getScenarioDataProvider();
    final IProject project = scenarioDataProvider.getScenarioFolder().getProject();
    final IFolder timeseriesFolder = project.getFolder( INaProjectConstants.PATH_TIMESERIES );
    final IFolder stationFolder = timeseriesFolder.getFolder( stationFoldername );

    return stationFolder.getFile( timeseriesFilename );
  }

  public ITimeseries getTimeseries( )
  {
    return m_timeseries;
  }

  public ImportObservationData getData( )
  {
    return m_data;
  }
}
