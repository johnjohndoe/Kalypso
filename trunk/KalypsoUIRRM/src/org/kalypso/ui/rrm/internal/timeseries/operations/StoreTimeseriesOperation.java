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
package org.kalypso.ui.rrm.internal.timeseries.operations;

import java.util.HashMap;
import java.util.Map;

import javax.xml.namespace.QName;

import org.apache.commons.io.IOCase;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.joda.time.Period;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.commons.time.PeriodUtils;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.java.net.UrlResolver;
import org.kalypso.contribs.java.util.CalendarUtilities.FIELD;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.binding.timeseries.IStation;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.model.hydrology.project.RrmProject;
import org.kalypso.model.hydrology.timeseries.Timeserieses;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.metadata.MetadataHelper;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ui.editor.gmleditor.command.AddFeatureCommand;
import org.kalypso.ui.rrm.internal.IUiRrmWorkflowConstants;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.timeseries.view.imports.IImportTimeseriesOperation;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypso.zml.ui.KalypsoZmlUI;
import org.kalypso.zml.ui.imports.IStoreObservationData;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Gernot Belger
 */
public class StoreTimeseriesOperation implements ICoreRunnableWithProgress
{
  private final IStation m_station;

  private ITimeseries m_timeseries;

  private final IImportTimeseriesOperation m_operation;

  public StoreTimeseriesOperation( final IStation station, final IImportTimeseriesOperation importOperation )
  {
    m_station = station;
    m_operation = importOperation;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    final IStatusCollector stati = new StatusCollector( KalypsoCorePlugin.getID() );

    final IObservation observation = m_operation.getObservation();
    final Period timestep = m_operation.getTimestep();
    if( Objects.isNull( observation, timestep ) )
      return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), Messages.getString( "StoreTimeseriesOperation.2" ) ); //$NON-NLS-1$

    final DateRange daterange = m_operation.getDateRange();

    final String description = m_operation.getDescription();
    final String quality = m_operation.getQuality();
    final String parameterType = m_operation.getData().getParameterType();
    if( m_station.hasTimeseries( parameterType, quality, timestep ) )
    {
      final String message = Messages.getString( "TargetTimeseriesValidator_0" ); //$NON-NLS-1$
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), message );
      throw new CoreException( status );
    }

    final IFile targetFile = createDataFile( quality, parameterType, timestep, stati );
    m_timeseries = createTimeseries( timestep, targetFile, daterange, parameterType, quality, description );

    writeResult( targetFile, observation, daterange );

    return stati.asMultiStatus( Messages.getString( "StoreTimeseriesOperation.0" ) ); //$NON-NLS-1$
  }

  private ITimeseries createTimeseries( final Period timestep, final IFile targetFile, final DateRange daterange, final String parameterType, final String quality, final String description ) throws CoreException
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
        final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), Messages.getString( "ImportTimeseriesOperation_1" ) ); //$NON-NLS-1$
        throw new CoreException( status );
      }

      final TimeseriesLinkType dataLink = new TimeseriesLinkType();
      dataLink.setHref( projectPath );

      final Map<QName, Object> properties = new HashMap<>();
      properties.put( ITimeseries.QN_DESCRIPTION, description );
      properties.put( ITimeseries.PROPERTY_TIMESTEP_AMOUNT, timestepAmount );
      properties.put( ITimeseries.PROPERTY_TIMESTEP_FIELD, timestepField.name() );
      properties.put( ITimeseries.PROPERTY_DATA, dataLink );
      properties.put( ITimeseries.PROPERTY_MEASUREMENT_START, DateUtilities.toXMLGregorianCalendar( daterange.getFrom() ) );
      properties.put( ITimeseries.PROPERTY_MEASUREMENT_END, DateUtilities.toXMLGregorianCalendar( daterange.getTo() ) );
      properties.put( ITimeseries.PROPERTY_QUALITY, quality );
      properties.put( ITimeseries.PROPERTY_PARAMETER_TYPE, parameterType );

      final IScenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
      final CommandableWorkspace stationsWorkspace = dataProvider.getCommandableWorkSpace( IUiRrmWorkflowConstants.SCENARIO_DATA_STATIONS );

      final AddFeatureCommand command = new AddFeatureCommand( stationsWorkspace, ITimeseries.FEATURE_TIMESERIES, m_station, parentRelation, -1, properties, null, -1 );

      stationsWorkspace.postCommand( command );

      return (ITimeseries) command.getNewFeature();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), Messages.getString( "ImportTimeseriesOperation_2" ), e ); //$NON-NLS-1$
      throw new CoreException( status );
    }
  }

  private String buildTargetPath( final IFile targetFile )
  {
    final IPath projectRelativePath = targetFile.getProjectRelativePath();
    final String projectPath = UrlResolver.PROJECT_PROTOCOLL + "//" + projectRelativePath.toPortableString(); //$NON-NLS-1$

    return projectPath;
  }

  private void writeResult( final IFile targetFile, final IObservation newObservation, final DateRange daterange ) throws CoreException
  {
    try
    {
      MetadataHelper.setTargetDateRange( newObservation.getMetadataList(), daterange );

      targetFile.getLocation().toFile().getParentFile().mkdirs();
      ZmlFactory.writeToFile( newObservation, targetFile );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, KalypsoZmlUI.PLUGIN_ID, Messages.getString( "ImportTimeseriesOperation_4" ), e ); //$NON-NLS-1$
      throw new CoreException( status );
    }
  }

  private IFile createDataFile( final String quality, final String parameterType, final Period timestep, final IStatusCollector stati )
  {
    final String stationFoldername = m_station.getTimeseriesFoldername();
    final String timeseriesFilename = Timeserieses.formatTimeseriesFilename( parameterType, quality, timestep );

    // FIXME overwrite existing file add warning state!
    if( fileNameExists( timeseriesFilename ) )
    {
      stati.add( IStatus.WARNING, Messages.getString( "StoreTimeseriesOperation.1" ) ); //$NON-NLS-1$
    }

    final IProject project = ScenarioHelper.getScenarioFolder().getProject();
    final RrmProject rrmProject = new RrmProject( project );

    final IFolder timeseriesFolder = rrmProject.getTimeseriesFolder();

    final IFolder stationFolder = timeseriesFolder.getFolder( stationFoldername );

    return stationFolder.getFile( timeseriesFilename );
  }

  private boolean fileNameExists( final String file )
  {
    final IStoreObservationData data = m_operation.getData();
    final String[] existing = data.getExistingTimeserieses();
    for( final String exists : existing )
    {
      if( IOCase.SYSTEM.checkEquals( exists, file ) )
        return true;
    }

    return false;
  }

  public ITimeseries getTimeseries( )
  {
    return m_timeseries;
  }
}