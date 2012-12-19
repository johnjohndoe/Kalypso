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
package org.kalypso.ui.rrm.internal.cm.view;

import java.util.HashSet;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.java.net.UrlResolver;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.model.hydrology.binding.cm.ILinearSumGenerator;
import org.kalypso.model.hydrology.binding.timeseries.IStation;
import org.kalypso.model.hydrology.binding.timeseries.IStationCollection;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.model.hydrology.project.RrmProject;
import org.kalypso.model.hydrology.project.RrmScenario;
import org.kalypso.model.hydrology.timeseries.Timeserieses;
import org.kalypso.model.rcm.binding.IThiessenStation;
import org.kalypso.model.rcm.binding.IThiessenStationCollection;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ui.rrm.internal.IUiRrmWorkflowConstants;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Gernot Belger
 * @author Holger Albert
 */
public class InitThiessenTimeseriesOperation implements ICoreRunnableWithProgress
{
  private final LinearSumBean m_generator;

  public InitThiessenTimeseriesOperation( final LinearSumBean generator )
  {
    m_generator = generator;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    try
    {
      final IScenarioDataProvider scenarioDataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
      final IStationCollection modelStations = scenarioDataProvider.getModel( IUiRrmWorkflowConstants.SCENARIO_DATA_STATIONS );

      /* Create workspace */
      final GMLWorkspace stationsWorkspace = FeatureFactory.createGMLWorkspace( IThiessenStationCollection.FEATURE_THIESSEN_COLLECTION, null, GmlSerializer.DEFAULT_FACTORY );
      final IThiessenStationCollection collection = (IThiessenStationCollection) stationsWorkspace.getRootFeature();

      /* Hash already used timeseries */
      final Set<String> usedTimeseries = hashTimeseries();

      /* Fill timeseries into workspace */
      final IFeatureBindingCollection<IThiessenStation> stations = collection.getStations();
      addTimeseries( modelStations, stations, usedTimeseries );

      /* save workspace */
      final IContainer scenarioFolder = scenarioDataProvider.getScenarioFolder();
      final RrmScenario rrmScenario = new RrmScenario( scenarioFolder );
      final IFile thiessenFile = rrmScenario.getThiessenTempFile();

      GmlSerializer.serializeWorkspace( thiessenFile, stationsWorkspace, monitor );

      return Status.OK_STATUS;
    }
    catch( final GMLSchemaException e )
    {
      return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "Failed to create gml workspace", e ); //$NON-NLS-1$
    }
  }

  private Set<String> hashTimeseries( )
  {
    final Set<String> usedTimeseries = new HashSet<>();

    final CatchmentBean[] catchments = m_generator.getCatchments();
    for( final CatchmentBean catchment : catchments )
    {
      final FactorizedTimeseriesBean[] timeseries = catchment.getTimeseries();
      for( final FactorizedTimeseriesBean timeseriesBean : timeseries )
      {
        if( timeseriesBean.getFactor() > 0 )
        {
          final ITimeseries feature = timeseriesBean.getFeature();
          if( feature != null )
          {
            final ZmlLink dataLink = feature.getDataLink();
            if( dataLink != null )
              usedTimeseries.add( dataLink.getHref() );
          }
        }
      }
    }

    return usedTimeseries;
  }

  private void addTimeseries( final IStationCollection modelStations, final IFeatureBindingCollection<IThiessenStation> thiessenStations, final Set<String> usedTimeseries )
  {
    final IFeatureBindingCollection<IStation> stations = modelStations.getStations();
    for( final IStation station : stations )
    {
      final GM_Point location = station.getStationLocation();
      if( location != null )
      {
        final IFeatureBindingCollection<ITimeseries> stationTimeseries = station.getTimeseries();
        for( final ITimeseries timeseries : stationTimeseries )
          addTimeseries( timeseries, thiessenStations, usedTimeseries );
      }
    }
  }

  private void addTimeseries( final ITimeseries timeseries, final IFeatureBindingCollection<IThiessenStation> thiessenStations, final Set<String> usedTimeseries )
  {
    final IStation station = timeseries.getStation();

    final String stationParameterType = (String) m_generator.getProperty( ILinearSumGenerator.PROPERTY_PARAMETER_TYPE );
    if( StringUtils.isBlank( stationParameterType ) )
      return;

    final String timeseriesParameterType = timeseries.getParameterType();
    if( !stationParameterType.equals( timeseriesParameterType ) )
      return;

    /* Is valid? */
    final ZmlLink dataLink = timeseries.getDataLink();
    if( dataLink == null )
      return;

    final String dataHref = dataLink.getHref();
    if( StringUtils.isBlank( dataHref ) )
      return;

    final IThiessenStation newThiessenStation = thiessenStations.addNew( IThiessenStation.FEATURE_THIESSEN_STATION );

    /* Active, if existing model uses this timeseries */
    final boolean active = usedTimeseries.contains( dataHref );
    newThiessenStation.setActive( active );

    /* Link to underlying timeseries */
    final IPath stationsGmlPath = RrmProject.getStationsGmlPath();

    final String refId = timeseries.getId();
    final String href = String.format( "%s//%s#%s", UrlResolver.PROJECT_PROTOCOLL, stationsGmlPath.toPortableString(), refId ); //$NON-NLS-1$
    newThiessenStation.setStation( href );

    /* Copy properties from station and timeseries, it is hard to access them via gtt and gmt templates */
    newThiessenStation.setDescription( Timeserieses.toLinkLabel( timeseries ) );
    newThiessenStation.setName( station.getDescription() );
    newThiessenStation.setStationLocation( station.getStationLocation() );
  }
}