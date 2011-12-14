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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.commons.command.ICommand;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.project.INaProjectConstants;
import org.kalypso.model.hydrology.timeseries.binding.IStation;
import org.kalypso.model.hydrology.timeseries.binding.IStationCollection;
import org.kalypso.model.hydrology.timeseries.binding.ITimeseries;
import org.kalypso.model.rcm.binding.ICatchment;
import org.kalypso.model.rcm.binding.IFactorizedTimeseries;
import org.kalypso.model.rcm.binding.ILinearSumGenerator;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ui.editor.gmleditor.util.command.AddFeatureCommand;
import org.kalypso.ui.rrm.internal.IUiRrmWorkflowConstants;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Gernot Belger
 * @author Holger Albert
 */
public class CatchmentBean extends FeatureBean<ICatchment>
{
  private final Map<String, FactorizedTimeseriesBean> m_cache;

  private final List<FactorizedTimeseriesBean> m_timeseries;

  private String m_catchmentRef;

  public CatchmentBean( )
  {
    super( ICatchment.FEATURE_CATCHMENT );

    m_cache = new HashMap<String, FactorizedTimeseriesBean>();
    m_timeseries = new ArrayList<FactorizedTimeseriesBean>();
    m_catchmentRef = null;
  }

  public CatchmentBean( final ICatchment catchment )
  {
    super( catchment );

    m_cache = new HashMap<String, FactorizedTimeseriesBean>();
    m_timeseries = new ArrayList<FactorizedTimeseriesBean>();
    final Feature areaLink = catchment.getAreaLink();
    m_catchmentRef = areaLink == null ? null : areaLink.getId();

    initTimeseries();
  }

  public FactorizedTimeseriesBean[] getTimeseries( )
  {
    return m_timeseries.toArray( new FactorizedTimeseriesBean[] {} );
  }

  public String getCatchmentRef( )
  {
    return m_catchmentRef;
  }

  public void setCatchmentRef( final String catchmentRef )
  {
    m_catchmentRef = catchmentRef;
  }

  public String getLabel( )
  {
    return (String) getProperty( Feature.QN_DESCRIPTION );
  }

  public void apply( CommandableWorkspace workspace, Feature parent ) throws Exception
  {
    // TODO

    /* Get the feature. */
    ICatchment feature = getFeature();
    if( feature == null )
    {
      /* The catchment feature does not exist. */
      Map<QName, Object> properties = new HashMap<>( getProperties() );
      properties.put( ICatchment.PROPERTY_AREA_LINK, INaProjectConstants.GML_MODELL_FILE + "#" + m_catchmentRef );
      ILinearSumGenerator collection = (ILinearSumGenerator) parent;
      IRelationType parentRelation = (IRelationType) collection.getFeatureType().getProperty( ILinearSumGenerator.MEMBER_CATCHMENT );
      QName type = getFeatureType().getQName();

      /* Create the add feature command. */
      AddFeatureCommand command = new AddFeatureCommand( workspace, type, collection, parentRelation, -1, properties, null, -1 );

      /* Post the command. */
      workspace.postCommand( command );

      /* Apply also all contained timeseries. */
      for( FactorizedTimeseriesBean timeseriesBean : m_timeseries )
        timeseriesBean.apply( workspace, command.getNewFeature() );
    }
    else
    {
      /* The catchment feature does already exist. */
      ICommand command = applyChanges();

      /* Post the command. */
      workspace.postCommand( command );

      /* Apply also all contained timeseries. */
      for( FactorizedTimeseriesBean timeseriesBean : m_timeseries )
        timeseriesBean.apply( workspace, feature );
    }
  }

  private void initTimeseries( )
  {
    try
    {
      /* Get the stations. */
      final SzenarioDataProvider scenarioDataProvider = ScenarioHelper.getScenarioDataProvider();
      final IStationCollection stationCollection = scenarioDataProvider.getModel( IUiRrmWorkflowConstants.SCENARIO_DATA_STATIONS, IStationCollection.class );
      final IFeatureBindingCollection<IStation> stations = stationCollection.getStations();
      for( IStation station : stations )
      {
        /* Get the timeseries. */
        IFeatureBindingCollection<ITimeseries> timeseries = station.getTimeseries();
        for( ITimeseries oneTimeseries : timeseries )
        {
          FactorizedTimeseriesBean factorizedTimeseries = new FactorizedTimeseriesBean( oneTimeseries );
          m_cache.put( oneTimeseries.getDataLink().getHref(), factorizedTimeseries );
          m_timeseries.add( factorizedTimeseries );
        }
      }

      /* Get the feature. */
      ICatchment feature = getFeature();
      if( feature == null )
        return;

      /* Initialize with factors. */
      IFactorizedTimeseries[] factorizedTimeseries = feature.getFactorizedTimeseries();
      for( IFactorizedTimeseries oneFactorizedTimeseries : factorizedTimeseries )
      {
        ZmlLink timeseriesLink = oneFactorizedTimeseries.getTimeseriesLink();
        if( timeseriesLink == null )
          continue;

        String href = timeseriesLink.getHref();
        FactorizedTimeseriesBean bean = m_cache.get( href );
        if( bean == null )
          continue;

        Double factor = oneFactorizedTimeseries.getFactor();
        if( factor != null )
          bean.setFactor( factor.doubleValue() );
      }
    }
    catch( CoreException ex )
    {
      ex.printStackTrace();
    }
  }
}