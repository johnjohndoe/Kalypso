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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.project.INaProjectConstants;
import org.kalypso.model.hydrology.timeseries.binding.IStation;
import org.kalypso.model.hydrology.timeseries.binding.IStationCollection;
import org.kalypso.model.hydrology.timeseries.binding.ITimeseries;
import org.kalypso.model.rcm.binding.ICatchment;
import org.kalypso.model.rcm.binding.IFactorizedTimeseries;
import org.kalypso.model.rcm.binding.ILinearSumGenerator;
import org.kalypso.ogc.gml.command.ChangeFeatureCommand;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ui.editor.gmleditor.util.command.AddFeatureCommand;
import org.kalypso.ui.rrm.internal.IUiRrmWorkflowConstants;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathUtilities;

/**
 * @author Gernot Belger
 * @author Holger Albert
 */
public class CatchmentBean extends FeatureBean<ICatchment>
{
  private final Map<String, FactorizedTimeseriesBean> m_cache;

  private final List<FactorizedTimeseriesBean> m_timeseries;

  private String m_catchmentRef;

  private String m_catchmentName;

  public CatchmentBean( )
  {
    super( ICatchment.FEATURE_CATCHMENT );

    m_cache = new HashMap<String, FactorizedTimeseriesBean>();
    m_timeseries = new ArrayList<FactorizedTimeseriesBean>();
    m_catchmentRef = null;
    m_catchmentName = null;

    initTimeseries();
  }

  public CatchmentBean( final ICatchment catchment )
  {
    super( catchment );

    m_cache = new HashMap<String, FactorizedTimeseriesBean>();
    m_timeseries = new ArrayList<FactorizedTimeseriesBean>();
    m_catchmentRef = resolveRef( catchment );
    m_catchmentName = resolveName( catchment );

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

  public String getCatchmentName( )
  {
    return m_catchmentName;
  }

  public void setCatchmentRef( final String catchmentRef )
  {
    m_catchmentRef = catchmentRef;
  }

  public void setCatchmentName( final String catchmentName )
  {
    m_catchmentName = catchmentName;
  }

  public String getLabel( )
  {
    /* The feature may or may not exist here. */
    /* So we cant use it to resolve the area. */
    return m_catchmentName;
  }

  public IStatus checkFactor( )
  {
    int completeFactor = 0;
    for( final FactorizedTimeseriesBean timeseries : m_timeseries )
    {
      final int factor = timeseries.getFactor();
      completeFactor += factor;
    }

    if( completeFactor <= 0 )
      return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), String.format( Locale.PRC, "The sum of factors (%d%%) is not allowed to be zero or negative.", completeFactor ) );

    if( completeFactor > 100 )
      return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), String.format( Locale.PRC, "The sum of factors (%d%%) is not allowed to be greater than 100.", completeFactor ) );

    if( completeFactor > 0 && completeFactor < 100 )
      return new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), String.format( Locale.PRC, "The sum of factors (%d%%) is not 100%%.", completeFactor ) );

    return new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), "100 Percent" );
  }

  /**
   * This function applies the changes of this catchment. It assumes, that the catchment does not exist or does not
   * exist anymore, because it will create a new catchment feature.
   * 
   * @param workspace
   *          The workspace.
   * @param parent
   *          The parent feature.
   * @param parameterType
   *          The selected parameter type.
   */
  public void apply( final CommandableWorkspace workspace, final Feature parent, final String parameterType ) throws Exception
  {
    /* The catchment feature does not exist. */
    final Map<QName, Object> properties = new HashMap<QName, Object>( getProperties() );
    final ILinearSumGenerator collection = (ILinearSumGenerator) parent;
    final IRelationType parentRelation = (IRelationType) collection.getFeatureType().getProperty( ILinearSumGenerator.MEMBER_CATCHMENT );
    final QName type = getFeatureType().getQName();

    /* Create the add feature command. */
    final AddFeatureCommand command = new AddFeatureCommand( workspace, type, collection, parentRelation, -1, properties, null, -1 );

    /* Post the command. */
    workspace.postCommand( command );

    /* Get the new feature. */
    final Feature newFeature = command.getNewFeature();

    /* Build the area link. */
    final String href = INaProjectConstants.GML_MODELL_FILE + "#" + m_catchmentRef;
    final IRelationType relation = (IRelationType) getFeatureType().getProperty( ICatchment.PROPERTY_AREA_LINK );
    final XLinkedFeature_Impl areaLink = new XLinkedFeature_Impl( newFeature, relation, null, href );

    /* Create the change feature command. */
    final ChangeFeatureCommand changeCommand = new ChangeFeatureCommand( newFeature, relation, areaLink );

    /* Post the command. */
    workspace.postCommand( changeCommand );

    /* Apply also all contained timeseries. */
    for( final FactorizedTimeseriesBean timeseriesBean : m_timeseries )
      timeseriesBean.apply( workspace, newFeature, parameterType );
  }

  private String resolveRef( final ICatchment catchment )
  {
    final Feature areaLink = catchment.getAreaLink();
    if( areaLink instanceof XLinkedFeature_Impl )
    {
      final XLinkedFeature_Impl xlink = (XLinkedFeature_Impl) areaLink;
      return xlink.getFeatureId();
    }

    return null;
  }

  private String resolveName( final ICatchment catchment )
  {
    /* Get the parent. */
    final ILinearSumGenerator parent = (ILinearSumGenerator) catchment.getParent();

    /* Get the name property. */
    final String nameProperty = parent.getAreaNameProperty();
    if( nameProperty == null || nameProperty.length() == 0 )
      return null;

    /* Get the area. */
    final Feature area = catchment.getAreaLink();
    if( area == null )
      return null;

    /* Build the xpath. */
    final GMLXPath xPath = new GMLXPath( nameProperty, catchment.getWorkspace().getNamespaceContext() );

    /* Query. */
    return (String) GMLXPathUtilities.queryQuiet( xPath, area );
  }

  private void initTimeseries( )
  {
    try
    {
      /* Get the stations. */
      final SzenarioDataProvider scenarioDataProvider = ScenarioHelper.getScenarioDataProvider();
      final IStationCollection stationCollection = scenarioDataProvider.getModel( IUiRrmWorkflowConstants.SCENARIO_DATA_STATIONS, IStationCollection.class );
      final IFeatureBindingCollection<IStation> stations = stationCollection.getStations();
      for( final IStation station : stations )
      {
        /* Get the timeseries. */
        final IFeatureBindingCollection<ITimeseries> timeseries = station.getTimeseries();
        for( final ITimeseries oneTimeseries : timeseries )
        {
          final FactorizedTimeseriesBean factorizedTimeseries = new FactorizedTimeseriesBean( oneTimeseries );
          final ZmlLink dataLink = oneTimeseries.getDataLink();
          if( dataLink != null && dataLink.isLinkSet() )
          {
            m_cache.put( dataLink.getHref(), factorizedTimeseries );
            m_timeseries.add( factorizedTimeseries );
          }
        }
      }

      /* Get the feature. */
      final ICatchment feature = getFeature();
      if( feature == null )
        return;

      /* Initialize with factors. */
      final IFactorizedTimeseries[] factorizedTimeseries = feature.getFactorizedTimeseries();
      for( final IFactorizedTimeseries oneFactorizedTimeseries : factorizedTimeseries )
      {
        final ZmlLink timeseriesLink = oneFactorizedTimeseries.getTimeseriesLink();
        if( timeseriesLink == null )
          continue;

        final FactorizedTimeseriesBean bean = m_cache.get( timeseriesLink.getHref() );
        if( bean == null )
          continue;

        final BigDecimal factor = oneFactorizedTimeseries.getFactor();
        if( factor != null )
          bean.setFactor( factor.intValue() );
      }
    }
    catch( final CoreException ex )
    {
      ex.printStackTrace();
    }
  }
}