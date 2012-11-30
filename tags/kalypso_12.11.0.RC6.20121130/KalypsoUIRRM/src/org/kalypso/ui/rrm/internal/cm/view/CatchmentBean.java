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
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.binding.cm.ICatchment;
import org.kalypso.model.hydrology.binding.cm.IFactorizedTimeseries;
import org.kalypso.model.hydrology.binding.cm.ILinearSumGenerator;
import org.kalypso.model.hydrology.binding.timeseries.IStation;
import org.kalypso.model.hydrology.binding.timeseries.IStationCollection;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.model.hydrology.project.RrmScenario;
import org.kalypso.ogc.gml.command.ChangeFeatureCommand;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ui.editor.gmleditor.command.AddFeatureCommand;
import org.kalypso.ui.rrm.internal.IUiRrmWorkflowConstants;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.IXLinkedFeature;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

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

  private String m_catchmentDescription;

  private IStatus m_status;

  private GM_Polygon m_catchmentArea;

  public CatchmentBean( )
  {
    super( ICatchment.FEATURE_CATCHMENT );

    m_cache = new HashMap<>();
    m_timeseries = new ArrayList<>();
    m_catchmentRef = null;
    m_catchmentName = null;
    m_catchmentDescription = null;
    m_status = null;

    initTimeseries();
  }

  public CatchmentBean( final ICatchment catchment )
  {
    super( catchment );

    m_cache = new HashMap<>();
    m_timeseries = new ArrayList<>();
    m_catchmentRef = resolveRef( catchment );
    m_catchmentName = catchment.resolveName();
    m_catchmentDescription = catchment.resolveDescription();
    m_catchmentArea = catchment.resolveArea();
    m_status = null;

    initTimeseries();
  }

  public FactorizedTimeseriesBean[] getTimeseries( )
  {
    return m_timeseries.toArray( new FactorizedTimeseriesBean[] {} );
  }

  public FactorizedTimeseriesBean getTimeseries( final String href )
  {
    return m_cache.get( href );
  }

  public String getCatchmentRef( )
  {
    return m_catchmentRef;
  }

  public String getCatchmentName( )
  {
    return m_catchmentName;
  }

  public String getCatchmentDescription( )
  {
    return m_catchmentDescription;
  }

  public GM_Polygon getCatchmentArea( )
  {
    return m_catchmentArea;
  }

  public void setCatchmentRef( final String catchmentRef )
  {
    m_catchmentRef = catchmentRef;
  }

  public void setCatchmentName( final String catchmentName )
  {
    m_catchmentName = catchmentName;
  }

  public void setCatchmentDescription( final String catchmentDescription )
  {
    m_catchmentDescription = catchmentDescription;
  }

  public void setCatchmentArea( final GM_Polygon catchmentArea )
  {
    m_catchmentArea = catchmentArea;
  }

  public String getLabel( )
  {
    /* The feature may or may not exist here. */
    /* So we cant use it to resolve the area. */
    return m_catchmentName;
  }

  public IStatus getStatus( )
  {
    if( m_status == null )
      m_status = checkFactor();

    return m_status;
  }

  public void updateStatus( )
  {
    m_status = checkFactor();
  }

  private IStatus checkFactor( )
  {
    int completeFactor = 0;
    for( final FactorizedTimeseriesBean timeseries : m_timeseries )
    {
      final int factor = timeseries.getFactor();
      completeFactor += factor;
    }

    if( completeFactor <= 0 )
      return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), String.format( Locale.PRC, Messages.getString( "CatchmentBean_0" ), completeFactor ) ); //$NON-NLS-1$

    if( completeFactor > 100 )
      return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), String.format( Locale.PRC, Messages.getString( "CatchmentBean_1" ), completeFactor ) ); //$NON-NLS-1$

    if( completeFactor > 0 && completeFactor < 100 )
      return new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), String.format( Locale.PRC, Messages.getString( "CatchmentBean_2" ), completeFactor ) ); //$NON-NLS-1$

    return new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), Messages.getString( "CatchmentBean_3" ) ); //$NON-NLS-1$
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
    final Map<QName, Object> properties = new HashMap<>( getProperties() );
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
    final String href = RrmScenario.FILE_MODELL_GML + "#" + m_catchmentRef; //$NON-NLS-1$
    final IRelationType relation = (IRelationType) getFeatureType().getProperty( ICatchment.PROPERTY_AREA_LINK );
    final IXLinkedFeature areaLink = FeatureFactory.createXLink( newFeature, relation, null, href );

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
    if( areaLink instanceof IXLinkedFeature )
    {
      final IXLinkedFeature xlink = (IXLinkedFeature) areaLink;
      return xlink.getFeatureId();
    }

    return null;
  }

  /**
   * Make sure that the catchments always contains all stations.
   */
  private void initTimeseries( )
  {
    try
    {
      /* Get the stations. */
      final IScenarioDataProvider scenarioDataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
      final IStationCollection stationCollection = scenarioDataProvider.getModel( IUiRrmWorkflowConstants.SCENARIO_DATA_STATIONS );
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
      initializeFactors( feature.getFactorizedTimeseries() );
    }
    catch( final CoreException ex )
    {
      ex.printStackTrace();
    }
  }

  public void initializeFactors( final IFeatureBindingCollection<IFactorizedTimeseries> factorizedTimeseries )
  {
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

  public void clearAllWeights( )
  {
    for( final FactorizedTimeseriesBean timeseries : m_timeseries )
      timeseries.setFactor( 0 );
  }
}