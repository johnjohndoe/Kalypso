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
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import javax.xml.namespace.QName;

import org.joda.time.Period;
import org.kalypso.commons.time.PeriodUtils;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.binding.cm.ICatchment;
import org.kalypso.model.hydrology.binding.cm.IFactorizedTimeseries;
import org.kalypso.model.hydrology.binding.timeseries.IStation;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.editor.gmleditor.command.AddFeatureCommand;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;

/**
 * ATTENTION: This bean does not wrap {@link IFactorizedTimeseries} features. It wraps the {@link ITimeseries} features.
 *
 * @author Gernot Belger
 * @author Holger Albert
 */
public class FactorizedTimeseriesBean extends FeatureBean<ITimeseries>
{
  private int m_factor;

  public FactorizedTimeseriesBean( )
  {
    super( ITimeseries.FEATURE_TIMESERIES );
  }

  public FactorizedTimeseriesBean( final ITimeseries timeseries )
  {
    super( timeseries );
  }

  public String getLabel( )
  {
    return (String) getProperty( Feature.QN_DESCRIPTION );
  }

  public int getFactor( )
  {
    return m_factor;
  }

  public void setFactor( final int factor )
  {
    m_factor = Math.max( 0, factor );
  }

  public String getGroupText( )
  {
    final ITimeseries timeseries = getFeature();
    if( timeseries == null )
      return null;

    final IStation station = timeseries.getStation();
    return station.getGroup();
  }

  public String getStationText( )
  {
    final ITimeseries timeseries = getFeature();
    if( timeseries == null )
      return null;

    final IStation station = timeseries.getStation();
    return station.getDescription();
  }

  public String getTimestepText( )
  {
    final ITimeseries timeseries = getFeature();
    if( timeseries == null )
      return null;

    final Period timestep = timeseries.getTimestep();
    return PeriodUtils.formatDefault( timestep );
  }

  public String getQualityText( )
  {
    final ITimeseries timeseries = getFeature();
    if( timeseries == null )
      return null;

    return timeseries.getQuality();
  }

  public String getFactorText( )
  {
    if( m_factor <= 0 )
      return ""; //$NON-NLS-1$

    return String.format( Locale.PRC, "%d", m_factor ); //$NON-NLS-1$
  }

  /**
   * This function applies the changes of this factorized timeseries. It assumes, that the factorized timeseries does
   * not exist or does not exist anymore, because it will create a new factorized timeseries feature. <br/>
   * <br/>
   * ATTENTION: This workspace is another workspace then the one of the feature of this class.
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
    if( m_factor <= 0 || m_factor > 100 )
      return;

    /* HINT: This ensures, that only the timeseries with the same parameter type as the generator will get factors. */
    if( parameterType == null || !parameterType.equals( getFeature().getParameterType() ) )
      return;

    final TimeseriesLinkType timeseriesLink = new TimeseriesLinkType();
    timeseriesLink.setHref( getFeature().getDataLink().getHref() );

    final Map<QName, Object> properties = new HashMap<>();
    properties.put( IFactorizedTimeseries.PROPERTY_FACTOR, new BigDecimal( m_factor ) );
    properties.put( IFactorizedTimeseries.PROPERTY_TIMESERIES_LINK, timeseriesLink );

    final ICatchment collection = (ICatchment) parent;
    final IRelationType parentRelation = (IRelationType) collection.getFeatureType().getProperty( ICatchment.MEMBER_FACTORIZED_TIMESERIES );
    final QName type = IFactorizedTimeseries.FEATURE_FACTORIZED_TIMESERIES;

    final AddFeatureCommand command = new AddFeatureCommand( workspace, type, collection, parentRelation, -1, properties, null, -1 );
    workspace.postCommand( command );
  }
}