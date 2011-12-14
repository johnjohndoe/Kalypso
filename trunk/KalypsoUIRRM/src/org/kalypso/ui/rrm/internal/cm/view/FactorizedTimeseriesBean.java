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

import java.util.HashMap;
import java.util.Map;

import javax.xml.namespace.QName;

import org.joda.time.Period;
import org.kalypso.commons.time.PeriodUtils;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.timeseries.binding.IStation;
import org.kalypso.model.hydrology.timeseries.binding.ITimeseries;
import org.kalypso.model.rcm.binding.ICatchment;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.editor.gmleditor.util.command.AddFeatureCommand;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;
import org.kalypsodeegree.model.feature.Feature;

/**
 * ATTENTION: This bean does not wrap {@link IFactorizedTimeseries} features. It wraps the {@link ITimeseries} features.
 * 
 * @author Gernot Belger
 * @author Holger Albert
 */
public class FactorizedTimeseriesBean extends FeatureBean<ITimeseries>
{
  private double m_factor;

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

  public double getFactor( )
  {
    return m_factor;
  }

  public void setFactor( double factor )
  {
    m_factor = factor;
  }

  public String getGroupText( )
  {
    ITimeseries timeseries = getFeature();
    if( timeseries == null )
      return null;

    IStation station = timeseries.getStation();
    return station.getGroup();
  }

  public String getStationText( )
  {
    ITimeseries timeseries = getFeature();
    if( timeseries == null )
      return null;

    IStation station = timeseries.getStation();
    return station.getName();
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
    if( Double.isNaN( m_factor ) || Double.isInfinite( m_factor ) )
      return null;

    return String.format( "%f", m_factor );
  }

  /**
   * ATTENTION: This workspace is another workspace then the one of the feature of this class.
   */
  public void apply( CommandableWorkspace workspace, Feature parent ) throws Exception
  {
    // TODO

    Map<QName, Object> properties = new HashMap<>( getProperties() );
    ICatchment collection = (ICatchment) parent;
    IRelationType parentRelation = (IRelationType) collection.getFeatureType().getProperty( ICatchment.MEMBER_FACTORIZED_TIMESERIES );
    QName type = getFeatureType().getQName();

    AddFeatureCommand command = new AddFeatureCommand( workspace, type, collection, parentRelation, -1, properties, null, -1 );
    workspace.postCommand( command );
  }
}