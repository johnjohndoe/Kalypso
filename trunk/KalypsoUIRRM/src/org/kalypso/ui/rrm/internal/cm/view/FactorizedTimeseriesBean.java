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
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.time.PeriodUtils;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.rcm.binding.ICatchment;
import org.kalypso.model.rcm.binding.IFactorizedTimeseries;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.editor.gmleditor.util.command.AddFeatureCommand;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 * @author Holger Albert
 */
public class FactorizedTimeseriesBean extends FeatureBean<IFactorizedTimeseries>
{
  public FactorizedTimeseriesBean( )
  {
    super( IFactorizedTimeseries.FEATURE_FACTORIZED_TIMESERIES );
  }

  public FactorizedTimeseriesBean( final IFactorizedTimeseries factorizedTimeseries )
  {
    super( factorizedTimeseries );
  }

  public String getLabel( )
  {
    return (String) getProperty( Feature.QN_DESCRIPTION );
  }

  public double getFactor( )
  {
    return (double) getProperty( IFactorizedTimeseries.PROPERTY_FACTOR );
  }

  public void setFactor( double factor )
  {
    setProperty( IFactorizedTimeseries.PROPERTY_FACTOR, factor );
  }

  public String getStationText( )
  {
    return getLabel();
  }

  public String getTimestepText( )
  {
    final IFactorizedTimeseries timeseries = getFeature();
    if( timeseries == null )
      return null;

    final Period timestep = timeseries.getTimestep();
    return PeriodUtils.formatDefault( timestep );
  }

  public String getQualityText( )
  {
    final IFactorizedTimeseries timeseries = getFeature();
    if( timeseries == null )
      return null;

    return timeseries.getQuality();
  }

  public String getFactorText( )
  {
    double factor = getFactor();
    if( Double.isNaN( factor ) || Double.isInfinite( factor ) )
      return null;

    return String.format( "%f", factor );
  }

  public void apply( CommandableWorkspace workspace, CatchmentBean parent ) throws Exception
  {
    IFactorizedTimeseries feature = getFeature();
    if( feature == null )
    {
      Map<QName, Object> properties = new HashMap<>( getProperties() );
      ICatchment collection = parent.getFeature();
      IRelationType parentRelation = (IRelationType) collection.getFeatureType().getProperty( ICatchment.MEMBER_FACTORIZED_TIMESERIES );
      QName type = getFeatureType().getQName();

      AddFeatureCommand command = new AddFeatureCommand( workspace, type, collection, parentRelation, -1, properties, null, -1 );
      workspace.postCommand( command );
    }
    else
    {
      ICommand command = applyChanges();
      workspace.postCommand( command );
    }
  }
}