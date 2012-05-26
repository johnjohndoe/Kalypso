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
package org.kalypso.model.hydrology.internal.binding.cm;

import java.math.BigDecimal;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.binding.cm.IFactorizedTimeseries;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * The factorized timeseries.
 *
 * @author Holger Albert
 */
public class FactorizedTimeseries extends Feature_Impl implements IFactorizedTimeseries
{
  public FactorizedTimeseries( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  @Override
  public BigDecimal getFactor( )
  {
    return getProperty( PROPERTY_FACTOR, BigDecimal.class );
  }

  @Override
  public void setFactor( final BigDecimal factor )
  {
    setProperty( PROPERTY_FACTOR, factor );
  }

  @Override
  public ZmlLink getTimeseriesLink( )
  {
    return new ZmlLink( this, PROPERTY_TIMESERIES_LINK, getWorkspace().getContext() );
  }

  @Override
  public void setTimeseriesLink( final String href )
  {
    final TimeseriesLinkType link = new TimeseriesLinkType();

    link.setHref( href );

    setProperty( PROPERTY_TIMESERIES_LINK, link );
  }
}