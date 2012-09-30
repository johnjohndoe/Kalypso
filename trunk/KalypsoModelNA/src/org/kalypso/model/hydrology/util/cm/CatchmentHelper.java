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
package org.kalypso.model.hydrology.util.cm;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.apache.commons.lang3.StringUtils;
import org.kalypso.model.hydrology.binding.cm.ICatchment;
import org.kalypso.model.hydrology.binding.cm.IFactorizedTimeseries;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Holger Albert
 */
public class CatchmentHelper
{
  /**
   * The constructor.
   */
  private CatchmentHelper( )
  {
  }

  /**
   * This function creates a hash from the catchment.<br/>
   * <br/>
   * A hash is generated from the catchment. It takes the factors/timeseries into account. An equal combination of
   * factors and timeseries creates the same hash.
   *
   * @param catchment
   *          The catchment.
   * @return The hash.
   */
  public static String buildHash( final ICatchment catchment )
  {
    /* Memory for the single values. */
    final List<String> values = new ArrayList<>();

    /* Build the hash. */
    final IFeatureBindingCollection<IFactorizedTimeseries> factorizedTimeseries = catchment.getFactorizedTimeseries();
    for( final IFactorizedTimeseries timeseries : factorizedTimeseries )
    {
      final BigDecimal factor = timeseries.getFactor();
      final ZmlLink link = timeseries.getTimeseriesLink();
      values.add( String.format( Locale.PRC, "%s_%s", factor.toPlainString(), link.getHref() ) ); //$NON-NLS-1$
    }

    /* Join the values. */
    return StringUtils.join( values.toArray( new String[] {} ), ";" ); //$NON-NLS-1$
  }
}