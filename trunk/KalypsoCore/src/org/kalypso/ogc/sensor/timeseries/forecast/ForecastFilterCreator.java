/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.sensor.timeseries.forecast;

import java.net.URL;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.filter.IFilterCreator;
import org.kalypso.ogc.sensor.filter.IObservationFilter;
import org.kalypso.ogc.sensor.filter.creators.FilterCreatorHelper;
import org.kalypso.zml.filters.AbstractFilterType;
import org.kalypso.zml.filters.ForecastFilterType;

/**
 * MergeFilterCreator
 * 
 * @author schlienger
 */
public class ForecastFilterCreator implements IFilterCreator
{
  public IObservationFilter createFilter( AbstractFilterType aft,
      IObservation baseObs, final URL context ) throws SensorException
  {
    if( !( aft instanceof ForecastFilterType ) )
      throw new IllegalArgumentException( "Not a "
          + ForecastFilterType.class.getName() );

    final ForecastFilterType ft = (ForecastFilterType)aft;

    final IObservation[] filteredObs = FilterCreatorHelper.resolveFilters( ft
        .getFilter(), baseObs, context );

    final ForecastFilter filter = new ForecastFilter();
    filter.initFilter( filteredObs, filteredObs[0], context );

    return filter;
  }
}
