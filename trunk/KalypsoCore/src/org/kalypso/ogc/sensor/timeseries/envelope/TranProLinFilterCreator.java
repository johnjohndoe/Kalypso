/*--------------- Kalypso-Header ------------------------------------------

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

--------------------------------------------------------------------------*/

package org.kalypso.ogc.sensor.timeseries.envelope;

import java.net.URL;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.filter.IFilterCreator;
import org.kalypso.ogc.sensor.filter.IObservationFilter;
import org.kalypso.ogc.sensor.filter.creators.FilterCreatorHelper;
import org.kalypso.zml.filters.AbstractFilterType;
import org.kalypso.zml.filters.TranProLinFilterType;

/**
 * @author schlienger
 */
public class TranProLinFilterCreator implements IFilterCreator
{
  /**
   * @see org.kalypso.ogc.sensor.filter.IFilterCreator#createFilter(org.kalypso.zml.filters.AbstractFilterType, org.kalypso.ogc.sensor.IObservation, java.net.URL)
   */
  public IObservationFilter createFilter( final AbstractFilterType aft, final IObservation baseObs, final URL context ) throws SensorException
  {
    final TranProLinFilterType ft = (TranProLinFilterType)aft;
    
    final TranProLinFilter filter = new TranProLinFilter( ft.getDateFrom().getTime(), ft.getDateTo().getTime(), ft.getFactorBegin(), ft.getFactorEnd() );
    
    final IObservation filteredObs = FilterCreatorHelper.resolveFilter( ft.getFilter(), baseObs, context );
    filter.initFilter( null, filteredObs, context );
    
    return filter;
  }
}
