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
package org.kalypso.ogc.sensor.filter.creators;

import java.util.List;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.filter.FilterFactory;
import org.kalypso.ogc.sensor.filter.IFilterCreator;
import org.kalypso.util.factory.FactoryException;
import org.kalypso.zml.filters.AbstractFilterType;

/**
 * FilterCreatorHelper
 * 
 * @author schlienger
 */
public final class FilterCreatorHelper
{
  /**
   * Resolves the filter by checking if it can be created. If not, the baseObs is returned.
   * 
   * @param aft the binding type to check
   * @param baseObs the base observation onto which the filters are applied
   * @return the filtered observation
   * @throws SensorException
   */
  public static IObservation resolveFilter( final AbstractFilterType aft, final IObservation baseObs ) throws SensorException
  {
    if( aft != null )
    {
	    final IFilterCreator creator;
	    try
	    {
	      creator = FilterFactory.getCreatorInstance( aft );
	    }
	    catch( FactoryException e )
	    {
	      e.printStackTrace();
	      throw new SensorException( e );
	    }
	    
	    // recursive filtering
	    return creator.createFilter( aft, baseObs );
    }
    
    // no subfilter
    return baseObs;
  }
  
  /**
   * Same as <code>FilterCreatorHelper.resolveFilter</code> but for n filters.
   * 
   * @param afts
   * @param baseObs
   * @return array of filtered observations
   * @throws SensorException
   */
  public static IObservation[] resolveFilters( final List afts, final IObservation baseObs ) throws SensorException
  {
    final IObservation[] obs = new IObservation[ afts.size()];
    
    for( int i = 0; i < obs.length; i++ )
      obs[i] = resolveFilter( (AbstractFilterType) afts.get( i ), baseObs );
    
    return obs;
  }
}
