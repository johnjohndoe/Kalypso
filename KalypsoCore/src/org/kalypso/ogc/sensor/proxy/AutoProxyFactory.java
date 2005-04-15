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
package org.kalypso.ogc.sensor.proxy;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.wq.WQObservationFilter;

/**
 * AutoProxyFactory: this class can create proxy of an IObservation based on its
 * metadata.
 * 
 * @author schlienger
 */
public class AutoProxyFactory implements IProxyFactory
{
  private static AutoProxyFactory m_instance = null;

  private AutoProxyFactory( )
  {
    // do not instanciate
  }

  /**
   * Return a proxy IObservation that may be a proxy of the original observation
   * if sufficient information is found to create such a proxy.
   * <p>
   * For instance, some observations have WQ-Information in their metadata that
   * allows one to create a WQ-Observation from it.
   * 
   * @param obs
   * @return either a proxy observation or the original observation
   * @throws SensorException
   */
  public IObservation proxyObservation( final IObservation obs )
      throws SensorException
  {
    // currently only the wechmann wq-filter as proxy for the observation
    IObservation proxy = proxyForWechmannWQ( obs );

    return proxy;
  }

  /**
   * Checks the metadata of the given observation for the presence of some
   * WQ-Info (either Wechmann, or WQ-Table, or ...). In the positive, it creates
   * a WQ-Filter from this information.
   * 
   * @param obs
   * @return either a WQObservationFilter or the original obs
   * @throws SensorException
   */
  private static IObservation proxyForWechmannWQ( final IObservation obs )
      throws SensorException
  {
    final MetadataList mdl = obs.getMetadataList();

    final String wq = mdl.getProperty( TimeserieConstants.MD_WQWECHMANN, mdl
        .getProperty( TimeserieConstants.MD_WQTABLE, "" ) );

    if( wq.length() > 0 )
    {
      final WQObservationFilter wqf = new WQObservationFilter();

      boolean foundW = false;
      boolean foundQ = false;
      final IAxis[] axes = obs.getAxisList();
      for( int i = 0; i < axes.length; i++ )
      {
        if( axes[i].getType().equals( TimeserieConstants.TYPE_RUNOFF ) )
          foundQ = true;
        else if( axes[i].getType().equals( TimeserieConstants.TYPE_WATERLEVEL ) )
          foundW = true;
      }

      // directly return original observation if no W nor Q or if both
      // already present
      if( !foundW && !foundQ || foundW && foundQ )
        return obs;

      // now that we have wq-params and that we know the type of the
      // axis, let's say the filter can be created
      wqf.initFilter( foundW ? TimeserieConstants.TYPE_WATERLEVEL
          : TimeserieConstants.TYPE_RUNOFF, obs );

      return wqf;
    }

    return obs;
  }

  /**
   * @return default instance of this factory class
   */
  public static AutoProxyFactory getInstance( )
  {
    if( m_instance == null )
      m_instance = new AutoProxyFactory();

    return m_instance;
  }
}