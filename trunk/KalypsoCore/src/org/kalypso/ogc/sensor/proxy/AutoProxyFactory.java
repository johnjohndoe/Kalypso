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

import java.io.StringReader;

import org.kalypso.binding.ratingtable.RatingTableList;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.wq.WQTimeserieProxy;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTableFactory;
import org.xml.sax.InputSource;

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
   */
  public IObservation proxyObservation( final IObservation obs )
  {
    // direct assignment, just to be able to use 'proxy' as name everywhere
    IObservation proxy = obs;

    proxy = proxyForWQTable( proxy );
    proxy = proxyForWQWechmann( proxy );

    return proxy;
  }

  private static IObservation proxyForWQTable( final IObservation obs )
  {
    final MetadataList mdl = obs.getMetadataList();

    final String wq = mdl.getProperty( TimeserieConstants.MD_WQTABLE, "" );

    if( wq.length() > 0 )
    {
      StringReader sr = new StringReader( wq );
      try
      {
        final RatingTableList tableList;
        tableList = WQTableFactory.parseSimple( new InputSource( sr ) );
        sr.close();
        
        final String source = tableList.getFromType();
        final String dest = tableList.getToType();

        boolean foundSource = false;
        boolean foundDest = false;

        final IAxis[] axes = obs.getAxisList();

        for( int i = 0; i < axes.length; i++ )
        {
          if( axes[i].getType().equals( source ) )
            foundSource = true;
          else if( axes[i].getType().equals( dest ) )
            foundDest = true;
        }

        // directly return original observation if no W nor Q or if both
        // already present
        if( !foundSource && !foundDest || foundSource && foundDest )
          return obs;

        final WQTimeserieProxy wqf = new WQTimeserieProxy( source, dest, obs );
        return wqf;
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        return obs;
      }
      finally
      {
        sr.close();
      }
    }

    return obs;
  }

  private static IObservation proxyForWQWechmann( final IObservation obs )
  {
    final MetadataList mdl = obs.getMetadataList();

    final String wq = mdl.getProperty( TimeserieConstants.MD_WQWECHMANN, "" );

    if( wq.length() > 0 )
    {
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

      final String source;
      final String dest;

      if( foundW )
      {
        source = TimeserieConstants.TYPE_WATERLEVEL;
        dest = TimeserieConstants.TYPE_RUNOFF;
      }
      else
      {
        source = TimeserieConstants.TYPE_RUNOFF;
        dest = TimeserieConstants.TYPE_WATERLEVEL;
      }

      // now that we have wq-params and that we know the type of the
      // axis, let's say the filter can be created
      final WQTimeserieProxy wqf = new WQTimeserieProxy( source, dest, obs );
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