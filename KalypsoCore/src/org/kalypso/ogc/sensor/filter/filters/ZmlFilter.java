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
package org.kalypso.ogc.sensor.filter.filters;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Iterator;
import java.util.List;

import org.kalypso.commons.java.net.UrlResolverSingleton;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ogc.sensor.zml.ZmlURL;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;

/**
 * ZmlFilter
 * 
 * @author schlienger
 */
public final class ZmlFilter extends AbstractObservationFilter
{
  /**
   * TRICKY: allows us to override the default behaviour of URL resolving by directly looking for the href as an
   * identifier within the list of repositories
   */
  private static List REPS = null;

  /**
   * @see org.kalypso.ogc.sensor.filter.IObservationFilter#initFilter(java.lang.Object,
   *      org.kalypso.ogc.sensor.IObservation, java.net.URL)
   */
  public void initFilter( final Object conf, final IObservation obs, final URL context ) throws SensorException
  {
    super.initFilter( conf, obs, context );

    // conf is the href string
    final String href = conf.toString();

    // if the href is empty, simply ignore and let the given obs replace this
    // filter
    if( href.length() != 0 )
    {
      // TRICKY: clients can set the REPS field. This will be used first to make
      // a lookup for the href as an identifier of an Observation.
      // The observation referred by the href can be localized within another
      // repository on the server side for instance. This trick allow us
      // to directly fetch the observation from the repository item.
      if( REPS != null && REPS.size() > 0 )
      {
        // only take id part since href can contain additional query stuff
        final String id = ZmlURL.getIdentifierPart( href );

        final Iterator it = REPS.iterator();

        while( it.hasNext() )
        {
          final IRepository rep = (IRepository)it.next();

          try
          {
            final IRepositoryItem item = rep.findItem( id );

            if( item != null )
            {
              m_obs = (IObservation)item.getAdapter( IObservation.class );

              return;
            }
          }
          catch( RepositoryException ignored )
          {
            // ignored
          }
        }
      }

      // no use the standard URL resolving facility
      try
      {
        final URL sourceUrl = UrlResolverSingleton.resolveUrl( context, href );
        final IObservation observation = ZmlFactory.parseXML( sourceUrl, href );

        // override observation from abstract filter (super type)
        m_obs = observation;
      }
      catch( MalformedURLException e )
      {
        throw new SensorException( e );
      }
    }
  }

  /**
   * Allows to specify the list of repositories to use when resolving the underlying zml. This facility is provided for
   * convenience for the server-side of Kalypso.
   * <p>
   * If the zml if found in one of the repositories, then there's no need to use the ZmlFactory.
   * 
   * @param repositories
   *          [nullable]
   */
  public static void configureFor( final List repositories )
  {
    REPS = repositories;
  }
}