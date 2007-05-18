/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.ogc.gml.wms;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.MalformedURLException;
import java.net.URL;

import org.apache.commons.io.IOUtils;
import org.deegree.services.wms.capabilities.WMSCapabilities;
import org.deegree.xml.XMLParsingException;
import org.deegree_impl.services.wms.capabilities.OGCWMSCapabilitiesFactory;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.util.net.URLGetter;

/**
 * Helper class for WMSCapabilities
 * 
 * @author belger
 */
public class WMSCapabilitiesHelper
{
  private WMSCapabilitiesHelper( )
  {
    // do not instantiate
  }

  public static URL createCapabilitiesRequest( final URL baseURL ) throws MalformedURLException
  {
    final String query = baseURL.getQuery();
    final String getCapabilitiesQuery = "SERVICE=WMS&VERSION=1.1.1&REQUEST=GetCapabilities";
    final String queryToken = (query == null || query.length() == 0) ? "?" : "&";
    final String urlGetCapabilitiesString = baseURL.toString() + queryToken + getCapabilitiesQuery;
    return new URL( urlGetCapabilitiesString );
  }

  public static WMSCapabilities loadCapabilities( final URL service, final IProgressMonitor monitor ) throws CoreException
  {
    InputStream inputStream = null;
    try
    {
      final URL urlGetCapabilities = WMSCapabilitiesHelper.createCapabilitiesRequest( service );

      // TODO set timeout somewhere
      // maybe inside the createHttpClient Method of the Plugin-Class
      // get the timeout from global preferences
      final int timeOut = 10000;

      final URLGetter getter = URLGetter.createURLGetter( urlGetCapabilities, timeOut );
      final IStatus status = getter.execute( monitor );
      if( !status.isOK() )
        throw new CoreException( status );

      inputStream = getter.getResult();
      final Reader urlReader = new InputStreamReader( inputStream );

// // Uncomment following lines to dump capabilities
// final String capabilitiesAsString = IOUtils.toString( urlReader );
// System.out.println( capabilitiesAsString );
// final StringReader reader = new StringReader( capabilitiesAsString );

      final Reader reader = urlReader;

      final OGCWMSCapabilitiesFactory wmsCapFac = new OGCWMSCapabilitiesFactory();
      return wmsCapFac.createCapabilities( reader );
    }
    catch( final XMLParsingException e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e ) );
    }
    catch( final MalformedURLException e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e ) );
    }
    finally
    {
      IOUtils.closeQuietly( inputStream );
    }
  }

}
