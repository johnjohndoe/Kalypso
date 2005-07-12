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
package org.kalypso.services.ocs;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;
import java.util.logging.Logger;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.request.RequestFactory;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ogc.sensor.zml.ZmlURL;
import org.kalypso.services.proxy.DataBean;
import org.kalypso.services.proxy.IObservationService;
import org.kalypso.ui.KalypsoGisPlugin;
import org.osgi.service.url.AbstractURLStreamHandlerService;

/**
 * Observation Collection Service URL Stream Handler.
 * <p>
 * It extends the <code>AbstractURLStreamHandlerService</code> of the OSGI-Platform in order to be registered as a
 * URLStreamHandler since Eclipse manages the handlers this way.
 * 
 * @author schlienger
 */
public class OcsURLStreamHandler extends AbstractURLStreamHandlerService
{
  /**
   * @see java.net.URLStreamHandler#openConnection(java.net.URL)
   */
  public URLConnection openConnection( final URL u ) throws IOException
  {
    final String href = u.toExternalForm();

    // use the default url connection if this is not a kalypso server-side one
    if( !ZmlURL.isServerSide( href ) )
      return u.openConnection();

    // create a local temp file for storing the zml
    final File file = KalypsoGisPlugin.getDefault().createTempFile( "zml-proxy", "zml", "zml" );
    file.deleteOnExit();

    InputStream ins = null;

    try
    {
      final IObservationService srv = KalypsoGisPlugin.getDefault().getObservationServiceProxy();

      final DataBean data = srv.readData( href );

      ins = data.getDataHandler().getInputStream();
      FileUtilities.makeFileFromStream( false, file, ins );
      ins.close();

      srv.clearTempData( data.getId() );

      return file.toURL().openConnection();
    }
    catch( final Exception e ) // generic exception caught for simplicity
    {
      String exceptionMessage = e.getLocalizedMessage();
      
      final Logger log = Logger.getLogger( getClass().getName() );
      log.info( "Link konnte nicht aufgelöst werden: " + href +
          "\nFehler: " + exceptionMessage );

      try
      {
        log.info( "Es wird versucht, eine Default-Zeitreihe zu erzeugen" );
        
        // we might be here because the server is down. If the href contains
        // a request, let create a default observation according to it.
        final IObservation obs = RequestFactory.createDefaultObservation( href );

        ZmlFactory.writeToFile( obs, file );

        return file.toURL().openConnection();
      }
      catch( final Exception se )
      {
        exceptionMessage += "\n" + se.getLocalizedMessage();
      }

      throw new IOException( "URL konnte nicht gelöst werden, Grund: " + exceptionMessage );
    }
    finally
    {
      IOUtils.closeQuietly( ins );
    }
  }
}