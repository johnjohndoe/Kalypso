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

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.util.logging.Logger;

import org.apache.commons.io.IOUtils;
import org.kalypso.contribs.eclipse.core.runtime.TempFileUtilities;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.request.RequestFactory;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ogc.sensor.zml.ZmlURL;
import org.kalypso.services.sensor.impl.DataBean;
import org.kalypso.services.sensor.impl.KalypsoObservationService;
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
  private final Logger m_logger = Logger.getLogger( getClass().getName() );

  @Override
  public URLConnection openConnection( final URL u ) throws IOException
  {
    final String href = u.toExternalForm();

    m_logger.info( "Lade ZML: " + href );

    // check if an empty id is provided, in that case use the request if provided
    if( ZmlURL.isEmpty( href ) )
    {
      try
      {
        m_logger.warning( "Leere Zeitreihe angefordert..." );

        return tryWithRequest( href, null );
      }
      catch( final Exception e )
      {
        m_logger.warning( "Leere Zeitreihe konnte nicht erzeugt werden: " + e.getLocalizedMessage() );

        throw new IOException( "Leere Zeitreihe konnte nicht erzeugt werden: " + e.getLocalizedMessage() );
      }
    }

    //InputStream ins = null;
    OutputStream stream = null;
    File file = null;

    try
    {
      // use the default url connection if this is not a kalypso server-side one
      if( !ZmlURL.isServerSide( href ) )
        return u.openConnection();

      // else fetch the observation from the server
      final KalypsoObservationService srv = KalypsoGisPlugin.getDefault().getObservationServiceProxy();

      final DataBean data = srv.readData( href );

      // create a local temp file for storing the zml
      file = TempFileUtilities.createTempFile( KalypsoGisPlugin.getDefault(), "zml-proxy", "zml", "zml" );
      file.deleteOnExit();

//      ins = data.getDataHandler().getInputStream();
//      FileUtilities.makeFileFromStream( false, file, ins );
//      ins.close();
      final byte[] bytes = data.getDataHandler();
      
      stream = new BufferedOutputStream( new FileOutputStream( file ) );
      stream.write( bytes );
      stream.close();

      srv.clearTempData( data.getId() );

      return file.toURL().openConnection();
    }
    catch( final Exception e ) // generic exception caught for simplicity
    {
      String exceptionMessage = e.getLocalizedMessage();

      m_logger.info( "Link konnte nicht aufgelöst werden: " + href + "\nFehler: " + exceptionMessage );

      try
      {
        m_logger.warning( "Es wird versucht, eine Default-Zeitreihe zu erzeugen..." );

        return tryWithRequest( href, file );
      }
      catch( final Exception se )
      {
        m_logger.warning( "Default-Zeitreihe konnte nicht erzeugt werden." );

        exceptionMessage += "\n" + se.getLocalizedMessage();
      }

      throw new IOException( "URL konnte nicht aufgelöst werden, Grund: " + exceptionMessage );
    }
    finally
    {
//      IOUtils.closeQuietly( ins );
      IOUtils.closeQuietly( stream );
    }
  }

  /**
   * Helper that tries to load the observation from its optional request
   * 
   * @param file
   *          temp file where to store the observation locally. Can be null, in that case a temp file is created
   */
  private URLConnection tryWithRequest( final String href, File file ) throws SensorException, MalformedURLException,
      IOException
  {
    // create a local temp file for storing the zml if not provided
    if( file == null )
    {
      file = TempFileUtilities.createTempFile( KalypsoGisPlugin.getDefault(), "zml-proxy", "zml", "zml" );
      file.deleteOnExit();
    }

    // we might be here because the server is down. If the href contains
    // a request, let create a default observation according to it.
    final IObservation obs = RequestFactory.createDefaultObservation( href );

    m_logger.info( "Default-Zeitreihe " + obs.getName() + " wurde erzeugt." );

    ZmlFactory.writeToFile( obs, file );

    return file.toURL().openConnection();
  }
}