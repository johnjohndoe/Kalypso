package org.kalypso.services.ocs;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;

import org.apache.commons.io.FileUtils;
import org.kalypso.services.proxy.IObservationService;
import org.kalypso.services.proxy.OCSDataBean;
import org.kalypso.services.proxy.ObservationBean;
import org.kalypso.ui.KalypsoGisPlugin;
import org.osgi.service.url.AbstractURLStreamHandlerService;

/**
 * Observation Collection Service URL Stream Handler.
 * <p>
 * It extends the <code>AbstractURLStreamHandlerService</code> of the
 * OSGI-Platform in order to be registered as a URLStreamHandler since Eclipse
 * manages the handlers this way.
 * 
 * @author schlienger
 */
public class OcsURLStreamHandler extends AbstractURLStreamHandlerService
{
  /** the protocol that identifies the observation service */
  public final static String SCHEME_OCS = "kalypso-ocs";

  /**
   * @see java.net.URLStreamHandler#openConnection(java.net.URL)
   */
  public URLConnection openConnection( final URL u ) throws IOException
  {
    final String strUrl = u.toExternalForm();

    // kalypso observation service protocol?
    if( !strUrl.startsWith( SCHEME_OCS ) )
      return u.openConnection();

    try
    {
      final String id = strUrl.replaceFirst( SCHEME_OCS + ":", "" );

      final ObservationBean ob = new ObservationBean( id, "", "", null );

      final IObservationService srv = KalypsoGisPlugin.getDefault()
          .getObservationServiceProxy();

      final OCSDataBean db = srv.readData( ob, null );

      final File file = File.createTempFile( "local-zml", ".zml" );
      file.deleteOnExit();

      FileUtils.copyURLToFile( new URL( db.getLocation() ), file );

      srv.clearTempData( db );

      return file.toURL().openConnection();
    }
    catch( Exception e ) // generic exception caught for simplicity
    {
      throw new IOException( "URL could not be resolved: "
          + e.getLocalizedMessage() );
    }
  }
}