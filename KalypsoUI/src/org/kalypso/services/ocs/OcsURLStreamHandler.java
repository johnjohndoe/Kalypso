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
      String id = strUrl.replaceFirst( SCHEME_OCS + ":", "" );
      
      // remove the ?... part of the URL that is supposed to contain filter specific stuff
      // TODO: check if the char '?' will really be used. Maybe '#' is better since
      // corresponds to the fragment part. Problem: cannot use '#' currently
      // because part of the Kalypso URL stuff still uses it as property
      // separator for the various template files...
      final int ix = id.indexOf('?');
      if( ix != -1)
        id = id.substring( 0, ix );

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
      e.printStackTrace();
      
      throw new IOException( "URL could not be resolved: "
          + e.getLocalizedMessage() );
    }
  }
}