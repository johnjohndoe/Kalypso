package org.kalypso.services.ocs;

import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLStreamHandler;

import javax.xml.rpc.ServiceException;

import org.kalypso.services.IServicesConstants;
import org.kalypso.services.proxy.IObservationService;
import org.kalypso.services.proxy.ObservationBean;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * Observation Collection Service URL Stream Handler.
 * 
 * @author schlienger
 */
public class OcsURLStreamHandler extends URLStreamHandler
{
  /**
   * @see java.net.URLStreamHandler#openConnection(java.net.URL)
   */
  protected URLConnection openConnection( final URL u ) throws IOException
  {
    final String strUrl = u.toExternalForm();
    
    // kalypso observation service protocol?
    if( !strUrl.startsWith( IServicesConstants.URL_PROTOCOL_OBSERVATION_SERVICE ) )
      return u.openConnection();
    
    try
    {
      final String id = strUrl.replaceFirst( IServicesConstants.URL_PROTOCOL_OBSERVATION_SERVICE, "" );

      // TODO final ObservationBean ob = new ObservationBean( id );
      
      final IObservationService srv = KalypsoGisPlugin.getDefault().getObservationServiceProxy();
    }
    catch( ServiceException e )
    {
      e.printStackTrace();
    }
    
    
    return null;
  }
}
