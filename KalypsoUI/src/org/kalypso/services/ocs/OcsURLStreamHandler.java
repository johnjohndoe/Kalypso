package org.kalypso.services.ocs;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.text.DateFormat;

import org.apache.commons.io.FileUtils;
import org.eclipse.jface.window.Window;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.internal.Workbench;
import org.kalypso.ogc.sensor.zml.ZmlURL;
import org.kalypso.services.proxy.DateRangeBean;
import org.kalypso.services.proxy.IObservationService;
import org.kalypso.services.proxy.OCSDataBean;
import org.kalypso.services.proxy.ObservationBean;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.repository.dialogs.DateRangeInputDialog;
import org.kalypso.util.runtime.args.DateRangeArgument;
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
    // kalypso observation service protocol?
    if( !u.toExternalForm().startsWith( SCHEME_OCS ) )
      return u.openConnection();

    try
    {
      DateRangeArgument dra = null;
      
      /*
       * The query part of the URL (after the ?...) contains some additional
       * specification: from-to, filter, etc.
       */
      final String query = u.getQuery();
      if( query != null )
        dra = ZmlURL.checkDateRange( query );
      else
      {
        /*
         * Ask user for a date range if a gui is available
         */
//        final IWorkbenchWindow window = Workbench.getInstance().getActiveWorkbenchWindow();
//        if( window != null )
//        {
//          final DateFormat df = DateFormat.getDateTimeInstance();
//          final DateRangeInputDialog dlg = new DateRangeInputDialog( window.getShell(), false, null, null, 15, df, KalypsoGisPlugin.getDefault() );
//          
//          if( dlg.open() == Window.OK )
//          {
//            if( dlg.isUseRange() )
//              dra = new DateRangeArgument( dlg.getDateFrom(), dlg.getDateTo() );
//            else
//              dra = DateRangeArgument.createFromPastDays( dlg.getNumberOfDays() );
//          }
//        }
      }

      final String obsId = ZmlURL.getIdentifierPart( u ).replaceFirst( SCHEME_OCS + ":", "" );
      final ObservationBean ob = new ObservationBean( obsId, "", "", null );

      final IObservationService srv = KalypsoGisPlugin.getDefault()
          .getObservationServiceProxy();

      // set the date range if available
      DateRangeBean drb = null;
      if( dra != null )
        drb = new DateRangeBean( dra.getFrom().getTime(), dra.getTo().getTime() );
      
      final OCSDataBean db = srv.readData( ob, drb );

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