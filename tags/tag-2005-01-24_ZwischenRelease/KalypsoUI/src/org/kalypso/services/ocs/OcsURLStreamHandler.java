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
import java.net.URL;
import java.net.URLConnection;

import org.apache.commons.io.FileUtils;
import org.kalypso.ogc.sensor.zml.ZmlURL;
import org.kalypso.services.ocs.repository.ServiceRepositoryObservation;
import org.kalypso.services.proxy.DateRangeBean;
import org.kalypso.services.proxy.IObservationService;
import org.kalypso.services.proxy.OCSDataBean;
import org.kalypso.services.proxy.ObservationBean;
import org.kalypso.ui.KalypsoGisPlugin;
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
  /**
   * @see java.net.URLStreamHandler#openConnection(java.net.URL)
   */
  public URLConnection openConnection( final URL u ) throws IOException
  {
    // is that an observation id of a server side observation?
    if( !ServiceRepositoryObservation.isServerSide( u.toExternalForm() ) )
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

      final String obsId = ZmlURL.getIdentifierPart( u );
      final ObservationBean ob = ServiceRepositoryObservation.getObservationBean( obsId );

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