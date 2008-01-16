/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
package org.kalypso.util.net;

import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpMethod;
import org.apache.commons.httpclient.StatusLine;
import org.apache.commons.httpclient.methods.GetMethod;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.net.ProxyUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.ui.KalypsoGisPlugin;

public class URLGetter implements ICoreRunnableWithProgress
{
  public static URLGetter createURLGetter( final URL url, final int timeOut, int retries )
  {
    try
    {
      final HttpClient client = ProxyUtilities.getConfiguredHttpClient( timeOut, new URL( url.getProtocol() + "://" + url.getHost() ), retries );
      return new URLGetter( client, url );
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();
    }

    return null;
  }

  private InputStream m_result = null;

  /** Will be returned by the execute method */
  private IStatus m_status = Status.OK_STATUS;

  private final URL m_url;

  private final HttpClient m_httpClient;

  public URLGetter( final HttpClient httpClient, final URL url )
  {
    m_httpClient = httpClient;
    m_url = url;
  }

  public InputStream getResult( )
  {
    return m_result;
  }

  protected void setResult( final InputStream result )
  {
    m_result = result;
  }

  /** Only to be called from the thread */
  protected void setStatus( final IStatus status )
  {
    m_status = status;
  }

  protected HttpClient getHttpClient( )
  {
    return m_httpClient;
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  public IStatus execute( final IProgressMonitor monitor )
  {
    final String urlAsString = m_url.toString();
    final HttpMethod method = new GetMethod( urlAsString );
    // do not forget the next line!
    method.setDoAuthentication( true );

    final Thread thread = new Thread()
    {
      /**
       * @see java.lang.Thread#run()
       */
      @Override
      public void run( )
      {
        try
        {
          getHttpClient().executeMethod( method );
          setResult( method.getResponseBodyAsStream() );
        }
        catch( final IOException e )
        {
          final IStatus status;

          String responseBodyAsString = "Response Body not available.";
          try
          {
            responseBodyAsString = method.getResponseBodyAsString();
          }
          catch( final IOException e1 )
          {
            final IStatus status2 = StatusUtilities.statusFromThrowable( e1 );
            KalypsoGisPlugin.getDefault().getLog().log( status2 );
          }

          status = new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0, "Fehler beim Zugriff auf: " + urlAsString + "\n" + responseBodyAsString, e );
          setStatus( status );
        }
      }
    };

    monitor.beginTask( urlAsString, IProgressMonitor.UNKNOWN );
    monitor.subTask( "Initialisiere Verbindung ..." );
    thread.start();
    while( thread.isAlive() )
    {
      try
      {
        Thread.sleep( 100 );
      }
      catch( final InterruptedException e1 )
      {
        // should never happen, ignore
        e1.printStackTrace();
      }

      final String statusText;
      final StatusLine statusLine = method.getStatusLine();
      if( statusLine == null )
        statusText = "Verbindung wird aufgebaut ...";
      else
        statusText = method.getStatusText();

      monitor.subTask( statusText );
      monitor.worked( 1 );
      if( monitor.isCanceled() )
      {
        // TODO: this does not stop the thread!
        thread.interrupt();

        monitor.done();
        return Status.CANCEL_STATUS;
      }
    }

    monitor.done();
    return m_status;
  }
}