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
package org.kalypso.ogc.gml.wms.loader;

import java.io.InputStream;
import java.net.URL;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.ogc.gml.wms.utils.KalypsoWMSUtilities;
import org.kalypso.util.net.URLGetter;

/**
 * This loader loads the capabilities.
 * 
 * @author Holger Albert
 */
public class WMSCapabilitiesLoader implements ICapabilitiesLoader
{
  /**
   * The base URL of the service.
   */
  private URL m_baseURL;

  /**
   * The timeout for the access.
   */
  private int m_timeout;

  /**
   * The conbstructor.
   * 
   * @param timeout
   *            The timeout for the access.
   */
  public WMSCapabilitiesLoader( int timeout )
  {
    m_baseURL = null;
    m_timeout = timeout;
  }

  /**
   * @see org.kalypso.ogc.gml.wms.loader.ICapabilitiesLoader#init(java.net.URL)
   */
  public void init( URL baseURL )
  {
    m_baseURL = baseURL;
  }

  /**
   * @see org.kalypso.ogc.gml.wms.loader.ICapabilitiesLoader#getCapabilitiesStream(org.eclipse.core.runtime.IProgressMonitor)
   */
  public InputStream getCapabilitiesStream( IProgressMonitor monitor ) throws CoreException
  {
    if( m_baseURL == null )
      return null;

    monitor.beginTask( "Loading capabilities: ", 100 );

    try
    {
      monitor.subTask( "Creating the request ..." );

      /* Create the capabilities URL. */
      URL capabilitiesURL = KalypsoWMSUtilities.createCapabilitiesRequest( m_baseURL );

      monitor.worked( 25 );
      monitor.subTask( "Creating the object for getting the cababilities ..." );

      /* Create a getter for retrieving the URL. */
      URLGetter getter = URLGetter.createURLGetter( capabilitiesURL, m_timeout, 0 );

      monitor.worked( 25 );
      monitor.subTask( "Loading the capabilities ..." );

      /* Execute. */
      IStatus status = getter.execute( new SubProgressMonitor( monitor, 50 ) );
      if( !status.isOK() )
        throw new Exception( status.getException() );

      return getter.getResult();
    }
    catch( Exception ex )
    {
      throw new CoreException( new Status( IStatus.ERROR, "org.kalypso.ui", "Fehler beim Zugriff auf " + m_baseURL.toExternalForm(), ex ) );
    }
    finally
    {
      monitor.done();
    }
  }
}