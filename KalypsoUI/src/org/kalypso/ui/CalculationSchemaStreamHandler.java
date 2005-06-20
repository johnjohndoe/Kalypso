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
package org.kalypso.ui;

import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;

import org.osgi.service.url.AbstractURLStreamHandlerService;

/**
 * @author belger
 */
public class CalculationSchemaStreamHandler extends AbstractURLStreamHandlerService
{
  public final static String PROTOCOL = "kalypso-calculation-schema";

  /**
   * @see java.net.URLStreamHandler#openConnection(java.net.URL)
   */
  public URLConnection openConnection( final URL u ) throws IOException
  {
    if( !PROTOCOL.equals( u.getProtocol() ) )
      throw new IOException( "Protocol not supported by this handler: " + u.getProtocol() );

    return new CalculationSchemaURLConnection( u );
  }

  /**
   * Overwritten for performance reasons. The super-method does hostname-lookup, which results in a huge perfomance
   * loss, becaue my 'hosts' are almost always not available.
   * 
   * @see org.osgi.service.url.AbstractURLStreamHandlerService#equals(java.net.URL, java.net.URL)
   */
  public boolean equals( final URL u1, final URL u2 )
  {
    return ( "" + u1 ).equals( "" + u2 );
  }
}
