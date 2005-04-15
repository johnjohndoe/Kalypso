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
package org.kalypso.java.net;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLConnection;
import java.net.UnknownServiceException;
import java.util.Iterator;
import java.util.Properties;

/**
 * @author belger
 */
public class UrlUtilities implements IUrlResolver
{
  private Properties m_replaceTokenMap = new Properties();

  /**
   * <p>
   * Resolves a (potential) relative URL to a base URL.
   * </p>
   * 
   * @param baseURL
   *          URL, to which the relative url will be resolved
   * @param relativeURL
   *          a string designating an absolute or relative URL
   * @return if relativeURL is relative, return new URL( baseURL, relativeURL ),
   *         else return new URL( relativeURL )
   * @throws MalformedURLException
   */
  public URL resolveURL( final URL baseURL, final String relativeURL ) throws MalformedURLException
  {
    try
    {
      final URI uri = new URI( relativeURL );
      return uri.isAbsolute() ? new URL( relativeURL ) : new URL( baseURL, relativeURL );
    }
    catch( final URISyntaxException e )
    {
      throw new MalformedURLException( e.getLocalizedMessage() );
    }
  }

  /**
   * @see org.kalypso.java.net.IUrlResolver#getReplaceEntries()
   */
  public final Iterator getReplaceEntries()
  {
    return m_replaceTokenMap.entrySet().iterator();
  }

  /**
   * @see org.kalypso.java.net.IUrlResolver#addReplaceToken(java.lang.String,
   *      java.lang.String)
   */
  public void addReplaceToken( final String key, final String value )
  {
    m_replaceTokenMap.setProperty( key, value );
  }

  /**
   * @throws IOException
   * @see org.kalypso.java.net.IUrlResolver#createBufferedWriter(java.net.URL)
   */
  public BufferedWriter createBufferedWriter( final URL url ) throws IOException
  {
    final URLConnection connection = url.openConnection();
    try
    {
      connection.setDoOutput( true );

      final OutputStream outputStream = connection.getOutputStream();
      return new BufferedWriter( new OutputStreamWriter( outputStream ) );
    }
    catch( final UnknownServiceException e )
    {
      // in diesem Fall unterst�tz die URL kein Output
      
      // jetzt versuchen, selbst einen Stream zu �ffnen
      final String protocol = url.getProtocol();
      if( "file".equals( protocol ) )
      {
        final File file = new File( url.getFile() );
        return new BufferedWriter( new OutputStreamWriter( new FileOutputStream( file ) ) );
      }
      
      // wenn alles nichts hilfe, doch die esception werden
      throw e;
    }
  }
}