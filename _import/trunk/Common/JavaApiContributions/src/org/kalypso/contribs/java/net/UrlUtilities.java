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
package org.kalypso.contribs.java.net;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
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
   * @return if relativeURL is relative, return new URL( baseURL, relativeURL ), else return new URL( relativeURL )
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
   * @see org.kalypso.contribs.java.net.IUrlResolver#getReplaceEntries()
   */
  public final Iterator getReplaceEntries()
  {
    return m_replaceTokenMap.entrySet().iterator();
  }

  /**
   * @see org.kalypso.contribs.java.net.IUrlResolver#addReplaceToken(java.lang.String, java.lang.String)
   */
  public void addReplaceToken( final String key, final String value )
  {
    m_replaceTokenMap.setProperty( key, value );
  }

  /**
   * @throws IOException
   * @see org.kalypso.contribs.java.net.IUrlResolver#createWriter(java.net.URL)
   */
  public OutputStreamWriter createWriter( final URL url ) throws IOException
  {
    final URLConnection connection = url.openConnection();
    try
    {
      connection.setDoOutput( true );

      final OutputStream outputStream = connection.getOutputStream();
      return new OutputStreamWriter( outputStream );
    }
    catch( final UnknownServiceException e )
    {
      // in diesem Fall unterstütz die URL kein Output

      // jetzt versuchen, selbst einen Stream zu öffnen
      final String protocol = url.getProtocol();
      if( "file".equals( protocol ) )
      {
        final File file = new File( url.getFile() );
        return new OutputStreamWriter( new FileOutputStream( file ) );
      }

      // wenn alles nichts hilfe, doch die esception werden
      throw e;
    }
  }

  /**
   * Erzeugt den Reader anhand der URL und {@link URLConnection#getContentEncoding()}.
   * 
   * @see org.kalypso.contribs.java.net.IUrlResolver#createReader(java.net.URL)
   */
  public InputStreamReader createReader( final URL url ) throws IOException
  {
    final URLConnection connection = url.openConnection();
    final String contentEncoding = connection.getContentEncoding();
    final InputStream inputStream = connection.getInputStream();
    // TODO: unsauber! wenn kein encoding da, am besten keinen Reader verwenden,
    // da sonst z.B: probleme beim xml-parsen
    if( contentEncoding == null )
      return new InputStreamReader( inputStream );

    return new InputStreamReader( inputStream, contentEncoding );
  }

  /**
   * Öffnet die URL-Connection und gibt null zurück bei Fehler
   */
  public static URLConnection connectQuietly( URL url )
  {
    URLConnection connection;

    try
    {
      connection = url.openConnection();
      connection.connect();
    }
    catch( IOException e )
    {
      connection = null;
    }
    return connection;
  }
}
