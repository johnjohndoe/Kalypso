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

import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLStreamHandler;
import java.util.Hashtable;
import java.util.Map;

/**
 * A URL context replacement based <code>URLStreamHandler</code>.
 * <p>
 * Removes the protocol of the <code>URL</code> and builds a new URL
 * using the mapped context URL for this protocol.
 * <p>
 * Example:
 * <pre>
 * URL input: project:foo/bar/test.txt#script#foobar
 * 
 * protocol: project
 * replacement: URL=file:C:/Temp/Project/
 * 
 * the URL will be created using the following syntax:
 * URL=file:C:/Temp/Project/foo/bar/test.txt#script#foobar
 * </pre>
 * 
 * @author schlienger
 */
public class ContextMappingURLStreamHandler extends URLStreamHandler
{
  private final Map m_replacements = new Hashtable();

  /**
   * Sets the replacement for the given protocol string.
   */
  public void setStringReplacement( final String protocol, final URL context )
  {
    m_replacements.put( protocol, context );
  }

  /**
   * @see java.net.URLStreamHandler#openConnection(java.net.URL)
   */
  protected URLConnection openConnection( final URL u ) throws IOException
  {
    final String protocol = u.getProtocol();

    if( !m_replacements.containsKey( protocol ) )
      return u.openConnection();

    final URL context = (URL)m_replacements.get( protocol );

    return new URL( context, u.toExternalForm().replaceFirst( protocol + ":", "" ) ).openConnection();
  }
}