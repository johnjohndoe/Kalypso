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
package org.kalypso.util.url;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Iterator;
import java.util.Properties;

import org.eclipse.core.internal.resources.PlatformURLResourceConnection;
import org.eclipse.core.resources.IProject;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.java.net.IUrlResolver;

/**
 * <p>Erzeugt aus einem String eine URL</p>
 * <p>Davor kann noch eine Token-Ersetzung stattfinden</p>
 * 
 * @author belger
 */
public class UrlResolver implements IUrlResolver
{
  private Properties m_replaceTokenMap = new Properties();

  /**
   * <p>Löst eine URL relativ zu einer anderen auf.</p>
   * <p>Also handles the pseudo protocol 'project:'. If project: ist specified in relativeURL,
   * it tries to guess the project from the baseURL (e.g. the baseURL must be of the form platfrom:/resource/).
   * It then replaces project: by 'platform:/resource/<projectname>/
   * </p>
   * @param baseURL
   * @param relativeURL
   * @return
   * @throws MalformedURLException
   */
  public URL resolveURL( final URL baseURL, final String relativeURL ) throws MalformedURLException
  {
    if( relativeURL.startsWith( "project:" ) )
    {
      if( !baseURL.toString().startsWith( PlatformURLResourceConnection.RESOURCE_URL_STRING ) )
        throw new MalformedURLException( "Protocol 'project:' need a resource url as context" );
      
      final IProject project = ResourceUtilities.findProjectFromURL( baseURL );
      final String projectURL = PlatformURLResourceConnection.RESOURCE_URL_STRING + "/" + project.getName();
      
      final String relPath = relativeURL.substring( "project:".length() + 1 );
      return new URL( projectURL + "/" + relPath );      
    }
    
    return new URL( baseURL, relativeURL );
  }

  /**
   * @see org.kalypso.java.net.IUrlResolver#getReplaceEntries()
   */
  public final Iterator getReplaceEntries()
  {
    return m_replaceTokenMap.entrySet().iterator();
  }

  /**
   * @see org.kalypso.java.net.IUrlResolver#addReplaceToken(java.lang.String, java.lang.String)
   */
  public void addReplaceToken( final String key, final String value )
  {
    m_replaceTokenMap.setProperty( key, value );
  }
}
