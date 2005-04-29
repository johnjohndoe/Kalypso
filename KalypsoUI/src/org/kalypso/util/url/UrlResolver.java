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

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.UnknownServiceException;
import java.util.Iterator;
import java.util.Properties;

import org.eclipse.core.internal.resources.PlatformURLResourceConnection;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.java.io.RunAfterCloseOutputStream;
import org.kalypso.java.net.IUrlResolver;
import org.kalypso.java.net.UrlUtilities;

/**
 * <p>
 * Erzeugt aus einem String eine URL
 * </p>
 * <p>
 * Davor kann noch eine Token-Ersetzung stattfinden
 * </p>
 * 
 * TODO: untersuchen warum es auch org.kalypso.java.net.UrlUtilities gibt???
 * Marc.
 * 
 * @author belger
 */
public class UrlResolver implements IUrlResolver
{
  private Properties m_replaceTokenMap = new Properties();
  private final UrlUtilities m_urlUtilities = new UrlUtilities();

  /**
   * <p>
   * Löst eine URL relativ zu einer anderen auf.
   * </p>
   * <p>
   * Also handles the pseudo protocol 'project:'. If project: ist specified in
   * relativeURL, it tries to guess the project from the baseURL (e.g. the
   * baseURL must be of the form platfrom:/resource/). It then replaces project:
   * by 'platform:/resource/ <projectname>/
   * </p>
   * 
   * @param baseURL
   * @param relativeURL
   * @throws MalformedURLException
   */
  public URL resolveURL( final URL baseURL, final String relativeURL ) throws MalformedURLException
  {
    if( relativeURL.startsWith( "project:" ) )
    {
      if( !baseURL.toString().startsWith( PlatformURLResourceConnection.RESOURCE_URL_STRING ) )
        throw new MalformedURLException( "Protocol 'project:' need a resource url as context" );

      final IProject project = ResourceUtilities.findProjectFromURL( baseURL );
      final String projectURL = PlatformURLResourceConnection.RESOURCE_URL_STRING + "/"
          + project.getName();

      final String relPath = relativeURL.substring( "project:".length() + 1 );
      return new URL( projectURL + "/" + relPath );
    }
    else if( relativeURL.startsWith("http://") || relativeURL.startsWith("file:/") ){
      return new URL ( relativeURL );
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
   * @see org.kalypso.java.net.IUrlResolver#addReplaceToken(java.lang.String,
   *      java.lang.String)
   */
  public void addReplaceToken( final String key, final String value )
  {
    m_replaceTokenMap.setProperty( key, value );
  }

  /**
   * If URL denotes a location within the workspace, special handling is done. Else, we rely on {@link UrlUtilities}.
   * 
   * @throws IOException
   * @see org.kalypso.java.net.IUrlResolver#createWriter(java.net.URL)
   */
  public OutputStreamWriter createWriter( final URL url ) throws IOException
  {
    try
    {
      return m_urlUtilities.createWriter( url );
    }
    catch( final UnknownServiceException e )
    {
      final IFile file = ResourceUtilities.findFileFromURL( url );
      if( file != null )
      {
        final IPath path = file.getLocation();
        final File realFile = path.toFile();

        final Runnable runnable = new Runnable()
        {
          public void run()
          {
            try
            {
              file.refreshLocal( IResource.DEPTH_ONE, null );
            }
            catch( final CoreException ce )
            {
              // maybe there is better error handling than that?
              ce.printStackTrace();
            }
          }
        };
        
        if( !realFile.exists() )
          realFile.createNewFile();
        final OutputStream os = new RunAfterCloseOutputStream( new FileOutputStream( realFile ),
            runnable );

        String charset;
        try
        {
          charset = file.getCharset();
        }
        catch( final CoreException ce )
        {
          ce.printStackTrace();

          charset = null;
        }

        final OutputStreamWriter osw = charset == null ? new OutputStreamWriter( os )
            : new OutputStreamWriter( os, charset );
        return osw;
      }

      throw e;
    }
  }

  /**
   * Ausnahmebehandlung von Platform URLs. In diesem Fall anhand der Workbench das encoding bestimmen.
   * 
   * @see org.kalypso.java.net.IUrlResolver#createReader(java.net.URL)
   */
  public InputStreamReader createReader( final URL url ) throws IOException
  {
    try
    {
      final IFile file = ResourceUtilities.findFileFromURL( url );
      if( file != null )
      {
        final InputStream is = file.getContents();
        final String charset = file.getCharset();
        return new InputStreamReader( is, charset );
      }
    }
    catch( final CoreException e )
    {
      throw new IOException( e.getMessage() );
    }
    
    // wenn alles nichts hilfe, auf Standardzeug zurückgreifen
    return m_urlUtilities.createReader( url );
  }
}
