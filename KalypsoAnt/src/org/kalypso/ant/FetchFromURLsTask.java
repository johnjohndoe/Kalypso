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
package org.kalypso.ant;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URL;

import org.apache.commons.io.CopyUtils;
import org.apache.commons.io.IOUtils;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Task;
import org.kalypso.commons.java.net.UrlResolver;

/**
 * copies data from a url to a file <br>
 * the source url can be a list of URLs separated by a given separator (default separator is <code>,</code>)<br>
 * the task uses the first valid url to copy, If copy succeded task will finish, if not it will give a try to the next
 * valid URL in the list ... and so on
 * 
 * @author doemming
 */
public class FetchFromURLsTask extends Task
{
  private String m_dest;

  private String m_listSeparator;

  private URL m_context;

  private String m_src;

  /**
   * @param context
   *          used to resolve relative urls
   */
  public final void setContext( URL context )
  {
    m_context = context;
  }

  public final URL getContext( )
  {
    return m_context;
  }

  public final String getSrc( )
  {
    return m_src;
  }

  /**
   * @param src
   *          source URL or a list of source URLs
   */
  public final void setSrc( String src )
  {
    m_src = src;
  }

  public final void setDest( String dest )
  {
    m_dest = dest;
  }

  public final String getDest( )
  {
    return m_dest;
  }

  public final void setListSeparator( String listSeparator )
  {
    m_listSeparator = listSeparator;
  }

  public final String getListSeparator( )
  {
    return m_listSeparator;
  }

  /**
   * @see org.apache.tools.ant.Task#execute()
   */
  @Override
  public void execute( ) throws BuildException
  {
    // set Listseparator
    final String listSeparator;
    final String userListSeparator = getListSeparator();
    if( userListSeparator != null && userListSeparator.length() > 0 )
      listSeparator = userListSeparator;
    else
      listSeparator = ",";

    final UrlResolver resolver = new UrlResolver();

    final String dest = getDest();
    final File destination = new File( dest );

    final String srcList = getSrc();
    final String[] src = srcList.split( listSeparator );
    int i = 0;
    boolean succeded = false;
    final Project project2 = getProject();
    while( !succeded && i < src.length )
    {
      URL url = null;
      FileOutputStream outputStream = null;
      try
      {
        url = resolver.resolveURL( m_context, src[i] );
        final File parentFile = destination.getParentFile();
        if( !parentFile.exists() )
          parentFile.mkdirs();
        outputStream = new FileOutputStream( destination );
        CopyUtils.copy( url.openStream(), outputStream );
        if( project2 != null )
        {
          project2.log( "copied from url : " + url.toExternalForm(), Project.MSG_INFO );
          project2.log( "copied to file  : " + destination.toString(), Project.MSG_INFO );
        }
        succeded = true;
      }
      catch( IOException e )
      {
        if( project2 != null )
        {
          project2.log( "failed to fetch from url: " + url.toExternalForm(), Project.MSG_INFO );
          project2.log( e.getLocalizedMessage(), Project.MSG_INFO );
        }
        e.printStackTrace();
      }
      finally
      {
        IOUtils.closeQuietly( outputStream );
      }
      i++;
    }
    if( !succeded && project2 != null )
      project2.log( "failed to fetch data from any url", Project.MSG_ERR );
  }
}
