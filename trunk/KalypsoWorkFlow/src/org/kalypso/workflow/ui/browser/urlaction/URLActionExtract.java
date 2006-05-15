/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.workflow.ui.browser.urlaction;

import java.io.File;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.workflow.WorkflowContext;
import org.kalypso.workflow.ui.browser.AbstractURLAction;
import org.kalypso.workflow.ui.browser.ICommandURL;

/**
 * example<br>
 * kalypso://copy?source=http://kalypso.org/archive.zip&target=project:/
 * 
 * @author doemming
 */
public class URLActionExtract extends AbstractURLAction
{
  private final static String COMMAND_NAME = "extract";

  private final static String PARAM_SOURCE = "sourceURL";

  private final static String PARAM_TARGET = "target";

  /**
   * @see org.kalypso.contribs.eclipse.ui.browser.commandable.ICommandURLAction#run(org.kalypso.contribs.eclipse.ui.browser.commandable.ICommandURL)
   */
  public boolean run( ICommandURL commandURL )
  {
    final String source = commandURL.getParameter( PARAM_SOURCE );
    final String relativeTargetURL = commandURL.getParameter( PARAM_TARGET );
    final URL sourceURL;
    final WorkflowContext wfContext = getWorkFlowContext();
    try
    {
      sourceURL = wfContext.resolveURL( source );
    }
    catch( MalformedURLException e )
    {
      e.printStackTrace();
      return false;
    }

    InputStream resourceAsStream = null;
    try
    {
      final URL targetURL = wfContext.resolveURL( relativeTargetURL );
      // * note:
      // * next line can not find IFile for a url that points to a project and not to a resource inside a project
      // * IFile iFile ResourceUtilities.findFileFromURL(url);
      final File targetFile = ResourceUtilities.findFileFromURL2( targetURL );
      resourceAsStream = sourceURL.openStream();
      ZipUtilities.unzip( resourceAsStream, targetFile );
      // refresh on the whole project, cause we cannot refresh from IPath
      final IProject project = ResourceUtilities.findProjectFromURL( targetURL );
      project.refreshLocal( IResource.DEPTH_INFINITE, null );
      return true;
    }
    catch( Exception e )
    {
      e.printStackTrace();
      return false;
    }
    finally
    {
      IOUtils.closeQuietly( resourceAsStream );
    }
  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.browser.commandable.ICommandURLAction#getActionName()
   */
  public String getActionName( )
  {
    return COMMAND_NAME;
  }
}
