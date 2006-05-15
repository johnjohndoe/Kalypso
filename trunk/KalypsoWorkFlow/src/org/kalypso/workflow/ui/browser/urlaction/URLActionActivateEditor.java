/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

import java.net.URL;

import org.eclipse.core.resources.IFile;
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.IEditorRegistry;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.part.FileEditorInput;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.workflow.ui.browser.AbstractURLAction;
import org.kalypso.workflow.ui.browser.ICommandURL;

/**
 * example<br>
 * kalypso://openEditor?input=project:file&activate=true&contentType=gmt
 * 
 * @author doemming
 */
public class URLActionActivateEditor extends AbstractURLAction
{
  private final static String COMMAND_NAME = "openEditor";

  private final static String PARAM_INPUT = "input";

  /**
   * optional e.g. "gmt"
   */
  private final static String PARAM_CONTENT_TYPE = "contentType";

  /**
   * optional boolean to control if editor should be active after open<br>
   * default is <code>false</code>
   */
  private final static String PARAM_ACTIVATE = "activate";

  /**
   * @see org.kalypso.contribs.eclipse.ui.browser.commandable.ICommandURLAction#run(org.kalypso.contribs.eclipse.ui.browser.commandable.ICommandURL)
   */
  public boolean run( ICommandURL commandURL )
  {
    final IWorkbenchPage activePage = getActivePage();
    final String inputLocation = commandURL.getParameter( PARAM_INPUT );
    final String activateEditorAsString = commandURL.getParameter( PARAM_ACTIVATE );
    final String contentTypeAsString = commandURL.getParameter( PARAM_CONTENT_TYPE );
    final boolean activateEditor = Boolean.parseBoolean( activateEditorAsString );
    final IWorkbench workbench = getWorkbench();
    final IEditorRegistry editorRegistry = workbench.getEditorRegistry();
    try
    {
      final URL inputURL = getWorkFlowContext().resolveURL( inputLocation );
      final IEditorDescriptor defaultDefaultDescriptor;
      if( contentTypeAsString != null && contentTypeAsString.length() > 0 )
        defaultDefaultDescriptor = editorRegistry.getDefaultEditor( contentTypeAsString );
      else
        defaultDefaultDescriptor = editorRegistry.getDefaultEditor( inputURL.toString() );
      final IFile file = ResourceUtilities.findFileFromURL( inputURL );
      final FileEditorInput input = new FileEditorInput( file );
      activePage.openEditor( input, defaultDefaultDescriptor.getId(), activateEditor );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    return true;
  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.browser.commandable.ICommandURLAction#getActionName()
   */
  public String getActionName( )
  {
    return COMMAND_NAME;
  }
}
