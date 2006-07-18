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
package org.kalypso.workflow.ui.browser.urlaction;

import java.io.InputStreamReader;
import java.net.URL;
import java.util.Properties;

import org.eclipse.core.resources.IProject;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.XMLMemento;
import org.kalypso.contribs.eclipse.ui.MementoUtils;
import org.kalypso.contribs.eclipse.ui.MementoWithUrlResolver;
import org.kalypso.contribs.java.net.IUrlResolver2;
import org.kalypso.workflow.ui.browser.AbstractURLAction;
import org.kalypso.workflow.ui.browser.ICommandURL;

/**
 * example<br>
 * kalypso://changePerspective?stateURL=relative/state_xyz.xml
 * 
 * @author doemming
 */
public class URLActionChangePerspective extends AbstractURLAction
{
  public final static String PARAM_STATE_URL = "stateURL";

//  public final static String COMMAND_NAME = "changePerspective";

  /**
   * @see org.kalypso.contribs.eclipse.ui.browser.commandable.ICommandURLAction#run(org.kalypso.contribs.eclipse.ui.browser.commandable.ICommandURL)
   */
  public boolean run( ICommandURL commandURL )
  {
    final IProject contextProject = getWorkFlowContext().getContextProject();
    if( contextProject == null )
      return false; // TODO error message

    final Properties replaceProps = new Properties();
    replaceProps.setProperty( MementoWithUrlResolver.PATH_KEY, getWorkspaceRoot().getLocation().toString() );
    replaceProps.setProperty( MementoWithUrlResolver.PROJECT_KEY, contextProject.getName() );

    final String relativeStateURL = commandURL.getParameter( PARAM_STATE_URL );

    try
    {
      final IUrlResolver2 resolver = getWorkFlowContext();
      final URL stateURL = resolver.resolveURL( relativeStateURL );
      final InputStreamReader reader = new InputStreamReader( stateURL.openStream() );
      final XMLMemento originalMemento = XMLMemento.createReadRoot( reader );
      final IMemento memento = MementoUtils.createMementoWithUrlResolver( originalMemento, replaceProps, resolver );
      MementoUtils.restoreWorkbenchPage( memento );
      return true;
    }
    catch( Exception e )
    {
      e.printStackTrace();
      // handleException( e, getActivePage() );
    }
    return false;
  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.browser.commandable.ICommandURLAction#getActionName()
   */
  public String getActionName( )
  {
    return m_commandName;
  }
}
