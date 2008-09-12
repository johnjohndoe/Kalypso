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

import java.net.URL;

import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.kalypso.workflow.ui.browser.AbstractURLAction;
import org.kalypso.workflow.ui.browser.CommandURLBrowserView;
import org.kalypso.workflow.ui.browser.ICommandURL;

/**
 * example<br>
 * kalypso://openURL?url=...&activePartId=...
 * 
 * @author doemming
 */
public class URLActionOpenURL extends AbstractURLAction
{
  private static final String DEFAULT_BROWSER_VIEW = "org.kalypso.workflow.ui.WorkflowBrowserView";

  private final static String PARAM_URL = "url";

  private final static String PARAM_BROWSER_ID = "browserId";

  /**
   * @see org.kalypso.contribs.eclipse.ui.browser.commandable.ICommandURLAction#run(org.kalypso.contribs.eclipse.ui.browser.commandable.ICommandURL)
   */
  public boolean run( ICommandURL commandURL )
  {
    final IWorkbenchPage activePage = getActivePage();
    final String urlAsString = commandURL.getParameter( PARAM_URL );
    String id = commandURL.getParameter( PARAM_BROWSER_ID );
    try
    {
      // activate the appropriate view (select the folder), where the new View will be relative to and
      // shown in.
      if( id == null )
        id = DEFAULT_BROWSER_VIEW;
      final IViewPart part = activePage.showView( id );
      if( part instanceof CommandURLBrowserView )
      {
        final CommandURLBrowserView browser = (CommandURLBrowserView) part;
        final URL url = getWorkFlowContext().resolveURL( urlAsString );
        browser.setURL( url.toString() );
      }

    }
    catch( Exception e )
    {
      e.printStackTrace();
      return false;
    }
    return true;
  }

}
