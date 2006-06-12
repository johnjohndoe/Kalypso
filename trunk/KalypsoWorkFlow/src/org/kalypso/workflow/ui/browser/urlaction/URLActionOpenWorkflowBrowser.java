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

import java.net.URL;

import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.kalypso.workflow.WorkflowContext;
import org.kalypso.workflow.ui.browser.AbstractURLAction;
import org.kalypso.workflow.ui.browser.CommandURLBrowserView;
import org.kalypso.workflow.ui.browser.ICommandURL;

/**
 * This URLAction opens the WorkflowBrowser relatvie to a optional ViewPart
 * 
 * @author kuepfer
 */
public class URLActionOpenWorkflowBrowser extends AbstractURLAction
{
  /**
   * The id of the view ExtenstionPoint where the WorkflowBrowser is registerd.
   * 
   * @see org.kalypso.workflow.ui.KalypsoWorkFlowPlugin (plugin.xml)
   */
  private final String WORKFLOW_BROWSER_PART_ID = "org.kalypso.workflow.ui.WorkflowBrowserView";

  private final String COMMAND_NAME = "openWorkflowBrowser";

  /**
   * required: the URL to load the first page from this is also the first context of the Browser
   */
  private final String PARAM_URL = "url";

  private final static String PARAM_SECONDARY_PART_ID = "secondaryPartId";

  /**
   * optional: activePartId to open the browser relative to.
   */
  private final String PARAM_ACTIVE_PARTID = "activePartId";

  /**
   * @see org.kalypso.workflow.ui.browser.IURLAction#getActionName()
   */
  public String getActionName( )
  {
    return COMMAND_NAME;
  }

  /**
   * @see org.kalypso.workflow.ui.browser.IURLAction#run(org.kalypso.workflow.ui.browser.ICommandURL)
   */
  public boolean run( ICommandURL commandURL )
  {
    final String activePartId = commandURL.getParameter( PARAM_ACTIVE_PARTID );
    final String secondaryViewID = commandURL.getParameter( PARAM_SECONDARY_PART_ID );
    final String url = commandURL.getParameter( PARAM_URL );
    IViewPart viewPart = null;
    try
    {
      // activate the appropriate view (select the folder), where the new View will be relative to and
      // shown in
      final IWorkbenchPage activePage = getActivePage();
      if( activePartId != null && activePartId.length() > 1 )
        activePage.showView( activePartId );
      if( secondaryViewID != null )
        viewPart = activePage.showView( WORKFLOW_BROWSER_PART_ID, secondaryViewID, IWorkbenchPage.VIEW_ACTIVATE );
      else
        viewPart = activePage.showView( WORKFLOW_BROWSER_PART_ID );
      // set view active
      activePage.bringToTop( viewPart );
      if( viewPart instanceof CommandURLBrowserView )
      {
        final CommandURLBrowserView browserView = (CommandURLBrowserView) viewPart;
        WorkflowContext workFlowContext = getWorkFlowContext();
        URL url2 = workFlowContext.resolveURL( url );
        (browserView).setURL( url2.toString() );
      }
      else
        return false; // should never happen only of the browser view was not found
    }
    catch( Exception e )
    {
      e.printStackTrace();
      return false;
    }
    return true;
  }

}
