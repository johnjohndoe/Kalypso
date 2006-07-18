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

import org.eclipse.ui.IViewPart;
import org.kalypso.workflow.ui.browser.AbstractURLAction;
import org.kalypso.workflow.ui.browser.CommandURLBrowserView;
import org.kalypso.workflow.ui.browser.ICommandURL;

/**
 * example<br>
 * TODO kalypso://showMessage?message=example&title=Warning
 * 
 * @author doemming
 */
public class URLActionNavigate extends AbstractURLAction
{
//  private final static String COMMAND_NAME = "navigate";

  private final static String PARAM_DIRECTION = "direction";

  private static final String PARAM_BACKWARD = "backward";

  private static final String PARAM_FORWARD = "forward";

  private static final String PARAM_HOME = "home";

  /**
   * @see org.kalypso.contribs.eclipse.ui.browser.commandable.ICommandURLAction#run(org.kalypso.contribs.eclipse.ui.browser.commandable.ICommandURL)
   */
  public boolean run( ICommandURL commandURL )
  {
    final String direction = commandURL.getParameter( PARAM_DIRECTION );
    final IViewPart part = getActivePage().findView( CommandURLBrowserView.WEB_BROWSER_VIEW_ID );
    if( part != null && (part instanceof CommandURLBrowserView) )
    {
      if( direction.equalsIgnoreCase( PARAM_BACKWARD ) )
      {
        ((CommandURLBrowserView) part).navigateBack();
        return true;
      }
      else if( direction.equalsIgnoreCase( PARAM_FORWARD ) )
      {
        ((CommandURLBrowserView) part).navigateForward();
        return true;
      }
      else if( direction.equalsIgnoreCase( PARAM_HOME ) )
      {
        ((CommandURLBrowserView)part).navigateHome();
        return false;
      }
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
