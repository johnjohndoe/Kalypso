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
package org.kalypso.portal.action;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Properties;

import org.eclipse.swt.browser.Browser;
import org.eclipse.ui.internal.intro.impl.IntroPlugin;
import org.eclipse.ui.internal.intro.impl.presentations.BrowserIntroPartImplementation;
import org.eclipse.ui.intro.IIntroSite;
import org.eclipse.ui.intro.config.IIntroAction;
import org.kalypso.workflow.WorkflowContext;
import org.kalypso.workflow.ui.KalypsoWorkFlowPlugin;

/**
 * @author doemming
 */
public abstract class AbstractIntroAction implements IIntroAction
// extends Action
{
  private Browser m_browser = null;

  public void run( IIntroSite site, Properties params )
  {
    final BrowserIntroPartImplementation impl = (BrowserIntroPartImplementation) IntroPlugin.getDefault().getIntroModelRoot().getPresentation().getIntroParttImplementation();
    m_browser = impl.getBrowser();
    final String urlAsString = m_browser.getUrl();
    final WorkflowContext wfContext = KalypsoWorkFlowPlugin.getDefault().getDefaultWorkflowContext();
    try
    {
      final URL url = new URL( urlAsString );
      wfContext.setContextActionURL( url );
    }
    catch( MalformedURLException e )
    {
      // quiet
    }
  }

  protected Browser getBrowser( )
  {
    return m_browser;
  }
}
