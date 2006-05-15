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
package org.kalypso.workflow.ui.browser;

import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.kalypso.workflow.WorkflowContext;

/**
 * @author kuepfer
 */
public abstract class AbstractURLAction implements IURLAction
{

  private WorkflowContext m_workFlowContext = null;

  /**
   * init will called befor run
   */
  public void init( final WorkflowContext workFlowActivityContext )
  {
    m_workFlowContext = workFlowActivityContext;
  }

  public WorkflowContext getWorkFlowContext( )
  {
    return m_workFlowContext;
  }

  protected IWorkbench getWorkbench( )
  {
    return PlatformUI.getWorkbench();
  }

  protected IWorkbenchPage getActivePage( )
  {
    return getWorkbench().getActiveWorkbenchWindow().getActivePage();
  }

  protected IWorkspaceRoot getWorkspaceRoot( )
  {
    return ResourcesPlugin.getWorkspace().getRoot();
  }

}
