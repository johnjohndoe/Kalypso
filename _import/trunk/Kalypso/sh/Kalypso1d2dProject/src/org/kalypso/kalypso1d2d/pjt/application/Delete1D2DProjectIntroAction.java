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
package org.kalypso.kalypso1d2d.pjt.application;

import java.util.Properties;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.ui.actions.DeleteResourceAction;
import org.eclipse.ui.intro.IIntroSite;
import org.eclipse.ui.intro.config.IIntroAction;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;

import de.renew.workflow.connector.context.ActiveWorkContext;

/**
 * @author Gernot Belger
 */
public class Delete1D2DProjectIntroAction implements IIntroAction
{
  /**
   * @see org.eclipse.ui.intro.config.IIntroAction#run(org.eclipse.ui.intro.IIntroSite, java.util.Properties)
   */
  public void run( final IIntroSite site, final Properties params )
  {
    /* Validate parameters */
    final String projectName = params.getProperty( "project", null );

    final IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject( projectName );
    if( !project.exists() )
      return;

    final ActiveWorkContext activeWorkContext = Kalypso1d2dProjectPlugin.getDefault().getActiveWorkContext();
    final IProject currentProject = activeWorkContext.getCurrentProject().getProject();
    // TODO: better the workflow context should be a resource listener and deactivate it himself
    if( project.equals( currentProject ))
      activeWorkContext.setActiveProject( null );
    
    final DeleteResourceAction deleteResourceAction = new DeleteResourceAction( site.getShell() );
    deleteResourceAction.selectionChanged( new StructuredSelection( project ) );
    deleteResourceAction.run();
  }

}
