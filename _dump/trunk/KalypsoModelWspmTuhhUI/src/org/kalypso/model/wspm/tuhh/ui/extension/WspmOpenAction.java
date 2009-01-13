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
package org.kalypso.model.wspm.tuhh.ui.extension;

import java.util.Properties;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.IPerspectiveDescriptor;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.intro.IIntroManager;
import org.kalypso.afgui.extension.IKalypsoProjectOpenAction;
import org.kalypso.model.wspm.ui.product.ProfileManagerPerspective;

/**
 * @author kuch
 */
public class WspmOpenAction implements IKalypsoProjectOpenAction
{

  /**
   * @see org.kalypso.afgui.extension.IKalypsoProjectOpenAction#open(java.util.Properties)
   */
  @Override
  public IStatus open( final Properties properties )
  {
    try
    {
      /* Validate parameters */
      final String projectName = properties.getProperty( "project", null ); //$NON-NLS-1$

      final IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject( projectName );
      if( !project.exists() || !project.isOpen() )
        return Status.CANCEL_STATUS;

      /* hide intro */
      final IWorkbench workbench = PlatformUI.getWorkbench();
      final IIntroManager introManager = workbench.getIntroManager();
      introManager.closeIntro( introManager.getIntro() );

      final IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
      final IWorkbenchPage page = window.getActivePage();
      if( page == null )
        return Status.CANCEL_STATUS;

      /* close unused perspectives */
      final IPerspectiveDescriptor[] perspectives = page.getOpenPerspectives();
      for( final IPerspectiveDescriptor descriptor : perspectives )
      {
        final String id = descriptor.getId();
        if( id.equals( ProfileManagerPerspective.ID ) )
          continue;
        else if( descriptor != null )
          page.closePerspective( descriptor, true, false );
      }

      final IPerspectiveDescriptor descriptor = page.getWorkbenchWindow().getWorkbench().getPerspectiveRegistry().findPerspectiveWithId( ProfileManagerPerspective.ID );
      if( descriptor != null )
        page.setPerspective( descriptor );

      page.showView( "org.kalypso.featureview.views.FeatureView" );
    }
    catch( final PartInitException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

    return Status.OK_STATUS;
  }

}
