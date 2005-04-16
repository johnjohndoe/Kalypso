/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
  
---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.application;

import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceDescription;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.WorkbenchException;
import org.eclipse.ui.application.IActionBarConfigurer;
import org.eclipse.ui.application.IWorkbenchConfigurer;
import org.eclipse.ui.application.IWorkbenchWindowConfigurer;
import org.eclipse.ui.internal.WorkbenchPlugin;
import org.eclipse.ui.internal.ide.IDEWorkbenchAdvisor;
import org.eclipse.ui.internal.registry.ActionSetRegistry;
import org.eclipse.ui.internal.registry.IActionSetDescriptor;
import org.kalypso.product.IKalypsoProductConstants;
import org.kalypso.services.user.UserServiceConstants;

/**
 * @author belger
 */
public class KalypsoWorkbenchAdvisor extends IDEWorkbenchAdvisor
{
  private final String[] m_rights;

  public KalypsoWorkbenchAdvisor( final String[] rights )
  {
    super();
    
    m_rights = rights;
  }
  
  /**
   * @see org.eclipse.ui.application.WorkbenchAdvisor#initialize(org.eclipse.ui.application.IWorkbenchConfigurer)
   */
  public void initialize( final IWorkbenchConfigurer configurer )
  {
    super.initialize( configurer );
    
    final ActionSetRegistry reg = WorkbenchPlugin.getDefault().getActionSetRegistry();
    final IActionSetDescriptor[] array = reg.getActionSets();
    final int count = array.length;
    for( int nX = 0; nX < count; nX++ )
    {
      final IActionSetDescriptor desc = array[nX];
      
      desc.setInitiallyVisible( desc.getId().startsWith( "org.kalypso" ) );
    }
  }
  
  private boolean hasRight( final String right )
  {
    for( int i = 0; i < m_rights.length; i++ )
    {
      if( m_rights[i].equals( right ) )
        return true;
    }
    
    return false;
  }
  
  /**
   * @see org.eclipse.ui.application.WorkbenchAdvisor#getInitialWindowPerspectiveId()
   */
  public String getInitialWindowPerspectiveId()
  {
    return IKalypsoProductConstants.PROGNOSE_PERSPECTIVE;
  }
  
  /**
   * @see org.eclipse.ui.internal.ide.IDEWorkbenchAdvisor#fillActionBars(org.eclipse.ui.IWorkbenchWindow, org.eclipse.ui.application.IActionBarConfigurer, int)
   */
  public void fillActionBars( final IWorkbenchWindow window, final IActionBarConfigurer actionConfigurer,
      final int flags )
  {
    super.fillActionBars( window, actionConfigurer, flags );

    if( !hasRight( UserServiceConstants.RIGHT_ADMIN ) )
    {
      final IMenuManager menuManager = actionConfigurer.getMenuManager();
      
//      // Men�s umbauen
//      final IMenuManager windowMenu = (IMenuManager)menuManager.find( IWorkbenchActionConstants.M_WINDOW );
//      final IMenuManager showViewMenu = (IMenuManager)windowMenu.find( "showView" );
//      menuManager.insertAfter( IWorkbenchActionConstants.M_EDIT, showViewMenu );
  
      // Men�s entfernen
      menuManager.remove( IWorkbenchActionConstants.M_PROJECT );
//      menuManager.remove( IWorkbenchActionConstants.M_NAVIGATE );
//      menuManager.remove( IWorkbenchActionConstants.M_WINDOW );
      
//      final IMenuManager fileMenu = (IMenuManager)menuManager.find( IWorkbenchActionConstants.M_FILE );
//      fileMenu.remove( "move" );
//      fileMenu.remove( "openWorkspace" );
//      fileMenu.remove( "import" );
//      fileMenu.remove( "export" );
  
      final IMenuManager editMenu = (IMenuManager)menuManager.find( IWorkbenchActionConstants.M_EDIT );
      editMenu.remove( "bookmark" );
      editMenu.remove( "addTask" );
  
      final IMenuManager helpMenu = menuManager.findMenuUsingPath( IWorkbenchActionConstants.M_HELP );
      helpMenu.remove( "tipsAndTricks" );
    }
  }
  
  /**
   * @see org.eclipse.ui.internal.ide.IDEWorkbenchAdvisor#preWindowOpen(org.eclipse.ui.application.IWorkbenchWindowConfigurer)
   */
  public void preWindowOpen( final IWorkbenchWindowConfigurer windowConfigurer )
  {
    super.preWindowOpen( windowConfigurer );
    

    if( !hasRight( UserServiceConstants.RIGHT_EXPERT ) && !hasRight( UserServiceConstants.RIGHT_ADMIN ) )
    {
      windowConfigurer.setShowCoolBar( false );
      windowConfigurer.setShowFastViewBars( false );
      windowConfigurer.setShowMenuBar( false );
      windowConfigurer.setShowPerspectiveBar( false );
      windowConfigurer.setShowProgressIndicator( false );
      windowConfigurer.setShowStatusLine( false );
    }
  }
  
  /**
   * @see org.eclipse.ui.internal.ide.IDEWorkbenchAdvisor#postStartup()
   */
  public void postStartup()
  {
    super.postStartup();

    // Das 'AutoBuild' aktivieren, damit das 'BuildAll' Icon in der Toolbar verschwindet
    final IWorkspace workspace = ResourcesPlugin.getWorkspace();
    final IWorkspaceDescription description = workspace.getDescription();
    description.setAutoBuilding( true );
    
    try
    {
      workspace.setDescription( description );
    }
    catch( CoreException e )
    {
      e.printStackTrace();
    }
    
    // immer in der Prognose-Perspective starten!
    final IWorkbench workbench = PlatformUI.getWorkbench();
    try
    {
      workbench.showPerspective( IKalypsoProductConstants.PROGNOSE_PERSPECTIVE, workbench.getActiveWorkbenchWindow() );
    }
    catch( final WorkbenchException e )
    {
      e.printStackTrace();
    }
  }
}
