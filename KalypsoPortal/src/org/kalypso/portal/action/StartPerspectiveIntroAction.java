package org.kalypso.portal.action;

import java.util.Properties;

import org.eclipse.ui.IPerspectiveDescriptor;
import org.eclipse.ui.IPerspectiveRegistry;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.intro.IIntroSite;

public class StartPerspectiveIntroAction extends AbstractIntroAction

{

  // same as openURL, but resolves relative links
  public final static String PARAM_ID = "perspectivId";

  @Override
  public void run( IIntroSite site, Properties params )
  {
    final String wishId = params.getProperty( PARAM_ID );
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final IWorkbenchWindow activeWorkbenchWindow = workbench.getActiveWorkbenchWindow();
    final IWorkbenchPage activePage = activeWorkbenchWindow.getActivePage();
    final IPerspectiveRegistry perspectiveRegistry = workbench.getPerspectiveRegistry();
    final IPerspectiveDescriptor[] perspectives = perspectiveRegistry.getPerspectives();
    for( int i = 0; i < perspectives.length; i++ )
    {
      final IPerspectiveDescriptor descriptor = perspectives[i];
      final String id = descriptor.getId();
      if( id.equals( wishId ) )
      {
        activePage.setPerspective( descriptor );
        break;
      }
    }
  }

}
