package org.kalypso.portal.intro;

import org.eclipse.swt.graphics.Point;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.application.ActionBarAdvisor;
import org.eclipse.ui.application.IActionBarConfigurer;
import org.eclipse.ui.application.IWorkbenchWindowConfigurer;
import org.eclipse.ui.application.WorkbenchWindowAdvisor;
import org.eclipse.ui.intro.IIntroManager;

public class ApplicationWorkbenchWindowAdvisor extends WorkbenchWindowAdvisor
{

  public ApplicationWorkbenchWindowAdvisor( IWorkbenchWindowConfigurer configurer )
  {
    super( configurer );
  }

  @Override
  public ActionBarAdvisor createActionBarAdvisor( IActionBarConfigurer configurer )
  {
    return new ApplicationActionBarAdvisor( configurer );
  }

  @Override
  public void preWindowOpen( )
  {
    IWorkbenchWindowConfigurer configurer = getWindowConfigurer();
    configurer.setInitialSize( new Point( 700, 550 ) );
    configurer.setShowCoolBar( false );
    configurer.setShowStatusLine( false );
    configurer.setShowProgressIndicator( true );
    configurer.setTitle( "FLOWS Planer Portal" );

  }

  /**
   * @see org.eclipse.ui.application.WorkbenchWindowAdvisor#openIntro()
   */
  @Override
  public void openIntro( )
  {
    final IWorkbench workbench = getWindowConfigurer().getWorkbenchConfigurer().getWorkbench();
    final IIntroManager introManager = workbench.getIntroManager();
    // starts the welcome page
    introManager.showIntro( workbench.getActiveWorkbenchWindow(), false );
    // makes shure the Welcome page is always maximized
    workbench.getActiveWorkbenchWindow().getShell().setMaximized( true );
  }
}
