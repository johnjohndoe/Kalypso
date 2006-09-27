package org.kalypso.portal.intro;

import org.eclipse.swt.graphics.Point;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.application.ActionBarAdvisor;
import org.eclipse.ui.application.IActionBarConfigurer;
import org.eclipse.ui.application.IWorkbenchWindowConfigurer;
import org.eclipse.ui.application.WorkbenchWindowAdvisor;
import org.eclipse.ui.intro.IIntroManager;

public class KalypsoPortalWorkbenchWindowAdvisor extends WorkbenchWindowAdvisor
{


  public KalypsoPortalWorkbenchWindowAdvisor(  IWorkbenchWindowConfigurer configurer )
  {
    super( configurer );
  }

  @Override
  public ActionBarAdvisor createActionBarAdvisor( IActionBarConfigurer configurer )
  {
    return new KalypsoPlanerClientActionBuilder( configurer );
  }

  @Override
  public void preWindowOpen( )
  {
    IWorkbenchWindowConfigurer configurer = getWindowConfigurer();
    configurer.setInitialSize( new Point( 700, 550 ) );
    configurer.setShowPerspectiveBar( false );
    configurer.setShowProgressIndicator( true );
    configurer.setShowFastViewBars( true );
    configurer.setShowStatusLine( true );
    configurer.setTitle( "FLOWS Planer Portal" );

  }

  /**
   * @see org.eclipse.ui.application.WorkbenchWindowAdvisor#openIntro()
   */
  @Override
  public void openIntro( )
  {
    final IWorkbench workbench = getWindowConfigurer().getWorkbenchConfigurer().getWorkbench();
    final IIntroManager intromanager = workbench.getIntroManager();
    // starts the welcome page
    intromanager.showIntro( workbench.getActiveWorkbenchWindow(), false );
    // makes shure the welcome page is always maximized
    workbench.getActiveWorkbenchWindow().getShell().setMaximized( true );

  }
}