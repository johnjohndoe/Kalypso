package org.kalypso.ui.application;

import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.WorkbenchException;
import org.eclipse.ui.application.IActionBarConfigurer;
import org.eclipse.ui.application.IWorkbenchConfigurer;
import org.eclipse.ui.application.IWorkbenchWindowConfigurer;
import org.eclipse.ui.internal.ide.IDEWorkbenchAdvisor;
import org.kalypso.ui.IKalypsoUIConstants;

/**
 * @author belger
 */
public class PrognoseWorkbenchAdvisor extends IDEWorkbenchAdvisor
{
  public PrognoseWorkbenchAdvisor()
  {
    super();
  }

  /**
   * @see org.eclipse.ui.application.WorkbenchAdvisor#initialize(org.eclipse.ui.application.IWorkbenchConfigurer)
   */
  public void initialize( final IWorkbenchConfigurer configurer )
  {
    super.initialize( configurer );
  }

  /**
   * @see org.eclipse.ui.application.WorkbenchAdvisor#getInitialWindowPerspectiveId()
   */
  public String getInitialWindowPerspectiveId()
  {
    return IKalypsoUIConstants.MODELER_PERSPECTIVE;
  }

  /**
   * @see org.eclipse.ui.internal.ide.IDEWorkbenchAdvisor#fillActionBars(org.eclipse.ui.IWorkbenchWindow,
   *      org.eclipse.ui.application.IActionBarConfigurer, int)
   */
  public void fillActionBars( final IWorkbenchWindow window,
      final IActionBarConfigurer actionConfigurer, final int flags )
  {
    super.fillActionBars( window, actionConfigurer, flags );
  }

  /**
   * @see org.eclipse.ui.internal.ide.IDEWorkbenchAdvisor#preWindowOpen(org.eclipse.ui.application.IWorkbenchWindowConfigurer)
   */
  public void preWindowOpen( final IWorkbenchWindowConfigurer windowConfigurer )
  {
    super.preWindowOpen( windowConfigurer );

    windowConfigurer.setShowCoolBar( false );
    windowConfigurer.setShowFastViewBars( false );
    windowConfigurer.setShowMenuBar( false );
    windowConfigurer.setShowPerspectiveBar( false );
    windowConfigurer.setShowProgressIndicator( false );
    windowConfigurer.setShowStatusLine( false );
  }

  /**
   * @see org.eclipse.ui.internal.ide.IDEWorkbenchAdvisor#postStartup()
   */
  public void postStartup()
  {
    super.postStartup();

    final IWorkbench workbench = PlatformUI.getWorkbench();
    try
    {
      workbench.showPerspective( IKalypsoUIConstants.REPOSITORY_PERSPECTIVE, workbench.getActiveWorkbenchWindow() );
    }
    catch( final WorkbenchException e )
    {
      e.printStackTrace();
    }
  }
}