package org.kalypso.portal.intro;

import org.eclipse.ui.application.IWorkbenchConfigurer;
import org.eclipse.ui.application.IWorkbenchWindowConfigurer;
import org.eclipse.ui.application.WorkbenchAdvisor;
import org.eclipse.ui.application.WorkbenchWindowAdvisor;
import org.eclipse.ui.internal.ide.IDEWorkbenchAdvisor;

public class KalypsoPortalWorkbenchAdvisor extends IDEWorkbenchAdvisor
{

  private static final String PERSPECTIVE_ID = "org.kalypso.portal.perspective.planner";

  @Override
  public WorkbenchWindowAdvisor createWorkbenchWindowAdvisor( IWorkbenchWindowConfigurer configurer )
  {
    return new KalypsoPortalWorkbenchWindowAdvisor( configurer );
  }

  @Override
  public void initialize( IWorkbenchConfigurer configurer )
  {
    super.initialize( configurer );
    configurer.setSaveAndRestore( false );
  }

  @Override
  public String getInitialWindowPerspectiveId( )
  {
    return PERSPECTIVE_ID;
  }

}
