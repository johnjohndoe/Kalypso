package org.kalypso.ui.application;

import org.eclipse.core.runtime.IPlatformRunnable;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.application.WorkbenchAdvisor;

/**
 * @author belger
 */
public class KalypsoApplication implements IPlatformRunnable
{
  /**
   * @see org.eclipse.core.runtime.IPlatformRunnable#run(java.lang.Object)
   */
  public Object run( final Object args ) throws Exception
  {
    final WorkbenchAdvisor advisor = new KalypsoWorkbenchAdvisor();
    final Display display = PlatformUI.createDisplay();
    
    final int returnCode = PlatformUI.createAndRunWorkbench( display, advisor );
    
    return returnCode == PlatformUI.RETURN_RESTART ? IPlatformRunnable.EXIT_RESTART : IPlatformRunnable.EXIT_OK;
  }
}
