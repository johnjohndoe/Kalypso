package org.kalypso.portal.intro;

import org.eclipse.core.runtime.IPlatformRunnable;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PlatformUI;

/**
 * This class controls all aspects of the application's execution
 */
public class Application implements IPlatformRunnable
{

  private static final ApplicationWorkbenchAdvisor APPLICATION_WORKBENCH_ADVISOR = new ApplicationWorkbenchAdvisor();

  /*
   * (non-Javadoc)
   * 
   * @see org.eclipse.core.runtime.IPlatformRunnable#run(java.lang.Object)
   */
  public Object run( Object args ) throws Exception
  {
    Display display = PlatformUI.createDisplay();
    boolean auth = authenticate();
    if( auth )
      configure();

    try
    {
      int returnCode = PlatformUI.createAndRunWorkbench( display, APPLICATION_WORKBENCH_ADVISOR );
      if( returnCode == PlatformUI.RETURN_RESTART )
      {
        return IPlatformRunnable.EXIT_RESTART;
      }
      return IPlatformRunnable.EXIT_OK;
    }
    finally
    {
      display.dispose();
    }
  }

  private boolean authenticate( )
  {
    // TODO Auto-generated method stub
    return false;
  }

  private void configure( )
  {
    // TODO Auto-generated method stub

  }
}
