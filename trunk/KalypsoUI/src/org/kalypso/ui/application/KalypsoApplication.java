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
    boolean bPrognose = false;
    if( args instanceof String[] )
    {
      final String[] strgargs = (String[])args;
      for( int i = 0; i < strgargs.length; i++ )
      {
        if( "-prognose".equals( strgargs[i] ) )
        {
          bPrognose = true;
          break;
        }
      }
    }
    
    final WorkbenchAdvisor advisor = bPrognose ? (WorkbenchAdvisor)new PrognoseWorkbenchAdvisor() : new KalypsoWorkbenchAdvisor();
    final Display display = PlatformUI.createDisplay();

    final int returnCode = PlatformUI.createAndRunWorkbench( display, advisor );

    return returnCode == PlatformUI.RETURN_RESTART ? IPlatformRunnable.EXIT_RESTART
        : IPlatformRunnable.EXIT_OK;
  }
}