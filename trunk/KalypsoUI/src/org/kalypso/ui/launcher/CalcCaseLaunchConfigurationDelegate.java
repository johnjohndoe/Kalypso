package org.kalypso.ui.launcher;

import java.util.Properties;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.model.LaunchConfigurationDelegate;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.kalypso.eclipse.debug.core.model.RunnableProcess;

/**
 * @author belger
 */
public class CalcCaseLaunchConfigurationDelegate extends LaunchConfigurationDelegate
{
  /**
   * @see org.eclipse.debug.core.model.ILaunchConfigurationDelegate#launch(org.eclipse.debug.core.ILaunchConfiguration, java.lang.String, org.eclipse.debug.core.ILaunch, org.eclipse.core.runtime.IProgressMonitor)
   */
  public void launch( final ILaunchConfiguration configuration, final String mode, final ILaunch launch,
      final IProgressMonitor monitor ) throws CoreException
  {
    final IRunnableWithProgress runnable = new CalcCaseRunnable( configuration ); 
    
    final Properties properties = new Properties();
    properties.putAll( configuration.getAttributes() );
    
    new RunnableProcess( launch, runnable, "Modellrechnung", properties );
  }
}
