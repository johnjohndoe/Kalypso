package org.kalypso.ui.launcher;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.jface.operation.IRunnableWithProgress;

/**
 * @author belger
 */
public class CalcCaseRunnable implements IRunnableWithProgress
{
  private ILaunchConfiguration m_configuration;

  public CalcCaseRunnable( final ILaunchConfiguration configuration )
  {
    m_configuration = configuration;
  }

  /**
   * @see org.eclipse.jface.operation.IRunnableWithProgress#run(org.eclipse.core.runtime.IProgressMonitor)
   */
  public void run( final IProgressMonitor monitor )
  {
    final int count = 100000;

    monitor.beginTask( "Modellrechnung", count );

    for( int i = 0; i < count ; i++ )
    {
      final double blubb = i * i;
      new Double( blubb );
      
      monitor.worked( i );
    }
  }
}
