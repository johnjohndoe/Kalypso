package org.kalypso.eclipse.core.runtime;

import org.kalypso.util.progress.IProgressMonitor;

/**
 * Wrappt einen Eclipse-ProgressMonitor als kalypso-util ProgressMonitor.
 * 
 * @author gernot
 */
public class UtilProgressMonitor implements IProgressMonitor
{
  private org.eclipse.core.runtime.IProgressMonitor m_monitor;

  public UtilProgressMonitor( final org.eclipse.core.runtime.IProgressMonitor monitor )
  {
    m_monitor = monitor;
  }

  /**
   * @see org.kalypso.util.progress.IProgressMonitor#beginTask(java.lang.String,
   *      int)
   */
  public void beginTask( final String name, final int totalWork )
  {
    m_monitor.beginTask( name, totalWork );
  }

  /**
   * @see org.kalypso.util.progress.IProgressMonitor#done()
   */
  public void done()
  {
    m_monitor.done();
  }

  /**
   * @see org.kalypso.util.progress.IProgressMonitor#isCanceled()
   */
  public boolean isCanceled()
  {
    return m_monitor.isCanceled();
  }

  /**
   * @see org.kalypso.util.progress.IProgressMonitor#setCanceled(boolean)
   */
  public void setCanceled( boolean value )
  {
    m_monitor.setCanceled( value );
  }

  /**
   * @see org.kalypso.util.progress.IProgressMonitor#worked(int)
   */
  public void worked( int work )
  {
    m_monitor.worked( work );
  }

}