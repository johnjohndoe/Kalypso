package org.kalypso.util.progress;

import org.eclipse.core.runtime.IProgressMonitor;

/**
 * @author belger
 */
public final class EclipseProgressMonitor implements org.kalypso.util.progress.IProgressMonitor
{
  private final IProgressMonitor m_monitor;

  public EclipseProgressMonitor( final IProgressMonitor monitor )
  {
    m_monitor = monitor;
  }

  /**
   * @see org.kalypso.util.progress.IProgressMonitor#beginTask(java.lang.String,
   *      int)
   */
  public void beginTask( String name, int totalWork )
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
   * @see org.kalypso.util.progress.IProgressMonitor#setTaskName(java.lang.String)
   */
  public void setTaskName( String name )
  {
    m_monitor.setTaskName( name );
  }

  /**
   * @see org.kalypso.util.progress.IProgressMonitor#subTask(java.lang.String)
   */
  public void subTask( String name )
  {
    m_monitor.subTask( name );
  }

  /**
   * @see org.kalypso.util.progress.IProgressMonitor#worked(int)
   */
  public void worked( int work )
  {
    m_monitor.worked( work );
  }

}