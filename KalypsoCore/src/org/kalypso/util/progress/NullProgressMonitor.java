package org.kalypso.util.progress;

/**
 * Ein ProgressMonitor, der nichts tut
 * 
 * @author belger
 */
public class NullProgressMonitor implements IProgressMonitor
{
  /**
   * @see org.kalypso.util.progress.IProgressMonitor#beginTask(java.lang.String,
   *      int)
   */
  public void beginTask( String name, int totalWork )
  {
  //  
  }

  /**
   * @see org.kalypso.util.progress.IProgressMonitor#done()
   */
  public void done()
  {
  //
  }

  /**
   * @see org.kalypso.util.progress.IProgressMonitor#isCanceled()
   */
  public boolean isCanceled()
  {
    return false;
  }

  /**
   * @see org.kalypso.util.progress.IProgressMonitor#setCanceled(boolean)
   */
  public void setCanceled( boolean value )
  {
  //  
  }

  /**
   * @see org.kalypso.util.progress.IProgressMonitor#setTaskName(java.lang.String)
   */
  public void setTaskName( String name )
  {
  //
  }

  /**
   * @see org.kalypso.util.progress.IProgressMonitor#subTask(java.lang.String)
   */
  public void subTask( String name )
  {
  //
  }

  /**
   * @see org.kalypso.util.progress.IProgressMonitor#worked(int)
   */
  public void worked( int work )
  {
  //
  }
}