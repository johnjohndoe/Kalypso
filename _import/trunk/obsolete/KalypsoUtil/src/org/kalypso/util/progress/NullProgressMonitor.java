package org.kalypso.util.progress;

/**
 * @author belger
 */
public class NullProgressMonitor implements IProgressMonitor
{
  /**
   * @see org.kalypso.util.progress.IProgressMonitor#beginTask(java.lang.String, int)
   */
  public void beginTask( final String name, final int totalWork )
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
   * @see org.kalypso.util.progress.IProgressMonitor#worked(int)
   */
  public void worked( int work )
  {
    //
  }

}
