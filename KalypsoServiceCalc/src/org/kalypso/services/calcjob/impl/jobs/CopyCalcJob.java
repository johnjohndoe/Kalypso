package org.kalypso.services.calcjob.impl.jobs;


/**
 * @author belger
 */
public class CopyCalcJob extends AbstractCalcJob
{
  /**
   * @see org.kalypso.services.calcjob.impl.jobs.AbstractCalcJob#runIntern(java.lang.String[], org.kalypso.services.calcjob.impl.jobs.CalcJobProgressMonitor)
   */
  protected String[] runIntern( final String[] arguments, final CalcJobProgressMonitor monitor ) 
  {
    monitor.worked( 100 );
    return arguments;
  }

  /**
   * @see org.kalypso.services.calcjob.CalcJob#disposeJob()
   */
  public void disposeJob()
  {
  // nichts zu tun  
  }
}
