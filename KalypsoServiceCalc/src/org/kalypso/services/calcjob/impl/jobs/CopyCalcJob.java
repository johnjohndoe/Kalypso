package org.kalypso.services.calcjob.impl.jobs;

import java.net.URL;

/**
 * @author belger
 */
public class CopyCalcJob extends AbstractCalcJob
{
  /**
   * @see org.kalypso.services.calcjob.impl.jobs.AbstractCalcJob#runIntern(java.net.URL[], org.kalypso.services.calcjob.impl.jobs.CalcJobProgressMonitor)
   */
  protected URL[] runIntern( final URL[] arguments, final CalcJobProgressMonitor monitor ) 
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
