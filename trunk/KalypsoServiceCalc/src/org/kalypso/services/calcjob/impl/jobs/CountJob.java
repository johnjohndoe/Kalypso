package org.kalypso.services.calcjob.impl.jobs;

import org.kalypso.services.calcjob.CalcJobServiceException;
import org.kalypso.services.calcjob.CalcJobStatus;

/**
 * @author belger
 */
public final class CountJob extends AbstractCalcJob
{
  /**
   * @see org.kalypso.services.calcjob.impl.jobs.AbstractCalcJob#runIntern(java.lang.String[], org.kalypso.services.calcjob.impl.jobs.CalcJobProgressMonitor)
   */
  protected String[] runIntern( final String[] arguments, final CalcJobProgressMonitor monitor ) throws CalcJobServiceException
  {
    while( getDescription().getState() == CalcJobStatus.RUNNING )
    {
      try
      {
        Thread.sleep( 500 );
      }
      catch( final InterruptedException e )
      {
        throw new CalcJobServiceException( "Thread interrupted", e );
      }

      final int progress = getDescription().getProgress();
      if( progress == 100 )
        return null;

      monitor.worked( 100 );
    }
    
    return new String[] {};
  }

  /**
   * @see org.kalypso.services.calcjob.CalcJob#disposeJob()
   */
  public void disposeJob()
  {
    // nichts zu tun
  }
}