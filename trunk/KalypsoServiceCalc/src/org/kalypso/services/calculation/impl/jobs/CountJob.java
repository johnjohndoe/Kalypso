package org.kalypso.services.calculation.impl.jobs;

import org.kalypso.services.calculation.CalcJobBean;
import org.kalypso.services.calculation.CalcJobResultBean;
import org.kalypso.services.calculation.CalcJobServiceException;

/**
 * @author belger
 */
public final class CountJob extends AbstractCalcJob
{
  /**
   * @see org.kalypso.services.calculation.impl.jobs.AbstractCalcJob#runIntern(java.lang.String[], org.kalypso.services.calculation.impl.jobs.CalcJobProgressMonitor)
   */
  protected void runIntern( final String[] arguments, final CalcJobProgressMonitor monitor ) throws CalcJobServiceException
  {
    while( getJobBean().getState() == CalcJobBean.RUNNING )
    {
      try
      {
        Thread.sleep( 500 );
      }
      catch( final InterruptedException e )
      {
        throw new CalcJobServiceException( "Thread interrupted", e );
      }

      final int progress = getJobBean().getProgress();
      if( progress == 100 )
        return;

      monitor.worked( 100 );
    }
    
    getJobBean().setResults( new CalcJobResultBean[] {} );
  }

  /**
   * @see org.kalypso.services.calculation.ICalcJob#disposeJob()
   */
  public void disposeJob()
  {
    // nichts zu tun
  }
}