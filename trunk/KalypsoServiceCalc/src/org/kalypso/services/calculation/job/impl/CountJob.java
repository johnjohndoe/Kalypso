package org.kalypso.services.calculation.job.impl;

import org.kalypso.services.calculation.service.CalcJobDataBean;
import org.kalypso.services.calculation.service.CalcJobServiceException;

/**
 * @author belger
 */
public final class CountJob extends AbstractCalcJob
{
  /**
   * @see org.kalypso.services.calculation.job.impl.AbstractCalcJob#runIntern(org.kalypso.services.calculation.service.CalcJobDataBean[])
   */
  protected void runIntern( final CalcJobDataBean[] arguments ) throws CalcJobServiceException
  {
    while( getState() == RUNNING )
    {
      try
      {
        Thread.sleep( 500 );
      }
      catch( final InterruptedException e )
      {
        throw new CalcJobServiceException( "Thread interrupted", e );
      }

      if( isCanceled() )
        return;
      
      final int progress = getProgress();
      if( progress == 100 )
        return;

      progress( 100 );
    }
  }
}