package org.kalypso.services.calcjob.impl.jobs;

import java.net.URL;

import org.kalypso.services.calcjob.CalcJobException;
import org.kalypso.services.calcjob.CalcJobStatus;

/**
 * @author belger
 */
public final class CountJob extends AbstractCalcJob
{
  /**
   * @see org.kalypso.services.calcjob.impl.jobs.AbstractCalcJob#runIntern(java.net.URL[], org.kalypso.services.calcjob.impl.jobs.CalcJobProgressMonitor)
   */
  protected URL[] runIntern( final URL[] arguments, final CalcJobProgressMonitor monitor ) throws CalcJobException
  {
    while( getDescription().getState() == CalcJobStatus.RUNNING )
    {
      try
      {
        Thread.sleep( 500 );
      }
      catch( final InterruptedException e )
      {
        throw new CalcJobException( e );
      }

      final int progress = getDescription().getProgress();
      if( progress == 100 )
        return null;

      monitor.worked( 100 );
    }
    
    return new URL[] {};
  }

  /**
   * @see org.kalypso.services.calcjob.CalcJob#disposeJob()
   */
  public void disposeJob()
  {
    // nichts zu tun
  }
}