package org.kalypso.services.calculation.job.impl;

import java.io.File;

import org.kalypso.services.calculation.service.CalcJobDataBean;


/**
 * @author belger
 */
public class CopyCalcJob extends AbstractCalcJob
{
  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#run(java.io.File, org.kalypso.services.calculation.service.CalcJobDataBean[])
   */
  public void run( final File basedir, final CalcJobDataBean[] arguments ) 
  {
    for( int i = 0; i < arguments.length; i++ )
    {
      addResult( arguments[i] );
      
      if( isCanceled() )
        return;
    }
    
    progress( 100 );
  }
}
