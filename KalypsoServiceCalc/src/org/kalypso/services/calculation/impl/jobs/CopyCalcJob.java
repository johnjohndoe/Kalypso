package org.kalypso.services.calculation.impl.jobs;

import java.util.LinkedList;
import java.util.List;

import org.kalypso.services.calculation.CalcJobResultBean;


/**
 * @author belger
 */
public class CopyCalcJob extends AbstractCalcJob
{
  /**
   * @see org.kalypso.services.calculation.impl.jobs.AbstractCalcJob#runIntern(java.lang.String[], org.kalypso.services.calculation.impl.jobs.CalcJobProgressMonitor)
   */
  protected void runIntern( final String[] arguments, final CalcJobProgressMonitor monitor ) 
  {
    monitor.worked( 100 );
    
    final List l = new LinkedList();
    for( int i = 0; i < arguments.length; i++ )
      l.add( new CalcJobResultBean(  ) );
    
    getJobBean().setResults( new CalcJobResultBean[l.size()] );
  }

  /**
   * @see org.kalypso.services.calculation.ICalcJob#disposeJob()
   */
  public void disposeJob()
  {
  // nichts zu tun  
  }
}
