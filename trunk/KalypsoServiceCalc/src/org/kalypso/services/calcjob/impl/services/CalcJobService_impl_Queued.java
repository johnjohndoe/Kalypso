package org.kalypso.services.calcjob.impl.services;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.Timer;
import java.util.TimerTask;

import org.kalypso.services.calcjob.CalcJob;
import org.kalypso.services.calcjob.CalcJobStatus;


/**
 * @author Belger
 */
public class CalcJobService_impl_Queued extends AbstractCalcJobService_impl
{
  private static final int MAX_THREADS = 3;
  
  private final LinkedList m_jobs = new LinkedList(  );
  
  private final Timer m_timer = new Timer();
  private final TimerTask m_timerTaks = new TimerTask() {
    public void run()
    {
      checkJobs();
    }};

	public CalcJobService_impl_Queued()
	{
		m_timer.schedule( m_timerTaks, 1000, 1000 );
	}
	
  public String[] getJobs(  ) 
  {
    final String[] ids = new String[m_jobs.size(  )];
    int count = 0;

    for( Iterator jIt = m_jobs.iterator(  ); jIt.hasNext(  ); count++ )
    {
      final String id = ( (CalcJob)jIt.next(  ) ).getDescription().getId(); 
      ids[count] = id;
    }

    return ids;
  }

  protected void checkJobs(  )
  {
    // count running thread    
    int runningCount = 0;
    for( final Iterator jIt = m_jobs.iterator(  ); jIt.hasNext(  ); )
    {
      final CalcJob job = (CalcJob)jIt.next(  );
      if( job.getDescription().getState(  ) == CalcJobStatus.RUNNING )
        runningCount++;
    }
    
    // start not running threads, till maximum is reached
    for( Iterator jIt = m_jobs.iterator(  ); jIt.hasNext(  ); )
    {
      if( runningCount >= MAX_THREADS )
        break;
      
      final CalcJob job = (CalcJob)jIt.next(  );

      if( job.getDescription().getState(  ) == CalcJobStatus.WAITING )
      {
        startJob( job.getDescription().getId() );
        runningCount++;
      }
    }
  }

  protected CalcJob getJob( final String jobID )
  {
    for( Iterator jIt = m_jobs.iterator(  ); jIt.hasNext(  ); )
    {
      final CalcJob job = (CalcJob)jIt.next(  ); 

      if( job.getDescription().getId().equals( jobID ) )
        return job;
    }
    
    return null;
  }

  protected void addJob( final CalcJob job )
  {
    m_jobs.addLast( job );
  }

  protected void removeJob( final CalcJob job )
  {
    m_jobs.remove( job );
  }
}
