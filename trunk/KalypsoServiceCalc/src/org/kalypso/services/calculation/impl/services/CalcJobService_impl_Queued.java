package org.kalypso.services.calculation.impl.services;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;

import org.kalypso.services.calculation.CalcJobBean;
import org.kalypso.services.calculation.CalcJobServiceException;
import org.kalypso.services.calculation.ICalcJob;
import org.kalypso.services.calculation.impl.jobs.CalcJobFactory;

/**
 * @author Belger
 */
public class CalcJobService_impl_Queued
{
  private static final int MAX_THREADS = 3;

  /** job -> thread */
  private final Map m_threads = new HashMap();

  private final ArrayList m_jobs = new ArrayList();

  private final CalcJobFactory m_calcJobFactory;

  private final Timer m_timer = new Timer();

  /** Der Timer ruft die schedule Methode regelmässig auf, damit
   * wartende Jobs gestarted werden, fall noch nicht genug laufen */
  private final TimerTask m_timerTaks = new TimerTask()
  {
    public void run()
    {
      scheduleJobs();
    }
  };

  public CalcJobService_impl_Queued()
  {
    /** TODO: Konfiguration mit typen */
    m_calcJobFactory = new CalcJobFactory();

    /** TODO: was soll das? */
    m_timer.schedule( m_timerTaks, 1000, 1000 );
  }

  public synchronized final String[] getJobTypes()
  {
    return m_calcJobFactory.getSupportedTypes();
  }

  public synchronized CalcJobBean[] getJobs()
  {
    final CalcJobBean[] jobBeans = new CalcJobBean[m_jobs.size()];
    int count = 0;

    for( final Iterator jIt = m_jobs.iterator(); jIt.hasNext(); count++ )
      jobBeans[count] = ( (ICalcJob)jIt.next() ).getJobBean();

    return jobBeans;
  }

  public final String createJob( final String typeID, final String description,
      final String[] arguments ) throws CalcJobServiceException
  {
    // eine unbenutzte ID finden
    int id = -1;
  
    synchronized( m_jobs )
    {
      for( int i = 0; i < m_jobs.size(); i++ )
      {
        if( m_jobs.get( i ) == null )
        {
          id = i;
          break;
        }
      }
      
      if( id == -1 )
        id = m_jobs.size();

      final ICalcJob job = m_calcJobFactory.createJob( "" + id, description, arguments, typeID );

      if( id == m_jobs.size() )
        m_jobs.add( job );
      else
        m_jobs.set( id, job );
    }

    return "" + id;
  }

  /**
   * @see org.kalypso.services.calculation.ICalculationService#cancelJob(java.lang.String)
   */
  public void cancelJob( final String jobID ) throws CalcJobServiceException
  {
    final ICalcJob job = getJob( jobID );
    job.getJobBean().setState( CalcJobBean.CANCELED );
  }

  /**
   * @see org.kalypso.services.calculation.ICalculationService#disposeJob(java.lang.String)
   */
  public void disposeJob( final String jobID ) throws CalcJobServiceException
  {
    // TODO: check threadding issues
    // - maybe the job is already canceled, but its thread has not yet returned
    final ICalcJob job = getJob( jobID );
    
    final CalcJobBean jobBean = job.getJobBean();
    if( jobBean.getState() == CalcJobBean.RUNNING )
      throw new CalcJobServiceException( "Cannot dispose a running job! Cancel it first.", null );
    
    job.disposeJob();
    
    synchronized( m_jobs )
    {
      m_jobs.remove( job );
    }
  }

  private ICalcJob getJob( final String jobID ) throws CalcJobServiceException
  {
    for( final Iterator jIt = m_jobs.iterator(); jIt.hasNext(); )
    {
      final ICalcJob job = (ICalcJob)jIt.next();

      if( job.getJobBean().getId().equals( jobID ) )
        return job;
    }

    throw new CalcJobServiceException( "Job not found: " + jobID, null );
  }


  private final void startJob( final String jobID ) throws CalcJobServiceException
  {
    final ICalcJob job = getJob( jobID );
    final Thread thread = new Thread( job, "CalcJobThread - " + job.getJobBean().getDescription() );

    m_threads.put( job, thread );
    thread.start();
  }

  public synchronized void scheduleJobs() 
  {
    // count running thread
    int runningCount = 0;
    for( final Iterator jIt = m_jobs.iterator(); jIt.hasNext(); )
    {
      final ICalcJob job = (ICalcJob)jIt.next();
      if( job.getJobBean().getState() == CalcJobBean.RUNNING )
        runningCount++;
    }

    // start not running threads, till maximum is reached
    for( final Iterator jIt = m_jobs.iterator(); jIt.hasNext(); )
    {
      if( runningCount >= MAX_THREADS )
        break;

      final ICalcJob job = (ICalcJob)jIt.next();

      if( job.getJobBean().getState() == CalcJobBean.WAITING )
      {
        try
        {
          startJob( job.getJobBean().getId() );
        }
        catch( CalcJobServiceException e )
        {
          // sollte nie passieren!
          e.printStackTrace();
        }
        runningCount++;
      }
    }
  }
}