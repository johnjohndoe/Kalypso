package org.kalypso.services.calculation.service.impl;

import java.io.File;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;

import org.kalypso.java.lang.reflect.ClassUtilities;
import org.kalypso.services.calculation.common.CalcJobHelper;
import org.kalypso.services.calculation.common.ICalcJobInfo;
import org.kalypso.services.calculation.job.ICalcJob;
import org.kalypso.services.calculation.job.impl.CalcJobFactory;
import org.kalypso.services.calculation.service.CalcJobBean;
import org.kalypso.services.calculation.service.CalcJobDataBean;
import org.kalypso.services.calculation.service.CalcJobServiceException;
import org.kalypso.services.calculation.service.ICalculationService;
import org.kalypso.services.common.ServiceConfig;

/**
 * @author Belger
 */
public class CalcJobService_impl_Queued implements ICalculationService
{
  private static final int MAX_THREADS = 3;

  /** job -> thread */
  private final Map m_threads = new HashMap();

  private final ArrayList m_jobs = new ArrayList();

  private final CalcJobFactory m_calcJobFactory;

  private final Timer m_timer = new Timer();

  /** Der Timer ruft die schedule Methode regelmässig auf, damit
   * wartende Jobs gestarted werden, falls noch nicht genug laufen 
   */
  private final TimerTask m_timerTaks = new TimerTask()
  {
    public void run()
    {
      scheduleJobs();
    }
  };

  public CalcJobService_impl_Queued() throws RemoteException
  {
    // die root aus dem Kalypso-Server-Properties lesen
    final File confDir = ServiceConfig.getConfDir();
    final File myConfDir = new File( confDir, ClassUtilities.getOnlyClassName( ICalculationService.class ) );
    final File typeFile = new File( myConfDir, "modelltypen.properties" );
    if( !typeFile.exists() )
      throw new RemoteException( "Can't find configuration file: " + typeFile.getAbsolutePath() );
    
    m_calcJobFactory = new CalcJobFactory( typeFile );

    // einmal in der Sekunde checken, ob wartende Jobs gestartet werden können
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
      jobBeans[count] = CalcJobHelper.createCalcJobBean( (ICalcJob)jIt.next() );

    return jobBeans;
  }

  public final CalcJobBean createJob( final String typeID, final String description, final CalcJobDataBean[] input ) throws CalcJobServiceException
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

      final ICalcJob job = m_calcJobFactory.createJob( "" + id, typeID, description, input );

      if( id == m_jobs.size() )
        m_jobs.add( job );
      else
        m_jobs.set( id, job );
      
      return CalcJobHelper.createCalcJobBean( job );
    }
  }


  /**
   * @see org.kalypso.services.calculation.service.ICalculationService#cancelJob(java.lang.String)
   */
  public void cancelJob( final String jobID ) throws CalcJobServiceException
  {
    getJob( jobID ).cancel();
  }

  /**
   * @see org.kalypso.services.calculation.service.ICalculationService#disposeJob(java.lang.String)
   */
  public void disposeJob( final String jobID ) throws CalcJobServiceException
  {
    // todo: check threading issues
    // - maybe the job has already beeen canceled, but its thread has not yet returned
    final ICalcJob job = getJob( jobID );
    
    if( job.getState() == ICalcJobInfo.RUNNING )
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

      if( job.getId().equals( jobID ) )
        return job;
    }

    throw new CalcJobServiceException( "Job not found: " + jobID, null );
  }

  public final void readyJob( final String jobID ) throws CalcJobServiceException
  {
    getJob( jobID ).setReady();
  }

  public synchronized void scheduleJobs() 
  {
    // count running thread
    int runningCount = 0;
    for( final Iterator jIt = m_jobs.iterator(); jIt.hasNext(); )
    {
      final ICalcJob job = (ICalcJob)jIt.next();
      if( job.getState() == ICalcJobInfo.RUNNING )
        runningCount++;
    }

    // start not running threads, till maximum is reached
    for( final Iterator jIt = m_jobs.iterator(); jIt.hasNext(); )
    {
      if( runningCount >= MAX_THREADS )
        break;

      final ICalcJob job = (ICalcJob)jIt.next();

      if( job.getState() == ICalcJobInfo.WAITING )
      {
          final Thread thread = new Thread( job, "CalcJobThread - " + job.getDescription() );

          m_threads.put( job, thread );
          thread.start();

          runningCount++;
      }
    }
  }
}