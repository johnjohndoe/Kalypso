package org.kalypso.services.calculation.service.impl;

import java.io.File;
import java.rmi.RemoteException;
import java.util.Iterator;
import java.util.Timer;
import java.util.TimerTask;
import java.util.Vector;

import org.kalypso.java.lang.reflect.ClassUtilities;
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

  private final Vector m_threads = new Vector();

  private final CalcJobFactory m_calcJobFactory;

  private final Timer m_timer = new Timer();

  /**
   * Der Timer ruft die schedule Methode regelmässig auf, damit wartende Jobs
   * gestarted werden, falls noch nicht genug laufen
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
    final File myConfDir = new File( confDir, ClassUtilities
        .getOnlyClassName( ICalculationService.class ) );
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
    final CalcJobBean[] jobBeans = new CalcJobBean[m_threads.size()];
    int count = 0;

    for( final Iterator jIt = m_threads.iterator(); jIt.hasNext(); count++ )
    {
      final CalcJobThread cjt = (CalcJobThread)jIt.next();
      jobBeans[count] = cjt.getJobBean();
    }

    return jobBeans;
  }

  public final CalcJobBean createJob( final String typeID, final String description,
      final CalcJobDataBean[] input ) throws CalcJobServiceException
  {
    // eine unbenutzte ID finden
    int id = -1;

    synchronized( m_threads )
    {
      // eine neue, eindeutige id erzeugen
      for( int i = 0; i < m_threads.size(); i++ )
      {
        if( m_threads.get( i ) == null )
        {
          id = i;
          break;
        }
      }

      if( id == -1 )
        id = m_threads.size();

      final ICalcJob job = m_calcJobFactory.createJob( typeID );

      final String baseURL = ServiceConfig.createNewTempDir( "calcJob-" ).getAbsolutePath();

      final CalcJobBean jobBean = new CalcJobBean( "" + id, description, typeID, WAITING_FOR_DATA,
          -1, baseURL, null );
      final CalcJobThread cjt = new CalcJobThread( job, jobBean );

      if( id == m_threads.size() )
        m_threads.add( job );
      else
        m_threads.set( id, cjt );

      return cjt.getJobBean();
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
    final CalcJobThread cjt = getJobThread( jobID );

    if( cjt.isAlive() )
      throw new CalcJobServiceException( "Cannot dispose a running job! Cancel it first.", null );

    cjt.job.disposeJob();

    synchronized( m_threads )
    {
      m_threads.remove( cjt );
    }
  }

  private ICalcJob getJob( final String jobID ) throws CalcJobServiceException
  {
    return getJobThread( jobID ).job;
  }

  private CalcJobThread getJobThread( final String jobID ) throws CalcJobServiceException
  {
    for( final Iterator jIt = m_threads.iterator(); jIt.hasNext(); )
    {
      final CalcJobThread cjt = (CalcJobThread)jIt.next();

      if( cjt.getJobBean().getId().equals( jobID ) )
        return cjt;
    }

    throw new CalcJobServiceException( "Job not found: " + jobID, null );
  }

  public final void readyJob( final String jobID ) throws CalcJobServiceException
  {
    final CalcJobBean jobBean = getJobThread( jobID ).getJobBean();
    if( jobBean.getState() == WAITING_FOR_DATA )
      jobBean.setState( WAITING );

    throw new CalcJobServiceException( "Cannot ready job. State is: " + jobBean.getState(), null );
  }

  public synchronized void scheduleJobs()
  {
    // count running thread
    int runningCount = 0;
    for( final Iterator jIt = m_threads.iterator(); jIt.hasNext(); )
    {
      final CalcJobThread cjt = (CalcJobThread)jIt.next();
      if( cjt.isAlive() )
        runningCount++;
    }

    // start not running threads, till maximum is reached
    for( final Iterator jIt = m_threads.iterator(); jIt.hasNext(); )
    {
      if( runningCount >= MAX_THREADS )
        break;

      final CalcJobThread cjt = (CalcJobThread)jIt.next();

      if( cjt.getJobBean().getState() == WAITING )
      {
        cjt.start();

        runningCount++;
      }
    }
  }

  private final static class CalcJobThread extends Thread
  {
    public final ICalcJob job;

    private final CalcJobBean jobBean;

    public CalcJobThread( final ICalcJob job, final CalcJobBean jobBean )
    {
      this.job = job;
      this.jobBean = jobBean;
    }

    public CalcJobBean getJobBean()
    {
      jobBean.setMessage( job.getMessage() );
      jobBean.setResults( job.getResults() );
      jobBean.setProgress( job.getProgress() );

      return jobBean;
    }

    /**
     * @see java.lang.Thread#run()
     */
    public void run()
    {
      jobBean.setState( RUNNING );

      try
      {
        job.run( jobBean.getInputData() );

        if( job.isCanceled() )
          jobBean.setState( CANCELED );
        else
          jobBean.setState( FINISHED );
      }
      catch( final Exception e )
      {
        e.printStackTrace();

        jobBean.setMessage( e.getLocalizedMessage() );
        jobBean.setState( ERROR );
      }
    }

  }
}