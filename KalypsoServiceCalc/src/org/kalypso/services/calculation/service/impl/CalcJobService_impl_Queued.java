package org.kalypso.services.calculation.service.impl;

import java.io.File;
import java.rmi.RemoteException;
import java.util.Iterator;
import java.util.Timer;
import java.util.TimerTask;
import java.util.Vector;
import java.util.logging.FileHandler;
import java.util.logging.Logger;

import org.deegree_impl.extension.TypeRegistrySingleton;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.java.lang.reflect.ClassUtilities;
import org.kalypso.ogc.sensor.deegree.ObservationLinkHandler;
import org.kalypso.services.calculation.common.ICalcServiceConstants;
import org.kalypso.services.calculation.job.ICalcJob;
import org.kalypso.services.calculation.job.impl.CalcJobFactory;
import org.kalypso.services.calculation.service.CalcJobBean;
import org.kalypso.services.calculation.service.CalcJobDataBean;
import org.kalypso.services.calculation.service.CalcJobServiceException;
import org.kalypso.services.calculation.service.ICalculationService;
import org.kalypso.services.common.ServiceConfig;

/**
 * TODO: remove all file on finalize
 * 
 * @author Belger
 */
public class CalcJobService_impl_Queued implements ICalculationService
{
  protected static final Logger LOGGER = Logger.getLogger( CalcJobService_impl_Queued.class.getName() );
  private static final int MAX_THREADS = 3;

  /** Vector of {@link CalcJobThread}s */
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
    // Logger initialisieren
    try
    {
      LOGGER.addHandler( new FileHandler( ServiceConfig.getTempDir() + "/" + ClassUtilities.getOnlyClassName( ICalculationService.class ) + "%g.log",
          10000000, 10, true ) );
    }
    catch( final Exception e ) // generic Exception caught for simplicity
    {
      System.out.println( "Could not initialize Logger" );
      e.printStackTrace();
    }
    
    LOGGER.info( "Rechendienst wird gestartet" );
    LOGGER.info( "Lese Konfigurationsdatei" );
    
    try
    {
      TypeRegistrySingleton.getTypeRegistry().registerTypeHandler( new ObservationLinkHandler() );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      
      LOGGER.severe( "Could not register type handler: Service will not work correctly" );
    }
    
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
  
  /**
   * @see org.kalypso.services.IKalypsoService#getServiceVersion()
   */
  public int getServiceVersion()
  {
    return 0;
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
  
  /**
   * @throws CalcJobServiceException
   * @see org.kalypso.services.calculation.service.ICalculationService#getJob(java.lang.String)
   */
  public CalcJobBean getJob( final String jobID ) throws CalcJobServiceException
  {
    return findJobThread( jobID ).getJobBean();
  }

  public final CalcJobBean prepareJob( final String typeID, final String description ) throws CalcJobServiceException
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

      final File basedir = FileUtilities.createNewTempDir( "CalcJob-" + id + "-", ServiceConfig.getTempDir() );

      final CalcJobBean jobBean = new CalcJobBean( "" + id, description, typeID, ICalcServiceConstants.WAITING_FOR_DATA,
          -1, basedir.getAbsolutePath(), null );
      final CalcJobThread cjt = new CalcJobThread( job, jobBean );

      if( id == m_threads.size() )
        m_threads.add( cjt );
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
    findJob( jobID ).cancel();
  }

  /**
   * @see org.kalypso.services.calculation.service.ICalculationService#disposeJob(java.lang.String)
   */
  public void disposeJob( final String jobID ) throws CalcJobServiceException
  {
    final CalcJobThread cjt = findJobThread( jobID );

    if( cjt.isAlive() )
      throw new CalcJobServiceException( "Cannot dispose a running job! Cancel it first.", null );

    cjt.job.disposeJob();
    
    FileUtilities.deleteRecursive( new File( cjt.getJobBean().getBasedir() ) );

    synchronized( m_threads )
    {
      m_threads.remove( cjt );
    }
  }

  private ICalcJob findJob( final String jobID ) throws CalcJobServiceException
  {
    return findJobThread( jobID ).job;
  }

  private CalcJobThread findJobThread( final String jobID ) throws CalcJobServiceException
  {
    for( final Iterator jIt = m_threads.iterator(); jIt.hasNext(); )
    {
      final CalcJobThread cjt = (CalcJobThread)jIt.next();

      if( cjt.getJobBean().getId().equals( jobID ) )
        return cjt;
    }

    throw new CalcJobServiceException( "Job not found: " + jobID, null );
  }

  public final void startJob( final String jobID, final CalcJobDataBean[] input ) throws CalcJobServiceException
  {
    final CalcJobThread jobThread = findJobThread( jobID );
    final CalcJobBean jobBean = jobThread.getJobBean();
    if( jobBean.getState() == ICalcServiceConstants.WAITING_FOR_DATA )
    {
      jobBean.setState( ICalcServiceConstants.WAITING );
      jobBean.setInputData( input );
      return;
    }

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

      final CalcJobBean jobBean = cjt.getJobBean();
      if( jobBean.getState() == ICalcServiceConstants.WAITING )
      {
        LOGGER.info( "Starting job: " + jobBean.getId() );
        
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
      if( jobBean.getState() == ICalcServiceConstants.RUNNING )
      {
        jobBean.setMessage( job.getMessage() );
        jobBean.setResults( job.getResults() );
        jobBean.setProgress( job.getProgress() );
      }
        
      return jobBean;
    }

    /**
     * @see java.lang.Thread#run()
     */
    public void run()
    {
      jobBean.setState( ICalcServiceConstants.RUNNING );

      try
      {
        job.run( new File( jobBean.getBasedir() ), jobBean.getInputData() );

        if( job.isCanceled() )
        {
          jobBean.setState( ICalcServiceConstants.CANCELED );
          LOGGER.info( "JOB exited because it was canceled: " + jobBean.getId() );
        }
        else
        {
          jobBean.setState( ICalcServiceConstants.FINISHED );
          LOGGER.info( "JOB exited normaly: " + jobBean.getId() );
        }
      }
      catch( final Throwable t )
      {
        System.out.println( "soso" );
        LOGGER.warning( "JOB exited with exception: " + jobBean.getId() );
        t.printStackTrace();

        jobBean.setMessage( t.getLocalizedMessage() );
        jobBean.setState( ICalcServiceConstants.ERROR );
      }
    }
  }
}