package org.kalypso.services.calcjob.impl.services;

import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import org.kalypso.services.calcjob.CalcJob;
import org.kalypso.services.calcjob.CalcJobDescription;
import org.kalypso.services.calcjob.CalcJobService;
import org.kalypso.services.calcjob.CalcJobServiceException;
import org.kalypso.services.calcjob.CalcJobStatus;
import org.kalypso.services.calcjob.impl.jobs.CalcJobFactory;

/**
 * @author Belger
 */
public abstract class AbstractCalcJobService_impl implements CalcJobService
{
  private static int MAX_ID = -1;

  /** job -> thread */
  private final Map m_threads = new HashMap();
  
  private final CalcJobFactory m_calcJobFactory;

  public AbstractCalcJobService_impl()
  {
    m_calcJobFactory = new CalcJobFactory();
  }
  
  public final String createJob( final String typeID, final String description,
      final URL[] arguments ) throws CalcJobServiceException
  {
    MAX_ID++;

    final CalcJob job = m_calcJobFactory.createJob( "" + MAX_ID, description, arguments, typeID ); 

    addJob( job );

    return job.getDescription().getId();
  }

  public final String[] getJobTypes()
  {
    return m_calcJobFactory.getSupportedTypes();
  }

  public final CalcJobDescription getJobDescription( final String jobID )
      throws CalcJobServiceException
  {
    checkID( jobID );
    
    return getJob( jobID ).getDescription();
  }

  public final URL[] retrieveResults( final String jobID ) throws CalcJobServiceException
  {
    final CalcJob job = getJob( jobID );

    if( job.getDescription().getState() != CalcJobStatus.FINISHED )
      throw new CalcJobServiceException( "CalcJobDescription not yet finished", null );

    return job.getResults();
  }

  /**
   * @see org.kalypso.services.calcjob.CalcJobService#cancelJob(java.lang.String)
   */
  public void cancelJob( final String jobID ) throws CalcJobServiceException
  {
    checkID( jobID );

    stopJob( jobID );
  }

  /**
   * @see org.kalypso.services.calcjob.CalcJobService#removeJob(java.lang.String)
   */
  public void removeJob( String jobID ) throws CalcJobServiceException
  {
    checkID( jobID );
    
    // TODO: ?
//    stopJob( jobID );

    final CalcJob job = getJob( jobID );
    removeJob( job );
    job.disposeJob();
  }

  private void checkID( final String jobID ) throws CalcJobServiceException
  {
    if( getJob(jobID) == null )
      throw new CalcJobServiceException( "Job not found: " + jobID, null );
  }
  
  protected abstract void addJob( final CalcJob job );

  protected abstract void removeJob( final CalcJob job );

  protected abstract CalcJob getJob( final String jobID );

  private void stopJob( final String jobID )
  {
    final CalcJob job = getJob( jobID );
    job.getDescription().setState( CalcJobStatus.CANCELED );
  }

  protected final void startJob( final String jobID )
  {
    final CalcJob job = getJob( jobID );
    final Thread thread = new Thread( job, "CalcJobThread - " + job.getDescription().getDescription() );

    m_threads.put( job, thread );
    thread.start();
  }
}