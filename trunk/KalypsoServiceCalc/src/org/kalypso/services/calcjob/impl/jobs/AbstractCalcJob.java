package org.kalypso.services.calcjob.impl.jobs;

import java.net.URL;

import org.kalypso.services.calcjob.CalcJob;
import org.kalypso.services.calcjob.CalcJobDescription;
import org.kalypso.services.calcjob.CalcJobException;
import org.kalypso.services.calcjob.CalcJobServiceException;
import org.kalypso.services.calcjob.CalcJobStatus;

/**
 * @author belger
 */
public abstract class AbstractCalcJob implements CalcJob
{
  private CalcJobDescription m_calcJobDescription = null;
  
  private URL[] m_arguments = null;
  
  private URL[] m_results = null;

  public final void init( final String id, final String description, final URL[] arguments, final String type )
      throws CalcJobServiceException
  {
    if( m_calcJobDescription != null )
      throw new CalcJobServiceException( "CalcJob already initialised", null );

    m_calcJobDescription = new CalcJobDescription( id, description, type, CalcJobStatus.WAITING, 0 );
    m_arguments = arguments;
  }

  /**
   * @see org.kalypso.services.calcjob.CalcJob#getDescription()
   */
  public final CalcJobDescription getDescription()
  {
    return m_calcJobDescription;
  }

  /**
   * @see java.lang.Runnable#run()
   */
  public final void run()
  {
    getDescription().setState( CalcJobStatus.RUNNING );

    try
    {
      m_results = runIntern( m_arguments, new CalcJobProgressMonitor( getDescription() ) );
      
      getDescription().setState( CalcJobStatus.FINISHED );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      
      getDescription().setMessage( e.getLocalizedMessage() );
      getDescription().setState( CalcJobStatus.ERROR );
    }
  }
  
  /**
   * @see org.kalypso.services.calcjob.CalcJob#getResults()
   */
  public final URL[] getResults()
  {
    return m_results;
  }
  
  protected abstract URL[] runIntern( final URL[] arguments, final CalcJobProgressMonitor monitor ) throws CalcJobException;
}