package org.kalypso.services.calculation.impl.jobs;

import org.kalypso.services.calculation.ICalcJob;
import org.kalypso.services.calculation.CalcJobBean;
import org.kalypso.services.calculation.CalcJobServiceException;

/**
 * @author belger
 */
public abstract class AbstractCalcJob implements ICalcJob
{
  private CalcJobBean m_calcJobDescription = null;
  
  private String[] m_arguments = null;
  
  public final void init( final String id, final String description, final String[] arguments, final String type )
      throws CalcJobServiceException
  {
    if( m_calcJobDescription != null )
      throw new CalcJobServiceException( "ICalcJob already initialised", null );

    m_calcJobDescription = new CalcJobBean( id, description, type, CalcJobBean.WAITING, 0, null );
    m_arguments = arguments;
  }

  /**
   * @see org.kalypso.services.calculation.ICalcJob#getJobBean()
   */
  public final CalcJobBean getJobBean()
  {
    return m_calcJobDescription;
  }

  /**
   * @see java.lang.Runnable#run()
   */
  public final void run()
  {
    getJobBean().setState( CalcJobBean.RUNNING );

    try
    {
      runIntern( m_arguments, new CalcJobProgressMonitor( getJobBean() ) );
      
      getJobBean().setState( CalcJobBean.FINISHED );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      
      getJobBean().setMessage( e.getLocalizedMessage() );
      getJobBean().setState( CalcJobBean.ERROR );
    }
  }
  
  protected abstract void runIntern( final String[] arguments, final CalcJobProgressMonitor monitor ) throws CalcJobServiceException;
}