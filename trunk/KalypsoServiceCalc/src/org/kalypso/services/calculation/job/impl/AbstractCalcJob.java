package org.kalypso.services.calculation.job.impl;

import java.util.Collection;
import java.util.Vector;
import java.util.logging.Logger;

import org.kalypso.services.calculation.job.ICalcJob;
import org.kalypso.services.calculation.service.CalcJobDataBean;

/**
 * @author Belger
 */
public abstract class AbstractCalcJob implements ICalcJob
{
  private Logger m_logger = Logger.getLogger( ICalcJob.class.getName() );
  
  private String m_message = "Warte auf Ausf�hrung...";
  
  private int m_progress = -1;
  
  private Collection m_results = new Vector();

  private boolean m_canceled = false;
  
  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#cancel()
   */
  public void cancel()
  {
    m_canceled = true;
  }
  
  public boolean isCanceled()
  {
    return m_canceled;
  }
  
  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#getProgress()
   */
  public final int getProgress()
  {
    return m_progress;
  }

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#getMessage()
   */
  public final String getMessage()
  {
    return m_message;
  }
  
  protected final void setMessage( final String message )
  {
    m_message = message;
  }

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#getResults()
   */
  public final CalcJobDataBean[] getResults()
  {
    m_logger.info( "Getting results" );
    synchronized( m_results )
    {
      return (CalcJobDataBean[])m_results.toArray( new CalcJobDataBean[m_results.size()] );
    }
  }

  protected void progress( final int work )
  {
    m_progress = Math.min( m_progress + work, 100 );
  }
  
  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#disposeJob()
   */
  public void disposeJob()
  {
    // normalerweise nichts zu tun
  }

  protected void addResult( final CalcJobDataBean bean )
  {
    m_logger.info( "Adding result: " + bean.getPath() );
    
    synchronized( m_results )
    {
      m_results.add( bean );
    }
    
    m_logger.info( "Added result: " + bean.getPath() );
  }
}