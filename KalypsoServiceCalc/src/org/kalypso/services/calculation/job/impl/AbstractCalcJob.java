package org.kalypso.services.calculation.job.impl;

import java.util.ArrayList;
import java.util.Collection;

import org.kalypso.services.calculation.job.ICalcJob;
import org.kalypso.services.calculation.service.CalcJobDataBean;
import org.kalypso.services.calculation.service.CalcJobServiceException;

/**
 * @author Belger
 */
public abstract class AbstractCalcJob implements ICalcJob
{
  private String m_id = null;
  
  private String m_description = null;
  
  private String m_message = "<uninitialisiert>";
  
  private String m_type = null;
  
  private int m_state = UNKNOWN;
  
  private int m_progress = -1;
  
  private CalcJobDataBean[] m_arguments = null;
  
  private Collection m_results = new ArrayList();

  private boolean m_canceled = false;
  
  public final void init( final String id, final String type, final String description, final CalcJobDataBean[] arguments )
      throws CalcJobServiceException
  {
    if( m_id != null )
      throw new CalcJobServiceException( "Already initialised", null );

    m_id = id;
    m_description = description;
    m_type = type;
    m_state = WAITING;
    m_message = "Warte auf Ausführung...";
    m_arguments = arguments;
  }

  /**
   * @see java.lang.Runnable#run()
   */
  public final void run()
  {
    m_state = RUNNING;

    try
    {
      runIntern( m_arguments );
      
      if( m_canceled )
        m_state = CANCELED;
      else
        m_state = FINISHED;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      
      m_message = e.getLocalizedMessage();
      m_state = ERROR;
    }
  }
  
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
  
  protected abstract void runIntern( final CalcJobDataBean[] arguments ) throws CalcJobServiceException;

  /**
   * @see org.kalypso.services.calculation.common.ICalcJobInfo#getId()
   */
  public final String getId()
  {
    return m_id;
  }

  /**
   * @see org.kalypso.services.calculation.common.ICalcJobInfo#getType()
   */
  public final String getType()
  {
    return m_type;
  }

  /**
   * @see org.kalypso.services.calculation.common.ICalcJobInfo#getDescription()
   */
  public final String getDescription()
  {
    return m_description;
  }

  /**
   * @see org.kalypso.services.calculation.common.ICalcJobInfo#getState()
   */
  public final int getState()
  {
    return m_state;
  }

  /**
   * @see org.kalypso.services.calculation.common.ICalcJobInfo#getProgress()
   */
  public final int getProgress()
  {
    return m_progress;
  }

  /**
   * @see org.kalypso.services.calculation.common.ICalcJobInfo#getMessage()
   */
  public final String getMessage()
  {
    return m_message;
  }

  /**
   * @see org.kalypso.services.calculation.common.ICalcJobInfo#getResults()
   */
  public final CalcJobDataBean[] getResults()
  {
    return (CalcJobDataBean[])m_results.toArray( new CalcJobDataBean[m_results.size()] );
  }

  protected void progress( final int work )
  {
    m_progress = Math.max( m_progress + work, 100 );
  }
  
  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#disposeJob()
   */
  public void disposeJob()
  {
    // TODO: alle Results löschen
  }

  protected void addResult( final CalcJobDataBean bean )
  {
    // TODO: allgemeinen Mechanimus zur Datenablage anlegen?
    m_results.add( bean );
  }
  
  /**
   * @throws CalcJobServiceException
   * @see org.kalypso.services.calculation.job.ICalcJob#setReady()
   */
  public void setReady() throws CalcJobServiceException
  {
    if( m_state == WAITING_FOR_DATA )
      m_state = WAITING;
    
    throw new CalcJobServiceException( "Job must be waiting", null );
  }
}