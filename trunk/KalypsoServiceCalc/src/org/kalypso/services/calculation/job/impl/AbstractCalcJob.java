package org.kalypso.services.calculation.job.impl;

import java.util.ArrayList;
import java.util.Collection;

import org.kalypso.services.calculation.job.ICalcJob;
import org.kalypso.services.calculation.service.CalcJobDataBean;

/**
 * @author Belger
 */
public abstract class AbstractCalcJob implements ICalcJob
{
  private String m_message = "Warte auf ausf�hrung...";
  
  private int m_progress = -1;
  
  private Collection m_results = new ArrayList();

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

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#getResults()
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
    // TODO: alle Results l�schen
  }

  protected void addResult( final CalcJobDataBean bean )
  {
    // TODO: allgemeinen Mechanimus zur Datenablage anlegen?
    m_results.add( bean );
  }
}