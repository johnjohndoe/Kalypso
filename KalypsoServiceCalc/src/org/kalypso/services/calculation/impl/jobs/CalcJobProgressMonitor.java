package org.kalypso.services.calculation.impl.jobs;

import org.kalypso.services.calculation.CalcJobBean;

/**
 * @author gernot
 */
public class CalcJobProgressMonitor
{
  private CalcJobBean m_description;
  private boolean m_isCanceled;

  public CalcJobProgressMonitor( final CalcJobBean description )
  {
    m_description = description;
  }

  public void beginTask( final String name )
  {
    m_description.setMessage( name );
  }

  public void done()
  {
    m_description.setProgress( 100 );
  }

  public boolean isCanceled()
  {
    return m_isCanceled;
  }

  public void setCanceled( final boolean isCanceled )
  {
    m_isCanceled = isCanceled;
  }

  public void worked( final int work )
  {
    m_description.setProgress( m_description.getProgress() + work );
  }
}
