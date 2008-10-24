package org.kalypso.commons.process.internal;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.java.lang.ICancelable;

/**
 * This job controls the execution of a process.<br>
 * The process will be killed if either a timeout is reached or the given {@link ICancelable} is canceled.
 * 
 * @author Monika Thül
 * @author Gernot Belger
 */
public class ProcessControlJob extends Job
{
  private final long m_lTimeout;

  private final Process m_proc;

  private final ICancelable m_cancelable;

  public ProcessControlJob( final Process proc, final ICancelable cancelable, final long lTimeout )
  {
    super( "Process-Control: " + proc.toString() );

    m_proc = proc;
    m_cancelable = cancelable;
    m_lTimeout = lTimeout;
  }

  /**
   * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  protected IStatus run( final IProgressMonitor monitor )
  {
    final long startTime = System.currentTimeMillis();

    synchronized( this )
    {
      while( !monitor.isCanceled() )
      {
        try
        {
          Thread.sleep( 100 );

          if( m_lTimeout > 0 )
          {
            final long currentTime = System.currentTimeMillis();
            final long processDuration = currentTime - startTime;
            if( processDuration > m_lTimeout )
            {
              m_proc.destroy();
              return StatusUtilities.createStatus( IStatus.ERROR, "Timeout reached", null );
            }
          }

          if( m_cancelable.isCanceled() )
          {
            m_proc.destroy();
            return Status.CANCEL_STATUS;
          }
        }
        catch( final InterruptedException e )
        {
          // sollte nicht passieren
          e.printStackTrace();
        }
      }

      return Status.OK_STATUS;
    }
  }
}