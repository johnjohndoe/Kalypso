package org.kalypso.eclipse.debug.core.model;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.operation.IRunnableWithProgress;


/**
 * Monitors a system thread, waiting for it to terminate, and then notifies
 * the associated runtime process.
 */
class RunnableJob extends Job
{
  private final IRunnableWithProgress m_runnable;
  private final RunnableProcess m_process;

  /**
   * Creates a new process monitor and starts monitoring the runnable for
   * termination in a new thread.
   * @param process
   */
  public RunnableJob( final RunnableProcess process, final IRunnableWithProgress runnable )
  {
    super( "Thread-Process" );

    setPriority( Job.INTERACTIVE );
//    setSystem( true );

    m_runnable = runnable;
    m_process = process;

    schedule();
  }

  
  /**
   * @see org.eclipse.core.internal.jobs.InternalJob#run(org.eclipse.core.runtime.IProgressMonitor)
   */
  public IStatus run( final IProgressMonitor monitor )
  {
    try
    {
      m_runnable.run( monitor );
    }
    catch( final InvocationTargetException e )
    {
      e.printStackTrace();
      
      // TODO: return status
    }
    catch( final InterruptedException e )
    {
      e.printStackTrace();
      
      // TODO: return status
    }
    
    m_process.onTerminated();

    return Status.OK_STATUS;
  }

  /**
   * Kills the monitoring thread.
   * 
   * This method is to be useful for dealing with the error case of an
   * underlying thread which has not informed this monitor of its termination.
   */
  protected void killJob()
  {
    cancel();
  }
}