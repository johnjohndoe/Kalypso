package org.kalypso.model.wspm.ui.profil.operation;

import org.eclipse.core.commands.operations.IUndoableOperation;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.contribs.eclipse.core.runtime.jobs.MutexRule;

/**
 * Job, to execute Profil-Operation
 * 
 * @author belger
 */
public class ProfilOperationJob extends Job
{
  private final static MutexRule MUTEX = new MutexRule();

  private ProfilOperationRunnable m_runnable;

  public ProfilOperationJob( final IUndoableOperation operation )
  {
    super( operation.getLabel() );

    m_runnable = new ProfilOperationRunnable( operation );

    setUser( true );
    setPriority( Job.SHORT );
    setRule( MUTEX );
  }

  /**
   * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  protected IStatus run( final IProgressMonitor monitor )
  {
    return m_runnable.execute( monitor );
  }

}
