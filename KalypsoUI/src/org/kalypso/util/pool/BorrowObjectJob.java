package org.kalypso.util.pool;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.plugin.KalypsoGisPlugin;

/**
 * @author belger
 */
public class BorrowObjectJob extends Job
{
  private final ResourcePool m_pool;
  private final IPoolListener m_poolListener;
  private final IPoolableObjectType m_key;
  private final Object m_dummyObject;

  public BorrowObjectJob( final String name, final ResourcePool pool, final IPoolListener l, final IPoolableObjectType key, Object dummyObject )
  {
    super( name );
    
    setPriority( Job.LONG );
    
    // TODO: setRULE??
    
    m_pool = pool;
    m_poolListener = l;
    m_key = key;
    m_dummyObject = dummyObject;
  }

  /**
   * @see org.eclipse.core.internal.jobs.InternalJob#run(org.eclipse.core.runtime.IProgressMonitor)
   */
  protected IStatus run( final IProgressMonitor monitor )
  {
    try
    {
      m_pool.getObject( m_key, monitor );
      m_poolListener.onObjectInvalid( m_dummyObject, false );
    }
    catch( final Exception e )
    {
      return new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0, "Fehler beim Laden einer Resource", e );
    }
    
    return Status.OK_STATUS;
  }
}
