package org.kalypso.util.pool;

import java.util.logging.Logger;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.i18n.Messages;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * Waits for a key to load. The object-loading can be performed synchronuously, in which case the caller will not get
 * access until objectLoaded is called internally.
 * <p>
 * In sychronuous mode and if there isn't already such a key in the pool, a new key will only be created for loading
 * purposes. Once done the key is removed.
 * 
 * @author belger
 */
public abstract class PoolableObjectWaiter implements IPoolListener
{
  private final ResourcePool m_pool = KalypsoGisPlugin.getDefault().getPool();

  protected final Object[] m_data;

  private IStatus m_result = Status.OK_STATUS;

  private final boolean m_synchron;

  private boolean m_disposed = false;

  /**
   * TRICKY: inherited classes may NOT use own fields, because calling this constructor via super() may immediately call
   * {@link #objectLoaded(IPoolableObjectType, Object)}
   * 
   * @param synchron
   *          when true the object-loading is done synchronuously and the caller of this constructor will only take
   *          control back once the object is loaded internally.
   */
  public PoolableObjectWaiter( final PoolableObjectType key, final Object[] data, final boolean synchron )
  {
    m_data = data;
    m_synchron = synchron;

    if( synchron )
    {
      Object value = null;
      try
      {
        value = m_pool.getObject( key );

        objectLoaded( key, value, m_result );
      }
      catch( final CoreException ce )
      {
        m_result = ce.getStatus();
      }
      catch( final Exception e )
      {
        //e.printStackTrace();
        m_result = StatusUtilities.statusFromThrowable( e, Messages.getString("org.kalypso.util.pool.PoolableObjectWaiter.0") + key ); //$NON-NLS-1$
      }
    }
    else
      m_pool.addPoolListener( this, key );
  }

  public final void objectLoaded( final IPoolableObjectType key, final Object newValue, final IStatus status )
  {
    try
    {
      if( newValue != null && status.isOK() )
        objectLoaded( key, newValue );
      else
        Logger.getLogger( getClass().getName() ).warning( Messages.getString("org.kalypso.util.pool.PoolableObjectWaiter.1") + key ); //$NON-NLS-1$
    }
    // what happens if objectLoaded throws an exception?
    // set status?
    finally
    {
      dispose();
    }
  }

  /**
   * This method may be called in the class constructor, so dont use own member-fields. See
   * {@link #PoolableObjectWaiter(PoolableObjectType, Object[], boolean)}
   */
  protected abstract void objectLoaded( final IPoolableObjectType key, final Object newValue );

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectInvalid(org.kalypso.util.pool.IPoolableObjectType, java.lang.Object)
   */
  public final void objectInvalid( IPoolableObjectType key, Object oldValue )
  {
    dispose();
  }
  
  /**
   * @see org.kalypso.util.pool.IPoolListener#dirtyChanged(org.kalypso.util.pool.IPoolableObjectType, boolean)
   */
  public void dirtyChanged( IPoolableObjectType key, boolean isDirty )
  {
    // TODO Auto-generated method stub
  }

  protected void dispose()
  {
    m_disposed = true;
    m_pool.removePoolListener( this );
  }

  protected boolean isSynchron()
  {
    return m_synchron;
  }

  public IStatus getResult()
  {
    return m_result;
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#isDisposed()
   */
  public boolean isDisposed()
  {
    return m_disposed;
  }
}