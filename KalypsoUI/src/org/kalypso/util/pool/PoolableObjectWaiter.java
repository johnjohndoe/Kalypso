package org.kalypso.util.pool;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * Waits for a key to load
 * 
 * 
 * @author belger
 */
public abstract class PoolableObjectWaiter implements IPoolListener
{
  private final ResourcePool m_pool = KalypsoGisPlugin.getDefault().getPool();
  protected final Object[] m_data;

  /** TRICKY:
   * inherited classes may NOT use own fields, because calling this constructor via super()
   * may immediately call {@link #objectLoaded(IPoolableObjectType, Object)} */
  public PoolableObjectWaiter( final PoolableObjectType key, final Object[] data )
  {
    m_data = data;
    m_pool.addPoolListener( this, key );
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectLoaded(org.kalypso.util.pool.IPoolableObjectType, java.lang.Object, org.eclipse.core.runtime.IStatus)
   */
  public final void objectLoaded( IPoolableObjectType key, Object newValue, IStatus status )
  {
    if( newValue != null && status.isOK() )
      objectLoaded( key, newValue );
    else
    {
      // error handling!
    }
    
    dispose();
  }

  /** This mehtod may be called in the class constructor, so dont use 
   * own member-fields. See {@link #PoolableObjectWaiter(PoolableObjectType, Object[])}*/
  protected abstract void objectLoaded( final IPoolableObjectType key, final Object newValue );

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectInvalid(org.kalypso.util.pool.IPoolableObjectType, java.lang.Object)
   */
  public final void objectInvalid( IPoolableObjectType key, Object oldValue )
  {
    dispose();
  }

  protected void dispose()
  {
    m_pool.removePoolListener( this );
  }
}
