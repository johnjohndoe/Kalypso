package org.kalypso.util.pool;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * Waits for a key to load
 * 
 * @author belger
 */
public abstract class PoolableObjectWaiter implements IPoolListener
{
  private final ResourcePool m_pool = KalypsoGisPlugin.getDefault().getPool();

  protected final Object[] m_data;

  private IStatus m_result = Status.OK_STATUS;

  /**
   * TRICKY: inherited classes may NOT use own fields, because calling this
   * constructor via super() may immediately call
   * {@link #objectLoaded(IPoolableObjectType, Object)}
   * 
   * @param synchron
   *          Falls true, wird die Obersvation sofort geladen und im gleichen
   *          thread objectLoaded ausgef�hrt
   */
  public PoolableObjectWaiter( final PoolableObjectType key, final Object[] data,
      final boolean synchron )
  {
    m_data = data;

    if( synchron )
    {
      Object value = null;
      try
      {
        value = m_pool.getObject( key );
        objectLoaded( null, value, m_result );
      }
      catch( final CoreException ce )
      {
        m_result = ce.getStatus();
      }
      catch( final Exception e )
      {
        m_result = KalypsoGisPlugin.createErrorStatus( "Fehler beim Laden eines Objektes", e );
      }
    }
    else
      m_pool.addPoolListener( this, key );
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectLoaded(org.kalypso.util.pool.IPoolableObjectType,
   *      java.lang.Object, org.eclipse.core.runtime.IStatus)
   */
  public final void objectLoaded( final IPoolableObjectType key, final Object newValue,
      final IStatus status )
  {
    if( newValue != null && status.isOK() )
      objectLoaded( key, newValue );
    else
    {
      // TODO error handling!
    }

    dispose();
  }

  /**
   * This method may be called in the class constructor, so dont use own
   * member-fields. See
   * {@link #PoolableObjectWaiter(PoolableObjectType, Object[], boolean)}
   */
  protected abstract void objectLoaded( final IPoolableObjectType key, final Object newValue );

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectInvalid(org.kalypso.util.pool.IPoolableObjectType,
   *      java.lang.Object)
   */
  public final void objectInvalid( IPoolableObjectType key, Object oldValue )
  {
    dispose();
  }

  protected void dispose()
  {
    m_pool.removePoolListener( this );
  }
  
  public IStatus getResult()
  {
    return m_result;
  }

}