package org.kalypso.util.pool;

import org.eclipse.core.runtime.IStatus;

/**
 * @author belger
 */
public interface IPoolListener
{
  public void objectLoaded( final IPoolableObjectType key, final Object newValue, final IStatus status );

  public void objectInvalid( final IPoolableObjectType key, final Object oldValue );
}
