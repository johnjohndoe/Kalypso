package org.kalypso.util.pool;

/**
 * @author belger
 */
public interface IPoolListener
{
  public void onObjectInvalid( final ResourcePool source, final IPoolableObjectType key, final Object oldObject, final boolean bCannotReload ) throws Exception;
}
