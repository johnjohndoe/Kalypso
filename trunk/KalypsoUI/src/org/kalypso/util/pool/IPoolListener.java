package org.kalypso.util.pool;

/**
 * @author belger
 */
public interface IPoolListener
{
  public void onObjectInvalid( final Object oldObject, final boolean bCannotReload ) throws Exception;
}
