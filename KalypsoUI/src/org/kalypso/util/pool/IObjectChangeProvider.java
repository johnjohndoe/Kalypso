package org.kalypso.util.pool;


/**
 * @author belger
 */
public interface IObjectChangeProvider
{
  public void addPoolListener( final IPoolListener l );
  
  public void removePoolListener( final IPoolListener l );
  
  public void fireOnObjectInvalid( final Object oldObject, final boolean bCannotReload ) throws Exception;
}
