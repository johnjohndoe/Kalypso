package org.kalypso.util.pool;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * @author belger
 */
public class ObjectChangeAdapter implements IObjectChangeProvider
{
  private final List m_listener = new ArrayList();
  
  public void addPoolListener( final IPoolListener l )
  {
    m_listener.add( l );
  }
  
  public void removePoolListener( final IPoolListener l )
  {
    m_listener.remove( l );
  }
  
  public void fireOnObjectInvalid( final ResourcePool pool, final IPoolableObjectType key, final Object oldObject, final boolean bCannotReload ) throws Exception
  {
    for( Iterator lIt = m_listener.iterator(); lIt.hasNext(); )
      ((IPoolListener)lIt.next()).onObjectInvalid( pool, key, oldObject, bCannotReload );
  }
}
