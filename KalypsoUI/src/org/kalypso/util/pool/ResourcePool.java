package org.kalypso.util.pool;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.loader.ILoader;
import org.kalypso.loader.ILoaderFactory;
import org.kalypso.loader.ILoaderListener;
import org.kalypso.loader.LoaderException;
import org.kalypso.util.factory.FactoryException;

/**
 * @author sbad0205
 */
public class ResourcePool implements ILoaderListener
{
  /** type -> CountableObject */
  private final Map myPool = new HashMap();

  private final IObjectChangeProvider m_objectChangeProvider = new ObjectChangeAdapter();

  private final ILoaderFactory m_factory;

  /** type -> loader */
  private final Map m_loaderCache = new HashMap();

  public ResourcePool( final ILoaderFactory factory )
  {
    m_factory = factory;
  }

  public void dispose()
  {
    for( Iterator iter = m_loaderCache.values().iterator(); iter.hasNext(); )
      ( (ILoader)iter.next() ).removeLoaderListener( this );
  }

  public Object getObject( final IPoolableObjectType key, final IProgressMonitor monitor )
      throws Exception
  {
    addObject( key, monitor );

    final CountableObject obj = (CountableObject)myPool.get( key );
    obj.increment();
    return obj.getObject();
  }

  public void releaseObject( final IPoolableObjectType key, final Object obj ) throws Exception
  {
    final CountableObject cObj = (CountableObject)myPool.get( key );
    if( cObj == null || cObj.getObject() != obj )
      throw new Exception( "invalid key or invalid object returned to pool" );

    cObj.decrement();

    if( cObj.isUnused() )
      myPool.remove( key );

    // TODO evt. statt aus dem pool werfen einen IDLE-state setzen?

    destroyObject( key, cObj.getObject() );
  }

  private void addObject( final IPoolableObjectType key, final IProgressMonitor monitor )
      throws Exception
  {
    if( !myPool.containsKey( key ) )
    {
      final Object newObject = makeObject( key, monitor );
      final CountableObject cObj = new CountableObject( newObject );

      myPool.put( key, cObj );
    }
  }

  private class CountableObject
  {
    private Object myObject;

    private int myCounter;

    public CountableObject( Object object )
    {
      myObject = object;
      myCounter = 0;
    }

    public void increment()
    {
      myCounter++;
    }

    public void decrement()
    {
      myCounter--;
    }

    public Object getObject()
    {
      return myObject;
    }

    public void setObject( final Object o )
    {
      myObject = o;
    }

    public boolean isUnused()
    {
      return myCounter == 0;
    }
  }

  public void addPoolListener( final IPoolListener l )
  {
    m_objectChangeProvider.addPoolListener( l );
  }

  public void fireOnObjectInvalid( final Object oldObject, final boolean bCannotReload )
      throws Exception
  {
    m_objectChangeProvider.fireOnObjectInvalid( this, findKey( oldObject ), oldObject, bCannotReload );
  }

  public void removePoolListener( final IPoolListener l )
  {
    m_objectChangeProvider.removePoolListener( l );
  }

  /**
   * @see org.kalypso.loader.ILoaderListener#onLoaderObjectInvalid(java.lang.Object,
   *      boolean)
   */
  public void onLoaderObjectInvalid( final Object oldValue, final boolean bCannotReload )
      throws Exception
  {
    final IPoolableObjectType key = findKey( oldValue );
    if( key != null )
    {
      myPool.remove( key );

      fireOnObjectInvalid( oldValue, bCannotReload );
    }
  }

  private IPoolableObjectType findKey( Object oldValue )
  {
    for( final Iterator iter = myPool.entrySet().iterator(); iter.hasNext(); )
    {
      final Map.Entry element = (Entry)iter.next();
      final CountableObject co = (CountableObject)element.getValue();
      if( co.getObject() == oldValue )
        return (IPoolableObjectType)element.getKey();
    }

    return null;
  }

  /**
   * Erzeugt ein Objekt anhand seines Typs. Benutzt den entsprechenden ILoader.
   */
  private Object makeObject( final IPoolableObjectType key, final IProgressMonitor monitor )
      throws Exception
  {
    final String type = key.getType();

    final ILoader loader = getLoader( type );

    final Object object = loader.load( key.getSource(), key.getProject(), monitor );

    return object;
  }

  private ILoader getLoader( final String type ) throws FactoryException
  {
    ILoader loader = (ILoader)m_loaderCache.get( type );
    if( loader == null )
    {
      loader = m_factory.getLoaderInstance( type );

      loader.addLoaderListener( this );

      m_loaderCache.put( type, loader );
    }

    return loader;
  }

  private void destroyObject( final IPoolableObjectType key, final Object object ) throws Exception
  {
    final ILoader loader = getLoader( key.getType() );
    loader.release( object );
  }

  public void saveObject( final Object object, final IProgressMonitor monitor ) throws FactoryException
  {
    final IPoolableObjectType key = findKey( object );
    
    if( key != null )
    {
      final ILoader loader = getLoader( key.getType() ); 

      try
      {
        loader.save( key.getSource(), key.getProject(), monitor, object );
      }
      catch( LoaderException e )
      {
        throw new FactoryException( e );
      }
    }
  }

}