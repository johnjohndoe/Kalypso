package org.kalypso.util.pool;

import java.util.HashMap;

import org.apache.commons.pool.KeyedObjectPool;
import org.apache.commons.pool.KeyedPoolableObjectFactory;

/**
 * @author sbad0205
 */
public class KalypsoKeyedObjectPool implements KeyedObjectPool
{
  private KeyedPoolableObjectFactory myFactory;
private final HashMap myPool;
  public KalypsoKeyedObjectPool(KeyedPoolableObjectFactory factory)
      {
    myFactory = factory;
    myPool = new HashMap();
      }

  /**
   * @see org.apache.commons.pool.KeyedObjectPool#borrowObject(java.lang.Object)
   */
  public Object borrowObject( Object key ) throws Exception
  {
    addObject(key);
    CountableObject obj= (CountableObject)myPool.get(key);
    obj.increment();
    return obj.getObject();
  }

  /**
   * @see org.apache.commons.pool.KeyedObjectPool#returnObject(java.lang.Object, java.lang.Object)
   */
  public void returnObject( Object key, Object obj ) throws Exception
  {
    CountableObject cObj= (CountableObject)myPool.get(key);
    if(cObj==null || cObj.getObject()!=obj)
      throw new Exception("invalid key or invalid object returned to pool");
    cObj.decrement();
    if(cObj.isUnused())
      myPool.remove(key);    
    // TODO wenn hier ein resourcelistener eingebaut wird, dann koennen die objecte
    // evt. statt aus dem pool geworfen zu werden einen IDLE-state bekommen
  }

  /**
   * @see org.apache.commons.pool.KeyedObjectPool#invalidateObject(java.lang.Object, java.lang.Object)
   */
  public void invalidateObject( Object key, Object obj ) throws Exception
  {
  throw new UnsupportedOperationException();  
  }

  /**
   * @see org.apache.commons.pool.KeyedObjectPool#addObject(java.lang.Object)
   */
  public void addObject( Object key ) throws Exception
  {
    if(!myPool.containsKey(key))
    {
      CountableObject cObj= new CountableObject(myFactory.makeObject(key));
      myPool.put(key, cObj);
    }
  }

  /**
   * @see org.apache.commons.pool.KeyedObjectPool#getNumIdle(java.lang.Object)
   */
  public int getNumIdle( Object key ) throws UnsupportedOperationException
  {
    return 0;
  }

  /**
   * @see org.apache.commons.pool.KeyedObjectPool#getNumActive(java.lang.Object)
   */
  public int getNumActive( Object key ) throws UnsupportedOperationException
  {
    if(myPool.containsKey(key))
      return 1;
    return 0;
   }

  /**
   * @see org.apache.commons.pool.KeyedObjectPool#getNumIdle()
   */
  public int getNumIdle() throws UnsupportedOperationException
  {
    return 0;
  }

  /**
   * @see org.apache.commons.pool.KeyedObjectPool#getNumActive()
   */
  public int getNumActive() throws UnsupportedOperationException
  {
    return myPool.size();
  }

  /**
   * @see org.apache.commons.pool.KeyedObjectPool#clear()
   */
  public void clear() throws Exception, UnsupportedOperationException
  {
    throw new UnsupportedOperationException();  
  
  }

  /**
   * @see org.apache.commons.pool.KeyedObjectPool#clear(java.lang.Object)
   */
  public void clear( Object key ) throws Exception, UnsupportedOperationException
  {
    throw new UnsupportedOperationException();  
  
  }

  /**
   * @see org.apache.commons.pool.KeyedObjectPool#close()
   */
  public void close() throws Exception
  {
    throw new UnsupportedOperationException();  
    }

  /**
   * @see org.apache.commons.pool.KeyedObjectPool#setFactory(org.apache.commons.pool.KeyedPoolableObjectFactory)
   */
  public void setFactory( KeyedPoolableObjectFactory factory ) throws IllegalStateException,
      UnsupportedOperationException
  {  
    myFactory = factory;
  }

  private class CountableObject
  {
    private final Object myObject;
    private int myCounter;
    public CountableObject(Object object)
    {
      myObject=object;
      myCounter=0;
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
    public boolean isUnused()
    {
      return myCounter==0;
    }
  }
}
