package org.kalypso.util.pool;

import org.apache.commons.pool.KeyedPoolableObjectFactory;
import org.kalypso.util.loader.ILoader;
import org.kalypso.util.loader.ILoaderFactory;

/**
 * @author schlienger
 *
 */
public class TypedKeyedPoolableObjectFactory implements KeyedPoolableObjectFactory
{
  private final ILoaderFactory m_factory;

  public TypedKeyedPoolableObjectFactory( final ILoaderFactory factory )
  {
    m_factory = factory;
  }

  /**
   * Erzeugt ein Objekt anhand seiner Typ. Benutzt das entsprechende ILoader.
   * 
   * @param key ein IPoolableObjectType
   * 
   * @see org.apache.commons.pool.KeyedPoolableObjectFactory#makeObject(java.lang.Object)
   */
  public Object makeObject( Object key ) throws Exception
  {
    IPoolableObjectType objType = testTypeOfKey( key );
    
    ILoader loader = m_factory.getLoaderInstance( objType.getType() );
    
    return loader.load( objType.getSource() );
  }

  /**
   * @see org.apache.commons.pool.KeyedPoolableObjectFactory#destroyObject(java.lang.Object, java.lang.Object)
   */
  public void destroyObject( Object key, Object arg1 ) throws Exception
  {
    testTypeOfKey( key );
  }

  /**
   * @see org.apache.commons.pool.KeyedPoolableObjectFactory#validateObject(java.lang.Object, java.lang.Object)
   */
  public boolean validateObject( Object key, Object arg1 )
  {
    testTypeOfKey( key );
    
    return false;
  }

  /**
   * @see org.apache.commons.pool.KeyedPoolableObjectFactory#activateObject(java.lang.Object, java.lang.Object)
   */
  public void activateObject( Object key, Object arg1 ) throws Exception
  {
    testTypeOfKey( key );
  }

  /**
   * @see org.apache.commons.pool.KeyedPoolableObjectFactory#passivateObject(java.lang.Object, java.lang.Object)
   */
  public void passivateObject( Object key, Object arg1 ) throws Exception
  {
    testTypeOfKey( key );
  }

  private IPoolableObjectType testTypeOfKey( Object key )
  {
    if( !(key instanceof IPoolableObjectType) )
      throw new IllegalArgumentException("Key soll eine Instanz von " + IPoolableObjectType.class.getName() + " sein." );
    
    return (IPoolableObjectType)key;
  }
}
