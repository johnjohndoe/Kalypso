package org.kalypso.util.pool;

import java.net.URL;

/**
 * @author vdoemming
 */
public class PoolableObjectType implements IPoolableObjectType
{
  private final String m_type;

  private final URL m_context;
  
  private final String m_source;

  private final boolean m_ignoreExceptions;

  /**
   * Constructor. Defaults ignoreExceptions to false.
   * 
   * @param type
   * @param source
   * @param context
   */
  public PoolableObjectType( final String type, final String source, final URL context )
  {
    this( type, source, context, false );
  }
  
  /**
   * Constructor.
   * 
   * @param type type of object to load
   * @param source location of the object
   * @param context context into which object is loaded
   * @param ignoreExceptions when true, exceptions occuring during load process are ignored, object won't get pooled
   */
  public PoolableObjectType( final String type, final String source, final URL context, final boolean ignoreExceptions )
  {
    m_type = type;
    m_source = source;
    m_context = context;
    m_ignoreExceptions = ignoreExceptions;
  }

  /**
   * @see org.kalypso.util.pool.IPoolableObjectType#getType()
   */
  public String getType()
  {
    return m_type;
  }

  /**
   * @see org.kalypso.util.pool.IPoolableObjectType#getLocation()
   */
  public String getLocation()
  {
    return m_source;
  }

  /**
   * @see org.kalypso.util.pool.IPoolableObjectType#getContext()
   */
  public URL getContext()
  {
    return m_context;
  }

  /**
   * @return Returns the ignoreExceptions.
   */
  public boolean isIgnoreExceptions( )
  {
    return m_ignoreExceptions;
  }
  
  public boolean equals( Object obj )
  {
    if( !( obj instanceof IPoolableObjectType ) )
      return false;

    final IPoolableObjectType other = (IPoolableObjectType)obj;
    if( !getType().equals( other.getType() ) )
      return false;
    if( !getLocation().equals( other.getLocation() ) )
      return false;
    if( getContext() != null && !getContext().equals( other.getContext() ) )
      return false;

    return true;
  }

  public int hashCode()
  {
    return toString().hashCode();
  }

  /**
   * @see java.lang.Object#toString()
   */
  public String toString()
  {
      return getClass().getName() + ": location=" + getLocation() + " type=" + getType() + " context= "
          + getContext();
  }
}