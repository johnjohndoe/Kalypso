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

  /**
   * Constructor.
   */
  public PoolableObjectType( final String type, final String source, final URL context )
  {
    m_type = type;
    m_source = source;
    m_context = context;
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