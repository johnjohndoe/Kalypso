package org.kalypso.util.pool;

import java.net.URL;
import java.util.Properties;

import org.kalypso.java.util.PropertiesHelper;

/**
 * @author vdoemming
 */
public class PoolableObjectType implements IPoolableObjectType
{
  private static final char PROPERTY_SEPARATOR = '#';

  private final String myType;

  private final Properties mySource;

  private final URL m_context;

  /**
   * Constructor with location information as string. It is parsed by
   * PropertiesHelper and a Properties object is build. Calls the constructor
   * <code>PoolableObjectType( String, Properties, URL )</code>
   */
  public PoolableObjectType( final String type, final String source, final URL context )
  {
    this( type, PropertiesHelper.parseFromString( source, PROPERTY_SEPARATOR ), context );
  }

  /**
   * Constructor.
   */
  public PoolableObjectType( final String type, final Properties source, final URL context )
  {
    myType = type;
    mySource = source;
    m_context = context;
  }

  /**
   * @see org.kalypso.util.pool.IPoolableObjectType#getType()
   */
  public String getType()
  {
    return myType;
  }

  /**
   * @see org.kalypso.util.pool.IPoolableObjectType#getSource()
   */
  public Properties getSource()
  {
    return mySource;
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
    if( !getSource().equals( other.getSource() ) )
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
      return getClass().getName() + ": source=" + getSource() + " type=" + getType() + " context= "
          + getContext();
  }

  public String getSourceAsString()
  {
    return PropertiesHelper.format( getSource(), PROPERTY_SEPARATOR );
  }
}