package org.kalypso.util.pool;

import java.util.Properties;

import org.kalypso.java.properties.PropertiesHelper;

/**
 * @author vdoemming
 */
public class PoolableObjectType implements IPoolableObjectType
{
  private final String myType;

  private final Properties mySource;
  
  private final Object myHelper;

  public PoolableObjectType( final String type, final String source, final Object helper )
  {
    this( type, PropertiesHelper.parseFromString( source, '#' ), helper );
  }

  public PoolableObjectType( final String type, final Properties source, final Object helper )
  {
    myType = type;
    mySource = source;
    myHelper = helper;
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
   * @see org.kalypso.util.pool.IPoolableObjectType#getHelper()
   */
  public Object getHelper()
  {
    return myHelper;
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
    if( !getHelper().equals( other.getHelper() ) )
      return false;

    return true;
  }

  public int hashCode()
  {
    return getSource().hashCode();
  }

  public String toString()
  {
    return getClass().getName() + ": source=" + getSource() + " type=" + getType();
  }
}