package org.kalypso.util.pool;

/**
 * @author vdoemming
 */
public class PoolableObjectType implements IPoolableObjectType
{
  private String myType = null;

  private String mySource = null;

  public PoolableObjectType()
  {}
  
  public PoolableObjectType( final String type, final String source )
  {
    myType = type;
    mySource = source;
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
  public String getSource()
  {
    return mySource;
  }

  /**
   * @see org.kalypso.util.pool.IPoolableObjectType#getHelper()
   */
  public Object getHelper()
  {
    return null;
  }

  public boolean equals( Object obj )
  {
    if( !( obj instanceof IPoolableObjectType ) )
      return false;
    IPoolableObjectType other = (IPoolableObjectType)obj;
    if( !getType().equals( other.getType() ) )
      return false;
    if( !getSource().equals( other.getSource() ) )
      return false;
    return true;
  }

  public int hashCode()
  {
    return getSource().hashCode();
  }

  public String toString()
  {
    return getClass().getName()+": source="+getSource()+" type="+getType();
  }
}