package org.kalypso.util.pool;

import java.util.HashMap;

/**
 * @author vdoemming
 */
public class PoolableObjectType implements IPoolableObjectType
{
  private final String myType;

  private final HashMap mySource;
  
  public PoolableObjectType( final String type, final String source )
  {
    myType = type;
    
    mySource = new HashMap();
     final String[] strings = source.split("&");
     for(int i=0;i<strings.length;i++)
     {
       int pos=strings[i].indexOf("=");
       if(pos>0)
         mySource.put(strings[i].substring(0,pos),strings[i].substring(pos+1));
      
     }
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
  public HashMap getSource()
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