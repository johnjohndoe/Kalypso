package org.kalypso.util.pool;

import java.util.Properties;

import org.eclipse.core.resources.IProject;
import org.kalypso.java.properties.PropertiesHelper;

/**
 * @author vdoemming
 */
public class PoolableObjectType implements IPoolableObjectType
{
  private static final char PROPERTY_SEPARATOR = '#';
  private final String myType;

  private final Properties mySource;
  
  private final IProject myProject;

  public PoolableObjectType( final String type, final String source, final IProject project )
  {
    this( type, PropertiesHelper.parseFromString( source, PROPERTY_SEPARATOR ), project );
  }

  public PoolableObjectType( final String type, final Properties source, final IProject project )
  {
    myType = type;
    mySource = source;
    myProject = project;
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
   * @see org.kalypso.util.pool.IPoolableObjectType#getProject()
   */
  public IProject getProject()
  {
    return myProject;
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
    if( !getProject().equals( other.getProject() ) )
      return false;

    return true;
  }

  public int hashCode()
  {
    return toString().hashCode();
  }

  public String toString()
  {
    return getClass().getName() + ": source=" + getSource() + " type=" + getType() + " project=" + getProject().getName();
  }

  public String getSourceAsString()
  {
    return PropertiesHelper.format( getSource(), PROPERTY_SEPARATOR );
  }

}