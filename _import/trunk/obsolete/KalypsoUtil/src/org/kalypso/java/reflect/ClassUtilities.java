package org.kalypso.java.reflect;

import java.io.IOException;
import java.util.Properties;

/**
 * Utilities method on Class stuff
 * 
 * @author schlienger
 */
public class ClassUtilities
{
  private static Properties m_typeToClassProps;

  /**
   * creates a new instance using reflection.
   * 
   * @param classname
   *          Object of this class to create
   * @param target
   *          [optional] classname must be assignable from target
   * 
   * @return new Object
   * 
   * @throws ClassUtilityException
   */
  public static Object newInstance( final String classname, final Class target, final ClassLoader cl ) throws ClassUtilityException
  {
    try
    {
      final Class c = Class.forName( classname, true, cl );

      if( ( target == null ) || target.isAssignableFrom( c ) )
        return c.newInstance();

      throw new ClassUtilityException( "Class " + classname + " not assignable from "
          + target.getName() );
    }
    catch( ClassNotFoundException e )
    {
      throw new ClassUtilityException( e );
    }
    catch( InstantiationException e )
    {
      throw new ClassUtilityException( e );
    }
    catch( IllegalAccessException e )
    {
      throw new ClassUtilityException( e );
    }
  }

  /**
   * Returns the Class that corresponds to the given type. Uses the mapping
   * defined in the type2class.map.ini file if no mapping is provided.
   * 
   * @param type the type to look for
   * @param mapping [optional] if not null, uses this mapping to resolve class name
   * @throws ClassNotFoundException
   */
  public static Class typeToClass( final String type, final Properties mapping /*, final ClassLoader cl */ ) throws ClassNotFoundException
  {
    final Properties typeMapping = mapping == null ? getTypeToClassProperties() : mapping;
    
    final String foo = typeMapping.getProperty(type);
    System.out.println( foo + " - " + type );
    return Class.forName( foo );//, true, cl );
  }
  
  
  /**
   * Helper that loads the properties for type mapping
   */
  private static Properties getTypeToClassProperties()
  {
    if( m_typeToClassProps == null )
    {
      m_typeToClassProps = new Properties();
      
      try
      {
        m_typeToClassProps.load( ClassUtilities.class.getResourceAsStream( "type2class.map.ini" ) );
      }
      catch( IOException e )
      {
        e.printStackTrace();
        throw new RuntimeException( e );
      }
    }
    
    return m_typeToClassProps;
  }
  
  /**
   * An Exception that can occur using the upper class
   * 
   * @author schlienger
   */
  public static class ClassUtilityException extends Exception
  {
    public ClassUtilityException()
    {
      super();
    }

    public ClassUtilityException( String message )
    {
      super( message );
    }

    public ClassUtilityException( String message, Throwable cause )
    {
      super( message, cause );
    }

    public ClassUtilityException( Throwable cause )
    {
      super( cause );
    }
  }
}