package org.kalypso.java.reflect;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;


/**
 * Utilities method on Class stuff
 * 
 * @author schlienger
 */
public class ClassUtilities
{
  /**
   * @see ClassUtilities#newInstance(String, Class, ClassLoader, Object[])
   */
  public static Object newInstance( final String classname, final Class target, final ClassLoader cl ) throws ClassUtilityException
  {
    return newInstance( classname, target, cl, null );
  }
  
  /**
   * creates a new instance using reflection.
   * 
   * @param classname
   *          Object of this class to create
   * @param target
   *          [optional] classname must be assignable from target
   * @param cl the ClassLoader to use for instantiating the object
   * 
   * @param arguments [optional] can be null, the list of arguments to pass to the constructor
   * 
   * @return new Object
   * 
   * @throws ClassUtilityException
   */
  public static Object newInstance( final String classname, final Class target, final ClassLoader cl, final Object[] arguments ) throws ClassUtilityException
  {
    try
    {
      final Class c = Class.forName( classname, true, cl );

      if( ( target == null ) || target.isAssignableFrom( c ) )
      {
        if( arguments == null )
          return c.newInstance();
        
        Class[] argClass = new Class[ arguments.length ];
        for( int i = 0; i < argClass.length; i++ )
          argClass[i] = arguments[i].getClass();
        
        Constructor cons = c.getConstructor( argClass );
        
        return cons.newInstance( arguments );
      }

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
    catch( SecurityException e )
    {
      throw new ClassUtilityException( e );
    }
    catch( NoSuchMethodException e )
    {
      throw new ClassUtilityException( e );
    }
    catch( IllegalArgumentException e )
    {
      throw new ClassUtilityException( e );
    }
    catch( InvocationTargetException e )
    {
      throw new ClassUtilityException( e );
    }
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