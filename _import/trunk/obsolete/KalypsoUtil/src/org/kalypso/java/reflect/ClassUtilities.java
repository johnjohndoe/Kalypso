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
   * @see ClassUtilities#newInstance(String, Class, ClassLoader, Class[],
   *      Object[])
   */
  public static Object newInstance( final String classname, final Class target, final ClassLoader cl )
      throws ClassUtilityException
  {
    return newInstance( classname, target, cl, null, null );
  }

  /**
   * fetches the classes of the arguments directly, calling getClass() on each
   * Object. This might work most of the time, but in some cases, where the
   * arguments do inherit from the class required in the constructor, the
   * constructor might not be found. In these cases, use the full version of
   * this method where you can exactly specify the classes of the arguments.
   * 
   * @see ClassUtilities#newInstance(String, Class, ClassLoader, Class[],
   *      Object[])
   */
  public static Object newInstance( final String classname, final Class target,
      final ClassLoader cl, final Object[] arguments ) throws ClassUtilityException
  {
    Class[] argClasses = null;
    
    if( arguments != null )
    {
      argClasses = new Class[arguments.length];
  
      for( int i = 0; i < argClasses.length; i++ )
        argClasses[i] = arguments[i].getClass();
    }

    return newInstance( classname, target, cl, argClasses, arguments );
  }

  /**
   * creates a new instance using reflection.
   * 
   * @param classname
   *          Object of this class to create
   * @param target
   *          [optional] classname must be assignable from target
   * @param cl
   *          the ClassLoader to use for instantiating the object
   * 
   * @param argClasses
   *          [optional] can be null, the list of the class of the arguments to
   *          pass to Class.getConstructor(java.lang.Class[]). If
   *          <code>arguments</code> is not null, then <code>argClasses</code>
   *          should neither be null.
   * @param arguments
   *          [optional] can be null, the list of arguments to pass to the
   *          constructor. If this argument is specified, you should also
   *          specifiy the <code>argClasses</code> argument.
   * 
   * @return new Object
   * 
   * @throws ClassUtilityException
   * 
   * @see Class#getConstructor(java.lang.Class[])
   */
  public static Object newInstance( final String classname, final Class target,
      final ClassLoader cl, final Class[] argClasses, final Object[] arguments )
      throws ClassUtilityException
  {
    try
    {
      final Class c = Class.forName( classname, true, cl );

      if( ( target == null ) || target.isAssignableFrom( c ) )
      {
        if( arguments == null )
          return c.newInstance();

        Constructor cons = c.getConstructor( argClasses );

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