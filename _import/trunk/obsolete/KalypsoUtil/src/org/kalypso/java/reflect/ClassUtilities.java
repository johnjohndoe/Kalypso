package org.kalypso.java.reflect;

/**
 * Utilities method on Class stuff
 * 
 * @author schlienger
 */
public class ClassUtilities
{
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