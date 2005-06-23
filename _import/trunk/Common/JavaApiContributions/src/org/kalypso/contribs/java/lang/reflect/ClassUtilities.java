/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.contribs.java.lang.reflect;

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
   * Returns the classname of the given class without package part.
   * <p>
   * Example: org.foo.bar.FooBar will return FooBar
   * 
   * @param someClass
   * @return the classname of the given class without package part.
   */
  public static String getOnlyClassName( final Class someClass )
  {
    final String className = someClass.getName().substring( someClass.getName().lastIndexOf( '.' ) + 1 );

    return className;
  }

  /**
   * @param classname
   * @param target
   * @param cl
   * @return new instance
   * @throws ClassUtilityException
   * 
   * @see ClassUtilities#newInstance(String, Class, ClassLoader, Class[], Object[])
   */
  public static Object newInstance( final String classname, final Class target, final ClassLoader cl )
      throws ClassUtilityException
  {
    return newInstance( classname, target, cl, null, null );
  }

  /**
   * fetches the classes of the arguments directly, calling getClass() on each Object. This might work most of the time,
   * but in some cases, where the arguments do inherit from the class required in the constructor, the constructor might
   * not be found. In these cases, use the full version of this method where you can exactly specify the classes of the
   * arguments.
   * 
   * @param classname
   * @param target
   * @param cl
   * @param arguments
   * @return new instance
   * @throws ClassUtilityException
   * 
   * @see ClassUtilities#newInstance(String, Class, ClassLoader, Class[], Object[])
   */
  public static Object newInstance( final String classname, final Class target, final ClassLoader cl,
      final Object[] arguments ) throws ClassUtilityException
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
   *          [optional] can be null, the list of the class of the arguments to pass to
   *          Class.getConstructor(java.lang.Class[]). If <code>arguments</code> is not null, then
   *          <code>argClasses</code> should neither be null.
   * @param arguments
   *          [optional] can be null, the list of arguments to pass to the constructor. If this argument is specified,
   *          you should also specifiy the <code>argClasses</code> argument.
   * 
   * @return new Object
   * 
   * @throws ClassUtilityException
   * 
   * @see Class#getConstructor(java.lang.Class[])
   */
  public static Object newInstance( final String classname, final Class target, final ClassLoader cl,
      final Class[] argClasses, final Object[] arguments ) throws ClassUtilityException
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

      throw new ClassUtilityException( "Class " + classname + " not assignable from " + target.getName() );
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
}