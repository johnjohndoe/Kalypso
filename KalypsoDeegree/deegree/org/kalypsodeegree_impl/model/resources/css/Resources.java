/*--------------- Kalypso-Deegree-Header ------------------------------------------------------------

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
 
 
 history:
  
 Files in this package are originally taken from deegree and modified here
 to fit in kalypso. As goals of kalypso differ from that one in deegree
 interface-compatibility to deegree is wanted but not retained always. 
     
 If you intend to use this software in other ways than in kalypso 
 (e.g. OGC-web services), you should consider the latest version of deegree,
 see http://www.deegree.org .

 all modifications are licensed as deegree, 
 original copyright:
 
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
 http://www.lat-lon.de
 
---------------------------------------------------------------------------------------------------*/
package org.kalypsodeegree_impl.model.resources.css;

// Miscellaneous
import java.util.Locale;
import java.util.MissingResourceException;

import org.kalypsodeegree_impl.model.resources.ResourceBundle;

/**
 * Base class for local-dependent resources. Instances of this class should
 * never been created directly. Use the factory method {@link #getResources}or
 * use static methods instead.
 * 
 * @version 1.0
 * @author Martin Desruisseaux
 */
public class Resources extends ResourceBundle
{
  /**
   * Construct a resource bundle using english language. This is the default
   * when no resource are available in user language.
   */
  public Resources()
  {
    super( // Set 'true' in front of language to use as default.
        false ? Resources_fr.FILEPATH : true ? Resources_en.FILEPATH : null );
  }

  /**
   * Construct a resource bundle using the specified UTF8 file.
   */
  Resources( final String filepath )
  {
    super( filepath );
  }

  /**
   * Returns the name of the logger to use, which is
   * <code>org.kalypsodeegree_impl.model.css</code>.
   */
  protected String getLoggerName()
  {
    return "org.kalypsodeegree_impl.model.css";
  }

  /**
   * Returns resources in the given locale.
   * 
   * @param locale
   *          The locale, or <code>null</code> for the default locale.
   * @return Resources in the given locale.
   * @throws MissingResourceException
   *           if resources can't be found.
   */
  public static Resources getResources( Locale locale ) throws MissingResourceException
  {
    if( locale == null )
      locale = Locale.getDefault();
    return (Resources)getBundle( Resources.class.getName(), locale );
    /*
     * We rely on cache capability of {@link java.util.ResourceBundle}.
     */
  }

  /**
   * Gets a string for the given key from this resource bundle or one of its
   * parents.
   * 
   * @param key
   *          The key for the desired string.
   * @return The string for the given key.
   * @throws MissingResourceException
   *           If no object for the given key can be found.
   */
  public static String format( final int key ) throws MissingResourceException
  {
    return getResources( null ).getString( key );
  }

  /**
   * Gets a string for the given key are replace all occurence of "{0}" with
   * values of <code>arg0</code>.
   * 
   * @param key
   *          The key for the desired string.
   * @param arg0
   *          Value to substitute to "{0}".
   * @return The formatted string for the given key.
   * @throws MissingResourceException
   *           If no object for the given key can be found.
   */
  public static String format( final int key, final Object arg0 ) throws MissingResourceException
  {
    return getResources( null ).getString( key, arg0 );
  }

  /**
   * Gets a string for the given key are replace all occurence of "{0}", "{1}",
   * with values of <code>arg0</code>,<code>arg1</code>.
   * 
   * @param key
   *          The key for the desired string.
   * @param arg0
   *          Value to substitute to "{0}".
   * @param arg1
   *          Value to substitute to "{1}".
   * @return The formatted string for the given key.
   * @throws MissingResourceException
   *           If no object for the given key can be found.
   */
  public static String format( final int key, final Object arg0, final Object arg1 )
      throws MissingResourceException
  {
    return getResources( null ).getString( key, arg0, arg1 );
  }

  /**
   * Gets a string for the given key are replace all occurence of "{0}", "{1}",
   * with values of <code>arg0</code>,<code>arg1</code>, etc.
   * 
   * @param key
   *          The key for the desired string.
   * @param arg0
   *          Value to substitute to "{0}".
   * @param arg1
   *          Value to substitute to "{1}".
   * @param arg2
   *          Value to substitute to "{2}".
   * @return The formatted string for the given key.
   * @throws MissingResourceException
   *           If no object for the given key can be found.
   */
  public static String format( final int key, final Object arg0, final Object arg1,
      final Object arg2 ) throws MissingResourceException
  {
    return getResources( null ).getString( key, arg0, arg1, arg2 );
  }
}