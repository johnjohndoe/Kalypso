/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.transformation;

import org.eclipse.osgi.framework.internal.core.FrameworkProperties;

/**
 * This class provides some functions for the GeoTransformer.
 * 
 * @author Holger Albert
 */
@SuppressWarnings("restriction")
public class TransformUtilities
{
  /**
   * The constructor.
   */
  private TransformUtilities( )
  {
  }

  /**
   * This function will check a SystemProperty, if a transformation of geographic data should be done.<br>
   * The default value is always true (e.g. in the case, the property is not set or set to true).<br>
   * Only if the property is set to false, this function returns false.
   * 
   * @return True, if a transformation should be done, false otherwise.
   */
  public static boolean shouldTransform( )
  {
    String transform = getTransformProperty();
    if( (transform != null) && (transform.equalsIgnoreCase( "false" )) )
    {
      Debug.TRANSFORM.printf( "Should not transform ...\n" );
      return false;
    }

    Debug.TRANSFORM.printf( "Should transform ...\n" );
    return true;
  }

  /**
   * This function will return the string of the SystemProperty 'org.kalypso.kalypsodegree.transform'.
   * 
   * @return The value property or null.
   */
  private static String getTransformProperty( )
  {
    return FrameworkProperties.getProperty( "org.kalypso.kalypsodegree.transform" );
  }
}
