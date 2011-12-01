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
package org.kalypso.model.flood.i18n;

import java.util.IllegalFormatException;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 * Own messages class for the project-maps, in order to spearate the map messages from the normal ones.
 * 
 */
public class MapMessages
{
  private static final String BUNDLE_NAME = "org.kalypso.model.flood.i18n.map_messages"; //$NON-NLS-1$

  private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle( BUNDLE_NAME );

  private static final Object[] NO_ARGS = new Object[0];

  private MapMessages( )
  {
  }

  /*
   * java reflections needs this method-signatur
   */
  public static String getString( final String key )
  {
    return getString( key, NO_ARGS );
  }

  public static String getString( final String key, final Object... args )
  {
    String formatStr = ""; //$NON-NLS-1$
    try
    {
      formatStr = RESOURCE_BUNDLE.getString( key );
      if( args.length == 0 )
        return formatStr;

      return String.format( formatStr, args );
    }
    catch( final MissingResourceException e )
    {
      return '!' + key + '!';
    }
    catch( final IllegalFormatException e )
    {
      e.printStackTrace();
      return '!' + formatStr + '!';
    }
  }
}
