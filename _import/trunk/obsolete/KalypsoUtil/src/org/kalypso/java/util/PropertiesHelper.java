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
package org.kalypso.java.util;

import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.Map.Entry;

/**
 * @author bce
 */
public class PropertiesHelper
{
  private static final char ENTRY_SEPARATOR = '=';

  private PropertiesHelper()
  {
  // wird nicht instantiiert
  }

  public static Properties parseFromString( final String source, final char separator )
  {
    final Properties props = new Properties();
    if( source != null )
    {
      final String[] strings = source.split( "" + separator );
      for( int i = 0; i < strings.length; i++ )
      {
        int pos = strings[i].indexOf( ENTRY_SEPARATOR );
        if( pos > 0 )
          props.put( strings[i].substring( 0, pos ), strings[i].substring( pos + 1 ) );
      }
    }
    return props;
  }

  public static String format( final Properties source, final char separator )
  {
    final StringBuffer sb = new StringBuffer();
    for( Iterator pIt = source.entrySet().iterator(); pIt.hasNext(); )
    {
      final Map.Entry entry = (Entry)pIt.next();

      sb.append( "" + entry.getKey() + ENTRY_SEPARATOR + entry.getValue() + separator );
    }

    return sb.toString();
  }

  /**
   * Macht ein Pattern-Matching auf allen Values einer Properties-Map und ersetzt gemäss der Replace-Properties Map.
   */
  public static Properties replaceValues( final Properties sourceProps, final Properties replaceProperties )
  {
    final Properties newProps = new Properties();

    for( Iterator sourceIt = sourceProps.entrySet().iterator(); sourceIt.hasNext(); )
    {
      final Map.Entry sourceEntry = (Entry)sourceIt.next();
      final String sourceKey = sourceEntry.getKey().toString();
      final String sourceValue = sourceEntry.getValue().toString();

      final String newValue = StringUtilities.replaceAll( sourceValue, replaceProperties );
      newProps.setProperty( sourceKey, newValue );
    }

    return newProps;
  }
}