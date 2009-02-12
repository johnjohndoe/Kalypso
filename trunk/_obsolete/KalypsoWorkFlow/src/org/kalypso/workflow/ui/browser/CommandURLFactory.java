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
package org.kalypso.workflow.ui.browser;

import java.util.Properties;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.kalypso.contribs.java.util.PropertiesUtilities;

/**
 * @author doemming
 */
public final class CommandURLFactory
{
  final private static Pattern URL_PATTERN = Pattern.compile( // 
      // protocol ___ host port___________ path______ ref_________ query ______
      "^(.+?://){0,1}(.*?)(:([0-9]*)){0,1}(/.*?){0,1}(#(.*?)){0,1}(\\?(.*)){0,1}" );

  /**
   * Non-instantiable.
   */
  private CommandURLFactory( )
  {
    // do nothing
  }

  public static ICommandURL createCommandURL( String urlString )
  {
    final Matcher matcher = URL_PATTERN.matcher( urlString );
    if( matcher.matches() )
    {
      // int x = matcher.groupCount();
      // for( int i = 0; i <= x; i++ )
      // System.out.println( i + ": " + matcher.group( i ) );
      // final String protocol = matcher.group( 1 );
      final String host = matcher.group( 2 );
      // final String port = matcher.group( 4 );
      final String path = matcher.group( 5 );
      // final String reference = matcher.group( 7 );
      final String query = matcher.group( 9 );

      final String action = host;
      final Properties properties = PropertiesUtilities.collectProperties( query, "&", "=", null );
      return new CommandURL( action, path, properties );
    }
    final Properties properties = new Properties();
    properties.put( "message", "could not process URL: " + urlString );
    return new CommandURL( "showMessage", null, properties );
  }
}
