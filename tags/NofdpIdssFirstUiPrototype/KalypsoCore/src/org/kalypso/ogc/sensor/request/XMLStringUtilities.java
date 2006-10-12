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
package org.kalypso.ogc.sensor.request;

/**
 * @author huebsch
 */
public class XMLStringUtilities
{
  /**
   * must return <code>null</code> if not present
   */
  public static String getXMLPart( String text, String xmlTag )
  {
    if( text == null )
      return null;
    if( xmlTag == null )
      return null;
    int pos1 = text.indexOf( xmlTag );
    if( pos1 < 0 )
      return null;
    int pos2 = text.indexOf( xmlTag + ">", pos1 + 1 );
    if( pos2 < 0 )
      return null;
    int bPos1 = getBefore( text, "<", pos1 );
    if( bPos1 < 0 )
      return null;

    int bPos2 = getAfter( text, ">", pos2 );
    if( bPos2 < 0 )
      return null;

    final String space = text.substring( bPos1 + 1, pos1 );
    if( isValid( space ) )
    {
      final String result = text.substring( bPos1, bPos2 + 1 );
      return result;
    }
    return null;
  }

  private static boolean isValid( String space )
  {
    return !space.matches( ".*(<|>).*" );
  }

  private static int getAfter( String text, String pattern, int pos )
  {
    for( int result = pos; result <= text.length(); result++ )
    {
      final String substring = text.substring( result );
      if( substring.startsWith( pattern ) )
        return result;
    }
    return -1;
  }

  private static int getBefore( String text, String pattern, int pos )
  {
    for( int result = pos; result >= 0; result-- )
    {
      final String substring = text.substring( result );
      if( substring.startsWith( pattern ) )
        return result;
    }
    return -1;
  }
}
