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

import java.awt.Color;
import java.awt.Font;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.Map.Entry;

/**
 * Utilities around the String class.
 * 
 * @author schlienger
 */
public final class StringUtilities
{
  /**
   * Not intended to be instanciated
   */
  private StringUtilities()
  {
    // empty
  }

  /**
   * Converts a String into a Color.
   * <p>
   * String has to have the following format: "R;G;B[;A]"
   * <p>
   * with R, G, B being the Red, Green, Blue components of the color
   * and expressed as integers in the range (0 - 255).
   * with A optional, being the alpha composite value in the range (0 - 255).
   *
   * @param s
   *
   * @throws IllegalArgumentException if s is null
   */
  public static final Color stringToColor( final String s ) throws IllegalArgumentException
  {
    if( s == null )
      throw new IllegalArgumentException( "Color String is null" );

    final String[] sc = s.split( ";" );

    if( sc.length == 3 )
      return new Color( Integer.parseInt( sc[0] ), Integer.parseInt( sc[1] ),
                      Integer.parseInt( sc[2] ) );
    
    if( sc.length == 4 )
      return new Color( Integer.parseInt( sc[0] ), Integer.parseInt( sc[1] ),
          Integer.parseInt( sc[2] ), Integer.parseInt( sc[3]) );
    
    throw new IllegalArgumentException( "Color String has wrong format: " + s );
  }

  /**
   * Converts a Color into a String.
   * <p>
   * String will have same format as specified in @link StringUtilities#stringToColor(String)
   *
   * @param c
   *
   * @throws IllegalArgumentException if color is null
   */
  public static final String colorToString( final Color c )
  {
    if( c == null )
      throw new IllegalArgumentException( "Color is null" );

    final StringBuffer buf = new StringBuffer(  );

    buf.append( c.getRed(  ) ).append( ";" ).append( c.getGreen(  ) )
       .append( ";" ).append( c.getBlue(  ) );

    // alpha component is optional
    if( c.getAlpha() != 255 )
      buf.append( ";" ).append( c.getAlpha() );
    
    return buf.toString(  );
  }

  /**
   * Converts a String to a Font.
   * <pre>
   *    FontName;FontStyle;FontSize
   * </pre>
   *
   * @param s
   *
   * @throws IllegalArgumentException if s is null
   */
  public static final Font stringToFont( final String s )
  {
    if( s == null )
      throw new IllegalArgumentException( "Font String is null" );

    final String[] sc = s.split( ";" );

    if( sc.length != 3 )
      throw new IllegalArgumentException( "Font String has wrong format" );

    final Font f = new Font( sc[0], Integer.parseInt( sc[1] ), Integer.parseInt( sc[2] ) );

    return f;
  }

  /**
   * Converts a font to a string. Format is defined in @link StringUtilities#stringToFont(String)
   *
   * @param f
   *
   * @throws IllegalArgumentException if f is null
   */
  public static final String fontToString( final Font f )
  {
    if( f == null )
      throw new IllegalArgumentException( "Font is null" );

    final StringBuffer buf = new StringBuffer(  );

    buf.append( f.getName(  ) ).append( ";" ).append( f.getStyle(  ) )
       .append( ";" ).append( f.getSize(  ) );

    return buf.toString(  );
  }

  /** 
   * Replacement per Pattern-Matching
   * 
   * @param sourceValue
   * @param replaceProperties
   * @return string
   */
  public static final String replaceAll( final String sourceValue, final Properties replaceProperties )
  {
    String newString = sourceValue;
    
    for( Iterator replaceIt = replaceProperties.entrySet().iterator(); replaceIt.hasNext(); )
    {
      final Map.Entry entry = (Entry)replaceIt.next();
      final String key = entry.getKey().toString();
      final String value = entry.getValue().toString();
      
      newString = newString.replaceAll( key, value );
    }
    
    return newString;
  }
}
