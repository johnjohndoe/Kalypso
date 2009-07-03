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
package org.kalypso.convert;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.deegree.model.feature.Feature;

/**
 * @author doemming
 */
public class ASCIIHelper
{
  public final static Pattern patternBrackets = Pattern.compile( "[\\(|\\)]" );
  public final static Pattern pFortranFormat = Pattern
  .compile( "(a|A|i|I|f|F)([0-9]*)\\.?([0-9]*)" );
  public final static Pattern pSpaceFormat = Pattern.compile( "_*" );
  public final static Pattern pPairFormat = Pattern.compile( "^.+,.+$" );
  public final static String textRegExp = "[a-zA-Z0-9_\\. ]";
  public final static String freeFormat = "\\s*[\\\\a-zA-Z0-9_:\\.-]+\\s*";
  public final static String decimalPoint = "[ \\.]";
  public final static String decimalValue = "[0-9 ]";

  public static String toAscii( String value, String format )
  {
    if( "*".equals( format ) || "a".equals( format ) || "A".equals( format ) )
      return value;
    Matcher m = ASCIIHelper.pFortranFormat.matcher( format );
    if( m.matches() )
    {
      String type = m.group( 1 );
      String charMax = m.group( 2 ); // gesamt anzahl stellen
      int max = Integer.parseInt( charMax );
      String decimalPlace = m.group( 3 ); // Nachkommastellen
      int decimal;
      StringBuffer result = new StringBuffer( "" );
      for( int i = 0; i < max; i++ )
        result.append( " " );
      if( value == null || "".equals( value ) )
        return result.toString();
      if( !"".equals( decimalPlace ) )
        decimal = Integer.parseInt( decimalPlace );
      else
      {
        if( "aA".indexOf( type ) >= 0 ) //TEXT
        {
          return ( result.replace( 0, value.length(), value ) ).toString();
        }
        decimal = 0;
      }
  
      boolean found = false;
      while( !found )
      {
        int pointPos = value.indexOf( '.' );
        if( pointPos < 0 && decimal > 0 ) // da fehlt ein Komma
        {
          value = value + ".0";
          continue;
        }
        if( pointPos < 0 && decimal == 0 )
        {
          found = true;
          continue;
        }
        int points = value.length() - pointPos - 1;
        if( points == decimal )
        {
          found = true;
          continue;
        }
  
        if( points > decimal ) // zuviele Nachkommastellen
        {
          value = value.substring( 0, value.length() - 1 );
          continue;
        }
        if( points < decimal ) // zuwenig Nachkommastellen
        {
          value = value + "0";
          continue;
        }
  
      }
  
      value = value.trim();
      return ( result.replace( max - value.length(), max, value ) ).toString();
    }
    throw new UnsupportedOperationException();
  
  }

  public static String toAsciiLine(Feature feature,String formatLine)
  {
    StringBuffer result = new StringBuffer( "" );
    String[] formats = patternBrackets.split( formatLine );
    for( int i = 0; i < formats.length; i++ )
    {
      String format = formats[i];
      Matcher m = pPairFormat.matcher( format );
      if( m.matches() )
        result.append( ASCIIHelper.toAsciiValue( feature, format ) );
      m = pSpaceFormat.matcher( format );
      if( m.matches() )
        result.append( format.replace( '_', ' ' ) );
    }
  
    return result.toString();
  }

  public static String toAsciiValue( Feature feature, String pairFormat )
  {
    if( "".equals( pairFormat ) )
      return "";
    final String[] s = pairFormat.split( "," );
    if( "todo".equals( s[0] ) )
      return "(TODO:" + s[0] + ")";
    if( "IGNORE".equals( s[0] ) )
      return "";
    //        System.out.println(s[0]);
    Object property = feature.getProperty( s[0] );
  
    if( property == null )
      return "(" + s[0] + "==NULL ?)";
    String value = property.toString(); // PropertyName
  
    final String format = s[1];
    return toAscii( value, format );
  }
  
}
