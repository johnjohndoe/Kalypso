/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de

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

 Andreas Poth
 lat/lon Fitzke/Fretter/Poth GbR
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de

 Jens Fitzke
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: jens.fitzke@uni-bonn.de

 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.tools;

import java.util.ArrayList;
import java.util.StringTokenizer;

/**
 * this is a collection of some methods that extends the functionallity of the
 * sun-java string class.
 */
public class StringExtend
{
  /**
   * replaces occurences of a string fragment within a string by a new string.
   * 
   * @param target
   *          is the original string
   * @param from
   *          is the string to be replaced
   * @param to
   *          is the string which will used to replace
   * @param all
   *          if it's true all occurences of the string to be replaced will be
   *          replaced. else only the first occurence will be replaced.
   */
  public static String replace( String target, String from, String to, boolean all )
  {
    int start = target.indexOf( from );

    if( start == -1 )
    {
      return target;
    }

    int lf = from.length();
    char[] targetChars = target.toCharArray();
    StringBuffer buffer = new StringBuffer( target.length() );
    int copyFrom = 0;

    while( start != -1 )
    {
      buffer.append( targetChars, copyFrom, start - copyFrom );
      buffer.append( to );
      copyFrom = start + lf;
      start = target.indexOf( from, copyFrom );

      if( !all )
      {
        start = -1;
      }
    }

    buffer.append( targetChars, copyFrom, targetChars.length - copyFrom );

    return buffer.toString();
  }

  /**
   * replaces all special german letters (umlaute)
   * 
   * draft version
   */
  public static String HTMLEncode( String source )
  {
    String target = null;

    target = StringExtend.replace( source, "ä", "&auml;", true );
    target = StringExtend.replace( target, "Ä", "&Auml;", true );
    target = StringExtend.replace( target, "ö", "&ouml;", true );
    target = StringExtend.replace( target, "Ö", "&Ouml;", true );
    target = StringExtend.replace( target, "ü", "&uuml;", true );
    target = StringExtend.replace( target, "Ü", "&Uuml;", true );
    target = StringExtend.replace( target, "ß", "&szlig;", true );

    return target;
  }

  /**
   * parse a string and return its tokens as array
   * 
   * @param s
   *          string to parse
   * @param delimiter
   *          delimiter that marks the end of a token
   * @param deleteDoubles
   *          if it's true all string that are already within the resulting
   *          array will be deleted, so that there will only be one copy of
   *          them.
   */
  public static String[] toArray( String s, String delimiter, boolean deleteDoubles )
  {
    if( s == null )
    {
      return null;
    }

    if( s.equals( "" ) )
    {
      return null;
    }

    StringTokenizer st = new StringTokenizer( s, delimiter );

    ArrayList vec = new ArrayList();

    for( int i = 0; st.hasMoreTokens(); i++ )
    {
      String t = st.nextToken();

      if( ( t != null ) && ( t.length() > 0 ) )
      {
        vec.add( t.trim() );
      }
    }

    // no value selected
    if( vec.size() == 0 )
    {
      return null;
    }

    String[] kw = (String[])vec.toArray( new String[vec.size()] );

    if( deleteDoubles )
    {
      kw = deleteDoubles( kw );
    }

    return kw;
  }

  /**
   * transforms a string array to one string. the array fields are seperated by
   * the submitted delimiter:
   * 
   * @param s
   *          stringarray to transform
   * @param delimiter
   */
  public static String arrayToString( String[] s, char delimiter )
  {
    String res = "";

    for( int i = 0; i < s.length; i++ )
    {
      res = res + s[i];

      if( i < ( s.length - 1 ) )
      {
        res = res + delimiter;
      }
    }

    return res;
  }

  /**
   * transforms a double array to one string. the array fields are seperated by
   * the submitted delimiter:
   * 
   * @param s
   *          stringarray to transform
   * @param delimiter
   */
  public static String arrayToString( double[] s, char delimiter )
  {
    String res = "";

    for( int i = 0; i < s.length; i++ )
    {
      res = res + s[i];

      if( i < ( s.length - 1 ) )
      {
        res = res + delimiter;
      }
    }

    return res;
  }

  /**
   * transforms a int array to one string. the array fields are seperated by the
   * submitted delimiter:
   * 
   * @param s
   *          stringarray to transform
   * @param delimiter
   */
  public static String arrayToString( int[] s, char delimiter )
  {
    String res = "";

    for( int i = 0; i < s.length; i++ )
    {
      res = res + s[i];

      if( i < ( s.length - 1 ) )
      {
        res = res + delimiter;
      }
    }

    return res;
  }

  /**
   * clears the begin and end of a string from the strings sumitted
   * 
   * @param s
   *          string to validate
   * @param mark
   *          string to remove from begin and end of <code>s</code>
   */
  public static String validateString( String s, String mark )
  {
    if( s == null )
    {
      return null;
    }

    if( s.length() == 0 )
    {
      return s;
    }

    s = s.trim();

    while( s.startsWith( mark ) )
    {
      s = s.substring( mark.length(), s.length() ).trim();
    }

    while( s.endsWith( mark ) )
    {
      s = s.substring( 0, s.length() - mark.length() ).trim();
    }

    return s;
  }

  /**
   * deletes all double entries from the submitted array
   */
  public static String[] deleteDoubles( String[] s )
  {
    ArrayList vec = new ArrayList();

    for( int i = 0; i < s.length; i++ )
    {
      if( !vec.contains( s[i] ) )
      {
        vec.add( s[i] );
      }
    }

    return (String[])vec.toArray( new String[vec.size()] );
  }

  /**
   * removes all fields from the array that equals <code>s</code>
   * 
   * @param target
   *          array where to remove the submitted string
   * @param s
   *          string to remove
   */
  public static String[] removeFromArray( String[] target, String s )
  {
    ArrayList vec = new ArrayList();

    for( int i = 0; i < target.length; i++ )
    {
      if( !target[i].equals( s ) )
      {
        vec.add( target[i] );
      }
    }

    return (String[])vec.toArray( new String[vec.size()] );
  }

  /**
   * checks if the submitted array contains the string <code>value</code>
   * 
   * @param target
   *          array to check if it contains <code>value</code>
   * @param value
   *          string to check if it within the array
   */
  public static boolean contains( String[] target, String value )
  {
    if( target == null )
    {
      return false;
    }

    if( value == null )
    {
      return false;
    }

    if( value.endsWith( "," ) )
    {
      value = value.substring( 0, value.length() - 1 );
    }

    for( int i = 0; i < target.length; i++ )
    {
      if( value.equalsIgnoreCase( target[i] ) )
      {
        return true;
      }
    }

    return false;
  }

  /**
   * countString count the occurrences of token into target
   * 
   * @param target
   * @param token
   * 
   * @return
   */
  public static int countString( String target, String token )
  {
    int start = target.indexOf( token );
    int count = 0;

    while( start != -1 )
    {
      count++;
      start = target.indexOf( token, start + 1 );
    }

    return count;
  }

  /**
   * Extract all the string that begin with "start" and end with "end" and store
   * it into an array of String
   * 
   * @param target
   * @param startString
   * @param end
   * 
   * @return
   */
  public static String[] extractString( String target, String startString, String end )
  {
    int start = target.indexOf( startString );

    if( start == -1 )
    {
      return null;
    }

    int count = countString( target, startString );
    String[] subString = new String[count];

    for( int i = 0; i < count; i++ )
    {
      subString[i] = target.substring( start, target.indexOf( end, start + 1 ) + 1 );
      start = target.indexOf( startString, start + 1 );
    }

    return subString;
  }

  /**
   * extract a string contained between startDel and endDel, you can remove t he
   * delimiters if set true the parameters delStart and delEnd
   * 
   * @param target
   * @param startDel
   * @param endDel
   * @param delStart
   * @param delEnd
   * 
   * @return
   */
  public static String extractArray( String target, String startDel, String endDel,
      boolean delStart, boolean delEnd )
  {
    int start = target.indexOf( startDel );

    if( start == -1 )
    {
      return null;
    }

    String s = target.substring( start, target.indexOf( endDel, start + 1 ) + 1 );

    s = s.trim();

    if( delStart )
    {
      while( s.startsWith( startDel ) )
      {
        s = s.substring( startDel.length(), s.length() ).trim();
      }
    }

    if( delEnd )
    {
      while( s.endsWith( endDel ) )
      {
        s = s.substring( 0, s.length() - endDel.length() ).trim();
      }
    }

    return s;
  }

  /**
   * convert the array of string like [(x1,y1),(x2,y2)...] into an array of
   * double [x1,y1,x2,y2...]
   * 
   * @param s
   * @param delimiter
   * 
   * @return
   */
  public static double[] toArrayDouble( String s, String delimiter )
  {
    if( s == null )
    {
      return null;
    }

    if( s.equals( "" ) )
    {
      return null;
    }

    StringTokenizer st = new StringTokenizer( s, delimiter );

    ArrayList vec = new ArrayList( st.countTokens() );

    for( int i = 0; st.hasMoreTokens(); i++ )
    {
      String t = st.nextToken();

      if( ( t != null ) && ( t.length() > 0 ) )
      {
        vec.add( t.trim() );
      }
    }

    double[] array = new double[vec.size()];

    for( int i = 0; i < vec.size(); i++ )
    {
      array[i] = Double.parseDouble( (String)vec.get( i ) );
    }

    return array;
  }

  /**
   * transforms an array of StackTraceElements into a String
   */
  public static String stackTraceToString( StackTraceElement[] se )
  {

    StringBuffer sb = new StringBuffer();
    for( int i = 0; i < se.length; i++ )
    {
      sb.append( se[i].getClassName() + " " );
      sb.append( se[i].getFileName() + " " );
      sb.append( se[i].getMethodName() + "(" );
      sb.append( se[i].getLineNumber() + ")\n" );
    }
    return sb.toString();
  }
}