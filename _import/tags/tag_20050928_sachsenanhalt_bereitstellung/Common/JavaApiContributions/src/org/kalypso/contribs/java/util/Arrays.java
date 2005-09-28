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
package org.kalypso.contribs.java.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

/**
 * utility stuff for arrays
 * 
 * @author schlienger
 */
public final class Arrays
{
  /**
   * Return a list containing the elements of the array.
   */
  public static List asList( final Object[] array )
  {
    final Vector v = new Vector( array.length );
    for( int i = 0; i < array.length; i++ )
      v.add( array[i] );

    return v;
  }

  /**
   * Add all elementsToAdd to the given collection.
   */
  public static void addAll( final Collection c, final Object[] elementsToAdd )
  {
    c.addAll( Arrays.asList( elementsToAdd ) );
  }

  /**
   * Add all object from source of the class desiredClass to the dest collection. Either instances of the desired class
   * or subclasses will be accepted.
   * 
   * @param source
   *          object from this source will be added to dest if they are instances of the desiredclass
   * @param dest
   *          destination
   * @param desiredclass
   *          instances of this class or subclasses of it will be added to dest
   */
  public static void addAllOfClass( final Collection source, final Collection dest, final Class desiredclass )
  {
    for( final Iterator it = source.iterator(); it.hasNext(); )
    {
      final Object obj = it.next();
      
      if( desiredclass.isAssignableFrom( obj.getClass() ) )
        dest.add( obj );        
    }
  }

  /**
   * creates an array of raw ints with a given array of Integer Objects by copying the int values.
   * 
   * @throws IllegalArgumentException
   *           if argument is null
   */
  public static int[] rawIntegers( Integer[] objDs )
  {
    if( objDs == null )
      throw new IllegalArgumentException( "Integer array is null" );

    int[] rawDs = new int[objDs.length];

    for( int i = 0; i < objDs.length; i++ )
      rawDs[i] = objDs[i].intValue();

    return rawDs;
  }

  /**
   * creates an array of raw doubles with a given array of Double Objects by copying the double values
   * 
   * @throws IllegalArgumentException
   *           if argument is null
   */
  public static double[] rawDoubles( Double[] objDs )
  {
    if( objDs == null )
      throw new IllegalArgumentException( "Double array is null" );

    double[] rawDs = new double[objDs.length];

    for( int i = 0; i < objDs.length; i++ )
      rawDs[i] = objDs[i].doubleValue();

    return rawDs;
  }

  /**
   * creates an array of object doubles with a given array of doubles by copying the double values
   * 
   * @throws IllegalArgumentException
   *           if argument is null
   */
  public static Double[] objectDoubles( double[] rawDs )
  {
    if( rawDs == null )
      throw new IllegalArgumentException( "double array is null" );

    Double[] objDs = new Double[rawDs.length];

    for( int i = 0; i < rawDs.length; i++ )
      objDs[i] = new Double( rawDs[i] );

    return objDs;
  }

  /**
   * Convenient method to cast array into type. The code here is based on the code of Vector.toArray(Object[] type)
   * 
   * <pre>
   * Call example:  Double[] ds = Arrays.castArray(someArray, new Double[0]);
   * </pre>
   * 
   * <p>
   * NOTE: the type of the elements in someArray must be the same as the type of the elements of the desired array.
   * </p>
   */
  public static Object[] castArray( Object[] array, Object[] type )
  {
    if( type.length < array.length )
      type = (Object[])java.lang.reflect.Array.newInstance( type.getClass().getComponentType(), array.length );

    System.arraycopy( array, 0, type, 0, array.length );

    if( type.length > array.length )
      type[array.length] = null;

    return type;
  }

  /**
   * returns the index of the first minimum value found in ds. If ds is empty, returns -1.
   */
  public static int indexOfMin( double[] ds )
  {
    if( ds.length == 0 )
      return -1;

    int pos = 0;
    double min = ds[0];

    for( int i = 1; i < ds.length; i++ )
    {
      if( ds[i] < min )
      {
        min = ds[i];
        pos = i;
      }
    }

    return pos;
  }

  /**
   * returns the index of the first minimum value found in is. If is is empty, returns -1.
   */
  public static int indexOfMin( int[] is )
  {
    if( is.length == 0 )
      return -1;

    int pos = 0;
    int min = is[0];

    for( int i = 1; i < is.length; i++ )
    {
      if( is[i] < min )
      {
        min = is[i];
        pos = i;
      }
    }

    return pos;
  }

  /**
   * returns the index of the first maximum value found in is. If is is empty, returns -1.
   */
  public static int indexOfMax( int[] is )
  {
    if( is.length == 0 )
      return -1;

    int pos = 0;
    int max = is[0];

    for( int i = 1; i < is.length; i++ )
    {
      if( is[i] > max )
      {
        max = is[i];
        pos = i;
      }
    }

    return pos;
  }

  public static String dump( double[] ds )
  {
    StringBuffer buf = new StringBuffer();

    for( int i = 0; i < ( ds.length - 1 ); i++ )
      buf.append( ds[i] ).append( ", " );

    if( ds.length > 0 )
      buf.append( ds[ds.length - 1] );

    return buf.toString();
  }

  public static String dump( int[] is )
  {
    StringBuffer buf = new StringBuffer();

    for( int i = 0; i < ( is.length - 1 ); i++ )
      buf.append( is[i] ).append( ", " );

    if( is.length > 0 )
      buf.append( is[is.length - 1] );

    return buf.toString();
  }

  public static String dump( Object[] os )
  {
    StringBuffer buf = new StringBuffer();

    for( int i = 0; i < ( os.length - 1 ); i++ )
      buf.append( os[i] ).append( ", " );

    if( os.length > 0 )
      buf.append( os[os.length - 1] );

    return buf.toString();
  }

  /**
   * Creates a new truncated array with values that are smaller than the given value. The value itself is inserted into
   * the array if it is not already contained.
   * 
   * @param arr
   *          array to be truncated, must be sorted. Unpredictable results if not sorted
   * @param value
   *          the value that will serve as upper bound when truncating. It will be added into the resulting array if it
   *          is not already inside
   *  
   */
  public static double[] truncSmaller( double[] arr, double value )
  {
    int pos = java.util.Arrays.binarySearch( arr, value );

    if( pos < 0 )
    {
      pos = -pos;

      if( pos > arr.length )
      {
        double[] newArr = new double[arr.length + 1];
        System.arraycopy( arr, 0, newArr, 0, arr.length );
        newArr[pos - 1] = value;

        return newArr;
      }
      double[] newArr = new double[pos];
      System.arraycopy( arr, 0, newArr, 0, pos - 1 );
      newArr[pos - 1] = value;

      return newArr;
    }

    double[] newArr = new double[pos];
    System.arraycopy( arr, 0, newArr, 0, pos );

    return newArr;
  }

  /**
   * Creates a new truncated array with values that are bigger than the given value. The value itself is inserted into
   * the array if it is not already contained.
   * 
   * @param arr
   *          array to be truncated, must be sorted. Unpredictable results if not sorted
   * @param value
   *          the value that will serve as lower bound when truncating. It will be added into the resulting array if it
   *          is not already inside
   *  
   */
  public static double[] truncBigger( double[] arr, double value )
  {
    int pos = java.util.Arrays.binarySearch( arr, value );

    if( pos < 0 )
    {
      pos = -pos - 1;

      if( pos > arr.length )
      {
        return new double[]
        { value };
      }
      double[] newArr = new double[arr.length - pos + 1];
      System.arraycopy( arr, pos, newArr, 1, arr.length - pos );
      newArr[0] = value;

      return newArr;
    }

    int length = arr.length - pos;
    double[] newArr = new double[length];
    System.arraycopy( arr, pos, newArr, 0, length );

    return newArr;
  }

  /**
   * Merges the contents of the given array into one String, separating the elements with separator. You can restrict
   * the elements to fetch from the array by setting from and to.
   *  
   */
  public static String implode( String[] array, String separator, int from, int to )
  {
    StringBuffer buf = new StringBuffer();

    for( int i = from; i < to; i++ )
      buf.append( array[i] ).append( separator );

    buf.append( array[to] );

    return buf.toString();
  }

  /**
   * Removes a new array same as da but without duplicates, given the array da is sorted! This method looks
   * sequentially, checking if the elements at position i and i+1 are equal. <br>
   * If you call this method on a potentially unsorted array, it might not change anything.
   *  
   */
  public static double[] removeDupicates( double[] da, double delta )
  {
    DoubleComparator dc = new DoubleComparator( delta );

    int i = 0;

    double[] dtmp = new double[da.length];
    int itmp = 0;

    while( i < da.length )
    {
      while( ( i < ( da.length - 1 ) ) && ( dc.compare( da[i], da[i + 1] ) == 0 ) )
        i++;

      dtmp[itmp] = da[i];

      itmp++;
      i++;
    }

    double[] dres = new double[itmp];

    System.arraycopy( dtmp, 0, dres, 0, itmp );

    return dres;
  }

  /**
   * Creates a new Long-array and uses Long.valueOf() for each string in strArray.
   */
  public static Long[] toLong( final String[] strArray )
  {
    final Long[] longs = new Long[strArray.length];
    for( int i = 0; i < strArray.length; i++ )
      longs[i] = Long.valueOf( strArray[i] );

    return longs;
  }

  public static char[] tochar( final byte[] byteArray )
  {
    final char[] chars = new char[byteArray.length];
    for( int i = 0; i < byteArray.length; i++ )
      chars[i] = (char)byteArray[i];

    return chars;
  }

  /**
   * @deprecated You should use java.util.Arrays.sort() and doubleArray[ doubleArray.length - 1 ] to get the max
   */
  public static double findMax( double[] doubleArray )
  {
    double result = doubleArray[0];
    for( int i = 1; i < doubleArray.length; i++ )
    {
      if( doubleArray[i] > result )
        result = doubleArray[i];
    }
    return result;
  }

  /**
   * merges bytearray a and part of bytearray b
   * 
   * @param a
   * @param b
   * @param lengthB
   *          the length of b to merge
   */
  public static byte[] append( byte[] a, byte[] b, int lengthB )
  {
    final byte[] result = new byte[a.length + b.length];
    for( int i = 0; i < a.length; i++ )
      result[i] = a[i];
    for( int i = 0; i < lengthB; i++ )
      result[i + a.length] = b[i];
    return result;
  }

  /**
   * checks if bytearray a and bytearray b are equal till position length
   * 
   * @param a
   * @param b
   * @param length
   *          compare till this position
   */
  public static boolean equals( byte[] a, byte[] b, int length )
  {
    for( int i = 0; i < length; i++ )
    {
      if( a[i] != b[i] )
        return false;
    }
    return true;
  }

  /**
   * @param array
   *          the data
   * @param fromPos
   * @param toPos
   * 
   * @deprecated you should use System.arrayCopy instead
   */
  public static byte[] copyPart( byte[] array, int fromPos, int toPos )
  {
    final byte[] result = new byte[toPos - fromPos];
    for( int i = fromPos; i < toPos; i++ )
      result[i - fromPos] = array[i];
    return result;
  }

  /**
   * @param strings
   * @param separator
   * @return content as string
   */
  public static String toString( final String[] strings, final String separator )
  {
    final StringBuffer result = new StringBuffer();
    for( int i = 0; i < strings.length; i++ )
    {
      result.append( strings[i] );
      if( i + 1 < strings.length )
        result.append( separator );
    }
    return result.toString();
  }

  /**
   * Concatenate the two given arrays in a new one containing all of the elements.
   * 
   * @param array1
   *          first array to append
   * @param array2
   *          second array to append
   * @param type
   *          type of the array to create
   * 
   * @see List#toArray(java.lang.Object[]) for an example of the use of the type argument
   */
  public static Object[] concat( final Object[] array1, final Object[] array2, final Object[] type )
  {
    final ArrayList list = new ArrayList( array1.length + array2.length );
    list.addAll( java.util.Arrays.asList( array1 ) );
    list.addAll( java.util.Arrays.asList( array2 ) );

    return list.toArray( type );
  }

  /**
   * Return new array containing the given one with the given object appended at the end of it.
   * 
   * @param object
   *          object to append to array
   * @param type
   *          type of the array to create
   * 
   * @see List#toArray(java.lang.Object[]) for an example of the use of the type argument
   */
  public static Object[] concat( final Object[] array, final Object object, final Object[] type )
  {
    final ArrayList list = new ArrayList( array.length + 1 );
    list.addAll( java.util.Arrays.asList( array ) );
    list.add( object );

    return list.toArray( type );
  }
}
