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
package org.kalypsodeegree_impl.model.resources;

// Miscellaneous

/**
 * Simple mathematical functions. Some of those function will be removed if JavaSoft provide a standard implementation
 * or fix some issues in Bug Parade: <br>
 * <ul>
 * <li><a href="http://developer.java.sun.com/developer/bugParade/bugs/4074599.html">Implement log10 (base 10
 * logarithm) </a></li>
 * <li><a href="http://developer.java.sun.com/developer/bugParade/bugs/4358794.html">implement pow10 (power of 10) with
 * optimization for integer powers </a>/li>
 * <li><a href="http://developer.java.sun.com/developer/bugParade/bugs/4461243.html">Math.acos is very slow </a></li>
 * </ul>
 * 
 * @version 1.0
 * @author Martin Desruisseaux
 */
public final class XMath
{
  /**
   * Natural logarithm of 10. Approximatively equals to 2.302585.
   */
  public static final double LN10 = 2.3025850929940456840179914546844;

  /**
   * Table of some integer powers of 10. Used for fast computation of {@link #pow10(int)}.
   */
  private static final double[] POW10 =
  {
      1E+00,
      1E+01,
      1E+02,
      1E+03,
      1E+04,
      1E+05,
      1E+06,
      1E+07,
      1E+08,
      1E+09,
      1E+10,
      1E+11,
      1E+12,
      1E+13,
      1E+14,
      1E+15,
      1E+16,
      1E+17,
      1E+18,
      1E+19,
      1E+20,
      1E+21,
      1E+22,
      1E+23 };

  /**
   * Do not allow instantiation of this class.
   */
  private XMath()
  {}

  /**
   * Compute the hypothenuse (<code>sqrt(x²+y²)</code>).
   */
  public static double hypot( double x, double y )
  {
    return Math.sqrt( x * x + y * y );
  }

  /**
   * Compute the logarithm in base 10. See http://developer.java.sun.com/developer/bugParade/bugs/4074599.html.
   */
  public static double log10( double x )
  {
    return Math.log( x ) / LN10;
  }

  /**
   * Compute 10 power <var>x </var>.
   */
  public static double pow10( double x )
  {
    final int ix = (int)x;
    if( ix == x )
      return pow10( ix );
    else
      return Math.pow( 10.0, x );
  }

  /**
   * Compute 10 power <var>x </var>. This computation is very fast for small power of 10 and avoir some rounding error
   * issue (see http://developer.java.sun.com/developer/bugParade/bugs/4358794.html).
   */
  public static double pow10( final int x )
  {
    if( x >= 0 )
    {
      if( x < POW10.length )
        return POW10[x];
    }
    else if( x != Integer.MIN_VALUE )
    {
      final int nx = -x;
      if( nx < POW10.length )
        return 1 / POW10[nx];
    }
    try
    {
      /*
       * Note: Method 'Math.pow(10,x)' has rounding errors: it doesn't always returns the closest IEEE floating point
       * representation. Method 'Double.parseDouble("1E"+x)' gives as good or better numbers for ALL integer powers, but
       * is much slower. The difference is usually negligible, but powers of 10 is a special case since it is often used
       * for scaling axis or formatting human-readable output. We hope that the current workaround is only temporary.
       * (see http://developer.java.sun.com/developer/bugParade/bugs/4358794.html).
       */
      return Double.parseDouble( "1E" + x );
    }
    catch( NumberFormatException exception )
    {
      return StrictMath.pow( 10, x );
    }
  }

  /**
   * Returns the sign of <var>x </var>. This method returns -1 if <var>x </var> is negative, 0 if <var>x </var> is null
   * or <code>NaN</code> and +1 if <var>x </var> is positive.
   */
  public static int sgn( double x )
  {
    if( x > 0 )
      return +1;
    if( x < 0 )
      return -1;
    else
      return 0;
  }

  /**
   * Returns the sign of <var>x </var>. This method returns -1 if <var>x </var> is negative, 0 if <var>x </var> is null
   * or <code>NaN</code> and +1 if <var>x </var> is positive.
   */
  public static int sgn( float x )
  {
    if( x > 0 )
      return +1;
    if( x < 0 )
      return -1;
    else
      return 0;
  }

  /**
   * Returns the sign of <var>x </var>. This method returns -1 if <var>x </var> is negative, 0 if <var>x </var> is null
   * and +1 if <var>x </var> is positive.
   */
  public static int sgn( long x )
  {
    if( x > 0 )
      return +1;
    if( x < 0 )
      return -1;
    else
      return 0;
  }

  /**
   * Returns the sign of <var>x </var>. This method returns -1 if <var>x </var> is negative, 0 if <var>x </var> is null
   * and +1 if <var>x </var> is positive.
   */
  public static int sgn( int x )
  {
    if( x > 0 )
      return +1;
    if( x < 0 )
      return -1;
    else
      return 0;
  }

  /**
   * Returns the sign of <var>x </var>. This method returns -1 if <var>x </var> is negative, 0 if <var>x </var> is null
   * and +1 if <var>x </var> is positive.
   */
  public static short sgn( short x )
  {
    if( x > 0 )
      return (short)+1;
    if( x < 0 )
      return (short)-1;
    else
      return (short)0;
  }

  /**
   * Returns the sign of <var>x </var>. This method returns -1 if <var>x </var> is negative, 0 if <var>x </var> is null
   * and +1 if <var>x </var> is positive.
   */
  public static byte sgn( byte x )
  {
    if( x > 0 )
      return (byte)+1;
    if( x < 0 )
      return (byte)-1;
    else
      return (byte)0;
  }
}