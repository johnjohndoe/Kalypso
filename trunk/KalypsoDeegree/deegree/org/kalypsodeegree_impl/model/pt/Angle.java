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
package org.kalypsodeegree_impl.model.pt;

// Miscellaneous
import java.io.Serializable;
import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.text.Format;
import java.text.ParseException;
import java.util.Locale;

import org.kalypsodeegree_impl.model.resources.ClassChanger;

/**
 * An angle in degrees. An angle is the amount of rotation needed to bring one line or plane into coincidence with
 * another, generally measured in degrees, sexagesimal degrees or grads.
 * 
 * @version 1.0
 * @author Martin Desruisseaux
 * 
 * @see Latitude
 * @see Longitude
 * @see AngleFormat
 */
public class Angle implements Comparable, Serializable
{
  /**
   * Serial number for interoperability with different versions.
   */
  private static final long serialVersionUID = 1158747349433104534L;

  /**
   * A shared instance of {@link AngleFormat}.
   */
  private static Reference format;

  /**
   * Define how angle can be converted to {@link Number}objects.
   */
  static
  {
    ClassChanger.register( new ClassChanger( Angle.class, Double.class )
    {
      protected Number convert( final Comparable o )
      {
        return new Double( ( (Angle)o ).theta );
      }

      protected Comparable inverseConvert( final Number value )
      {
        return new Angle( value.doubleValue() );
      }
    } );
  }

  /**
   * Angle value in degres.
   */
  private final double theta;

  /**
   * Contruct a new angle with the specified value.
   * 
   * @param theta
   *          Angle in degrees.
   */
  public Angle( final double theta )
  {
    this.theta = theta;
  }

  /**
   * Constructs a newly allocated <code>Angle</code> object that represents the angle value represented by the string.
   * The string should represents an angle in either fractional degrees (e.g. 45.5�) or degrees with minutes and seconds
   * (e.g. 45�30').
   * 
   * @param string
   *          A string to be converted to an <code>Angle</code>.
   * @throws NumberFormatException
   *           if the string does not contain a parsable angle.
   */
  public Angle( final String string ) throws NumberFormatException
  {
    try
    {
      final Angle theta = (Angle)getAngleFormat().parseObject( string );
      if( getClass().isAssignableFrom( theta.getClass() ) )
      {
        this.theta = theta.theta;
      }
      else
        throw new NumberFormatException();
    }
    catch( ParseException exception )
    {
      NumberFormatException e = new NumberFormatException( exception.getLocalizedMessage() );
      e.initCause( exception );
      throw e;
    }
  }

  /**
   * Returns the angle value in degrees.
   */
  public double degrees()
  {
    return theta;
  }

  /**
   * Returns the angle value in radians.
   */
  public double radians()
  {
    return Math.toRadians( theta );
  }

  /**
   * Returns a hash code for this <code>Angle</code> object.
   */
  public int hashCode()
  {
    final long code = Double.doubleToLongBits( theta );
    return (int)code ^ (int)( code >>> 32 );
  }

  /**
   * Compares the specified object with this angle for equality.
   */
  public boolean equals( final Object that )
  {
    if( that == this )
      return true;
    if( that != null && getClass().equals( that.getClass() ) )
    {
      return Double.doubleToLongBits( theta ) == Double.doubleToLongBits( ( (Angle)that ).theta );
    }
    else
      return false;
  }

  /**
   * Compares two <code>Angle</code> objects numerically. The comparaison is done as if by the
   * {@link Double#compare(double,double)}method.
   */
  public int compareTo( final Object that )
  {
    /*
     * //----- BEGIN JDK 1.4 DEPENDENCIES ---- return Double.compare(this.theta, ((Angle)that).theta); ----- END OF JDK
     * 1.4 DEPENDENCIES ---
     */
    final double d1 = this.theta;
    final double d2 = ( (Angle)that ).theta;
    if( d1 < d2 )
      return -1;
    if( d1 > d2 )
      return +1;
    if( d1 == d2 )
      return 0;
    final long bits1 = Double.doubleToLongBits( d1 );
    final long bits2 = Double.doubleToLongBits( d2 );
    if( bits1 < bits2 )
      return -1; // (-0.0, 0.0) or (!NaN, NaN)
    if( bits1 > bits2 )
      return +1; // (0.0, -0.0) or (NaN, !NaN)
    return 0;
    //------- END OF JDK 1.3 FALLBACK -------*/

  }

  /**
   * Returns a string representation of this <code>Angle</code> object.
   */
  public String toString()
  {
    return getAngleFormat().format( this, new StringBuffer(), null ).toString();
  }

  /**
   * Returns a shared instance of {@link AngleFormat}. The return type is {@link Format}in order to avoid class
   * loading before necessary.
   */
  private static synchronized Format getAngleFormat()
  {
    if( format != null )
    {
      final Format angleFormat = (Format)format.get();
      if( angleFormat != null )
        return angleFormat;
    }
    final Format newFormat = new AngleFormat( "D�MM.m'", Locale.US );
    format = new SoftReference( newFormat );
    return newFormat;
  }
}