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
package org.kalypsodeegree_impl.model.ct;

// OpenGIS dependencies (SEAGIS)
import java.io.Serializable;

import javax.media.jai.ParameterList;

import org.kalypsodeegree_impl.model.cs.Ellipsoid;
import org.kalypsodeegree_impl.model.cs.HorizontalDatum;
import org.kalypsodeegree_impl.model.cs.WGS84ConversionInfo;
import org.kalypsodeegree_impl.model.resources.css.ResourceKeys;

/**
 * Transforms a three dimensional geographic points using abridged versions of
 * formulas derived by Molodenski.
 * 
 * @version 1.00
 * @author OpenGIS (www.opengis.org)
 * @author Martin Desruisseaux
 */
class AbridgedMolodenskiTransform extends AbstractMathTransform implements Serializable
{
  /**
   * Serial number for interoperability with different versions.
   */
  //private static final long serialVersionUID = ?;
  /**
   * X,Y,Z shift in meters
   */
  private final double dx, dy, dz;

  /**
   * Source equatorial radius in meters.
   */
  private final double a;

  /**
   * Source polar radius in meters
   */
  private final double b;

  /**
   * Source flattening factor.
   */
  private final double f;

  /**
   * Difference in the semi-major axes (a1 - a2) of the first and second
   * ellipsoids.
   */
  private final double da;

  /**
   * Difference in the flattening of the two ellipsoids.
   */
  private final double df;

  /**
   * Square of the eccentricity of the ellipsoid.
   */
  private final double e2;

  /**
   * Defined as <code>(a*df) + (f*da)</code>.
   */
  private final double adf;

  /**
   * Construct a transform.
   */
  protected AbridgedMolodenskiTransform( final HorizontalDatum source, final HorizontalDatum target )
  {
    final WGS84ConversionInfo srcInfo = source.getWGS84Parameters();
    final WGS84ConversionInfo tgtInfo = source.getWGS84Parameters();
    final Ellipsoid srcEllipsoid = source.getEllipsoid();
    final Ellipsoid tgtEllipsoid = target.getEllipsoid();
    dx = srcInfo.dx - tgtInfo.dx;
    dy = srcInfo.dy - tgtInfo.dy;
    dz = srcInfo.dz - tgtInfo.dz;
    a = srcEllipsoid.getSemiMajorAxis();
    b = srcEllipsoid.getSemiMinorAxis();
    f = 1 / srcEllipsoid.getInverseFlattening();
    da = a - tgtEllipsoid.getSemiMajorAxis();
    df = f - 1 / tgtEllipsoid.getInverseFlattening();
    e2 = 1 - ( b * b ) / ( a * a );
    adf = ( a * df ) + ( f * da );
  }

  /**
   * Transforms a list of coordinate point ordinal values.
   */
  public void transform( final double[] srcPts, int srcOff, final double[] dstPts, int dstOff,
      int numPts )
  {
    int step = 0;
    if( srcPts == dstPts && srcOff < dstOff && srcOff + numPts * getDimSource() > dstOff )
    {
      step = -getDimSource();
      srcOff -= ( numPts - 1 ) * step;
      dstOff -= ( numPts - 1 ) * step;
    }
    final double rho = Double.NaN; // TODO: Definition???
    while( --numPts >= 0 )
    {
      double x = Math.toRadians( srcPts[srcOff++] );
      double y = Math.toRadians( srcPts[srcOff++] );
      double z = srcPts[srcOff++];
      final double sinX = Math.sin( x );
      final double cosX = Math.cos( x );
      final double sinY = Math.sin( y );
      final double cosY = Math.cos( y );
      final double sin2Y = sinY * sinY;
      final double nu = a / Math.sqrt( 1 - e2 * sin2Y );

      // Note: Computation of 'x' and 'y' ommit the division by sin(1"),
      // because
      //       1/sin(1") / (60*60*180/PI) = 1.0000000000039174050898603898692...
      //       (60*60 is for converting the final result from seconds to
      // degrees,
      //       and 180/PI is for converting degrees to radians). This is an
      // error
      //       of about 8E-7 arc seconds, probably close to rounding errors
      // anyway.
      y += ( dz * cosY - sinY * ( dy * sinX + dx * cosX ) + adf * Math.sin( 2 * y ) ) / rho;
      x += ( dy * cosX - dx * sinX ) / ( nu * cosY );
      z += dx * cosY * cosX + dy * cosY * sinX + dz * sinY + adf * sin2Y - da;

      dstPts[dstOff++] = Math.toDegrees( x );
      dstPts[dstOff++] = Math.toDegrees( y );
      dstPts[dstOff++] = z;
      srcOff += step;
      dstOff += step;
    }
  }

  /**
   * Transforms a list of coordinate point ordinal values.
   */
  public void transform( final float[] srcPts, final int srcOff, final float[] dstPts,
      final int dstOff, int numPts )
  {
    // TODO: Copy the implementation from 'transform(double[]...)'.
    try
    {
      super.transform( srcPts, srcOff, dstPts, dstOff, numPts );
    }
    catch( TransformException exception )
    {
      // Should not happen.
    }
  }

  /**
   * Gets the dimension of input points, which is 3.
   */
  public int getDimSource()
  {
    return 3;
  }

  /**
   * Gets the dimension of output points, which is the same than
   * {@link #getDimSource()}.
   */
  public final int getDimTarget()
  {
    return getDimSource();
  }

  /**
   * Tests whether this transform does not move any points. This method returns
   * always <code>false</code>.
   */
  public final boolean isIdentity()
  {
    return false;
  }

  /**
   * Returns a hash value for this transform.
   */
  public final int hashCode()
  {
    final long code = Double.doubleToLongBits( dx )
        + 37
        * ( Double.doubleToLongBits( dy ) + 37 * ( Double.doubleToLongBits( dz ) + 37 * ( Double
            .doubleToLongBits( a ) + 37 * ( Double.doubleToLongBits( b ) + 37 * ( Double
            .doubleToLongBits( da ) + 37 * ( Double.doubleToLongBits( df ) ) ) ) ) ) );
    return (int)code ^ (int)( code >>> 32 );
  }

  /**
   * Compares the specified object with this math transform for equality.
   */
  public final boolean equals( final Object object )
  {
    if( object == this )
      return true; // Slight optimization
    if( super.equals( object ) )
    {
      final AbridgedMolodenskiTransform that = (AbridgedMolodenskiTransform)object;
      return Double.doubleToLongBits( this.dx ) == Double.doubleToLongBits( that.dx )
          && Double.doubleToLongBits( this.dy ) == Double.doubleToLongBits( that.dy )
          && Double.doubleToLongBits( this.dz ) == Double.doubleToLongBits( that.dz )
          && Double.doubleToLongBits( this.a ) == Double.doubleToLongBits( that.a )
          && Double.doubleToLongBits( this.b ) == Double.doubleToLongBits( that.b )
          && Double.doubleToLongBits( this.da ) == Double.doubleToLongBits( that.da )
          && Double.doubleToLongBits( this.df ) == Double.doubleToLongBits( that.df );
    }
    return false;
  }

  /**
   * Returns the WKT for this math transform.
   */
  public final String toString()
  {
    final StringBuffer buffer = paramMT( "Abridged_Molodenski" );
    addParameter( buffer, "dim", getDimSource() );
    addParameter( buffer, "dx", dx );
    addParameter( buffer, "dy", dy );
    addParameter( buffer, "src_semi_major", a );
    addParameter( buffer, "src_semi_minor", b );
    addParameter( buffer, "tgt_semi_major", a - da );
    //      addParameter(buffer, "tgt_semi_minor", b); // TODO
    buffer.append( ']' );
    return buffer.toString();
  }

  /**
   * The provider for {@link AbridgedMolodenskiTransform}.
   * 
   * @version 1.0
   * @author Martin Desruisseaux
   */
  static final class Provider extends MathTransformProvider
  {
    /**
     * Create a provider.
     */
    public Provider()
    {
      super( "Abridged_Molodenski", ResourceKeys.ABRIDGED_MOLODENSKI_TRANSFORM, null );
      put( "dim", Double.NaN, POSITIVE_RANGE );
      put( "dx", Double.NaN, null );
      put( "dy", Double.NaN, null );
      put( "src_semi_major", Double.NaN, POSITIVE_RANGE );
      put( "src_semi_minor", Double.NaN, POSITIVE_RANGE );
      put( "tgt_semi_major", Double.NaN, POSITIVE_RANGE );
      put( "tgt_semi_minor", Double.NaN, POSITIVE_RANGE );
    }

    /**
     * Returns a transform for the specified parameters.
     * 
     * @param parameters
     *          The parameter values in standard units.
     * @return A {@link MathTransform}object of this classification.
     */
    public MathTransform create( final ParameterList parameters )
    {
      return null;
    } // TODO
  }
}