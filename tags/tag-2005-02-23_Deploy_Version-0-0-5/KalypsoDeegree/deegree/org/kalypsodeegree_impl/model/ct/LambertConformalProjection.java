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
package org.deegree_impl.model.ct;

// OpenGIS (SEAS) dependencies
import java.awt.geom.Point2D;
import java.util.Locale;

import org.deegree_impl.model.cs.Projection;
import org.deegree_impl.model.pt.Latitude;
import org.deegree_impl.model.resources.css.ResourceKeys;
import org.deegree_impl.model.resources.css.Resources;

/**
 * Projection conique conforme de Lambert. Les aires et les formes sont
 * d�form�es � mesure que l'on s'�loigne de parall�les standards. Les angles
 * sont vrais dans une r�gion limit�e. Cette projection est utilis�e pour les
 * cartes de l'Am�rique du Nord. Elle utilise par d�faut une latitude centrale
 * de 40�N. <br>
 * <br>
 * 
 * R�f�rence: John P. Snyder (Map Projections - A Working Manual, U.S.
 * Geological Survey Professional Paper 1395, 1987)
 * 
 * @version 1.0
 * @author Andr� Gosselin
 * @author Martin Desruisseaux
 */
final class LambertConformalProjection extends ConicProjection
{
  /**
   * Standards parallels in radians, for {@link #toString}implementation.
   */
  private final double phi1, phi2;

  /**
   * Variables internes pour les calculs.
   */
  private final double n, F, rho0;

  /**
   * Construct a new map projection from the suplied parameters.
   * 
   * @param parameters
   *          The parameter values in standard units.
   * @throws MissingParameterException
   *           if a mandatory parameter is missing.
   */
  protected LambertConformalProjection( final Projection parameters )
      throws MissingParameterException
  {
    //////////////////////////
    //   Fetch parameters //
    //////////////////////////
    super( parameters );
    phi1 = latitudeToRadians( parameters.getValue( "standard_parallel1", 30 ), true );
    phi2 = latitudeToRadians( parameters.getValue( "standard_parallel2", 45 ), true );

    //////////////////////////
    //  Compute constants //
    //////////////////////////
    if( Math.abs( phi1 + phi2 ) < EPS )
    {
      throw new IllegalArgumentException( Resources.format(
          ResourceKeys.ERROR_ANTIPODE_LATITUDES_$2, new Latitude( Math.toDegrees( phi1 ) ),
          new Latitude( Math.toDegrees( phi2 ) ) ) );
    }
    final double cosphi = Math.cos( phi1 );
    final double sinphi = Math.sin( phi1 );
    final boolean secant = Math.abs( phi1 - phi2 ) > EPS;
    if( isSpherical )
    {
      if( secant )
      {
        n = Math.log( cosphi / Math.cos( phi2 ) )
            / Math.log( Math.tan( ( Math.PI / 4 ) + 0.5 * phi2 )
                / Math.tan( ( Math.PI / 4 ) + 0.5 * phi1 ) );
      }
      else
        n = sinphi;
      F = cosphi * Math.pow( Math.tan( ( Math.PI / 4 ) + 0.5 * phi1 ), n ) / n;
      if( Math.abs( Math.abs( centralLatitude ) - ( Math.PI / 2 ) ) >= EPS )
      {
        rho0 = F * Math.pow( Math.tan( ( Math.PI / 4 ) + 0.5 * centralLatitude ), -n );
      }
      else
        rho0 = 0.0;
    }
    else
    {
      final double m1 = msfn( sinphi, cosphi );
      final double t1 = tsfn( phi1, sinphi );
      if( secant )
      {
        final double sinphi2 = Math.sin( phi2 );
        final double m2 = msfn( sinphi2, Math.cos( phi2 ) );
        final double t2 = tsfn( phi2, sinphi2 );
        n = Math.log( m1 / m2 ) / Math.log( t1 / t2 );
      }
      else
        n = sinphi;
      F = m1 * Math.pow( t1, -n ) / n;
      if( Math.abs( Math.abs( centralLatitude ) - ( Math.PI / 2 ) ) >= EPS )
      {
        rho0 = F * Math.pow( tsfn( centralLatitude, Math.sin( centralLatitude ) ), n );
      }
      else
        rho0 = 0.0;
    }
  }

  /**
   * Returns a human readable name localized for the specified locale.
   */
  public String getName( final Locale locale )
  {
    return Resources.getResources( locale ).getString( ResourceKeys.LAMBERT_CONFORMAL_PROJECTION );
  }

  /**
   * Transforms the specified ( <var>x </var>, <var>y </var>) coordinate and
   * stores the result in <code>ptDst</code>.
   */
  protected Point2D transform( double x, double y, final Point2D ptDst ) throws TransformException
  {
    x -= centralMeridian;
    double rho;
    if( Math.abs( Math.abs( y ) - ( Math.PI / 2 ) ) < EPS )
    {
      if( y * n <= 0 )
      {
        throw new TransformException( Resources.format( ResourceKeys.ERROR_POLE_PROJECTION_$1,
            new Latitude( Math.toDegrees( y ) ) ) );
      }
      else
        rho = 0;
    }
    else
    {
      if( isSpherical )
        rho = Math.pow( Math.tan( ( Math.PI / 4d ) + 0.5 * y ), -n );
      else
        rho = Math.pow( tsfn( y, Math.sin( y ) ), n );
      rho *= F;
    }

    x = n * x;
    y = a * ( rho0 - rho * Math.cos( x ) ) + false_northing;
    x = a * ( rho * Math.sin( x ) ) + false_easting;

    if( ptDst != null )
    {
      ptDst.setLocation( x, y );
      return ptDst;
    }
    else
      return new Point2D.Double( x, y );
  }

  /**
   * Transforms the specified ( <var>x </var>, <var>y </var>) coordinate and
   * stores the result in <code>ptDst</code>.
   */
  protected Point2D inverseTransform( double x, double y, final Point2D ptDst )
      throws TransformException
  {
    x -= false_easting;
    y -= false_northing;
    x /= a;
    y = rho0 - y / a;
    double rho = Math.sqrt( x * x + y * y );
    if( rho > EPS )
    {
      if( n < 0 )
      {
        rho = -rho;
        x = -x;
        y = -y;
      }
      x = centralMeridian + Math.atan2( x, y ) / n;
      if( isSpherical )
      {
        y = 2.0 * Math.atan( Math.pow( F / rho, 1.0 / n ) ) - ( Math.PI / 2 );
      }
      else
      {
        y = cphi2( Math.pow( rho / F, 1.0 / n ) );
      }
    }
    else
    {
      x = centralMeridian;
      y = n < 0 ? -( Math.PI / 2 ) : ( Math.PI / 2 );
    }
    if( ptDst != null )
    {
      ptDst.setLocation( x, y );
      return ptDst;
    }
    else
      return new Point2D.Double( x, y );
  }

  /**
   * Returns a hash value for this projection.
   */
  public int hashCode()
  {
    final long code = Double.doubleToLongBits( F );
    return ( (int)code ^ (int)( code >>> 32 ) ) + 37 * super.hashCode();
  }

  /**
   * Compares the specified object with this map projection for equality.
   */
  public boolean equals( final Object object )
  {
    if( object == this )
      return true; // Slight optimization
    if( super.equals( object ) )
    {
      final LambertConformalProjection that = (LambertConformalProjection)object;
      return Double.doubleToLongBits( this.n ) == Double.doubleToLongBits( that.n )
          && Double.doubleToLongBits( this.F ) == Double.doubleToLongBits( that.F )
          && Double.doubleToLongBits( this.rho0 ) == Double.doubleToLongBits( that.rho0 );
    }
    return false;
  }

  /**
   * Impl�mentation de la partie entre crochets de la cha�ne retourn�e par
   * {@link #toString()}.
   */
  void toString( final StringBuffer buffer )
  {
    super.toString( buffer );
    addParameter( buffer, "standard_parallel1", Math.toDegrees( phi1 ) );
    addParameter( buffer, "standard_parallel2", Math.toDegrees( phi2 ) );
  }

  /**
   * Informations about a {@link LambertConformalProjection}.
   * 
   * @version 1.0
   * @author Martin Desruisseaux
   */
  static final class Provider extends MapProjection.Provider
  {
    /**
     * Construct a new provider.
     */
    public Provider()
    {
      super( "Lambert_Conformal_Conic_2SP", ResourceKeys.LAMBERT_CONFORMAL_PROJECTION );
      put( "standard_parallel1", 30.0, LATITUDE_RANGE );
      put( "standard_parallel2", 45.0, LATITUDE_RANGE );
    }

    /**
     * Create a new map projection.
     */
    protected Object create( final Projection parameters )
    {
      return new LambertConformalProjection( parameters );
    }
  }
}