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
import org.deegree_impl.model.resources.css.ResourceKeys;
import org.deegree_impl.model.resources.css.Resources;

/**
 *  
 */
final class PlateCarreeProjection extends CylindricalProjection
{

  private double long0 = 0;

  private double lat0 = 0;

  private double e2 = 0;

  private double r = 0;

  /**
   * Construct a new map projection from the suplied parameters.
   * 
   * @param parameters
   *          The parameter values in standard units.
   * @throws MissingParameterException
   *           if a mandatory parameter is missing.
   */
  protected PlateCarreeProjection( final Projection parameters ) throws MissingParameterException
  {
    super( parameters );

    // calculate constants
    e2 = ( a * a - b * b ) / ( a * a );
    r = Math.sqrt( a * a * ( 1 - e2 ) );
  }

  /**
   * Returns a human readable name localized for the specified locale.
   */
  public String getName( final Locale locale )
  {
    return Resources.getResources( locale ).getString( ResourceKeys.PLATE_CARREE_PROJECTION );
  }

  /**
   * Transforms the specified ( <var>x </var>, <var>y </var>) coordinate and
   * stores the result in <code>ptDst</code>.
   */
  protected Point2D transform( double x, double y, final Point2D ptDst ) throws TransformException
  {
    /*
     * For the forward calculation:
     * 
     * X = R . (Long - longO) . cos(latO) Y = R . Lat
     * 
     * where R = SQRT[a^2 * (1-e^2)] and Lat and Long are expressed in radians.
     * 
     * For the reverse calculation:
     * 
     * Lat = Y / R Long = LongO + (X / R cos(latO))
     */
    x = r * ( x - long0 ) * Math.cos( lat0 );
    y = r * y;
    return new Point2D.Double( x, y );
  }

  /**
   * Transforms the specified ( <var>x </var>, <var>y </var>) coordinate and
   * stores the result in <code>ptDst</code>.
   */
  protected Point2D inverseTransform( double x, double y, final Point2D ptDst )
      throws TransformException
  {
    x = long0 + ( x / r * Math.cos( lat0 ) );
    y = y / r;
    return new Point2D.Double( x, y );
  }

  /**
   * Returns a hash value for this map projection.
   */
  public int hashCode()
  {
    final long code = Double.doubleToLongBits( 1234 );
    return ( (int)code ^ (int)( code >>> 32 ) ) + 37 * super.hashCode();
  }

  /**
   * Compares the specified object with this map projection for equality.
   */
  public boolean equals( final Object object )
  {
    return false;
  }

  /**
   * Implémentation de la partie entre crochets de la chaîne retournée par
   * {@link #toString()}.
   */
  void toString( final StringBuffer buffer )
  {
    super.toString( buffer );
  }

  /**
   * Informations about a {@link StereographicProjection}.
   * 
   * @version 1.0
   * @author Martin Desruisseaux
   */
  static final class Provider extends MapProjection.Provider
  {

    /**
     * Construct a new provider. The type (polar, oblique or equatorial) will be
     * choosen automatically according the latitude or origin.
     */
    public Provider()
    {
      super( "PlateCarree", ResourceKeys.PLATE_CARREE_PROJECTION );

    }

    /**
     * Create a new map projection.
     */
    protected Object create( final Projection parameters )
    {
      return new PlateCarreeProjection( parameters );
    }
  }
}