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
package org.deegree_impl.model.geometry;

import java.io.Serializable;
import java.util.Arrays;

import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Exception;
import org.deegree.model.geometry.GM_GenericSurface;
import org.deegree.model.geometry.GM_Point;
import org.deegree.model.geometry.GM_Position;
import org.deegree.model.geometry.GM_SurfaceInterpolation;
import org.deegree.model.geometry.GM_SurfacePatch;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * default implementation of the GM_SurfacePatch interface from package
 * jago.model. the class is abstract because it should be specialized by derived
 * classes <code>GM_Polygon</code> for example
 * 
 * ------------------------------------------------------------
 * 
 * @version 11.6.2001
 * @author Andreas Poth
 */
abstract class GM_SurfacePatch_Impl implements GM_GenericSurface, Serializable
{
  /** Use serialVersionUID for interoperability. */
  private final static long serialVersionUID = 7641735268892225180L;

  protected CS_CoordinateSystem crs = null;

  protected GM_Envelope envelope = null;

  protected GM_Point centroid = null;

  protected GM_SurfaceInterpolation interpolation = null;

  protected GM_Position[] exteriorRing = null;

  protected GM_Position[][] interiorRings = null;

  protected double area = 0;

  protected boolean valid = false;

  /**
   * Creates a new GM_SurfacePatch_Impl object.
   * 
   * @param interpolation
   * @param exteriorRing
   * @param interiorRings
   * @param crs
   * 
   * @throws GM_Exception
   */
  protected GM_SurfacePatch_Impl( GM_SurfaceInterpolation interpolation,
      GM_Position[] exteriorRing, GM_Position[][] interiorRings, CS_CoordinateSystem crs )
      throws GM_Exception
  {
    this.crs = crs;

    if( ( exteriorRing == null ) || ( exteriorRing.length < 3 ) )
    {
      throw new GM_Exception( "The exterior ring doesn't contains enough point!" );
    }

    // check, if the exteriorRing of the polygon is closed
    // and if the interiorRings (if !=null) are closed
    if( !exteriorRing[0].equals( exteriorRing[exteriorRing.length - 1] ) )
    {
      System.out.println( exteriorRing[0] );
      System.out.println( exteriorRing[exteriorRing.length - 1] );
      throw new GM_Exception( "The exterior ring isn't closed!" );
    }

    if( interiorRings != null )
    {
      for( int i = 0; i < interiorRings.length; i++ )
      {
        if( !interiorRings[i][0].equals( interiorRings[i][interiorRings[i].length - 1] ) )
        {
          throw new GM_Exception( "The interior ring " + i + " isn't closed!" );
        }
      }
    }

    this.interpolation = interpolation;
    this.exteriorRing = exteriorRing;
    this.interiorRings = interiorRings;

    setValid( false );
  }

  /**
   * invalidates the calculated parameters of the GM_Object
   */
  protected void setValid( boolean valid )
  {
    this.valid = valid;
  }

  /**
   * returns true if the calculated parameters of the GM_Object are valid and
   * false if they must be recalculated
   */
  protected boolean isValid()
  {
    return valid;
  }

  /**
   *  
   */
  private void calculateEnvelope()
  {
    double[] min = (double[])exteriorRing[0].getAsArray().clone();
    double[] max = (double[])min.clone();

    for( int i = 1; i < exteriorRing.length; i++ )
    {
      double[] pos = exteriorRing[i].getAsArray();

      for( int j = 0; j < pos.length; j++ )
      {
        if( pos[j] < min[j] )
        {
          min[j] = pos[j];
        }
        else if( pos[j] > max[j] )
        {
          max[j] = pos[j];
        }
      }
    }

    envelope = new GM_Envelope_Impl( new GM_Position_Impl( min ), new GM_Position_Impl( max ) );
  }

  /**
   * The interpolation determines the surface interpolation mechanism used for
   * this GM_SurfacePatch. This mechanism uses the control points and control
   * parameters defined in the various subclasses to determine the position of
   * this GM_ SurfacePatch.
   */
  public GM_SurfaceInterpolation getInterpolation()
  {
    return interpolation;
  }

  /**
   * returns the bounding box of the surface patch
   */
  public GM_Envelope getEnvelope()
  {
    if( !isValid() )
    {
      calculateParam();
    }
    return envelope;
  }

  /**
   * returns a reference to the exterior ring of the surface
   */
  public GM_Position[] getExteriorRing()
  {
    return exteriorRing;
  }

  /**
   * returns a reference to the interior rings of the surface
   */
  public GM_Position[][] getInteriorRings()
  {
    return interiorRings;
  }

  /**
   * returns the length of all boundaries of the surface in a reference system
   * appropriate for measuring distances.
   */
  public double getPerimeter()
  {
    return -1;
  }

  /**
   * returns the coordinate system of the surface patch
   */
  public CS_CoordinateSystem getCoordinateSystem()
  {
    return crs;
  }

  /**
   * 
   * 
   * @param other
   * 
   * @return
   */
  public boolean equals( Object other )
  {
    if( ( other == null ) || !( other instanceof GM_SurfacePatch_Impl ) )
    {
      return false;
    }

    // Assuming Interpolation can be null (not checked by Constructor)
    if( getInterpolation() != null )
    {
      if( !getInterpolation().equals( ( (GM_SurfacePatch)other ).getInterpolation() ) )
      {
        return false;
      }
    }
    else
    {
      if( ( (GM_SurfacePatch)other ).getInterpolation() != null )
      {
        return false;
      }
    }

    // Assuming envelope cannot be null (always calculated)
    if( !envelope.equals( ( (GM_SurfacePatch)other ).getEnvelope() ) )
    {
      return false;
    }

    // Assuming exteriorRing cannot be null (checked by Constructor)
    if( !Arrays.equals( exteriorRing, ( (GM_SurfacePatch)other ).getExteriorRing() ) )
    {
      return false;
    }

    // Assuming either can have interiorRings set to null (not checked
    //by Constructor)
    if( interiorRings != null )
    {
      if( ( (GM_SurfacePatch)other ).getInteriorRings() == null )
      {
        return false;
      }

      if( interiorRings.length != ( (GM_SurfacePatch)other ).getInteriorRings().length )
      {
        return false;
      }

      for( int i = 0; i < interiorRings.length; i++ )
      {
        if( !Arrays.equals( interiorRings[i], ( (GM_SurfacePatch)other ).getInteriorRings()[i] ) )
        {
          return false;
        }
      }
    }
    else
    {
      if( ( (GM_SurfacePatch)other ).getInteriorRings() != null )
      {
        return false;
      }
    }

    return true;
  }

  /**
   * The operation "centroid" shall return the mathematical centroid for this
   * GM_Object. The result is not guaranteed to be on the object.
   */
  public GM_Point getCentroid()
  {
    if( !isValid() )
    {
      calculateParam();
    }
    return centroid;
  }

  /**
   * The operation "area" shall return the area of this GM_GenericSurface. The
   * area of a 2 dimensional geometric object shall be a numeric measure of its
   * surface area Since area is an accumulation (integral) of the product of two
   * distances, its return value shall be in a unit of measure appropriate for
   * measuring distances squared.
   */
  public double getArea()
  {
    if( !isValid() )
    {
      calculateParam();
    }
    return area;
  }

  /**
   * calculates the centroid and area of the surface patch. this method is only
   * valid for the two-dimensional case.
   */
  private void calculateCentroidArea()
  {
    GM_Position centroid_ = calculateCentroid( exteriorRing );
    double varea = calculateArea( exteriorRing );

    double x = centroid_.getX();
    double y = centroid_.getY();

    x *= varea;
    y *= varea;

    if( interiorRings != null )
    {
      for( int i = 0; i < interiorRings.length; i++ )
      {
        double dum = -1 * calculateArea( interiorRings[i] );
        GM_Position temp = calculateCentroid( interiorRings[i] );
        x += ( temp.getX() * dum );
        y += ( temp.getY() * dum );
        varea += dum;
      }
    }

    area = varea;
    centroid = new GM_Point_Impl( x / varea, y / varea, crs );
  }

  /**
   * calculates the centroid and the area of the surface patch
   */
  protected void calculateParam()
  {
    calculateEnvelope();
    calculateCentroidArea();
    setValid( true );
  }

  /**
   * calculates the area of the surface patch
   * <p>
   * </p>
   * taken from gems iv (modified)
   * <p>
   * </p>
   * this method is only valid for the two-dimensional case.
   */
  private double calculateArea( GM_Position[] point )
  {
    int i;
    int j;
    double ai;
    double atmp = 0;

    for( i = point.length - 1, j = 0; j < point.length; i = j, j++ )
    {
      double xi = point[i].getX() - point[0].getX();
      double yi = point[i].getY() - point[0].getY();
      double xj = point[j].getX() - point[0].getX();
      double yj = point[j].getY() - point[0].getY();
      ai = ( xi * yj ) - ( xj * yi );
      atmp += ai;
    }

    return Math.abs( atmp / 2 );
  }

  /**
   * calculates the centroid of the surface patch
   * <p>
   * taken from gems iv (modified)
   * <p>
   * </p>
   * this method is only valid for the two-dimensional case.
   */
  protected GM_Position calculateCentroid( GM_Position[] point )
  {

    int i;
    int j;
    double ai;
    double x;
    double y;
    double atmp = 0;
    double xtmp = 0;
    double ytmp = 0;

    // move points to the origin of the coordinate space
    // (to solve precision issues)
    double transX = point[0].getX();
    double transY = point[0].getY();

    for( i = point.length - 1, j = 0; j < point.length; i = j, j++ )
    {
      double x1 = point[i].getX() - transX;
      double y1 = point[i].getY() - transY;
      double x2 = point[j].getX() - transX;
      double y2 = point[j].getY() - transY;
      ai = ( x1 * y2 ) - ( x2 * y1 );
      atmp += ai;
      xtmp += ( ( x2 + x1 ) * ai );
      ytmp += ( ( y2 + y1 ) * ai );
    }

    if( atmp != 0 )
    {
      x = xtmp / ( 3 * atmp ) + transX;
      y = ytmp / ( 3 * atmp ) + transY;
    }
    else
    {
      x = point[0].getX();
      y = point[0].getY();
    }

    return new GM_Position_Impl( x, y );
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    String ret = "GM_SurfacePatch: ";
    ret = "interpolation = " + interpolation + "\n";
    ret += "exteriorRing = \n";

    for( int i = 0; i < exteriorRing.length; i++ )
    {
      ret += ( exteriorRing + "\n" );
    }

    ret += ( "interiorRings = " + interiorRings + "\n" );
    ret += ( "envelope = " + envelope + "\n" );
    return ret;
  }
}