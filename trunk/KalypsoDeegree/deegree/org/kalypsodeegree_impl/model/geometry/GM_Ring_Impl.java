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
package org.deegree_impl.model.geometry;

import java.io.Serializable;
import java.util.Arrays;

import org.deegree.model.geometry.GM_Aggregate;
import org.deegree.model.geometry.GM_Curve;
import org.deegree.model.geometry.GM_CurveBoundary;
import org.deegree.model.geometry.GM_CurveSegment;
import org.deegree.model.geometry.GM_Exception;
import org.deegree.model.geometry.GM_MultiPrimitive;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Point;
import org.deegree.model.geometry.GM_Position;
import org.deegree.model.geometry.GM_Ring;
import org.deegree.model.geometry.GM_Surface;
import org.deegree.model.geometry.GM_SurfacePatch;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * default implementation of the GM_Ring interface of the
 * 
 * 
 * <p>
 * -----------------------------------------------------------------------
 * </p>
 * 
 * @version 05.04.2002
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class GM_Ring_Impl extends GM_OrientableCurve_Impl implements GM_Ring, Serializable
{
  /** Use serialVersionUID for interoperability. */
  private final static long serialVersionUID = 9157144642050604928L;

  private GM_Position[] points = null;

  private GM_SurfacePatch sp = null;

  /**
   * Constructor, with Array and CS_CoordinateSystem
   */
  public GM_Ring_Impl( GM_Position[] points, CS_CoordinateSystem crs ) throws GM_Exception
  {
    super( crs );

    setPositions( points );
  }

  /**
   * Constructor, with Array, CS_CoordinateSystem and Orientation
   */
  public GM_Ring_Impl( GM_Position[] points, CS_CoordinateSystem crs, char orientation )
      throws GM_Exception
  {
    super( crs, orientation );
    setPositions( points );
  }

  /**
   * calculates the envelope
   */
  private void calculateEnvelope()
  {
    double[] min = (double[])points[0].getAsArray().clone();
    double[] max = (double[])min.clone();

    for( int i = 1; i < points.length; i++ )
    {
      double[] pos = points[i].getAsArray();

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
   * GM_Ring must be closed, so isCycle returns TRUE.
   */
  public boolean isCycle()
  {
    return true;
  }

  /**
   * GM_Ring is a PrimitiveBoundary, so isSimple returns TRUE.
   */
  public boolean isSimple()
  {
    return true;
  }

  /**
   * The operation "dimension" shall return the inherent dimension of this
   * GM_Object, which shall be less than or equal to the coordinate dimension.
   * The dimension of a collection of geometric objects shall be the largest
   * dimension of any of its pieces. Points are 0-dimensional, curves are
   * 1-dimensional, surfaces are 2-dimensional, and solids are 3-dimensional.
   */
  public int getDimension()
  {
    return 1;
  }

  /**
   * The operation "coordinateDimension" shall return the dimension of the
   * coordinates that define this GM_Object, which must be the same as the
   * coordinate dimension of the coordinate reference system for this GM_Object.
   */
  public int getCoordinateDimension()
  {
    return getPositions()[0].getAsArray().length;
  }

  /**
   * gets the Ring as a Array of positions.
   */
  public GM_Position[] getPositions()
  {
    if( getOrientation() == '-' )
    {
      GM_Position[] temp = new GM_Position[points.length];

      for( int i = 0; i < points.length; i++ )
      {
        temp[i] = points[( points.length - 1 ) - i];
      }

      return temp;
    }
    else
    {
      return points;
    }
  }

  /**
   * sets the Ring as a ArrayList of points
   */
  protected void setPositions( GM_Position[] positions ) throws GM_Exception
  {
    this.points = positions;

    // checks if the ring has more than 3 elements [!(points.length > 3)]
    if( positions.length < 3 )
    {
      throw new GM_Exception( "invalid length of a Ring!" );
    }

    // checks if the startpoint = endpoint of the ring
    if( !positions[0].equals( positions[positions.length - 1] ) )
    {
      throw new GM_Exception( "StartPoint of ring isn't equal to EndPoint!" );
    }

    setValid( false );
  }

  /**
   * returns the Ring as a CurveSegment
   */
  public GM_CurveSegment getAsCurveSegment() throws GM_Exception
  {
    return new GM_LineString_Impl( points, getCoordinateSystem() );
  }

  /**
   * returns the CurveBoundary of the Ring. For a CurveBoundary is defines as
   * the first and the last point of a Curve the CurveBoundary of a Ring
   * contains two indentical point (because a Ring is closed)
   */
  public GM_CurveBoundary getCurveBoundary()
  {
    return (GM_CurveBoundary)boundary;
  }

  /**
   * checks if this curve segment is completly equal to the submitted geometry
   * 
   * @param other
   *          object to compare to
   */
  public boolean equals( Object other )
  {
    if( !super.equals( other ) || !( other instanceof GM_Ring_Impl ) )
    {
      return false;
    }

    if( !envelope.equals( ( (GM_Object)other ).getEnvelope() ) )
    {
      return false;
    }

    GM_Position[] p2 = ( (GM_Ring)other ).getPositions();

    if( !Arrays.equals( points, p2 ) )
    {
      return false;
    }

    return true;
  }

  /**
   * returns a shallow copy of the geometry
   */
  public Object clone()
  {
    GM_Ring r = null;

    try
    {
      GM_Position[] p = (GM_Position[])points.clone();

      r = new GM_Ring_Impl( p, getCoordinateSystem(), getOrientation() );
    }
    catch( Exception ex )
    {
      System.out.println( "GM_Ring_Impl.clone: " + ex );
    }

    return r;
  }

  /**
   * The Boolean valued operation "intersects" shall return TRUE if this
   * GM_Object intersects another GM_Object. Within a GM_Complex, the
   * GM_Primitives do not intersect one another. In general, topologically
   * structured data uses shared geometric objects to capture intersection
   * information.
   */
  public boolean intersects( GM_Object gmo )
  {
    boolean inter = false;

    try
    {
      GM_CurveSegment sp = new GM_LineString_Impl( points, crs );

      if( gmo instanceof GM_Point )
      {
        inter = LinearIntersects.intersects( ( (GM_Point)gmo ).getPosition(), sp );
      }
      else if( gmo instanceof GM_Curve )
      {
        GM_Curve curve = new GM_Curve_Impl( new GM_CurveSegment[]
        { sp } );
        inter = LinearIntersects.intersects( (GM_Curve)gmo, curve );
      }
      else if( gmo instanceof GM_Surface )
      {
        GM_Curve curve = new GM_Curve_Impl( new GM_CurveSegment[]
        { sp } );
        inter = LinearIntersects.intersects( curve, (GM_Surface)gmo );
      }
      else if( gmo instanceof GM_MultiPrimitive )
      {
        inter = intersectsAggregate( (GM_MultiPrimitive)gmo );
      }
    }
    catch( Exception e )
    {}

    return inter;
  }

  /**
   * the operations returns true if the submitted multi primitive intersects
   * with the curve segment
   */
  private boolean intersectsAggregate( GM_Aggregate mprim ) throws Exception
  {
    boolean inter = false;

    int cnt = mprim.getSize();

    for( int i = 0; i < cnt; i++ )
    {
      if( intersects( mprim.getObjectAt( i ) ) )
      {
        inter = true;
        break;
      }
    }

    return inter;
  }

  /**
   * The Boolean valued operation "contains" shall return TRUE if this GM_Object
   * contains another GM_Object.
   * <p>
   * </p>
   * At the moment the operation just works with point geometries
   */
  public boolean contains( GM_Object gmo )
  {

    try
    {
      if( sp == null )
      {
        sp = new GM_Polygon_Impl( new GM_SurfaceInterpolation_Impl(), points, null, crs );
      }
      return sp.contains( gmo );
    }
    catch( Exception e )
    {}

    return false;
  }

  /**
   * The Boolean valued operation "contains" shall return TRUE if this GM_Object
   * contains a single point given by a coordinate.
   * <p>
   * </p>
   * dummy implementation
   */
  public boolean contains( GM_Position position )
  {
    return contains( new GM_Point_Impl( position, null ) );
  }

  /**
   * calculates the centroid of the ring
   */
  protected void calculateCentroid()
  {
    double[] cen = new double[getCoordinateDimension()];

    for( int i = 0; i < points.length; i++ )
    {
      for( int j = 0; j < getCoordinateDimension(); j++ )
      {
        cen[j] += ( points[i].getAsArray()[j] / points.length );
      }
    }

    centroid = new GM_Point_Impl( new GM_Position_Impl( cen ), crs );
  }

  /**
   * calculates the centroid and the envelope of the ring
   */
  protected void calculateParam()
  {
    calculateCentroid();
    calculateEnvelope();
    setValid( true );
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    String ret = null;
    ret = "points = " + points + "\n";
    ret += ( "envelope = " + envelope + "\n" );
    return ret;
  }
}