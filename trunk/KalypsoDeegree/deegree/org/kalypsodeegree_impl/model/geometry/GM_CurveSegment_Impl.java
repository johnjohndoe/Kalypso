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

import org.deegree.model.geometry.GM_CurveSegment;
import org.deegree.model.geometry.GM_Exception;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Point;
import org.deegree.model.geometry.GM_Position;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * default implementation of the GM_CurveSegment interface from package
 * jago.model. the class is abstract because it should be specialized by derived
 * classes <code>GM_LineString</code> for example
 * 
 * <p>
 * ---------------------------------------------------------------------------
 * </p>
 * 
 * @version 10.6.2001
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 *  
 */
abstract class GM_CurveSegment_Impl implements GM_CurveSegment, Serializable
{
  /** Use serialVersionUID for interoperability. */
  private final static long serialVersionUID = -8102075931849374162L;

  protected CS_CoordinateSystem crs = null;

  protected GM_Position[] points = new GM_Position[0];

  /**
   * Creates a new GM_CurveSegment_Impl object.
   * 
   * @param gmps
   * @param crs
   * 
   * @throws GM_Exception
   */
  protected GM_CurveSegment_Impl( GM_Position[] gmps, CS_CoordinateSystem crs ) throws GM_Exception
  {
    if( gmps == null )
    {
      throw new GM_Exception( "can't create an empty curve segment" );
    }

    points = gmps;

    // get spatial reference system of the curve segment from the first point
    this.crs = crs;
  }

  /**
   * returns the first point of the curve. if the curve segment doesn't contain
   * a point <code>null</code> will be returned
   */
  public GM_Point getStartPoint()
  {
    return new GM_Point_Impl( points[0], crs );
  }

  /**
   * returns the last point of the curve. if the curve segment doesn't contain a
   * point <code>null</code> will be returned
   */
  public GM_Point getEndPoint()
  {
    return new GM_Point_Impl( points[getNumberOfPoints() - 1], crs );
  }

  /**
   * returns the number of points building the curve or curve segment
   */
  public int getNumberOfPoints()
  {
    return points.length;
  }

  /**
   * returns all positions of the segement as array of GM_Position. If the
   * segment is empty null will be returned
   */
  public GM_Position[] getPositions()
  {
    return points;
  }

  /**
   * returns the curve segment position at the submitted index
   */
  public GM_Position getPositionAt( int index )
  {
    return points[index];
  }

  /**
   * reverses the direction of the curvesegment
   */
  public void reverse()
  {
    GM_Position[] reverse_ = new GM_Position[points.length];

    for( int i = 0; i < points.length; i++ )
    {
      reverse_[points.length - 1 - i] = points[i];
    }

    points = reverse_;
  }

  /**
   * returns the coordinate system of the curve segment
   */
  public CS_CoordinateSystem getCoordinateSystem()
  {
    return crs;
  }

  /**
   * checks if this curve segment is completly equal to the submitted geometry
   * 
   * @param other
   *          object to compare to
   */
  public boolean equals( Object other )
  {
    if( ( other == null ) || !( other instanceof GM_CurveSegment_Impl ) )
    {
      return false;
    }

    if( ( crs == null ) && ( ( (GM_CurveSegment_Impl)other ).getCoordinateSystem() != null ) )
    {
      return false;
    }

    if( crs != null )
    {
      if( !crs.equals( ( (GM_CurveSegment_Impl)other ).getCoordinateSystem() ) )
      {
        return false;
      }
    }
    else
    {
      if( ( (GM_CurveSegment_Impl)other ).getCoordinateSystem() != null )
      {
        return false;
      }
    }

    GM_Position[] p1 = getPositions();
    GM_Position[] p2 = ( (GM_CurveSegment)other ).getPositions();

    if( !Arrays.equals( p1, p2 ) )
    {
      return false;
    }

    return true;
  }

  /**
   * The Boolean valued operation "contains" shall return TRUE if this GM_Object
   * contains another GM_Object.
   * <p>
   * </p>
   */
  public boolean contains( GM_Object gmo )
  {
    throw new NoSuchMethodError( "the contains operation for curve segments "
        + "isn't supported at the moment." );
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    String ret = null;
    ret = "points = ";
    ret += ( "crs = " + crs + "\n" );
    return ret;
  }
}