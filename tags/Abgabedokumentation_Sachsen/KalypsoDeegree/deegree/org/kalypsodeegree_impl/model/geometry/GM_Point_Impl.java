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
package org.kalypsodeegree_impl.model.geometry;

import java.io.Serializable;

import org.kalypsodeegree.model.geometry.GM_Aggregate;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * default implementation of the GM_Point interface.
 * 
 * <p>
 * ------------------------------------------------------------
 * </p>
 * 
 * @version 5.6.2001
 * @author Andreas Poth
 *         <p>
 */
final class GM_Point_Impl extends GM_Primitive_Impl implements GM_Point, Serializable
{
  /** Use serialVersionUID for interoperability. */
  private final static long serialVersionUID = 6106017748940535740L;

  private GM_Position position = null;

  /**
   * constructor. initializes a point to the coordinate 0/0
   * 
   * @param crs
   *          spatial reference system of the point
   */
  public GM_Point_Impl( CS_CoordinateSystem crs )
  {
    super( crs );
    position = new GM_Position_Impl();
    empty = true;
    centroid = this;
  }

  /**
   * constructor for initializing a point within a two-dimensional coordinate
   * system
   * 
   * @param x
   *          x-value of the point
   * @param y
   *          y-value of the point
   * @param crs
   *          spatial reference system of the point
   */
  public GM_Point_Impl( double x, double y, CS_CoordinateSystem crs )
  {
    super( crs );
    position = new GM_Position_Impl( x, y );
    empty = false;
    centroid = this;
  }

  /**
   * constructor for initializing a point within a three-dimensional coordinate
   * system
   * 
   * @param x
   *          x-value of the point
   * @param y
   *          y-value of the point
   * @param z
   *          z-value of the point
   * @param crs
   *          spatial reference system of the point
   */
  public GM_Point_Impl( double x, double y, double z, CS_CoordinateSystem crs )
  {
    super( crs );
    position = new GM_Position_Impl( x, y, z );
    empty = false;
    centroid = this;
  }

  /**
   * constructor
   * 
   * @param gmo
   *          existing GM_Point
   */
  public GM_Point_Impl( GM_Point gmo )
  {
    super( gmo.getCoordinateSystem() );
    position = new GM_Position_Impl( gmo.getAsArray() );
    empty = false;
    centroid = this;
  }

  /**
   * constructor
   * 
   * @param gmo
   *          existing GM_Point
   * @param crs
   *          spatial reference system of the point
   */
  public GM_Point_Impl( GM_Position gmo, CS_CoordinateSystem crs )
  {
    super( crs );
    position = gmo;
    empty = false;
    centroid = this;
  }

  /**
   * checks if this point is completly equal to the submitted geometry
   */
  public boolean equals( Object other )
  {
    if( super.equals( other ) && ( other instanceof GM_Point ) )
    {
      GM_Point p = (GM_Point)other;
      boolean flagEq = Math.abs( getX() - p.getX() ) < mute && Math.abs( getY() - p.getY() ) < mute;
      if( getCoordinateDimension() == 3 )
      {
        flagEq = flagEq && Math.abs( getZ() - p.getZ() ) < mute;
      }
      return flagEq;
    }

    return false;
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
    return 0;
  }

  /**
   * The operation "coordinateDimension" shall return the dimension of the
   * coordinates that define this GM_Object, which must be the same as the
   * coordinate dimension of the coordinate reference system for this GM_Object.
   */
  public int getCoordinateDimension()
  {
    return position.getAsArray().length;
  }

  /**
   * returns a shallow copy of the geometry.
   */
  public Object clone()
  {
    return new GM_Point_Impl( this );
  }

  /**
   * returns the x-value of this point
   */
  public double getX()
  {
    return position.getX();
  }

  /**
   * returns the y-value of this point
   */
  public double getY()
  {
    return position.getY();
  }

  /**
   * returns the y-value of this point
   */
  public double getZ()
  {
    return position.getZ();
  }

  /**
   * returns the x- and y-value of the point as a two dimensional array the
   * first field contains the x- the second field the y-value.
   */
  public double[] getAsArray()
  {
    return position.getAsArray();
  }

  /**
   * translate the point by the submitted values. the <code>dz</code>- value
   * will be ignored.
   */
  public void translate( double[] d )
  {
    setValid( false );
    position.translate( d );
  }

  /**
   * 
   * 
   * @return
   */
  public GM_Position getPosition()
  {
    return position;
  }

  /**
   * The Boolean valued operation "intersects" shall return TRUE if this
   * GM_Object intersects another GM_Object. Within a GM_Complex, the
   * GM_Primitives do not intersect one another. In general, topologically
   * structured data uses shared geometric objects to capture intersection
   * information.
   * <p>
   * </p>
   * dummy implementation
   */
  public boolean intersects( GM_Object gmo )
  {
    boolean inter = false;

    try
    {
      if( gmo instanceof GM_Point )
      {
        inter = LinearIntersects.intersects( (GM_Point)gmo, this );
      }
      else if( gmo instanceof GM_Curve )
      {
        inter = LinearIntersects.intersects( this, (GM_Curve)gmo );
      }
      else if( gmo instanceof GM_Surface )
      {
        inter = LinearIntersects.intersects( this, (GM_Surface)gmo );
      }
      else if( gmo instanceof GM_Aggregate )
      {
        inter = intersectsAggregate( (GM_Aggregate)gmo );
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
   */
  public boolean contains( GM_Object gmo )
  {
    throw new NoSuchMethodError( "the contains operation for points "
        + "isn't supported at the moment." );
  }

  /**
   * recalculates internal parameters
   */
  protected void calculateParam()
  {
    setValid( true );
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    String ret = "GM_Point: ";

    for( int i = 0; i < getCoordinateDimension(); i++ )
    {
      ret += ( getAsArray()[i] + " " );
    }

    return ret;
  }
}