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

import org.eclipse.core.runtime.Assert;
import org.kalypsodeegree.model.geometry.GM_Aggregate;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.ct.MathTransform;
import org.kalypsodeegree_impl.tools.Debug;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * default implementation of the GM_Point interface.
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

  private final GM_Position m_position;

  /**
   * constructor. initializes a point to the coordinate 0/0
   * 
   * @param crs
   *            spatial reference system of the point
   */
  public GM_Point_Impl( final CS_CoordinateSystem crs )
  {
    this( new GM_Position_Impl(), crs );
  }

  /**
   * constructor for initializing a point within a two-dimensional coordinate system
   * 
   * @param x
   *            x-value of the point
   * @param y
   *            y-value of the point
   * @param crs
   *            spatial reference system of the point
   */
  public GM_Point_Impl( final double x, final double y, final CS_CoordinateSystem crs )
  {
    this( new GM_Position_Impl( x, y ), crs );
  }

  /**
   * constructor for initializing a point within a three-dimensional coordinate system
   * 
   * @param x
   *            x-value of the point
   * @param y
   *            y-value of the point
   * @param z
   *            z-value of the point
   * @param crs
   *            spatial reference system of the point
   */
  public GM_Point_Impl( final double x, final double y, final double z, final CS_CoordinateSystem crs )
  {
    this( new GM_Position_Impl( x, y, z ), crs );
  }

  /**
   * constructor
   * 
   * @param gmo
   *            existing GM_Point
   */
  public GM_Point_Impl( final GM_Point gmo )
  {
    this( new GM_Position_Impl( gmo.getAsArray() ), gmo.getCoordinateSystem() );
  }

  /**
   * constructor
   * 
   * @param gmo
   *            existing GM_Point
   * @param crs
   *            spatial reference system of the point
   */
  public GM_Point_Impl( final GM_Position gmo, final CS_CoordinateSystem crs )
  {
    super( crs );

    Assert.isNotNull( gmo );

    m_position = gmo;
    setEmpty( false );
    setCentroid( this );
  }

  /**
   * checks if this point is completly equal to the submitted geometry
   */
  @Override
  public boolean equals( final Object other )
  {
    if( super.equals( other ) && (other instanceof GM_Point) )
    {
      final GM_Point p = (GM_Point) other;
      boolean flagEq = (Math.abs( getX() - p.getX() ) < MUTE) && (Math.abs( getY() - p.getY() ) < MUTE);
      if( getCoordinateDimension() == 3 )
      {
        flagEq = flagEq && (Math.abs( getZ() - p.getZ() ) < MUTE);
      }
      return flagEq;
    }

    return false;
  }

  /**
   * The operation "dimension" shall return the inherent dimension of this GM_Object, which shall be less than or equal
   * to the coordinate dimension. The dimension of a collection of geometric objects shall be the largest dimension of
   * any of its pieces. Points are 0-dimensional, curves are 1-dimensional, surfaces are 2-dimensional, and solids are
   * 3-dimensional.
   */
  public int getDimension( )
  {
    return 0;
  }

  /**
   * The operation "coordinateDimension" shall return the dimension of the coordinates that define this GM_Object, which
   * must be the same as the coordinate dimension of the coordinate reference system for this GM_Object.
   */
  public int getCoordinateDimension( )
  {
    return m_position.getAsArray().length;
  }

  /**
   * returns a shallow copy of the geometry.
   */
  @Override
  public Object clone( )
  {
    final CS_CoordinateSystem system = getCoordinateSystem();

    if( getDimension() == 3 )
      return new GM_Point_Impl( getX(), getY(), getZ(), system );

    return new GM_Point_Impl( getX(), getY(), system );
  }

  /**
   * returns the x-value of this point
   */
  public double getX( )
  {
    return m_position.getX();
  }

  /**
   * returns the y-value of this point
   */
  public double getY( )
  {
    return m_position.getY();
  }

  /**
   * returns the y-value of this point
   */
  public double getZ( )
  {
    return m_position.getZ();
  }

  /**
   * returns the x- and y-value of the point as a two dimensional array the first field contains the x- the second field
   * the y-value.
   */
  public double[] getAsArray( )
  {
    return m_position.getAsArray();
  }

  /**
   * translate the point by the submitted values. the <code>dz</code>- value will be ignored.
   */
  @Override
  public void translate( final double[] d )
  {
    setValid( false );
    m_position.translate( d );
  }

  public GM_Position getPosition( )
  {
    return m_position;
  }

  /**
   * The Boolean valued operation "intersects" shall return TRUE if this GM_Object intersects another GM_Object. Within
   * a GM_Complex, the GM_Primitives do not intersect one another. In general, topologically structured data uses shared
   * geometric objects to capture intersection information.
   * <p>
   * </p>
   * dummy implementation
   */
  @Override
  public boolean intersects( final GM_Object gmo )
  {
    boolean inter = false;

    try
    {
      if( gmo instanceof GM_Point )
      {
        inter = LinearIntersects.intersects( (GM_Point) gmo, this );
      }
      else if( gmo instanceof GM_Curve )
      {
        inter = LinearIntersects.intersects( this, (GM_Curve) gmo );
      }
      else if( gmo instanceof GM_Surface )
      {
        inter = LinearIntersects.intersects( this, (GM_Surface) gmo );
      }
      else if( gmo instanceof GM_Aggregate )
      {
        inter = intersectsAggregate( (GM_Aggregate) gmo );
      }
    }
    catch( final Exception e )
    {
    }

    return inter;
  }

  /**
   * the operations returns true if the submitted multi primitive intersects with the curve segment
   */
  private boolean intersectsAggregate( final GM_Aggregate mprim ) throws Exception
  {
    boolean inter = false;

    final int cnt = mprim.getSize();

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
   * The Boolean valued operation "contains" shall return TRUE if this GM_Object contains another GM_Object.
   * <p>
   * </p>
   */
  @Override
  public boolean contains( final GM_Object gmo )
  {
    return equals( gmo );
    // TODO: check if this is correct in all cases
    // throw new UnsupportedOperationException( "the contains operation for points " + "isn't supported at the moment."
    // );
  }

  /**
   * recalculates internal parameters
   */
  @Override
  protected void calculateParam( )
  {
    setValid( true );
  }

  @Override
  public String toString( )
  {
    String ret = "GM_Point: ";

    for( int i = 0; i < getCoordinateDimension(); i++ )
    {
      ret += (getAsArray()[i] + " ");
    }

    return ret;
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Object#transform(org.kalypsodeegree_impl.model.ct.MathTransform)
   */
  public GM_Object transform( final MathTransform trans, final CS_CoordinateSystem targetOGCCS ) throws Exception
  {
    Debug.debugMethodBegin( this, "transformPoint" );

    final double[] din = getAsArray();
    // TODO macht der folgende if Sinn ?
    if( getCoordinateSystem().getName().equalsIgnoreCase( "EPSG:4326" ) )
    {
      if( din[0] <= -180 )
        din[0] = -179.999;
      else if( din[0] >= 180 )
        din[0] = 179.999;
      if( din[1] <= -90 )
        din[1] = -89.999;
      else if( din[1] >= 90 )
        din[1] = 89.999;
    }
    final double[] dout = new double[din.length];
    try
    {
      if( din.length < 3 )
        trans.transform( din, 0, dout, 0, din.length - 1 );
      else
      {
        final double[] din2d = new double[2];
        final double[] dout2d = new double[2];
        din2d[0] = din[0];
        din2d[1] = din[1];
        trans.transform( din2d, 0, dout2d, 0, din2d.length - 1 );
        dout[0] = dout2d[0];
        dout[1] = dout2d[1];
        dout[2] = din[2];
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    if( din.length > 2 )
    {
      Debug.debugMethodEnd();
      return GeometryFactory.createGM_Point( dout[0], dout[1], dout[2], targetOGCCS );
    }
    else
    {
      Debug.debugMethodEnd();
      return GeometryFactory.createGM_Point( dout[0], dout[1], targetOGCCS );
    }

  }
}