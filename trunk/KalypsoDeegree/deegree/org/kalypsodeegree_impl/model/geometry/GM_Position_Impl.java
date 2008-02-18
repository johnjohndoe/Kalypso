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
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.ct.MathTransform;
import org.kalypsodeegree_impl.model.ct.TransformException;
import org.kalypsodeegree_impl.tools.Debug;

/**
 * A sequence of decimals numbers which when written on a width are a sequence of coordinate positions. The width is
 * derived from the CRS or coordinate dimension of the container.
 * <p>
 * -----------------------------------------------------------------------
 * </p>
 * 
 * @version
 * @author Andreas Poth
 *         <p>
 */
class GM_Position_Impl implements GM_Position, Serializable
{
  /** Use serialVersionUID for interoperability. */
  private final static long serialVersionUID = -3780255674921824356L;

  private final double MUTE = 0.000001;

  private final double[] m_point;

  /**
   * constructor. initializes a point to the coordinate 0/0
   */
  GM_Position_Impl( )
  {
    m_point = new double[] { 0, 0, 0 };
  }

  /**
   * constructor
   * 
   * @param x
   *            x-value of the point
   * @param y
   *            y-value of the point
   */
  GM_Position_Impl( final double x, final double y )
  {
    m_point = new double[] { x, y };
  }

  /**
   * constructor
   * 
   * @param x
   *            x-value of the point
   * @param y
   *            y-value of the point
   * @param z
   *            z-value of the point
   */
  GM_Position_Impl( final double x, final double y, final double z )
  {
    m_point = new double[] { x, y, z };
  }

  /**
   * Copies the content of the given array, does NOT keep a reference to it.
   */
  GM_Position_Impl( final double[] coords )
  {
    Assert.isNotNull( coords );

    m_point = coords.clone();
  }

  /**
   * returns a deep copy of the geometry.
   */
  @Override
  public Object clone( )
  {
    return new GM_Position_Impl( m_point.clone() );
  }

  /**
   * returns the x-value of this point
   */
  public double getX( )
  {
    return m_point[0];
  }

  /**
   * returns the y-value of this point
   */
  public double getY( )
  {
    return m_point[1];
  }

  /**
   * returns the z-value of this point
   */
  public double getZ( )
  {
    return m_point[2];
  }

  /**
   * returns the position as a array the first field contains the x- the second field the y-value etc.
   */
  public double[] getAsArray( )
  {
    // return (double[])point.clone();
    return m_point;
  }

  /**
   * translate the point by the submitted values. the <code>dz</code>- value will be ignored.
   */
  public void translate( final double[] d )
  {
    for( int i = 0; i < d.length; i++ )
    {
      m_point[i] += d[i];
    }
  }

  /**
   * compares if all field of other are equal to the corresponding fields of this position
   */
  @Override
  public boolean equals( final Object other )
  {
    boolean eq = true;
    final double[] other_ = ((GM_Position) other).getAsArray();

    if( other_.length != m_point.length )
    {
      eq = false;
    }

    if( eq )
    {
      for( int i = 0; i < m_point.length; i++ )
      {
        if( Math.abs( m_point[i] - other_[i] ) > MUTE )
        {
          eq = false;
          break;
        }
      }
    }

    return eq;
  }

  @Override
  public String toString( )
  {
    String ret = "GM_Position: ";

    for( final double element : m_point )
    {
      ret += (element + " ");
    }

    return ret;
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Position#getDistance(org.kalypsodeegree.model.geometry.GM_Position)
   */
  public double getDistance( final GM_Position other )
  {
    final double[] otherPoint = other.getAsArray();

    // final double[] pos = getAsArray();
    // double square = 0;
    // for( int j = 0; j < 2; j++ )
    // square += Math.pow( pos[j] - otherPos[j], 2d );
    //
    // return Math.pow( square, 0.5d );

    // RMARK: calculating the square / power of 2 directly instead via Math.pow
    // is noticeable faster.

    final double dx = m_point[0] - otherPoint[0];
    final double dy = m_point[1] - otherPoint[1];
    final double d = dx * dx + dy * dy;
    return Math.sqrt( d );

  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Position#transform(org.kalypsodeegree_impl.model.ct.MathTransform)
   */
  public GM_Position transform( final MathTransform trans ) throws TransformException
  {
    Debug.debugMethodBegin( this, "transformPosition" );

    final double[] din = getAsArray();
    final double[] dout = transform( din, trans );

    Debug.debugMethodEnd();
    return GeometryFactory.createGM_Position( dout );
  }

  public static double[] transform( final double[] din, final MathTransform trans ) throws TransformException
  {
    final double[] dout = new double[din.length];

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

    return dout;
  }
}