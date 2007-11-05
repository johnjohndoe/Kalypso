/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypsodeegree_impl.model.geometry;

import org.kalypso.jts.JTSUtilities;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_SurfaceInterpolation;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.opengis.cs.CS_CoordinateSystem;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * @author Gernot Belger
 */
public class GM_Triangle_Impl extends GM_Polygon_Impl implements GM_Triangle
{
  public static GM_SurfaceInterpolation PLANAR_INTERPOLATION;

  static
  {
    try
    {
      PLANAR_INTERPOLATION = new GM_SurfaceInterpolation_Impl( GM_SurfaceInterpolation.PLANAR );
    }
    catch( final GM_Exception e )
    {
      // will never happen
      e.printStackTrace();
    }
  }

  public GM_Triangle_Impl( final GM_Position pos1, final GM_Position pos2, final GM_Position pos3, final CS_CoordinateSystem crs ) throws GM_Exception
  {
    super( PLANAR_INTERPOLATION, new GM_Position[] { pos1, pos2, pos3, pos1 }, null, crs );
  }

  @Override
  public CS_CoordinateSystem getCoordinateSystem( )
  {
    return m_crs;
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Triangle#getInterpolation()
   */
  @Override
  public GM_SurfaceInterpolation getInterpolation( )
  {
    return PLANAR_INTERPOLATION;
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Triangle#getValue(org.kalypsodeegree.model.geometry.GM_Point)
   */
  public double getValue( final GM_Point location )
  {
    final GM_Position position = location.getPosition();
    // TODO: transform into own crs if necessairy
    return getValue( position );
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Triangle#getValue(org.kalypsodeegree.model.geometry.GM_Position)
   */
  public double getValue( final GM_Position position )
  {
    final double x = position.getX();
    final double y = position.getY();

    final GM_Position[] exteriorRing = getExteriorRing();
    final Coordinate c0 = JTSAdapter.export( exteriorRing[0] );
    final Coordinate c1 = JTSAdapter.export( exteriorRing[1] );
    final Coordinate c2 = JTSAdapter.export( exteriorRing[2] );
    final Coordinate[] coords = new Coordinate[] { c0, c1, c2 };
    final double[] planarEquation = JTSUtilities.calculateTrianglePlaneEquation( coords );
    return JTSUtilities.calculateTriangleZ( planarEquation, x, y );
  }

  /**
   * Overwritten for better perfomance.
   * 
   * @see org.kalypsodeegree.model.geometry.GM_Triangle#contains(org.kalypsodeegree.model.geometry.GM_Position)
   */
  public boolean contains( final GM_Position position )
  {
    final GM_Position[] exteriorRing = getExteriorRing();

    final GM_Position pos1 = exteriorRing[0];
    final GM_Position pos2 = exteriorRing[1];
    final GM_Position pos3 = exteriorRing[2];

    final int orientation12 = orientation( pos1, pos2, position );
    final int orientation23 = orientation( pos2, pos3, position );
    final int orientation31 = orientation( pos3, pos1, position );

    return orientation12 == orientation23 && orientation23 == orientation31;
  }

  private static int orientation( final GM_Position pos1, final GM_Position pos2, final GM_Position pos3 )
  {
    final double s_a = signedArea( pos1, pos2, pos3 );
    return s_a > 0 ? 1 : (s_a < 0 ? -1 : 0);
  }

  private static double signedArea( final GM_Position pos1, final GM_Position pos2, final GM_Position pos3 )
  {
    return (pos1.getX() * (pos2.getY() - pos3.getY()) + pos2.getX() * (pos3.getY() - pos1.getY()) + pos3.getX() * (pos1.getY() - pos2.getY()));
  }
}
