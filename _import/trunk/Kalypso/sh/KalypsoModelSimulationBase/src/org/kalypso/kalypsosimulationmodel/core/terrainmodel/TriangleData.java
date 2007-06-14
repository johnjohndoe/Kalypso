/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree.model.geometry.ISurfacePatchVisitable;
import org.kalypsodeegree.model.geometry.ISurfacePatchVisitor;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;

/**
 * @author Patrice Congo
 * @author Madanagopal
 */

class TriangleData implements ISurfacePatchVisitable<GM_SurfacePatch>
{
  private final LinearRing ring;

  private final ITriangleAlgorithm _divider;

  private final Polygon polygon;

  private final double[] planeEquation;

  private final double centerElevation;

  public TriangleData( final LinearRing ring )
  {
    this.ring = ring;
    // Choice of the Triangle Division Algoritm
    /* 1. */_divider = new TriangleFourDividerAlgorithm( ring );
    /* 2. */// _divider = new TriangleDivider(ring);
    polygon = new Polygon( ring, null, ring.getFactory() );
    final Coordinate[] coords = ring.getCoordinates();
    planeEquation = calculateTrianglePlaneEquation( coords );
    centerElevation = calculateCenterElevation( coords );
  }

  /**
   * Calculate Center Elevatation from an Array of Coordinates representing the GM_Surface
   */
  private double calculateCenterElevation( final Coordinate[] coords )
  {
    final double xCenter = (coords[0].x + coords[1].x + coords[2].x) / 3;
    final double yCenter = (coords[0].y + coords[1].y + coords[2].y) / 3;
    return computeZOfTrianglePlanePoint( xCenter, yCenter );
  }

  public boolean contains( final Point point )
  {
    if( point == null )
    {
      return false;
    }
    else
    {
      if( polygon.contains( point ) )
      {
        return true;
      }
      else
      {
        // also look on the exterior ring since polygon.contains()
        // seems not to return true
        // System.out.println("point:"+point+ " "+ring.contains( point ));
        return ring.contains( point );
      }
    }
  }

  public double getCenterElevation( )
  {
    return centerElevation;// ring.getCoordinateN( 0 ).z;//Centroid().getCoordinate().z;
  }

  /**
   * Given 3 coordinate this methode return the equation of a plan containing those points. The return equation as the
   * form: z = Q*x+P*y+O The coefficients Q, P amd O are return as array
   * 
   * @param coords
   *            coordinate of 3 plane points
   * @return the cooeficients of the plane equation z = Q*x+P*y+O as array of double {Q,P,O}
   */
  public static final double[] calculateTrianglePlaneEquation( final Coordinate[] coords )
  {
    Assert.throwIAEOnNullParam( coords, "coords" );
    if( coords.length < 3 )
    {
      throw new IllegalArgumentException( "Param coord which represent the point of a triangle must " + "have a minimum length of 3: current length=" + coords.length );
    }

    Coordinate coord = coords[0];

    final double x1 = coord.x;
    final double y1 = coord.y;
    final double z1 = coord.z;

    coord = coords[1];
    final double x2 = coord.x;
    final double y2 = coord.y;
    final double z2 = coord.z;

    coord = coords[2];
    final double x3 = coord.x;
    final double y3 = coord.y;
    final double z3 = coord.z;
    if( z1 == z2 && z2 == z3 )
    {
      // z=-A/Cx-B/Cy-D/C = Q*x+P*y+O
      return new double[] { 0, 0, z1 };
    }
    else
    {
      // build the equation Ax + By + Cz - D = 0
      double A = y1 * (z2 - z3) + y2 * (z3 - z1) + y3 * (z1 - z2);
      double B = z1 * (x2 - x3) + z2 * (x3 - x1) + z3 * (x1 - x2);
      final double C = x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2);
      final double D = x1 * (y2 * z3 - y3 * z2) + x2 * (y3 * z1 - y1 * z3) + x3 * (y1 * z2 - y2 * z1);

      // C=-C;
      // z=-A/Cx-B/Cy-D/C = Q*x+P*y+O
      return new double[] { -A / C, -B / C, D / C };
    }
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.SurfacePatchVisitable#aceptSurfacePatches(org.kalypsodeegree.model.geometry.GM_Envelope,
   *      org.kalypso.kalypsosimulationmodel.core.terrainmodel.SurfacePatchVisitor)
   */
  public void acceptSurfacePatches( final GM_Envelope envToVisit, final ISurfacePatchVisitor<GM_SurfacePatch> surfacePatchVisitor ) throws GM_Exception
  {
    _divider.acceptSurfacePatches( envToVisit, surfacePatchVisitor );
  }

  public double getMinElevation( )
  {
    final Coordinate[] coordinates = ring.getCoordinates();
    double min = coordinates[0].z;
    if( min > coordinates[1].z )
    {
      min = coordinates[1].z;
    }

    if( min > coordinates[2].z )
    {
      min = coordinates[2].z;
    }
    return min;
  }

  public double getMaxElevation( )
  {
    final Coordinate[] coordinates = ring.getCoordinates();
    double max = coordinates[0].z;
    if( max < coordinates[1].z )
    {
      max = coordinates[1].z;
    }

    if( max < coordinates[2].z )
    {
      max = coordinates[2].z;
    }
    return max;
  }

  public final double getMinX( )
  {
    final Coordinate[] coordinates = ring.getCoordinates();

    double min = coordinates[0].x;
    if( min > coordinates[1].x )
    {
      min = coordinates[1].x;
    }

    if( min > coordinates[2].x )
    {
      min = coordinates[2].x;
    }
    return min;
  }

  public final double getMinY( )
  {
    final Coordinate[] coordinates = ring.getCoordinates();

    double min = coordinates[0].y;
    if( min > coordinates[1].y )
    {
      min = coordinates[1].y;
    }

    if( min > coordinates[2].y )
    {
      min = coordinates[2].y;
    }
    return min;

  }

  public double computeZOfTrianglePlanePoint( final double x, final double y )
  {
    return planeEquation[0] * x + planeEquation[1] * y + planeEquation[2];
  }

  public LinearRing getRing( )
  {
    return ring;
  }

}