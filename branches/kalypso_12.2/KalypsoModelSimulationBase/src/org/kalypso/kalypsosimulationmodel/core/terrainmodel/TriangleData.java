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

import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.jts.JTSUtilities;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree.model.geometry.ISurfacePatchVisitable;
import org.kalypsodeegree.model.geometry.ISurfacePatchVisitor;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

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
  private final LinearRing m_ring;

  private final Polygon m_polygon;

  private final double[] relativePlaneEquation;

  private final double centerElevation;

  private GM_Triangle m_trianglePath;

  public TriangleData( final LinearRing ring, final String crs )
  {
    // TODO: there is no need to divide the triangles anymore, because now they get split at
    // the real borders. Therefore convert the ring into an Triangle an paint it.

    // TODO: give the paint method the right border.

    this.m_ring = ring;
    m_polygon = new Polygon( ring, null, ring.getFactory() );
    final Coordinate[] coords = ring.getCoordinates();
    // planeEquation = JTSUtilities.calculateTrianglePlaneEquation( coords );
    relativePlaneEquation = JTSUtilities.calculateRelativeTrianglePlaneEquation( coords );
    centerElevation = calculateCenterElevation( coords );

    final GM_Position pos1 = JTSAdapter.wrap( ring.getCoordinateN( 0 ) );
    final GM_Position pos2 = JTSAdapter.wrap( ring.getCoordinateN( 1 ) );
    final GM_Position pos3 = JTSAdapter.wrap( ring.getCoordinateN( 2 ) );

    try
    {
      m_trianglePath = GeometryFactory.createGM_Triangle( pos1, pos2, pos3, crs );
    }
    catch( final GM_Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

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
      return m_polygon.intersects( point );
  }

  public double getCenterElevation( )
  {
    return centerElevation;// ring.getCoordinateN( 0 ).z;//Centroid().getCoordinate().z;
  }

  /**
   * @see org.kalypsodeegree.model.geometry.ISurfacePatchVisitable#acceptSurfacePatches(org.kalypsodeegree.model.geometry.GM_Envelope,
   *      org.kalypsodeegree.model.geometry.ISurfacePatchVisitor, org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public void acceptSurfacePatches( final GM_Envelope envToVisit, final ISurfacePatchVisitor<GM_SurfacePatch> surfacePatchVisitor, final IProgressMonitor monitor )
  {
    surfacePatchVisitor.visit( m_trianglePath, centerElevation );
    monitor.done();
  }

  public double getMinElevation( )
  {
    final Coordinate[] coordinates = m_ring.getCoordinates();
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
    final Coordinate[] coordinates = m_ring.getCoordinates();
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
    final Coordinate[] coordinates = m_ring.getCoordinates();

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
    final Coordinate[] coordinates = m_ring.getCoordinates();

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
    final Coordinate[] coords = m_ring.getCoordinates();

    // use relative plane equation and relative coords
    return JTSUtilities.calculateTriangleZ( relativePlaneEquation, x - coords[0].x, y - coords[0].y );
  }

  public LinearRing getRing( )
  {
    return m_ring;
  }

}