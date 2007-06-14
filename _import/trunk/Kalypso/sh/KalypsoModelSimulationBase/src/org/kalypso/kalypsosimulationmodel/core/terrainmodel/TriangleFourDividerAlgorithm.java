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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.kalypso.core.KalypsoCorePlugin;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree.model.geometry.ISurfacePatchVisitable;
import org.kalypsodeegree.model.geometry.ISurfacePatchVisitor;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LinearRing;

/**
 * @author Madanagopal
 * 
 * Divide the GM_Surface into Four GM_Surfaces using the Middle Points of Each Side of a GM_Surface (Triangle) to create
 * an another triangle. This approach would result in more number of Comparisions in dividing the triangle and much
 * efficient then the TriangleThreeDividerAlgorithm.java
 */
public class TriangleFourDividerAlgorithm implements ITriangleAlgorithm, ISurfacePatchVisitable<GM_SurfacePatch>
{
  private final LinearRing ring;

  private Map<GM_SurfacePatch, Double> toBeVisited = new HashMap<GM_SurfacePatch, Double>();

  public TriangleFourDividerAlgorithm( final LinearRing _ring )
  {
    this.ring = _ring;
  }

  /**
   * checks if further division of triangles is needed or not.
   */
  public boolean furtherDivisionNeeded( final GM_Position[] coOrds )
  {
    final double max = PlaneUtils.convertToTwoDecimals( ColorModelIntervalSingleton.getInstance().getInterval() );
    // System.out.println("max :" +max);
    final double _z1 = coOrds[0].getZ();
    final double _z2 = coOrds[1].getZ();
    final double _z3 = coOrds[2].getZ();

    final GM_Position _center = PlaneUtils.calculateCenterCoOrdinate( coOrds );

    /**
     * Implementation to check if the three points of a triangle form a Straight Line
     */

    final double _inX_1 = Math.abs( coOrds[0].getX() - coOrds[1].getX() );
    final double _inY_1 = Math.abs( coOrds[0].getY() - coOrds[1].getY() );
    final double _inZ_1 = Math.abs( coOrds[0].getZ() - coOrds[1].getZ() );

    final double _inX_2 = Math.abs( coOrds[1].getX() - coOrds[2].getX() );
    final double _inY_2 = Math.abs( coOrds[1].getY() - coOrds[2].getY() );
    final double _inZ_2 = Math.abs( coOrds[1].getZ() - coOrds[2].getZ() );

    /**
     * Represents - -2 | |x1| |x2| | | |y1| |y2| | | |z1| |z1| | - -
     */
    final double specA = PlaneUtils.convertToTwoDecimals( ((_inX_1 * _inX_2) + (_inY_1 * _inY_2) + (_inZ_1 * _inZ_2)) * ((_inX_1 * _inX_2) + (_inY_1 * _inY_2) + (_inZ_1 * _inZ_2)) );

    /**
     * Represents x1^2 + y1^2 + z1^2 * x2^2 + y2^2 + z2^2
     */
    final double specB = PlaneUtils.convertToTwoDecimals( ((_inX_1 * _inX_1) + (_inY_1 * _inY_1) + (_inZ_1 * _inZ_1)) * ((_inX_2 * _inX_2) + (_inY_2 * _inY_2) + (_inZ_2 * _inZ_2)) );

    /**
     * Checks if the difference between the elevations of the corners of GM_Surface with the elevation of the center is
     * more that the max, which represents the discretisation interval.
     */
    if( (Math.abs( _z1 - _center.getZ() ) >= max) || (Math.abs( _z2 - _center.getZ() ) >= max) || (Math.abs( _z3 - _center.getZ() ) >= max) )
    { // Checks if the Triangles form a straight Line
      if( specA != specB )
        return true;
    }
    return false;
  }

  /**
   * Given a GM_Surface, this method returns the List of divided GM_Surfaces that each representing an elevation with
   * the variation that should comply with standard Elevation Discretisation
   */
  public Map<GM_SurfacePatch, Double> visitThisDivisionSurface( final GM_SurfacePatch surfacePatch )
  {
    final CS_CoordinateSystem crs = KalypsoCorePlugin.getDefault().getCoordinatesSystem();

    final List<GM_Position[]> toSplit = new ArrayList<GM_Position[]>();
    final HashMap<GM_SurfacePatch, Double> notToSplit = new HashMap<GM_SurfacePatch, Double>();
    toSplit.add( surfacePatch.getExteriorRing() );
    while( !toSplit.isEmpty() )
    {
      final GM_Position[] splitCandidate = toSplit.remove( 0 );

      // System.out.println( "-----------------------------------------" );
      /**
       * Checks if a GM_Surface should be divided further
       */
      if( this.furtherDivisionNeeded( splitCandidate ) )
      {
        // System.out.println( "toSp" + Arrays.asList( splitCandidate ) );
        final GM_Position[] tri1 = new GM_Position[] { splitCandidate[0], PlaneUtils.calculateMidPoint( new GM_Position[] { splitCandidate[0], splitCandidate[1] } ),
            PlaneUtils.calculateMidPoint( new GM_Position[] { splitCandidate[0], splitCandidate[2] } ), splitCandidate[0] };
        final GM_Position[] tri2 = new GM_Position[] { PlaneUtils.calculateMidPoint( new GM_Position[] { splitCandidate[0], splitCandidate[1] } ), splitCandidate[1],
            PlaneUtils.calculateMidPoint( new GM_Position[] { splitCandidate[1], splitCandidate[2] } ), PlaneUtils.calculateMidPoint( new GM_Position[] { splitCandidate[0], splitCandidate[1] } ), };
        final GM_Position[] tri3 = new GM_Position[] { PlaneUtils.calculateMidPoint( new GM_Position[] { splitCandidate[0], splitCandidate[1] } ),
            PlaneUtils.calculateMidPoint( new GM_Position[] { splitCandidate[1], splitCandidate[2] } ), PlaneUtils.calculateMidPoint( new GM_Position[] { splitCandidate[0], splitCandidate[2] } ),
            PlaneUtils.calculateMidPoint( new GM_Position[] { splitCandidate[0], splitCandidate[1] } ), };
        final GM_Position[] tri4 = new GM_Position[] { PlaneUtils.calculateMidPoint( new GM_Position[] { splitCandidate[0], splitCandidate[2] } ),
            PlaneUtils.calculateMidPoint( new GM_Position[] { splitCandidate[1], splitCandidate[2] } ), splitCandidate[2],
            PlaneUtils.calculateMidPoint( new GM_Position[] { splitCandidate[0], splitCandidate[2] } ), };
        toSplit.add( tri1 );
        toSplit.add( tri2 );
        toSplit.add( tri3 );
        toSplit.add( tri4 );
        // System.out.println( "tri1" + Arrays.asList( tri1 ) );
        // System.out.println( "tri2" + Arrays.asList( tri2 ) );
        // System.out.println( "tri3" + Arrays.asList( tri3 ) );
        // System.out.println( "tri4" + Arrays.asList( tri4 ) );
      }
      else
      {
        try
        {
          /**
           * Creates a GM_Surface using the coordinates and adds it to the HashMap along with the center coordinate of
           * GM_Surface.
           */
          final GM_SurfacePatch patch = GeometryFactory.createGM_SurfacePatch( splitCandidate, HMOTerrainElevationModel.NO_INTERIOR_POS, null, crs );
          notToSplit.put( patch, PlaneUtils.calculateCenterCoOrdinate( splitCandidate ).getZ() );
        }
        catch( final Throwable e )
        {
          // TODO Auto-generated catch block
          e.printStackTrace();
        }
      }
    }

    return notToSplit;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.SurfacePatchVisitable#aceptSurfacePatches(org.kalypsodeegree.model.geometry.GM_Envelope,
   *      org.kalypso.kalypsosimulationmodel.core.terrainmodel.SurfacePatchVisitor)
   */
  public void acceptSurfacePatches( final GM_Envelope envToVisit, final ISurfacePatchVisitor surfacePatchVisitor ) throws GM_Exception
  {
    final Coordinate[] coordinates = ring.getCoordinates();
    final double[] exterior = { coordinates[0].x, coordinates[0].y, coordinates[0].z, coordinates[1].x, coordinates[1].y, coordinates[1].z, coordinates[2].x, coordinates[2].y, coordinates[2].z,
        coordinates[0].x, coordinates[0].y, coordinates[0].z };

    final CS_CoordinateSystem crs = KalypsoCorePlugin.getDefault().getCoordinatesSystem();
    final GM_SurfacePatch surfacePatch = GeometryFactory.createGM_SurfacePatch( exterior, HMOTerrainElevationModel.NO_INTERIOR, 3, crs );

    /**
     * toBeVisited HashMap is cached..this prevents from recalculating the Triangles from a Given Surface
     */
    if( toBeVisited.isEmpty() )
      toBeVisited = visitThisDivisionSurface( surfacePatch );

    try
    {
      /**
       * iterates through the complete HashMap and paints all the triangles
       */
      for( final Map.Entry<GM_SurfacePatch, Double> entry : toBeVisited.entrySet() )
      {
        surfacePatchVisitor.visit( entry.getKey(), entry.getValue() );
      }
    }
    catch( final Exception e )
    {
      throw new GM_Exception( e.getLocalizedMessage(), e );
    }
  }

}