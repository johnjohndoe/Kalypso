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
package org.kalypsodeegree_impl.tools.refinement;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.model.geometry.GM_Ring_Impl;
import org.kalypsodeegree_impl.model.geometry.GM_Triangle_Impl;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Polygon;

/**
 * TODO: move to the utils of JTS, deegree...
 * 
 * @author jung
 */
public class RefinementUtils
{
  private static final GeometryFactory GF = new GeometryFactory();

  private static double CHOP_THICKNESS = .000001;

  /**
   * Cut geom into pieces at the given line segment. <br>
   * This is a hack: we subtract a thin polygon from the given geom, and so will introduce a small gap between polygons.
   * Further, if the segment ends inside the polygon, this will introduce a thin slot.
   * 
   * @author http://lists.jump-project.org/pipermail/jts-devel/2002-October/000119.html
   */
  public static Geometry chop( final Geometry geom, final Coordinate[] coords )
  {
    Geometry chopline = GF.createLineString( coords );
// Geometry chopline = new LineString( chopverts, new PrecisionModel(), 0 );
    chopline = chopline.buffer( CHOP_THICKNESS );
    final Geometry chopped = geom.difference( chopline );

    return chopped;
  }

  public static Geometry exportGeometry( final GM_Position[] poses )
  {
    final Coordinate[] coords = JTSAdapter.export( poses );
    final LinearRing ring = GF.createLinearRing( coords );
    final Polygon polygon = GF.createPolygon( ring, null );
    return polygon;
  }

  /**
   * splits a given surface into several surfaces by a line given by its positions at the intersection point(s) with the
   * surface
   */
  @SuppressWarnings("unchecked")
  public static GM_Surface[] splitPolygonbyLine( final GM_Position[] patchPoses, final GM_Position[] linePoses, final String crs ) throws GM_Exception
  {
    final List<GM_Surface<GM_SurfacePatch>> surfaceList = new ArrayList<GM_Surface<GM_SurfacePatch>>();
    final Set<GM_Position> originalPosList = new HashSet<GM_Position>();
    final List<Coordinate> coordList = new ArrayList<Coordinate>();

    /* EQUALITY CHECK */
    /* if the split-line is identical with one of the patch's border segments, do nothing */
    final GM_Curve[] segments = getPositionsAsCurves( patchPoses, crs );
    if( checkForEqualSegment( linePoses, segments, crs ) )
      surfaceList.toArray( new GM_Surface[surfaceList.size()] );

    /* START */
    /* store the original coodinates in a set in order to re-assign the coordinates later */
    for( final GM_Position position : patchPoses )
      originalPosList.add( position );
    for( final GM_Position position : linePoses )
    {
      originalPosList.add( position );
      coordList.add( JTSAdapter.export( position ) );
    }

    /* convert input data into JTS formats */
    final Geometry geom = exportGeometry( patchPoses );

    final Coordinate[] coords = coordList.toArray( new Coordinate[coordList.size()] );

    /* SPLIT */
    // REMARK: the returned geometries do not have exactly the same coords as before (because of the buffer)!
    // so we have to re-assign the original positions again.
    final Geometry geometry = chop( geom, coords );

    if( geometry instanceof MultiPolygon )
    {
      final MultiPolygon multi = (MultiPolygon) geometry;
      for( int i = 0; i < multi.getNumGeometries(); i++ )
      {
        final Geometry polygon = multi.getGeometryN( i );
        final GM_Object object = JTSAdapter.wrap( polygon );
        if( object instanceof GM_Surface )
        {
          final GM_Surface<GM_SurfacePatch> surface = (GM_Surface) object;

          for( final GM_SurfacePatch patch : surface )
          {
            final GM_Position[] ring = patch.getExteriorRing();

            /* RE-ASSIGNMENT */
            // check for each position, if it lies within a given search radius of an original position
            final GM_Surface origPosSurface = reassignOriginalPositions( originalPosList, ring, crs );
            surfaceList.add( origPosSurface );
          }
        }
      }
    }

    return surfaceList.toArray( new GM_Surface[surfaceList.size()] );
  }

  private static GM_Surface<GM_SurfacePatch> reassignOriginalPositions( final Set<GM_Position> originalPosList, final GM_Position[] ring, final String crs ) throws GM_Exception
  {
    final List<GM_Position> posList = new ArrayList<GM_Position>();

    for( GM_Position position : ring )
    {
      if( !originalPosList.contains( position ) )
      {
        final GM_Position[] origPoses = originalPosList.toArray( new GM_Position[originalPosList.size()] );
        for( final GM_Position origPos : origPoses )
        {
          if( origPos.getDistance( position ) <= CHOP_THICKNESS * 10 )
          {
            position = origPos;
            break;
          }
        }
      }
      if( !posList.contains( position ) )
        posList.add( position );
    }
    if( posList.size() > 0 )
    {
      /* close ring */
      posList.add( posList.get( 0 ) );
      final GM_Position[] poses = posList.toArray( new GM_Position[posList.size()] );
      final GM_Ring_Impl origRing = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Ring( poses, crs );
      final GM_SurfacePatch patch = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_SurfacePatch( origRing, null, crs );
      return org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Surface( patch );
    }
    else
      return null;
  }

  public static GM_Surface<GM_SurfacePatch> getSurface( final GM_Position[] poses, final String crs ) throws GM_Exception
  {
    final GM_Ring_Impl ring = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Ring( poses, crs );

    final GM_SurfacePatch patch = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_SurfacePatch( ring, null, crs );

    return org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Surface( patch );
  }

  /**
   * get the several patch segments
   */
  public static GM_Curve[] getPositionsAsCurves( final GM_Position[] exteriorRing, final String crs ) throws GM_Exception
  {
    final GM_Curve[] curves = new GM_Curve[exteriorRing.length - 1];
    for( int i = 0; i < exteriorRing.length - 1; i++ )
    {
      final GM_Position[] positions = new GM_Position[2];
      positions[0] = exteriorRing[i];
      positions[1] = exteriorRing[i + 1];
      final GM_Curve curve = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Curve( positions, crs );
      curves[i] = curve;
    }
    return curves;
  }

  private static boolean checkForEqualSegment( final GM_Position[] poses, final GM_Curve[] segments, final String crs ) throws GM_Exception
  {
    final GM_Curve pointSegment = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Curve( poses, crs );

    for( int i = 0; i < segments.length; i++ )
    {
      if( segments[i].equals( pointSegment ) )
        return true;
    }

    // check also for a flipped point segment
    final GM_Curve flippedSegment = flipCurve( pointSegment );
    for( int i = 0; i < segments.length; i++ )
    {
      if( segments[i].equals( flippedSegment ) )
        return true;
    }

    return false;

  }

  private static GM_Curve flipCurve( final GM_Curve curve ) throws GM_Exception
  {
    final List<GM_Position> posList = new ArrayList<GM_Position>();
    for( int i = curve.getAsLineString().getNumberOfPoints() - 1; i == 0; i-- )
    {
      posList.add( curve.getAsLineString().getPositionAt( i ) );
    }

    final GM_Position[] poses = posList.toArray( new GM_Position[posList.size()] );
    return org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Curve( poses, curve.getCoordinateSystem() );
  }

  /**
   * Examines if a point lies on or very close to a curve (given by its positions) and interpolates the z value for the
   * given point.
   * 
   * @param point
   *          The point for which we like to interpolate the z value
   * @param poses
   *          the positions of the curve
   */
  public static GM_Point interpolateZ( final GM_Point point, final GM_Position[] poses ) throws GM_Exception
  {
    final String crs = point.getCoordinateSystem();
    final GM_Curve[] curves = RefinementUtils.getPositionsAsCurves( poses, crs );

    for( final GM_Curve curve : curves )
    {
      final GM_Object object = curve.intersection( point );
      if( object != null || (curve.distance( point ) < CHOP_THICKNESS * 2) )
      {
        final GM_Point startPoint = curve.getAsLineString().getStartPoint();
        final GM_Point endPoint = curve.getAsLineString().getEndPoint();

        // REMARK: here we have to calculate everything in a 2d way

        final Double dz = endPoint.getZ() - startPoint.getZ();

        final double distanceStartPoint = startPoint.distance( point );
        final double distanceStartEnd = startPoint.distance( endPoint );
        final double z = startPoint.getZ() + dz / distanceStartEnd * distanceStartPoint;

        return org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Point( point.getX(), point.getY(), z, crs );
      }
    }
    return point;
  }

  /**
   * Splits a {@link GM_SurfacePatch} with a line, that is defined by the positions of its intersection points with the
   * hull of the patch.
   * 
   * @param surfacePatch
   *          the surface patch
   * @param poses
   *          the positions of the intersection points
   * @return the resulting surfaces
   */
  @SuppressWarnings("unchecked")
  public static GM_Surface[] splitSurfacePatch( final GM_SurfacePatch surfacePatch, final GM_Position[] linePoses ) throws GM_Exception
  {
    final List<GM_Surface<GM_SurfacePatch>> surfaceList = new ArrayList<GM_Surface<GM_SurfacePatch>>();

    final GM_Position[] patchPoses = surfacePatch.getExteriorRing();

    final String crs = surfacePatch.getCoordinateSystem();
    final GM_Surface[] surfaces = RefinementUtils.splitPolygonbyLine( patchPoses, linePoses, crs );

    for( int i = 0; i < surfaces.length; i++ )
      surfaceList.add( surfaces[i] );

    return surfaceList.toArray( new GM_Surface[surfaceList.size()] );
  }

  @SuppressWarnings("unchecked")
  public static GM_Surface<GM_SurfacePatch>[] triangulatePolygon( final String crs, final GM_Position[] ring ) throws GM_Exception
  {
    final List<GM_Surface<GM_SurfacePatch>> surfaceList = new ArrayList<GM_Surface<GM_SurfacePatch>>();
    final GM_Position[] orientedRing = GeometryUtilities.orientateRing( ring );
    final GM_Position[][] triangles = GeometryUtilities.triangulateRing( orientedRing );
    for( final GM_Position[] poses : triangles )
    {
      final GM_Triangle_Impl gmTriangle = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Triangle( poses, crs );
      surfaceList.add( org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Surface( gmTriangle ) );
    }
    return surfaceList.toArray( new GM_Surface[surfaceList.size()] );
  }

}
