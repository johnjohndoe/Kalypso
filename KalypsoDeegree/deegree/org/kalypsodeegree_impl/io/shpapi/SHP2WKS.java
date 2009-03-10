/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 *
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always.
 *
 * If you intend to use this software in other ways than in kalypso
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree,
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree_impl.io.shpapi;

import java.util.ArrayList;

import org.kalypso.jts.JTSUtilities;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_CurveSegment;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.algorithm.PointInRing;
import com.vividsolutions.jts.algorithm.SIRtreePointInRing;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Polygon;

/**
 * the class SHP2WKS transforms a polygon structure read from a shape-file <BR>
 * into a WKSLinearPolygon specified by the sf-specifications <BR>
 * <P>
 * <B>Last changes <B>: <BR>
 * 14.12.1999 ap: import clauses added <BR>
 * 08.02.2000 ap: method transformPoint(..) declared and implemented <BR>
 * 21.03.2000 ap: method: transformMultiPoint(..) declared and implemented <BR>
 * 21.03.2000 ap: method: transformPolyLine(..) declared and implemented <BR>
 * <!------------------------------------------------------------------------>
 * 
 * @version 21.03.2000
 * @author Andreas Poth
 */
public class SHP2WKS
{
  private final static com.vividsolutions.jts.geom.GeometryFactory GF = new com.vividsolutions.jts.geom.GeometryFactory();

  /**
   * method: GM_Point transformPoint(CS_CoordinateSystem srs, <BR>
   * SHPPoint shppoint)) <BR>
   * transforms a SHPPoint to a WKSGeometry <BR>
   * gets a point that should be transformed <BR>
   */
  public GM_Point transformPoint( final String crs, final SHPPoint shppoint )
  {
    return GeometryFactory.createGM_Point( shppoint.getX(), shppoint.getY(), crs );
  }

  /**
   * method: GM_Point transformPointz(CS_CoordinateSystem srs, <BR>
   * SHPPointz shppointz)) <BR>
   * transforms a SHPPointz to a WKSGeometry <BR>
   * gets a pointz that should be transformed <BR>
   */
  public GM_Point transformPointz( final String crs, final SHPPointz shppointz )
  {
    // return GeometryFactory.createGM_Point( shppointz.x, shppointz.y, shppointz.z, crs );
    return GeometryFactory.createGM_Point( shppointz.getX(), shppointz.getY(), shppointz.getZ(), crs );
  }

  /**
   * method: GM_Point[] transformMultiPoint(CS_CoordinateSystem srs, <BR>
   * SHPMultiPoint shpmultipoint)) <BR>
   * transforms a SHPMultiPoint to a WKSGeometry <BR>
   * gets a multipoint that should be transformed <BR>
   */
  public GM_Point[] transformMultiPoint( final String srs, final SHPMultiPoint shpmultipoint )
  {
    final GM_Point[] gm_points = new GM_Point[shpmultipoint.numPoints];

    for( int i = 0; i < shpmultipoint.numPoints; i++ )
      gm_points[i] = GeometryFactory.createGM_Point( shpmultipoint.points[i].getX(), shpmultipoint.points[i].getY(), srs );

    return gm_points;
  }

  /**
   * method: GM_Point[] transformMultiPointz(CS_CoordinateSystem srs, <BR>
   * SHPMultiPointz shpmultipointz)) <BR>
   * transforms a SHPMultiPointz to a WKSGeometry <BR>
   * gets a multipointz that should be transformed <BR>
   */
  public GM_Point[] transformMultiPointz( final String srs, final SHPMultiPointz shpmultipointz )
  {
    final GM_Point[] gm_points = new GM_Point[shpmultipointz.numPoints];

    for( int i = 0; i < shpmultipointz.numPoints; i++ )
      gm_points[i] = GeometryFactory.createGM_Point( shpmultipointz.pointsz[i].getX(), shpmultipointz.pointsz[i].getY(), shpmultipointz.pointsz[i].getZ(), srs );

    return gm_points;
  }

  /**
   * method: GM_Point[][] transformPolyLine(CS_CoordinateSystem srs, <BR>
   * SHPPolyLine shppolyline)) <BR>
   * transforms a SHPPolyLine to a WKSGeometry <BR>
   * gets a polyline that should be transformed <BR>
   */
  public GM_Curve[] transformPolyLine( final String crs, final SHPPolyLine shppolyline )
  {
    final GM_Curve[] curve = new GM_Curve[shppolyline.numParts];

    try
    {
      for( int j = 0; j < shppolyline.numParts; j++ )
      {
        final GM_Position[] gm_points = new GM_Position[shppolyline.points[j].length];

        for( int i = 0; i < shppolyline.points[j].length; i++ )
        {
          gm_points[i] = GeometryFactory.createGM_Position( shppolyline.points[j][i].getX(), shppolyline.points[j][i].getY() );
        }

        final GM_CurveSegment cs = GeometryFactory.createGM_CurveSegment( gm_points, crs );
        curve[j] = GeometryFactory.createGM_Curve( cs );
      }
    }
    catch( final Exception e )
    {
      System.out.println( "SHP2WKS::" + e );
    }

    return curve;
  }

  /**
   * method: GM_Point[][] transformPolyLinez(CS_CoordinateSystem srs, <BR>
   * SHPPolyLinez shppolylinez)) <BR>
   * transforms a SHPPolyLinez to a WKSGeometry <BR>
   * gets a polylinez that should be transformed <BR>
   */
  public GM_Curve[] transformPolyLinez( final String crs, final SHPPolyLinez shpPolyLineZ )
  {
    final GM_Curve[] curve = new GM_Curve[shpPolyLineZ.getNumParts()];

    try
    {
      for( int j = 0; j < shpPolyLineZ.getNumParts(); j++ )
      {
        final SHPPointz[][] pointsz = shpPolyLineZ.getPointsz();
        final GM_Position[] gm_points = new GM_Position[pointsz[j].length];

        for( int i = 0; i < pointsz[j].length; i++ )
        {
          gm_points[i] = GeometryFactory.createGM_Position( pointsz[j][i].getX(), pointsz[j][i].getY(), pointsz[j][i].getZ() );
        }

        final GM_CurveSegment cs = GeometryFactory.createGM_CurveSegment( gm_points, crs );
        curve[j] = GeometryFactory.createGM_Curve( cs );
      }
    }
    catch( final Exception e )
    {
      System.out.println( "SHP2WKS::" + e );
    }

    return curve;
  }

  /**
   * method: private boolean isInsideRing(GM_Point[] ring, GM_Point point) <BR>
   * checks if a point is inside a polygon. the algorithm is taken from: <BR>
   * http://www.ics.uci.edu/~eppstein/161/960307.html#intest <BR>
   * 
   * @deprecated This method does not work properly!
   */
  @Deprecated
  private boolean isInsideRing( final GM_Position[] ring, final GM_Position point )
  {
    int crossings = 0;

    for( int i = 0; i < ring.length; i++ )
    {
      int z = i + 1;

      if( (i + 1) >= ring.length )
      {
        z = 0;
      }

      // check if point.x is between x of vertex i and z of ring
      if( (ring[i].getX() < point.getX() && point.getX() < ring[z].getX()) || (ring[i].getX() > point.getX() && point.getX() > ring[z].getX()) )
      {
        final double t = (point.getX() - ring[z].getX()) / (ring[i].getX() - ring[z].getX());

        final double cy = (t * ring[i].getY()) + ((1 - t) * ring[z].getY());

        if( point.getY() == cy )
          return false;
        else if( point.getY() > cy )
        { // downwards vertical line through point crosses ring
          crossings++;
        }
      }

      // check if point.x equals x of vertex i of ring while point.y > ring[i].y
      if( (ring[i].getX() == point.getX()) && (ring[i].getY() <= point.getY()) )
      {

        if( ring[i].getY() == point.getY() )
          return false;

        // find next point on ring with different x
        // (adjacent points in shapefile can have equal x&y)
        while( ring[z].getX() == point.getX() )
        {
          z += 1;
          if( z == ring.length )
            z = 0;
        }

        // find previous point on ring with different x
        int zz = i - 1;
        if( zz < 0 )
          zz = ring.length - 1;
        while( ring[zz].getX() == point.getX() )
        {
          zz -= 1;
          if( zz < 0 )
            zz = ring.length - 1;
        }

        // if point.x between previous and next x then crossing
        if( ring[z].getX() < point.getX() && point.getX() < ring[zz].getX() || ring[z].getX() > point.getX() && point.getX() > ring[zz].getX() )
        {
          crossings++;
        }

      }
    }

    if( (crossings % 2) != 0 )
      return true;

    return false;
  }

  /**
   * transforms the SHPPolygon to a WKSGeometry <BR>
   * gets the polygon that should be transformed <BR>
   */
  public GM_Surface[] transformPolygon( final String crs, final SHPPolygon shppolygon )
  {
    // final Map<LinearRing, PointInRing> pirs = new HashMap<LinearRing, PointInRing>();
    final ArrayList<LinearRing> outer_rings = new ArrayList<LinearRing>( shppolygon.numRings );
    final ArrayList<LinearRing> inner_rings = new ArrayList<LinearRing>( shppolygon.numRings );

    for( int i = 0; i < shppolygon.numRings; i++ )
    {
      final Coordinate[] ring = new Coordinate[shppolygon.m_rings.points[i].length];

      for( int k = 0; k < shppolygon.m_rings.points[i].length; k++ )
        ring[k] = new Coordinate( shppolygon.m_rings.points[i][k].getX(), shppolygon.m_rings.points[i][k].getY() );

      // note: esris (unmathemathic) definition of positive area is clockwise => outer ring, negative => inner ring
      final double area = JTSUtilities.calcSignedAreaOfRing( ring );
      final double esriArea = -area;

      final LinearRing linearRing = GF.createLinearRing( ring );
      if( esriArea >= 0 )
        outer_rings.add( linearRing );
      else
        inner_rings.add( linearRing );

      // pirs.put( linearRing, new SIRtreePointInRing( linearRing ) );

    }

    final ArrayList<GM_Surface> wkslp = new ArrayList<GM_Surface>();
    for( int i = 0; i < outer_rings.size(); i++ )
    {
      final LinearRing out_ring = outer_rings.get( i );

      final int count = inner_rings.size() - 1;
      final ArrayList<LinearRing> list = new ArrayList<LinearRing>( count + 2 );
      // find inner rings of the current outter ring
      final PointInRing pir = new SIRtreePointInRing( out_ring );
      // pirs.get( out_ring );

      for( int k = count; k >= 0; k-- )
      {
        final LinearRing in_ring = inner_rings.get( k );

        // TODO why?
        // if( pir.isInside( in_ring.getCoordinateN( 0 ) ) )
        // {
        list.add( inner_rings.get( k ) );
        // }
      }
      for( final LinearRing ring : list )
        inner_rings.remove( ring );
      final LinearRing[] inrings = list.toArray( new LinearRing[list.size()] );

      try
      {
        final Polygon polygon = GF.createPolygon( out_ring, inrings );
        final GM_Surface sur = (GM_Surface) JTSAdapter.wrap( polygon );
        sur.setCoordinateSystem( crs );
        wkslp.add( sur );
      }
      catch( final Exception e )
      {
        System.out.println( "SHP2WKS:: transformPolygon\n" + e );
      }
    }

    return wkslp.toArray( new GM_Surface[wkslp.size()] );
  }

  /**
   * transforms the SHPPolygon to a WKSGeometry <BR>
   * gets the polygon that should be transformed <BR>
   */
  public GM_Surface[] transformPolygonz( final String crs, final SHPPolygonz shppolygonz )
  {
    // final Map<LinearRing, PointInRing> pirs = new HashMap<LinearRing, PointInRing>();
    final ArrayList<LinearRing> outer_rings = new ArrayList<LinearRing>( shppolygonz.getNumRings() );
    final ArrayList<LinearRing> inner_rings = new ArrayList<LinearRing>( shppolygonz.getNumRings() );

    for( int i = 0; i < shppolygonz.getNumRings(); i++ )
    {
      final SHPPointz[][] pointsz = shppolygonz.getRings().getPointsz();
      final Coordinate[] ring = new Coordinate[pointsz[i].length];

      for( int k = 0; k < pointsz[i].length; k++ )
        ring[k] = new Coordinate( pointsz[i][k].getX(), pointsz[i][k].getY(), pointsz[i][k].getZ() );

      // note: esris (unmathemathic) definition of positive area is clockwise => outer ring, negative => inner ring
      final double area = JTSUtilities.calcSignedAreaOfRing( ring );
      final double esriArea = -area;

      final LinearRing linearRing = GF.createLinearRing( ring );
      if( esriArea >= 0 )
        outer_rings.add( linearRing );
      else
        inner_rings.add( linearRing );

      // pirs.put( linearRing, new SIRtreePointInRing( linearRing ) );

    }

    final ArrayList<GM_Surface> wkslp = new ArrayList<GM_Surface>();
    for( int i = 0; i < outer_rings.size(); i++ )
    {
      final LinearRing out_ring = outer_rings.get( i );

      final int count = inner_rings.size() - 1;
      final ArrayList<LinearRing> list = new ArrayList<LinearRing>( count + 2 );
      // find inner rings of the current outter ring
      final PointInRing pir = new SIRtreePointInRing( out_ring );
      // pirs.get( out_ring );

      for( int k = count; k >= 0; k-- )
      {
        final LinearRing in_ring = inner_rings.get( k );
        if( pir.isInside( in_ring.getCoordinateN( 0 ) ) )
        {
          list.add( inner_rings.get( k ) );
        }
      }
      for( final LinearRing ring : list )
        inner_rings.remove( ring );
      final LinearRing[] inrings = list.toArray( new LinearRing[list.size()] );

      try
      {
        final Polygon polygon = GF.createPolygon( out_ring, inrings );
        final GM_Surface sur = (GM_Surface) JTSAdapter.wrap( polygon );
        sur.setCoordinateSystem( crs );
        wkslp.add( sur );
      }
      catch( final Exception e )
      {
        System.out.println( "SHP2WKS:: transformPolygonz\n" + e );
      }
    }

    return wkslp.toArray( new GM_Surface[wkslp.size()] );
  }
} // end of class WKB2WKS
