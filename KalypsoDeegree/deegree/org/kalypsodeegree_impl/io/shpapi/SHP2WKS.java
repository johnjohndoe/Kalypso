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
package org.deegree_impl.io.shpapi;

import java.util.ArrayList;

import org.deegree.model.geometry.GM_Curve;
import org.deegree.model.geometry.GM_CurveSegment;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Point;
import org.deegree.model.geometry.GM_Position;
import org.deegree.model.geometry.GM_Surface;
import org.deegree_impl.model.geometry.GM_SurfaceInterpolation_Impl;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * 
 * the class SHP2WKS transforms a polygon structure read from a shape-file <BR>
 * into a WKSLinearPolygon specified by the sf-specifications <BR>
 * 
 * <P>
 * <B>Last changes <B>: <BR>
 * 14.12.1999 ap: import clauses added <BR>
 * 08.02.2000 ap: method transformPoint(..) declared and implemented <BR>
 * 21.03.2000 ap: method: transformMultiPoint(..) declared and implemented <BR>
 * 21.03.2000 ap: method: transformPolyLine(..) declared and implemented <BR>
 * 
 * <!------------------------------------------------------------------------>
 * 
 * @version 21.03.2000
 * @author Andreas Poth
 */
public class SHP2WKS
{

  /**
   * method: GM_Point transformPoint(CS_CoordinateSystem srs, <BR>
   * SHPPoint shppoint)) <BR>
   * transforms a SHPPoint to a WKSGeometry <BR>
   * gets a point that should be transformed <BR>
   */
  public GM_Point transformPoint( CS_CoordinateSystem crs, SHPPoint shppoint )
  {
    return GeometryFactory.createGM_Point( shppoint.x, shppoint.y, crs );
  }

  /**
   * method: GM_Point[] transformMultiPoint(CS_CoordinateSystem srs, <BR>
   * SHPMultiPoint shpmultipoint)) <BR>
   * transforms a SHPMultiPoint to a WKSGeometry <BR>
   * gets a multipoint that should be transformed <BR>
   */
  public GM_Point[] transformMultiPoint( CS_CoordinateSystem srs, SHPMultiPoint shpmultipoint )
  {
    GM_Point[] gm_points = new GM_Point[shpmultipoint.numPoints];

    for( int i = 0; i < shpmultipoint.numPoints; i++ )
      gm_points[i] = GeometryFactory.createGM_Point( shpmultipoint.points[i].x,
          shpmultipoint.points[i].y, srs );

    return gm_points;
  }

  /**
   * method: GM_Point[][] transformPolyLine(CS_CoordinateSystem srs, <BR>
   * SHPPolyLine shppolyline)) <BR>
   * transforms a SHPPolyLine to a WKSGeometry <BR>
   * gets a polyline that should be transformed <BR>
   */
  public GM_Curve[] transformPolyLine( CS_CoordinateSystem crs, SHPPolyLine shppolyline )
  {
    GM_Curve[] curve = new GM_Curve[shppolyline.numParts];

    try
    {
      for( int j = 0; j < shppolyline.numParts; j++ )
      {
        GM_Position[] gm_points = new GM_Position[shppolyline.points[j].length];

        for( int i = 0; i < shppolyline.points[j].length; i++ )
        {
          gm_points[i] = GeometryFactory.createGM_Position( shppolyline.points[j][i].x,
              shppolyline.points[j][i].y );
        }

        GM_CurveSegment cs = GeometryFactory.createGM_CurveSegment( gm_points, crs );
        curve[j] = GeometryFactory.createGM_Curve( cs );
      }
    }
    catch( Exception e )
    {
      System.out.println( "SHP2WKS::" + e );
    }

    return curve;
  }

  /**
   * method: getRange(LinearRing ring, int[] pts) <BR>
   * returns the range envelope of a polygon. after calling getRange <BR>
   * the array pts contains the indieces of the points corresponding <BR>
   * to the left, top, right and bottom of the range envelope. <BR>
   */
  private GM_Envelope getRange( SHPPoint[] ring, int[] pts )
  {
    double minx = ring[0].x;
    double miny = ring[0].y;
    double maxx = ring[0].x;
    double maxy = ring[0].y;

    for( int i = 1; i < ( ring.length - 1 ); i++ )
    {
      if( ring[i].x < minx )
      {
        minx = ring[i].x;
        pts[0] = i;
      }
      else if( ring[i].x > maxx )
      {
        maxx = ring[i].x;
        pts[2] = i;
      }

      if( ring[i].y < miny )
      {
        miny = ring[i].y;
        pts[1] = i;
      }
      else if( ring[i].y > maxy )
      {
        maxy = ring[i].y;
        pts[3] = i;
      }
    }

    return GeometryFactory.createGM_Envelope( minx, miny, maxx, maxy );
  }

  /**
   * 
   * 
   * @param ring
   * 
   * @return
   */
  private boolean ccw( SHPPoint[] ring )
  {

    int p0;
    int p1;
    int p2;
    double dx1;
    double dx2;
    double dy1;
    double dy2;
    int[] pts = new int[4];
    boolean loop = false;

    p1 = pts[0];

    do
    {
      p0 = p1 - 1;

      /*
       * happens if p1 is the first point of the ring
       */
      if( p0 < 0 )
      {
        p0 = ring.length - 2;
      }

      /*
       * p2 have not to be checked like p0. because of the loop used in
       * getRange() p2 can't be larger then ring.numPoints-1.
       */
      p2 = p1 + 1;

      dx1 = ring[p1].x - ring[p0].x;
      dy1 = ring[p1].y - ring[p0].y;
      dx2 = ring[p2].x - ring[p0].x;
      dy2 = ring[p2].y - ring[p0].y;

      /*
       * are the three points collinear in vertical or horizontal level?
       */
      if( ( ( dx1 == 0 ) && ( dx2 == 0 ) ) || ( ( dy1 == 0 ) && ( dy2 == 0 ) ) )
      {
        p1++;

        if( p1 > ( ring.length - 2 ) )
        {
          p1 = 0;
        }
      }
      else
      {
        loop = true;
      }
    }
    while( loop == false );

    /*
     * taken from sedgewick
     */
    if( ( dx1 * dy2 ) > ( dy1 * dx2 ) )
    {
      return false;
    }

    if( ( dx1 * dy2 ) < ( dy1 * dx2 ) )
    {
      return true;
    }

    if( ( dx1 * dx2 < 0 ) || ( dy1 * dy2 < 0 ) )
    {
      return true;
    }

    if( ( dx1 * dx1 + dy1 * dy1 ) < ( dx2 * dx2 + dy2 * dy2 ) )
    {
      return false;
    }

    return false;
  }

  /**
   * method: private boolean isInsideRing(GM_Point[] ring, GM_Point point) <BR>
   * checks if a point is inside a polygon. the algorithm is taken from: <BR>
   * http://www.ics.uci.edu/~eppstein/161/960307.html#intest <BR>
   */
  private boolean isInsideRing( GM_Position[] ring, GM_Position point )
  {
    int crossings = 0;

    for( int i = 0; i < ring.length; i++ )
    {
      int z = i + 1;

      if( ( i + 1 ) >= ring.length )
      {
        z = 0;
      }

      //check if point.x is between x of vertex i and z of ring
      if( ( ring[i].getX() < point.getX() && point.getX() < ring[z].getX() )
          || ( ring[i].getX() > point.getX() && point.getX() > ring[z].getX() ) )
      {
        double t = ( point.getX() - ring[z].getX() ) / ( ring[i].getX() - ring[z].getX() );

        double cy = ( t * ring[i].getY() ) + ( ( 1 - t ) * ring[z].getY() );

        if( point.getY() == cy )
        { //point is on border of ring
          return false;
        }
        else if( point.getY() > cy )
        { //downwards vertical line
          // through point crosses ring
          crossings++;
        }
      }

      //check if point.x equals x of vertex i of ring while point.y >
      // ring[i].y
      if( ( ring[i].getX() == point.getX() ) && ( ring[i].getY() <= point.getY() ) )
      {

        if( ring[i].getY() == point.getY() )
        { //point is on border of
          // ring
          return false;
        }

        //find next point on ring with different x
        // (adjacent points in shapefile can have equal x&y)
        while( ring[z].getX() == point.getX() )
        {
          z += 1;
          if( z == ring.length )
            z = 0;
        }

        //find previous point on ring with different x
        int zz = i - 1;
        if( zz < 0 )
          zz = ring.length - 1;
        while( ring[zz].getX() == point.getX() )
        {
          zz -= 1;
          if( zz < 0 )
            zz = ring.length - 1;
        }

        //if point.x between previous and next x then crossing
        if( ring[z].getX() < point.getX() && point.getX() < ring[zz].getX()
            || ring[z].getX() > point.getX() && point.getX() > ring[zz].getX() )
        {
          crossings++;
        }

      }
    }

    if( ( crossings % 2 ) != 0 )
    {
      return true;
    }
    else
    {
      return false;
    }
  }

  //    
  //    /**
  //     * method: WKSLinearPolygon[] transformPolygon(CS_CoordinateSystem
  // srs,<BR>
  //     * SHPPolygon shppolygon))<BR>
  //     * transforms the SHPPolygon to a WKSGeometry<BR>
  //     * gets the polygon that should be transformed<BR>
  //     */
  //    public GM_Surface[] transformPolygon2(CS_CoordinateSystem crs,
  //    SHPPolygon shppolygon) {
  //        
  //        ArrayList polygons = new ArrayList(shppolygon.numRings);
  //        ArrayList outer_rings = new ArrayList(shppolygon.numRings);
  //        ArrayList inner_rings = new ArrayList(shppolygon.numRings);
  //        GM_Position[][] in_rings = null;
  //        
  //        int cnt = 0;
  //        for (int i = 0; i < shppolygon.numRings; i++) {
  //            
  //            GM_Position[] ring = new GM_Position[shppolygon.rings.points[i].length];
  //            for (int j = 0; j < shppolygon.rings.points[i].length; j++) {
  //                ring[j] =
  // GeometryFactory.createGM_Position(shppolygon.rings.points[i][j].x,
  //                                                    shppolygon.rings.points[i][j].y);
  //            }
  //            
  //            if ( (shppolygon.numRings == 1) || ccw(shppolygon.rings.points[i]) ) {
  //                outer_rings.add( ring );
  //            } else {
  //                inner_rings.add( ring );
  //            }
  //            
  //        }
  //        
  //        GM_Surface[] wkslp = new GM_Surface[outer_rings.size()];
  //        
  //        // for every outer ring
  //        for (int i = 0; i < outer_rings.size(); i++) {
  //            
  //            GM_Position[] out_rings = (GM_Position[]) outer_rings.get(i);
  //            
  //            ArrayList innerRingsElements = new ArrayList(inner_rings.size());
  //            
  //            // for ever remaining inner ring check if it's inside
  //            // the actual outer ring
  //            int count = inner_rings.size()-1;
  //            for (int j = count; j >= 0; j--) {
  //                
  //                GM_Position[] inring = (GM_Position[]) inner_rings.get(j);
  //                
  //                // check if one or more points of a inner ring are
  //                // within the actual otter ring
  //                try {
  //                    
  //                    if ( (isInsideRing(out_rings,inring[0]) == true) ||
  //                    (isInsideRing(out_rings,inring[1]) == true) ){
  //                        innerRingsElements.add((Object) inner_rings.get(j));
  //                        inner_rings.remove(j);
  //                    }
  //                    
  //                } catch (Exception e) {
  //                    System.out.println( "Error: " + e.toString());
  //                }
  //                
  //                in_rings = null;
  //                
  //                // build inner rings of the actual outer ring as GM_Position[]
  //                if (innerRingsElements.size() > 0) {
  //                    
  //                    in_rings = new GM_Position[innerRingsElements.size()][];
  //                    
  //                    for (int k = 0; k < innerRingsElements.size(); k++) {
  //                        
  //                        in_rings[k] = (GM_Position[]) innerRingsElements.get(k);
  //                        
  //                    }
  //                    
  //                }
  //                
  //            } // end of building inner rings as GM_Position[]
  //            
  //            try {
  //                
  //                wkslp[i] = GeometryFactory.createGM_Surface( out_rings, in_rings,
  //                new GM_SurfaceInterpolation_Impl(),
  //                crs );
  //                
  //            } catch(Exception e) {
  //                System.out.println("SHP2WKS::\n" + e);
  //            }
  //        }
  //        
  //        return wkslp;
  //        
  //    }

  /**
   * transforms the SHPPolygon to a WKSGeometry <BR>
   * gets the polygon that should be transformed <BR>
   */
  public GM_Surface[] transformPolygon( CS_CoordinateSystem crs, SHPPolygon shppolygon )
  {
    ArrayList all_rings = new ArrayList( shppolygon.numRings );
    ArrayList outer_rings = new ArrayList( shppolygon.numRings );
    ArrayList inner_rings = new ArrayList( shppolygon.numRings );

    for( int i = 0; i < shppolygon.numRings; i++ )
    {

      GM_Position[] ring = new GM_Position[shppolygon.rings.points[i].length];

      for( int k = 0; k < shppolygon.rings.points[i].length; k++ )
      {
        ring[k] = GeometryFactory.createGM_Position( shppolygon.rings.points[i][k].x,
            shppolygon.rings.points[i][k].y );
      }
      all_rings.add( ring );
    }

    ArrayList wkslp = new ArrayList();

    // for every outer ring
    for( int i = 0; i < all_rings.size(); i++ )
    {
      GM_Position[] out_ring = (GM_Position[])all_rings.get( i );

      boolean inn = false;
      for( int j = 0; j < all_rings.size(); j++ )
      {
        if( i == j )
          continue;
        GM_Position[] inring = (GM_Position[])all_rings.get( j );

        // check if one or more points of a inner ring are
        // within the actual outer ring
        try
        {
          if( isInsideRing( inring, out_ring[0] ) )
          {
            inn = true;
            inner_rings.add( out_ring );
            break;
          }
        }
        catch( Exception e )
        {
          System.out.println( "Error: " + e.toString() );
        }

      }
      if( !inn )
      {
        outer_rings.add( out_ring );
      }

    }

    for( int i = 0; i < outer_rings.size(); i++ )
    {
      GM_Position[] out_ring = (GM_Position[])outer_rings.get( i );

      int count = inner_rings.size() - 1;
      ArrayList list = new ArrayList( count + 2 );
      // find inner rings of the current outter ring
      for( int k = count; k >= 0; k-- )
      {
        GM_Position[] in_ring = (GM_Position[])inner_rings.get( k );
        if( isInsideRing( out_ring, in_ring[0] ) )
        {
          list.add( inner_rings.remove( k ) );
        }
      }
      GM_Position[][] inrings = (GM_Position[][])list.toArray( new GM_Position[list.size()][] );

      try
      {
        GM_Surface sur = GeometryFactory.createGM_Surface( out_ring, inrings,
            new GM_SurfaceInterpolation_Impl(), crs );
        wkslp.add( sur );
      }
      catch( Exception e )
      {
        System.out.println( "SHP2WKS:: transformPolygon\n" + e );
      }

    }

    return (GM_Surface[])wkslp.toArray( new GM_Surface[wkslp.size()] );
  }
} // end of class WKB2WKS
