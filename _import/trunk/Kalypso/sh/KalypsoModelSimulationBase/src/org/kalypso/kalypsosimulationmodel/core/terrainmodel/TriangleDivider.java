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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.kalypso.kalypsomodel1d2d.ui.map.temsys.viz.ElevationColorControl;
import org.kalypso.kalypsomodel1d2d.ui.map.temsys.viz.IElevationColorModel;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LinearRing;

/**
 * @author Madanagopal
 *
 */
public class TriangleDivider implements SurfacePatchVisitable
{ 
  //IElevationColorModel colorModel = ElevationColorControl.getColorModel();
  
  private LinearRing ring;
  private double in = 0.0;
  
  
  private static ListRetriever _listRetriver;

  private Map<GM_Surface, Double> toBeVisited =  new HashMap<GM_Surface, Double>();
  public TriangleDivider( LinearRing _ring)
  {
   this.ring = _ring;
  }

  public boolean furtherDivisionNeeded( GM_Position[] coOrds )
  {
//    final double max = convertToTwoDecimals(
//                       (ElevationColorControl.getMaxElevation() -
//                        ElevationColorControl.getMinElevation()
//                       ) / 
//                        ElevationColorControl.getColorIndex());
    
    final double max = 1.0;
    double _z1 = coOrds[0].getZ();
    double _z2 = coOrds[1].getZ();
    double _z3 = coOrds[2].getZ();
    
    GM_Position _center = calculateCenterCoOrdinate( coOrds );    

    double _inX_1 = Math.abs(coOrds[0].getX() - coOrds[1].getX());
    double _inY_1 = Math.abs(coOrds[0].getY() - coOrds[1].getY());
    double _inZ_1 = Math.abs(coOrds[0].getZ() - coOrds[1].getZ());
    
    double _inX_2 = Math.abs(coOrds[1].getX() - coOrds[2].getX());
    double _inY_2 = Math.abs(coOrds[1].getY() - coOrds[2].getY());
    double _inZ_2 = Math.abs(coOrds[1].getZ() - coOrds[2].getZ());
    
    double specA = convertToTwoDecimals((
                                         (_inX_1 * _inX_2)+
                                         (_inY_1 * _inY_2)+
                                         (_inZ_1 * _inZ_2)
                                         )
                                       *(
                                         (_inX_1 * _inX_2)+
                                         (_inY_1 * _inY_2)+
                                         (_inZ_1 * _inZ_2))
                                         );
    
    double specB = convertToTwoDecimals( 
                                         ((_inX_1 * _inX_1)+(_inY_1 * _inY_1)+(_inZ_1 * _inZ_1))*
                                         ((_inX_2 * _inX_2)+(_inY_2 * _inY_2)+(_inZ_2 * _inZ_2))
                                        );
        
    if((Math.abs( _z1 - _center.getZ() ) >= max) || 
        (Math.abs( _z2 - _center.getZ() ) >= max) ||
        (Math.abs( _z3 - _center.getZ() ) >= max))
    {
      if (specA != specB)
      return true;
    }
    return false;
  }

  public GM_Position calculateCenterCoOrdinate( GM_Position[] coords )
  {
    
    double[] centerCo = new double[3];
    centerCo[0] = (coords[0].getX() + coords[1].getX() + coords[2].getX()) / 3;
    centerCo[1] = (coords[0].getY() + coords[1].getY() + coords[2].getY()) / 3;
    centerCo[2] = (coords[0].getZ() + coords[1].getZ() + coords[2].getZ()) / 3; 
    return org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Position( centerCo ); 
  }

  public double computeZOfTrianglePlanePoint( GM_Position[] coords, double x, double y )
  {
    
    return calculateTrianglePlaneEquation(coords)[0] * x +
           calculateTrianglePlaneEquation(coords)[1] * y + 
           calculateTrianglePlaneEquation(coords)[2];
  }
  
  public static final double[] calculateTrianglePlaneEquation( GM_Position[] coords )
  {
    Assert.throwIAEOnNullParam( coords, "coords" );
    if( coords.length < 3 )
    {
      throw new IllegalArgumentException( "Param coord which represent the point of a triangle must " + "have a minimum length of 3: current length=" + coords.length );
    }

    GM_Position coord = coords[0];

    double x1 = coord.getX();
    double y1 = coord.getY();
    double z1 = coord.getZ();

    coord = coords[1];
    double x2 = coord.getX();
    double y2 = coord.getY();
    double z2 = coord.getZ();

    coord = coords[2];
    double x3 = coord.getX();
    double y3 = coord.getY();
    double z3 = coord.getZ();
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
      double C = x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2);
      double D = x1 * (y2 * z3 - y3 * z2) + x2 * (y3 * z1 - y1 * z3) + x3 * (y1 * z2 - y2 * z1);

      // C=-C;
      // z=-A/Cx-B/Cy-D/C = Q*x+P*y+O
      return new double[] { -A / C, -B / C, D / C };
    }
  }
  
  private double convertToTwoDecimals( double r )
  {
    BigDecimal bd = new BigDecimal(r);
    bd = bd.setScale(1, BigDecimal.ROUND_HALF_UP);
    return bd.doubleValue();   
  }
  public HashMap<GM_Surface, Double> visitThisDivisionSurface( GM_Surface pos )
  {
    List<GM_Position[]> toSplit = new ArrayList<GM_Position[]>();
    //List<GM_Surface> notToSplit = new ArrayList<GM_Surface>();
    HashMap<GM_Surface, Double> notToSplit = new HashMap<GM_Surface, Double>();
    toSplit.add( getGM_PositionForThisSurface(pos) );
    while( !toSplit.isEmpty() )
    {
      final GM_Position[] splitCandidate = toSplit.remove( 0 );

      System.out.println( "-----------------------------------------" );
      if( this.furtherDivisionNeeded( splitCandidate ) )
      {
        System.out.println( "toSp" + Arrays.asList( splitCandidate ) );
        // for (GM_Position gm: splitCandidate) {
        // System.out.println("TOSplitCandidate : X :"+gm.getX()+" Y:"+gm.getY()+" Z:"+gm.getZ());
        // }
        final GM_Position center = this.calculateCenterCoOrdinate( splitCandidate );
        final GM_Position[] tri1 = new GM_Position[] { center, splitCandidate[0], splitCandidate[1], center };
        final GM_Position[] tri2 = new GM_Position[] { center, splitCandidate[1], splitCandidate[2], center };
        final GM_Position[] tri3 = new GM_Position[] { center, splitCandidate[2], splitCandidate[0], center };
//        System.out.println( "cntr" + center );
//        System.out.println( "tri1" + Arrays.asList( tri1 ) );
//        System.out.println( "tri2" + Arrays.asList( tri2 ) );
//        System.out.println( "tri3" + Arrays.asList( tri3 ) );
        if( toSplit.size() < 10 )
        {
          toSplit.add( tri1 );
          toSplit.add( tri2 );
          toSplit.add( tri3 );
        }
      }
      else
      {
        try
        {
          GM_Surface createGM_Surface = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Surface
                                       (  splitCandidate,
                                          HMOTerrainElevationModel.NO_INTERIOR_POS,
                                          null,
                                          IElevationProvider.CRS_GAUSS_KRUEGER );
          notToSplit.put( createGM_Surface, calculateCenterCoOrdinate( getGM_PositionForThisSurface( createGM_Surface )).getZ());
        }
        catch( Throwable e )
        {
          // TODO Auto-generated catch block
          e.printStackTrace();
        }
      }
    }
  
    return notToSplit;
  }
  

  public GM_Position[] getGM_PositionForThisSurface(GM_Surface surface) {
    
    GM_Position[] pos = null;
    GM_SurfacePatch patch;    
    try
    {
      patch = surface.getSurfacePatchAt( 0 );
      pos = patch.getExteriorRing();    
    }   
    catch( GM_Exception e1 )
    {
      e1.printStackTrace();
    }    
    return pos;
  }
  
  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.SurfacePatchVisitable#aceptSurfacePatches(org.kalypsodeegree.model.geometry.GM_Envelope, org.kalypso.kalypsosimulationmodel.core.terrainmodel.SurfacePatchVisitor)
   */
  public void acceptSurfacePatches( GM_Envelope envToVisit, SurfacePatchVisitor surfacePatchVisitor ) throws GM_Exception
  {     
  
    Coordinate[] coordinates = ring.getCoordinates();
  double[] exterior = { coordinates[0].x, coordinates[0].y, coordinates[0].z, coordinates[1].x, coordinates[1].y, coordinates[1].z, coordinates[2].x, coordinates[2].y, coordinates[2].z,
        coordinates[0].x, coordinates[0].y, coordinates[0].z };
    
    GM_Surface surfacePatch = org.kalypsodeegree_impl.model.geometry.GeometryFactory.
        createGM_Surface( exterior, HMOTerrainElevationModel.NO_INTERIOR, 3, IElevationProvider.CRS_GAUSS_KRUEGER );

    if (toBeVisited.isEmpty()) 
       toBeVisited  = visitThisDivisionSurface( surfacePatch );

    for (Iterator iter = toBeVisited.entrySet().iterator(); iter.hasNext();)
    {
        Map.Entry entry = (Map.Entry)iter.next();        
        surfacePatchVisitor.visit((GM_Surface)entry.getKey(),(double)(Double)entry.getValue());        
    }   
  }
}
