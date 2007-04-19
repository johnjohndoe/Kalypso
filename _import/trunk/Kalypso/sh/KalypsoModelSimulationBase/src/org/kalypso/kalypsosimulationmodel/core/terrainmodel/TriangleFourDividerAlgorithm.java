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
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LinearRing;

/**
 * @author madanago
 *
 */
public class TriangleFourDividerAlgorithm implements ITriangleAlgorithm,SurfacePatchVisitable
{
  private LinearRing ring;  
  private Map<GM_Surface, Double> toBeVisited =  new HashMap<GM_Surface, Double>();  
  public TriangleFourDividerAlgorithm( LinearRing _ring)
  {
   this.ring = _ring;
  }  
  public boolean furtherDivisionNeeded( GM_Position[] coOrds )
  {
    double max = PlaneUtils.convertToTwoDecimals( SampleColorModelInterval.getInstance().getInterval());
    System.out.println("max :" +max);    
    double _z1 = coOrds[0].getZ();
    double _z2 = coOrds[1].getZ();
    double _z3 = coOrds[2].getZ();
    
    GM_Position _center = PlaneUtils.calculateCenterCoOrdinate( coOrds );    

    double _inX_1 = Math.abs(coOrds[0].getX() - coOrds[1].getX());
    double _inY_1 = Math.abs(coOrds[0].getY() - coOrds[1].getY());
    double _inZ_1 = Math.abs(coOrds[0].getZ() - coOrds[1].getZ());
    
    double _inX_2 = Math.abs(coOrds[1].getX() - coOrds[2].getX());
    double _inY_2 = Math.abs(coOrds[1].getY() - coOrds[2].getY());
    double _inZ_2 = Math.abs(coOrds[1].getZ() - coOrds[2].getZ());
    
    double specA = PlaneUtils.convertToTwoDecimals((
                                         (_inX_1 * _inX_2)+
                                         (_inY_1 * _inY_2)+
                                         (_inZ_1 * _inZ_2)
                                         )
                                       *(
                                         (_inX_1 * _inX_2)+
                                         (_inY_1 * _inY_2)+
                                         (_inZ_1 * _inZ_2))
                                         );
    
    double specB = PlaneUtils.convertToTwoDecimals( 
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

  public HashMap<GM_Surface, Double> visitThisDivisionSurface( GM_Surface pos )
  {
    List<GM_Position[]> toSplit = new ArrayList<GM_Position[]>();
    //List<GM_Surface> notToSplit = new ArrayList<GM_Surface>();
    HashMap<GM_Surface, Double> notToSplit = new HashMap<GM_Surface, Double>();
    toSplit.add( PlaneUtils.getGM_PositionForThisSurface(pos) );
    while( !toSplit.isEmpty() )
    {
      final GM_Position[] splitCandidate = toSplit.remove( 0 );

      System.out.println( "-----------------------------------------" );
      if( this.furtherDivisionNeeded( splitCandidate ) )
      {
        System.out.println( "toSp" + Arrays.asList( splitCandidate ) );
        final GM_Position center = PlaneUtils.calculateCenterCoOrdinate( splitCandidate );
        final GM_Position[] tri1 = new GM_Position[] { 
                                              splitCandidate[0],
                                              PlaneUtils.calculateMidPoint( new GM_Position[] {splitCandidate[0],splitCandidate[1]}),
                                              PlaneUtils.calculateMidPoint( new GM_Position[] {splitCandidate[0],splitCandidate[2]}),
                                              splitCandidate[0]
                                                     };
        final GM_Position[] tri2 = new GM_Position[] { 
                                              PlaneUtils.calculateMidPoint( new GM_Position[] {splitCandidate[0],splitCandidate[1]}),
                                              splitCandidate[1],
                                              PlaneUtils.calculateMidPoint( new GM_Position[] {splitCandidate[1],splitCandidate[2]}),
                                              PlaneUtils.calculateMidPoint( new GM_Position[] {splitCandidate[0],splitCandidate[1]}),
                                                     };
        final GM_Position[] tri3 = new GM_Position[] { 
                                              PlaneUtils.calculateMidPoint( new GM_Position[] {splitCandidate[0],splitCandidate[1]}),
                                              PlaneUtils.calculateMidPoint( new GM_Position[] {splitCandidate[1],splitCandidate[2]}),
                                              PlaneUtils.calculateMidPoint( new GM_Position[] {splitCandidate[0],splitCandidate[2]}),
                                              PlaneUtils.calculateMidPoint( new GM_Position[] {splitCandidate[0],splitCandidate[1]}),
                                                     };
        final GM_Position[] tri4 = new GM_Position[] { 
                                              PlaneUtils.calculateMidPoint( new GM_Position[] {splitCandidate[0],splitCandidate[2]}),
                                              PlaneUtils.calculateMidPoint( new GM_Position[] {splitCandidate[1],splitCandidate[2]}),
                                              splitCandidate[2],
                                              PlaneUtils.calculateMidPoint( new GM_Position[] {splitCandidate[0],splitCandidate[2]}),
                                                     };
        toSplit.add( tri1 );
        toSplit.add( tri2 );
        toSplit.add( tri3 );
        toSplit.add( tri4 );
        System.out.println( "tri1" + Arrays.asList( tri1 ) );
        System.out.println( "tri2" + Arrays.asList( tri2 ) );
        System.out.println( "tri3" + Arrays.asList( tri3 ) );
        System.out.println( "tri4" + Arrays.asList( tri4 ) );   
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
          notToSplit.put( createGM_Surface,
                          PlaneUtils.calculateCenterCoOrdinate( 
                                          PlaneUtils.getGM_PositionForThisSurface( createGM_Surface )).getZ());
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
  
  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.SurfacePatchVisitable#aceptSurfacePatches(org.kalypsodeegree.model.geometry.GM_Envelope, org.kalypso.kalypsosimulationmodel.core.terrainmodel.SurfacePatchVisitor)
   */
  public void acceptSurfacePatches( GM_Envelope envToVisit, SurfacePatchVisitor surfacePatchVisitor) throws GM_Exception
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