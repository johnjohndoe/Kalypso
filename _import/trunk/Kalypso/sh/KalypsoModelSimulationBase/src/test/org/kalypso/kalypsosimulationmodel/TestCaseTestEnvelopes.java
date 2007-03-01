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
package test.org.kalypso.kalypsosimulationmodel;

import org.kalypso.kalypsosimulationmodel.core.terrainmodel.GMRectanglesClip;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

import junit.framework.TestCase;

/**
 * @author Madanagopal
 *
 *
 */
public class TestCaseTestEnvelopes extends TestCase
{
  
//public GMRectanglesClip(float a_min_x, float a_min_y, float a_max_x, float a_max_y,
//float b_min_x, float b_min_y, float b_max_x, float b_max_y){
//RectAmin = GeometryFactory.createGM_Position( a_min_x,a_min_y );
//RectAmax = GeometryFactory.createGM_Position( a_max_x,a_max_y );
//env1= GeometryFactory.createGM_Envelope( RectAmin, RectAmax );
// 
//RectBmin = GeometryFactory.createGM_Position( b_min_x,b_min_y );
//RectBmax = GeometryFactory.createGM_Position( b_max_x,b_max_y );
//env2= GeometryFactory.createGM_Envelope( RectBmin, RectBmax );
//}
  
  
//public void startCalculate(){
//  if (env1.intersects(env2)){
//    gm = this.calculateIntersectionPoints( env1, env2 );
//    for (int i= 0; i <gm.length;i++){
//      System.out.println(gm[i].toString());
//    }
//    }
//    else {
//      System.out.println("Doesnt Intersect");
//    }
//  
//}
  
  public void testingTestEnvelopes()
  {
    GM_Envelope env1 = GeometryFactory.createGM_Envelope( 0, 0, 4, 4 );
    GM_Envelope env2 = GeometryFactory.createGM_Envelope( 1, 1, 3, 3 );
    GM_Envelope interEnv12 = GeometryFactory.createGM_Envelope( 1, 1, 3, 3 );
    
    GMRectanglesClip test = new GMRectanglesClip(env1,env2);
    assertEquals( true, env1.intersects(env2));
    GM_Envelope intersetionEnv = 
        GMRectanglesClip.getIntersectionEnv( env1, env2 );
    assertEquals( interEnv12, intersetionEnv );
    
    //envelop which do not intersects
    GM_Envelope noInterEnv1 = GeometryFactory.createGM_Envelope( 0, 0, 4, 4 );
    GM_Envelope noInterEnv2 = GeometryFactory.createGM_Envelope( 5, 5, 6, 6 );
    assertNull( GMRectanglesClip.getIntersectionEnv( noInterEnv1, noInterEnv2 ) );
    
    //1 point of intersection
    GM_Envelope onePointInterEnv1 = GeometryFactory.createGM_Envelope( 0, 0, 4, 4 );
    GM_Envelope onePointInterEnv2 = GeometryFactory.createGM_Envelope( 4, 4, 6, 6 );
    GM_Envelope onePointInterEnv = GeometryFactory.createGM_Envelope( 4, 4, 4, 4 );
    
    assertEquals(
            onePointInterEnv, 
            GMRectanglesClip.getIntersectionEnv( 
                      onePointInterEnv1, onePointInterEnv2 ) );
    
    //selfs intersection
    assertEquals(
        onePointInterEnv1, 
        GMRectanglesClip.getIntersectionEnv( 
                  onePointInterEnv1, onePointInterEnv1 ) );
  }

}
