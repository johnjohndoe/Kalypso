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
package org.kalypso.kalypsomodel1d2d.ui.map.temsys.viz;

import java.math.BigDecimal;

import org.kalypsodeegree.model.geometry.GM_Position;

/**
 * @author madanago
 *
 */
public class Test
{
  private static GM_Position[] GM_Position = new GM_Position[4];
  public static void main( String[] args )
  {

    Test test= new Test();
    double[] centerCo_1 = new double[3];
    centerCo_1[0] = 1.0;
    centerCo_1[1] = 0.5;
    centerCo_1[2] = 5.0;
    
    double[] centerCo_2 = new double[3];
    centerCo_2[0] = 1.0;
    centerCo_2[1] = 1.0;
    centerCo_2[2] = 8.0;
    
    double[] centerCo_3 = new double[3];
    centerCo_3[0] = 1.0;
    centerCo_3[1] = 0.0;
    centerCo_3[2] = 10.0;
//    centerCo_1[0] = 1.0;
//    centerCo_1[1] = 0.5;
//    centerCo_1[2] = 2.0;
//    
//    double[] centerCo_2 = new double[3];
//    centerCo_2[0] = 1.0;
//    centerCo_2[1] = 1.0;
//    centerCo_2[2] = 3.0;
//    
//    double[] centerCo_3 = new double[3];
//    centerCo_3[0] = 1.0;
//    centerCo_3[1] = 0.0;
//    centerCo_3[2] = 1.0;
 //   centerCo[2] = computeZOfTrianglePlanePoint( coords, centerCo[0], centerCo[1] );
    
    //toSp[GM_Position: 1.0 0.5 2.0 , GM_Position: 1.0 1.0 3.0 , GM_Position: 1.0 0.0 1.0 , GM_Position: 1.0 0.5 2.0 ]
    
    String aa = "ABVSS"; //$NON-NLS-1$
    System.out.println(aa.getBytes());
    System.out.println("Ex: "+test.furtherDivisionNeeded(new GM_Position[] { //$NON-NLS-1$
        org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Position(centerCo_1),
        org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Position(centerCo_2),
        org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Position(centerCo_3),
        org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Position(centerCo_1)}));
  }
  public boolean furtherDivisionNeeded( GM_Position[] coOrds )
  {
    final double max = 1.0;
    double _z1 = coOrds[0].getZ();
    double _z2 = coOrds[1].getZ();
    double _z3 = coOrds[2].getZ();
    
    GM_Position _center = calculateCenterCoOrdinate( coOrds );    

    double _inX_1 = Math.abs( coOrds[0].getX() - coOrds[1].getX());
    double _inY_1 = Math.abs(coOrds[0].getY() - coOrds[1].getY());
    double _inZ_1 = Math.abs(coOrds[0].getZ() - coOrds[1].getZ());
    
    double _inX_2 = Math.abs( coOrds[1].getX() - coOrds[2].getX());
    double _inY_2 = Math.abs( coOrds[1].getY() - coOrds[2].getY());
    double _inZ_2 = Math.abs( coOrds[1].getZ() - coOrds[2].getZ());
    
    double specA = convertToTwoDecimals(((_inX_1 * _inX_2)+
                                         (_inY_1 * _inY_2)+
                                         (_inZ_1 * _inZ_2))
                                       *((_inX_1 * _inX_2)+
                                         (_inY_1 * _inY_2)+
                                         (_inZ_1 * _inZ_2)));
    
    double specB = convertToTwoDecimals(    ((_inX_1 * _inX_1)+(_inY_1 * _inY_1)+(_inZ_1 * _inZ_1)) *
                                            ((_inX_2 * _inX_2)+(_inY_2 * _inY_2)+(_inZ_2 * _inZ_2)));
        
//    if (specA == specB)
//      return false;
    
    
    
//    if (_center.getX() == coOrds[0].getX()&&_center.getY() == coOrds[0].getY() ||
//        _center.getX() == coOrds[1].getX()&&_center.getY() == coOrds[1].getY() ||
//        _center.getX() == coOrds[2].getX()&&_center.getY() == coOrds[2].getY())
//    {
//      return false;
//    }
/*
    final double A = (coOrds[1].getX()* coOrds[2].getY()-coOrds[2].getX()* coOrds[1].getY())/ (coOrds[1].getX()- coOrds[2].getX());
    final double B = (coOrds[1].getY()-(A))/coOrds[1].getX();
    if (convertToTwoDecimals(coOrds[0].getY()) == convertToTwoDecimals(A + B*coOrds[0].getX()))
      return false;
  
*/

    
//    if ((convertToTwoDecimals(_center.getX()/_center.getY()) == convertToTwoDecimals(coOrds[1].getX()/coOrds[1].getY()))
//        ||(convertToTwoDecimals(_center.getX()/_center.getY()) == convertToTwoDecimals(coOrds[1].getX()/coOrds[1].getY()))
//        ||(convertToTwoDecimals(_center.getX()/_center.getY()) == convertToTwoDecimals(coOrds[2].getX()/coOrds[2].getY())))
      //return false;
    
    
    if((Math.abs( _z1 - _center.getZ() ) >= max) || 
        (Math.abs( _z2 - _center.getZ() ) >= max) ||
        (Math.abs( _z3 - _center.getZ() ) >= max))
    {
      if (specA != specB)
      return true;
//      else 
//      return false;
    }
    return false;
  }
  
  private GM_Position calculateCenterCoOrdinate( GM_Position[] coords )
  {
    
    double[] centerCo = new double[3];
    centerCo[0] = (coords[0].getX() + coords[1].getX() + coords[2].getX()) / 3;
    centerCo[1] = (coords[0].getY() + coords[1].getY() + coords[2].getY()) / 3;
    centerCo[2] = (coords[0].getZ() + coords[1].getZ() + coords[2].getZ()) / 3; 
 //   centerCo[2] = computeZOfTrianglePlanePoint( coords, centerCo[0], centerCo[1] );
    
    return org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Position( centerCo ); 
  }
  private double convertToTwoDecimals( double r )
  {
    BigDecimal bd = new BigDecimal(r);
    bd = bd.setScale(1, BigDecimal.ROUND_HALF_UP);
    return bd.doubleValue();   
  }
}
