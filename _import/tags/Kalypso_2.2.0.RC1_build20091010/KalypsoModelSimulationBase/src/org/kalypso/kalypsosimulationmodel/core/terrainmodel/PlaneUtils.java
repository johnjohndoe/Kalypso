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

import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.i18n.Messages;
import org.kalypsodeegree.model.geometry.GM_Position;

/**
 * @author Madanagopal API for common methods required in Elevation Model..
 */
public class PlaneUtils
{

  /**
   * Converts a double into a double with two decimal places.
   */
  public static double convertToTwoDecimals( final double r )
  {
    BigDecimal bd = new BigDecimal( r );
    bd = bd.setScale( 1, BigDecimal.ROUND_HALF_UP );
    return bd.doubleValue();
  }

  /**
   * Calculates the center Co-ordinate from a List of GM_Positions
   */
  public static GM_Position calculateCenterCoOrdinate( final GM_Position[] coords )
  {

    final double[] centerCo = new double[3];
    centerCo[0] = (coords[0].getX() + coords[1].getX() + coords[2].getX()) / 3;
    centerCo[1] = (coords[0].getY() + coords[1].getY() + coords[2].getY()) / 3;
    centerCo[2] = (coords[0].getZ() + coords[1].getZ() + coords[2].getZ()) / 3;
    return org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Position( centerCo );
  }

  public double computeZOfTrianglePlanePoint( final GM_Position[] coords, final double x, final double y )
  {

    return calculateTrianglePlaneEquation( coords )[0] * x + calculateTrianglePlaneEquation( coords )[1] * y + calculateTrianglePlaneEquation( coords )[2];
  }

  public static GM_Position calculateMidPoint( final GM_Position[] coords )
  {
    final double[] midPoint = new double[3];
    midPoint[0] = (coords[0].getX() + coords[1].getX()) / 2;
    midPoint[1] = (coords[0].getY() + coords[1].getY()) / 2;
    midPoint[2] = (coords[0].getZ() + coords[1].getZ()) / 2;
    return org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Position( midPoint );
  }

  public static final double[] calculateTrianglePlaneEquation( final GM_Position[] coords )
  {
    Assert.throwIAEOnNullParam( coords, "coords" ); //$NON-NLS-1$
    if( coords.length < 3 )
    {
      throw new IllegalArgumentException( Messages.getString("org.kalypso.kalypsosimulationmodel.core.terrainmodel.PlaneUtils.1") + Messages.getString("org.kalypso.kalypsosimulationmodel.core.terrainmodel.PlaneUtils.2") + coords.length ); //$NON-NLS-1$ //$NON-NLS-2$
    }

    GM_Position coord = coords[0];

    final double x1 = coord.getX();
    final double y1 = coord.getY();
    final double z1 = coord.getZ();

    coord = coords[1];
    final double x2 = coord.getX();
    final double y2 = coord.getY();
    final double z2 = coord.getZ();

    coord = coords[2];
    final double x3 = coord.getX();
    final double y3 = coord.getY();
    final double z3 = coord.getZ();
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
}
