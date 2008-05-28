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

import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author Madanagopal
 * @author Patrice Congo
 * 
 */
public class GMRectanglesClip
{

  /**
   * returns upper left corner and lower right corner of the intersection envelop or null if the 2 envelop do not
   * intersect
   * 
   */
  public static final GM_Position[] getIntersectionPoints( final GM_Envelope envOne, final GM_Envelope envTwo )
  {
    if( !envOne.intersects( envTwo ) )
    {
      return null;
    }
    final GM_Position[] gmA = new GM_Position[2];

    gmA[0] = GeometryFactory.createGM_Position( Math.max( envOne.getMin().getX(), envTwo.getMin().getX() ), Math.min( envOne.getMin().getY() + envOne.getHeight(), envTwo.getMin().getY()
        + envTwo.getHeight() ) );
    gmA[1] = GeometryFactory.createGM_Position( Math.min( envOne.getMax().getX(), envTwo.getMax().getX() ), Math.max( envOne.getMax().getY() - envOne.getHeight(), envTwo.getMax().getY()
        - envTwo.getHeight() ) );
    return gmA;
  }

  /**
   * To get the intersection of the 2 envelop. If the 2 envelops intesects the resulting envelope is return otherwise
   * null is return
   * 
   */
  public static final GM_Envelope getIntersectionEnv( final GM_Envelope envOne, final GM_Envelope envTwo )
  {
    // TODO Check, if the coordinate systems of both envelopes are equal.
    final GM_Position[] poses = getIntersectionPoints( envOne, envTwo );
    if( poses == null )
      return null;

    final GM_Envelope envelope = GeometryFactory.createGM_Envelope( poses[0].getX(),// minx,
    poses[1].getY(),// miny,
    poses[1].getX(),// maxx,
    poses[0].getY(),// maxy
    envOne.getCoordinateSystem() );
    return envelope;
  }

}
