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
package org.kalypso.model.wspm.sobek.core.pub;

import java.awt.Point;

import org.kalypso.jts.SnapUtilities.SNAP_TYPE;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author kuch
 */
public class FNGeoUtils
{

  /**
   * @see org.kalypso.nofdpidss.ui.application.flow.network.ISnapPainter#isSnapPoaint(org.kalypso.ogc.gml.map.MapPanel,
   *      java.awt.Point)
   */
  public static boolean snapsOnBranch( final MapPanel panel, final GM_Curve curve, final GM_Point point, final int radius ) throws GM_Exception
  {
    final Point p = MapUtilities.retransform( panel, point );

    final GM_Point pSnap = MapUtilities.snap( panel, curve, p, radius, SNAP_TYPE.SNAP_AUTO );
    if( pSnap != null )
      return true;

    return false;
  }

  public static boolean snapsOnPoint( final MapPanel panel, final GM_Point base, final GM_Point point, final int radius ) throws GM_Exception
  {
    final Point p = MapUtilities.retransform( panel, point );

    final GM_Point pSnap = MapUtilities.snap( panel, base, p, radius, SNAP_TYPE.SNAP_AUTO );
    if( pSnap != null )
      return true;

    return false;
  }

// private static GM_Curve[] getSplittedCurveGeometry( final GM_Curve curve, final GM_Point point ) throws GM_Exception
// {
// final GM_LineString lineString = curve.getAsLineString();
// final CS_CoordinateSystem coordinateSystem = curve.getCoordinateSystem();
//
// /* point lies on which segment? */
// int intersects = -1;
// final GM_Position[] positions = lineString.getPositions();
// for( int i = 0; i < positions.length - 1; i++ )
// {
// final GM_Position[] subPositions = new GM_Position[] { positions[i], positions[i + 1] };
// final GM_Curve subCurve = GeometryFactory.createGM_Curve( subPositions, coordinateSystem );
//
// if( subCurve.intersects( point ) )
// {
// intersects = i;
// break;
// }
// }
//
// if( intersects == -1 )
// throw new IllegalStateException( "no line intersection" );
//
// final List<GM_Position> myPositions = new ArrayList<GM_Position>();
//
// /* curve c1 */
// for( int i = 0; i <= intersects; i++ )
// myPositions.add( positions[i] );
// myPositions.add( point.getPosition() );
//
// final GM_Curve c1 = GeometryFactory.createGM_Curve( myPositions.toArray( new GM_Position[] {} ), coordinateSystem );
//
// myPositions.clear();
//
// /* curve c2 */
// myPositions.add( point.getPosition() );
// for( int i = intersects + 1; i < positions.length; i++ )
// myPositions.add( positions[i] );
//
// final GM_Curve c2 = GeometryFactory.createGM_Curve( myPositions.toArray( new GM_Position[] {} ), coordinateSystem );
//
// /* return curves */
// return new GM_Curve[] { c1, c2 };
// }
//
}
