/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.kalypsomodel1d2d.ui.map.dikeditchgen;

import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author kurzbach
 */
public class AbstractCreateStructuredNetworkStrategy
{
  protected void addPolygonAsBreakline( final TriangulationBuilder tinBuilder, final GM_Polygon outerRing ) throws GM_Exception
  {
    final String coordinateSystem = outerRing.getCoordinateSystem();

    // exterior ring
    final GM_Curve exteriorRingAsCurve = GeometryFactory.createGM_Curve( outerRing.getSurfacePatch().getExteriorRing(), coordinateSystem );
    tinBuilder.addBreakLine( exteriorRingAsCurve, false );

    // interior rings
    final GM_Position[][] interiorRings = outerRing.getSurfacePatch().getInteriorRings();
    for( int i = 0; i < interiorRings.length; i++ )
    {
      final GM_Position[] interiorRing = interiorRings[i];
      final GM_Curve innerRingAsCurve = GeometryFactory.createGM_Curve( interiorRing, coordinateSystem );
      tinBuilder.addBreakLine( innerRingAsCurve, false );
    }
  }
}
