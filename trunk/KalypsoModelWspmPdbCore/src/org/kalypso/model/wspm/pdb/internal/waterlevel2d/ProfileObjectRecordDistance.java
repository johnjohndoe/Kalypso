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
package org.kalypso.model.wspm.pdb.internal.waterlevel2d;

import org.kalypso.commons.math.simplify.ISegmentDistance;
import org.kalypso.model.wspm.core.profil.IProfileObjectRecord;

import com.vividsolutions.jts.algorithm.CGAlgorithms;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Computes the segment distance by easting, northing, height (i.e. 3d geographic line distance).<br/>
 * TODO: make universally usable by configuring the properties for the distance.
 * 
 * @author Gernot Belger
 */
public class ProfileObjectRecordDistance implements ISegmentDistance<IProfileObjectRecord>
{
  private final double m_maxDistance;

  public ProfileObjectRecordDistance( final double maxDistance )
  {
    m_maxDistance = maxDistance;
  }

  @Override
  public double getMaxTolerance( )
  {
    return m_maxDistance;
  }

  @Override
  public double distance( final IProfileObjectRecord segmentStart, final IProfileObjectRecord segmentEnd, final IProfileObjectRecord point )
  {
    final Coordinate p = asCoordinate( point );
    final Coordinate s1 = asCoordinate( segmentStart );
    final Coordinate s2 = asCoordinate( segmentEnd );

    return CGAlgorithms.distancePointLine( p, s1, s2 );

    // EXAMPLE for 3d
    // final Vector3D s1 = asVector( segmentStart );
    // final Vector3D s2 = asVector( segmentEnd );
    // final Vector3D p = asVector( point );
    // final Line line = new Line( s1, s2 );
    // return line.distance( p );
  }

  // TODO: would be nice to use arbitrary properties
  private Coordinate asCoordinate( final IProfileObjectRecord record )
  {
    final double x = record.getBreite();
    final double y = record.getHoehe();

    return new Coordinate( x, y );
  }
}