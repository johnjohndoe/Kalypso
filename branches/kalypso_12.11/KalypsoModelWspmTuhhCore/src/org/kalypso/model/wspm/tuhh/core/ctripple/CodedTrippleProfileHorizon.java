/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Bj�rnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universit�t Hamburg-Harburg, Institut f�r Wasserbau, Hamburg, Germany
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
package org.kalypso.model.wspm.tuhh.core.ctripple;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Holger Albert
 */
public class CodedTrippleProfileHorizon
{
  private final List<CodedTrippleProfilePoint> m_points;

  public CodedTrippleProfileHorizon( )
  {
    m_points = new ArrayList<>();
  }

  public void addProfilePoint( CodedTrippleProfilePoint point )
  {
    m_points.add( point );
  }

  public CodedTrippleProfilePoint[] getProfilePoints( )
  {
    return m_points.toArray( new CodedTrippleProfilePoint[] {} );
  }
}