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
package org.kalypso.model.wspm.tuhh.ui.chart.data;

import java.util.ArrayList;
import java.util.Collection;

import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;

/**
 * @author Gernot Belger
 */
public class WaterlevelObject
{
  private final Collection<IProfileObject> m_points = new ArrayList<>();

  private final Collection<IProfileObject> m_segments = new ArrayList<>();

  public void addObject( final IProfileObject object )
  {
    final String type = object.getType();
    switch( type )
    {
      case IWspmTuhhConstants.OBJECT_TYPE_WATERLEVEL_POINTS:
        m_points.add( object );
        return;

      case IWspmTuhhConstants.OBJECT_TYPE_WATERLEVEL_SEGMENT:
        m_segments.add( object );
        return;

      default:
        throw new IllegalArgumentException();
    }
  }
}