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
package org.kalypso.model.hydrology.internal.preprocessing.resolve;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.Assert;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.CatchmentInfo;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.ICatchmentInfos;

/**
 * Holds information about the catchments derived from the hydrotopes.
 * 
 * @author Gernot Belger
 */
class CatchmentInfos implements ICatchmentInfos
{
  private final Map<Catchment, CatchmentInfo> m_infos = new HashMap<>();

  public void addInfo( final CatchmentInfo info )
  {
    final Catchment catchment = info.getCatchment();

    Assert.isTrue( !m_infos.containsKey( catchment ) );

    m_infos.put( catchment, info );
  }

  @Override
  public Catchment[] getCatchments( )
  {
    return m_infos.keySet().toArray( new Catchment[m_infos.size()] );
  }

  @Override
  public CatchmentInfo getInfo( final Catchment catchment )
  {
    return m_infos.get( catchment );
  }
}