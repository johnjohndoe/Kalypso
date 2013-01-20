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
package org.kalypso.model.hydrology.internal.preprocessing.preparation;

import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.CatchmentInfo;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.ICatchmentInfos;

/**
 * Restricts the catchment infos to the relevant ones.
 * 
 * @author Gernot Belger
 */
class RelevantCatchmentInfos implements ICatchmentInfos
{
  private final Catchment[] m_relevantCatchments;

  private final ICatchmentInfos m_catchmentInfos;

  public RelevantCatchmentInfos( final Catchment[] relevantCatchments, final ICatchmentInfos catchmentInfos )
  {
    m_relevantCatchments = relevantCatchments;
    m_catchmentInfos = catchmentInfos;
  }

  @Override
  public Catchment[] getCatchments( )
  {
    return m_relevantCatchments;
  }

  @Override
  public CatchmentInfo getInfo( final Catchment catchment )
  {
    return m_catchmentInfos.getInfo( catchment );
  }
}