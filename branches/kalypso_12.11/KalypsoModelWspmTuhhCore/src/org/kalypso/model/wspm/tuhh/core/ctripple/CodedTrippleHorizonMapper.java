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
package org.kalypso.model.wspm.tuhh.core.ctripple;

import java.util.HashMap;
import java.util.Map;

/**
 * @author Holger Albert
 */
public class CodedTrippleHorizonMapper
{
  private final Map<String, String> m_codeToHorizonId;

  private final Map<String, String> m_horizonIdToPartId;

  public CodedTrippleHorizonMapper( )
  {
    m_codeToHorizonId = new HashMap<>();
    m_horizonIdToPartId = new HashMap<>();
  }

  /**
   * This function returns the horizon id associated with the code of the point.
   * 
   * @param code
   *          The code of the point.
   * @return The horizon id.
   */
  public String getHorizonId( String code )
  {
    // TODO
    return null;
  }

  /**
   * This function returns the part id of a profile part of Kalypso for the horizon id.
   * 
   * @param horizonId
   *          The horizon id.
   * @return The part id of a profile part of Kalypso.
   */
  public String getPartId( String horizonId )
  {
    // TODO
    return null;
  }
}