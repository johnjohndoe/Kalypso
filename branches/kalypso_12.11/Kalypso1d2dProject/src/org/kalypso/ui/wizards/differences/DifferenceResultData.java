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
package org.kalypso.ui.wizards.differences;

import org.apache.commons.lang3.StringUtils;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.ui.wizards.results.SelectResultData;

/**
 * @author Gernot Belger
 */
public class DifferenceResultData extends SelectResultData
{
  public static final String PROPERTY_DESTINATION_NAME = "destinationName"; //$NON-NLS-1$

  private String m_destinationName = StringUtils.EMPTY;

  public DifferenceResultData( final IScenarioResultMeta currentScenarioResult )
  {
    super( currentScenarioResult );
  }

  public String getDestinationName( )
  {
    return m_destinationName;
  }

  public void setDestinationName( final String destinationName )
  {
    final String oldValue = m_destinationName;

    m_destinationName = destinationName;

    firePropertyChange( PROPERTY_DESTINATION_NAME, oldValue, destinationName );
  }
}