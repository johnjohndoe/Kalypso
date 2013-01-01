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

import org.eclipse.core.resources.IFolder;
import org.kalypso.kalypsomodel1d2d.conv.results.differences.MathOperator;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;

/**
 * Holds the data needed to create the difference tin.
 * 
 * @author Gernot Belger
 */
class TinDifferenceData
{
  private final IDocumentResultMeta m_masterResult;

  private final IDocumentResultMeta m_slaveResult;

  private final IStepResultMeta m_destinationResult;

  private final IFolder m_scenarioFolder;

  private final MathOperator m_operator;

  public TinDifferenceData( final IDocumentResultMeta masterResult, final IDocumentResultMeta slaveResult, final IStepResultMeta destinationResult, final MathOperator operator, final IFolder scenarioFolder )
  {
    m_masterResult = masterResult;
    m_slaveResult = slaveResult;
    m_destinationResult = destinationResult;
    m_operator = operator;
    m_scenarioFolder = scenarioFolder;
  }

  public IDocumentResultMeta getMasterResult( )
  {
    return m_masterResult;
  }

  public IDocumentResultMeta getSlaveResult( )
  {
    return m_slaveResult;
  }

  public IStepResultMeta getDestinationResult( )
  {
    return m_destinationResult;
  }

  public IFolder getScenarioFolder( )
  {
    return m_scenarioFolder;
  }

  public MathOperator getOperator( )
  {
    return m_operator;
  }
}