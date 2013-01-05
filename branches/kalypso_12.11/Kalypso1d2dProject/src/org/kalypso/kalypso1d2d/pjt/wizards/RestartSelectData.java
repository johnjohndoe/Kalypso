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
package org.kalypso.kalypso1d2d.pjt.wizards;

import java.util.ArrayList;
import java.util.Arrays;

import org.eclipse.core.databinding.observable.list.IObservableList;
import org.eclipse.core.databinding.observable.list.WritableList;
import org.eclipse.core.resources.IFolder;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.ui.wizards.results.SelectResultData;

/**
 * @author Gernot Belger
 */
class RestartSelectData extends SelectResultData
{
  public static final String PROPERTY_SELECTED_RESTART = "selectedRestart"; //$NON-NLS-1$

  private final IObservableList m_restartResults = new WritableList( new ArrayList<>(), IStepResultMeta.class );

  private IStepResultMeta m_selectedRestart;

  private final IFolder m_currentScenario;

  public RestartSelectData( final IFolder currentScenario, final IScenarioResultMeta currentScenarioResult, final IResultMeta[] restartResults )
  {
    super( currentScenarioResult );

    m_currentScenario = currentScenario;

    m_restartResults.addAll( Arrays.asList( restartResults ) );
  }

  public IObservableList getRestartResultSet( )
  {
    return m_restartResults;
  }

  public IStepResultMeta[] getRestartResults( )
  {
    return (IStepResultMeta[])m_restartResults.toArray( new IStepResultMeta[m_restartResults.size()] );
  }

  public IStepResultMeta getSelectedRestart( )
  {
    return m_selectedRestart;
  }

  public void setSelectedRestart( final IStepResultMeta selectedRestart )
  {
    final IStepResultMeta oldValue = m_selectedRestart;

    m_selectedRestart = selectedRestart;

    firePropertyChange( PROPERTY_SELECTED_RESTART, oldValue, selectedRestart );
  }

  public IFolder getCurrentScenario( )
  {
    return m_currentScenario;
  }
}