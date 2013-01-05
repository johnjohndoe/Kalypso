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

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import org.eclipse.core.databinding.observable.list.IObservableList;
import org.eclipse.jface.action.Action;
import org.eclipse.swt.widgets.Event;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;

/**
 * @author Gernot Belger
 */
class RestartSorterMoveAction extends Action
{
  private final RestartSelectData m_data;

  private final int m_step;

  public RestartSorterMoveAction( final RestartSelectData data, final int step )
  {
    m_data = data;
    m_step = step;

    m_data.addPropertyChangeListener( RestartSelectData.PROPERTY_SELECTED_RESTART, new PropertyChangeListener()
    {
      @Override
      public void propertyChange( final PropertyChangeEvent evt )
      {
        update();
      }
    } );

    update();
  }

  protected void update( )
  {
    final IStepResultMeta selectedStep = m_data.getSelectedRestart();
    final IObservableList resultSet = m_data.getRestartResultSet();

    final int position = resultSet.indexOf( selectedStep );
    final int newPosition = position + m_step;
    setEnabled( position != -1 && newPosition >= 0 && newPosition <= resultSet.size() - 1 );
  }

  @Override
  public void runWithEvent( final Event event )
  {
    if( !isEnabled() )
      return;

    final IStepResultMeta selectedStep = m_data.getSelectedRestart();
    final IObservableList resultSet = m_data.getRestartResultSet();

    final int position = resultSet.indexOf( selectedStep );
    if( position == -1 )
      return;

    resultSet.move( position, position + m_step );
  }
}