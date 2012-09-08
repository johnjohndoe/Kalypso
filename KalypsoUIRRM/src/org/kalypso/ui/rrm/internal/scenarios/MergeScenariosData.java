/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.ui.rrm.internal.scenarios;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.databinding.observable.set.ISetChangeListener;
import org.eclipse.core.databinding.observable.set.SetChangeEvent;
import org.eclipse.core.databinding.observable.set.WritableSet;
import org.kalypso.commons.java.util.AbstractModelObject;

import de.renew.workflow.connector.cases.IScenario;

/**
 * The data binding object for the merge scenarios wizard and operation.
 *
 * @author Holger Albert
 */
public class MergeScenariosData extends AbstractModelObject
{
  /**
   * The target scenario.
   */
  private final IScenario m_targetScenario;

  /**
   * The selected scenarios as writable set.
   */
  private final WritableSet m_selectedScenariosSet;

  /**
   * The selected scenarios.
   */
  private IScenario[] m_selectedScenarios;

  /**
   * True, if the imported scenarios should be deleted.
   */
  private boolean m_deleteScenarios;

  /**
   * The constructor.
   *
   * @param targetScenario
   *          The target scenario.
   */
  public MergeScenariosData( final IScenario targetScenario )
  {
    m_targetScenario = targetScenario;
    m_selectedScenariosSet = new WritableSet();
    m_selectedScenarios = new IScenario[] {};
    m_deleteScenarios = false;

    m_selectedScenariosSet.addSetChangeListener( new ISetChangeListener()
    {
      @Override
      public void handleSetChange( final SetChangeEvent event )
      {
        scenariosChanged();
      }
    } );
  }

  /**
   * This function updates the selected scenarios array.
   */
  protected void scenariosChanged( )
  {
    m_selectedScenarios = (IScenario[]) m_selectedScenariosSet.toArray( new IScenario[] {} );
  }

  /**
   * This function returns the target scenario.
   *
   * @return The target scenario.
   */
  public IScenario getTargetScenario( )
  {
    return m_targetScenario;
  }

  /**
   * This function returns the selected scenarios as writable set.
   *
   * @return The selected scenarios as writable set.
   */
  public WritableSet getSelectedScenariosSet( )
  {
    return m_selectedScenariosSet;
  }

  /**
   * This function returns the selected scenarios. The target scenario will not be contained in this array.
   *
   * @return The selected scenarios. The target scenario will not be contained in this array.
   */
  public IScenario[] getSelectedScenarios( )
  {
    final List<IScenario> results = new ArrayList<>();

    for( final IScenario selectedScenario : m_selectedScenarios )
    {
      if( selectedScenario.equals( m_targetScenario ) )
        continue;

      results.add( selectedScenario );
    }

    return results.toArray( new IScenario[] {} );
  }

  /**
   * This function returns true, if the imported scenarios should be deleted.
   *
   * @return True, if the imported scenarios should be deleted.
   */
  public boolean isDeleteScenarios( )
  {
    return m_deleteScenarios;
  }

  /**
   * This function sets the delete scenarios flag.
   *
   * @param deleteScenarios
   *          True, if the imported scenarios should be deleted.
   */
  public void setDeleteScenarios( final boolean deleteScenarios )
  {
    m_deleteScenarios = deleteScenarios;
  }
}