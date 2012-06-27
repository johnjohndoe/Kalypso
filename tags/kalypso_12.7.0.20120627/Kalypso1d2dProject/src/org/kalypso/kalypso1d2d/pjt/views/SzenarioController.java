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
package org.kalypso.kalypso1d2d.pjt.views;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.services.RoughnessAssignListener;
import org.kalypso.kalypsomodel1d2d.services.RoughnessStyleUpdateListener;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;

import de.renew.workflow.connector.cases.IModel;
import de.renew.workflow.connector.cases.IScenario;
import de.renew.workflow.connector.cases.IScenarioDataListener;

/**
 * A central place for controlling scenario specific stuff for Kalypso1d2d.
 * <p>
 * Get informed when models are loaded and/or scenario is changed.
 * 
 * @author Dejan Antanaskovic
 * @author Gernot Belger
 */
public class SzenarioController implements IScenarioDataListener
{
  private IFEDiscretisationModel1d2d m_discModel = null;

  private ITerrainModel m_terrainModel = null;

  private RoughnessAssignListener m_roughnessAssignListener = null;

  private final RoughnessStyleUpdateListener m_roughnessStyleUpdateListener = new RoughnessStyleUpdateListener();

  private IScenario m_scenario = null;

  public SzenarioController( )
  {
    ResourcesPlugin.getWorkspace().addResourceChangeListener( m_roughnessStyleUpdateListener, IResourceChangeEvent.POST_CHANGE );
  }

  @Override
  public synchronized void modelLoaded( final IModel model, final IStatus status )
  {
    if( m_discModel != null && m_terrainModel != null )
    {
      m_discModel.getWorkspace().removeModellListener( m_roughnessAssignListener );
      m_terrainModel.getWorkspace().removeModellListener( m_roughnessAssignListener );
      if( m_roughnessAssignListener != null )
        m_roughnessAssignListener.dispose();
    }

    if( model instanceof IFEDiscretisationModel1d2d )
    {
      m_discModel = (IFEDiscretisationModel1d2d) model;
    }

    if( model instanceof ITerrainModel )
    {
      m_terrainModel = (ITerrainModel) model;
    }

    if( m_discModel != null && m_terrainModel != null )
    {
      m_roughnessAssignListener = new RoughnessAssignListener( m_discModel, m_terrainModel );

      // register common listener to terrain/deisc modell
      m_discModel.getWorkspace().addModellListener( m_roughnessAssignListener );
      m_terrainModel.getWorkspace().addModellListener( m_roughnessAssignListener );
    }

    if( model instanceof IRoughnessClsCollection && m_scenario != null )
    {
      final IFile roughnessDbFile = m_scenario.getProject().getFile( RoughnessStyleUpdateListener.ROUGHNESS_DATABASE_PATH );
      m_roughnessStyleUpdateListener.startStyleUpdateJob( roughnessDbFile );
    }

    // maybe get status info from status-model
  }

  @Override
  public synchronized void scenarioChanged( final IScenario scenario )
  {
    m_scenario = scenario;

    // unregister any listeners
    if( m_discModel != null )
    {
      m_discModel.getWorkspace().removeModellListener( m_roughnessAssignListener );
    }
    if( m_terrainModel != null )
    {
      m_terrainModel.getWorkspace().removeModellListener( m_roughnessAssignListener );
    }
    if( m_roughnessAssignListener != null )
    {
      m_roughnessAssignListener.dispose();
    }

    m_roughnessAssignListener = null;
    m_discModel = null;
    m_terrainModel = null;

    // maybe save status into dstatus model
  }
}