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
package org.kalypso.risk.project;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.ResourcesPlugin;
import org.kalypso.afgui.scenarios.IScenarioDataListener;
import org.kalypso.kalypsosimulationmodel.core.modeling.IModel;
import org.kalypso.risk.model.services.LanduseStyleUpdateListener;

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
//  private IRasterizationControlModel m_rasterizationControlModel = null;
//  
//  private ILanduseModel m_landuseModel = null;
//  
//  private IRasterDataModel m_waterdepthCoverageModel = null;
  
  private LanduseStyleUpdateListener m_landuseStyleUpdateListener = new LanduseStyleUpdateListener();

//  private IFolder m_scenarioDataPath;

  public SzenarioController( )
  {
    ResourcesPlugin.getWorkspace().addResourceChangeListener( m_landuseStyleUpdateListener, IResourceChangeEvent.POST_CHANGE );
  }

  public synchronized void modelLoaded( final IModel model )
  {
//    if( model instanceof IRasterizationControlModel )
//      m_rasterizationControlModel = (IRasterizationControlModel) model;
//
//    if( model instanceof ILanduseModel )
//      m_landuseModel = (ILanduseModel) model;
//
//    if( m_rasterizationControlModel != null && m_landuseModel != null )
//    {
//      m_landuseStyleUpdateListener = new LanduseStyleUpdateListener( m_discModel, m_terrainModel );
//
//      // register common listener to terrain/deisc modell
////      m_rasterizationControlModel.getWrappedFeature().getWorkspace().addModellListener( m_landuseStyleUpdateListener );
//      m_landuseModel.getWrappedFeature().getWorkspace().addModellListener( m_landuseStyleUpdateListener );
//    }

//    if( model instanceof ILanduseClassCollection )
//    {
//      final IFile roughnessDbFile = m_scenarioDataPath.getProject().getFile( LanduseStyleUpdateListener.ROUGHNESS_DATABASE_PATH );
//      m_landuseStyleUpdateListener.startStyleUpdateJob( roughnessDbFile );
//    }

    // maybe get status info from status-model
  }

  public synchronized void scenarioChanged( final IFolder scenarioDataPath )
  {
//    m_scenarioDataPath = scenarioDataPath;

    // unregister any listeners
//    if( m_discModel != null )
//      m_discModel.getWrappedFeature().getWorkspace().removeModellListener( m_roughnessAssignListener );
//    if( m_terrainModel != null )
//      m_terrainModel.getWrappedFeature().getWorkspace().removeModellListener( m_roughnessAssignListener );
//    if( m_roughnessAssignListener != null )
//      m_roughnessAssignListener.dispose();
//
//    m_roughnessAssignListener = null;
//    m_discModel = null;
//    m_terrainModel = null;

    // maybe save status into dstatus model
  }

}
