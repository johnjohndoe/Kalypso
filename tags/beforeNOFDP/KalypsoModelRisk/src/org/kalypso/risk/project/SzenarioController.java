/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.kalypso.afgui.scenarios.IScenarioDataListener;
import org.kalypso.kalypsosimulationmodel.core.modeling.IModel;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
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
  private LanduseStyleUpdateListener m_landuseStyleUpdateListener = new LanduseStyleUpdateListener();

  private IContainer m_scenarioDataPath = null;

  public SzenarioController( )
  {
  }

  public synchronized void modelLoaded( final IModel model, final IStatus status )
  {
    if( m_scenarioDataPath == null )
      return;
    if( model instanceof IRasterizationControlModel )
    {
      final Path path = new Path( "/models/RasterizationControlModel.gml" );
      final IFile file = m_scenarioDataPath.getFile( path );
      m_landuseStyleUpdateListener.startStyleUpdateJob( file );
    }

    // maybe get status info from status-model
  }

  public synchronized void scenarioChanged( final IContainer scenarioDataPath )
  {
    // unregister any listeners
    ResourcesPlugin.getWorkspace().removeResourceChangeListener( m_landuseStyleUpdateListener );
    m_scenarioDataPath = scenarioDataPath;
    if( scenarioDataPath != null )
      ResourcesPlugin.getWorkspace().addResourceChangeListener( m_landuseStyleUpdateListener, IResourceChangeEvent.POST_CHANGE );

    // maybe save status into dstatus model
  }

}
