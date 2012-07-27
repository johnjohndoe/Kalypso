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
package org.kalypso.kalypsomodel1d2d.ui.map.cmds.ele;

import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.IFeatureChangeCommand;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.INativeTerrainElevationModelWrapper;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModelSystem;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Command for deleting a native terrain elevation model wrapper
 * 
 * @author Patrice Congo
 * 
 */
public class DeleteNativeTerrainElevationWrapper implements IFeatureChangeCommand
{
  private final ITerrainElevationModelSystem m_terrainElevationModelSystem;

  private final INativeTerrainElevationModelWrapper m_elevationModel;

  private boolean m_deleteFile;

  private boolean m_done = false;

  public DeleteNativeTerrainElevationWrapper( final ITerrainElevationModelSystem terrainElevationModelSystem, final INativeTerrainElevationModelWrapper elevationModel, final boolean deleteFile )
  {
    m_terrainElevationModelSystem = terrainElevationModelSystem;
    m_elevationModel = elevationModel;
    m_deleteFile = deleteFile;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.IFeatureChangeCommand#getChangedFeature()
   */
  @Override
  public Feature[] getChangedFeatures( )
  {
    if( m_done )
    {
      return new Feature[] { m_elevationModel };
    }
    return null;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  @Override
  public String getDescription( )
  {
    return Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.cmds.ele.DeleteNativeTerrainElevationWrapper.0") + m_elevationModel.getName(); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  @Override
  public boolean isUndoable( )
  {
    return !m_deleteFile;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  @Override
  public void process( ) throws Exception
  {
    if( m_done )
    {
      return;
    }
    m_terrainElevationModelSystem.getTerrainElevationModels().remove( m_elevationModel );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  @Override
  public void redo( ) throws Exception
  {
    process();
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  @Override
  public void undo( ) throws Exception
  {
    if( !m_done || m_deleteFile )
    {
      return;
    }
  }

}
