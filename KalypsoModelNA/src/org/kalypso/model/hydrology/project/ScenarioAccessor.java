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
package org.kalypso.model.hydrology.project;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;

/**
 * Helper that encapsulates the constants to access data inside a rrm scenario.
 *
 * @author Gernot Belger
 */
public class ScenarioAccessor
{
  private final String FOLDER_RECHENVARIANTEN = "Rechenvarianten";//$NON-NLS-1$

  private final IFolder m_scenarioFolder;

  public ScenarioAccessor( final IFolder scenarioFolder )
  {
    m_scenarioFolder = scenarioFolder;
  }

  public IFile getSimulationsGtt( )
  {
    return getViewsFile( "Simulations.gtt" ); //$NON-NLS-1$
  }

  public IFile getParametersSnowGtt( )
  {
    return getViewsFile( "Parameters_Snow.gtt" ); //$NON-NLS-1$
  }

  public IFile getParametersSoilLayersGtt( )
  {
    return getViewsFile( "Parameters_SoilLayers.gtt" ); //$NON-NLS-1$
  }

  public IFile getParametersSoilProfilesGtt( )
  {
    return getViewsFile( "Parameters_SoilProfiles.gtt" ); //$NON-NLS-1$
  }

  public IFile getParametersSealingGtt( )
  {
    return getViewsFile( "Parameters_SealingClasses.gtt" ); //$NON-NLS-1$
  }

  public IFile getParametersLanduseGroupsGtt( )
  {
    return getViewsFile( "Parameters_LanduseGroups.gtt" ); //$NON-NLS-1$
  }

  public IFile getParametersSeasonalCycleGtt( )
  {
    return getViewsFile( "Parameters_SeasonalCycle.gtt" ); //$NON-NLS-1$
  }

  public IFile getNodesNetGtt( )
  {
    return getViewsFile( "ModelConstruction_Nodes.gtt" ); //$NON-NLS-1$
  }

  public IFile getReachesNetGtt( )
  {
    return getViewsFile( "ModelConstruction_Reaches.gtt" ); //$NON-NLS-1$
  }

  public IFile getCatchmentsNetGtt( )
  {
    return getViewsFile( "ModelConstruction_Catchments.gtt" ); //$NON-NLS-1$
  }

  public IFile getHydrotopesCatchmentsGtt( )
  {
    return getViewsFile( "Hydrotopes_Catchments.gtt" ); //$NON-NLS-1$
  }

  public IFile getHydrotopesLanduseGtt( )
  {
    return getViewsFile( "Hydrotopes_Landuse.gtt" ); //$NON-NLS-1$
  }

  public IFile getHydrotopesGeologieGtt( )
  {
    return getViewsFile( "Hydrotopes_Geologie.gtt" ); //$NON-NLS-1$
  }

  public IFile getHydrotopesPedologieGtt( )
  {
    return getViewsFile( "Hydrotopes_Pedologie.gtt" ); //$NON-NLS-1$
  }

  public IFile getHydrotopesHydrotopesGtt( )
  {
    return getViewsFile( "Hydrotopes_Hydrotopes.gtt" ); //$NON-NLS-1$
  }

  public IFolder getViewsFolder( )
  {
    return m_scenarioFolder.getFolder( ".views" ); //$NON-NLS-1$
  }

  public IFolder getSimulationsFolder( )
  {
    return m_scenarioFolder.getFolder( FOLDER_RECHENVARIANTEN );
  }

  /**
   * Check if a folder is a scenario folder.
   */
  public static boolean isScenarioFolder( final IFolder parent )
  {
    final ScenarioAccessor accessor = new ScenarioAccessor( parent );

    /* check for some key files */
    final IFolder simulationsFolder = accessor.getSimulationsFolder();
    if( !simulationsFolder.exists() )
      return false;

    // TODO: should check more...

    return true;
  }

  private IFile getViewsFile( final String filename )
  {
    final IFolder viewsFolder = getViewsFolder();

    return viewsFolder.getFile( filename ); //$NON-NLS-1$
  }
}