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
package org.kalypso.model.hydrology.project;

import java.net.URL;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * Helper that encapsulates the constants to access data inside a rrm scenario.
 * 
 * @author Gernot Belger
 */
public class RrmScenario
{
  private static final String FOLDER_RECHENVARIANTEN = "Rechenvarianten";//$NON-NLS-1$

  private static final String FOLDER_MODELS = ".models"; //$NON-NLS-1$

  private static final String FOLDER_ANFANGSWERTE = "Anfangswerte"; //$NON-NLS-1$

  private static final String FILE_MODELL_GML = "modell.gml"; //$NON-NLS-1$

  private static final String FILE_EXPERT_CONTROL_GML = "expertControl.gml"; //$NON-NLS-1$

  public static final String FILE_PARAMETER_GML = "parameter.gml"; //$NON-NLS-1$

  private static final String FILE_HYDROTOP_GML = "hydrotop.gml"; //$NON-NLS-1$

  private static final String FILE_GEOLOGIE = "geologie.gml";//$NON-NLS-1$

  private static final String FILE_LANDUSE = "landuse.gml";//$NON-NLS-1$

  private static final String FILE_PEDOLOGIE = "pedologie.gml";//$NON-NLS-1$

  private static final String FILE_SUDS_GML = "suds.gml"; //$NON-NLS-1$

  private static final String FILE_SYNTHN_GML = "synthN.gml"; //$NON-NLS-1$

  private static final String FILE_CATCHMENT_MODELS_GML = "catchmentModels.gml"; //$NON-NLS-1$

  private static final String FILE_TIMESERIES_MAPPINGS_GML = "timeseriesMappings.gml"; //$NON-NLS-1$

  private final IFolder m_scenarioFolder;

  /**
   * Find a {@link RrmScenario} for a given model workspace.
   */
  public static RrmScenario forAnyModelGml( final GMLWorkspace oneOfTheModels )
  {
    final URL context = oneOfTheModels.getContext();
    final IFile modelFile = ResourceUtilities.findFileFromURL( context );
    if( modelFile == null )
      return null;

    final IContainer modelFolder = modelFile.getParent();

    final IContainer scenarioFolder = modelFolder.getParent();

    if( scenarioFolder instanceof IFolder )
      return new RrmScenario( (IFolder) scenarioFolder );

    return null;
  }

  public RrmScenario( final IFolder scenarioFolder )
  {
    m_scenarioFolder = scenarioFolder;
  }

  /**
   * Check if a folder is a scenario folder.
   */
  public static boolean isScenarioFolder( final IFolder parent )
  {
    final RrmScenario accessor = new RrmScenario( parent );

    /* check for some key files */
    final IFolder simulationsFolder = accessor.getSimulationsFolder();
    if( !simulationsFolder.exists() )
      return false;

    // TODO: should check more...

    return true;
  }

  public IFolder getModelsFolder( )
  {
    return m_scenarioFolder.getFolder( FOLDER_MODELS );
  }

  public IFolder getLzsimFolder( )
  {
    return m_scenarioFolder.getFolder( FOLDER_ANFANGSWERTE );
  }

  public IFolder getSimulationsFolder( )
  {
    return m_scenarioFolder.getFolder( FOLDER_RECHENVARIANTEN );
  }

  public IFile getModelFile( )
  {
    return getModelsFolder().getFile( FILE_MODELL_GML );
  }

  public IFile getExpertControlGml( )
  {
    return getModelsFolder().getFile( FILE_EXPERT_CONTROL_GML );
  }

  public IFile getParameterGml( )
  {
    return getModelsFolder().getFile( FILE_PARAMETER_GML );
  }

  public IFile getHydrotopGml( )
  {
    return getModelsFolder().getFile( FILE_HYDROTOP_GML );
  }

  public IFile getSudsGml( )
  {
    return getModelsFolder().getFile( FILE_SUDS_GML );
  }

  public IFile getSyntnGml( )
  {
    return getModelsFolder().getFile( FILE_SYNTHN_GML );
  }

  public IResource getCatchmentModelsGml( )
  {
    return getModelsFolder().getFile( FILE_CATCHMENT_MODELS_GML );
  }

  public IResource getTimeseriesMappingsGml( )
  {
    return getModelsFolder().getFile( FILE_TIMESERIES_MAPPINGS_GML );
  }

  public IFile getLanduseFile( )
  {
    return getModelsFolder().getFile( FILE_LANDUSE );
  }

  public IFile getPedologyFile( )
  {
    return getModelsFolder().getFile( FILE_PEDOLOGIE );
  }

  public IFile getGeologyFile( )
  {
    return getModelsFolder().getFile( FILE_GEOLOGIE );
  }
}