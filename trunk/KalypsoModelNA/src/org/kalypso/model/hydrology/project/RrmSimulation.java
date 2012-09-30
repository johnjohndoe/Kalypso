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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;

/**
 * Represents a simulation of the rrm model (i.e. one calculation case folder) and allows access to its data.
 *
 * @author Gernot Belger
 */
public class RrmSimulation
{
  public static final String FOLDER_ANFANGSWERTE = "InitialValues"; //$NON-NLS-1$

  public static final String FOLDER_MODELS = ".models"; //$NON-NLS-1$

  public static final String FOLDER_KLIMA = "Timeseries_Climate"; //$NON-NLS-1$

  public static final String FOLDER_RESULTS = "Results"; //$NON-NLS-1$

  public static final String FOLDER_NIEDERSCHLAG = "Timeseries_Precipitation"; //$NON-NLS-1$

  public static final String FOLDER_PEGEL = "Timeseries_Gauges"; //$NON-NLS-1$

  public static final String FOLDER_ZUFLUSS = "Timeseries_Tributary"; //$NON-NLS-1$

  public static final String FILE_MODELL_GML = "modell.gml"; //$NON-NLS-1$

  public static final String FILE_EXPERT_CONTROL_GML = "expertControl.gml"; //$NON-NLS-1$

  public static final String FILE_CALCULATION_GML = "calculation.gml"; //$NON-NLS-1$

  private static final String FILE_PARAMETER_GML = "parameter.gml"; //$NON-NLS-1$

  private static final String FILE_HYDROTOP_GML = "hydrotop.gml"; //$NON-NLS-1$

  private static final String FILE_SYNTHN_GML = "synthN.gml"; //$NON-NLS-1$

  private static final String FILE_LZSIM_GML = "lzsim.gml"; //$NON-NLS-1$

  public static final String FOLDER_LAST_RESULT = "MostRecent"; //$NON-NLS-1$

  private final IFolder m_simulation;

  public RrmSimulation( final IFolder simulationFolder )
  {
    m_simulation = simulationFolder;
  }

  public RrmCalculationResult[] getCalculationResults( )
  {
    try
    {
      final HydrologyCalculationFoldersCollector visitor = new HydrologyCalculationFoldersCollector( this );
      getResultsFolder().accept( visitor, 1, false );

      return visitor.getFolders();
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
    }

    return new RrmCalculationResult[] {};
  }

  public RrmCalculationResult getCurrentCalculationResult( )
  {
    final IFolder folder = getResultsFolder().getFolder( FOLDER_LAST_RESULT ); //$NON-NLS-1$

    return new RrmCalculationResult( folder );
  }

  public RrmProject getProject( )
  {
    return new RrmProject( m_simulation.getProject() );
  }

  public RrmScenario getScenario( )
  {
    IContainer parent = m_simulation.getParent();
    while( parent != null )
    {
      if( RrmScenario.isScenarioFolder( parent ) )
        return new RrmScenario( parent );
      else
        parent = parent.getParent();
    }

    throw new IllegalStateException();
  }

  public IFolder getSimulationFolder( )
  {
    return m_simulation;
  }

  public IFolder getModelsFolder( )
  {
    return m_simulation.getFolder( FOLDER_MODELS );
  }

  public IFolder getResultsFolder( )
  {
    return m_simulation.getFolder( FOLDER_RESULTS );
  }

  public IFolder getLzsimFolder( )
  {
    return m_simulation.getFolder( FOLDER_ANFANGSWERTE );
  }

  public IFile getModelGml( )
  {
    return getModelsFolder().getFile( FILE_MODELL_GML );
  }

  public IFile getExpertControlGml( )
  {
    return getModelsFolder().getFile( FILE_EXPERT_CONTROL_GML );
  }

  public IFile getCalculationGml( )
  {
    return getModelsFolder().getFile( FILE_CALCULATION_GML );
  }

  public IFile getParameterGml( )
  {
    return getModelsFolder().getFile( FILE_PARAMETER_GML );
  }

  public IFile getHydrotopGml( )
  {
    return getModelsFolder().getFile( FILE_HYDROTOP_GML );
  }

  public IFile getSyntnGml( )
  {
    return getModelsFolder().getFile( FILE_SYNTHN_GML );
  }

  public IFile getLzsimGml( )
  {
    return getLzsimFolder().getFile( FILE_LZSIM_GML );
  }

  public String getName( )
  {
    return m_simulation.getName();
  }

  public boolean exists( )
  {
    return m_simulation.exists();
  }

  public static IPath getCalculationGmlPath( )
  {
    return new Path( FOLDER_MODELS ).append( FILE_CALCULATION_GML );
  }

  public IFolder[] getTimeseriesFolders( )
  {
    final List<IFolder> folders = new ArrayList<>();

    final IFolder climateFolder = getClimateFolder();
    folders.add( climateFolder );
    final IFolder precipitationFolder = getPrecipitationFolder();
    folders.add( precipitationFolder );
    final IFolder gaugeFolder = getGaugeFolder();
    folders.add( gaugeFolder );
    final IFolder nodeInflowFolder = getNodeInflowFolder();
    folders.add( nodeInflowFolder );

    return folders.toArray( new IFolder[] {} );
  }

  public IFolder getClimateFolder( )
  {
    return m_simulation.getFolder( FOLDER_KLIMA );
  }

  public IFolder getPrecipitationFolder( )
  {
    return m_simulation.getFolder( FOLDER_NIEDERSCHLAG );
  }

  public IFolder getGaugeFolder( )
  {
    return m_simulation.getFolder( FOLDER_PEGEL );
  }

  public IFolder getNodeInflowFolder( )
  {
    return m_simulation.getFolder( FOLDER_ZUFLUSS );
  }
}