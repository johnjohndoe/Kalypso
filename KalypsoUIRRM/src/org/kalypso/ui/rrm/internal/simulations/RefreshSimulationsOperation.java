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
package org.kalypso.ui.rrm.internal.simulations;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.dialogs.ContainerGenerator;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.project.INaCalcCaseConstants;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.simulation.ui.calccase.ModelNature;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.calccase.UpdateSimulationWorker;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

import com.google.common.base.Charsets;

/**
 * @author Gernot Belger
 */
public class RefreshSimulationsOperation extends WorkspaceModifyOperation
{
  private final IStatusCollector m_log = new StatusCollector( KalypsoUIRRMPlugin.getID() );

  private final NAControl[] m_simulations;

  private final IContainer m_baseFolder;

  public RefreshSimulationsOperation( final IContainer baseFolder, final NAControl... simulations )
  {
    super( baseFolder );

    m_baseFolder = baseFolder;

    m_simulations = simulations;
  }

  @Override
  protected void execute( final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( "Refreshing simulations", m_simulations.length );

    for( final NAControl simulation : m_simulations )
    {
      final String name = simulation.getDescription();
      monitor.subTask( name );

      try
      {
        final IStatus status = refreshSimulation( simulation, new SubProgressMonitor( monitor, 1 ) );
        m_log.add( status );
      }
      catch( final CoreException e )
      {
        m_log.add( IStatus.ERROR, "%s: failed to create simulation", e, name );
      }
      catch( final UnsupportedEncodingException e )
      {
        m_log.add( IStatus.ERROR, "%s: Bad folder name", e, name );
      }

      // check for cancel, only after completion of one simulation to avoid inconsistent simulations
      ProgressUtilities.worked( monitor, 0 );
    }

    final IStatus status = m_log.asMultiStatusOrOK( "Problem(s) while refreshing simulations", "Simulations successfully refreshed" );
    throw new CoreException( status );
  }

  private IStatus refreshSimulation( final NAControl simulation, final IProgressMonitor monitor ) throws CoreException, UnsupportedEncodingException
  {
    final IStatusCollector log = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    final String name = simulation.getDescription();

    monitor.beginTask( String.format( "Refreshing simulation: %s", name ), 100 );

    /* Build simulation folder */
    final String encodedName = URLEncoder.encode( name, Charsets.UTF_8.name() );
    final IFolder simulationFolder = m_baseFolder.getFolder( new Path( encodedName ) );

    /* Delete existing data */
    // TODO: should we always do that? What about existing results etc.?
    if( simulationFolder.exists() )
    {
      monitor.subTask( "delete existing data" );
      simulationFolder.delete( false, false, new SubProgressMonitor( monitor, 10 ) );
    }

    /* create the simulation */
    final IStatus createStatus = createSimulation( simulation, simulationFolder, new SubProgressMonitor( monitor, 40 ) );
    log.add( createStatus );

    /* update the simulation */
    final UpdateSimulationWorker updateWorker = new UpdateSimulationWorker( simulationFolder );
    final IStatus updateStatus = updateWorker.execute( new SubProgressMonitor( monitor, 50 ) );
    log.add( updateStatus );

    return log.asMultiStatusOrOK( name, name );
  }

  private IStatus createSimulation( final NAControl simulation, final IFolder simulationFolder, final IProgressMonitor monitor ) throws CoreException
  {
    final IFile simulationFile = simulationFolder.getFile( INaCalcCaseConstants.CALCULATION_GML_PATH );

    /* create the folder (including .model folder) */
    final ContainerGenerator generator = new ContainerGenerator( simulationFile.getParent().getFullPath() );
    generator.generateContainer( new SubProgressMonitor( monitor, 1000 ) );

    final ModelNature nature = (ModelNature) simulationFolder.getProject().getNature( ModelNature.ID );

    /* Call ant */
    // TODO: for rrm, replace this with legacy code instead of using ant
    final Map<String, Object> antProperties = new HashMap<>();
    final IStatus createStatus = nature.createCalculationCaseInFolder( simulationFolder, antProperties, new SubProgressMonitor( monitor, 1000 ) );
    if( createStatus.matches( IStatus.ERROR ) )
      throw new CoreException( createStatus );

    /* Save simulation to folder */
    try
    {
      // REMARK: after call to ant, because that creates an empty file

      /* Crude: clone the feature in order to create a new workspace */
      final IFeatureType simulationType = simulation.getFeatureType();
      final GMLWorkspace simulationWorkspace = FeatureFactory.createGMLWorkspace( simulationType, null, null );
      final Feature targetFeature = simulationWorkspace.getRootFeature();

      final IPropertyType[] properties = simulationType.getProperties();
      for( final IPropertyType pt : properties )
      {
        final Object value = simulation.getProperty( pt );
        final Object clonedValue = FeatureHelper.cloneData( simulation, targetFeature, pt, value, null );
        targetFeature.setProperty( pt, clonedValue );
      }

      GmlSerializer.saveWorkspace( simulationWorkspace, simulationFile );
    }
    catch( final Exception e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e, "Failed to save control file for simulation: %s", simulation.getName() );
      throw new CoreException( status );
    }

    return createStatus;
  }
}