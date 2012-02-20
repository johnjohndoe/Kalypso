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
import org.eclipse.ui.dialogs.ContainerGenerator;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.simulation.ui.calccase.ModelNature;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.calccase.UpdateSimulationWorker;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author Gernot Belger
 */
public class RefreshSimulationsOperation implements ICoreRunnableWithProgress
{
  private final IStatusCollector m_log = new StatusCollector( KalypsoUIRRMPlugin.getID() );

  private final NAControl[] m_simulations;

  private final IContainer m_baseFolder;

  public RefreshSimulationsOperation( final IContainer baseFolder, final NAControl... simulations )
  {
    m_baseFolder = baseFolder;

    m_simulations = simulations;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( Messages.getString("RefreshSimulationsOperation_0"), m_simulations.length ); //$NON-NLS-1$

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
        m_log.add( IStatus.ERROR, Messages.getString("RefreshSimulationsOperation_1"), e, name ); //$NON-NLS-1$
      }

      // check for cancel, only after completion of one simulation to avoid inconsistent simulations
      ProgressUtilities.worked( monitor, 0 );
    }

    return m_log.asMultiStatusOrOK( Messages.getString( "RefreshSimulationsOperation_2" ), Messages.getString( "RefreshSimulationsOperation_3" ) ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  private IStatus refreshSimulation( final NAControl simulation, final IProgressMonitor monitor ) throws CoreException
  {
    final IStatusCollector log = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    final IFolder simulationFolder = createFolder( m_baseFolder, simulation );

    monitor.beginTask( String.format( Messages.getString("RefreshSimulationsOperation_4"), simulationFolder.getName() ), 100 ); //$NON-NLS-1$

    /* Delete existing data */
    // TODO: should we always do that? What about existing results etc.?
    // TODO: give warning to user!
    if( simulationFolder.exists() )
    {
      monitor.subTask( Messages.getString("RefreshSimulationsOperation_5") ); //$NON-NLS-1$
      simulationFolder.delete( false, false, new SubProgressMonitor( monitor, 10 ) );
    }

    /* create the simulation */
    final IStatus createStatus = createSimulation( simulation, simulationFolder, new SubProgressMonitor( monitor, 40 ) );
    log.add( createStatus );

    /* update the simulation */
    final UpdateSimulationWorker updateWorker = new UpdateSimulationWorker( simulationFolder );
    final IStatus updateStatus = updateWorker.execute( new SubProgressMonitor( monitor, 50 ) );
    log.add( updateStatus );

    final String name = simulation.getDescription();
    return log.asMultiStatusOrOK( name, name );
  }

  static IFolder createFolder( final IContainer baseFolder, final NAControl simulation )
  {
    final String name = simulation.getDescription();
    /* Build simulation folder */
    // FIXME: encode does not work, as the gmlSerializer creates URLs from files without encoding, so the encoding will
    // be lost then
    // final String encodedName = URLEncoder.encode( name, Charsets.UTF_8.name() );
    final String encodedName = name;
    return baseFolder.getFolder( new Path( encodedName ) );
  }

  private IStatus createSimulation( final NAControl simulation, final IFolder simulationFolder, final IProgressMonitor monitor ) throws CoreException
  {
    final RrmSimulation calcCase = new RrmSimulation( simulationFolder );
    final IFile simulationFile = calcCase.getCalculationGml();

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

      FeatureHelper.copyData( simulation, targetFeature );

      GmlSerializer.saveWorkspace( simulationWorkspace, simulationFile );
    }
    catch( final Exception e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e, Messages.getString("RefreshSimulationsOperation_6"), simulation.getName() ); //$NON-NLS-1$
      throw new CoreException( status );
    }

    return createStatus;
  }
}