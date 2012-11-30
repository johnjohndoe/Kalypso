/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.kalypsomodel1d2d.sim.preprocessing;

import java.io.File;
import java.io.IOException;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.Building1D2DConverter;
import org.kalypso.kalypsomodel1d2d.conv.BuildingIDProvider;
import org.kalypso.kalypsomodel1d2d.conv.Control1D2DConverter;
import org.kalypso.kalypsomodel1d2d.conv.Gml2RMA10SConv;
import org.kalypso.kalypsomodel1d2d.conv.SWANResults2RmaConverter;
import org.kalypso.kalypsomodel1d2d.conv.WQboundaryConditions1D2DConverter;
import org.kalypso.kalypsomodel1d2d.conv.results.RestartNodes;
import org.kalypso.kalypsomodel1d2d.conv.wind.IWindDataWriter;
import org.kalypso.kalypsomodel1d2d.conv.wind.RMA10WindDataWriter;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.sim.ISimulation1D2DConstants;
import org.kalypso.kalypsomodel1d2d.sim.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.geolog.IGeoLog;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCollection;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindModel;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * @author Gernot Belger
 */
public class PreRMAKalypsoWorker
{
  private final IGeoLog m_log;

  private final IPreRMAData m_preData;

  private final File m_tmpDir;

  private final PreRMAFiles m_outputFiles;

  public PreRMAKalypsoWorker( final IPreRMAData preData, final IGeoLog log, final File tmpDir, final PreRMAFiles outputFiles )
  {
    m_preData = preData;
    m_log = log;
    m_tmpDir = tmpDir;
    m_outputFiles = outputFiles;
  }

  public void execute( final IProgressMonitor progressMonitor ) throws CoreException, SimulationException, IOException
  {
    final IControlModel1D2D controlModel = m_preData.getControlModel();

    final IFEDiscretisationModel1d2d discretisationModel = m_preData.getDiscretisationModel();

    final IFlowRelationshipModel flowRelationshipModel = m_preData.getFlowRelationshipModel();

    final IRoughnessClsCollection roughnessModel = m_preData.getRoughnessClassCollection();

    final IWindModel windModel = m_preData.getWindModel();

    final RestartNodes restartNodes = m_preData.prepareRestart( controlModel, m_tmpDir );

    /* write results */
    final ICalculationUnit calculationUnit = controlModel.getCalculationUnit();
    writeRma10Files( progressMonitor, discretisationModel, flowRelationshipModel, windModel, roughnessModel, restartNodes, controlModel, calculationUnit );
  }

  private void writeRma10Files( final IProgressMonitor monitor, final IFEDiscretisationModel1d2d discretisationModel, final IFlowRelationshipModel flowRelationshipModel, final IWindModel windRelationshipModel, final IRoughnessClsCollection roughnessModel, final RestartNodes restartNodes, final IControlModel1D2D controlModel, final ICalculationUnit calculationUnit ) throws CoreException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, 100 );

    try
    {
      m_log.formatLog( IStatus.INFO, ISimulation1D2DConstants.CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.3" ) ); //$NON-NLS-1$

      /* Read restart data */
      m_log.formatLog( IStatus.INFO, ISimulation1D2DConstants.CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.4" ) ); //$NON-NLS-1$
      progress.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.5" ) ); //$NON-NLS-1$

      ProgressUtilities.worked( progress, 10 );

      /* .2d File */
      m_log.formatLog( IStatus.INFO, ISimulation1D2DConstants.CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.6" ) ); //$NON-NLS-1$
      monitor.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.7" ) ); //$NON-NLS-1$

      final Gml2RMA10SConv converter2D = new Gml2RMA10SConv( discretisationModel, flowRelationshipModel, calculationUnit, roughnessModel, restartNodes, false, true, m_log );
      converter2D.writeMesh( m_outputFiles.getMeshFile() );
      ProgressUtilities.worked( progress, 60 );

      /* Control File */
      m_log.formatLog( IStatus.INFO, ISimulation1D2DConstants.CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.8" ) ); //$NON-NLS-1$
      progress.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.9" ) ); //$NON-NLS-1$

      final BuildingIDProvider buildingProvider = converter2D.getBuildingProvider();
      final Control1D2DConverter controlConverter = new Control1D2DConverter( controlModel, calculationUnit, flowRelationshipModel, roughnessModel, converter2D, buildingProvider, m_log );
      controlConverter.writeR10File( m_outputFiles.getControlFile() );
      ProgressUtilities.worked( progress, 10 );

      /* Building File */
      m_log.formatLog( IStatus.INFO, ISimulation1D2DConstants.CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.10" ) ); //$NON-NLS-1$
      progress.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.11" ) ); //$NON-NLS-1$
      final Building1D2DConverter buildingConverter = new Building1D2DConverter( buildingProvider );
      buildingConverter.writeBuildingFile( m_outputFiles.getBuildingFile() );
      ProgressUtilities.worked( progress, 10 );

      /* Wind File */
      m_log.formatLog( IStatus.INFO, ISimulation1D2DConstants.CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.16" ) ); //$NON-NLS-1$
      progress.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.17" ) ); //$NON-NLS-1$
      final GM_Envelope lGmEnvelope = calculationUnit.getBoundingBox();
      final IWindDataWriter lRMA10WindWriter = new RMA10WindDataWriter( m_tmpDir, lGmEnvelope, controlConverter.getListDateSteps(), windRelationshipModel.getWindDataModelSystems() );
      lRMA10WindWriter.setWindDataModel( windRelationshipModel );
      lRMA10WindWriter.write( controlModel.isConstantWindSWAN() );
      ProgressUtilities.worked( progress, 10 );

      if( controlModel.isRestartAfterSWAN() )
      {
        // TODO check: not used as output of this simulation

        /* Surface traction File */
        m_log.formatLog( IStatus.INFO, ISimulation1D2DConstants.CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.10" ) ); //$NON-NLS-1$
        progress.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.11" ) ); //$NON-NLS-1$
        final File lFileSurfaceTractiondFile = new File( m_tmpDir, ISimulation1D2DConstants.SURFACE_TRACTTION_RMA10_File );
        final SWANResults2RmaConverter lRMA10SurfaceTractionWriter = new SWANResults2RmaConverter();
        lRMA10SurfaceTractionWriter.writeRMAWaterSurfaceASCDataFile( lFileSurfaceTractiondFile );
      }
      ProgressUtilities.worked( progress, 10 );

      /* W/Q BC File */
      m_log.formatLog( IStatus.INFO, ISimulation1D2DConstants.CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.12" ) ); //$NON-NLS-1$
      progress.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.13" ) ); //$NON-NLS-1$
      final WQboundaryConditions1D2DConverter bc1D2DConverter = new WQboundaryConditions1D2DConverter( controlConverter.getBoundaryConditionsIDProvider() );
      bc1D2DConverter.writeWQbcFile( m_outputFiles.getBcWqFile() );
      ProgressUtilities.worked( progress, 10 );
    }
    catch( final CoreException e )
    {
      throw e;
    }
    catch( final IOException e )
    {
      final String msg = Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.14", e.getLocalizedMessage() ); //$NON-NLS-1$
      throw new CoreException( new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, ISimulation1D2DConstants.CODE_PRE, msg, e ) );
    }
  }
}