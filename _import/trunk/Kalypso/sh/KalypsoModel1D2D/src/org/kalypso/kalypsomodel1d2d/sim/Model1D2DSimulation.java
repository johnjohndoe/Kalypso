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
package org.kalypso.kalypsomodel1d2d.sim;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModelGroup;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsosimulationmodel.core.Util;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.core.modeling.IModel;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCollection;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.simulation.core.util.SimulationUtilitites;
import org.kalypso.simulation.ui.IKalypsoSimulationUIConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.gml.binding.commons.IStatusCollection;

import de.renew.workflow.connector.cases.ICaseDataProvider;

/**
 * Starting point for running 1d2d simulations.
 * 
 * @author Gernot Belger
 */
public class Model1D2DSimulation implements ISimulation1D2DConstants
{
  private static final String STRING_DLG_TITLE_RMA10S = Messages.getString( "CalculationUnitPerformComponent.2" );

  private final ICaseDataProvider<IModel> m_caseDataProvider;

  private final IContainer m_scenarioFolder;

  private final IFolder m_unitFolder;

  private final String m_calculationUnit;

  private final Shell m_shell;

  public Model1D2DSimulation( final Shell shell, final ICaseDataProvider<IModel> caseDataProvider, final IContainer scenarioFolder, final IFolder unitFolder, final String calculationUnit )
  {
    m_shell = shell;
    m_caseDataProvider = caseDataProvider;
    m_scenarioFolder = scenarioFolder;
    m_unitFolder = unitFolder;
    m_calculationUnit = calculationUnit;
  }

  /**
   * Starting point of calculation
   * <ul>
   * <li>create temporary directory and make sure it gets deleted after calculation</li>
   * <li>create output directory and make sure it gets synchronized with workspace after calculation</li>
   * </ul>
   */
  public void process( )
  {
    File tmpDir = null;

    try
    {
      tmpDir = SimulationUtilitites.createSimulationTmpDir( "rma10s" + m_calculationUnit );

      final File outputDir = SimulationUtilitites.createSimulationTmpDir( "output" + m_calculationUnit );
      FileUtils.forceMkdir( outputDir );

      process1( tmpDir, outputDir );
    }
    catch( final IOException e )
    {
      MessageDialog.openError( m_shell, STRING_DLG_TITLE_RMA10S, "Tempor‰res Simulationsverzeichnis konnte nicht erstellt werden: " + tmpDir );
    }
    finally
    {
      if( tmpDir != null && !Boolean.valueOf( Platform.getDebugOption( IKalypsoSimulationUIConstants.DEBUG_KEEP_SIM_FILES ) ) )
        FileUtilities.deleteRecursive( tmpDir );
    }
  }

  /**
   * Second level of calculation:
   * <ul>
   * <li>initialize logging</li>
   * <li>start level-2</li>
   * <li>save log file</li>
   * </ul>
   */
  private void process1( final File tmpDir, final File outputDir )
  {
    // First level of calculation: initialize logging, then call real calculation stuff
    IGeoLog geoLog = null;
    try
    {
      /* Initialize the logging facilities */
      final ILog model1d2dlog = KalypsoModel1D2DPlugin.getDefault().getLog();
      // Additionally, everything is logged into the log of the KalypsoModel1D2D-Plug-In
      geoLog = new GeoLog( model1d2dlog );

      process2( tmpDir, outputDir, geoLog );
    }
    catch( final InvocationTargetException e )
    {
      MessageDialog.openError( m_shell, STRING_DLG_TITLE_RMA10S, "Simulation-Log konnte nicht initialisiert werden: " + e.getTargetException().toString() );
    }
    catch( final CoreException e )
    {
      // REMARK: this should only happen if the data cannot be retrieved from the caseDataProvider and so should never
      // happen...
      ErrorDialog.openError( m_shell, STRING_DLG_TITLE_RMA10S, "Fehler bei der Simulation", e.getStatus() );
    }
    finally
    {
      if( geoLog != null )
      {
        try
        {
          /* Close and save the geo log */
          geoLog.close();
          final IStatusCollection statusCollection = geoLog.getStatusCollection();
          final GMLWorkspace workspace = statusCollection.getWrappedFeature().getWorkspace();
          // REMARK: we directly save the log into the unit-folder, as the results already where moved form the output
          // directory
          final File simDir = m_unitFolder.getLocation().toFile();
          final File loggerFile = new File( simDir, SIMULATION_LOG_GML );
          GmlSerializer.serializeWorkspace( loggerFile, workspace, "UTF-8" );
          m_unitFolder.refreshLocal( IFolder.DEPTH_ONE, new NullProgressMonitor() );
        }
        catch( final Throwable e )
        {
          MessageDialog.openError( m_shell, STRING_DLG_TITLE_RMA10S, "Simulation-Log konnte nicht geschrieben werden: " + e.toString() );
        }
      }
    }
  }

  /**
   * third level of calculation:
   * <ul>
   * <li>run rma10s in progress dialog</li>
   * <li>show rma10s results and ask user how/if to process results</li>
   * <li>run result processing in progress dialog</li>
   * </ul>
   */
  public void process2( final File tmpDir, final File outputDir, final IGeoLog geoLog ) throws CoreException
  {
    final IFEDiscretisationModel1d2d discModel = m_caseDataProvider.getModel( IFEDiscretisationModel1d2d.class );
    final IFlowRelationshipModel flowModel = m_caseDataProvider.getModel( IFlowRelationshipModel.class );
    final IControlModelGroup controlModelGroup = m_caseDataProvider.getModel( IControlModelGroup.class );
    final IRoughnessClsCollection roughnessModel = m_caseDataProvider.getModel( IRoughnessClsCollection.class );
    final IScenarioResultMeta scenarioResultMeta = m_caseDataProvider.getModel( IScenarioResultMeta.class );

    /* Set correct activeControlModel according to selected calcUnit */
    final IControlModel1D2D controlModel = saveControlModel( controlModelGroup );
    if( controlModel == null )
      throw new CoreException( StatusUtilities.createErrorStatus( "Could not find active control model for: " + m_calculationUnit ) );

    final RMA10Calculation calculation = new RMA10Calculation( tmpDir, geoLog, discModel, flowModel, controlModel, roughnessModel, m_scenarioFolder );
    final ResultManager resultManager = new ResultManager( tmpDir, outputDir, "A", controlModel, flowModel, scenarioResultMeta, geoLog );

    final RMA10CalculationWizard calcWizard = new RMA10CalculationWizard( calculation, resultManager, m_unitFolder, m_caseDataProvider, geoLog );
    calcWizard.setWindowTitle( STRING_DLG_TITLE_RMA10S );
    final WizardDialog2 calcDialog = new WizardDialog2( m_shell, calcWizard );
    calcDialog.open();
  }

  /**
   * Sets the unit to calculate into the Control-Model as active unit, and save all models.<br>
   * 
   * TODO: this is a bit fishy... Better would be to couple calc-units and its corresponding control-models more
   * closely. Then no search for and/or setting of active unit should be necessary any more.
   */
  private IControlModel1D2D saveControlModel( final IControlModelGroup controlModelGroup ) throws CoreException
  {
    for( final IControlModel1D2D controlModel : controlModelGroup.getModel1D2DCollection() )
    {
      final ICalculationUnit currentCalcUnit = controlModel.getCalculationUnit();
      if( currentCalcUnit != null )
      {
        if( m_calculationUnit.equals( currentCalcUnit.getGmlID() ) )
        {
          final Feature feature = controlModelGroup.getModel1D2DCollection().getWrappedFeature();
          final FeatureChange change = new FeatureChange( feature, feature.getFeatureType().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_XP_ACTIVE_MODEL ), controlModel.getGmlID() );
          final ChangeFeaturesCommand command = new ChangeFeaturesCommand( feature.getWorkspace(), new FeatureChange[] { change } );
          try
          {
            final CommandableWorkspace commandableWorkspace = Util.getCommandableWorkspace( IControlModelGroup.class );
            commandableWorkspace.postCommand( command );
          }
          catch( final Throwable e )
          {
            throw new CoreException( StatusUtilities.createErrorStatus( "Could not set active control model for: " + m_calculationUnit ) );
          }

          // Saves ALL models, this is not really necessary but not really a problem, as only
          // dirty models get saved (and probably here only the control model is dirty).
          m_caseDataProvider.saveModel( null );

          return controlModel;
        }
      }
    }
    return null;
  }

}
