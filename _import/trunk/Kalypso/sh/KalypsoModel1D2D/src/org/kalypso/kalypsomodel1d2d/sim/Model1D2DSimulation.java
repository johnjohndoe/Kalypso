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
import java.util.Date;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.ControlModel1D2DCollection;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModelGroup;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.ui.geolog.GeoLog;
import org.kalypso.kalypsomodel1d2d.ui.geolog.IGeoLog;
import org.kalypso.kalypsosimulationmodel.core.Util;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.core.modeling.IModel;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCollection;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.simulation.core.util.SimulationUtilitites;
import org.kalypsodeegree.model.feature.Feature;

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
    File outputDir = null;

    try
    {
      tmpDir = SimulationUtilitites.createSimulationTmpDir( "rma10s" + m_calculationUnit );
      outputDir = SimulationUtilitites.createSimulationTmpDir( "output" + m_calculationUnit );

      FileUtils.forceMkdir( outputDir );

      process1( tmpDir, outputDir );
    }
    catch( final IOException e )
    {
      MessageDialog.openError( m_shell, STRING_DLG_TITLE_RMA10S, "Tempor‰res Simulationsverzeichnis konnte nicht erstellt werden: " + tmpDir );
    }
    finally
    {
      SimulationUtilitites.clearTmpDir( tmpDir );
      SimulationUtilitites.clearTmpDir( outputDir );
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
  }

  /**
   * third level of calculation:
   * <ul>
   * <li>run rma10s in wizard</li>
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

    /* Initialize result meta */
    final ICalculationUnit calculationUnit = controlModel.getCalculationUnit();
    final ICalcUnitResultMeta existingCalcUnitMeta = scenarioResultMeta.findCalcUnitMetaResult( calculationUnit.getGmlID() );
    final ICalcUnitResultMeta calcUnitMeta;
    if( existingCalcUnitMeta == null )
      calcUnitMeta = scenarioResultMeta.getChildren().addNew( ICalcUnitResultMeta.QNAME, ICalcUnitResultMeta.class );
    else
      calcUnitMeta = existingCalcUnitMeta;

    calcUnitMeta.setCalcStartTime( geoLog.getStartTime() );
    calcUnitMeta.setCalcUnit( calculationUnit.getGmlID() );
    calcUnitMeta.setName( calculationUnit.getName() );
    calcUnitMeta.setDescription( calculationUnit.getDescription() );
    calcUnitMeta.setPath( new Path( m_unitFolder.getName() ) );
    calcUnitMeta.setCalcEndTime( new Date() );

    // Add geo log to calcMeta as document
    final IDocumentResultMeta logMeta = calcUnitMeta.getChildren().addNew( IDocumentResultMeta.QNAME, IDocumentResultMeta.class );
    logMeta.setName( "Simulations-Log" );
    logMeta.setDescription( "Die Log-Datei der letzten Simulation dieser Berechnungseinheit." );
    logMeta.setDocumentType( IDocumentResultMeta.DOCUMENTTYPE.log );
    logMeta.setPath( new Path( SIMULATION_LOG_GML ) );

    final RMA10Calculation calculation = new RMA10Calculation( tmpDir, geoLog, discModel, flowModel, controlModel, roughnessModel, m_scenarioFolder );
    final ResultManager resultManager = new ResultManager( tmpDir, outputDir, "A", controlModel, flowModel, discModel, scenarioResultMeta, geoLog );

    final RMA10CalculationWizard calcWizard = new RMA10CalculationWizard( calculation, resultManager, m_unitFolder, m_caseDataProvider, geoLog );
    calcWizard.setWindowTitle( STRING_DLG_TITLE_RMA10S );
    calcWizard.setDialogSettings( PluginUtilities.getDialogSettings( KalypsoModel1D2DPlugin.getDefault(), "rma10simulation" ) );
    final WizardDialog2 calcDialog = new WizardDialog2( m_shell, calcWizard );
    calcDialog.setRememberSize( true );
    if( calcDialog.open() == Window.OK )
    {
      // save log

      // move results and save them

      // TODO: maybe just save log and result stuff depending on wizard result?
      // PROBLEM: if the user cancels during result processing, maybe old result already have been deleted...
    }

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
          final Feature feature = controlModelGroup.getModel1D2DCollection().getFeature();
          final FeatureChange change = new FeatureChange( feature, feature.getFeatureType().getProperty( ControlModel1D2DCollection.WB1D2DCONTROL_XP_ACTIVE_MODEL ), controlModel.getGmlID() );
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
