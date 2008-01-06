/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
import java.util.Arrays;
import java.util.Date;
import java.util.SortedSet;
import java.util.TreeSet;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModelGroup;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsosimulationmodel.core.ICommandPoster;
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
import org.kalypsodeegree_impl.gml.binding.commons.IGeoStatus;
import org.kalypsodeegree_impl.gml.binding.commons.IStatusCollection;

import de.renew.workflow.connector.cases.ICaseDataProvider;

/**
 * Starting point for running 1d2d simulations.
 * 
 * @author Gernot Belger
 */
public class Model1D2DSimulation implements ISimulation1D2DConstants
{
  private static final String SIMULATION_LOG_GML = "simulation_log.gml";

  private static final String STRING_DLG__TITLE_PROCESS_RESULTS = "Ergebnisse auswerten";

  private static final String STRING_DLG_TITLE_RMA10S = Messages.getString( "CalculationUnitPerformComponent.2" );

  private final ICaseDataProvider<IModel> m_caseDataProvider;

  private final IFolder m_scenarioFolder;

  private final IFolder m_unitFolder;

  private final String m_calculationUnit;

  private final Shell m_shell;

  public Model1D2DSimulation( final Shell shell, final ICaseDataProvider<IModel> caseDataProvider, final IFolder scenarioFolder, final IFolder unitFolder, final String calculationUnit )
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
      MessageDialog.openError( m_shell, STRING_DLG_TITLE_RMA10S, "Temporäres Simulationsverzeichnis konnte nicht erstellt werden: " + tmpDir );
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
    final IWorkbench workbench = PlatformUI.getWorkbench();

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

    /* Process rma10s calculation */
    final ICoreRunnableWithProgress calculationOperation = new ICoreRunnableWithProgress()
    {
      public IStatus execute( final IProgressMonitor monitor ) throws CoreException
      {
        return calculation.runCalculation( monitor );
      }
    };

    final IGeoStatus logStatus = geoLog.formatLog( IStatus.INFO, CODE_RUNNING, "Start der Simulation" );
    final Date startTime = logStatus.getTime();
    final IStatus simulationStatus = RunnableContextHelper.execute( workbench.getProgressService(), true, true, calculationOperation );

    /* Evaluate result of simulation */
    final ResultManager resultManager = new ResultManager( tmpDir, outputDir, "A", controlModel, flowModel, scenarioResultMeta, geoLog );
    final Date[] calculatedSteps = resultManager.findCalculatedSteps();

    final ProcessResultsBean processBean = evaluateSimulationResult( simulationStatus, calculatedSteps, controlModel, geoLog );
    if( processBean == null )
      return;

    /* Result processing */
    final ICoreRunnableWithProgress resultOperation = new ICoreRunnableWithProgress()
    {
      public IStatus execute( IProgressMonitor monitor ) throws CoreException
      {
        return processResults( resultManager, processBean, startTime, simulationStatus, geoLog, monitor );
      }
    };

    geoLog.formatLog( IStatus.INFO, CODE_RUNNING, "Start der Ergebnisauswertung" );
    final IStatus resultStatus = RunnableContextHelper.execute( workbench.getProgressService(), true, true, resultOperation );
    if( resultStatus.isOK() )
    {
      geoLog.formatLog( IStatus.OK, CODE_RUNNING, "Ergebnisauswertung erfolgreich beendet" );
      MessageDialog.openInformation( m_shell, STRING_DLG__TITLE_PROCESS_RESULTS, "Alle Ergebnisse wurden erfolgreich ausgewertet." );
    }
    else if( resultStatus.matches( IStatus.CANCEL ) )
    {
      geoLog.formatLog( IStatus.WARNING, CODE_RUNNING, "Ergebnisauswertung durch den Benutzer abgebrochen." );
      MessageDialog.openInformation( m_shell, STRING_DLG__TITLE_PROCESS_RESULTS, "Die Operation wurde durch den Benutzer abgebrochen." );
    }
    else
    {
      geoLog.formatLog( IStatus.ERROR, CODE_RUNNING, "Ergebnisauswertung mit Fehler beendet." );
      geoLog.log( resultStatus );
      ErrorDialog.openError( m_shell, STRING_DLG__TITLE_PROCESS_RESULTS, "Fehler beim Auswerten der Ergebnisse", resultStatus );
    }
  }

  private ProcessResultsBean evaluateSimulationResult( final IStatus simulationStatus, final Date[] calculatedSteps, final IControlModel1D2D controlModel, final IGeoLog geoLog )
  {
    // TODO: distinguish different type of problems:
    // - exe/data errors: calculation could not be started at all
    // - no results
    // - some results

    // TODO: show dialog to user where he can
    // - choose, which steps to process
    // - choose, which results to be deleted

    if( simulationStatus.isOK() )
    {
      geoLog.formatLog( IStatus.OK, CODE_RUNNING, "Simulation erfolgreich beendet" );
      MessageDialog.openInformation( m_shell, STRING_DLG_TITLE_RMA10S, Messages.getString( "CalculationUnitMetaTable.16" ) ); //$NON-NLS-1$
    }
    else if( simulationStatus.matches( IStatus.CANCEL ) )
    {
      geoLog.formatLog( IStatus.WARNING, CODE_RUNNING, "Simulation durch den Benutzer abgebrochen." );
      MessageDialog.openInformation( m_shell, STRING_DLG_TITLE_RMA10S, "Die Operation wurde durch den Benutzer abgebrochen." ); //$NON-NLS-1$
      return null;
    }
    else
    {
      geoLog.log( simulationStatus );
      ErrorDialog.openError( m_shell, STRING_DLG_TITLE_RMA10S, Messages.getString( "CalculationUnitPerformComponent.3" ), simulationStatus ); //$NON-NLS-1$
      if( simulationStatus.matches( IStatus.ERROR ) )
      {
        geoLog.formatLog( IStatus.ERROR, CODE_RUNNING, "Simulation mit Fehler beendet. Abbruch der Ergebnisauswertung." );
        return null;
      }
      else if( simulationStatus.matches( IStatus.WARNING ) )
      {
        geoLog.formatLog( IStatus.WARNING, CODE_RUNNING, "Simulation mit Warnung beendet. Ergebnisauswertung wird durchgeführt." );
        // Only a warning: fall through to continue result processing
      }
    }

    final ProcessResultsBean bean = new ProcessResultsBean();

    // Remark: if not restart, always delete everything, in that
    // case do not ask the user either
    bean.deleteAll = !controlModel.getRestart();
    bean.deleteFollowers = true;
    bean.userCalculatedSteps = calculatedSteps;

    return bean;
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

  protected IStatus processResults( final ResultManager resultManager, final ProcessResultsBean processBean, final Date startTime, final IStatus simulationStatus, final IGeoLog geoLog, final IProgressMonitor monitor ) throws CoreException
  {
    final IControlModel1D2D controlModel = resultManager.getControlModel();
    final IScenarioResultMeta scenarioMeta = resultManager.getScenarioMeta();
    final ICalculationUnit calculationUnit = controlModel.getCalculationUnit();
    final File outputDir = resultManager.getOutputDir();

    final SubMonitor progress = SubMonitor.convert( monitor, "Ergebnisauswertung: " + calculationUnit.getName(), 100 );

    /* Process Results */

    // Step 1: Delete existing results and save result-DB (in case of problems while processing)
    geoLog.formatLog( IStatus.INFO, CODE_RUNNING_FINE, "Bestehende Ergebnisse werden gelöscht." );
    deleteExistingResults( scenarioMeta, calculationUnit, processBean, progress.newChild( 5 ) );

    // Step 2: Create or find CalcUnitMeta and fill with data
    final ICalcUnitResultMeta existingCalcUnitMeta = scenarioMeta.findCalcUnitMetaResult( calculationUnit.getGmlID() );
    final ICalcUnitResultMeta calcUnitMeta;
    if( existingCalcUnitMeta == null )
      calcUnitMeta = scenarioMeta.getChildren().addNew( ICalcUnitResultMeta.QNAME, ICalcUnitResultMeta.class );
    else
      calcUnitMeta = existingCalcUnitMeta;

    // Step 3: Process results and add new entries to result-DB
    final IStatus processResultsStatus = resultManager.processResults( calcUnitMeta, progress.newChild( 90 ) );
    geoLog.log( processResultsStatus );

    // Step 4:
    calcUnitMeta.setCalcStartTime( startTime );
    calcUnitMeta.setCalcUnit( calculationUnit.getGmlID() );
    calcUnitMeta.setName( calculationUnit.getName() );
    calcUnitMeta.setDescription( calculationUnit.getDescription() );
    calcUnitMeta.setPath( new Path( m_unitFolder.getName() ) );
    calcUnitMeta.setStatus( simulationStatus );
    calcUnitMeta.setCalcEndTime( new Date() );

    if( processResultsStatus.matches( IStatus.ERROR | IStatus.CANCEL ) )
      return processResultsStatus;

    if( processResultsStatus.matches( IStatus.WARNING | IStatus.INFO ) )
    {
      if( ErrorDialog.openError( m_shell, STRING_DLG__TITLE_PROCESS_RESULTS, "Ergebnisauswertung wurde mit Warnung beendet.\nSollen die Ergebnisse übernommen werden?", processResultsStatus, IStatus.WARNING
          | IStatus.INFO ) == ErrorDialog.CANCEL )
        return processResultsStatus;
    }

    // Step 5: Add geo log to calcMeta as document
    final IDocumentResultMeta logMeta = calcUnitMeta.getChildren().addNew( IDocumentResultMeta.QNAME, IDocumentResultMeta.class );
    logMeta.setName( "Simulations-Log" );
    logMeta.setDescription( "Die Log-Datei der letzten Simulation dieser Berechnungseinheit." );
    logMeta.setDocumentType( IDocumentResultMeta.DOCUMENTTYPE.log );
    logMeta.setPath( new Path( SIMULATION_LOG_GML ) );

    // Step 5: Move results into workspace and save result-DB
    geoLog.formatLog( IStatus.INFO, CODE_RUNNING_FINE, "Ergebnisse werden in Arbeitsbereich verschoben und Ergebnisdatenbank aktualisiert." );
    return moveResults( outputDir, progress.newChild( 5 ) );
  }

  /**
   * Delete all existing results inside the current result database.
   */
  private IStatus deleteExistingResults( final IScenarioResultMeta scenarioMeta, final ICalculationUnit calcUnit, final ProcessResultsBean processBean, final IProgressMonitor monitor ) throws CoreException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, 100 );
    progress.subTask( "Bestehende Ergebnisse werden gelöscht..." );

    final ICalcUnitResultMeta calcUnitMeta = scenarioMeta.findCalcUnitMetaResult( calcUnit.getGmlID() );

    /* If no results available yet, nothing to do. */
    if( calcUnitMeta == null )
      return Status.OK_STATUS;

    final Date[] stepsToDelete = findStepsToDelete( calcUnitMeta, processBean );
    ProgressUtilities.worked( progress, 5 );

    final IStatus result = ResultMeta1d2dHelper.deleteResults( calcUnitMeta, stepsToDelete, progress.newChild( 90 ) );
    if( !result.isOK() )
      throw new CoreException( result );

    /* Save result DB */
    try
    {
      ((ICommandPoster) m_caseDataProvider).postCommand( IScenarioResultMeta.class, new EmptyCommand( "", false ) );
    }
    catch( final InvocationTargetException e )
    {
      throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, "Fehler beim Speichern der Ergebnisdatenbank", e.getTargetException() ) );
    }

    m_caseDataProvider.saveModel( IScenarioResultMeta.class, progress.newChild( 5 ) );

    return result;
  }

  private static Date[] findStepsToDelete( final ICalcUnitResultMeta calcUnitMeta, final ProcessResultsBean processBean )
  {
    final Date[] existingSteps = ResultMeta1d2dHelper.getStepDates( calcUnitMeta );

    if( processBean.deleteAll )
      return existingSteps;

    final SortedSet<Date> dates = new TreeSet<Date>();

    /* Always delete all calculated steps */
    dates.addAll( Arrays.asList( processBean.userCalculatedSteps ) );

    if( processBean.deleteFollowers && !dates.isEmpty() )
    {
      /* Delete all steps later than the first calculated */
      final Date firstCalculated = dates.first();
      for( final Date date : existingSteps )
      {
        if( date.after( firstCalculated ) )
          dates.add( date );
      }
    }

    return dates.toArray( new Date[dates.size()] );
  }

  private IStatus moveResults( final File outputDir, final IProgressMonitor monitor )
  {
    final SubMonitor progress = SubMonitor.convert( monitor, 100 );
    progress.subTask( "Ergebnisdaten werden in Arbeitsbereich verschoben..." );

    try
    {
      final File unitWorkspaceDir = m_unitFolder.getLocation().toFile();
      FileUtils.forceMkdir( unitWorkspaceDir );
      FileUtilities.moveContents( outputDir, unitWorkspaceDir );
      ProgressUtilities.worked( progress, 70 );

      m_unitFolder.refreshLocal( IResource.DEPTH_INFINITE, progress.newChild( 20 ) );

      /* Output dir should now be empty, so there is no sense in keeping it */
      outputDir.delete();

      ((ICommandPoster) m_caseDataProvider).postCommand( IScenarioResultMeta.class, new EmptyCommand( "", false ) );
      m_caseDataProvider.saveModel( IScenarioResultMeta.class, progress.newChild( 10 ) );

      return Status.OK_STATUS;
    }
    catch( final IOException e )
    {
      return StatusUtilities.createStatus( IStatus.ERROR, "Ergebnisdateien konnten nicht in den Arbeitsbereich verschoben werden", e );
    }
    catch( final InvocationTargetException e )
    {
      return StatusUtilities.createStatus( IStatus.ERROR, "Ergebnisdateien konnten nicht in den Arbeitsbereich verschoben werden", e.getTargetException() );
    }
    catch( final CoreException e )
    {
      return e.getStatus();
    }
  }

}
