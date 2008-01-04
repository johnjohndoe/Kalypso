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
import java.util.Date;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModelGroup;
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
public class Model1D2DSimulation
{
  private static final String STRING_DLG_TITLE = Messages.getString( "CalculationUnitPerformComponent.2" );

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
      MessageDialog.openError( m_shell, STRING_DLG_TITLE, "Temporäres Simulationsverzeichnis konnte nicht erstellt werden: " + tmpDir );
    }
    finally
    {
      if( tmpDir != null && !Boolean.valueOf( Platform.getDebugOption( IKalypsoSimulationUIConstants.DEBUG_KEEP_SIM_FILES ) ) )
        FileUtilities.deleteRecursive( tmpDir );
    }
  }

  /**
   * First level of calculation:
   * <ul>
   * <li>initialize logging</li>
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

      final IGeoStatus logStatus = geoLog.formatLog( IStatus.INFO, "Start der Simulation" );
      final Date startTime = logStatus.getTime();

      process2( tmpDir, outputDir, startTime, geoLog );

      geoLog.formatLog( IStatus.INFO, "Ende der Simulation" );
    }
    catch( final InvocationTargetException e )
    {
      MessageDialog.openError( m_shell, STRING_DLG_TITLE, "Simulation-Log konnte nicht initialisiert werden: " + e.getTargetException().toString() );
    }
    catch( final CoreException e )
    {
      // REMARK: this should only happen if the data cannot be retrieved from the caseDataProvider and so should never
      // happen...
      ErrorDialog.openError( m_shell, STRING_DLG_TITLE, "Fehler bei der Simulation", e.getStatus() );
    }
    finally
    {
      if( geoLog != null )
      {
        try
        {
          geoLog.close();
          final IStatusCollection statusCollection = geoLog.getStatusCollection();
          final GMLWorkspace workspace = statusCollection.getWrappedFeature().getWorkspace();
          final File loggerFile = new File( outputDir, "simulation.log" );
          GmlSerializer.serializeWorkspace( loggerFile, workspace, "UTF-8" );
        }
        catch( final Throwable e )
        {
          MessageDialog.openError( m_shell, STRING_DLG_TITLE, "Simulation-Log konnte nicht geschrieben werden: " + e.toString() );
        }
      }
    }
  }

  /**
   * First level of calculation:
   * <ul>
   * <li>run rma10s in progress dialog</li>
   * <li>run result processing in progress dialog</li>
   * </ul>
   */
  public void process2( final File tmpDir, final File outputDir, final Date startTime, final IGeoLog geoLog ) throws CoreException
  {
    final IWorkbench workbench = PlatformUI.getWorkbench();

    /* Process rma10s calculation */
    final ICoreRunnableWithProgress calculationOperation = new ICoreRunnableWithProgress()
    {
      public IStatus execute( IProgressMonitor monitor ) throws CoreException
      {
        return runCalculationLevel0( tmpDir, geoLog, monitor );
      }
    };

    final IStatus calculationStatus = RunnableContextHelper.execute( workbench.getProgressService(), true, true, calculationOperation );
    if( calculationStatus.isOK() )
      MessageDialog.openInformation( m_shell, STRING_DLG_TITLE, Messages.getString( "CalculationUnitMetaTable.16" ) ); //$NON-NLS-1$ 
    else if( calculationStatus.matches( IStatus.CANCEL ) )
    {
      MessageDialog.openInformation( m_shell, STRING_DLG_TITLE, "Die Operation wurde durch den Benutzer abgebrochen." ); //$NON-NLS-1$
      return;
    }
    else
    {
      ErrorDialog.openError( m_shell, STRING_DLG_TITLE, Messages.getString( "CalculationUnitPerformComponent.3" ), calculationStatus ); //$NON-NLS-1$
      if( calculationStatus.matches( IStatus.ERROR ) )
        return;
    }

    /* Result processing */
    final ResultManager resultRunner = new ResultManager( tmpDir, outputDir, "A", startTime, m_caseDataProvider, m_unitFolder, geoLog );
    final Date[] calculatedSteps = resultRunner.getCalculatedSteps();

    // TODO: unterscheide verschiedene Problemarten:
    // - 'programmier- bzw. datenfehler' rechnung konnte gar nicht gestartet werden
    // - keine Ergebnisse
    // - teilergebnisse
    final boolean deleteAll = false;
    final boolean deleteFollowers = true;
    final Date[] userCalculatedSteps = calculatedSteps;

    final ICoreRunnableWithProgress resultOperation = new ICoreRunnableWithProgress()
    {
      public IStatus execute( IProgressMonitor monitor ) throws CoreException
      {
        return resultRunner.process( deleteAll, deleteFollowers, userCalculatedSteps, monitor );
      }
    };

    final IStatus resultStatus = RunnableContextHelper.execute( workbench.getProgressService(), true, true, resultOperation );
    if( resultStatus.isOK() )
      MessageDialog.openInformation( m_shell, "Ergebnisse auswerten", "Alle Ergebnisse wurden erfolgreich ausgewertet." ); //$NON-NLS-1$ 
    else if( resultStatus.matches( IStatus.CANCEL ) )
      MessageDialog.openInformation( m_shell, "Ergebnisse auswerten", "Die Operation wurde durch den Benutzer abgebrochen." ); //$NON-NLS-1$
    else
      ErrorDialog.openError( m_shell, "Ergebnisse auswerten", "Fehler beim Auswerten der Ergebnisse", resultStatus ); //$NON-NLS-1$ 
  }

  /**
   * Very first level of simulation. *
   * <ul>
   * <li>Sets the calcUnit to the control-model and saves it</li>
   * </ul>
   * 
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  public IStatus runCalculationLevel0( final File tmpDir, final IGeoLog geoLog, final IProgressMonitor monitor ) throws CoreException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, "Simulation wird gestartet", 100 );
    progress.subTask( "Daten werden gespeichert..." );

    /**
     * Set the unit to calculate into the Control-Model as active unit, and save all models.<br>
     * 
     * TODO: this is a bit fishy... Better would be to couple calc-units and its corresponding control-models more
     * closely. Then no search for and/or setting of active unit should be necessary any more.
     */
    final IControlModelGroup model = m_caseDataProvider.getModel( IControlModelGroup.class );
    IControlModel1D2D activeControlModel = null;
    for( final IControlModel1D2D controlModel : model.getModel1D2DCollection() )
    {
      final ICalculationUnit currentCalcUnit = controlModel.getCalculationUnit();
      if( currentCalcUnit != null )
      {
        if( m_calculationUnit.equals( currentCalcUnit.getGmlID() ) )
        {
          final Feature feature = model.getModel1D2DCollection().getWrappedFeature();
          final FeatureChange change = new FeatureChange( feature, feature.getFeatureType().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_XP_ACTIVE_MODEL ), controlModel.getGmlID() );
          final ChangeFeaturesCommand command = new ChangeFeaturesCommand( feature.getWorkspace(), new FeatureChange[] { change } );
          try
          {
            final CommandableWorkspace commandableWorkspace = Util.getCommandableWorkspace( IControlModelGroup.class );
            commandableWorkspace.postCommand( command );
          }
          catch( final Exception e )
          {
            e.printStackTrace();
            return StatusUtilities.createErrorStatus( "Could not set active control model for: " + m_calculationUnit );
          }
          activeControlModel = controlModel;

          // Saves ALL models, this is not really necessary but not really a problem, as only
          // dirty models get saved (and probably here only the control model is dirty).
          m_caseDataProvider.saveModel( null );

          // one found control model is enough
          break;
        }
      }
    }

    ProgressUtilities.worked( progress, 2 );
    progress.subTask( "" );

    if( activeControlModel == null )
      return StatusUtilities.createErrorStatus( "Could not find active control model for: " + m_calculationUnit );

    /* Reset task name so it shows something more meaningful */
    final String simMsg = String.format( "Simulation von '%s'", activeControlModel.getName() );
    progress.setTaskName( simMsg );

    return runCalculationLevel1( tmpDir, progress.newChild( 98 ), geoLog );
  }

  /**
   * Second level of calculation:
   * <ul>
   * <li>initialize rma10calculation and run it</li>
   * </ul>
   */
  private IStatus runCalculationLevel1( final File tmpDir, final IProgressMonitor monitor, final IGeoLog geoLog ) throws CoreException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, 100 );

    /* Initialize the calculation... */
    geoLog.formatLog( IStatus.INFO, "Eingangsdaten für Simulation werden gelesen." );
    progress.subTask( "Eingangsdaten für Simulation werden gelesen..." );
    final IFEDiscretisationModel1d2d discModel = m_caseDataProvider.getModel( IFEDiscretisationModel1d2d.class );
    final IFlowRelationshipModel flowModel = m_caseDataProvider.getModel( IFlowRelationshipModel.class );
    final IControlModelGroup controlModel = m_caseDataProvider.getModel( IControlModelGroup.class );
    final IRoughnessClsCollection roughnessModel = m_caseDataProvider.getModel( IRoughnessClsCollection.class );
    final IControlModel1D2D controlModelToCalc = controlModel.getModel1D2DCollection().getActiveControlModel();

    ProgressUtilities.worked( progress, 2 );

    /* ... and run it! */
    final RMA10Calculation calculation = new RMA10Calculation( tmpDir, geoLog, discModel, flowModel, controlModelToCalc, roughnessModel, m_scenarioFolder );
    return calculation.runCalculation( progress.newChild( 50 ) );
  }
}
