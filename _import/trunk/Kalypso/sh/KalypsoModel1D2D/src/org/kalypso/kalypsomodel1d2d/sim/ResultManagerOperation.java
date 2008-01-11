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
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsosimulationmodel.core.ICommandPoster;
import org.kalypso.kalypsosimulationmodel.core.modeling.IModel;

import de.renew.workflow.connector.cases.ICaseDataProvider;

/**
 * @author Gernot Belger
 */
public class ResultManagerOperation implements ICoreRunnableWithProgress, ISimulation1D2DConstants
{
  private final ResultManager m_resultManager;

  private final ProcessResultsBean m_processBean;

  private final IContainer m_unitFolder;

  private final IStatus m_simulationStatus;

  private final ICaseDataProvider<IModel> m_caseDataProvider;

  private final IGeoLog m_geoLog;

  public ResultManagerOperation( final ResultManager resultManager, final IContainer unitFolder, final IStatus simulationStatus, final ProcessResultsBean processBean, final ICaseDataProvider<IModel> caseDataProvider )
  {
    m_resultManager = resultManager;
    m_unitFolder = unitFolder;
    m_simulationStatus = simulationStatus;
    m_processBean = processBean;
    m_geoLog = m_resultManager.getGeoLog();
    m_caseDataProvider = caseDataProvider;
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  public IStatus execute( final IProgressMonitor monitor )
  {
    m_geoLog.formatLog( IStatus.INFO, CODE_RUNNING, "Start der Ergebnisauswertung" );

    final IStatus resultStatus = doExecute( monitor );

    // Adapt status
    if( resultStatus.isOK() )
      return m_geoLog.formatLog( IStatus.OK, CODE_RUNNING, "Ergebnisauswertung erfolgreich beendet" );

    if( resultStatus.matches( IStatus.CANCEL ) )
      return m_geoLog.formatLog( IStatus.CANCEL, CODE_RUNNING, "Ergebnisauswertung durch den Benutzer abgebrochen." );

    /* Warning or Error */
    m_geoLog.formatLog( IStatus.ERROR, CODE_RUNNING, "Ergebnisauswertung mit Fehler beendet." );
    m_geoLog.log( resultStatus );

    return resultStatus;
  }

  private IStatus doExecute( final IProgressMonitor monitor )
  {
    try
    {
      final IControlModel1D2D controlModel = m_resultManager.getControlModel();
      final IScenarioResultMeta scenarioMeta = m_resultManager.getScenarioMeta();
      final ICalculationUnit calculationUnit = controlModel.getCalculationUnit();
      final File outputDir = m_resultManager.getOutputDir();

      final SubMonitor progress = SubMonitor.convert( monitor, 100 );

      /* Process Results */

      // Step 1: Delete existing results and save result-DB (in case of problems while processing)
      m_geoLog.formatLog( IStatus.INFO, CODE_RUNNING_FINE, "Bestehende Ergebnisse werden gelöscht." );
      final IStatus deleteStatus = deleteExistingResults( scenarioMeta, calculationUnit, m_processBean, progress.newChild( 5 ) );
      if( deleteStatus.matches( IStatus.CANCEL ) )
        return deleteStatus;
      if( !deleteStatus.isOK() )
        m_geoLog.log( deleteStatus );

      // Step 2: Create or find CalcUnitMeta and fill with data
      final ICalcUnitResultMeta existingCalcUnitMeta = scenarioMeta.findCalcUnitMetaResult( calculationUnit.getGmlID() );
      final ICalcUnitResultMeta calcUnitMeta;
      if( existingCalcUnitMeta == null )
        calcUnitMeta = scenarioMeta.getChildren().addNew( ICalcUnitResultMeta.QNAME, ICalcUnitResultMeta.class );
      else
        calcUnitMeta = existingCalcUnitMeta;

      // Step 3: Process results and add new entries to result-DB
      final IStatus processResultsStatus = m_resultManager.processResults( calcUnitMeta, progress.newChild( 90 ) );
      m_geoLog.log( processResultsStatus );

      // Step 4:
      calcUnitMeta.setCalcStartTime( m_geoLog.getStartTime() );
      calcUnitMeta.setCalcUnit( calculationUnit.getGmlID() );
      calcUnitMeta.setName( calculationUnit.getName() );
      calcUnitMeta.setDescription( calculationUnit.getDescription() );
      calcUnitMeta.setPath( new Path( m_unitFolder.getName() ) );
      calcUnitMeta.setStatus( m_simulationStatus );
      calcUnitMeta.setCalcEndTime( new Date() );

      if( processResultsStatus.matches( IStatus.ERROR | IStatus.CANCEL ) )
        return processResultsStatus;

      // TODO: ask user, if processing produced a warning
      // if( processResultsStatus.matches( IStatus.WARNING | IStatus.INFO ) )
      // {
      // if( ErrorDialog.openError( m_shell, STRING_DLG__TITLE_PROCESS_RESULTS, "Ergebnisauswertung wurde mit Warnung
      // beendet.\nSollen die Ergebnisse übernommen werden?", processResultsStatus, IStatus.WARNING
      // | IStatus.INFO ) == ErrorDialog.CANCEL )
      // return processResultsStatus;
      // }

      // Step 5: Add geo log to calcMeta as document
      final IDocumentResultMeta logMeta = calcUnitMeta.getChildren().addNew( IDocumentResultMeta.QNAME, IDocumentResultMeta.class );
      logMeta.setName( "Simulations-Log" );
      logMeta.setDescription( "Die Log-Datei der letzten Simulation dieser Berechnungseinheit." );
      logMeta.setDocumentType( IDocumentResultMeta.DOCUMENTTYPE.log );
      logMeta.setPath( new Path( SIMULATION_LOG_GML ) );

      // Step 5: Move results into workspace and save result-DB
      m_geoLog.formatLog( IStatus.INFO, CODE_RUNNING_FINE, "Ergebnisse werden in Arbeitsbereich verschoben und Ergebnisdatenbank aktualisiert." );
      return moveResults( outputDir, progress.newChild( 5 ) );
    }
    catch( final CoreException ce )
    {
      return ce.getStatus();
    }
    catch( final Throwable t )
    {
      return StatusUtilities.createStatus( IStatus.ERROR, CODE_POST, "Unerwartetet Fehler bei der Ergebnisauswertung: " + t.toString(), t );
    }
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

    // REMARK: we save the result DB, even if deletion fails; in doubt, the new results will just overwrite the old ones

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
