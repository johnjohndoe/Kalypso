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
package org.kalypso.ui.wizards.results;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import org.apache.commons.vfs2.FileObject;
import org.apache.commons.vfs2.FileSystemException;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.afgui.model.ICommandPoster;
import org.kalypso.commons.KalypsoCommonsPlugin;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.commons.io.VFSUtilities;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;
import org.kalypso.kalypsomodel1d2d.sim.ISimulation1D2DConstants;
import org.kalypso.kalypsomodel1d2d.sim.ProcessResultsBean;
import org.kalypso.kalypsomodel1d2d.sim.ResultManager;
import org.kalypso.kalypsomodel1d2d.sim.ResultManagerOperation;
import org.kalypso.kalypsomodel1d2d.sim.ResultProcessingOperation;
import org.kalypso.kalypsomodel1d2d.ui.geolog.IGeoLog;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.ogc.gml.IKalypsoLayerModell;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Ilya Gershovich
 */
public class ReevaluateResultOperation implements ICoreRunnableWithProgress
{
  private final IResultMeta[] m_selectedResults;

  private final IContainer m_scenarioFolder;

  private final ICommandTarget m_commandTarget;

  private final IKalypsoLayerModell m_modell;

  private final IScenarioDataProvider m_modelProvider;

  private final IGeoLog m_geoLog;

  public ReevaluateResultOperation( final IResultMeta[] selectedResults, final IContainer scenarioFolder, final ICommandTarget commandTarget, final IKalypsoLayerModell modell, final IScenarioDataProvider modelProvider, final IGeoLog geoLog )
  {
    m_selectedResults = selectedResults;
    m_scenarioFolder = scenarioFolder;
    m_commandTarget = commandTarget;
    m_modell = modell;
    m_modelProvider = modelProvider;
    m_geoLog = geoLog;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    monitor.beginTask( Messages.getString("ReevaluateResultOperation.1"), m_selectedResults.length ); //$NON-NLS-1$

    final IStatusCollector stati = new StatusCollector( Kalypso1d2dProjectPlugin.PLUGIN_ID );

    for( final IResultMeta resultMeta : m_selectedResults )
    {
      if( resultMeta instanceof IStepResultMeta )
      {
        final IStatus status = processStepResult( (IStepResultMeta)resultMeta, new SubProgressMonitor( monitor, 1 ) );
        stati.add( status );
      }
    }

    return stati.asMultiStatusOrOK( Messages.getString( "ReevaluateResultOperation.0" ) ); //$NON-NLS-1$
  }

  private IStatus processStepResult( final IStepResultMeta stepResult, final IProgressMonitor monitor )
  {
    /* delete map theme, if any */
    if( m_modell != null && m_commandTarget != null )
      ResultMeta1d2dHelper.deleteResultThemeFromMap( stepResult, m_modell, m_commandTarget );

    final ProcessResultsBean bean = new ProcessResultsBean();
    bean.deleteAll = false;
    bean.deleteFollowers = false;
    bean.evaluateFullResults = true;

    if( stepResult.getFullPath().toOSString().contains( ResultManager.STEADY_PREFIX ) )
      bean.userCalculatedSteps = new Date[] { ResultManager.STEADY_DATE };
    else if( stepResult.getFullPath().toOSString().contains( ResultManager.MAXI_PREFIX ) )
      bean.userCalculatedSteps = new Date[] { ResultManager.MAXI_DATE };
    else
      bean.userCalculatedSteps = new Date[] { stepResult.getStepTime() };

    FileObject actResult = null;
    FileObject fileObjSWANResult = null;

    final ILog lLog = KalypsoCommonsPlugin.getDefault().getLog();


    // FIXME: ugly, local try/catches are a sign of bad code!

    try
    {
      // FIXME: file manager never closed, resource leak!, same for other elements below!
      // FIXME: can it ever happen, that the results are not on the local file system
      actResult = VFSUtilities.getNewManager().resolveFile( m_scenarioFolder.getFolder( stepResult.getFullPath() ).getLocationURI().toURL().toExternalForm() );
      fileObjSWANResult = actResult.resolveFile( ResultMeta1d2dHelper.getSavedSWANRawResultData( stepResult ).toOSString() );
    }
    catch( final Exception e )
    {
      // FIXME: bad! this is no error handling! And also no way to handle missing results -> check for existence of file instead
      final IStatus status = new Status( IStatus.WARNING, Kalypso1d2dProjectPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.ui.wizards.results.ResultManager1d2dWizardPage.8" ) ); //$NON-NLS-1$
      lLog.log( status );
    }

    // FIXME: ugly, local try/catches are a sign of bad code!

    ResultManager resultManager = null;
    try
    {
      resultManager = new ResultManager( actResult, fileObjSWANResult, m_modelProvider, m_geoLog );
    }
    catch( final CoreException e )
    {
      // FIXME: this is no error handling!
      lLog.log( StatusUtilities.statusFromThrowable( e ) );
    }

    // FIXME ugly: if the result manager above was not initialize, we will run into another NPE here, this is really not necesary!

    try
    {
      resultManager.setStepsToProcess( bean.userCalculatedSteps, resultManager.getControlModel() );
    }
    catch( final IOException e1 )
    {
      return StatusUtilities.statusFromThrowable( e1 );
    }

    // FIXME: dangerous: that operation also handles what results will be deleted etc. This should be separated, because that functionality is probably only needed
    // directly after calculation. Better abstraction is needed.
    final ResultProcessingOperation processingOperation = new ResultProcessingOperation( resultManager, bean );

    final IStatus resultStatus = processingOperation.execute( monitor );
    // if anything happened during the processing, restore the original results db from disk
    if( !resultStatus.isOK() )
    {
      lLog.log( resultStatus );
      try
      {
        // FIXME: cannot work -> model is reloaded for each error, this makes no sense! -> in this case, we need to stop the evaluation at all and reset everything!

        // FIXME: also we will loose previously correctly processed steps

        // FIXME: also: why is it needed at all: the code below makes sure, that bad results are not added to the result database, so where is the problem?

        // set the dirty flag of the results model
        ((ICommandPoster)m_modelProvider).postCommand( IScenarioResultMeta.class.getName(), new EmptyCommand( "", false ) ); //$NON-NLS-1$
      }
      catch( final Exception e )
      {
        lLog.log( StatusUtilities.statusFromThrowable( e ) );
      }

      m_modelProvider.reloadModel();

      return resultStatus;
    }

    // if OK move the new results data to the results folder
    // this operation is not cancelable

    // processing finished without problems, prepare the data-operation
    // this is where the name of the result folder is actually set
    final ICalcUnitResultMeta calcUnitMeta = processingOperation.getCalcUnitMeta();
    final String calcUnitId = calcUnitMeta.getCalcUnit();
    List<String> lListResultsToRemove = new ArrayList<>();
    lListResultsToRemove.addAll( Arrays.asList( processingOperation.getOriginalStepsToDelete() ) );
    if( lListResultsToRemove.size() == 0 )
    {
      lListResultsToRemove.add( stepResult.getId() );
    }
    lListResultsToRemove = removeAllOthersStepWithDate( lListResultsToRemove, stepResult.getId() );

    final String[] lResultsToRemove = lListResultsToRemove.toArray( new String[lListResultsToRemove.size()] );

    // FIXME: this is not the right place to do delete these file! (and why is this not necessary for the other result types?)
    // FIXME: better: the code that creates the files should be responsible to delete them
    final Path unitFolderRelativePath = new Path( "results/" + calcUnitId ); //$NON-NLS-1$
    // remove temporary unzipped swan data
    try
    {
      final FileObject unzippedSwanFile = VFSUtilities.getNewManager().resolveFile( processingOperation.getOutputDir(), ISimulation1D2DConstants.SIM_SWAN_TRIANGLE_FILE + "." //$NON-NLS-1$
          + ISimulation1D2DConstants.SIM_SWAN_MAT_RESULT_EXT );
      final FileObject unzippedShiftFile = VFSUtilities.getNewManager().resolveFile( processingOperation.getOutputDir(), ISimulation1D2DConstants.SIM_SWAN_COORD_SHIFT_FILE );
      final FileObject unzippedTabFile = VFSUtilities.getNewManager().resolveFile( processingOperation.getOutputDir(), ISimulation1D2DConstants.SIM_SWAN_TRIANGLE_FILE + "_out.tab" ); //$NON-NLS-1$
      unzippedSwanFile.delete();
      unzippedShiftFile.delete();
      unzippedTabFile.delete();
    }
    catch( final FileSystemException e )
    {
      lLog.log( StatusUtilities.statusFromThrowable( e ) );
    }
    final IFolder unitFolder = m_scenarioFolder.getFolder( unitFolderRelativePath );
    final ResultManagerOperation dataOperation = new ResultManagerOperation( resultManager, unitFolder.getLocation().toFile(), Status.OK_STATUS, processingOperation.getOutputDir(), calcUnitMeta, lResultsToRemove );
    dataOperation.setBoolRemoveRawResult( false );
    return dataOperation.execute( monitor );
  }

  private List<String> removeAllOthersStepWithDate( final List<String> lListResultsToRemove, final String stepId )
  {
    final List<String> lListRes = new ArrayList<>();
    for( final String lId : lListResultsToRemove )
    {
      if( lId.equals( stepId ) )
      {
        lListRes.add( lId );
        break;
      }
    }
    return lListRes;
  }
}
