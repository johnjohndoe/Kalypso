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
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.commons.io.VFSUtilities;
import org.kalypso.commons.vfs.FileSystemManagerWrapper;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
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
  private static FileSystemManagerWrapper m_vfsManager;

  private final IResultMeta[] m_selectedResults;

  private final IContainer m_scenarioFolder;

  private final ICommandTarget m_commandTarget;

  private final IKalypsoLayerModell m_modell;

  private final IScenarioDataProvider m_modelProvider;

  private final IGeoLog m_geoLog;

  private FileObject m_fileObjSWANResult = null;

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
    monitor.beginTask( Messages.getString( "ReevaluateResultOperation.1" ), m_selectedResults.length ); //$NON-NLS-1$

    final IStatusCollector stati = new StatusCollector( Kalypso1d2dProjectPlugin.PLUGIN_ID );
    try
    {
      m_vfsManager = VFSUtilities.getNewManager();
    }
    catch( FileSystemException e )
    {
      final IStatus status = new Status( IStatus.ERROR, Kalypso1d2dProjectPlugin.PLUGIN_ID, Messages.getString( "ReevaluateResultOperation.9" ) ); //$NON-NLS-1$
      m_geoLog.log( status );
      return status;
    }

    try
    {
      FileObject actResFolder = m_vfsManager.resolveFile( m_scenarioFolder.getFolder( m_selectedResults[0].getFullPath() ).getLocationURI().toURL().toExternalForm() );
      m_fileObjSWANResult = actResFolder.resolveFile( ResultMeta1d2dHelper.resolvePathFromResultDataByMetaName( ((IStepResultMeta)m_selectedResults[0]), ResultMeta1d2dHelper.SWAN_RAW_DATA_META_NAME ).toOSString() );
    }
    catch( final Exception e )
    {
      final IStatus status = new Status( IStatus.WARNING, Kalypso1d2dProjectPlugin.PLUGIN_ID, Messages.getString( "ReevaluateResultOperation.8" ) ); //$NON-NLS-1$
      m_geoLog.log( status );
      stati.add( status );
    }

    for( final IResultMeta resultMeta : m_selectedResults )
    {
      if( resultMeta instanceof IStepResultMeta )
      {
        final IStatus status = processStepResult( (IStepResultMeta)resultMeta, new SubProgressMonitor( monitor, 1 ) );
        stati.add( status );
      }
    }

    if( m_vfsManager != null )
      m_vfsManager.close();

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
    ResultManager resultManager = null;

    // FIXME: ugly, local try/catches are a sign of bad code!
    try
    {
      actResult = m_vfsManager.resolveFile( m_scenarioFolder.getFolder( stepResult.getFullPath() ).getLocationURI().toURL().toExternalForm() );
      if( stepResult.getOwner() instanceof ICalcUnitResultMeta )
      {
        resultManager = new ResultManager( actResult, m_fileObjSWANResult, m_modelProvider, m_geoLog, (ICalcUnitResultMeta)stepResult.getOwner() );
      }
      else
      {
        resultManager = new ResultManager( actResult, m_fileObjSWANResult, m_modelProvider, m_geoLog );
      }

      resultManager.setStepsToProcess( bean.userCalculatedSteps );
    }
    catch( final CoreException | IOException e )
    {
      final IStatus status = new Status( IStatus.ERROR, Kalypso1d2dProjectPlugin.PLUGIN_ID, Messages.getString( "ReevaluateResultOperation.9" ) ); //$NON-NLS-1$
      m_geoLog.log( status );
      return status;
    }

    // FIXME: dangerous: that operation also handles what results will be deleted etc. This should be separated, because that functionality is probably only needed
    // directly after calculation. Better abstraction is needed.
    final ResultProcessingOperation processingOperation = new ResultProcessingOperation( resultManager, bean );

    final IStatus resultStatus = processingOperation.execute( monitor );
    m_geoLog.log( resultStatus );

    // FIXME: this is not the right place to do delete these file! (and why is this not necessary for the other result types?)
    // FIXME: better: the code that creates the files should be responsible to delete them
    // the files are unzipped ones for all evaluated steps and only after finishing the complete reevaluation we can delete them, 
    // so here we remove temporary unzipped swan data
    try
    {
      final FileObject unzippedSwanFile = m_vfsManager.resolveFile( processingOperation.getOutputDir(), ISimulation1D2DConstants.SIM_SWAN_TRIANGLE_FILE + "." //$NON-NLS-1$
          + ISimulation1D2DConstants.SIM_SWAN_MAT_RESULT_EXT );
      final FileObject unzippedShiftFile = m_vfsManager.resolveFile( processingOperation.getOutputDir(), ISimulation1D2DConstants.SIM_SWAN_COORD_SHIFT_FILE );
      final FileObject unzippedTabFile = m_vfsManager.resolveFile( processingOperation.getOutputDir(), ISimulation1D2DConstants.SIM_SWAN_TRIANGLE_FILE + "_out.tab" ); //$NON-NLS-1$
      unzippedSwanFile.delete();
      unzippedShiftFile.delete();
      unzippedTabFile.delete();
      unzippedSwanFile.close();
      unzippedShiftFile.close();
      unzippedTabFile.close();
    }
    catch( final FileSystemException e )
    {
      m_geoLog.log( StatusUtilities.statusFromThrowable( e ) );
    }

    if( !resultStatus.isOK() )
    {
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

    final Path unitFolderRelativePath = new Path( "results/" + calcUnitId ); //$NON-NLS-1$

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
