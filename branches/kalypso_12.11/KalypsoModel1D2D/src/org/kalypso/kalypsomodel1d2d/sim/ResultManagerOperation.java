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
import java.util.Date;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.commons.io.VFSUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.sim.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.geolog.IGeoLog;

/**
 * @author Gernot Belger
 */
public class ResultManagerOperation implements ICoreRunnableWithProgress, ISimulation1D2DConstants
{
  private final ResultManager m_resultManager;

  private final File m_unitFolder;

  private final IStatus m_simulationStatus;

  private final IGeoLog m_geoLog;

  private final File m_outputDir;

  private final ICalcUnitResultMeta m_calcUnitMeta;

  private final String[] m_originalStepsToDelete;

  private boolean m_boolRemoveRawResult;

  public ResultManagerOperation( final ResultManager resultManager, final File unitFolder, final IStatus simulationStatus, final File outputDir, final ICalcUnitResultMeta calcUnitMeta, final String[] originalStepsToDelete )
  {
    m_resultManager = resultManager;
    m_unitFolder = unitFolder;
    m_simulationStatus = simulationStatus;
    m_geoLog = m_resultManager.getGeoLog();
    m_outputDir = outputDir;
    m_calcUnitMeta = calcUnitMeta;
    m_originalStepsToDelete = originalStepsToDelete;
    m_boolRemoveRawResult = true;
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    m_geoLog.formatLog( IStatus.INFO, CODE_RUNNING, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManagerOperation.0" ) ); //$NON-NLS-1$

    final IStatus resultStatus = doExecute( monitor );

    // Adapt status
    if( resultStatus.isOK() )
      return m_geoLog.formatLog( IStatus.OK, CODE_RUNNING, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManagerOperation.1" ) ); //$NON-NLS-1$

    if( resultStatus.matches( IStatus.CANCEL ) )
      return m_geoLog.formatLog( IStatus.CANCEL, CODE_RUNNING, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManagerOperation.2" ) ); //$NON-NLS-1$

    /* Warning or Error */
    m_geoLog.formatLog( IStatus.ERROR, CODE_RUNNING, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManagerOperation.3" ) ); //$NON-NLS-1$
    m_geoLog.log( resultStatus );

    return resultStatus;
  }

  private IStatus doExecute( final IProgressMonitor monitor )
  {
    try
    {
      final SubMonitor progress = SubMonitor.convert( monitor, 100 );

      // Step 1: Delete existing results and save result-DB (in case of problems while processing)
      m_geoLog.formatLog( IStatus.INFO, CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManagerOperation.4" ) ); //$NON-NLS-1$
      final IStatus deleteStatus = deleteExistingResults( progress.newChild( 5 ) );
      if( deleteStatus.matches( IStatus.CANCEL ) )
        return deleteStatus;
      if( !deleteStatus.isOK() )
        m_geoLog.log( deleteStatus );

      // Step 3: Fill in result of calculation
      m_calcUnitMeta.setStatus( m_simulationStatus );
      m_calcUnitMeta.setCalcEndTime( new Date() );

      // Step 4: Move results into workspace and save result-DB
      m_geoLog.formatLog( IStatus.INFO, CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManagerOperation.5" ) ); //$NON-NLS-1$

      return moveResults( m_outputDir, progress.newChild( 5 ) );
    }
    catch( final Throwable t )
    {
      return new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManagerOperation.6" ) + t.toString(), t ); //$NON-NLS-1$
    }
  }

  /**
   * Delete all existing results inside the current result database.
   */
  private IStatus deleteExistingResults( final IProgressMonitor monitor )
  // private IStatus deleteExistingResults( final IScenarioResultMeta scenarioMeta, final ICalculationUnit calcUnit,
  // final IProgressMonitor monitor ) throws CoreException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, 100 );
    progress.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManagerOperation.7" ) ); //$NON-NLS-1$

    // final ICalcUnitResultMeta calcUnitMeta = scenarioMeta.findCalcUnitMetaResult( calcUnit.getId() );

    /* If no results available yet, nothing to do. */
    if( m_calcUnitMeta == null )
      return Status.OK_STATUS;

    // final String[] stepsToDelete = findStepsToDelete( calcUnitMeta, processBean );
    ProgressUtilities.worked( progress, 5 );

    final IStatus result = ResultMeta1d2dHelper.deleteAllByID( m_calcUnitMeta, m_originalStepsToDelete, progress.newChild( 90 ), m_boolRemoveRawResult );

    // REMARK: we save the result DB, even if deletion fails; in doubt, the new results will just overwrite the old ones

    /* Save result DB */

    // REMARL: Saving in between causes sometimes, that results are not moved to the ScenarioResultMeta.gml.
    // try
    // {
    //      ((ICommandPoster) m_caseDataProvider).postCommand( IScenarioResultMeta.class, new EmptyCommand( "", false ) ); //$NON-NLS-1$
    // }
    // catch( final InvocationTargetException e )
    // {
    //      throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManagerOperation.9" ), e.getTargetException() ) ); //$NON-NLS-1$
    // }
    //
    // m_caseDataProvider.saveModel( IScenarioResultMeta.class, progress.newChild( 5 ) );
    return result;
  }

  private IStatus moveResults( final File outputDir, final IProgressMonitor monitor )
  {
    final SubMonitor progress = SubMonitor.convert( monitor, 100 );
    progress.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManagerOperation.10" ) ); //$NON-NLS-1$

    try
    {
      FileUtils.forceMkdir( m_unitFolder );

      VFSUtilities.moveContents( outputDir, m_unitFolder );
      ProgressUtilities.worked( progress, 70 );

      /* Output dir should now be empty, so there is no sense in keeping it */
      outputDir.delete();

      return Status.OK_STATUS;
    }
    catch( final IOException e )
    {
      return new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManagerOperation.12" ), e ); //$NON-NLS-1$
    }
  }

  public final void setBoolRemoveRawResult( final boolean boolRemoveRawResult )
  {
    m_boolRemoveRawResult = boolRemoveRawResult;
  }

}
