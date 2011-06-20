/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.sim.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.geolog.IGeoLog;

/**
 * @author barbarins
 * 
 *         This part of result processing should be cancelable We only process the results, but do not move it into the
 *         database nor delete the old results
 */
public class ResultProcessingOperation implements ICoreRunnableWithProgress, ISimulation1D2DConstants
{
  private final ResultManager m_resultManager;

  private final IGeoLog m_geoLog;

  private File m_outputDir;

  private ICalcUnitResultMeta m_calcUnitMeta;

  private String[] m_originalStepsToDelete;

  private ProcessResultsBean m_bean;

  public String[] getOriginalStepsToDelete( )
  {
    return m_originalStepsToDelete;
  }

  public File getOutputDir( )
  {
    return m_outputDir;
  }

  public ResultProcessingOperation( final ResultManager resultManager, ProcessResultsBean bean )
  {
    m_resultManager = resultManager;
    m_geoLog = m_resultManager.getGeoLog();
    m_bean = bean;
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
      final IControlModel1D2D controlModel = m_resultManager.getControlModel();
      final IScenarioResultMeta scenarioMeta = m_resultManager.getScenarioMeta();
      final ICalculationUnit calculationUnit = controlModel.getCalculationUnit();

      final SubMonitor progress = SubMonitor.convert( monitor, 100 );

      m_calcUnitMeta = m_resultManager.getCalcUnitMeta();
      if( m_calcUnitMeta == null )
        m_calcUnitMeta = scenarioMeta.findCalcUnitMetaResult( calculationUnit.getId() );

      m_originalStepsToDelete = findStepsToDelete( m_calcUnitMeta, m_bean );

      final IStatus processResultsStatus = m_resultManager.processResults( m_calcUnitMeta, m_bean.evaluateFullResults, progress.newChild( 90 ) );

      m_outputDir = m_resultManager.getOutputDir();
      m_geoLog.log( processResultsStatus );

      if( processResultsStatus.matches( IStatus.ERROR | IStatus.CANCEL ) )
        return processResultsStatus;

      return Status.OK_STATUS;
    }
    catch( final Throwable t )
    {
      return StatusUtilities.createStatus( IStatus.ERROR, CODE_POST, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.ResultManagerOperation.6" ) + t.toString(), t ); //$NON-NLS-1$
    }
  }

  public ICalcUnitResultMeta getCalcUnitMeta( )
  {
    return m_calcUnitMeta;
  }

  private static String[] findStepsToDelete( final ICalcUnitResultMeta calcUnitMeta, final ProcessResultsBean processBean )
  {
    final Map<String, Date> existingSteps = ResultMeta1d2dHelper.getAllIDs( calcUnitMeta );

    if( processBean.deleteAll )
      return existingSteps.keySet().toArray( new String[existingSteps.size()] );

    final SortedSet<Date> allCalculatedDates = new TreeSet<Date>();

    /* Always delete all calculated steps */
//    allCalculatedDates.addAll( Arrays.asList( processBean.userCalculatedSteps ) );
    for( final Date dateTest: processBean.userCalculatedSteps ){
      if( dateTest != null ){
        allCalculatedDates.add( dateTest );
      }
    }

    List<String> ids = new ArrayList<String>();

    if( processBean.deleteFollowers && !allCalculatedDates.isEmpty() )
    {
      allCalculatedDates.remove( MAXI_DATE );
      allCalculatedDates.remove( STEADY_DATE );

      if( allCalculatedDates.size() > 0 )
      {
        final Date firstCalculated = allCalculatedDates.first();
        for( final String id : existingSteps.keySet() )
        {
          Date date = existingSteps.get( id );
          if( date == null )
          {
            ids.add( id );
          }
          else if( date.after( firstCalculated ) || date.equals( firstCalculated ) )
          {
            ids.add( id );
          }
        }
      } else {
        ids.addAll( existingSteps.keySet() );
      }

      // // then add them again.
      // if( maxi == true )
      // allDates.add( MAXI_DATE );
      // if( steady == true )
      // allDates.add( STEADY_DATE );
    }
    else if( !allCalculatedDates.isEmpty() ){
//      final Date firstCalculated = allCalculatedDates.first();
      for( final String id : existingSteps.keySet() )
      {
        Date date = existingSteps.get( id );
        if( date == null || date.getTime() == 0)
        {
          ids.add( id );
        }
//        else if( date.equals( firstCalculated ) )
//        {
//          ids.add( id );
//        }
        else if( allCalculatedDates.contains( date ) )
        {
          ids.add( id );
        }
      }
    }

    return ids.toArray( new String[ids.size()] );
  }

}
