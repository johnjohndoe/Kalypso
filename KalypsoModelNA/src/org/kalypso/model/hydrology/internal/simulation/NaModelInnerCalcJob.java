/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.model.hydrology.internal.simulation;

import java.io.File;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.kalypso.model.hydrology.INaSimulationData;
import org.kalypso.model.hydrology.internal.NACalculationLogger;
import org.kalypso.model.hydrology.internal.NAModelSimulation;
import org.kalypso.model.hydrology.internal.NaSimulationDirs;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.utils.log.GeoStatusLog;

/**
 * @author doemming, huebsch
 */
public class NaModelInnerCalcJob implements INaSimulationRunnable
{
  private static final String STRING_SIMULATION_FAILED = Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.36" ); //$NON-NLS-1$

  private final boolean m_succeeded = false;

  private final INaSimulationData m_data;

  private final NaSimulationDirs m_simDirs;

  public NaModelInnerCalcJob( final INaSimulationData data, final File simulationDir )
  {
    m_data = data;
    m_simDirs = new NaSimulationDirs( simulationDir );
  }

  @Override
  public boolean run( final ISimulationMonitor monitor ) throws SimulationException
  {
    final NACalculationLogger naCalculationLogger = new NACalculationLogger( m_simDirs.currentResultDirs.logDir );
    final Logger logger = naCalculationLogger.getLogger();

    NAModelSimulation simulation = null;
    IStatus status = null;

    try
    {
      simulation = new NAModelSimulation( m_simDirs, m_data, logger );
      status = simulation.runSimulation( monitor );

      if( status.getSeverity() == IStatus.CANCEL )
        throw new OperationCanceledException();

      return true;
    }
    catch( final OperationCanceledException e )
    {
      // FIXME: in future return status, the client should be responsible to save it to a file

      final String msg = Messages.getString( "NaModelInnerCalcJob.0" ); //$NON-NLS-1$
      logger.log( Level.INFO, msg );
      monitor.setFinishInfo( IStatus.CANCEL, msg );
      return false;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      logger.log( Level.SEVERE, STRING_SIMULATION_FAILED, e );
      throw new SimulationException( STRING_SIMULATION_FAILED, e );
    }
    finally
    {
      saveSimulationLog( status );
      naCalculationLogger.stopLogging();
    }
  }

  @Override
  public File getOptimizeResult( )
  {
    return null;
  }

  @Override
  public File getResultDir( )
  {
    return m_simDirs.resultDir;
  }

  private void saveSimulationLog( final IStatus status )
  {
    try
    {
      if( status == null )
        return;

      final File resultDir = m_simDirs.currentResultDir;
      final File logDir = new File( resultDir, "Log" ); //$NON-NLS-1$
      final File simulationLogFile = new File( logDir, "simulationLog.gml" ); //$NON-NLS-1$

      final GeoStatusLog geoStatusLog = new GeoStatusLog( simulationLogFile );
      geoStatusLog.log( status );
      geoStatusLog.serialize();
    }
    catch( final Exception ex )
    {
      /* Ignore. */
      ex.printStackTrace();
    }
  }

  public boolean isSucceeded( )
  {
    return m_succeeded;
  }
}