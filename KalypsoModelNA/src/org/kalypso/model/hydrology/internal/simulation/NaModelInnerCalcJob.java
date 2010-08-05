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
import java.net.URL;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.internal.NACalculationLogger;
import org.kalypso.model.hydrology.internal.NAModelSimulation;
import org.kalypso.model.hydrology.internal.NaSimulationDirs;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;

/**
 * @author doemming, huebsch
 */
public class NaModelInnerCalcJob implements ISimulation
{
  private static final String STRING_SIMULATION_FAILED = Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.36" ); //$NON-NLS-1$

  private boolean m_succeeded = false;

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#getSpezifikation()
   */
  @Override
  public URL getSpezifikation( )
  {
    // this is just an inner job, so need not return this
    return null;
  }

  /**
   * @throws CalcJobServiceException
   * @see org.kalypso.services.calculation.job.ICalcJob#run(java.io.File,
   *      org.kalypso.services.calculation.job.ICalcDataProvider, org.kalypso.services.calculation.job.ICalcResultEater,
   *      org.kalypso.services.calculation.job.ICalcMonitor)
   */
  @Override
  public void run( final File simulationDir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    final NaSimulationDirs simDirs = new NaSimulationDirs( simulationDir );

    final NACalculationLogger naCalculationLogger = new NACalculationLogger( simDirs.currentResultDirs.logDir );

    final Logger logger = naCalculationLogger.getLogger();

    final NAModelSimulation simulation = new NAModelSimulation( simDirs, inputProvider, logger );

    final File resultDir = simDirs.resultDir;

    try
    {
      m_succeeded = simulation.runSimulation( monitor );
    }
    catch( final SimulationException se )
    {
      throw se;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      logger.log( Level.SEVERE, STRING_SIMULATION_FAILED, e );
      throw new SimulationException( STRING_SIMULATION_FAILED, e );
    }
    finally
    {
      naCalculationLogger.stopLogging();

      simulation.backupResults();
    }
    
    resultEater.addResult( NaModelConstants.OUT_ZML, resultDir );
  }

  public boolean isSucceeded( )
  {
    return m_succeeded;
  }
}