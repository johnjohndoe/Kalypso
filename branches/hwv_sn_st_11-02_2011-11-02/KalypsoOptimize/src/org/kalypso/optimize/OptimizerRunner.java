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
package org.kalypso.optimize;

import java.io.File;
import java.util.logging.Logger;

import org.kalypso.optimizer.AutoCalibration;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.SimulationException;

/**
 * calcjob that optimizes parameters of an encapsulated caljob
 * 
 * @author doemming
 */
public class OptimizerRunner
{
  private final IOptimizingJob m_optimizingJob;

  private final Logger m_logger;

  private final File m_tmpdir;

  /**
   * @param logger
   * @param job
   *          encapsulated job to optimize
   */
  public OptimizerRunner( final File tmpdir, final Logger logger, final IOptimizingJob job )
  {
    m_tmpdir = tmpdir;
    m_logger = logger;
    m_optimizingJob = job;
  }

  public boolean run( final ISimulationMonitor monitor ) throws SimulationException
  {
    final AutoCalibration autoCalibration = m_optimizingJob.getOptimizeConfiguration();
    final SceJob sceJob = new SceJob( autoCalibration, m_tmpdir );

    final SceIOHandler sceIO = new SceIOHandler( m_logger, autoCalibration, m_optimizingJob );

    if( monitor.isCanceled() )
      return false;
    sceJob.optimize( sceIO, monitor );

    return m_optimizingJob.isSucceeded();
  }
}