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
package org.kalypso.model.hydrology.internal.simulation;

import java.io.File;
import java.util.logging.Logger;

import org.kalypso.model.hydrology.INaSimulationData;
import org.kalypso.model.hydrology.internal.NaOptimizeLoader;
import org.kalypso.optimize.OptimizeMonitor;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.SimulationException;

/**
 * @author Gernot Belger
 *
 */
public class NAMultiOptimizingJob implements INaSimulationRunnable
{
  private final File m_tmpdir;

  private final INaSimulationData m_data;

  private final OptimizeMonitor m_optimizeMonitor;

  private final Logger m_logger;

  public NAMultiOptimizingJob( final File tmpdir, final INaSimulationData data, final OptimizeMonitor optimizeMonitor, final Logger logger )
  {
    m_tmpdir = tmpdir;
    m_data = data;
    m_optimizeMonitor = optimizeMonitor;
    m_logger = logger;
  }

  /**
   * @see org.kalypso.model.hydrology.internal.simulation.INaSimulationRunnable#run(org.kalypso.simulation.core.ISimulationMonitor)
   */
  @Override
  public boolean run( final ISimulationMonitor monitor ) throws SimulationException
  {
    // FIXME: no, better implement a new wps

    final NaOptimizeLoader optimizeData = m_data.getOptimizeData();

    // TODO: prepare common resultDir -> copy result data into tmpDir
    final File commonResultDir = new File( m_tmpdir, "commonResultDir" );

    final int optimizeCount = optimizeData.getOptimizeCount();
    for( int i = 0; i < optimizeCount; i++ )
    {
      optimizeData.setCurrentOptimize( i );

      final String tmpDirName = String.format( "pegelOptimierung_%d", i );
      final File currentTmpDir = new File( m_tmpdir, tmpDirName );

      final NAOptimizingJob currentOptimizeJob = new NAOptimizingJob( currentTmpDir, m_data, monitor, m_logger );
      // TODO: fix progress
      /* boolean success = */currentOptimizeJob.run( monitor );
      final File resultDir = currentOptimizeJob.getResultDir();

    }

    return true;
  }

  /**
   * @see org.kalypso.model.hydrology.internal.simulation.INaSimulationRunnable#getOptimizeResult()
   */
  @Override
  public File getOptimizeResult( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.model.hydrology.internal.simulation.INaSimulationRunnable#getResultDir()
   */
  @Override
  public File getResultDir( )
  {
    // TODO Auto-generated method stub
    return null;
  }
}
