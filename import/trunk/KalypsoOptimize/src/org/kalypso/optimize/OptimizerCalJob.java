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
import org.kalypso.services.calculation.common.ICalcServiceConstants;
import org.kalypso.services.calculation.job.impl.AbstractCalcJob;
import org.kalypso.services.calculation.service.CalcJobClientBean;

/**
 * calcjob that optimizes parameters of an encapsulated caljob
 * 
 * @author doemming
 */
public class OptimizerCalJob extends AbstractCalcJob
{
  private final IOptimizingJob m_optimizingJob;

  private final Logger m_logger;

  private SceJob m_sceJob = null;

  /**
   * @param logger
   * @param job
   *          encapsulated job to optimize
   */
  public OptimizerCalJob( Logger logger, IOptimizingJob job )
  {
    m_logger = logger;
    m_optimizingJob = job;
  }

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#run(java.io.File,
   *      org.kalypso.services.calculation.service.CalcJobClientBean[])
   */
  public void run( final File baseDir, final CalcJobClientBean[] input )
  {
    final File calcDir = new File( baseDir, ICalcServiceConstants.CALC_DIR_NAME );
    try
    {
      final AutoCalibration autoCalibration = m_optimizingJob.getOptimizeConfiguration();
      m_sceJob = new SceJob( autoCalibration, calcDir );
      final SceIOHandler sceIO = new SceIOHandler( m_logger, autoCalibration, m_optimizingJob );
      m_sceJob.optimize( sceIO );
      if( isCanceled() )
        return;
      CalcJobClientBean[] results = m_optimizingJob.getResults();
      for( int i = 0; i < results.length; i++ )
        addResult( results[i] );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * @see org.kalypso.services.calculation.job.impl.AbstractCalcJob#cancel()
   */
  public void cancel()
  {
    m_sceJob.cancel();
    super.cancel();
  }
}