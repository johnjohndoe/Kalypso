package org.kalypso.optimize;

import java.io.File;
import java.util.logging.Logger;

import org.kalypso.optimizer.AutoCalibration;
import org.kalypso.services.calculation.common.ICalcServiceConstants;
import org.kalypso.services.calculation.job.impl.AbstractCalcJob;
import org.kalypso.services.calculation.service.CalcJobDataBean;

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
   *      org.kalypso.services.calculation.service.CalcJobDataBean[])
   */
  public void run( final File baseDir, final CalcJobDataBean[] input )
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
      CalcJobDataBean[] results = m_optimizingJob.getResults();
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