package org.kalypso.optimize;

import java.net.MalformedURLException;
import java.util.TreeMap;

import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.optimizer.AutoCalibration;
import org.kalypso.optimizer.Parameter;
import org.kalypso.services.calculation.service.CalcJobDataBean;

/**
 * IOpmizingJob
 * 
 * @author doemmig
 */
public interface IOpmizingJob
{
  /**
   * commits population for next calculation run
   * 
   * @param parameterConf
   *          parameter mata data
   * @param values
   *          the values
   */
  public void optimize( Parameter[] parameterConf, double[] values ) throws Exception;

  /**
   * start calculation
   */
  public void calculate();

  /**
   * @return timeseries of measured values
   */
  public TreeMap getMeasuredTimeSeries() throws MalformedURLException, SensorException;

  /**
   * @return calculated timeseries from last calculation run
   */
  public TreeMap getCalcedTimeSeries() throws MalformedURLException, SensorException;

  /**
   * inform job, if last calculation got the best results (till last run)</br>
   * e.g. to implement: keep best results and clear the rest
   * 
   * @param wasBest
   */
  public void setBestEvaluation( boolean wasBest );

  /**
   * @return result beans of best optimized calculation
   */
  public CalcJobDataBean[] getResults();

  public AutoCalibration getOptimizeConfiguration();

}