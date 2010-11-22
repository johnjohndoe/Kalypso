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
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Calendar;
import java.util.Date;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.kalypso.model.hydrology.INaSimulationData;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.NAControl;
import org.kalypso.model.hydrology.binding.NAOptimize;
import org.kalypso.model.hydrology.internal.NACalculationLogger;
import org.kalypso.model.hydrology.internal.NAModelSimulation;
import org.kalypso.model.hydrology.internal.NaOptimizeLoader;
import org.kalypso.model.hydrology.internal.NaSimulationDirs;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.optimize.IOptimizingJob;
import org.kalypso.optimize.OptimizerRunner;
import org.kalypso.optimize.transform.OptimizeModelUtils;
import org.kalypso.optimize.transform.ParameterOptimizeContext;
import org.kalypso.optimizer.AutoCalibration;
import org.kalypso.optimizer.Parameter;
import org.kalypso.optimizer.Pegel;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.IFeatureProviderFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

/**
 * encapsulates an NAModellCalculation job to optimize it
 * 
 * @author doemming
 */
public class NAOptimizingJob implements IOptimizingJob, INaSimulationRunnable
{
  private final File m_tmpDir;

  private SortedMap<Date, Double> m_measuredTS;

  private final File m_bestOptimizedFile;

  private final ISimulationMonitor m_monitor;

  private final File m_bestOptimizeRunDir;

  private int m_counter = 0;

  private int m_bestNumber = 0;

  private boolean m_lastSucceeded = false;

  private boolean m_bestSucceeded = false;

  private final File m_optimizeRunDir;

  private final File m_bestResultDir;

  private NAModelSimulation m_simulation;

  private final NaSimulationDirs m_simDirs;

  private final INaSimulationData m_data;

  private final Logger m_logger;

  public NAOptimizingJob( final File tmpDir, final INaSimulationData data, final ISimulationMonitor monitor, final Logger logger ) throws Exception
  {
    m_tmpDir = tmpDir;
    m_data = data;
    m_monitor = monitor;
    m_logger = logger;

    m_optimizeRunDir = new File( m_tmpDir, "optimizeRun" );
    m_bestOptimizeRunDir = new File( m_tmpDir, "bestRun" );
    m_bestResultDir = new File( m_tmpDir, "bestResult" );
    m_bestOptimizedFile = new File( m_tmpDir, "bestOptimizeConfig.gml" );
    m_simDirs = new NaSimulationDirs( m_optimizeRunDir );

    monitor.setMessage( "Loading simulation data..." );

    final NAControl metaControl = data.getMetaControl();
    final Date optimizationStartDate = metaControl.getOptimizationStart();
    final Date measuredEndDate = metaControl.getStartForecast();

    final NaOptimizeLoader optimizeData = data.getOptimizeData();

    final AutoCalibration autoCalibration = optimizeData.getAutoCalibration();
    // correct in intervall autocalibration
    final Pegel pegel = autoCalibration.getPegel();

    final Calendar calendarStart = Calendar.getInstance();
    calendarStart.setTime( optimizationStartDate );
    pegel.setStartDate( calendarStart );

    final Calendar calendarEnd = Calendar.getInstance();
    calendarEnd.setTime( measuredEndDate );
    pegel.setEndDate( calendarEnd );
  }

  @Override
  public void dispose( )
  {
    m_data.dispose();
  }

  /**
   * Run myself in the {@link OptimizerRunner}.
   * 
   * @see org.kalypso.simulation.core.ISimulationRunnable#run(org.kalypso.simulation.core.ISimulationMonitor)
   */
  @Override
  public boolean run( final ISimulationMonitor monitor )
  {
    final OptimizerRunner runner = new OptimizerRunner( m_tmpDir, m_logger, this );
    return runner.run( monitor );
  }

  /**
   * @throws MalformedURLException
   * @see org.kalypso.optimize.IOptimizingJob#calculate()
   */
  @Override
  public void calculate( ) throws Exception
  {
    if( m_counter == 0 )
      m_lastSucceeded = runFirst();
    else
      m_lastSucceeded = runAgain();

    m_counter++;
  }

  private boolean runFirst( )
  {
    final NACalculationLogger naCalculationLogger = new NACalculationLogger( new File( m_tmpDir, "logRun_" + m_counter ) );

    final Logger logger = naCalculationLogger.getLogger();

    try
    {
      m_simulation = new NAModelSimulation( m_simDirs, m_data, logger );
      return m_simulation.runSimulation( m_monitor );
    }
    catch( final OperationCanceledException e )
    {
      final String msg = "Simulation cancelled by user";
      logger.log( Level.INFO, msg );
      m_monitor.setFinishInfo( IStatus.CANCEL, msg );
      return false;
    }
    catch( final Exception e )
    {
      // FIXME: error handling
      e.printStackTrace();
// logger.log( Level.SEVERE, STRING_SIMULATION_FAILED, e );
// throw new SimulationException( STRING_SIMULATION_FAILED, e );
      return false;
    }
    finally
    {
      naCalculationLogger.stopLogging();
    }
  }

  private boolean runAgain( ) throws Exception
  {
    final INaSimulationData simulationData = m_simulation.getSimulationData();
    final GMLWorkspace contextWorkspace = simulationData.getModelWorkspace();
    final URL context = contextWorkspace.getContext();
    final IFeatureProviderFactory factory = contextWorkspace.getFeatureProviderFactory();

    final Node naOptimizeDom = m_data.getOptimizeData().getOptimizeDom();
    final NAOptimize optimize = NaOptimizeLoader.toOptimizeConfig( naOptimizeDom, context, factory );
    return m_simulation.rerunForOptimization( optimize, m_monitor );
  }

  /**
   * @see org.kalypso.optimize.IOptimizingJob#setBestEvaluation(boolean)
   */
  @Override
  public void setBestEvaluation( final boolean lastWasBest )
  {
    if( lastWasBest )
    {
      saveOptimizeConfig( m_bestOptimizedFile );

      clear( m_bestOptimizeRunDir );
      clear( m_bestResultDir );

      try
      {
        final File resultDir = m_simDirs.resultDir;

        FileUtils.copyDirectory( m_optimizeRunDir, m_bestOptimizeRunDir );
        FileUtils.copyDirectory( resultDir, m_bestResultDir );
      }
      catch( final IOException e )
      {
        // FIXME: error handling
        e.printStackTrace();
      }

      m_bestNumber = m_counter;
      m_bestSucceeded = m_lastSucceeded;
    }

// // FIXME DEBUG: save last run
// try
// {
// FileUtils.copyDirectory( m_optimizeRunDir, new File( m_tmpDir, "optimizeRun_" + m_counter ) );
// saveOptimizeConfig( new File( m_tmpDir, "optimizeBean_" + m_counter + ".gml" ) );
// }
// catch( final IOException e )
// {
// // FIXME: error handling
// e.printStackTrace();
// }

    /* clear last results */
    clear( m_simDirs.resultDir );
  }

  private void clear( final File dir )
  {
    if( dir == null )
      return;

    FileUtils.deleteQuietly( dir );
  }

  /**
   * @see org.kalypso.optimize.IOptimizingJob#optimize(org.kalypso.optimizer.Parameter[], double[])
   */
  @Override
  public void optimize( final Parameter[] parameterConf, final double values[] ) throws Exception
  {
    final ParameterOptimizeContext[] calcContexts = new ParameterOptimizeContext[parameterConf.length];
    for( int i = 0; i < parameterConf.length; i++ )
      calcContexts[i] = new ParameterOptimizeContext( parameterConf[i] );

    try
    {
      final Node naOptimizeDom = m_data.getOptimizeData().getOptimizeDom();
      OptimizeModelUtils.transformModel( naOptimizeDom, values, calcContexts );
    }
    catch( final TransformerException e )
    {
      e.printStackTrace();
    }
  }

  private void saveOptimizeConfig( final File file )
  {
    try
    {
      final TransformerFactory factory = TransformerFactory.newInstance();
      final Transformer t = factory.newTransformer();

      t.setOutputProperty( "{http://xml.apache.org/xslt}indent-amount", "2" ); //$NON-NLS-1$ //$NON-NLS-2$
      t.setOutputProperty( OutputKeys.INDENT, "yes" ); //$NON-NLS-1$

      final Node naOptimizeDom = m_data.getOptimizeData().getOptimizeDom();
      final Document ownerDocument = naOptimizeDom instanceof Document ? (Document) naOptimizeDom : naOptimizeDom.getOwnerDocument();
      final String encoding = ownerDocument.getInputEncoding();
      t.setOutputProperty( OutputKeys.ENCODING, encoding );
      t.transform( new DOMSource( ownerDocument ), new StreamResult( file ) );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * @throws SensorException
   * @see org.kalypso.optimize.IOptimizingJob#getMeasuredTimeSeries()
   */
  @Override
  public SortedMap<Date, Double> getMeasuredTimeSeries( ) throws SensorException
  {
    if( m_measuredTS == null )
    {
      final SortedMap<Date, Double> result = new TreeMap<Date, Double>();
      final NAOptimize naOptimize = m_data.getNaOptimize();
      final ZmlLink linkMeasuredTS = naOptimize.getPegelZRLink();

      final IObservation observation = linkMeasuredTS.loadObservation();

      final IAxis dateAxis = ObservationUtilities.findAxisByType( observation.getAxisList(), ITimeseriesConstants.TYPE_DATE );
      final IAxis qAxis = ObservationUtilities.findAxisByType( observation.getAxisList(), ITimeseriesConstants.TYPE_RUNOFF );
      final ITupleModel values = observation.getValues( null );
      for( int i = 0; i < values.size(); i++ )
      {
        final Date date = (Date) values.get( i, dateAxis );
        final Double value = (Double) values.get( i, qAxis );
        result.put( date, value );
      }
      m_measuredTS = result;
    }
    return m_measuredTS;
  }

  /**
   * @throws SensorException
   * @throws MalformedURLException
   * @see org.kalypso.optimize.IOptimizingJob#getCalcedTimeSeries()
   */
  @Override
  public SortedMap<Date, Double> getCalcedTimeSeries( ) throws MalformedURLException, SensorException
  {
    final SortedMap<Date, Double> result = new TreeMap<Date, Double>();
    final File optimizeResultDir = new File( m_optimizeRunDir, NaModelConstants.OUTPUT_DIR_NAME );

    final NAOptimize naOptimize = m_data.getNaOptimize();
    final ZmlLink linkCalcedTS = naOptimize.getResultLink();

    final String calcHref = linkCalcedTS.getHref().replaceFirst( "^" + NaModelConstants.OUTPUT_DIR_NAME + ".", "" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    final File tsFile = new File( optimizeResultDir, calcHref );
    final IObservation observation = ZmlFactory.parseXML( tsFile.toURI().toURL() ); //$NON-NLS-1$
    final IAxis dateAxis = ObservationUtilities.findAxisByType( observation.getAxisList(), ITimeseriesConstants.TYPE_DATE );
    final IAxis qAxis = ObservationUtilities.findAxisByType( observation.getAxisList(), ITimeseriesConstants.TYPE_RUNOFF );
    final ITupleModel values = observation.getValues( null );
    for( int i = 0; i < values.size(); i++ )
    {
      final Date date = (Date) values.get( i, dateAxis );
      final Double value = (Double) values.get( i, qAxis );
      result.put( date, value );
    }
    return result;
  }

  /**
   * @throws CalcJobServiceException
   * @see org.kalypso.optimize.IOptimizingJob#publishResults(org.kalypso.services.calculation.job.ICalcResultEater)
   */
  @Override
  public void publishResults( final ISimulationResultEater resultEater ) throws SimulationException
  {
    resultEater.addResult( NaModelConstants.OUT_ZML, m_bestResultDir );

    System.out.println( "best was #" + m_bestNumber ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.optimize.IOptimizingJob#getOptimizeConfiguration()
   */
  @Override
  public AutoCalibration getOptimizeConfiguration( )
  {
    return m_data.getOptimizeData().getAutoCalibration();
  }

  @Override
  public boolean isSucceeded( )
  {
    return m_bestSucceeded;
  }

  /**
   * @see org.kalypso.optimize.IOptimizingJob#getResultDir()
   */
  @Override
  public File getResultDir( )
  {
    return m_bestResultDir;
  }

  public File getBestOptimizeFile( )
  {
    return m_bestOptimizedFile;
  }

  /**
   * @see org.kalypso.model.hydrology.internal.simulation.INaSimulationRunnable#getOptimizeResult()
   */
  @Override
  public File getOptimizeResult( )
  {
    return m_bestOptimizedFile;
  }

}