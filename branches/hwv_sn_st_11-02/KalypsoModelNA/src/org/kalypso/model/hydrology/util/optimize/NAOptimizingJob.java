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
package org.kalypso.model.hydrology.util.optimize;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
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
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.hydrology.INaSimulationData;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.NAControl;
import org.kalypso.model.hydrology.binding.NAOptimize;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.internal.NACalculationLogger;
import org.kalypso.model.hydrology.internal.NAModelSimulation;
import org.kalypso.model.hydrology.internal.NaOptimizeData;
import org.kalypso.model.hydrology.internal.NaResultDirs;
import org.kalypso.model.hydrology.internal.NaSimulationDirs;
import org.kalypso.model.hydrology.internal.simulation.INaSimulationRunnable;
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
import org.kalypso.optimize.transform.ParameterOptimizeContext;
import org.kalypso.optimizer.AutoCalibration;
import org.kalypso.optimizer.Parameter;
import org.kalypso.optimizer.Pegel;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.GMLWorkspace;
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

  private final IStatusCollector m_resultLog = new StatusCollector( ModelNA.PLUGIN_ID );

  private final File m_bestOptimizedFile;

  private final File m_bestOptimizeRunDir;

  private int m_counter = 0;

  private IStatus m_lastResult = null;

  private IStatus m_bestResult = null;

  private final File m_optimizeRunDir;

  private final File m_bestResultDir;

  private NAModelSimulation m_simulation;

  private final NaSimulationDirs m_simDirs;

  private final INaSimulationData m_data;

  private final Logger m_logger;

  public NAOptimizingJob( final File tmpDir, final INaSimulationData data, final Logger logger )
  {
    m_tmpDir = tmpDir;
    m_data = data;
    m_logger = logger;

    m_optimizeRunDir = new File( m_tmpDir, "optimizeRun" );
    // Debug purpose only: copy of best ascii files
    m_bestOptimizeRunDir = null; // new File( m_tmpDir, "bestRun" );
    m_bestResultDir = new File( m_tmpDir, "bestResult" );
    m_bestOptimizedFile = new File( m_tmpDir, "bestOptimizeConfig.gml" );
    m_simDirs = new NaSimulationDirs( m_optimizeRunDir );
  }

  /**
   * Run myself in the {@link OptimizerRunner}.
   */
  @Override
  public IStatus run( final ISimulationMonitor monitor ) throws SimulationException
  {
    monitor.setMessage( "Lade Eingangsdaten..." );

    final NAControl metaControl = m_data.getMetaControl();
    final Date optimizationStartDate = metaControl.getOptimizationStart();
    final Date measuredEndDate = metaControl.getStartForecast();

    final NaOptimizeData optimizeData = m_data.getOptimizeData();

    final AutoCalibration autoCalibration = optimizeData.getAutoCalibration();
    // correct in intervall autocalibration
    final Pegel pegel = autoCalibration.getPegel();

    final Calendar calendarStart = Calendar.getInstance();
    calendarStart.setTime( optimizationStartDate );
    pegel.setStartDate( calendarStart );

    final Calendar calendarEnd = Calendar.getInstance();
    calendarEnd.setTime( measuredEndDate );
    pegel.setEndDate( calendarEnd );

    final OptimizerRunner runner = new OptimizerRunner( m_tmpDir, m_logger, this );
    return runner.run( monitor );
  }

  @Override
  public boolean calculate( final ISimulationMonitor monitor )
  {
    try
    {
      if( m_counter == 0 )
        // FIXME: if first run fails, we cannot 'runAgain', as the processor may not be initialized.
        m_lastResult = runFirst( monitor );
      else
        m_lastResult = runAgain( monitor );

      final String stepMessage = String.format( "Schritt %d", m_counter + 1 );
      final MultiStatus stepStatus = new MultiStatus( ModelNA.PLUGIN_ID, IStatus.OK, new IStatus[] { m_lastResult }, stepMessage, null );
      // TODO: güte der Optimierung in den stepStatus schreiben?!
      m_resultLog.add( stepStatus );

      return !m_lastResult.matches( IStatus.ERROR );
    }
    catch( final OperationCanceledException e )
    {
      final String msg = "Simulation canceled by user";
      m_logger.log( Level.INFO, msg );
      m_resultLog.add( IStatus.INFO, msg );
      monitor.setFinishInfo( IStatus.CANCEL, msg );
      return false;
    }
    catch( final Exception exception )
    {
      final String msg = "Unexpected error during simulation";
      m_logger.log( Level.SEVERE, msg, exception );
      m_resultLog.add( IStatus.ERROR, msg );
      return false;
    }
    finally
    {
      m_counter++;
    }
  }

  private IStatus runFirst( final ISimulationMonitor monitor ) throws Exception
  {
    final NACalculationLogger naCalculationLogger = new NACalculationLogger( new File( m_tmpDir, "logRun_" + m_counter ) );

    final Logger logger = naCalculationLogger.getLogger();

    try
    {
      m_simulation = new NAModelSimulation( m_simDirs, m_data, logger );
      return m_simulation.runSimulation( monitor );
    }
    finally
    {
      naCalculationLogger.stopLogging();
    }
  }

  private IStatus runAgain( final ISimulationMonitor monitor ) throws Exception
  {
    final NAOptimize optimize = m_data.getNaOptimize();
    return m_simulation.rerunForOptimization( optimize, monitor );
  }

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

        if( m_bestOptimizeRunDir != null )
          FileUtils.copyDirectory( m_optimizeRunDir, m_bestOptimizeRunDir );

        FileUtils.copyDirectory( resultDir, m_bestResultDir );
      }
      catch( final IOException e )
      {
        // FIXME: error handling
        e.printStackTrace();
      }

      m_bestResult = m_lastResult;
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

  @Override
  public void optimize( final Parameter[] parameterConf, final double values[] ) throws Exception
  {
    final ParameterOptimizeContext[] calcContexts = new ParameterOptimizeContext[parameterConf.length];
    for( int i = 0; i < parameterConf.length; i++ )
      calcContexts[i] = new ParameterOptimizeContext( parameterConf[i] );

    try
    {
      final GMLWorkspace contextWorkspace = m_data.getModelWorkspace();
      final NaOptimizeData optimizeData = m_data.getOptimizeData();
      optimizeData.applyCalibration( values, calcContexts, contextWorkspace );
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

      final IAxis dateAxis = ObservationUtilities.findAxisByType( observation.getAxes(), ITimeseriesConstants.TYPE_DATE );
      final IAxis qAxis = ObservationUtilities.findAxisByTypeNoEx( observation.getAxes(), ITimeseriesConstants.TYPE_RUNOFF );

      if( qAxis == null )
        return result;

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
    final IAxis dateAxis = ObservationUtilities.findAxisByType( observation.getAxes(), ITimeseriesConstants.TYPE_DATE );
    final IAxis qAxis = ObservationUtilities.findAxisByType( observation.getAxes(), ITimeseriesConstants.TYPE_RUNOFF );
    final ITupleModel values = observation.getValues( null );
    for( int i = 0; i < values.size(); i++ )
    {
      final Date date = (Date) values.get( i, dateAxis );
      final Double value = (Double) values.get( i, qAxis );
      result.put( date, value );
    }
    return result;
  }

  @Override
  public AutoCalibration getOptimizeConfiguration( )
  {
    return m_data.getOptimizeData().getAutoCalibration();
  }

  @Override
  public IStatus getResultStatus( )
  {
    // TODO: before: Gesamtergebnis war das Ergebnis (boolesch) des besten laufs

    return m_resultLog.asMultiStatus( "Optimierungrechnung" );
  }

  @Override
  public File getResultDir( )
  {
    return m_bestResultDir;
  }

  public File getBestOptimizeFile( )
  {
    return m_bestOptimizedFile;
  }

  @Override
  public File getOptimizeResult( )
  {
    return m_bestOptimizedFile;
  }

  @Override
  public void copyExeLog( final File targetFile ) throws IOException
  {
    final File currentResultDir = new File( m_bestResultDir, NaSimulationDirs.DIR_AKTUELL );
    final NaResultDirs currentResultDirs = new NaResultDirs( currentResultDir );
    currentResultDirs.copyExeLog( targetFile );
  }
}