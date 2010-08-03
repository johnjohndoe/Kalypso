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
package org.kalypso.convert.namodel;

import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.LineNumberReader;
import java.io.Reader;
import java.io.Writer;
import java.net.MalformedURLException;
import java.net.URL;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.SortedMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.namespace.QName;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.kalypso.commons.java.lang.ProcessHelper;
import org.kalypso.commons.lhwz.LhwzHelper;
import org.kalypso.contribs.java.io.filter.MultipleWildCardFileFilter;
import org.kalypso.contribs.java.net.UrlResolver;
import org.kalypso.convert.namodel.manager.IDManager;
import org.kalypso.convert.namodel.manager.LzsimManager;
import org.kalypso.convert.namodel.optimize.NAOptimizingJob;
import org.kalypso.convert.namodel.timeseries.BlockTimeSeries;
import org.kalypso.convert.namodel.timeseries.NATimeSettings;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.kalypsosimulationmodel.ui.calccore.CalcCoreUtils;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.internal.NaAsciiDirs;
import org.kalypso.model.hydrology.internal.NaSimulationDirs;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.internal.postprocessing.statistics.NAStatistics;
import org.kalypso.model.hydrology.internal.preprocessing.NAModelPreprocessor;
import org.kalypso.model.hydrology.internal.preprocessing.NAPreprocessorException;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IAxisRange;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTupleModel;
import org.kalypso.ogc.sensor.metadata.ITimeserieConstants;
import org.kalypso.ogc.sensor.metadata.MetadataList;
import org.kalypso.ogc.sensor.request.IRequest;
import org.kalypso.ogc.sensor.request.ObservationRequest;
import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.ogc.sensor.timeseries.envelope.TranProLinFilterUtilities;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author Gernot Belger
 */
public class NAModelSimulation
{
  private static final String SUFFIX_QGS = "qgs";

  public static final String EXECUTABLES_FILE_TEMPLATE = "Kalypso-NA_%s.exe"; //$NON-NLS-1$

  public static final String EXECUTABLES_FILE_PATTERN = "Kalypso-NA_(.+)\\.exe"; //$NON-NLS-1$

  private final DateFormat START_DATE_FORMAT = new SimpleDateFormat( "yyyy-MM-dd(HH-mm-ss)" ); //$NON-NLS-1$

  private final String m_startDateText = START_DATE_FORMAT.format( new Date() );

  final NAStatistics m_naStatistics;

  private final ISimulationDataProvider m_inputProvider;

  private final Logger m_logger;

  private final NAConfiguration m_conf;

  private final IDManager m_idManager;

  private final NaSimulationDirs m_simDirs;

  public NAModelSimulation( final NaSimulationDirs simDirs, final ISimulationDataProvider inputProvider, final Logger logger ) throws SimulationException
  {
    m_simDirs = simDirs;
    m_inputProvider = inputProvider;
    m_logger = logger;

    m_logger.log( Level.INFO, Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.13", m_startDateText ) ); //$NON-NLS-1$ 

    m_conf = new NAConfiguration( simDirs.asciiDir );
    m_conf.setZMLContext( (URL) inputProvider.getInputForID( NaModelConstants.IN_META_ID ) );

    m_idManager = m_conf.getIdManager();
    m_naStatistics = new NAStatistics( m_logger );
  }

  /**
   * Translates the id inside the KalypsoNA log to KalypsoHydrology id's.
   */
  public void translateLogs( )
  {
    final NaFortranLogTranslater logTranslater = new NaFortranLogTranslater( m_simDirs.asciiDir, m_idManager, m_logger );

    final File resultFile = new File( m_simDirs.logDir, "error.gml" ); //$NON-NLS-1$
    resultFile.getParentFile().mkdirs();

    logTranslater.translate( resultFile );
  }

  public boolean runSimulation( final ISimulationMonitor monitor ) throws Exception
  {
    final NaSimulationData simulationData = loadData();

    if( !preprocess( simulationData, monitor ) )
      return false;

    if( monitor.isCanceled() )
      return false;

    // starte berechnung
    monitor.setMessage( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.27" ) ); //$NON-NLS-1$
    if( monitor.isCanceled() )
      return false;

    startCalculation( simulationData, monitor );

    final boolean succeeded = checkSucceeded();
    if( succeeded )
    {
      monitor.setMessage( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.28" ) ); //$NON-NLS-1$
      m_logger.log( Level.FINEST, Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.29" ) ); //$NON-NLS-1$
      loadResults( simulationData, m_logger, m_simDirs.resultDir, m_conf );
      monitor.setMessage( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.30" ) ); //$NON-NLS-1$
      m_logger.log( Level.FINEST, Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.31" ) ); //$NON-NLS-1$

      m_naStatistics.writeStatistics( m_simDirs.currentResultDir, m_simDirs.reportDir );
    }
    else
    {
      monitor.setMessage( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.32" ) ); //$NON-NLS-1$
      m_logger.log( Level.SEVERE, Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.33" ) ); //$NON-NLS-1$
    }

    return succeeded;
  }

  private NaSimulationData loadData( ) throws Exception
  {
    final URL modelUrl = (URL) m_inputProvider.getInputForID( NaModelConstants.IN_MODELL_ID );
    final URL controlURL = (URL) m_inputProvider.getInputForID( NaModelConstants.IN_CONTROL_ID );
    final URL metaUrl = (URL) m_inputProvider.getInputForID( NaModelConstants.IN_META_ID );
    final URL parameterUrl = (URL) m_inputProvider.getInputForID( NaModelConstants.IN_PARAMETER_ID );
    final URL hydrotopUrl = (URL) m_inputProvider.getInputForID( NaModelConstants.IN_HYDROTOP_ID );
    final URL syntNUrl = (URL) m_inputProvider.getInputForID( NaModelConstants.IN_RAINFALL_ID );
    final URL lzsimUrl = getStartConditionFile();

// final URL landuseUrl = getInputOrNull( NaModelConstants.IN_LANDUSE_ID );
    final URL sudsUrl = getInputOrNull( NaModelConstants.IN_SUDS_ID );

    return new NaSimulationData( modelUrl, controlURL, metaUrl, parameterUrl, hydrotopUrl, sudsUrl, syntNUrl, lzsimUrl );
  }

  private URL getInputOrNull( final String id ) throws SimulationException
  {
    if( !m_inputProvider.hasID( id ) )
      return null;

    return (URL) m_inputProvider.getInputForID( id );
  }

  private boolean preprocess( final NaSimulationData simulationData, final ISimulationMonitor monitor ) throws SimulationException
  {
    try
    {
      final NAModelPreprocessor preprocessor = new NAModelPreprocessor( m_conf, m_simDirs.asciiDirs, m_idManager, simulationData, m_logger );
      preprocessor.setPreprocessedFilesDir( getPreprocessFilesDir() );
      preprocessor.process( monitor );

      dumpIdManager();

      return true;
    }
    catch( final NAPreprocessorException e )
    {
      final String msg = String.format( "Failed to convert data in Kalypso-NA.exe format files: %s", e.getLocalizedMessage() );
      m_logger.log( Level.SEVERE, msg, e );
      return false;
    }
    catch( final OperationCanceledException e )
    {
      return false;
    }
  }

  private void dumpIdManager( ) throws SimulationException
  {
    // dump idmapping to file
    final IDManager idManager = m_conf.getIdManager();
    Writer idWriter = null;
    try
    {
      idWriter = new FileWriter( new File( m_simDirs.simulationDir, "IdMap.txt" ) ); //$NON-NLS-1$
      idManager.dump( idWriter );
    }
    catch( final IOException e )
    {
      throw new SimulationException( "Failed to dump idManager", e );
    }
    finally
    {
      IOUtils.closeQuietly( idWriter );
    }
  }

  private URL getStartConditionFile( ) throws SimulationException
  {
    if( !m_inputProvider.hasID( NaModelConstants.LZSIM_IN_ID ) )
      return null;

    final URL iniValuesFolderURL = (URL) m_inputProvider.getInputForID( NaModelConstants.LZSIM_IN_ID );
    try
    {
      // TODO: crude way to create the new URL, necessary as probably we do not have a '/' at the end of the path
      return new URL( iniValuesFolderURL.toExternalForm() + "/lzsim.gml" ); //$NON-NLS-1$
    }
    catch( final MalformedURLException e )
    {
      throw new SimulationException( "Failed to read start condition file", e );
    }
  }

  private File getPreprocessFilesDir( ) throws SimulationException
  {
    if( !m_inputProvider.hasID( NAOptimizingJob.IN_BestOptimizedRunDir_ID ) )
      return null;

    final URL url = (URL) m_inputProvider.getInputForID( NAOptimizingJob.IN_BestOptimizedRunDir_ID );
    return FileUtils.toFile( url );
  }

  public void backupResults( )
  {
    final File resultDirTo = new File( m_simDirs.resultDir, m_startDateText ); //$NON-NLS-1$

    try
    {
      // Copy results to restore the actual results in the dateDir as well... .
      // FIXME: probably this should be done by the ant task instead
      FileUtils.copyDirectory( m_simDirs.currentResultDir, resultDirTo );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      final String msg = String.format( "Failed to backup results to directory: %s (%s)", resultDirTo.getAbsoluteFile(), e.getLocalizedMessage() );
      m_logger.warning( msg );
    }
  }

  private File copyExecutable( final NaSimulationData simulationData, final File destDir ) throws SimulationException
  {
    final GMLWorkspace metaWorkspace = simulationData.getMetaWorkspace();
    final Feature metaFE = metaWorkspace.getRootFeature();

    try
    {
      final String kalypsoNAVersion = (String) metaFE.getProperty( NaModelConstants.CONTROL_VERSION_KALYPSONA_PROP );
      final File kalypsoNaExe = CalcCoreUtils.findExecutable( kalypsoNAVersion, EXECUTABLES_FILE_TEMPLATE, EXECUTABLES_FILE_PATTERN, CalcCoreUtils.COMPATIBILITY_MODE.NA );
      if( kalypsoNaExe == null )
        throw new SimulationException( "No Kalypso-NA.exe version configured." );

      final File destFile = new File( destDir, kalypsoNaExe.getName() );
      if( !destFile.exists() )
        FileUtils.copyFile( kalypsoNaExe, destFile );

      return destFile;
    }
    catch( final CoreException e )
    {
      final IStatus status = e.getStatus();
      final String msg = String.format( "No Kalypso-NA.exe version configured: %s", status.getMessage() );
      throw new SimulationException( msg, e );
    }
    catch( final IOException e )
    {
      final String msg = String.format( "Failed to copy Kalypso-NA.exe into calculation directory: %s", e.getLocalizedMessage() );
      throw new SimulationException( msg, e );
    }
  }

  private void startCalculation( final NaSimulationData simulationData, final ISimulationMonitor monitor ) throws SimulationException
  {
    final NaAsciiDirs asciiDirs = m_simDirs.asciiDirs;
    final File startDir = asciiDirs.startDir;
    final File kalypsoNaExe = copyExecutable( simulationData, startDir );

    final String commandString = kalypsoNaExe.getAbsolutePath();

    final long timeOut = 0l; // no timeout control

    FileOutputStream logOS = null;
    FileOutputStream errorOS = null;
    try
    {
      logOS = new FileOutputStream( new File( asciiDirs.asciiDir, "exe.log" ) ); //$NON-NLS-1$
      errorOS = new FileOutputStream( new File( asciiDirs.asciiDir, "exe.err" ) ); //$NON-NLS-1$
      ProcessHelper.startProcess( commandString, new String[0], startDir, monitor, timeOut, logOS, errorOS, null );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new SimulationException( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.249" ), e ); //$NON-NLS-1$
    }
    finally
    {
      IOUtils.closeQuietly( logOS );
      IOUtils.closeQuietly( errorOS );
    }
  }

  private boolean checkSucceeded( )
  {
    Reader logFileReader = null;
    LineNumberReader reader = null;
    try
    {
      final File startDir = m_simDirs.asciiDirs.startDir;
      final File logFile = new File( startDir, "output.res" ); //$NON-NLS-1$
      logFileReader = new FileReader( logFile );
      reader = new LineNumberReader( logFileReader );
      String line;
      while( (line = reader.readLine()) != null )
      {
        if( line.indexOf( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.54" ) ) >= 0 || line.indexOf( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.55" ) ) >= 0 ) //$NON-NLS-1$ //$NON-NLS-2$
          return true;
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    finally
    {
      IOUtils.closeQuietly( reader );
      IOUtils.closeQuietly( logFileReader );
    }

    return false;
  }

  private void loadResults( final NaSimulationData simulationData, final Logger logger, final File resultDir, final NAConfiguration conf ) throws Exception
  {
    final GMLWorkspace modelWorkspace = simulationData.getModelWorkspace();

    loadTSResults( modelWorkspace, conf );
    try
    {
      final GMLWorkspace naControlWorkspace = simulationData.getControlWorkspace();
      loadTesultTSPredictionIntervals( naControlWorkspace, logger, resultDir, conf );
    }
    catch( final Exception e )
    {
      logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.83", e.getLocalizedMessage() ) ); //$NON-NLS-1$
    }
    loadTextFileResults();

    final Date[] initialDates = conf.getInitialDates();
    final LzsimManager lzsimManager = new LzsimManager( initialDates, m_simDirs.anfangswertDir );
    lzsimManager.readInitialValues( conf.getIdManager(), m_simDirs.asciiDirs.lzsimDir, logger );
  }

  private void loadTSResults( final GMLWorkspace modellWorkspace, final NAConfiguration conf ) throws Exception
  {
    // j Gesamtabfluss Knoten .qgs
    final IFeatureType nodeFT = modellWorkspace.getGMLSchema().getFeatureType( NaModelConstants.NODE_ELEMENT_FT );
    loadTSResults( SUFFIX_QGS, nodeFT, ITimeserieConstants.TYPE_RUNOFF, "pegelZR", "qberechnetZR", modellWorkspace, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

    final IFeatureType catchmentFT = modellWorkspace.getGMLSchema().getFeatureType( NaModelConstants.CATCHMENT_ELEMENT_FT );
    final IFeatureType rhbChannelFT = modellWorkspace.getGMLSchema().getFeatureType( NaModelConstants.STORAGE_CHANNEL_ELEMENT_FT );
    // j Niederschlag .pre
    loadTSResults( "pre", catchmentFT, ITimeserieConstants.TYPE_RAINFALL, null, null, modellWorkspace, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // j Temperatur .tmp
    loadTSResults( "tmp", catchmentFT, ITimeserieConstants.TYPE_TEMPERATURE, null, null, modellWorkspace, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // n Interflow .qif
    loadTSResults( "qif", catchmentFT, ITimeserieConstants.TYPE_RUNOFF, null, null, modellWorkspace, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // n Grundwasser .qgw
    loadTSResults( "qgw", catchmentFT, ITimeserieConstants.TYPE_RUNOFF, null, null, modellWorkspace, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // n Gesamtabfluss TG .qgg
    loadTSResults( "qgg", catchmentFT, ITimeserieConstants.TYPE_RUNOFF, null, null, modellWorkspace, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // n Grundwasserstand .gws - Umrechnung von m auf cm
    loadTSResults( "gws", catchmentFT, ITimeserieConstants.TYPE_WATERLEVEL, null, null, modellWorkspace, 100.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // n Basisabfluss .qbs
    loadTSResults( "qbs", catchmentFT, ITimeserieConstants.TYPE_RUNOFF, null, null, modellWorkspace, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // n Oberflaechenabfluss .qna
    loadTSResults( "qna", catchmentFT, ITimeserieConstants.TYPE_RUNOFF, null, null, modellWorkspace, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // n Abfluss vers. Flaechen .qvs
    loadTSResults( "qvs", catchmentFT, ITimeserieConstants.TYPE_RUNOFF, null, null, modellWorkspace, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // TODO:check output for the next time series
    // n Schnee .sch [mm]
    loadTSResults( "sch", catchmentFT, ITimeserieConstants.TYPE_WATERLEVEL, null, null, modellWorkspace, 0.1d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // n Kluftgrundw1 .qt1
    loadTSResults( "qt1", catchmentFT, ITimeserieConstants.TYPE_RUNOFF, null, null, modellWorkspace, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // n Kluftgrundw .qtg
    loadTSResults( "qtg", catchmentFT, ITimeserieConstants.TYPE_RUNOFF, null, null, modellWorkspace, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // n Evapotranspiration .vet [mm]
    loadTSResults( "vet", catchmentFT, ITimeserieConstants.TYPE_EVAPORATION, null, null, modellWorkspace, 0.1d, conf ); //$NON-NLS-1$ //$NON-NLS-2$

    // TODO: Zeitreihe als mittlere Bodenfeuchte aus Fortran übernehmen, daher bisher nicht zu übertragen (Zur zeit wird
    // die Bodenfeuchte des ersten Hydrotopes in allen Schichten ausgegeben)
    // j Bodenfeuchte .bof [mm]
    // loadTSResults( "bof", catchmentFT, "name", TimeserieConstants.TYPE_WATERLEVEL, null, null, inputDir,
    // modellWorkspace, logger, outputDir, 0.1d, conf );

    // Straenge
    // n Wasserstand Speicher .sph [muNN]
    loadTSResults( "sph", rhbChannelFT, ITimeserieConstants.TYPE_NORMNULL, null, null, modellWorkspace, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // n Speicherueberlauf .sub [m³/s]
    loadTSResults( "sub", rhbChannelFT, ITimeserieConstants.TYPE_RUNOFF, null, null, modellWorkspace, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // n Speicherinhalt .spi [hm³] - Umrechnung auf m³
    loadTSResults( "spi", rhbChannelFT, ITimeserieConstants.TYPE_VOLUME, null, null, modellWorkspace, 1000000.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // n Talsperrenverdunstung .spv [m³/d]
    // loadTSResults( "spv", catchmentFT, "name", TimeserieConstants.TYPE_RUNOFF, null, null, inputDir,
    // modellWorkspace, logger, outputDir, 1.0d , idManager);
    // n Zehrung .spn [m³/d]
    // loadTSResults( "spn", catchmentFT, "name", TimeserieConstants.TYPE_RUNOFF, null, null, inputDir,
    // modellWorkspace, logger, outputDir, 1.0d , idManager);

    // n Kapil.Aufstieg/Perkolation .kap [mm]
    // loadTSResults( "kap", catchmentFT, "name", TimeserieConstants.TYPE_WATERLEVEL, null, null, inputDir,
    // modellWorkspace, logger, outputDir, 0.1d , idManager);
    // n Ausgabe hydrotope .hyd
    // loadTSResults( "hyd", catchmentFT, "name", TimeserieConstants.TYPE_WATERLEVEL, null, null, inputDir,
    // modellWorkspace, logger, outputDir, 1.0d , idManager);
  }

  private void loadTSResults( final String suffix, final IFeatureType resultFT, final String resultType, final String metadataTSLink, final String targetTSLink, final GMLWorkspace modellWorkspace, final double resultFactor, final NAConfiguration conf ) throws Exception
  {
    final String titlePropName = "name";

    final IFeatureType FT_NODE = modellWorkspace.getGMLSchema().getFeatureType( NaModelConstants.NODE_ELEMENT_FT );
    final IFeatureType FT_CATCHMENT = modellWorkspace.getGMLSchema().getFeatureType( NaModelConstants.CATCHMENT_ELEMENT_FT );
    final IFeatureType FT_STORAGE_CHANNEL = modellWorkspace.getGMLSchema().getFeatureType( NaModelConstants.STORAGE_CHANNEL_ELEMENT_FT );

    final boolean isConsiderableFeatureType = FT_NODE.equals( resultFT ) || FT_CATCHMENT.equals( resultFT ) || FT_STORAGE_CHANNEL.equals( resultFT );

    final IDManager idManager = conf.getIdManager();
    // ASCII-Files
    // generiere ZML Ergebnis Dateien
    final MultipleWildCardFileFilter filter = new MultipleWildCardFileFilter( new String[] { "*" + suffix + "*" }, false, false, true ); //$NON-NLS-1$ //$NON-NLS-2$
    final File[] qgsFiles = m_simDirs.asciiDirs.outWeNatDir.listFiles( filter );
    if( qgsFiles.length != 0 )
    {
      // read ascii result file
      m_logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.123" ) + qgsFiles[0].getName() + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$
      final BlockTimeSeries ts = new BlockTimeSeries();
      ts.importBlockFile( qgsFiles[0] );

      // iterate model nodes/Catchments/rhbChannels and generate zml

      final Feature[] resultFeatures = modellWorkspace.getFeatures( resultFT );
      for( final Feature resultFeature : resultFeatures )
      {
        if( isConsiderableFeatureType && !FeatureHelper.booleanIsTrue( resultFeature, NaModelConstants.GENERATE_RESULT_PROP, false ) )
          continue; // should not generate results

        final String key = Integer.toString( idManager.getAsciiID( resultFeature ) );

        final String axisTitle = getAxisTitleForSuffix( suffix );

        if( !ts.dataExistsForKey( key ) )
          continue; // no results available
        m_logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.125", key, resultFeature.getFeatureType().getQName(), suffix ) + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$

        // transform data to tuppelmodel
        final SortedMap<Date, String> data = ts.getTimeSerie( key );
        final Object[][] tupelData = new Object[data.size()][3];
        final Set<Entry<Date, String>> dataSet = data.entrySet();
        final Iterator<Entry<Date, String>> iter = dataSet.iterator();
        int pos = 0;
        while( iter.hasNext() )
        {
          final Map.Entry<Date, String> entry = iter.next();
          tupelData[pos][0] = entry.getKey();
          tupelData[pos][1] = new Double( Double.parseDouble( entry.getValue().toString() ) * resultFactor );
          tupelData[pos][2] = new Integer( KalypsoStati.BIT_OK );
          pos++;
        }

        final IAxis dateAxis = new DefaultAxis( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.4" ), ITimeserieConstants.TYPE_DATE, "", Date.class, true ); //$NON-NLS-1$ //$NON-NLS-2$
        final IAxis qAxis = new DefaultAxis( axisTitle, resultType, TimeserieUtils.getUnit( resultType ), Double.class, false );
        final IAxis statusAxis = KalypsoStatusUtils.createStatusAxisFor( qAxis, true );
        final IAxis[] axis = new IAxis[] { dateAxis, qAxis, statusAxis };
        final ITupleModel qTuppelModel = new SimpleTupleModel( axis, tupelData );

        final MetadataList metadataList = new MetadataList();

        // if pegel exists, copy metadata (inclusive wq-function)
        TimeseriesLinkType pegelLink = null;
        if( metadataTSLink != null )
          pegelLink = (TimeseriesLinkType) resultFeature.getProperty( metadataTSLink );
        if( pegelLink != null )
        {
          final URL pegelURL = new URL( modellWorkspace.getContext(), pegelLink.getHref() );
          boolean itExists;
          // test if url exists
          try
          {
            pegelURL.openStream();
            itExists = true;
          }
          catch( final Exception e )
          {
            itExists = false;
          }
          if( itExists )
          {
            m_logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.132" ) ); //$NON-NLS-1$
            final IObservation pegelObservation = ZmlFactory.parseXML( pegelURL ); //$NON-NLS-1$

            copyMetaData( pegelObservation.getMetadataList(), metadataList, new String[] { ITimeserieConstants.MD_ALARM_1, ITimeserieConstants.MD_ALARM_2, ITimeserieConstants.MD_ALARM_3,
                ITimeserieConstants.MD_ALARM_4, ITimeserieConstants.MD_GEWAESSER, ITimeserieConstants.MD_FLUSSGEBIET, ITimeserieConstants.MD_GKH, ITimeserieConstants.MD_GKR,
                ITimeserieConstants.MD_HOEHENANGABEART, ITimeserieConstants.MD_PEGELNULLPUNKT, ITimeserieConstants.MD_WQWECHMANN, ITimeserieConstants.MD_WQTABLE, ITimeserieConstants.MD_TIMEZONE,
                ITimeserieConstants.MD_VORHERSAGE_START, ITimeserieConstants.MD_VORHERSAGE_ENDE } );

          }
        }
        // lese ergebnis-link um target fuer zml zu finden
        String resultPathRelative = ""; //$NON-NLS-1$
        if( targetTSLink != null )
        {
          try
          {
            final TimeseriesLinkType resultLink = (TimeseriesLinkType) resultFeature.getProperty( targetTSLink );
            if( resultLink == null )
            {
              m_logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.134", resultFeature.getId() ) ); //$NON-NLS-1$ 
              resultPathRelative = DefaultPathGenerator.generateResultPathFor( resultFeature, titlePropName, suffix, null );
            }
            else
            {
              final String href = resultLink.getHref();

              // WTF!?
              // resultPathRelative = href.substring( 19 );

              resultPathRelative = "Pegel" + href.substring( href.lastIndexOf( "/" ) ); //$NON-NLS-1$ //$NON-NLS-2$
            }
          }
          catch( final Exception e )
          {
            // if there is target defined or there are some problems with that
            // we generate one
            resultPathRelative = DefaultPathGenerator.generateResultPathFor( resultFeature, titlePropName, suffix, null );
          }
        }
        else
        {
          resultPathRelative = DefaultPathGenerator.generateResultPathFor( resultFeature, titlePropName, suffix, null );
        }

        final File resultFile = tweakResultPath( resultPathRelative, resultFeature, titlePropName, suffix );
        resultFile.getParentFile().mkdirs();

        // FIXME: Performance: use this observation to calculate statistics,
        // no need to read the observation a second time
        if( SUFFIX_QGS.equals( suffix ) )
          m_naStatistics.add( resultFeature, resultFile );

        // create observation object
        final String titleForObservation = DefaultPathGenerator.generateTitleForObservation( resultFeature, titlePropName, suffix );

        final IObservation resultObservation = new SimpleObservation( resultPathRelative, titleForObservation, metadataList, qTuppelModel ); //$NON-NLS-1$

        ZmlFactory.writeToFile( resultObservation, resultFile );
      }
    }
  }

  private File tweakResultPath( final String resultPathRelative, final Feature resultFeature, final String titlePropName, final String suffix )
  {
    final File resultFile = new File( m_simDirs.currentResultDir, resultPathRelative ); //$NON-NLS-1$
    if( !resultFile.exists() )
      return resultFile;

    // FIXME: Arrg! Is this really possible to happen? Most probably something else is wrong. We should not
    // do such terrible things here!
    m_logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.136", resultPathRelative ) ); //$NON-NLS-1$
    final String extra = "(ID" + Integer.toString( m_idManager.getAsciiID( resultFeature ) ).trim() + ")";
    final String resultPath = DefaultPathGenerator.generateResultPathFor( resultFeature, titlePropName, suffix, extra ); //$NON-NLS-1$ //$NON-NLS-2$
    m_logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.140", resultPath ) ); //$NON-NLS-1$

    return new File( m_simDirs.currentResultDir, resultPath ); //$NON-NLS-1$
  }

  private static void copyMetaData( final MetadataList srcMeta, final MetadataList destMeta, final String[] mdKeys )
  {
    for( final String key : mdKeys )
    {
      final String property = srcMeta.getProperty( key );
      if( property != null )
        destMeta.put( key, property );
    }
  }

  /**
   * @param naControlWorkspace
   * @param logger
   * @param resultDir
   * @param conf
   * @throws Exception
   */
  private void loadTesultTSPredictionIntervals( final GMLWorkspace naControlWorkspace, final Logger logger, final File resultDir, final NAConfiguration conf ) throws Exception
  {
    // Load the calculated prediction
    final Feature rootFeature = naControlWorkspace.getRootFeature();
    final IObservation resultObservation = loadPredictedResult( resultDir, rootFeature );
    final IAxis[] axisList = resultObservation.getAxisList();
    final String axisType = determineTranpolinAxis( resultObservation );

    final File fileMitte = getResultFileFor( resultDir, rootFeature, new QName( NaModelConstants.NS_NACONTROL, "qAblageSpurMittlerer" ) ); //$NON-NLS-1$
    final File fileUnten = getResultFileFor( resultDir, rootFeature, new QName( NaModelConstants.NS_NACONTROL, "qAblageSpurUnterer" ) ); //$NON-NLS-1$
    final File fileOben = getResultFileFor( resultDir, rootFeature, new QName( NaModelConstants.NS_NACONTROL, "qAblageSpurOberer" ) ); //$NON-NLS-1$

    // Initalize some commen variables
    final ITupleModel resultValues = resultObservation.getValues( null );
    final IAxis resultDateAxis = ObservationUtilities.findAxisByClass( axisList, Date.class );
    final IAxis resultValueAxis = ObservationUtilities.findAxisByType( axisList, axisType );

    final Date startForecast = conf.getSimulationForecastStart();
    final Date endForecast = conf.getSimulationEnd();

    final IAxisRange rangeFor = resultValues.getRangeFor( resultDateAxis );
    final Date endPrediction = (Date) rangeFor.getUpper();

    final NATimeSettings timeSettings = NATimeSettings.getInstance();
    final Calendar calBegin = timeSettings.getCalendar( startForecast );
    // REMARK: using endPrediction instead of endForecast, as they are not equals (but they should...)
    final Calendar calEnd = timeSettings.getCalendar( endPrediction );

    final double calcStartValue = ObservationUtilities.getInterpolatedValueAt( resultValues, resultDateAxis, resultValueAxis, startForecast );
    final double calcEndValue = ObservationUtilities.getInterpolatedValueAt( resultValues, resultDateAxis, resultValueAxis, endForecast );

    //
    // First, we adapt the result: correction at start and/or end of the calculated timeserie
    //

    double deltaMeasureCalculation;
    try
    {
      final NaNodeResultProvider nodeResultProvider = conf.getNodeResultProvider();
      final URL pegelURL = nodeResultProvider.getMeasuredURL( rootFeature );

      // from measuered timeseries
      final IObservation pegelObservation = ZmlFactory.parseXML( pegelURL ); //$NON-NLS-1$
      final ITupleModel pegelValues = pegelObservation.getValues( null );
      final IAxis pegelDateAxis = ObservationUtilities.findAxisByClass( pegelObservation.getAxisList(), Date.class );
      final IAxis pegelValueAxis = ObservationUtilities.findAxisByType( pegelObservation.getAxisList(), axisType );
      final double measureValue = ObservationUtilities.getInterpolatedValueAt( pegelValues, pegelDateAxis, pegelValueAxis, startForecast );
      deltaMeasureCalculation = measureValue - calcStartValue;
    }
    catch( final Exception e )
    {
      deltaMeasureCalculation = 0;
    }

    final double offsetStartPrediction;
    final double offsetEndPrediction;
    if( FeatureHelper.booleanIsTrue( rootFeature, NaModelConstants.NACONTROL_USEOFFSTARTPRED_PROP, false ) )
      offsetStartPrediction = deltaMeasureCalculation;
    else
      offsetStartPrediction = 0;
    if( FeatureHelper.booleanIsTrue( rootFeature, NaModelConstants.NACONTROL_USEOFFENDPRED_PROP, false ) )
      offsetEndPrediction = deltaMeasureCalculation;
    else
      offsetEndPrediction = 0;

    final Calendar tranpolinEnd = timeSettings.getCalendar( startForecast );
    tranpolinEnd.add( Calendar.HOUR, 24 );

    final IRequest request = new ObservationRequest( calBegin.getTime(), calEnd.getTime() );
    TranProLinFilterUtilities.transformAndWrite( resultObservation, calBegin, tranpolinEnd, offsetStartPrediction, offsetEndPrediction, "+", axisType, KalypsoStati.BIT_DERIVATED, fileMitte, " - Spur Mitte", request ); //$NON-NLS-1$ //$NON-NLS-2$

    // read the freshly created file into a new observation, we are going to umhüll it
    final IObservation adaptedResultObservation = ZmlFactory.parseXML( fileMitte.toURI().toURL() ); //$NON-NLS-1$

    //
    // Second, we build the umhüllenden for the adapted result
    //
    double accuracyPrediction = LhwzHelper.getDefaultUmhuellendeAccuracy();
    final Double featureAccuracy = (Double) rootFeature.getProperty( NaModelConstants.NACONTROL_ACCPRED_PROP );
    if( featureAccuracy == null )
      logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.44", accuracyPrediction ) ); //$NON-NLS-1$ 
    else
      accuracyPrediction = featureAccuracy.doubleValue();

    // accuracyPrediction // %/60h
    final long millisOf60hours = 1000 * 60 * 60 * 60;
    // endAccuracy: %/simulationRange
    final double endAccuracy = accuracyPrediction * (((double) (endForecast.getTime() - startForecast.getTime())) / ((double) millisOf60hours));

    final double endOffset = calcEndValue * (endAccuracy / 100);

    TranProLinFilterUtilities.transformAndWrite( adaptedResultObservation, calBegin, calEnd, 0, endOffset, "-", axisType, KalypsoStati.BIT_DERIVATED, fileUnten, " - spur Unten", request ); //$NON-NLS-1$ //$NON-NLS-2$
    TranProLinFilterUtilities.transformAndWrite( adaptedResultObservation, calBegin, calEnd, 0, endOffset, "+", axisType, KalypsoStati.BIT_DERIVATED, fileOben, " - spur Oben", request ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  private void loadTextFileResults( )
  {
    // kopiere statistische Ergebnis-Dateien
    final String[] wildcards = new String[] { "*" + "bil" + "*" }; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    final MultipleWildCardFileFilter filter = new MultipleWildCardFileFilter( wildcards, false, false, true );

    final File[] qgsFiles = m_simDirs.asciiDirs.outWeNatDir.listFiles( filter );
    if( qgsFiles.length != 0 )

    {
      for( final File element : qgsFiles )
      {
        // read ascii result file
        m_logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.220" ) + element.getName() + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$

        final File resultFile = new File( m_simDirs.bilanzDir, "Bilanz.txt" ); //$NON-NLS-1$ 
        resultFile.getParentFile().mkdirs();

        try
        {
          FileUtils.copyFile( element, resultFile );
        }
        catch( final IOException e )
        {
          final String inputPath = m_simDirs.asciiDirs.outWeNatDir.getName() + element.getName();
          e.printStackTrace();
          System.out.println( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.223" ) + inputPath + Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.224" ) ); //$NON-NLS-1$ //$NON-NLS-2$
        }
      }
    }
  }

  /**
   * @param suffix
   * @return AxisTitle
   */
  private String getAxisTitleForSuffix( final String suffix )
  {
    // j Temperatur .tmp
    if( suffix.equalsIgnoreCase( "tmp" ) ) //$NON-NLS-1$
      return Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.5" ); //$NON-NLS-1$
    // j Niederschlag .pre
    if( suffix.equalsIgnoreCase( "pre" ) ) //$NON-NLS-1$
      return Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.6" ); //$NON-NLS-1$
    // n Schnee .sch
    if( suffix.equalsIgnoreCase( "sch" ) ) //$NON-NLS-1$
      return Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.7" ); //$NON-NLS-1$
    // j Bodenfeuchte .bof
    if( suffix.equalsIgnoreCase( "bof" ) ) //$NON-NLS-1$
      return Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.8" ); //$NON-NLS-1$
    // n Bodenspeicher .bsp
    if( suffix.equalsIgnoreCase( "bsp" ) ) //$NON-NLS-1$
      return Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.9" ); //$NON-NLS-1$
    // n Grundwasserstand .gws
    if( suffix.equalsIgnoreCase( "gws" ) ) //$NON-NLS-1$
      return Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.10" ); //$NON-NLS-1$
    // Gesamtabfluss Knoten .qgs, Gesamtabfluss TG .qgg, Oberflaechenabfluss .qna, Interflow .qif, Abfluss vers.
    // Flaechen .qvs, Basisabfluss .qbs, Kluftgrundw1 .qt1, Kluftgrundw .qtg, Grundwasser .qgw
    if( suffix.equalsIgnoreCase( SUFFIX_QGS ) | suffix.equalsIgnoreCase( "qgg" ) | suffix.equalsIgnoreCase( "qna" ) | suffix.equalsIgnoreCase( "qif" ) | suffix.equalsIgnoreCase( "qvs" ) //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
        | suffix.equalsIgnoreCase( "qbs" ) | suffix.equalsIgnoreCase( "qt1" ) | suffix.equalsIgnoreCase( "qtg" ) | suffix.equalsIgnoreCase( "qgw" ) ) //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
      return Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.11" ); //$NON-NLS-1$
    // n Evapotranspiration .vet
    if( suffix.equalsIgnoreCase( "vet" ) ) //$NON-NLS-1$
      return Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.12" ); //$NON-NLS-1$
    // n Ausgabe hydrotope .hyd
    if( suffix.equalsIgnoreCase( "hyd" ) ) //$NON-NLS-1$
      return Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.16" ); //$NON-NLS-1$
    // n Abflussbilanz .bil
    if( suffix.equalsIgnoreCase( "bil" ) ) //$NON-NLS-1$
      return Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.17" ); //$NON-NLS-1$
    // n Statistische Abflusswerte .nmq
    if( suffix.equalsIgnoreCase( "nmq" ) ) //$NON-NLS-1$
      return Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.18" ); //$NON-NLS-1$
    // n Speicherinhalt .spi
    if( suffix.equalsIgnoreCase( "spi" ) ) //$NON-NLS-1$
      return Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.19" ); //$NON-NLS-1$
    // n Wasserspiegelhöhe .sph
    if( suffix.equalsIgnoreCase( "sph" ) ) //$NON-NLS-1$
      return Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.20" ); //$NON-NLS-1$
    // n Verdunstung aus Talsperre .spv
    if( suffix.equalsIgnoreCase( "spv" ) ) //$NON-NLS-1$
      return Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.21" ); //$NON-NLS-1$
    // n Niederschlag in Talsperre .spn
    if( suffix.equalsIgnoreCase( "spn" ) ) //$NON-NLS-1$
      return Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.22" ); //$NON-NLS-1$
    // n Zehrung .spb
    if( suffix.equalsIgnoreCase( "spb" ) ) //$NON-NLS-1$
      return Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.24" ); //$NON-NLS-1$
    // n Speicherueberlauf .sub
    if( suffix.equalsIgnoreCase( "sub" ) ) //$NON-NLS-1$
      return Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.25" ); //$NON-NLS-1$
    return suffix;
  }

  private IObservation loadPredictedResult( final File resultDir, final Feature rootFeature ) throws MalformedURLException, SensorException
  {
    final TimeseriesLinkType resultLink = (TimeseriesLinkType) rootFeature.getProperty( NaModelConstants.NACONTROL_RESULT_TIMESERIESLINK_PROP );

    // from predicted timeseries
    final UrlResolver urlResolver = new UrlResolver();
    final URL resultURL = urlResolver.resolveURL( resultDir.toURI().toURL(), resultLink.getHref() );
    return ZmlFactory.parseXML( resultURL ); //$NON-NLS-1$
  }

  /**
   * @param resultDir
   * @param feature
   * @param tsLinkPropName
   * @return file for result or null
   */
  private File getResultFileFor( final File resultDir, final Feature feature, final QName tsLinkPropName )
  {
    try
    {
      final TimeseriesLinkType trackLink = (TimeseriesLinkType) feature.getProperty( tsLinkPropName );
      final String href = trackLink.getHref();
      final File resultFile = new File( resultDir, href );
      resultFile.getParentFile().mkdirs();
      return resultFile;
    }
    catch( final Exception e )
    {
      Logger.getAnonymousLogger().log( Level.WARNING, e.getLocalizedMessage() );
      return null; // no track available
    }
  }

  /**
   * Return with which axis we are going to transform the umhüllenden. Q or W, depending on what is present)
   */
  private String determineTranpolinAxis( final IObservation observation ) throws SimulationException
  {
    final IAxis[] axisList = observation.getAxisList();

    if( ObservationUtilities.hasAxisOfType( axisList, ITimeserieConstants.TYPE_RUNOFF ) )
      return ITimeserieConstants.TYPE_RUNOFF;

    if( ObservationUtilities.hasAxisOfType( axisList, ITimeserieConstants.TYPE_WATERLEVEL ) )
      return ITimeserieConstants.TYPE_WATERLEVEL;

    throw new SimulationException( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.50" ), null ); //$NON-NLS-1$
  }
}
