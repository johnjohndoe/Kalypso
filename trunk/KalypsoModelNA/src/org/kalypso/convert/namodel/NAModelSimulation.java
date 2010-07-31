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
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.LineNumberReader;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;
import java.net.MalformedURLException;
import java.net.URL;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.SortedMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.namespace.QName;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.java.io.FileCopyVisitor;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.lang.ProcessHelper;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.commons.lhwz.LhwzHelper;
import org.kalypso.contribs.java.io.filter.MultipleWildCardFileFilter;
import org.kalypso.contribs.java.net.UrlResolver;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.contribs.java.xml.XMLHelper;
import org.kalypso.convert.namodel.i18n.Messages;
import org.kalypso.convert.namodel.manager.IDManager;
import org.kalypso.convert.namodel.manager.LzsimManager;
import org.kalypso.convert.namodel.optimize.CalibarationConfig;
import org.kalypso.convert.namodel.optimize.NAOptimizingJob;
import org.kalypso.convert.namodel.timeseries.BlockTimeSeries;
import org.kalypso.convert.namodel.timeseries.NATimeSettings;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsosimulationmodel.ui.calccore.CalcCoreUtils;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IAxisRange;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.request.IRequest;
import org.kalypso.ogc.sensor.request.ObservationRequest;
import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.ogc.sensor.timeseries.envelope.TranProLinFilterUtilities;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.optimize.transform.OptimizeModelUtils;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.GMLWorkspace_Impl;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;
import org.w3c.dom.Document;
import org.xml.sax.InputSource;
import org.xml.sax.SAXParseException;

/**
 * @author Gernot Belger
 */
public class NAModelSimulation
{
  private static final String SUFFIX_QGS = "qgs";

  public static final String EXECUTABLES_FILE_TEMPLATE = "Kalypso-NA_%s.exe"; //$NON-NLS-1$

  public static final String EXECUTABLES_FILE_PATTERN = "Kalypso-NA_(.+)\\.exe"; //$NON-NLS-1$

  private static final String WE_RESOURCE_HYDROTOP_GML = "resources/WE/hydrotop.gml"; //$NON-NLS-1$

  private static final String WE_PARAMETER_GML = "resources/WE/parameter.gml"; //$NON-NLS-1$

  private static String[] CATCHMENT_FACTOR_PARAMETER_TARGET = { "retob", "retint", "aigw" }; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

  private final static String[][] CATCHMENT_FACTORS_PARAMETER = { new String[] { "retob", "faktorRetobRetint" }, new String[] { "retint", "faktorRetobRetint" }, new String[] { "aigw", "faktorAigw" } }; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$

  private final DateFormat START_DATE_FORMAT = new SimpleDateFormat( "yyyy-MM-dd(HH-mm-ss)" ); //$NON-NLS-1$

  private final String m_startDateText = START_DATE_FORMAT.format( new Date() );

  final NAStatistics m_naStatistics;

  private final ISimulationDataProvider m_inputProvider;

  private final Logger m_logger;

  private final NAConfiguration m_conf;

  private final IDManager m_idManager;

  private File m_kalypsoKernelPath = null;

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

  public void loadLogs( )
  {
    final NaFortranLogTranslater logTranslater = new NaFortranLogTranslater( m_simDirs.asciiDir, m_idManager, m_logger );

    final File resultFile = new File( m_simDirs.logDir, "error.gml" ); //$NON-NLS-1$
    resultFile.getParentFile().mkdirs();

    logTranslater.translate( resultFile );
  }

  public boolean runSimulation( final ISimulationMonitor monitor ) throws Exception
  {
    final File newModellFile = new File( m_simDirs.simulationDir, "namodellBerechnung.gml" ); //$NON-NLS-1$

    monitor.setMessage( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.15" ) ); //$NON-NLS-1$
    if( monitor.isCanceled() )
      return false;

    // Kopieren von Berechnungsstandardverzeichnis
    final URL asciiTemplateURL = getClass().getResource( "template/emptyAsciiTemplate.zip" ); //$NON-NLS-1$
    unzipInput( asciiTemplateURL, m_simDirs.asciiDir );

    if( m_inputProvider.hasID( NAOptimizingJob.IN_BestOptimizedRunDir_ID ) )
    {
      // while optimization, you can recycle files from a former run.
      // implement here to copy the files to your tmp dir and while generating
      // files you should check if files already exist, and on your option do
      // not generate them.
      // WARNING: never use result files or files that vary during
      // optimization.
      final URL url = (URL) m_inputProvider.getInputForID( NAOptimizingJob.IN_BestOptimizedRunDir_ID );
      final File from1 = new File( url.getFile(), "klima.dat" ); //$NON-NLS-1$
      final File to1 = new File( m_simDirs.asciiDir, "klima.dat" ); //$NON-NLS-1$
      if( from1.exists() && to1.exists() )
      {
        final FileCopyVisitor copyVisitor = new FileCopyVisitor( from1, to1, true );
        FileUtilities.accept( from1, copyVisitor, true );
      }
      final File from2 = new File( url.getFile(), "zufluss" ); //$NON-NLS-1$
      final File to2 = new File( m_simDirs.asciiDir, "zufluss" ); //$NON-NLS-1$
      if( from2.exists() && to2.exists() )
      {
        final FileCopyVisitor copyVisitor = new FileCopyVisitor( from2, to2, true );
        FileUtilities.accept( from2, copyVisitor, true );
      }

      final File from3 = new File( url.getFile(), "hydro.top" ); //$NON-NLS-1$
      final File to3 = new File( m_simDirs.asciiDir, "hydro.top" ); //$NON-NLS-1$
      if( from3.exists() && to3.exists() )
      {
        final FileCopyVisitor copyVisitor = new FileCopyVisitor( from3, to3, true );
        FileUtilities.accept( from3, copyVisitor, true );
      }
    }

    // generiere ascii-dateien
    monitor.setMessage( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.23" ) ); //$NON-NLS-1$
    if( monitor.isCanceled() )
      return false;

    // calualtion model
    final GMLWorkspace modellWorkspace = generateASCII( m_conf, m_simDirs, m_inputProvider, newModellFile, monitor );
    if( modellWorkspace == null )
      return false;

    final URL naControlURL = (URL) m_inputProvider.getInputForID( NaModelConstants.IN_CONTROL_ID );
    final GMLWorkspace naControlWorkspace = GmlSerializer.createGMLWorkspace( naControlURL, null );

    final URL iniValuesFolderURL = (URL) m_inputProvider.getInputForID( NaModelConstants.LZSIM_IN_ID );

    if( iniValuesFolderURL != null )
    {
      // TODO: crude way to create the new URL, necessary as probably we do not have a '/' at the end of the path
      final URL lzsimURL = new URL( iniValuesFolderURL.toExternalForm() + "/lzsim.gml" ); //$NON-NLS-1$
      try
      {
        final GMLWorkspace iniValuesWorkspace = GmlSerializer.createGMLWorkspace( lzsimURL, null );
        LzsimManager.writeLzsimFiles( m_idManager, m_simDirs.lzsimDir, iniValuesWorkspace );
      }
      // We still assume it is a file.... ignore file not found, we do not have starting conditions then
      catch( final FileNotFoundException e )
      {
        m_logger.log( Level.INFO, Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.26" ), m_conf.getSimulationStart().toString() ); //$NON-NLS-1$
      }
    }

    if( monitor.isCanceled() )
      return false;
    // kopiere executable aus resourcen:
    copyExecutable();

    // starte berechnung
    monitor.setMessage( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.27" ) ); //$NON-NLS-1$
    if( monitor.isCanceled() )
      return false;
    startCalculation( monitor );
    final boolean succeeded = checkSucceeded();
    if( succeeded )
    {
      monitor.setMessage( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.28" ) ); //$NON-NLS-1$
      m_logger.log( Level.FINEST, Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.29" ) ); //$NON-NLS-1$
      loadResults( modellWorkspace, naControlWorkspace, m_logger, m_simDirs.resultDir, m_conf );
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

  private static void unzipInput( final URL asciiZipURL, final File exeDir )
  {
    try
    {
      ZipUtilities.unzip( asciiZipURL, exeDir );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
    }
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

  private GMLWorkspace generateASCII( final NAConfiguration conf, final NaSimulationDirs simDirs, final ISimulationDataProvider dataProvider, final File newModellFile, final ISimulationMonitor monitor ) throws Exception
  {
    final GMLWorkspace metaWorkspace = GmlSerializer.createGMLWorkspace( (URL) dataProvider.getInputForID( NaModelConstants.IN_META_ID ), null );
    final Feature metaFE = metaWorkspace.getRootFeature();
    final GMLWorkspace controlWorkspace = GmlSerializer.createGMLWorkspace( (URL) dataProvider.getInputForID( NaModelConstants.IN_CONTROL_ID ), null );

    // model Parameter
    final GMLWorkspace parameterWorkspace;
    if( dataProvider.hasID( NaModelConstants.IN_PARAMETER_ID ) )
      parameterWorkspace = GmlSerializer.createGMLWorkspace( (URL) dataProvider.getInputForID( NaModelConstants.IN_PARAMETER_ID ), null );
    else
      parameterWorkspace = GmlSerializer.createGMLWorkspace( getClass().getResource( WE_PARAMETER_GML ), null );

// GMLWorkspace landuseWorkspace = null;
// if( dataProvider.hasID( NaModelConstants.IN_LANDUSE_ID ) )
// {
// final URL url = (URL) dataProvider.getInputForID( NaModelConstants.IN_LANDUSE_ID );
// final File f = new File( url.toURI().getPath() );
// if( f.exists() )
// landuseWorkspace = GmlSerializer.createGMLWorkspace( url, null );
// }

    GMLWorkspace sudsWorkspace = null;
    if( dataProvider.hasID( NaModelConstants.IN_SUDS_ID ) )
    {
      final URL url = (URL) dataProvider.getInputForID( NaModelConstants.IN_SUDS_ID );
      final File f = new File( url.toURI().getPath() );
      if( f.exists() )
        sudsWorkspace = GmlSerializer.createGMLWorkspace( url, null );
    }

    // initialize model with values of control file
    initializeModell( controlWorkspace.getRootFeature(), (URL) dataProvider.getInputForID( NaModelConstants.IN_MODELL_ID ), newModellFile );

    final GMLWorkspace modellWorkspace = GmlSerializer.createGMLWorkspace( newModellFile.toURI().toURL(), null );
    // final GMLWorkspace modellWorkspace = GmlSerializer.createGMLWorkspace( newModellFile.toURL() );
    ((GMLWorkspace_Impl) modellWorkspace).setContext( (URL) dataProvider.getInputForID( NaModelConstants.IN_MODELL_ID ) );

    final NaNodeResultProvider nodeResultProvider = new NaNodeResultProvider( modellWorkspace, controlWorkspace, conf.getZMLContext() );
    conf.setNodeResultProvider( nodeResultProvider );
    updateModelWithExtraVChannel( modellWorkspace, nodeResultProvider );

    // model Parameter
    final GMLWorkspace synthNWorkspace;
    final File synthNGML = new File( ((URL) dataProvider.getInputForID( NaModelConstants.IN_RAINFALL_ID )).getFile(), "calcSynthN.gml" ); //$NON-NLS-1$
    if( synthNGML.exists() )
      synthNWorkspace = GmlSerializer.createGMLWorkspace( synthNGML.toURI().toURL(), null );
    else
      synthNWorkspace = null;

    // model Hydrotop
    final GMLWorkspace hydrotopWorkspace;

    if( dataProvider.hasID( NaModelConstants.IN_HYDROTOP_ID ) )
    {
      hydrotopWorkspace = GmlSerializer.createGMLWorkspace( (URL) dataProvider.getInputForID( NaModelConstants.IN_HYDROTOP_ID ), null );
    }
    else
    {
      // TODO: remove this special case: move hydrotop.gml into WE model
      hydrotopWorkspace = GmlSerializer.createGMLWorkspace( getClass().getResource( WE_RESOURCE_HYDROTOP_GML ), null );
    }

    final Feature[] hydroFES = hydrotopWorkspace.getFeatures( hydrotopWorkspace.getGMLSchema().getFeatureType( NaModelConstants.HYDRO_ELEMENT_FT ) );
    String targetCS = null;
    for( int i = 0; i < hydroFES.length && targetCS == null; i++ )
    {
      final GM_Object geom = (GM_Object) hydroFES[i].getProperty( NaModelConstants.HYDRO_PROP_GEOM );
      if( geom != null && geom.getCoordinateSystem() != null )
        targetCS = geom.getCoordinateSystem();
    }
    if( targetCS != null )
    {
      final TransformVisitor visitor = new TransformVisitor( targetCS );
      modellWorkspace.accept( visitor, "/", FeatureVisitor.DEPTH_INFINITE ); //$NON-NLS-1$
    }

    // setting duration of simulation...
    // start
    final Date start = DateUtilities.toDate( (XMLGregorianCalendar) metaFE.getProperty( NaModelConstants.CONTROL_STARTSIMULATION ) );
    conf.setSimulationStart( start );
    // start forecast

    final Date startForecastDate = DateUtilities.toDate( (XMLGregorianCalendar) metaFE.getProperty( NaModelConstants.CONTROL_FORECAST ) );
    conf.setSimulationForecasetStart( startForecastDate );
    // end of simulation
    int hoursForecast = 0; // default length of forecast hours
    final Integer hoursOfForecast = (Integer) metaFE.getProperty( NaModelConstants.CONTROL_HOURS_FORECAST_PROP );
    if( hoursOfForecast != null )
      hoursForecast = hoursOfForecast.intValue();
    final Calendar c = Calendar.getInstance();
    c.setTime( startForecastDate );
    c.add( Calendar.HOUR, hoursForecast );
    final Date endDate = c.getTime();
    conf.setSimulationEnd( endDate );

    // calculate timestep
    final Integer minutesOfTimeStep = (Integer) metaFE.getProperty( NaModelConstants.CONTROL_MINUTES_TIMESTEP_PROP );
    final int minutesTimeStep;
    if( minutesOfTimeStep != null && minutesOfTimeStep.intValue() != 0 )
      minutesTimeStep = minutesOfTimeStep.intValue();
    else
      minutesTimeStep = 60;

    conf.setMinutesOfTimeStep( minutesTimeStep );

    // choose simulation kernel
    m_kalypsoKernelPath = chooseSimulationExe( (String) metaFE.getProperty( NaModelConstants.CONTROL_VERSION_KALYPSONA_PROP ), monitor );
    if( m_kalypsoKernelPath == null )
      return null;

    // choose precipitation form and parameters
    final Boolean pns = (Boolean) metaFE.getProperty( NaModelConstants.CONTROL_PNS_PROP );
    conf.setUsePrecipitationForm( pns == null ? false : pns );
    if( conf.isUsePrecipitationForm().equals( true ) )
    {
      // the GUI asks for return period [a] - the fortran kernal needs annuality [1/a]
      conf.setAnnuality( 1d / (Double) metaFE.getProperty( NaModelConstants.CONTROL_XJAH_PROP ) );
      final Double durationMinutes = (Double) metaFE.getProperty( NaModelConstants.CONTROL_XWAHL2_PROP );
      final Double durationHours = durationMinutes / 60d;
      conf.setDuration( durationHours );
      conf.setForm( (String) metaFE.getProperty( NaModelConstants.CONTROL_IPVER_PROP ) );
    }

    // set rootnode
    conf.setRootNodeID( (String) controlWorkspace.getRootFeature().getProperty( NaModelConstants.NACONTROL_ROOTNODE_PROP ) );

    // generate modell files
    conf.setModelWorkspace( modellWorkspace );
    conf.setParameterWorkspace( parameterWorkspace );
    conf.setHydrotopeWorkspace( hydrotopWorkspace );
    conf.setSynthNWorkspace( synthNWorkspace );
    conf.setSudsWorkspace( sudsWorkspace );

    // generate control files
    NAControlConverter.featureToASCII( conf, simDirs.startDir, controlWorkspace, modellWorkspace );

    // update model with factor values from control
    updateFactorParameter( modellWorkspace );

    final NAModellConverter main = new NAModellConverter( conf, m_logger );
    main.write();

    // dump idmapping to file
    final IDManager idManager = conf.getIdManager();
    Writer idWriter = null;
    try
    {
      idWriter = new FileWriter( new File( simDirs.simulationDir, "IdMap.txt" ) ); //$NON-NLS-1$
      idManager.dump( idWriter );
    }
    finally
    {
      IOUtils.closeQuietly( idWriter );
    }
    return modellWorkspace;
  }

  private void copyExecutable( ) throws Exception
  {
    final File destFile = new File( m_simDirs.startDir, m_kalypsoKernelPath.getName() );
    if( !destFile.exists() )
      FileUtils.copyFile( m_kalypsoKernelPath, destFile );

    // TODO: do not commit...
// URL defaultZfl = getClass().getResource( m_resourceBase + "inp.dat/we999-weisseelster.zfl" );
// FileUtils.copyURLToFile( defaultZfl, new File( basedir, "inp.dat/we999.zfl" ) );
//
// URL defaultVer = getClass().getResource( m_resourceBase + "klima.dat/std.ver" );
// FileUtils.copyURLToFile( defaultVer, new File( basedir, "klima.dat/std.ver" ) );
//
// URL defaultTmp = getClass().getResource( m_resourceBase + "klima.dat/std.tmp" );
// FileUtils.copyURLToFile( defaultTmp, new File( basedir, "klima.dat/std.tmp" ) );
  }

  private void startCalculation( final ISimulationMonitor monitor ) throws SimulationException
  {
    final File exeFile = new File( m_simDirs.startDir, m_kalypsoKernelPath.getName() );
    final String commandString = exeFile.getAbsolutePath();

    final long timeOut = 0l; // no timeout control

    FileOutputStream logOS = null;
    FileOutputStream errorOS = null;
    try
    {
      logOS = new FileOutputStream( new File( m_simDirs.asciiDir, "exe.log" ) ); //$NON-NLS-1$
      errorOS = new FileOutputStream( new File( m_simDirs.asciiDir, "exe.err" ) ); //$NON-NLS-1$
      ProcessHelper.startProcess( commandString, new String[0], m_simDirs.startDir, monitor, timeOut, logOS, errorOS, null );
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
      final File logFile = new File( m_simDirs.startDir, "output.res" ); //$NON-NLS-1$
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

  private void loadResults( final GMLWorkspace modellWorkspace, final GMLWorkspace naControlWorkspace, final Logger logger, final File resultDir, final NAConfiguration conf ) throws Exception
  {
    loadTSResults( modellWorkspace, conf );
    try
    {
      loadTesultTSPredictionIntervals( naControlWorkspace, logger, resultDir, conf );
    }
    catch( final Exception e )
    {
      logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.83", e.getLocalizedMessage() ) ); //$NON-NLS-1$
    }
    loadTextFileResults();

    final Date[] initialDates = conf.getInitialDates();
    final LzsimManager lzsimManager = new LzsimManager( initialDates, m_simDirs.anfangswertDir );
    lzsimManager.readInitialValues( conf.getIdManager(), m_simDirs.lzsimDir, logger );
  }

  private void loadTSResults( final GMLWorkspace modellWorkspace, final NAConfiguration conf ) throws Exception
  {
    // j Gesamtabfluss Knoten .qgs
    final IFeatureType nodeFT = modellWorkspace.getGMLSchema().getFeatureType( NaModelConstants.NODE_ELEMENT_FT );
    loadTSResults( SUFFIX_QGS, nodeFT, TimeserieConstants.TYPE_RUNOFF, "pegelZR", "qberechnetZR", modellWorkspace, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

    final IFeatureType catchmentFT = modellWorkspace.getGMLSchema().getFeatureType( NaModelConstants.CATCHMENT_ELEMENT_FT );
    final IFeatureType rhbChannelFT = modellWorkspace.getGMLSchema().getFeatureType( NaModelConstants.STORAGE_CHANNEL_ELEMENT_FT );
    // j Niederschlag .pre
    loadTSResults( "pre", catchmentFT, TimeserieConstants.TYPE_RAINFALL, null, null, modellWorkspace, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // j Temperatur .tmp
    loadTSResults( "tmp", catchmentFT, TimeserieConstants.TYPE_TEMPERATURE, null, null, modellWorkspace, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // n Interflow .qif
    loadTSResults( "qif", catchmentFT, TimeserieConstants.TYPE_RUNOFF, null, null, modellWorkspace, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // n Grundwasser .qgw
    loadTSResults( "qgw", catchmentFT, TimeserieConstants.TYPE_RUNOFF, null, null, modellWorkspace, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // n Gesamtabfluss TG .qgg
    loadTSResults( "qgg", catchmentFT, TimeserieConstants.TYPE_RUNOFF, null, null, modellWorkspace, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // n Grundwasserstand .gws - Umrechnung von m auf cm
    loadTSResults( "gws", catchmentFT, TimeserieConstants.TYPE_WATERLEVEL, null, null, modellWorkspace, 100.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // n Basisabfluss .qbs
    loadTSResults( "qbs", catchmentFT, TimeserieConstants.TYPE_RUNOFF, null, null, modellWorkspace, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // n Oberflaechenabfluss .qna
    loadTSResults( "qna", catchmentFT, TimeserieConstants.TYPE_RUNOFF, null, null, modellWorkspace, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // n Abfluss vers. Flaechen .qvs
    loadTSResults( "qvs", catchmentFT, TimeserieConstants.TYPE_RUNOFF, null, null, modellWorkspace, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // TODO:check output for the next time series
    // n Schnee .sch [mm]
    loadTSResults( "sch", catchmentFT, TimeserieConstants.TYPE_WATERLEVEL, null, null, modellWorkspace, 0.1d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // n Kluftgrundw1 .qt1
    loadTSResults( "qt1", catchmentFT, TimeserieConstants.TYPE_RUNOFF, null, null, modellWorkspace, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // n Kluftgrundw .qtg
    loadTSResults( "qtg", catchmentFT, TimeserieConstants.TYPE_RUNOFF, null, null, modellWorkspace, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // n Evapotranspiration .vet [mm]
    loadTSResults( "vet", catchmentFT, TimeserieConstants.TYPE_EVAPORATION, null, null, modellWorkspace, 0.1d, conf ); //$NON-NLS-1$ //$NON-NLS-2$

    // TODO: Zeitreihe als mittlere Bodenfeuchte aus Fortran übernehmen, daher bisher nicht zu übertragen (Zur zeit wird
    // die Bodenfeuchte des ersten Hydrotopes in allen Schichten ausgegeben)
    // j Bodenfeuchte .bof [mm]
    // loadTSResults( "bof", catchmentFT, "name", TimeserieConstants.TYPE_WATERLEVEL, null, null, inputDir,
    // modellWorkspace, logger, outputDir, 0.1d, conf );

    // Straenge
    // n Wasserstand Speicher .sph [muNN]
    loadTSResults( "sph", rhbChannelFT, TimeserieConstants.TYPE_NORMNULL, null, null, modellWorkspace, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // n Speicherueberlauf .sub [m³/s]
    loadTSResults( "sub", rhbChannelFT, TimeserieConstants.TYPE_RUNOFF, null, null, modellWorkspace, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // n Speicherinhalt .spi [hm³] - Umrechnung auf m³
    loadTSResults( "spi", rhbChannelFT, TimeserieConstants.TYPE_VOLUME, null, null, modellWorkspace, 1000000.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
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
    final File[] qgsFiles = m_simDirs.outWeNatDir.listFiles( filter );
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

        final IAxis dateAxis = new DefaultAxis( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.4" ), TimeserieConstants.TYPE_DATE, "", Date.class, true ); //$NON-NLS-1$ //$NON-NLS-2$
        final IAxis qAxis = new DefaultAxis( axisTitle, resultType, TimeserieUtils.getUnit( resultType ), Double.class, false );
        final IAxis statusAxis = KalypsoStatusUtils.createStatusAxisFor( qAxis, true );
        final IAxis[] axis = new IAxis[] { dateAxis, qAxis, statusAxis };
        final ITuppleModel qTuppelModel = new SimpleTuppleModel( axis, tupelData );

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
            final IObservation pegelObservation = ZmlFactory.parseXML( pegelURL, "pegelmessung" ); //$NON-NLS-1$

            copyMetaData( pegelObservation.getMetadataList(), metadataList, new String[] { TimeserieConstants.MD_ALARM_1, TimeserieConstants.MD_ALARM_2, TimeserieConstants.MD_ALARM_3,
              TimeserieConstants.MD_ALARM_4, TimeserieConstants.MD_GEWAESSER, TimeserieConstants.MD_FLUSSGEBIET, TimeserieConstants.MD_GKH, TimeserieConstants.MD_GKR,
              TimeserieConstants.MD_HOEHENANGABEART, TimeserieConstants.MD_PEGELNULLPUNKT, TimeserieConstants.MD_WQWECHMANN, TimeserieConstants.MD_WQTABLE, TimeserieConstants.MD_TIMEZONE,
              TimeserieConstants.MD_VORHERSAGE_START, TimeserieConstants.MD_VORHERSAGE_ENDE } );

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

  private void initializeModell( final Feature controlFeature, final URL inputModellURL, final File outputModelFile ) throws Exception
  {
    final CalibarationConfig config = new CalibarationConfig();
    config.addFromNAControl( controlFeature );

    Document modelDoc = null;
    try
    {
      modelDoc = XMLHelper.getAsDOM( new InputSource( inputModellURL.openStream() ), true );
    }

    catch( final SAXParseException e )
    {
      e.printStackTrace();
      final int lineNumber = e.getLineNumber();
      System.out.println( e.getLocalizedMessage() + " #" + lineNumber ); //$NON-NLS-1$
    }
    catch( final Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();

      throw e;
    }

    OptimizeModelUtils.initializeModel( modelDoc, config.getCalContexts() );

    // TODO: take charset from Document
    final String charset = "UTF-8"; //$NON-NLS-1$
    final Writer writer = new OutputStreamWriter( new FileOutputStream( outputModelFile ), charset );
    final Transformer t = TransformerFactory.newInstance().newTransformer();
    t.transform( new DOMSource( modelDoc ), new StreamResult( writer ) );
    writer.close();
  }

  /**
   * update workspace and do some tricks in order to fix some things the fortran-kernel can not handle for now
   * 
   * @param workspace
   * @throws Exception
   */
  private void updateModelWithExtraVChannel( final GMLWorkspace workspace, final NaNodeResultProvider nodeResultProvider ) throws Exception
  {
    updateGWNet( workspace );
    updateNode2NodeNet( workspace );
    updateZuflussNet( workspace );
    updateResultAsZuflussNet( workspace, nodeResultProvider );
  }

  /**
   * before: <br>
   * <code>
   * 
   * Node1 O <---  O Node2
   * 
   * </code> after: <br>
   * <code>
   * 
   * Node1 O <--- newVChannel <-- newNode O <-- newVChannel
   *                                      A
   *                                      |
   *                                      O-- Node2
   * 
   * </code>
   * 
   * @param workspace
   * @throws Exception
   */
  private void updateNode2NodeNet( final GMLWorkspace workspace ) throws Exception
  {
    final IFeatureType kontEntnahmeFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.NODE_VERZW_ENTNAHME );
    final IFeatureType ueberlaufFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.NODE_VERZW_UEBERLAUF );
    final IFeatureType verzweigungFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.NODE_VERZW_VERZWEIGUNG );
    final IFeatureType nodeFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.NODE_ELEMENT_FT );
    final Feature[] features = workspace.getFeatures( nodeFT );
    final IRelationType branchingMemberRT = (IRelationType) nodeFT.getProperty( NaModelConstants.NODE_BRANCHING_MEMBER_PROP );
    for( final Feature nodeFE : features )
    {
      final Feature branchingFE = workspace.resolveLink( nodeFE, branchingMemberRT );
      if( branchingFE != null )
      {
        final IFeatureType branchFT = branchingFE.getFeatureType();
        final IRelationType branchingNodeMemberRT = (IRelationType) branchFT.getProperty( NaModelConstants.NODE_BRANCHING_NODE_MEMBER_PROP );
        if( branchFT == kontEntnahmeFT || branchFT == ueberlaufFT || branchFT == verzweigungFT )
        {
          final Feature targetNodeFE = workspace.resolveLink( branchingFE, branchingNodeMemberRT );
          if( targetNodeFE == null )
          {
            final String relationLabel = branchingNodeMemberRT.getAnnotation().getLabel();
            final String branchingFElabel = FeatureHelper.getAnnotationValue( branchingFE, IAnnotation.ANNO_LABEL );
            final String message = String.format( "'%s' not set for '%s' in Node '%s'", relationLabel, branchingFElabel, nodeFE.getName() );
            throw new SimulationException( message );
          }

          final Feature newNodeFE = buildVChannelNet( workspace, targetNodeFE );
          workspace.setFeatureAsComposition( branchingFE, branchingNodeMemberRT, newNodeFE, true );
        }
      }
    }
  }

  /**
   * TODO scetch<br>
   * if results exists (from a former simulation) for a node, use this results as input, later the upstream nodes will
   * be ignored for calculation
   * 
   * @param workspace
   * @throws Exception
   */
  private void updateResultAsZuflussNet( final GMLWorkspace workspace, final NaNodeResultProvider nodeResultprovider ) throws Exception
  {
    final IFeatureType nodeFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.NODE_ELEMENT_FT );
    final IFeatureType abstractChannelFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.CHANNEL_ABSTRACT_FT );
    final Feature[] features = workspace.getFeatures( nodeFT );
    for( final Feature nodeFE : features )
    {
      if( nodeResultprovider.resultExists( nodeFE ) )
      {
        final Object resultValue = nodeFE.getProperty( NaModelConstants.NODE_RESULT_TIMESERIESLINK_PROP );
        // disconnect everything upstream (channel -> node)
        final IRelationType downStreamNodeMemberRT = (IRelationType) abstractChannelFT.getProperty( NaModelConstants.LINK_CHANNEL_DOWNSTREAMNODE );
        final Feature[] channelFEs = workspace.resolveWhoLinksTo( nodeFE, abstractChannelFT, downStreamNodeMemberRT );
        for( final Feature element : channelFEs )
        {
          final Feature newEndNodeFE = workspace.createFeature( element, downStreamNodeMemberRT, nodeFT );
          workspace.setFeatureAsComposition( element, downStreamNodeMemberRT, newEndNodeFE, true );
        }
        // add as zufluss
        final Feature newNodeFE = buildVChannelNet( workspace, nodeFE );
        // final FeatureProperty zuflussProp = FeatureFactory.createFeatureProperty( "zuflussZR", resultValue );
        newNodeFE.setProperty( NaModelConstants.NODE_ZUFLUSS_ZR_PROP, resultValue );
        newNodeFE.setProperty( NaModelConstants.NODE_SYNTHETIC_ZUFLUSS_ZR_PROP, nodeFE.getProperty( NaModelConstants.NODE_SYNTHETIC_ZUFLUSS_ZR_PROP ) );
      }
    }
  }

  /**
   * put one more VChannel to each Q-source, so that this discharge will appear in the result of the connected node <br>
   * zml inflow <br>
   * before: <br>
   * <code>
   * |Channel| <- o(1) <- input.zml <br>
   * </code><br>
   * now: <br>
   * <code>
   * |Channel| <- o(1) <- |VChannel (new)| <- o(new) <- |VChannel (new)|
   *                                          A- input.zml <br>
   * </code> constant inflow <br>
   * before: <br>
   * <code>
   * |Channel| <- o(1) <- Q(constant) <br>
   * </code><br>
   * now: <br>
   * <code>
   * |Channel| <- o(1) <- |VChannel (new)| <- o(new) <- |VChannel (new)|
   *                                          A- Q(constant)<br>
   * </code>
   * 
   * @param workspace
   * @throws Exception
   */
  private void updateZuflussNet( final GMLWorkspace workspace ) throws Exception
  {

    final IFeatureType kontZuflussFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.NODE_VERZW_ZUFLUSS );
    final IFeatureType nodeFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.NODE_ELEMENT_FT );
    final Feature[] features = workspace.getFeatures( nodeFT );
    final IRelationType branchingMemberRT = (IRelationType) nodeFT.getProperty( NaModelConstants.NODE_BRANCHING_MEMBER_PROP );
    for( final Feature nodeFE : features )
    {
      final Object zuflussValue = nodeFE.getProperty( NaModelConstants.NODE_ZUFLUSS_ZR_PROP );
      if( zuflussValue != null )
      {
        // update zufluss
        final Feature newNode = buildVChannelNet( workspace, nodeFE );
        // nove zufluss-property to new node
        // nodeFE.setProperty( FeatureFactory.createFeatureProperty( "zuflussZR", null ) );
        // newNode.setProperty( FeatureFactory.createFeatureProperty( "zuflussZR", zuflussValue ) );
        nodeFE.setProperty( NaModelConstants.NODE_ZUFLUSS_ZR_PROP, null );
        newNode.setProperty( NaModelConstants.NODE_ZUFLUSS_ZR_PROP, zuflussValue );
        newNode.setProperty( NaModelConstants.NODE_SYNTHETIC_ZUFLUSS_ZR_PROP, nodeFE.getProperty( NaModelConstants.NODE_SYNTHETIC_ZUFLUSS_ZR_PROP ) );
      }
      final Feature branchingFE = workspace.resolveLink( nodeFE, branchingMemberRT );
      if( branchingFE != null && branchingFE.getFeatureType() == kontZuflussFT )
      {
        // update zufluss
        final Feature newNode = buildVChannelNet( workspace, nodeFE );
        // nove constant-inflow to new node
        workspace.setFeatureAsComposition( nodeFE, branchingMemberRT, null, true );
        workspace.setFeatureAsComposition( newNode, branchingMemberRT, branchingFE, true );
      }
    }
  }

  /**
   * @param kalypsoNAVersion
   *          name/version of simulation kernel
   */
  private File chooseSimulationExe( final String kalypsoNAVersion, final ISimulationMonitor monitor )
  {
    try
    {
      return CalcCoreUtils.findExecutable( kalypsoNAVersion, EXECUTABLES_FILE_TEMPLATE, EXECUTABLES_FILE_PATTERN, CalcCoreUtils.COMPATIBILITY_MODE.NA );
    }
    catch( final CoreException e )
    {
      final IStatus status = e.getStatus();
      monitor.setFinishInfo( status.getSeverity(), status.getMessage() );
      return null;
    }
  }

  /**
   * some parameter have factors that must be processed before generating asciifiles, as these factors do not occur in
   * ascci-format
   * 
   * @param modellWorkspace
   */
  private void updateFactorParameter( final GMLWorkspace modellWorkspace )
  {
    // Catchments
    final Feature[] catchmentFEs = modellWorkspace.getFeatures( modellWorkspace.getGMLSchema().getFeatureType( NaModelConstants.CATCHMENT_ELEMENT_FT ) );
    update( catchmentFEs, CATCHMENT_FACTOR_PARAMETER_TARGET, CATCHMENT_FACTORS_PARAMETER );

    // KMChannels
    final Feature[] kmChanneFEs = modellWorkspace.getFeatures( modellWorkspace.getGMLSchema().getFeatureType( NaModelConstants.KM_CHANNEL_ELEMENT_FT ) );
    for( final Feature feature : kmChanneFEs )
    {
      final double rkfFactor = FeatureHelper.getAsDouble( feature, NaModelConstants.KM_CHANNEL_FAKTOR_RKF_PROP, 1.0 );
      final double rnfFactor = FeatureHelper.getAsDouble( feature, NaModelConstants.KM_CHANNEL_FAKTOR_RNF_PROP, 1.0 );
      final List< ? > kmParameter = (List< ? >) feature.getProperty( NaModelConstants.KM_CHANNEL_PARAMETER_MEMBER );
      final Iterator< ? > iterator = kmParameter.iterator();
      while( iterator.hasNext() )
      {
        final Feature kmParameterFE = (Feature) iterator.next();
        // rnf
        final double _rnf = rnfFactor * FeatureHelper.getAsDouble( kmParameterFE, NaModelConstants.KM_CHANNEL_RNF_PROP, 1.0 );
        // FeatureProperty rnfProp = FeatureFactory.createFeatureProperty( "rnf", new Double( _rnf ) );
        kmParameterFE.setProperty( NaModelConstants.KM_CHANNEL_RNF_PROP, new Double( _rnf ) );
        // rkf
        final double _rkf = rkfFactor * FeatureHelper.getAsDouble( kmParameterFE, NaModelConstants.KM_CHANNEL_RKF_PROP, 1.0 );
        // FeatureProperty rkfProp = FeatureFactory.createFeatureProperty( "rkf", new Double( _rkf ) );
        kmParameterFE.setProperty( NaModelConstants.KM_CHANNEL_RKF_PROP, new Double( _rkf ) );
      }
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
    final ITuppleModel resultValues = resultObservation.getValues( null );
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
      final IObservation pegelObservation = ZmlFactory.parseXML( pegelURL, "pegelmessung" ); //$NON-NLS-1$
      final ITuppleModel pegelValues = pegelObservation.getValues( null );
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
    final IObservation adaptedResultObservation = ZmlFactory.parseXML( fileMitte.toURI().toURL(), "adaptedVorhersage" ); //$NON-NLS-1$

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

    final File[] qgsFiles = m_simDirs.outWeNatDir.listFiles( filter );
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
          final String inputPath = m_simDirs.outWeNatDir.getName() + element.getName();
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

  /**
   * updates workspace, so that interflow and channelflow dependencies gets optimized <br>
   * groundwater flow can now run in opposite direction to channel flow
   * 
   * @param workspace
   */
  private void updateGWNet( final GMLWorkspace workspace )
  {
    final IFeatureType catchmentFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.CATCHMENT_ELEMENT_FT );
    final IFeatureType vChannelFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.V_CHANNEL_ELEMENT_FT );
    final Feature[] features = workspace.getFeatures( catchmentFT );
    final IRelationType entwaesserungsStrangMemberRT = (IRelationType) catchmentFT.getProperty( NaModelConstants.LINK_CATCHMENT_CHANNEL );
    for( final Feature catchmentFE : features )
    {
      final Feature orgChannelFE = workspace.resolveLink( catchmentFE, entwaesserungsStrangMemberRT );
      if( orgChannelFE == null )
        continue;
      final IRelationType downStreamNodeMemberRT = (IRelationType) vChannelFT.getProperty( NaModelConstants.LINK_CHANNEL_DOWNSTREAMNODE );
      final Feature nodeFE = workspace.resolveLink( orgChannelFE, downStreamNodeMemberRT );
      final Feature newChannelFE = workspace.createFeature( catchmentFE, entwaesserungsStrangMemberRT, vChannelFT );
      // set new relation: catchment -> new V-channel
      try
      {
        workspace.setFeatureAsComposition( catchmentFE, entwaesserungsStrangMemberRT, newChannelFE, true );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
      // set new relation: new V-channel -> downstream node
      try
      {
        final IRelationType downStreamNodeMemberRT2 = (IRelationType) newChannelFE.getFeatureType().getProperty( NaModelConstants.LINK_CHANNEL_DOWNSTREAMNODE );
        workspace.addFeatureAsAggregation( newChannelFE, downStreamNodeMemberRT2, 1, nodeFE.getId() );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
    }
  }

  /**
   * before: <code>
   * 
   *     o(existing)
   * 
   * </code> after: <code>
   * 
   *  |new Channel3|
   *     |
   *     V
   *     o(new Node2)  (return value)
   *     |
   *     V
   *  |new Channel1|
   *     |
   *     V
   *     o(existing)
   * 
   * </code>
   */
  private Feature buildVChannelNet( final GMLWorkspace workspace, final Feature existingNode ) throws Exception
  {
    final IFeatureType nodeColFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.NODE_COLLECTION_FT );
    final IFeatureType nodeFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.NODE_ELEMENT_FT );
    final IRelationType nodeMemberRT = (IRelationType) nodeColFT.getProperty( NaModelConstants.NODE_MEMBER_PROP );
    final IFeatureType vChannelFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.V_CHANNEL_ELEMENT_FT );

    final IFeatureType channelColFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.NA_CHANNEL_COLLECTION_FT );
    final IRelationType channelMemberRT = (IRelationType) channelColFT.getProperty( NaModelConstants.CHANNEL_MEMBER_PROP );
    final Feature channelColFE = workspace.getFeatures( channelColFT )[0];
    final Feature nodeColFE = workspace.getFeatures( workspace.getGMLSchema().getFeatureType( NaModelConstants.NODE_COLLECTION_FT ) )[0];

    // add to collections:
    final Feature newChannelFE1 = workspace.createFeature( channelColFE, channelMemberRT, vChannelFT );
    workspace.addFeatureAsComposition( channelColFE, channelMemberRT, 0, newChannelFE1 );
    final Feature newChannelFE3 = workspace.createFeature( channelColFE, channelMemberRT, vChannelFT );
    workspace.addFeatureAsComposition( channelColFE, channelMemberRT, 0, newChannelFE3 );
    final Feature newNodeFE2 = workspace.createFeature( nodeColFE, nodeMemberRT, nodeFT );
    workspace.addFeatureAsComposition( nodeColFE, nodeMemberRT, 0, newNodeFE2 );
    final IRelationType downStreamNodeMemberRT = (IRelationType) vChannelFT.getProperty( NaModelConstants.LINK_CHANNEL_DOWNSTREAMNODE );

    // 3 -> 2
    workspace.setFeatureAsAggregation( newChannelFE3, downStreamNodeMemberRT, newNodeFE2.getId(), true );
    // 2 -> 1
    final IRelationType downStreamChannelMemberRT = (IRelationType) nodeFT.getProperty( NaModelConstants.LINK_NODE_DOWNSTREAMCHANNEL );
    workspace.setFeatureAsAggregation( newNodeFE2, downStreamChannelMemberRT, newChannelFE1.getId(), true );
    // 1 -> existing

    // final IRelationType downStreamNodeMemberRT1 = (IRelationType) vChannelFT.getProperty( "downStreamNodeMember" );
    workspace.setFeatureAsAggregation( newChannelFE1, downStreamNodeMemberRT, existingNode.getId(), true );
    return newNodeFE2;
  }

  private void update( final Feature[] features, final String[] targetPropNames, final String[][] factorPropNames )
  {
    for( final Feature feature : features )
    {
      for( int _p = 0; _p < targetPropNames.length; _p++ ) // iterate parameters
      {
        final String[] factors = factorPropNames[_p];
        double value = 1.0; // initial value
        for( final String element : factors )
          // iterate factors
          value *= FeatureHelper.getAsDouble( feature, element, 1.0 );
        // set parameter
        final String targetPropName = targetPropNames[_p];
        // FeatureProperty valueProp = FeatureFactory.createFeatureProperty( targetPropName, new Double( value ) );
        feature.setProperty( targetPropName, new Double( value ) );
      }
    }
  }

  private IObservation loadPredictedResult( final File resultDir, final Feature rootFeature ) throws MalformedURLException, SensorException
  {
    final TimeseriesLinkType resultLink = (TimeseriesLinkType) rootFeature.getProperty( NaModelConstants.NACONTROL_RESULT_TIMESERIESLINK_PROP );

    // from predicted timeseries
    final UrlResolver urlResolver = new UrlResolver();
    final URL resultURL = urlResolver.resolveURL( resultDir.toURI().toURL(), resultLink.getHref() );
    return ZmlFactory.parseXML( resultURL, "vorhersage" ); //$NON-NLS-1$
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

    if( ObservationUtilities.hasAxisOfType( axisList, TimeserieConstants.TYPE_RUNOFF ) )
      return TimeserieConstants.TYPE_RUNOFF;

    if( ObservationUtilities.hasAxisOfType( axisList, TimeserieConstants.TYPE_WATERLEVEL ) )
      return TimeserieConstants.TYPE_WATERLEVEL;

    throw new SimulationException( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.50" ), null ); //$NON-NLS-1$
  }
}
