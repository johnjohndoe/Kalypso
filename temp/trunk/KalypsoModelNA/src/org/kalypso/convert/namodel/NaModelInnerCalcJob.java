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
package org.kalypso.convert.namodel;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.Writer;
import java.net.URL;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.logging.Formatter;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;
import java.util.logging.StreamHandler;

import javax.xml.bind.Marshaller;
import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.io.CopyUtils;
import org.apache.commons.io.IOUtils;
import org.kalypso.commons.java.io.FileCopyVisitor;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.lang.ProcessHelper;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.java.io.filter.MultipleWildCardFileFilter;
import org.kalypso.contribs.java.net.UrlResolver;
import org.kalypso.contribs.java.net.UrlUtilities;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.contribs.java.xml.XMLHelper;
import org.kalypso.convert.namodel.manager.IDManager;
import org.kalypso.convert.namodel.optimize.CalibarationConfig;
import org.kalypso.convert.namodel.optimize.NAOptimizingJob;
import org.kalypso.convert.namodel.timeseries.BlockTimeSeries;
import org.kalypso.convert.namodel.timeseries.DummyTimeSeriesWriter;
import org.kalypso.convert.namodel.timeseries.NATimeSettings;
import org.kalypso.gmlschema.adapter.IAnnotation;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.AnnotationUtilities;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.ObservationConstants;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.ogc.sensor.timeseries.envelope.TranProLinFilterUtilities;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.optimize.transform.OptimizeModelUtils;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.zml.Observation;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.GMLWorkspace_Impl;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;
import org.opengis.cs.CS_CoordinateSystem;
import org.w3c.dom.Document;
import org.xml.sax.InputSource;
import org.xml.sax.SAXParseException;

/**
 * @author doemming, huebsch
 */
public class NaModelInnerCalcJob implements ISimulation
{

  // resourcebase for static files used in calculation
  private final String m_resourceBase = "template/";

  private final String EXE_FILE_WEISSE_ELSTER = "start/kalypso_2.0.1a.exe";

  private final String EXE_FILE_2_02 = "start/kalypso_2.02.exe";

  private final String EXE_FILE_2_04beta = "start/kalypso_2.0.4beta.exe";

  private final String EXE_FILE_2_05beta = "start/kalypso_2.0.5beta.exe";

  private boolean m_succeeded = false;

  private String m_kalypsoKernelPath = EXE_FILE_WEISSE_ELSTER;

  final HashMap m_resultMap = new HashMap();

  public NaModelInnerCalcJob( )
  {
    m_urlUtilities = new UrlUtilities();
  }

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#getSpezifikation()
   */
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
  // public void run( File tmpdir, ICalcDataProvider inputProvider, ICalcResultEater resultEater, ICalcMonitor monitor )
  // throws CalcJobServiceException
  public void run( File tmpdir, ISimulationDataProvider inputProvider, ISimulationResultEater resultEater, ISimulationMonitor monitor ) throws SimulationException
  {
    final Logger logger = Logger.getAnonymousLogger();
    Formatter f = new SimpleFormatter();
    Handler h = null;
    try
    {
      File loggerFile = new File( tmpdir, "infoLog.txt" );
      h = new StreamHandler( new FileOutputStream( loggerFile ), f );
      logger.addHandler( h );
      h.flush();
    }
    catch( FileNotFoundException e1 )
    {
      e1.printStackTrace();
      logger.fine( e1.getLocalizedMessage() );
    }
    final Date date = new Date( Calendar.getInstance().getTimeInMillis() );
    logger.log( Level.INFO, "Zeitpunkt Start Berechnung: " + date.toString() + " (Serverzeit)\n" );
    final File resultDir = new File( tmpdir, NaModelConstants.OUTPUT_DIR_NAME );
    try
    {
      monitor.setMessage( "richte Berechnungsverzeichnis ein" );
      if( monitor.isCanceled() )
        return;

      // unzip templates aus inputdir (wird vom client als .asciitemplate.zip
      // -siehe modelspec- dorthin kopiert):
      if( monitor.isCanceled() )
        return;
      // Kopieren von Berechnungsstandardverzeichnis

      unzipInput( (URL) inputProvider.getInputForID( NaModelConstants.IN_TEMPLATE_ID ), tmpdir );

      // Kopieren von zu verwendenden Anfangswerten in das Berechnungsverzeichnis
      final File lzsimDir = new File( tmpdir, "lzsim" );
      unzipInput( (URL) inputProvider.getInputForID( NaModelConstants.LZSIM_IN_ID ), lzsimDir );
      // performance
      if( inputProvider.hasID( NAOptimizingJob.IN_BestOptimizedRunDir_ID ) )
      {
        // while optimization, you can recycle files from a former run.
        // implement here to copy the files to your tmp dir and while generating
        // files you should check if files allready exist, and on your option do
        // not generate them.
        // WARNING: never use result files or files that vary during
        // optimization.
        final URL url = (URL) inputProvider.getInputForID( NAOptimizingJob.IN_BestOptimizedRunDir_ID );
        final File from1 = new File( url.getFile(), "klima.dat" );
        final File to1 = new File( tmpdir, "klima.dat" );
        if( from1.exists() && to1.exists() )
        {
          final FileCopyVisitor copyVisitor = new FileCopyVisitor( from1, to1, true );
          FileUtilities.accept( from1, copyVisitor, true );
        }
        final File from2 = new File( url.getFile(), "zufluss" );
        final File to2 = new File( tmpdir, "zufluss" );
        if( from2.exists() && to2.exists() )
        {
          final FileCopyVisitor copyVisitor = new FileCopyVisitor( from2, to2, true );
          FileUtilities.accept( from2, copyVisitor, true );
        }
      }

      // generiere ascii-dateien
      monitor.setMessage( "generiere ASCII-Dateien (Modelldaten und Zeitreihen)" );
      if( monitor.isCanceled() )
        return;

      final File newModellFile = new File( tmpdir, "namodellBerechnung.gml" );

      // calualtion model

      final NAConfiguration conf = NAConfiguration.getGml2AsciiConfiguration( newModellFile.toURL(), tmpdir );
      conf.setZMLContext( (URL) inputProvider.getInputForID( NaModelConstants.IN_META_ID ) );
      final GMLWorkspace modellWorkspace = generateASCII( conf, tmpdir, inputProvider, newModellFile );
      final URL naControlURL = (URL) inputProvider.getInputForID( NaModelConstants.IN_CONTROL_ID );
      final GMLWorkspace naControlWorkspace = GmlSerializer.createGMLWorkspace( naControlURL );

      if( monitor.isCanceled() )
        return;
      // kopiere executable aus resourcen:
      copyExecutable( tmpdir );

      // starte berechnung
      monitor.setMessage( "starte Simulationskern" );
      if( monitor.isCanceled() )
        return;
      startCalculation( tmpdir, monitor );
      checkSucceeded( tmpdir );
      if( isSucceeded() )
      {
        monitor.setMessage( "Simulation erfolgreich beendet - lade Ergebnisse" );
        loadResults( tmpdir, modellWorkspace, naControlWorkspace, logger, resultDir, resultEater, conf );

        System.out.println( "fertig - Ergebnisse vorhanden" );
      }
      else
      {
        monitor.setMessage( "Simulation konnte nicht erfolgreich durchgeführt werden - lade Log-Dateien" );
        loadLogs( tmpdir, logger, resultEater );
        System.out.println( "fertig - Fehler siehe Log-Dateien" );
      }
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw new SimulationException( "Simulation konnte nicht durchgefuehrt werden", e );
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
    final Feature rootFeature = naControlWorkspace.getRootFeature();
    // final TimeseriesLink pegelLink = (TimeseriesLink)rootFeature.getProperty( "pegelZR" );
    final TimeseriesLinkType resultLink = (TimeseriesLinkType) rootFeature.getProperty( "qberechnetZR" );
    double accuracyPrediction = 5d;
    try
    {
      accuracyPrediction = ((Double) rootFeature.getProperty( "accuracyPrediction" )).doubleValue();
    }
    catch( Exception e )
    {
      logger.info( "Genauigkeit für Umhüllende ist nicht angegeben. Für die Vorhersage wird " + accuracyPrediction + " [cm/Tag] als Vorhersagegenauigkeit angenommen." );
    }
    // Object accuracyProp = rootFeature.getProperty( "accuracyPrediction" );
    // if(accuracyProp==null)
    // return;
    // accuracyPrediction = ( (Double)accuracyProp ).doubleValue();

    final Date startPrediction = conf.getSimulationForecastStart();
    final Date endPrediction = conf.getSimulationEnd();

    final UrlResolver urlResolver = new UrlResolver();
    // TODO if measured does not exists

    // find values at startPrediction
    final String axisType = TimeserieConstants.TYPE_WATERLEVEL;

    // from predicted timeseries
    final URL resultURL = urlResolver.resolveURL( resultDir.toURL(), resultLink.getHref() );
    final IObservation resultObservation = ZmlFactory.parseXML( resultURL, "vorhersage" );
    final ITuppleModel resultValues = resultObservation.getValues( null );
    final IAxis resultDateAxis = ObservationUtilities.findAxisByClass( resultObservation.getAxisList(), Date.class );

    final IAxis resultValueAxis;
    try
    {
      resultValueAxis = ObservationUtilities.findAxisByType( resultObservation.getAxisList(), axisType );
    }
    catch( Exception e )
    {
      logger.info( "Umhüllende kann nicht berechnet werden: Ursache: das Ergenis (" + resultObservation.getName()
          + ") kann nicht als Wasserstand berechnet werden. Möglicherweise ungültige WQ-Beziehung am Messpegel." );
      throw e;
    }

    double calcValue = ObservationUtilities.getInterpolatedValueAt( resultValues, resultDateAxis, resultValueAxis, startPrediction );

    double deltaMeasureCalculation;
    try
    {
      final NaNodeResultProvider nodeResultProvider = conf.getNodeResultProvider();
      final URL pegelURL = nodeResultProvider.getMeasuredURL( rootFeature );
      // from measuered timeseries

      final IObservation pegelObservation = ZmlFactory.parseXML( pegelURL, "pegelmessung" );
      final ITuppleModel pegelValues = pegelObservation.getValues( null );
      final IAxis pegelDateAxis = ObservationUtilities.findAxisByClass( pegelObservation.getAxisList(), Date.class );
      final IAxis pegelValueAxis = ObservationUtilities.findAxisByType( pegelObservation.getAxisList(), axisType );
      double measureValue = ObservationUtilities.getInterpolatedValueAt( pegelValues, pegelDateAxis, pegelValueAxis, startPrediction );
      deltaMeasureCalculation = measureValue - calcValue;
    }
    catch( Exception e )
    {
      deltaMeasureCalculation = 0;
    }

    final NATimeSettings timeSettings = NATimeSettings.getInstance();
    final Calendar calBegin = timeSettings.getCalendar( startPrediction );
    final Calendar calEnd = timeSettings.getCalendar( endPrediction );

    // test
    // final ObservationType observationType = ZmlFactory.createXML( resultObservation, null );
    // observationType.setName( "orginal-ergebnis" );
    // final Marshaller m = ZmlFactory.getMarshaller();
    // m.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
    // FileOutputStream stream = null;
    // OutputStreamWriter writer = null;
    // try
    // {
    // stream = new FileOutputStream( new File( "C://TMP/2-test-org.zml" ) );
    // writer = new OutputStreamWriter( stream, "UTF-8" );
    // m.marshal( observationType, writer );
    // }
    // finally
    // {
    // IOUtils.closeQuietly( writer );
    // IOUtils.closeQuietly( stream );
    // }
    // test
    final File[] result = new File[] { getResultFileFor( resultDir, rootFeature, "qAblageSpurMittlerer" ), getResultFileFor( resultDir, rootFeature, "qAblageSpurUnterer" ),
        getResultFileFor( resultDir, rootFeature, "qAblageSpurOberer" ) };
    // accuracyPrediction // cm/day
    final long dayOfMillis = 1000 * 60 * 60 * 24;
    final double endOffest = accuracyPrediction * (((double) (endPrediction.getTime() - startPrediction.getTime())) / ((double) dayOfMillis));

    final double offsetStartPrediction;
    final double offsetEndPrediction;
    if( FeatureHelper.booleanIsTrue( rootFeature, "useOffsetStartPrediction", false ) )
      offsetStartPrediction = deltaMeasureCalculation;
    else
      offsetStartPrediction = 0;
    if( FeatureHelper.booleanIsTrue( rootFeature, "useOffsetEndPrediction", false ) )
      offsetEndPrediction = deltaMeasureCalculation;
    else
      offsetEndPrediction = 0;

    final double[] operandStart = new double[] { offsetStartPrediction, offsetStartPrediction, offsetStartPrediction };
    final double[] operandEnd = new double[] { offsetEndPrediction, offsetEndPrediction - endOffest, offsetEndPrediction + endOffest };
    final String[] sufix = new String[] { " - Spur Mitte", " - spur Unten", " - spur Oben" };
    for( int i = 0; i < 3; i++ )
    {
      TranProLinFilterUtilities.transformAndWrite( resultObservation, calBegin, calEnd, operandStart[i], operandEnd[i], "+", axisType, KalypsoStati.BIT_DERIVATED, result[i], sufix[i] );
    }
  }

  /**
   * @param resultDir
   * @param feature
   * @param tsLinkPropName
   * @return file for result or null
   */
  private File getResultFileFor( final File resultDir, final Feature feature, final String tsLinkPropName )
  {
    try
    {
      final TimeseriesLinkType trackLink = (TimeseriesLinkType) feature.getProperty( tsLinkPropName );
      final String href = trackLink.getHref();
      final File resultFile = new File( resultDir, href );
      resultFile.getParentFile().mkdirs();
      return resultFile;
    }
    catch( Exception e )
    {
      return null; // no track available
    }

  }

  public void checkSucceeded( final File inputDir )
  {
    Reader logFileReader = null;
    LineNumberReader reader = null;
    try
    {
      final File logDir = new File( inputDir, "start" );
      final File logFile = new File( logDir, "output.res" );
      logFileReader = new FileReader( logFile );
      reader = new LineNumberReader( logFileReader );
      String line;
      while( (line = reader.readLine()) != null )
      {
        if( line.indexOf( "berechnung wurde ohne fehler beendet" ) >= 0 || line.indexOf( "Berechnung wurde ohne Fehler beendet!" ) >= 0 )
          m_succeeded = true;
      }
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    finally
    {
      IOUtils.closeQuietly( reader );
      IOUtils.closeQuietly( logFileReader );
    }
  }

  private GMLWorkspace generateASCII( NAConfiguration conf, File tmpDir, ISimulationDataProvider dataProvider, final File newModellFile ) throws Exception
  {
    final GMLWorkspace metaWorkspace = GmlSerializer.createGMLWorkspace( (URL) dataProvider.getInputForID( NaModelConstants.IN_META_ID ) );
    final Feature metaFE = metaWorkspace.getRootFeature();
    final GMLWorkspace controlWorkspace = GmlSerializer.createGMLWorkspace( (URL) dataProvider.getInputForID( NaModelConstants.IN_CONTROL_ID ) );

    // model Parameter
    final GMLWorkspace parameterWorkspace;
    if( dataProvider.hasID( NaModelConstants.IN_PARAMETER_ID ) )
      parameterWorkspace = GmlSerializer.createGMLWorkspace( (URL) dataProvider.getInputForID( NaModelConstants.IN_PARAMETER_ID ) );
    else
      parameterWorkspace = null;

    // initialize model with values of control file
    initializeModell( controlWorkspace.getRootFeature(), (URL) dataProvider.getInputForID( NaModelConstants.IN_MODELL_ID ), newModellFile );

    final GMLWorkspace modellWorkspace = GmlSerializer.createGMLWorkspace( newModellFile.toURL() );
    ((GMLWorkspace_Impl) modellWorkspace).setContext( (URL) dataProvider.getInputForID( NaModelConstants.IN_MODELL_ID ) );
    // final GMLWorkspace modellWorkspace = GmlSerializer.createGMLWorkspace( newModellFile.toURL() );
    ((GMLWorkspace_Impl) modellWorkspace).setContext( (URL) dataProvider.getInputForID( NaModelConstants.IN_MODELL_ID ) );

    final NaNodeResultProvider nodeResultProvider = new NaNodeResultProvider( modellWorkspace, controlWorkspace, conf.getZMLContext() );
    conf.setNodeResultProvider( nodeResultProvider );
    updateModelWithExtraVChannel( modellWorkspace, nodeResultProvider );

    // model Hydrotop
    final GMLWorkspace hydrotopWorkspace;

    // model Parameter
    final GMLWorkspace synthNWorkspace;
    if( dataProvider.hasID( NaModelConstants.IN_SYNTHN_ID ) )
      synthNWorkspace = GmlSerializer.createGMLWorkspace( (URL) dataProvider.getInputForID( NaModelConstants.IN_SYNTHN_ID ) );
    else
      synthNWorkspace = null;

    if( dataProvider.hasID( NaModelConstants.IN_HYDROTOP_ID ) )
    {
      hydrotopWorkspace = GmlSerializer.createGMLWorkspace( (URL) dataProvider.getInputForID( NaModelConstants.IN_HYDROTOP_ID ) );
      final Feature[] hydroFES = hydrotopWorkspace.getFeatures( hydrotopWorkspace.getFeatureType( "Hydrotop" ) );
      CS_CoordinateSystem targetCS = null;
      for( int i = 0; i < hydroFES.length && targetCS == null; i++ )
      {
        final GM_Object geom = (GM_Object) hydroFES[i].getProperty( "position" );
        if( geom != null && geom.getCoordinateSystem() != null )
          targetCS = geom.getCoordinateSystem();
      }
      if( targetCS != null )
      {
        final TransformVisitor visitor = new TransformVisitor( targetCS );
        modellWorkspace.accept( visitor, "/", FeatureVisitor.DEPTH_INFINITE );
      }
    }
    else
      hydrotopWorkspace = null;

    // setting duration of simulation...
    // start
    // conf.setSimulationStart( (Date) metaFE.getProperty( "startsimulation" ) );
    final Date start = DateUtilities.toDate( (XMLGregorianCalendar) metaFE.getProperty( "startsimulation" ) );
    conf.setSimulationStart( start );
    conf.setSzenarioID( (String) metaFE.getProperty( "scenarioId" ) );
    // start forecast

    // final Date startForecastDate = (Date) metaFE.getProperty( "startforecast" );
    final Date startForecastDate = DateUtilities.toDate( (XMLGregorianCalendar) metaFE.getProperty( "startforecast" ) );
    conf.setSimulationForecasetStart( startForecastDate );
    // end of simulation
    int hoursForecast = 0; // default length of forecast hours
    final Integer hoursOfForecast = (Integer) metaFE.getProperty( "hoursforecast" );
    if( hoursOfForecast != null )
      hoursForecast = hoursOfForecast.intValue();
    final Calendar c = Calendar.getInstance();
    c.setTime( startForecastDate );
    c.add( Calendar.HOUR, hoursForecast );
    final Date endDate = c.getTime();
    conf.setSimulationEnd( endDate );

    // calculate timestep
    int minutesTimeStep = 60;
    final Integer minutesOfTimeStep = (Integer) metaFE.getProperty( "minutesTimestep" );
    if( minutesOfTimeStep != null )
      minutesTimeStep = minutesOfTimeStep.intValue() != 0 ? minutesOfTimeStep.intValue() : 60;
    conf.setMinutesOfTimeStep( minutesTimeStep );

    // choose simulation kernel
    chooseSimulationExe( (String) metaFE.getProperty( "versionKalypsoNA" ) );

    // choose precipitation form and parameters
    final Boolean pns = (Boolean) metaFE.getProperty( "pns" );
    conf.setUsePrecipitationForm( pns == null ? false : pns );
    if( conf.isUsePrecipitationForm().equals( true ) )
    {
      conf.setAnnuality( (Double) metaFE.getProperty( "xjah" ) );
      conf.setDuration( (Double) metaFE.getProperty( "xwahl2" ) );
      conf.setForm( (String) metaFE.getProperty( "ipver" ) );
    }

    // set rootnode
    conf.setRootNodeID( (String) controlWorkspace.getRootFeature().getProperty( "rootNode" ) );

    // generate control files
    NAControlConverter.featureToASCII( conf, tmpDir, controlWorkspace, modellWorkspace );

    // update model with factor values from control
    updateFactorParameter( modellWorkspace );

    // generate modell files
    NAModellConverter main = new NAModellConverter( conf );
    main.write( modellWorkspace, parameterWorkspace, hydrotopWorkspace, synthNWorkspace, nodeResultProvider );

    // create temperatur und verdunstung timeseries
    final File klimaDir = new File( tmpDir, "klima.dat" );
    // TODO: wird das hier noch benötigt (Weisse Elster)?
    // final File tempFile = new File( klimaDir, CatchmentManager.STD_TEMP_FILENAME );
    // final File verdFile = new File( klimaDir, CatchmentManager.STD_VERD_FILENAME );
    final DummyTimeSeriesWriter writer = new DummyTimeSeriesWriter( conf.getSimulationStart(), conf.getSimulationEnd() );

    // if( !tempFile.exists() )
    // {
    // writer.writeTmpFile( tempFile );
    // }
    // if( !verdFile.exists() )
    // {
    // writer.writeVerdFile( verdFile );
    // }
    // dump idmapping to file

    final IDManager idManager = conf.getIdManager();
    Writer idWriter = null;
    try
    {
      idWriter = new FileWriter( new File( tmpDir, "IdMap.txt" ) );
      idManager.dump( idWriter );
    }
    finally
    {
      IOUtils.closeQuietly( idWriter );
    }
    return modellWorkspace;
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
    final IFeatureType kontEntnahmeFT = workspace.getFeatureType( "KontEntnahme" );
    final IFeatureType ueberlaufFT = workspace.getFeatureType( "Ueberlauf" );
    final IFeatureType verzweigungFT = workspace.getFeatureType( "Verzweigung" );
    final IFeatureType nodeFT = workspace.getFeatureType( "Node" );
    final Feature[] features = workspace.getFeatures( nodeFT );
    final IRelationType branchingMemberRT = (IRelationType) nodeFT.getProperty( "branchingMember" );
    for( int i = 0; i < features.length; i++ )
    {
      final Feature nodeFE = features[i];
      final Feature branchingFE = workspace.resolveLink( nodeFE, branchingMemberRT );
      if( branchingFE != null )
      {
        final IFeatureType branchFT = branchingFE.getFeatureType();
        final IRelationType branchingNodeMemberRT = (IRelationType) branchFT.getProperty( "branchingNodeMember" );
        if( branchFT == kontEntnahmeFT || branchFT == ueberlaufFT || branchFT == verzweigungFT )
        {
          final Feature targetNodeFE = workspace.resolveLink( branchingFE, branchingNodeMemberRT );
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
    final IFeatureType nodeFT = workspace.getFeatureType( "Node" );
    final IFeatureType abstractChannelFT = workspace.getFeatureType( "_Channel" );
    final Feature[] features = workspace.getFeatures( nodeFT );
    for( int i = 0; i < features.length; i++ )
    {
      final Feature nodeFE = features[i];
      if( nodeResultprovider.resultExists( nodeFE ) )
      {
        final Object resultValue = nodeFE.getProperty( "qberechnetZR" );
        // disconnect everything upstream (channel -> node)
        final IRelationType downStreamNodeMemberRT = (IRelationType) abstractChannelFT.getProperty( "downStreamNodeMember" );
        final Feature[] channelFEs = workspace.resolveWhoLinksTo( nodeFE, abstractChannelFT, downStreamNodeMemberRT );
        for( int j = 0; j < channelFEs.length; j++ )
        {
          final Feature newEndNodeFE = workspace.createFeature( channelFEs[j], nodeFT );
          workspace.setFeatureAsComposition( channelFEs[j], downStreamNodeMemberRT, newEndNodeFE, true );
        }
        // add as zufluss
        final Feature newNodeFE = buildVChannelNet( workspace, nodeFE );
        // final FeatureProperty zuflussProp = FeatureFactory.createFeatureProperty( "zuflussZR", resultValue );
        newNodeFE.setProperty( "zuflussZR", resultValue );
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

    final IFeatureType kontZuflussFT = workspace.getFeatureType( "KontZufluss" );
    final IFeatureType nodeFT = workspace.getFeatureType( "Node" );
    final Feature[] features = workspace.getFeatures( nodeFT );
    final IRelationType branchingMemberRT = (IRelationType) nodeFT.getProperty( "branchingMember" );
    for( int i = 0; i < features.length; i++ )
    {
      final Feature nodeFE = features[i];
      final Object zuflussValue = nodeFE.getProperty( "zuflussZR" );
      if( zuflussValue != null )
      {
        // update zufluss
        Feature newNode = buildVChannelNet( workspace, nodeFE );
        // nove zufluss-property to new node
        // nodeFE.setProperty( FeatureFactory.createFeatureProperty( "zuflussZR", null ) );
        // newNode.setProperty( FeatureFactory.createFeatureProperty( "zuflussZR", zuflussValue ) );
        nodeFE.setProperty( "zuflussZR", null );
        newNode.setProperty( "zuflussZR", zuflussValue );
      }
      final Feature branchingFE = workspace.resolveLink( nodeFE, branchingMemberRT );
      if( branchingFE != null && branchingFE.getFeatureType() == kontZuflussFT )
      {
        // update zufluss
        Feature newNode = buildVChannelNet( workspace, nodeFE );
        // nove constant-inflow to new node
        workspace.setFeatureAsComposition( nodeFE, branchingMemberRT, null, true );
        workspace.setFeatureAsComposition( newNode, branchingMemberRT, branchingFE, true );
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
    final IFeatureType nodeColFT = workspace.getFeatureType( "NodeCollection" );
    final IFeatureType nodeFT = workspace.getFeatureType( "Node" );
    final IRelationType nodeMemberRT = (IRelationType) nodeColFT.getProperty( "nodeMember" );
    final IFeatureType vChannelFT = workspace.getFeatureType( "VirtualChannel" );

    final IFeatureType channelColFT = workspace.getFeatureType( "ChannelCollection" );
    final IRelationType channelMemberRT = (IRelationType) channelColFT.getProperty( "channelMember" );
    final Feature channelColFE = workspace.getFeatures( channelColFT )[0];
    final Feature nodeColFE = workspace.getFeatures( workspace.getFeatureType( "NodeCollection" ) )[0];

    // add to collections:
    final Feature newChannelFE1 = workspace.createFeature( channelColFE, vChannelFT );
    workspace.addFeatureAsComposition( channelColFE, channelMemberRT, 0, newChannelFE1 );
    final Feature newChannelFE3 = workspace.createFeature( channelColFE, vChannelFT );
    workspace.addFeatureAsComposition( channelColFE, channelMemberRT, 0, newChannelFE3 );
    final Feature newNodeFE2 = workspace.createFeature( nodeColFE, nodeFT );
    workspace.addFeatureAsComposition( nodeColFE, nodeMemberRT, 0, newNodeFE2 );
    final IRelationType downStreamNodeMemberRT = (IRelationType) vChannelFT.getProperty( "downStreamNodeMember" );

    // 3 -> 2
    workspace.setFeatureAsAggregation( newChannelFE3, downStreamNodeMemberRT, newNodeFE2.getId(), true );
    // 2 -> 1
    final IRelationType downStreamChannelMemberRT = (IRelationType) nodeFT.getProperty( "downStreamChannelMember" );
    workspace.setFeatureAsAggregation( newNodeFE2, downStreamChannelMemberRT, newChannelFE1.getId(), true );
    // 1 -> existing

    final IRelationType downStreamNodeMemberRT1 = (IRelationType) vChannelFT.getProperty( "downStreamNodeMember" );
    workspace.setFeatureAsAggregation( newChannelFE1, downStreamNodeMemberRT, existingNode.getId(), true );
    return newNodeFE2;
  }

  /**
   * updates workspace, so that interflow and channelflow dependencies gets optimized <br>
   * groundwater flow can now run in opposite direction to channel flow
   * 
   * @param workspace
   */
  private void updateGWNet( final GMLWorkspace workspace )
  {
    final IFeatureType catchmentFT = workspace.getFeatureType( "Catchment" );
    final IFeatureType vChannelFT = workspace.getFeatureType( "VirtualChannel" );
    final Feature[] features = workspace.getFeatures( catchmentFT );
    final IRelationType entwaesserungsStrangMemberRT = (IRelationType) catchmentFT.getProperty( "entwaesserungsStrangMember" );
    for( int i = 0; i < features.length; i++ )
    {
      final Feature catchmentFE = features[i];

      final Feature orgChannelFE = workspace.resolveLink( catchmentFE, entwaesserungsStrangMemberRT );
      if( orgChannelFE == null )
        continue;
      if( orgChannelFE.getId().equals( "VirtualChannel0" ) )
      {
        System.out.println( orgChannelFE.getId() );
      }
      final IRelationType downStreamNodeMemberRT = (IRelationType) vChannelFT.getProperty( "downStreamNodeMember" );
      final Feature nodeFE = workspace.resolveLink( orgChannelFE, downStreamNodeMemberRT );
      // TODO This might be my problem because the link is set inline.
      // should go into the channel collection and liked with href:
      final Feature newChannelFE = workspace.createFeature( catchmentFE, vChannelFT );
      // set new relation: catchment -> new V-channel
      try
      {
        // not inline
        workspace.setFeatureAsComposition( catchmentFE, entwaesserungsStrangMemberRT, newChannelFE, true );
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
      // set new relation: new V-channel -> downstream node
      try
      {
        final IRelationType downStreamNodeMemberRT2 = (IRelationType) newChannelFE.getFeatureType().getProperty( "downStreamNodeMember" );
        workspace.addFeatureAsAggregation( newChannelFE, downStreamNodeMemberRT2, 1, nodeFE.getId() );
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
    }

  }

  /**
   * @param kalypsoNAVersion
   *          name/version of simulation kernel
   */
  private void chooseSimulationExe( final String kalypsoNAVersion )
  {
    if( kalypsoNAVersion == null || kalypsoNAVersion.equals( "lfug" ) || kalypsoNAVersion.equals( "" ) )
      m_kalypsoKernelPath = EXE_FILE_WEISSE_ELSTER;
    else if( kalypsoNAVersion.equals( "v2.0.2" ) )
      m_kalypsoKernelPath = EXE_FILE_2_02;
    else if( kalypsoNAVersion.equals( "test" ) )
      m_kalypsoKernelPath = EXE_FILE_2_04beta;
    else if( kalypsoNAVersion.equals( "neueste" ) || kalypsoNAVersion.equals( "latest" ) )
      m_kalypsoKernelPath = EXE_FILE_2_04beta;
    else if( kalypsoNAVersion.equals( "v2.0.4" ) )
      m_kalypsoKernelPath = EXE_FILE_2_04beta;
    else if( kalypsoNAVersion.equals( "v2.0.5" ) )
      m_kalypsoKernelPath = EXE_FILE_2_05beta;
    else
    {
      System.out.println( "Sie haben keine Version des Fortran Codes angegeben oder \n" + " die von Ihnen angegebene Version wird nicht weiter unterstützt.\n"
          + " Es wird mit der default version gerechnet." );
      m_kalypsoKernelPath = EXE_FILE_WEISSE_ELSTER;
    }
  }

  private void initializeModell( Feature controlFeature, URL inputModellURL, File outputModelFile ) throws Exception
  {
    CalibarationConfig config = new CalibarationConfig();
    config.addFromNAControl( controlFeature );

    Document modelDoc = null;
    try
    {
      modelDoc = XMLHelper.getAsDOM( new InputSource( inputModellURL.openStream() ), true );
    }

    catch( SAXParseException e )
    {
      e.printStackTrace();
      int lineNumber = e.getLineNumber();
      System.out.println( e.getLocalizedMessage() + " #" + lineNumber );
    }
    catch( Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();

      throw e;
    }

    OptimizeModelUtils.initializeModel( modelDoc, config.getCalContexts() );

    // TODO: take charset from Document
    final String charset = "UTF-8";
    final Writer writer = new OutputStreamWriter( new FileOutputStream( outputModelFile ), charset );
    final Transformer t = TransformerFactory.newInstance().newTransformer();
    t.transform( new DOMSource( modelDoc ), new StreamResult( writer ) );
    writer.close();
  }

  /**
   *  
   */
  private final static String[][] m_catchmentFactorsParameter = { new String[] { "retob", "faktorRetobRetint" }, new String[] { "retint", "faktorRetobRetint" }, new String[] { "aigw", "faktorAigw" } };

  private static String[] m_catchmentFactorParameterTarget = { "retob", "retint", "aigw" };

  private final UrlUtilities m_urlUtilities;

  /**
   * some parameter have factors that must be processed before generating asciifiles, as these factors do not occur in
   * ascci-format
   * 
   * @param modellWorkspace
   */
  private void updateFactorParameter( GMLWorkspace modellWorkspace )
  {
    // Catchments
    final Feature[] catchmentFEs = modellWorkspace.getFeatures( modellWorkspace.getFeatureType( "Catchment" ) );
    update( catchmentFEs, m_catchmentFactorParameterTarget, m_catchmentFactorsParameter );

    // KMChannels
    final Feature[] kmChanneFEs = modellWorkspace.getFeatures( modellWorkspace.getFeatureType( "KMChannel" ) );
    for( int i = 0; i < kmChanneFEs.length; i++ )
    {
      Feature feature = kmChanneFEs[i];
      double rkfFactor = FeatureHelper.getAsDouble( feature, "faktorRkf", 1.0 );
      double rnfFactor = FeatureHelper.getAsDouble( feature, "faktorRnf", 1.0 );
      List kmParameter = (List) feature.getProperty( "KMParameterMember" );
      Iterator iterator = kmParameter.iterator();
      while( iterator.hasNext() )
      {
        final Feature kmParameterFE = (Feature) iterator.next();
        // rnf
        final double _rnf = rnfFactor * FeatureHelper.getAsDouble( kmParameterFE, "rnf", 1.0 );
        // FeatureProperty rnfProp = FeatureFactory.createFeatureProperty( "rnf", new Double( _rnf ) );
        kmParameterFE.setProperty( "rnf", new Double( _rnf ) );
        // rkf
        final double _rkf = rkfFactor * FeatureHelper.getAsDouble( kmParameterFE, "rkf", 1.0 );
        // FeatureProperty rkfProp = FeatureFactory.createFeatureProperty( "rkf", new Double( _rkf ) );
        kmParameterFE.setProperty( "rkf", new Double( _rkf ) );
      }
    }
  }

  private void update( Feature[] features, String[] targetPropNames, String[][] factorPropNames )
  {
    for( int i = 0; i < features.length; i++ ) // iterate features
    {
      final Feature feature = features[i];
      for( int _p = 0; _p < targetPropNames.length; _p++ ) // iterate parameters
      {
        final String[] factors = factorPropNames[_p];
        double value = 1.0; // initial value
        for( int _f = 0; _f < factors.length; _f++ )
          // iterate factors
          value *= FeatureHelper.getAsDouble( feature, factors[_f], 1.0 );
        // set parameter
        final String targetPropName = targetPropNames[_p];
        // FeatureProperty valueProp = FeatureFactory.createFeatureProperty( targetPropName, new Double( value ) );
        feature.setProperty( targetPropName, new Double( value ) );
      }
    }
  }

  private void loadResults( final File tmpdir, final GMLWorkspace modellWorkspace, final GMLWorkspace naControlWorkspace, final Logger logger, final File resultDir, ISimulationResultEater resultEater, final NAConfiguration conf ) throws Exception
  {
    loadTSResults( tmpdir, modellWorkspace, logger, resultDir, conf );
    try
    {
      loadTesultTSPredictionIntervals( naControlWorkspace, logger, resultDir, conf );
    }
    catch( Exception e )
    {
      logger.info( "konnte Umhüllende nicht generieren, evt. keine WQ-Informationen vorhanden , Fehler: " + e.getLocalizedMessage() );
    }
    loadTextFileResults( tmpdir, logger, resultDir );
    if( conf.getIniWrite() )
    {
      loadIniValues( tmpdir, logger, resultEater );
    }
    loadLogs( tmpdir, logger, resultEater );
    final File[] files = resultDir.listFiles();
    if( files != null )
    {
      for( int i = 0; i < files.length; i++ )
      {
        if( files[i].isDirectory() ) // Ergebnisse
        {
          resultEater.addResult( NaModelConstants.OUT_ZML, files[i] );
          return;
        }
      }
    }
  }

  private void loadIniValues( final File tmpDir, final Logger logger, final ISimulationResultEater resultEater )
  {
    try
    {
      final String[] wildcards = new String[] { "*" + "lzs", "*" + "lzg" };
      File lzsimDir = new File( tmpDir, "lzsim" );
      final MultipleWildCardFileFilter filter = new MultipleWildCardFileFilter( wildcards, false, false, true );
      final File[] lzsimFiles = lzsimDir.listFiles( filter );
      File lzsimZIP = new File( tmpDir, "lzsim.zip" );
      try
      {
        ZipUtilities.zip( lzsimZIP, lzsimFiles, lzsimDir );
      }
      catch( IOException e )
      {
        e.printStackTrace();
      }
      resultEater.addResult( NaModelConstants.LZSIM_OUT_ID, lzsimZIP );
    }
    catch( SimulationException e )
    {
      e.printStackTrace();
      logger.info( e.getMessage() );
    }
  }

  private String getTitleForSuffix( String suffix )
  {
    // j Temperatur .tmp
    if( suffix.equalsIgnoreCase( "tmp" ) )
      return "Temperatur";
    // j Niederschlag .pre
    if( suffix.equalsIgnoreCase( "pre" ) )
      return "Niederschlag";
    // n Schnee .sch
    if( suffix.equalsIgnoreCase( "sch" ) )
      return "Schneehoehe";
    // j Bodenfeuchte .bof
    if( suffix.equalsIgnoreCase( "bof" ) )
      return "Bodenfeuchte";
    // n Bodenspeicher .bsp
    if( suffix.equalsIgnoreCase( "bsp" ) )
      return "Bodenspeicherbilanz";
    // n Grundwasserstand .gws
    if( suffix.equalsIgnoreCase( "gws" ) )
      return "Grundwasserstand";
    // j Gesamtabfluss Knoten .qgs
    if( suffix.equalsIgnoreCase( "qgs" ) )
      return "Gesamtabfluss";
    // n Gesamtabfluss TG .qgg
    if( suffix.equalsIgnoreCase( "qgg" ) )
      return "Gesamtabfluss";
    // n Oberflaechenabfluss .qna
    if( suffix.equalsIgnoreCase( "qna" ) )
      return "Oberflaechenabfluss(natuerlich)";
    // n Interflow .qif
    if( suffix.equalsIgnoreCase( "qif" ) )
      return "Interflow";
    // n Abfluss vers. Flaechen .qvs
    if( suffix.equalsIgnoreCase( "qvs" ) )
      return "Oberflaechenabfluss(versiegelt)";
    // n Basisabfluss .qbs
    if( suffix.equalsIgnoreCase( "qbs" ) )
      return "Basisabfluss";
    // n Kluftgrundw1 .qt1
    if( suffix.equalsIgnoreCase( "qt1" ) )
      return "Kluftgrundw1abfluss";
    // n Kluftgrundw .qtg
    if( suffix.equalsIgnoreCase( "qtg" ) )
      return "KluftGWAbfluss";
    // n Grundwasser .qgw
    if( suffix.equalsIgnoreCase( "qgw" ) )
      return "Grundwasserabfluss";
    // n Evapotranspiration .vet
    if( suffix.equalsIgnoreCase( "vet" ) )
      return "Evapotranspiration";
    // n Ausgabe hydrotope .hyd
    if( suffix.equalsIgnoreCase( "hyd" ) )
      return "Hydrotope";
    // n Abflussbilanz .bil
    if( suffix.equalsIgnoreCase( "bil" ) )
      return "Abflussbilanz";
    // n Statistische Abflusswerte .nmq
    if( suffix.equalsIgnoreCase( "nmq" ) )
      return "Abflusswerte(statistisch)";
    // n Speicherinhalt .spi
    if( suffix.equalsIgnoreCase( "spi" ) )
      return "Fuellvolumen";
    // n Wasserspiegelhöhe .sph
    if( suffix.equalsIgnoreCase( "sph" ) )
      return "Wasserspiegelhoehe";
    // n Verdunstung aus Talsperre .spv
    if( suffix.equalsIgnoreCase( "spv" ) )
      return "Talsperrenverdunstung";
    // n Niederschlag in Talsperre .spn
    if( suffix.equalsIgnoreCase( "spn" ) )
      return "Niederschlag";
    // n Zehrung .spb
    if( suffix.equalsIgnoreCase( "spb" ) )
      return "Zehrung";
    // n Speicherueberlauf .sub
    if( suffix.equalsIgnoreCase( "sub" ) )
      return "Speicherueberlauf";
    // n Kapil.Aufstieg/Perkolation .kap - not available, because not used in the calculation core
    // if( suffix.equalsIgnoreCase( "kap" ) )
    // return "Kapil.Aufstieg/Perkolation";
    return suffix;
  }

  private void loadTSResults( final File inputDir, final GMLWorkspace modellWorkspace, final Logger logger, final File outputDir, final NAConfiguration conf ) throws Exception
  {
    // j Gesamtabfluss Knoten .qgs
    final IFeatureType nodeFT = modellWorkspace.getFeatureType( "Node" );
    loadTSResults( "qgs", nodeFT, "name", TimeserieConstants.TYPE_RUNOFF, "pegelZR", "qberechnetZR", inputDir, modellWorkspace, logger, outputDir, 1.0d, conf );

    final IFeatureType catchmentFT = modellWorkspace.getFeatureType( "Catchment" );
    final IFeatureType rhbChannelFT = modellWorkspace.getFeatureType( "StorageChannel" );
    // j Niederschlag .pre
    loadTSResults( "pre", catchmentFT, "name", TimeserieConstants.TYPE_RAINFALL, null, null, inputDir, modellWorkspace, logger, outputDir, 1.0d, conf );
    // j Temperatur .tmp
    loadTSResults( "tmp", catchmentFT, "name", TimeserieConstants.TYPE_TEMPERATURE, null, null, inputDir, modellWorkspace, logger, outputDir, 1.0d, conf );
    // n Interflow .qif
    loadTSResults( "qif", catchmentFT, "name", TimeserieConstants.TYPE_RUNOFF, null, null, inputDir, modellWorkspace, logger, outputDir, 1.0d, conf );
    // n Grundwasser .qgw
    loadTSResults( "qgw", catchmentFT, "name", TimeserieConstants.TYPE_RUNOFF, null, null, inputDir, modellWorkspace, logger, outputDir, 1.0d, conf );
    // n Gesamtabfluss TG .qgg
    loadTSResults( "qgg", catchmentFT, "name", TimeserieConstants.TYPE_RUNOFF, null, null, inputDir, modellWorkspace, logger, outputDir, 1.0d, conf );
    // n Grundwasserstand .gws - Umrechnung von m auf cm
    loadTSResults( "gws", catchmentFT, "name", TimeserieConstants.TYPE_WATERLEVEL, null, null, inputDir, modellWorkspace, logger, outputDir, 100.0d, conf );
    // n Basisabfluss .qbs
    loadTSResults( "qbs", catchmentFT, "name", TimeserieConstants.TYPE_RUNOFF, null, null, inputDir, modellWorkspace, logger, outputDir, 1.0d, conf );
    // n Oberflaechenabfluss .qna
    loadTSResults( "qna", catchmentFT, "name", TimeserieConstants.TYPE_RUNOFF, null, null, inputDir, modellWorkspace, logger, outputDir, 1.0d, conf );
    // n Abfluss vers. Flaechen .qvs
    loadTSResults( "qvs", catchmentFT, "name", TimeserieConstants.TYPE_RUNOFF, null, null, inputDir, modellWorkspace, logger, outputDir, 1.0d, conf );
    // TODO:check output for the next time series
    // n Schnee .sch [mm]
    loadTSResults( "sch", catchmentFT, "name", TimeserieConstants.TYPE_WATERLEVEL, null, null, inputDir, modellWorkspace, logger, outputDir, 0.1d, conf );
    // n Kluftgrundw1 .qt1
    loadTSResults( "qt1", catchmentFT, "name", TimeserieConstants.TYPE_RUNOFF, null, null, inputDir, modellWorkspace, logger, outputDir, 1.0d, conf );
    // n Kluftgrundw .qtg
    loadTSResults( "qtg", catchmentFT, "name", TimeserieConstants.TYPE_RUNOFF, null, null, inputDir, modellWorkspace, logger, outputDir, 1.0d, conf );
    // n Evapotranspiration .vet [mm]
    loadTSResults( "vet", catchmentFT, "name", TimeserieConstants.TYPE_WATERLEVEL, null, null, inputDir, modellWorkspace, logger, outputDir, 0.1d, conf );

    // TODO: Zeitreihe als mittlere Bodenfeuchte aus Fortran übernehmen, daher bisher nicht zu übertragen (Zur zeit wird
    // die Bodenfeuchte des ersten Hydrotopes in allen Schichten ausgegeben)
    // j Bodenfeuchte .bof [mm]
    // loadTSResults( "bof", catchmentFT, "name", TimeserieConstants.TYPE_WATERLEVEL, null, null, inputDir,
    // modellWorkspace, logger, outputDir, 0.1d, conf );

    // Straenge
    // n Wasserstand Speicher .sph [muNN]
    loadTSResults( "sph", rhbChannelFT, "name", TimeserieConstants.TYPE_NORMNULL, null, null, inputDir, modellWorkspace, logger, outputDir, 1.0d, conf );
    // n Speicherueberlauf .sub [m³/s]
    loadTSResults( "sub", rhbChannelFT, "name", TimeserieConstants.TYPE_RUNOFF, null, null, inputDir, modellWorkspace, logger, outputDir, 1.0d, conf );
    // n Speicherinhalt .spi [hm³] - Umrechnung auf m³
    loadTSResults( "spi", rhbChannelFT, "name", TimeserieConstants.TYPE_VOLUME, null, null, inputDir, modellWorkspace, logger, outputDir, 1000000.0d, conf );
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

  private void loadTSResults( final String suffix, final IFeatureType resultFT, final String titlePropName, final String resultType, final String metadataTSLink, final String targetTSLink, final File inputDir, final GMLWorkspace modellWorkspace, final Logger logger, final File outputDir, final double resultFactor, final NAConfiguration conf ) throws Exception
  {
    final IDManager idManager = conf.getIdManager();
    // ASCII-Files
    // generiere ZML Ergebnis Dateien
    final File ascciResultDir = new File( inputDir, "out_we.nat" );
    final MultipleWildCardFileFilter filter = new MultipleWildCardFileFilter( new String[] { "*" + suffix + "*" }, false, false, true );
    final File[] qgsFiles = ascciResultDir.listFiles( filter );
    if( qgsFiles.length != 0 )
    {
      // read ascii result file
      logger.info( "lese ergebnissdatei " + qgsFiles[0].getName() + "\n" );
      final BlockTimeSeries ts = new BlockTimeSeries();
      ts.importBlockFile( qgsFiles[0] );

      // iterate model nodes/Catchments/rhbChannels and generate zml

      final Feature[] nodeFEs = modellWorkspace.getFeatures( resultFT );
      for( int i = 0; i < nodeFEs.length; i++ )
      {
        final Feature feature = nodeFEs[i];
        if( resultFT == (modellWorkspace.getFeatureType( "Node" )) || resultFT == (modellWorkspace.getFeatureType( "Catchment" )) || resultFT == (modellWorkspace.getFeatureType( "StorageChannel" )) )
        {
          if( !FeatureHelper.booleanIsTrue( feature, "generateResult", false ) )
            continue; // should not generate results
        }
        // FIXME @huebsch das geht so nicht, wie soll der server auf die eingestellte sprache beim client zugreifen
        // ????, ich machs erstmal deutsch, evt. brauchen wir ein allgemeines konzept, dass der calcjob die sprache mit
        // bekommt, askme (doemming)
        //        
        // final String lang = KalypsoGisPlugin.getDefault().getPluginPreferences().getString(
        // IKalypsoPreferences.LANGUAGE );
        final String lang = "de";
        // hm, so gehts auch nicht
        final IAnnotation annotation = AnnotationUtilities.getAnnotation( feature.getFeatureType() );
        final String annotationLabel = annotation != null ? annotation.getLabel() : feature.getFeatureType().getQName().getLocalPart();
        final String key = Integer.toString( idManager.getAsciiID( feature ) );
        final String feName = (String) feature.getProperty( titlePropName );
        final String observationTitle;
        if( feName != null && feName.length() > 0 )
          observationTitle = feName;
        else
          observationTitle = feature.getId();

        final String axisTitle = getAxisTitleForSuffix( suffix );

        if( !ts.dataExistsForKey( key ) )
          continue; // no results available
        logger.info( "lese berechnetes Ergebnis fuer #" + key + ", Name:" + observationTitle + "\n" );

        // transform data to tuppelmodel
        final SortedMap data = ts.getTimeSerie( key );
        final Object[][] tupelData = new Object[data.size()][3];
        final Set dataSet = data.entrySet();
        final Iterator iter = dataSet.iterator();
        int pos = 0;
        while( iter.hasNext() )
        {
          Map.Entry entry = (Map.Entry) iter.next();
          tupelData[pos][0] = (Date) entry.getKey();
          tupelData[pos][1] = new Double( Double.parseDouble( entry.getValue().toString() ) * resultFactor );
          tupelData[pos][2] = new Integer( KalypsoStati.BIT_OK );
          pos++;
        }

        final IAxis dateAxis = new DefaultAxis( "Datum", TimeserieConstants.TYPE_DATE, "", Date.class, true );
        final IAxis qAxis = new DefaultAxis( axisTitle, resultType, TimeserieUtils.getUnit( resultType ), Double.class, false );
        final IAxis statusAxis = KalypsoStatusUtils.createStatusAxisFor( qAxis, true );
        final IAxis[] axis = new IAxis[] { dateAxis, qAxis, statusAxis };
        final ITuppleModel qTuppelModel = new SimpleTuppleModel( axis, tupelData );

        final MetadataList metadataList = new MetadataList();

        // if pegel exists, copy metadata (inclusive wq-function)
        TimeseriesLinkType pegelLink = null;
        if( metadataTSLink != null )
          pegelLink = (TimeseriesLinkType) feature.getProperty( metadataTSLink );
        if( pegelLink != null )
        {
          final URL pegelURL = m_urlUtilities.resolveURL( modellWorkspace.getContext(), pegelLink.getHref() );
          boolean itExists;
          // test if url exists
          try
          {
            pegelURL.openStream();
            itExists = true;
          }
          catch( Exception e )
          {
            itExists = false;
          }
          if( itExists )
          {
            logger.info( "zu diesem Knoten existiert ein Pegel, einige Pegelmetadaten (z.B. Wechmann-Funktion) werden in Ergebniszeitreihe uebernommen\n" );
            final IObservation pegelObservation = ZmlFactory.parseXML( pegelURL, "pegelmessung" );

            copyMetaData( pegelObservation.getMetadataList(), metadataList, new String[] { TimeserieConstants.MD_ALARM_1, TimeserieConstants.MD_ALARM_2, TimeserieConstants.MD_ALARM_3,
                TimeserieConstants.MD_ALARM_4, TimeserieConstants.MD_GEWAESSER, TimeserieConstants.MD_FLUSSGEBIET, TimeserieConstants.MD_GKH, TimeserieConstants.MD_GKR,
                TimeserieConstants.MD_HOEHENANGABEART, TimeserieConstants.MD_PEGELNULLPUNKT, TimeserieConstants.MD_WQWECHMANN, TimeserieConstants.MD_WQTABLE, TimeserieConstants.MD_TIMEZONE,
                TimeserieConstants.MD_VORHERSAGE, ObservationConstants.MD_SCENARIO } );

          }
        }
        // lese ergebnis-link um target fuer zml zu finden
        String resultPathRelative;
        try
        {
          TimeseriesLinkType resultLink = (TimeseriesLinkType) feature.getProperty( targetTSLink );
          if( resultLink == null )
          {
            logger.info( "kein ergebnislink gesetzt für FID=#" + feature.getId() + " ." );
          }
          resultPathRelative = resultLink.getHref();
        }
        catch( Exception e )
        {
          // if there is target defined or there are some problems with that
          // we generate one

          resultPathRelative = "Ergebnisse/Berechnet/" + annotationLabel + "/" + observationTitle + "/" + getTitleForSuffix( suffix ) + ".zml";
          // resultPathRelative = "Ergebnisse/Berechnet/" + annotationLabel + "/" + observationTitle + "/" + suffix +
          // "(" + observationTitle + ").zml";
        }
        if( !m_resultMap.containsKey( resultPathRelative ) )
        {
          m_resultMap.put( resultPathRelative, observationTitle );
        }
        else
        {
          logger.info( "Datei existiert bereits: " + resultPathRelative + "." );
          resultPathRelative = "Ergebnisse/Berechnet/" + annotationLabel + "/" + observationTitle + "(ID" + Integer.toString( idManager.getAsciiID( feature ) ).trim() + ")/" + suffix + "("
              + observationTitle + ").zml";
          m_resultMap.put( resultPathRelative, observationTitle );
          logger.info( "Der Dateiname wurde daher um die ObjektID erweitert: " + resultPathRelative + "." );
        }

        final File resultFile = new File( outputDir, resultPathRelative );
        resultFile.getParentFile().mkdirs();

        // create observation object
        final IObservation resultObservation = new SimpleObservation( resultPathRelative, "ID", observationTitle + " - " + getTitleForSuffix( suffix ) + " " + annotationLabel, false, null, metadataList, axis, qTuppelModel );

        // update with Scenario metadata
        final String scenarioID = conf.getScenarioID();
        if( scenarioID != null && scenarioID.length() > 0 )
          resultObservation.getMetadataList().put( ObservationConstants.MD_SCENARIO, scenarioID );

        // write result
        // final ObservationType observationType = ZmlFactory.createXML( resultObservation, null );
        final Observation observation = ZmlFactory.createXML( resultObservation, null );
        final Marshaller marshaller = ZmlFactory.getMarshaller();
        marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );

        FileOutputStream stream = null;
        OutputStreamWriter writer = null;
        try
        {
          stream = new FileOutputStream( resultFile );
          writer = new OutputStreamWriter( stream, "UTF-8" );
          marshaller.marshal( observation, writer );
        }
        finally
        {
          IOUtils.closeQuietly( writer );
          IOUtils.closeQuietly( stream );
        }
      }
    }
  }

  /**
   * @param suffix
   * @return AxisTitle
   */
  private String getAxisTitleForSuffix( String suffix )
  {
    // j Temperatur .tmp
    if( suffix.equalsIgnoreCase( "tmp" ) )
      return "Temperatur";
    // j Niederschlag .pre
    if( suffix.equalsIgnoreCase( "pre" ) )
      return "Niederschlag";
    // n Schnee .sch
    if( suffix.equalsIgnoreCase( "sch" ) )
      return "Schneehoehe";
    // j Bodenfeuchte .bof
    if( suffix.equalsIgnoreCase( "bof" ) )
      return "Bodenfeuchte";
    // n Bodenspeicher .bsp
    if( suffix.equalsIgnoreCase( "bsp" ) )
      return "Bodenspeicherbilanz";
    // n Grundwasserstand .gws
    if( suffix.equalsIgnoreCase( "gws" ) )
      return "Grundwasserstand";
    // Gesamtabfluss Knoten .qgs, Gesamtabfluss TG .qgg, Oberflaechenabfluss .qna, Interflow .qif, Abfluss vers.
    // Flaechen .qvs, Basisabfluss .qbs, Kluftgrundw1 .qt1, Kluftgrundw .qtg, Grundwasser .qgw
    if( suffix.equalsIgnoreCase( "qgs" ) | suffix.equalsIgnoreCase( "qgg" ) | suffix.equalsIgnoreCase( "qna" ) | suffix.equalsIgnoreCase( "qif" ) | suffix.equalsIgnoreCase( "qvs" )
        | suffix.equalsIgnoreCase( "qbs" ) | suffix.equalsIgnoreCase( "qt1" ) | suffix.equalsIgnoreCase( "qtg" ) | suffix.equalsIgnoreCase( "qgw" ) )
      return "Abfluss";
    // n Evapotranspiration .vet
    if( suffix.equalsIgnoreCase( "vet" ) )
      return "Evapotranspiration";
    // n Ausgabe hydrotope .hyd
    if( suffix.equalsIgnoreCase( "hyd" ) )
      return "Ausgabe Hydrotope";
    // n Abflussbilanz .bil
    if( suffix.equalsIgnoreCase( "bil" ) )
      return "Abflussbilanz";
    // n Statistische Abflusswerte .nmq
    if( suffix.equalsIgnoreCase( "nmq" ) )
      return "Statistische Abflusswerte";
    // n Speicherinhalt .spi
    if( suffix.equalsIgnoreCase( "spi" ) )
      return "Fuellvolumen";
    // n Wasserspiegelhöhe .sph
    if( suffix.equalsIgnoreCase( "sph" ) )
      return "Wasserspiegelhöhe";
    // n Verdunstung aus Talsperre .spv
    if( suffix.equalsIgnoreCase( "spv" ) )
      return "Talsperrenverdunstung";
    // n Niederschlag in Talsperre .spn
    if( suffix.equalsIgnoreCase( "spn" ) )
      return "Niederschlag";
    // n Zehrung .spb
    if( suffix.equalsIgnoreCase( "spb" ) )
      return "Zehrung";
    // n Speicherueberlauf .sub
    if( suffix.equalsIgnoreCase( "sub" ) )
      return "Speicherueberlauf";
    return suffix;
  }

  private void loadTextFileResults( File inputDir, Logger logger, File outputDir )
  {
    // ASCII-Files
    // kopiere statistische Ergebnis-Dateien
    final String[] wildcards = new String[] { "*" + "bil" + "*" };
    // final String[] wildcards = new String[] { "*" + "bil" + "*", "*" + "txt" + "*", "*" + "nmq" + "*", "*" + "bsp" +
    // "*" };
    final File ascciResultDir = new File( inputDir, "out_we.nat" );
    MultipleWildCardFileFilter filter = new MultipleWildCardFileFilter( wildcards, false, false, true );

    File[] qgsFiles = ascciResultDir.listFiles( filter );
    if( qgsFiles.length != 0 )

    {
      for( int i = 0; i < qgsFiles.length; i++ )
      {
        // read ascii result file
        logger.info( "kopiere Ergebnissdatei " + qgsFiles[i].getName() + "\n" );

        String resultPathRelative = "Ergebnisse/Berechnet/Bilanz/Bilanz.txt";
        final String inputPath = inputDir.getName() + qgsFiles[i].getName();
        final File resultFile = new File( outputDir, resultPathRelative );
        resultFile.getParentFile().mkdirs();
        FileInputStream FileIS = null;
        try
        {
          FileIS = new FileInputStream( qgsFiles[i] );
        }
        catch( FileNotFoundException e1 )
        {
          e1.printStackTrace();
        }
        try
        {
          FileUtilities.makeFileFromStream( false, resultFile, FileIS );
        }
        catch( IOException e )
        {
          e.printStackTrace();
          System.out.println( "ERR: " + inputPath + " may not exist" );
        }
      }
    }
  }

  private static void copyMetaData( MetadataList srcMeta, MetadataList destMeta, String[] mdKeys )
  {
    for( int i = 0; i < mdKeys.length; i++ )
    {
      final String key = mdKeys[i];
      final String property = srcMeta.getProperty( key );
      if( property != null )
        destMeta.put( key, property );
    }
  }

  private void loadLogs( final File tmpDir, final Logger logger, ISimulationResultEater resultEater )
  {

    try
    {
      resultEater.addResult( NaModelConstants.LOG_EXE_STDOUT_ID, new File( tmpDir, "exe.log" ) );
    }
    catch( SimulationException e )
    {
      e.printStackTrace();
      logger.info( e.getMessage() );
    }
    try
    {
      resultEater.addResult( NaModelConstants.LOG_EXE_ERROUT_ID, new File( tmpDir, "exe.err" ) );
    }
    catch( SimulationException e )
    {
      e.printStackTrace();
      logger.info( e.getMessage() );
    }

    try
    {
      resultEater.addResult( NaModelConstants.LOG_OUTRES_ID, new File( tmpDir, "start/output.res" ) );
    }
    catch( SimulationException e )
    {
      e.printStackTrace();
      logger.info( e.getMessage() );
    }
    try
    {
      resultEater.addResult( NaModelConstants.LOG_OUTERR_ID, new File( tmpDir, "start/output.err" ) );
    }
    catch( SimulationException e )
    {
      e.printStackTrace();
      logger.info( e.getMessage() );
    }

    Handler[] handlers = logger.getHandlers();
    for( int i = 0; i < handlers.length; i++ )
    {
      Handler h = handlers[i];
      h.flush();
      h.close();
    }
    try
    {
      resultEater.addResult( NaModelConstants.LOG_INFO_ID, new File( tmpDir, "infoLog.txt" ) );
    }
    catch( SimulationException e )
    {
      e.printStackTrace();
      logger.info( e.getMessage() );
    }

  }

  private void copyExecutable( File basedir ) throws Exception
  {
    final String exeResource = m_resourceBase + m_kalypsoKernelPath;
    final File destFile = new File( basedir, m_kalypsoKernelPath );
    if( !destFile.exists() )
    {
      try
      {
        final InputStream inputStream = getClass().getResourceAsStream( exeResource );
        FileUtilities.makeFileFromStream( false, destFile, inputStream );
        System.out.println( " ...copied" );
      }
      catch( Exception e )
      {
        e.printStackTrace();

        System.out.println( "ERR: " + exeResource + " max not exist" );
      }
    }
  }

  private void unzipInput( URL asciiZipURL, File exeDir )
  {
    try
    {
      InputStream openStream = asciiZipURL.openStream();
      ZipUtilities.unzip( openStream, exeDir );
      IOUtils.closeQuietly( openStream );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  private void startCalculation( final File basedir, final ISimulationMonitor monitor ) throws SimulationException
  {
    final File exeFile = new File( basedir, m_kalypsoKernelPath );
    final File exeDir = exeFile.getParentFile();
    final String commandString = exeFile.getAbsolutePath();
    long timeOut = 1000l * 60l * 60l; // max 60 minutes
    PrintWriter logWriter = null;
    PrintWriter errorWriter = null;
    try
    {
      logWriter = new PrintWriter( new FileWriter( new File( basedir, "exe.log" ) ) );
      errorWriter = new PrintWriter( new FileWriter( new File( basedir, "exe.err" ) ) );
      ProcessHelper.startProcess( commandString, new String[0], exeDir, monitor, timeOut, logWriter, errorWriter );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new SimulationException( "Fehler beim Ausfuehren", e );
    }
    finally
    {
      IOUtils.closeQuietly( logWriter );
      IOUtils.closeQuietly( errorWriter );
    }
  }

  private void startCalculationOld( final File basedir, final ISimulationMonitor monitor ) throws SimulationException
  {
    InputStreamReader inStream = null;
    InputStreamReader errStream = null;
    PrintWriter outwriter = null;
    PrintWriter errwriter = null;

    try
    {
      final File exeFile = new File( basedir, m_kalypsoKernelPath );
      final File exeDir = exeFile.getParentFile();
      final String commandString = exeFile.getAbsolutePath();

      final Process process = Runtime.getRuntime().exec( commandString, null, exeDir );

      outwriter = new PrintWriter( new FileWriter( new File( basedir, "exe.log" ) ) );
      errwriter = new PrintWriter( new FileWriter( new File( basedir, "exe.err" ) ) );

      inStream = new InputStreamReader( process.getInputStream() );
      errStream = new InputStreamReader( process.getErrorStream() );
      while( true )
      {
        CopyUtils.copy( inStream, outwriter );
        CopyUtils.copy( errStream, errwriter );

        try
        {
          process.exitValue();
          return;
        }
        catch( IllegalThreadStateException e )
        {
          // noch nicht fertig
        }

        if( monitor.isCanceled() )
        {
          process.destroy();
          return;
        }
        Thread.sleep( 100 );
      }
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      throw new SimulationException( "Fehler beim Ausfuehren", e );
    }
    catch( final InterruptedException e )
    {
      e.printStackTrace();
      throw new SimulationException( "Fehler beim Ausfuehren", e );
    }
    finally
    {
      try
      {
        if( outwriter != null )
          outwriter.close();

        if( errwriter != null )
          errwriter.close();

        if( inStream != null )
          inStream.close();

        if( errStream != null )
          errStream.close();
      }
      catch( final IOException e1 )
      {
        e1.printStackTrace();
      }
    }
  }

  public boolean isSucceeded( )
  {
    return m_succeeded;
  }

}