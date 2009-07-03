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
import java.io.LineNumberReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.Writer;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.logging.Logger;

import javax.xml.bind.Marshaller;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.java.io.FileCopyVisitor;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.lang.ProcessHelper;
import org.kalypso.commons.java.net.UrlResolver;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.commons.lhwz.LhwzHelper;
import org.kalypso.contribs.java.io.filter.MultipleWildCardFileFilter;
import org.kalypso.contribs.java.net.UrlUtilities;
import org.kalypso.convert.namodel.manager.CatchmentManager;
import org.kalypso.convert.namodel.manager.IDManager;
import org.kalypso.convert.namodel.optimize.CalibarationConfig;
import org.kalypso.convert.namodel.optimize.NAOptimizingJob;
import org.kalypso.convert.namodel.timeseries.BlockTimeSeries;
import org.kalypso.convert.namodel.timeseries.DummyTimeSeriesWriter;
import org.kalypso.convert.namodel.timeseries.NATimeSettings;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IAxisRange;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.ObservationConstants;
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
import org.kalypso.services.calculation.job.ICalcDataProvider;
import org.kalypso.services.calculation.job.ICalcJob;
import org.kalypso.services.calculation.job.ICalcMonitor;
import org.kalypso.services.calculation.job.ICalcResultEater;
import org.kalypso.services.calculation.service.CalcJobServiceException;
import org.kalypso.zml.ObservationType;
import org.kalypso.zml.obslink.TimeseriesLink;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.gml.schema.XMLHelper;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.GMLWorkspace_Impl;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;
import org.opengis.cs.CS_CoordinateSystem;
import org.w3c.dom.Document;

/**
 * @author doemming, huebsch
 */
public class NaModelInnerCalcJob implements ICalcJob
{

  // resourcebase for static files used in calculation
  private final String m_resourceBase = "template/";

  //  private final String EXE_FILE_WEISSE_ELSTER = "start/kalypso_WeisseElster.exe";
  private final String EXE_FILE_WEISSE_ELSTER = "start/kalypso_2.0.1a.exe";

  private final String EXE_FILE_2_02 = "start/kalypso_2.02.exe";

  private final String EXE_FILE_2_03beta = "start/kalypso_2.0.3beta.exe";

  private boolean m_succeeded = false;

  private String m_kalypsoKernelPath = EXE_FILE_WEISSE_ELSTER;

  final HashMap m_resultMap = new HashMap();

  private static final String WE_RESOURCE_HYDROTOP_GML = "resources/WE/hydrotop.gml";

  private static final String WE_PARAMETER_GML = "resources/WE/parameter.gml";

  public NaModelInnerCalcJob()
  {
    m_urlUtilities = new UrlUtilities();
  }

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#getSpezifikation()
   */
  public URL getSpezifikation()
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
  public void run( File tmpdir, ICalcDataProvider inputProvider, ICalcResultEater resultEater, ICalcMonitor monitor )
      throws CalcJobServiceException
  {
    final Logger logger = Logger.getAnonymousLogger();
    File infoFile = new File( tmpdir, "infolog.txt" );
    FileWriter writer = null;
    try
    {
      writer = new FileWriter( infoFile );
      final Date date = new Date( Calendar.getInstance().getTimeInMillis() );
      writer.write( "Zeitpunkt Start Berechnung: " + date.toString() + " (Serverzeit)\n" );
    }
    catch( IOException e1 )
    {
      logger.fine( e1.getLocalizedMessage() );
    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }
    try
    {
      resultEater.addResult( NaModelConstants.LOG_INFO_ID, infoFile );
    }
    catch( CalcJobServiceException e )
    {
      e.printStackTrace();
    }
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
      unzipTemplates( inputProvider.getURLForID( NaModelConstants.IN_TEMPLATE_ID ), tmpdir );
      URL hydrotopURL = null;
      if( inputProvider.hasID( NaModelConstants.IN_HYDROTOP_ID ) )
        hydrotopURL = inputProvider.getURLForID( NaModelConstants.IN_HYDROTOP_ID );
      else
        hydrotopURL = getClass().getResource( WE_RESOURCE_HYDROTOP_GML );

      // performance
      if( inputProvider.hasID( NAOptimizingJob.IN_BestOptimizedRunDir_ID ) )
      {
        // while optimization, you can recycle files from a former run.
        // implement here to copy the files to your tmp dir and while generating
        // files you should check if files allready exist, and on your option do
        // not generate them.
        // WARNING: never use result files or files that vary during
        // optimization.
        final URL url = inputProvider.getURLForID( NAOptimizingJob.IN_BestOptimizedRunDir_ID );
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

        final File from3 = new File( url.getFile(), "hydro.top" );
        final File to3 = new File( tmpdir, "hydro.top" );
        if( from3.exists() && to3.exists() )
        {
          final FileCopyVisitor copyVisitor = new FileCopyVisitor( from3, to3, true );
          FileUtilities.accept( from3, copyVisitor, true );
        }
        hydrotopURL = null; // this will recycle the hydrotops
      }

      // generiere ascii-dateien
      monitor.setMessage( "generiere ASCII-Dateien (Modelldaten und Zeitreihen)" );
      if( monitor.isCanceled() )
        return;

      final File newModellFile = new File( tmpdir, "namodellBerechnung.gml" );

      // calualtion model

      final NAConfiguration conf = NAConfiguration.getGml2AsciiConfiguration( newModellFile.toURL(), tmpdir );

      final GMLWorkspace modellWorkspace = generateASCII( conf, tmpdir, inputProvider, newModellFile, hydrotopURL );
      final URL naControlURL = inputProvider.getURLForID( NaModelConstants.IN_CONTROL_ID );
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
      throw new CalcJobServiceException( "Simulation konnte nicht durchgefuehrt werden", e );
    }
  }

  /**
   * @param naControlWorkspace
   * @param logger
   * @param resultDir
   * @param conf
   * @throws Exception
   */
  private void loadTesultTSPredictionIntervals( final GMLWorkspace naControlWorkspace, final Logger logger,
      final File resultDir, final NAConfiguration conf ) throws Exception
  {
    // Load the calculated prediction
    final Feature rootFeature = naControlWorkspace.getRootFeature();
    final IObservation resultObservation = loadPredictedResult( resultDir, rootFeature );
    final IAxis[] axisList = resultObservation.getAxisList();
    final String axisType = determineTranpolinAxis( resultObservation );

    final File fileMitte = getResultFileFor( resultDir, rootFeature, "qAblageSpurMittlerer" );
    final File fileUnten = getResultFileFor( resultDir, rootFeature, "qAblageSpurUnterer" );
    final File fileOben = getResultFileFor( resultDir, rootFeature, "qAblageSpurOberer" );

    // Initalize some commen variables
    final ITuppleModel resultValues = resultObservation.getValues( null );
    final IAxis resultDateAxis = ObservationUtilities.findAxisByClass( axisList, Date.class );
    final IAxis resultValueAxis = ObservationUtilities.findAxisByType( axisList, axisType );

    final Date startForecast = conf.getSimulationForecastStart();
    final Date endForecast = conf.getSimulationEnd();

    final IAxisRange rangeFor = resultValues.getRangeFor( resultDateAxis );
    final Date endPrediction = (Date)rangeFor.getUpper();

    final NATimeSettings timeSettings = NATimeSettings.getInstance();
    final Calendar calBegin = timeSettings.getCalendar( startForecast );
    // REMARK: using endPrediction instead of endForecast, as they are not equals (but they should...)
    final Calendar calEnd = timeSettings.getCalendar( endPrediction );

    final double calcStartValue = ObservationUtilities.getInterpolatedValueAt( resultValues, resultDateAxis,
        resultValueAxis, startForecast );
    final double calcEndValue = ObservationUtilities.getInterpolatedValueAt( resultValues, resultDateAxis,
        resultValueAxis, endForecast );

    //
    // First, we adapt the result: correction at start and/or end of the calculated timeserie
    //

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
      final double measureValue = ObservationUtilities.getInterpolatedValueAt( pegelValues, pegelDateAxis,
          pegelValueAxis, startForecast );
      deltaMeasureCalculation = measureValue - calcStartValue;
    }
    catch( Exception e )
    {
      deltaMeasureCalculation = 0;
    }

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

    final Calendar tranpolinEnd = timeSettings.getCalendar( startForecast );
    tranpolinEnd.add( Calendar.HOUR, 24 );

    final IRequest request = new ObservationRequest(calBegin.getTime(), calEnd.getTime());
    
    TranProLinFilterUtilities.transformAndWrite( resultObservation, calBegin, tranpolinEnd, offsetStartPrediction,
        offsetEndPrediction, "+", axisType, KalypsoStati.BIT_DERIVATED, fileMitte, " - Spur Mitte", request );

    // read the freshly created file into a new observation, we are going to umhüll it
    IObservation adaptedResultObservation = ZmlFactory.parseXML( fileMitte.toURL(), "adaptedVorhersage" );

    //
    // Second, we build the umhüllenden for the adapted result
    //
    double accuracyPrediction = LhwzHelper.getDefaultUmhuellendeAccuracy();
    final Double featureAccuracy = (Double)rootFeature.getProperty( "accuracyPrediction" );
    if( featureAccuracy == null )
      logger.info( "Genauigkeit für Umhüllende ist nicht angegeben. Für die Vorhersage wird " + accuracyPrediction
          + " [%/60h] als Vorhersagegenauigkeit angenommen." );
    else
      accuracyPrediction = featureAccuracy.doubleValue();

    //  accuracyPrediction // %/60h
    final long millisOf60hours = 1000 * 60 * 60 * 60;
    // endAccuracy: %/simulationRange
    final double endAccuracy = accuracyPrediction
        * ( ( (double)( endForecast.getTime() - startForecast.getTime() ) ) / ( (double)millisOf60hours ) );

    final double endOffset = calcEndValue * ( endAccuracy / 100 );

    TranProLinFilterUtilities.transformAndWrite( adaptedResultObservation, calBegin, calEnd, 0, endOffset, "-",
        axisType, KalypsoStati.BIT_DERIVATED, fileUnten, " - spur Unten", request );
    TranProLinFilterUtilities.transformAndWrite( adaptedResultObservation, calBegin, calEnd, 0, endOffset, "+",
        axisType, KalypsoStati.BIT_DERIVATED, fileOben, " - spur Oben", request );
  }

  /**
   * Return with which axis we are going to transform the umhüllenden. Q or W, depending on what is present)
   */
  private String determineTranpolinAxis( final IObservation observation ) throws CalcJobServiceException
  {
    final IAxis[] axisList = observation.getAxisList();

    if( ObservationUtilities.hasAxisOfType( axisList, TimeserieConstants.TYPE_RUNOFF ) )
      return TimeserieConstants.TYPE_RUNOFF;

    if( ObservationUtilities.hasAxisOfType( axisList, TimeserieConstants.TYPE_WATERLEVEL ) )
      return TimeserieConstants.TYPE_WATERLEVEL;

    throw new CalcJobServiceException(
        "Ergebniszeitreihe enthält weder Abfluss noch Waserstand, Umhüllendenberechnung nicht möglich.", null );
  }

  private IObservation loadPredictedResult( final File resultDir, final Feature rootFeature )
      throws MalformedURLException, SensorException
  {
    final TimeseriesLink resultLink = (TimeseriesLink)rootFeature.getProperty( "qberechnetZR" );

    // from predicted timeseries
    final UrlResolver urlResolver = new UrlResolver();
    final URL resultURL = urlResolver.resolveURL( resultDir.toURL(), resultLink.getHref() );
    return ZmlFactory.parseXML( resultURL, "vorhersage" );
  }

  private File getResultFileFor( final File resultDir, final Feature feature, final String tsLinkPropName )
  {
    try
    {
      final TimeseriesLink trackLink = (TimeseriesLink)feature.getProperty( tsLinkPropName );
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
      while( ( line = reader.readLine() ) != null )
      {
        if( line.indexOf( "berechnung wurde ohne fehler beendet" ) >= 0
            || line.indexOf( "Berechnung wurde ohne Fehler beendet!" ) >= 0 )
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

  private GMLWorkspace generateASCII( NAConfiguration conf, File tmpDir, ICalcDataProvider dataProvider,
      final File newModellFile, final URL hydrotopURL ) throws Exception
  {
    //    final File newModellFile = new File( tmpDir, "namodellBerechnung.gml" );
    //
    //    // calualtion model
    //    final URL newModellURL = newModellFile.toURL();
    //    final NAConfiguration conf = NAConfiguration.getGml2AsciiConfiguration( newModellURL, tmpDir );

    final GMLWorkspace metaWorkspace = GmlSerializer.createGMLWorkspace( dataProvider
        .getURLForID( NaModelConstants.IN_META_ID ) );
    final Feature metaFE = metaWorkspace.getRootFeature();
    final GMLWorkspace controlWorkspace = GmlSerializer.createGMLWorkspace( dataProvider
        .getURLForID( NaModelConstants.IN_CONTROL_ID ) );

    //model Parameter
    final GMLWorkspace parameterWorkspace;
    if( dataProvider.hasID( NaModelConstants.IN_PARAMETER_ID ) )
      parameterWorkspace = GmlSerializer.createGMLWorkspace( dataProvider
          .getURLForID( NaModelConstants.IN_PARAMETER_ID ) );
    else
      parameterWorkspace = GmlSerializer.createGMLWorkspace( getClass().getResource( WE_PARAMETER_GML ) );

    // initialize model with values of control file
    initializeModell( controlWorkspace.getRootFeature(), dataProvider.getURLForID( NaModelConstants.IN_MODELL_ID ),
        newModellFile );

    final GMLWorkspace modellWorkspace = GmlSerializer.createGMLWorkspace( newModellFile.toURL() );
    ( (GMLWorkspace_Impl)modellWorkspace ).setContext( dataProvider.getURLForID( NaModelConstants.IN_MODELL_ID ) );

    final NaNodeResultProvider nodeResultProvider = new NaNodeResultProvider( modellWorkspace, controlWorkspace );
    conf.setNodeResultProvider( nodeResultProvider );
    updateModelWithExtraVChannel( modellWorkspace, nodeResultProvider );

    //  model Hydrotop
    final GMLWorkspace hydrotopWorkspace;

    if( hydrotopURL != null )
    {
      hydrotopWorkspace = GmlSerializer.createGMLWorkspace( hydrotopURL );
      final Feature[] hydroFES = hydrotopWorkspace.getFeatures( hydrotopWorkspace.getFeatureType( "Hydrotop" ) );
      CS_CoordinateSystem targetCS = null;
      for( int i = 0; i < hydroFES.length && targetCS == null; i++ )
      {
        final GM_Object geom = (GM_Object)hydroFES[i].getProperty( "position" );
        if( geom != null && geom.getCoordinateSystem() != null )
        {
          targetCS = geom.getCoordinateSystem();
          break;
        }
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
    conf.setSimulationStart( (Date)metaFE.getProperty( "startsimulation" ) );
    conf.setSzenarioID( (String)metaFE.getProperty( "scenarioId" ) );
    // start forecast
    final Date startForecastDate = (Date)metaFE.getProperty( "startforecast" );
    conf.setSimulationForecasetStart( startForecastDate );
    // end of simulation
    int hoursForecast = 0; // default length of forecast hours
    final Integer hoursOfForecast = (Integer)metaFE.getProperty( "hoursforecast" );
    if( hoursOfForecast != null )
      hoursForecast = hoursOfForecast.intValue();
    final Calendar c = Calendar.getInstance();
    c.setTime( startForecastDate );
    c.add( Calendar.HOUR, hoursForecast );
    final Date endDate = c.getTime();
    conf.setSimulationEnd( endDate );

    // calculate timestep
    int minutesTimeStep = 60;
    final Integer minutesOfTimeStep = (Integer)metaFE.getProperty( "minutesTimestep" );
    if( minutesOfTimeStep != null )
      minutesTimeStep = minutesOfTimeStep.intValue() != 0 ? minutesOfTimeStep.intValue() : 60;
    conf.setMinutesOfTimeStep( minutesTimeStep );

    // choose simulation kernel
    chooseSimulationExe( (String)metaFE.getProperty( "versionKalypsoNA" ) );

    // set rootnode
    conf.setRootNodeID( (String)controlWorkspace.getRootFeature().getProperty( "rootNode" ) );

    // generate control files
    NAControlConverter.featureToASCII( conf, tmpDir, controlWorkspace, modellWorkspace );

    // update model with factor values from control
    updateFactorParameter( modellWorkspace );

    // generate modell files
    NAModellConverter main = new NAModellConverter( conf );
    main.write( modellWorkspace, parameterWorkspace, hydrotopWorkspace, nodeResultProvider );

    // create temperatur und verdunstung timeseries
    final File klimaDir = new File( tmpDir, "klima.dat" );
    final File tempFile = new File( klimaDir, CatchmentManager.STD_TEMP_FILENAME );
    final File verdFile = new File( klimaDir, CatchmentManager.STD_VERD_FILENAME );
    final DummyTimeSeriesWriter writer = new DummyTimeSeriesWriter( conf.getSimulationStart(), conf.getSimulationEnd() );

    if( !tempFile.exists() )
    {
      writer.writeTmpFile( tempFile );
    }
    if( !verdFile.exists() )
    {
      writer.writeVerdFile( verdFile );
    }
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
  private void updateModelWithExtraVChannel( final GMLWorkspace workspace, final NaNodeResultProvider nodeResultProvider )
      throws Exception
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
    final FeatureType kontEntnahmeFT = workspace.getFeatureType( "KontEntnahme" );
    final FeatureType ueberlaufFT = workspace.getFeatureType( "Ueberlauf" );
    final FeatureType verzweigungFT = workspace.getFeatureType( "Verzweigung" );
    final FeatureType nodeFT = workspace.getFeatureType( "Node" );
    final Feature[] features = workspace.getFeatures( nodeFT );
    for( int i = 0; i < features.length; i++ )
    {
      final Feature nodeFE = features[i];
      final Feature branchingFE = workspace.resolveLink( nodeFE, "branchingMember" );
      if( branchingFE != null )
      {
        final FeatureType branchFT = branchingFE.getFeatureType();
        if( branchFT == kontEntnahmeFT || branchFT == ueberlaufFT || branchFT == verzweigungFT )
        {
          final Feature targetNodeFE = workspace.resolveLink( branchingFE, "branchingNodeMember" );
          final Feature newNodeFE = buildVChannelNet( workspace, targetNodeFE );
          workspace.setFeatureAsComposition( branchingFE, "branchingNodeMember", newNodeFE, true );
        }
      }
    }
  }

  /**
   * @param workspace
   * @throws Exception
   */
  private void updateResultAsZuflussNet( final GMLWorkspace workspace, final NaNodeResultProvider nodeResultprovider )
      throws Exception
  {
    final FeatureType nodeFT = workspace.getFeatureType( "Node" );
    final FeatureType abstractChannelFT = workspace.getFeatureType( "_Channel" );
    final Feature[] features = workspace.getFeatures( nodeFT );
    for( int i = 0; i < features.length; i++ )
    {
      final Feature nodeFE = features[i];
      if( nodeResultprovider.resultExists( nodeFE ) )
      {
        final Object resultValue = nodeFE.getProperty( "qberechnetZR" );
        // disconnect everything upstream (channel -> node)
        final Feature[] channelFEs = workspace.resolveWhoLinksTo( nodeFE, abstractChannelFT, "downStreamNodeMember" );
        for( int j = 0; j < channelFEs.length; j++ )
        {
          final Feature newEndNodeFE = workspace.createFeature( nodeFT );
          workspace.setFeatureAsComposition( channelFEs[j], "downStreamNodeMember", newEndNodeFE, true );
        }
        // add as zufluss
        final Feature newNodeFE = buildVChannelNet( workspace, nodeFE );
        final FeatureProperty zuflussProp = FeatureFactory.createFeatureProperty( "zuflussZR", resultValue );
        newNodeFE.setProperty( zuflussProp );
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
   * </code>
   * 
   * constant inflow <br>
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

    final FeatureType kontZuflussFT = workspace.getFeatureType( "KontZufluss" );
    final FeatureType nodeFT = workspace.getFeatureType( "Node" );
    final Feature[] features = workspace.getFeatures( nodeFT );
    for( int i = 0; i < features.length; i++ )
    {
      final Feature nodeFE = features[i];
      final Object zuflussValue = nodeFE.getProperty( "zuflussZR" );
      if( zuflussValue != null )
      {
        // update zufluss
        Feature newNode = buildVChannelNet( workspace, nodeFE );
        // nove zufluss-property to new node
        nodeFE.setProperty( FeatureFactory.createFeatureProperty( "zuflussZR", null ) );
        newNode.setProperty( FeatureFactory.createFeatureProperty( "zuflussZR", zuflussValue ) );
      }
      final Feature branchingFE = workspace.resolveLink( nodeFE, "branchingMember" );
      if( branchingFE != null && branchingFE.getFeatureType() == kontZuflussFT )
      {
        // update zufluss
        Feature newNode = buildVChannelNet( workspace, nodeFE );
        // nove constant-inflow to new node
        workspace.setFeatureAsComposition( nodeFE, "branchingMember", null, true );
        workspace.setFeatureAsComposition( newNode, "branchingMember", branchingFE, true );
      }
    }
  }

  private Feature buildVChannelNet( final GMLWorkspace workspace, final Feature existingNode ) throws Exception
  {
    final FeatureType nodeFT = workspace.getFeatureType( "Node" );
    final FeatureType vChannelFT = workspace.getFeatureType( "VirtualChannel" );
    final Feature channelColFE = workspace.getFeatures( workspace.getFeatureType( "ChannelCollection" ) )[0];
    final Feature nodeColFE = workspace.getFeatures( workspace.getFeatureType( "NodeCollection" ) )[0];

    final Feature newChannelFE1 = workspace.createFeature( vChannelFT );
    workspace.addFeatureAsComposition( channelColFE, "channelMember", 0, newChannelFE1 );
    final Feature newChannelFE3 = workspace.createFeature( vChannelFT );
    workspace.addFeatureAsComposition( channelColFE, "channelMember", 0, newChannelFE3 );
    final Feature newNodeFE2 = workspace.createFeature( nodeFT );
    workspace.addFeatureAsComposition( nodeColFE, "nodeMember", 0, newNodeFE2 );

    // 3 -> 2
    workspace.setFeatureAsAggregation( newChannelFE3, "downStreamNodeMember", newNodeFE2.getId(), true );
    // 2 -> 1
    workspace.setFeatureAsAggregation( newNodeFE2, "downStreamChannelMember", newChannelFE1.getId(), true );
    // 1 -> existing
    workspace.setFeatureAsAggregation( newChannelFE1, "downStreamNodeMember", existingNode.getId(), true );
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
    final FeatureType catchmentFT = workspace.getFeatureType( "Catchment" );
    final FeatureType vChannelFT = workspace.getFeatureType( "VirtualChannel" );
    final Feature[] features = workspace.getFeatures( catchmentFT );
    for( int i = 0; i < features.length; i++ )
    {
      final Feature catchmentFE = features[i];
      final Feature orgChannelFE = workspace.resolveLink( catchmentFE, "entwaesserungsStrangMember" );
      if( orgChannelFE == null )
        continue;
      final Feature nodeFE = workspace.resolveLink( orgChannelFE, "downStreamNodeMember" );
      final Feature newChannelFE = workspace.createFeature( vChannelFT );
      // set new relation: catchment -> new V-channel
      try
      {
        workspace.setFeatureAsComposition( catchmentFE, "entwaesserungsStrangMember", newChannelFE, true );
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
      // set new relation: new V-channel -> downstream node
      try
      {
        workspace.addFeatureAsAggregation( newChannelFE, "downStreamNodeMember", 1, nodeFE.getId() );
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
      m_kalypsoKernelPath = EXE_FILE_2_03beta;
    else if( kalypsoNAVersion.equals( "neueste" ) || kalypsoNAVersion.equals( "neuste" )
        || kalypsoNAVersion.equals( "latest" ) )
      m_kalypsoKernelPath = EXE_FILE_2_03beta;
    else
      m_kalypsoKernelPath = EXE_FILE_WEISSE_ELSTER;
  }

  private void initializeModell( Feature controlFeature, URL inputModellURL, File outputModelFile ) throws IOException,
      Exception
  {
    CalibarationConfig config = new CalibarationConfig();
    config.addFromNAControl( controlFeature );
    Document modelDoc = XMLHelper.getAsDOM( inputModellURL, true );

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
  private final static String[][] m_catchmentFactorsParameter =
  {
      new String[]
      {
          "retob",
          "faktorRetobRetint" },
      new String[]
      {
          "retint",
          "faktorRetobRetint" },
      new String[]
      {
          "aigw",
          "faktorAigw" } };

  private static String[] m_catchmentFactorParameterTarget =
  {
      "retob",
      "retint",
      "aigw" };

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
      List kmParameter = (List)feature.getProperty( "KMParameterMember" );
      Iterator iterator = kmParameter.iterator();
      while( iterator.hasNext() )
      {
        final Feature kmParameterFE = (Feature)iterator.next();
        // rnf
        final double _rnf = rnfFactor * FeatureHelper.getAsDouble( kmParameterFE, "rnf", 1.0 );
        FeatureProperty rnfProp = FeatureFactory.createFeatureProperty( "rnf", new Double( _rnf ) );
        kmParameterFE.setProperty( rnfProp );
        // rkf
        final double _rkf = rkfFactor * FeatureHelper.getAsDouble( kmParameterFE, "rkf", 1.0 );
        FeatureProperty rkfProp = FeatureFactory.createFeatureProperty( "rkf", new Double( _rkf ) );
        kmParameterFE.setProperty( rkfProp );
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
        FeatureProperty valueProp = FeatureFactory.createFeatureProperty( targetPropName, new Double( value ) );
        feature.setProperty( valueProp );
      }
    }
  }

  private void loadResults( final File tmpdir, final GMLWorkspace modellWorkspace,
      final GMLWorkspace naControlWorkspace, final Logger logger, final File resultDir, ICalcResultEater resultEater,
      final NAConfiguration conf ) throws Exception
  {
    loadTSResults( tmpdir, modellWorkspace, logger, resultDir, conf );
    try
    {
      loadTesultTSPredictionIntervals( naControlWorkspace, logger, resultDir, conf );
    }
    catch( Exception e )
    {
      logger.info( "konnte Umhüllende nicht generieren, evt. keine WQ-Informationen vorhanden , Fehler: "
          + e.getLocalizedMessage() );
    }
    loadTextFileResults( tmpdir, logger, resultDir );
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

  private String getTitleForSuffix( String suffix )
  {
    //    j Temperatur .tmp
    if( suffix.equalsIgnoreCase( "tmp" ) )
      return "Temperatur TG";
    //    j Niederschlag .pre
    if( suffix.equalsIgnoreCase( "pre" ) )
      return "Niederschlag TG";
    //    n Schnee .sch
    if( suffix.equalsIgnoreCase( "sch" ) )
      return "Schneehöhe TG";
    //    j Bodenfeuchte .bof
    if( suffix.equalsIgnoreCase( "bof" ) )
      return "Bodenfeuchte";
    //    n Bodenspeicher .bsp
    if( suffix.equalsIgnoreCase( "bsp" ) )
      return "Bodenspeicherbilanz";
    //    n Grundwasserstand .gws
    if( suffix.equalsIgnoreCase( "gws" ) )
      return "Grundwasserstand TG";
    //    j Gesamtabfluss Knoten .qgs
    if( suffix.equalsIgnoreCase( "qgs" ) )
      return "Gesamtabfluss K";
    //    n Gesamtabfluss TG .qgg
    if( suffix.equalsIgnoreCase( "qgg" ) )
      return "Gesamtabfluss TG";
    //    n Oberflaechenabfluss .qna
    if( suffix.equalsIgnoreCase( "qna" ) )
      return "Oberflaechenabfluss nat. Flaechen TG";
    //    n Interflow .qif
    if( suffix.equalsIgnoreCase( "qif" ) )
      return "Interflow TG";
    //    n Abfluss vers. Flaechen .qvs
    if( suffix.equalsIgnoreCase( "qvs" ) )
      return "Abfluss vers. Flaechen TG";
    //    n Basisabfluss .qbs
    if( suffix.equalsIgnoreCase( "qbs" ) )
      return "Basisabfluss TG";
    //    n Kluftgrundw1 .qt1
    if( suffix.equalsIgnoreCase( "qt1" ) )
      return "Abfluss Kluftgrundw1";
    //    n Kluftgrundw .qtg
    if( suffix.equalsIgnoreCase( "qtg" ) )
      return "Abfluss KluftGW";
    //    n Grundwasser .qgw
    if( suffix.equalsIgnoreCase( "qgw" ) )
      return "Grundwasserabfluss TG";
    //    n Evapotranspiration .vet
    if( suffix.equalsIgnoreCase( "vet" ) )
      return "Evapotranspiration";
    //    n Ausgabe hydrotope .hyd
    if( suffix.equalsIgnoreCase( "hyd" ) )
      return "Ausgabe Hydrotope";
    //    n Abflussbilanz .bil
    if( suffix.equalsIgnoreCase( "bil" ) )
      return "Abflussbilanz";
    //    n Statistische Abflusswerte .nmq
    if( suffix.equalsIgnoreCase( "nmq" ) )
      return "Statistische Abflusswerte";
    //    n Speicherinhalt .spi
    if( suffix.equalsIgnoreCase( "spi" ) )
      return "Füllvolumen";
    //    n Wasserspiegelhöhe .sph
    if( suffix.equalsIgnoreCase( "sph" ) )
      return "Wasserspiegelhöhe";
    //    n Verdunstung aus Talsperre .spv
    if( suffix.equalsIgnoreCase( "spv" ) )
      return "Talsperrenverdunstung";
    //    n Niederschlag in Talsperre .spn
    if( suffix.equalsIgnoreCase( "spn" ) )
      return "Niederschlag";
    //    n Zehrung .spb
    if( suffix.equalsIgnoreCase( "spb" ) )
      return "Zehrung";
    //    n Speicherueberlauf .sup
    if( suffix.equalsIgnoreCase( "sup" ) )
      return "Speicherueberlauf";
    //    n Kapil.Aufstieg/Perkolation .kap - not available, because not used in the calculation core
    //    if( suffix.equalsIgnoreCase( "kap" ) )
    //      return "Kapil.Aufstieg/Perkolation";
    return suffix;
  }

  private void loadTSResults( final File inputDir, final GMLWorkspace modellWorkspace, final Logger logger,
      final File outputDir, final NAConfiguration conf ) throws Exception
  {
    //    j Gesamtabfluss Knoten .qgs
    final FeatureType nodeFT = modellWorkspace.getFeatureType( "Node" );
    loadTSResults( "qgs", nodeFT, "name", TimeserieConstants.TYPE_RUNOFF, "pegelZR", "qberechnetZR", inputDir,
        modellWorkspace, logger, outputDir, 1.0d, conf );

    final FeatureType catchmentFT = modellWorkspace.getFeatureType( "Catchment" );
    final FeatureType rhbChannelFT = modellWorkspace.getFeatureType( "StorageChannel" );
    //    j Niederschlag .pre
    loadTSResults( "pre", catchmentFT, "name", TimeserieConstants.TYPE_RAINFALL, null, null, inputDir, modellWorkspace,
        logger, outputDir, 1.0d, conf );
    //    j Temperatur .tmp
    loadTSResults( "tmp", catchmentFT, "name", TimeserieConstants.TYPE_TEMPERATURE, null, null, inputDir,
        modellWorkspace, logger, outputDir, 1.0d, conf );
    //    n Interflow .qif
    loadTSResults( "qif", catchmentFT, "name", TimeserieConstants.TYPE_RUNOFF, null, null, inputDir, modellWorkspace,
        logger, outputDir, 1.0d, conf );
    //    n Grundwasser .qgw
    loadTSResults( "qgw", catchmentFT, "name", TimeserieConstants.TYPE_RUNOFF, null, null, inputDir, modellWorkspace,
        logger, outputDir, 1.0d, conf );
    //    n Gesamtabfluss TG .qgg
    loadTSResults( "qgg", catchmentFT, "name", TimeserieConstants.TYPE_RUNOFF, null, null, inputDir, modellWorkspace,
        logger, outputDir, 1.0d, conf );
    //    n Grundwasserstand .gws - Umrechnung von m auf cm
    loadTSResults( "gws", catchmentFT, "name", TimeserieConstants.TYPE_WATERLEVEL, null, null, inputDir,
        modellWorkspace, logger, outputDir, 100.0d, conf );
    //    n Basisabfluss .qbs
    loadTSResults( "qbs", catchmentFT, "name", TimeserieConstants.TYPE_RUNOFF, null, null, inputDir, modellWorkspace,
        logger, outputDir, 1.0d, conf );
    //    n Oberflaechenabfluss .qna
    loadTSResults( "qna", catchmentFT, "name", TimeserieConstants.TYPE_RUNOFF, null, null, inputDir, modellWorkspace,
        logger, outputDir, 1.0d, conf );
    //    n Abfluss vers. Flaechen .qvs
    loadTSResults( "qvs", catchmentFT, "name", TimeserieConstants.TYPE_RUNOFF, null, null, inputDir, modellWorkspace,
        logger, outputDir, 1.0d, conf );
    //TODO:check output for the next time series
    //    n Schnee .sch [mm]
    loadTSResults( "sch", catchmentFT, "name", TimeserieConstants.TYPE_WATERLEVEL, null, null, inputDir,
        modellWorkspace, logger, outputDir, 0.1d, conf );
    //    n Kluftgrundw1 .qt1
    loadTSResults( "qt1", catchmentFT, "name", TimeserieConstants.TYPE_RUNOFF, null, null, inputDir, modellWorkspace,
        logger, outputDir, 1.0d, conf );
    //    n Kluftgrundw .qtg
    loadTSResults( "qtg", catchmentFT, "name", TimeserieConstants.TYPE_RUNOFF, null, null, inputDir, modellWorkspace,
        logger, outputDir, 1.0d, conf );
    //    n Evapotranspiration .vet [mm]
    loadTSResults( "vet", catchmentFT, "name", TimeserieConstants.TYPE_WATERLEVEL, null, null, inputDir,
        modellWorkspace, logger, outputDir, 0.1d, conf );
    //    j Bodenfeuchte .bof [mm]
    loadTSResults( "bof", catchmentFT, "name", TimeserieConstants.TYPE_WATERLEVEL, null, null, inputDir,
        modellWorkspace, logger, outputDir, 0.1d, conf );

    //Straenge
    //    n Wasserstand Speicher .sph [muNN]
    loadTSResults( "sph", rhbChannelFT, "name", TimeserieConstants.TYPE_NORMNULL, null, null, inputDir,
        modellWorkspace, logger, outputDir, 1.0d, conf );
    //    n Speicherueberlauf .sup [m³/s]
    loadTSResults( "sup", rhbChannelFT, "name", TimeserieConstants.TYPE_RUNOFF, null, null, inputDir, modellWorkspace,
        logger, outputDir, 1.0d, conf );
    //        n Speicherinhalt .spi [hm³]
    loadTSResults( "spi", rhbChannelFT, "name", TimeserieConstants.TYPE_VOLUME, null, null, inputDir, modellWorkspace,
        logger, outputDir, 1.0d, conf );
    //    n Talsperrenverdunstung .spv [m³/d]
    //    loadTSResults( "spv", catchmentFT, "name", TimeserieConstants.TYPE_RUNOFF, null, null, inputDir,
    //        modellWorkspace, logger, outputDir, 1.0d , idManager);
    //    n Zehrung .spn [m³/d]
    //    loadTSResults( "spn", catchmentFT, "name", TimeserieConstants.TYPE_RUNOFF, null, null, inputDir,
    //        modellWorkspace, logger, outputDir, 1.0d , idManager);

    //    n Kapil.Aufstieg/Perkolation .kap [mm]
    //    loadTSResults( "kap", catchmentFT, "name", TimeserieConstants.TYPE_WATERLEVEL, null, null, inputDir,
    //        modellWorkspace, logger, outputDir, 0.1d , idManager);
    //    n Ausgabe hydrotope .hyd
    //    loadTSResults( "hyd", catchmentFT, "name", TimeserieConstants.TYPE_WATERLEVEL, null, null, inputDir,
    //        modellWorkspace, logger, outputDir, 1.0d , idManager);
  }

  private void loadTSResults( final String suffix, final FeatureType resultFT, final String titlePropName,
      final String resultType, final String metadataTSLink, final String targetTSLink, final File inputDir,
      final GMLWorkspace modellWorkspace, final Logger logger, final File outputDir, final double resultFactor,
      final NAConfiguration conf ) throws Exception
  {
    final IDManager idManager = conf.getIdManager();
    // ASCII-Files
    // generiere ZML Ergebnis Dateien
    final File ascciResultDir = new File( inputDir, "out_we.nat" );
    final MultipleWildCardFileFilter filter = new MultipleWildCardFileFilter( new String[]
    { "*" + suffix + "*" }, false, false, true );
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
        if( resultFT == ( modellWorkspace.getFeatureType( "Node" ) )
            || resultFT == ( modellWorkspace.getFeatureType( "Catchment" ) ) )
        {
          if( !FeatureHelper.booleanIsTrue( feature, "generateResult", false ) )
            continue; // should not generate results
        }
        //        final String lang = KalypsoGisPlugin.getDefault().getPluginPreferences().getString(
        //            IKalypsoPreferences.LANGUAGE );
        final String lang = "de";
        final String annotation = feature.getFeatureType().getAnnotation( lang ).getLabel();
        final String key = Integer.toString( idManager.getAsciiID( feature ) );
        final String feName = (String)feature.getProperty( titlePropName );
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
          Map.Entry entry = (Map.Entry)iter.next();
          tupelData[pos][0] = (Date)entry.getKey();
          tupelData[pos][1] = new Double( Double.parseDouble( entry.getValue().toString() ) * resultFactor );
          tupelData[pos][2] = new Integer( KalypsoStati.BIT_OK );
          pos++;
        }

        final IAxis dateAxis = new DefaultAxis( "Datum", TimeserieConstants.TYPE_DATE, "", Date.class, true );
        final IAxis qAxis = new DefaultAxis( axisTitle, resultType, TimeserieUtils.getUnit( resultType ), Double.class,
            false );
        final IAxis statusAxis = KalypsoStatusUtils.createStatusAxisFor( qAxis, true );
        final IAxis[] axis = new IAxis[]
        {
            dateAxis,
            qAxis,
            statusAxis };
        final ITuppleModel qTuppelModel = new SimpleTuppleModel( axis, tupelData );

        final MetadataList metadataList = new MetadataList();

        // if pegel exists, copy metadata (inclusive wq-function)

        final TimeseriesLink pegelLink = (TimeseriesLink)feature.getProperty( metadataTSLink );
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
            logger
                .info( "zu diesem Knoten existiert ein Pegel, einige Pegelmetadaten (z.B. Wechmann-Funktion) werden in Ergebniszeitreihe uebernommen\n" );
            final IObservation pegelObservation = ZmlFactory.parseXML( pegelURL, "pegelmessung" );

            copyMetaData( pegelObservation.getMetadataList(), metadataList, new String[]
            {
                TimeserieConstants.MD_ALARM_1,
                TimeserieConstants.MD_ALARM_2,
                TimeserieConstants.MD_ALARM_3,
                TimeserieConstants.MD_ALARM_4,
                TimeserieConstants.MD_GEWAESSER,
                TimeserieConstants.MD_FLUSSGEBIET,
                TimeserieConstants.MD_GKH,
                TimeserieConstants.MD_GKR,
                TimeserieConstants.MD_HOEHENANGABEART,
                TimeserieConstants.MD_PEGELNULLPUNKT,
                TimeserieConstants.MD_WQWECHMANN,
                TimeserieConstants.MD_WQTABLE,
                TimeserieConstants.MD_TIMEZONE,
                TimeserieConstants.MD_VORHERSAGE,
                ObservationConstants.MD_SCENARIO } );

          }
        }
        // lese ergebnis-link um target fuer zml zu finden
        String resultPathRelative;
        try
        {
          TimeseriesLink resultLink = (TimeseriesLink)feature.getProperty( targetTSLink );
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
          //          resultPathRelative = "Ergebnisse/Berechnet/" + feature.getFeatureType().getName() + "/" + suffix + "_"
          //              + Integer.toString( idManager.getAsciiID( feature ) ).trim() + "_" + feature.getId() + ".zml";

          resultPathRelative = "Ergebnisse/Berechnet/" + annotation + "/" + observationTitle + "/" + suffix + "("
              + observationTitle + ").zml";
        }
        if( !m_resultMap.containsKey( resultPathRelative ) )
        {
          m_resultMap.put( resultPathRelative, observationTitle );
        }
        else
        {
          logger.info( "Datei existiert bereits: " + resultPathRelative + "." );
          resultPathRelative = "Ergebnisse/Berechnet/" + annotation + "/" + observationTitle + "(ID"
              + Integer.toString( idManager.getAsciiID( feature ) ).trim() + ")/" + suffix + "(" + observationTitle
              + ").zml";
          m_resultMap.put( resultPathRelative, observationTitle );
          logger.info( "Der Dateiname wurde daher um die ObjektID erweitert: " + resultPathRelative + "." );
        }

        final File resultFile = new File( outputDir, resultPathRelative );
        resultFile.getParentFile().mkdirs();

        final IObservation resultObservation = new SimpleObservation( resultPathRelative, "ID", observationTitle,
            false, null, metadataList, axis, qTuppelModel );

        // Write result type as string into the 'Description'-Metatag
        resultObservation.getMetadataList().put( ObservationConstants.MD_DESCRIPTION, getTitleForSuffix( suffix ) );

        // update with Scenario metadata

        final String scenarioID = conf.getScenarioID();
        if( scenarioID != null && scenarioID.length() > 0 )
          resultObservation.getMetadataList().put( ObservationConstants.MD_SCENARIO, scenarioID );

        // write result
        final ObservationType observationType = ZmlFactory.createXML( resultObservation, null );
        final Marshaller marshaller = ZmlFactory.getMarshaller();
        marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );

        FileOutputStream stream = null;
        OutputStreamWriter writer = null;
        try
        {
          stream = new FileOutputStream( resultFile );
          writer = new OutputStreamWriter( stream, "UTF-8" );
          marshaller.marshal( observationType, writer );
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
    //    j Temperatur .tmp
    if( suffix.equalsIgnoreCase( "tmp" ) )
      return "Temperatur";
    //    j Niederschlag .pre
    if( suffix.equalsIgnoreCase( "pre" ) )
      return "Niederschlag";
    //    n Schnee .sch
    if( suffix.equalsIgnoreCase( "sch" ) )
      return "Schneehöhe";
    //    j Bodenfeuchte .bof
    if( suffix.equalsIgnoreCase( "bof" ) )
      return "Bodenfeuchte";
    //    n Bodenspeicher .bsp
    if( suffix.equalsIgnoreCase( "bsp" ) )
      return "Bodenspeicherbilanz";
    //    n Grundwasserstand .gws
    if( suffix.equalsIgnoreCase( "gws" ) )
      return "Grundwasserstand";
    //    Gesamtabfluss Knoten .qgs, Gesamtabfluss TG .qgg, Oberflaechenabfluss .qna, Interflow .qif, Abfluss vers.
    // Flaechen .qvs, Basisabfluss .qbs, Kluftgrundw1 .qt1, Kluftgrundw .qtg, Grundwasser .qgw
    if( suffix.equalsIgnoreCase( "qgs" ) | suffix.equalsIgnoreCase( "qgg" ) | suffix.equalsIgnoreCase( "qna" )
        | suffix.equalsIgnoreCase( "qif" ) | suffix.equalsIgnoreCase( "qvs" ) | suffix.equalsIgnoreCase( "qbs" )
        | suffix.equalsIgnoreCase( "qt1" ) | suffix.equalsIgnoreCase( "qtg" ) | suffix.equalsIgnoreCase( "qgw" ) )
      return "Abfluss";
    //    n Evapotranspiration .vet
    if( suffix.equalsIgnoreCase( "vet" ) )
      return "Evapotranspiration";
    //    n Ausgabe hydrotope .hyd
    if( suffix.equalsIgnoreCase( "hyd" ) )
      return "Ausgabe Hydrotope";
    //    n Abflussbilanz .bil
    if( suffix.equalsIgnoreCase( "bil" ) )
      return "Abflussbilanz";
    //    n Statistische Abflusswerte .nmq
    if( suffix.equalsIgnoreCase( "nmq" ) )
      return "Statistische Abflusswerte";
    //    n Speicherinhalt .spi
    if( suffix.equalsIgnoreCase( "spi" ) )
      return "Füllvolumen";
    //    n Wasserspiegelhöhe .sph
    if( suffix.equalsIgnoreCase( "sph" ) )
      return "Wasserspiegelhöhe";
    //    n Verdunstung aus Talsperre .spv
    if( suffix.equalsIgnoreCase( "spv" ) )
      return "Talsperrenverdunstung";
    //    n Niederschlag in Talsperre .spn
    if( suffix.equalsIgnoreCase( "spn" ) )
      return "Niederschlag";
    //    n Zehrung .spb
    if( suffix.equalsIgnoreCase( "spb" ) )
      return "Zehrung";
    //    n Speicherueberlauf .sup
    if( suffix.equalsIgnoreCase( "sup" ) )
      return "Speicherueberlauf";
    return suffix;
  }

  private void loadTextFileResults( File inputDir, Logger logger, File outputDir )
  {
    // ASCII-Files
    // kopiere statistische Ergebnis-Dateien
    final String[] wildcards = new String[]
    {
        "*" + "bil" + "*",
        "*" + "txt" + "*",
        "*" + "nmq" + "*",
        "*" + "bsp" + "*" };
    final File ascciResultDir = new File( inputDir, "out_we.nat" );
    MultipleWildCardFileFilter filter = new MultipleWildCardFileFilter( wildcards, false, false, true );

    File[] qgsFiles = ascciResultDir.listFiles( filter );
    if( qgsFiles.length != 0 )

    {
      for( int i = 0; i < qgsFiles.length; i++ )
      {
        // read ascii result file
        logger.info( "kopiere Ergebnissdatei " + qgsFiles[i].getName() + "\n" );

        String resultPathRelative = "Ergebnisse/Berechnet/Bilanz/" + qgsFiles[i].getName();
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

  private void loadLogs( final File tmpDir, final Logger logger, ICalcResultEater resultEater )
  {
    try
    {
      resultEater.addResult( NaModelConstants.LOG_EXE_STDOUT_ID, new File( tmpDir, "exe.log" ) );
    }
    catch( CalcJobServiceException e )
    {
      e.printStackTrace();
      logger.info( e.getMessage() );
    }
    try
    {
      resultEater.addResult( NaModelConstants.LOG_EXE_ERROUT_ID, new File( tmpDir, "exe.err" ) );
    }
    catch( CalcJobServiceException e )
    {
      e.printStackTrace();
      logger.info( e.getMessage() );
    }

    try
    {
      resultEater.addResult( NaModelConstants.LOG_OUTRES_ID, new File( tmpDir, "start/output.res" ) );
    }
    catch( CalcJobServiceException e )
    {
      e.printStackTrace();
      logger.info( e.getMessage() );
    }
    try
    {
      resultEater.addResult( NaModelConstants.LOG_OUTERR_ID, new File( tmpDir, "start/output.err" ) );
    }
    catch( CalcJobServiceException e )
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

  private void unzipTemplates( URL asciiZipURL, File exeDir )
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

  private void startCalculation( final File basedir, final ICalcMonitor monitor ) throws CalcJobServiceException
  {
    final File exeFile = new File( basedir, m_kalypsoKernelPath );
    final File exeDir = exeFile.getParentFile();
    final String commandString = exeFile.getAbsolutePath();
    long timeOut = 1000l * 60l * 10l; // max 10 minutes
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
      throw new CalcJobServiceException( "Fehler beim Ausfuehren", e );
    }
    finally
    {
      IOUtils.closeQuietly( logWriter );
      IOUtils.closeQuietly( errorWriter );
    }
  }

  public boolean isSucceeded()
  {
    return m_succeeded;
  }

}