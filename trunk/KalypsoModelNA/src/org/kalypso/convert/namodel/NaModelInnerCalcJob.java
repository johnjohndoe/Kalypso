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
import java.io.Reader;
import java.io.Writer;
import java.net.MalformedURLException;
import java.net.URL;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.bind.Marshaller;
import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.namespace.QName;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.java.io.FileCopyVisitor;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.lang.ProcessHelper;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.commons.lhwz.LhwzHelper;
import org.kalypso.contribs.java.io.filter.MultipleWildCardFileFilter;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.contribs.java.net.UrlResolver;
import org.kalypso.contribs.java.net.UrlUtilities;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.contribs.java.xml.XMLHelper;
import org.kalypso.convert.namodel.i18n.Messages;
import org.kalypso.convert.namodel.manager.IDManager;
import org.kalypso.convert.namodel.manager.LzsimManager;
import org.kalypso.convert.namodel.optimize.CalibarationConfig;
import org.kalypso.convert.namodel.optimize.NAOptimizingJob;
import org.kalypso.convert.namodel.timeseries.BlockTimeSeries;
import org.kalypso.convert.namodel.timeseries.NATimeSettings;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
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
import org.w3c.dom.Document;
import org.xml.sax.InputSource;
import org.xml.sax.SAXParseException;

/**
 * @author doemming, huebsch
 */
public class NaModelInnerCalcJob implements ISimulation
{

  // resourcebase for static files used in calculation
  private final String m_resourceBase = Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.0" ); //$NON-NLS-1$

  private final String EXE_FILE_WEISSE_ELSTER = Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.1" ); //$NON-NLS-1$

  private final String EXE_FILE_2_05beta = Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.2" ); //$NON-NLS-1$

  private final String EXE_FILE_2_06 = Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.3" ); //$NON-NLS-1$

  private final String EXE_FILE_2_07 = "start/kalypso_2.0.7.exe"; //$NON-NLS-1$

  private final String EXE_FILE_2_08 = "start/kalypso_2.0.8.exe"; //$NON-NLS-1$

  private final String EXE_FILE_2_11 = "start/kalypso_2.1.1.exe"; //$NON-NLS-1$

  private final String EXE_FILE_2_13 = "start/kalypso_2.1.3.exe"; //$NON-NLS-1$

  private final String EXE_FILE_TEST = "start/kalypso_test.exe"; //$NON-NLS-1$

  private boolean m_succeeded = false;

  private String m_kalypsoKernelPath = null;

  final private List<String> m_resultMap = new ArrayList<String>();

  private String m_dateString;

  private static final String WE_RESOURCE_HYDROTOP_GML = "resources/WE/hydrotop.gml"; //$NON-NLS-1$

  private static final String WE_PARAMETER_GML = "resources/WE/parameter.gml"; //$NON-NLS-1$

  private static final String WE_LANDUSE_GML = "resources/WE/landuse.gml"; //$NON-NLS-1$

  private static final String WE_SUDS_GML = "resources/WE/suds.gml"; //$NON-NLS-1$

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
  public void run( final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    final File resultDir = new File( tmpdir, NaModelConstants.OUTPUT_DIR_NAME );
    final Date startRunDate = new Date( Calendar.getInstance().getTimeInMillis() );
    final DateFormat format = new SimpleDateFormat( "yyyy-MM-dd(HH-mm-ss)" ); //$NON-NLS-1$
    m_dateString = format.format( startRunDate );

    final File newModellFile = new File( tmpdir, "namodellBerechnung.gml" ); //$NON-NLS-1$
    NAConfiguration conf;
    try
    {
      conf = NAConfiguration.getGml2AsciiConfiguration( newModellFile.toURI().toURL(), tmpdir );
    }
    catch( final Exception e1 )
    {
      e1.printStackTrace();
      throw new SimulationException( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.36" ), e1.getCause() ); //$NON-NLS-1$
    }
    conf.setZMLContext( (URL) inputProvider.getInputForID( NaModelConstants.IN_META_ID ) );
    final Logger logger = conf.getLogger();
    logger.log( Level.INFO, Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.13" ) + m_dateString + Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.14" ) ); //$NON-NLS-1$ //$NON-NLS-2$

    try
    {
      monitor.setMessage( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.15" ) ); //$NON-NLS-1$
      if( monitor.isCanceled() )
        return;

      if( monitor.isCanceled() )
        return;
      // Kopieren von Berechnungsstandardverzeichnis
      final URL asciiTemplateURL = getClass().getResource( "template/emptyAsciiTemplate.zip" ); //$NON-NLS-1$
      unzipInput( asciiTemplateURL, tmpdir );

      if( inputProvider.hasID( NAOptimizingJob.IN_BestOptimizedRunDir_ID ) )
      {
        // while optimization, you can recycle files from a former run.
        // implement here to copy the files to your tmp dir and while generating
        // files you should check if files allready exist, and on your option do
        // not generate them.
        // WARNING: never use result files or files that vary during
        // optimization.
        final URL url = (URL) inputProvider.getInputForID( NAOptimizingJob.IN_BestOptimizedRunDir_ID );
        final File from1 = new File( url.getFile(), "klima.dat" ); //$NON-NLS-1$
        final File to1 = new File( tmpdir, "klima.dat" ); //$NON-NLS-1$
        if( from1.exists() && to1.exists() )
        {
          final FileCopyVisitor copyVisitor = new FileCopyVisitor( from1, to1, true );
          FileUtilities.accept( from1, copyVisitor, true );
        }
        final File from2 = new File( url.getFile(), "zufluss" ); //$NON-NLS-1$
        final File to2 = new File( tmpdir, "zufluss" ); //$NON-NLS-1$
        if( from2.exists() && to2.exists() )
        {
          final FileCopyVisitor copyVisitor = new FileCopyVisitor( from2, to2, true );
          FileUtilities.accept( from2, copyVisitor, true );
        }

        final File from3 = new File( url.getFile(), "hydro.top" ); //$NON-NLS-1$
        final File to3 = new File( tmpdir, "hydro.top" ); //$NON-NLS-1$
        if( from3.exists() && to3.exists() )
        {
          final FileCopyVisitor copyVisitor = new FileCopyVisitor( from3, to3, true );
          FileUtilities.accept( from3, copyVisitor, true );
        }
      }

      // generiere ascii-dateien
      monitor.setMessage( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.23" ) ); //$NON-NLS-1$
      if( monitor.isCanceled() )
        return;

      // calualtion model
      final GMLWorkspace modellWorkspace = generateASCII( conf, tmpdir, inputProvider, newModellFile );
      final URL naControlURL = (URL) inputProvider.getInputForID( NaModelConstants.IN_CONTROL_ID );
      final GMLWorkspace naControlWorkspace = GmlSerializer.createGMLWorkspace( naControlURL, null );

      final URL iniValuesFolderURL = (URL) inputProvider.getInputForID( NaModelConstants.LZSIM_IN_ID );

      if( iniValuesFolderURL != null )
      {
        // TODO: crude way to create the new URL, necessary as probably we do not have a '/' at the end of the path
        final URL lzsimURL = new URL( iniValuesFolderURL.toExternalForm() + "/lzsim.gml" );
        try
        {
          final GMLWorkspace iniValuesWorkspace = GmlSerializer.createGMLWorkspace( lzsimURL, null );
          LzsimManager.writeLzsimFiles( conf, tmpdir, iniValuesWorkspace );
        }
        // We still assume it is a file.... ignore file not found, we do not have starting conditions then
        catch( final FileNotFoundException e )
        {
// e.printStackTrace();

          logger.log( Level.INFO, Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.26" ), conf.getSimulationStart().toString() ); //$NON-NLS-1$
        }
      }

      if( monitor.isCanceled() )
        return;
      // kopiere executable aus resourcen:
      copyExecutable( tmpdir );

      // starte berechnung
      monitor.setMessage( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.27" ) ); //$NON-NLS-1$
      if( monitor.isCanceled() )
        return;
      startCalculation( tmpdir, monitor );
      checkSucceeded( tmpdir );
      if( isSucceeded() )
      {
        monitor.setMessage( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.28" ) ); //$NON-NLS-1$
        logger.log( Level.FINEST, Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.29" ) ); //$NON-NLS-1$
        loadResults( tmpdir, modellWorkspace, naControlWorkspace, logger, resultDir, conf );
        monitor.setMessage( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.30" ) ); //$NON-NLS-1$
        logger.log( Level.FINEST, Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.31" ) ); //$NON-NLS-1$
        createStatistics( tmpdir, modellWorkspace, naControlWorkspace, logger, resultDir, conf );
      }
      else
      {
        monitor.setMessage( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.32" ) ); //$NON-NLS-1$
        logger.log( Level.SEVERE, Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.33" ) ); //$NON-NLS-1$
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new SimulationException( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.36" ), e.getCause() ); //$NON-NLS-1$
    }
    finally
    {
      loadLogs( tmpdir, logger, conf, resultDir );

      // Copy results to restore the actual results in the dateDir as well... .
      final File resultDirFrom = new File( resultDir, "Ergebnisse/Aktuell" ); //$NON-NLS-1$
      final File resultDirTo = new File( resultDir, "Ergebnisse/" + m_dateString ); //$NON-NLS-1$
      resultDirTo.mkdirs();
      if( resultDirFrom.exists() && resultDirTo.exists() )
      {
        final FileCopyVisitor copyVisitor = new FileCopyVisitor( resultDirFrom, resultDirTo, true );
        try
        {
          FileUtilities.accept( resultDirFrom, copyVisitor, true );
        }
        catch( final IOException e )
        {
          e.printStackTrace();
        }
      }
      final File[] files = resultDir.listFiles();
      if( files != null )
      {
        for( final File element : files )
        {
          if( element.isDirectory() ) // Ergebnisse
          {
            resultEater.addResult( NaModelConstants.OUT_ZML, element );
            return;
          }
        }
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

    final File fileMitte = getResultFileFor( resultDir, rootFeature, "qAblageSpurMittlerer" ); //$NON-NLS-1$
    final File fileUnten = getResultFileFor( resultDir, rootFeature, "qAblageSpurUnterer" ); //$NON-NLS-1$
    final File fileOben = getResultFileFor( resultDir, rootFeature, "qAblageSpurOberer" ); //$NON-NLS-1$

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
    TranProLinFilterUtilities.transformAndWrite( resultObservation, calBegin, tranpolinEnd, offsetStartPrediction, offsetEndPrediction, "+", axisType, KalypsoStati.BIT_DERIVATED, fileMitte, " - Spur Mitte", request ); //$NON-NLS-1$

    // read the freshly created file into a new observation, we are going to umhüll it
    final IObservation adaptedResultObservation = ZmlFactory.parseXML( fileMitte.toURL(), "adaptedVorhersage" ); //$NON-NLS-1$

    //
    // Second, we build the umhüllenden for the adapted result
    //
    double accuracyPrediction = LhwzHelper.getDefaultUmhuellendeAccuracy();
    final Double featureAccuracy = (Double) rootFeature.getProperty( NaModelConstants.NACONTROL_ACCPRED_PROP );
    if( featureAccuracy == null )
      logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.44" ) + accuracyPrediction + Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.45" ) ); //$NON-NLS-1$ //$NON-NLS-2$
    else
      accuracyPrediction = featureAccuracy.doubleValue();

    // accuracyPrediction // %/60h
    final long millisOf60hours = 1000 * 60 * 60 * 60;
    // endAccuracy: %/simulationRange
    final double endAccuracy = accuracyPrediction * (((double) (endForecast.getTime() - startForecast.getTime())) / ((double) millisOf60hours));

    final double endOffset = calcEndValue * (endAccuracy / 100);

    TranProLinFilterUtilities.transformAndWrite( adaptedResultObservation, calBegin, calEnd, 0, endOffset, "-", axisType, KalypsoStati.BIT_DERIVATED, fileUnten, " - spur Unten", request ); //$NON-NLS-1$
    TranProLinFilterUtilities.transformAndWrite( adaptedResultObservation, calBegin, calEnd, 0, endOffset, "+", axisType, KalypsoStati.BIT_DERIVATED, fileOben, " - spur Oben", request ); //$NON-NLS-1$
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

  private IObservation loadPredictedResult( final File resultDir, final Feature rootFeature ) throws MalformedURLException, SensorException
  {
    final TimeseriesLinkType resultLink = (TimeseriesLinkType) rootFeature.getProperty( NaModelConstants.NODE_RESULT_TIMESERIESLINK_PROP );

    // from predicted timeseries
    final UrlResolver urlResolver = new UrlResolver();
    final URL resultURL = urlResolver.resolveURL( resultDir.toURL(), resultLink.getHref() );
    return ZmlFactory.parseXML( resultURL, Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.51" ) ); //$NON-NLS-1$
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
    catch( final Exception e )
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
      final File logDir = new File( inputDir, "start" ); //$NON-NLS-1$
      final File logFile = new File( logDir, "output.res" ); //$NON-NLS-1$
      logFileReader = new FileReader( logFile );
      reader = new LineNumberReader( logFileReader );
      String line;
      while( (line = reader.readLine()) != null )
      {
        if( line.indexOf( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.54" ) ) >= 0 || line.indexOf( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.55" ) ) >= 0 ) //$NON-NLS-1$ //$NON-NLS-2$
          m_succeeded = true;
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
  }

  private GMLWorkspace generateASCII( final NAConfiguration conf, final File tmpDir, final ISimulationDataProvider dataProvider, final File newModellFile ) throws Exception
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

    GMLWorkspace landuseWorkspace = null, sudsWorkspace = null;
    if( dataProvider.hasID( NaModelConstants.IN_LANDUSE_ID ) )
    {
      final URL url = (URL) dataProvider.getInputForID( NaModelConstants.IN_LANDUSE_ID );
      final File f = new File( url.toURI().getPath() );
      if( f.exists() )
        landuseWorkspace = GmlSerializer.createGMLWorkspace( url, null );
    }
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
    conf.setSzenarioID( (String) metaFE.getProperty( NaModelConstants.CONTROL_SCENARIO_ID_PROP ) );
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
    int minutesTimeStep = 60;
    final Integer minutesOfTimeStep = (Integer) metaFE.getProperty( NaModelConstants.CONTROL_MINUTES_TIMESTEP_PROP );
    if( minutesOfTimeStep != null )
      minutesTimeStep = minutesOfTimeStep.intValue() != 0 ? minutesOfTimeStep.intValue() : 60;
    conf.setMinutesOfTimeStep( minutesTimeStep );

    // choose simulation kernel
    chooseSimulationExe( (String) metaFE.getProperty( NaModelConstants.CONTROL_VERSION_KALYPSONA_PROP ) );

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

    // generate control files
    NAControlConverter.featureToASCII( conf, tmpDir, controlWorkspace, modellWorkspace );

    // update model with factor values from control
    updateFactorParameter( modellWorkspace );

    // generate modell files
    conf.setModelWorkspace( modellWorkspace );
    conf.setParameterWorkspace( parameterWorkspace );
    conf.setHydrotopeWorkspace( hydrotopWorkspace );
    conf.setSynthNWorkspace( synthNWorkspace );
    conf.setLanduseWorkspace( landuseWorkspace );
    conf.setSudsWorkspace( sudsWorkspace );

    final NAModellConverter main = new NAModellConverter( conf );
    main.write();

    // dump idmapping to file
    final IDManager idManager = conf.getIdManager();
    Writer idWriter = null;
    try
    {
      idWriter = new FileWriter( new File( tmpDir, "IdMap.txt" ) ); //$NON-NLS-1$
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
   * @param kalypsoNAVersion
   *          name/version of simulation kernel
   */
  private void chooseSimulationExe( final String kalypsoNAVersion )
  {
    if( kalypsoNAVersion.equals( "test" ) )
      m_kalypsoKernelPath = EXE_FILE_TEST;
    else if( kalypsoNAVersion.equals( "lfug" ) )
      m_kalypsoKernelPath = EXE_FILE_WEISSE_ELSTER;
    else if( kalypsoNAVersion.equals( "v2.0.5" ) ) //$NON-NLS-1$
      m_kalypsoKernelPath = EXE_FILE_2_05beta;
    else if( kalypsoNAVersion.equals( "v2.0.6" ) ) //$NON-NLS-1$
      m_kalypsoKernelPath = EXE_FILE_2_06;
    else if( kalypsoNAVersion.equals( "v2.0.7" ) ) //$NON-NLS-1$
      m_kalypsoKernelPath = EXE_FILE_2_07;
    else if( kalypsoNAVersion.equals( "v2.0.8" ) ) //$NON-NLS-1$
      m_kalypsoKernelPath = EXE_FILE_2_08;
    else if( kalypsoNAVersion.equals( "v2.1.1" ) ) //$NON-NLS-1$
      m_kalypsoKernelPath = EXE_FILE_2_11;
    else if( kalypsoNAVersion.equals( "v2.1.3" ) ) //$NON-NLS-1$
      m_kalypsoKernelPath = EXE_FILE_2_13;
    else if( kalypsoNAVersion.equals( "neueste" ) || kalypsoNAVersion.equals( "latest" ) ) // latest stable is 2.1.3
      m_kalypsoKernelPath = EXE_FILE_2_13;
    else
    {
      System.out.println( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.69" ) + Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.70" ) //$NON-NLS-1$ //$NON-NLS-2$
          + Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.71" ) ); //$NON-NLS-1$
      m_kalypsoKernelPath = EXE_FILE_2_11;
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
   *
   */
  private final static String[][] m_catchmentFactorsParameter = { new String[] { "retob", "faktorRetobRetint" }, new String[] { "retint", "faktorRetobRetint" }, new String[] { "aigw", "faktorAigw" } }; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$

  private static String[] m_catchmentFactorParameterTarget = { "retob", "retint", "aigw" }; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

  private final UrlUtilities m_urlUtilities;

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
    update( catchmentFEs, m_catchmentFactorParameterTarget, m_catchmentFactorsParameter );

    // KMChannels
    final Feature[] kmChanneFEs = modellWorkspace.getFeatures( modellWorkspace.getGMLSchema().getFeatureType( NaModelConstants.KM_CHANNEL_ELEMENT_FT ) );
    for( final Feature feature : kmChanneFEs )
    {
      final double rkfFactor = FeatureHelper.getAsDouble( feature, NaModelConstants.KM_CHANNEL_FAKTOR_RKF_PROP, 1.0 );
      final double rnfFactor = FeatureHelper.getAsDouble( feature, NaModelConstants.KM_CHANNEL_FAKTOR_RNF_PROP, 1.0 );
      final List kmParameter = (List) feature.getProperty( NaModelConstants.KM_CHANNEL_PARAMETER_MEMBER );
      final Iterator iterator = kmParameter.iterator();
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

  private void loadResults( final File tmpdir, final GMLWorkspace modellWorkspace, final GMLWorkspace naControlWorkspace, final Logger logger, final File resultDir, final NAConfiguration conf ) throws Exception
  {
    loadTSResults( tmpdir, modellWorkspace, logger, resultDir, conf );
    try
    {
      loadTesultTSPredictionIntervals( naControlWorkspace, logger, resultDir, conf );
    }
    catch( final Exception e )
    {
      logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.83" ) + e.getLocalizedMessage() ); //$NON-NLS-1$
    }
    loadTextFileResults( tmpdir, logger, resultDir );
    if( conf.getIniWrite() )
    {
      final LzsimManager lzsimManager = new LzsimManager();
      lzsimManager.initialValues( conf.getIdManager(), tmpdir, logger, resultDir, conf );
    }

  }

  private void loadTSResults( final File inputDir, final GMLWorkspace modellWorkspace, final Logger logger, final File outputDir, final NAConfiguration conf ) throws Exception
  {
    // j Gesamtabfluss Knoten .qgs
    final IFeatureType nodeFT = modellWorkspace.getGMLSchema().getFeatureType( NaModelConstants.NODE_ELEMENT_FT );
    loadTSResults( "qgs", nodeFT, "name", TimeserieConstants.TYPE_RUNOFF, "pegelZR", "qberechnetZR", inputDir, modellWorkspace, logger, outputDir, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

    final IFeatureType catchmentFT = modellWorkspace.getGMLSchema().getFeatureType( NaModelConstants.CATCHMENT_ELEMENT_FT );
    final IFeatureType rhbChannelFT = modellWorkspace.getGMLSchema().getFeatureType( NaModelConstants.STORAGE_CHANNEL_ELEMENT_FT );
    // j Niederschlag .pre
    loadTSResults( "pre", catchmentFT, "name", TimeserieConstants.TYPE_RAINFALL, null, null, inputDir, modellWorkspace, logger, outputDir, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // j Temperatur .tmp
    loadTSResults( "tmp", catchmentFT, "name", TimeserieConstants.TYPE_TEMPERATURE, null, null, inputDir, modellWorkspace, logger, outputDir, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // n Interflow .qif
    loadTSResults( "qif", catchmentFT, "name", TimeserieConstants.TYPE_RUNOFF, null, null, inputDir, modellWorkspace, logger, outputDir, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // n Grundwasser .qgw
    loadTSResults( "qgw", catchmentFT, "name", TimeserieConstants.TYPE_RUNOFF, null, null, inputDir, modellWorkspace, logger, outputDir, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // n Gesamtabfluss TG .qgg
    loadTSResults( "qgg", catchmentFT, "name", TimeserieConstants.TYPE_RUNOFF, null, null, inputDir, modellWorkspace, logger, outputDir, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // n Grundwasserstand .gws - Umrechnung von m auf cm
    loadTSResults( "gws", catchmentFT, "name", TimeserieConstants.TYPE_WATERLEVEL, null, null, inputDir, modellWorkspace, logger, outputDir, 100.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // n Basisabfluss .qbs
    loadTSResults( "qbs", catchmentFT, "name", TimeserieConstants.TYPE_RUNOFF, null, null, inputDir, modellWorkspace, logger, outputDir, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // n Oberflaechenabfluss .qna
    loadTSResults( "qna", catchmentFT, "name", TimeserieConstants.TYPE_RUNOFF, null, null, inputDir, modellWorkspace, logger, outputDir, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // n Abfluss vers. Flaechen .qvs
    loadTSResults( "qvs", catchmentFT, "name", TimeserieConstants.TYPE_RUNOFF, null, null, inputDir, modellWorkspace, logger, outputDir, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // TODO:check output for the next time series
    // n Schnee .sch [mm]
    loadTSResults( "sch", catchmentFT, "name", TimeserieConstants.TYPE_WATERLEVEL, null, null, inputDir, modellWorkspace, logger, outputDir, 0.1d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // n Kluftgrundw1 .qt1
    loadTSResults( "qt1", catchmentFT, "name", TimeserieConstants.TYPE_RUNOFF, null, null, inputDir, modellWorkspace, logger, outputDir, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // n Kluftgrundw .qtg
    loadTSResults( "qtg", catchmentFT, "name", TimeserieConstants.TYPE_RUNOFF, null, null, inputDir, modellWorkspace, logger, outputDir, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // n Evapotranspiration .vet [mm]
    loadTSResults( "vet", catchmentFT, "name", TimeserieConstants.TYPE_EVAPORATION, null, null, inputDir, modellWorkspace, logger, outputDir, 0.1d, conf ); //$NON-NLS-1$ //$NON-NLS-2$

    // TODO: Zeitreihe als mittlere Bodenfeuchte aus Fortran übernehmen, daher bisher nicht zu übertragen (Zur zeit wird
    // die Bodenfeuchte des ersten Hydrotopes in allen Schichten ausgegeben)
    // j Bodenfeuchte .bof [mm]
    // loadTSResults( "bof", catchmentFT, "name", TimeserieConstants.TYPE_WATERLEVEL, null, null, inputDir,
    // modellWorkspace, logger, outputDir, 0.1d, conf );

    // Straenge
    // n Wasserstand Speicher .sph [muNN]
    loadTSResults( "sph", rhbChannelFT, "name", TimeserieConstants.TYPE_NORMNULL, null, null, inputDir, modellWorkspace, logger, outputDir, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // n Speicherueberlauf .sub [m³/s]
    loadTSResults( "sub", rhbChannelFT, "name", TimeserieConstants.TYPE_RUNOFF, null, null, inputDir, modellWorkspace, logger, outputDir, 1.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
    // n Speicherinhalt .spi [hm³] - Umrechnung auf m³
    loadTSResults( "spi", rhbChannelFT, "name", TimeserieConstants.TYPE_VOLUME, null, null, inputDir, modellWorkspace, logger, outputDir, 1000000.0d, conf ); //$NON-NLS-1$ //$NON-NLS-2$
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
    final File ascciResultDir = new File( inputDir, "out_we.nat" ); //$NON-NLS-1$
    final MultipleWildCardFileFilter filter = new MultipleWildCardFileFilter( new String[] { "*" + suffix + "*" }, false, false, true ); //$NON-NLS-1$ //$NON-NLS-2$
    final File[] qgsFiles = ascciResultDir.listFiles( filter );
    if( qgsFiles.length != 0 )
    {
      // read ascii result file
      logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.123" ) + qgsFiles[0].getName() + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$
      final BlockTimeSeries ts = new BlockTimeSeries();
      ts.importBlockFile( qgsFiles[0] );

      // iterate model nodes/Catchments/rhbChannels and generate zml

      final Feature[] nodeFEs = modellWorkspace.getFeatures( resultFT );
      for( final Feature feature : nodeFEs )
      {
        if( resultFT == (modellWorkspace.getGMLSchema().getFeatureType( NaModelConstants.NODE_ELEMENT_FT ))
            || resultFT == (modellWorkspace.getGMLSchema().getFeatureType( NaModelConstants.CATCHMENT_ELEMENT_FT ))
            || resultFT == (modellWorkspace.getGMLSchema().getFeatureType( NaModelConstants.STORAGE_CHANNEL_ELEMENT_FT )) )
        {
          if( !FeatureHelper.booleanIsTrue( feature, NaModelConstants.GENERATE_RESULT_PROP, false ) )
            continue; // should not generate results
        }
        final String key = Integer.toString( idManager.getAsciiID( feature ) );

        final String axisTitle = getAxisTitleForSuffix( suffix );

        if( !ts.dataExistsForKey( key ) )
          continue; // no results available
        logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.125" ) + key + ", Name:" + feature.getFeatureType().getQName() + "(" + suffix + ")" + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$

        // transform data to tuppelmodel
        final SortedMap data = ts.getTimeSerie( key );
        final Object[][] tupelData = new Object[data.size()][3];
        final Set dataSet = data.entrySet();
        final Iterator iter = dataSet.iterator();
        int pos = 0;
        while( iter.hasNext() )
        {
          final Map.Entry entry = (Map.Entry) iter.next();
          tupelData[pos][0] = entry.getKey();
          tupelData[pos][1] = new Double( Double.parseDouble( entry.getValue().toString() ) * resultFactor );
          tupelData[pos][2] = new Integer( KalypsoStati.BIT_OK );
          pos++;
        }

        final IAxis dateAxis = new DefaultAxis( "Datum", TimeserieConstants.TYPE_DATE, "", Date.class, true ); //$NON-NLS-1$ //$NON-NLS-2$
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
          catch( final Exception e )
          {
            itExists = false;
          }
          if( itExists )
          {
            logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.132" ) ); //$NON-NLS-1$
            final IObservation pegelObservation = ZmlFactory.parseXML( pegelURL, Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.133" ) ); //$NON-NLS-1$

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
          final TimeseriesLinkType resultLink = (TimeseriesLinkType) feature.getProperty( targetTSLink );
          if( resultLink == null )
          {
            logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.134" ) + feature.getId() + " ." ); //$NON-NLS-1$ //$NON-NLS-2$
          }
          final String href = resultLink.getHref();
          resultPathRelative = href.substring( 19 );
        }
        catch( final Exception e )
        {
          // if there is target defined or there are some problems with that
          // we generate one
          resultPathRelative = DefaultPathGenerator.generateResultPathFor( feature, titlePropName, suffix, null );
        }
        if( !m_resultMap.contains( resultPathRelative ) )
        {
          m_resultMap.add( resultPathRelative );
        }
        else
        {
          logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.136" ) + resultPathRelative + "." ); //$NON-NLS-1$ //$NON-NLS-2$
          resultPathRelative = DefaultPathGenerator.generateResultPathFor( feature, titlePropName, suffix, "(ID" + Integer.toString( idManager.getAsciiID( feature ) ).trim() + ")" ); //$NON-NLS-1$ //$NON-NLS-2$
          m_resultMap.add( resultPathRelative );
          logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.140" ) + resultPathRelative + "." ); //$NON-NLS-1$ //$NON-NLS-2$
        }

        final File resultFile = new File( outputDir, "Ergebnisse/Aktuell/" + resultPathRelative ); //$NON-NLS-1$
        resultFile.getParentFile().mkdirs();

        // create observation object
        final String titleForObservation = DefaultPathGenerator.generateTitleForObservation( feature, titlePropName, suffix );

        final IObservation resultObservation = new SimpleObservation( resultPathRelative, "ID", titleForObservation, false, null, metadataList, axis, qTuppelModel ); //$NON-NLS-1$

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
          writer = new OutputStreamWriter( stream, "UTF-8" ); //$NON-NLS-1$
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

  private void createStatistics( final File tmpdir, final GMLWorkspace modellWorkspace, final GMLWorkspace naControlWorkspace, final Logger logger, final File resultDir, final NAConfiguration conf ) throws Exception
  {
    final String nodeResultFileNamePattern = "Gesamtabfluss.zml"; //$NON-NLS-1$
    final String reportPathZML = "Ergebnisse/Aktuell/Reports/nodesMax.zml"; //$NON-NLS-1$
    final String reportPathCSV = "Ergebnisse/Aktuell/Reports/nodesMax.csv"; //$NON-NLS-1$
    final String separatorCSV = ","; //$NON-NLS-1$
    final Pattern stationNodePattern = Pattern.compile( "([0-9]+).*" ); //$NON-NLS-1$
    final Pattern stationNamePattern = Pattern.compile( ".+_(.+)\\.zml" ); //$NON-NLS-1$

    final File reportFileZML = new File( resultDir.getAbsolutePath(), reportPathZML );
    final File reportFileCSV = new File( resultDir.getAbsolutePath(), reportPathCSV );
    reportFileZML.getParentFile().mkdirs();
    final List<Object[]> resultValuesList = new ArrayList<Object[]>();
    final List<IAxis> resultAxisList = new ArrayList<IAxis>();
    resultAxisList.add( new DefaultAxis( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.149" ), TimeserieConstants.TYPE_NODEID, "", Integer.class, true ) ); //$NON-NLS-1$ //$NON-NLS-2$
    resultAxisList.add( new DefaultAxis( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.151" ), TimeserieConstants.TYPE_PEGEL, "", String.class, false ) ); //$NON-NLS-1$ //$NON-NLS-2$
    resultAxisList.add( new DefaultAxis( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.153" ), TimeserieConstants.TYPE_DATE, "", Date.class, false ) ); //$NON-NLS-1$ //$NON-NLS-2$
    resultAxisList.add( new DefaultAxis( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.155" ), TimeserieConstants.TYPE_RUNOFF, TimeserieUtils.getUnit( TimeserieConstants.TYPE_RUNOFF ), Double.class, false ) ); //$NON-NLS-1$

    for( final String resultFileRelativePath : m_resultMap )
    {
      final boolean isNodeResult = resultFileRelativePath.endsWith( nodeResultFileNamePattern );
      final File resultFile = new File( resultDir.getAbsolutePath(), "/Ergebnisse/Aktuell/" + resultFileRelativePath ); //$NON-NLS-1$
      final IObservation observation = ZmlFactory.parseXML( resultFile.toURL(), null );
      final IAxis[] axisList = observation.getAxisList();
      IAxis dateAxis = null;
      IAxis valueAxis = null;
      final ITuppleModel tuppleModel = observation.getValues( null );
      for( final IAxis axis : axisList )
      {
        if( axis.getType().equals( TimeserieConstants.TYPE_DATE ) )
          dateAxis = axis;
        else if( axis.getType().equals( TimeserieConstants.TYPE_RUNOFF ) )
          valueAxis = axis;
      }
      if( dateAxis == null || valueAxis == null )
        continue;
      double maxValue = -Double.MAX_VALUE;
      Date maxValueDate = null;
      for( int i = 0; i < tuppleModel.getCount(); i++ )
      {
        final double value = (Double) tuppleModel.getElement( i, valueAxis );
        if( maxValue < value )
        {
          maxValue = value;
          maxValueDate = (Date) tuppleModel.getElement( i, dateAxis );
        }
      }
      if( maxValueDate == null )
      {
        logger.log( Level.WARNING, Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.157" ) + resultFileRelativePath + "'" ); //$NON-NLS-1$ //$NON-NLS-2$
        continue;
      }
      if( isNodeResult )
      {
        final Matcher nodeMatcher = stationNodePattern.matcher( observation.getName() );
        if( nodeMatcher.matches() )
          resultValuesList.add( new Object[] { nodeMatcher.group( 1 ), " ", maxValueDate, maxValue } ); //$NON-NLS-1$
        else
          logger.log( Level.WARNING, Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.160" ) + observation.getName() + Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.161" ) + resultFileRelativePath + "'" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      }
      else
      {
        final Matcher stationNodeMatcher = stationNodePattern.matcher( observation.getName() );
        final Matcher stationNameMatcher = stationNamePattern.matcher( resultFileRelativePath );
        if( stationNodeMatcher.matches() && stationNameMatcher.matches() )
          resultValuesList.add( new Object[] { stationNodeMatcher.group( 1 ), stationNameMatcher.group( 1 ), maxValueDate, maxValue } );
        else
          logger.log( Level.WARNING, Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.163" ) + observation.getName() + Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.164" ) + resultFileRelativePath + "'" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      }
    }
    final IAxis[] axis = resultAxisList.toArray( new IAxis[0] );
    final ITuppleModel resultTuppleModel = new SimpleTuppleModel( axis, resultValuesList.toArray( new Object[0][] ) );
    final IObservation resultObservation = new SimpleObservation( reportPathZML, "ID", Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.167" ), false, null, new MetadataList(), axis, resultTuppleModel ); //$NON-NLS-1$ //$NON-NLS-2$
    final Observation observation = ZmlFactory.createXML( resultObservation, null );
    final Marshaller marshaller = ZmlFactory.getMarshaller();
    marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );

    FileOutputStream streamZML = null;
    OutputStreamWriter writerZML = null;
    FileOutputStream streamCSV = null;
    OutputStreamWriter writerCSV = null;
    final SimpleDateFormat dateFormat = new SimpleDateFormat( "MM/dd/yy" ); //$NON-NLS-1$
    try
    {
      streamCSV = new FileOutputStream( reportFileCSV );
      writerCSV = new OutputStreamWriter( streamCSV, "UTF-8" ); //$NON-NLS-1$
      Object currentElement;
      for( int i = 0; i < resultTuppleModel.getCount(); i++ )
      {
        currentElement = resultTuppleModel.getElement( i, resultAxisList.get( 0 ) );
        writerCSV.write( currentElement.toString() );
        writerCSV.write( separatorCSV );
        currentElement = resultTuppleModel.getElement( i, resultAxisList.get( 1 ) );
        writerCSV.write( currentElement.toString() );
        writerCSV.write( separatorCSV );
        currentElement = resultTuppleModel.getElement( i, resultAxisList.get( 2 ) );
        writerCSV.write( dateFormat.format( currentElement ) );
        writerCSV.write( separatorCSV );
        currentElement = resultTuppleModel.getElement( i, resultAxisList.get( 3 ) );
        writerCSV.write( currentElement.toString().replaceFirst( ",", "." ) ); //$NON-NLS-1$ //$NON-NLS-2$
        writerCSV.write( "\n" ); //$NON-NLS-1$
      }
      writerCSV.flush();
      streamZML = new FileOutputStream( reportFileZML );
      writerZML = new OutputStreamWriter( streamZML, "UTF-8" ); //$NON-NLS-1$
      marshaller.marshal( observation, writerZML );
    }
    finally
    {
      IOUtils.closeQuietly( writerZML );
      IOUtils.closeQuietly( streamZML );
      IOUtils.closeQuietly( writerCSV );
      IOUtils.closeQuietly( streamCSV );
    }
  }

  /**
   * @param suffix
   * @return AxisTitle
   */
  private String getAxisTitleForSuffix( final String suffix )
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

  private void loadTextFileResults( final File inputDir, final Logger logger, final File outputDir )
  {
    // ASCII-Files
    // kopiere statistische Ergebnis-Dateien
    final String[] wildcards = new String[] { "*" + "bil" + "*" }; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    // final String[] wildcards = new String[] { "*" + "bil" + "*", "*" + "txt" + "*", "*" + "nmq" + "*", "*" + "bsp" +
    // "*" };
    final File ascciResultDir = new File( inputDir, "out_we.nat" ); //$NON-NLS-1$
    final MultipleWildCardFileFilter filter = new MultipleWildCardFileFilter( wildcards, false, false, true );

    final File[] qgsFiles = ascciResultDir.listFiles( filter );
    if( qgsFiles.length != 0 )

    {
      for( final File element : qgsFiles )
      {
        // read ascii result file
        logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.220" ) + element.getName() + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$

        final String resultPathRelative = "Ergebnisse/Aktuell/Bilanz/Bilanz.txt"; //$NON-NLS-1$
        final String inputPath = inputDir.getName() + element.getName();
        final File resultFile = new File( outputDir, resultPathRelative );
        resultFile.getParentFile().mkdirs();
        FileInputStream FileIS = null;
        try
        {
          FileIS = new FileInputStream( element );
        }
        catch( final FileNotFoundException e1 )
        {
          e1.printStackTrace();
        }
        try
        {
          FileUtilities.makeFileFromStream( false, resultFile, FileIS );
        }
        catch( final IOException e )
        {
          e.printStackTrace();
          System.out.println( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.223" ) + inputPath + Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.224" ) ); //$NON-NLS-1$ //$NON-NLS-2$
        }
        finally
        {
          IOUtils.closeQuietly( FileIS );
        }
      }
    }
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

  private void loadLogs( final File tmpDir, final Logger logger, final NAConfiguration conf, final File resultDir )
  {
    final File logFile = new File( tmpDir, "start/error.gml" ); //$NON-NLS-1$

    if( logFile.exists() )
    {
      try
      {
        final GMLWorkspace naFortranLogWorkspace = GmlSerializer.createGMLWorkspace( logFile.toURI().toURL(), null );
        final GMLWorkspace changedNAFortranLogWorkspace = readLog( naFortranLogWorkspace, conf );
        GmlSerializer.serializeWorkspace( logFile, changedNAFortranLogWorkspace, "UTF-8" ); //$NON-NLS-1$
      }
      catch( final Exception e1 )
      {
        // TODO Auto-generated catch block
        e1.printStackTrace();
      }

      final String resultPathRelative = "Ergebnisse/Aktuell/Log/error.gml"; //$NON-NLS-1$
      final String inputPath = logFile.getPath();
      final File resultFile = new File( resultDir, resultPathRelative );
      resultFile.getParentFile().mkdirs();
      FileInputStream fortranLogFileIS = null;
      try
      {
        fortranLogFileIS = new FileInputStream( logFile );
      }
      catch( final FileNotFoundException e1 )
      {
        e1.printStackTrace();
      }
      try
      {
        FileUtilities.makeFileFromStream( false, resultFile, fortranLogFileIS );
      }
      catch( final IOException e )
      {
        e.printStackTrace();
        System.out.println( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.228" ) + inputPath + Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.229" ) ); //$NON-NLS-1$ //$NON-NLS-2$
      }
      finally
      {
        IOUtils.closeQuietly( fortranLogFileIS );
      }

      final Handler[] handlers = logger.getHandlers();
      for( final Handler h : handlers )
      {
        h.flush();
        h.close();
      }
    }
  }

  private GMLWorkspace readLog( final GMLWorkspace naFortranLogWorkspace, final NAConfiguration conf ) throws ParseException
  {
    final IDManager idManager = conf.getIdManager();
    final IFeatureType recordFT = naFortranLogWorkspace.getGMLSchema().getFeatureType( new QName( NaModelConstants.NS_NAFORTRANLOG, "record" ) ); //$NON-NLS-1$
    final Feature[] recordFEs = naFortranLogWorkspace.getFeatures( recordFT );
    for( final Feature feature : recordFEs )
    {
      final String elementString = (String) feature.getProperty( new QName( NaModelConstants.NS_NAFORTRANLOG, "element" ) ); //$NON-NLS-1$
      final int i = elementString.indexOf( "       " ); //$NON-NLS-1$
      final String element = elementString.substring( 0, i );
      final String fortranID = elementString.substring( i ).trim();
      Integer asciiID = 0;
      String asciiType = null;
      int type = 0;
      if( !fortranID.equals( "" ) ) //$NON-NLS-1$
        asciiID = NumberUtils.toInteger( fortranID );
      if( (elementString.contains( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.234" ) )) ) //$NON-NLS-1$
      {
        type = IDManager.CATCHMENT;
        asciiType = "Teilgebiet";
      }
      if( (elementString.contains( "Knoten" )) )
      {
        type = IDManager.NODE;
        asciiType = "Knoten";
      }
      if( (elementString.contains( "Strang" )) )
      {
        type = IDManager.CHANNEL;
        asciiType = "Strang";
      }
      try
      {
        final String gmlName = (String) (idManager.getFeature( asciiID, type )).getProperty( NaModelConstants.GML_FEATURE_NAME_PROP );

        // String FeatureID = idManager.getFeature( asciiID, type ).getId();
        feature.setProperty( NaModelConstants.GML_FEATURE_NAME_PROP, asciiType + " " + gmlName.trim() ); //$NON-NLS-1$
        feature.setProperty( NaModelConstants.GML_FEATURE_DESCRIPTION_PROP, (Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.241" ) + gmlName.trim()) ); //$NON-NLS-1$
      }
      catch( final Exception e )
      {
        System.out.println( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.242" ) + element + " " + fortranID ); //$NON-NLS-1$ //$NON-NLS-2$
      }
    }
    return naFortranLogWorkspace;
  }

  private void copyExecutable( final File basedir ) throws Exception
  {
    final String exeResource = m_resourceBase + m_kalypsoKernelPath;
    final File destFile = new File( basedir, m_kalypsoKernelPath );
    if( !destFile.exists() )
    {
      try
      {
        final InputStream inputStream = getClass().getResourceAsStream( exeResource );
        FileUtilities.makeFileFromStream( false, destFile, inputStream );
        System.out.println( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.244" ) ); //$NON-NLS-1$
      }
      catch( final Exception e )
      {
        e.printStackTrace();

        System.out.println( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.245" ) + exeResource + Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.246" ) ); //$NON-NLS-1$ //$NON-NLS-2$
      }
    }

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

  private void unzipInput( final URL asciiZipURL, final File exeDir )
  {
    try
    {
      final InputStream openStream = asciiZipURL.openStream();
      ZipUtilities.unzip( openStream, exeDir );
      IOUtils.closeQuietly( openStream );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  private void startCalculation( final File basedir, final ISimulationMonitor monitor ) throws SimulationException
  {
    final File exeFile = new File( basedir, m_kalypsoKernelPath );
    final File exeDir = exeFile.getParentFile();
    final String commandString = exeFile.getAbsolutePath();
    final long timeOut = 1000l * 60l * 60l; // max 60 minutes
    FileOutputStream logOS = null;
    FileOutputStream errorOS = null;
    try
    {
      logOS = new FileOutputStream( new File( basedir, "exe.log" ) ); //$NON-NLS-1$
      errorOS = new FileOutputStream( new File( basedir, "exe.err" ) ); //$NON-NLS-1$
      ProcessHelper.startProcess( commandString, new String[0], exeDir, monitor, timeOut, logOS, errorOS, null );
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

  public boolean isSucceeded( )
  {
    return m_succeeded;
  }
}