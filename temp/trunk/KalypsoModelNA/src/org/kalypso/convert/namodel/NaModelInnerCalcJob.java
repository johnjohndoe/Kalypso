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

import org.apache.commons.io.CopyUtils;
import org.apache.commons.io.IOUtils;
import org.kalypso.convert.namodel.optimize.CalibarationConfig;
import org.kalypso.convert.namodel.optimize.NAOptimizingJob;
import org.kalypso.convert.namodel.timeseries.BlockTimeSeries;
import org.kalypso.convert.namodel.timeseries.DummyTimeSeriesWriter;
import org.kalypso.java.io.FileCopyVisitor;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.java.io.filter.MultipleWildCardFileFilter;
import org.kalypso.java.net.UrlUtilities;
import org.kalypso.java.util.zip.ZipUtilities;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
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
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.gml.schema.XMLHelper;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.GMLWorkspace_Impl;
import org.w3c.dom.Document;

/**
 * @author doemming, huebsch
 */
public class NaModelInnerCalcJob implements ICalcJob
{

  // resourcebase for static files used in calculation
  private final String m_resourceBase = "template/";

  // TODO hier noch was machen
  //  private final String EXE_FILE = "start/kalypso_WeisseElster.exe";
//  private final String EXE_FILE = "start/kalypso_2.01_3000.exe";
  private final String EXE_FILE = "start/kalypso_2.01_7500.exe";

  private boolean m_succeeded = false;

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
   *      org.kalypso.services.calculation.job.ICalcDataProvider,
   *      org.kalypso.services.calculation.job.ICalcResultEater,
   *      org.kalypso.services.calculation.job.ICalcMonitor)
   */
  public void run( File tmpdir, ICalcDataProvider inputProvider, ICalcResultEater resultEater, ICalcMonitor monitor ) throws CalcJobServiceException
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
      }

      // generiere ascii-dateien
      monitor.setMessage( "generiere ASCII-Dateien (Modelldaten und Zeitreihen)" );
      if( monitor.isCanceled() )
        return;

      final GMLWorkspace modellWorkspace = generateASCII( tmpdir, inputProvider );

      // kopiere executable aus resourcen:
      if( monitor.isCanceled() )
        return;
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
        loadResults( tmpdir, modellWorkspace, logger, resultDir, resultEater );
      }
      System.out.println( "fertig" );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw new CalcJobServiceException( "Simulation konnte nicht durchgefuehrt werden", e );
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
        if( line.indexOf( "berechnung wurde ohne fehler beendet" ) >= 0 )
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

  private GMLWorkspace generateASCII( File tmpDir, ICalcDataProvider dataProvider ) throws Exception
  {
    final File newModellFile = new File( tmpDir, "namodellBerechnung.gml" );

    // calualtion model
    final URL newModellURL = newModellFile.toURL();
    final NAConfiguration conf = NAConfiguration.getGml2AsciiConfiguration( newModellURL, tmpDir );

    final GMLWorkspace metaWorkspace = GmlSerializer.createGMLWorkspace( dataProvider.getURLForID( NaModelConstants.IN_META_ID ) );
    final Feature metaFE = metaWorkspace.getRootFeature();
    final GMLWorkspace controlWorkspace = GmlSerializer.createGMLWorkspace( dataProvider.getURLForID( NaModelConstants.IN_CONTROL_ID ) );

    //  model Hydrotop
    final GMLWorkspace hydrotopWorkspace;
    if( dataProvider.hasID( NaModelConstants.IN_HYDROTOP_ID ) )
      hydrotopWorkspace = GmlSerializer.createGMLWorkspace( dataProvider.getURLForID( NaModelConstants.IN_HYDROTOP_ID ) );
    else
      hydrotopWorkspace = null;

    //model Parameter
    final GMLWorkspace parameterWorkspace;
    if( dataProvider.hasID( NaModelConstants.IN_PARAMETER_ID ) )
      parameterWorkspace = GmlSerializer.createGMLWorkspace( dataProvider.getURLForID( NaModelConstants.IN_PARAMETER_ID ) );
    else
      parameterWorkspace = null;

    // initialize model with values of control file
    initializeModell( controlWorkspace.getRootFeature(), dataProvider.getURLForID( NaModelConstants.IN_MODELL_ID ), newModellFile );

    final GMLWorkspace modellWorkspace = GmlSerializer.createGMLWorkspace( newModellURL );
    ( (GMLWorkspace_Impl)modellWorkspace ).setContext( dataProvider.getURLForID( NaModelConstants.IN_MODELL_ID ) );
    conf.setSimulationStart( (Date)metaFE.getProperty( "startsimulation" ) );
    conf.setSimulationForecasetStart( (Date)metaFE.getProperty( "startforecast" ) );
    // TODO add endsimulation [in hours after forecast start] in control.xsd and
    // use it here
    // TODO change also in NAControlConverter

    Date startForecastDate = (Date)metaWorkspace.getRootFeature().getProperty( "startforecast" );

    //  , "yyyy MM dd HH",
    //       "notset" );
    Calendar c = Calendar.getInstance();
    c.setTime( startForecastDate );
    // FETTES TODO
    //    c.add( Calendar.DATE, 2 );
    Date endDate = c.getTime();
    conf.setSimulationEnd( endDate );
    conf.setRootNodeID( (String)controlWorkspace.getRootFeature().getProperty( "rootNode" ) );

    // generate control files
    NAControlConverter.featureToASCII( conf, tmpDir, controlWorkspace, modellWorkspace );

    // update model with factor values from control
    updateFactorParameter( modellWorkspace );

    // modell files
    NAModellConverter.featureToAscii( conf, modellWorkspace, parameterWorkspace, hydrotopWorkspace );

    // create temperatur und verdunstung timeseries
    File tmpFile = new File( tmpDir, "klima.dat/std.tmp" );
    File verFile = new File( tmpDir, "klima.dat/std.ver" );
    final DummyTimeSeriesWriter writer = new DummyTimeSeriesWriter( conf.getSimulationStart(), conf.getSimulationEnd() );

    if( !tmpFile.exists() )
    {
      writer.writeTmpFile( new File( tmpDir, "klima.dat/std.tmp" ) );
    }
    if( !verFile.exists() )
    {
      writer.writeVerdFile( new File( tmpDir, "klima.dat/std.ver" ) );
    }
    return modellWorkspace;
  }

  private void initializeModell( Feature controlFeature, URL inputModellURL, File outputModelFile ) throws IOException, Exception
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
   * some parameter have factors that must be processed before generating
   * asciifiles, as these factors do not occur in ascci-format
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

  private void loadResults( final File inputDir, final GMLWorkspace modellWorkspace, final Logger logger, final File outputDir,
      ICalcResultEater resultEater ) throws Exception
  {
    loadTSResults( inputDir, modellWorkspace, logger, outputDir );
    loadLogs( inputDir, logger, resultEater );
    File[] files = outputDir.listFiles();
    // TODO change for Ergebnis/Berechnungen
    if( files != null )
    {
      for( int i = 0; i < files.length; i++ )
      {
        if( files[i].isDirectory() )
        {
          resultEater.addResult( NaModelConstants.OUT_ZML, files[i].listFiles()[0] );
          return;
        }
      }
    }
    //    resultEater.addResult( NaModelConstants.OUT_ZML, outputDir );
  }

  private String getTitleForSuffix( String suffix )
  {
    //    j Gesamtabfluss Knoten .qgs
    if( suffix.equalsIgnoreCase( "qgs" ) )
      return "Abfluss";
    //    n Schnee .sch
    if( suffix.equalsIgnoreCase( "sch" ) )
      return "Schnee";
    //    n Bodenspeicher .bsp
    if( suffix.equalsIgnoreCase( "bsp" ) )
      return "Bodenspeicher";
    //    n Gesamtabfluss TG .qgg
    if( suffix.equalsIgnoreCase( "qgg" ) )
      return "Gesamtabfluss TG";
    //    n Kluftgrundw1 .qt1
    if( suffix.equalsIgnoreCase( "qt1" ) )
      return "Kluftgrundw1";
    //    n Kluftgrundw .qtg
    if( suffix.equalsIgnoreCase( "qtg" ) )
      return "Kluftgrundw";
    //    n Grundwasser .qgw
    if( suffix.equalsIgnoreCase( "qgw" ) )
      return "Grundwasser";
    //    n Kapil.Aufstieg/Perkolation .kap
    if( suffix.equalsIgnoreCase( "kap" ) )
      return "Kapil.Aufstieg/Perkolation";
    //    n Evapotranspiration .vet
    if( suffix.equalsIgnoreCase( "vet" ) )
      return "Evapotranspiration";
    //    n Ausgabe hydrotope .hyd
    if( suffix.equalsIgnoreCase( "hyd" ) )
      return "Ausgabe hydrotope";
    //    n Abflussbilanz .bil
    if( suffix.equalsIgnoreCase( "bil" ) )
      return "Abflussbilanz";
    //    n Statistische Abflusswerte .nmq
    if( suffix.equalsIgnoreCase( "nmq" ) )
      return "Statistische Abflusswerte";
    //    n Speicherinhalt .spi
    if( suffix.equalsIgnoreCase( "spi" ) )
      return "Speicherinhalt";
    //    n Speicherueberlauf .sup
    if( suffix.equalsIgnoreCase( "sup" ) )
      return "Speicherueberlauf";
    //    n Wasserstand Speicher .sph
    if( suffix.equalsIgnoreCase( "sph" ) )
      return "Wasserstand Speicher";
    //    n Talsperrenverdunstung .spv
    if( suffix.equalsIgnoreCase( "spv" ) )
      return "Talsperrenverdunstung";
    //    n Zehrung .spn
    if( suffix.equalsIgnoreCase( "spn" ) )
      return "Zehrung";
    //    n Evaporation .vep
    if( suffix.equalsIgnoreCase( "vep" ) )
      return "Evaporation";
    //    j Temperatur .tmp
    if( suffix.equalsIgnoreCase( "tmp" ) )
      return "Temperatur";
    //    j Bodenfeuchte .bof
    if( suffix.equalsIgnoreCase( "bof" ) )
      return "Bodenfeuchte";
    //    n Grundwasserstand .gws
    if( suffix.equalsIgnoreCase( "gws" ) )
      return "Grundwasserstand";
    //    n Basisabfluss .qbs
    if( suffix.equalsIgnoreCase( "qbs" ) )
      return "Basisabfluss";
    //    n Oberflaechenabfluss .qna
    if( suffix.equalsIgnoreCase( "qna" ) )
      return "Oberflaechenabfluss";
    //    n Abfluss vers. Flaechen .qvs
    if( suffix.equalsIgnoreCase( "qvs" ) )
      return "Abfluss vers. Flaechen";
    //    j Niederschlag .pre
    if( suffix.equalsIgnoreCase( "pre" ) )
      return "Niederschlag";
    //    n Interflow .qif
    if( suffix.equalsIgnoreCase( "qif" ) )
      return "Interflow";
    return suffix;
  }

  private void loadTSResults( File inputDir, GMLWorkspace modellWorkspace, Logger logger, File outputDir ) throws Exception
  {
    //    j Gesamtabfluss Knoten .qgs
    FeatureType nodeFT = modellWorkspace.getFeatureType( "Node" );
    loadTSResults( "qgs", nodeFT, "num", "name", TimeserieConstants.TYPE_RUNOFF, "pegelZR", "qberechnetZR", inputDir, modellWorkspace, logger,
        outputDir, 1.0d );
    //    n Schnee .sch
    //    n Bodenspeicher .bsp
    //    n Kluftgrundw1 .qt1
    //    n Kluftgrundw .qtg
    //    n Kapil.Aufstieg/Perkolation .kap
    //    n Evapotranspiration .vet
    //    n Ausgabe hydrotope .hyd
    //    n Abflussbilanz .bil
    //    n Statistische Abflusswerte .nmq
    //    n Speicherinhalt .spi
    //    n Speicherueberlauf .sup
    //    n Talsperrenverdunstung .spv
    //    n Zehrung .spn
    //    n Evaporation .vep
    //    j Bodenfeuchte .bof

    FeatureType catchmentFT = modellWorkspace.getFeatureType( "Catchment" );
    //    j Niederschlag .pre
    loadTSResults( "pre", catchmentFT, "inum", "name", TimeserieConstants.TYPE_RAINFALL, null, null, inputDir, modellWorkspace, logger, outputDir,
        1.0d );
    //    j Temperatur .tmp
    loadTSResults( "tmp", catchmentFT, "inum", "name", TimeserieConstants.TYPE_TEMPERATURE, null, null, inputDir, modellWorkspace, logger, outputDir,
        1.0d );

    //    n Interflow .qif
    loadTSResults( "qif", catchmentFT, "inum", "name", TimeserieConstants.TYPE_RUNOFF, null, null, inputDir, modellWorkspace, logger, outputDir, 1.0d );

    //    n Grundwasser .qgw
    loadTSResults( "qgw", catchmentFT, "inum", "name", TimeserieConstants.TYPE_RUNOFF, null, null, inputDir, modellWorkspace, logger, outputDir, 1.0d );
    //    n Wasserstand Speicher .sph
    loadTSResults( "shp", catchmentFT, "inum", "name", TimeserieConstants.TYPE_WATERLEVEL, null, null, inputDir, modellWorkspace, logger, outputDir,
        1.0d );
    //    n Gesamtabfluss TG .qgg
    loadTSResults( "qgg", catchmentFT, "inum", "name", TimeserieConstants.TYPE_RUNOFF, null, null, inputDir, modellWorkspace, logger, outputDir, 1.0d );

    //    n Grundwasserstand .gws
    loadTSResults( "gws", catchmentFT, "inum", "name", TimeserieConstants.TYPE_WATERLEVEL, null, null, inputDir, modellWorkspace, logger, outputDir,
        100.0d );
    //    n Basisabfluss .qbs
    loadTSResults( "qbs", catchmentFT, "inum", "name", TimeserieConstants.TYPE_RUNOFF, null, null, inputDir, modellWorkspace, logger, outputDir, 1.0d );
    //    n Oberflaechenabfluss .qna
    loadTSResults( "qna", catchmentFT, "inum", "name", TimeserieConstants.TYPE_RUNOFF, null, null, inputDir, modellWorkspace, logger, outputDir, 1.0d );
    //    n Abfluss vers. Flaechen .qvs
    loadTSResults( "qvs", catchmentFT, "inum", "name", TimeserieConstants.TYPE_RUNOFF, null, null, inputDir, modellWorkspace, logger, outputDir, 1.0d );
  }

  private void loadTSResults( String suffix, FeatureType resultFT, String keyPropName, String titlePropName, String resultType,
      String metadataTSLink, String targetTSLink, File inputDir, GMLWorkspace modellWorkspace, Logger logger, File outputDir, double resultFactor )
      throws Exception
  {
    // ASCII-Files
    // generiere ZML Ergebnis Dateien
    final File ascciResultDir = new File( inputDir, "out_we.nat" );
    MultipleWildCardFileFilter filter = new MultipleWildCardFileFilter( new String[]
    { "*" + suffix+"*" }, false, false, true );
    File[] qgsFiles = ascciResultDir.listFiles( filter );
    if( qgsFiles.length != 0 )
    {
      // read ascii result file
      logger.info( "lese ergebnissdatei " + qgsFiles[0].getName() + "\n" );
      final BlockTimeSeries ts = new BlockTimeSeries();
      ts.importBlockFile( qgsFiles[0] );

      // iterate model nodes and generate zml

      final Feature[] nodeFEs = modellWorkspace.getFeatures( resultFT );
      for( int i = 0; i < nodeFEs.length; i++ )
      {
        final Feature feature = nodeFEs[i];
        if( !FeatureHelper.booleanIsTrue( feature, "generateResult", false ) )
          continue; // should not generate results
        final String key = FeatureHelper.getAsString( feature, keyPropName );
        final String feID = feature.getId();
        final String feName = (String)feature.getProperty( titlePropName );
        final String title;
        if( feName != null )
          title = "Berechnung " + getTitleForSuffix( suffix ) + " " + feName + " (" + feID + ")";
        else
          title = "Berechnung " + getTitleForSuffix( suffix ) + " (" + feID + ")";

        if( !ts.dataExistsForKey( key ) )
          continue; // no results available
        logger.info( "lese berechnetes Ergebnis fuer #" + key + "\n" );

        // transform data to tuppelmodel
        final SortedMap data = ts.getTimeSerie( key );
        final Object[][] tupelData = new Object[data.size()][2];
        final Set dataSet = data.entrySet();
        final Iterator iter = dataSet.iterator();
        int pos = 0;
        while( iter.hasNext() )
        {
          Map.Entry entry = (Map.Entry)iter.next();
          tupelData[pos][0] = (Date)entry.getKey();
          tupelData[pos][1] = new Double( Double.parseDouble( entry.getValue().toString() ) * resultFactor );
          pos++;
        }

        final IAxis dateAxis = new DefaultAxis( "Datum", TimeserieConstants.TYPE_DATE, "", Date.class, true );

        final IAxis qAxis = new DefaultAxis( getTitleForSuffix( suffix ) + "_" + title, resultType, TimeserieUtils.getUnit( resultType ),
            Double.class, false );
        IAxis[] axis = new IAxis[]
        {
            dateAxis,
            qAxis };
        ITuppleModel qTuppelModel = new SimpleTuppleModel( axis, tupelData );

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
                TimeserieConstants.MD_VORHERSAGE } );

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
          // if there is target defined or there are some problems with that we
          // generate one
          resultPathRelative = "Ergebnisse/Berechnet/" + feature.getFeatureType().getName() + "/" + suffix + "_" + key + "_" + feature.getId()
              + ".zml";
        }

        final File resultFile = new File( outputDir, resultPathRelative );
        resultFile.getParentFile().mkdirs();

        // create observation object
        //        final IObservation resultObservation = new SimpleObservation(
        // pegelLink.getHref(), "ID", title, false, null, metadataList, axis,
        // qTuppelModel );
        final IObservation resultObservation = new SimpleObservation( resultPathRelative, "ID", title, false, null, metadataList, axis, qTuppelModel );

        // write result
        final ObservationType observationType = ZmlFactory.createXML( resultObservation, null );
        final Marshaller marshaller = ZmlFactory.getMarshaller();
        marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );

        FileOutputStream stream = new FileOutputStream( resultFile );
        OutputStreamWriter writer = new OutputStreamWriter( stream, "UTF-8" );
        marshaller.marshal( observationType, writer );
        IOUtils.closeQuietly( writer );
        IOUtils.closeQuietly( stream );
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

    final File logDir = new File( tmpDir, "start" );
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
      resultEater.addResult( NaModelConstants.LOG_OUTERR_ID, new File( logDir, "start/output.err" ) );
    }
    catch( CalcJobServiceException e )
    {
      e.printStackTrace();
      logger.info( e.getMessage() );
    }
  }

  private void copyExecutable( File basedir ) throws Exception
  {
    final String exeResource = m_resourceBase + EXE_FILE;
    final File destFile = new File( basedir, EXE_FILE );
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
    InputStreamReader inStream = null;
    InputStreamReader errStream = null;
    PrintWriter outwriter = null;
    PrintWriter errwriter = null;

    try
    {
      final File exeFile = new File( basedir, EXE_FILE );
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
          // TODO mit remote debugging testen ob dies auch im tomcat
          // funktioniert - sollte eigentlich
          process.destroy();
          return;
        }
        Thread.sleep( 100 );
      }
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      throw new CalcJobServiceException( "Fehler beim Ausfuehren", e );
    }
    catch( final InterruptedException e )
    {
      e.printStackTrace();
      throw new CalcJobServiceException( "Fehler beim Ausfuehren", e );
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

  public boolean isSucceeded()
  {
    return m_succeeded;
  }

}