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
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;

import javax.xml.bind.Marshaller;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.io.CopyUtils;
import org.apache.commons.io.IOUtils;
import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureProperty;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree_impl.gml.schema.XMLHelper;
import org.deegree_impl.model.feature.FeatureFactory;
import org.deegree_impl.model.feature.FeatureHelper;
import org.kalypso.convert.namodel.timeseries.BlockTimeSeries;
import org.kalypso.convert.namodel.varymodel.CalibarationConfig;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.java.io.filter.MultipleWildCardFileFilter;
import org.kalypso.java.net.UrlUtilities;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ogc.sensor.zml.ZmlURL;
import org.kalypso.optimize.transform.OptimizeModelUtils;
import org.kalypso.services.calculation.common.ICalcServiceConstants;
import org.kalypso.services.calculation.job.impl.AbstractCalcJob;
import org.kalypso.services.calculation.job.impl.CalcJobHelper;
import org.kalypso.services.calculation.service.CalcJobDataBean;
import org.kalypso.services.calculation.service.CalcJobServiceException;
import org.kalypso.zml.ObservationType;
import org.kalypso.zml.obslink.TimeseriesLink;
import org.w3c.dom.Document;

/**
 * @author doemming
 */
public class NaModelInnerCalcJob extends AbstractCalcJob
{

  // IDs
  public static final String META_ID = "MetaSteuerdaten";

  public final static String MODELL_ID = "Modell";

  public final static String CONTROL_ID = "Control";

  // subdirectories to create
  private final static String[] subDirs =
  {
      "lzsim",
      "start",
      "zufluss",
      "hydro.top",
      "inp.dat",
      "klima.dat",
      "out_we.nat" };

  // resourcebase for static files used in calculation
  private final String m_resourceBase = "template/";

  // static resources under resourcebase

  private final String EXE_FILE = "start/kalypso.exe";

  private final String TEMPLATE_CONF_FILE = "misc/resourceFile.conf";

  private boolean m_succeeded = false;

  public NaModelInnerCalcJob()
  {
    m_urlUtilities = new UrlUtilities();
  }

  public void run( final File basedir, final CalcJobDataBean[] input )
      throws CalcJobServiceException
  {
    final File outDir = new File( basedir, ICalcServiceConstants.OUTPUT_DIR_NAME );
    outDir.mkdirs();
    final StringBuffer logBuffer = new StringBuffer();
    if( !basedir.exists() )
      basedir.mkdirs();
    final File inputDir = new File( basedir, ICalcServiceConstants.INPUT_DIR_NAME );
    //    final File calcDir = new File( inputDir,
    // ICalcServiceConstants.CALC_DIR_NAME );
    final File exeDir = new File( basedir, "sim" );

    try
    {
      setMessage( "richte Berechnungsverzeichnis ein" );
      if( isCanceled() )
        return;
      prepareBaseDir( exeDir );
      // kopiere template aus resourcen:
      if( isCanceled() )
        return;
      copyTemplates( exeDir );
      // generiere ascii-dateien
      setMessage( "generiere ASCII-Dateien (Modelldaten und Zeitreihen)" );
      if( isCanceled() )
        return;
      final GMLWorkspace modellWorkspace = generateASCII( exeDir, inputDir, input, outDir );
      // starte berechnung
      setMessage( "starte Simulationskern" );
      if( isCanceled() )
        return;
      startCalculation( exeDir );
      checkSucceeded( exeDir );
      if( isSucceeded() )
      {
        setMessage( "lade Ergebnisse" );

        loadResults( exeDir, modellWorkspace, logBuffer, outDir );
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

  private GMLWorkspace generateASCII( final File exeDir, final File inputDir,
      final CalcJobDataBean[] beans, File outDir ) throws Exception
  {
    // input model
    final CalcJobDataBean modellBean = CalcJobHelper.getBeanForId( MODELL_ID, beans );
    final File modelFile = new File( inputDir, modellBean.getPath() );

    final URL inputModellURL = modelFile.toURL();
    final File modellFile = new File( modelFile.getParentFile(), "namodellBerechnung.gml" );

    // calualtion model
    final URL modellURL = modellFile.toURL();
    final NAConfiguration conf = NAConfiguration.getGml2AsciiConfiguration( modellURL, exeDir );

    final CalcJobDataBean metaBean = CalcJobHelper.getBeanForId( META_ID, beans );
    final File metaFile = new File( inputDir, metaBean.getPath() );

    final GMLWorkspace metaWorkspace = GmlSerializer.createGMLWorkspace( metaFile.toURL(), conf
        .getMetaSchemaURL() );
    final Feature metaFE = metaWorkspace.getRootFeature();
    // control
    final CalcJobDataBean controlBean = CalcJobHelper.getBeanForId( CONTROL_ID, beans );
    final File controlFile = new File( inputDir, controlBean.getPath() );
    final URL controlURL = controlFile.toURL();
    final GMLWorkspace controlWorkspace = GmlSerializer.createGMLWorkspace( controlURL, conf
        .getControlSchemaURL() );

    // initialize model with values of control file
    initializeModell( controlWorkspace.getRootFeature(), inputModellURL, modellFile );
    // TODO copy result
    //    copyResult( inputDir, modellFile, outDir, MODELL_ID, modellFile.getName()
    // );

    final GMLWorkspace modellWorkspace = GmlSerializer.createGMLWorkspace( modellURL, conf
        .getSchemaURL() );
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
    c.add( Calendar.DATE, 2 );
    Date endDate = c.getTime();
    conf.setSimulationEnd( endDate );
    //    conf.setSimulationEnd( (Date)metaWorkspace.getRootFeature().getProperty(
    // "startforecast" ) );
    //    conf.setSimulationEnd( (Date)metaWorkspace.getRootFeature().getProperty(
    // "endsimulation" ) );
    conf.setRootNodeID( (String)controlWorkspace.getRootFeature().getProperty( "rootNode" ) );

    // generate control files
    NAControlConverter.featureToASCII( conf, exeDir, controlWorkspace, modellWorkspace );

    // update model with factor values from control
    updateFactorParameter( modellWorkspace );

    // modell files
    NAModellConverter.featureToAscii( conf, modellWorkspace );

    // create temperatur und verdunstung timeseries
    final DummyTimeSeriesWriter writer = new DummyTimeSeriesWriter( conf.getSimulationStart(), conf
        .getSimulationEnd() );
    writer.writeTmpFile( new File( exeDir, "klima.dat/std.tmp" ) );
    writer.writeVerdFile( new File( exeDir, "klima.dat/std.ver" ) );
    return modellWorkspace;
  }

  private void initializeModell( Feature controlFeature, URL inputModellURL, File outputMOdelFile )
      throws IOException, Exception
  {
    CalibarationConfig config = new CalibarationConfig();
    config.addFromNAControl( controlFeature );

    Document modelDoc = XMLHelper.getAsDOM( inputModellURL, false );

    OptimizeModelUtils.initializeModel( modelDoc, config.getCalContexts() );

    // TODO: take charset from Document
    final String charset = "UTF-8";
    final Writer writer = new OutputStreamWriter( new FileOutputStream( outputMOdelFile ), charset );
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
    final Feature[] catchmentFEs = modellWorkspace.getFeatures( modellWorkspace
        .getFeatureType( "Catchment" ) );
    update( catchmentFEs, m_catchmentFactorParameterTarget, m_catchmentFactorsParameter );

    // KMChannels
    final Feature[] kmChanneFEs = modellWorkspace.getFeatures( modellWorkspace
        .getFeatureType( "KMChannel" ) );
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
        FeatureProperty valueProp = FeatureFactory.createFeatureProperty( targetPropName,
            new Double( value ) );
        feature.setProperty( valueProp );
      }
    }
  }

  private void loadResults( final File inputDir, final GMLWorkspace modellWorkspace,
      final StringBuffer log, final File outputDir ) throws Exception
  {
    loadTSResults( inputDir, modellWorkspace, log, outputDir );
    loadLogs( inputDir, outputDir, log );
  }

  private void loadTSResults( File simDir, GMLWorkspace modellWorkspace, StringBuffer log,
      File outputDir ) throws Exception
  {
    // ASCII-Files
    // generiere ZML Ergebnis Dateien
    final File ascciResultDir = new File( simDir, "out_we.nat" );
    MultipleWildCardFileFilter filter = new MultipleWildCardFileFilter( new String[]
    { "*.qgs" }, false, false, true );
    File[] qgsFiles = ascciResultDir.listFiles( filter );
    if( qgsFiles.length != 0 )
    {
      // read ascii result file
      log.append( "lese ergebnissdatei " + qgsFiles[0].getName() + "\n" );
      final BlockTimeSeries ts = new BlockTimeSeries();
      ts.importBlockFile( qgsFiles[0] );

      // iterate model nodes and generate zml
      final FeatureType nodeFT = modellWorkspace.getFeatureType( "Node" );
      final Feature[] nodeFEs = modellWorkspace.getFeatures( nodeFT );
      for( int i = 0; i < nodeFEs.length; i++ )
      {
        final Feature feature = nodeFEs[i];
        if( !FeatureHelper.booleanIsTrue( feature, "generateResult", false ) )
          continue; // should not generate results
        final String key = FeatureHelper.getAsString( feature, "num" );
        final String feID = feature.getId();
        final String feName = (String)feature.getProperty( "name" );
        final String title;
        if( feName != null )
          title = "Berechnung " + feName + " (" + feID + ")";
        else
          title = "Berechnung (" + feID + ")";

        if( !ts.dataExistsForKey( key ) )
          continue; // no results available
        log.append( "lese berechneten Abfluss fuer Knoten #" + key + "\n" );

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
          tupelData[pos][1] = new Double( entry.getValue().toString() );
          pos++;
        }

        final IAxis dateAxis = new DefaultAxis( "Datum", TimeserieConstants.TYPE_DATE, "",
            Date.class, true );
        final IAxis qAxis = new DefaultAxis( TimeserieConstants.TYPE_RUNOFF,
            TimeserieConstants.TYPE_RUNOFF, "qm/s", Double.class, false );
        IAxis[] axis = new IAxis[]
        {
            dateAxis,
            qAxis };
        ITuppleModel qTuppelModel = new SimpleTuppleModel( axis, tupelData );

        final MetadataList metadataList = new MetadataList();

        // if pegel exists, copy metadata (inclusive wq-function)
        final TimeseriesLink pegelLink = (TimeseriesLink)feature.getProperty( "pegelZR" );
        if( pegelLink != null )
        {
          final URL pegelURL = m_urlUtilities.resolveURL( modellWorkspace.getContext(), pegelLink
              .getHref() );
          if( ( new File( pegelURL.getFile() ) ).exists() )
          {

            log
                .append( "zu diesem Knoten existiert ein Pegel, einige Pegelmetadaten (z.B. Wechmann-Funktion) werden in Ergebniszeitreihe uebernommen\n" );
            final IObservation pegelObservation = ZmlFactory.parseXML( pegelURL, "pegelmessung" );
            copyMetaData( pegelObservation.getMetadataList(), metadataList, new String[]
            {
                TimeserieConstants.MD_ALARM_1,
                TimeserieConstants.MD_ALARM_2,
                TimeserieConstants.MD_ALARM_3,
                TimeserieConstants.MD_ALARM_4,
                TimeserieConstants.MD_FLUSS,
                TimeserieConstants.MD_FLUSSGEBIET,
                TimeserieConstants.MD_GKH,
                TimeserieConstants.MD_GKR,
                TimeserieConstants.MD_HOEHENANGABEART,
                TimeserieConstants.MD_PEGELNULLPUNKT,
                TimeserieConstants.MD_WQ,
                TimeserieConstants.MD_VORHERSAGE } );

          }
        }
        // lese ergebnis-link um target fuer zml zu finden
        TimeseriesLink resultLink = (TimeseriesLink)feature.getProperty( "qberechnetZR" );
        if( resultLink == null )
        {
          log
              .append( "ergebnis konnte nicht geschrieben werden, da ergebnislink nicht gesetzt ist FID=#"
                  + feature.getId() + " ." );
          continue;
        }
        //        String resultPathRelative = resultLink.getHref();
        String resultPathRelative = ZmlURL.getIdentifierPart( FileUtilities.getRelativePathTo(
            new File( outputDir, "Ergebnisse" ), new File( outputDir, resultLink.getHref() ) ) );

        final File resultFile = new File( outputDir, resultPathRelative );
        //        final File resultFile = new File( outputDir, resultPathRelative );
        resultFile.getParentFile().mkdirs();

        // create observation object
        final IObservation resultObservation = new SimpleObservation( pegelLink.getHref(), "ID",
            title, false, null, metadataList, axis, qTuppelModel );

        // write result
        final ObservationType observationType = ZmlFactory.createXML( resultObservation, null );
        final Marshaller marshaller = ZmlFactory.getMarshaller();
        marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );

        FileOutputStream stream = new FileOutputStream( resultFile );

        //        final FileWriter writer = new FileWriter( resultFile );
        OutputStreamWriter writer = new OutputStreamWriter( stream, "UTF-8" );
        marshaller.marshal( observationType, writer );
        writer.close();

        addResult( new CalcJobDataBean( "ERG" + feature.getId(), "Berechnungsergebnis zu Knoten #"
            + feature.getId(), resultPathRelative ) );
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

  private void addDirToResults( File inputbaseDir, String inputDirName, File outputFolder )
  {
    final File inpDir = new File( inputbaseDir, inputDirName );
    // inputdateien
    final File[] inpDirResults = inpDir.listFiles();
    for( int i = 0; i < inpDirResults.length; i++ )
    {
      final File file = inpDirResults[i];
      copyResult( inputbaseDir, file, outputFolder, file.getName(), file.getName() );
    }
  }

  private void loadLogs( final File simDir, final File outputdir, final StringBuffer log )
  {
    // TODO: use it, or REMOVE it
    log.getClass();

    addDirToResults( simDir, "inp.dat", outputdir );
    addDirToResults( simDir, "start", outputdir );
    addDirToResults( simDir, "klima.dat", outputdir );
    addDirToResults( simDir, "out_we.nat", outputdir );
    // log und error dateien:
    final File logDir = new File( simDir, "start" );
    final File logFile = new File( logDir, "output.res" );
    if( logFile.exists() )
      copyResult( logDir, logFile, outputdir, FileUtilities.getSuffix( logFile ), logFile.getName() );

    final File errFile = new File( logDir, "output.err" );
    if( errFile.exists() )
      copyResult( logDir, errFile, outputdir, FileUtilities.getSuffix( errFile ), errFile.getName() );

    final File exeerrFile = new File( simDir, "exe.err" );
    if( exeerrFile.exists() )
      copyResult( simDir, exeerrFile, outputdir, FileUtilities.getSuffix( exeerrFile ), exeerrFile
          .getName() );

    final File exelogFile = new File( simDir, "exe.log" );
    if( exelogFile.exists() )
      copyResult( simDir, exelogFile, outputdir, FileUtilities.getSuffix( exelogFile ), exelogFile
          .getName() );
  }

  private void prepareBaseDir( File baseDir )
  {
    for( int i = 0; i < subDirs.length; i++ )
      ( new File( baseDir, subDirs[i] ) ).mkdirs();
  }

  private void copyTemplates( File basedir ) throws IOException
  {
    String[] templateResources = getTemplateResources();
    for( int i = 0; i < templateResources.length; i++ )
    {
      final File destFile = new File( basedir, templateResources[i] );
      final String resource = m_resourceBase + templateResources[i];
      System.out.print( "resource: " + resource );
      if( !destFile.exists() )
      {
        try
        {
          final InputStream inputStream = getClass().getResourceAsStream( resource );
          FileUtilities.makeFileFromStream( false, destFile, inputStream );
          System.out.println( " ...copied" );
        }
        catch( Exception e )
        {
          e.printStackTrace();

          System.out.println( "ERR: " + resource + " max not exist" );
        }
      }
      else
        System.out.println( " exists" );
    }
  }

  private String[] getTemplateResources() throws IOException
  {
    List result = new ArrayList();
    LineNumberReader reader = new LineNumberReader( new InputStreamReader( getClass()
        .getResourceAsStream( TEMPLATE_CONF_FILE ) ) );
    String line = null;
    try
    {
      while( ( line = reader.readLine() ) != null )
        if( !line.startsWith( "#" ) )
          result.add( line );
    }
    catch( IOException e )
    {
      throw e;
    }
    finally
    {
      reader.close();
    }
    return (String[])result.toArray( new String[result.size()] );
  }

  private void startCalculation( final File basedir ) throws CalcJobServiceException
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

        if( isCanceled() )
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