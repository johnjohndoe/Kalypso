package org.kalypso.convert.namodel;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.PrintWriter;
import java.io.Writer;
import java.net.URL;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;

import javax.xml.bind.Marshaller;

import org.apache.commons.io.CopyUtils;
import org.apache.commons.io.FileUtils;
import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureProperty;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree_impl.gml.schema.GMLSchema;
import org.deegree_impl.model.feature.FeatureFactory;
import org.deegree_impl.model.feature.FeatureHelper;
import org.kalypso.convert.namodel.timeseries.BlockTimeSeries;
import org.kalypso.convert.namodel.varymodel.CalibarationConfig;
import org.kalypso.convert.namodel.varymodel.ModelVary;
import org.kalypso.convert.namodel.varymodel.XMLServiceTools;
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
public class NaModelCalcJob extends AbstractCalcJob
{
  // IDs
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
      "out_we.nat",
      "tmp" };

  // resourcebase for static files used in calculation
  private final String m_resourceBase = "template/";

  // static resources under resourcebase

  private final String EXE_FILE = "start/kalypso.exe";

  private final String TEMPLATE_CONF_FILE = "misc/resourceFile.conf";


  public void run( File basedir, CalcJobDataBean[] input ) throws CalcJobServiceException
  {
    final File tmpDir=new File(basedir,"tmp");
    tmpDir.mkdirs();
    final File outDir=new File(basedir,ICalcServiceConstants.OUTPUT_DIR_NAME);
    outDir.mkdirs();
    final StringBuffer logBuffer = new StringBuffer();
    if( !basedir.exists() )
      basedir.mkdirs();
    final File simDir=new File(basedir,ICalcServiceConstants.INPUT_DIR_NAME);
    try
    {
      setMessage( "richte Berechnungsverzeichnis ein" );
      if( isCanceled() )
        return;
      prepareBaseDir( simDir );
      // kopiere template aus resourcen:
      if( isCanceled() )
        return;
      copyTemplates( simDir );
      // generiere ascii-dateien
      setMessage( "generiere ASCII-Dateien (Modelldaten und Zeitreihen)" );
      if( isCanceled() )
        return;
      NAConfiguration conf = generateASCII( simDir, input,tmpDir,outDir );
      // starte berechnung
      setMessage( "starte Simulationskern" );
      if( isCanceled() )
        return;
      startCalculation( simDir ); // TODO
      // ergebnisse aufbereiten
      setMessage( "lade Ergebnisse" );
      loadResults( simDir, conf, logBuffer,basedir,outDir);

      System.out.println( "fertig" );
    }
    catch( Exception e )
    {
      throw new CalcJobServiceException( "Simulation konnte nicht durchgefuehrt werden", e );
    }
  }

  private NAConfiguration generateASCII( File simDir, CalcJobDataBean[] beans,File tmpDir, File outDir) throws Exception
  {
    // input model
    final CalcJobDataBean modellBean = CalcJobHelper.getBeanForId( MODELL_ID, beans );
    final URL inputModellURL = new File( simDir,modellBean.getPath() ).toURL();

    final File modellFile = new File( simDir, "namodellBerechnung.gml" );

    // calualtion model
    final URL modellURL = modellFile.toURL();
    final NAConfiguration conf = NAConfiguration.getGml2AsciiConfiguration( modellURL, simDir,tmpDir );

    // control
    final CalcJobDataBean controlBean = CalcJobHelper.getBeanForId( CONTROL_ID, beans );
    final File controlFile = new File(simDir, controlBean.getPath() );
    final URL controlURL = controlFile.toURL();
    final GMLWorkspace controlWorkspace = GmlSerializer.createGMLWorkspace( controlURL, conf
        .getControlSchemaURL() );

    // initialize model with values of control file
    initializeModell( controlWorkspace.getRootFeature(), inputModellURL, modellFile );

    FileUtils.copyFile( controlFile, new File(outDir, "control.gml" ) );
    FileUtils.copyFile( modellFile, new File( outDir, "model.gml" ) );

    final GMLWorkspace modellWorkspace = GmlSerializer.createGMLWorkspace( modellURL, conf
        .getSchemaURL() );
    conf.setSimulationStart( (Date)controlWorkspace.getRootFeature()
        .getProperty( "startsimulation" ) );
    conf.setSimulationForecasetStart( (Date)controlWorkspace.getRootFeature().getProperty(
        "startforecast" ) );
    conf.setSimulationEnd( (Date)controlWorkspace.getRootFeature().getProperty( "endsimulation" ) );
    conf.setRootNodeID( (String)controlWorkspace.getRootFeature().getProperty( "rootNode" ) );

    // generate control files
    NAControlConverter.featureToASCII( simDir, controlWorkspace, modellWorkspace );

    // update model with factor values from control
    updateFactorParameter( modellWorkspace );

    // modell files
    NAModellConverter.featureToAscii( conf, modellWorkspace );

    // create temperatur und verdunstung timeseries
    DummyTimeSeriesWriter writer = new DummyTimeSeriesWriter( conf.getSimulationStart(), conf
        .getSimulationEnd() );
    writer.writeTmpFile( new File( simDir, "klima.dat/std.tmp" ) );
    writer.writeVerdFile( new File( simDir, "klima.dat/std.ver" ) );
    return conf;
  }

  private void initializeModell( Feature controlFeature, URL inputModellURL, File outputMOdelFile )
      throws IOException, Exception
  {
    CalibarationConfig config = new CalibarationConfig();
    config.addFromNAControl( controlFeature );
    Document modelDoc = XMLServiceTools.getXML( inputModellURL.openStream() );
    ModelVary.initializeModel( modelDoc, config.getCalContexts() );
    XMLServiceTools.toFile( outputMOdelFile, modelDoc );
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

  /**
   * some parameter have factors that must be processed before generating
   * asciifiles, as these factors do not occur in ascci-format
   */
  private void updateFactorParameter( GMLWorkspace modellWorkspace )
  {
    final GMLSchema schema = modellWorkspace.getSchema();

    // Catchments
    final Feature[] catchmentFEs = modellWorkspace
        .getFeatures( schema.getFeatureType( "Catchment" ) );
    update( catchmentFEs, m_catchmentFactorParameterTarget, m_catchmentFactorsParameter );

    // KMChannels
    final Feature[] kmChanneFEs = modellWorkspace
        .getFeatures( schema.getFeatureType( "KMChannel" ) );
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

  private void loadResults( File inputDir, NAConfiguration conf, StringBuffer log,File baseDir,File outputDir ) throws Exception
  {
    // load modeldata
    final File controlFile = new File( outputDir, "control.gml" );
    final File modelFile = new File(outputDir, "model.gml" );
    // create GML workspaces on result models
    final GMLWorkspace modelWorkspace = GmlSerializer.createGMLWorkspace( modelFile.toURL(), conf
        .getSchemaURL() );
    final GMLWorkspace controlWorkspace = GmlSerializer.createGMLWorkspace( controlFile.toURL(),
        conf.getControlSchemaURL() );
    loadTSResults( inputDir, modelWorkspace,  log,outputDir );
    GmlSerializer.serializeFeature( new FileWriter( controlFile ), controlWorkspace
        .getRootFeature(), null );
    GmlSerializer.serializeFeature( new FileWriter( modelFile ), modelWorkspace.getRootFeature(),
        null );
//    loadLogs( baseDir, GMLWorkspace modellWorkspace, StringBuffer log );
    
  }

  private void loadTSResults( File simDir, GMLWorkspace modellWorkspace,
       StringBuffer log,File outputDir ) throws Exception
  {
    // ASCII-Files
    // generiere ZML Ergebnis Dateien
    final File outDir = new File( simDir, "out_we.nat" );
    MultipleWildCardFileFilter filter = new MultipleWildCardFileFilter( new String[]
    { "*.qgs" }, false, false, true );
    File[] qgsFiles = outDir.listFiles( filter );
    if( qgsFiles.length != 0 )
    {    
      // read ascii result file
      log.append( "lese ergebnissdatei " + qgsFiles[0].getName() + "\n" );
      final BlockTimeSeries ts = new BlockTimeSeries();
      ts.importBlockFile( qgsFiles[0] );

      // iterate model nodes and generate zml
      final FeatureType nodeFT = modellWorkspace.getSchema().getFeatureType( "Node" );
      final Feature[] nodeFEs = modellWorkspace.getFeatures( nodeFT );
      for( int i = 0; i < nodeFEs.length; i++ )
      {
        final Feature feature = nodeFEs[i];
        if( !FeatureHelper.booleanIsTrue( feature, "generateResult", false ) )
          continue; // should not generate results
        final String key = FeatureHelper.getAsString( feature, "num" );

        if( !ts.dataExistsForKey( key ) )
          continue; // no results available
        log.append( "lese berechneten Abfluss fuer Knoten #" + key + "\n" );

        // transform data to tuppelmodel
        final SortedMap data = ts.getTimeSerie( key );
        final Object[][] tupelData = new Object[data.size()][2];
        final Set dataSet = data.entrySet();
        Iterator iter = dataSet.iterator();
        int pos = 0;
        while( iter.hasNext() )
        {
          Map.Entry entry = (Map.Entry)iter.next();
          tupelData[pos][0] = entry.getKey();
          tupelData[pos][1] = entry.getValue();
          pos++;
          final IAxis dateAxis = new DefaultAxis( "Datum", TimeserieConstants.TYPE_DATE, "",
              Date.class, 1, true );
          final IAxis qAxis = new DefaultAxis( "Abfluss", TimeserieConstants.TYPE_RUNOFF, "qm/s",
              Double.class, 2, false );
          IAxis[] axis = new IAxis[]
          {
              dateAxis,
              qAxis };
          ITuppleModel qTuppelModel = new SimpleTuppleModel( axis, tupelData );
          final MetadataList metaData = null; // TODO werden richtige  MetaDaten ... von
          // Knoten bzw. Pegel uebernommen ??
          final IObservation resultObservation = new SimpleObservation( "ID",
              "SimulationsErgebnis", false, null, metaData, axis, qTuppelModel );
          // if pegel exists, copy metadata (inclusive wq-function)
          final TimeseriesLink pegelLink = (TimeseriesLink)feature.getProperty( "pegelZR" );
          if( pegelLink != null )
          {
            log
                .append( "zu diesem Knoten existiert ein Pegel, einige Pegelmetadaten (z.B. Wechmann-Funktion) werden in Ergebniszeitreihe uebernommen\n" );
            final URL pegelURL = UrlUtilities.resolveURL( modellWorkspace.getModelUrl(), pegelLink
                .getHref() );
            final IObservation pegelObservation = ZmlFactory.parseXML( pegelURL, "pegelmessung" );
            copyMetaData( pegelObservation.getMetadataList(), resultObservation.getMetadataList(),
                new String[]
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
                    TimeserieConstants.MD_WQ } );

          }
          // lese ergebnis-link um target fuer zml zu finden
          TimeseriesLink resultLink = (TimeseriesLink)feature.getProperty("qberechnetZR");
          if(resultLink==null)
          {
            log.append("ergebnis konnte nicht geschrieben werden, da ergebnislink nicht gesetzt ist FID=#"+feature.getId()+" .");
            continue;
          }
          String resultPathRelative=resultLink.getHref();
          File resultFile=new File(new File(outputDir,ICalcServiceConstants.RESULT_DIR_NAME),resultPathRelative);

          // write result
          final ObservationType observationType = ZmlFactory.createXML( resultObservation, null );
          final Marshaller marshaller = ZmlFactory.getMarshaller();
          final Writer writer = new FileWriter( resultFile );
          marshaller.marshal( observationType, writer );
          writer.close();        
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
  private void loadLogs( File simDir, StringBuffer log)
  {
    
    final File outDir = new File( simDir, "out_we.nat" );
    // zeitreihen im out Dir
    final File[] outDirResults = outDir.listFiles();
    for( int i = 0; i < outDirResults.length; i++ )
    {
      final File file = outDirResults[i];     
      addResult( new CalcJobDataBean( FileUtilities.getSuffix( file ), file.getName(), file
          .getAbsolutePath() ) );
    }

    // log und error dateien:
    final File logDir = new File( simDir, "out_err" );
    final File logFile = new File( logDir, "" );
    if( logFile.exists() )
      addResult( new CalcJobDataBean( "LOG", "Bericht Simulationskern", logFile.getAbsolutePath() ) );

    final File errFile = new File( logDir, "" );
    if( errFile.exists() )
      addResult( new CalcJobDataBean( "ERR", "Fehlerbericht", errFile.getAbsolutePath() ) );
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

        //        ReaderUtilities.dumpAllAvailable( inStream );
        //        ReaderUtilities.dumpAllAvailable( errStream );

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
          process.destroy();
          return;
        }
        Thread.sleep( 100 );
      }
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      throw new CalcJobServiceException( "Fehler beim Ausf?hren", e );
    }
    catch( final InterruptedException e )
    {
      e.printStackTrace();
      throw new CalcJobServiceException( "Fehler beim Ausf?hren", e );
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

  /**
   * Kopiert eine Datei in den Ausgabeordner und f?gt die entsprechende Bean zur
   * Ausgabe hinzu.
   * 
   * Die Pfade werden wie folgt angelegt:
   * 
   * Das Resultfile wird relativ zu resultdir aufgel?st und unter dem gleichen
   * rleativen Pfad unter das Outputdir abgelegt: z.B.: resultdir
   * C:\tmp\kalypsonatest\exe\ resultfile:
   * C:\tmp\kalypsonatest\exe\out_we.nat\950901.bof Ablage im utputdir:
   * C:\tmp\kalypsonatest\output\out_we.nat\950901.bof pfad in der Bean:
   * .\out_we.nat\950901.bof
   *  
   */
  private void copyResult( final File resultdir, final File resultfile, final File outputdir,
      final String id, final String description )
  {
    final String relativePathTo = FileUtilities.getRelativePathTo( resultdir, resultfile );
    final File outputfile = new File( outputdir, relativePathTo );

    try
    {
      FileUtils.copyFile( resultfile, outputfile );
      addResult( new CalcJobDataBean( id, description, "." + relativePathTo ) );
    }
    catch( IOException e )
    {
      e.printStackTrace();
    }
  }

}