package org.kalypso.convert.namodel;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.PrintWriter;
import java.net.URL;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.apache.commons.io.CopyUtils;
import org.apache.commons.io.FileUtils;
import org.deegree.model.feature.GMLWorkspace;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.services.calculation.common.ICalcServiceConstants;
import org.kalypso.services.calculation.job.impl.AbstractCalcJob;
import org.kalypso.services.calculation.job.impl.CalcJobHelper;
import org.kalypso.services.calculation.service.CalcJobDataBean;
import org.kalypso.services.calculation.service.CalcJobServiceException;

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
      "out_we.nat" };

  // resourcebase for static files used in calculation
  private final String m_resourceBase = "template/";

  // static resources under resourcebase

  private final String EXE_FILE = "start/kalypso.exe";

  private final String TEMPLATE_CONF_FILE = "misc/resourceFile.conf";

  public void run( final File basedir, final CalcJobDataBean[] input )
      throws CalcJobServiceException
  {
    // sollte schon da sein
    //    if( !basedir.exists() )
    //      basedir.mkdirs();
    // erstmal ein Verzeichnis für die generierten Daten erzeugen
    final File exedir = new File( basedir, "exe" );

    // in diesem Verzeichnis liegen die Input-Dateien
    final File inputdir = new File( basedir, ICalcServiceConstants.INPUT_DIR_NAME );
    final File outputdir = new File( basedir, ICalcServiceConstants.OUTPUT_DIR_NAME );

    try
    {
      setMessage( "richte Berechnungsverzeichnis ein" );
      if( !isCanceled() )
        prepareBaseDir( exedir );
      // kopiere template aus resourcen:
      if( !isCanceled() )
        copyTemplates( exedir );
      // generiere ascii-dateien
      setMessage( "generiere ASCII-Dateien (Modelldaten und Zeitreihen)" );
      if( !isCanceled() )
        generateASCII( exedir, inputdir, input );
      // starte berechnung
      setMessage( "starte Simulationskern" );
      if( !isCanceled() )
        startCalculation( exedir );
      // ergebnisse aufbereiten
      setMessage( "lade Ergebnisse" );
      loadResults( exedir, outputdir );

      System.out.println( "fertig" );
    }
    catch( Exception e )
    {
      throw new CalcJobServiceException( "Simulation konnte nicht durchgefuehrt werden", e );
    }
  }

  private void generateASCII( final File exedir, final File inputdir, CalcJobDataBean[] beans )
      throws Exception
  {
    final CalcJobDataBean modellBean = CalcJobHelper.getBeanForId( MODELL_ID, beans );
    final URL modellURL = new File( inputdir, modellBean.getPath() ).toURL();

    final CalcJobDataBean controlBean = CalcJobHelper.getBeanForId( CONTROL_ID, beans );
    final URL controlURL = new File( inputdir, controlBean.getPath() ).toURL();

    final NAConfiguration conf = NAConfiguration.getGml2AsciiConfiguration( modellURL, exedir );

    final GMLWorkspace controlWorkspace = GmlSerializer.createGMLWorkspace( controlURL, conf
        .getControlSchemaURL() );
    final GMLWorkspace modellWorkspace = GmlSerializer.createGMLWorkspace( modellURL, conf
        .getSchemaURL() );
    conf.setSimulationStart( (Date)controlWorkspace.getRootFeature()
        .getProperty( "startsimulation" ) );
    conf.setSimulationForecasetStart( (Date)controlWorkspace.getRootFeature().getProperty(
        "startforecast" ) );
    conf.setSimulationEnd( (Date)controlWorkspace.getRootFeature().getProperty( "endsimulation" ) );
    conf.setRootNodeID( (String)controlWorkspace.getRootFeature().getProperty( "rootNode" ) );
    NAControlConverter.featureToASCII( exedir, controlWorkspace, modellWorkspace );

    NAModellConverter.featureToAscii( conf, modellWorkspace );

    // create temperatur und verdunstung timeseries
    DummyTimeSeriesWriter writer = new DummyTimeSeriesWriter( conf.getSimulationStart(), conf
        .getSimulationEnd() );
    writer.writeTmpFile( new File( exedir, "klima.dat/std.tmp" ) );
    writer.writeVerdFile( new File( exedir, "klima.dat/std.ver" ) );
  }

  private void loadResults( final File baseDir, final File outdir )
  {
    outdir.mkdirs();

    // ASCII-Files:

    // zeitreihen im out Dir
    final File outDir = new File( baseDir, "out_we.nat" );
    final File[] outDirResults = outDir.listFiles();
    for( int i = 0; i < outDirResults.length; i++ )
    {
      final File file = outDirResults[i];

      copyResult( baseDir, file, outdir, FileUtilities.getSuffix( file ), file.getName() );
      //      addResult( new CalcJobDataBean( FileUtilities.getSuffix( file ),
      // file.getName(), file
      //          .getAbsolutePath() ) );
    }

    final File startDir = new File( baseDir, "start" );

    final File outputerr = new File( startDir, "output.err" );
    if( outputerr.exists() )
      copyResult( startDir, outputerr, outdir, "outputerr", "output.err" );
    final File outputres = new File( startDir, "output.res" );
    if( outputres.exists() )
      copyResult( startDir, outputres, outdir, "outputres", "output.res" );

    // TODO Andreas: diese beiden Dateien existieren nicht!
    // log und error dateien:
    final File logDir = new File( baseDir, "out_err" );
    final File logFile = new File( logDir, "" );
    if( logFile.exists() )
      // TODO: 'berich' ?
      addResult( new CalcJobDataBean( "LOG", "Berich Simulationskern", logFile.getAbsolutePath() ) );

    final File errFile = new File( logDir, "" );
    if( errFile.exists() )
      // TODO: 'berich' ?
      addResult( new CalcJobDataBean( "ERR", "Fehlerberich", errFile.getAbsolutePath() ) );
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
        catch( final Exception e )
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
      throw new CalcJobServiceException( "Fehler beim Ausführen", e );
    }
    catch( final InterruptedException e )
    {
      e.printStackTrace();
      throw new CalcJobServiceException( "Fehler beim Ausführen", e );
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
   * Kopiert eine Datei in den Ausgabeordner und fügt die entsprechende Bean zur
   * Ausgabe hinzu.
   * 
   * Die Pfade werden wie folgt angelegt:
   * 
   * Das Resultfile wird relativ zu resultdir aufgelöst und unter dem gleichen
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