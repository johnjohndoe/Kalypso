package org.kalypso.convert.namodel;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.deegree.model.feature.GMLWorkspace;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.java.io.ReaderUtilities;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
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

  public void run( File basedir, CalcJobDataBean[] input ) throws CalcJobServiceException
  {
    if( !basedir.exists() )
      basedir.mkdirs();
    try
    {
      setMessage( "richte Berechnungsverzeichnis ein" );
      if( !isCanceled() )
        prepareBaseDir( basedir );
      // kopiere template aus resourcen:
      if( !isCanceled() )
        copyTemplates( basedir );
      // generiere ascii-dateien
      setMessage( "generiere ASCII-Dateien (Modelldaten und Zeitreihen)" );
      if( !isCanceled() )
        generateASCII( basedir, input );
      // starte berechnung
      setMessage( "starte Simulationskern" );
      if( !isCanceled() )
        startCalculation( basedir );
      // ergebnisse aufbereiten
      setMessage( "lade Ergebnisse" );
      loadResults( basedir );

      System.out.println( "fertig" );
    }
    catch( Exception e )
    {
      throw new CalcJobServiceException( "Simulation konnte nicht durchgefuehrt werden", e );
    }
  }

  private void generateASCII( File basedir, CalcJobDataBean[] beans ) throws Exception
  {

    final CalcJobDataBean modellBean = CalcJobHelper.getBeanForId( MODELL_ID, beans );
    final URL modellURL = new File( modellBean.getPath() ).toURL();

    final CalcJobDataBean controlBean = CalcJobHelper.getBeanForId( CONTROL_ID, beans );
    final URL controlURL = new File( controlBean.getPath() ).toURL();

    final NAConfiguration conf = NAConfiguration.getGml2AsciiConfiguration( modellURL, basedir );

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
    NAControlConverter.featureToASCII( basedir, controlWorkspace, modellWorkspace );

    NAModellConverter.featureToAscii( conf, modellWorkspace );

    // create temperatur und verdunstung timeseries
    DummyTimeSeriesWriter writer = new DummyTimeSeriesWriter( conf.getSimulationStart(), conf
        .getSimulationEnd() );
    writer.writeTmpFile( new File( basedir, "klima.dat/std.tmp" ) );
    writer.writeVerdFile( new File( basedir, "klima.dat/std.ver" ) );
  }

  private void loadResults( File baseDir )
  {
    // ASCII-Files:
    
    // zeitreihen im out Dir
    final File outDir = new File( baseDir, "out_we.nat" );
    final File[] outDirResults = outDir.listFiles();
    for( int i = 0; i < outDirResults.length; i++ )
    {
      final File file = outDirResults[i];
      addResult( new CalcJobDataBean( FileUtilities.getSuffix( file ), file.getName(), file
          .getAbsolutePath() ) );
    }

    // log und error dateien:
    final File logDir = new File( baseDir, "out_err" );
    final File logFile=new File(logDir,"");
    if(logFile.exists())
      addResult( new CalcJobDataBean( "LOG", "Berich Simulationskern", logFile.getAbsolutePath()));
      
    final File errFile=new File(logDir,"");
    if(errFile.exists())
      addResult( new CalcJobDataBean( "ERR", "Fehlerberich", errFile.getAbsolutePath()));
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

    try
    {
      final File exeFile = new File( basedir, EXE_FILE );
      final File exeDir = exeFile.getParentFile();
      final String commandString = exeFile.getAbsolutePath();

      final Process process = Runtime.getRuntime().exec( commandString, null, exeDir );

      inStream = new InputStreamReader( process.getInputStream() );
      errStream = new InputStreamReader( process.getErrorStream() );
      while( true )
      {
        ReaderUtilities.dumpAllAvailable( inStream );
        ReaderUtilities.dumpAllAvailable( errStream );

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

}