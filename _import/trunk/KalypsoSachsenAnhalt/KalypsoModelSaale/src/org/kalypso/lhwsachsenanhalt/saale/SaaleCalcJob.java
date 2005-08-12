package org.kalypso.lhwsachsenanhalt.saale;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;
import java.util.logging.StreamHandler;

import javax.xml.bind.JAXBException;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.kalypso.commons.java.lang.ProcessHelper;
import org.kalypso.commons.java.lang.ProcessHelper.ProcessTimeoutException;
import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypso.contribs.java.net.UrlUtilities;
import org.kalypso.lhwsachsenanhalt.saale.batch.HWVORBatch;
import org.kalypso.ogc.gml.convert.GmlConvertException;
import org.kalypso.ogc.gml.convert.GmlConvertFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.services.calculation.job.ICalcDataProvider;
import org.kalypso.services.calculation.job.ICalcJob;
import org.kalypso.services.calculation.job.ICalcMonitor;
import org.kalypso.services.calculation.job.ICalcResultEater;
import org.kalypso.services.calculation.service.CalcJobServiceException;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * <p>
 * Der Rechenservice für das Bode-Modell
 * </p>
 * 
 * @author Thül
 */
public class SaaleCalcJob implements ICalcJob
{
  private final Logger m_logger = Logger.getLogger( getClass().getName() );

  private static final String HWDIR_DATEN = "Daten";

  private static final String HWDIR_STAMMDAT = "stammdat";

  private static final String HWDIR_WQ = "wq";

  private static final String HWDIR_AUSGABE = "ausgabe";

  private static final String HWDIR_ARCHIV = "archiv";

  private static final String HWEXE = "hwvor00.exe";

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#getSpezifikation()
   */
  public URL getSpezifikation()
  {
    return getClass().getResource( SaaleConst.CALCJOB_SPEC );
  }

  /**
   * @throws CalcJobServiceException
   * @see org.kalypso.services.calculation.job.ICalcJob#run(java.io.File,
   *      org.kalypso.services.calculation.job.ICalcDataProvider, org.kalypso.services.calculation.job.ICalcResultEater,
   *      org.kalypso.services.calculation.job.ICalcMonitor)
   */
  public void run( final File tmpdir, final ICalcDataProvider inputProvider, final ICalcResultEater resultEater,
      final ICalcMonitor monitor ) throws CalcJobServiceException

  {
    final File loggerFile = new File( tmpdir, "saale.log" );
    StreamHandler streamHandler = null;
    try
    {
      final FileOutputStream loggerStream = new FileOutputStream( loggerFile );
      final Logger packageLogger = Logger.getLogger( getClass().getPackage().getName() );
      streamHandler = new StreamHandler( loggerStream, new SimplisticFormatter() );
      // TODO: besser als info vom client holen? oder andersrum, das encoding weitergeben an den client?
      streamHandler.setEncoding( "UTF-8" );
      packageLogger.addHandler( streamHandler );

      // create input files
      if( monitor.isCanceled() )
        return;
      m_logger.info( "Erzeuge Eingabedateien für HWVOR00" );
      monitor.setMessage( "Erzeuge Eingabedateien für HWVOR00" );
      final SaaleInputBean inputData = createInputFiles( tmpdir, inputProvider );

      // calculate
      if( monitor.isCanceled() )
        return;
      runCalculation( inputData.getExeFile(), monitor );

      m_logger.info( "Berechnung wird durchgeführt" );

      // parse output
      if( monitor.isCanceled() )
        return;
      
      m_logger.info( "Ergebnisdaten werden zurückgelesen." );
      monitor.setMessage( "Ergebnisdaten werden zurückgelesen." );
      final File resultdir = writeResults( tmpdir, inputData.getDataDir() );
      resultEater.addResult( "ZML_RESULT", resultdir );
      resultEater.addResult( "HWVOR00", inputData.getHwvorDir() );
    }
    catch( final FileNotFoundException e )
    {
      // should never happen
      e.printStackTrace();
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      throw new CalcJobServiceException( "Fehler bei der Berechnung", e );
    }
    finally
    {
      if( streamHandler != null )
        streamHandler.close();
    }

    resultEater.addResult( "LOG", loggerFile );
  }

  private void runCalculation( final File exeFile, final ICalcMonitor monitor ) throws IOException, ProcessTimeoutException
  {
    final StringWriter logStream = new StringWriter();
    final StringWriter errStream = new StringWriter();
    
    try
    {
      final String cmdLine = exeFile.getAbsolutePath();
      ProcessHelper.startProcess( cmdLine, null, exeFile.getParentFile(), monitor, 5 * 60 * 1000, logStream, errStream );

      System.out.println( logStream.toString() );
      System.out.println( logStream.toString() );
    }
    finally
    {
      logStream.close();
      errStream.close();
    }
    
  }

  private SaaleInputBean createInputFiles( final File tmpdir, final ICalcDataProvider inputProvider )
      throws CalcJobServiceException, Exception
  {
    // VERZEICHNISSE erstellen //
    final File hwvordir = new File( tmpdir, "HWVOR00" );
    hwvordir.mkdir();

    // braucht mans?
    createHwzugrStv( hwvordir );

    final File datendir = new File( hwvordir, HWDIR_DATEN );
    datendir.mkdir();
    final File ausgabedir = new File( hwvordir, HWDIR_AUSGABE );
    ausgabedir.mkdir();
    final File stammdatdir = new File( hwvordir, HWDIR_STAMMDAT );
    stammdatdir.mkdir();

    // gml nur einmal lesen
    final URL gmlContext = inputProvider.getURLForID( "MODELL" );
    final GMLWorkspace modellWorkspace = GmlSerializer.createGMLWorkspace( gmlContext );
    writeStammdaten( stammdatdir, modellWorkspace );
    writeDaten( datendir, modellWorkspace, gmlContext );
    
    // exe ins datenverzeichnis kopieren
    final File exeFile = copyExeToDir( hwvordir );
    
    m_logger.info( "Eingangsdaten für HWVOR00.exe wurden erzeugt." );
    m_logger.info( "" );

    return new SaaleInputBean( hwvordir, datendir, exeFile );
  }

  private File copyExeToDir( final File hwvordir ) throws IOException
  {
    final File exeFile = new File( hwvordir, HWEXE );
    FileUtils.copyURLToFile( getClass().getResource( "resources/HWVOR00.exe" ), exeFile );
    return exeFile;
  }

  private void createHwzugrStv( final File hwvordir ) throws IOException
  {
    final File hwzugrstv = new File( hwvordir, "Hwzugr.stv" );
    
    final String hwvorPath = hwvordir.getAbsolutePath();
    
    PrintWriter writer = null;
    try
    {
      writer = new PrintWriter( new BufferedWriter( new FileWriter( hwzugrstv ) ) );
      writer.println( hwvorPath + "\\" + HWDIR_DATEN +"\\" );
      writer.println( hwvorPath + "\\" + HWDIR_STAMMDAT +"\\" );
      writer.println( hwvorPath + "\\" + HWDIR_WQ +"\\" );
      writer.println( hwvorPath + "\\" + HWDIR_AUSGABE +"\\" );
      writer.println( hwvorPath + "\\" + HWDIR_ARCHIV +"\\" );
      writer.println( "SA" );
      writer.println( "versteckt" );
      writer.close();
    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }
  }

  private void writeStammdaten( final File stammdatdir, final GMLWorkspace modellWorkspace ) throws IOException,
      JAXBException, GmlConvertException
  {
    final Map externData = new HashMap( 1 );
    externData.put( SaaleConst.REGISTERED_ID, modellWorkspace );

    final URL stammdatURL = stammdatdir.toURL();

    convertGml( externData, stammdatURL, "Stammdat/pegel.std", "1_pegel_std.gmc" );
    convertGml( externData, stammdatURL, "Stammdat/met_geb.std", "2_met_geb_std.gmc" );
    convertGml( externData, stammdatURL, "Stammdat/egmpar.hwp", "3_egmpar_hwp.gmc" );
    convertGml( externData, stammdatURL, "Stammdat/tl.std", "4_tl_std.gmc" );
    convertGml( externData, stammdatURL, "Stammdat/ts.std", "5_ts_std.gmc" );
    convertGml( externData, stammdatURL, "Stammdat/wlmpar.hwp", "6_wlmpar_hwp.gmc" );
    convertGml( externData, stammdatURL, "Stammdat/hwsteu.stv", "7_hwsteu_stv.gmc" );
  }

  private void convertGml( final Map externData, final URL stammdatURL, final String filenameForLog, final String gmcFile ) throws IOException, JAXBException, GmlConvertException
  {
    m_logger.info( "Schreibe: " + filenameForLog );
    GmlConvertFactory.convertXml( getClass().getResource( "resources/gml2hwvor/" + gmcFile ), new UrlUtilities(),
        stammdatURL, externData );
  }

  private void writeDaten( final File datendir, final GMLWorkspace modellWorkspace, final URL context )
      throws IOException, JAXBException, GmlConvertException
  {
    final IUrlResolver resolver = new UrlUtilities();

    final URL datenURL = datendir.toURL();
    final Map externData = new HashMap( 1 );
    externData.put( SaaleConst.REGISTERED_ID, modellWorkspace );
    convertGml( externData, datenURL, "Daten/hwablauf.vor", "8_hwablauf_vor.gmc" );
    
    writeObservations( modellWorkspace, "Durchfluß", "PegelCollectionAssociation/PegelMember", new File( datendir,
        "Q_dat.vor" ), TimeserieConstants.TYPE_RUNOFF, resolver, context );
    writeObservations( modellWorkspace, "Niederschlag", "PegelCollectionAssociation/PegelMember[Gebiet]", new File(
        datendir, "P_dat.vor" ), TimeserieConstants.TYPE_RAINFALL, resolver, context );
    writeObservations( modellWorkspace, "Schnee", "PegelCollectionAssociation/PegelMember[Gebiet]", new File( datendir,
        "SNDAT.vor" ), TimeserieConstants.TYPE_RAINFALL, resolver, context );
    writeObservations( modellWorkspace, "Temperatur", "TempCollectionAssociation/TempMember", new File( datendir,
        "TLDAT.vor" ), TimeserieConstants.TYPE_TEMPERATURE, resolver, context );
    writeObservations( modellWorkspace, "Inhalt", "SpeicherCollectionAssociation/SpeicherMember", new File( datendir,
        "Tsdat.vor" ), TimeserieConstants.TYPE_VOLUME, resolver, context );
  }

  private void writeObservations( final GMLWorkspace workspace, final String linkProperty, final String featurePath,
      final File file, final String axisType, final IUrlResolver resolver, final URL context ) throws IOException
  {
    m_logger.info( "Schreibe Zeitreihen: " + linkProperty );
    final HWVorZMLWriterVisitor visitor = new HWVorZMLWriterVisitor( linkProperty, axisType, resolver, context );
    workspace.accept( visitor, featurePath, FeatureVisitor.DEPTH_ZERO );
    visitor.writeObservations( file );
  }

  private File writeResults( final File tmpdir, final File datendir ) throws CalcJobServiceException
  {
    final File resultdir = new File( tmpdir, "out" );
    try
    {
      new HWVORBatch().convert( new String[]
      {
          datendir.getAbsolutePath(),
          resultdir.getAbsolutePath() } );
      m_logger.info( "Ergebnisdaten wurden konvertiert." );
      m_logger.info( "" );
    }
    catch( Throwable e )
    {
      e.printStackTrace();

      throw new CalcJobServiceException( "Fehler beim Konvertieren der Ergebnisdaten", e );
    }
    return resultdir;
  }

}