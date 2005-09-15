package org.kalypso.lhwsachsenanhalt.saale;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URL;
import java.text.ParseException;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;
import java.util.logging.StreamHandler;

import javax.xml.bind.JAXBException;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.factory.FactoryException;
import org.kalypso.commons.java.lang.ProcessHelper;
import org.kalypso.commons.java.lang.ProcessHelper.ProcessTimeoutException;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.java.io.CharsetUtilities;
import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypso.contribs.java.net.UrlUtilities;
import org.kalypso.lhwsachsenanhalt.saale.batch.HWVORBatch;
import org.kalypso.ogc.gml.convert.GmlConvertException;
import org.kalypso.ogc.gml.convert.GmlConvertFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.wq.WQException;
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
  public static final String HWDIR_DATEN = "Daten";

  public static final String HWDIR_STAMMDAT = "stammdat";

  public static final String HWDIR_WQ = "wq";

  public static final String HWDIR_AUSGABE = "ausgabe";

  public static final String HWDIR_ARCHIV = "archiv";

  public static final String HWEXE = "hwvor00.exe";

  private final Logger m_logger = Logger.getLogger( getClass().getName() );

  private Map m_metadataMap = new HashMap();

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
    resultEater.addResult( "LOG", loggerFile );
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

      if( monitor.isCanceled() )
        return;

      final File hwqvorFile = new File( inputData.getHwvorDir(), HWDIR_AUSGABE + "/HWQVOR.TXT" );
      resultEater.addResult( "HWVOR00_LOG", hwqvorFile );

      checkHWVORLog( hwqvorFile );

      m_logger.info( "Berechnung abgeschlossen" );

      // parse output
      if( monitor.isCanceled() )
        return;

      m_logger.info( "Ergebnisdaten werden zurückgelesen." );
      monitor.setMessage( "Ergebnisdaten werden zurückgelesen." );
      final File resultdir = readResults( tmpdir, inputData.getDataDir() );
      resultEater.addResult( "ZML_RESULT", resultdir );

      // if you want an info message at the end of each calc job uncomment the next line
      // monitor.setFinishInfo( IStatus.INFO, "Berechnung erfolgreich abgeschlossen" );
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      final IStatus status = StatusUtilities.createStatus( IStatus.ERROR, "Fehler bei der Berechnung", e );
      monitor.setFinishInfo( status.getSeverity(), StatusUtilities.messageFromStatus( status ) );
    }
    finally
    {
      if( streamHandler != null )
        streamHandler.close();
    }
  }

  private void checkHWVORLog( final File hwqvorFile ) throws IOException
  {
    final String defaultCharset = CharsetUtilities.getDefaultCharset();
    final String string = FileUtils.readFileToString( hwqvorFile, defaultCharset );
    if( string.trim().endsWith( "ENDE QVOR" ) )
      return;

    throw new CalcJobServiceException(
        "Berechnung wurde nicht erfolgreich abgeschlossen.\nBitte sehen Sie die Log-Datei unter Ergebnisse/HWQVOR.TXT ein.",
        null );
  }

  private void runCalculation( final File exeFile, final ICalcMonitor monitor ) throws IOException,
      ProcessTimeoutException
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
    final File datendir = new File( hwvordir, HWDIR_DATEN );
    datendir.mkdir();
    final File ausgabedir = new File( hwvordir, HWDIR_AUSGABE );
    ausgabedir.mkdir();
    final File stammdatdir = new File( hwvordir, HWDIR_STAMMDAT );
    stammdatdir.mkdir();

    // gml nur einmal lesen
    final URL gmlContext = inputProvider.getURLForID( "MODELL" );
    final GMLWorkspace modellWorkspace = GmlSerializer.createGMLWorkspace( gmlContext );

    // braucht mans?
    writeHwzugrStv( hwvordir );
    writeStammdaten( stammdatdir, modellWorkspace );
    writeDaten( datendir, modellWorkspace, gmlContext );
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

  private void writeHwzugrStv( final File hwvordir ) throws IOException
  {
    final File hwzugrstv = new File( hwvordir, "Hwzugr.stv" );

    final String hwvorPath = hwvordir.getAbsolutePath();

    PrintWriter writer = null;
    try
    {
      writer = new PrintWriter( new BufferedWriter( new FileWriter( hwzugrstv ) ) );
      writer.println( hwvorPath + "\\" + HWDIR_DATEN + "\\" );
      writer.println( hwvorPath + "\\" + HWDIR_STAMMDAT + "\\" );
      //      writer.println( hwvorPath + "\\" + HWDIR_WQ +"\\" );
      writer.println(); // empty path, so no popups regarding missing WQ-files
      writer.println( hwvorPath + "\\" + HWDIR_AUSGABE + "\\" );
      writer.println( hwvorPath + "\\" + HWDIR_ARCHIV + "\\" );
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

    convertGml( externData, stammdatURL, "Stammdat/pegel.std", "pegel_std.gmc" );
    convertGml( externData, stammdatURL, "Stammdat/egmpar.hwp", "egmpar_hwp.gmc" );
    convertGml( externData, stammdatURL, "Stammdat/tssteu.hwp", "tssteu_hwp.gmc" );
    convertGml( externData, stammdatURL, "Stammdat/ts.std", "ts_std.gmc" );
    convertGml( externData, stammdatURL, "Stammdat/wlmpar.hwp", "wlmpar_hwp.gmc" );
    //convertGml( externData, stammdatURL, "Stammdat/hwsteu.stv", "hwsteu_stv.gmc" );

    // hwsteu.stv is no more in the model, just copy it out of the resources
    final URL resource = getClass().getResource( "resources/HW_STEU.STV" );
    FileUtils.copyURLToFile( resource, new File( stammdatdir, "HW_STEU.STV" ) );
  }

  private void convertGml( final Map externData, final URL stammdatURL, final String filenameForLog,
      final String gmcFile ) throws IOException, JAXBException, GmlConvertException
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
    convertGml( externData, datenURL, "Daten/hwablauf.vor", "hwablauf_vor.gmc" );

    featurezml2vor( modellWorkspace, "Durchfluss", "PegelCollectionAssociation/PegelMember", new File( datendir,
        "Q_dat.vor" ), TimeserieConstants.TYPE_RUNOFF, resolver, context );
    featurezml2vor( modellWorkspace, "Niederschlag", "PegelCollectionAssociation/PegelMember[Gebiet]", new File(
        datendir, "P_dat.vor" ), TimeserieConstants.TYPE_RAINFALL, resolver, context );
    featurezml2vor( modellWorkspace, "Schnee", "PegelCollectionAssociation/PegelMember[Gebiet]", new File( datendir,
        "SNDAT.vor" ), TimeserieConstants.TYPE_RAINFALL, resolver, context );
    featurezml2vor( modellWorkspace, "Inhalt", "SpeicherCollectionAssociation/SpeicherMember", new File( datendir,
        "Tsdat.vor" ), TimeserieConstants.TYPE_VOLUME, resolver, context );
  }

  private void featurezml2vor( final GMLWorkspace workspace, final String linkProperty, final String featurePath,
      final File file, final String axisType, final IUrlResolver resolver, final URL context ) throws IOException
  {
    m_logger.info( "Schreibe Zeitreihen: " + linkProperty );
    final HWVorZMLWriterVisitor visitor = new HWVorZMLWriterVisitor( linkProperty, axisType, resolver, context );
    workspace.accept( visitor, featurePath, FeatureVisitor.DEPTH_ZERO );
    m_metadataMap.putAll( visitor.getMetadataMap() );
    final boolean bSuccess = visitor.writeObservations( file );
    if( bSuccess )
      m_logger.info( linkProperty + " geschrieben" );
    else
      m_logger.info( "Keine Zeitreihen vorhanden: " + linkProperty );
  }

  private File readResults( final File tmpdir, final File datendir ) throws CalcJobServiceException
  {
    final File resultdir = new File( tmpdir, "out" );
    try
    {
      convertVor2Zml( new File( datendir, "Q_dat.vor" ), new File( resultdir, "Durchfluss" ),
          TimeserieConstants.TYPE_RUNOFF );
      convertVor2Zml( new File( datendir, "Tsdat.vor" ), new File( resultdir, "Speicherinhalt" ),
          TimeserieConstants.TYPE_RUNOFF );

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

  private void convertVor2Zml( final File vorFile, final File outDir, final String type ) throws ParseException,
      WQException, SensorException, JAXBException, FactoryException, IOException
  {
    final HWVORBatch converter = new HWVORBatch();

    final IObservation[] obs = converter.readObservations( vorFile, type, null );

    // change metadata: add FILE_NAME and copy all metadata from corresponding input zml
    for( int i = 0; i < obs.length; i++ )
    {
      final IObservation observation = obs[i];
      final MetadataList obsMeta = observation.getMetadataList();

      final String identifier = observation.getName();
      final MetadataList oldMeta = (MetadataList)m_metadataMap.get( type + "#" + identifier );
      if( oldMeta != null )
        obsMeta.putAll( oldMeta );

      obsMeta.put( "Datei", vorFile.getAbsolutePath() );
    }

    converter.obs2zml( null, outDir, type, obs );
  }

}