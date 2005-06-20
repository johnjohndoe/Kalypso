package org.kalypso.lhwsachsenanhalt.saale;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;
import java.util.logging.StreamHandler;

import javax.xml.bind.JAXBException;

import org.kalypso.java.net.IUrlResolver;
import org.kalypso.java.net.UrlUtilities;
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
      final File datendir = createInputFiles( tmpdir, inputProvider );

      // calculate
      if( monitor.isCanceled() )
        return;

      m_logger.info( "Berechnung wird durchgeführt" );

      // parse output
      if( monitor.isCanceled() )
        return;
      m_logger.info( "Ergebnisdaten werden zurückgelesen." );
      monitor.setMessage( "Ergebnisdaten werden zurückgelesen." );
      final File resultdir = writeResults( tmpdir, datendir );
      resultEater.addResult( "ZML_RESULT", resultdir );
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

  private File createInputFiles( final File tmpdir, final ICalcDataProvider inputProvider )
      throws CalcJobServiceException, Exception
  {
    // VERZEICHNISSE erstellen //
    final File hwvordir = new File( tmpdir, "HWVOR00" );
    hwvordir.mkdir();

    // braucht mans?
    //    new File hwzugrstv = new File( hwvordir, "Hwzugr.stv" );

    //    new File( hwvordir, "ARCHIV" ).mkdir();
    new File( hwvordir, "AUSGABE" ).mkdir();
    final File datendir = new File( hwvordir, "Daten" );
    datendir.mkdir();
    //    new File( hwvordir, "TLUG" ).mkdir();
    //    new File( hwvordir, "WQ" ).mkdir();

    // gml nur einmal lesen
    final URL gmlContext = inputProvider.getURLForID( "MODELL" );
    final GMLWorkspace modellWorkspace = GmlSerializer.createGMLWorkspace( gmlContext );
    writeStammdaten( hwvordir, modellWorkspace );
    writeDaten( datendir, modellWorkspace, gmlContext );
    m_logger.info( "Eingangsdaten für HWVOR00.exe wurden erzeugt." );
    m_logger.info( "" );

    return datendir;
  }

  private void writeStammdaten( final File hwvordir, final GMLWorkspace modellWorkspace ) throws IOException,
      JAXBException, GmlConvertException
  {
    final File stammdatdir = new File( hwvordir, "Stammdat" );
    stammdatdir.mkdir();

    final Map externData = new HashMap( 1 );
    externData.put( SaaleConst.REGISTERED_ID, modellWorkspace );

    final URL stammdatURL = stammdatdir.toURL();

    m_logger.info( "Schreibe: Stammdat/pegel.std" );
    GmlConvertFactory.convertXml( getClass().getResource( "resources/gml2hwvor/1_pegel_std.gmc" ), new UrlUtilities(),
        stammdatURL, externData );

    m_logger.info( "Schreibe: Stammdat/met_geb.std" );
    GmlConvertFactory.convertXml( getClass().getResource( "resources/gml2hwvor/2_met_geb_std.gmc" ),
        new UrlUtilities(), stammdatURL, externData );

    m_logger.info( "Schreibe: Stammdat/egmpar.hwp" );
    GmlConvertFactory.convertXml( getClass().getResource( "resources/gml2hwvor/3_egmpar_hwp.gmc" ), new UrlUtilities(),
        stammdatURL, externData );

    m_logger.info( "Schreibe: Stammdat/tl.std" );
    GmlConvertFactory.convertXml( getClass().getResource( "resources/gml2hwvor/4_tl_std.gmc" ), new UrlUtilities(),
        stammdatURL, externData );

    m_logger.info( "Schreibe: Stammdat/ts.std" );
    GmlConvertFactory.convertXml( getClass().getResource( "resources/gml2hwvor/5_ts_std.gmc" ), new UrlUtilities(),
        stammdatURL, externData );

    m_logger.info( "Schreibe: Stammdat/wlmpar.hwp" );
    GmlConvertFactory.convertXml( getClass().getResource( "resources/gml2hwvor/6_wlmpar_hwp.gmc" ), new UrlUtilities(),
        stammdatURL, externData );

    m_logger.info( "Schreibe: Stammdat/hwsteu.stv" );
    GmlConvertFactory.convertXml( getClass().getResource( "resources/gml2hwvor/7_hwsteu_stv.gmc" ), new UrlUtilities(),
        stammdatURL, externData );
  }

  private void writeDaten( final File datendir, final GMLWorkspace modellWorkspace, final URL context )
      throws IOException
  {
    final IUrlResolver resolver = new UrlUtilities();

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