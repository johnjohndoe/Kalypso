package org.kalypso.lhwzsachsen.spree;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.net.URL;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Properties;
import java.util.logging.Logger;

import org.kalypso.java.io.FileUtilities;
import org.kalypso.java.io.StreamUtilities;
import org.kalypso.java.net.IUrlResolver;
import org.kalypso.java.net.UrlUtilities;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.wq.WQObservationFilter;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.services.calculation.job.ICalcDataProvider;
import org.kalypso.services.calculation.service.CalcJobClientBean;
import org.kalypso.services.calculation.service.CalcJobServiceException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.io.shpapi.DBaseFile;
import org.kalypsodeegree_impl.io.shpapi.FieldDescriptor;

/**
 * Diese Klasse sammelt alles, was mit dem Erzeugen der Nativen Daten aus den
 * Eingabedaten zu tun hat.
 * 
 * @author Belger
 */
public class SpreeInputWorker
{
  protected final static Logger LOGGER = Logger.getLogger( SpreeInputWorker.class
      .getName() );

  private SpreeInputWorker( )
  {
    // wird nicht instantiiert
  }

  /**
   * Erzeugt aus den InputBeans eine HashMap der Form [id |-> bean]
   * 
   * @param input
   * @return map
   */
  public static Map hashInput( final CalcJobClientBean[] input )
  {
    final Map map = new HashMap( input.length );
    for( int i = 0; i < input.length; i++ )
    {
      final CalcJobClientBean bean = input[i];
      map.put( bean.getId(), bean );
    }

    return map;
  }

  /**
   * <p>
   * Converts inputfiles to nativefiles and reads control parameters
   * </p>
   * 
   * @return Location of native files
   * @throws IOException
   */
  public static File createNativeInput( final File tmpdir,
      final ICalcDataProvider inputProvider, final Properties props,
      final PrintWriter logwriter, final TSMap tsmap ) throws Exception
  {
    try
    {
      final File nativedir = new File( tmpdir, "native" );
      nativedir.mkdirs();


      final URL controlGmlURL = inputProvider.getURLForID( "CONTROL_GML" );
      final URL controlSchemaURL = inputProvider.getURLForID( "CONTROL_XSD" );
      
      logwriter.println( "Lese Steuerparameter: " + controlGmlURL.toString() );

      final Map map = parseControlFile( controlGmlURL, controlSchemaURL, nativedir );
      props.putAll( map );

      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( inputProvider.getURLForID( "GML" ), inputProvider.getURLForID( "MODELL_XSD" ) );
      
      props.put( SpreeCalcJob.DATA_GML,workspace );

      final String tsFilename = writeNonTs( props, logwriter, workspace );

      final Date startDate = (Date) props.get( SpreeCalcJob.DATA_STARTSIM_DATE );
      setAnfangsstauvolumen( "V_TSQUITZ", startDate, "TS_QUITZDORF", tsmap,
          workspace, logwriter );
      setAnfangsstauvolumen( "V_TSBAUTZ", startDate, "TS_BAUTZEN", tsmap,
          workspace, logwriter );

      logwriter.println( "Erzeuge Zeitreihen-Datei: " + tsFilename );
      readZML( inputProvider, tsmap );
      calcNiederschlagsummen( tsmap );
      createTimeseriesFile( tsFilename, tsmap );

      return nativedir;
    }
    catch( final CalcJobServiceException e )
    {
      e.printStackTrace();
      throw new CalcJobServiceException(
          "Fehler beim Erzeugen der Inputdateien", e );
    }
  }

  private static void calcNiederschlagsummen( final TSMap tsmap )
  {
    final Date[] dates = tsmap.getDates();
    
    for( int i = 0; i < SpreeCalcJob.TS_DESCRIPTOR.length; i++ )
    {
      final String id = SpreeCalcJob.TS_DESCRIPTOR[i].id;
      if( id.startsWith( "PA_" ) )
      {
        final String pgid = "PG" + id.substring( 2 );
        final String ppid = "PP" + id.substring( 2 );
        
        final Map datesToValuesMap = tsmap.getTimeserie( id );
        
        Double lastValue = null;
        for( int j = 0; j < dates.length; j++ )
        {
          final Date date = dates[j];
          
          // die beiden ersten dürfen nicht gesetzt werden, sonst rechnet das Modell Mist
          final Double value = ( j == 0 || j == 1 ) ? null : (Double)datesToValuesMap.get( date );

          if( j % 2 == 0 )
            tsmap.putValue( pgid, date, null );
          else if( value != null && lastValue != null && j % 2 == 1 )
          {
            final double summe = value.doubleValue() + lastValue.doubleValue();
            tsmap.putValue( pgid, date, new Double( summe ) );
          }
          else
            tsmap.putValue( pgid, date, new Double( -99.9 ) );
          
          if( value == null )
            tsmap.putValue( id, date, new Double( -99.9 ) );
          
          tsmap.putValue( ppid, date, new Double( 50.0 ) );
          
          lastValue = value;
        }
      }
    }
  }

  private static void setAnfangsstauvolumen( final String name,
      final Date startDate, final String fid, final TSMap tsmap,
      final GMLWorkspace workspace, final PrintWriter logwriter )
  {
    // das Anfangsstauvolumen aus der GMl raussuchen und als Wert in den
    // Zeitreihen setzen
    final Feature feature = workspace.getFeature( fid );
    final Object property = feature.getProperty( "Anfangsstauvolumen" );
    if( property != null && property instanceof Double )
      tsmap.putValue( name, startDate, (Double) property );
    else
      logwriter.println( "Kein Anfangsstauvolumen angegeben für: " + fid );
  }

  private static String writeNonTs( final Properties props,
      final PrintWriter logwriter, final GMLWorkspace workspace )
      throws IOException, FileNotFoundException, CalcJobServiceException
  {
    final File vhsFile = (File) props.get( SpreeCalcJob.DATA_VHSFILE );
    final String flpFilename = (String) props
        .get( SpreeCalcJob.DATA_FLPFILENAME );
    final String napFilename = (String) props
        .get( SpreeCalcJob.DATA_NAPFILENAME );
    final String tsFilename = (String) props.get( SpreeCalcJob.DATA_TSFILENAME );

    logwriter.println( "Erzeuge _vhs Datei: " + vhsFile.getName() );
    StreamUtilities.streamCopy( SpreeInputWorker.class
        .getResourceAsStream( "resources/" + SpreeCalcJob.VHS_FILE ),
        new FileOutputStream( vhsFile ) );

    logwriter.println( "Erzeuge _flp Datei: " + flpFilename );
    findAndWriteLayer( workspace, SpreeCalcJob.FLP_NAME, SpreeCalcJob.FLP_MAP,
        SpreeCalcJob.FLP_GEOM, flpFilename );

    logwriter.println( "Erzeuge _nap Datei: " + napFilename );
    findAndWriteLayer( workspace, SpreeCalcJob.NAP_NAME, SpreeCalcJob.NAP_MAP,
        SpreeCalcJob.NAP_GEOM, napFilename );

    final File shpfile = new File( tsFilename + ".shp" );
    final InputStream shpresource = SpreeInputWorker.class
        .getResourceAsStream( "resources/HW.shp" );
    FileUtilities.makeFileFromStream( false, shpfile, shpresource );

    final File shxfile = new File( tsFilename + ".shx" );
    final InputStream shxresource = SpreeInputWorker.class
        .getResourceAsStream( "resources/HW.shx" );
    FileUtilities.makeFileFromStream( false, shxfile, shxresource );

    return tsFilename;
  }

  public static void createTimeseriesFile( final String tsFilename,
      final TSMap valuesMap ) throws CalcJobServiceException
  {
    try
    {
      final FieldDescriptor[] fds = new FieldDescriptor[SpreeCalcJob.TS_DESCRIPTOR.length + 5];
      fds[0] = new FieldDescriptor( "DZAHL", "N", (byte) 6, (byte) 2 );
      fds[1] = new FieldDescriptor( "STUNDE", "N", (byte) 2, (byte) 0 );
      fds[2] = new FieldDescriptor( "DATUM", "C", (byte) 10, (byte) 0 );
      fds[3] = new FieldDescriptor( "VON", "N", (byte) 2, (byte) 0 );
      fds[4] = new FieldDescriptor( "AB", "N", (byte) 2, (byte) 0 );

      for( int j = 0; j < SpreeCalcJob.TS_DESCRIPTOR.length; j++ )
      {
        final TSDesc desc = SpreeCalcJob.TS_DESCRIPTOR[j];
        final String name = desc.id;

        final int i = j + 5;

        if( name.startsWith( "S_" ) )
          fds[i] = new FieldDescriptor( name, "C", (byte) 5, (byte) 0 );
        else if( name.startsWith( "W_" ) )
          fds[i] = new FieldDescriptor( name, "N", (byte) 5, (byte) 0 );
        else if( name.startsWith( "Q_" ) )
          fds[i] = new FieldDescriptor( name, "N", (byte) 7, (byte) 2 );
        else if( name.startsWith( "QX_" ) )
          fds[i] = new FieldDescriptor( name, "N", (byte) 7, (byte) 2 );
        else if( name.startsWith( "WV_" ) )
          fds[i] = new FieldDescriptor( name, "N", (byte) 5, (byte) 0 );
        else if( name.startsWith( "QV_" ) )
          fds[i] = new FieldDescriptor( name, "N", (byte) 7, (byte) 2 );
        else if( name.startsWith( "QP_" ) )
          fds[i] = new FieldDescriptor( name, "N", (byte) 8, (byte) 3 );
        else if( name.startsWith( "PG_" ) )
          fds[i] = new FieldDescriptor( name, "N", (byte) 6, (byte) 1 );
        else if( name.startsWith( "PP_" ) )
          fds[i] = new FieldDescriptor( name, "N", (byte) 3, (byte) 0 );
        else if( name.startsWith( "PA_" ) )
          fds[i] = new FieldDescriptor( name, "N", (byte) 7, (byte) 2 );
        else if( name.startsWith( "V_" ) )
          fds[i] = new FieldDescriptor( name, "N", (byte) 7, (byte) 2 );
        else if( name.startsWith( "ZG_" ) )
          fds[i] = new FieldDescriptor( name, "N", (byte) 7, (byte) 2 );
      }

      final DBaseFile dbf = new DBaseFile( tsFilename, fds );

      // Werte schreiben
      final DateFormat specialDateFormat = new SimpleDateFormat( "yMM.dd" );
      final DateFormat dateFormat = new SimpleDateFormat( "dd.MM.yyyy" );
      final Calendar calendar = Calendar.getInstance();

      final Date[] dateArray = valuesMap.getDates();

      final int datelength = ( dateArray.length % 2 == 0 ) ? dateArray.length : dateArray.length - 1;
      
      for( int i = 0; i < datelength; i++ )
      {
        final Date date = dateArray[i];

        final ArrayList record = new ArrayList();

        calendar.setTime( date );
        record.add( new Double( specialDateFormat.format( date ) ) );
        record.add( new Integer( calendar.get( Calendar.HOUR_OF_DAY ) ) );
        record.add( dateFormat.format( date ) );
        calendar.add( Calendar.HOUR_OF_DAY, -3 );
        record.add( new Integer( calendar.get( Calendar.HOUR_OF_DAY ) ) );
        calendar.add( Calendar.HOUR_OF_DAY, -3 );
        record.add( new Integer( calendar.get( Calendar.HOUR_OF_DAY ) ) );

        for( int j = 0; j < SpreeCalcJob.TS_DESCRIPTOR.length; j++ )
        {
          final String id = SpreeCalcJob.TS_DESCRIPTOR[j].id;
          final Map datesToValuesMap = valuesMap.getTimeserie( id );

          Double outVal = null;

          if( datesToValuesMap != null )
            outVal = ((Double) datesToValuesMap.get( date ));
          
          // die Erste Zeile darf keine Talsperrenabgabe enthalten
          if( id.startsWith( "QV_TS" ) && i == 0 )
            outVal = null;

          record.add( outVal );
        }

        dbf.setRecord( record );
      }

      dbf.writeAllToFile();
      dbf.close();
    }
    catch( final Exception e1 )
    {
      e1.printStackTrace();

      throw new CalcJobServiceException( "Fehler beim Scheiben der Zeitreihen",
          e1 );
    }
  }

  /**
   * Liest die Zeitreihen und erzeugt daraus eine Tabelle (Map)
   *  
   * @throws IOException
   */
  public static TSMap readZML( final ICalcDataProvider inputProvider,
      final TSMap tsmap ) throws IOException
  {
    final URL zmlURL = inputProvider.getURLForID( "ZML" );
    final String zmlURLstr = zmlURL.toExternalForm();
    final URL zmlURLDir = new URL( zmlURLstr + "/" );
    
    final IUrlResolver urlUtilities = new UrlUtilities();
    
    // alle Zeitreihen lesen
    for( int i = 0; i < SpreeCalcJob.TS_DESCRIPTOR.length; i++ )
    {
      final TSDesc tsDesc = SpreeCalcJob.TS_DESCRIPTOR[i];

      final URL obsURL = urlUtilities.resolveURL( zmlURLDir, tsDesc.id + ".zml" );
      try
      {
        final IObservation obs = ZmlFactory.parseXML( obsURL, "" );

        tsmap.addObservation( obs, tsDesc.id );

        if( tsDesc.id.startsWith( "W_" ) )
        {
          // neuen Namen generieren
          final String qName = "Q_" + tsDesc.id.substring( 2 );

          final WQObservationFilter filter = new WQObservationFilter();
          filter.initFilter( TimeserieConstants.TYPE_WATERLEVEL, obs );

          tsmap.addObservation( filter, qName );
        }
      }
      catch( final NoSuchElementException nse )
      {
        // passiert, wenn es keine entsprechende Axen giebt
        nse.printStackTrace();

        throw new CalcJobServiceException( "Fehlerhafte Eingabedateien: " + obsURL.toString(), nse );
      }
      catch( final SensorException se )
      {
        se.printStackTrace();
//
//        throw new CalcJobServiceException(
//            "Fehler beim Einlesen der Zeitreihen: " + obsURL.toString(), se );
      }
    }

    return tsmap;
  }

  public static void findAndWriteLayer( final GMLWorkspace workspace,
      final String layerName, final Map mapping, final String geoName,
      final String filenameBase ) throws CalcJobServiceException
  {
    try
    {
      final FeatureType featureType = workspace.getFeatureType( layerName );
      if( featureType == null )
        throw new CalcJobServiceException(
            "Eingabedatei für Rechenmodell konnte nicht erzeugt werden. Layer nicht gefunden: "
                + layerName, null );

      final Feature[] features = workspace.getFeatures( featureType );

      ShapeSerializer.serializeFeatures( features, mapping, geoName,
          filenameBase );
    }
    catch( final GmlSerializeException e )
    {
      e.printStackTrace();

      throw new CalcJobServiceException(
          "Fehler beim Schreiben der Eingabedateien", e );
    }
  }

  public static Map parseControlFile( final URL gmlURL,
      final URL schemaURL, final File nativedir )
      throws CalcJobServiceException
  {
    try
    {
      final Feature controlFeature = GmlSerializer.createGMLWorkspace( gmlURL,
          schemaURL ).getRootFeature();

      final Date startSimTime = (Date) controlFeature
          .getProperty( "startsimulation" );
      final Date startForecastTime = (Date) controlFeature
          .getProperty( "startforecast" );

      final String startTimeString = new SimpleDateFormat( "yyMMdd" )
          .format( startForecastTime );
      final String baseFileName = "HW" + startTimeString;

      final String tsFilename = new File( nativedir, baseFileName )
          .getAbsolutePath();
      final File tsFile = new File( tsFilename + ".dbf" );
      final String napFilename = tsFilename + SpreeCalcJob.NAP_FILE;
      final File napFile = new File( napFilename + ".dbf" );
      final File vhsFile = new File( tsFilename + SpreeCalcJob.VHS_FILE );
      final String flpFilename = tsFilename + SpreeCalcJob.FLP_FILE;
      final File flpFile = new File( flpFilename + ".dbf" );

      final Map dataMap = new HashMap();
      dataMap.put( SpreeCalcJob.DATA_STARTSIM_DATE, startSimTime );
      dataMap.put( SpreeCalcJob.DATA_STARTFORECAST_DATE, startForecastTime );
      dataMap.put( SpreeCalcJob.DATA_STARTDATESTRING, startTimeString );
      dataMap.put( SpreeCalcJob.DATA_BASEFILENAME, baseFileName );
      dataMap.put( SpreeCalcJob.DATA_FLPFILE, flpFile );
      dataMap.put( SpreeCalcJob.DATA_VHSFILE, vhsFile );
      dataMap.put( SpreeCalcJob.DATA_NAPFILE, napFile );
      dataMap.put( SpreeCalcJob.DATA_FLPFILENAME, flpFilename );
      dataMap.put( SpreeCalcJob.DATA_NAPFILENAME, napFilename );
      dataMap.put( SpreeCalcJob.DATA_TSFILENAME, tsFilename );
      dataMap.put( SpreeCalcJob.DATA_TSFILE, tsFile );

      return dataMap;
    }
    catch( final Exception e )
    {
      throw new CalcJobServiceException(
          "Fehler beim Einlesen der Berechnungsparameter", e );
    }
  }

  /**
   * Gibt die Datei zum entsprechenden index zurück
   * 
   * @param id
   * @param input
   * @param basedir
   * @return file
   * 
   * @throws CalcJobServiceException
   */
  public static File checkInput( final String id, final Map input,
      final File basedir ) throws CalcJobServiceException
  {
    final CalcJobClientBean bean = (CalcJobClientBean) input.get( id );
    if( bean == null )
      throw new CalcJobServiceException( "Eingabedatei für Index <" + id
          + "> fehlt", null );

    final File file = new File( basedir, bean.getPath() );
    if( !file.exists() || !file.isFile() )
      throw new CalcJobServiceException( "Eingabedatei für Index <" + id
          + "> fehlt: " + file.getAbsolutePath(), null );

    return file;
  }
}